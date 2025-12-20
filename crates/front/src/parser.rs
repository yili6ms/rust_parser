use crate::{
    ast::{
        BinaryOp, Block, BlockItem, Expr, ExprKind, Function, Item, LetStmt, Param, Program,
        TypeExpr,
    },
    span::Span,
    token::{Token, TokenKind},
};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("unexpected token {found:?} at {span}, expected {expected}")]
    UnexpectedToken {
        expected: &'static str,
        found: TokenKind,
        span: Span,
    },
    #[error("unexpected end of input, expected {expected}")]
    UnexpectedEof { expected: &'static str },
}

pub fn parse(tokens: &[Token]) -> Result<Program, ParseError> {
    let mut parser = Parser { tokens, pos: 0 };
    parser.parse_program()
}

struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
}

impl<'a> Parser<'a> {
    fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut items = Vec::new();
        while !self.is_at_end() {
            items.push(self.parse_item()?);
        }
        Ok(Program { items })
    }

    fn parse_item(&mut self) -> Result<Item, ParseError> {
        self.expect_token("`fn`", |k| matches!(k, TokenKind::Fn))?;
        let (name, name_span) = self.expect_ident()?;
        self.expect_token("`(`", |k| matches!(k, TokenKind::LParen))?;
        let mut params = Vec::new();
        if !self.check(|k| matches!(k, TokenKind::RParen)) {
            loop {
                params.push(self.parse_param()?);
                if self
                    .match_token(|k| matches!(k, TokenKind::Comma))
                    .is_some()
                {
                    continue;
                }
                break;
            }
        }
        self.expect_token("`)`", |k| matches!(k, TokenKind::RParen))?;
        self.expect_token("`->`", |k| matches!(k, TokenKind::Arrow))?;
        let ret_type = self.parse_type_expr()?;
        let body = self.parse_block_expr()?;
        let span = name_span.join(body.span);
        Ok(Item::Function(Function {
            name,
            params,
            ret_type,
            body,
            span,
        }))
    }

    fn parse_param(&mut self) -> Result<Param, ParseError> {
        let (name, span) = self.expect_ident()?;
        self.expect_token("`:`", |k| matches!(k, TokenKind::Colon))?;
        let ty = self.parse_type_expr()?;
        Ok(Param { name, ty, span })
    }

    fn parse_type_expr(&mut self) -> Result<TypeExpr, ParseError> {
        if self
            .match_token(|k| matches!(k, TokenKind::I32Type))
            .is_some()
        {
            Ok(TypeExpr::I32)
        } else if self
            .match_token(|k| matches!(k, TokenKind::BoolType))
            .is_some()
        {
            Ok(TypeExpr::Bool)
        } else if self
            .match_token(|k| matches!(k, TokenKind::UnitType))
            .is_some()
        {
            Ok(TypeExpr::Unit)
        } else {
            Err(self.error("type"))
        }
    }

    fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        self.parse_if()
    }

    fn parse_if(&mut self) -> Result<Expr, ParseError> {
        if let Some(tok) = self.match_token(|k| matches!(k, TokenKind::If)) {
            let cond = self.parse_expression()?;
            self.expect_token("`then`", |k| matches!(k, TokenKind::Then))?;
            let then_branch = self.parse_expression()?;
            self.expect_token("`else`", |k| matches!(k, TokenKind::Else))?;
            let else_branch = self.parse_expression()?;
            let span = tok.span.join(else_branch.span);
            Ok(Expr::new(
                ExprKind::If {
                    cond: Box::new(cond),
                    then_branch: Box::new(then_branch),
                    else_branch: Box::new(else_branch),
                },
                span,
            ))
        } else {
            self.parse_equality()
        }
    }

    fn parse_equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_comparison()?;
        while self.match_token(|k| matches!(k, TokenKind::EqEq)).is_some() {
            let right = self.parse_comparison()?;
            let span = expr.span.join(right.span);
            expr = Expr::new(
                ExprKind::Binary {
                    op: BinaryOp::Eq,
                    left: Box::new(expr),
                    right: Box::new(right),
                },
                span,
            );
        }
        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_term()?;
        loop {
            let op = if self.match_token(|k| matches!(k, TokenKind::Less)).is_some() {
                Some(BinaryOp::Lt)
            } else if self
                .match_token(|k| matches!(k, TokenKind::Greater))
                .is_some()
            {
                Some(BinaryOp::Gt)
            } else {
                None
            };
            if let Some(op) = op {
                let right = self.parse_term()?;
                let span = expr.span.join(right.span);
                expr = Expr::new(
                    ExprKind::Binary {
                        op,
                        left: Box::new(expr),
                        right: Box::new(right),
                    },
                    span,
                );
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_term(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_factor()?;
        loop {
            let op = if self.match_token(|k| matches!(k, TokenKind::Plus)).is_some() {
                Some(BinaryOp::Add)
            } else if self
                .match_token(|k| matches!(k, TokenKind::Minus))
                .is_some()
            {
                Some(BinaryOp::Sub)
            } else {
                None
            };
            if let Some(op) = op {
                let right = self.parse_factor()?;
                let span = expr.span.join(right.span);
                expr = Expr::new(
                    ExprKind::Binary {
                        op,
                        left: Box::new(expr),
                        right: Box::new(right),
                    },
                    span,
                );
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_call()?;
        loop {
            let op = if self.match_token(|k| matches!(k, TokenKind::Star)).is_some() {
                Some(BinaryOp::Mul)
            } else if self
                .match_token(|k| matches!(k, TokenKind::Slash))
                .is_some()
            {
                Some(BinaryOp::Div)
            } else {
                None
            };
            if let Some(op) = op {
                let right = self.parse_call()?;
                let span = expr.span.join(right.span);
                expr = Expr::new(
                    ExprKind::Binary {
                        op,
                        left: Box::new(expr),
                        right: Box::new(right),
                    },
                    span,
                );
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_call(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_primary()?;
        loop {
            if self.check(|k| matches!(k, TokenKind::LParen)) {
                let start_span = expr.span;
                self.expect_token("`(`", |k| matches!(k, TokenKind::LParen))?;
                let mut args = Vec::new();
                if !self.check(|k| matches!(k, TokenKind::RParen)) {
                    loop {
                        args.push(self.parse_expression()?);
                        if self
                            .match_token(|k| matches!(k, TokenKind::Comma))
                            .is_some()
                        {
                            continue;
                        }
                        break;
                    }
                }
                let close = self.expect_token("`)`", |k| matches!(k, TokenKind::RParen))?;
                let span = start_span.join(close.span);
                expr = Expr::new(
                    ExprKind::Call {
                        callee: Box::new(expr),
                        args,
                    },
                    span,
                );
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        if let Some(tok) = self.match_token(|k| matches!(k, TokenKind::Int(_))) {
            if let TokenKind::Int(value) = tok.kind.clone() {
                return Ok(Expr::new(ExprKind::Int(value), tok.span));
            }
        }
        if let Some(tok) = self.match_token(|k| matches!(k, TokenKind::True)) {
            return Ok(Expr::new(ExprKind::Bool(true), tok.span));
        }
        if let Some(tok) = self.match_token(|k| matches!(k, TokenKind::False)) {
            return Ok(Expr::new(ExprKind::Bool(false), tok.span));
        }
        if let Some(tok) = self.match_token(|k| matches!(k, TokenKind::Ident(_))) {
            if let TokenKind::Ident(name) = tok.kind.clone() {
                return Ok(Expr::new(ExprKind::Var(name), tok.span));
            }
        }
        if self
            .match_token(|k| matches!(k, TokenKind::LParen))
            .is_some()
        {
            let expr = self.parse_expression()?;
            self.expect_token("`)`", |k| matches!(k, TokenKind::RParen))?;
            return Ok(expr);
        }
        if self.check(|k| matches!(k, TokenKind::LBrace)) {
            return self.parse_block_expr();
        }
        Err(self.error("expression"))
    }

    fn parse_block_expr(&mut self) -> Result<Expr, ParseError> {
        let open = self.expect_token("`{`", |k| matches!(k, TokenKind::LBrace))?;
        let mut items = Vec::new();
        while !self.check(|k| matches!(k, TokenKind::RBrace)) {
            if self.check(|k| matches!(k, TokenKind::Let)) {
                let let_tok = self.expect_token("`let`", |k| matches!(k, TokenKind::Let))?;
                let (name, _) = self.expect_ident()?;
                let ty = if self
                    .match_token(|k| matches!(k, TokenKind::Colon))
                    .is_some()
                {
                    Some(self.parse_type_expr()?)
                } else {
                    None
                };
                self.expect_token("`=`", |k| matches!(k, TokenKind::Equal))?;
                let value = self.parse_expression()?;
                let span = let_tok.span.join(value.span);
                items.push(BlockItem::Let(LetStmt {
                    name,
                    ty,
                    value,
                    span,
                }));
            } else {
                let expr = self.parse_expression()?;
                items.push(BlockItem::Expr(expr));
            }

            if self.check(|k| matches!(k, TokenKind::RBrace)) {
                break;
            }
            self.expect_token("`;`", |k| matches!(k, TokenKind::Semi))?;
        }
        let close = self.expect_token("`}`", |k| matches!(k, TokenKind::RBrace))?;
        let span = open.span.join(close.span);
        Ok(Expr::new(ExprKind::Block(Block { items }), span))
    }

    fn expect_ident(&mut self) -> Result<(String, Span), ParseError> {
        match self.peek() {
            Some(Token {
                kind: TokenKind::Ident(name),
                span,
            }) => {
                let name = name.clone();
                let span = *span;
                self.advance();
                Ok((name, span))
            }
            Some(token) => Err(ParseError::UnexpectedToken {
                expected: "identifier",
                found: token.kind.clone(),
                span: token.span,
            }),
            None => Err(ParseError::UnexpectedEof {
                expected: "identifier",
            }),
        }
    }

    fn match_token(&mut self, predicate: impl Fn(&TokenKind) -> bool) -> Option<&'a Token> {
        if let Some(token) = self.peek() {
            if predicate(&token.kind) {
                let tok = &self.tokens[self.pos];
                self.pos += 1;
                return Some(tok);
            }
        }
        None
    }

    fn expect_token(
        &mut self,
        expected: &'static str,
        predicate: impl Fn(&TokenKind) -> bool,
    ) -> Result<&'a Token, ParseError> {
        if let Some(token) = self.peek() {
            if predicate(&token.kind) {
                let tok = &self.tokens[self.pos];
                self.pos += 1;
                return Ok(tok);
            }
            Err(ParseError::UnexpectedToken {
                expected,
                found: token.kind.clone(),
                span: token.span,
            })
        } else {
            Err(ParseError::UnexpectedEof { expected })
        }
    }

    fn check(&self, predicate: impl Fn(&TokenKind) -> bool) -> bool {
        self.peek().map_or(false, |tok| predicate(&tok.kind))
    }

    fn advance(&mut self) -> Option<&'a Token> {
        if self.is_at_end() {
            None
        } else {
            let tok = &self.tokens[self.pos];
            self.pos += 1;
            Some(tok)
        }
    }

    fn peek(&self) -> Option<&'a Token> {
        self.tokens.get(self.pos)
    }

    fn is_at_end(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    fn error(&self, expected: &'static str) -> ParseError {
        if let Some(token) = self.peek() {
            ParseError::UnexpectedToken {
                expected,
                found: token.kind.clone(),
                span: token.span,
            }
        } else {
            ParseError::UnexpectedEof { expected }
        }
    }
}

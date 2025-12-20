use crate::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Fn,
    Let,
    If,
    Then,
    Else,
    True,
    False,
    I32Type,
    BoolType,
    UnitType,
    Ident(String),
    Int(i32),
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Colon,
    Arrow,
    Equal,
    EqEq,
    Less,
    Greater,
    Plus,
    Minus,
    Star,
    Slash,
    Semi,
}

impl Token {
    pub fn simple(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}

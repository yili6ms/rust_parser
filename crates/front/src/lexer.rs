use crate::{
    span::Span,
    token::{Token, TokenKind},
};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum LexError {
    #[error("unexpected character `{ch}` at {span}")]
    UnexpectedCharacter { ch: char, span: Span },
    #[error("invalid integer literal `{literal}` at {span}")]
    InvalidNumber { literal: String, span: Span },
}

pub fn lex(source: &str) -> Result<Vec<Token>, LexError> {
    let mut tokens = Vec::new();
    let bytes = source.as_bytes();
    let mut idx = 0;

    while idx < bytes.len() {
        let ch = bytes[idx] as char;
        if ch.is_ascii_whitespace() {
            idx += 1;
            continue;
        }

        if ch == '/' {
            if idx + 1 < bytes.len() && bytes[idx + 1] as char == '/' {
                idx += 2;
                while idx < bytes.len() && bytes[idx] as char != '\n' {
                    idx += 1;
                }
                continue;
            }
        }

        let start = idx;
        let token = match ch {
            '(' => {
                idx += 1;
                Token::simple(TokenKind::LParen, Span::new(start, idx))
            }
            ')' => {
                idx += 1;
                Token::simple(TokenKind::RParen, Span::new(start, idx))
            }
            '{' => {
                idx += 1;
                Token::simple(TokenKind::LBrace, Span::new(start, idx))
            }
            '}' => {
                idx += 1;
                Token::simple(TokenKind::RBrace, Span::new(start, idx))
            }
            ',' => {
                idx += 1;
                Token::simple(TokenKind::Comma, Span::new(start, idx))
            }
            ':' => {
                idx += 1;
                Token::simple(TokenKind::Colon, Span::new(start, idx))
            }
            ';' => {
                idx += 1;
                Token::simple(TokenKind::Semi, Span::new(start, idx))
            }
            '+' => {
                idx += 1;
                Token::simple(TokenKind::Plus, Span::new(start, idx))
            }
            '*' => {
                idx += 1;
                Token::simple(TokenKind::Star, Span::new(start, idx))
            }
            '/' => {
                idx += 1;
                Token::simple(TokenKind::Slash, Span::new(start, idx))
            }
            '<' => {
                idx += 1;
                Token::simple(TokenKind::Less, Span::new(start, idx))
            }
            '>' => {
                idx += 1;
                Token::simple(TokenKind::Greater, Span::new(start, idx))
            }
            '-' => {
                if idx + 1 < bytes.len() && bytes[idx + 1] as char == '>' {
                    idx += 2;
                    Token::simple(TokenKind::Arrow, Span::new(start, idx))
                } else {
                    idx += 1;
                    Token::simple(TokenKind::Minus, Span::new(start, idx))
                }
            }
            '=' => {
                if idx + 1 < bytes.len() && bytes[idx + 1] as char == '=' {
                    idx += 2;
                    Token::simple(TokenKind::EqEq, Span::new(start, idx))
                } else {
                    idx += 1;
                    Token::simple(TokenKind::Equal, Span::new(start, idx))
                }
            }
            '0'..='9' => {
                idx += 1;
                while idx < bytes.len() && (bytes[idx] as char).is_ascii_digit() {
                    idx += 1;
                }
                let literal = &source[start..idx];
                let value = literal
                    .parse::<i32>()
                    .map_err(|_| LexError::InvalidNumber {
                        literal: literal.to_string(),
                        span: Span::new(start, idx),
                    })?;
                Token {
                    kind: TokenKind::Int(value),
                    span: Span::new(start, idx),
                }
            }
            ch if is_ident_start(ch) => {
                idx += 1;
                while idx < bytes.len() && is_ident_continue(bytes[idx] as char) {
                    idx += 1;
                }
                let ident = &source[start..idx];
                let kind = match ident {
                    "fn" => TokenKind::Fn,
                    "let" => TokenKind::Let,
                    "if" => TokenKind::If,
                    "then" => TokenKind::Then,
                    "else" => TokenKind::Else,
                    "true" => TokenKind::True,
                    "false" => TokenKind::False,
                    "i32" => TokenKind::I32Type,
                    "bool" => TokenKind::BoolType,
                    "unit" => TokenKind::UnitType,
                    _ => TokenKind::Ident(ident.to_string()),
                };
                Token {
                    kind,
                    span: Span::new(start, idx),
                }
            }
            _ => {
                return Err(LexError::UnexpectedCharacter {
                    ch,
                    span: Span::new(start, start + ch.len_utf8()),
                });
            }
        };

        tokens.push(token);
    }

    Ok(tokens)
}

fn is_ident_start(ch: char) -> bool {
    ch == '_' || ch.is_ascii_alphabetic()
}

fn is_ident_continue(ch: char) -> bool {
    is_ident_start(ch) || ch.is_ascii_digit()
}

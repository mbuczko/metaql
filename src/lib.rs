use std::fmt::Display;

use thiserror::Error;

#[derive(Debug, PartialEq, Eq)]
enum Token<'a> {
    RoundOpen,
    RoundClose,
    CurlyOpen,
    CurlyClose,
    SquareOpen,
    SquareClose,
    Eq,
    Comma,
    Str(&'a str),
    Unknown,
}

#[derive(Error, Debug, PartialEq)]
pub enum Brace {
    Round,
    Square,
    Curly,
}

impl Display for Brace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Brace::Round => f.write_str("(...)"),
            Brace::Curly => f.write_str("{...}"),
            Brace::Square => f.write_str("[...]"),
        }
    }
}

#[derive(Error, Debug, PartialEq)]
pub enum TokenizeError {
    #[error("non-terminated string")]
    InvalidString(usize),
    #[error("unbalanced braces")]
    UnbalancedBraces(usize, Brace),
}

fn whitespace(ch: char) -> bool {
    ch == ' ' || ch == '\r' || ch == '\n' || ch == '\t'
}

fn tokenize(input: &str) -> anyhow::Result<Vec<Token>, TokenizeError> {
    let chars = input.chars().enumerate();
    let mut output = Vec::new();
    let mut braces = Vec::new();
    let mut sstart = 0;

    for (pos, ch) in chars {
        if ch == '"' {
            if sstart > 0 {
                output.push(Token::Str(&input[sstart + 1..pos]));
                sstart = 0;
            } else {
                sstart = pos;
            }
        } else if sstart == 0 && !whitespace(ch) {
            let token = match ch {
                '(' => {
                    braces.push(Brace::Round);
                    Token::RoundOpen
                }
                ')' => match braces.pop() {
                    Some(Brace::Round) => Token::RoundClose,
                    b => {
                        return Err(TokenizeError::UnbalancedBraces(
                            pos,
                            b.unwrap_or(Brace::Round),
                        ))
                    }
                },
                '{' => {
                    braces.push(Brace::Curly);
                    Token::CurlyOpen
                }
                '}' => match braces.pop() {
                    Some(Brace::Curly) => Token::CurlyClose,
                    b => {
                        return Err(TokenizeError::UnbalancedBraces(
                            pos,
                            b.unwrap_or(Brace::Curly),
                        ))
                    }
                },
                '[' => {
                    braces.push(Brace::Square);
                    Token::SquareOpen
                }
                ']' => match braces.pop() {
                    Some(Brace::Square) => Token::SquareClose,
                    b => {
                        return Err(TokenizeError::UnbalancedBraces(
                            pos,
                            b.unwrap_or(Brace::Square),
                        ))
                    }
                },
                ':' => Token::Eq,
                ',' => Token::Comma,
                _ => Token::Unknown,
            };
            output.push(token);
        }
    }
    if sstart == 0 {
        Ok(output)
    } else {
        Err(TokenizeError::InvalidString(sstart))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_tokens() {
        let result = tokenize("{ { } }").unwrap();
        assert_eq!(
            result,
            vec![
                Token::CurlyOpen,
                Token::CurlyOpen,
                Token::CurlyClose,
                Token::CurlyClose
            ]
        );
    }
    #[test]
    fn valid_string() {
        let result = tokenize("{ \"lorem ipsum\" }").unwrap();
        assert_eq!(
            result,
            vec![
                Token::CurlyOpen,
                Token::Str("lorem ipsum"),
                Token::CurlyClose
            ]
        );
    }
    #[test]
    fn invalid_string() {
        let _result = tokenize("{\"lorem ipsum }").unwrap_err();
        assert_eq!(TokenizeError::InvalidString(1), _result);
    }
    #[test]
    fn unbalanced_braces() {
        let _result = tokenize(")").unwrap_err();
        assert_eq!(TokenizeError::UnbalancedBraces(0, Brace::Round), _result);

        let _result = tokenize("[)]").unwrap_err();
        assert_eq!(
            TokenizeError::UnbalancedBraces(1, Brace::Square),
            _result
        );

        let _result = tokenize("([{{]}}])").unwrap_err();
        assert_eq!(
            TokenizeError::UnbalancedBraces(4, Brace::Curly),
            _result
        );
    }
}

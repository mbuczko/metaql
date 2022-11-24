use std::fmt::Display;

use thiserror::Error;

#[derive(Debug, PartialEq)]
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
    Ident(&'a str),
    Float(f64),
    Integer(i64),
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
    #[error("malformed float number")]
    MalformedFloat(usize),
}

fn whitespace(ch: char) -> bool {
    ch == ' ' || ch == '\r' || ch == '\n' || ch == '\t'
}

fn tokenize(input: &str) -> anyhow::Result<Vec<Token>, TokenizeError> {
    let mut chars = input.chars().enumerate().peekable();
    let mut output = Vec::new();
    let mut braces = Vec::new();

    while let Some((pos, ch)) = chars.next() {
        let token = match ch {
            b if whitespace(b) => continue,
            b if b.is_alphabetic() => {
                let mut eoi = pos;
                loop {
                    if let Some((p, ch)) = chars.peek() {
                        if ch.is_alphanumeric() || *ch == '-' {
                            eoi = *p;
                            chars.next();
                            continue;
                        }
                    }
                    break Token::Ident(&input[pos..eoi + 1]);
                }
            }
            b if b.is_numeric() => {
                let mut eoi = pos;
                let mut point = false;
                loop {
                    if let Some((p, ch)) = chars.peek() {
                        if ch.is_numeric() {
                            eoi = *p;
                            chars.next();
                            continue;
                        }
                        // is it a float number, maybe?
                        if *ch == '.' {
                            if !point {
                                chars.next();
                                point = true;
                                continue;
                            }
                        }
                    }
                    break if point {
                        Token::Float(
                            input[pos..eoi + 1]
                                .parse()
                                .map_err(|_| TokenizeError::MalformedFloat(eoi))?
                        )
                    } else {
                        Token::Integer(input[pos..eoi + 1].parse().unwrap())
                    };
                }
            }
            '"' => loop {
                if let Some((p, ch)) = chars.next() {
                    if ch == '"' {
                        break Token::Str(&input[pos + 1..p]);
                    }
                } else {
                    return Err(TokenizeError::InvalidString(pos));
                }
            },
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
    Ok(output)
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
        let result = tokenize(")").unwrap_err();
        assert_eq!(TokenizeError::UnbalancedBraces(0, Brace::Round), result);

        let result = tokenize("[)]").unwrap_err();
        assert_eq!(TokenizeError::UnbalancedBraces(1, Brace::Square), result);

        let result = tokenize("([{{]}}])").unwrap_err();
        assert_eq!(TokenizeError::UnbalancedBraces(4, Brace::Curly), result);
    }
    #[test]
    fn ident() {
        let result = tokenize("{mime-type:\"image/png\"}").unwrap();
        assert_eq!(
            result,
            vec![
                Token::CurlyOpen,
                Token::Ident("mime-type"),
                Token::Eq,
                Token::Str("image/png"),
                Token::CurlyClose
            ]
        );
    }
    #[test]
    fn integer() {
        let result = tokenize("{focal-length: 32}").unwrap();
        assert_eq!(
            result,
            vec![
                Token::CurlyOpen,
                Token::Ident("focal-length"),
                Token::Eq,
                Token::Integer(32),
                Token::CurlyClose
            ]
        );
    }
    #[test]
    fn float() {
        let result = tokenize("{width: 32.122}").unwrap();
        assert_eq!(
            result,
            vec![
                Token::CurlyOpen,
                Token::Ident("width"),
                Token::Eq,
                Token::Float(32.122),
                Token::CurlyClose
            ]
        );
        let result = tokenize("{width: 32.122.3}").unwrap();
        assert_eq!(
            result,
            vec![
                Token::CurlyOpen,
                Token::Ident("width"),
                Token::Eq,
                Token::Float(32.122),
                Token::Unknown,
                Token::Integer(3),
                Token::CurlyClose
            ]
        );
        let result = tokenize("{width: 32.}").unwrap();
        assert_eq!(
            result,
            vec![
                Token::CurlyOpen,
                Token::Ident("width"),
                Token::Eq,
                Token::Float(32.0),
                Token::CurlyClose
            ]
        );

    }
}

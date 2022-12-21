use std::fmt::Display;
use thiserror::Error;

#[derive(Debug, PartialEq)]
pub enum Terminal<'a> {
    RoundOpen,
    RoundClose,
    CurlyOpen,
    CurlyClose,
    SquareOpen,
    SquareClose,
    Colon,
    Comma,
    Equal,
    Exclamation,
    Contains,
    String(&'a str),
    Path(&'a str),
    Float(f32),
    Integer(i32),
    Bool(bool),
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

#[derive(Debug, PartialEq)]
pub struct Token<'a>(pub Terminal<'a>, pub usize, pub usize);

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

pub fn tokenize(input: &str) -> Result<Vec<Token>, TokenizeError> {
    let mut chars = input.chars().enumerate().peekable();
    let mut output = Vec::new();
    let mut braces = Vec::with_capacity(8);

    while let Some((pos, ch)) = chars.next() {
        let token = match ch {
            b if whitespace(b) => continue,
            b if b.is_alphabetic() => {
                let mut eoi = pos;
                loop {
                    if let Some((p, ch)) = chars.peek() {
                        if ch.is_alphanumeric() || *ch == '-' || *ch == '.' || *ch == '_' {
                            eoi = *p;
                            chars.next();
                            continue;
                        }
                    }
                    let string = &input[pos..eoi + 1];
                    let boolean = string == "true" || string == "false";
                    break if boolean {
                        Token(Terminal::Bool(string.parse().unwrap()), pos, eoi)
                    } else {
                        Token(Terminal::Path(string), pos, eoi)
                    };
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
                        // a float number, maybe?
                        if *ch == '.' {
                            if !point {
                                chars.next();
                                point = true;
                                continue;
                            }
                        }
                    }
                    break if point {
                        Token(
                            Terminal::Float(
                                input[pos..eoi + 1]
                                    .parse()
                                    .map_err(|_| TokenizeError::MalformedFloat(eoi))?
                            ),
                            pos, eoi
                        )
                    } else {
                        Token(
                            Terminal::Integer(input[pos..eoi + 1].parse().unwrap()),
                            pos, eoi
                        )
                    };
                }
            }
            '"' => loop {
                if let Some((p, ch)) = chars.next() {
                    if ch == '"' {
                        break Token(Terminal::String(&input[pos + 1..p]), pos, p)
                    }
                } else {
                    return Err(TokenizeError::InvalidString(pos));
                }
            },
            '(' => {
                braces.push(Brace::Round);
                Token(Terminal::RoundOpen, pos, pos)
            }
            ')' => match braces.pop() {
                Some(Brace::Round) => Token(Terminal::RoundClose, pos, pos),
                b => {
                    return Err(TokenizeError::UnbalancedBraces(
                        pos,
                        b.unwrap_or(Brace::Round),
                    ))
                }
            },
            '{' => {
                braces.push(Brace::Curly);
                Token(Terminal::CurlyOpen, pos, pos)
            }
            '}' => match braces.pop() {
                Some(Brace::Curly) => Token(Terminal::CurlyClose, pos, pos),
                b => {
                    return Err(TokenizeError::UnbalancedBraces(
                        pos,
                        b.unwrap_or(Brace::Curly),
                    ))
                }
            },
            '[' => {
                braces.push(Brace::Square);
                Token(Terminal::SquareOpen, pos, pos)
            }
            ']' => match braces.pop() {
                Some(Brace::Square) => Token(Terminal::SquareClose, pos, pos),
                b => {
                    return Err(TokenizeError::UnbalancedBraces(
                        pos,
                        b.unwrap_or(Brace::Square),
                    ))
                }
            },
            '!' => Token(Terminal::Exclamation, pos, pos),
            ':' => Token(Terminal::Colon, pos, pos),
            ',' => Token(Terminal::Comma, pos, pos),
            '=' => Token(Terminal::Equal, pos, pos),
            '~' => Token(Terminal::Contains, pos, pos),
            _ => Token(Terminal::Unknown, pos, pos),
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
                Token(Terminal::CurlyOpen, 0, 0),
                Token(Terminal::CurlyOpen, 2, 2),
                Token(Terminal::CurlyClose, 4, 4),
                Token(Terminal::CurlyClose, 6, 6)
            ]
        );
    }

    #[test]
    fn valid_string() {
        let result = tokenize("{ \"lorem ipsum\" }").unwrap();
        assert_eq!(
            result,
            vec![
                Token(Terminal::CurlyOpen, 0, 0),
                Token(Terminal::String("lorem ipsum"), 2, 14),
                Token(Terminal::CurlyClose, 16, 16)
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
    fn path() {
        let result = tokenize("{mime-type:\"image/png\"}").unwrap();
        assert_eq!(
            result,
            vec![
                Token(Terminal::CurlyOpen, 0, 0),
                Token(Terminal::Path("mime-type"), 1, 9),
                Token(Terminal::Colon, 10, 10),
                Token(Terminal::String("image/png"), 11, 21),
                Token(Terminal::CurlyClose, 22, 22)
            ]
        );
    }
    #[test]
    fn integer() {
        let result = tokenize("{focal-length: 32}").unwrap();
        assert_eq!(
            result,
            vec![
                Token(Terminal::CurlyOpen, 0, 0),
                Token(Terminal::Path("focal-length"), 1, 12),
                Token(Terminal::Colon, 13, 13),
                Token(Terminal::Integer(32), 15, 16),
                Token(Terminal::CurlyClose, 17, 17)
            ]
        );
    }
    #[test]
    fn float() {
        let result = tokenize("{width: 32.122}").unwrap();
        assert_eq!(
            result,
            vec![
                Token(Terminal::CurlyOpen, 0, 0),
                Token(Terminal::Path("width"), 1, 5),
                Token(Terminal::Colon, 6, 6),
                Token(Terminal::Float(32.122), 8, 13),
                Token(Terminal::CurlyClose, 14, 14)
            ]
        );
        let result = tokenize("{width: 32.122.3}").unwrap();
        assert_eq!(
            result,
            vec![
                Token(Terminal::CurlyOpen, 0, 0),
                Token(Terminal::Path("width"), 1, 5),
                Token(Terminal::Colon, 6, 6),
                Token(Terminal::Float(32.122), 8, 13),
                Token(Terminal::Unknown, 14, 14),
                Token(Terminal::Integer(3), 15, 15),
                Token(Terminal::CurlyClose, 16, 16)
            ]
        );
        let result = tokenize("{width: 32..}").unwrap();
        assert_eq!(
            result,
            vec![
                Token(Terminal::CurlyOpen, 0, 0),
                Token(Terminal::Path("width"), 1, 5),
                Token(Terminal::Colon, 6, 6),
                Token(Terminal::Float(32.0), 8, 9),
                Token(Terminal::Unknown, 11, 11),
                Token(Terminal::CurlyClose, 12, 12)
            ]
        );
    }
}

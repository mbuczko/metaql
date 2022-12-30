use std::fmt::Display;
use thiserror::Error;

#[derive(Debug, PartialEq)]
pub enum Term<'a> {
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
pub struct Token<'a>(pub Term<'a>, pub usize, pub usize);

#[derive(Error, Debug, PartialEq)]
pub enum TokenizeError {
    #[error("non-terminated string")]
    InvalidString(usize),
    #[error("unbalanced braces")]
    UnbalancedBraces(usize, Brace),
    #[error("malformed numeric value")]
    MalformedNumeric(usize),
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
                        Token(Term::Bool(string.parse().unwrap()), pos, eoi)
                    } else {
                        Token(Term::Path(string), pos, eoi)
                    };
                }
            }
            b if b.is_numeric() || b == '-' => {
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
                        if *ch == '.' && !point {
                            chars.next();
                            point = true;
                            continue;
                        }
                    }
                    break if point {
                        Token(
                            Term::Float(
                                input[pos..eoi + 1]
                                    .parse()
                                    .map_err(|_| TokenizeError::MalformedNumeric(eoi))?,
                            ),
                            pos,
                            eoi,
                        )
                    } else {
                        Token(
                            Term::Integer(
                                input[pos..eoi + 1]
                                    .parse()
                                    .map_err(|_| TokenizeError::MalformedNumeric(eoi))?,
                            ),
                            pos,
                            eoi,
                        )
                    };
                }
            }
            '"' => loop {
                if let Some((p, ch)) = chars.next() {
                    if ch == '"' {
                        break Token(Term::String(&input[pos + 1..p]), pos, p);
                    }
                } else {
                    return Err(TokenizeError::InvalidString(pos));
                }
            },
            '(' => {
                braces.push(Brace::Round);
                Token(Term::RoundOpen, pos, pos)
            }
            ')' => match braces.pop() {
                Some(Brace::Round) => Token(Term::RoundClose, pos, pos),
                b => {
                    return Err(TokenizeError::UnbalancedBraces(
                        pos,
                        b.unwrap_or(Brace::Round),
                    ))
                }
            },
            '{' => {
                braces.push(Brace::Curly);
                Token(Term::CurlyOpen, pos, pos)
            }
            '}' => match braces.pop() {
                Some(Brace::Curly) => Token(Term::CurlyClose, pos, pos),
                b => {
                    return Err(TokenizeError::UnbalancedBraces(
                        pos,
                        b.unwrap_or(Brace::Curly),
                    ))
                }
            },
            '[' => {
                braces.push(Brace::Square);
                Token(Term::SquareOpen, pos, pos)
            }
            ']' => match braces.pop() {
                Some(Brace::Square) => Token(Term::SquareClose, pos, pos),
                b => {
                    return Err(TokenizeError::UnbalancedBraces(
                        pos,
                        b.unwrap_or(Brace::Square),
                    ))
                }
            },
            '!' => Token(Term::Exclamation, pos, pos),
            ':' => Token(Term::Colon, pos, pos),
            ',' => Token(Term::Comma, pos, pos),
            '=' => Token(Term::Equal, pos, pos),
            '~' => Token(Term::Contains, pos, pos),
            _ => Token(Term::Unknown, pos, pos),
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
        let result = tokenize("{  ()  }").unwrap();
        assert_eq!(
            result,
            vec![
                Token(Term::CurlyOpen, 0, 0),
                Token(Term::RoundOpen, 3, 3),
                Token(Term::RoundClose, 4, 4),
                Token(Term::CurlyClose, 7, 7)
            ]
        );
    }

    #[test]
    fn valid_string() {
        let result = tokenize("{ \"lorem ipsum\" }").unwrap();
        assert_eq!(
            result,
            vec![
                Token(Term::CurlyOpen, 0, 0),
                Token(Term::String("lorem ipsum"), 2, 14),
                Token(Term::CurlyClose, 16, 16)
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
                Token(Term::CurlyOpen, 0, 0),
                Token(Term::Path("mime-type"), 1, 9),
                Token(Term::Colon, 10, 10),
                Token(Term::String("image/png"), 11, 21),
                Token(Term::CurlyClose, 22, 22)
            ]
        );
    }

    #[test]
    fn integer() {
        let result = tokenize("{focal-length: 32}").unwrap();
        assert_eq!(
            result,
            vec![
                Token(Term::CurlyOpen, 0, 0),
                Token(Term::Path("focal-length"), 1, 12),
                Token(Term::Colon, 13, 13),
                Token(Term::Integer(32), 15, 16),
                Token(Term::CurlyClose, 17, 17)
            ]
        );
    }

    #[test]
    fn float() {
        let result = tokenize("{width: 32.122}").unwrap();
        assert_eq!(
            result,
            vec![
                Token(Term::CurlyOpen, 0, 0),
                Token(Term::Path("width"), 1, 5),
                Token(Term::Colon, 6, 6),
                Token(Term::Float(32.122), 8, 13),
                Token(Term::CurlyClose, 14, 14)
            ]
        );
        let result = tokenize("{width: 32.122.3}").unwrap();
        assert_eq!(
            result,
            vec![
                Token(Term::CurlyOpen, 0, 0),
                Token(Term::Path("width"), 1, 5),
                Token(Term::Colon, 6, 6),
                Token(Term::Float(32.122), 8, 13),
                Token(Term::Unknown, 14, 14),
                Token(Term::Integer(3), 15, 15),
                Token(Term::CurlyClose, 16, 16)
            ]
        );
        let result = tokenize("{width: 32..}").unwrap();
        assert_eq!(
            result,
            vec![
                Token(Term::CurlyOpen, 0, 0),
                Token(Term::Path("width"), 1, 5),
                Token(Term::Colon, 6, 6),
                Token(Term::Float(32.0), 8, 9),
                Token(Term::Unknown, 11, 11),
                Token(Term::CurlyClose, 12, 12)
            ]
        );
    }

    #[test]
    fn negative() {
        let result = tokenize("-10 -3.12").unwrap();
        assert_eq!(
            result,
            vec![
                Token(Term::Integer(-10), 0, 2),
                Token(Term::Float(-3.12), 4, 8)
            ]
        )
    }

    #[test]
    fn invalid_negative() {
        let result = tokenize("-\"negative\"").unwrap_err();
        assert_eq!(result, TokenizeError::MalformedNumeric(0));

        let result = tokenize("-true").unwrap_err();
        assert_eq!(result, TokenizeError::MalformedNumeric(0));
    }

    #[test]
    fn booleans() {
        let result = tokenize("true flase false").unwrap();
        assert_eq!(
            result,
            vec![
                Token(Term::Bool(true), 0, 3),
                Token(Term::Path("flase"), 5, 9),
                Token(Term::Bool(false), 11, 15)
            ]
        )
    }
}

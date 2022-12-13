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
    Colon,
    Comma,
    Equal,
    Exclamation,
    Contains,
    String(&'a str),
    Path(&'a str),
    Float(f32),
    Integer(i32),
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

#[derive(Error, Debug, PartialEq)]
pub enum ParseError {
    #[error("malformed expression")]
    MalformedExpression,
    #[error("malformed filter")]
    MalformedFilter,
    #[error("invalid filter value")]
    MalformedFilterValue,
    #[error("invalid filter comparision operator")]
    MalformedFilterOperator,
}

fn whitespace(ch: char) -> bool {
    ch == ' ' || ch == '\r' || ch == '\n' || ch == '\t'
}

fn tokenize(input: &str) -> Result<Vec<Token>, TokenizeError> {
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
                    break Token::Path(&input[pos..eoi + 1]);
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
                        Token::Float(
                            input[pos..eoi + 1]
                                .parse()
                                .map_err(|_| TokenizeError::MalformedFloat(eoi))?,
                        )
                    } else {
                        Token::Integer(input[pos..eoi + 1].parse().unwrap())
                    };
                }
            }
            '"' => loop {
                if let Some((p, ch)) = chars.next() {
                    if ch == '"' {
                        break Token::String(&input[pos + 1..p]);
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
            '!' => Token::Exclamation,
            ':' => Token::Colon,
            ',' => Token::Comma,
            '=' => Token::Equal,
            '~' => Token::Contains,
            _ => Token::Unknown,
        };
        output.push(token);
    }
    Ok(output)
}

/// Parser rules:
///
/// expr     ->  query range?
/// query    ->  "{" filter (COMMA filter)* "}"
/// filter   ->  PATH op value
/// op       ->  "!"? (EQ | CONTAINS)
/// value    ->  STRING | BOOL | numeric
/// numeric  ->  "-"? (INTEGER | FLOAT)

enum Operator {
    Equal,
    Contains,
}

enum Value {
    String(String),
    Integer(i32),
    Float(f32),
    Bool(bool),
}

enum Matcher<'a> {
    Exact(Token<'a>),
    Path,
    Operator,
    Negation,
    Value,
    Integer,
    Float,
    String,
    Numeric,
    Bool,
}
struct Expr<'a> {
    filters: Vec<Filter<'a>>,
    range: Option<Range>,
}

struct Filter<'a> {
    path: Vec<&'a str>, // property path split by dot
    value: Value,
    op: Operator,
    negated: bool,
}

struct Range {}

impl<'a> TryFrom<&Token<'a>> for Value {
    type Error = ParseError;

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match value {
            Token::String(s) => Ok(Self::String(s.to_string())),
            Token::Integer(i) => Ok(Self::Integer(*i)),
            Token::Float(f) => Ok(Self::Float(*f)),
            _ => Err(ParseError::MalformedFilterValue)
        }
    }
}

impl<'a> TryFrom<&Token<'a>> for Operator {
    type Error = ParseError;

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match value {
            Token::Equal => Ok(Self::Equal),
            Token::Contains => Ok(Self::Contains),
            _ => Err(ParseError::MalformedFilterOperator)
        }
    }
}

fn match_token<'a>(
    tokens: &'a [Token],
    matcher: Matcher,
) -> Option<(&'a Token<'a>, &'a [Token<'a>])> {
    if let Some(token) = tokens.first() {
        let result = match (matcher, token) {
            (Matcher::Exact(t), token) if t == *token => Some(token),
            (Matcher::Value, Token::String(_) | Token::Integer(_) | Token::Float(_)) => Some(token),
            (Matcher::Path, Token::Path(_)) => Some(token),
            (Matcher::Float, Token::Float(_)) => Some(token),
            (Matcher::Integer, Token::Integer(_)) => Some(token),
            (Matcher::Numeric, Token::Integer(_) | Token::Float(_)) => Some(token),
            (Matcher::Operator, Token::Equal | Token::Contains) => Some(token),
            (Matcher::Negation, Token::Exclamation) => Some(token),
            _ => None,
        };
        if let Some(matched) = result {
            return Some((matched, &tokens[1..]));
        }
    }
    None
}

fn parse_expression<'a>(tokens: &'a [Token]) -> Result<Expr<'a>, ParseError> {
    if let Some((_, tokens)) = match_token(tokens, Matcher::Exact(Token::CurlyOpen)) {
        let (filters, _) = parse_filters(tokens)?;
        let range = Some(Range {});

        return Ok(Expr { filters, range });
    }
    Err(ParseError::MalformedExpression)
}

fn parse_filters<'a>(
    tokens: &'a [Token],
) -> Result<(Vec<Filter<'a>>, &'a [Token<'a>]), ParseError> {
    let mut filters = Vec::<Filter>::new();

    // look for property path
    if let Some((Token::Path(id), tokens)) = match_token(tokens, Matcher::Path) {
        // look for operator (equal, contains, ...) and potential negation
        let negated = match_token(tokens, Matcher::Negation).is_some();
        if let Some((op, tokens)) = match_token(tokens, Matcher::Operator) {
            // look for a value and compose a `Filter` if all elements have been matched correctly
            if let Some((val, _)) = match_token(tokens, Matcher::Value) {
                filters.push(Filter {
                    path: id.to_owned().split('.').collect::<Vec<_>>(),
                    value: Value::try_from(val)?,
                    op: Operator::try_from(op)?,
                    negated
                });
            }
        }
    }
    Err(ParseError::MalformedFilter)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn token_matching() {
        let tokens = tokenize("{ meta.focal_length }").unwrap();

        let tokens = match_token(tokens.as_slice(), Matcher::Exact(Token::CurlyOpen));
        assert!(tokens.is_some());

        let tokens = match_token(tokens.unwrap().1, Matcher::Path);
        assert_eq!(tokens.unwrap().0, &Token::Path("meta.focal_length"));

        let tokens = match_token(tokens.unwrap().1, Matcher::Exact(Token::CurlyClose));
        assert!(tokens.is_some());
    }

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
                Token::String("lorem ipsum"),
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
    fn path() {
        let result = tokenize("{mime-type:\"image/png\"}").unwrap();
        assert_eq!(
            result,
            vec![
                Token::CurlyOpen,
                Token::Path("mime-type"),
                Token::Colon,
                Token::String("image/png"),
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
                Token::Path("focal-length"),
                Token::Colon,
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
                Token::Path("width"),
                Token::Colon,
                Token::Float(32.122),
                Token::CurlyClose
            ]
        );
        let result = tokenize("{width: 32.122.3}").unwrap();
        assert_eq!(
            result,
            vec![
                Token::CurlyOpen,
                Token::Path("width"),
                Token::Colon,
                Token::Float(32.122),
                Token::Unknown,
                Token::Integer(3),
                Token::CurlyClose
            ]
        );
        let result = tokenize("{width: 32..}").unwrap();
        assert_eq!(
            result,
            vec![
                Token::CurlyOpen,
                Token::Path("width"),
                Token::Colon,
                Token::Float(32.0),
                Token::Unknown,
                Token::CurlyClose
            ]
        );
    }
}

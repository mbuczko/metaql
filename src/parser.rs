use thiserror::Error;

use crate::lexer::{tokenize, Token};

/// Parser rules:
///
/// expr     ->  query range?
/// query    ->  "{" filter (COMMA filter)* "}"
/// filter   ->  PATH op value
/// op       ->  "!"? (EQ | CONTAINS)
/// value    ->  STRING | BOOL | numeric
/// numeric  ->  "-"? (INTEGER | FLOAT)

#[derive(Debug, PartialEq)]
enum Operator {
    Equal,
    Contains,
}

#[derive(Debug, PartialEq)]
enum Value {
    String(String),
    Integer(i32),
    Float(f32),
    Bool(bool),
}

pub enum Matcher<'a> {
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

#[derive(Debug)]
pub struct Expr<'a> {
    filters: Vec<Filter<'a>>,
    range: Option<Range>,
}

#[derive(Debug)]
pub struct Filter<'a> {
    path: Vec<&'a str>, // property path split by dot
    value: Value,
    op: Operator,
    negated: bool,
}

#[derive(Debug)]
pub struct Range {}

#[derive(Error, Debug, PartialEq)]
pub enum ParseError {
    #[error("malformed expression")]
    MalformedExpression,
    #[error("malformed filter")]
    MalformedFilter(u8),
    #[error("filter value expected")]
    MalformedFilterValue(u8, String),
    #[error("filter operator expected")]
    MalformedFilterOperator(u8, String),
    #[error("unknown value type")]
    UnknownFilterValueType,
    #[error("unknown filter operator")]
    UnknownFilterOperator,
}

impl<'a> TryFrom<&Token<'a>> for Value {
    type Error = ParseError;

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match value {
            Token::String(s) => Ok(Self::String(s.to_string())),
            Token::Integer(i) => Ok(Self::Integer(*i)),
            Token::Float(f) => Ok(Self::Float(*f)),
            Token::Bool(f) => Ok(Self::Bool(*f)),
            _ => Err(ParseError::UnknownFilterValueType),
        }
    }
}

impl<'a> TryFrom<&Token<'a>> for Operator {
    type Error = ParseError;

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match value {
            Token::Equal => Ok(Self::Equal),
            Token::Contains => Ok(Self::Contains),
            _ => Err(ParseError::UnknownFilterOperator),
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
            (
                Matcher::Value,
                Token::String(_) | Token::Integer(_) | Token::Float(_) | Token::Bool(_),
            ) => Some(token),
            (Matcher::Path, Token::Path(_)) => Some(token),
            (Matcher::Float, Token::Float(_)) => Some(token),
            (Matcher::Bool, Token::Bool(_)) => Some(token),
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

pub fn parse_expression<'a>(tokens: &'a [Token]) -> Result<Expr<'a>, ParseError> {
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
    let mut tokens_slice = tokens;
    let mut filter_idx = 0;

    // look for property path
    while let Some((Token::Path(id), tokens)) = match_token(tokens_slice, Matcher::Path) {
        // look for operator (equal, contains, ...) and potential negation
        let negated = match_token(tokens, Matcher::Negation).is_some();
        let tokens = if negated { &tokens[1..] } else { tokens };

        if let Some((op, tokens)) = match_token(tokens, Matcher::Operator) {
            // look for a value and compose a `Filter` if all elements have been matched correctly
            if let Some((val, _)) = match_token(tokens, Matcher::Value) {
                filters.push(Filter {
                    path: id.to_owned().split('.').collect::<Vec<_>>(),
                    value: Value::try_from(val)?,
                    op: Operator::try_from(op)?,
                    negated,
                });
            } else {
                return Err(ParseError::MalformedFilterValue(filter_idx, id.to_string()));
            }
        } else {
            return Err(ParseError::MalformedFilterOperator(
                filter_idx,
                id.to_string(),
            ));
        }
        tokens_slice = tokens;
        filter_idx += 1;
    }
    Ok((filters, tokens_slice))
}

pub fn main() {
    let tokens = tokenize("{meta.focal_length=2.15}").unwrap();
    let res = parse_expression(tokens.as_slice());
    println!("{:?}", res);
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
    fn parse_filter_simple() {
        let tokens = tokenize("{ meta.focal_length=2.3 }").unwrap();
        let expr = parse_expression(tokens.as_slice());

        assert!(expr.is_ok());

        let filters = expr.unwrap().filters;
        assert_eq!(filters.len(), 1);

        let filter = filters.first().unwrap();
        assert_eq!(filter.path, vec!["meta", "focal_length"]);
        assert_eq!(filter.op, Operator::Equal);
        assert_eq!(filter.value, Value::Float(2.3));
        assert!(!filter.negated);
    }

    #[test]
    fn parse_filter_negated() {
        let tokens = tokenize("{ meta.tag !~ \"favourite\" }").unwrap();
        let expr = parse_expression(tokens.as_slice());

        assert!(expr.is_ok());

        let filters = expr.unwrap().filters;
        assert_eq!(filters.len(), 1);

        let filter = filters.first().unwrap();
        assert_eq!(filter.path, vec!["meta", "tag"]);
        assert_eq!(filter.op, Operator::Contains);
        assert_eq!(filter.value, Value::String("favourite".to_string()));
        assert!(filter.negated);
    }

    #[test]
    fn invalid_operator() {
        let tokens = tokenize("{ meta.tag !+ \"wrong\" }").unwrap();
        let expr = parse_expression(tokens.as_slice()).unwrap_err();

        assert_eq!(
            expr,
            ParseError::MalformedFilterOperator(0, "meta.tag".to_string())
        );
    }

    #[test]
    fn invalid_value() {
        let tokens = tokenize("{ meta.tag != invalid }").unwrap();
        let expr = parse_expression(tokens.as_slice()).unwrap_err();

        assert_eq!(
            expr,
            ParseError::MalformedFilterValue(0, "meta.tag".to_string())
        );
    }
}

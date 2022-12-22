use thiserror::Error;

use crate::lexer::{tokenize, Terminal, Token};

/// Parser rules:
///
/// expr     ->  query range?
/// query    ->  CURLY_OPEN filter (COMMA filter)* CURLY_CLOSE
/// filter   ->  PATH op value
/// op       ->  "!"? (EQ | CONTAINS)
/// value    ->  STRING | BOOL | numeric
/// numeric  ->  (INTEGER | FLOAT)
/// range    ->  SQARE_OPEN duration SQUARE_CLOSE
/// duration ->  INTEGER unit
/// unit     ->  "ms" | "s" | "m" | "h" | "d" | "w" | "mo" | "y"

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
    Exact(Terminal<'a>),
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
    op_negative: bool,
}

#[derive(Debug)]
pub struct Range(i32, RangeUnit);

#[derive(Debug, PartialEq)]
pub enum RangeUnit {
    Milliseconds,
    Seconds,
    Minutes,
    Hours,
    Days,
    Weeks,
    Months,
    Years,
}

#[derive(Debug, PartialEq)]
pub struct ErrorOffset(usize);

#[derive(Error, Debug, PartialEq)]
pub enum ParseError {
    #[error("malformed expression")]
    MalformedExpression,
    #[error("malformed filter")]
    MalformedFilter(ErrorOffset),
    #[error("filter value expected")]
    MalformedFilterValue(ErrorOffset, String),
    #[error("filter operator expected")]
    MalformedFilterOperator(ErrorOffset, String),
    #[error("malformed range")]
    MalformedRange(ErrorOffset),
    #[error("range value (integer) expected")]
    MalformedRangeValue(ErrorOffset),
    #[error("filter separator (comma) expected")]
    UnknownFilterSeparator,
    #[error("unknown value type")]
    UnknownFilterValueType,
    #[error("unknown filter operator")]
    UnknownFilterOperator,
    #[error("unknown range unit")]
    UnknownRangeUnit,
}

impl TryFrom<&str> for RangeUnit {
    type Error = ParseError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "ms" => Ok(Self::Milliseconds),
            "s" => Ok(Self::Seconds),
            "m" => Ok(Self::Minutes),
            "h" => Ok(Self::Hours),
            "d" => Ok(Self::Days),
            "w" => Ok(Self::Weeks),
            "mo" => Ok(Self::Months),
            "y" => Ok(Self::Years),
            _ => Err(ParseError::UnknownRangeUnit),
        }
    }
}

impl<'a> TryFrom<&Token<'a>> for Value {
    type Error = ParseError;

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match value.0 {
            Terminal::String(s) => Ok(Self::String(s.to_string())),
            Terminal::Integer(i) => Ok(Self::Integer(i)),
            Terminal::Float(f) => Ok(Self::Float(f)),
            Terminal::Bool(b) => Ok(Self::Bool(b)),
            _ => Err(ParseError::UnknownFilterValueType),
        }
    }
}

impl<'a> TryFrom<&Token<'a>> for Operator {
    type Error = ParseError;

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match value.0 {
            Terminal::Equal => Ok(Self::Equal),
            Terminal::Contains => Ok(Self::Contains),
            _ => Err(ParseError::UnknownFilterOperator),
        }
    }
}

fn match_token<'a>(
    tokens: &'a [Token],
    matcher: Matcher,
) -> Option<(&'a Token<'a>, &'a [Token<'a>])> {
    if let Some(token) = tokens.first() {
        let Token(terminal, _, _) = token;
        let result = match (matcher, terminal) {
            (Matcher::Exact(t), terminal) if t == *terminal => Some(token),
            (
                Matcher::Value,
                Terminal::String(_) | Terminal::Integer(_) | Terminal::Float(_) | Terminal::Bool(_),
            ) => Some(token),
            (Matcher::Path, Terminal::Path(_)) => Some(token),
            (Matcher::Float, Terminal::Float(_)) => Some(token),
            (Matcher::Bool, Terminal::Bool(_)) => Some(token),
            (Matcher::Integer, Terminal::Integer(_)) => Some(token),
            (Matcher::Numeric, Terminal::Integer(_) | Terminal::Float(_)) => Some(token),
            (Matcher::Operator, Terminal::Equal | Terminal::Contains) => Some(token),
            (Matcher::Negation, Terminal::Exclamation) => Some(token),
            _ => None,
        };
        if let Some(matched) = result {
            return Some((matched, &tokens[1..]));
        }
    }
    None
}

pub fn parse_expression<'a>(tokens: &'a [Token]) -> Result<Expr<'a>, ParseError> {
    if let Some((_, tokens)) = match_token(tokens, Matcher::Exact(Terminal::CurlyOpen)) {
        let (filters, tokens) = parse_filters(tokens)?;

        // curly closing brace = end of filters
        if let Some((_, _tokens)) = match_token(tokens, Matcher::Exact(Terminal::CurlyClose)) {
            let range = Some(Range(0, RangeUnit::Minutes));
            return Ok(Expr { filters, range });
        }
        // something's wrong with filter path
        return Err(ParseError::MalformedFilter(ErrorOffset(tokens[0].2)));
    }
    // entire expression starts with wrong token
    Err(ParseError::MalformedExpression)
}

fn parse_filters<'a>(
    tokens: &'a [Token],
) -> Result<(Vec<Filter<'a>>, &'a [Token<'a>]), ParseError> {
    let mut filters = Vec::<Filter>::new();
    let mut tokens_slice = tokens;

    // look for property path first...
    while let Some((Token(Terminal::Path(id), _, _), tokens)) =
        match_token(tokens_slice, Matcher::Path)
    {
        // ...then look for operator (equal, contains, ...) and its potential negation
        let negative = match_token(tokens, Matcher::Negation).is_some();
        let tokens = if negative { &tokens[1..] } else { tokens };
        let (op, tokens) = match_token(tokens, Matcher::Operator).ok_or_else(|| {
            ParseError::MalformedFilterOperator(ErrorOffset(tokens[0].2), id.to_string())
        })?;

        // ...then look for filter value and if it matches one of possible variants - compose a `Filter`
        let (val, tokens) = match_token(tokens, Matcher::Value).ok_or_else(|| {
            ParseError::MalformedFilterValue(ErrorOffset(op.2 + 1), id.to_string())
        })?;

        tokens_slice = tokens;
        filters.push(Filter {
            path: id.to_owned().split('.').collect::<Vec<_>>(),
            value: Value::try_from(val)?,
            op: Operator::try_from(op)?,
            op_negative: negative,
        });

        if let Some((_, tokens)) = match_token(tokens_slice, Matcher::Exact(Terminal::Comma)) {
            tokens_slice = tokens;
        } else {
            break;
        }
    }
    Ok((filters, tokens_slice))
}

fn parse_range<'a>(tokens: &'a [Token]) -> Result<(Option<Range>, &'a [Token<'a>]), ParseError> {
    if let Some((_, tokens)) = match_token(tokens, Matcher::Exact(Terminal::SquareOpen)) {
        let (token, tokens) = match_token(tokens, Matcher::Integer)
            .ok_or_else(|| ParseError::MalformedRangeValue(ErrorOffset(tokens[0].2 + 1)))?;

        if let Terminal::Integer(int) = token.0 {
            let unit = match_token(tokens, Matcher::Path)
                .map(|(token, _)| match token.0 {
                    Terminal::Path(s) => RangeUnit::try_from(s),
                    _ => Ok(RangeUnit::Minutes)
                })
                .unwrap_or(Ok(RangeUnit::Minutes));

            return Ok((Some(Range(int, unit.unwrap())), tokens));
        }
    }
    Ok((None, tokens))
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
    fn invalid_expression() {
        let tokens = tokenize("[ meta.tag != \"boo\"]").unwrap();
        let expr = parse_expression(tokens.as_slice()).unwrap_err();

        assert_eq!(expr, ParseError::MalformedExpression);
    }

    #[test]
    fn empty_expression() {
        let tokens = tokenize("{}").unwrap();
        let expr = parse_expression(tokens.as_slice()).unwrap();

        assert_eq!(expr.filters.len(), 0);
    }

    #[test]
    fn empty_input() {
        let tokens = tokenize("").unwrap();
        let expr = parse_expression(tokens.as_slice()).unwrap_err();

        assert_eq!(expr, ParseError::MalformedExpression);
    }

    #[test]
    fn token_matching() {
        let tokens = tokenize("{ meta.focal_length }").unwrap();
        let tokens = match_token(tokens.as_slice(), Matcher::Exact(Terminal::CurlyOpen));
        assert!(tokens.is_some());

        let tokens = match_token(tokens.unwrap().1, Matcher::Path);
        let Token(terminal, _start, _end) = tokens.unwrap().0;
        assert_eq!(terminal, &Terminal::Path("meta.focal_length"));

        let tokens = match_token(tokens.unwrap().1, Matcher::Exact(Terminal::CurlyClose));
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
        assert!(!filter.op_negative);
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
        assert!(filter.op_negative);
    }

    #[test]
    fn multiple_filters() {
        let tokens = tokenize("{ meta.tag != \"favourite\", meta.focal.length=3 }").unwrap();
        let mut expr = parse_expression(tokens.as_slice()).unwrap();

        assert_eq!(expr.filters.len(), 2);

        let second = expr.filters.pop().unwrap();
        assert_eq!(second.path, vec!["meta", "focal", "length"]);
        assert_eq!(second.value, Value::Integer(3));
        assert_eq!(second.op, Operator::Equal);
        assert!(!second.op_negative);

        let first = expr.filters.pop().unwrap();
        assert_eq!(first.path, vec!["meta", "tag"]);
        assert_eq!(first.value, Value::String("favourite".to_string()));
        assert_eq!(first.op, Operator::Equal);
        assert!(first.op_negative);
    }

    #[test]
    fn multiple_filters_invalid_separator() {
        let tokens = tokenize("{ meta.tag != \"favourite\"; meta.focal_length=3.2 }").unwrap();
        let expr = parse_expression(tokens.as_slice()).unwrap_err();
        assert_eq!(expr, ParseError::MalformedFilter(ErrorOffset(25)));
    }

    #[test]
    fn invalid_filter_operator() {
        let tokens = tokenize("{ meta.tag !+ \"wrong\" }").unwrap();
        let expr = parse_expression(tokens.as_slice()).unwrap_err();

        assert_eq!(
            expr,
            ParseError::MalformedFilterOperator(ErrorOffset(12), "meta.tag".to_string())
        );
    }

    #[test]
    fn invalid_filter_value() {
        let tokens = tokenize("{ meta.tag != invalid }").unwrap();
        let expr = parse_expression(tokens.as_slice()).unwrap_err();

        assert_eq!(
            expr,
            ParseError::MalformedFilterValue(ErrorOffset(13), "meta.tag".to_string())
        );
    }

    #[test]
    fn negative_filter_value() {
        let tokens = tokenize("{ meta.tag = -1 }").unwrap();
        let mut expr = parse_expression(tokens.as_slice()).unwrap();

        let first = expr.filters.pop().unwrap();
        assert_eq!(first.path, vec!["meta", "tag"]);
        assert_eq!(first.value, Value::Integer(-1));
    }

    #[test]
    fn missing_filter_value() {
        let tokens = tokenize("{ meta.tag= }").unwrap();
        let expr = parse_expression(tokens.as_slice()).unwrap_err();

        assert_eq!(
            expr,
            ParseError::MalformedFilterValue(ErrorOffset(11), "meta.tag".to_string())
        );
    }

    #[test]
    fn invalid_filter_value_other() {
        let tokens = tokenize("{ meta.tag != \"boo\", meta.focal=invalid}").unwrap();
        let expr = parse_expression(tokens.as_slice()).unwrap_err();

        assert_eq!(
            expr,
            ParseError::MalformedFilterValue(ErrorOffset(32), "meta.focal".to_string())
        );
    }

    #[test]
    fn invalid_path_filter_other() {
        let tokens = tokenize("{ meta.tag != \"boo\", +=true}").unwrap();
        let expr = parse_expression(tokens.as_slice()).unwrap_err();

        assert_eq!(expr, ParseError::MalformedFilter(ErrorOffset(21)));
    }

    #[test]
    fn valid_range() {
        let tokens = tokenize("[10d]").unwrap();
        let (opt, _) = parse_range(tokens.as_slice()).unwrap();
        let range = opt.unwrap();

        assert_eq!(range.0, 10);
        assert_eq!(range.1, RangeUnit::Days);
    }

    #[test]
    fn default_range() {
        let tokens = tokenize("[10]").unwrap();
        let (opt, _) = parse_range(tokens.as_slice()).unwrap();
        let range = opt.unwrap();

        assert_eq!(range.0, 10);
        assert_eq!(range.1, RangeUnit::Minutes);
    }

    #[test]
    fn invalid_range() {
        let tokens = tokenize("  (10)").unwrap();
        let (opt, _) = parse_range(tokens.as_slice()).unwrap();

        assert!(opt.is_none())
    }

    #[test]
    fn invalid_range_value() {
        let tokens = tokenize("[none]").unwrap();
        let result = parse_range(tokens.as_slice()).unwrap_err();

        assert_eq!(result, ParseError::MalformedRangeValue(ErrorOffset(5)));
    }

    #[test]
    fn invalid_range_unit() {
        let tokens = tokenize("[10none]").unwrap();
        let result = parse_range(tokens.as_slice()).unwrap_err();

        assert_eq!(result, ParseError::UnknownRangeUnit);
    }
}

use std::fmt::Display;

use thiserror::Error;

use crate::lexer::{Term, Token};

/// Parser rules:
///
/// query    ->  filter ('|' filter)* range?
/// filter   ->  CURLY_OPEN cond (COMMA cond)* CURLY_CLOSE
/// cond     ->  PATH op value
/// op       ->  '!'? ( EQ | CONTAINS )
/// value    ->  scalar | array
/// scalar   ->  STRING | BOOL | numeric
/// numeric  ->  INTEGER | FLOAT
/// array    ->  SQARE_OPEN value ( COMMA value )* SQARE_CLOSE
/// range    ->  SQARE_OPEN duration SQUARE_CLOSE
/// duration ->  INTEGER unit
/// unit     ->  "ms" | "s" | "m" | "h" | "d" | "w" | "mo" | "y"

#[derive(Debug, PartialEq)]
pub struct Query<'a> {
    pub filters: Vec<Filter<'a>>,
    pub range: Option<Range>,
}

#[derive(Debug, PartialEq)]
pub struct Filter<'a> {
    pub conds: Vec<Cond<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct Cond<'a> {
    pub path: Vec<&'a str>, // property path split by dot
    pub value: Value,
    pub op: Operator,
    pub op_negative: bool,
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    Equal,
    Contains,
}

#[derive(Debug, PartialEq)]
pub enum Value {
    Scalar(Scalar),
    Array(Vec<Scalar>),
}

#[derive(Debug, PartialEq)]
pub enum Scalar {
    String(String),
    Integer(i32),
    Float(f32),
    Bool(bool),
}

#[allow(dead_code)]
pub enum Matcher<'a> {
    Exact(Term<'a>),
    Path,
    Operator,
    Negation,
    Scalar,
    Integer,
    Float,
    String,
    Numeric,
    Bool,
}

#[derive(Debug, PartialEq)]
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
    #[error("malformed query")]
    MalformedQuery,
    #[error("malformed filter")]
    MalformedFilter(ErrorOffset),
    #[error("invalid scalar value (one of: integer, float, bool, string expected)")]
    InvalidScalarValue(ErrorOffset),
    #[error("invalid array value (comma-separated scalars expected)")]
    InvalidArrayValue(ErrorOffset),
    #[error("invalid value type (scalar values expected)")]
    InvalidFilterValueType(ErrorOffset),
    #[error("invalid filter operator (one of: =, !=, ~, ~= expected)")]
    InvalidFilterOperator(ErrorOffset),
    #[error("invalid range (range in square brackets expected)")]
    InvalidRange(ErrorOffset),
    #[error("invalid range value (integer expected)")]
    InvalidRangeValue(ErrorOffset),
    #[error("invalid range unit (one of: ms, s, m, h, d, w, mo, y expected)")]
    InvalidRangeUnit,
}

impl<'a> TryFrom<&Token<'a>> for Operator {
    type Error = ParseError;

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match value.0 {
            Term::Equal => Ok(Self::Equal),
            Term::Contains => Ok(Self::Contains),
            _ => Err(ParseError::InvalidFilterOperator(ErrorOffset(value.1))),
        }
    }
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
            _ => Err(ParseError::InvalidRangeUnit),
        }
    }
}

impl<'a> TryFrom<&Token<'a>> for Scalar {
    type Error = ParseError;

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match value.0 {
            Term::String(s) => Ok(Self::String(s.to_string())),
            Term::Integer(i) => Ok(Self::Integer(i)),
            Term::Float(f) => Ok(Self::Float(f)),
            Term::Bool(b) => Ok(Self::Bool(b)),
            _ => Err(ParseError::InvalidFilterValueType(ErrorOffset(value.1))),
        }
    }
}

impl Display for Scalar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Scalar::Integer(i) => write!(f, "{i}"),
            Scalar::Float(l) => write!(f, "{l}"),
            Scalar::Bool(b) => write!(f, "{b}"),
            Scalar::String(s) => write!(f, "'{s}'"),
        }
    }
}

impl From<Scalar> for Value {
    fn from(value: Scalar) -> Self {
        Self::Scalar(value)
    }
}

impl From<Vec<Scalar>> for Value {
    fn from(value: Vec<Scalar>) -> Self {
        Self::Array(value)
    }
}

impl Value {
    pub fn patternize(self, op: &Operator) -> Self {
        match self {
            Self::Scalar(Scalar::String(s)) if *op == Operator::Contains => {
                Self::Scalar(Scalar::String(format!("%{s}%")))
            }
            other => other,
        }
    }
}

impl Range {
    pub fn to_query_string(&self) -> String {
        let unit = match self.1 {
            RangeUnit::Milliseconds => "milliseconds",
            RangeUnit::Seconds => "seconds",
            RangeUnit::Minutes => "minutes",
            RangeUnit::Hours => "hours",
            RangeUnit::Days => "days",
            RangeUnit::Weeks => "weeks",
            RangeUnit::Months => "months",
            RangeUnit::Years => "years",
        };
        format!("{} {}", self.0, unit)
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
                Matcher::Scalar,
                Term::String(_) | Term::Integer(_) | Term::Float(_) | Term::Bool(_),
            ) => Some(token),
            (Matcher::Path, Term::Path(_)) => Some(token),
            (Matcher::Float, Term::Float(_)) => Some(token),
            (Matcher::Bool, Term::Bool(_)) => Some(token),
            (Matcher::Integer, Term::Integer(_)) => Some(token),
            (Matcher::Numeric, Term::Integer(_) | Term::Float(_)) => Some(token),
            (Matcher::Operator, Term::Equal | Term::Contains) => Some(token),
            (Matcher::Negation, Term::Exclamation) => Some(token),
            _ => None,
        };
        if let Some(matched) = result {
            return Some((matched, &tokens[1..]));
        }
    }
    None
}

/// A top-level parsing function returning query consisting of vector
/// of `Filter`s and optional `Range`.
///
/// Returns a `ParseError` in case of parsing problems.
pub fn parse_query<'a>(tokens: &'a [Token]) -> Result<Query<'a>, ParseError> {
    let mut filters = Vec::with_capacity(3);
    let mut tokens_slice = tokens;

    while let (Some(filter), tokens) = parse_filter(tokens_slice)? {
        filters.push(filter);

        // OR operator found? There might be the other filter coming next.
        if let Some((_, tokens)) = match_token(tokens, Matcher::Exact(Term::Or)) {
            tokens_slice = tokens;
        } else {
            let (range, _) = parse_range(tokens)?;

            // if there are still tokens to digest and it's not `Range`
            // coming next then clearly something is wrong.
            return if !tokens.is_empty() && range.is_none() {
                Err(ParseError::InvalidFilterOperator(ErrorOffset(tokens[0].1)))
            } else {
                Ok(Query { filters, range })
            }
        }
    }
    Err(ParseError::MalformedQuery)
}

pub fn parse_filter<'a>(
    tokens: &'a [Token],
) -> Result<(Option<Filter<'a>>, &'a [Token<'a>]), ParseError> {
    if let Some((_, tokens)) = match_token(tokens, Matcher::Exact(Term::CurlyOpen)) {
        let (conds, tokens) = parse_conds(tokens)?;

        // curly closing brace = end of filters
        return if let Some((_, tokens)) = match_token(tokens, Matcher::Exact(Term::CurlyClose)) {
            Ok((Some(Filter { conds }), tokens))
        } else {
            Err(ParseError::MalformedFilter(ErrorOffset(tokens[0].2)))
        };
    }
    // entire filter starts with wrong token
    Err(ParseError::MalformedFilter(ErrorOffset(
        tokens.first().map(|t| t.1).unwrap_or(0),
    )))
}

fn parse_conds<'a>(tokens: &'a [Token]) -> Result<(Vec<Cond<'a>>, &'a [Token<'a>]), ParseError> {
    let mut conds = Vec::<Cond>::new();
    let mut tokens_slice = tokens;

    while let Some((Token(Term::Path(id), _, _), tokens)) = match_token(tokens_slice, Matcher::Path)
    {
        let negative = match_token(tokens, Matcher::Negation).is_some();
        let tokens = if negative { &tokens[1..] } else { tokens };
        let (op, tokens) = match_token(tokens, Matcher::Operator)
            .ok_or_else(|| ParseError::InvalidFilterOperator(ErrorOffset(tokens[0].1)))?;
        let (value, tokens) = parse_value(tokens)?;

        tokens_slice = tokens;
        conds.push(Cond {
            path: id.to_owned().split('.').collect::<Vec<_>>(),
            op: Operator::try_from(op)?,
            op_negative: negative,
            value,
        });

        // Comma found? There might be the other condition coming next.
        if let Some((_, tokens)) = match_token(tokens_slice, Matcher::Exact(Term::Comma)) {
            tokens_slice = tokens;
        } else {
            break;
        }
    }
    Ok((conds, tokens_slice))
}

fn parse_value<'a>(tokens: &'a [Token]) -> Result<(Value, &'a [Token<'a>]), ParseError> {
    if let (Some(array), tokens) = parse_array(tokens)? {
        return Ok((Value::from(array), tokens));
    }
    let (scalar, tokens) = match_token(tokens, Matcher::Scalar)
        .ok_or_else(|| ParseError::InvalidScalarValue(ErrorOffset(tokens[0].1)))?;

    Ok((Value::from(Scalar::try_from(scalar)?), tokens))
}

fn parse_array<'a>(
    tokens: &'a [Token],
) -> Result<(Option<Vec<Scalar>>, &'a [Token<'a>]), ParseError> {
    if let Some((_, tokens)) = match_token(tokens, Matcher::Exact(Term::SquareOpen)) {
        let mut values = Vec::new();
        let mut tokens_slice = tokens;

        while let Some((val, tokens)) = match_token(tokens_slice, Matcher::Scalar) {
            values.push(Scalar::try_from(val)?);
            tokens_slice = tokens;

            // Comma found? There might be the other values coming next.
            if let Some((_, tokens)) = match_token(tokens_slice, Matcher::Exact(Term::Comma)) {
                tokens_slice = tokens;
            } else {
                break;
            }
        }
        let (_, tokens) = match_token(tokens_slice, Matcher::Exact(Term::SquareClose))
            .ok_or_else(|| ParseError::InvalidArrayValue(ErrorOffset(tokens_slice[0].1)))?;

        return Ok((Some(values), tokens));
    }
    Ok((None, tokens))
}

fn parse_range<'a>(tokens: &'a [Token]) -> Result<(Option<Range>, &'a [Token<'a>]), ParseError> {
    if let Some((_, tokens)) = match_token(tokens, Matcher::Exact(Term::SquareOpen)) {
        let (token, tokens) = match_token(tokens, Matcher::Integer)
            .ok_or_else(|| ParseError::InvalidRangeValue(ErrorOffset(tokens[0].2 + 1)))?;

        if let Term::Integer(int) = token.0 {
            let unit = match_token(tokens, Matcher::Path)
                .map(|(token, _)| match token.0 {
                    Term::Path(s) => RangeUnit::try_from(s),
                    _ => Ok(RangeUnit::Minutes),
                })
                .unwrap_or(Ok(RangeUnit::Minutes))?;

            return Ok((Some(Range(int, unit)), tokens));
        }
    }
    Ok((None, tokens))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::tokenize;

    #[test]
    fn invalid_query() {
        let tokens = tokenize("< meta.tag != \"boo\">").unwrap();
        let query = parse_query(tokens.as_slice()).unwrap_err();

        assert_eq!(query, ParseError::MalformedFilter(ErrorOffset(0)));
    }

    #[test]
    fn empty_query() {
        let tokens = tokenize("").unwrap();
        let query = parse_query(tokens.as_slice()).unwrap_err();

        assert_eq!(query, ParseError::MalformedFilter(ErrorOffset(0)));
    }

    #[test]
    fn empty_filter() {
        let tokens = tokenize("{}").unwrap();
        let query = parse_query(tokens.as_slice()).unwrap();

        assert_eq!(
            query,
            Query {
                filters: vec![Filter { conds: vec![] }],
                range: None
            }
        );
    }

    #[test]
    fn token_matching() {
        let tokens = tokenize("{ meta.focal_length }").unwrap();
        let tokens = match_token(tokens.as_slice(), Matcher::Exact(Term::CurlyOpen));
        assert!(tokens.is_some());

        let tokens = match_token(tokens.unwrap().1, Matcher::Path);
        let Token(terminal, _start, _end) = tokens.unwrap().0;
        assert_eq!(terminal, &Term::Path("meta.focal_length"));

        let tokens = match_token(tokens.unwrap().1, Matcher::Exact(Term::CurlyClose));
        assert!(tokens.is_some());
    }

    #[test]
    fn parse_cond_simple() {
        let tokens = tokenize("{ meta.focal_length=2.3 }").unwrap();
        let query = parse_query(tokens.as_slice()).unwrap();
        let filter = query.filters.first().unwrap();
        assert_eq!(filter.conds.len(), 1);

        let cond = filter.conds.first().unwrap();
        assert_eq!(cond.path, vec!["meta", "focal_length"]);
        assert_eq!(cond.op, Operator::Equal);
        assert_eq!(cond.value, Value::from(Scalar::Float(2.3)));
        assert!(!cond.op_negative);
    }

    #[test]
    fn parse_cond_negated() {
        let tokens = tokenize("{ meta.tag !~ \"favourite\" }").unwrap();
        let query = parse_query(tokens.as_slice()).unwrap();
        let filter = query.filters.first().unwrap();
        assert_eq!(filter.conds.len(), 1);

        let cond = filter.conds.first().unwrap();
        assert_eq!(cond.path, vec!["meta", "tag"]);
        assert_eq!(cond.op, Operator::Contains);
        assert_eq!(
            cond.value,
            Value::from(Scalar::String("favourite".to_string()))
        );
        assert!(cond.op_negative);
    }

    #[test]
    fn multiple_conds() {
        let tokens = tokenize("{ meta.tag != \"favourite\", meta.focal.length=3 }").unwrap();
        let query = parse_query(tokens.as_slice()).unwrap();
        let filter = query.filters.first().unwrap();
        assert_eq!(filter.conds.len(), 2);

        let first = &filter.conds[0];
        assert_eq!(first.path, vec!["meta", "tag"]);
        assert_eq!(
            first.value,
            Value::from(Scalar::String("favourite".to_string()))
        );
        assert_eq!(first.op, Operator::Equal);
        assert!(first.op_negative);

        let second = &filter.conds[1];
        assert_eq!(second.path, vec!["meta", "focal", "length"]);
        assert_eq!(second.value, Value::from(Scalar::Integer(3)));
        assert_eq!(second.op, Operator::Equal);
        assert!(!second.op_negative);
    }

    #[test]
    fn multiple_filters() {
        let tokens = tokenize("{ tag != \"favourite\" } | { focal_length=3.2 }").unwrap();
        let query = parse_query(tokens.as_slice()).unwrap();
        let filters = query.filters;

        assert_eq!(filters.len(), 2);

        let filter_1 = &filters[0];
        let cond_1 = &filter_1.conds[0];
        assert_eq!(cond_1.path[0], "tag");
        assert_eq!(
            cond_1.value,
            Value::from(Scalar::String("favourite".to_string()))
        );

        let filter_2 = &filters[1];
        let cond_2 = &filter_2.conds[0];
        assert_eq!(cond_2.path[0], "focal_length");
        assert_eq!(cond_2.value, Value::from(Scalar::Float(3.2)));
    }

    #[test]
    fn multiple_conds_with_invalid_separator() {
        let tokens = tokenize("{ meta.tag != \"favourite\"; meta.focal_length=3.2 }").unwrap();
        let query = parse_query(tokens.as_slice()).unwrap_err();
        assert_eq!(query, ParseError::MalformedFilter(ErrorOffset(25)));
    }

    #[test]
    fn filter_with_invalid_operator() {
        let tokens = tokenize("{ meta.tag !+ \"wrong\" }").unwrap();
        let query = parse_query(tokens.as_slice()).unwrap_err();

        assert_eq!(query, ParseError::InvalidFilterOperator(ErrorOffset(12)));
    }

    #[test]
    fn filter_with_invalid_value() {
        let tokens = tokenize("{ meta.tag != invalid }").unwrap();
        let query = parse_query(tokens.as_slice()).unwrap_err();

        assert_eq!(query, ParseError::InvalidScalarValue(ErrorOffset(14)));
    }

    #[test]
    fn filter_with_negative_value() {
        let tokens = tokenize("{ meta.tag = -1 }").unwrap();
        let query = parse_query(tokens.as_slice()).unwrap();
        let filter = query.filters.first().unwrap();
        let first = filter.conds.first().unwrap();

        assert_eq!(first.path, vec!["meta", "tag"]);
        assert_eq!(first.value, Value::from(Scalar::Integer(-1)));
    }

    #[test]
    fn filter_with_array_value() {
        let tokens = tokenize("{ meta.tag = [1,2,3] }").unwrap();
        let query = parse_query(tokens.as_slice()).unwrap();
        let filter = query.filters.first().unwrap();
        let first = filter.conds.first().unwrap();

        assert_eq!(first.path, vec!["meta", "tag"]);
        assert_eq!(
            first.value,
            vec![Scalar::Integer(1), Scalar::Integer(2), Scalar::Integer(3)].into()
        );
    }

    #[test]
    fn filter_with_empty_array_value() {
        let tokens = tokenize("{ meta.tag = [] }").unwrap();
        let query = parse_query(tokens.as_slice()).unwrap();
        let filter = query.filters.first().unwrap();
        let first = filter.conds.first().unwrap();

        assert_eq!(first.path, vec!["meta", "tag"]);
        assert_eq!(first.value, Value::from(vec![]));
    }

    #[test]
    fn filter_with_array_missing_separator() {
        let tokens = tokenize("{ meta.tag = [1 2] }").unwrap();
        let query = parse_query(tokens.as_slice()).unwrap_err();

        assert_eq!(query, ParseError::InvalidArrayValue(ErrorOffset(16)));
    }

    #[test]
    fn filter_with_array_invalid_value() {
        let tokens = tokenize("{ meta.tag = [1, invalid] }").unwrap();
        let query = parse_query(tokens.as_slice()).unwrap_err();

        assert_eq!(query, ParseError::InvalidArrayValue(ErrorOffset(17)));
    }

    #[test]
    fn filter_with_missing_value() {
        let tokens = tokenize("{ meta.tag= }").unwrap();
        let query = parse_query(tokens.as_slice()).unwrap_err();

        assert_eq!(query, ParseError::InvalidScalarValue(ErrorOffset(12)));
    }

    #[test]
    fn filter_with_invalid_value_other() {
        let tokens = tokenize("{ meta.tag != \"boo\", meta.focal=invalid}").unwrap();
        let query = parse_query(tokens.as_slice()).unwrap_err();

        assert_eq!(query, ParseError::InvalidScalarValue(ErrorOffset(32)));
    }

    #[test]
    fn filter_with_invalid_path_other() {
        let tokens = tokenize("{ meta.tag != \"boo\", +=true}").unwrap();
        let query = parse_query(tokens.as_slice()).unwrap_err();

        assert_eq!(query, ParseError::MalformedFilter(ErrorOffset(21)));
    }

    #[test]
    fn multiple_filters_with_wrong_operator() {
        let tokens = tokenize("{ tag != \"favourite\" } & { focal_length=3.2 }").unwrap();
        let query = parse_query(tokens.as_slice()).unwrap_err();

        assert_eq!(query, ParseError::InvalidFilterOperator(ErrorOffset(23)));
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
    fn default_range_unit() {
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

        assert_eq!(result, ParseError::InvalidRangeValue(ErrorOffset(5)));
    }

    #[test]
    fn invalid_range_unit() {
        let tokens = tokenize("[10none]").unwrap();
        let result = parse_range(tokens.as_slice()).unwrap_err();

        assert_eq!(result, ParseError::InvalidRangeUnit);
    }

    #[test]
    fn range_with_filter() {
        let tokens = tokenize("{meta.tag=\"favourite\"}[10w]").unwrap();
        let query = parse_query(tokens.as_slice()).unwrap();

        assert_eq!(query.range.unwrap(), Range(10, RangeUnit::Weeks));
    }
}

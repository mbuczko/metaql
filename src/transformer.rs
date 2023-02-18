use std::collections::HashMap;

use crate::{
    lexer::tokenize,
    parser::{parse_query, Operator, ParseError, Scalar, Value},
};

#[derive(Debug, PartialEq)]
pub struct Statement {
    pub query: String,
    pub params: Vec<Value>,
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum Column<'a> {
    Filter(&'a str),
    Range,
}

type Alias = &'static str;
type Columns<'a> = HashMap<Column<'a>, Alias>;

#[macro_export]
macro_rules! columns {
    ($( $key: expr => $val: expr ),*) => {{
         let mut map = ::std::collections::HashMap::new();
         $( map.insert($key, $val); )*
         map
    }}
}

impl Statement {
    fn new(filter: String, range: String, params: Vec<Value>) -> Self {
        let mut stmt = Vec::with_capacity(2);
        if !filter.is_empty() {
            stmt.push(format!("({filter})"));
        }
        if !range.is_empty() {
            stmt.push(range);
        }

        let query = stmt.join(" AND ");
        if query.is_empty() {
            Self::default()
        } else {
            Self { query, params }
        }
    }
}

impl Default for Statement {
    fn default() -> Self {
        Self {
            query: "1=1".to_string(),
            params: vec![],
        }
    }
}

pub fn transform<I: AsRef<str>>(
    input: I,
    columns: Option<Columns>,
    start_params_index: u8,
) -> Result<Statement, ParseError> {
    let tokens = tokenize(input.as_ref()).map_err(ParseError::InvalidTokens)?;
    let query = parse_query(tokens.as_slice())?;

    let mut range_fragment = String::new();
    let mut filters_fragments = Vec::with_capacity(3);
    let mut filters_params = Vec::with_capacity(5);
    let mut params_idx = start_params_index;

    for group in query.filter.groups {
        let mut filter_fragment = String::new();
        for cond in group.matchers {
            let column = columns
                .as_ref()
                .map(|c| c.get(&Column::Filter(cond.path.first().unwrap())))
                .unwrap_or(None);
            if !filter_fragment.is_empty() {
                filter_fragment.push_str(" AND ");
            }
            filter_fragment.push_str(path_to_condition_lhs(cond.path, column).as_str());
            filter_fragment.push_str(
                op_to_condition_operator(&cond.op, cond.op_negative, &cond.value).as_str(),
            );
            filter_fragment.push_str(
                value_to_condition_rhs(&cond.op, cond.op_negative, &cond.value, params_idx)
                    .as_str(),
            );

            filters_params.push(cond.value.patternize(&cond.op));
            params_idx += 1;
        }
        if !filter_fragment.is_empty() {
            filters_fragments.push(filter_fragment);
        }
    }
    if let Some(range) = query.range {
        let columns = columns.expect("Range query condition requires column definition");
        let range_column = columns.get(&Column::Range).unwrap();

        range_fragment = format!(
            "{} >= now() - INTERVAL '{}'",
            range_column,
            range.to_query_string()
        );
    }
    Ok(Statement::new(
        filters_fragments.join(" OR "),
        range_fragment,
        filters_params,
    ))
}

fn op_to_condition_operator(op: &Operator, negative: bool, value: &Value) -> String {
    match (op, value) {
        (Operator::Contains, Value::Scalar(Scalar::String(_))) => {
            String::from(if negative { " NOT LIKE " } else { " LIKE " })
        }
        _ => String::from(if negative { "!=" } else { "=" }),
    }
}

fn path_to_condition_lhs(path: Vec<&str>, alias: Option<&Alias>) -> String {
    let len = path.len();
    let mut output = String::new();
    for (pos, p) in path.into_iter().enumerate() {
        if pos == len - 1 && len > 1 {
            output.push_str(format!("->>'{p}'").as_str());
        } else if pos > 0 {
            output.push_str(format!("->'{p}'").as_str());
        } else {
            output.push_str(alias.unwrap_or(&p));
        }
    }
    output
}

fn value_to_condition_rhs(op: &Operator, op_negative: bool, value: &Value, idx: u8) -> String {
    match value {
        Value::Array(_) if *op == Operator::Contains => {
            if op_negative {
                format!("ALL(${idx})")
            } else {
                format!("ANY(${idx})")
            }
        }
        _ => format!("${idx}"),
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        parser::Scalar,
        transformer::{Column, Statement},
    };

    use super::{op_to_condition_operator, path_to_condition_lhs, transform};

    #[test]
    fn json_query_path() {
        assert_eq!(path_to_condition_lhs(vec!["matata"], None), "matata");
        assert_eq!(
            path_to_condition_lhs(vec!["kuna", "matata"], None),
            "kuna->>'matata'"
        );
        assert_eq!(
            path_to_condition_lhs(vec!["a", "kuna", "matata"], None),
            "a->'kuna'->>'matata'"
        );
    }

    #[test]
    fn json_query_path_with_aliases() {
        let columns = columns!(
            Column::Filter("matata") => "ma.tata",
            Column::Filter("makuna") => "ma.kuna"
        );
        assert_eq!(
            path_to_condition_lhs(vec!["matata"], columns.get(&Column::Filter("matata"))),
            "ma.tata"
        );
        assert_eq!(
            path_to_condition_lhs(
                vec!["makuna", "kuna", "tata"],
                columns.get(&Column::Filter("makuna"))
            ),
            "ma.kuna->'kuna'->>'tata'"
        );
    }

    #[test]
    fn query_operator() {
        let scalar_int = Scalar::Integer(1).into();
        let scalar_str = Scalar::String(String::from("foo")).into();
        let array = vec![Scalar::Integer(2)].into();

        assert_eq!(
            op_to_condition_operator(&crate::parser::Operator::Equal, false, &scalar_int),
            "="
        );
        assert_eq!(
            op_to_condition_operator(&crate::parser::Operator::Equal, true, &scalar_int),
            "!="
        );
        assert_eq!(
            op_to_condition_operator(&crate::parser::Operator::Contains, false, &scalar_str),
            " LIKE "
        );
        assert_eq!(
            op_to_condition_operator(&crate::parser::Operator::Contains, true, &scalar_str),
            " NOT LIKE "
        );
        assert_eq!(
            op_to_condition_operator(&crate::parser::Operator::Contains, false, &array),
            "="
        );
        assert_eq!(
            op_to_condition_operator(&crate::parser::Operator::Contains, true, &array),
            "!="
        );
    }
    #[test]
    fn no_filter_no_range() {
        let q = transform("{ }", Some(columns!(Column::Range => "created_at")), 1);
        assert_eq!(
            q.unwrap(),
            Statement {
                query: "1=1".to_string(),
                params: vec![]
            }
        );
    }

    #[test]
    fn no_filter_with_range() {
        let q = transform(
            "{ }[10ms]",
            Some(columns!(Column::Range => "created_at")),
            1,
        );
        assert_eq!(
            q.unwrap(),
            Statement {
                query: "created_at >= now() - INTERVAL '10 milliseconds'".to_string(),
                params: vec![]
            }
        );
    }

    #[test]
    fn simple_filter() {
        let q = transform("{meta.focal_length=32}", None, 1);
        assert_eq!(
            q.unwrap(),
            Statement {
                query: "(meta->>'focal_length'=$1)".to_string(),
                params: vec![Scalar::Integer(32).into()]
            }
        );
    }

    #[test]
    fn nested_filter() {
        let q = transform("{meta.focal.length=18.5}", None, 1);
        assert_eq!(
            q.unwrap(),
            Statement {
                query: "(meta->'focal'->>'length'=$1)".to_string(),
                params: vec![Scalar::Float(18.5).into()]
            }
        );
    }

    #[test]
    fn pattern_string_filter() {
        let q = transform("{meta.description !~ \"dog\"}", None, 1);
        assert_eq!(
            q.unwrap(),
            Statement {
                query: "(meta->>'description' NOT LIKE $1)".to_string(),
                params: vec![Scalar::String("%dog%".to_string()).into()]
            }
        );
    }

    #[test]
    fn exact_string_filter() {
        let q = transform("{meta.description = \"dog\"}", None, 1);
        assert_eq!(
            q.unwrap(),
            Statement {
                query: "(meta->>'description'=$1)".to_string(),
                params: vec![Scalar::String("dog".to_string()).into()]
            }
        );
    }

    #[test]
    fn multi_cond_filter_query() {
        let q = transform(
            "{favourite.tag ~ \"cats\", meta.focal.length=18.5}",
            None,
            1,
        );
        assert_eq!(
            q.unwrap(),
            Statement {
                query: "(favourite->>'tag' LIKE $1 AND meta->'focal'->>'length'=$2)".to_string(),
                params: vec![
                    Scalar::String("%cats%".to_string()).into(),
                    Scalar::Float(18.5).into()
                ]
            }
        );
    }

    #[test]
    fn aliased_filter_query() {
        let q = transform(
            "{favourite.tag = \"cats\"}",
            Some(columns!(Column::Filter("favourite") => "u.favourite")),
            1,
        );
        assert_eq!(
            q.unwrap(),
            Statement {
                query: "(u.favourite->>'tag'=$1)".to_string(),
                params: vec![Scalar::String("cats".to_string()).into()]
            }
        );
    }

    #[test]
    fn multi_filter_query() {
        let q = transform(
            "{favourite.tag = \"cats\", folder = \"pets\" | offset = 1, version = 2}",
            Some(columns!(Column::Filter("favourite") => "u.favourite")),
            1,
        );
        assert_eq!(
            q.unwrap(),
            Statement {
                query: "(u.favourite->>'tag'=$1 AND folder=$2 OR offset=$3 AND version=$4)"
                    .to_string(),
                params: vec![
                    Scalar::String("cats".to_string()).into(),
                    Scalar::String("pets".to_string()).into(),
                    Scalar::Integer(1).into(),
                    Scalar::Integer(2).into(),
                ]
            }
        );
    }

    #[test]
    fn multi_filter_query_with_range() {
        let q = transform(
            "{favourite.tag ~ \"cats\" | meta.focal.length=18.5} [10d]",
            Some(columns!(
                Column::Range => "created_at"
            )),
            1,
        );

        assert_eq!(
            q.unwrap(),
            Statement {
                query: "(favourite->>'tag' LIKE $1 OR meta->'focal'->>'length'=$2) AND created_at >= now() - INTERVAL '10 days'".to_string(),
                params: vec![
                    Scalar::String("%cats%".to_string()).into(),
                    Scalar::Float(18.5).into()
                ]
            }
        );
    }

    #[test]
    fn multi_filter_query_with_empty_filter() {
        let q = transform(
            "{favourite.tag = \"cats\", folder = \"pets\" | }",
            Some(columns!(Column::Filter("favourite") => "u.favourite")),
            1,
        );
        assert_eq!(
            q.unwrap(),
            Statement {
                query: "(u.favourite->>'tag'=$1 AND folder=$2)".to_string(),
                params: vec![
                    Scalar::String("cats".to_string()).into(),
                    Scalar::String("pets".to_string()).into(),
                ]
            }
        );
    }

    #[test]
    fn array_filter_contains_value() {
        let q = transform("{favourite.tag ~ [\"cat\", \"dog\"]}", None, 1);
        assert_eq!(
            q.unwrap(),
            Statement {
                query: "(favourite->>'tag'=ANY($1))".to_string(),
                params: vec![vec![
                    Scalar::String("cat".to_string()),
                    Scalar::String("dog".to_string())
                ]
                .into()]
            }
        );
    }

    #[test]
    fn array_filter_not_contains_value() {
        let q = transform("{favourite.tag !~ [\"cat\", \"dog\"]}", None, 1);
        assert_eq!(
            q.unwrap(),
            Statement {
                query: "(favourite->>'tag'!=ALL($1))".to_string(),
                params: vec![vec![
                    Scalar::String("cat".to_string()),
                    Scalar::String("dog".to_string())
                ]
                .into()]
            }
        );
    }

    #[test]
    fn array_filter_equal_value() {
        let q = transform("{favourite.tags = [\"cat\", \"dog\"]}", None, 1);
        assert_eq!(
            q.unwrap(),
            Statement {
                query: "(favourite->>'tags'=$1)".to_string(),
                params: vec![vec![
                    Scalar::String("cat".to_string()),
                    Scalar::String("dog".to_string())
                ]
                .into()]
            }
        );
    }

    #[test]
    fn range_query() {
        let columns = columns!(
            Column::Range => "created_at",
            Column::Filter("favourite") => "u.favourite"
        );
        let q = transform("{favourite.tag = \"cats\"}[10d]", Some(columns), 1);
        assert_eq!(
            q.unwrap(),
            Statement {
                query: "(u.favourite->>'tag'=$1) AND created_at >= now() - INTERVAL '10 days'"
                    .to_string(),
                params: vec![Scalar::String("cats".to_string()).into()]
            }
        );
    }

    #[test]
    #[should_panic]
    fn range_query_without_columns() {
        let q = transform("{favourite.tag = \"cats\"}[10d]", None, 1);
        assert!(q.is_ok());
    }

    #[test]
    #[should_panic]
    fn range_query_without_range_column() {
        let q = transform(
            "{favourite.tag = \"cats\"}[10d]",
            Some(columns!(Column::Filter("favourite") => "u.favourite")),
            1,
        );
        assert!(q.is_ok());
    }
}

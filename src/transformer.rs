use std::{collections::HashMap, error::Error};

use crate::{
    lexer::tokenize,
    parser::{parse_expression, Operator, Value},
};

#[derive(Debug, PartialEq)]
pub struct Query {
    pub stmt: String,
    pub params: Vec<Value>,
}

#[derive(Hash, Eq, PartialEq, Debug)]
pub enum Column<'a> {
    Filter(&'a str),
    Range,
}

type Alias = &'static str;
type Columns<'a> = HashMap<Column<'a>, Alias>;

#[allow(unused_macros)]
macro_rules! columns {
    ($( $key: expr => $val: expr ),*) => {{
         let mut map = ::std::collections::HashMap::new();
         $( map.insert($key, $val); )*
         map
    }}
}

pub fn transform<I: AsRef<str>>(
    input: I,
    columns: Option<Columns>,
) -> Result<Query, Box<dyn Error>> {
    let tokens = tokenize(input.as_ref())?;
    let expr = parse_expression(tokens.as_slice())?;

    let mut f_stmt = String::new();
    let mut r_stmt = String::new();
    let mut params = Vec::with_capacity(5);

    for filter in expr.filters {
        params.push(filter.value.patternize(&filter.op));

        let column = columns
            .as_ref()
            .map(|c| c.get(&Column::Filter(filter.path.first().unwrap())))
            .unwrap_or(None);
        if !f_stmt.is_empty() {
            f_stmt.push_str(" AND ");
        }
        f_stmt.push_str(path_to_condition_lhs(filter.path, column).as_str());
        f_stmt.push_str(op_to_condition_operator(filter.op, filter.op_negative).as_str());
        f_stmt.push('?');
    }
    if let Some(range) = expr.range {
        let columns = columns.expect("Range query condition requires column definition");
        let range_column = columns.get(&Column::Range).unwrap();

        r_stmt = format!(
            "{} >= now() - INTERVAL '{}'",
            range_column,
            range.to_query_string()
        );
    }
    Ok(Query {
        stmt: [ensure_non_empty(&f_stmt), ensure_non_empty(&r_stmt)].join(" AND "),
        params,
    })
}

fn ensure_non_empty(cond: &str) -> &str {
    if cond.is_empty() {
        return "1=1";
    }
    cond
}

fn op_to_condition_operator(op: Operator, negative: bool) -> String {
    match op {
        Operator::Equal => String::from(if negative { "!=" } else { "=" }),
        Operator::Contains => String::from(if negative { " NOT LIKE " } else { " LIKE " }),
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

#[cfg(test)]
mod tests {
    use crate::{
        parser::Value,
        transformer::{Column, Query},
    };

    use super::{op_to_condition_operator, path_to_condition_lhs, transform};

    #[test]
    fn json_query_compose() {
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
    fn json_query_compose_with_aliases() {
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
        assert_eq!(
            op_to_condition_operator(crate::parser::Operator::Equal, false),
            "="
        );
        assert_eq!(
            op_to_condition_operator(crate::parser::Operator::Equal, true),
            "!="
        );
        assert_eq!(
            op_to_condition_operator(crate::parser::Operator::Contains, false),
            " LIKE "
        );
        assert_eq!(
            op_to_condition_operator(crate::parser::Operator::Contains, true),
            " NOT LIKE "
        );
    }

    #[test]
    fn no_filter_no_range() {
        let q = transform("{ }", Some(columns!(Column::Range => "created_at")));
        assert_eq!(
            q.unwrap(),
            Query {
                stmt: "1=1 AND 1=1".to_string(),
                params: vec![]
            }
        );
    }

    #[test]
    fn no_filter_with_range() {
        let q = transform("{ }[10ms]", Some(columns!(Column::Range => "created_at")));
        assert_eq!(
            q.unwrap(),
            Query {
                stmt: "1=1 AND created_at >= now() - INTERVAL '10 milliseconds'".to_string(),
                params: vec![]
            }
        );
    }

    #[test]
    fn simple_filter_to_condition() {
        let q = transform("{meta.focal_length=32}", None);
        assert_eq!(
            q.unwrap(),
            Query {
                stmt: "meta->>'focal_length'=? AND 1=1".to_string(),
                params: vec![Value::Integer(32)]
            }
        );
    }

    #[test]
    fn nested_filter_to_condition() {
        let q = transform("{meta.focal.length=18.5}", None);
        assert_eq!(
            q.unwrap(),
            Query {
                stmt: "meta->'focal'->>'length'=? AND 1=1".to_string(),
                params: vec![Value::Float(18.5)]
            }
        );
    }

    #[test]
    fn pattern_string_filter_to_condition() {
        let q = transform("{meta.description !~ \"dog\"}", None);
        assert_eq!(
            q.unwrap(),
            Query {
                stmt: "meta->>'description' NOT LIKE ? AND 1=1".to_string(),
                params: vec![Value::String("%dog%".to_string())]
            }
        );
    }

    #[test]
    fn exact_string_filter_to_condition() {
        let q = transform("{meta.description = \"dog\"}", None);
        assert_eq!(
            q.unwrap(),
            Query {
                stmt: "meta->>'description'=? AND 1=1".to_string(),
                params: vec![Value::String("dog".to_string())]
            }
        );
    }

    #[test]
    fn multiple_filter_expr_to_query() {
        let q = transform("{favourite.tag ~ \"cats\", meta.focal.length=18.5}", None);
        assert_eq!(
            q.unwrap(),
            Query {
                stmt: "favourite->>'tag' LIKE ? AND meta->'focal'->>'length'=? AND 1=1".to_string(),
                params: vec![Value::String("%cats%".to_string()), Value::Float(18.5)]
            }
        );
    }

    #[test]
    fn aliased_filter_expr_to_query() {
        let q = transform(
            "{favourite.tag = \"cats\"}",
            Some(columns!(Column::Filter("favourite") => "u.favourite")),
        );
        assert_eq!(
            q.unwrap(),
            Query {
                stmt: "u.favourite->>'tag'=? AND 1=1".to_string(),
                params: vec![Value::String("cats".to_string())]
            }
        );
    }

    #[test]
    fn range_expr_to_query() {
        let columns = columns!(
            Column::Range => "created_at",
            Column::Filter("favourite") => "u.favourite"
        );
        let q = transform("{favourite.tag = \"cats\"}[10d]", Some(columns));
        assert_eq!(
            q.unwrap(),
            Query {
                stmt: "u.favourite->>'tag'=? AND created_at >= now() - INTERVAL '10 days'"
                    .to_string(),
                params: vec![Value::String("cats".to_string())]
            }
        );
    }

    #[test]
    #[should_panic]
    fn range_expr_without_columns_to_query() {
        let q = transform("{favourite.tag = \"cats\"}[10d]", None);
        assert!(q.is_ok());
    }

    #[test]
    #[should_panic]
    fn range_expr_without_range_column_to_query() {
        let q = transform(
            "{favourite.tag = \"cats\"}[10d]",
            Some(columns!(Column::Filter("favourite") => "u.favourite")),
        );
        assert!(q.is_ok());
    }
}

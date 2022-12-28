use std::error::Error;

use crate::{
    lexer::tokenize,
    parser::{parse_expression, Operator, Value},
};

#[derive(Debug, PartialEq)]
pub struct Query {
    pub stmt: String,
    pub params: Vec<Value>,
}

pub fn transform<I: AsRef<str>>(
    input: I,
    range_column: Option<&'static str>,
) -> Result<Query, Box<dyn Error>> {
    let tokens = tokenize(input.as_ref())?;
    let expr = parse_expression(tokens.as_slice())?;

    let mut stmt = String::new();
    let mut params = Vec::with_capacity(5);

    for filter in expr.filters {
        params.push(filter.value.patternize(&filter.op));
        if !stmt.is_empty() {
            stmt.push_str(" AND ");
        }
        stmt.push_str(path_to_condition_lhs(filter.path).as_str());
        stmt.push_str(op_to_condition_operator(filter.op, filter.op_negative).as_str());
        stmt.push('?');
    }
    if let Some(range_column) = range_column {
        if let Some(range) = expr.range {
            stmt.push_str(
                format!(
                    " AND {} >= now() - INTERVAL '{}'",
                    range_column,
                    range.to_query_string()
                )
                .as_str(),
            );
        }
    }

    Ok(Query { stmt, params })
}

fn op_to_condition_operator(op: Operator, negative: bool) -> String {
    match op {
        Operator::Equal => String::from(if negative { "!=" } else { "=" }),
        Operator::Contains => String::from(if negative { " NOT LIKE " } else { " LIKE " }),
    }
}

fn path_to_condition_lhs(path: Vec<&str>) -> String {
    if path.len() == 1 {
        return path[0].to_owned();
    }
    let len = path.len();
    let mut output = String::new();
    for (pos, p) in path.into_iter().enumerate() {
        if pos == len - 1 {
            output.push_str(format!("->>'{p}'").as_str());
        } else if pos > 0 {
            output.push_str(format!("->'{p}'").as_str());
        } else {
            output.push_str(p);
        }
    }
    output
}

#[cfg(test)]
mod tests {
    use crate::{transformer::Query, parser::Value};

    use super::{op_to_condition_operator, path_to_condition_lhs, transform};

    #[test]
    fn json_query_compose() {
        assert_eq!(path_to_condition_lhs(vec!["matata"]), "matata");
        assert_eq!(
            path_to_condition_lhs(vec!["kuna", "matata"]),
            "kuna->>'matata'"
        );
        assert_eq!(
            path_to_condition_lhs(vec!["a", "kuna", "matata"]),
            "a->'kuna'->>'matata'"
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
    fn simple_filter_to_condition() {
        let q = transform("{meta.focal_length=32}", None);
        assert_eq!(
            q.unwrap(),
            Query {
                stmt: "meta->>'focal_length'=?".to_string(),
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
                stmt: "meta->'focal'->>'length'=?".to_string(),
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
                stmt: "meta->>'description' NOT LIKE ?".to_string(),
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
                stmt: "meta->>'description'=?".to_string(),
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
                stmt: "favourite->>'tag' LIKE ? AND meta->'focal'->>'length'=?".to_string(),
                params: vec![Value::String("%cats%".to_string()), Value::Float(18.5)]
            }
        );
    }

    #[test]
    fn range_expr_to_query() {
        let q = transform("{favourite.tag = \"cats\"}[10d]", Some("created_at"));
        assert_eq!(
            q.unwrap(),
            Query {
                stmt: "favourite->>'tag'=? AND created_at >= now() - INTERVAL '10 days'".to_string(),
                params: vec![Value::String("cats".to_string())]
            }
        );
    }

}

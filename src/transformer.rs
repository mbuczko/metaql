use std::error::Error;

use crate::{
    lexer::tokenize,
    parser::{parse_expression, Operator},
};

pub struct Query {
    pub conds: String,
}

pub fn transform<I: AsRef<str>>(input: I) -> Result<String, Box<dyn Error>> {
    let tokens = tokenize(input.as_ref())?;
    let expr = parse_expression(tokens.as_slice())?;

    let mut s = String::new();
    for filter in expr.filters {
        let is_contains = filter.op == Operator::Contains;
        if !s.is_empty() {
            s.push_str(" AND ");
        }
        s.push_str(path_to_condition_lhs(filter.path).as_str());
        s.push_str(op_to_condition_operator(filter.op, filter.op_negative).as_str());
        s.push_str(filter.value.to_query_string(is_contains).as_str());
    }
    Ok(s)
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
        let q = transform("{meta.focal_length=32}");
        assert_eq!(q.unwrap(), "meta->>'focal_length'=32".to_string());
    }

    #[test]
    fn nested_filter_to_condition() {
        let q = transform("{meta.focal.length=18.5}");
        assert_eq!(q.unwrap(), "meta->'focal'->>'length'=18.5".to_string());
    }

    #[test]
    fn pattern_string_filter_to_condition() {
        let q = transform("{meta.description !~ \"dog\"}");
        assert_eq!(q.unwrap(), "meta->>'description' NOT LIKE '%dog%'".to_string());
    }

    #[test]
    fn exact_string_filter_to_condition() {
        let q = transform("{meta.description = \"dog\"}");
        assert_eq!(q.unwrap(), "meta->>'description'='dog'".to_string());
    }

    #[test]
    fn multiple_filter_expr_to_query() {
        let q = transform("{favourite.tag ~ \"cats\", meta.focal.length=18.5}");
        assert_eq!(
            q.unwrap(),
            "favourite->>'tag' LIKE '%cats%' AND meta->'focal'->>'length'=18.5".to_string()
        );
    }
}

[![Build Status](https://drone.rodzinks.pl/api/badges/michal/metaql/status.svg?ref=refs/heads/develop)](https://drone.rodzinks.pl/michal/metaql)

The goal of this crate is to parse a simple query language, similar to one used by [Prometheus](https://prometheus.io/docs/prometheus/latest/querying/basics/).
Basing grammar considered here comes down to:

``` antlr
expr     ->  query range?
query    ->  CURLY_OPEN filter (COMMA filter)* CURLY_CLOSE
filter   ->  PATH op value
op       ->  "!"? (EQ | CONTAINS)
value    ->  STRING | BOOL | numeric
numeric  ->  (INTEGER | FLOAT)
range    ->  SQARE_OPEN duration SQUARE_CLOSE
duration ->  INTEGER unit
unit     ->  "ms" | "s" | "m" | "h" | "d" | "w" | "mo" | "y"
```

Query, as simple as:

```
{name ~ "lisa", meta.tags ~ "vacation", meta.focal_length = 18.5}[10d]
```

gets parsed to following AST:

```
Expr { 
  filters: [
    Filter { 
      path: ["name"],
      value: String("lisa"),
      op: Contains,
      op_negative: false
    },
    Filter {
      path: ["meta", "tags"],
      value: String("vacation"),
      op: Contains,
      op_negative: false
    },
    Filter {
      path: ["meta", "focal_length"],
      value: Float(18.5),
      op: Equal,
      op_negative: false
    }
  ],
  range: Some(Range(10, Days))
}
```

Futhermore, AST can be auto-transformed into a proper database WHERE conditions. Conditions specific for Postgres as for now, as nested "paths", like `meta.focal_length` are assumed to be a valid paths within a JSON objects, and are turned into corresponding JSON queries (`meta->>'focal_length` in this case).


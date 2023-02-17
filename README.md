[![Build Status](https://drone.rodzinks.pl/api/badges/michal/metaql/status.svg?ref=refs/heads/develop)](https://drone.rodzinks.pl/michal/metaql)

The goal of this crate is to parse a simple query language, similar to one used by [Prometheus](https://prometheus.io/docs/prometheus/latest/querying/basics/).
Basing grammar considered here comes down to:

``` antlr
query    ->  filter range?
filter   ->  CURLY_OPEN group (OR group)* CURLY_CLOSE
group    ->  matcher (COMMA matcher)*
matcher  ->  PATH op value
op       ->  '!'? ( EQ | CONTAINS )
value    ->  scalar | array
scalar   ->  STRING | BOOL | numeric
numeric  ->  INTEGER | FLOAT
array    ->  SQARE_OPEN value ( COMMA value )* SQARE_CLOSE
range    ->  SQARE_OPEN duration SQUARE_CLOSE
duration ->  INTEGER unit
unit     ->  "ms" | "s" | "m" | "h" | "d" | "w" | "mo" | "y"
```

Query, as simple as:

```
{ nickname   ~   "alice", meta.tags  ~  "vacation", meta.focal_length = 18.5 | meta.camera = "pentax"} [10d]

  ----v---  -v-   --v--   -----------v------------
    path     op   value            matcher
 --------------------------------v------------------------------------------   ----------v----------
                           matchers group                                          matchers group
 ----------------------------------------------v----------------------------------------------------   ---v---
                                             filter                                                     range
```

gets parsed to following AST:

```
Query {
  filter: 
    Filter {
      groups: [
        Group: {
          Matcher {
            path: ["name"],
            value: String("lisa"),
            op: Contains,
            op_negative: false
          },
          Matcher {
            path: ["meta", "tags"],
            value: String("vacation"),
            op: Contains,
            op_negative: false
          },
          Matcher {
            path: ["meta", "focal_length"],
            value: Float(18.5),
            op: Equal,
            op_negative: false
          }
        }
      ]
    }
    
  range:
    Some(Range(10, Days))
}
```

Futhermore, AST can be auto-transformed into a proper database WHERE conditions. Conditions specific for Postgres as for now, as nested "paths", like `meta.focal_length` are assumed to be a valid paths within a JSON objects, and are turned into corresponding JSON queries (`meta->>'focal_length` in this case).


This crate is to parse a simple query language, similar to one used by [Prometheus](https://prometheus.io/docs/prometheus/latest/querying/basics/).

Grammar considered here comes down to:

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

which allows to define queries, as simple as:

```
{ meta.tags ~ "vacation" }
```

or more complicated, like:

```
{ meta.tags ~ "vacation", status = "completed" }
{ meta.tags ~ "vacation", status ~ ["completed", "in-progress"] }
{ meta.focal_length = "35mm" } [10d]
{ meta.focal_length = "35mm" | meta.camera = "pentax" } 
{ meta.tags ~ "vacation", nickname = "alice", meta.focal_length = 18.5 | meta.camera = "pentax"} [10d]
```


Futhermore, AST can be auto-transformed into corresponding WHERE conditions of SQl query.

Note, conditions are very specific for Postgres as for now. For example, nested paths like `meta.focal_length` are assumed to be a valid paths within JSON objects, and as such are turned into postgres-specific JSON queries (`meta->>'focal_length` in this case).

WIP.

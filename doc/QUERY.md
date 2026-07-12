# Org-Supertag Query Language

Org-Supertag's query language is a small S-expression grammar: a handful of
leaf conditions (`tag`, `field`, `term`, `after`, `before`, `between`)
combined with three combinators (`and`, `or`, `not`). It is parsed and
executed by `supertag-query--parse-sexp` / `supertag-query--execute-ast` in
`supertag-services-query.el`. Every operator and example in this document was
checked against that parser directly — nothing here is aspirational syntax.

This guide covers the grammar, the date formats, worked examples from simple
to complex, the places you can use a query, and how to read the errors the
engine raises when a query is malformed.

For hands-on help while writing a query, see:

- `M-x supertag-query-describe-syntax` — a short in-Emacs cheat sheet (same
  content as the table below, condensed).
- `M-x supertag-query-build` — an interactive wizard that assembles a query
  for you, with tag/field completion from your own data.

## Grammar reference

| Operator | Arguments | Meaning | Example |
|---|---|---|---|
| `and` | one or more conditions | all of them must match | `(and (tag "task") (tag "work"))` |
| `or` | one or more conditions | any of them must match | `(or (tag "work") (tag "personal"))` |
| `not` | exactly one condition | the condition must not match | `(not (tag "archived"))` |
| `tag` | `NAME` | nodes carrying tag `NAME` | `(tag "project")` |
| `field` | `KEY VALUE` | nodes whose field `KEY` equals `VALUE` (exact match) | `(field "status" "active")` |
| `term` | `WORD` | substring search over node title and content (case-insensitive) | `(term "meeting")` |
| `after` | `DATE` | nodes dated after `DATE` | `(after "2025-01-01")` |
| `before` | `DATE` | nodes dated before `DATE` | `(before "2025-12-31")` |
| `between` | `START END` | nodes dated between `START` and `END` | `(between "-7d" "now")` |

Notes on argument types, verified against `supertag-query--parse-sexp`:

- `NAME`, `KEY`, `VALUE`, and `WORD` should be double-quoted strings. Bare
  symbols are also accepted (e.g. `(tag project)` works the same as
  `(tag "project")`), but **numbers are not** — `(field "priority" 5)` raises
  `wrong-type-argument symbolp 5`. Write `(field "priority" "5")` instead.
- `tag` takes exactly one argument, `field` exactly two, `after`/`before`
  exactly one, `between` exactly two, `not` exactly one condition. Passing
  the wrong number of arguments raises an explicit error naming the operator,
  e.g. `'field' operator expects exactly two arguments, but got (...)`.
- An unknown operator raises `Invalid query operator: NAME`.
- An empty `(and)` matches every node (the identity for AND); an empty `(or)`
  matches no nodes (the identity for OR). This is rarely intentional — it
  usually means a combinator lost its children while you were editing.
- `field` matching is an exact `equal` comparison, not a substring or
  case-insensitive match. Use `term` if you want fuzzy/substring matching
  over title and content instead.

## Date formats

`after`, `before`, and `between` all resolve their date arguments the same
way (`supertag-query--resolve-date-string`, supertag-services-query.el
around lines 214-247):

| Format | Example | Meaning |
|---|---|---|
| `"now"` | `(after "now")` | the current moment |
| Absolute date | `"2025-06-01"` | `YYYY-MM-DD`, exactly four digits, two digits, two digits |
| Relative offset | `"-7d"`, `"+2w"`, `"-1m"`, `"1y"` | a signed (or unsigned) integer followed by `d`/`w`/`m`/`y` |

Relative-offset details:

- The unit is one of `d` (day), `w` (week, 7 days), `m` (month, approximated
  as **30 days**), or `y` (year, approximated as **365.25 days**).
- **A missing sign means `+`**, i.e. the future: `"7d"` is the same as
  `"+7d"` (seven days from now), *not* seven days ago. If you mean "the last
  7 days", write `"-7d"` explicitly.
- Anything that doesn't match `"now"`, an absolute date, or this relative
  pattern is treated as invalid and raises `Invalid date format for 'after':
  ...` (or `'before'`/the corresponding start/end date for `between`).

Known limitation (verified, not a documentation gap): as of this writing,
absolute `YYYY-MM-DD` dates passed to `after`, `before`, or `between` raise
`Invalid time specification` at query time, because the date is parsed with
`parse-time-string` (which returns a decoded-time list with `nil` fields for
an unqualified day) and then compared with `time-less-p`, which requires a
real time value. **Relative offsets (`"-7d"`, `"+2w"`, ...) and `"now"` work
correctly; plain `YYYY-MM-DD` currently does not.** Until this is fixed
upstream, prefer relative offsets for date filtering, e.g. `(after "-30d")`
instead of `(after "2025-06-01")`.

## Composition examples, simple to complex

1. Everything tagged `project`:

   ```
   (tag "project")
   ```

2. Everything tagged `task` whose `status` field is `active`:

   ```
   (and (tag "task") (field "status" "active"))
   ```

3. Anything tagged `work` or `personal`:

   ```
   (or (tag "work") (tag "personal"))
   ```

4. Tasks that are not done:

   ```
   (and (tag "task") (not (field "status" "done")))
   ```

5. Tasks created in the last 7 days (see the relative-date caveat above —
   this uses a relative offset, not an absolute date):

   ```
   (and (tag "task") (after "-7d"))
   ```

6. A larger example combining nested `or`, `not`, and a relative date filter:

   ```
   (and
     (or (tag "work") (tag "project"))
     (not (field "status" "completed"))
     (after "-30d"))
   ```

## Where queries can be used

- **Babel query blocks** — the primary way to embed a live query result table
  in an Org file:

  ```org
  #+BEGIN_SRC org-supertag-query-block :results raw
  (and (tag "task") (field "status" "active"))
  #+END_SRC
  ```

  Insert one with `M-x supertag-insert-query-block`. The block layer is also
  gaining extra babel header parameters (`:sort`, `:order`, `:limit`,
  `:columns`) for controlling result presentation without changing the query
  itself — see the docstring of `org-babel-execute:org-supertag-query-block`
  in `supertag-ui-query-block.el` for the current, authoritative parameter
  list, e.g.:

  ```org
  #+BEGIN_SRC org-supertag-query-block :results raw :sort modified :order desc :limit 10
  (tag "task")
  #+END_SRC
  ```

- **Dynamic blocks** — a `#+BEGIN: supertag-query ... #+END:` form is also
  being added alongside the babel block (`org-dblock-write:supertag-query`,
  insertable with `M-x supertag-insert-query-dblock`), for queries that
  re-render in place with `C-c C-c` or `org-update-all-dblocks`. See
  `supertag-ui-query-block.el` for the exact header arguments; the query
  S-expression syntax itself is identical to what's documented here, for
  example:

  ```org
  #+BEGIN: supertag-query :query "(tag \"task\")"
  #+END:
  ```

- **Saved queries** — name a query once, reuse it forever:
  - `M-x supertag-query-save` — save a query (from the region, or typed in)
    under a name, in `supertag-query-saved`.
  - `M-x supertag-query-run-saved` — pick a saved query and see its results
    immediately in a temp buffer.
  - `M-x supertag-query-insert-saved` — insert a saved query as a block at
    point (babel form, or the dynamic-block form if that layer is loaded).

- **The guided builder** — `M-x supertag-query-build` walks you through
  picking an operator, filling in its arguments (with completion for tag and
  field names pulled from your own data), and combining conditions with
  AND/OR/NOT, then lets you copy, insert, run, or save the result.

## Troubleshooting

**`Invalid query operator: NAME`**
You used an operator the parser doesn't recognize. Only `and`, `or`, `not`,
`tag`, `field`, `after`, `before`, `between`, and `term` are implemented.
(Older docs and examples elsewhere may mention `recent-days`, `in-month`, or
`in-year` — these are **not** implemented by the current parser and will
raise this error if you try them.)

**`'OPERATOR' operator expects ... arguments, but got (...)`**
You passed the wrong number of arguments to `tag`, `field`, `not`, `after`,
`before`, or `between`. Check the grammar table above for the exact arity.

**`wrong-type-argument symbolp N`**
You passed a bare number where a string or symbol was expected, e.g.
`(field "priority" 5)`. Quote it: `(field "priority" "5")`.

**`Invalid date format for 'after': ...` (or `'before'`)**
The date string didn't match `"now"`, `YYYY-MM-DD`, or a relative offset
like `-7d`/`+2w`. Check for typos, and remember the unit letters are
lowercase `d`/`w`/`m`/`y`.

**`Invalid time specification`**
You used an absolute `YYYY-MM-DD` date with `after`/`before`/`between`. See
the known limitation above; use a relative offset instead.

**A query returns no results and you expected some**
- Double-check tag/field spelling — `tag` and `field` do exact matches, not
  substring matches. Use `term` for fuzzy text search.
- If you're combining conditions, confirm you meant `and` (all must match)
  and not `or` (any must match), or vice versa.
- For `between`, confirm `START` really is earlier than `END` — if reversed,
  the query silently matches nothing rather than erroring.
- A query result table only shows columns for fields explicitly named in a
  `field` condition; a node can still match without every field being
  populated, in which case that column is blank for that row, not missing.

## Related commands

| Command | What it does |
|---|---|
| `supertag-insert-query-block` | Insert an empty babel query block, then prompt for the S-expression |
| `supertag-query-build` | Interactive wizard to assemble a query S-expression |
| `supertag-query-save` | Save a query under a name |
| `supertag-query-run-saved` | Run a saved query and show results in a buffer |
| `supertag-query-insert-saved` | Insert a saved query as a block at point |
| `supertag-query-describe-syntax` | Show the quick-reference cheat sheet |

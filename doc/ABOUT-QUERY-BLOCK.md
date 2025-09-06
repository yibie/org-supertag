# Org Supertag Query-Block Examples

Author: Refactoring Completed  
Date: 2025-08-23

## S-expression Query Function Renamed to Query-Block

### New Language Name

`org-supertag-query-block` is now used as the new Org Babel language name:

```org-supertag-query-block
(tag "work")
```

### Backward Compatibility

The old `org-supertag-query` language name can still be used (backward compatible):

```org-supertag-query
(and (tag "project") (field "priority" "high"))
```

### Supported Query Syntax

#### Tag Query

```org-supertag-query-block
(tag "work")
```

#### Field Query

```org-supertag-query-block
(field "status" "active")
```

#### Full-text Search

```org-supertag-query-block
(term "meeting")
```

#### Logical Combinations

Logical combinations are a powerful feature of Query-Block, supporting complex query conditions:

##### AND Combination
```org-supertag-query-block
(and (tag "project") (field "priority" "high"))
```

##### OR Combination
```org-supertag-query-block
(or (tag "work") (tag "personal"))
```

##### NOT Combination
```org-supertag-query-block
(not (tag "archived"))
```

##### Nested Logical Combinations
```org-supertag-query-block
(or 
  (and (tag "project") (field "status" "active"))
  (and (tag "task") (after "2025-06-01")))
```

##### Complex Logical Combination Example
```org-supertag-query-block
(and 
  (or (tag "work") (tag "project"))
  (not (field "status" "completed"))
  (after "2025-01-01"))
```

#### Time Queries

Time queries support multiple time-related conditions:

##### After a Specific Date
```org-supertag-query-block
(after "2025-01-01")
```

##### Before a Specific Date
```org-supertag-query-block
(before "2025-12-31")
```

##### Date Range
```org-supertag-query-block
(and (after "2025-01-01") (before "2025-12-31"))
```

##### Recent Days
```org-supertag-query-block
;; Query nodes created in the last 7 days
(recent-days 7)
```

##### Specific Month
```org-supertag-query-block
;; Query nodes in a specific month
(in-month "2025-06")
```

##### Specific Year
```org-supertag-query-block
;; Query nodes in a specific year
(in-year "2025")
```

#### Complex Query Example

```org-supertag-query-block
(or 
  (and (tag "project") (field "status" "active"))
  (and (tag "task") (after "2025-06-01")))
```

### Query Result Format

Query results are displayed in table format by default, including node titles, tags, and queried field values:

```org-supertag-query-block
(field "priority" "high")
```

#### Query Result Style Example

Here is a simulated query result display:

| Title | Tags | priority |
|------|------|----------|
| Important Project A | project,work | high |
| Urgent Task B | task,work | high |
| Key Meeting Preparation | meeting,project | high |

#### Query Result Example with Time Conditions

```org-supertag-query-block
(and (tag "task") (after "2025-06-01"))
```

| Title | Tags | Creation Date |
|------|------|----------|
| New Task 1 | task,work | 2025-06-15 |
| New Task 2 | task,personal | 2025-07-20 |

### Standalone Query Window

In addition to S-expression query blocks, there is also a standalone interactive query window feature:

- `M-x supertag-query-interactive` - Open interactive query window
- Supports keyword search, card-style display, navigation, tagging, and export functions

#### Interactive Query Features

- Card-style result display, including title, file path, tags, and context preview
- Keyboard navigation: n/p keys for up/down movement
- Spacebar to tag nodes, Enter to access nodes
- Supports inserting selected node links into documents
- Supports exporting selected results to new files

### Advanced Query Features

#### Relationship Queries

```org-supertag-query-block
;; Query all entities related to a specified entity
;; (Note: This feature needs to be implemented in supertag-services-query.el)
(tag "ProjectA")
```

#### Database Record Queries

```org-supertag-query-block
;; Query all records in a specific database
;; (Note: This feature needs to be implemented in supertag-services-query.el)
(tag "Tasks")
```

### Rename Summary

1. **S-expression query function** renamed to **query-block**
2. **Standalone query window function** remains unchanged, continuing to provide a complete interactive query experience
3. **Backward compatibility**: Old language names and functions can still be used
4. **Insert function**: `supertag-insert-query-block` now generates new format code blocks

### Practical Tips

#### Quick Insert Query Block

Use the `M-x supertag-insert-query-block` command to quickly insert a query block template.

#### Query History

The interactive query window saves query history for easy reuse of common queries.

#### Result Export

Query block results can be exported as Org tables, and interactive query results can be exported as new files containing links.

This design satisfies the renaming requirements while maintaining system integrity and backward compatibility.
# Programming Guide

This guide collects patterns and examples for TempleCode programs.

## Data & expressions

- Use dicts `{}` and lists `[]` to aggregate data. Index with `a[0]`, slice with `a[1:5]`.
- Call built-in helpers: `upper`, `lower`, `substr`, `find`, `replace`, `split`, `join`, `sum`, `avg`.
- Random/time: `rand()`, `randint(a,b)`, `now()`, `date()`, `time()`.

## Procedures

- Return values with `RETURN expr`, capture with `CALL name args INTO var`.
- Locals are isolated per call; globals live in the outer scope.

## Control flow

- Choose between `FOR/NEXT`, `WHILE/ENDWHILE`, and `REPEAT/ENDREPEAT` depending on the loop shape.
- Use inline `IF ... THEN ... ELSE ...` for compact logic.

## Graphics patterns

- Animate with a loop and small `SLEEP` delays.
- Use `PENWIDTH` and `COLOR` for emphasis; use `FILLCOLOR` + `RECT ... FILL` for solids.
- Keep the turtle orientation in mind; `SETHEADING` + `FD` combines well.

Example: bouncing dot

```
CLS
LET x = 100
LET y = 100
LET vx = 2
LET vy = 3
LET W = 500
LET H = 400
PENWIDTH 3
COLOR 255 128 0
FOR t = 1 TO 2000
  CLS
  CIRCLE x, y, 10 FILL
  LET x = x + vx
  LET y = y + vy
  IF x <= 0 THEN LET vx = abs(vx)
  IF x >= W THEN LET vx = -abs(vx)
  IF y <= 0 THEN LET vy = abs(vy)
  IF y >= H THEN LET vy = -abs(vy)
  SLEEP 0.01
NEXT
```

## Files & data

- `READJSON`/`WRITEJSON` for structured data; `CSVREAD`/`CSVWRITE` for tabular.
- `DIRLIST` + `EXISTS` for simple filesystem tasks.

## Database patterns (MySQL)

Install the driver:

```bash
pip install mysql-connector-python
```

Connect once, then execute and query. Use `%s` placeholders and pass params as a list.

Example: simple upsert and query

```text
DBCONNECT main, host, user, password, dbname, 3306
DBEXEC main, "CREATE TABLE IF NOT EXISTS demo (id INT PRIMARY KEY, name VARCHAR(50))"
DBEXECPARAM main, "INSERT INTO demo (id, name) VALUES (%s, %s)", [1, "Alice"]
DBQUERYPARAM main, "SELECT id, name FROM demo WHERE id >= %s ORDER BY id", [1] INTO rows
FOR i = 0 TO len(rows)-1
  PRINT rows[i]["id"] + ": " + rows[i]["name"]
NEXT
DBDISCONNECT main
```

### SQR-style mapping

- SQR `BEGIN-SELECT ... END-SELECT` ≈ TempleCode `DBQUERYPARAM ... INTO rows` + loop over `rows`.
- SQR `EXECUTE` ≈ TempleCode `DBEXEC` or `DBEXECPARAM`.
- SQR `LET :var = ...` ≈ TempleCode `LET var = expr`.

Sketch:

```text
REM SQR-like: parameterized SELECT loop
LET min_id = 100
DBQUERYPARAM main, "SELECT id, name FROM emp WHERE id >= %s", [min_id] INTO rows
FOR i = 0 TO len(rows)-1
  LET r = rows[i]
  PRINT r["id"] + ": " + r["name"]
NEXT
```

## Debug & diagnostics

- Enable tracing: `TRACE ON` (or Run > Trace Execution in IDE).
- Inspect variables: `DUMPVARS`.
- Guard assumptions: `ASSERT x > 0, "x must be positive"`.

# TempleCode User Guide

## Running Time Warp

- Start: `/home/james/Temple_Code/.venv/bin/python -m time.warp.app`
- Editor: write TempleCode, press F5 to run; Esc to stop.
- Console: shows PRINT output and prompts for INPUT.
- Canvas: turtle drawings.
- Examples: Use the Examples menu to load sample programs.

## Core commands

- PRINT expr; TYPE expr (alias)
- INPUT var [prompt]; ACCEPT var [prompt] (alias)
- LET var = expr; or `var = expr`
- SLEEP seconds; REM comment; `# comment`

## Control flow

- IF cond THEN statement [ELSE statement]
- FOR var = start TO end [STEP step]; NEXT
- WHILE cond; ENDWHILE
- REPEAT n; ENDREPEAT
- GOTO label; labels: `10 PRINT ...` or `start:`

## Procedures

- PROC name [param1 param2 ...]; ENDPROC
- CALL name [arg1, arg2, ...] [INTO resultVar]
- RETURN [expr]

Notes:

- PROC params are space-separated
- CALL args must be comma-separated

## Turtle graphics

- FD n, LT a, RT a, PU, PD, CLS, SETXY x, y, COLOR r g b
- PENWIDTH w, FILLCOLOR r g b, BACKGROUND r g b
- RECT x, y, w, h [FILL]
- CIRCLE x, y, r [FILL]
- TEXT x, y, message
- HOME, SETHEADING a

## Files and data

- READFILE var, path; WRITEFILE path, expr; APPENDFILE path, expr
- CSVREAD var, path; CSVWRITE path, rows
- READJSON var, path; WRITEJSON path, data
- DIRLIST var, path; EXISTS var, path

## Database (MySQL)

TempleCode can talk to MySQL using simple statements. Install the driver first:

```bash
pip install mysql-connector-python
```

Commands:

- DBCONNECT name, host, user, password, database[, port]
- DBDISCONNECT name
- DBEXEC name, sql
- DBEXECPARAM name, sql, params
- DBQUERY name, sql [INTO var]
- DBQUERYPARAM name, sql, params [INTO var]

Notes:

- Use `%s` placeholders in SQL and supply params as a list, e.g. `[42, "Alice"]`.
- `DBQUERY`/`DBQUERYPARAM` return a list of rows. If the driver reports column names, each row is a dict.
- Use `INTO resultVar` to capture query results.

Example:

```text
REM See examples/mysql_demo.tc for a full script
DBCONNECT main, host, user, password, dbname, 3306
DBEXEC main, "CREATE TABLE IF NOT EXISTS demo (id INT PRIMARY KEY, name VARCHAR(50))"
DBEXECPARAM main, "INSERT INTO demo (id, name) VALUES (%s, %s)", [1, "Alice"]
DBQUERYPARAM main, "SELECT id, name FROM demo WHERE id >= %s", [1] INTO rows
PRINT "Rows: " + str(rows)
DBDISCONNECT main
```

## Debugging

- TRACE ON/OFF — echo lines before execution
- DUMPVARS — print all variables (merged scopes)
- PAUSE [prompt] — wait for Enter
- ASSERT expr[, message] — fail if condition is false

## Expressions and functions

- Math: sin cos tan asin acos atan sqrt log log10 floor ceil fabs pow pi e tau degrees radians
- Core: abs min max len int float str round
- Strings/lists: upper lower substr find replace split join
- Random/time: rand randint now date time
- Aggregates: sum avg

## Tips

- Use commas for CALL and shape commands (e.g., `RECT x, y, w, h`)
- For SETXY with expressions, commas avoid ambiguity: `SETXY cx + dx, cy - dy`
- Use RUN menu > Trace Execution for quick debugging.
- The Examples menu includes a MySQL Demo you can adapt to your database.

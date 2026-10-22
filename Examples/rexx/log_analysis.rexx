/* ============================================================ */
/* WEB LOG TRIAGE                                               */
/*                                                              */
/* A REXX program of the kind REXX was invented for: read        */
/* records, pull them apart, print a report.                     */
/*                                                              */
/* REXX values are typeless -- '42' and 42 are the same value    */
/* to every operator here -- and there are no arrays, so the     */
/* work is done by handing each record to a subroutine and       */
/* keeping running totals. A subroutine reads its arguments      */
/* with ARG(n), this dialect's way of reaching them.             */
/*                                                              */
/* Run: open the file and press Run (F5). Expect a report.       */
/* ============================================================ */

ok = 0
redirect = 0
client_error = 0
server_error = 0
requests = 0
bytes = 0
biggest = 0
biggest_path = ''
failures = ''

SAY 'W E B   L O G   T R I A G E'
SAY COPIES('=', 60)
SAY ''

/* --- the log ------------------------------------------------- */
/* One CALL per line, in the order a server wrote them. Columns  */
/* are method, path, status, size; the subroutine splits them.   */
CALL triage 'GET  /index.html             200   5120'
CALL triage 'GET  /assets/app.css         200  14320'
CALL triage 'GET  /pricing                200   7340'
CALL triage 'POST /login                 401    512'
CALL triage 'GET  /favicon.ico           404    128'
CALL triage 'GET  /assets/app.js         200  91240'
CALL triage 'GET  /docs/getting-started  200  24180'
CALL triage 'POST /api/orders            500   1024'
CALL triage 'GET  /health                200     12'
CALL triage 'GET  /assets/logo.png       304      0'
CALL triage 'POST /api/orders            201   2048'
CALL triage 'GET  /missing-page          404    256'

/* --- the report ---------------------------------------------- */
SAY 'SUMMARY'
SAY COPIES('-', 60)
SAY LEFT('requests', 24) RIGHT(requests, 8)
SAY LEFT('bytes served', 24) RIGHT(bytes, 8)
SAY LEFT('average response', 24) RIGHT(TRUNC(bytes / requests), 8)||' bytes'
SAY LEFT('largest response', 24) RIGHT(biggest, 8)||' bytes ('||biggest_path||')'
SAY ''

SAY 'BY STATUS CLASS'
SAY COPIES('-', 60)
CALL share '2xx success', ok
CALL share '3xx redirect', redirect
CALL share '4xx client error', client_error
CALL share '5xx server error', server_error
SAY ''

SAY 'PATHS THAT FAILED'
SAY COPIES('-', 60)
IF failures = '' THEN
  SAY '  none'
ELSE
  DO path OVER failures
    SAY '  '||path
  END
SAY ''

/* A verdict, not just numbers: a log a person has to read is a  */
/* log nobody reads.                                             */
IF server_error > 0 THEN
  SAY 'VERDICT: the server is failing requests -- fix the 5xx paths first.'
ELSE IF client_error * 4 > requests THEN
  SAY 'VERDICT: a quarter of the traffic is turned away -- fix the links.'
ELSE
  SAY 'VERDICT: the site is healthy.'
EXIT


/* ------------------------------------------------------------ */
/* triage: one record -> the running totals                     */
/* ------------------------------------------------------------ */
triage:
  line = ARG(1)
  PARSE VAR line method path status size

  requests = requests + 1
  bytes = bytes + size
  IF size > biggest THEN DO
    biggest = size
    biggest_path = path
  END

  /* The class is the first digit of the status: 200 -> 2xx.    */
  class = SUBSTR(status, 1, 1)
  IF class = 2 THEN
    ok = ok + 1
  ELSE IF class = 3 THEN
    redirect = redirect + 1
  ELSE IF class = 4 THEN DO
    client_error = client_error + 1
    failures = failures||' '||path
  END
  ELSE DO
    server_error = server_error + 1
    failures = failures||' '||path
  END
  RETURN

/* share: one line of the class table, with its percentage.      */
/* `failures` is a string of words rather than a list, so the    */
/* report walks it with DO OVER -- there is no array to walk.    */
share:
  label = ARG(1)
  count = ARG(2)
  /* One decimal place, from the tenths: 583 -> '58.3', 250 -> '25.0'. */
  tenths = TRUNC(count * 1000 / requests)
  digits = LENGTH(tenths)
  IF digits = 1 THEN
    SAY LEFT(label, 24) RIGHT(count, 8) RIGHT(tenths, 5)||'.0%'
  ELSE DO
    whole = RIGHT(SUBSTR(tenths, 1, digits - 1), 4)
    SAY LEFT(label, 24) RIGHT(count, 8)||' '||whole||'.'||SUBSTR(tenths, digits)||'%'
  END
  RETURN

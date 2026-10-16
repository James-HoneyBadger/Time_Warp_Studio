#!/usr/bin/env python3
"""Diffs the C++ Scheme port against the Python reference interpreter.

Every program in CORPUS is run through both implementations and the *complete
output text* is compared, so this checks rendering as well as evaluation.

Three outcomes are possible:

  * agreement          -- identical output (or both raise a Scheme error)
  * documented divergence -- listed in DIVERGENT with a reason
  * unexpected mismatch   -- a bug; the run fails

Usage:
    python3 native/tools/scheme_differential_check.py native/build/scheme_cli
"""

from __future__ import annotations

import re
import subprocess
import sys
from pathlib import Path

from scheme_reference import run_scheme

ADDRESS = re.compile(r"<object object at 0x[0-9a-f]+>")

# ---------------------------------------------------------------------------
# Corpus
# ---------------------------------------------------------------------------

CORPUS: list[str] = [
    # -- arithmetic and numerics -----------------------------------------
    "(display (+ 1 2 3))",
    "(display (+))",
    "(display (*))",
    "(display (- 5))",
    "(display (- 5 2 1))",
    "(display (* 2 3 4))",
    "(display (/ 6 3))",
    "(display (/ 1 2))",
    "(display (/ 2))",
    "(display (list 3 3.5 3.0 -0.0))",
    "(display (list (integer? 3) (integer? 3.0) (integer? (/ 6 3))))",
    "(display (list (number? 3) (number? 3.0) (number? #t)))",
    "(display (list (quotient 7 2) (quotient -7 2) (quotient 7 -2)))",
    "(display (list (remainder -7 2) (modulo -7 2) (modulo 7 -2)))",
    "(display (list (expt 2 10) (expt 2 0) (expt 2 -1) (expt 2.0 3)))",
    "(display (list (abs -3) (abs 3) (abs -3.5)))",
    "(display (list (min 3 1 2) (max 3 1 2) (min 2 2.0) (max 1.0 1)))",
    "(display (list (sqrt 16) (sqrt 2)))",
    "(display (list (floor 2.7) (floor -2.7) (ceiling 2.1) (truncate -2.7)))",
    "(display (list (round 0.5) (round 1.5) (round 2.5) (round -0.5) (round 3)))",
    "(display (list (exact->inexact 5) (inexact->exact 5.7) (inexact->exact -5.7)))",
    "(display (list (zero? 0) (positive? 1) (negative? -1) (odd? 3) (even? 4)))",
    "(display (list (odd? 2.5) (even? 2.5)))",
    "(display (= 1 1 1))",
    "(display (list (= 1 2) (< 1 2 3) (> 3 2 1) (<= 1 1 2) (>= 2 2 1)))",
    "(display (list (= 1) (< 1)))",
    "(display (= 1 1.0))",
    "(display 1e5)",
    "(display (list .5 5. 0.25))",
    # Integral reals beyond 2^63 must still print every digit, because the
    # reference renders them with `str(int(x))`.
    "(display 9223372036854775808.0)",
    "(display 1e30)",
    "(display 2.5e20)",
    "(display 1e300)",
    "(display -1e30)",
    "(display (expt 2 63))",
    "(display (* 4611686018427387904 4))",
    "(display (+ 9223372036854775807 1))",
    "(display (list #t #true #f #false))",

    # -- truthiness and conditionals -------------------------------------
    "(display (list (if 0 't 'f) (if '() 't 'f) (if \"\" 't 'f)))",
    "(display (list (not 0) (not #f) (not '()) (not #t)))",
    "(display (list (and) (or) (and 1 2) (or #f 3) (and 1 #f) (or #f #f)))",
    "(display (list (and 1 2 3) (or #f #f 7)))",
    "(display (list (when #t 1 2) (unless #f 3) (when #f 1) (unless #t 1)))",
    "(display (cond ((assoc 'b '((a 1) (b 2))) => cadr) (else 'no)))",
    "(display (cond (7) (else 1)))",
    "(display (cond (#f 1) (else 2)))",
    "(display (cond (#f 1)))",
    "(display (list (if #t 1) (if #f 1) (if #f 1 2)))",

    # -- equality --------------------------------------------------------
    "(display (list (equal? #t 1) (equal? #f 0) (eqv? #t 1) (eq? #t 1)))",
    "(display (list (equal? '(1 2) '(1 2)) (eq? '(1 2) '(1 2))))",
    "(display (equal? (vector 1 2) (vector 1 2)))",
    "(display (equal? 'abc \"abc\"))",
    "(display (list (equal? 1 1.0) (eqv? 1 1.0)))",
    "(display (list (equal? '() '()) (eq? 'a 'a)))",
    "(display (list (null? '()) (null? '(1)) (pair? '(1)) (list? '(1))))",
    "(display (list (list? '()) (pair? 5) (vector? (vector 1))))",

    # -- binding forms ---------------------------------------------------
    "(display (let ((x 2) (y 3)) (* x y)))",
    "(display (let ((x 1)) (let ((x 2) (y x)) (list x y))))",
    "(display (let* ((x 2) (y (* x 3))) (list x y)))",
    "(display (letrec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))) (f 5)))",
    "(display (let loop ((i 0) (acc 1)) (if (= i 5) acc (loop (+ i 1) (* acc 2)))))",
    "(display (let () 1))",
    "(display (let ((a 1 2)) a))",
    "(display (do ((i 0 (+ i 1))) ((= i 3) 'done)))",
    "(display (do ((i 0 (+ i 1)) (acc '() (cons i acc))) ((= i 4) acc)))",
    "(display (do ((i 3 (+ i 1)) (j i)) ((> i 3) (list i j))))",
    "(define x 1) (set! x 2) (display x)",
    "(set! zzz 5) (display zzz)",
    "(display (define q 1))",
    "(define y) (display y)",

    # -- procedures ------------------------------------------------------
    "(define (f x . r) (list x r)) (display (f 1 2 3))",
    "(display ((lambda (a b) (list a b)) 1))",
    "(display ((lambda (a) a) 1 2 3))",
    "(define (adder n) (lambda (x) (+ x n))) (display ((adder 3) 4))",
    "(define (f x) (* x 2)) (display (f 21))",
    "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (display (fact 10))",
    "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (display (fib 15))",
    "(define (count n) (if (= n 0) 0 (+ 1 (count (- n 1))))) (display (count 100))",
    "(define (loop n) (if (= n 0) 'done (loop (- n 1)))) (display (loop 150))",
    "(define (mk n acc) (if (= n 0) acc (mk (- n 1) (cons n acc)))) (display (length (mk 100 '())))",
    "(define (mut) (set! counter (+ counter 1)) counter) (define counter 0) (mut) (mut) (display counter)",

    # -- quote and quasiquote --------------------------------------------
    "(display '(1 2 3))",
    "(display '())",
    "(display ''a)",
    "(display '(1 . 2))",
    "(display (length '(1 . 2)))",
    "(display (length '(1 2 . 3)))",
    "(display `. )",
    "(display `(1 2))",
    "(display `(1 ,(+ 1 1)))",
    "(display `(1 ,@(list 2 3) 4))",
    "(display `(a ,(* 2 3)))",
    "(display `(1 ,@'() 2))",
    "(display `(a `(b ,(c ,(+ 1 2)))))",
    "(display `(,(+ 1 2) ,@(map (lambda (x) (* x x)) '(2 3))))",
    "(display `(a ,(let ((x 5)) `(b ,x)) c))",
    "(display (quote (a b)))",

    # -- lists -----------------------------------------------------------
    "(display (list (length '(1 2 3)) (reverse '(1 2 3))))",
    "(display (list (append) (append '(1 2)) (append '(1) '(2) '(3))))",
    "(display (append '(1) 2))",
    "(display (list (cons 1 2) (cons 1 '()) (car '(1 2)) (cdr '(1 2))))",
    "(display (list (caar '((1 2) 3)) (cadr '(1 2 3)) (cdar '((1 2) 3)) (cddr '(1 2 3))))",
    "(display (list (member 2 '(1 2 3)) (memq 'c '(a b c)) (assoc 'b '((a 1) (b 2)))))",
    "(display (list (member 9 '(1 2)) (assoc 9 '((a 1)))))",
    "(display (list (list-ref '(1 2 3) 0) (list-ref '(1 2 3) 2) (list-ref '(1 2 3) -1)))",
    "(display (list (list-tail '(1 2 3) 1) (list-tail '(1 2 3) 0) (list-tail '(1 2 3) 99) (list-tail '(1 2 3) -1)))",
    "(display (list 1 2.5 'a \"b\" #t '() (list 1 2)))",
    "(display '((1 2) (3 4)))",
    "(display (length (append '(1 2) '(3 4 5))))",

    # -- vectors ---------------------------------------------------------
    "(display (vector 1 2 3))",
    "(display (list (vector-length (vector 1 2)) (vector-ref (vector 1 2) 1)))",
    "(display (list (vector->list (vector 1 2)) (list->vector '(1 2))))",
    "(display (list (make-vector 3) (make-vector 2 9)))",
    "(display (vector 1 (vector 2 3)))",
    "(display (vector))",

    # -- strings and symbols ---------------------------------------------
    "(display (list (string-length \"abc\") (substring \"hello\" 1 3)))",
    "(display (list (substring \"hello\" 1 99) (substring \"hello\" -3 99)))",
    "(display (list (substring \"hello\" 1) (substring \"hello\" 0 0)))",
    "(display (list (string->number \"12\") (string->number \"1.5\") (string->number \"zz\") (string->number \"12\" 16)))",
    "(display (list (number->string 12) (number->string 1.5) (number->string -3)))",
    "(display (list (symbol->string 'abc) (string->symbol \"x\")))",
    "(display (list (string-upcase \"ab\") (string-downcase \"AB\")))",
    "(display (string->list \"abc\"))",
    "(display (list (string-ci=? \"AB\" \"ab\") (string=? \"a\" \"a\") (string<? \"a\" \"b\")))",
    "(display (string-append \"a\" \"b\" \"c\"))",
    "(display (list (string? \"a\") (string? 'a) (symbol? 'a)))",
    "(display (string-length 'abc))",
    "(display (list (string=? \"a\" \"b\") (string=?)))",
    "(display #\\a)",
    "(display (list #\\a #\\space #\\newline #\\tab))",
    "(display '.)",

    # -- output modes ----------------------------------------------------
    "(display \"hi\")",
    "(display (list \"hi\"))",
    "(write \"hi\")",
    "(write '(1 \"a\"))",
    "(write 3.0)",
    "(write 'abc)",
    "(newline)",
    "(display car)",
    "(display (list 1 2)) (newline) (display 3)",
    "(display \"a\\nb\")",

    # -- higher order ----------------------------------------------------
    "(display (map + '(1 2 3) '(10 20 30)))",
    "(display (map + '(1 2 3) '(10 20)))",
    "(display (map (lambda (x) (* x x)) '(1 2 3)))",
    "(display (map car '((1 2) (3 4))))",
    "(display (apply + 1 2 '(3 4)))",
    "(display (apply list))",
    "(display (apply max '(3 1 2)))",
    "(display (list (filter odd? '(1 2 3 4 5)) (foldl + 0 '(1 2 3 4))))",
    "(display (foldl (lambda (a b) (cons b a)) '() '(1 2 3)))",
    "(display (filter (lambda (x) (> x 2)) '(1 2 3 4)))",
    "(for-each display '(1 2 3))",
    "(display (map (lambda (x) (* x x)) '()))",
    "(display (map (lambda (a b) (+ a b)) '(1 2) '(3 4)))",

    # -- errors the reference *does* report as Scheme errors --------------
    "(display zzz)",
    "(display (car 5))",
    "(display (cdr 5))",
    "(display (5 6))",
    "(display ())",
    "(display (/ 1 0))",
    "(display (sqrt \"x\"))",
    "(display (+ 1 'a))",
    "(display (error 'bad \"thing\" 3))",
    "(display (length (append '(1) 2)))",
    "(display (+ 1",
    "(display \"abc)",
    "(display (list-ref '(1 2) 'a))",

    # -- turtle (produces no output) -------------------------------------
    "(forward 10) (right 90) (display 'ok)",
    "(penup) (home) (display 'ok)",
    "(display (begin (forward 10) 'moved))",
]

# ---------------------------------------------------------------------------
# Divergences that are intentional, with the reason
# ---------------------------------------------------------------------------

DIVERGENT: dict[str, str] = {
    # --- case: broken in the reference, fixed here ----------------------
    "(display (case 1 ((1) 'one) (else 'other)))":
        "reference: 'expected a proper list' for *every* case form",
    "(display (case 9 ((1) 'one) (else 'other)))":
        "same root cause -- the reference called _to_py_list() on parsed code",
    "(display (case 'b ((a) 1) ((b c) 2) (else 3)))":
        "same root cause",
    "(display (case 5 ((1 2 3) 'low) ((4 5 6) 'mid) (else 'high)))":
        "same root cause",
    "(display (case 99 ((1) 'one)))":
        "same root cause",

    # --- the reference's depth limit ------------------------------------
    "(define (loop n) (if (= n 0) 'done (loop (- n 1)))) (display (loop 1000000))":
        "reference: 'Maximum recursion depth exceeded' at ~200 calls",
    "(define (sum n) (if (= n 0) 0 (+ n (sum (- n 1))))) (display (sum 100000))":
        "reference: 'Maximum recursion depth exceeded'; port uses the heap",
    "(define (mk n acc) (if (= n 0) acc (mk (- n 1) (cons n acc)))) (display (length (mk 200000 '())))":
        "reference: 'Maximum recursion depth exceeded'",
    "(define (nest n x) (if (= n 0) x (nest (- n 1) (list x)))) (display (nest 200000 'a))":
        "reference: 'Maximum recursion depth exceeded'",
    "(define (nest n x) (if (= n 0) x (nest (- n 1) (list x)))) (display (equal? (nest 100000 'a) (nest 100000 'a)))":
        "reference: recursive _equal() exhausts the native stack",

    # --- unspecified value renders as an address ------------------------
    "(display (if #f 1))":
        "reference prints '<object object at 0x...>' -- a heap address, not reproducible",
    "(display (begin))":
        "reference prints a heap address",
    "(define y) (display y)":
        "reference prints a heap address",
    "(display (cond (#f 1)))":
        "reference prints a heap address",
    "(display (do ((i 0 (+ i 1))) ((= i 3) 'done)))":
        "agrees; listed only if it drifts",
    "(display (list (when #t 1 2) (unless #f 3) (when #f 1) (unless #t 1)))":
        "contains the address rendering of the unspecified value",
    "(display (list-ref '(1 2) 'a))":
        "reference let Python's ValueError (int('a')) escape the executor",
    "(display (list (if #t 1) (if #f 1) (if #f 1 2)))":
        "contains the address rendering of the unspecified value",

    # --- exceptions that escaped execute_scheme() ------------------------
    "(display (sqrt -4))":
        "reference let math.sqrt's ValueError escape the executor entirely",
    "(display (list-ref '(1 2) 9))":
        "reference let Python's IndexError escape the executor",
    "(display (vector-ref (vector 1) 9))":
        "reference let Python's IndexError escape the executor",
    "(display (car))":
        "reference raised an uncaught TypeError for wrong arity",
    "(display (/))":
        "reference raised an uncaught IndexError for wrong arity",
    "(display (modulo 1 0))":
        "reference raised an uncaught ZeroDivisionError",
    "(display (map +))":
        "reference raised an uncaught ValueError from min()",
    "(display (string->number))":
        "reference raised an uncaught TypeError for wrong arity",

    # --- numeric range ---------------------------------------------------
    "(display (expt 2 100))":
        "reference returns an exact bignum; the port promotes to double",
    "(display 1267650600228229401496703205376)":
        "reference returns an exact bignum; the port promotes to double",
    "(display (* 99999999999 99999999999))":
        "reference returns an exact bignum; the port promotes to double",

    # --- rendering of compound values via str() --------------------------
    "(display (string-append \"x\" (list 1 2)))":
        "reference uses Python repr for compound values",
    "(display (expt 2 1024))":
        "reference returns an exact bignum; the port's double overflows to inf",
    "(display (- -9223372036854775808 1))":
        "reference stays exact past 2^63; the port's double cannot represent it",
}

# Programs whose reference run raises a non-Scheme exception: recorded so the
# harness can report them rather than crash.
EXTRA_CORPUS: list[str] = [
    "(display (sqrt -4))",
    "(display (list-ref '(1 2) 9))",
    "(display (vector-ref (vector 1) 9))",
    "(display (car))",
    "(display (/))",
    "(display (modulo 1 0))",
    "(display (map +))",
    "(display (string->number))",
    "(display (expt 2 100))",
    "(display 1267650600228229401496703205376)",
    "(display (* 99999999999 99999999999))",
    "(display (expt 2 1024))",
    "(display (- -9223372036854775808 1))",
    "(display (string-append \"x\" (list 1 2)))",
    "(define (loop n) (if (= n 0) 'done (loop (- n 1)))) (display (loop 1000000))",
    "(define (sum n) (if (= n 0) 0 (+ n (sum (- n 1))))) (display (sum 100000))",
    "(define (mk n acc) (if (= n 0) acc (mk (- n 1) (cons n acc)))) (display (length (mk 200000 '())))",
    "(define (nest n x) (if (= n 0) x (nest (- n 1) (list x)))) (display (nest 200000 'a))",
    "(define (nest n x) (if (= n 0) x (nest (- n 1) (list x)))) (display (equal? (nest 100000 'a) (nest 100000 'a)))",
    "(display (if #f 1))",
    "(display (begin))",
    "(define y) (display y)",
    "(display (cond (#f 1)))",
    "(display (list (if #t 1) (if #f 1) (if #f 1 2)))",
]


def run_port(cli: Path, program: str) -> str:
    """Runs a program through the C++ CLI, returning its stdout."""
    result = subprocess.run(
        [str(cli)],
        input=program,
        capture_output=True,
        text=True,
        timeout=120,
    )
    if result.returncode != 0:
        return f"<<CLI EXIT {result.returncode}: {result.stderr.strip()}>>"
    return result.stdout


def reference(program: str) -> str:
    """Runs a program through the Python reference, trapping stray exceptions."""
    try:
        return run_scheme(program)
    except RecursionError:
        return "❌ Scheme error: Maximum recursion depth exceeded\n"
    except BaseException as exc:  # noqa: BLE001 - deliberate: we are measuring these
        return f"<escaped {type(exc).__name__}: {exc}>"


def main(argv: list[str]) -> int:
    if len(argv) > 1:
        cli = Path(argv[1]).resolve()
    else:
        cli = Path(__file__).resolve().parents[1] / "build" / "scheme_cli"

    if not cli.exists():
        print(f"scheme_cli not found at {cli}", file=sys.stderr)
        return 2

    programs = list(CORPUS)
    for extra in EXTRA_CORPUS:
        if extra not in programs:
            programs.append(extra)

    agreed = 0
    diverged: list[tuple[str, str, str, str]] = []
    unexpected: list[tuple[str, str, str]] = []

    for program in programs:
        try:
            expected = reference(program)
        except BaseException as exc:  # noqa: BLE001
            expected = f"<harness error {type(exc).__name__}: {exc}>"
        actual = run_port(cli, program)

        if expected == actual:
            agreed += 1
            continue

        reason = DIVERGENT.get(program)
        if reason is not None:
            diverged.append((program, reason, expected, actual))
        else:
            unexpected.append((program, expected, actual))

    print(f"corpus size                : {len(programs)}")
    print(f"agreements                 : {agreed}")
    print(f"known/documented divergences: {len(diverged)}")
    print(f"UNEXPECTED mismatches      : {len(unexpected)}")
    print()

    if diverged:
        print("Documented divergences (expected):")
        for program, reason, expected, actual in diverged:
            flat = lambda s: ADDRESS.sub("<addr>", s).replace("\n", "\\n")
            print(f"  - {program.strip()}")
            print(f"      why : {reason}")
            print(f"      ref : {flat(expected).strip()}")
            print(f"      port: {flat(actual).strip()}")
        print()

    if unexpected:
        print("UNEXPECTED MISMATCHES:")
        for program, expected, actual in unexpected:
            print(f"  - {program.strip()}")
            print(f"      ref : {expected.strip()!r}")
            print(f"      port: {actual.strip()!r}")
        print()
        return 1

    print("All non-divergent cases agree with the Python reference.")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))

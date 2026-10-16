#!/usr/bin/env python3
"""Establishes what the Python Scheme reference actually does.

Every finding here is checked against a real run, because porting by reading
code alone is how bugs get faithfully reproduced as bugs.
"""

from __future__ import annotations

import re
import sys

from scheme_reference import run_scheme

ADDRESS = re.compile(r"0x[0-9a-f]+")

CASES = [
    # -- core evaluation --------------------------------------------------
    ("arith", '(display (+ 1 2 3))'),
    ("arith-f", '(display (+ 1.0 2))'),
    ("int-vs-float", '(display (list (integer? 3) (integer? 3.0) (integer? (/ 6 3))))'),
    ("write-float", '(display (list 2.0 2.5 -0.0 (/ 1 2)))'),
    ("quotient", '(display (list (quotient 7 2) (quotient -7 2) (remainder -7 2) (modulo -7 2)))'),
    ("round", '(display (list (round 0.5) (round 1.5) (round 2.5) (round -0.5)))'),
    ("expt-big", '(display (expt 2 100))'),
    ("expt-neg", '(display (expt 2 -1))'),
    ("sqrt-neg", '(display (sqrt -4))'),
    # -- truthiness -------------------------------------------------------
    ("truthy", '(display (list (if 0 (quote t) (quote f)) (if (quote ()) (quote t) (quote f)) (if "" (quote t) (quote f))))'),
    ("not", '(display (list (not 0) (not #f) (not (quote ()))))'),
    # -- equality quirks --------------------------------------------------
    ("equal-bool-num", '(display (list (equal? #t 1) (equal? #f 0) (eqv? #t 1) (eq? #t 1)))'),
    ("equal-list", "(display (list (equal? '(1 2) '(1 2)) (eq? '(1 2) '(1 2))))"),
    ("equal-vec", '(display (equal? (vector 1 2) (vector 1 2)))'),
    # -- unspecified leaks an address -------------------------------------
    ("unspecified", '(display (if #f 1))'),
    ("begin-empty", '(display (begin))'),
    ("define-empty", '(define x) (display x)'),
    # -- special forms ----------------------------------------------------
    ("if-one-arm", '(display (list (if #t 1) (if #f 1)))'),
    ("and-or", '(display (list (and) (or) (and 1 2) (or #f 3) (and 1 #f) (or #f #f)))'),
    ("cond-arrow", '(display (cond ((assoc (quote b) (quote ((a 1) (b 2)))) => cadr) (else (quote nope))))'),
    ("cond-test-only", '(display (cond (7) (else 1)))'),
    ("case-basic", "(display (case 1 ((1) 'one) (else 'other)))"),
    ("case-else", "(display (case 9 ((1) 'one) (else 'other)))"),
    ("let-named", '(display (let loop ((i 0) (acc 1)) (if (= i 5) acc (loop (+ i 1) (* acc 2)))))'),
    ("let-star-seq", '(display (let* ((x 2) (y (* x 3))) (list x y)))'),
    ("letrec", '(display (letrec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))) (f 5)))'),
    ("do-loop", '(display (do ((i 0 (+ i 1)) (acc (quote ()) (cons i acc))) ((= i 4) acc)))'),
    ("do-init-sees-earlier", '(display (do ((i 3 (+ i 1)) (j i)) ((> i 3) (list i j))))'),
    ("when-unless", '(display (list (when #t 1 2) (unless #f 3)))'),
    ("quasi", '(display `(1 ,(+ 1 1) ,@(list 3 4) 5))'),
    ("quasi-nested", '(display `(a `(b ,(c ,(+ 1 2)))))'),
    ("quote-dotted", "(display '(1 . 2))"),
    ("quote-vec", "(display '#(1 2 3))"),
    ("defun-sugar", "(define (f x . r) (list x r)) (display (f 1 2 3))"),
    ("set-unbound", '(set! zzz 5) (display zzz)'),
    ("define-returns-name", '(display (define q 1))'),
    # -- arity ------------------------------------------------------------
    ("arity-missing", "((lambda (a b) (list a b)) 1)"),
    ("arity-extra", "((lambda (a) a) 1 2 3)"),
    # -- higher order -----------------------------------------------------
    ("map2", '(display (map + (quote (1 2 3)) (quote (10 20 30))))'),
    ("map-ragged", '(display (map + (quote (1 2 3)) (quote (10 20))))'),
    ("apply", "(display (apply + 1 2 '(3 4)))"),
    ("apply-none", "(display (apply list))"),
    ("filter-foldl", "(display (list (filter odd? '(1 2 3 4 5)) (foldl + 0 '(1 2 3 4))))"),
    # -- strings / chars --------------------------------------------------
    ("strings", '(display (list (string-length "abc") (substring "hello" 1 3) (substring "hello" 1 99) (string-append "a" "b")))'),
    ("str-slice-neg", '(display (substring "hello" -3 99))'),
    ("str-num", '(display (list (string->number "12") (string->number "1.5") (string->number "zz") (string->number "12" 16)))'),
    ("num-str", '(display (list (number->string 12) (number->string 1.5) (number->number 1)))'),
    ("char", '(display (list #\\a #\\space #\\newline))'),
    ("sym-str", '(display (list (symbol->string (quote abc)) (string->symbol "x")))'),
    ("str-case", '(string-upcase "ab") (display (list (string-upcase "ab") (string-downcase "AB")))'),
    ("string-ci", '(display (list (string-ci=? "AB" "ab") (string=? "a" "a") (string<? "a" "b")))'),
    ("string-list", '(display (string->list "abc"))'),
    # -- lists ------------------------------------------------------------
    ("list-ops", "(display (list (length '(1 2 3)) (reverse '(1 2 3)) (append '(1) '(2) '(3))))"),
    ("append-one", "(display (append '(1 2)))"),
    ("append-improper", "(display (append '(1) 2))"),
    ("append-none", "(display (append))"),
    ("member", "(display (list (member 2 '(1 2 3)) (memq 'c '(a b c)) (assoc 'b '((a 1) (b 2)))))"),
    ("member-false", "(display (list (member 9 '(1 2)) (assoc 9 '((a 1)))))"),
    ("list-ref-neg", "(display (list-ref '(1 2 3) -1))"),
    ("list-tail", "(display (list (list-tail '(1 2 3) 1) (list-tail '(1 2 3) 99)))"),
    ("caar-cadr", "(display (list (caar '((1 2) 3)) (cadr '(1 2 3)) (cdar '((1 2) 3)) (cddr '(1 2 3))))"),
    ("list-improper-error", "(display (length '(1 . 2)))"),
    ("list-ref-oob", "(display (list-ref '(1 2) 9))"),
    # -- vectors ----------------------------------------------------------
    ("vector-ops", "(display (list (vector-length (vector 1 2 3)) (vector-ref (vector 1 2 3) 1) (vector->list (vector 1 2)) (list->vector '(1 2))))"),
    ("make-vector", "(display (list (make-vector 3) (make-vector 2 9)))"),
    ("vector-display", "(display (list (vector 1 (vector 2 3)) (quote #(a b))))"),
    # -- errors -----------------------------------------------------------
    ("div-zero", "(display (/ 1 0))"),
    ("car-non-pair", "(display (car 5))"),
    ("unbound", "(display zzz)"),
    ("error-builtin", "(error 'bad \"thing\" 3)"),
    ("bad-syntax", "(display (+ 1"),
    ("unterminated-string", '(display "abc)'),
    ("non-proc-call", "(display (5 6))"),
    ("let-bad-bind", "(display (let ((a 1 2)) a))"),
    # -- recursion depth --------------------------------------------------
    ("deep-tail-200", "(define (loop n) (if (= n 0) 'done (loop (- n 1)))) (display (loop 200))"),
    ("deep-tail-5000", "(define (loop n) (if (= n 0) 'done (loop (- n 1)))) (display (loop 5000))"),
    ("deep-nontail-5000", "(define (sum n) (if (= n 0) 0 (+ n (sum (- n 1))))) (display (sum 5000))"),
    ("deep-list-1000", "(define (mk n acc) (if (= n 0) acc (mk (- n 1) (cons n acc)))) (display (length (mk 1000 '())))"),
]


def main() -> int:
    failures = 0
    for name, src in CASES:
        try:
            out = run_scheme(src)
        except BaseException as exc:  # noqa: BLE001 - probing escapes on purpose
            out = f"<<ESCAPED {type(exc).__name__}: {exc}>>"
            failures += 1
        flat = ADDRESS.sub("0xADDR", out).replace("\n", "\\n")
        print(f"{name:22} {flat}")
    print(f"\n{failures} case(s) escaped execute_scheme() as a non-Scheme exception")
    return 0


if __name__ == "__main__":
    sys.exit(main())

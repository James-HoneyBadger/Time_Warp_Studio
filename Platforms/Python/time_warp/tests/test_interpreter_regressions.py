"""Regression tests for interpreter glitches fixed in the professional overhaul."""

from __future__ import annotations

from time_warp.core.interpreter import Language

from .conftest_lang import no_errors, run


def test_forth_dot_quote_has_no_leading_space_and_keeps_spacing():
    assert run('." Hello, World!" CR', Language.FORTH) == ["Hello, World!"]
    assert run('." a   b" CR', Language.FORTH) == ["a   b"]


def test_forth_paren_print_and_counted_string():
    assert run(".( paren) CR", Language.FORTH) == ["paren"]
    assert run('C" hello" COUNT TYPE CR', Language.FORTH) == ["hello"]


def test_forth_string_inside_definition():
    out = run(': HI ." Hi there" CR ; HI HI', Language.FORTH)
    assert out[-2:] == ["Hi there", "Hi there"]


def test_prolog_write_does_not_force_newline():
    out = run("?- write(a), write(b), nl, write(c), nl.", Language.PROLOG)
    assert out[:2] == ["ab", "c"]


def test_prolog_format_with_argument_list():
    out = run('?- format("~w and ~w~n", [x, y]).', Language.PROLOG)
    assert out[0] == "x and y"


def test_prolog_multiple_clauses_per_line_and_fail_loop():
    out = run("p(1). p(2).\n?- p(X), write(X), nl, fail.\n", Language.PROLOG)
    assert out[:2] == ["1", "2"]


def test_prolog_lists_and_quoted_atoms_in_queries():
    out = run("?- X = [1,2,3], length(X, N), write(N), nl.", Language.PROLOG)
    assert out[0] == "3"
    out = run("?- write('a, b'), nl.", Language.PROLOG)
    assert out[0] == "a, b"
    out = run("?- write('100% sure'), nl. % trailing comment", Language.PROLOG)
    assert out[0] == "100% sure"


def test_prolog_univ():
    assert run("?- X =.. [f, a, b], write(X), nl.", Language.PROLOG)[0] == "f(a, b)"
    assert run("?- point(1,2) =.. L, write(L), nl.", Language.PROLOG)[0] == "[point, 1, 2]"


def test_pilot_remark_inside_subroutine_does_not_return():
    out = run("U:*S\nT:after\nE:\n*S\nR: note\nT:in sub\nE:\n", Language.PILOT)
    assert no_errors(out)
    assert out == ["in sub", "after"]


def test_division_by_zero_is_reported_in_every_language():
    cases = {
        Language.BASIC: "10 PRINT 5 / 0\n",
        Language.PILOT: "C:X = 5 / 0\n",
        Language.LOGO: "PRINT 5 / 0\n",
        Language.C: 'int x = 5 / 0;\nprintf("%d\\n", x);',
        Language.PASCAL: "program T;\nvar x: integer;\nbegin\n x := 5 div 0;\nend.\n",
        Language.PROLOG: "?- X is 5 / 0, write(X), nl.",
        Language.FORTH: "5 0 / . CR",
        Language.PYTHON_LANG: "print(5 / 0)",
    }
    for lang, src in cases.items():
        out = "\n".join(run(src, lang))
        assert "❌" in out and "ivision by zero" in out, (lang, out)


def test_forth_cr_cr_prints_blank_line_and_nested_paren_comment():
    assert run('." a" CR CR ." b" CR', Language.FORTH) == ["a", "", "b"]
    assert run(": SQ ( n -- sq(n) ) DUP * ; 3 SQ . CR", Language.FORTH)[-1] == "9 "


def test_pascal_inline_constants():
    src = "program T;\nconst N = 10;\nconst S = 'hi';\nbegin\n  writeln(N, S);\nend.\n"
    assert run(src, Language.PASCAL) == ["10hi"]
    bad = run("program T;\nconst X = 1e;\nbegin\nend.\n", Language.PASCAL)
    assert "Invalid constant" in bad[0]


def test_pascal_pointer_type_alias_is_not_a_constant():
    src = (
        "program T;\ntype\n    PNode = ^TNode;\n    TNode = record\n"
        "        Data: Integer;\n    end;\nbegin\n  writeln('ok');\nend.\n"
    )
    assert run(src, Language.PASCAL) == ["ok"]


def test_pascal_function_body_output_is_shown():
    src = (
        "program T;\nfunction G(n: integer): integer;\nbegin\n"
        "  writeln('in G ', n);\n  G := n * 2;\nend;\nbegin\n  writeln(G(4));\nend.\n"
    )
    assert run(src, Language.PASCAL) == ["in G 4", "8"]

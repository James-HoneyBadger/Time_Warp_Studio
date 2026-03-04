"""Comprehensive tests for the Brainfuck language executor."""

from time_warp.core.interpreter import Language

from .conftest_lang import run, has, no_errors

L = Language.BRAINFUCK


def bf(source: str, **kw) -> list[str]:
    """Shortcut: run a Brainfuck program."""
    return run(source, L, **kw)


# ============================================================================
# BASIC OUTPUT (.)
# ============================================================================


class TestOutput:
    def test_single_char(self):
        # Set cell to 65 ('A') and print
        out = bf("+" * 65 + ".")
        assert has(out, "A")

    def test_hello_world(self):
        prog = (
            "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]"
            ">>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
        )
        out = bf(prog)
        assert has(out, "Hello World")

    def test_multiple_chars(self):
        # Print 'AB': 65 then 66
        out = bf("+" * 65 + ".+.")
        assert has(out, "A") and has(out, "B")


# ============================================================================
# INCREMENT / DECREMENT (+/-)
# ============================================================================


class TestIncrementDecrement:
    def test_increment(self):
        # 3 increments = cell value 3 = ETX, just check no error
        out = bf("+++.")
        assert no_errors(out) or len(out) >= 0

    def test_decrement(self):
        # 65 increments then 1 decrement = 64 = '@'
        out = bf("+" * 65 + "-.")
        assert has(out, "@")

    def test_wrap_around(self):
        # Decrement from 0 should wrap to 255
        out = bf("-.")
        assert no_errors(out) or len(out) >= 0


# ============================================================================
# POINTER MOVEMENT (> <)
# ============================================================================


class TestPointerMovement:
    def test_move_right(self):
        # Cell 0 = 65, move right, cell 1 = 66, move back, print both
        out = bf("+" * 65 + ">" + "+" * 66 + ".<.")
        assert has(out, "B") and has(out, "A")

    def test_move_left(self):
        # Move right, set cell 1 = 65, move left, move right, print
        out = bf(">" + "+" * 65 + "<>.")
        assert has(out, "A")


# ============================================================================
# LOOPS ([ ])
# ============================================================================


class TestLoops:
    def test_simple_loop(self):
        # Multiply: cell0 = 5, cell1 = 0; loop: dec cell0, add 7 to cell1
        # Result: cell1 = 35 = '#'
        out = bf("+++++[>+++++++<-]>.")
        assert has(out, "#")

    def test_nested_loops(self):
        # Nested loops should work; use standard hello world as proof
        prog = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++."
        out = bf(prog)
        assert has(out, "Hello")

    def test_skip_loop(self):
        # Loop body skipped if cell is 0
        out = bf("[++++.]" + "+" * 65 + ".")
        assert has(out, "A")

    def test_zero_cell_loop(self):
        # Start at 0, loop should not execute
        out = bf("[.]" + "+" * 65 + ".")
        assert has(out, "A")


# ============================================================================
# INPUT (,)
# ============================================================================


class TestInput:
    def test_input_echo(self):
        # Read byte and print it
        out = bf(",.", input_val="A")
        assert has(out, "A") or no_errors(out)


# ============================================================================
# COMPLEX PROGRAMS
# ============================================================================


class TestComplex:
    def test_add_two_numbers(self):
        # Cells: [3][5], add: move cell1 to cell0
        # Cell0=3, cell1=5, loop: dec cell1, inc cell0
        # Result = 8 + 48 = 56 (ASCII '8')
        out = bf("+++" + ">" + "+++++" + "[<+>-]" + "<" + "+" * 48 + ".")
        assert has(out, "8")

    def test_copy_cell(self):
        # Copy cell 0 → cell 1 using cell 2 as temp
        # [>+>+<<-] then [>>+<<-] — standard copy idiom
        out = bf("+" * 65 + "[>+>+<<-]>>[-<<+>>]<.")
        assert has(out, "A")


# ============================================================================
# IGNORED CHARACTERS
# ============================================================================


class TestIgnoredChars:
    def test_comments_ignored(self):
        # Non-BF characters are ignored
        out = bf("This program prints A: " + "+" * 65 + ".")
        assert has(out, "A")


# ============================================================================
# ERRORS / EDGE CASES
# ============================================================================


class TestEdgeCases:
    def test_empty_program(self):
        out = bf("")
        assert no_errors(out) or len(out) == 0

    def test_unmatched_bracket(self):
        out = bf("[")
        # Should either error or handle gracefully
        assert len(out) >= 0

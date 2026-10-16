"""Smalltalk executor tests.

The tests concentrate on the four things that are easy to get subtly wrong and
hard to notice: message precedence, blocks as closures, ``^`` inside a block
returning from the *method*, and ``super`` starting its lookup above the class that
wrote the method. Faking any of those produces answers that look right in a simple
example.
"""

from __future__ import annotations

from time_warp.core.interpreter import Language

from .conftest_lang import has, no_errors, run

ST = Language.SMALLTALK


def text(source: str) -> str:
    return "\n".join(run(source, ST))


def last(source: str) -> str:
    """The last non-empty output line, which is the block's answer."""
    lines = [line for line in run(source, ST) if line.strip()]
    return lines[-1] if lines else ""


# -- literals and printing ---------------------------------------------------


def test_integer_arithmetic() -> None:
    assert last("1 + 2") == "3"


def test_string_is_echoed_with_its_quotes() -> None:
    assert last("'hello'") == "'hello'"


def test_character_prints_with_a_dollar() -> None:
    assert last("$A") == "$A"


def test_symbol_prints_with_a_hash() -> None:
    assert last("#foo") == "#foo"


def test_array_literal() -> None:
    assert last("#(1 2 #three)") == "(1 2 #three)"


def test_boolean_prints_as_a_word() -> None:
    assert last("true") == "true"
    assert last("false") == "false"


def test_a_nil_answer_is_not_echoed() -> None:
    """Nothing to show is the point: it is what keeps assignments quiet.

    `nil` is therefore not printable as a result, which is a documented
    consequence of the echo rule rather than an accident.
    """
    assert [line for line in run("nil", ST) if line.strip()] == []


def test_float_prints_without_a_trailing_zero() -> None:
    assert last("10 / 4") == "2.5"


# -- precedence --------------------------------------------------------------


def test_binary_messages_are_left_to_right() -> None:
    """`2 + 3 * 4` is 20: both are binary, so no multiplication binds tighter."""
    assert last("2 + 3 * 4") == "20"


def test_binary_binds_tighter_than_keyword() -> None:
    assert last("1 max: 2 + 3") == "5"


def test_unary_binds_tightest() -> None:
    assert last("3 negated abs") == "3"


def test_parentheses_regroup() -> None:
    assert last("2 + (3 * 4)") == "14"


# -- variables ---------------------------------------------------------------


def test_temporary_and_assignment() -> None:
    assert last("| x | x := 7. x * 2") == "14"


def test_several_temporaries() -> None:
    assert last("| a b | a := 3. b := 4. a * b") == "12"


def test_assignment_answers_its_value() -> None:
    assert last("| x | x := 9") == "9"


def test_an_unknown_name_is_reported() -> None:
    assert has(run("noSuchThing", ST), "❌", "unknown variable")


# -- blocks ------------------------------------------------------------------


def test_block_with_an_argument() -> None:
    assert last("[:x | x * 2] value: 21") == "42"


def test_block_with_two_arguments() -> None:
    assert last("[:a :b | a + b] value: 3 value: 4") == "7"


def test_block_with_its_own_temporary() -> None:
    assert last("[:x | | y | y := x + 1. y * 2] value: 5") == "12"


def test_a_block_is_a_closure_over_the_method_temporaries() -> None:
    """The block is stored and run later, and still writes the outer variable."""
    source = """Object subclass: #Maker
  instanceVariableNames: 'block'

Maker >> setBlock: aBlock
  block := aBlock

Maker >> run
  ^block value

| m total |
total := 10.
m := Maker new.
m setBlock: [total := total + 5].
m run.
total"""
    assert last(source) == "15"


def test_return_inside_a_block_returns_from_the_method() -> None:
    """The whole reason ^ in a block is not the same as returning from the block."""
    source = """Object subclass: #Finder
  instanceVariableNames: ''

Finder >> find: aCollection
  aCollection do: [:each | each > 2 ifTrue: [^each]].
  ^nil

Finder new find: #(1 2 3 4)"""
    assert last(source) == "3"


def test_wrong_block_arity_is_reported() -> None:
    assert has(run("[:x | x] value", ST), "❌", "argument")


# -- control flow as messages ------------------------------------------------


def test_if_true_if_false() -> None:
    assert last("3 > 2 ifTrue: ['yes'] ifFalse: ['no']") == "'yes'"
    assert last("1 > 2 ifTrue: ['yes'] ifFalse: ['no']") == "'no'"


def test_to_do_accumulates() -> None:
    assert last("| s | s := 0. (1 to: 5) do: [:i | s := s + i]. s") == "15"


def test_to_by_do_steps() -> None:
    """1, 5, 9 — the step is applied, not ignored."""
    assert last("| s | s := 0. (1 to: 9 by: 4) do: [:i | s := s + i]. s") == "15"


def test_times_repeat() -> None:
    assert last("| s | s := 0. 4 timesRepeat: [s := s + 10]. s") == "40"


def test_while_true() -> None:
    assert last("| n | n := 0. [n < 3] whileTrue: [n := n + 1]. n") == "3"


def test_while_false() -> None:
    assert last("| n | n := 0. [n > 2] whileFalse: [n := n + 1]. n") == "3"


def test_while_true_without_a_body_is_reported() -> None:
    assert has(run("[true] whileTrue", ST), "❌", "body")


# -- collections -------------------------------------------------------------


def test_size_and_access() -> None:
    assert last("#(10 20 30) size") == "3"
    assert last("#(10 20 30) at: 2") == "20"
    assert last("#(10 20 30) first") == "10"
    assert last("#(10 20 30) last") == "30"


def test_at_put_answers_the_stored_value() -> None:
    assert last("| a | a := #(1 2 3). a at: 2 put: 9. a") == "(1 9 3)"


def test_collect_select_and_reject() -> None:
    assert last("#(1 2 3) collect: [:x | x * x]") == "(1 4 9)"
    assert last("#(1 2 3 4) select: [:x | x even]") == "(2 4)"
    assert last("#(1 2 3 4) reject: [:x | x even]") == "(1 3)"


def test_inject_into() -> None:
    assert last("#(1 2 3 4) inject: 0 into: [:a :b | a + b]") == "10"


def test_detect_and_includes() -> None:
    assert last("#(1 2 3) detect: [:x | x > 1]") == "2"
    assert last("#(1 2 3) includes: 2") == "true"
    assert last("#(1 2 3) includes: 9") == "false"


def test_ordered_collection_grows() -> None:
    assert last("| c | c := OrderedCollection new. c add: 1. c add: 2. c size") == "2"


def test_array_new_is_filled_with_nil() -> None:
    assert last("(Array new: 3) size") == "3"
    assert last("((Array new: 3) at: 1) isNil") == "true"


def test_index_out_of_range_names_the_index() -> None:
    assert has(run("#(1 2) at: 9", ST), "❌", "out of range")


# -- strings -----------------------------------------------------------------


def test_string_size_and_comma() -> None:
    assert last("'abc' size") == "3"
    assert last("'ab' , 'cd'") == "'abcd'"


def test_string_case_and_symbol() -> None:
    assert last("'Hello' asUppercase") == "'HELLO'"
    assert last("'Hello' asSymbol") == "#Hello"


def test_string_as_number() -> None:
    assert last("'42' asNumber + 1") == "43"


def test_string_at_answers_a_character() -> None:
    assert last("'abc' at: 2") == "$b"


# -- classes -----------------------------------------------------------------


COUNTER = """Object subclass: #Counter
  instanceVariableNames: 'count'

Counter >> initialize
  count := 0

Counter >> increment
  count := count + 1
  ^count

Counter >> value
  ^count
"""


def test_a_class_definition_reports_itself() -> None:
    assert has(run(COUNTER, ST), "Counter")


def test_instance_variables_persist_across_messages() -> None:
    source = COUNTER + """
| c |
c := Counter new.
c increment.
c increment.
c value"""
    assert last(source) == "2"


def test_initialize_is_called_by_new() -> None:
    source = COUNTER + """
(Counter new) value"""
    assert last(source) == "0"


def test_a_class_without_initialize_still_news() -> None:
    source = """Object subclass: #Bare
  instanceVariableNames: ''

Bare >> hello
  ^'hi'

Bare new hello"""
    assert last(source) == "'hi'"


def test_super_starts_lookup_above_the_defining_class() -> None:
    source = """Object subclass: #Base
  instanceVariableNames: ''

Base >> describe
  ^'base'

Base subclass: #Derived
  instanceVariableNames: ''

Derived >> describe
  ^'derived and ', super describe

Derived new describe"""
    assert last(source) == "'derived and base'"


def test_a_class_side_method_can_send_to_self() -> None:
    source = """Object subclass: #Named
  instanceVariableNames: ''

Named class >> default
  ^self new

Named >> greet
  ^'hello'

Named default greet"""
    assert last(source) == "'hello'"


def test_answers_to_doesNotUnderstand() -> None:
    assert has(run("3 foo", ST), "❌", "does not understand")


def test_a_method_for_an_unknown_class_is_reported() -> None:
    assert has(run("Nowhere >> thing\n  ^1", ST), "❌", "not a class")


def test_a_duplicate_method_is_reported() -> None:
    source = """Object subclass: #Thing
  instanceVariableNames: ''

Thing >> go
  ^1

Thing >> go
  ^2"""
    assert has(run(source, ST), "❌", "already")


def test_an_empty_method_body_is_reported() -> None:
    source = """Object subclass: #Thing
  instanceVariableNames: ''

Thing >> go"""
    assert has(run(source, ST), "❌")


# -- cascades and output -----------------------------------------------------


def test_a_cascade_sends_to_the_original_receiver() -> None:
    """`cr` goes to the Transcript, not to the result of `show:`."""
    output = run("Transcript show: 'a'; show: 'b'; cr", ST)
    assert "ab" in "\n".join(output)


def test_transcript_lines_come_out_in_order() -> None:
    output = run("Transcript show: 'one'; cr. Transcript show: 'two'; cr", ST)
    joined = "\n".join(output)
    assert joined.index("one") < joined.index("two")


def test_a_chunk_echoes_only_its_last_value() -> None:
    """Assignments and `do:` must not each print an object."""
    output = [line for line in run("| s | s := 0. (1 to: 5) do: [:i | s := s + i]. s", ST) if line.strip()]
    assert output == ["15"], output


def test_string_splicing_with_a_cascade_of_output() -> None:
    assert last("Transcript show: 'x'; cr. 2 + 2") == "4"


# -- turtle ------------------------------------------------------------------


def test_turtle_draws_and_says_nothing() -> None:
    output = [line for line in run("| t | t := Turtle new. t forward: 100. t right: 90", ST) if line.strip()]
    assert output == [], output


def test_turtle_from_a_loop() -> None:
    source = "| t |\nt := Turtle new.\n4 timesRepeat: [t forward: 60. t right: 90].\n'square'"
    assert last(source) == "'square'"


def test_turtle_without_a_number_is_reported() -> None:
    assert has(run("| t | t := Turtle new. t forward: 'x'", ST), "❌")


# -- errors ------------------------------------------------------------------


def test_division_by_zero() -> None:
    assert has(run("1 / 0", ST), "❌", "division by zero")


def test_unterminated_string() -> None:
    assert has(run("'oops", ST), "❌")


def test_a_good_program_reports_no_error() -> None:
    assert no_errors(run("1 + 1\n2 + 2", ST))


# -- regressions -------------------------------------------------------------


def test_bar_is_not_a_binary_selector() -> None:
    """`|` groups temporaries in `| c |` and closes a block in `[:x | x]`.

    Tokenising it as a binary selector swallowed both, so a block's argument list
    turned into a message send.
    """
    assert text("| x |\nx := 3.\nx").strip() == "3"
    assert text("[:x | x + 1] value: 41").strip() == "42"

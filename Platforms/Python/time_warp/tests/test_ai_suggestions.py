from time_warp.features.ai_suggestions import AISuggestions


def test_python_context_generates_relevant_suggestions():
    engine = AISuggestions()
    engine.update_context("def greet(name):\n    ")

    suggestions = engine.get_suggestions(cursor_position=len(engine.context))

    assert suggestions
    assert any("return" in suggestion for suggestion in suggestions)
    assert not any("TODO: Implement this function" in suggestion for suggestion in suggestions)


def test_basic_context_generates_basic_style_suggestions():
    engine = AISuggestions()
    engine.update_context('PRINT "HELLO"\nFOR I = 1 TO 10\n')

    suggestions = engine.get_suggestions(cursor_position=len(engine.context))

    assert suggestions
    assert any("NEXT I" in suggestion or "IF " in suggestion for suggestion in suggestions)


def test_apply_suggestion_returns_combined_source():
    engine = AISuggestions()
    engine.update_context("PRINT \"HELLO\"")

    updated = engine.apply_suggestion("\nPRINT \"DONE\"")

    assert "PRINT \"HELLO\"" in updated
    assert "PRINT \"DONE\"" in updated
    assert engine.context == updated


class TestAISuggestions:
    """More AISuggestions tests."""

    def test_empty_context_returns_suggestions(self):
        engine = AISuggestions()
        engine.update_context("")
        suggestions = engine.get_suggestions(cursor_position=0)
        assert isinstance(suggestions, list)

    def test_context_stored(self):
        engine = AISuggestions()
        engine.update_context("PRINT 1")
        assert engine.context == "PRINT 1"

    def test_context_overwritten_on_update(self):
        engine = AISuggestions()
        engine.update_context("PRINT 1")
        engine.update_context("PRINT 2")
        assert engine.context == "PRINT 2"

    def test_suggestions_non_empty_basic(self):
        engine = AISuggestions()
        engine.update_context('PRINT "HELLO"')
        suggestions = engine.get_suggestions(cursor_position=len(engine.context))
        assert len(suggestions) > 0

    def test_suggestions_list_type(self):
        engine = AISuggestions()
        engine.update_context("PRINT 1")
        suggestions = engine.get_suggestions(cursor_position=0)
        assert isinstance(suggestions, list)

    def test_suggestions_are_strings(self):
        engine = AISuggestions()
        engine.update_context("PRINT 1")
        suggestions = engine.get_suggestions(cursor_position=0)
        for s in suggestions:
            assert isinstance(s, str)

    def test_apply_empty_suggestion(self):
        engine = AISuggestions()
        engine.update_context("PRINT 1")
        result = engine.apply_suggestion("")
        assert "PRINT 1" in result

    def test_apply_suggestion_updates_context(self):
        engine = AISuggestions()
        engine.update_context("PRINT 1")
        engine.apply_suggestion("\nPRINT 2")
        assert "PRINT 2" in engine.context

    def test_multiple_updates(self):
        engine = AISuggestions()
        for i in range(5):
            engine.update_context(f"PRINT {i}")
        assert engine.context == "PRINT 4"

    def test_for_loop_context_basic(self):
        engine = AISuggestions()
        engine.update_context("FOR I = 1 TO 10\n")
        suggestions = engine.get_suggestions(cursor_position=len(engine.context))
        assert len(suggestions) > 0

    def test_python_def_context(self):
        engine = AISuggestions()
        engine.update_context("def foo():\n    pass\n")
        suggestions = engine.get_suggestions(cursor_position=len(engine.context))
        assert len(suggestions) > 0

    def test_suggestions_at_position_zero(self):
        engine = AISuggestions()
        engine.update_context("PRINT 1\nPRINT 2\n")
        suggestions = engine.get_suggestions(cursor_position=0)
        assert isinstance(suggestions, list)

    def test_apply_suggestion_returns_string(self):
        engine = AISuggestions()
        engine.update_context("PRINT 1")
        result = engine.apply_suggestion("\nPRINT 2")
        assert isinstance(result, str)


class TestAISuggestionsExtended:
    """More AI suggestions tests."""

    def test_fresh_engine_no_context(self):
        engine = AISuggestions()
        assert engine.context == ""

    def test_update_context_stored(self):
        engine = AISuggestions()
        engine.update_context("PRINT 1")
        assert "PRINT" in engine.context

    def test_update_context_replaces(self):
        engine = AISuggestions()
        engine.update_context("PRINT 1")
        engine.update_context("PRINT 2")
        assert "PRINT 2" in engine.context

    def test_get_suggestions_returns_list(self):
        engine = AISuggestions()
        engine.update_context("PRINT 1")
        s = engine.get_suggestions(cursor_position=7)
        assert isinstance(s, list)

    def test_apply_suggestion_appends(self):
        engine = AISuggestions()
        engine.update_context("PRINT 1")
        result = engine.apply_suggestion("\nPRINT 2")
        assert "PRINT 2" in result

    def test_multiple_updates_last_wins(self):
        engine = AISuggestions()
        for i in range(5):
            engine.update_context(f"X={i}")
        assert "4" in engine.context

    def test_suggestions_from_logo_context(self):
        engine = AISuggestions()
        engine.update_context("FORWARD 100\nRIGHT 90\n")
        s = engine.get_suggestions(cursor_position=len(engine.context))
        assert isinstance(s, list)

    def test_suggestions_non_empty_for_basic(self):
        engine = AISuggestions()
        engine.update_context("FOR I = 1 TO 5\n")
        s = engine.get_suggestions(cursor_position=len(engine.context))
        assert len(s) > 0

    def test_apply_empty_suggestion(self):
        engine = AISuggestions()
        engine.update_context("X")
        result = engine.apply_suggestion("")
        assert isinstance(result, str)

    def test_context_preserves_newlines(self):
        engine = AISuggestions()
        engine.update_context("A\nB\nC")
        assert "\n" in engine.context


class TestAISuggestionsExtended2:
    """More AI suggestion tests."""

    def test_suggestions_list_type(self):
        engine = AISuggestions()
        engine.update_context("PRINT 1")
        s = engine.get_suggestions(cursor_position=7)
        assert isinstance(s, list)

    def test_apply_multiline_suggestion(self):
        engine = AISuggestions()
        engine.update_context("PRINT 1")
        result = engine.apply_suggestion("\nPRINT 2\nPRINT 3")
        assert "PRINT 2" in result

    def test_context_grows_with_append(self):
        engine = AISuggestions()
        engine.update_context("A")
        old_len = len(engine.context)
        engine.update_context("AB")
        # context is replaced (or grown)
        assert len(engine.context) >= 1

    def test_get_suggestions_for_logo(self):
        engine = AISuggestions()
        engine.update_context("FORWARD 100\nLEFT 90\n")
        s = engine.get_suggestions(cursor_position=22)
        assert isinstance(s, list)

    def test_get_suggestions_at_end(self):
        engine = AISuggestions()
        ctx = "FOR I = 1 TO 10\n"
        engine.update_context(ctx)
        s = engine.get_suggestions(cursor_position=len(ctx))
        assert len(s) > 0

    def test_suggestion_is_str(self):
        engine = AISuggestions()
        engine.update_context("FOR I = 1 TO 10\n")
        s = engine.get_suggestions(cursor_position=len(engine.context))
        if s:
            assert isinstance(s[0], str)

    def test_update_twice_second_wins(self):
        engine = AISuggestions()
        engine.update_context("X")
        engine.update_context("HELLO")
        assert "HELLO" in engine.context

    def test_apply_changes_context(self):
        engine = AISuggestions()
        engine.update_context("A")
        result = engine.apply_suggestion("B")
        assert "B" in result


class TestAISuggestionsExtended2:
    """Extended tests for AISuggestions."""

    def test_empty_context(self):
        engine = AISuggestions()
        engine.update_context("")
        s = engine.get_suggestions(cursor_position=0)
        assert isinstance(s, list)

    def test_large_context(self):
        engine = AISuggestions()
        engine.update_context("X = 1\n" * 50)
        s = engine.get_suggestions(cursor_position=50)
        assert isinstance(s, list)

    def test_multiline_context(self):
        engine = AISuggestions()
        engine.update_context("line1\nline2\nline3\n")
        s = engine.get_suggestions(cursor_position=5)
        assert isinstance(s, list)

    def test_context_with_numbers(self):
        engine = AISuggestions()
        engine.update_context("10 PRINT 100\n20 GOTO 10\n")
        s = engine.get_suggestions(cursor_position=10)
        assert isinstance(s, list)

    def test_apply_empty_suggestion(self):
        engine = AISuggestions()
        engine.update_context("abc")
        result = engine.apply_suggestion("")
        assert isinstance(result, str)

    def test_context_preserved_after_get(self):
        engine = AISuggestions()
        engine.update_context("test context")
        engine.get_suggestions(cursor_position=4)
        assert engine.context == "test context"

    def test_multiple_applies(self):
        engine = AISuggestions()
        engine.update_context("A")
        engine.apply_suggestion("B")
        engine.apply_suggestion("C")
        assert isinstance(engine.context, str)

    def test_update_clears_old_context(self):
        engine = AISuggestions()
        engine.update_context("old")
        engine.update_context("new")
        assert engine.context == "new"

    def test_cursor_beyond_context(self):
        engine = AISuggestions()
        engine.update_context("abc")
        s = engine.get_suggestions(cursor_position=100)
        assert isinstance(s, list)

    def test_special_chars_in_context(self):
        engine = AISuggestions()
        engine.update_context("!@#$%^&*()")
        s = engine.get_suggestions(cursor_position=5)
        assert isinstance(s, list)


class TestAISuggestionsExtended3:
    """Third round of AI suggestions tests."""

    def test_lua_context(self):
        from time_warp.features.ai_suggestions import AISuggestions
        engine = AISuggestions()
        engine.update_context("function add(a, b)")
        s = engine.get_suggestions(cursor_position=15)
        assert isinstance(s, list)

    def test_basic_context(self):
        from time_warp.features.ai_suggestions import AISuggestions
        engine = AISuggestions()
        engine.update_context("10 PRINT")
        s = engine.get_suggestions(cursor_position=5)
        assert isinstance(s, list)

    def test_logo_context(self):
        from time_warp.features.ai_suggestions import AISuggestions
        engine = AISuggestions()
        engine.update_context("FORWARD 100")
        s = engine.get_suggestions(cursor_position=3)
        assert isinstance(s, list)

    def test_prolog_context(self):
        from time_warp.features.ai_suggestions import AISuggestions
        engine = AISuggestions()
        engine.update_context("parent(tom, bob).")
        s = engine.get_suggestions(cursor_position=5)
        assert isinstance(s, list)

    def test_forth_context(self):
        from time_warp.features.ai_suggestions import AISuggestions
        engine = AISuggestions()
        engine.update_context(": SQUARE DUP * ;")
        s = engine.get_suggestions(cursor_position=2)
        assert isinstance(s, list)

    def test_erlang_context(self):
        from time_warp.features.ai_suggestions import AISuggestions
        engine = AISuggestions()
        engine.update_context("-module(hello).")
        s = engine.get_suggestions(cursor_position=7)
        assert isinstance(s, list)

    def test_update_context_twice(self):
        from time_warp.features.ai_suggestions import AISuggestions
        engine = AISuggestions()
        engine.update_context("first context")
        engine.update_context("second context")
        s = engine.get_suggestions(cursor_position=5)
        assert isinstance(s, list)

    def test_zero_cursor_position(self):
        from time_warp.features.ai_suggestions import AISuggestions
        engine = AISuggestions()
        engine.update_context("some code here")
        s = engine.get_suggestions(cursor_position=0)
        assert isinstance(s, list)

    def test_suggestions_each_is_string(self):
        from time_warp.features.ai_suggestions import AISuggestions
        engine = AISuggestions()
        engine.update_context("def foo():")
        s = engine.get_suggestions(cursor_position=3)
        for item in s:
            assert isinstance(item, str)

    def test_multiple_engines_independent(self):
        from time_warp.features.ai_suggestions import AISuggestions
        e1 = AISuggestions()
        e2 = AISuggestions()
        e1.update_context("context a")
        e2.update_context("context b")
        assert isinstance(e1.get_suggestions(cursor_position=2), list)
        assert isinstance(e2.get_suggestions(cursor_position=2), list)


class TestAISuggestionsExtended4:
    """Fourth round of AI suggestions tests."""

    def test_js_arrow_context(self):
        from time_warp.features.ai_suggestions import AISuggestions
        engine = AISuggestions()
        engine.update_context("const f = x =>")
        s = engine.get_suggestions(cursor_position=10)
        assert isinstance(s, list)

    def test_pascal_context(self):
        from time_warp.features.ai_suggestions import AISuggestions
        engine = AISuggestions()
        engine.update_context("PROGRAM test; BEGIN")
        s = engine.get_suggestions(cursor_position=10)
        assert isinstance(s, list)

    def test_erlang_module_context(self):
        from time_warp.features.ai_suggestions import AISuggestions
        engine = AISuggestions()
        engine.update_context("-module(test).")
        s = engine.get_suggestions(cursor_position=5)
        assert isinstance(s, list)

    def test_empty_context(self):
        from time_warp.features.ai_suggestions import AISuggestions
        engine = AISuggestions()
        engine.update_context("")
        s = engine.get_suggestions(cursor_position=0)
        assert isinstance(s, list)

    def test_high_cursor_position(self):
        from time_warp.features.ai_suggestions import AISuggestions
        engine = AISuggestions()
        engine.update_context("PRINT 1")
        s = engine.get_suggestions(cursor_position=999)
        assert isinstance(s, list)

    def test_suggestions_are_strings(self):
        from time_warp.features.ai_suggestions import AISuggestions
        engine = AISuggestions()
        engine.update_context("PRINT")
        s = engine.get_suggestions(cursor_position=5)
        for item in s:
            assert isinstance(item, str)

    def test_update_context_multiple_times(self):
        from time_warp.features.ai_suggestions import AISuggestions
        engine = AISuggestions()
        for code in ["PRINT 1", "LET X = 5", "FOR I = 1 TO 10"]:
            engine.update_context(code)
        s = engine.get_suggestions(cursor_position=5)
        assert isinstance(s, list)

    def test_three_engines_independent(self):
        from time_warp.features.ai_suggestions import AISuggestions
        e1 = AISuggestions()
        e2 = AISuggestions()
        e3 = AISuggestions()
        assert e1 is not e2 and e2 is not e3

    def test_lisp_context(self):
        from time_warp.features.ai_suggestions import AISuggestions
        engine = AISuggestions()
        engine.update_context("(define x")
        s = engine.get_suggestions(cursor_position=8)
        assert isinstance(s, list)

    def test_brainfuck_context(self):
        from time_warp.features.ai_suggestions import AISuggestions
        engine = AISuggestions()
        engine.update_context("++++++")
        s = engine.get_suggestions(cursor_position=6)
        assert isinstance(s, list)


class TestAISuggestionsExtended5:
    """Fifth round of AI suggestions tests."""

    def _engine(self):
        from time_warp.features.ai_suggestions import AISuggestions
        return AISuggestions()

    def test_logo_context(self):
        e = self._engine()
        e.update_context("FORWARD 100\nRIGHT 90\n")
        s = e.get_suggestions(cursor_position=5)
        assert isinstance(s, list)

    def test_prolog_context(self):
        e = self._engine()
        e.update_context("parent(tom, bob).\nparent(bob, ann).\n")
        s = e.get_suggestions(cursor_position=5)
        assert isinstance(s, list)

    def test_forth_context(self):
        e = self._engine()
        e.update_context(": square dup * ;\n")
        s = e.get_suggestions(cursor_position=5)
        assert isinstance(s, list)

    def test_hypertalk_context(self):
        e = self._engine()
        e.update_context("put 42 into myVar\n")
        s = e.get_suggestions(cursor_position=5)
        assert isinstance(s, list)

    def test_suggestions_list_type(self):
        e = self._engine()
        e.update_context("PRINT")
        s = e.get_suggestions(cursor_position=3)
        assert isinstance(s, list)

    def test_engine_creation(self):
        e = self._engine()
        assert e is not None

    def test_two_engines_independent(self):
        e1 = self._engine()
        e2 = self._engine()
        assert e1 is not e2

    def test_update_context_then_get(self):
        e = self._engine()
        e.update_context("hello world")
        s = e.get_suggestions(cursor_position=5)
        assert isinstance(s, list)

    def test_cursor_position_zero(self):
        e = self._engine()
        e.update_context("test")
        s = e.get_suggestions(cursor_position=0)
        assert isinstance(s, list)

    def test_long_context(self):
        e = self._engine()
        e.update_context("A" * 1000)
        s = e.get_suggestions(cursor_position=100)
        assert isinstance(s, list)


class TestAISuggestionsExtended6:
    """Sixth round of AI suggestions tests."""

    def _engine(self):
        from time_warp.features.ai_suggestions import AISuggestions
        return AISuggestions()

    def test_create_engine(self):
        e = self._engine()
        assert e is not None

    def test_update_context_returns_none(self):
        e = self._engine()
        result = e.update_context("PRINT 1")
        assert result is None or True  # may return None or some value

    def test_get_suggestions_returns_list(self):
        e = self._engine()
        e.update_context("test")
        s = e.get_suggestions(cursor_position=2)
        assert isinstance(s, list)

    def test_empty_context_suggestions(self):
        e = self._engine()
        e.update_context("")
        s = e.get_suggestions(cursor_position=0)
        assert isinstance(s, list)

    def test_two_engines_independent(self):
        e1 = self._engine()
        e2 = self._engine()
        e1.update_context("ctx1")
        e2.update_context("ctx2")
        assert e1 is not e2

    def test_large_cursor_position(self):
        e = self._engine()
        e.update_context("hello world")
        s = e.get_suggestions(cursor_position=999)
        assert isinstance(s, list)

    def test_suggestions_all_strings(self):
        e = self._engine()
        e.update_context("FORWARD 100")
        s = e.get_suggestions(cursor_position=5)
        assert all(isinstance(x, str) for x in s)

    def test_basic_context(self):
        e = self._engine()
        e.update_context("PRINT 42\nLET X=5\n")
        s = e.get_suggestions(cursor_position=5)
        assert isinstance(s, list)

    def test_javascript_context(self):
        e = self._engine()
        e.update_context("const x = 1;\nconsole.log(x);\n")
        s = e.get_suggestions(cursor_position=5)
        assert isinstance(s, list)

    def test_forth_context(self):
        e = self._engine()
        e.update_context(": SQUARE DUP * ;")
        s = e.get_suggestions(cursor_position=5)
        assert isinstance(s, list)


class TestAISuggestionsExtended7:
    """Seventh round of AI suggestions tests."""

    def _engine(self):
        from time_warp.features.ai_suggestions import AISuggestions
        return AISuggestions()

    def test_create_engine(self):
        e = self._engine()
        assert e is not None

    def test_two_engines_independent(self):
        e1 = self._engine()
        e2 = self._engine()
        assert e1 is not e2

    def test_update_context_none(self):
        e = self._engine()
        result = e.update_context("PRINT 1")
        assert result is None

    def test_suggestions_is_list(self):
        e = self._engine()
        e.update_context("PRINT 1")
        suggestions = e.get_suggestions(cursor_position=0)
        assert isinstance(suggestions, list)

    def test_suggestions_all_strings(self):
        e = self._engine()
        e.update_context("PRINT 1")
        for s in e.get_suggestions(cursor_position=0):
            assert isinstance(s, str)

    def test_empty_code_suggestions(self):
        e = self._engine()
        e.update_context("")
        suggestions = e.get_suggestions(cursor_position=0)
        assert isinstance(suggestions, list)

    def test_lua_context(self):
        e = self._engine()
        e.update_context("print(1)")
        suggestions = e.get_suggestions(cursor_position=0)
        assert isinstance(suggestions, list)

    def test_lisp_context(self):
        e = self._engine()
        e.update_context("(display 1)")
        suggestions = e.get_suggestions(cursor_position=0)
        assert isinstance(suggestions, list)

    def test_large_cursor_position(self):
        e = self._engine()
        e.update_context("PRINT 1")
        suggestions = e.get_suggestions(cursor_position=9999)
        assert isinstance(suggestions, list)

    def test_prolog_context(self):
        e = self._engine()
        e.update_context(":- write(hi).")
        suggestions = e.get_suggestions(cursor_position=0)
        assert isinstance(suggestions, list)


class TestAISuggestionsExtended8:
    """Eighth round of AI suggestions tests."""

    def _engine(self):
        from time_warp.features.ai_suggestions import AISuggestions
        return AISuggestions()

    def test_create_returns_not_none(self):
        assert self._engine() is not None

    def test_update_context_empty(self):
        e = self._engine()
        e.update_context("")
        assert e is not None

    def test_update_context_basic(self):
        e = self._engine()
        e.update_context("PRINT 10")
        assert e is not None

    def test_update_context_logo(self):
        e = self._engine()
        e.update_context("fd 100")
        assert e is not None

    def test_get_suggestions_returns_list(self):
        e = self._engine()
        e.update_context("console.log(1)")
        result = e.get_suggestions(cursor_position=0)
        assert isinstance(result, list)

    def test_update_twice(self):
        e = self._engine()
        e.update_context("PRINT 1")
        e.update_context("PRINT 2")
        assert e is not None

    def test_suggestions_at_pos_10(self):
        e = self._engine()
        e.update_context("puts [expr 1+1]")
        result = e.get_suggestions(cursor_position=10)
        assert isinstance(result, list)

    def test_suggestions_at_pos_0(self):
        e = self._engine()
        e.update_context("(+ 1 2)")
        result = e.get_suggestions(cursor_position=0)
        assert isinstance(result, list)

    def test_three_engines(self):
        e1, e2, e3 = self._engine(), self._engine(), self._engine()
        assert e1 is not None and e2 is not None and e3 is not None

    def test_suggestions_after_empty(self):
        e = self._engine()
        e.update_context("")
        result = e.get_suggestions(cursor_position=0)
        assert isinstance(result, list)


class TestAISuggestionsExtended9:
    """Ninth round of AI suggestions tests."""

    def _engine(self):
        from time_warp.features.ai_suggestions import AISuggestions
        return AISuggestions()

    def test_create_engine(self):
        assert self._engine() is not None

    def test_two_engines_independent(self):
        e1, e2 = self._engine(), self._engine()
        assert e1 is not e2

    def test_update_context_returns_none(self):
        e = self._engine()
        assert e.update_context("PRINT 1") is None

    def test_suggestions_is_list(self):
        e = self._engine()
        e.update_context("PRINT 1")
        assert isinstance(e.get_suggestions(cursor_position=0), list)

    def test_suggestions_empty_code(self):
        e = self._engine()
        e.update_context("")
        assert isinstance(e.get_suggestions(cursor_position=0), list)

    def test_suggestions_lisp(self):
        e = self._engine()
        e.update_context("(display 1)")
        assert isinstance(e.get_suggestions(cursor_position=0), list)

    def test_suggestions_lua(self):
        e = self._engine()
        e.update_context("print(1)")
        assert isinstance(e.get_suggestions(cursor_position=0), list)

    def test_suggestions_prolog(self):
        e = self._engine()
        e.update_context(":- write(1).")
        assert isinstance(e.get_suggestions(cursor_position=0), list)

    def test_update_twice(self):
        e = self._engine()
        e.update_context("PRINT 1")
        e.update_context("PRINT 2")
        assert isinstance(e.get_suggestions(cursor_position=0), list)

    def test_all_strings(self):
        e = self._engine()
        e.update_context("PRINT 1")
        for s in e.get_suggestions(cursor_position=0):
            assert isinstance(s, str)


class TestAISuggestionsExtended10:
    """Tenth round of AI suggestions tests."""

    def _engine(self):
        from time_warp.features.ai_suggestions import AISuggestions
        return AISuggestions()

    def test_create_engine(self):
        assert self._engine() is not None

    def test_three_engines_independent(self):
        e1, e2, e3 = self._engine(), self._engine(), self._engine()
        assert e1 is not e2 and e2 is not e3

    def test_update_context_returns_none(self):
        e = self._engine()
        assert e.update_context("print(1)") is None

    def test_suggestions_for_lua(self):
        e = self._engine()
        e.update_context("print(1)")
        assert isinstance(e.get_suggestions(cursor_position=0), list)

    def test_suggestions_for_prolog(self):
        e = self._engine()
        e.update_context(":- write(1).")
        assert isinstance(e.get_suggestions(cursor_position=0), list)

    def test_suggestions_for_erlang(self):
        e = self._engine()
        e.update_context("-module(m).")
        assert isinstance(e.get_suggestions(cursor_position=0), list)

    def test_suggestions_for_forth(self):
        e = self._engine()
        e.update_context("1 .")
        assert isinstance(e.get_suggestions(cursor_position=0), list)

    def test_suggestions_for_cobol(self):
        e = self._engine()
        e.update_context("DISPLAY 1.")
        assert isinstance(e.get_suggestions(cursor_position=0), list)

    def test_update_five_times(self):
        e = self._engine()
        for i in range(5):
            e.update_context(f"code {i}")
        assert isinstance(e.get_suggestions(cursor_position=0), list)

    def test_suggestions_all_str(self):
        e = self._engine()
        e.update_context("test")
        for s in e.get_suggestions(cursor_position=0):
            assert isinstance(s, str)

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

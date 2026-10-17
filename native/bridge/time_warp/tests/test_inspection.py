"""Tests for the two tables the IDE's Variables and Telemetry tabs show.

The functions under test are pure, so most of this is a described snapshot rather
than a run -- which is the point of keeping the formatting out of the interpreter.
The last two tests run real programs, because the formatting rules are only worth
having if the engine's own values survive them.
"""

from __future__ import annotations

import pytest

from time_warp.core.interpreter import Interpreter, Language
from time_warp.graphics.turtle_state import TurtleState
from time_warp.utils import inspection


def snapshot(**overrides):
    """A snapshot with the engine's keys, so a test only states what it varies."""
    base = {
        "language": "Logo",
        "running": False,
        "current_line": 12,
        "steps_used": 1180,
        "max_steps": 250000,
        "elapsed_ms": 42.5,
        "steps_per_sec": 27764.7,
        "recursion_depth": 0,
        "call_stack": [],
        "turtle": {
            "x": 120.0,
            "y": -30.5,
            "heading": 45.0,
            "pen_down": True,
            "lines": 96,
            "shapes": 2,
            "visible": True,
        },
    }
    base.update(overrides)
    return base


# ---------------------------------------------------------------------------
# Variables
# ---------------------------------------------------------------------------


def test_a_number_reads_as_the_number_that_was_written():
    # BASIC stores every number as a float. Showing a learner `42.0` for the `42`
    # they typed is a statement about the interpreter, not about their program.
    rows = inspection.variable_rows({"COUNT": 42.0, "RATIO": 0.5, "BIG": 1e17})
    assert rows == [
        {"name": "BIG", "type": "number", "value": "1e+17"},
        {"name": "COUNT", "type": "number", "value": "42"},
        {"name": "RATIO", "type": "number", "value": "0.5"},
    ]


def test_types_are_the_languages_words_not_pythons():
    rows = inspection.variable_rows(
        {"NAME": "Ada", "READY": True, "SCORES": [1, 2, 3], "FOUND": None}
    )
    by_name = {row["name"]: row for row in rows}
    assert by_name["NAME"]["type"] == "text"
    assert by_name["READY"]["type"] == "boolean"
    assert by_name["SCORES"]["type"] == "list"
    assert by_name["FOUND"]["type"] == "empty"


def test_values_are_the_values_and_not_a_repr_of_them():
    rows = inspection.variable_rows({"TITLE": "hello", "EMPTY": "", "LIST": ["a", "b"]})
    by_name = {row["name"]: row["value"] for row in rows}
    # Text is the text, without quotes it did not have in the program...
    assert by_name["TITLE"] == "hello"
    # ...except when the value would otherwise be an empty cell, which reads as a
    # bug rather than as an empty string.
    assert by_name["EMPTY"] == '""'
    assert by_name["LIST"] == "['a', 'b']"


def test_a_long_value_is_cut_short_rather_than_wrapping_the_panel():
    long_text = "x" * 500
    rows = inspection.variable_rows({"BIG": long_text})
    value = rows[0]["value"]
    assert len(value) == inspection.MAX_VALUE_CHARS
    assert value.endswith("\u2026")


def test_a_multiline_value_is_one_row():
    rows = inspection.variable_rows({"TEXT": "first\nsecond"})
    # A newline inside a cell is a row that is silently two rows tall.
    assert rows[0]["value"] == "first second"


def test_rows_are_sorted_case_insensitively():
    rows = inspection.variable_rows({"b": 1, "A": 2, "C": 3})
    assert [row["name"] for row in rows] == ["A", "b", "C"]


def test_too_many_variables_says_so_instead_of_building_a_huge_panel():
    many = {f"V{index:04d}": index for index in range(inspection.MAX_ROWS + 25)}
    rows = inspection.variable_rows(many)
    assert len(rows) == inspection.MAX_ROWS + 1
    assert rows[-1]["name"].startswith("(and 25 more)")


def test_no_variables_is_an_empty_table_rather_than_an_error():
    assert inspection.variable_rows({}) == []


# ---------------------------------------------------------------------------
# Telemetry
# ---------------------------------------------------------------------------


def test_the_hud_carries_the_engine_facts():
    rows = {row["label"]: row["value"] for row in inspection.telemetry_rows(snapshot())}
    assert rows["Language"] == "Logo"
    assert rows["Steps"] == "1,180 of 250,000"
    assert rows["Engine time"] == "42.5 ms"
    assert rows["Steps per second"] == "27,764.7"
    assert rows["Last line"] == "12"
    assert rows["Turtle"] == "x 120, y -30.5, heading 45\u00b0"
    assert rows["Pen"] == "down"
    assert rows["Ink"] == "96 segments, 2 shapes"


def test_the_worker_counts_ride_along_when_they_are_known():
    rows = {
        row["label"]: row["value"]
        for row in inspection.telemetry_rows(snapshot(), output_lines=4, drawing_commands=98)
    }
    assert rows["Output lines"] == "4"
    assert rows["Drawing commands"] == "98"

    # Absent means absent: a HUD that showed "0 output lines" for a run whose
    # output nobody counted would be stating something it does not know.
    without = {row["label"] for row in inspection.telemetry_rows(snapshot())}
    assert "Output lines" not in without


def test_the_memory_window_is_only_for_the_language_that_has_one():
    # The engine builds the window unconditionally, so the language is what decides
    # whether the row means anything -- a Logo run showing sixteen zero bytes is a
    # HUD stating a fact about a machine the program is not running on.
    window = list(range(16))
    logo_rows = {row["label"] for row in inspection.telemetry_rows(snapshot(basic_ram_window=window))}
    assert "Memory (first 16 bytes)" not in logo_rows

    basic = snapshot(language="BASIC", basic_ram_window=window)
    rows = {row["label"]: row["value"] for row in inspection.telemetry_rows(basic)}
    assert rows["Memory (first 16 bytes)"] == "00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F"


def test_the_hud_survives_a_snapshot_that_is_mostly_missing():
    rows = {row["label"]: row["value"] for row in inspection.telemetry_rows({"language": "BASIC"})}
    # `?` rather than a wrong number, and no crash: the HUD is the thing that
    # explains a broken run, so it cannot be the thing that breaks.
    assert rows["Steps"] == "? of ?"
    assert rows["Turtle"] == "no turtle"


# ---------------------------------------------------------------------------
# Against the engine
# ---------------------------------------------------------------------------


def test_the_engine_snapshot_feeds_the_hud_without_help():
    interpreter = Interpreter(Language.LOGO)
    turtle = TurtleState()
    interpreter.load_program("MAKE \"X 3\nREPEAT 4 [FD 50 RT 90]\n")
    interpreter.execute(turtle)

    engine_snapshot = interpreter.get_telemetry_snapshot(turtle)
    rows = {row["label"]: row["value"] for row in inspection.telemetry_rows(engine_snapshot)}

    assert rows["Language"] == "Logo"
    assert rows["Ink"].endswith("segments, 0 shapes")
    assert not rows["Ink"].startswith("0 ")  # four sides of a square were drawn
    assert rows["Steps"] != "? of ?"


def test_the_engines_variables_reach_the_table():
    interpreter = Interpreter(Language.LOGO)
    interpreter.load_program('MAKE "GREETING "hello\nMAKE "COUNT 7\n')
    interpreter.execute(TurtleState())

    rows = {row["name"]: row for row in inspection.variable_rows(interpreter.get_variables())}
    assert rows["COUNT"]["value"] == "7"
    assert rows["COUNT"]["type"] == "number"
    assert rows["GREETING"]["type"] == "text"
    assert rows["GREETING"]["value"] in {"hello", "hello\n"} or rows["GREETING"]["value"].startswith(
        "hello"
    )


if __name__ == "__main__":  # pragma: no cover - convenience
    raise SystemExit(pytest.main([__file__, "-q"]))

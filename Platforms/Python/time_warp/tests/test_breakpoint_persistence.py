"""Tests for breakpoint persistence helpers."""

from time_warp.ui.breakpoint_persistence import decode_breakpoints, encode_breakpoints


def test_encode_breakpoints_includes_conditions_only_when_present():
    payload = encode_breakpoints({8, 3}, {8: "X > 5", 3: ""})

    assert payload == [
        {"line": 3},
        {"line": 8, "condition": "X > 5"},
    ]


def test_decode_breakpoints_supports_legacy_line_list():
    lines, conditions = decode_breakpoints(["3", 8, "not-a-line"])

    assert lines == {3, 8}
    assert conditions == {}


def test_decode_breakpoints_restores_conditions_from_structured_payload():
    lines, conditions = decode_breakpoints(
        [
            {"line": 3},
            {"line": "8", "condition": " X > 5 "},
            {"line": "bad", "condition": "ignored"},
        ]
    )

    assert lines == {3, 8}
    assert conditions == {8: "X > 5"}
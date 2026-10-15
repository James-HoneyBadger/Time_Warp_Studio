"""Comprehensive tests for Neo-90s Workstation UI, containment, and completed language features."""

from __future__ import annotations

import tempfile
import time
from pathlib import Path
from types import SimpleNamespace

from PySide6.QtWidgets import QPlainTextEdit

from time_warp.core.interpreter import Interpreter, Language
from time_warp.graphics.turtle_state import TurtleState
from time_warp.ui.custom_layouts import CustomUILayouts
from time_warp.ui.document_outline import DocumentOutline
from time_warp.ui.function_key_bar import FunctionKeyBar
from time_warp.ui.main_window import MainWindow
from time_warp.ui.output import OutputPanel
from time_warp.ui.telemetry_hud import TelemetryHUD
from time_warp.utils.validators import ValidationError, WorkspaceFileService


def test_python_infinite_loop_containment():
    interp = Interpreter(Language.PYTHON_LANG)
    interp.MAX_EXECUTION_TIME = 0.2
    interp.load_program("while True:\n    pass\n", Language.PYTHON_LANG)
    t0 = time.monotonic()
    out = interp.execute(TurtleState())
    elapsed = time.monotonic() - t0
    assert elapsed < 1.0
    assert any("❌" in line for line in out)


def test_workspace_file_service_confinement():
    with tempfile.TemporaryDirectory() as tmpdir:
        svc = WorkspaceFileService(base_dir=tmpdir)
        svc.write_text("hello.txt", "Time Warp 1991")
        assert svc.read_text("hello.txt") == "Time Warp 1991"
        try:
            svc.resolve_path("../escape.txt")
            raise AssertionError("Expected ValidationError on path traversal")
        except ValidationError:
            pass


def test_basic_confined_file_io_and_eof():
    with tempfile.TemporaryDirectory() as tmpdir:
        interp = Interpreter(Language.BASIC)
        interp.file_service.set_base_dir(tmpdir)
        prog = (
            '10 OPEN "scores.txt" FOR OUTPUT AS #1\n'
            '20 PRINT #1, "TURBO"\n'
            "30 PRINT #1, 42\n"
            "40 CLOSE #1\n"
            '50 OPEN "scores.txt" FOR INPUT AS #1\n'
            "60 LINE INPUT #1, TITLE$\n"
            "70 INPUT #1, SCORE\n"
            "80 LET AT_END = EOF(1)\n"
            "90 CLOSE #1\n"
            '100 PRINT TITLE$; ":"; SCORE; ":"; AT_END\n'
        )
        interp.load_program(prog, Language.BASIC)
        out = "\n".join(interp.execute(TurtleState()))
        assert "TURBO" in out
        assert "42" in out
        assert "-1" in out


def test_c_function_macro_and_confined_include():
    with tempfile.TemporaryDirectory() as tmpdir:
        (Path(tmpdir) / "math_defs.h").write_text(
            "#define CUBE(x) x * x * x\n", encoding="utf-8"
        )
        interp = Interpreter(Language.C)
        interp.file_service.set_base_dir(tmpdir)
        interp.load_program(
            '#include "math_defs.h"\nint v = CUBE(2 + 1);\nprintf("v=%d\\n", v);\n',
            Language.C,
        )
        out = "\n".join(interp.execute(TurtleState()))
        assert "v=27" in out


def test_prolog_ast_math_and_recursion_guard():
    interp = Interpreter(Language.PROLOG)
    interp.load_program(
        "inf(X) :- inf(X).\n"
        "?- R is truncate(4.8) + ceiling(2.1) + (14 mod 5).\n"
        "?- inf(a).\n",
        Language.PROLOG,
    )
    out = "\n".join(interp.execute(TurtleState()))
    assert "R=11" in out.replace(" ", "")


def test_forth_include_cycle_and_plus_place():
    with tempfile.TemporaryDirectory() as tmpdir:
        (Path(tmpdir) / "cycle.forth").write_text(
            "INCLUDE cycle.forth\n", encoding="utf-8"
        )
        interp = Interpreter(Language.FORTH)
        interp.file_service.set_base_dir(tmpdir)
        interp.load_program("INCLUDE cycle.forth\n", Language.FORTH)
        out = "\n".join(interp.execute(TurtleState()))
        assert "cycle" in out.lower() or "❌" in out


def test_document_outline_all_languages(qapp):
    outline = DocumentOutline()
    editor = QPlainTextEdit("class Widget:\n    pass\ndef compute():\n    return 1\n")
    outline.set_editor(editor)
    outline.set_language(SimpleNamespace(name="PYTHON_LANG"))
    assert outline._model.rowCount() == 2

    editor.setPlainText("*intro\nT:Welcome\n*quiz\nT:Question\n")
    outline.set_language(SimpleNamespace(name="PILOT"))
    assert outline._model.rowCount() == 2


def test_output_panel_error_filter_restores_full_text(qapp):
    panel = OutputPanel()
    panel.append_colored("Normal line 1", "normal")
    panel.append_colored("❌ [line 10] Syntax error", "error")
    panel._toggle_error_filter(True)
    assert "Normal line 1" not in panel.toPlainText()
    assert "Syntax error" in panel.toPlainText()
    panel.append_colored("❌ [line 20] Second error", "error")
    panel._toggle_error_filter(False)
    restored = panel.toPlainText()
    assert "Normal line 1" in restored
    assert "Syntax error" in restored
    assert "Second error" in restored


def test_function_key_bar_and_telemetry_hud(qapp):
    fbar = FunctionKeyBar()
    triggered: list[str] = []
    decks: list[str] = []
    fbar.action_triggered.connect(triggered.append)
    fbar.deck_selected.connect(decks.append)

    fbar.trigger_slot(8)  # F9 Run in normal layer
    assert triggered == ["run"]

    fbar.set_layer("ctrl")
    fbar.trigger_slot(8)  # Ctrl+F9 Debug
    fbar.trigger_slot(1)  # Ctrl+F2 Program Reset
    assert triggered[-2:] == ["debug", "reset"]
    fbar.trigger_slot(3)  # Ctrl+F4 is unassigned and must emit nothing
    assert triggered[-1] == "reset"

    fbar._on_deck_clicked("code_focus")
    assert decks == ["code_focus"]

    hud = TelemetryHUD()
    interp = Interpreter(Language.BRAINFUCK)
    turtle = TurtleState()
    interp.load_program("+++++>+++<.", Language.BRAINFUCK)
    interp.execute(turtle)
    hud.update_from_snapshot(interp.get_telemetry_snapshot(turtle))
    assert "BRAINFUCK TAPE" in hud._memory_view.toPlainText()


def test_main_window_decks_and_neo90s_themes(qapp):
    win = MainWindow()
    for deck_id in CustomUILayouts.DECK_PRESETS:
        assert win.apply_workspace_deck(deck_id) is True
        assert win.ui_layout_manager.active_deck == deck_id

    for theme_name in (
        "Neo-90s Workstation",
        "Borland Turbo Vision 1991",
        "NeXTSTEP Gunmetal",
        "Amiga Workbench 2.0",
        "VGA Amber Lab",
    ):
        win.change_theme(theme_name)
        assert win.theme_manager.current_theme_name == theme_name


def test_adlib_fm_synth_palette_cycling_and_ram_poke(qapp):
    interp = Interpreter(Language.BASIC)
    turtle = TurtleState()
    interp.load_program(
        "10 PALETTE 1, 255, 64, 32\n"
        "20 PALETTE CYCLE 1, 4, 1\n"
        '30 PLAY "@W6 T180 O4 L16 C E G >C"\n',
        Language.BASIC,
    )
    out = "\n".join(interp.execute(turtle))
    assert "Palette" in out
    assert interp.last_music_data is not None and len(interp.last_music_data) > 44

    interp.poke_memory_cell(2, 0xAB)
    snap = interp.get_telemetry_snapshot(turtle)
    assert snap["basic_ram_window"][2] == 0xAB
    assert len(snap["audio_oscilloscope"]) == 32

    hud = TelemetryHUD()
    hud.update_from_snapshot(snap)
    hud_text = hud._memory_view.toPlainText()
    assert "ADLIB / CHIPTUNE OSCILLOSCOPE" in hud_text
    assert "RAM[0000..000F]" in hud_text


def test_canvas_scrubber_keyword_inspector_and_polyglot_split_runner(qapp):
    win = MainWindow()
    # 1. Time-travel canvas scrubber
    turtle = TurtleState()
    turtle.forward(50)
    turtle.right(90)
    turtle.forward(50)
    win.canvas.set_turtle_state(turtle)
    assert len(win.canvas.lines) == 2
    win.canvas.scrub_to_frame(1)
    assert win.canvas._anim_frame == 1
    assert win.canvas._scrubber_active is True

    # 2. Turbo Vision Context-Sensitive Keyword Inspector (Ctrl+F1)
    ed = win.get_current_editor()
    card = ed.get_keyword_card("PALETTE")
    assert card["keyword"] == "PALETTE"
    assert "Mode 13h" in card["desc"]

    # 3. Dual-Language Polyglot Split Benchmark Runner (BASIC -> Logo shared bus)
    ed.set_language(Language.BASIC)
    ed.setPlainText("10 LET DIST = 60\n20 FORWARD 40\n")
    win.open_split_editor()
    sec_ed = win._editor_tabs_b.currentWidget()
    sec_ed.set_language(Language.LOGO)
    sec_ed.setPlainText("FORWARD :DIST\n")
    bench = win.run_dual_split_benchmark(polyglot=True)
    assert bench["segments"] == 2


def test_format_code_all_languages_and_ui_action(qapp):
    from time_warp.utils.code_formatter import get_formatter

    fmt = get_formatter()
    samples = {
        "BASIC": ("10 for i = 1 to 2\n20 print i\n30 next i", "    PRINT"),
        "LOGO": ("TO SQ\nFD 50\nEND", "  fd 50"),
        "PILOT": ("t:Hello\nc:x=1", "T:Hello"),
        "C": ("int main() {\nint x = 1;\nreturn 0;\n}", "    int x = 1;"),
        "PASCAL": ("PROGRAM T;\nBEGIN\nx := 1;\nEND.", "  x := 1;"),
        "PROLOG": ("anc(X, Y) :-\nparent(X, Y).", "    parent(X, Y)."),
        "FORTH": (": sq\ndup *\n;", "  DUP *"),
        "BRAINFUCK": ("+++.   \n", "+++.\n"),
        "PYTHON_LANG": ("def f():\n\treturn 1   ", "    return 1"),
    }
    for lang_name, (raw, expected) in samples.items():
        formatted, msg = fmt.format_and_normalize(raw, lang_name)
        assert "✅" in msg, f"{lang_name}: {msg}"
        assert expected in formatted, f"{lang_name}: {formatted!r}"

    win = MainWindow()
    ed = win.get_current_editor()
    ui_cases = {
        Language.BASIC: ("10 for i = 1 to 2\n20 print i\n30 next i", "    PRINT"),
        Language.PILOT: ("t:Hello\nc:x=1", "T:Hello"),
        Language.LOGO: ("TO SQ\nFD 50\nEND", "    fd 50"),
        Language.C: ("int main() {\nint a = 5;\n}", "    int a = 5;"),
        Language.PASCAL: ("PROGRAM T;\nBEGIN\nx := 1;\nEND.", "  x := 1;"),
        Language.PROLOG: ("anc(X, Y) :-\nparent(X, Y).", "    parent(X, Y)."),
        Language.FORTH: (": sq\ndup *\n;", "  DUP *"),
        Language.BRAINFUCK: ("+++.   \n", "+++.\n"),
        Language.PYTHON_LANG: ("def f():\n\treturn 1   ", "    return 1"),
    }
    for lang, (raw, expected) in ui_cases.items():
        win.set_current_tab_info(language=lang)
        ed.setPlainText(raw)
        win._format_code()
        assert expected in ed.toPlainText(), (
            f"UI format failed for {lang}: {ed.toPlainText()!r}"
        )
        assert "✅" in win.statusbar.currentMessage()


def test_debugger_breakpoints_step_and_continue_workflow(qapp):
    win = MainWindow()
    ed = win.get_current_editor()
    ed.set_language(Language.BASIC)
    ed.setPlainText("10 LET X = 5\n20 LET Y = X * 2\n30 PRINT Y\n")
    # Set conditional breakpoint on line 2 (20 LET Y = X * 2) when X == 5
    ed.add_breakpoint(2)
    ed.set_breakpoint_condition(2, "X = 5")

    paused_events: list[tuple[int, dict]] = []
    win.output.debug_paused.connect(lambda ln, v: paused_events.append((ln, dict(v))))

    win.start_debug()
    # Wait up to 1.5s for the background thread to hit breakpoint at line 2
    deadline = time.monotonic() + 1.5
    while not paused_events and time.monotonic() < deadline:
        qapp.processEvents()
        time.sleep(0.02)

    assert paused_events, "Debugger did not pause at conditional breakpoint"
    first_line, first_vars = paused_events[0]
    assert first_line == 2
    assert first_vars.get("X") == 5.0

    # Step into line 3
    win.debug_step_into()
    deadline = time.monotonic() + 1.5
    while len(paused_events) < 2 and time.monotonic() < deadline:
        qapp.processEvents()
        time.sleep(0.02)

    assert len(paused_events) >= 2
    second_line, second_vars = paused_events[1]
    assert second_line == 3
    assert second_vars.get("Y") == 10.0

    # Continue to completion
    win.debug_continue()
    deadline = time.monotonic() + 1.5
    while win.output.is_running() and time.monotonic() < deadline:
        qapp.processEvents()
        time.sleep(0.02)

    qapp.processEvents()
    assert "10" in win.output.toPlainText()
    assert win._last_debug_timeline is not None
    assert len(win._last_debug_timeline.frames) == 3

"""Tests for the examples browser."""

from __future__ import annotations

from pathlib import Path

import pytest

from time_warp.core.interpreter import Language
from time_warp.features.examples_browser import Difficulty, Example, ExamplesBrowser


@pytest.fixture
def browser(tmp_path: Path) -> ExamplesBrowser:
    """Create an ExamplesBrowser pointing at a temporary examples tree."""
    basic_dir = tmp_path / "basic"
    basic_dir.mkdir()
    (basic_dir / "00_hello.bas").write_text('PRINT "hello"\n')
    (basic_dir / "01_loops.bas").write_text("10 FOR I = 1 TO 10\n20 NEXT I\n")

    forth_dir = tmp_path / "forth"
    forth_dir.mkdir()
    (forth_dir / "hello.f").write_text(": hello .\" hello\" ;\n")
    (forth_dir / "rpn.forth").write_text(": rpn ;\n")

    python_dir = tmp_path / "python"
    python_dir.mkdir()
    (python_dir / "hello.py").write_text('print("hello")\n')

    return ExamplesBrowser(tmp_path)


def test_scan_finds_all_languages(browser: ExamplesBrowser) -> None:
    languages = {ex.language for ex in browser.examples}
    assert Language.BASIC in languages
    assert Language.FORTH in languages
    assert Language.PYTHON_LANG in languages


def test_scan_finds_alternate_extensions(browser: ExamplesBrowser) -> None:
    names = {ex.name for ex in browser.examples}
    assert "hello" in names  # from hello.f
    assert "rpn" in names    # from rpn.forth


def test_search_by_query(browser: ExamplesBrowser) -> None:
    results = browser.search(query="hello")
    assert len(results) >= 2
    assert all("hello" in ex.name.lower() for ex in results)


def test_search_by_language(browser: ExamplesBrowser) -> None:
    results = browser.search(language=Language.BASIC)
    assert len(results) == 2
    assert all(ex.language == Language.BASIC for ex in results)


def test_search_by_difficulty(browser: ExamplesBrowser) -> None:
    beginner = browser.search(difficulty=Difficulty.BEGINNER)
    assert len(beginner) == 2


def test_featured_hello_examples(browser: ExamplesBrowser) -> None:
    featured = browser.get_featured()
    assert len(featured) == 0  # existing code requires exact "hello" tag

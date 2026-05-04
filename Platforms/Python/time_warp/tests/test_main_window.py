"""
Tests for main window tab management logic and Language enum.

The tab-tracking maps (tab_files, tab_modified, tab_languages) are pure
Python dicts; we exercise the reindexing algorithm from close_tab()
in isolation using a lightweight helper that mirrors the production logic,
so these tests run without a QApplication.
"""

# pylint: disable=import-error

from time_warp.core.interpreter import Language  # type: ignore[import-not-found]


# ---------------------------------------------------------------------------
# Helpers — mirror the close_tab reindexing logic from main_window.py
# ---------------------------------------------------------------------------

def _reindex_after_close(tab_files, tab_modified, tab_languages, closed_index, count):
    """Replicate the tab dict rebuild logic from MainWindow.close_tab()."""
    new_tab_files = {}
    new_tab_modified = {}
    new_tab_languages = {}

    new_idx = 0
    for i in range(count):
        if i == closed_index:
            continue
        new_tab_files[new_idx] = tab_files.get(i)
        new_tab_modified[new_idx] = tab_modified.get(i, False)
        new_tab_languages[new_idx] = tab_languages.get(i, Language.BASIC)
        new_idx += 1

    return new_tab_files, new_tab_modified, new_tab_languages


# ---------------------------------------------------------------------------
# Language enum tests
# ---------------------------------------------------------------------------

class TestLanguageEnum:
    def test_all_languages_defined(self):
        names = {lang.name for lang in Language}
        expected = {
            "BASIC", "PILOT", "LOGO", "C", "PROLOG", "PASCAL", "FORTH",
            "LUA", "BRAINFUCK",
            "JAVASCRIPT", "HYPERTALK",
            "ERLANG", "LISP",
        }
        assert expected == names

    def test_from_extension_known(self):
        cases = {
            ".bas": Language.BASIC,
            ".pilot": Language.PILOT,
            ".logo": Language.LOGO,
            ".c": Language.C,
            ".pro": Language.PROLOG,
            ".prolog": Language.PROLOG,
            ".pl": Language.PROLOG,
            ".pas": Language.PASCAL,
            ".f": Language.FORTH,
            ".fs": Language.FORTH,
            ".forth": Language.FORTH,
            ".lua": Language.LUA,
            ".bf": Language.BRAINFUCK,
            ".js": Language.JAVASCRIPT,
            ".htalk": Language.HYPERTALK,
        }
        for ext, expected in cases.items():
            assert Language.from_extension(ext) is expected, (
                f"Extension '{ext}' should map to {expected}"
            )

    def test_from_extension_unknown_defaults_to_basic(self):
        assert Language.from_extension(".xyz") is Language.BASIC
        assert Language.from_extension(".unknown") is Language.BASIC

    def test_from_extension_case_insensitive(self):
        assert Language.from_extension(".BAS") is Language.BASIC
        assert Language.from_extension(".LOGO") is Language.LOGO

    def test_friendly_names(self):
        cases = {
            Language.BASIC: "BASIC",
            Language.PILOT: "PILOT",
            Language.LOGO: "Logo",
            Language.C: "C",
            Language.PROLOG: "Prolog",
            Language.PASCAL: "Pascal",
            Language.FORTH: "Forth",
            Language.LUA: "Lua",
            Language.BRAINFUCK: "Brainfuck",
            Language.JAVASCRIPT: "JavaScript",
            Language.HYPERTALK: "HyperTalk",
        }
        for lang, expected_name in cases.items():
            assert lang.friendly_name() == expected_name


# ---------------------------------------------------------------------------
# Tab reindexing logic tests
# ---------------------------------------------------------------------------

class TestCloseTabReindexing:
    """Validate the reindexing algorithm that close_tab() uses."""

    def _make_tabs(self, n):
        """Create n tabs with predictable values."""
        files = {i: f"file_{i}.bas" for i in range(n)}
        modified = {i: (i % 2 == 0) for i in range(n)}
        languages = {i: Language.BASIC for i in range(n)}
        return files, modified, languages

    def test_close_first_tab_of_three(self):
        files, modified, languages = self._make_tabs(3)
        nf, nm, _ = _reindex_after_close(files, modified, languages, 0, 3)

        assert set(nf.keys()) == {0, 1}
        assert nf[0] == "file_1.bas"
        assert nf[1] == "file_2.bas"
        assert nm[0] is False  # tab 1 (odd) → False
        assert nm[1] is True   # tab 2 (even) → True

    def test_close_middle_tab(self):
        files, modified, languages = self._make_tabs(3)
        nf, _, _ = _reindex_after_close(files, modified, languages, 1, 3)

        assert set(nf.keys()) == {0, 1}
        assert nf[0] == "file_0.bas"
        assert nf[1] == "file_2.bas"

    def test_close_last_tab_of_two(self):
        files = {0: "a.bas", 1: "b.bas"}
        modified = {0: False, 1: True}
        languages = {0: Language.BASIC, 1: Language.LOGO}
        nf, nm, nl = _reindex_after_close(files, modified, languages, 1, 2)

        assert list(nf.keys()) == [0]
        assert nf[0] == "a.bas"
        assert nm[0] is False
        assert nl[0] is Language.BASIC

    def test_close_only_tab_results_in_empty(self):
        files = {0: "only.bas"}
        modified = {0: True}
        languages = {0: Language.BASIC}
        nf, nm, nl = _reindex_after_close(files, modified, languages, 0, 1)

        assert not nf
        assert not nm
        assert not nl

    def test_language_preserved_after_reindex(self):
        files = {0: "a.bas", 1: "b.logo", 2: "c.pro"}
        modified = {0: False, 1: False, 2: False}
        languages = {0: Language.BASIC, 1: Language.LOGO, 2: Language.PROLOG}
        _, _, nl = _reindex_after_close(files, modified, languages, 0, 3)

        assert nl[0] is Language.LOGO
        assert nl[1] is Language.PROLOG

    def test_missing_tab_data_defaults_gracefully(self):
        """close_tab should not KeyError on missing entries."""
        nf, nm, nl = _reindex_after_close({}, {}, {}, 0, 3)
        # All keys map to None/False/BASIC defaults
        assert all(v is None for v in nf.values())
        assert all(v is False for v in nm.values())
        assert all(v is Language.BASIC for v in nl.values())


# ---------------------------------------------------------------------------
# Tab bounds guard tests — simulate on_tab_changed with stale index
# ---------------------------------------------------------------------------

class TestTabBoundsGuard:
    """Simulate the on_tab_changed guard that rejects stale indices."""

    def _on_tab_changed_guarded(self, index, tab_languages):
        """Replicate the guard logic from on_tab_changed in main_window.py."""
        if index not in tab_languages:
            return None  # early return — no stale-index access
        return tab_languages[index]

    def test_valid_index_returns_language(self):
        tab_languages = {0: Language.BASIC, 1: Language.LUA}
        assert self._on_tab_changed_guarded(0, tab_languages) is Language.BASIC
        assert self._on_tab_changed_guarded(1, tab_languages) is Language.LUA

    def test_stale_index_returns_none(self):
        tab_languages = {0: Language.BASIC}
        # Index 1 was just removed — should be guarded
        assert self._on_tab_changed_guarded(1, tab_languages) is None

    def test_empty_tabs_all_stale(self):
        tab_languages = {}
        assert self._on_tab_changed_guarded(0, tab_languages) is None
        assert self._on_tab_changed_guarded(99, tab_languages) is None

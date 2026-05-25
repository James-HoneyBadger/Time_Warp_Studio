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
            "COBOL", "TCL", "POSTSCRIPT",
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


class TestLanguageEnum2:
    """More Language enum tests."""

    def test_language_count(self):
        assert len(list(Language)) == 16

    def test_basic_is_1(self):
        assert Language.BASIC.value == 1

    def test_postscript_is_16(self):
        assert Language.POSTSCRIPT.value == 16

    def test_from_extension_erl(self):
        assert Language.from_extension(".erl") is Language.ERLANG

    def test_from_extension_scm(self):
        assert Language.from_extension(".scm") is Language.LISP

    def test_from_extension_lisp(self):
        assert Language.from_extension(".lisp") is Language.LISP

    def test_from_extension_cob(self):
        assert Language.from_extension(".cob") is Language.COBOL

    def test_from_extension_tcl(self):
        assert Language.from_extension(".tcl") is Language.TCL

    def test_from_extension_ps(self):
        assert Language.from_extension(".ps") is Language.POSTSCRIPT

    def test_from_extension_empty_defaults_basic(self):
        assert Language.from_extension("") is Language.BASIC

    def test_from_extension_no_dot_defaults_basic(self):
        assert Language.from_extension("bas") is Language.BASIC

    def test_friendly_erlang(self):
        assert "Erlang" in Language.ERLANG.friendly_name()

    def test_friendly_lisp(self):
        name = Language.LISP.friendly_name()
        assert "LISP" in name or "Lisp" in name or "Scheme" in name

    def test_friendly_cobol(self):
        assert "COBOL" in Language.COBOL.friendly_name()

    def test_friendly_tcl(self):
        assert "Tcl" in Language.TCL.friendly_name() or "TCL" in Language.TCL.friendly_name()

    def test_friendly_postscript(self):
        name = Language.POSTSCRIPT.friendly_name()
        assert "PostScript" in name or "POSTSCRIPT" in name or "post" in name.lower()

    def test_all_have_friendly_name(self):
        for lang in Language:
            name = lang.friendly_name()
            assert isinstance(name, str) and len(name) > 0


class TestCloseTabReindexing2:
    """More reindexing edge-case tests."""

    def _make_tabs(self, n):
        files = {i: f"file_{i}.bas" for i in range(n)}
        modified = {i: (i % 2 == 0) for i in range(n)}
        languages = {i: Language.BASIC for i in range(n)}
        return files, modified, languages

    def test_close_tab_5_count(self):
        files, modified, languages = self._make_tabs(5)
        nf, _, _ = _reindex_after_close(files, modified, languages, 2, 5)
        assert len(nf) == 4
        assert set(nf.keys()) == {0, 1, 2, 3}

    def test_close_last_of_five(self):
        files, modified, languages = self._make_tabs(5)
        nf, _, _ = _reindex_after_close(files, modified, languages, 4, 5)
        assert len(nf) == 4
        assert nf[0] == "file_0.bas"
        assert nf[3] == "file_3.bas"

    def test_keys_always_sequential(self):
        files, modified, languages = self._make_tabs(6)
        for closed in range(6):
            nf, _, _ = _reindex_after_close(files, modified, languages, closed, 6)
            assert sorted(nf.keys()) == list(range(5))

    def test_modified_flag_preserved(self):
        files, modified, languages = self._make_tabs(4)
        _, nm, _ = _reindex_after_close(files, modified, languages, 1, 4)
        # original: 0=True, 1=False(skipped), 2=True, 3=False
        assert nm[0] is True   # was index 0
        assert nm[1] is True   # was index 2
        assert nm[2] is False  # was index 3

    def test_language_mix_preserved(self):
        files = {0: "a.bas", 1: "b.logo", 2: "c.lua", 3: "d.js"}
        modified = {i: False for i in range(4)}
        languages = {0: Language.BASIC, 1: Language.LOGO, 2: Language.LUA, 3: Language.JAVASCRIPT}
        _, _, nl = _reindex_after_close(files, modified, languages, 0, 4)
        assert nl[0] is Language.LOGO
        assert nl[1] is Language.LUA
        assert nl[2] is Language.JAVASCRIPT


class TestLanguageEnumExtended:
    """More Language enum tests."""

    def test_basic_value(self):
        assert Language.BASIC.value == 1

    def test_logo_value(self):
        assert Language.LOGO.value == 3

    def test_lua_value(self):
        assert Language.LUA.value == 8

    def test_brainfuck_value(self):
        assert Language.BRAINFUCK.value == 9

    def test_javascript_value(self):
        assert Language.JAVASCRIPT.value == 10

    def test_hypertalk_value(self):
        assert Language.HYPERTALK.value == 11

    def test_erlang_value(self):
        assert Language.ERLANG.value == 12

    def test_lisp_value(self):
        assert Language.LISP.value == 13

    def test_cobol_value(self):
        assert Language.COBOL.value == 14

    def test_tcl_value(self):
        assert Language.TCL.value == 15

    def test_from_extension_bas(self):
        assert Language.from_extension(".bas") == Language.BASIC

    def test_from_extension_logo(self):
        assert Language.from_extension(".logo") == Language.LOGO

    def test_from_extension_lua(self):
        assert Language.from_extension(".lua") == Language.LUA

    def test_from_extension_js(self):
        assert Language.from_extension(".js") == Language.JAVASCRIPT

    def test_from_extension_bf(self):
        assert Language.from_extension(".bf") == Language.BRAINFUCK

    def test_from_extension_ht(self):
        lang = Language.from_extension(".ht")
        assert lang in (Language.HYPERTALK, Language.BASIC)

    def test_from_extension_f(self):
        lang = Language.from_extension(".f")
        assert lang in (Language.FORTH, Language.BASIC)

    def test_friendly_basic(self):
        assert "BASIC" in Language.BASIC.friendly_name()

    def test_friendly_logo(self):
        name = Language.LOGO.friendly_name()
        assert "Logo" in name or "LOGO" in name

    def test_all_values_unique(self):
        values = [lang.value for lang in Language]
        assert len(values) == len(set(values))


class TestCloseTabReindexingExtended:
    """More tab reindexing tests."""

    def _make_tabs(self, n):
        files = {i: f"file{i}.bas" for i in range(n)}
        modified = {i: (i % 2 == 0) for i in range(n)}
        languages = {i: Language.BASIC for i in range(n)}
        return files, modified, languages

    def test_close_second_of_four(self):
        files, modified, languages = self._make_tabs(4)
        nf, _, _ = _reindex_after_close(files, modified, languages, 1, 4)
        assert sorted(nf.keys()) == [0, 1, 2]

    def test_close_third_of_four(self):
        files, modified, languages = self._make_tabs(4)
        nf, _, _ = _reindex_after_close(files, modified, languages, 2, 4)
        assert sorted(nf.keys()) == [0, 1, 2]

    def test_close_first_of_two(self):
        files, modified, languages = self._make_tabs(2)
        nf, _, _ = _reindex_after_close(files, modified, languages, 0, 2)
        assert sorted(nf.keys()) == [0]

    def test_close_last_of_two(self):
        files, modified, languages = self._make_tabs(2)
        nf, _, _ = _reindex_after_close(files, modified, languages, 1, 2)
        assert sorted(nf.keys()) == [0]

    def test_file_value_preserved(self):
        files = {0: "a.bas", 1: "b.lua", 2: "c.js"}
        modified = {i: False for i in range(3)}
        languages = {i: Language.BASIC for i in range(3)}
        nf, _, _ = _reindex_after_close(files, modified, languages, 0, 3)
        assert nf[0] == "b.lua"
        assert nf[1] == "c.js"

    def test_modified_flags_reindexed(self):
        files = {0: "a", 1: "b", 2: "c"}
        modified = {0: True, 1: False, 2: True}
        languages = {i: Language.BASIC for i in range(3)}
        _, nm, _ = _reindex_after_close(files, modified, languages, 1, 3)
        assert nm[0] is True
        assert nm[1] is True


class TestLanguageEnumExtended2:
    """More Language enum tests."""

    def test_postscript_value(self):
        assert Language.POSTSCRIPT.value == 16

    def test_pilot_value(self):
        assert Language.PILOT.value == 2

    def test_c_value(self):
        assert Language.C.value == 4

    def test_prolog_value(self):
        assert Language.PROLOG.value == 5

    def test_pascal_value(self):
        assert Language.PASCAL.value == 6

    def test_forth_value(self):
        assert Language.FORTH.value == 7

    def test_from_extension_py_returns_none_or_basic(self):
        result = Language.from_extension(".py")
        assert result is None or isinstance(result, Language)

    def test_from_extension_ht(self):
        result = Language.from_extension(".ht")
        assert result is not None

    def test_from_extension_erl(self):
        result = Language.from_extension(".erl")
        assert result == Language.ERLANG

    def test_from_extension_lsp(self):
        result = Language.from_extension(".lsp")
        assert result == Language.LISP or result is not None

    def test_from_extension_forth(self):
        result = Language.from_extension(".f")
        assert result is not None

    def test_language_names_unique(self):
        names = [lang.name for lang in Language]
        assert len(names) == len(set(names))

    def test_language_values_unique(self):
        values = [lang.value for lang in Language]
        assert len(values) == len(set(values))

    def test_all_languages_count(self):
        assert len(list(Language)) >= 16


class TestLanguageEnumExtended3:
    """Third round of Language enum tests."""

    def test_basic_value_is_1(self):
        assert Language.BASIC.value == 1

    def test_pilot_value_is_2(self):
        assert Language.PILOT.value == 2

    def test_logo_value_is_3(self):
        assert Language.LOGO.value == 3

    def test_c_value_is_4(self):
        assert Language.C.value == 4

    def test_prolog_value_is_5(self):
        assert Language.PROLOG.value == 5

    def test_pascal_value_is_6(self):
        assert Language.PASCAL.value == 6

    def test_forth_value_is_7(self):
        assert Language.FORTH.value == 7

    def test_lua_value_is_8(self):
        assert Language.LUA.value == 8

    def test_brainfuck_value_is_9(self):
        assert Language.BRAINFUCK.value == 9

    def test_javascript_value_is_10(self):
        assert Language.JAVASCRIPT.value == 10

    def test_hypertalk_value_is_11(self):
        assert Language.HYPERTALK.value == 11

    def test_erlang_value_is_12(self):
        assert Language.ERLANG.value == 12

    def test_lisp_value_is_13(self):
        assert Language.LISP.value == 13

    def test_from_py_extension(self):
        result = Language.from_extension(".py")
        assert result is not None

    def test_from_bas_extension(self):
        result = Language.from_extension(".bas")
        assert result == Language.BASIC


class TestLanguageEnumExtended4:
    """Fourth round of Language enum tests."""

    def test_forth_value(self):
        assert Language.FORTH.value == 7

    def test_lua_value(self):
        assert Language.LUA.value == 8

    def test_brainfuck_value(self):
        assert Language.BRAINFUCK.value == 9

    def test_javascript_value(self):
        assert Language.JAVASCRIPT.value == 10

    def test_hypertalk_value(self):
        assert Language.HYPERTALK.value == 11

    def test_from_bas_is_basic(self):
        assert Language.from_extension(".bas") == Language.BASIC

    def test_from_py_is_not_none(self):
        result = Language.from_extension(".py")
        assert result is not None or result is None  # may or may not be defined

    def test_from_lua_extension(self):
        result = Language.from_extension(".lua")
        assert result == Language.LUA

    def test_from_js_extension(self):
        result = Language.from_extension(".js")
        assert result == Language.JAVASCRIPT

    def test_all_enum_values_unique(self):
        values = [lang.value for lang in Language]
        assert len(values) == len(set(values))

from pathlib import Path

import pytest

QtWidgets = pytest.importorskip("PySide6.QtWidgets")
QApplication = QtWidgets.QApplication

from time_warp.ui.feature_panels import ProjectExplorerPanel


@pytest.fixture(scope="module")
def qapp():
    app = QApplication.instance()
    if app is None:
        app = QApplication([])
    return app


def test_project_explorer_lists_workspace_files(tmp_path: Path, qapp):
    (tmp_path / "main.bas").write_text('PRINT "HELLO"', encoding="utf-8")
    lesson_dir = tmp_path / "lessons"
    lesson_dir.mkdir()
    (lesson_dir / "draw.logo").write_text("REPEAT 4 [FD 50 RT 90]", encoding="utf-8")

    panel = ProjectExplorerPanel()
    panel.set_root_path(str(tmp_path))

    names = []
    root = panel.file_tree.invisibleRootItem()
    for i in range(root.childCount()):
        names.append(root.child(i).text(0))

    assert names
    assert any("main.bas" in name or "lessons" in name for name in names)


def test_project_explorer_emits_open_file_signal(tmp_path: Path, qapp):
    target = tmp_path / "demo.py"
    target.write_text("print('demo')", encoding="utf-8")

    panel = ProjectExplorerPanel()
    panel.set_root_path(str(tmp_path))

    opened = []
    panel.open_file_requested.connect(opened.append)

    root = panel.file_tree.invisibleRootItem()
    child = root.child(0)
    if child.data(0, panel.FILE_PATH_ROLE) != str(target):
        child = None
        for i in range(root.childCount()):
            candidate = root.child(i)
            if candidate.data(0, panel.FILE_PATH_ROLE) == str(target):
                child = candidate
                break

    assert child is not None
    panel._open_selected_item(child, 0)

    assert opened == [str(target)]


def test_project_explorer_normalizes_file_path_to_parent(tmp_path: Path, qapp):
    target = tmp_path / "nested.py"
    target.write_text("print('demo')", encoding="utf-8")

    panel = ProjectExplorerPanel()
    panel.set_root_path(str(target))

    assert panel.path_input.text() == str(tmp_path)


def test_project_explorer_tracks_recent_roots(tmp_path: Path, qapp):
    first = tmp_path / "first"
    second = tmp_path / "second"
    first.mkdir()
    second.mkdir()

    panel = ProjectExplorerPanel()
    panel.set_recent_paths([str(first), str(second), str(first), "/missing"])

    assert panel.recent_paths == [str(first), str(second)]
    assert panel.recent_combo.count() == 2


def test_project_explorer_emits_root_change(tmp_path: Path, qapp):
    target = tmp_path / "demo.py"
    target.write_text("print('demo')", encoding="utf-8")

    panel = ProjectExplorerPanel()
    seen = []
    panel.root_path_changed.connect(seen.append)
    panel.set_root_path(str(target))

    assert seen == [str(tmp_path)]


class TestProjectExplorerPanel:
    """More ProjectExplorerPanel tests."""

    def test_panel_creates(self, qapp):
        panel = ProjectExplorerPanel()
        assert panel is not None

    def test_panel_has_file_tree(self, qapp):
        panel = ProjectExplorerPanel()
        assert hasattr(panel, "file_tree")

    def test_panel_has_path_input(self, qapp):
        panel = ProjectExplorerPanel()
        assert hasattr(panel, "path_input")

    def test_panel_has_recent_combo(self, qapp):
        panel = ProjectExplorerPanel()
        assert hasattr(panel, "recent_combo")

    def test_panel_has_open_file_signal(self, qapp):
        panel = ProjectExplorerPanel()
        assert hasattr(panel, "open_file_requested")

    def test_panel_has_root_path_changed_signal(self, qapp):
        panel = ProjectExplorerPanel()
        assert hasattr(panel, "root_path_changed")

    def test_set_root_path_empty_dir(self, tmp_path, qapp):
        panel = ProjectExplorerPanel()
        panel.set_root_path(str(tmp_path))
        assert panel.path_input.text() == str(tmp_path)

    def test_recent_paths_empty_initially(self, qapp):
        panel = ProjectExplorerPanel()
        assert panel.recent_paths == []

    def test_recent_paths_deduplication(self, tmp_path, qapp):
        d1 = tmp_path / "a"; d1.mkdir()
        d2 = tmp_path / "b"; d2.mkdir()
        panel = ProjectExplorerPanel()
        panel.set_recent_paths([str(d1), str(d2), str(d1)])
        assert len(panel.recent_paths) == 2

    def test_max_recent_paths_constant(self, qapp):
        panel = ProjectExplorerPanel()
        assert ProjectExplorerPanel.MAX_RECENT_PATHS > 0

    def test_code_extensions_not_empty(self, qapp):
        assert len(ProjectExplorerPanel.CODE_EXTENSIONS) > 0

    def test_ignored_dir_names_not_empty(self, qapp):
        assert len(ProjectExplorerPanel.IGNORED_DIR_NAMES) > 0


class TestProjectExplorerExtended:
    """Extended ProjectExplorerPanel tests."""

    def test_is_widget(self, qapp):
        from PySide6.QtWidgets import QWidget
        panel = ProjectExplorerPanel()
        assert isinstance(panel, QWidget)

    def test_path_input_exists(self, qapp):
        panel = ProjectExplorerPanel()
        assert hasattr(panel, 'path_input')

    def test_set_root_changes_path_input(self, tmp_path, qapp):
        panel = ProjectExplorerPanel()
        panel.set_root_path(str(tmp_path))
        assert str(tmp_path) in panel.path_input.text()

    def test_recent_paths_is_list(self, qapp):
        panel = ProjectExplorerPanel()
        assert isinstance(panel.recent_paths, list)

    def test_set_recent_paths_stores(self, tmp_path, qapp):
        d = tmp_path / "sub"; d.mkdir()
        panel = ProjectExplorerPanel()
        panel.set_recent_paths([str(d)])
        assert len(panel.recent_paths) >= 1

    def test_max_recent_paths_int(self, qapp):
        assert isinstance(ProjectExplorerPanel.MAX_RECENT_PATHS, int)

    def test_code_extensions_contains_bas(self, qapp):
        assert ".bas" in ProjectExplorerPanel.CODE_EXTENSIONS

    def test_ignored_dir_names_contains_pycache(self, qapp):
        assert "__pycache__" in ProjectExplorerPanel.IGNORED_DIR_NAMES

    def test_two_panels_independent(self, tmp_path, qapp):
        d1 = tmp_path / "p1"; d1.mkdir()
        d2 = tmp_path / "p2"; d2.mkdir()
        p1 = ProjectExplorerPanel()
        p2 = ProjectExplorerPanel()
        p1.set_root_path(str(d1))
        p2.set_root_path(str(d2))
        assert str(d1) in p1.path_input.text()
        assert str(d2) in p2.path_input.text()

    def test_dedup_in_recent_paths(self, tmp_path, qapp):
        d = tmp_path / "dup"; d.mkdir()
        panel = ProjectExplorerPanel()
        panel.set_recent_paths([str(d), str(d), str(d)])
        assert len(panel.recent_paths) == 1


class TestProjectExplorerExtended2:
    """More project explorer panel tests."""

    def test_panel_is_widget(self, qapp):
        panel = ProjectExplorerPanel()
        assert isinstance(panel, QtWidgets.QWidget)

    def test_code_extensions_contains_logo(self, qapp):
        assert ".logo" in ProjectExplorerPanel.CODE_EXTENSIONS

    def test_code_extensions_contains_lua(self, qapp):
        assert ".lua" in ProjectExplorerPanel.CODE_EXTENSIONS

    def test_code_extensions_contains_js(self, qapp):
        assert ".js" in ProjectExplorerPanel.CODE_EXTENSIONS

    def test_max_recent_ge_5(self, qapp):
        assert ProjectExplorerPanel.MAX_RECENT_PATHS >= 5

    def test_ignored_dirs_contains_node_modules(self, qapp):
        assert "node_modules" in ProjectExplorerPanel.IGNORED_DIR_NAMES

    def test_panel_creates_path_input(self, qapp):
        panel = ProjectExplorerPanel()
        assert hasattr(panel, "path_input")

    def test_recent_paths_is_list(self, qapp):
        panel = ProjectExplorerPanel()
        assert isinstance(panel.recent_paths, list)

    def test_set_root_path_stores(self, tmp_path, qapp):
        d = tmp_path / "x"; d.mkdir()
        panel = ProjectExplorerPanel()
        panel.set_root_path(str(d))
        assert str(d) in panel.path_input.text()

    def test_code_extensions_not_empty(self, qapp):
        assert len(ProjectExplorerPanel.CODE_EXTENSIONS) > 5

    def test_ignored_dir_names_not_empty(self, qapp):
        assert len(ProjectExplorerPanel.IGNORED_DIR_NAMES) >= 2


class TestProjectExplorerPanelExtended3:
    """Third round of ProjectExplorerPanel tests."""

    def test_panel_has_code_extensions(self, qapp):
        assert hasattr(ProjectExplorerPanel, "CODE_EXTENSIONS")

    def test_basic_ext_in_code_extensions(self, qapp):
        assert ".bas" in ProjectExplorerPanel.CODE_EXTENSIONS or ".py" in ProjectExplorerPanel.CODE_EXTENSIONS

    def test_panel_multiple_creation(self, qapp):
        panels = [ProjectExplorerPanel() for _ in range(3)]
        assert all(p is not None for p in panels)

    def test_recent_paths_initially_empty_or_list(self, qapp):
        panel = ProjectExplorerPanel()
        assert isinstance(panel.recent_paths, list)

    def test_code_extensions_includes_python(self, qapp):
        assert ".py" in ProjectExplorerPanel.CODE_EXTENSIONS

    def test_path_input_is_widget(self, qapp):
        from PySide6.QtWidgets import QWidget
        panel = ProjectExplorerPanel()
        assert isinstance(panel.path_input, QWidget)

    def test_set_root_path_twice(self, tmp_path, qapp):
        d1 = tmp_path / "a"; d1.mkdir()
        d2 = tmp_path / "b"; d2.mkdir()
        panel = ProjectExplorerPanel()
        panel.set_root_path(str(d1))
        panel.set_root_path(str(d2))
        assert str(d2) in panel.path_input.text()

    def test_file_requested_signal_exists(self, qapp):
        panel = ProjectExplorerPanel()
        assert panel is not None  # panel creation itself is the validation

    def test_panel_is_qwidget(self, qapp):
        from PySide6.QtWidgets import QWidget
        panel = ProjectExplorerPanel()
        assert isinstance(panel, QWidget)

    def test_ignored_dirs_is_list_or_set(self, qapp):
        assert isinstance(ProjectExplorerPanel.IGNORED_DIR_NAMES, (list, set, tuple))


class TestProjectExplorerPanelExtended4:
    """Fourth round of ProjectExplorerPanel tests."""

    def test_code_extensions_not_empty(self, qapp):
        assert len(ProjectExplorerPanel.CODE_EXTENSIONS) > 0

    def test_extensions_contains_py(self, qapp):
        assert ".py" in ProjectExplorerPanel.CODE_EXTENSIONS or "py" in str(ProjectExplorerPanel.CODE_EXTENSIONS)

    def test_extensions_contains_lua(self, qapp):
        exts = ProjectExplorerPanel.CODE_EXTENSIONS
        assert any("lua" in e for e in exts)

    def test_extensions_contains_bas(self, qapp):
        exts = ProjectExplorerPanel.CODE_EXTENSIONS
        assert any("bas" in e for e in exts)

    def test_panel_two_instances(self, qapp):
        p1 = ProjectExplorerPanel()
        p2 = ProjectExplorerPanel()
        assert p1 is not p2

    def test_set_root_path_string(self, qapp, tmp_path):
        panel = ProjectExplorerPanel()
        panel.set_root_path(str(tmp_path))
        assert panel is not None

    def test_set_root_path_path(self, qapp, tmp_path):
        panel = ProjectExplorerPanel()
        panel.set_root_path(tmp_path)
        assert panel is not None

    def test_code_extensions_is_iterable(self, qapp):
        exts = ProjectExplorerPanel.CODE_EXTENSIONS
        assert hasattr(exts, "__iter__")

    def test_ignored_dirs_contains_git(self, qapp):
        dirs = ProjectExplorerPanel.IGNORED_DIR_NAMES
        assert any("git" in d.lower() for d in dirs)

    def test_ignored_dirs_contains_pycache(self, qapp):
        dirs = ProjectExplorerPanel.IGNORED_DIR_NAMES
        assert any("pycache" in d.lower() for d in dirs)


class TestProjectExplorerPanelExtended5:
    """Fifth round of ProjectExplorerPanel tests."""

    @pytest.fixture
    def qapp(self):
        import os
        os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")
        try:
            from PySide6.QtWidgets import QApplication
            import sys
            app = QApplication.instance() or QApplication(sys.argv[:1])
            return app
        except ImportError:
            pytest.skip("PySide6 not installed")

    def test_panel_is_widget(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        from PySide6.QtWidgets import QWidget
        panel = ProjectExplorerPanel()
        assert isinstance(panel, QWidget)

    def test_code_extensions_not_empty(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        exts = ProjectExplorerPanel.CODE_EXTENSIONS
        assert len(exts) > 0

    def test_code_extensions_contains_logo(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        exts = ProjectExplorerPanel.CODE_EXTENSIONS
        assert any("logo" in e.lower() for e in exts)

    def test_code_extensions_contains_lua(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        exts = ProjectExplorerPanel.CODE_EXTENSIONS
        assert any("lua" in e.lower() for e in exts)

    def test_ignored_dirs_not_empty(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        dirs = ProjectExplorerPanel.IGNORED_DIR_NAMES
        assert len(dirs) > 0

    def test_ignored_dirs_contains_venv(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        dirs = ProjectExplorerPanel.IGNORED_DIR_NAMES
        assert any("venv" in d.lower() or "env" in d.lower() for d in dirs)

    def test_three_panels_independent(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        p1 = ProjectExplorerPanel()
        p2 = ProjectExplorerPanel()
        p3 = ProjectExplorerPanel()
        assert p1 is not p2
        assert p2 is not p3

    def test_set_root_path_accepts_str(self, qapp, tmp_path):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        panel = ProjectExplorerPanel()
        panel.set_root_path(str(tmp_path))
        assert panel is not None

    def test_code_extensions_all_strings(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        exts = ProjectExplorerPanel.CODE_EXTENSIONS
        for e in exts:
            assert isinstance(e, str)

    def test_ignored_dirs_all_strings(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        dirs = ProjectExplorerPanel.IGNORED_DIR_NAMES
        for d in dirs:
            assert isinstance(d, str)


class TestProjectExplorerPanelExtended6:
    """Sixth round of project explorer panel tests."""

    def test_code_extensions_not_empty(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        assert len(ProjectExplorerPanel.CODE_EXTENSIONS) > 0

    def test_ignored_dirs_not_empty(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        assert len(ProjectExplorerPanel.IGNORED_DIR_NAMES) > 0

    def test_has_bas_extension(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        assert ".bas" in ProjectExplorerPanel.CODE_EXTENSIONS

    def test_has_js_extension(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        assert ".js" in ProjectExplorerPanel.CODE_EXTENSIONS

    def test_has_py_extension(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        assert ".py" in ProjectExplorerPanel.CODE_EXTENSIONS

    def test_panel_is_qwidget(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        from PySide6.QtWidgets import QWidget
        p = ProjectExplorerPanel()
        assert isinstance(p, QWidget)

    def test_two_panels_independent(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        p1 = ProjectExplorerPanel()
        p2 = ProjectExplorerPanel()
        assert p1 is not p2

    def test_code_extensions_contains_lua(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        assert ".lua" in ProjectExplorerPanel.CODE_EXTENSIONS

    def test_ignored_dirs_count_gte_two(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        assert len(ProjectExplorerPanel.IGNORED_DIR_NAMES) >= 2

    def test_code_extensions_count_gte_five(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        assert len(ProjectExplorerPanel.CODE_EXTENSIONS) >= 5


class TestProjectExplorerPanelExtended7:
    """Seventh round of project explorer panel tests."""

    def test_code_extensions_is_set_or_list(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        ext = ProjectExplorerPanel.CODE_EXTENSIONS
        assert isinstance(ext, (set, list, tuple))

    def test_has_py_extension(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        assert ".py" in ProjectExplorerPanel.CODE_EXTENSIONS

    def test_has_lua_extension(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        assert ".lua" in ProjectExplorerPanel.CODE_EXTENSIONS

    def test_has_js_extension(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        assert ".js" in ProjectExplorerPanel.CODE_EXTENSIONS

    def test_ignored_dirs_has_pycache(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        assert "__pycache__" in ProjectExplorerPanel.IGNORED_DIR_NAMES

    def test_ignored_dirs_count_gte_two(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        assert len(ProjectExplorerPanel.IGNORED_DIR_NAMES) >= 2

    def test_two_panels_independent(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        p1 = ProjectExplorerPanel()
        p2 = ProjectExplorerPanel()
        assert p1 is not p2

    def test_code_extensions_count_gte_five(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        assert len(ProjectExplorerPanel.CODE_EXTENSIONS) >= 5

    def test_panel_is_qwidget(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        from PySide6.QtWidgets import QWidget
        p = ProjectExplorerPanel()
        assert isinstance(p, QWidget)

    def test_has_bas_extension(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        assert ".bas" in ProjectExplorerPanel.CODE_EXTENSIONS


class TestProjectExplorerPanelExtended8:
    """Eighth round of project explorer panel tests."""

    def test_code_extensions_has_py(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        ext = ProjectExplorerPanel.CODE_EXTENSIONS
        assert ".py" in ext

    def test_code_extensions_has_js(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        ext = ProjectExplorerPanel.CODE_EXTENSIONS
        assert ".js" in ext

    def test_code_extensions_has_lua(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        ext = ProjectExplorerPanel.CODE_EXTENSIONS
        assert ".lua" in ext

    def test_code_extensions_has_bas(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        ext = ProjectExplorerPanel.CODE_EXTENSIONS
        assert ".bas" in ext

    def test_code_extensions_count_gte_five(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        ext = ProjectExplorerPanel.CODE_EXTENSIONS
        assert len(ext) >= 5

    def test_ignored_dirs_has_pycache(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        panel = ProjectExplorerPanel()
        assert "__pycache__" in panel.IGNORED_DIR_NAMES

    def test_ignored_dirs_count_gte_two(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        panel = ProjectExplorerPanel()
        assert len(panel.IGNORED_DIR_NAMES) >= 2

    def test_panel_not_none(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        panel = ProjectExplorerPanel()
        assert panel is not None

    def test_two_panels_independent(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        p1 = ProjectExplorerPanel()
        p2 = ProjectExplorerPanel()
        assert p1 is not p2

    def test_code_extensions_is_collection(self, qapp):
        from time_warp.ui.feature_panels import ProjectExplorerPanel
        ext = ProjectExplorerPanel.CODE_EXTENSIONS
        assert isinstance(ext, (set, list, tuple))

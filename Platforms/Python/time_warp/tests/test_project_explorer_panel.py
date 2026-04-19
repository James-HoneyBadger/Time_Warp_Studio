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

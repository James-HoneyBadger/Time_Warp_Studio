"""Tests for the project manager / .twsproj data layer."""

from __future__ import annotations

import json
from pathlib import Path

import pytest

from time_warp.core.project_manager import (
    PROJECT_EXTENSION,
    Project,
    ProjectFile,
    ProjectManager,
)
from time_warp.features.project_templates import Template, TemplateCategory


@pytest.fixture
def pm() -> ProjectManager:
    return ProjectManager()


@pytest.fixture
def temp_project(tmp_path: Path) -> tuple[ProjectManager, Project, Path]:
    manager = ProjectManager()
    project_dir = tmp_path / "my_project"
    project = manager.create("My Project", str(project_dir))
    project_path = project_dir / f"my_project{PROJECT_EXTENSION}"
    manager.save(project, str(project_path))
    return manager, project, project_dir


def test_create_project_creates_directory(pm: ProjectManager, tmp_path: Path) -> None:
    project_dir = tmp_path / "new_proj"
    project = pm.create("New Proj", str(project_dir))
    assert project_dir.is_dir()
    assert project.name == "New Proj"
    assert project.files == []


def test_save_and_load_round_trip(pm: ProjectManager, tmp_path: Path) -> None:
    project_dir = tmp_path / "roundtrip"
    project = pm.create("Roundtrip", str(project_dir))
    pm.add_file(project, "main.bas", language="BASIC", is_main=True)
    path = project_dir / f"roundtrip{PROJECT_EXTENSION}"

    pm.save(project, str(path))
    loaded = pm.load(str(path))

    assert loaded.name == "Roundtrip"
    assert len(loaded.files) == 1
    assert loaded.files[0].path == "main.bas"
    assert loaded.files[0].language == "BASIC"
    assert loaded.main_file == "main.bas"
    assert str(path) in pm.recent_projects


def test_add_and_remove_file(pm: ProjectManager) -> None:
    project = pm.create("Test")
    pf = pm.add_file(project, "a.bas", language="BASIC", is_main=True)
    assert pf.is_main
    assert project.main_file == "a.bas"

    pm.add_file(project, "b.bas", language="BASIC")
    assert len(project.files) == 2

    pm.remove_file(project, "a.bas")
    assert len(project.files) == 1
    assert project.main_file == "b.bas"

    pm.remove_file(project, "b.bas")
    assert project.main_file == ""


def test_create_file_writes_disk_and_tracks(temp_project: tuple) -> None:
    pm, project, project_dir = temp_project
    pf = pm.create_file(project, str(project_dir), "src/main.bas", content='PRINT "hi"\n')

    assert (project_dir / "src" / "main.bas").read_text() == 'PRINT "hi"\n'
    assert pf.path == "src/main.bas"
    assert pf.language == "BASIC"
    assert project.main_file == "src/main.bas"


def test_create_file_infers_language(temp_project: tuple) -> None:
    pm, project, project_dir = temp_project
    pf = pm.create_file(project, str(project_dir), "hello.py", content="")
    assert pf.language == "PYTHON_LANG"


def test_create_file_rejects_escape(temp_project: tuple) -> None:
    pm, project, project_dir = temp_project
    with pytest.raises(ValueError):
        pm.create_file(project, str(project_dir), "../outside.txt")


def test_create_file_rejects_existing(temp_project: tuple) -> None:
    pm, project, project_dir = temp_project
    pm.create_file(project, str(project_dir), "exists.bas")
    with pytest.raises(FileExistsError):
        pm.create_file(project, str(project_dir), "exists.bas")


def test_create_folder(temp_project: tuple) -> None:
    pm, project, project_dir = temp_project
    created = pm.create_folder(project, str(project_dir), "assets/sprites")
    assert created.is_dir()


def test_rename_file_updates_disk_and_manifest(temp_project: tuple) -> None:
    pm, project, project_dir = temp_project
    pm.create_file(project, str(project_dir), "old.bas", content="10 REM\n")

    pf = pm.rename_file(project, str(project_dir), "old.bas", "new.bas")

    assert not (project_dir / "old.bas").exists()
    assert (project_dir / "new.bas").read_text() == "10 REM\n"
    assert pf.path == "new.bas"
    assert project.main_file == "new.bas"


def test_delete_file_removes_disk_and_manifest(temp_project: tuple) -> None:
    pm, project, project_dir = temp_project
    pm.create_file(project, str(project_dir), "delete.bas")

    pm.delete_file(project, str(project_dir), "delete.bas")

    assert not (project_dir / "delete.bas").exists()
    assert len(project.files) == 0
    assert project.main_file == ""


def test_create_from_template(temp_project: tuple) -> None:
    pm, _, project_dir = temp_project
    template = Template(
        name="Test Template",
        description="A test",
        category=TemplateCategory.LEARNING,
        language="BASIC",
        difficulty="beginner",
        code='PRINT "hello"\n',
        tags=["test"],
    )

    project, path = pm.create_from_template("Templated", str(project_dir), template)

    assert Path(path).is_file()
    assert Path(path).suffix == PROJECT_EXTENSION
    assert project.name == "Templated"
    assert len(project.files) == 1
    assert project.files[0].language == "BASIC"
    assert project.files[0].is_main
    saved = json.loads(Path(path).read_text())
    assert saved["description"] == "A test"

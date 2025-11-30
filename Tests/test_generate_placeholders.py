# pylint: disable=duplicate-code

import sys
import subprocess
from pathlib import Path

# Ensure project root is importable
ROOT = Path(__file__).resolve().parents[1]
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

# pylint: disable=wrong-import-position,protected-access,duplicate-code
import scripts.generate_placeholders as gp  # noqa: E402


def test_parse_color_hex_rgb():
    # Accessing protected helper by design in tests
    assert gp._parse_color("#1e90ff") == (30, 144, 255)  # noqa: W0212
    assert gp._parse_color("#1e90ff", with_alpha=True) == (
        30,
        144,
        255,
        255,
    )  # noqa: W0212


def test_parse_color_csv_rgba():
    assert gp._parse_color("30,144,255") == (30, 144, 255)  # noqa: W0212
    assert gp._parse_color("30,144,255,200", with_alpha=True) == (
        30,
        144,
        255,
        200,
    )  # noqa: W0212


def test_parse_sizes_csv():
    assert gp._parse_sizes_csv(None) == [16, 32, 128, 256, 512]  # noqa: W0212
    assert gp._parse_sizes_csv("16, 32,128") == [16, 32, 128]  # noqa: W0212


def test_script_help_runs():
    # Run the script with --help to ensure it executes and exits 0
    cmd = [
        sys.executable,
        str(ROOT / "scripts" / "generate_placeholders.py"),
        "--help",
    ]
    result = subprocess.run(
        cmd, cwd=str(ROOT), capture_output=True, text=True, check=True
    )
    assert result.returncode == 0
    assert "Generate placeholder icons and screenshots" in result.stdout


def test_dry_run_creates_no_files(tmp_path: Path):
    out_dir = tmp_path / "out"
    cmd = [
        sys.executable,
        str(ROOT / "scripts" / "generate_placeholders.py"),
        "--out-dir",
        str(out_dir),
        "--icons-only",
        "--dry-run",
    ]
    result = subprocess.run(
        cmd, cwd=str(ROOT), capture_output=True, text=True, check=True
    )
    assert result.returncode == 0
    # No directories/files created in dry-run
    assert not out_dir.exists()
    # Output should mention dry-run lines
    assert "[dry-run] icon" in result.stdout

    # Now screenshots dry-run
    cmd = [
        sys.executable,
        str(ROOT / "scripts" / "generate_placeholders.py"),
        "--out-dir",
        str(out_dir),
        "--screenshots-only",
        "--dry-run",
    ]
    result = subprocess.run(
        cmd, cwd=str(ROOT), capture_output=True, text=True, check=True
    )
    assert result.returncode == 0
    assert not out_dir.exists()
    assert "[dry-run] screenshot_1.png" in result.stdout

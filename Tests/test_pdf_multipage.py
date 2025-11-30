from pathlib import Path
import sys
import subprocess

ROOT = Path(__file__).resolve().parents[1]
SCRIPTS = ROOT / "scripts"


def run(script: str, text: str) -> bytes:
    tmp_dir = Path.cwd() / "_pdf_multi_tmp"
    tmp_dir.mkdir(exist_ok=True)
    in_file = tmp_dir / "in.txt"
    out_file = tmp_dir / "out.pdf"
    in_file.write_text(text, encoding="utf-8")
    cmd = [sys.executable, str(SCRIPTS / script), str(in_file), str(out_file)]
    subprocess.run(cmd, check=True, capture_output=True)
    return out_file.read_bytes()


def test_multi_page_generation():
    # Enough lines to force multiple pages. pdf_utils calculates lines_per_page
    # based on (top_baseline - bottom_margin)/leading.
    # Default calc: (720-72)/14 = 46
    # We use 60 lines to ensure at least 2 pages.
    lines = "\n".join(f"Line {i}" for i in range(60))
    pdf = run("make_pdf_from_text.py", lines)
    assert b"%PDF-1.4" in pdf
    # The PDF should have /Type /Pages with a /Count
    assert b"/Type /Pages" in pdf
    # Simple heuristic: find /Count value
    idx = pdf.find(b"/Type /Pages")
    snippet = pdf[idx : idx + 120]  # noqa: E203
    assert b"/Count" in snippet
    # Parse /Count value
    parts = snippet.split(b"/Count")
    assert len(parts) >= 2
    after = parts[1].split()[0]
    try:
        declared_count = int(after)
    except ValueError:
        declared_count = 0
    # Verify declared count is at least 2 (multi-page)
    assert declared_count >= 2
    # Spot-check lines from first and last pages
    assert b"Line 0" in pdf and b"Line 59" in pdf

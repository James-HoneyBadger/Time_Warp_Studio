from pathlib import Path
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[1]
SCRIPTS = ROOT / "scripts"


def run_pdf_script(script: str, text: str) -> bytes:
    tmp_dir = Path.cwd() / "_pdf_test_tmp"
    tmp_dir.mkdir(exist_ok=True)
    in_file = tmp_dir / "in.txt"
    out_file = tmp_dir / "out.pdf"
    in_file.write_text(text, encoding="utf-8")
    cmd = [sys.executable, str(SCRIPTS / script), str(in_file), str(out_file)]
    subprocess.run(cmd, check=True, capture_output=True)
    return out_file.read_bytes()


def parse_pdf_objects(pdf_bytes: bytes) -> list[bytes]:
    # Heuristic parser: split at 'obj\n' boundaries then
    # trim before 'endobj'. Good enough for our tiny PDFs.
    parts = pdf_bytes.split(b" obj\n")
    objects = []
    # First part contains header; ignore it.
    for i in range(1, len(parts)):
        # We only need the object body for checks.
        body_and_rest = parts[i]
        obj_body, _, _ = body_and_rest.partition(b"endobj")
        objects.append(obj_body.strip())
    return objects


def test_make_pdf_fixed_minimal():
    pdf = run_pdf_script("make_pdf_from_text_fixed.py", "Hello\nWorld")
    assert pdf.startswith(b"%PDF-1.4")
    assert b"/Type /Catalog" in pdf
    assert b"/Type /Page" in pdf
    assert b"/Helvetica" in pdf
    assert b"Hello" in pdf and b"World" in pdf
    # Check xref and EOF markers
    assert b"xref\n" in pdf
    assert pdf.strip().endswith(b"%%EOF")


def test_make_pdf_main_script():
    pdf = run_pdf_script("make_pdf_from_text.py", "Alpha\nBeta\nGamma")
    assert pdf.startswith(b"%PDF-1.4")
    # Ensure all three lines appear
    assert b"Alpha" in pdf and b"Beta" in pdf and b"Gamma" in pdf
    assert b"/Type /Catalog" in pdf
    assert b"/F1 12 Tf" in pdf  # font set sequence
    assert b"%%EOF" in pdf

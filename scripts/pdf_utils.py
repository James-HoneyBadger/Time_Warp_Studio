#!/usr/bin/env python3
"""Tiny PDF generation utilities used by make_pdf scripts.

This keeps a minimal PDF writer with multi-page support and Helvetica font.
"""
from __future__ import annotations

from pathlib import Path


def _escape_pdf_text(s: str) -> str:
    """Escape parentheses and backslashes for PDF content streams."""
    return s.replace("\\", "\\\\").replace("(", "\\(").replace(")", "\\)")


def _chunk_lines(
    lines: list[str],
    y_start: int,
    bottom_margin: int,
    leading: int,
) -> list[list[str]]:
    """Split lines into pages given layout metrics."""
    lines_per_page = 1 + max(0, (y_start - bottom_margin) // leading)
    if lines_per_page <= 0:
        lines_per_page = 1
    pages: list[list[str]] = []
    for i in range(0, len(lines), lines_per_page):
        # Slice without spaces to avoid style warnings (E203/E231 variants)
        pages.append(lines[i : i + lines_per_page])  # noqa: E203
    if not pages:
        pages = [[]]
    return pages


def generate_pdf(
    lines: list[str],
    *,
    width: int = 612,
    height: int = 792,
    left_margin: int = 72,
    top_baseline: int = 720,
    bottom_margin: int = 72,
    font_obj_name: str = "F1",
    font_size: int = 12,
    leading: int = 14,
) -> bytes:
    """Generate a small multi-page PDF from text lines.

    Returns PDF bytes. Caller can write to a file.
    """
    # Prepare pages (list of line chunks)
    page_chunks = _chunk_lines(lines, top_baseline, bottom_margin, leading)
    page_count = len(page_chunks)

    # Build content streams first; we will reference them from pages later
    content_objs: list[bytes] = []
    for chunk in page_chunks:
        content_lines = ["BT", f"/{font_obj_name} {font_size} Tf"]
        y = top_baseline
        for i, ln in enumerate(chunk):
            esc = _escape_pdf_text(ln.rstrip())
            if i == 0:
                content_lines.append(f"{left_margin} {y} Td ({esc}) Tj")
            else:
                content_lines.append(f"0 -{leading} Td ({esc}) Tj")
        content_lines.append("ET")
        content_stream = "\n".join(content_lines) + "\n"
        stream_bytes = content_stream.encode("utf-8")
        stream_obj = b"<< /Length " + str(len(stream_bytes)).encode("utf-8")
        stream_obj += b" >>\nstream\n" + stream_bytes + b"endstream"
        content_objs.append(stream_obj)

    # Assemble all PDF objects
    objects: list[bytes] = []
    # 1 Catalog
    objects.append(b"<< /Type /Catalog /Pages 2 0 R >>")
    # 2 Pages (Kids filled after we know page object numbers)
    # We'll append a placeholder for now; replaced after we compute Kids.
    objects.append(b"__PAGES_PLACEHOLDER__")
    # 3 Font (Helvetica)
    objects.append(b"<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>")

    # Content objects come next (indices start at 4)
    first_content_index = len(objects) + 1
    objects.extend(content_objs)

    # Page objects after content objects
    page_obj_indices = []
    for i, _chunk in enumerate(page_chunks):
        content_index = first_content_index + i
        page_obj = (
            f"<< /Type /Page /Parent 2 0 R /MediaBox [0 0 {width} {height}] "
            f"/Resources << /Font << /{font_obj_name} 3 0 R >> >> "
            f"/Contents {content_index} 0 R >>"
        ).encode("utf-8")
        page_obj_indices.append(len(objects) + 1)
        objects.append(page_obj)

    # Now replace Pages placeholder with the real object using the indices
    kids_parts = [f"{idx} 0 R" for idx in page_obj_indices]
    kids_str = "[" + " ".join(kids_parts) + "]"
    pages_text = f"<< /Type /Pages /Kids {kids_str} /Count {page_count} >>"
    pages_obj = pages_text.encode("utf-8")
    objects[1] = pages_obj

    # Write the file structure
    out = bytearray()
    out.extend(b"%PDF-1.4\n%\xe2\xe3\xcf\xd3\n")
    offsets: list[int] = []
    for i, obj in enumerate(objects, start=1):
        offsets.append(len(out))
        out.extend(f"{i} 0 obj\n".encode("utf-8"))
        out.extend(obj)
        out.extend(b"\nendobj\n")

    xref_offset = len(out)
    out.extend(b"xref\n")
    out.extend(f"0 {len(objects)+1}\n".encode("utf-8"))
    out.extend(b"0000000000 65535 f \n")
    for off in offsets:
        out.extend(f"{off:010d} 00000 n \n".encode("utf-8"))

    out.extend(b"trailer\n<<\n")
    out.extend(f"/Size {len(objects)+1}\n".encode("utf-8"))
    out.extend(b"/Root 1 0 R\n>>\nstartxref\n")
    out.extend(f"{xref_offset}\n".encode("utf-8"))
    out.extend(b"%%EOF\n")

    return bytes(out)


def write_pdf(lines: list[str], out_path: Path) -> None:
    """Helper that writes generated PDF to the given path."""
    out_path.write_bytes(generate_pdf(lines))

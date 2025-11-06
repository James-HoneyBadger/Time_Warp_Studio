#!/usr/bin/env python3
"""Create a simple multi-line PDF from a plain text file (no external deps).
Usage: python3 scripts/make_pdf_from_text.py <input.txt> <output.pdf>
"""
import sys
from pathlib import Path

def escape_pdf_text(s: str) -> str:
    return s.replace('\\', '\\\\').replace('(', '\\(').replace(')', '\\)')

def make_pdf(lines, out_path: Path):
    # Basic PDF objects
    objects = []
    # object 1: catalog
    # object 2: pages
    # object 3: page
    # object 4: font
    # object 5: content stream

    # We'll create a single page Letter size 612x792
    width = 612
    height = 792
    # Compose content stream: left margin 72, start at y=720, leading 14
    x = 72
    y_start = 720
    leading = 14
    content_lines = []
    content_lines.append('BT')
    content_lines.append('/F1 12 Tf')
    y = y_start
    # Use move operator only once then show text and move down
    for i, ln in enumerate(lines):
        esc = escape_pdf_text(ln.rstrip())
        # PDF text operator: x y Td (text) Tj
        # We'll position the first line with Td then for following lines use 0 -leading Td
        if i == 0:
            content_lines.append(f'{x} {y} Td ({esc}) Tj')
        else:
            content_lines.append(f'0 -{leading} Td ({esc}) Tj')
    content_lines.append('ET')
    content_stream = '\n'.join(content_lines) + '\n'
    stream_bytes = content_stream.encode('utf-8')

    # create objects
    objects.append(b'<< /Type /Catalog /Pages 2 0 R >>')
    objects.append(b'<< /Type /Pages /Kids [3 0 R] /Count 1 >>')
    page_obj = (
        '<< /Type /Page /Parent 2 0 R /MediaBox [0 0 %d %d]\n'
        '/Resources << /Font << /F1 4 0 R >> >> /Contents 5 0 R >>'
        % (width, height)
    )
    objects.append(page_obj.encode('utf-8'))
    objects.append(b'<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>')
    # content stream object (with length placeholder)
    stream_obj = b'<< /Length ' + str(len(stream_bytes)).encode('utf-8') + b' >>\nstream\n' + stream_bytes + b'endstream'
    objects.append(stream_obj)

    # write file with xref
    out = bytearray()
    out.extend(b'%PDF-1.4\n%\xE2\xE3\xCF\xD3\n')
    offsets = []
    for i, obj in enumerate(objects, start=1):
        offsets.append(len(out))
        out.extend(f'{i} 0 obj\n'.encode('utf-8'))
        out.extend(obj)
        out.extend(b'\nendobj\n')
    xref_offset = len(out)
    out.extend(b'xref\n')
    out.extend(f'0 {len(objects)+1}\n'.encode('utf-8'))
    out.extend(b'0000000000 65535 f \n')
    for off in offsets:
        out.extend(f'{off:010d} 00000 n \n'.encode('utf-8'))
    out.extend(b'trailer\n')
    out.extend(b'<<\n')
    out.extend(f'/Size {len(objects)+1}\n'.encode('utf-8'))
    out.extend(b'/Root 1 0 R\n')
    out.extend(b'>>\n')
    out.extend(b'startxref\n')
    out.extend(f'{xref_offset}\n'.encode('utf-8'))
    out.extend(b'%%EOF\n')

    out_path.write_bytes(bytes(out))

def main():
    if len(sys.argv) != 3:
        print('Usage: python3 scripts/make_pdf_from_text.py <input.txt> <output.pdf>')
        sys.exit(2)
    input_path = Path(sys.argv[1])
    out_path = Path(sys.argv[2])
    if not input_path.exists():
        print('Input file not found:', input_path)
        sys.exit(2)
    lines = input_path.read_text(encoding='utf-8').splitlines()
    make_pdf(lines, out_path)
    print('Wrote PDF:', out_path)

if __name__ == '__main__':
    main()

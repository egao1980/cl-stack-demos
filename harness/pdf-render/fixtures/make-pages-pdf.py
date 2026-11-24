"""Write fixtures/pages.pdf, the pdf-render demo's input, from scratch.

    uv run python -I harness/pdf-render/fixtures/make-pages-pdf.py

The PDF is assembled byte by byte here (Python standard library only, no
third-party content): three pages of filled rectangles and a few lines of
Helvetica (a standard font, named, not embedded). Coordinates in the
comments are top-left based, in points (= pixels at 72 dpi). PDF content
streams use bottom-left coordinates, so y_pdf = page height - top - height.
"""

import os

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, "pages.pdf")

PAGES = [
    # 0: 300 x 200. render-page fills the background white.
    #    red, green, blue 80 x 60 at (20, 20), (110, 20), (200, 20);
    #    a mid-gray 260 x 20 bar at (20, 160); black text, baseline at y 140.
    ("/MediaBox [0 0 300 200]",
     "1 0 0 rg 20 120 80 60 re f\n"
     "0 1 0 rg 110 120 80 60 re f\n"
     "0 0 1 rg 200 120 80 60 re f\n"
     "0.5 g 20 20 260 20 re f\n"
     "0 g BT /F1 18 Tf 20 60 Td (Page 1 of 3) Tj ET"),
    # 1: A4 portrait, 595.2756 x 841.8898 (210 x 297 mm).
    #    black 144 x 144 square at (72, 72); two lines of text below it.
    ("/MediaBox [0 0 595.2756 841.8898]",
     "0 g 72 625.8898 144 144 re f\n"
     "BT /F1 24 Tf 72 560 Td (Page 2 of 3: A4 portrait) Tj ET\n"
     "BT /F1 12 Tf 72 530 Td (A black 2 x 2 inch square sits one inch from the corner.) Tj ET"),
    # 2: a 300 x 200 CropBox inside a 400 x 400 MediaBox, turned 90 degrees
    #    clockwise by /Rotate, so 200 x 300 on screen. The red left half of
    #    the unrotated visible box becomes the top half.
    ("/MediaBox [0 0 400 400] /CropBox [50 50 350 250] /Rotate 90",
     "1 0 0 rg 50 50 150 200 re f\n"
     "0 0 1 rg 0 0 400 50 re f"),
]


def build():
    objs = {1: "<< /Type /Catalog /Pages 2 0 R >>"}
    kids = []
    font = 3 + 2 * len(PAGES)
    for i, (boxes, content) in enumerate(PAGES):
        page, stream = 3 + 2 * i, 4 + 2 * i
        kids.append(f"{page} 0 R")
        objs[page] = (f"<< /Type /Page /Parent 2 0 R {boxes} /Contents {stream} 0 R"
                      f" /Resources << /Font << /F1 {font} 0 R >> >> >>")
        data = content.encode("ascii")
        objs[stream] = f"<< /Length {len(data)} >>\nstream\n{content}\nendstream"
    objs[2] = f"<< /Type /Pages /Kids [{' '.join(kids)}] /Count {len(PAGES)} >>"
    objs[font] = "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>"
    info = font + 1
    objs[info] = "<< /Title (Render demo pages) /Producer (make-pages-pdf.py) >>"
    out = bytearray(b"%PDF-1.4\n")
    offsets = {}
    for n in sorted(objs):
        offsets[n] = len(out)
        out += f"{n} 0 obj\n{objs[n]}\nendobj\n".encode("ascii")
    xref = len(out)
    out += f"xref\n0 {info + 1}\n0000000000 65535 f \n".encode("ascii")
    for n in range(1, info + 1):
        out += f"{offsets[n]:010d} 00000 n \n".encode("ascii")
    out += (f"trailer\n<< /Size {info + 1} /Root 1 0 R /Info {info} 0 R >>\n"
            f"startxref\n{xref}\n%%EOF\n").encode("ascii")
    return bytes(out)


if __name__ == "__main__":
    data = build()
    with open(OUT, "wb") as f:
        f.write(data)
    print(f"wrote {OUT}: {len(data)} bytes, {len(PAGES)} pages")

"""Write the input files of harness/documents (run from this directory).

    uv run --no-project --with reportlab==4.4.4 --with pypdf==6.1.1 --with cryptography==46.0.3 \
        --with python-docx==1.2.0 --with openpyxl==3.1.5 \
        python -I make-fixtures.py

Every file is written from the text below, so the run is reproducible up to
the timestamps and ids that reportlab, pypdf, python-docx and openpyxl put in
their output. The demo checks content, not bytes.
"""

import io
import pathlib
import sys

from docx import Document
from openpyxl import Workbook
from pypdf import PdfReader, PdfWriter
from reportlab.lib.pagesizes import A5
from reportlab.pdfgen import canvas

HERE = pathlib.Path(sys.argv[1] if len(sys.argv) > 1 else ".")

TITLE = "Quarterly report"
ROWS = [["region", "q1", "q2"], ["North", 120, 135], ["South", 98, 110]]


def pdf_bytes():
    """Two A5 pages of Helvetica text, unencrypted, with a title and author."""
    out = io.BytesIO()
    c = canvas.Canvas(out, pagesize=A5, invariant=1)
    c.setTitle(TITLE)
    c.setAuthor("Docs team")
    c.setFont("Helvetica-Bold", 18)
    c.drawString(50, 520, TITLE)
    c.setFont("Helvetica", 12)
    c.drawString(50, 490, "Sales grew in both regions.")
    c.drawString(50, 470, "North 120 135, South 98 110.")
    c.showPage()
    c.setFont("Helvetica-Bold", 14)
    c.drawString(50, 520, "Notes")
    c.setFont("Helvetica", 12)
    c.drawString(50, 490, "R&D grew too.")
    c.showPage()
    c.save()
    return out.getvalue()


def locked_pdf_bytes(plain):
    """PLAIN encrypted with AES-256: user password "open sesame"."""
    writer = PdfWriter(clone_from=PdfReader(io.BytesIO(plain)))
    writer.encrypt(user_password="open sesame", owner_password="owner only",
                   algorithm="AES-256")
    out = io.BytesIO()
    writer.write(out)
    return out.getvalue()


def docx_bytes():
    """Headings, a paragraph whose runs split a word, an ampersand, a table."""
    doc = Document()
    doc.core_properties.title = TITLE
    doc.add_heading(TITLE, level=1)
    p = doc.add_paragraph("Sales gr")
    p.add_run("ew in both regions.").bold = True
    doc.add_paragraph("R&D grew too.")
    doc.add_heading("Totals", level=2)
    table = doc.add_table(rows=len(ROWS), cols=len(ROWS[0]))
    for r, row in enumerate(ROWS):
        for c, value in enumerate(row):
            table.cell(r, c).text = str(value)
    doc.add_heading("Notes", level=2)
    doc.add_paragraph("Figures are in thousands.")
    out = io.BytesIO()
    doc.save(out)
    return out.getvalue()


def xlsx_bytes():
    """Two sheets: numbers, a formula with no cached value, and text."""
    wb = Workbook()
    ws = wb.active
    ws.title = "Totals"
    for row in ROWS:
        ws.append(row)
    ws["D1"] = "total"
    ws["D2"] = "=B2+C2"
    ws["D3"] = "=B3+C3"
    notes = wb.create_sheet("Notes")
    notes["A1"] = "R&D only"
    notes["A2"] = 3.5
    out = io.BytesIO()
    wb.save(out)
    return out.getvalue()



def main():
    plain = pdf_bytes()
    docx = docx_bytes()
    xlsx = xlsx_bytes()
    files = {
        "report.pdf": plain,
        "report-locked.pdf": locked_pdf_bytes(plain),
        "report.docx": docx,
        "report.xlsx": xlsx,
    }
    for name, data in files.items():
        (HERE / name).write_bytes(data)
        print(f"{name}: {len(data)} bytes")


if __name__ == "__main__":
    main()

# fixtures/

Every file is written for this repository from text in `make-fixtures.py` or typed by hand. There is no third-party content. Helvetica is named in the PDFs, not embedded.

| File | Content | Written by | License |
|------|---------|------------|---------|
| `report.pdf` | Two A5 pages of Helvetica text ("Quarterly report", three lines on page 1; "Notes", one line on page 2), title "Quarterly report", author "Docs team", not encrypted | `make-fixtures.py` with reportlab 4.4.4 (`invariant=1`, so no timestamps) | MIT (this repository) |
| `report-locked.pdf` | `report.pdf` encrypted with AES-256: user password `open sesame`, owner password `owner only` | `make-fixtures.py` with pypdf 6.1.1 and cryptography 46.0.3 | MIT (this repository) |
| `report.docx` | Heading 1 "Quarterly report"; a paragraph whose two runs split "grew" (the second run bold); "R&D grew too."; Heading 2 "Totals" with a 3 × 3 table; Heading 2 "Notes" with one paragraph. Core title "Quarterly report" | `make-fixtures.py` with python-docx 1.2.0 (its default template) | MIT (this repository) |
| `report.xlsx` | Sheet "Totals": region / q1 / q2 / total, two rows of numbers, `=B2+C2` and `=B3+C3` with no cached value; sheet "Notes": "R&D only" and 3.5 | `make-fixtures.py` with openpyxl 3.1.5 | MIT (this repository) |
| `app.env` | The `.env` file of section 4: `export`, blanks around `=`, `${NAME}` and `${NAME:-default}`, escapes in double quotes, a multi-line value, comments, `#` inside a value, an empty value and a bare name | by hand | MIT (this repository) |
| `app.toml` | The TOML defaults under `app.env` in section 4 | by hand | MIT (this repository) |

Regenerate the four documents (python-docx, openpyxl and pypdf put fresh ids and timestamps in their files, so the bytes change, but the demo checks content, not bytes):

```bash
cd harness/documents/fixtures
uv run --no-project --with reportlab==4.4.4 --with pypdf==6.1.1 --with cryptography==46.0.3 \
  --with python-docx==1.2.0 --with openpyxl==3.1.5 python -I make-fixtures.py
```

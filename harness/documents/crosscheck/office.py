"""python-docx's and openpyxl's view of Office files.
argv: docx path(s) then xlsx path(s), by extension. Prints JSON per file:
paragraphs (text, style) and tables (cell text) for a DOCX; per sheet the
cell values (openpyxl data_only=True: a formula with no cached value is
null) for an XLSX; or the exception raised."""

import importlib.metadata
import json
import sys

import docx
import openpyxl


def error_name(e):
    """The exception class, with its module unless it is a builtin."""
    cls = type(e)
    return cls.__name__ if cls.__module__ == "builtins" else f"{cls.__module__}.{cls.__name__}"


def read_docx(path):
    d = docx.Document(path)
    return {"paragraphs": [[p.text, p.style.name] for p in d.paragraphs],
            "tables": [[[c.text for c in row.cells] for row in t.rows] for t in d.tables],
            "title": d.core_properties.title}


def read_xlsx(path):
    wb = openpyxl.load_workbook(path, data_only=True)
    return {"sheets": [[ws.title, [list(r) for r in ws.iter_rows(values_only=True)]]
                       for ws in wb.worksheets]}


out = {"python-docx": importlib.metadata.version("python-docx"),
       "openpyxl": importlib.metadata.version("openpyxl"), "files": []}
for path in sys.argv[1:]:
    try:
        out["files"].append(read_xlsx(path) if path.endswith(".xlsx") else read_docx(path))
    except Exception as e:
        out["files"].append({"error": error_name(e), "message": str(e)})
print(json.dumps(out))

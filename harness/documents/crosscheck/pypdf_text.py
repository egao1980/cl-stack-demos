"""pypdf's view of a PDF: argv[1] the file, argv[2] the password (optional).
Prints JSON: version, encrypted, per-page text, title / author, or the
exception pypdf raised."""

import importlib.metadata
import json
import sys

from pypdf import PdfReader


def error_name(e):
    """The exception class, with its module unless it is a builtin."""
    cls = type(e)
    return cls.__name__ if cls.__module__ == "builtins" else f"{cls.__module__}.{cls.__name__}"


result = {"version": importlib.metadata.version("pypdf")}
try:
    reader = PdfReader(sys.argv[1], password=sys.argv[2] if len(sys.argv) > 2 else None)
    result["encrypted"] = reader.is_encrypted
    result["pages"] = [page.extract_text() for page in reader.pages]
    meta = reader.metadata or {}
    result["title"] = meta.get("/Title")
    result["author"] = meta.get("/Author")
except Exception as e:  # the demo prints the class name
    result["error"] = error_name(e)
    result["message"] = str(e)
print(json.dumps(result))

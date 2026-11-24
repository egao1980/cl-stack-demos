"""python-dotenv's view of .env files: argv are paths. Prints JSON per file:
dotenv_values() as (name, value) pairs in file order (a bare NAME is null),
or the exception raised."""

import importlib.metadata
import json
import sys

from dotenv import dotenv_values


def error_name(e):
    """The exception class, with its module unless it is a builtin."""
    cls = type(e)
    return cls.__name__ if cls.__module__ == "builtins" else f"{cls.__module__}.{cls.__name__}"


out = {"version": importlib.metadata.version("python-dotenv"), "files": []}
for path in sys.argv[1:]:
    try:
        out["files"].append({"pairs": [[k, v] for k, v in dotenv_values(path).items()]})
    except Exception as e:
        out["files"].append({"error": error_name(e), "message": str(e)})
print(json.dumps(out))

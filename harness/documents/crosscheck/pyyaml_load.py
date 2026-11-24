"""PyYAML's view of YAML files: argv are paths. Prints JSON per file: the
document loaded with yaml.safe_load (anchors resolved), the lines that end
in a space or tab, and PyYAML's own safe_dump of the same data."""

import importlib.metadata
import json
import sys

import yaml

out = {"version": importlib.metadata.version("PyYAML"), "files": []}
for path in sys.argv[1:]:
    with open(path, encoding="utf-8") as f:
        text = f.read()
    data = yaml.safe_load(text)
    out["files"].append({
        "data": data,
        "trailing": [n for n, line in enumerate(text.split("\n"), 1)
                     if line.endswith((" ", "\t"))],
        "dump": yaml.safe_dump(data, sort_keys=True),
    })
print(json.dumps(out))

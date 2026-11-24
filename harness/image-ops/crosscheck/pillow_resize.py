"""Pillow's resize for the closeness table of harness/image-ops. argv[1] is a
manifest the demo writes, one case per line, tab-separated:

    in-path  mode  width  height  out-width  out-height  filter  gap  out-path

Each input file holds raw pixels (read with Image.frombytes); each result is
written raw (Image.tobytes()) to out-path. GAP is "-" for no reducing gap.
Prints Pillow's version."""

import importlib.metadata
import sys

from PIL import Image

R = Image.Resampling
FILTERS = {"nearest": R.NEAREST, "box": R.BOX, "bilinear": R.BILINEAR,
           "bicubic": R.BICUBIC, "lanczos3": R.LANCZOS}

with open(sys.argv[1], encoding="utf-8") as manifest:
    for line in manifest:
        line = line.rstrip("\n")
        if not line:
            continue
        src, mode, w, h, ow, oh, flt, gap, dst = line.split("\t")
        with open(src, "rb") as f:
            im = Image.frombytes(mode, (int(w), int(h)), f.read())
        out = im.resize((int(ow), int(oh)), FILTERS[flt],
                        reducing_gap=None if gap == "-" else float(gap))
        with open(dst, "wb") as f:
            f.write(out.tobytes())

print(importlib.metadata.version("pillow"))

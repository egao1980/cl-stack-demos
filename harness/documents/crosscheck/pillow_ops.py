"""Pillow's results for the image section: argv[1] input.png, argv[2]
logo.png (both written by the demo). Prints JSON: the SHA-256 of the raw
pixels (Image.tobytes()) and the size of the inputs and of each operation,
keyed by the labels the Lisp side uses."""

import hashlib
import importlib.metadata
import json
import sys

from PIL import Image

src = Image.open(sys.argv[1])
src.load()
logo = Image.open(sys.argv[2])
logo.load()
T = Image.Transpose
R = Image.Resampling


def thumb(im, size):
    im = im.copy()
    im.thumbnail(size)  # bicubic, reducing_gap=2.0
    return im


def over(dst, src, dest):
    dst = dst.copy()
    dst.alpha_composite(src, dest=dest)
    return dst


ops = {
    "input": src,
    "logo": logo,
    "crop 10 8 40 24": src.crop((10, 8, 50, 32)),
    "rotate 90": src.transpose(T.ROTATE_90),
    "rotate 180": src.transpose(T.ROTATE_180),
    "rotate 270": src.transpose(T.ROTATE_270),
    "rotate -90": src.rotate(-90, expand=True),
    "flip :horizontal": src.transpose(T.FLIP_LEFT_RIGHT),
    "flip :vertical": src.transpose(T.FLIP_TOP_BOTTOM),
    "resize 37x23 :nearest": src.resize((37, 23), R.NEAREST),
    "resize 37x23 :box": src.resize((37, 23), R.BOX),
    "resize 37x23 :bilinear": src.resize((37, 23), R.BILINEAR),
    "resize 37x23 :bicubic": src.resize((37, 23), R.BICUBIC),
    "resize 37x23 :lanczos3": src.resize((37, 23), R.LANCZOS),
    "resize 160x120 :bicubic": src.resize((160, 120), R.BICUBIC),
    "rgb resize 21x16 :lanczos3 :reducing-gap 2": src.convert("RGB").resize(
        (21, 16), R.LANCZOS, reducing_gap=2.0),
    "thumbnail 32x32 :bicubic": thumb(src, (32, 32)),
    "composite logo at 12,6": over(src, logo, (12, 6)),
}
print(json.dumps({
    "version": importlib.metadata.version("pillow"),
    "ops": {k: {"sha256": hashlib.sha256(im.tobytes()).hexdigest(),
                "size": list(im.size), "mode": im.mode} for k, im in ops.items()},
}))

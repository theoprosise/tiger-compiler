#!/usr/bin/env python3

import sys

if len(sys.argv) != 2:
    print("usage: python3 show_positions.py test.tig")
    sys.exit(1)

text = open(sys.argv[1], "r").read()

for i, ch in enumerate(text):
    if ch == "\n":
        shown = "\\n"
    elif ch == "\t":
        shown = "\\t"
    else:
        shown = ch
    print(f"{i:4}: {shown}")

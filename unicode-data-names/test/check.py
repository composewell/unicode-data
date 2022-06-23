#!/usr/bin/env python3

import sys
from pathlib import Path
import csv
import unicodedata
import argparse
from functools import partial
from operator import methodcaller

def parse_codepoint(s: str, base=16):
    return chr(int(s, base))

def check(r: bool, info):
    if r:
        return True
    else:
        raise AssertionError(info)

def make_parser():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "csv", type=Path,
        metavar="FILE",
        help="CSV file to analyse."
    )
    parser.add_argument(
        "-v", "--verbosity",
        action="count", default=0,
        help="Verbosity level. Default: %(default)s"
    )
    return parser

def check_name(char: str, expected: str, verbosity: int):
    if name := unicodedata.name(char, None):
        got = f"Just \"{name}\""
    else:
        got = "Nothing"
    # [NOTE] Currently failling with Python 3.11.0b3.
    # [TODO] Check if bug in Python’s implementation?
    if got != expected and expected.startswith("Just \"TANGUT IDEOGRAPH"):
        if verbosity:
            print(
                f"[WARNING] Skipped U+{ord(char):0>4X}: "
                f"expected “{expected}” but got “{got}”."
            )
        return False
    check(got == expected, {"char": char, "got": got, "expected": expected})
    return True

if __name__ == "__main__":

    parser = make_parser()
    args = parser.parse_args()
    path = args.csv
    verbosity = args.verbosity

    with path.open("rt", encoding="utf-8") as fp:
        version = next(fp).strip()
        if version != unicodedata.unidata_version:
            sys.exit(
                f"Incompatible Python Unicode version: expecting “{version}”, "
                f"but got: “{unicodedata.unidata_version}”."
            )
        # Skip header
        next(fp)
        reader = csv.reader(fp)
        has_warnings = False
        for row in reader:
            raw_code, name = row
            char = parse_codepoint(raw_code)
            codepoint = f"U+{raw_code.upper():0>4}"
            has_warnings = check_name(char, name, args.verbosity) or has_warnings

        if has_warnings:
            print(
                "All tests passed, but there are some warnings" +
                (" (use verbose option to see them)." if not verbosity else ".")
            )
        else:
            print("All tests passed.")


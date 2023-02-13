#!/usr/bin/env python3

from pathlib import Path
import csv
import unicodedata
import argparse

def make_parser():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "csv", type=Path,
        metavar="FILE",
        help="CSV file to generate."
    )
    return parser


def make_code_points(s):
    return " ".join(f"{ord(c):X}" for c in s)


if __name__ == "__main__":

    parser = make_parser()
    args = parser.parse_args()
    path = args.csv

    with path.open("wt", encoding="utf-8") as fp:
        # Unicode version
        fp.write(unicodedata.unidata_version + "\n")
        # Characters properties
        writer = csv.writer(fp, lineterminator="\n")
        for cp in range(0, 0x10FFFF + 1):
            char = chr(cp)
            row = (
                f"{cp:X}",
                unicodedata.category(char),
                make_code_points(char.lower()),
                make_code_points(char.upper()),
                make_code_points(char.title()),
                char.islower(),
                char.isupper(),
                # char.istitle(),
                unicodedata.numeric(char,""),
                unicodedata.decimal(char,unicodedata.digit(char, "")),
            )
            writer.writerow(row)

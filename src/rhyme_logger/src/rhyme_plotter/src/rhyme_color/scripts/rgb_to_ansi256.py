#!/usr/bin/env python

import argparse


def main():
    parser = argparse.ArgumentParser(description='RGB colors to ANSI256')

    parser.add_argument('r', help='red', type=int)
    parser.add_argument('g', help='green', type=int)
    parser.add_argument('b', help='blue', type=int)

    args = parser.parse_args()

    r = args.r
    g = args.g
    b = args.b

    if r == g and g == b:
        if r < 8:
            return 16
        elif r > 248:
            return 231
        else:
            return round(((r - 8) / 247.) * 24) + 232
    else:
        return \
            16 + 36 * round(r / 255. * 5) + 6 * \
            round(g / 255. * 5) + round(b / 255. * 5)


if __name__ == "__main__":
    code = int(main())

    fg = '\033[38;5;' + str(code) + 'm '
    clear = '\033[0m'

    print(fg + str(code) + ' ' + 10 * unichr(0x2588) + clear)

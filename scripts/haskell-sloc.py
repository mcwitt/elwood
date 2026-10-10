#!/usr/bin/env python3
"""Count Haskell source lines excluding blank lines and comments, including pragmas.
Use --revision REV for a reproducible committed baseline. Formatting must be
checked separately: this measure is not permission to pack expressions on lines.
"""
import argparse
import pathlib
import re
import subprocess


def sloc(text):
    depth = 0
    total = 0
    for line in text.splitlines():
        i, code = 0, False
        while i < len(line):
            if depth:
                if line.startswith('{-', i):
                    depth += 1
                    i += 2
                elif line.startswith('-}', i):
                    depth -= 1
                    i += 2
                else:
                    i += 1
            elif line.startswith('{-#', i):
                code = True
                end = line.find('#-}', i + 3)
                i = len(line) if end < 0 else end + 3
            elif line.startswith('{-', i):
                depth = 1
                i += 2
            elif line.startswith('--', i):
                break
            elif line[i] == '"':
                code = True
                i += 1
                while i < len(line):
                    if line[i] == chr(92):
                        i += 2
                    elif line[i] == '"':
                        i += 1
                        break
                    else:
                        i += 1
            else:
                code |= not line[i].isspace()
                i += 1
        total += code
    return total


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--revision')
    args = parser.parse_args()
    if args.revision:
        files = subprocess.check_output(['git', 'ls-tree', '-r', '--name-only', args.revision], text=True).splitlines()
        read = lambda p: subprocess.check_output(['git', 'show', f'{args.revision}:{p}'], text=True)
    else:
        files = [str(p) for root in ('src', 'app') for p in pathlib.Path(root).rglob('*.hs')]
        read = lambda p: pathlib.Path(p).read_text()
    counts = [(p, sloc(read(p))) for p in sorted(files) if p.endswith('.hs') and p.startswith(('src/', 'app/'))]
    for path, count in counts:
        print(f'{count:5} {path}')
    print(f'{sum(n for _, n in counts):5} TOTAL')


if __name__ == '__main__':
    main()

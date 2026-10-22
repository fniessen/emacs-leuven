#!/usr/bin/env python3
"""Check that Org headings do not skip outline levels."""

import re
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
ORG_DIR = ROOT / "org"
HEADING_RE = re.compile(r"^(\*+)[ \t]+")
BEGIN_BLOCK_RE = re.compile(r"^[ \t]*#\+begin_([a-z0-9_-]+)\b", re.IGNORECASE)
END_BLOCK_RE = re.compile(r"^[ \t]*#\+end_([a-z0-9_-]+)\b", re.IGNORECASE)


def find_level_errors(path):
    """Return (line number, message) for headings with missing parents."""
    errors = []
    previous_level = 0
    block = None

    with path.open(encoding="utf-8") as org_file:
        for line_number, line in enumerate(org_file, start=1):
            if block:
                ending = END_BLOCK_RE.match(line)
                if ending and ending.group(1).lower() == block:
                    block = None
                continue

            beginning = BEGIN_BLOCK_RE.match(line)
            if beginning:
                block = beginning.group(1).lower()
                continue

            heading = HEADING_RE.match(line)
            if not heading:
                continue

            level = len(heading.group(1))
            expected_level = previous_level + 1
            if level > expected_level:
                errors.append(
                    (
                        line_number,
                        f"level {level} follows level {previous_level}; "
                        f"expected at most level {expected_level}",
                    )
                )
            previous_level = level

    return errors


def main():
    if not ORG_DIR.is_dir():
        print(f"Org directory not found: {ORG_DIR}", file=sys.stderr)
        return 2

    files = sorted(ORG_DIR.rglob("*.org"))
    failures = 0

    for path in files:
        try:
            errors = find_level_errors(path)
        except (OSError, UnicodeError) as error:
            print(f"{path.relative_to(ROOT)}: cannot read file: {error}", file=sys.stderr)
            failures += 1
            continue

        for line_number, message in errors:
            print(f"{path.relative_to(ROOT)}:{line_number}: {message}")
            failures += 1

    if failures:
        print(f"Found {failures} Org heading level error(s) in {len(files)} file(s).")
        return 1

    print(f"Org heading levels are valid in {len(files)} file(s).")
    return 0


if __name__ == "__main__":
    sys.exit(main())

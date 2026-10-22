#!/usr/bin/env python3
"""Check that static HTTP(S) URLs in repository text files respond successfully."""

from __future__ import annotations

import re
import sys
from collections import defaultdict
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
from urllib.error import HTTPError, URLError
from urllib.request import Request, urlopen


ROOT = Path(__file__).resolve().parents[1]
URL_PATTERN = re.compile(r"""https?://[^\s<>"'\]]+""", re.IGNORECASE)
TRAILING_PUNCTUATION = ".,;:!?)]}"
IGNORED_DIRECTORIES = {".git", "__pycache__", ".venv", "venv", "node_modules"}
IGNORED_SUFFIXES = {
    ".pdf",
    ".doc",
    ".docx",
    ".xls",
    ".xlsx",
    ".ppt",
    ".pptx",
    ".mp3",
    ".mp4",
    ".mov",
    ".avi",
    ".zip",
    ".gz",
    ".png",
    ".jpg",
    ".jpeg",
    ".gif",
    ".webp",
    ".svg",
}
TIMEOUT_SECONDS = 15
MAX_FILE_SIZE = 5 * 1024 * 1024


def repository_files() -> list[Path]:
    files = []
    for path in ROOT.rglob("*"):
        if not path.is_file():
            continue
        relative_parts = path.relative_to(ROOT).parts
        if any(part in IGNORED_DIRECTORIES for part in relative_parts):
            continue
        if path.suffix.lower() in IGNORED_SUFFIXES or path.stat().st_size > MAX_FILE_SIZE:
            continue
        files.append(path)
    return files


def collect_links() -> tuple[dict[str, list[tuple[Path, int]]], int, list[str]]:
    links: dict[str, list[tuple[Path, int]]] = defaultdict(list)
    skipped_templates = 0
    scan_errors = []

    for path in repository_files():
        try:
            content = path.read_text(encoding="utf-8")
        except (UnicodeDecodeError, OSError) as error:
            scan_errors.append(f"{path.relative_to(ROOT)}: {error}")
            continue

        for line_number, line in enumerate(content.splitlines(), start=1):
            for match in URL_PATTERN.finditer(line):
                url = match.group().rstrip(TRAILING_PUNCTUATION)
                if not url:
                    continue
                if "${" in url or "{" in url:
                    skipped_templates += 1
                    continue
                links[url].append((path.relative_to(ROOT), line_number))

    return links, skipped_templates, scan_errors


def check_url(url: str) -> tuple[str, str | None]:
    request = Request(url, headers={"User-Agent": "repository-link-checker/1.0"})
    try:
        with urlopen(request, timeout=TIMEOUT_SECONDS) as response:
            status = response.status
            final_url = response.geturl()
        if 200 <= status < 400:
            return url, None
        return url, f"HTTP {status} ({final_url})"
    except HTTPError as error:
        return url, f"HTTP {error.code} ({error.geturl()})"
    except (URLError, TimeoutError, OSError, ValueError) as error:
        return url, str(error)


def main() -> int:
    links, skipped_templates, scan_errors = collect_links()
    if not links:
        print("No static HTTP(S) links found.")

    if links:
        print(f"Checking {len(links)} unique HTTP(S) links...")
        with ThreadPoolExecutor(max_workers=8) as executor:
            results = executor.map(check_url, links)

        failures = [(url, error) for url, error in results if error is not None]
    else:
        failures = []
    if failures:
        print(f"\n{len(failures)} link(s) failed:")
        for url, error in failures:
            print(f"\n  {url}\n    {error}")
            for path, line_number in links[url]:
                print(f"    referenced in {path}:{line_number}")
    else:
        print(f"All {len(links)} unique HTTP(S) links returned a successful status.")

    if skipped_templates:
        print(f"Skipped {skipped_templates} URL template(s) containing unresolved variables.")

    if scan_errors:
        print(f"\n{len(scan_errors)} file(s) could not be scanned:")
        for error in scan_errors:
            print(f"  {error}")

    return 1 if failures or scan_errors else 0


if __name__ == "__main__":
    sys.exit(main())

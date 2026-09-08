#!/usr/bin/env python3
"""Refuse broken internal links in docs/*.md.

Internal destinations must be relative paths to files that exist (usually
other .md pages). Site-root Jekyll URLs like /heritage/ 404 on GitHub and in
the editor; jekyll-relative-links rewrites the relative .md form at build
time. Fragments must match a heading id on the target page.

Usage (repo root):

    python3 scripts/check-doc-links.py
"""

from __future__ import annotations

import os
import re
import sys
import urllib.parse
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
DOCS = ROOT / "docs"

MD_LINK = re.compile(
    r"(?<!!)\[(?:[^\]])*?\]\(([^)\s]+)(?:\s+\"[^\"]*\")?\)",
    re.S,
)
IMG_LINK = re.compile(r"!\[(?:[^\]]*)\]\(([^)\s]+)\)")
HEADING = re.compile(r"^(#{1,6})\s+(.*)$")
EXPLICIT_ID = re.compile(r"\{#([A-Za-z0-9_-]+)\}\s*$")
SKIP_URL = ("http://", "https://", "mailto:", "{{", "{%", "//")


def strip_fences(text: str) -> str:
    out: list[str] = []
    in_fence = False
    for line in text.splitlines(True):
        if line.startswith("```"):
            in_fence = not in_fence
            continue
        if not in_fence:
            out.append(line)
    return "".join(out)


def gh_slug(text: str) -> str:
    t = re.sub(r"`+", "", text)
    t = re.sub(r"<[^>]+>", "", t)
    t = t.strip().lower()
    t = re.sub(r"[^\w\s-]", "", t, flags=re.UNICODE)
    t = re.sub(r"[-\s]+", "-", t).strip("-")
    return t


def heading_ids(text: str) -> set[str]:
    ids: set[str] = set()
    seen: dict[str, int] = {}
    for line in strip_fences(text).splitlines():
        hm = HEADING.match(line)
        if not hm:
            continue
        rest = hm.group(2).strip()
        eid = EXPLICIT_ID.search(rest)
        if eid:
            ids.add(eid.group(1).lower())
            rest = EXPLICIT_ID.sub("", rest).strip()
        slug = gh_slug(rest)
        if slug:
            n = seen.get(slug, 0)
            ids.add(slug if n == 0 else f"{slug}-{n}")
            seen[slug] = n + 1
    return ids


def extract_urls(text: str) -> list[str]:
    body = strip_fences(text)
    body = re.sub(r"`[^`]+`", "", body)
    urls = [m.group(1) for m in MD_LINK.finditer(body)]
    urls.extend(m.group(1) for m in IMG_LINK.finditer(body))
    return urls


def main() -> int:
    errors: list[str] = []
    md_files = [
        p
        for p in DOCS.rglob("*.md")
        if "_site" not in p.parts
        and "vendor" not in p.parts
        and p.name != "README.md"
    ]
    id_cache: dict[Path, set[str]] = {}

    def ids_for(path: Path) -> set[str]:
        if path not in id_cache:
            id_cache[path] = heading_ids(path.read_text(encoding="utf-8"))
        return id_cache[path]

    wrap = re.compile(r"\]\(([^)/][^)]*\.md[^)]*)\)")
    for path in md_files:
        rel = path.relative_to(ROOT).as_posix()
        raw = path.read_text(encoding="utf-8")
        in_fence = False
        for i, line in enumerate(raw.splitlines(), 1):
            if line.startswith("```"):
                in_fence = not in_fence
                continue
            if in_fence:
                continue
            for m in wrap.finditer(line):
                if "[" not in line[: m.start()]:
                    errors.append(
                        f"{rel}:{i}: wrapped [text](file.md) link; put it on one line "
                        f"so the site builder rewrites it ({m.group(1)})"
                    )
        for url in extract_urls(raw):
            if url.startswith(SKIP_URL) or url.startswith("http://127."):
                if "github.com/spek-lang/spek/" in url and "/main/docs" in url:
                    errors.append(
                        f"{rel}: {url} points at main/docs; that tree is not on main"
                    )
                continue
            parsed = urllib.parse.urlparse(url)
            dest_path, frag = parsed.path, parsed.fragment
            if dest_path.startswith("/"):
                errors.append(
                    f"{rel}: {url} is a site-root path; use a relative .md file"
                )
                continue
            if not dest_path:
                target = path
            else:
                target = (path.parent / dest_path).resolve()
                try:
                    target.relative_to(ROOT)
                except ValueError:
                    errors.append(f"{rel}: {url} escapes the repository")
                    continue
                if not target.exists():
                    errors.append(f"{rel}: {url} -> missing {target.relative_to(ROOT)}")
                    continue
            if frag:
                if target.suffix != ".md":
                    continue
                if frag.lower() not in ids_for(target):
                    errors.append(
                        f"{rel}: {url} -> no heading id #{frag} in {target.relative_to(ROOT)}"
                    )

    if errors:
        print("Broken docs links:", file=sys.stderr)
        for e in errors:
            print(f"  {e}", file=sys.stderr)
        print(f"{len(errors)} error(s)", file=sys.stderr)
        return 1
    print(f"OK: {len(md_files)} markdown files, internal links resolve")
    return 0


if __name__ == "__main__":
    os.chdir(ROOT)
    sys.exit(main())

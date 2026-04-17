"""Fail CI when runtime config files contain absolute local filesystem paths."""

from __future__ import annotations

import pathlib
import re
import sys


# Keep scope intentionally small: runtime-relevant model configuration files.
INCLUDED_GLOBS = (
    "model/**/*.xml",
    "model/**/*.xrun",
    "model/**/*.yaml",
    "parameterisation/**/*.xrun",
    "parameterisation/**/*.yaml",
)


FORBIDDEN_PATTERNS: tuple[tuple[re.Pattern[str], str], ...] = (
    (
        re.compile(r"(?i)[A-Z]:\\LocalWork\\xAquaticRisk"),
        "developer-root path (C:\\LocalWork\\xAquaticRisk)",
    ),
    (
        re.compile(r"(?i)[A-Z]:\\Users\\"),
        "machine-specific user profile path (C:\\Users\\...)",
    ),
)


def iter_target_files(repo_root: pathlib.Path) -> list[pathlib.Path]:
    files: set[pathlib.Path] = set()
    for pattern in INCLUDED_GLOBS:
        files.update(path for path in repo_root.glob(pattern) if path.is_file())
    return sorted(files)


def scan_file(file_path: pathlib.Path) -> list[tuple[int, str, str]]:
    findings: list[tuple[int, str, str]] = []
    try:
        content = file_path.read_text(encoding="utf-8", errors="replace")
    except OSError as exc:
        findings.append((1, "", f"Unable to read file: {exc}"))
        return findings

    for line_number, line in enumerate(content.splitlines(), start=1):
        for pattern, description in FORBIDDEN_PATTERNS:
            if pattern.search(line):
                findings.append((line_number, line.strip(), description))
    return findings


def main() -> int:
    repo_root = pathlib.Path(__file__).resolve().parents[1]
    findings_count = 0

    for file_path in iter_target_files(repo_root):
        relative_path = file_path.relative_to(repo_root).as_posix()
        for line_number, line, description in scan_file(file_path):
            findings_count += 1
            print(
                "::error file={file},line={line}::{msg}. Offending text: {text}".format(
                    file=relative_path,
                    line=line_number,
                    msg=description,
                    text=line if line else "<unavailable>",
                )
            )

    if findings_count:
        print(f"Found {findings_count} forbidden absolute local path reference(s).")
        print(
            "Use repository-relative placeholders (for example $(_X3DIR_)/...) instead of machine-specific paths."
        )
        return 1

    print("No forbidden absolute local paths detected in runtime config files.")
    return 0


if __name__ == "__main__":
    sys.exit(main())

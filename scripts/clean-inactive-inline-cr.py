#!/usr/bin/env python3
import re
import subprocess
import sys
from pathlib import Path


HEADER_RE = re.compile(
    r"^(?P<prefix>\s*(?:(?://|;|#)\s*)*)"
    r"(?P<tag>\[#gh:\d+\]\s*)?>\s*(?P<kind>N?X?CR)\s+"
    r"(?P<reviewer>[^ ]+)\s+for\s+(?P<author>[^:]+):"
)
THREAD_RE = re.compile(
    r"^\s*(?:(?://|;|#)\s*)*(?:\[#gh:\d+\]\s*)?>\s*"
)


def run(cmd):
    result = subprocess.run(cmd, text=True, capture_output=True, check=False)
    if result.returncode != 0:
        raise RuntimeError(
            "Command failed: {}\nstdout:\n{}\nstderr:\n{}".format(
                " ".join(cmd), result.stdout, result.stderr
            )
        )
    return result.stdout


def iter_tracked_files():
    output = run(["git", "ls-files"])
    for line in output.splitlines():
        if line.strip():
            yield Path(line.strip())


def remove_inactive_threads(lines):
    changed = False
    out = []
    i = 0
    while i < len(lines):
        line = lines[i]
        m = HEADER_RE.match(line)
        if m and m.group("kind").startswith("N"):
            changed = True
            i += 1
            while i < len(lines):
                if HEADER_RE.match(lines[i]):
                    break
                if not THREAD_RE.match(lines[i]):
                    break
                i += 1
            continue
        out.append(line)
        i += 1
    return out, changed


def main():
    changed_files = []
    for path in iter_tracked_files():
        try:
            data = path.read_bytes()
        except OSError:
            continue
        if b"\x00" in data:
            continue
        try:
            text = data.decode("utf-8")
        except UnicodeDecodeError:
            continue
        lines = text.splitlines(keepends=True)
        new_lines, changed = remove_inactive_threads(lines)
        if changed:
            path.write_text("".join(new_lines), encoding="utf-8")
            changed_files.append(str(path))

    if changed_files:
        sys.stderr.write(
            "Removed inactive inline-cr threads in:\n{}\n".format(
                "\n".join(changed_files)
            )
        )
        sys.stderr.write("Commit these changes before merging.\n")
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())

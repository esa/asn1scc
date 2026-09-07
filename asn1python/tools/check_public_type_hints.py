#!/usr/bin/env python3
"""Check that every public function/method in the given Python files has
complete type annotations (all parameters plus the return type).

Scoped to the *public* interface only (ASN1SCC-OPER-REQ-005):

- "Public" = a function/method whose name does not start with `_`, or a
  dunder method (`__init__`, `__eq__`, ...). A single- or double-leading-
  underscore name (and not a dunder) is treated as an internal helper and
  skipped, matching ordinary Python convention.
- Only module-level functions and class methods are checked. A function
  nested inside another function is always an implementation detail
  regardless of its name, so it is skipped.
- `self`/`cls` (by name, first positional parameter) are not required to
  be annotated.

Usage:
    python3 check_public_type_hints.py <file-or-dir> [<file-or-dir> ...]
    python3 check_public_type_hints.py --summary asn1pylib/asn1src/
"""

from __future__ import annotations

import argparse
import ast
import sys
from pathlib import Path


def is_public_name(name: str) -> bool:
    if name.startswith("__") and name.endswith("__"):
        return True  # dunder method - part of the public protocol
    return not name.startswith("_")


class PublicFunctionVisitor(ast.NodeVisitor):
    """Visits every function/method, skipping any nested inside another
    function (always an implementation detail, regardless of name)."""

    def __init__(self, path: Path):
        self.path = path
        self.violations: list[str] = []
        self.checked = 0
        self._in_function = False

    def visit_FunctionDef(self, node) -> None:
        if self._in_function:
            return
        if is_public_name(node.name):
            self.checked += 1
            self._check_annotations(node)
        self._in_function = True
        self.generic_visit(node)
        self._in_function = False

    visit_AsyncFunctionDef = visit_FunctionDef

    def _violation(self, node, detail: str) -> None:
        self.violations.append(f"{self.path}:{node.lineno}: {node.name}: {detail}")

    def _check_annotations(self, node) -> None:
        args = node.args
        positional = args.posonlyargs + args.args
        if positional and positional[0].arg in ("self", "cls"):
            positional = positional[1:]

        for arg in positional + args.kwonlyargs:
            if arg.annotation is None:
                self._violation(node, f"missing type annotation for parameter '{arg.arg}'")
        if args.vararg is not None and args.vararg.annotation is None:
            self._violation(node, f"missing type annotation for '*{args.vararg.arg}'")
        if args.kwarg is not None and args.kwarg.annotation is None:
            self._violation(node, f"missing type annotation for '**{args.kwarg.arg}'")
        if node.returns is None:
            self._violation(node, "missing return type annotation")


def check_file(path: Path) -> tuple[list[str], int]:
    tree = ast.parse(path.read_text(encoding="utf-8"), filename=str(path))
    visitor = PublicFunctionVisitor(path)
    visitor.visit(tree)
    return visitor.violations, visitor.checked


def collect_files(paths: list[str]) -> list[Path]:
    files: list[Path] = []
    for p in paths:
        path = Path(p)
        if path.is_dir():
            files.extend(sorted(path.rglob("*.py")))
        elif path.is_file():
            files.append(path)
        else:
            print(f"ERROR: {path} not found", file=sys.stderr)
            sys.exit(2)
    return files


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("paths", nargs="+", help="Files or directories to check")
    parser.add_argument("--summary", action="store_true", help="Print a one-line summary even when clean")
    args = parser.parse_args()

    files = collect_files(args.paths)
    violations: list[str] = []
    checked = 0
    for f in files:
        file_violations, file_checked = check_file(f)
        violations.extend(file_violations)
        checked += file_checked

    if violations:
        print("\n".join(violations))

    if violations or args.summary:
        print(
            f"\n{len(violations)} violation(s) across {checked} public "
            f"function(s) in {len(files)} file(s)",
            file=sys.stderr if violations else sys.stdout,
        )

    sys.exit(1 if violations else 0)


if __name__ == "__main__":
    main()

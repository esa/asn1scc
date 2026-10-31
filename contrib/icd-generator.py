#!/usr/bin/env python3
"""
icd-generator.py

Generate an ICD-style HTML document from ASN.1 and ACN source files.

The script calls asn1scc itself with the -icdRaw flag, which pre-processes the
model into ICD tables: implicit encoding fields that do not appear in the
source files (array length determinants, presence bits, choice discriminants,
ACN patterns, padding, terminator patterns) are all present in the dump. The
temporary files produced by asn1scc are never shown to the user.

An ASN.1/ACN XML AST (-x) is produced in the same call and used only to render
the local ASN.1/ACN grammar blocks and the full grammar sections. A previously
generated -icdRaw dump (.json) can also be passed directly; the grammar blocks
are then omitted (they need the XML AST).

Without --root every type of the model gets its own table, in declaration
order. With --root the dump is restricted to the requested types and their
transitive dependencies (asn1scc -icdPdus).

Main capabilities
-----------------
* Bit-level tables straight from the asn1scc ICD model, including the implicit
  encoding fields (length determinants, presence bits, choice discriminants,
  ACN patterns, padding) with their real sizes and bit offsets.
* Per-row bit offset ranges computed by asn1scc (exact, range or variable).
* Keep CHOICE alternatives in dedicated linked tables.
* Render local ASN.1, local ACN, compact layout, and detailed table above each type.
* Make every local view collapsible with HTML <details>/<summary> blocks.
* Render units in ASN.1 grammar using ASN.1SCC syntax: --{ UNIT }--, and in a
  dedicated Unit column of every row that carries them.
* Preserve user comments and ACN parameters of parameterized types.

Typical usage
-------------
    ./icd-generator.py -o model.html model.asn model.acn
    ./icd-generator.py --root Packet -o model.html model.asn model.acn
    ./icd-generator.py --root Packet --root Body -o model.html model.asn model.acn
"""
from __future__ import annotations

import argparse
import html
import json
import math
import os
import re
import shutil
import subprocess
import sys
import tempfile
import xml.etree.ElementTree as ET
from dataclasses import dataclass, replace
from pathlib import Path
from typing import Optional

LEAF_KINDS = {
    "INTEGER", "REAL", "IA5String", "NumericString", "OCTET_STRING", "NULL",
    "BIT_STRING", "BOOLEAN", "ENUMERATED", "ObjectIdentifier",
}
TYPE_KINDS = LEAF_KINDS | {"SEQUENCE", "SEQUENCE_OF", "CHOICE", "REFERENCE_TYPE"}
INDEX_NAMES = ["n", "m", "k", "j", "i"]
TABLE_HEADERS = [
    "No", "Field", "Comment", "Present", "Type", "Constraint",
    "Offset (bits)", "Min Bits", "Max Bits", "Unit",
]


# ---------------------------------------------------------------------------
# XML and formatting helpers
# ---------------------------------------------------------------------------

def tag(node: Optional[ET.Element]) -> str:
    if node is None:
        return ""
    return node.tag.split("}", 1)[-1] if "}" in node.tag else node.tag


def children(node: Optional[ET.Element], name: str) -> list[ET.Element]:
    return [] if node is None else [c for c in list(node) if tag(c) == name]


def child(node: Optional[ET.Element], name: str) -> Optional[ET.Element]:
    if node is None:
        return None
    for item in list(node):
        if tag(item) == name:
            return item
    return None


def attr(node: Optional[ET.Element], *names: str, default: str = "") -> str:
    if node is None:
        return default
    for name in names:
        if name in node.attrib:
            return node.attrib[name]
    return default


def text(node: Optional[ET.Element]) -> str:
    return " ".join((node.text or "").split()) if node is not None else ""


def html_escape(value: object) -> str:
    return html.escape(str(value or ""), quote=False)


def strip_html(value: object) -> str:
    return re.sub(r"<[^>]+>", "", str(value or "")).replace("├──", "").strip()


def safe_id(value: object) -> str:
    return re.sub(r"[^A-Za-z0-9_.:-]+", "_", str(value or ""))


def qualified_name(module: str, name: str) -> str:
    return f"{module}.{name}" if module else name


# Compatibility alias, kept deliberately because several versions used qn().
def qn(module: str, name: str) -> str:
    return qualified_name(module, name)


def anchor_link(label: str, target: str) -> str:
    return f'<a href="#{html_escape(target)}">{html_escape(label)}</a>'


def type_kind(node: Optional[ET.Element]) -> str:
    return tag(node)


def min_bits(node: Optional[ET.Element]) -> str:
    return attr(node, "acnMinSizeInBits", "uperMinSizeInBits")


def max_bits(node: Optional[ET.Element]) -> str:
    return attr(node, "acnMaxSizeInBits", "uperMaxSizeInBits")


def bit_size(node: Optional[ET.Element]) -> str:
    return max_bits(node) or min_bits(node)


def is_integer(value: object) -> bool:
    return bool(re.fullmatch(r"\d+", str(value or "").strip()))


def nest_offset(parent: object, local: object) -> str:
    """Offset of a nested row: parent (absolute) + local (child-relative).

    The dump gives every table's rows offsets relative to that table's
    first item. Once a child table is flattened into its parent
    (expand_rows), each row's offset becomes parent + local. Exact +
    exact -> the sum; anything involving a varying offset ('var', a
    range like '40..552') -> 'var'.
    """
    p, l = str(parent or "").strip(), str(local or "").strip()
    if l == "":
        return ""
    if p == "":
        return l
    if p in ("var", "variable") or l in ("var", "variable"):
        return "var"
    if is_integer(p) and is_integer(l):
        return str(int(p) + int(l))
    return "var"


def add_offset_expression(left: object, right: object) -> str:
    left = str(left or "0").strip()
    right = str(right or "0").strip()
    if right in ("", "0"):
        return left or "0"
    if left in ("", "0"):
        return right
    return str(int(left) + int(right)) if is_integer(left) and is_integer(right) else f"{left} + {right}"


def next_absolute_offset(current: object, step: object) -> str:
    """Return the next exact offset, or blank when it is not reliable.

    If a previous field has variable length, `step` or `current` will not be a plain
    integer. In that case the downstream absolute offset is intentionally blank.
    This avoids showing misleading expressions as if they were exact bit offsets.
    """
    return str(int(current) + int(step)) if is_integer(current) and is_integer(step) else ""


# ---------------------------------------------------------------------------
# ASN.1/ACN AST helpers
# ---------------------------------------------------------------------------

def first_type(node: Optional[ET.Element]) -> Optional[ET.Element]:
    if node is None:
        return None
    for item in list(node):
        if tag(item) in TYPE_KINDS:
            return item
    return None


def component_asn(component: ET.Element) -> Optional[ET.Element]:
    return child(component, "Asn1Type")


def component_type(component: ET.Element) -> Optional[ET.Element]:
    return first_type(component_asn(component))


def component_unit(component: ET.Element, concrete_type: Optional[ET.Element] = None) -> str:
    return (attr(component_asn(component), "unitsOfMeasure") or attr(concrete_type, "unitsOfMeasure")).strip()


def component_presence(component: ET.Element) -> str:
    if attr(component, "ALWAYS-ABSENT") == "true":
        return "never"
    if attr(component, "Optional") == "true":
        return "optional"
    return attr(component, "present-when", default="always")


def sequence_children(sequence: ET.Element, include_acn_components: bool) -> list[ET.Element]:
    return [
        item for item in list(sequence)
        if tag(item) == "SEQUENCE_COMPONENT"
        or (include_acn_components and tag(item) == "ACN_COMPONENT")
    ]


def sequence_of_asn(sequence_of: ET.Element) -> Optional[ET.Element]:
    return child(sequence_of, "Asn1Type")


def sequence_of_type(sequence_of: ET.Element) -> Optional[ET.Element]:
    return first_type(sequence_of_asn(sequence_of))


def scalar_value(node: Optional[ET.Element]) -> str:
    if node is None:
        return ""
    for name in ("IntegerValue", "RealValue", "StringValue", "EnumValue", "BooleanValue"):
        item = child(node, name)
        if item is not None:
            return text(item)
    return text(node)


def constraint_node_text(node: Optional[ET.Element]) -> str:
    if node is None:
        return ""
    node_kind = tag(node)
    if node_kind == "Range":
        return f"({scalar_value(child(node, 'Min'))} .. {scalar_value(child(node, 'Max'))})"
    if node_kind == "SIZE":
        values = [text(x) for x in node.iter() if tag(x) == "IntegerValue" and text(x)]
        if len(values) == 1:
            return f"({values[0]})"
        if len(values) > 1:
            return f"({values[0]} .. {values[1]})"
    if node_kind in ("AND", "OR", "EXCEPT"):
        parts = [constraint_node_text(c) for c in list(node)]
        return (" " + node_kind + " ").join(p for p in parts if p)
    return ""


def subtype_constraint(concrete_type: Optional[ET.Element]) -> str:
    if type_kind(concrete_type) == "ENUMERATED":
        return ""
    constraints = child(concrete_type, "Constraints")
    parts: list[str] = []
    if constraints is not None:
        for node in list(constraints):
            rendered = constraint_node_text(node)
            if rendered:
                parts.append(rendered)
    return " AND ".join(parts)


def enumeration_comment(concrete_type: Optional[ET.Element]) -> str:
    if type_kind(concrete_type) != "ENUMERATED":
        return ""
    items = children(child(concrete_type, "Items"), "Item")
    if not items:
        return ""
    return 'Possible values:<br/><ul class="enumeration">' + "".join(
        f'<li><font class="enumeration_name">{html_escape(attr(item, "Name"))}</font> '
        f'({html_escape(attr(item, "Value"))})</li>'
        for item in items
    ) + "</ul>"


def mapping_function_of(comment_html: str) -> tuple[str, Optional[str]]:
    """Extract the mapping-function note from a row's comments.

    asn1scc appends a machine-generated comment 'mapping-function <name>: ...'
    on every INTEGER whose ACN properties name one. Returns the comments with
    that note removed, plus the rendered note ('<fn> — <tail>') for the table
    and compact views to show with a star.
    """
    for part in (comment_html.split("<br/>") if comment_html else []):
        stripped = part.strip()
        m = re.match(r"^mapping-function\s+([\w.-]+):\s*(.*)$", stripped)
        if m:
            rest = [p for p in comment_html.split("<br/>") if p.strip() != stripped]
            return "<br/>".join(rest), m.group(1) + " — " + m.group(2)
    return comment_html, None


def comments_of(node: Optional[ET.Element]) -> str:
    asn_comment = text(child(node, "AsnComment"))
    acn_comment = text(child(node, "AcnComment"))
    return "<br/>".join(x for x in (asn_comment, acn_comment) if x)


def join_comment(user_comment: str, generated_html: str) -> str:
    user_html = html_escape(user_comment).strip()
    return user_html + "<br/>" + generated_html if user_html and generated_html else (generated_html or user_html)


def acn_pattern(pattern: str) -> str:
    if not pattern:
        return ""
    suffix = "B" if set(pattern) <= {"0", "1"} and len(pattern) > 2 else "H"
    return "'" + html_escape(pattern) + "'" + suffix


def acn_component_type(component: ET.Element) -> str:
    if attr(component, "pattern"):
        return "fixed pattern: " + acn_pattern(attr(component, "pattern"))
    return attr(component, "Type", default="ACN_COMPONENT")


def sequence_of_bounds(sequence_of: ET.Element) -> tuple[Optional[int], Optional[int], bool]:
    """Return (lower, upper, fixed_size) for a SEQUENCE OF constraint."""
    if attr(sequence_of, "size") and is_integer(attr(sequence_of, "size")):
        value = int(attr(sequence_of, "size"))
        return value, value, True

    constraints = child(sequence_of, "Constraints")
    values = [
        int(text(x))
        for x in constraints.iter()
        if tag(x) == "IntegerValue" and text(x).isdigit()
    ] if constraints is not None else []

    if len(values) == 1:
        return values[0], values[0], True
    if len(values) >= 2:
        return values[0], values[1], values[0] == values[1]
    return None, None, False


def sequence_of_count(sequence_of: ET.Element) -> tuple[str, bool]:
    lower, upper, fixed = sequence_of_bounds(sequence_of)
    if lower is None:
        return "N", False
    return (str(lower) if fixed else f"{lower}..{upper}"), fixed


def implicit_length_bits(sequence_of: ET.Element) -> str:
    """Smallest number of bits required for the implicit length determinant."""
    _lower, upper, _fixed = sequence_of_bounds(sequence_of)
    if upper is None:
        return "size-determinant-size"
    return str(max(1, math.ceil(math.log2(upper + 1))))


# ---------------------------------------------------------------------------
# icdRaw JSON document model (source of truth for the tables)
# ---------------------------------------------------------------------------

@dataclass
class IcdRow:
    """One row of an ICD table, as dumped by asn1scc -icdRaw."""
    number: str
    level: int
    field: str
    comment: str
    present: str
    typ: str
    constraint: str
    offset: str
    min_bits: str
    max_bits: str
    unit: str
    field_html: bool = False
    comment_html: bool = False
    # 'fn — the value is provided by the function at run time' when the field
    # has an ACN mapping-function; shown with a star in table and compact views.
    mapping_note: str = ""
    typ_html: bool = False
    anchor: str = ""
    ref_id: str = ""           # dump id of the referenced table, when sType is a reference
    row_type: str = "FieldRow"  # raw icdRaw rowType (FieldRow, ThreeDOTs, SubItemRow, ...)
    ref_kind: str = ""           # kind of the referenced table ("SEQUENCE", "CHOICE", ...)
    is_choice_link: bool = False  # the referenced table is a CHOICE
    kind_override: str = ""       # renderer kind: "title"/"type-link"/"choice-link"/"summary"
    is_repeat_summary: bool = False  # the 'Items #2..#N' marker of a collapsed run

    # -- compact representation ---------------------------------------------
    repeat_label: str = ""       # "Repeated N times" for array elements
    length_field: Optional[str] = ""   # external determinant field ("smplCount", "n", ...)
    is_length_det: bool = False   # anonymous encoded array determinant (shown as N)

    def bits_meta(self) -> str:
        """Size annotation for the compact representation: 'N bits' or 'A..B bits'."""
        mn, mx = self.min_bits, self.max_bits
        if not str(mn) or not str(mx):
            return ""
        try:
            mn_i, mx_i = int(str(mn)), int(str(mx))
        except ValueError:
            return ""
        if mn_i == 0 and mx_i == 0:
            return ""
        if mn_i == mx_i:
            return f"{mn_i} bit" + ("s" if mn_i != 1 else "")
        return f"{mn_i}..{mx_i} bits"

    def compact_type(self) -> str:
        """Type text in the compact diagram: 'bool' for BOOLEAN, 'int' for INTEGER, 'enum' for ENUMERATED."""
        typ = strip_html(self.typ)
        if typ.startswith("BOOLEAN"):
            return "bool" + typ[len("BOOLEAN"):]
        if typ.startswith("INTEGER"):
            return "int" + typ[len("INTEGER"):]
        if typ.startswith("ENUMERATED"):
            return "enum" + typ[len("ENUMERATED"):]
        return typ

    @property
    def kind(self) -> str:
        """Renderer kind of the row. A plain wire row is "field"; a row that
        references another table renders as "type-link"/"choice-link", and a
        composite reference flattened inline (root mode) as a "title" row."""
        return self.kind_override or "field"

    @property
    def repeat(self) -> str:
        return "repeated" if self.ref_kind == "SEQUENCE OF" else ""


@dataclass
class _SectionRow(IcdRow):
    """Header row of a table (the type itself), rendered as a section row."""
    @property
    def kind(self) -> str:
        return "section"


@dataclass
class IcdTable:
    """One ICD table (an asn1scc IcdTypeAss), keyed by its stable id."""
    id: str
    name: str
    kind: str
    tas_name: Optional[str]          # bare type-assignment name (None for specializations)
    module: str
    usage_path: str
    min_bytes: int
    max_bytes: int
    comments: list
    acn_parameters: list
    rows: list

    def is_type_assignment(self) -> bool:
        """True for the definition-site table of a type assignment."""
        return self.tas_name is not None and self.usage_path == qualified_name(self.module, self.tas_name)


class IcdDocument:
    """Parses the -icdRaw JSON dump into tables and provides row conversion."""

    def __init__(self, data: dict, anchors: dict[str, str]):
        self.data = data
        self.anchors = anchors                      # table id -> html anchor
        self.tables: dict[str, IcdTable] = {}
        # child table id -> the parent row referencing it (first one wins)
        self.referrers: dict[str, IcdRow] = {}
        # type name -> ASN.1 grammar-section anchor, filled by
        # render_document once the GrammarRenderer exists. Types with no
        # icdRaw table (primitives, ENUMERATEDs) still get their type name
        # linked - to their definition in the grammar section.
        self.grammar_anchors: dict[str, str] = {}
        self.order: list[str] = list(data.get("navOrder", []))
        # Two passes: the type column of a row may reference a table that
        # appears later in the dump, so rows are parsed only after every
        # table shell has been registered (links must find their target).
        raw_rows: dict[str, list[dict]] = {}
        for table in data.get("tables", []):
            parsed = self._parse_table(table)
            self.tables[parsed.id] = parsed
            raw_rows[parsed.id] = list(table.get("rows", []))
        for table_id, rows in raw_rows.items():
            self.tables[table_id].rows = [self._parse_row(r) for r in rows]
            # Referrer map: child table id -> the parent row referencing it.
            # A collapsed child SEQUENCE OF table carries the repetition
            # range on its referrer row in the parent table.
            for r in self.tables[table_id].rows:
                if r.ref_id:
                    self.referrers.setdefault(r.ref_id, r)
        for table_id in raw_rows:
            self._postprocess_rows(self.tables[table_id])
        # Type names rendered inside a plain type text ('SEQUENCE OF
        # (Samples)', 'INTEGER (U16)') link to that type's table when it is
        # rendered: the by_type_name map is built once the shells exist.
        self.by_type_name: dict[str, IcdTable] = {}
        for t in self.tables.values():
            if t.is_type_assignment():
                self.by_type_name.setdefault(t.tas_name or t.name, t)

    def _postprocess_rows(self, table: IcdTable) -> None:
        """Per-table pass for the array length determinants.

        1. An external determinant may be announced by the SIZE field
           itself rather than the array row: 'size determinant for
           Module.T.field' (ACN [size f]). Link both ends.
        2. asn1scc names every anonymous encoded array determinant 'Length'
           (comment 'The number of items'). The v2 convention is N for the
           first array of the table, N2, N3, ... for the following ones.
        """
        # 1. sibling external determinants: a field with the comment
        #    'size determinant for <path>' (ACN [size f]) announces the
        #    array field it sizes.
        by_field = {r.field: r for r in table.rows}
        for row in table.rows:
            for comment in (row.comment.split("<br/>") if row.comment else []):
                m = re.search(r"size determinant for ([\w.-]+)", strip_html(comment).strip())
                if not m:
                    continue
                sized = m.group(1).split(".")[-1]
                target = by_field.get(sized)
                if target is None or target.length_field:
                    continue
                # The sized row may be a reference row whose type column is
                # resolved later: ref_kind carries the target's kind.
                if "SEQUENCE OF" in target.typ or target.ref_kind == "SEQUENCE OF":
                    target.length_field = row.field
        # 2. rename anonymous encoded determinants N / N1 / ...
        counter = 0
        for row in table.rows:
            if row.is_length_det:
                counter += 1
                row.field = "N" if counter == 1 else f"N{counter - 1}"
        # 3. Collapse repeated SEQUENCE OF element instances: the dump
        #    shows the first instance, dots, then the last one
        #    (a[1], ..., a[15] / Item #1, ..., Item #7). The element must
        #    appear ONCE as 'item i (repeated a..b times)', where a..b is
        #    the repetition range carried by the array determinant. The
        #    element type column stays a live link (ref_id kept) so the
        #    nested fields still inline in root mode.
        rows = table.rows
        i = 0
        while i < len(rows):
            if rows[i].row_type != "ThreeDOTs":
                i += 1
                continue
            dots = rows[i]
            first = rows[i - 1] if i > 0 else None
            if first is None:
                i += 1
                continue
            # j lands ON the last instance header, or past the run.
            j = i + 1
            while j < len(rows):
                r = rows[j]
                if r.row_type == "ThreeDOTs":
                    break
                if r.level <= first.level and _instance_suffix(r.field):
                    break
                j += 1
            if j >= len(rows) or rows[j].row_type == "ThreeDOTs":
                i = j
                continue
            last = rows[j]
            # The repetition range a..b comes from the determinant
            # constraint of the array itself (parent row or the child
            # table's own Length row).
            count = self._array_count_range(table.id, rows, i)
            # The element's own type link and label are taken from the
            # last instance row (composite) or the first (leaf): both
            # carry the element's sType.
            elem_row = last if last.ref_id else first
            elem_typ = strip_html(elem_row.typ).strip() if not elem_row.typ_html else strip_html(elem_row.typ)
            if not elem_typ or elem_row.typ_html:
                # composite instance rows resolved later: use the ref label
                target = self.tables.get(elem_row.ref_id) if elem_row.ref_id else None
                elem_typ = (target.kind + " " + target.name) if target else "element"
            label = f"item i (repeated {count} times)" if count else "item i"
            summary = replace(elem_row, field=label,
                              typ=elem_row.typ, typ_html=elem_row.typ_html,
                              ref_id=elem_row.ref_id, ref_kind=elem_row.ref_kind,
                              kind_override="title", constraint="", present="always",
                              unit="", offset="", comment="", comment_html=False,
                              is_repeat_summary=True)
            rows[i - 1:j + 1] = [summary]
            i = i - 1 + 1
        return

    def _array_count_range(self, table_id: str, rows: list[IcdRow], dots_index: int) -> str:
        """Repetition range 'a..b' of the array whose dots are at rows[dots_index].

        The instances live in this table (inlined array: 'smpl' header row
        with constraint '(0 .. 10)' followed by smpl[1], ..., smpl[10]) or
        in a collapsed child SEQUENCE OF table (ASW 'instructions': only
        'Length', 'Item #1', dots, 'Item #4' - the range is then on the
        referrer row in the parent table, found via self.referrers).
        """
        first = rows[dots_index - 1] if dots_index > 0 else None
        # Inlined array: the header row at a shallower level just above
        # the first instance, with a SEQUENCE OF type column.
        for k in range(dots_index - 2, -1, -1):
            r = rows[k]
            if r.row_type == "ThreeDOTs":
                continue
            if r.level < first.level and is_array_header(r):
                rng = normalize_count(r.constraint)
                if rng:
                    return rng
            if r.row_type == "ThreeDOTs":
                break
        # Collapsed child table: the range is the referrer's constraint
        # in the parent table (self.referrers: table.id -> referrer row).
        referrer = self.referrers.get(table_id)
        if referrer is not None:
            rng = normalize_count(referrer.constraint)
            if rng:
                return rng
        # Name-based reference: the parent names the type ('SEQUENCE OF
        # (Samples)'); find the referrer row whose type text mentions this
        # table's type-assignment name.
        this = self.tables.get(table_id)
        if this is not None and (this.tas_name or this.name):
            wanted = this.tas_name or this.name
            for t in self.tables.values():
                if t.id == table_id:
                    continue
                for r in t.rows:
                    typ = strip_html(r.typ).strip()
                    if typ.startswith("SEQUENCE OF (") and typ.endswith(")"):
                        if typ[len("SEQUENCE OF ("):-1].strip() == wanted:
                            rng = normalize_count(r.constraint)
                            if rng:
                                return rng
        # Last resort: the child table's own Length determinant constraint.
        det = next((r for r in rows[:dots_index] if r.is_length_det), None)
        if det is not None:
            rng = normalize_count(det.constraint)
            if rng:
                return rng
        return ""

    def _parse_table(self, table: dict) -> IcdTable:
        tas = table.get("tasInfo")
        comments = list(table.get("comments") or [])
        acn_comment = self._acn_parameters_comment(table)
        if acn_comment:
            comments.append(acn_comment)
        return IcdTable(
            id=table["id"],
            name=table.get("name", ""),
            kind=table.get("kind", ""),
            tas_name=(tas or {}).get("tasName"),
            module=table.get("module", ""),
            usage_path=table.get("usagePath", ""),
            min_bytes=int(table.get("minLengthInBytes", 0)),
            max_bytes=int(table.get("maxLengthInBytes", 0)),
            comments=comments,
            acn_parameters=list(table.get("acnParameters") or []),
            rows=[],   # parsed in the second pass, once all tables exist
        )

    @staticmethod
    def _acn_parameters_comment(table: dict) -> str:
        parameters = table.get("acnParameters") or []
        if not parameters:
            return ""
        items = []
        for parameter in parameters:
            ptype = parameter.get("type") or {}
            label = ptype.get("value") or ptype.get("id") or ""
            items.append(parameter.get("name", "") + " : " + str(label))
        # Raw text: the caller escapes the joined table comments once.
        return "ACN parameters: <" + ", ".join(items) + ">"

    def _parse_row(self, row: dict) -> IcdRow:
        row_type = row.get("rowType", "FieldRow")
        s_type = row.get("sType") or {}
        # The type column of a reference row is resolved in render_plan: the
        # link must point to a table that is actually rendered, which is only
        # known once the --root selection has been made.
        if s_type.get("kind") == "reference":
            typ, typ_html = "", False
        else:
            typ, typ_html = str(s_type.get("value", "")), False
        off = row.get("offset") or {}
        offset = off.get("display") or ""
        # A varying offset gets a short marker: the absolute position of a
        # variable-size field cannot be given as a number.
        if off.get("kind") == "variable":
            offset = "var"
        # icdRaw comments are HTML by design: asn1scc renders ENUMERATED value
        # lists as <ul class="enumeration"> markup inside the dump (AcnEnum).
        # They are emitted verbatim, exactly like in the official -icdAcn HTML.
        comments = "<br/>".join(c for c in (row.get("comments") or []) if c)
        # The mapping-function note moves to its own column-level marker: it
        # is rendered with a star (*) in the table and the compact view.
        comments, mapping_note = mapping_function_of(comments)
        unit = (row.get("sUnits") or "").strip()
        # SubItemRow (array element instances) and the ThreeDOTs that
        # separate the first/last instance live one level under the header.
        level = 1 if row_type in ("SubItemRow", "ThreeDOTs") else 0
        field_name = row.get("fieldName", "")
        if row_type == "ThreeDOTs":
            field_name = "…"
        if not field_name and row_type == "FieldRow":
            # A primitive type's own table has a single row whose field name
            # is the type itself (the dump leaves it empty).
            field_name = "value"
        ref_id = (s_type.get("id") or "") if s_type.get("kind") == "reference" else ""
        target = self.tables.get(ref_id) if ref_id else None
        # Array length determinant: the external ACN/uPER field that carries
        # the element count ('Length is determined by the external field: n').
        length_field = ""
        if "SEQUENCE OF" in typ:
            for c in (row.get("comments") or []):
                length_field = array_length_comment(c)
                if length_field:
                    break
        # Anonymous encoded determinant: asn1scc names it 'Length' with the
        # comment 'The number of items' (AcnSequenceOf). Shown as N/N2/... per
        # the v2 convention; string 'Length' rows keep their name.
        is_length_det = (row_type == "LengthDeterminantRow" and field_name == "Length"
                         and any("number of items" in c for c in (row.get("comments") or [])))
        return IcdRow(
            number=str(row.get("idxOffset") or ""),
            field=field_name,
            level=level,
            comment=comments,
            comment_html=bool(comments),
            mapping_note=mapping_note,
            present=row.get("sPresent", ""),
            typ=typ,
            typ_html=typ_html,
            constraint=row.get("sConstraint") or "",
            offset=str(offset),
            min_bits=str(row.get("minLengthInBits", "")),
            max_bits=str(row.get("maxLengthInBits", "")),
            unit=unit,
            ref_id=ref_id,
            row_type=row_type,
            ref_kind=(target.kind if target is not None else ""),
            length_field=length_field,
            is_length_det=is_length_det,
            is_choice_link=(target is not None and target.kind == "CHOICE"),
        )

    def resolve_row(self, row: IcdRow, rendered_ids: set[str]) -> IcdRow:
        """Type column and renderer kind of a reference row.

        The link must point to a table that is actually rendered: a link to
        a table flattened inline (or restricted away by -icdPdus) would
        dangle, so such rows get plain text instead.
        """
        if not row.ref_id:
            # A plain type naming a type - 'SEQUENCE OF (Samples)',
            # 'INTEGER (U16)' - links the named type to its own table
            # when that table is rendered.
            m = re.search(r"\(([A-Za-z][\w-]*)\)", row.typ) if "(" in row.typ else None
            if m:
                target = self.by_type_name.get(m.group(1))
                if target is not None and target.id in rendered_ids:
                    linked = row.typ.replace(
                        "(" + m.group(1) + ")",
                        '(' + anchor_link(m.group(1), self.anchors[target.id]) + ")")
                    return replace(row, typ=linked, typ_html=True)
                ga = self.grammar_anchors.get(m.group(1))
                if ga:
                    # No table for this type (primitive / ENUMERATED): the
                    # name links to its ASN.1 grammar definition instead.
                    linked = row.typ.replace(
                        "(" + m.group(1) + ")",
                        '(' + anchor_link(m.group(1), ga) + ")")
                    return replace(row, typ=linked, typ_html=True)
            return row
        target = self.tables.get(row.ref_id)
        if target is None:
            return replace(row, typ=html_escape(row.ref_id.split("@")[0]), typ_html=False)
        label = (target.kind or "TYPE").replace("_", " ") + " " + target.name
        row_kind = "choice-link" if target.kind == "CHOICE" else "type-link"
        if row.ref_id in rendered_ids:
            return replace(row, typ=anchor_link(label, self.anchors[row.ref_id]),
                           typ_html=True, kind_override=row_kind)
        ga = self.grammar_anchors.get(target.tas_name or target.name)
        if ga:
            return replace(row, typ=anchor_link(label, ga), typ_html=True, kind_override=row_kind)
        return replace(row, typ=html_escape(label), typ_html=False, kind_override=row_kind)

    # -- queries -----------------------------------------------------------

    def render_plan(self, root_names: Optional[list[str]]) -> list[tuple[IcdTable, bool]]:
        """Tables to render, in order, with their root flag.

        Sequential mode (no --root): every table of the dump, in nav order;
        reference rows stay links (v2 semantics).

        Root mode (--root given): the dump is already restricted by -icdPdus.
        The v2 behaviour is restored: composite references (SEQUENCE /
        SEQUENCE OF) are flattened INLINE into the root table, so only the
        requested roots plus the tables that cannot be nested -- CHOICE
        tables and the types referenced by their alternatives -- are emitted
        as separate tables. This keeps the number of tables minimal while each
        root table shows the full wire layout.
        """
        all_tables = [(self.tables[t], self.tables[t].is_type_assignment()) for t in self.order if t in self.tables]
        if not root_names:
            return all_tables
        wanted: list[IcdTable] = []
        for name in root_names:
            match = next((t for t, _ in all_tables if t.name == name or t.tas_name == name), None)
            if match is None:
                raise SystemExit("Root not found: " + name)
            if match not in wanted:
                wanted.append(match)
        plan = [(t, True) for t in wanted]
        # consumed: tables whose content is inlined into a rendered table.
        consumed, needed = self._analyze(wanted)
        root_ids = {t.id for t in wanted}
        for tid in self.order:
            if tid not in self.tables or tid in root_ids:
                continue
            # A table reachable from a CHOICE alternative is rendered on its
            # own even when some other table also inlines it (v2 behaviour:
            # the choice links to a standalone table).
            if tid in needed:
                plan.append((self.tables[tid], False))
            elif tid not in consumed:
                # Unreachable from the roots: only possible if -icdPdus
                # pulled in something odd; keep it to stay complete.
                plan.append((self.tables[tid], False))
        return plan

    def _analyze(self, roots: list[IcdTable]) -> tuple[set[str], set[str]]:
        """Which tables are inlined, and which must still be rendered.

        Starting from the root tables: composite references (SEQUENCE /
        SEQUENCE OF) are consumed (inlined, recursively); CHOICE references
        are rendered as separate tables, and the references made by CHOICE
        tables are rendered as separate tables too (v2: choice tables + the
        payloads referenced by their alternatives).
        """
        consumed: set[str] = set()
        needed: set[str] = set()

        def expand(tid: str, path: set[str]) -> list[str]:
            """Inline composites under tid; return the CHOICE refs found."""
            if tid in path:
                return []
            path = path | {tid}
            choices: list[str] = []
            for row in self.tables[tid].rows:
                if not row.ref_id:
                    continue
                target = self.tables.get(row.ref_id)
                if target is None:
                    continue
                if target.kind in ("SEQUENCE", "SEQUENCE OF"):
                    if row.ref_id not in consumed:
                        consumed.add(row.ref_id)
                    choices += expand(row.ref_id, path)
                elif target.kind == "CHOICE":
                    choices.append(row.ref_id)
            return choices

        choice_queue: list[str] = []
        for root in roots:
            if root.kind == "CHOICE":
                # A CHOICE root IS its alternatives table: its references are
                # rendered as separate tables (v2 standalone behaviour),
                # never inlined.
                choice_queue.append(root.id)
            else:
                choice_queue += expand(root.id, set())

        # Choice tables are rendered; their non-choice references too.
        seen_choices: set[str] = set()
        i = 0
        while i < len(choice_queue):
            cid = choice_queue[i]
            i += 1
            if cid in seen_choices or cid not in self.tables:
                continue
            seen_choices.add(cid)
            needed.add(cid)
            for row in self.tables[cid].rows:
                if not row.ref_id or row.ref_id not in self.tables:
                    continue
                target = self.tables[row.ref_id]
                if target.kind == "CHOICE":
                    if row.ref_id not in seen_choices:
                        choice_queue.append(row.ref_id)
                else:
                    if row.ref_id not in needed:
                        needed.add(row.ref_id)
                        choice_queue += expand(row.ref_id, set())
        return consumed, needed

    def definition_tables(self) -> list[IcdTable]:
        """Definition-site tables (one per type assignment), in nav order."""
        return [self.tables[t] for t in self.order
                if t in self.tables and self.tables[t].is_type_assignment()]

    def by_type_name(self, name: str) -> Optional[IcdTable]:
        for table in self.definition_tables():
            if table.name == name or table.tas_name == name:
                return table
        return None

    def table_bits(self, table: IcdTable) -> tuple[str, str]:
        """Min/max size of the whole table, in bits (icdRaw gives bytes)."""
        return str(table.min_bytes * 8), str(table.max_bytes * 8)

    def expand_rows(self, rows: list[IcdRow], visited: Optional[set[str]] = None,
                    rendered_ids: Optional[set[str]] = None) -> list[IcdRow]:
        """Rows with referenced composite tables flattened in place (v2 root mode).

        A reference to a SEQUENCE / SEQUENCE OF becomes a "title" row (field
        name + composite kind, titleRow styling) followed by the referenced
        table's rows one level deeper, recursively. The title row carries an
        anchor link to the referenced table when that table is also rendered
        on its own (a CHOICE-referenced type); otherwise plain text. CHOICE
        references and references to non-rendered tables stay single link
        rows. A table can flatten several times inside one root (e.g. the
        first/last element of a SEQUENCE OF); the visited set only guards
        against cycles.
        """
        is_top = visited is None
        if visited is None:
            visited = set()
        if rendered_ids is None:
            rendered_ids = set(self.tables)
        expanded: list[IcdRow] = []
        for row in rows:
            target = self.tables.get(row.ref_id) if row.ref_id else None
            if target is not None and target.kind in ("SEQUENCE", "SEQUENCE OF") \
                    and target.id not in visited and target.rows:
                # Flatten the composite: title row + its rows one level deeper.
                label = (target.kind or "COMPOSITE").replace("_", " ") + " " + target.name
                title_typ = label
                if row.ref_id in rendered_ids and row.ref_id in self.anchors:
                    title_typ = anchor_link(label, self.anchors[row.ref_id])
                else:
                    ga = self.grammar_anchors.get(target.tas_name or target.name)
                    if ga:
                        # The composite is inlined here and has no table of
                        # its own: its name links to the grammar section.
                        title_typ = anchor_link(label, ga)
                title_row = replace(row, kind_override="title", typ=title_typ, typ_html=(title_typ != label),
                                    anchor="inline_" + safe_id(target.id),
                                    ref_id=row.ref_id, ref_kind=target.kind)
                expanded.append(title_row)
                visited.add(target.id)
                child_rows = [r for r in target.rows if r.row_type != "ThreeDOTs"]
                # A repeated element ('item i (repeated a..b times)') has no
                # single offset: fields inlined under it are 'var'.
                parent_off = "var" if row.is_repeat_summary else row.offset
                remapped = [replace(r, level=row.level + 1 + r.level,
                                    offset=nest_offset(parent_off, r.offset)) for r in child_rows]
                expanded.extend(self.expand_rows(remapped, visited, rendered_ids))
                visited.discard(target.id)
            elif target is not None and target.kind == "CHOICE":
                # A CHOICE keeps its own separate table (v2 behaviour).
                label = "CHOICE " + target.name
                if row.ref_id in rendered_ids:
                    expanded.append(replace(row, kind_override="choice-link",
                                            typ=anchor_link(label, self.anchors[row.ref_id]),
                                            typ_html=True, is_choice_link=True))
                else:
                    expanded.append(replace(row, kind_override="choice-link",
                                            typ=html_escape(label), typ_html=False,
                                            is_choice_link=True))
            elif target is not None:
                # Reference to a leaf or a non-rendered table: link or plain
                # text, pointing only at tables that are really rendered.
                label = (target.kind or "TYPE").replace("_", " ") + " " + target.name
                if row.ref_id in rendered_ids:
                    expanded.append(replace(row, kind_override="type-link",
                                            typ=anchor_link(label, self.anchors[row.ref_id]),
                                            typ_html=True))
                else:
                    expanded.append(replace(row, typ=html_escape(label), typ_html=False))
            else:
                expanded.append(row)
        if is_top:
            counter = 0
            renumbered: list[IcdRow] = []
            for r in expanded:
                if r.is_length_det:
                    counter += 1
                    name = "N" if counter == 1 else f"N{counter - 1}"
                    renumbered.append(replace(r, field=name))
                else:
                    renumbered.append(r)
            return renumbered
        return expanded



# ---------------------------------------------------------------------------
# Grammar rendering
# ---------------------------------------------------------------------------

@dataclass
class TypeDef:
    module: str
    name: str
    asn: Optional[ET.Element]
    concrete: Optional[ET.Element]
    comment: str


class GrammarModel:
    """Indexes the ASN.1 type assignments of the XML AST (grammar rendering)."""

    def __init__(self, root: Optional[ET.Element]):
        self.root = root
        self.by_name: dict[str, TypeDef] = {}
        self.by_qualified_name: dict[str, TypeDef] = {}
        if root is None:
            return
        for module in root.iter():
            if tag(module) != "Module":
                continue
            module_name = attr(module, "Name")
            for assignment in children(child(module, "TypeAssignments"), "TypeAssignment"):
                asn_node = child(assignment, "Asn1Type")
                concrete = first_type(asn_node)
                name = attr(assignment, "Name")
                if name and concrete is not None:
                    typedef = TypeDef(module_name, name, asn_node, concrete, comments_of(assignment))
                    self.by_name.setdefault(name, typedef)
                    self.by_qualified_name[qualified_name(module_name, name)] = typedef

    def by_usage_path(self, usage_path: str) -> Optional[TypeDef]:
        """TypeDef of an icdRaw usagePath (Module.Type or a deeper path)."""
        if self.root is None:
            return None
        for node in self.root.iter():
            if tag(node) == "Asn1Type" and attr(node, "id") == usage_path:
                concrete = first_type(node)
                if concrete is not None:
                    return TypeDef("", attr(node, "id", default="anonymous"), node, concrete, comments_of(node))
        return None


class GrammarRenderer:
    def __init__(self, model: Model, anchors: dict[str, str]):
        self.model = model
        self.anchors = anchors

    def table_anchor(self, typedef: TypeDef) -> str:
        anchor = self.anchors.get(qualified_name(typedef.module, typedef.name))
        if anchor is not None:
            return anchor
        # No table for this type in the current dump (e.g. --root restricted
        # it away): return an empty anchor, rendered as plain text.
        return ""

    def type_reference(self, name: str) -> str:
        typedef = self.model.by_name.get(name)
        anchor = self.table_anchor(typedef) if typedef else ""
        return anchor_link(name, anchor) if anchor else html_escape(name)

    def has_assignment(self, typedef: TypeDef) -> bool:
        """True when the type is a named type assignment of the model.

        Only type assignments get an anchor in the ASN.1/ACN grammar sections,
        so only they can be linked to from a table header.
        """
        return self.model.by_qualified_name.get(qualified_name(typedef.module, typedef.name)) is typedef

    def unit_comment(self, asn_node: Optional[ET.Element] = None, concrete_type: Optional[ET.Element] = None) -> str:
        unit = (attr(asn_node, "unitsOfMeasure") or attr(concrete_type, "unitsOfMeasure")).strip()
        return ' <font color="#008000"><i>--{ ' + html_escape(unit) + ' }--</i></font>' if unit else ""

    def render_type(self, concrete_type: Optional[ET.Element], indent: int = 0, asn_node: Optional[ET.Element] = None) -> str:
        current_kind = type_kind(concrete_type)
        if current_kind == "REFERENCE_TYPE":
            return self.type_reference(attr(concrete_type, "TypeAssignment")) + self.unit_comment(asn_node, concrete_type)
        if current_kind == "SEQUENCE_OF":
            count_text, _fixed = sequence_of_count(concrete_type)
            return '<b>SEQUENCE</b> (<b>SIZE</b> (' + html_escape(count_text) + ')) <b>OF</b> ' + self.render_type(sequence_of_type(concrete_type), indent, sequence_of_asn(concrete_type)) + self.unit_comment(asn_node, concrete_type)
        if current_kind == "ENUMERATED":
            items = children(child(concrete_type, "Items"), "Item")
            return '<b>ENUMERATED</b> { ' + html_escape(", ".join(f'{attr(i, "Name")}({attr(i, "Value")})' for i in items)) + ' }' + self.unit_comment(asn_node, concrete_type)
        if current_kind == "SEQUENCE":
            return self.inline_sequence(concrete_type, indent) + self.unit_comment(asn_node, concrete_type)
        if current_kind == "CHOICE":
            return self.inline_choice(concrete_type, indent) + self.unit_comment(asn_node, concrete_type)
        return '<b>' + html_escape(current_kind) + '</b>' + (" " + html_escape(subtype_constraint(concrete_type)) if subtype_constraint(concrete_type) else "") + self.unit_comment(asn_node, concrete_type)

    def inline_sequence(self, sequence: ET.Element, indent: int = 0) -> str:
        components = [c for c in sequence_children(sequence, False) if component_type(c) is not None]
        if not components:
            return '<b>SEQUENCE</b> {}'
        pad = max(len(attr(c, "Name")) for c in components) + 1
        prefix = " " * (indent + 3)
        lines = ['<b>SEQUENCE</b> {']
        for i, component in enumerate(components):
            comma = "," if i < len(components) - 1 else ""
            lines.append(prefix + html_escape(attr(component, "Name")).ljust(pad) + self.render_type(component_type(component), indent + 3, component_asn(component)) + comma)
        lines.append(" " * indent + "}")
        return "\n".join(lines)

    def inline_choice(self, choice: ET.Element, indent: int = 0) -> str:
        alternatives = [c for c in children(choice, "CHOICE_ALTERNATIVE") if component_type(c) is not None]
        if not alternatives:
            return '<b>CHOICE</b> {}'
        pad = max(len(attr(c, "Name")) for c in alternatives) + 1
        prefix = " " * (indent + 3)
        lines = ['<b>CHOICE</b> {']
        for i, alternative in enumerate(alternatives):
            comma = "," if i < len(alternatives) - 1 else ""
            lines.append(prefix + html_escape(attr(alternative, "Name")).ljust(pad) + self.render_type(component_type(alternative), indent + 3, component_asn(alternative)) + comma)
        lines.append(" " * indent + "}")
        return "\n".join(lines)

    def comment_lines(self, comment: str, indent: str = "") -> list[str]:
        plain = re.sub(r"<br\s*/?>", "\n", comment or "")
        plain = re.sub(r"<[^>]+>", "", plain)
        return [indent + '<font color="#008000"><i>-- ' + html_escape(x.strip()) + '</i></font>' for x in plain.split("\n") if x.strip()]

    def asn_assignment(self, typedef: TypeDef) -> str:
        output = self.comment_lines(typedef.comment)
        current_kind = type_kind(typedef.concrete)
        anchor = self.table_anchor(typedef)
        name = anchor_link(typedef.name, anchor) if anchor else html_escape(typedef.name)
        if current_kind in ("SEQUENCE", "CHOICE"):
            items = sequence_children(typedef.concrete, False) if current_kind == "SEQUENCE" else children(typedef.concrete, "CHOICE_ALTERNATIVE")
            valid = [c for c in items if component_type(c) is not None]
            pad = max([len(attr(c, "Name")) for c in valid], default=0) + 1
            output.append(f"{name} ::= <b>{current_kind}</b> " + "{")
            for i, component in enumerate(valid):
                output += self.comment_lines(comments_of(component), "   ")
                comma = "," if i < len(valid) - 1 else ""
                output.append("   " + html_escape(attr(component, "Name")).ljust(pad) + self.render_type(component_type(component), 3, component_asn(component)) + comma)
            output.append("}")
        else:
            output.append(name + " ::= " + self.render_type(typedef.concrete, 0, typedef.asn))
        return "\n".join(output)

    def acn_arguments(self, asn_node: Optional[ET.Element]) -> str:
        """'<a, b>' - the ACN determinant arguments of a component referencing
        a parameterized type (XML AcnArguments, nested under REFERENCE_TYPE)."""
        if asn_node is None:
            return ""
        args = asn_node.find(".//AcnArguments")
        if args is None:
            return ""
        values = [text(a).strip() for a in args if (text(a) or "").strip()]
        if not values:
            return ""
        # The angle brackets must be HTML-escaped: a literal '<a.b>' would
        # be swallowed as an (unknown) HTML tag by the browser.
        return "&lt;" + ", ".join(html_escape(v) for v in values) + "&gt;"

    def acn_options(self, component: ET.Element, concrete_type: Optional[ET.Element] = None,
                    asn_node: Optional[ET.Element] = None) -> str:
        options: list[str] = []
        if tag(component) == "ACN_COMPONENT":
            if attr(component, "Type") == "NULL" and attr(component, "pattern"):
                return "<b>pattern</b> " + acn_pattern(attr(component, "pattern"))
            if attr(component, "size"):
                options.append("<b>size</b> " + html_escape(attr(component, "size")))
            if attr(component, "pattern"):
                options.append("<b>pattern</b> " + acn_pattern(attr(component, "pattern")))
        elif concrete_type is not None:
            for option in ["size", "encoding", "endianness"]:
                if attr(concrete_type, option):
                    value = html_escape(attr(concrete_type, option))
                    options.append("<b>" + option + "</b> " + ("<b>" + value + "</b>" if option != "size" else value))
        # The mapping-function (INTEGER only): the value on the wire is not
        # the value of the field; both the ACN component and the type
        # assignment carry it.
        mf = attr(component, "mapping-function") or (attr(concrete_type, "mapping-function")
                                                      if concrete_type is not None else None)
        if mf:
            options.append("<b>mapping-function</b> " + html_escape(mf))
        return ", ".join(options)

    def acn_assignment(self, typedef: TypeDef) -> str:
        output = self.comment_lines(typedef.comment)
        anchor = self.table_anchor(typedef)
        name = anchor_link(typedef.name, anchor) if anchor else html_escape(typedef.name)
        if type_kind(typedef.concrete) != "SEQUENCE":
            # A primitive type assignment may still carry ACN properties
            # (e.g. an INTEGER with a mapping-function).
            options = self.acn_options(typedef.concrete, None, typedef.concrete)
            return "\n".join(output + [name + " [" + options + "]" if options else name + " []"])
        parts = sequence_children(typedef.concrete, True)
        pad = max([len(attr(c, "Name", "Id")) for c in parts], default=0) + 1
        output.append(name + " [] {")
        for i, component in enumerate(parts):
            output += self.comment_lines(comments_of(component), "   ")
            comma = "," if i < len(parts) - 1 else ""
            arguments = self.acn_arguments(component_asn(component))
            options = self.acn_options(component, component_type(component) if tag(component) != "ACN_COMPONENT" else None,
                                       component_asn(component))
            output.append("   " + html_escape(attr(component, "Name", "Id")).ljust(pad)
                          + arguments + " [" + options + "]" + comma)
        output.append("}")
        return "\n".join(output)

    def full_grammar(self) -> str:
        asn = ['<h1 id="ASN1_GRAMMAR">ASN.1 grammar</h1><pre class="grammar">']
        acn = ['<h1 id="ACN_GRAMMAR">ACN grammar</h1><pre class="grammar">']
        for key, typedef in sorted(self.model.by_qualified_name.items()):
            asn.append(f'<a id="ASN1_{html_escape(safe_id(typedef.name))}"></a><a id="ASN1_{html_escape(safe_id(key))}"></a>' + self.asn_assignment(typedef) + "\n\n")
            acn.append(f'<a id="ACN_{html_escape(safe_id(typedef.name))}"></a><a id="ACN_{html_escape(safe_id(key))}"></a>' + self.acn_assignment(typedef) + "\n\n")
        return "".join(asn) + "</pre>" + "".join(acn) + "</pre>"


# ---------------------------------------------------------------------------
# HTML rendering
# ---------------------------------------------------------------------------

def build_compact_nodes(rows: list[IcdRow]) -> list[dict]:
    roots: list[dict] = []
    stack: list[dict] = []
    for row in rows:
        if row.kind not in ("field", "title", "choice-link", "type-link", "summary"):
            continue
        node = {"row": row, "children": []}
        while stack and stack[-1]["row"].level >= row.level:
            stack.pop()
        if stack:
            stack[-1]["children"].append(node)
        else:
            roots.append(node)
        stack.append(node)
    return roots


def row_is_choice(row: IcdRow) -> bool:
    return "CHOICE" in strip_html(row.typ).upper() or row.kind == "choice-link" or row.is_choice_link


def is_array_header(row: IcdRow) -> bool:
    """The row is a SEQUENCE OF header whose element rows follow it."""
    return "SEQUENCE OF" in strip_html(row.typ).upper() and not row_is_choice(row)


def array_length_comment(comment: str) -> str:
    """Determinant field name from 'Length is determined by the external field: X'."""
    m = re.search(r"determined by the external field:\s*([\w.-]+)", strip_html(comment))
    return m.group(1) if m else ""


def to_int(value) -> Optional[int]:
    try:
        return int(str(value))
    except (TypeError, ValueError):
        return None


def implicit_array_det_bits(header: IcdRow) -> str:
    """Bit size of the uPER-encoded length determinant of a variable array.

    With no explicit determinant row, asn1scc embeds it in the array row's
    minimum length (e.g. (0..15) with a 4-bit count: min_bits = 4). When the
    minimum is zero (external/deduced determinant), fall back to
    ceil(log2(max+1)) of the constraint range.
    """
    mn = to_int(header.min_bits)
    if mn is not None and mn > 0:
        return str(mn)
    count_text = strip_html(header.constraint) or ""
    m = re.match(r"\(\s*(\d+)\s*\.\.\s*(\d+)\s*\)", count_text)
    if m:
        try:
            return str(math.ceil(math.log2(int(m.group(2)) + 1)))
        except ValueError:
            return ""
    return ""


def next_array_det_name(out: list[IcdRow]) -> str:
    """N for the first array of the diagram, N1, N2, ... for the next ones."""
    count = sum(1 for r in out if r.is_length_det)
    return "N" if count == 0 else f"N{count}"


def element_type_name(first_instance: list[IcdRow], header: IcdRow) -> str:
    """Element type text when the header's type column is bare 'SEQUENCE OF'.

    The collapsed element instance ('smpl[1..10]', 'Item #1') carries the
    element's type; the bracketed named type is kept.
    """
    for r in first_instance:
        t = strip_html(r.typ).strip()
        if t and t != "SEQUENCE OF":
            return re.sub(r"\s*\(\s*[^)]*\)$", "", t).strip()
    return "element"


def element_field_name(header: IcdRow) -> str:
    """The field name of an array, cleaned for the compact diagram.

    The dump names child tables 'Parent-instructions' or
    'ASW-...-GENERIC-instructions'; the compact diagram must show the
    plain field name ('instructions'), not the child table's name.
    """
    name = strip_html(header.field).strip()
    # strip the '<type>-<field>' synthesis used by specializations
    m = re.match(r"^(?:[A-Z0-9-]+)-([a-z][\w-]*)$", name)
    if m:
        return m.group(1)
    return name


def _instance_suffix(field: str) -> bool:
    """True when the row field names an array instance ('Item #2', 'smpl[15]')."""
    name = strip_html(field).strip()
    return bool(re.match(r"^(Item #\d+|.+\[\d+\])$", name))


def repeat_summary_name(first: Optional[IcdRow], last: IcdRow, start: int = 2) -> str:
    """Marker for the collapsed instances of a repeated element.

    start=2 (composite): 'Items #2..#7' after the kept first instance;
    start=1 (leaf): 'a[1..15]' - a single row covering all instances.
    """
    last_name = strip_html(last.field)
    m = re.match(r"^(.*?)(\d+)(\]?)$", last_name)
    if not m:
        return "…" + last_name
    prefix, number, tail = m.group(1), int(m.group(2)), m.group(3)
    if prefix.endswith("["):
        return f"{prefix}{start}..{number}{tail}"
    if prefix.endswith("#"):
        # 'Item #' + start=2 -> 'Items #2..#4'
        stem = prefix[:-2].rstrip() if prefix[:-1].endswith(" ") else prefix[:-1]
        plural = "s" if start > 1 else ""
        return stem + plural + " #" + f"{start}..#{number}{tail}"
    return f"{prefix}{start}..{number}{tail}"


def normalize_count(text: str) -> str:
    """'(0 .. 10)' -> '0..10', '(3)' -> '3'."""
    t = strip_html(text).strip()
    if t.startswith("(") and t.endswith(")"):
        t = t[1:-1].strip()
    return re.sub(r"\s*\.\.\s*", "..", t)


def prepare_compact_rows(rows: list[IcdRow], det_counter: Optional[list[int]] = None) -> list[IcdRow]:
    """Transform expanded rows for the compact diagram.

    Arrays (SEQU OF): the element is shown ONCE - the header row becomes
    '<elem>' - and the element instance rows (SubItemRow
    smpl[1]/smpl[10], the dots, the expanded composite 'Item #n' instances)
    are dropped; a composite element keeps only its fields, grouped under
    the element cell. The length determinant becomes its own cell BEFORE
    the element (N, N1, ... per diagram), EXCEPT for the external-field
    arrays (ACN [size f]): that field is already a cell of the table, and
    for deduced arrays no determinant is encoded at all. The element cell
    carries the footer 'Repeated <N|field|count|deduced> times'.
    """
    if det_counter is None:
        det_counter = [0]
    out: list[IcdRow] = []
    i = 0
    while i < len(rows):
        row = rows[i]
        if not is_array_header(row):
            out.append(row)
            i += 1
            continue
        header = row
        j = i + 1
        instance_rows: list[IcdRow] = []
        while j < len(rows) and rows[j].level > header.level:
            instance_rows.append(rows[j])
            j += 1

        det_row = next((r for r in instance_rows if r.is_length_det and r.level == header.level + 1), None)
        count_text = normalize_count(header.constraint)
        deduced = "deduced" in strip_html(header.comment).lower()

        # Determinant cell (N / N1...), never for external or deduced.
        det_name = ""
        if header.length_field:
            pass                     # external: the field has its own cell
        elif deduced:
            pass                     # nothing is encoded
        elif det_row is not None:
            det_name = det_row.field
            m = re.match(r"^N(\d*)$", det_name)
            if m:
                idx = int(m.group(1)) if m.group(1) else 0
                det_counter[0] = max(det_counter[0], idx + 1)
            det_bits = implicit_array_det_bits(det_row) or det_row.bits_meta()
            out.append(replace(header, field=det_name, typ=count_text, constraint="",
                               min_bits=str(to_int(det_bits) or ""), max_bits=str(to_int(det_bits) or ""),
                               comment="", comment_html=False, kind_override="",
                               ref_id="", ref_kind="", anchor="", repeat_label="",
                               length_field="", is_length_det=True))
        elif count_text and ".." in count_text:
            det_idx = det_counter[0]
            det_counter[0] += 1
            det_name = "N" if det_idx == 0 else f"N{det_idx}"
            det_bits = implicit_array_det_bits(header)
            out.append(replace(header, field=det_name, typ=count_text, constraint="",
                               min_bits=det_bits, max_bits=det_bits, comment="",
                               comment_html=False, kind_override="", ref_id="",
                               ref_kind="", anchor="", repeat_label="",
                               length_field="", is_length_det=True))

        # Element cell: '<elem>' + repeated footer.
        element_typ = strip_html(header.typ).replace("SEQUENCE OF", "").strip()
        if element_typ.startswith("(") and element_typ.endswith(")"):
            element_typ = element_typ[1:-1].strip()

        # The compact cell shows the FIELD name ('instructions', 'content'), never
        # the child table's synthesized name ('ASW-...-GENERIC-instructions')
        # and without '× count' (which is already in the repeat footer).
        field_name = element_field_name(header)
        element_name = field_name

        if header.length_field:
            repeat_text = f"Repeated {header.length_field} times"
        elif deduced:
            repeat_text = "Repeated (deduced) times"
        elif not count_text:
            repeat_text = "Repeated"
        elif ".." in count_text:
            repeat_text = f"Repeated {det_name or 'N'} times"
        else:
            repeat_text = f"Repeated {count_text} times"

        # Find first instance and its children in instance_rows
        first_inst_idx = next(
            (k for k, r in enumerate(instance_rows) if not r.is_length_det and r.row_type != "ThreeDOTs"),
            None
        )

        children: list[IcdRow] = []
        first_inst: Optional[IcdRow] = None
        if first_inst_idx is not None:
            first_inst = instance_rows[first_inst_idx]
            inst_level = first_inst.level
            if not element_typ:
                t = strip_html(first_inst.typ).strip()
                if t and t != "SEQUENCE OF":
                    element_typ = re.sub(r"\s*\(\s*[^)]*\)$", "", t).strip()
            k = first_inst_idx + 1
            while k < len(instance_rows) and instance_rows[k].level > inst_level:
                children.append(instance_rows[k])
                k += 1

        is_choice = row_is_choice(first_inst) if first_inst else False
        elem_row = replace(
            header,
            field=element_name,
            typ=element_typ or "SEQUENCE OF",
            constraint="",
            repeat_label=repeat_text,
            anchor="",
            is_choice_link=is_choice,
        )
        out.append(elem_row)

        # Composite element: its fields nest under the element cell;
        # a leaf element has no children.
        if children:
            shift = inst_level - header.level
            remapped = [replace(r, level=r.level - shift) for r in children]
            out.extend(prepare_compact_rows(remapped, det_counter=det_counter))

        i = j
    return out


def compact_choice_reference(row: IcdRow) -> str:
    type_text = strip_html(row.typ)
    choice_name = re.sub(r"^CHOICE\s*", "", type_text, flags=re.I).strip()
    return "See " + choice_name if choice_name else "See CHOICE"


def compact_node_html(node: dict) -> str:
    row: IcdRow = node["row"]
    name = strip_html(row.field)
    typ = row.compact_type()
    meta: list[str] = []
    if "optional" in str(row.present).lower() or str(row.present).strip().lower().startswith("when"):
        # Optional field, or an ACN present-when / presence-mask condition
        # ('when pos-present is true') - not always on the wire.
        meta.append("optional")
    if row.repeat_label:
        meta.append(row.repeat_label)
    if row.mapping_note:
        # Starred note under the field: the value is (partly) computed by the
        # mapping function, e.g. '* fn — provided by the function at run time'.
        meta.append("* " + row.mapping_note)
    if is_array_header(row):
        pass                       # the element cell says '× <count>' already
    elif not row_is_choice(row):
        bits = row.bits_meta()
        if bits and not node["children"]:
            # A group's footer already carries its own size annotation;
            # leaf cells get the per-field bit count.
            meta.append(bits)
    meta_html = "".join('<div class="packetMeta">' + html_escape(m) + "</div>" for m in meta)

    if row_is_choice(row):
        return '<div class="packetCell packetChoice"><div class="packetFieldName">' + html_escape(name) + '</div><div class="packetFieldType">' + html_escape(compact_choice_reference(row)) + '</div>' + meta_html + '</div>'
    if node["children"]:
        return '<div class="packetGroup"><div class="packetGroupTitle">' + html_escape(name) + '</div><div class="packetGroupChildren">' + "".join(compact_node_html(c) for c in node["children"]) + '</div>' + meta_html + '</div>'
    return '<div class="packetCell"><div class="packetFieldName">' + html_escape(name) + '</div><div class="packetFieldType">' + html_escape(typ) + '</div>' + meta_html + '</div>'


def compact_diagram_html(title: str, rows: list[IcdRow], is_root: bool, is_choice: bool,
                         table_comments: Optional[list[str]] = None) -> str:
    css_class = "rootCompact" if is_root else "otherCompact"
    if is_choice:
        # The determinant is external whenever ACN decides the alternative
        # (table comment "Active alternative is determined by ACN using the
        # field: 'X'"): no ChoiceIndex is encoded, so the diagram names the
        # deciding field instead. With an internal determinant the dump has
        # a ChoiceIndex row and the diagram keeps the v2 generic boxes.
        selector_name = "ChoiceIndex"
        for row in rows:
            if strip_html(row.field).lower() == "choiceindex":
                selector_name = strip_html(row.field)
                break
        deduced_from = "deduced from ChoiceIndex"
        for comment in (table_comments or []):
            m = re.search(r"determined by ACN using the field:\s*'([^']+)'", strip_html(comment))
            if m:
                selector_name = m.group(1)
                deduced_from = "deduced from " + m.group(1)
                break
        return ('<div class="packetDiagram ' + css_class + '"><div class="packetDiagramTitle">Compact packet layout: '
                + html_escape(title) + '</div><div class="packetMap">'
                + '<div class="packetCell packetChoice"><div class="packetFieldName">' + html_escape(selector_name)
                + '</div><div class="packetFieldType">choice selector</div></div>'
                + '<div class="packetCell packetChoice"><div class="packetFieldName">selected alternative</div>'
                + '<div class="packetFieldType">' + html_escape(deduced_from) + '</div></div></div></div>')

    nodes = build_compact_nodes(prepare_compact_rows(rows))
    if not nodes:
        return ""
    return '<div class="packetDiagram ' + css_class + '"><div class="packetDiagramTitle">Compact packet layout: ' + html_escape(title) + '</div><div class="packetMap">' + "".join(compact_node_html(n) for n in nodes) + '</div></div>'


def collapsible(title: str, html_content: str) -> str:
    return '<details class="block" open><summary>' + html_escape(title) + '</summary>' + html_content + '</details>'


def bits_and_bytes(bits: str) -> str:
    """'16 bits (2 bytes)' - the byte part only when it is a whole number."""
    n = to_int(bits)
    if n is None or n == 0:
        return ""
    bytes_f = n / 8
    if bytes_f == int(bytes_f):
        return f" ({int(bytes_f)} bytes)"
    return ""


def table_html(anchor: str, title: str, kind_name: str, min_size: str, max_size: str,
               rows: list, grammar: Optional[GrammarRenderer],
               typedef: Optional[TypeDef], is_root: bool = False,
               compact_rows: Optional[list] = None,
               table_comments: Optional[list[str]] = None) -> str:
    simple = title.split(".")[-1]
    # Grammar links point to the ASN.1/ACN grammar sections, where every type
    # assignment has an anchor. Anonymous tables (e.g. CHOICE alternative
    # payloads such as 'red' inside a CHOICE) are not type assignments, so no
    # anchor exists for them: skip the link instead of pointing nowhere.
    grammar_anchor = "ASN1_" + safe_id(simple) if (grammar and typedef and grammar.has_assignment(typedef)) else ""
    acn_anchor = "ACN_" + safe_id(simple) if grammar_anchor else ""
    file_links = (
        (f'<a class="fileLink" href="#{html_escape(grammar_anchor)}">ASN.1</a> ' if grammar_anchor else "")
        + (f'<a class="fileLink" href="#{html_escape(acn_anchor)}">ACN</a>' if acn_anchor else "")
    )
    local_asn = collapsible("ASN.1 representation", '<pre class="grammar localGrammar">' + (grammar.asn_assignment(typedef) if grammar and typedef else "(ASN.1 AST not available)") + "</pre>")
    local_acn = collapsible("ACN representation", '<pre class="grammar localGrammar">' + (grammar.acn_assignment(typedef) if grammar and typedef else "(ASN.1 AST not available)") + "</pre>")
    compact = collapsible("Compact representation", compact_diagram_html(title, compact_rows if compact_rows is not None else rows, is_root, kind_name.upper() == "CHOICE", table_comments))

    def row_html(row: IcdRow) -> str:
        row_class = {"section": "sectionRow", "title": "titleRow", "choice-link": "choiceLinkRow", "type-link": "choiceLinkRow", "summary": "titleRow", "field": ""}.get(row.kind, "")
        field_html = row.field if row.field_html else html_escape(row.field)
        # Tree chars: every non-section row shows the leaf marker at its
        # indentation level, so nested (inlined) rows read as a tree.
        if row.kind != "section":
            field_html = "├── " + field_html
        if row.mapping_note:
            field_html += ' <span class="mapFnStar" title="' + html_escape(row.mapping_note) + '">*</span>'
        if row.anchor:
            field_html = '<a id="' + html_escape(row.anchor) + '"></a>' + field_html
        type_html = row.typ if row.typ_html else html_escape(row.typ)
        comment_html = row.comment if row.comment_html else html_escape(row.comment)
        if row.mapping_note:
            starred = '<span class="mapFnStar">*</span> ' + html_escape(row.mapping_note)
            comment_html = (comment_html + "<br/>" + starred) if comment_html else starred
        values = [
            html_escape(row.number),
            f'<span class="level level{min(row.level, 8)}">{field_html}</span>',
            comment_html,
            html_escape(row.present),
            type_html,
            html_escape(row.constraint),
            html_escape(row.offset),
            html_escape(row.min_bits),
            html_escape(row.max_bits),
            html_escape(row.unit),
        ]
        return f'<tr class="{row_class}">' + "".join(f'<td class="c{i}">{v}</td>' for i, v in enumerate(values)) + "</tr>"

    table = (
        f'<table><tbody><tr class="typeRow"><td colspan="8">{html_escape(title)} ({html_escape(kind_name)}) '
        f'{file_links}</td>'
        f'<td>Min: {html_escape(min_size)} bits{bits_and_bytes(min_size)}</td>'
        f'<td>Max: {html_escape(max_size)} bits{bits_and_bytes(max_size)}</td></tr><tr>'
        + "".join("<th>" + html_escape(h) + "</th>" for h in TABLE_HEADERS)
        + "</tr>"
        + "".join(row_html(row) for row in rows)
        + "</tbody></table>"
    )

    return f'<h2 id="{html_escape(anchor)}">{html_escape(title)}</h2>' + local_asn + local_acn + compact + collapsible("Detailed table", table)


def make_document(data: dict) -> IcdDocument:
    """Build the IcdDocument and its id->anchor map (anchors need all ids)."""
    ids = [t.get("id", "") for t in data.get("tables", [])]
    order = [i for i in data.get("navOrder", []) if i in set(ids)]
    anchors = build_anchors(ids, order, data)
    return IcdDocument(data, anchors)


def build_anchors(ids: list[str], order: list[str], data: dict) -> dict[str, str]:
    """HTML anchor of every table: definition sites keep the root_ prefix."""
    tables_by_id = {t.get("id"): t for t in data.get("tables", [])}
    anchors: dict[str, str] = {}
    for table_id in order:
        table = tables_by_id.get(table_id)
        if table is None:
            continue
        tas = table.get("tasInfo") or {}
        module = table.get("module", "")
        is_definition = tas.get("tasName") and table.get("usagePath", "") == qualified_name(module, tas.get("tasName", ""))
        prefix = "root_" if is_definition else "type_"
        anchor = safe_id(prefix + table_id)
        anchors[table_id] = anchor
        # Also key by usage path: for a definition table this is the
        # qualified type name (Module.Type) used by grammar-side lookups.
        usage_path = table.get("usagePath", "")
        if usage_path:
            anchors.setdefault(usage_path, anchor)
    return anchors


def table_html_for(doc: IcdDocument, table: IcdTable, anchor: str, grammar: Optional[GrammarRenderer],
                   typedef: Optional[TypeDef], is_root: bool, nested: bool = False,
                   rendered_ids: Optional[set[str]] = None) -> str:
    """Render one icdRaw table: header section row + the dump's rows.

    nested=True (root mode, non-CHOICE tables): referenced composites are
    flattened inline like the pre-icdRaw pipeline, so the table shows the
    full wire layout and the number of tables stays minimal. CHOICE tables
    keep their alternatives as links (v2 rendered the payloads separately).
    The compact diagram always expands the references, so it keeps showing
    the subfields even in sequential mode.
    """
    min_bits_s, max_bits_s = str(table.min_bytes * 8), str(table.max_bytes * 8)
    # icdRaw comments are HTML fragments (enum lists) - emitted verbatim.
    header_comment = "<br/>".join(c for c in table.comments if c)
    section = _SectionRow(number="", level=0, field=table.name, comment=header_comment, comment_html=bool(header_comment),
                          present="", typ=table.kind, constraint="", offset="0",
                          min_bits=min_bits_s, max_bits=max_bits_s, unit="")
    if nested:
        expanded = doc.expand_rows(table.rows, rendered_ids=rendered_ids)
    else:
        expanded = list(table.rows)
    rows = [section] + expanded
    compact_rows = [section] + doc.expand_rows(table.rows, rendered_ids=rendered_ids)
    title = qualified_name(table.module, table.tas_name) if table.is_type_assignment() else table.name
    return table_html(anchor, title, table.kind, min_bits_s, max_bits_s, rows, grammar, typedef, is_root,
                      compact_rows=compact_rows, table_comments=table.comments)


def visible_anchors(doc: IcdDocument, rendered_ids: set[str]) -> dict[str, str]:
    """Anchors of the rendered tables only, keyed by table id and usage path.

    Grammar-side type references link through this mapping: a type whose
    table was flattened inline (or restricted away) must not be linked.
    """
    out: dict[str, str] = {}
    for table_id in rendered_ids:
        if table_id not in doc.anchors:
            continue
        out[table_id] = doc.anchors[table_id]
        table = doc.tables.get(table_id)
        if table is not None and table.usage_path:
            out.setdefault(table.usage_path, doc.anchors[table_id])
    return out


def render_document(doc: IcdDocument, model: Optional[GrammarModel], root_names: Optional[list[str]]) -> str:
    content: list[str] = []
    navigation: list[tuple[str, str, str]] = []

    plan = doc.render_plan(root_names)
    nested_mode = bool(root_names)      # root mode: flatten composites inline
    rendered_ids = {table.id for table, _ in plan}
    # Type names that have no icdRaw table (primitives, ENUMERATEDs) still
    # link to their ASN.1 grammar definition.
    if model is not None:
        for name in model.by_name:
            doc.grammar_anchors[name] = "ASN1_" + safe_id(name)
    # Resolve the type-column links of every table against the tables
    # that actually appear in the document. ALL tables, not only the
    # rendered ones: in root mode the composites flattened inline come
    # from tables that are not in the plan, and their rows must carry
    # their links too.
    for table in doc.tables.values():
        table.rows = [doc.resolve_row(row, rendered_ids) for row in table.rows]
    grammar = GrammarRenderer(model, visible_anchors(doc, rendered_ids)) if model is not None else None

    for table, is_root in plan:
        anchor = doc.anchors[table.id]
        typedef = None
        if grammar is not None:
            typedef = (grammar.model.by_qualified_name.get(table.usage_path)
                       or grammar.model.by_usage_path(table.usage_path))
        navigation.append((anchor, table.name, "root" if is_root else "normal"))
        # CHOICE tables keep their alternatives as links to the separate
        # payload tables (v2 behaviour); everything else flattens inline.
        nested = nested_mode and table.kind != "CHOICE"
        content.append(table_html_for(doc, table, anchor, grammar, typedef, is_root, nested=nested,
                                      rendered_ids=rendered_ids))

    if grammar is not None:
        navigation += [("ASN1_GRAMMAR", "ASN.1 grammar", "normal"), ("ACN_GRAMMAR", "ACN grammar", "normal")]

    css = """
*{box-sizing:border-box}body{margin:0;font-family:Arial,sans-serif;color:#222;display:flex;height:100vh}.nav-pane{width:270px;background:#f4f4f4;padding:15px;overflow-y:auto;position:fixed;height:100vh;border-right:2px solid #ccc}.content{margin-left:270px;padding:20px;overflow-y:auto;height:100vh;flex-grow:1}.nav-pane a{display:block;padding:5px;text-decoration:none;color:#333;border-bottom:1px solid #ddd;font-size:14px}.nav-pane a.root{font-weight:bold;background-color:#e4e4e4;margin-top:8px}.nav-pane a:hover{background:#ddd}#navFilterInput{width:100%;padding:8px;margin-bottom:12px;border:1px solid #ccc;border-radius:4px}.block{border:1px solid #ccc;border-radius:4px;margin:8px 0 14px 0;padding:0;background:#fff}.block>summary{cursor:pointer;background:#eee;padding:7px 10px;font-weight:700}table{border-collapse:collapse;width:100%;font-size:13px;margin:0 0 18px 0}th,td{border:1px solid #777;padding:5px 7px;vertical-align:top}th{background:#e6e6e6;text-align:left}.typeRow td{background:#cfd8e6;font-weight:900;font-size:15px}.sectionRow td{background:#d9ead3;font-weight:900;font-size:14px}.titleRow td{background:#eef5ff;font-weight:900;font-size:14px}.choiceLinkRow td{background:#fff2cc;font-weight:800}.c0,.c7,.c8{text-align:right;white-space:nowrap}.c1{white-space:nowrap}.c6{font-family:Consolas,"Courier New",monospace;white-space:nowrap}.level{display:inline-block}.level0{padding-left:0}.level1{padding-left:1.5em}.level2{padding-left:3em}.level3{padding-left:4.5em}.level4{padding-left:6em}.enumeration{margin:0;padding-left:18px}.enumeration_name{font-weight:bold}.choiceOptions{margin:4px 0 0 18px;padding:0}.packetDiagram{margin:10px}.packetDiagramTitle{font-weight:bold;margin:4px 0 6px 0;color:#000}.packetMap{display:flex;align-items:stretch;flex-wrap:wrap;gap:0}.packetCell{min-width:105px;max-width:180px;text-align:center;display:flex;flex-direction:column;color:#000}.packetFieldName{padding:8px 6px;min-height:54px;display:flex;align-items:center;justify-content:center;font-size:14px;color:#000}.packetFieldType{padding:5px 4px;min-height:30px;background:rgba(255,255,255,.35);font-size:11px;display:flex;align-items:center;justify-content:center;flex-grow:1;color:#000}.packetGroup{display:flex;flex-direction:column;min-width:150px;color:#000}.packetGroupTitle{text-align:center;font-weight:700;padding:7px;color:#000}.packetGroupChildren{display:flex;align-items:stretch}.packetMeta{font-size:10px;font-style:italic;border-top:1px solid #999;background:#fff;padding:1px 2px;text-align:center;color:#000}.rootCompact .packetCell,.rootCompact .packetGroup{border:1px solid #1f5fa8;background:#9eb9d8}.rootCompact .packetFieldName,.rootCompact .packetGroupTitle{border-bottom:1px solid #1f5fa8;background:#9eb9d8}.rootCompact .packetFieldType{background:rgba(255,255,255,.28)}.otherCompact .packetCell,.otherCompact .packetGroup{border:1px solid #d93025;background:#f9c08e}.otherCompact .packetFieldName,.otherCompact .packetGroupTitle{border-bottom:1px solid #d93025;background:#f7bd88}.packetChoice{font-style:italic}.grammar{background:#fbfbfb;border:1px solid #ccc;padding:12px;white-space:pre-wrap;font-family:Consolas,"Courier New",monospace;margin:0}.localGrammar{border:0}.grammar b{color:#5F9EA0}.grammar font{color:#008000}.fileLink{font-size:11px;margin-left:6px}a{color:#0645ad}.mapFnStar{color:#c0392b;font-weight:bold;text-decoration:none;cursor:help}
""".strip()

    javascript = """
<script>(function(){document.addEventListener("DOMContentLoaded",function(){const f=document.getElementById("navFilterInput"),a=document.getElementById("navigationMenu").getElementsByTagName("a");f.addEventListener("keyup",function(){const s=f.value.toUpperCase();for(let i=0;i<a.length;i++){const t=a[i].textContent||a[i].innerText;a[i].style.display=t.toUpperCase().indexOf(s)>-1?"":"none";}});});})();</script>
""".strip()

    nav_html = (
        '<nav class="nav-pane" id="navigationMenu"><h3>Navigation</h3>'
        '<input type="text" id="navFilterInput" placeholder="Filter tables..."/>'
        + "".join(f'<a class="{html_escape(css_class)}" href="#{html_escape(anchor)}">{html_escape(title)}</a>' for anchor, title, css_class in navigation)
        + "</nav>"
    )

    body_intro = (
        '<div class="content"><em>The following tables describe the binary encodings of the data model using the ACN Encoding Rules. '
        'Implicit encoding fields (length determinants, presence bits, choice discriminants, padding and fixed patterns) are included.</em><br/><br/>'
    )
    grammar_html = grammar.full_grammar() if grammar else ""

    return (
        '<!DOCTYPE html><html><head><meta charset="utf-8"><title>ICD</title><style>' + css + "</style></head><body>"
        + nav_html
        + body_intro
        + "".join(content)
        + grammar_html
        + "</div>"
        + javascript
        + "</body></html>"
    )


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

ASN1_EXTENSIONS = {".asn", ".asn1"}
ACN_EXTENSIONS = {".acn"}
JSON_EXTENSIONS = {".json"}
XML_EXTENSIONS = {".xml"}


def find_asn1scc() -> str:
    """Locate the asn1scc executable.

    The ASN1SCC environment variable takes precedence, then the PATH, then the
    binary built inside this repository (contrib/../asn1scc/bin).
    """
    from_env = os.environ.get("ASN1SCC")
    if from_env:
        if not Path(from_env).exists():
            raise SystemExit(f"asn1scc not found at ASN1SCC={from_env}")
        return from_env

    found = shutil.which("asn1scc")
    if found:
        return found

    repo_bin = Path(__file__).resolve().parent.parent / "asn1scc" / "bin"
    for configuration in ("Release", "Debug"):
        for candidate in (repo_bin / configuration).glob("net*/asn1scc"):
            if candidate.is_file():
                return str(candidate)

    raise SystemExit(
        "asn1scc executable not found. Install it or set the ASN1SCC environment\n"
        "variable to the asn1scc binary (used to produce the ICD tables)."
    )


def run_asn1scc(files: list[str], root_names: Optional[list[str]]) -> tuple[dict, Optional[ET.Element]]:
    """Run asn1scc and return (icdRaw json data, XML AST element or None).

    One invocation produces both files: the -icdRaw dump drives the tables
    (including all implicit encoding fields pre-processed by the compiler),
    and the -x XML AST is used only to render the ASN.1/ACN grammar blocks.
    Both are written inside a temporary directory invisible to the user.
    """
    asn1_files = [f for f in files if Path(f).suffix.lower() in ASN1_EXTENSIONS]
    acn_files = [f for f in files if Path(f).suffix.lower() in ACN_EXTENSIONS]
    if not asn1_files:
        raise SystemExit("No ASN.1 input file found (expected a .asn or .asn1 file).")
    known = ASN1_EXTENSIONS | ACN_EXTENSIONS
    unsupported = [f for f in files if Path(f).suffix.lower() not in known]
    if unsupported:
        raise SystemExit(
            "Unsupported input file(s) (expected .asn, .asn1 or .acn): " + ", ".join(unsupported)
        )

    executable = find_asn1scc()
    with tempfile.TemporaryDirectory(prefix="icd-generator-") as work_dir:
        json_path = Path(work_dir) / "icd.json"
        xml_path = Path(work_dir) / "model.xml"
        command = [executable, "-icdRaw", str(json_path), "-x", str(xml_path), "-ACN"]
        if root_names:
            command += ["-icdPdus", ",".join(root_names)]
        command += asn1_files + acn_files
        try:
            completed = subprocess.run(command, capture_output=True, text=True)
        except OSError as error:
            raise SystemExit(f"Could not run asn1scc ({executable}): {error}")
        if completed.returncode != 0 or not json_path.exists():
            sys.stderr.write(completed.stdout)
            sys.stderr.write(completed.stderr)
            raise SystemExit(
                f"asn1scc failed with exit code {completed.returncode}.\n"
                f"Command was: {' '.join(command)}"
            )
        icd_data = json.loads(json_path.read_text(encoding="utf-8"))
        xml_root = ET.parse(xml_path).getroot() if xml_path.exists() else None
        return icd_data, xml_root


def load_input(args_files: list[str], root_names: Optional[list[str]]) -> tuple[dict, Optional[ET.Element]]:
    """Load the ICD dump: from .json passthrough, or by running asn1scc."""
    json_files = [f for f in args_files if Path(f).suffix.lower() in JSON_EXTENSIONS]
    if json_files:
        if len(json_files) != len(args_files):
            raise SystemExit(
                "A .json input must be the only input file "
                "(it is a complete -icdRaw dump; ASN.1/ACN files cannot be mixed with it)."
            )
        # A pre-generated dump: tables are available, but the grammar blocks
        # have no XML AST to be rendered from and are omitted.
        return json.loads(Path(json_files[0]).read_text(encoding="utf-8")), None
    return run_asn1scc(args_files, root_names)


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Generate an ICD HTML document from ASN.1/ACN files (or from an existing -icdRaw dump).",
        epilog=(
            "asn1scc is called internally with -icdRaw: the ICD tables it produces already "
            "contain all implicit encoding fields (array lengths, presence bits, choice "
            "discriminants, patterns, padding). When --root is omitted, every type of the "
            "model gets its own table, in declaration order. "
            "Examples: icd-generator.py -o out.html model.asn model.acn | "
            "icd-generator.py --root Packet -o out.html model.asn model.acn"
        ),
    )
    parser.add_argument("-o", "--output", required=True, metavar="icd.html",
                        help="output HTML file")
    parser.add_argument("--root", action="append", default=None, metavar="TYPE",
                        help="root type name to document (default: every type, sequentially); "
                             "repeat --root for multiple root tables")
    parser.add_argument("files", nargs="+", metavar="file",
                        help="input ASN.1 (.asn/.asn1), ACN (.acn) files, or a single -icdRaw "
                             "dump (.json)")
    args = parser.parse_args()

    icd_data, xml_root = load_input(args.files, args.root)
    doc = make_document(icd_data)
    model = GrammarModel(xml_root) if xml_root is not None else None
    Path(args.output).write_text(render_document(doc, model, args.root), encoding="utf-8")


if __name__ == "__main__":
    main()

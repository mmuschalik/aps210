#!/usr/bin/env python3
"""Generate a paragraph-by-paragraph traceability matrix from aps-210.html to APS210/*.lean."""

from __future__ import annotations

import glob
import re
from html.parser import HTMLParser
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
APS210_HTML = ROOT / "aps-210.html"
OUT_MD = ROOT / "APS210_TRACEABILITY_MATRIX.md"

IMPL_FILES = sorted(
    p
    for p in glob.glob(str(ROOT / "APS210/**/*.lean"), recursive=True)
    if not p.endswith(("Tests.lean", "Theorems.lean"))
)

DEFAULT_SECTION = {
    "APS210/Basic.lean": "Main Body",
    "APS210/Governance.lean": "Main Body",
    "APS210/Compliance.lean": "Main Body",
    "APS210/ExternalStandards.lean": "Attachment C",
    "APS210/LCR.lean": "Attachment A",
    "APS210/LCR/HQLA.lean": "Attachment A",
    "APS210/LCR/Outflows.lean": "Attachment A",
    "APS210/LCR/Inflows.lean": "Attachment A",
    "APS210/MLH.lean": "Attachment B",
    "APS210/NSFR.lean": "Attachment C",
}


class ParaParser(HTMLParser):
    def __init__(self) -> None:
        super().__init__()
        self.section = "Pre"
        self.in_heading = False
        self.hbuf: list[str] = []
        self.in_item = False
        self.item_depth = 0
        self.cur_num = ""
        self.ibuf: list[str] = []
        self.paras: list[tuple[str, int, str]] = []

    def handle_starttag(self, tag: str, attrs: list[tuple[str, str | None]]) -> None:
        d = dict(attrs)
        if tag in ("h2", "h3", "h4"):
            self.in_heading = True
            self.hbuf = []
        if tag == "div" and "list-item-1" in (d.get("class") or ""):
            self.in_item = True
            self.item_depth = 1
            self.cur_num = (d.get("data-item-number") or "").rstrip(".")
            self.ibuf = []
        elif self.in_item and tag == "div":
            self.item_depth += 1

    def handle_endtag(self, tag: str) -> None:
        if tag in ("h2", "h3", "h4") and self.in_heading:
            self.in_heading = False
            htxt = re.sub(r"\s+", " ", "".join(self.hbuf)).strip()
            if "Prudential Standard APS 210" in htxt:
                self.section = "Main Body"
            elif "Attachment A" in htxt:
                self.section = "Attachment A"
            elif "Attachment B" in htxt:
                self.section = "Attachment B"
            elif "Attachment C" in htxt:
                self.section = "Attachment C"
            elif htxt.startswith("APG 210"):
                self.section = "APG 210"
        if self.in_item and tag == "div":
            self.item_depth -= 1
            if self.item_depth == 0:
                self.in_item = False
                txt = re.sub(r"\s+", " ", "".join(self.ibuf)).strip()
                if (
                    self.section
                    in ("Main Body", "Attachment A", "Attachment B", "Attachment C")
                    and self.cur_num.isdigit()
                ):
                    self.paras.append((self.section, int(self.cur_num), txt))

    def handle_data(self, data: str) -> None:
        if self.in_heading:
            self.hbuf.append(data)
        if self.in_item:
            self.ibuf.append(data)


def find_enclosing_symbol(lines: list[str], idx: int) -> str:
    """Find the nearest enclosing or following def/structure/inductive name."""
    decl_rx = re.compile(
        r"^(?:private\s+|protected\s+)?(?:def|structure|inductive|abbrev)\s+([A-Za-z0-9_.']+)"
    )
    for j in range(idx, min(len(lines), idx + 15)):
        m = decl_rx.match(lines[j].strip())
        if m:
            return m.group(1)
    for j in range(idx, max(-1, idx - 25), -1):
        m = decl_rx.match(lines[j].strip())
        if m:
            return m.group(1)
    return "(module-level)"


def expand_para_refs(line: str, default_sec: str) -> list[tuple[str, int]]:
    refs: list[tuple[str, int]] = []
    sec_markers: list[tuple[int, str]] = []
    for sm in re.finditer(
        r"(?:Attachment|Att)\s+([ABC])|Main\s+Body|APS\s*210\s+(?:para|paragraph)",
        line,
        re.IGNORECASE,
    ):
        g = sm.group(1)
        if g:
            sec_markers.append((sm.start(), f"Attachment {g.upper()}"))
        else:
            sec_markers.append((sm.start(), "Main Body"))

    def section_at(pos: int) -> str:
        cur = default_sec
        for mpos, msec in sec_markers:
            if mpos <= pos:
                cur = msec
            else:
                break
        return cur

    # Match ranges like "Paragraphs 15–20" or "paras 6-8" or single "para 25"
    for m in re.finditer(
        r"(?:paragraphs?|paras?)\s+(\d+)(?:\s*[–-]\s*(\d+))?", line, re.IGNORECASE
    ):
        start = int(m.group(1))
        end = int(m.group(2)) if m.group(2) else start
        if end >= start and (end - start) <= 25:
            sec = section_at(m.start())
            for n in range(start, end + 1):
                target_sec = sec
                if default_sec == "Attachment B" and sec == "Attachment B" and n >= 50:
                    target_sec = "Main Body"
                if (
                    default_sec == "Attachment C"
                    and sec == "Attachment C"
                    and n in (59, 60, 61)
                    and "Attachment C" not in line
                    and "Att C" not in line
                ):
                    target_sec = "Main Body"
                refs.append((target_sec, n))
    return refs


def collect_lean_mappings() -> dict[tuple[str, int], list[tuple[str, int, str]]]:
    mapping: dict[tuple[str, int], list[tuple[str, int, str]]] = {}
    for fpath in IMPL_FILES:
        rel = str(Path(fpath).relative_to(ROOT))
        default_sec = DEFAULT_SECTION.get(rel, "Main Body")
        lines = Path(fpath).read_text().splitlines()
        for i, line in enumerate(lines):
            if "--" in line or "/--" in line or "Para" in line or "para" in line:
                refs = expand_para_refs(line, default_sec)
                if refs:
                    sym = find_enclosing_symbol(lines, i)
                    for key in refs:
                        entries = mapping.setdefault(key, [])
                        entry = (rel, i + 1, sym)
                        if entry not in entries:
                            entries.append(entry)
    return mapping


def main() -> int:
    parser = ParaParser()
    parser.feed(APS210_HTML.read_text(encoding="utf-8", errors="ignore"))
    mapping = collect_lean_mappings()

    sections = ["Main Body", "Attachment A", "Attachment B", "Attachment C"]
    md: list[str] = [
        "# APS 210 Paragraph Traceability Matrix",
        "",
        "Generated from `aps-210.html` against `APS210/**/*.lean` (excluding `Tests.lean` and `Theorems.lean`).",
        "",
        "## Coverage Summary",
        "",
        "| Section | Total Paragraphs | Mapped to Lean | Unmapped | Coverage |",
        "| --- | ---: | ---: | ---: | ---: |",
    ]

    total_all = 0
    mapped_all = 0
    for sec in sections:
        sec_paras = [(n, txt) for s, n, txt in parser.paras if s == sec]
        sec_mapped = sum(1 for n, _ in sec_paras if (sec, n) in mapping)
        total_all += len(sec_paras)
        mapped_all += sec_mapped
        pct = (100.0 * sec_mapped / len(sec_paras)) if sec_paras else 0.0
        md.append(
            f"| {sec} | {len(sec_paras)} | {sec_mapped} | {len(sec_paras) - sec_mapped} | {pct:.1f}% |"
        )
        print(f"{sec:14s}: {sec_mapped:3d}/{len(sec_paras):3d} mapped ({pct:.1f}%)")

    pct_all = (100.0 * mapped_all / total_all) if total_all else 0.0
    md.append(
        f"| **Total** | **{total_all}** | **{mapped_all}** | **{total_all - mapped_all}** | **{pct_all:.1f}%** |"
    )
    print(f"{'Total':14s}: {mapped_all:3d}/{total_all:3d} mapped ({pct_all:.1f}%)")

    for sec in sections:
        md.extend(
            [
                "",
                f"## {sec}",
                "",
                "| Para | APS 210 Summary | Lean Mapping |",
                "| ---: | --- | --- |",
            ]
        )
        for s, n, txt in parser.paras:
            if s != sec:
                continue
            summary = txt[:110].replace("|", "\\|") + ("..." if len(txt) > 110 else "")
            hits = mapping.get((sec, n), [])
            if hits:
                locs = "; ".join(f"`{sym}` (`{rel}:{ln}`)" for rel, ln, sym in hits[:3])
                if len(hits) > 3:
                    locs += f" (+{len(hits) - 3} more)"
            else:
                locs = "**UNMAPPED**"
            md.append(f"| {n} | {summary} | {locs} |")

    OUT_MD.write_text("\n".join(md) + "\n", encoding="utf-8")
    print(f"Wrote {OUT_MD.relative_to(ROOT)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

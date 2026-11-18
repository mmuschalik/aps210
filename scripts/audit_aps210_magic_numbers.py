#!/usr/bin/env python3
"""Audit APS 210 magic numbers and paragraph/table coverage against Lean implementation."""

from __future__ import annotations

import glob
import re
from html.parser import HTMLParser
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
APS210_HTML = ROOT / "aps-210.html"

IMPL_FILES = sorted(
    p
    for p in glob.glob(str(ROOT / "APS210/**/*.lean"), recursive=True)
    if not p.endswith(("Tests.lean", "Theorems.lean"))
)


class _TextExtractor(HTMLParser):
    def __init__(self) -> None:
        super().__init__()
        self.parts: list[str] = []
        self.skip = False

    def handle_starttag(self, tag: str, attrs: list[tuple[str, str | None]]) -> None:
        if tag in ("script", "style"):
            self.skip = True
        if tag in ("p", "div", "li", "tr", "h1", "h2", "h3", "h4", "td", "th"):
            self.parts.append("\n")

    def handle_endtag(self, tag: str) -> None:
        if tag in ("script", "style"):
            self.skip = False

    def handle_data(self, data: str) -> None:
        if not self.skip:
            self.parts.append(data)


def extract_aps210_body() -> str:
    parser = _TextExtractor()
    parser.feed(APS210_HTML.read_text(encoding="utf-8", errors="ignore"))
    raw = "".join(parser.parts)
    lines = [re.sub(r"\s+", " ", ln).strip() for ln in raw.splitlines()]
    lines = [ln for ln in lines if ln]
    start = next(
        i
        for i, ln in enumerate(lines)
        if "Prudential Standard APS 210" in ln and i > 50
    )
    end = next(
        (i for i, ln in enumerate(lines) if i > start and ln.startswith("APG 210")),
        len(lines),
    )
    return "\n".join(lines[start:end])


def scan_lean_pcts() -> dict[int, list[tuple[str, int]]]:
    found: dict[int, list[tuple[str, int]]] = {}
    for fpath in IMPL_FILES:
        rel = str(Path(fpath).relative_to(ROOT))
        for lineno, line in enumerate(Path(fpath).read_text().splitlines(), 1):
            code = line.split("--")[0]
            for m in re.finditer(r"\bpct\s+(\d+)\b", code):
                val = int(m.group(1))
                found.setdefault(val, []).append((rel, lineno))
            for m in re.finditer(
                r"\b(?:aps112RiskWeightPct|lvrPct)\s*<=\s*(\d+)\b", code
            ):
                val = int(m.group(1))
                found.setdefault(val, []).append((rel, lineno))
    return found


def scan_lean_token(pattern: str) -> list[tuple[str, int, str]]:
    hits: list[tuple[str, int, str]] = []
    rx = re.compile(pattern)
    for fpath in IMPL_FILES:
        rel = str(Path(fpath).relative_to(ROOT))
        for lineno, line in enumerate(Path(fpath).read_text().splitlines(), 1):
            if rx.search(line):
                hits.append((rel, lineno, line.strip()))
    return hits


def main() -> int:
    body = extract_aps210_body()
    lean_pcts = scan_lean_pcts()

    # Percentages in APS 210 prose ("X per cent" and spelled-out)
    spelled_map = {"zero": 0, "five": 5, "nine": 9}
    std_pcts: set[int] = set()
    for m in re.finditer(r"\b(\d+)\s+per\s*cent\b", body, re.I):
        std_pcts.add(int(m.group(1)))
    for word, val in spelled_map.items():
        if re.search(rf"\b{word}\s+per\s*cent\b", body, re.I):
            std_pcts.add(val)

    # Table-only rates in APS 210 (Attachment A Table 3 & Attachment C Table 1)
    for table_rate, anchor in [
        (1, r"Unconditionally revocable credit and liquidity facilities"),
        (30, r"30\s+liquidity facilities"),
    ]:
        if re.search(anchor, body, re.I):
            std_pcts.add(table_rate)

    print("=== 1. PERCENTAGE CONSTANTS (APS 210 vs Lean `pct N`) ===")
    missing_pcts = sorted(std_pcts - set(lean_pcts.keys()))
    extra_pcts = sorted(set(lean_pcts.keys()) - std_pcts)
    for val in sorted(std_pcts | set(lean_pcts.keys())):
        locs = ", ".join(f"{f}:{ln}" for f, ln in lean_pcts.get(val, [])[:3])
        if len(lean_pcts.get(val, [])) > 3:
            locs += f" (+{len(lean_pcts[val]) - 3} more)"
        status = (
            "MATCH"
            if val in std_pcts and val in lean_pcts
            else ("ONLY_STD" if val in std_pcts else "ONLY_LEAN")
        )
        print(f"  {val:3d}%  [{status:9s}]  {locs}")

    print("\n=== 2. STRUCTURAL & THRESHOLD MAGIC NUMBERS ===")
    checks = [
        ("HQLA2B cap 15/85", r"15\s*:\s*Rat\)\s*/\s*85", "ENFORCED"),
        ("HQLA2 cap 2/3 (two-thirds)", r"2\s*:\s*Rat\)\s*/\s*3", "ENFORCED"),
        (
            "APS 112 RW <= 35% (NSFR 65% RSF)",
            r"aps112RiskWeightPct\s*<=\s*35",
            "ENFORCED",
        ),
        ("APS 112 LVR <= 80% (NSFR 65% RSF)", r"lvrPct\s*<=\s*80", "ENFORCED"),
        (
            "Foreign outage >= 3 business days",
            r"offshoreOutageMinBusinessDays\s*>=\s*3",
            "ENFORCED",
        ),
        ("Scorecard <= 2 (10%) vs > 2 (25%)", r"score\s*<=\s*2", "ENFORCED"),
        (
            "30-day HQLA downgrade grace period",
            r"becameIneligibleWithin30Days",
            "ABSTRACTED_BOOL",
        ),
        ("MLH 2-business-day liquidation", r"two business days", "ABSTRACTED_BOOL"),
        (
            "LCR 24-month collateral lookback",
            r"derivativeValuationLargest30dFlow24m",
            "ABSTRACTED_INPUT",
        ),
    ]
    for label, pat, kind in checks:
        hits = scan_lean_token(pat)
        loc = f"{hits[0][0]}:{hits[0][1]}" if hits else "NOT FOUND"
        print(f"  [{kind:16s}] {label:38s} -> {loc}")

    print("\nSummary:")
    print(
        f"  Standard percentages: {len(std_pcts)}, Lean `pct` values: {len(lean_pcts)}"
    )
    print(f"  Missing in Lean: {missing_pcts}, Extra in Lean: {extra_pcts}")
    return 0 if not missing_pcts else 1


if __name__ == "__main__":
    raise SystemExit(main())

---
name: differential-testing-aps210
description: >-
  Run three-layer differential testing (Domain Lean <-> Query IR in Lean <->
  Generated Ibis in DuckDB) whenever fixing issues or verifying equivalence in
  the APS 210 codebase.
---

# Differential Testing for APS 210

Verify equivalence across three layers:

1. **Domain Lean** (`APS210/*`) ↔ **Query IR in Lean** (`Lean2Ibis/*Spec`) — isolates specification drift.
2. **Query IR in Lean** ↔ **Generated Ibis in DuckDB** (`generated/*.py`) — isolates emission, DuckDB semantics, null handling, and decimal precision.

## Five-Part Workflow

1. **Compare branches and input fields:** Diff domain functions against IR `SExpr`/`RelExpr` definitions to spot missing conditions, unsupported fields, or mismatched defaults.
2. **Enumerate Boolean combinations:** Test interacting Boolean flags (e.g., `isIntermediated` + `!intermediatedLookThroughMet` + `noticeDepositCalled` + term-deposit exclusion).
3. **Test numerical and date boundaries:** Probe exact thresholds (`$250k` FCS split, `30` days, calendar `6m` and `1y`, `15%`/`40%` HQLA caps, `75%` inflow cap, `50%` HO facility cap, ratio thresholds).
4. **Generate multi-row portfolios:** Include empty portfolios, duplicate dates, missing dates, multiple currencies, shared CLF/FCS limits, and heterogeneous `excessClfFallbackFactor` overrides.
5. **Shrink failures & lock in regressions:** Supply all fields explicitly via canonical adapters (not minimal rows), shrink failing inputs to the smallest counterexample, add permanent regression tests in `APS210/Tests.lean` and `scripts/verify_*.py`, and pursue universal equivalence proofs where practical.

# APS 210 Workspace Rules

## Mandatory Differential Testing on Fixes
Whenever fixing a bug or changing calculation/query logic in `APS210/`, `Lean2Ibis/`, `Lean2Sql/`, or `generated/`:
- Spawn a dedicated differential-testing subagent (`differential-testing-aps210` skill) to independently test the change across all three layers:
  1. Domain Lean (`APS210/*`)
  2. Query IR evaluated in Lean (`Lean2Ibis/*Spec`)
  3. Generated Ibis executed in DuckDB (`generated/*.py`)
- Use shared canonical adapters supplying **every required field explicitly** (never rely on missing-field defaults).
- Shrink any failing counterexample to its minimal reproducible input and add it as a permanent regression test before reporting the fix complete.

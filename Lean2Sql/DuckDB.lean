import Lean2Ibis.IR

namespace Lean2Sql.DuckDB

open Lean2Ibis

/-- DuckDB SQL runtime value domain, including SQL `NULL` (3-valued logic). -/
inductive SqlVal where
  | null
  | num (r : Rat)
  | bool (b : Bool)
  | str (s : String)
  | date (d : Int)
  deriving DecidableEq, Repr

/-- Embedding of non-null Lean IR `Val` into `SqlVal`. -/
def valToSql : Val → SqlVal
  | .rat r => .num r
  | .bool b => .bool b
  | .str s => .str s
  | .date d => .date d

abbrev SqlRow := List (String × SqlVal)

def SqlRow.get (row : SqlRow) (name : String) : SqlVal :=
  match row.find? (fun (k, _) => k == name) with
  | some (_, v) => v
  | none => .null

/-- SQL binary operators (subset used by APS 210). -/
inductive SqlBinOp where
  | add | sub | mul | div
  | least | greatest
  | eq | le | lt | gt
  | and | or
  deriving DecidableEq, Repr

/-- DuckDB SQL scalar expression AST. -/
inductive SqlExpr where
  | col (name : String)
  | litNum (n : Int) (d : Nat)
  | litBool (b : Bool)
  | litStr (s : String)
  | not (e : SqlExpr)
  | greatestZero (e : SqlExpr)
  | coalesceZero (e : SqlExpr)
  | bin (op : SqlBinOp) (l r : SqlExpr)
  | caseWhen (branches : List (SqlExpr × SqlExpr)) (elseE : SqlExpr)
  deriving Repr

/-- SQL 3-valued NOT. -/
def sqlNot : SqlVal → SqlVal
  | .bool b => .bool (!b)
  | _ => .null

/-- SQL 3-valued AND. -/
def sqlAnd : SqlVal → SqlVal → SqlVal
  | .bool false, _ => .bool false
  | _, .bool false => .bool false
  | .bool true, .bool true => .bool true
  | _, _ => .null

/-- SQL 3-valued OR. -/
def sqlOr : SqlVal → SqlVal → SqlVal
  | .bool true, _ => .bool true
  | _, .bool true => .bool true
  | .bool false, .bool false => .bool false
  | _, _ => .null

/-- Strict SQL binary evaluation (propagates `NULL` for arithmetic/comparisons). -/
def evalSqlBinOp (op : SqlBinOp) (lv rv : SqlVal) : SqlVal :=
  match op with
  | .and => sqlAnd lv rv
  | .or => sqlOr lv rv
  | .add => match lv, rv with | .num a, .num b => .num (a + b) | _, _ => .null
  | .sub => match lv, rv with | .num a, .num b => .num (a - b) | _, _ => .null
  | .mul => match lv, rv with | .num a, .num b => .num (a * b) | _, _ => .null
  | .div => match lv, rv with
    | .num a, .num b => if b == 0 then .null else .num (a / b)
    | _, _ => .null
  | .least => match lv, rv with | .num a, .num b => .num (min a b) | _, _ => .null
  | .greatest => match lv, rv with | .num a, .num b => .num (max a b) | _, _ => .null
  | .eq => match lv, rv with
    | .null, _ => .null
    | _, .null => .null
    | a, b => .bool (a == b)
  | .le => match lv, rv with | .num a, .num b => .bool (a <= b) | _, _ => .null
  | .lt => match lv, rv with | .num a, .num b => .bool (a < b) | _, _ => .null
  | .gt => match lv, rv with | .num a, .num b => .bool (a > b) | _, _ => .null

mutual
def evalSqlExpr (env : SqlRow) : SqlExpr → SqlVal
  | .col name => env.get name
  | .litNum n d => .num (if d == 0 then 0 else (n : Rat) / (d : Rat))
  | .litBool b => .bool b
  | .litStr s => .str s
  | .not e => sqlNot (evalSqlExpr env e)
  | .greatestZero e =>
    match evalSqlExpr env e with
    | .num r => .num (max r 0)
    | _ => .null
  | .coalesceZero e =>
    match evalSqlExpr env e with
    | .null => .num 0
    | v => v
  | .bin op l r => evalSqlBinOp op (evalSqlExpr env l) (evalSqlExpr env r)
  | .caseWhen branches dflt => evalSqlCase env branches dflt
  termination_by e => sizeOf e

def evalSqlCase (env : SqlRow) : List (SqlExpr × SqlExpr) → SqlExpr → SqlVal
  | [], dflt => evalSqlExpr env dflt
  | (c, v) :: rest, dflt =>
    match evalSqlExpr env c with
    | .bool true => evalSqlExpr env v
    | _ => evalSqlCase env rest dflt
  termination_by bs dflt => sizeOf bs + sizeOf dflt
end

/-- DuckDB aggregate query: `SELECT COALESCE(SUM(expr), 0) FROM table WHERE pred`. -/
structure SqlSumQuery where
  fromTable : String
  wherePred? : Option SqlExpr
  sumExpr : SqlExpr
  deriving Repr

inductive SqlLetRhs where
  | sumQuery (q : SqlSumQuery)
  | scalar (e : SqlExpr)
  deriving Repr

structure SqlProgram where
  name : String
  lets : List (String × SqlLetRhs)
  outputs : List (String × SqlExpr)
  deriving Repr

abbrev SqlTableEnv := List (String × List SqlRow)

def SqlTableEnv.get (tenv : SqlTableEnv) (name : String) : List SqlRow :=
  match tenv.find? (fun (k, _) => k == name) with
  | some (_, rows) => rows
  | none => []

/-- SQL `SUM` over a list of `SqlVal`: returns `.null` on empty, otherwise sums `.num` values. -/
def sqlSumVals : List SqlVal → SqlVal
  | [] => .null
  | xs => .num (xs.foldl (fun acc v => match v with | .num r => acc + r | _ => acc) 0)

def evalSqlSumQuery (tenv : SqlTableEnv) (scalarEnv : SqlRow) (q : SqlSumQuery) : SqlVal :=
  let rows := tenv.get q.fromTable
  let filtered := match q.wherePred? with
    | none => rows
    | some p => rows.filter (fun r => evalSqlExpr (r ++ scalarEnv) p == SqlVal.bool true)
  let vals := filtered.map (fun r => evalSqlExpr (r ++ scalarEnv) q.sumExpr)
  match sqlSumVals vals with
  | .null => .num 0
  | v => v

def evalSqlProgram (prog : SqlProgram) (tenv : SqlTableEnv) (initialScalars : SqlRow) : SqlRow :=
  let finalEnv := prog.lets.foldl (fun env (varName, rhs) =>
    let v := match rhs with
      | .sumQuery q => evalSqlSumQuery tenv env q
      | .scalar e => evalSqlExpr env e
    (varName, v) :: env
  ) initialScalars
  prog.outputs.map (fun (k, e) => (k, evalSqlExpr finalEnv e))

-- ============================================================================
-- Verified Compiler: Lean2Ibis.IR -> DuckDB SqlProgram
-- ============================================================================

def compileBinOp : BinOp → SqlBinOp
  | .add => .add | .sub => .sub | .mul => .mul | .div => .div
  | .min => .least | .max => .greatest
  | .eq => .eq | .ne => .eq | .le => .le | .lt => .lt | .gt => .gt | .ge => .le
  | .and => .and | .or => .or | .dateDiff => .sub

mutual
def compileSExpr : SExpr → SqlExpr
  | .col name => .col name
  | .var name => .col name
  | .litRat n d => .litNum n d
  | .litBool b => .litBool b
  | .litStr s => .litStr s
  | .litDate y m d => .litNum (daysFromCivil y m d) 1
  | .dateAddMonths d m => .bin .add (compileSExpr d) (.litNum (m * 30) 1)
  | .unOp .not e => .not (compileSExpr e)
  | .unOp .nonNeg e => .greatestZero (compileSExpr e)
  | .unOp .abs e =>
    let ce := compileSExpr e
    .caseWhen [(.bin .lt ce (.litNum 0 1), .bin .sub (.litNum 0 1) ce)] ce
  | .binOp .div l r =>
    let cl := compileSExpr l
    let cr := compileSExpr r
    .caseWhen [(.bin .eq cr (.litNum 0 1), .litNum 0 1)] (.bin .div cl cr)
  | .binOp .ne l r => .not (.bin .eq (compileSExpr l) (compileSExpr r))
  | .binOp .ge l r => .bin .le (compileSExpr r) (compileSExpr l)
  | .binOp op l r => .bin (compileBinOp op) (compileSExpr l) (compileSExpr r)
  | .ite c t f => .caseWhen [(compileSExpr c, compileSExpr t)] (compileSExpr f)
  | .caseStr scrut branches dflt =>
    let cs := compileSExpr scrut
    .caseWhen (compileCaseBranches cs branches) (compileSExpr dflt)
  termination_by e => sizeOf e

def compileCaseBranches (cs : SqlExpr) : List (String × SExpr) → List (SqlExpr × SqlExpr)
  | [] => []
  | (k, rhs) :: rest => (.bin .eq cs (.litStr k), compileSExpr rhs) :: compileCaseBranches cs rest
  termination_by bs => sizeOf bs
end

/-- Flatten nested `.filter` / `.table` into `(tableName, combinedWherePred?)`. -/
def compileRelToWhere : RelExpr → Option (String × Option SqlExpr)
  | .table name => some (name, none)
  | .filter pred input =>
    match compileRelToWhere input with
    | some (tbl, none) => some (tbl, some (compileSExpr pred))
    | some (tbl, some p0) => some (tbl, some (.bin .and p0 (compileSExpr pred)))
    | none => none
  | .withColumns _ _ => none
  | .groupBy _ _ _ => none
  | .window _ _ => none

def compileProgram (prog : Program) : Option SqlProgram := do
  let lets ← prog.lets.mapM (fun (varName, rhs) =>
    match rhs with
    | .aggSum rel expr => do
      let (tbl, pred?) ← compileRelToWhere rel
      pure (varName, .sumQuery { fromTable := tbl, wherePred? := pred?, sumExpr := compileSExpr expr })
    | .agg .sum rel expr => do
      let (tbl, pred?) ← compileRelToWhere rel
      pure (varName, .sumQuery { fromTable := tbl, wherePred? := pred?, sumExpr := compileSExpr expr })
    | .agg _ _ _ => none
    | .scalar expr =>
      pure (varName, .scalar (compileSExpr expr)))
  let outputs := prog.outputs.map (fun (k, e) => (k, compileSExpr e))
  pure { name := prog.name, lets, outputs }

-- ============================================================================
-- Deterministic DuckDB SQL Emitter
-- ============================================================================

def emitSqlRat (n : Int) (d : Nat) : String :=
  if d == 1 then s!"CAST({n} AS DECIMAL(38, 12))"
  else s!"(CAST({n} AS DECIMAL(38, 12)) / CAST({d} AS DECIMAL(38, 12)))"

mutual
def emitSqlExpr : SqlExpr → String
  | .col name => name
  | .litNum n d => emitSqlRat n d
  | .litBool b => if b then "TRUE" else "FALSE"
  | .litStr s => s!"'{s}'"
  | .not e => s!"(NOT ({emitSqlExpr e}))"
  | .greatestZero e => s!"GREATEST({emitSqlExpr e}, CAST(0 AS DECIMAL(38, 12)))"
  | .coalesceZero e => s!"COALESCE({emitSqlExpr e}, CAST(0 AS DECIMAL(38, 12)))"
  | .bin op l r =>
    let ls := emitSqlExpr l
    let rs := emitSqlExpr r
    match op with
    | .add => s!"({ls} + {rs})"
    | .sub => s!"({ls} - {rs})"
    | .mul => s!"({ls} * {rs})"
    | .div => s!"({ls} / {rs})"
    | .least => s!"LEAST({ls}, {rs})"
    | .greatest => s!"GREATEST({ls}, {rs})"
    | .eq => s!"({ls} = {rs})"
    | .le => s!"({ls} <= {rs})"
    | .lt => s!"({ls} < {rs})"
    | .gt => s!"({ls} > {rs})"
    | .and => s!"({ls} AND {rs})"
    | .or => s!"({ls} OR {rs})"
  | .caseWhen branches dflt =>
    let whens := emitSqlWhens branches
    s!"CASE {whens}ELSE {emitSqlExpr dflt} END"
  termination_by e => sizeOf e

def emitSqlWhens : List (SqlExpr × SqlExpr) → String
  | [] => ""
  | (c, v) :: rest => s!"WHEN {emitSqlExpr c} THEN {emitSqlExpr v} " ++ emitSqlWhens rest
  termination_by bs => sizeOf bs
end

def emitDuckDbSql (prog : SqlProgram) (scalarInputs : List (String × String)) : String :=
  let paramCols := scalarInputs.map (fun (k, sqlLit) => s!"{sqlLit} AS {k}")
  let paramCte := s!"_params AS (SELECT {String.intercalate ", " paramCols})"
  let (_, ctes) := prog.lets.foldl (fun (prevCte, acc) (varName, rhs) =>
    let body := match rhs with
      | .sumQuery q =>
        let whereClause := match q.wherePred? with
          | some p => s!" WHERE {emitSqlExpr p}"
          | none => ""
        s!"SELECT COALESCE(SUM({emitSqlExpr q.sumExpr}), CAST(0 AS DECIMAL(38, 12))) AS {varName} FROM {q.fromTable}, {prevCte}{whereClause}"
      | .scalar e =>
        s!"SELECT {emitSqlExpr e} AS {varName} FROM {prevCte}"
    let cteName := s!"_cte_{varName}"
    let fullCte := s!"{cteName} AS (SELECT {prevCte}.*, ({body}) AS {varName} FROM {prevCte})"
    (cteName, acc ++ [fullCte])
  ) ("_params", [paramCte])
  let finalCte := match prog.lets.reverse with
    | (lastVar, _) :: _ => s!"_cte_{lastVar}"
    | [] => "_params"
  let outCols := prog.outputs.map (fun (k, e) => s!"{emitSqlExpr e} AS {k}")
  s!"-- AUTO-GENERATED BY Lean2Sql.DuckDB — DO NOT EDIT\nWITH\n  {String.intercalate ",\n  " ctes}\nSELECT {String.intercalate ", " outCols}\nFROM {finalCte};\n"

end Lean2Sql.DuckDB

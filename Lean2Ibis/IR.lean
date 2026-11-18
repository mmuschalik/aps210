import APS210.Basic

namespace Lean2Ibis

/-- Convert Gregorian `(year, month, day)` to days since 1970-01-01 (Howard Hinnant algorithm). -/
def daysFromCivil (y : Int) (m d : Nat) : Int :=
  let mInt : Int := m
  let yAdj : Int := if mInt ≤ 2 then y - 1 else y
  let era : Int := (if yAdj ≥ 0 then yAdj else yAdj - 399) / 400
  let yoe : Int := yAdj - era * 400
  let mAdj : Int := if mInt > 2 then mInt - 3 else mInt + 9
  let doy : Int := (153 * mAdj + 2) / 5 + (d : Int) - 1
  let doe : Int := yoe * 365 + yoe / 4 - yoe / 100 + doy
  era * 146097 + doe - 719468

/-- Inverse of `daysFromCivil`: convert days since 1970-01-01 to `(year, month, day)`. -/
def civilFromDays (z : Int) : Int × Nat × Nat :=
  let z1 := z + 719468
  let era := (if z1 ≥ 0 then z1 else z1 - 146096) / 146097
  let doe := (z1 - era * 146097).toNat
  let yoe := (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365
  let y := (yoe : Int) + era * 400
  let doy := doe - (365 * yoe + yoe / 4 - yoe / 100)
  let mp := (5 * doy + 2) / 153
  let d := doy - (153 * mp + 2) / 5 + 1
  let m := if mp < 10 then mp + 3 else mp - 9
  let yFinal := if m ≤ 2 then y + 1 else y
  (yFinal, m, d)

private def isLeapYear (y : Int) : Bool :=
  (y % 4 == 0 && y % 100 != 0) || (y % 400 == 0)

private def daysInMonth (y : Int) (m : Nat) : Nat :=
  match m with
  | 1 | 3 | 5 | 7 | 8 | 10 | 12 => 31
  | 4 | 6 | 9 | 11 => 30
  | 2 => if isLeapYear y then 29 else 28
  | _ => 30

/-- Add `deltaMonths` calendar months to `epochDays`, clamping day to month-end. -/
def addMonthsToEpoch (epochDays : Int) (deltaMonths : Int) : Int :=
  let (y, m, d) := civilFromDays epochDays
  let totalMonths : Int := (y * 12) + ((m : Int) - 1) + deltaMonths
  let newY : Int := if totalMonths ≥ 0 then totalMonths / 12 else (totalMonths - 11) / 12
  let newM0 : Int := totalMonths - newY * 12
  let newM : Nat := newM0.toNat + 1
  let newD : Nat := min d (daysInMonth newY newM)
  daysFromCivil newY newM newD

/-- Runtime value domain for the Lean-to-Ibis IR. -/
inductive Val where
  | rat (r : Rat)
  | bool (b : Bool)
  | str (s : String)
  | date (epochDays : Int)
  deriving DecidableEq, Repr

namespace Val

def asRat : Val → Rat
  | .rat r => r
  | _ => 0

def asBool : Val → Bool
  | .bool b => b
  | _ => false

def asStr : Val → String
  | .str s => s
  | _ => ""

def asDate : Val → Int
  | .date d => d
  | _ => 0

end Val

/-- A row is an association list from column name to `Val`. -/
abbrev Row := List (String × Val)

def Row.get (row : Row) (name : String) : Val :=
  match row.find? (fun (k, _) => k == name) with
  | some (_, v) => v
  | none => .rat 0

/-- Unary operators in the scalar IR. -/
inductive UnOp where
  | not
  | nonNeg
  | abs
  deriving DecidableEq, Repr

/-- Binary operators in the scalar IR. -/
inductive BinOp where
  | add
  | sub
  | mul
  | div
  | min
  | max
  | eq
  | ne
  | le
  | lt
  | gt
  | ge
  | and
  | or
  | dateDiff
  deriving DecidableEq, Repr

/-- Scalar expression AST (compiles directly to Ibis column / scalar expressions). -/
inductive SExpr where
  | col (name : String)
  | var (name : String)
  | litRat (num : Int) (den : Nat)
  | litBool (b : Bool)
  | litStr (s : String)
  | litDate (y : Int) (m d : Nat)
  | dateAddMonths (d : SExpr) (months : Int)
  | yearMonth (d : SExpr)
  | unOp (op : UnOp) (e : SExpr)
  | binOp (op : BinOp) (l r : SExpr)
  | ite (cond thenE elseE : SExpr)
  | caseStr (scrut : SExpr) (branches : List (String × SExpr)) (defaultE : SExpr)
  deriving Repr

mutual
def evalSExpr (env : Row) : SExpr → Val
  | .col name => env.get name
  | .var name => env.get name
  | .litRat n d => .rat (if d == 0 then 0 else (n : Rat) / (d : Rat))
  | .litBool b => .bool b
  | .litStr s => .str s
  | .litDate y m d => .date (daysFromCivil y m d)
  | .dateAddMonths d m => .date (addMonthsToEpoch (evalSExpr env d).asDate m)
  | .yearMonth d =>
    let (y, m, _) := civilFromDays (evalSExpr env d).asDate
    .rat (y * 100 + (m : Int))
  | .unOp op e =>
    let v := evalSExpr env e
    match op with
    | .not => .bool (!v.asBool)
    | .nonNeg => .rat (APS210.nonNeg v.asRat)
    | .abs => .rat (if v.asRat < 0 then -v.asRat else v.asRat)
  | .binOp op l r =>
    let lv := evalSExpr env l
    let rv := evalSExpr env r
    match op with
    | .add => .rat (lv.asRat + rv.asRat)
    | .sub => .rat (lv.asRat - rv.asRat)
    | .mul => .rat (lv.asRat * rv.asRat)
    | .div => .rat (if rv.asRat == 0 then 0 else lv.asRat / rv.asRat)
    | .min => .rat (min lv.asRat rv.asRat)
    | .max => .rat (max lv.asRat rv.asRat)
    | .eq => .bool (lv == rv)
    | .ne => .bool (lv != rv)
    | .le => .bool (lv.asRat <= rv.asRat)
    | .lt => .bool (lv.asRat < rv.asRat)
    | .gt => .bool (lv.asRat > rv.asRat)
    | .ge => .bool (lv.asRat >= rv.asRat)
    | .and => .bool (lv.asBool && rv.asBool)
    | .or => .bool (lv.asBool || rv.asBool)
    | .dateDiff => .rat ((lv.asDate - rv.asDate : Int) : Rat)
  | .ite c t f =>
    if (evalSExpr env c).asBool then evalSExpr env t else evalSExpr env f
  | .caseStr scrut branches dflt =>
    let sv := (evalSExpr env scrut).asStr
    evalCaseStr env sv branches dflt
  termination_by e => sizeOf e

def evalCaseStr (env : Row) (sv : String) : List (String × SExpr) → SExpr → Val
  | [], dflt => evalSExpr env dflt
  | (k, rhs) :: rest, dflt =>
    if k == sv then evalSExpr env rhs else evalCaseStr env sv rest dflt
  termination_by bs dflt => sizeOf bs + sizeOf dflt
end

/-- Aggregate operators for `groupBy`, `window`, and scalar reductions. -/
inductive AggOp where
  | sum
  | min
  | max
  | avg
  | count
  | countDistinct
  | anyBool
  | allBool
  | rowNumber
  deriving DecidableEq, Repr

def evalAggOp (op : AggOp) (vals : List Val) : Val :=
  match op with
  | .sum => .rat (vals.foldl (fun acc v => acc + v.asRat) 0)
  | .min =>
    match vals with
    | [] => .rat 0
    | v :: rest => .rat (rest.foldl (fun acc x => min acc x.asRat) v.asRat)
  | .max =>
    match vals with
    | [] => .rat 0
    | v :: rest => .rat (rest.foldl (fun acc x => max acc x.asRat) v.asRat)
  | .avg =>
    if vals.isEmpty then .rat 0
    else .rat (vals.foldl (fun acc v => acc + v.asRat) 0 / (vals.length : Rat))
  | .count => .rat (vals.length : Rat)
  | .countDistinct => .rat (vals.eraseDups.length : Rat)
  | .anyBool => .bool (vals.any Val.asBool)
  | .allBool => .bool (vals.all Val.asBool)
  | .rowNumber => .rat (vals.length : Rat)

/-- Window specification matching SQL / Ibis `ROWS BETWEEN` or calendar-day `RANGE` semantics. -/
structure WindowSpec where
  partitionBy : List String
  orderBy : List (String × Bool) := []
  preceding? : Option Nat := none
  following? : Option Nat := none
  precedingDays? : Option Nat := none
  deriving DecidableEq, Repr

/-- Relational query AST (compiles directly to Ibis `Table` pipelines). -/
inductive RelExpr where
  | table (name : String)
  | filter (pred : SExpr) (input : RelExpr)
  | withColumns (cols : List (String × SExpr)) (input : RelExpr)
  | groupBy (keys : List String) (aggs : List (String × AggOp × SExpr)) (input : RelExpr)
  | window (cols : List (String × AggOp × SExpr × WindowSpec)) (input : RelExpr)
  deriving Repr

/-- Table environment mapping table names to lists of rows. -/
abbrev TableEnv := List (String × List Row)

def TableEnv.get (tenv : TableEnv) (name : String) : List Row :=
  match tenv.find? (fun (k, _) => k == name) with
  | some (_, rows) => rows
  | none => []

private def valLt : Val → Val → Bool
  | .rat a, .rat b => a < b
  | .date a, .date b => a < b
  | .str a, .str b => a < b
  | .bool false, .bool true => true
  | _, _ => false

private def rowLtBy (orderBy : List (String × Bool)) (r1 r2 : Row) : Bool :=
  orderBy.foldr (fun (col, asc) rest =>
    let v1 := r1.get col
    let v2 := r2.get col
    if v1 == v2 then rest
    else if asc then valLt v1 v2 else valLt v2 v1
  ) false

private def insertRowGroup (keyVals : List Val) (row : Row) :
    List (List Val × List Row) → List (List Val × List Row)
  | [] => [(keyVals, [row])]
  | (kv, rs) :: rest =>
    if kv == keyVals then (kv, rs ++ [row]) :: rest
    else (kv, rs) :: insertRowGroup keyVals row rest

private def groupRowsBy (keys : List String) (rows : List Row) : List (List Val × List Row) :=
  rows.foldl (fun acc r =>
    let kvs := keys.map (fun k => r.get k)
    insertRowGroup kvs r acc) []

private def listSlice {α : Type} (xs : List α) (lo hi : Nat) : List α :=
  (xs.drop lo).take (hi - lo + 1)

private def sortByList {α : Type} (lt : α → α → Bool) : List α → List α :=
  List.mergeSort (le := fun a b => !lt b a)

private def evalWindowCol (rows : List Row) (scalarEnv : Row)
    (outCol : String) (op : AggOp) (expr : SExpr) (spec : WindowSpec) : List Row :=
  if spec.orderBy.isEmpty then
    rows.map (fun r =>
      let kvs := spec.partitionBy.map (fun k => Row.get r k)
      let part := rows.filter (fun r' => spec.partitionBy.map (fun k => Row.get r' k) == kvs)
      let frame :=
        match spec.preceding?, spec.following? with
        | none, none => part
        | _, _ => part
      let v := evalAggOp op (frame.map (fun r' => evalSExpr (r' ++ scalarEnv) expr))
      (outCol, v) :: r)
  else
    let groups := groupRowsBy spec.partitionBy rows
    groups.flatMap (fun (_, part) =>
      let sorted := sortByList (rowLtBy spec.orderBy) part
      let n := sorted.length
      (List.range n).zip sorted |>.map (fun (i, r) =>
        let v :=
          if op == .rowNumber then
            .rat ((i + 1 : Nat) : Rat)
          else
            match spec.precedingDays?, spec.orderBy with
            | some pd, [(dateCol, true)] =>
              let d := (Row.get r dateCol).asDate
              let frame := sorted.filter (fun r' =>
                let d' := (Row.get r' dateCol).asDate
                d - (pd : Int) ≤ d' && d' ≤ d)
              evalAggOp op (frame.map (fun r' => evalSExpr (r' ++ scalarEnv) expr))
            | _, _ =>
              let lo := match spec.preceding? with | none => 0 | some p => i - p
              let hi := match spec.following? with | none => n - 1 | some f => min (n - 1) (i + f)
              let frame := listSlice sorted lo hi
              evalAggOp op (frame.map (fun r' => evalSExpr (r' ++ scalarEnv) expr))
        (outCol, v) :: r))

/-- Denotational semantics of `RelExpr` with ambient scalar environment. -/
def evalRelWithEnv (tenv : TableEnv) (scalarEnv : Row) : RelExpr → List Row
  | .table name => tenv.get name
  | .filter pred input =>
    (evalRelWithEnv tenv scalarEnv input).filter (fun row =>
      (evalSExpr (row ++ scalarEnv) pred).asBool)
  | .withColumns cols input =>
    (evalRelWithEnv tenv scalarEnv input).map (fun row =>
      cols.foldl (fun r (k, e) => (k, evalSExpr (r ++ scalarEnv) e) :: r) row)
  | .groupBy keys aggs input =>
    let rows := evalRelWithEnv tenv scalarEnv input
    let groups := groupRowsBy keys rows
    groups.map (fun (kvs, grpRows) =>
      let keyPairs := keys.zip kvs
      let aggPairs := aggs.map (fun (outCol, op, expr) =>
        (outCol, evalAggOp op (grpRows.map (fun r => evalSExpr (r ++ scalarEnv) expr))))
      keyPairs ++ aggPairs)
  | .window cols input =>
    let rows0 := evalRelWithEnv tenv scalarEnv input
    cols.foldl (fun rs (outCol, op, expr, spec) =>
      evalWindowCol rs scalarEnv outCol op expr spec) rows0

/-- Denotational semantics of `RelExpr` without ambient scalar bindings. -/
def evalRel (tenv : TableEnv) (r : RelExpr) : List Row :=
  evalRelWithEnv tenv [] r

/-- A step in a multi-stage scalar/relational program. -/
inductive LetRhs where
  | aggSum (rel : RelExpr) (expr : SExpr)
  | agg (op : AggOp) (rel : RelExpr) (expr : SExpr)
  | scalar (expr : SExpr)
  deriving Repr

inductive ScalarParamTy where
  | rat
  | bool
  | str
  | date
  deriving DecidableEq, Repr

/-- A complete deterministic program with table inputs, scalar bindings, and named outputs. -/
structure Program where
  name : String
  tableParams : List (String × List String)
  scalarParams : List (String × ScalarParamTy)
  lets : List (String × LetRhs)
  outputs : List (String × SExpr)
  deriving Repr

/-- Evaluate a `Program` in pure Lean. -/
def evalProgram (prog : Program) (tenv : TableEnv) (initialScalars : Row) : Row :=
  let finalEnv := prog.lets.foldl (fun env (varName, rhs) =>
    let v : Val := match rhs with
      | .aggSum rel expr =>
        let rows := evalRel tenv rel
        .rat (APS210.sumBy rows (fun r => (evalSExpr (r ++ env) expr).asRat))
      | .agg op rel expr =>
        let rows := evalRelWithEnv tenv env rel
        evalAggOp op (rows.map (fun r => evalSExpr (r ++ env) expr))
      | .scalar expr =>
        evalSExpr env expr
    (varName, v) :: env
  ) initialScalars
  prog.outputs.map (fun (outName, expr) => (outName, evalSExpr finalEnv expr))

end Lean2Ibis

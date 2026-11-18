import Lean2Ibis.IR

namespace Lean2Ibis

mutual
def emitSExpr (tableVar? : Option String) : SExpr → String
  | .col name =>
    match tableVar? with
    | some t => s!"{t}[\"{name}\"]"
    | none => name
  | .var name => name
  | .litRat n d =>
    if d == 1 then s!"ibis.literal({n}, type=\"decimal(38, 12)\")"
    else s!"(ibis.literal({n}, type=\"decimal(38, 12)\") / ibis.literal({d}, type=\"decimal(38, 12)\"))"
  | .litBool b => if b then "ibis.literal(True)" else "ibis.literal(False)"
  | .litStr s => s!"ibis.literal(\"{s}\")"
  | .litDate y m d => s!"ibis.date({y}, {m}, {d})"
  | .unOp .not e => s!"(~({emitSExpr tableVar? e}))"
  | .unOp .nonNeg e => s!"ibis.greatest({emitSExpr tableVar? e}, ibis.literal(0, type=\"decimal(38, 12)\"))"
  | .unOp .abs e => s!"({emitSExpr tableVar? e}).abs()"
  | .binOp op l r =>
    let ls := emitSExpr tableVar? l
    let rs := emitSExpr tableVar? r
    match op with
    | .add => s!"({ls} + {rs})"
    | .sub => s!"({ls} - {rs})"
    | .mul => s!"({ls} * {rs})"
    | .div => s!"ibis.ifelse({rs} == 0, ibis.literal(0, type=\"decimal(38, 12)\"), {ls} / {rs})"
    | .min => s!"ibis.least({ls}, {rs})"
    | .max => s!"ibis.greatest({ls}, {rs})"
    | .eq => s!"({ls} == {rs})"
    | .ne => s!"({ls} != {rs})"
    | .le => s!"({ls} <= {rs})"
    | .lt => s!"({ls} < {rs})"
    | .gt => s!"({ls} > {rs})"
    | .ge => s!"({ls} >= {rs})"
    | .and => s!"({ls} & {rs})"
    | .or => s!"({ls} | {rs})"
    | .dateDiff => s!"({ls}.delta({rs}, unit=\"day\").cast(\"decimal(38, 12)\"))"
  | .ite c t f =>
    s!"ibis.ifelse({emitSExpr tableVar? c}, {emitSExpr tableVar? t}, {emitSExpr tableVar? f})"
  | .caseStr scrut branches dflt =>
    let sc := emitSExpr tableVar? scrut
    let chain := emitCaseBranches tableVar? sc branches
    s!"ibis.cases({chain}else_={emitSExpr tableVar? dflt})"
  termination_by e => sizeOf e

def emitCaseBranches (tableVar? : Option String) (sc : String) : List (String × SExpr) → String
  | [] => ""
  | (k, rhs) :: rest =>
    s!"({sc} == \"{k}\", {emitSExpr tableVar? rhs}), " ++ emitCaseBranches tableVar? sc rest
  termination_by bs => sizeOf bs
end

def emitAggCall (op : AggOp) (exprStr : String) : String :=
  match op with
  | .sum => s!"({exprStr}).sum()"
  | .min => s!"({exprStr}).min()"
  | .max => s!"({exprStr}).max()"
  | .avg => s!"({exprStr}).mean().cast(\"decimal(38, 12)\")"
  | .count => s!"({exprStr}).count().cast(\"decimal(38, 12)\")"
  | .anyBool => s!"({exprStr}).any()"
  | .allBool => s!"({exprStr}).all()"

def emitAggDefault (op : AggOp) : String :=
  match op with
  | .anyBool => "ibis.literal(False)"
  | .allBool => "ibis.literal(True)"
  | _ => "ibis.literal(0, type=\"decimal(38, 12)\")"

def emitWindowSpec (spec : WindowSpec) : String :=
  let gbPart :=
    if spec.partitionBy.isEmpty then []
    else [s!"group_by=[{String.intercalate ", " (spec.partitionBy.map (fun k => s!"\"{k}\""))}]"]
  let obPart :=
    if spec.orderBy.isEmpty then []
    else
      let obItems := spec.orderBy.map (fun (c, asc) => if asc then s!"\"{c}\"" else s!"ibis.desc(\"{c}\")")
      [s!"order_by=[{String.intercalate ", " obItems}]"]
  let framePart :=
    if spec.orderBy.isEmpty then []
    else
      let prec := match spec.preceding? with | none => "None" | some p => toString p
      let foll := match spec.following? with | none => "None" | some f => toString f
      [s!"preceding={prec}", s!"following={foll}"]
  s!"ibis.window({String.intercalate ", " (gbPart ++ obPart ++ framePart)})"

/-- Emit a `RelExpr` as a deterministic Python/Ibis table pipeline. -/
def emitRel : RelExpr → String
  | .table name => name
  | .filter pred input =>
    let inp := emitRel input
    let p := emitSExpr (some "t") pred
    s!"({inp}).filter(lambda t: {p})"
  | .withColumns cols input =>
    let inp := emitRel input
    cols.foldl (fun acc (k, e) =>
      s!"({acc}).mutate({k}=lambda t: {emitSExpr (some "t") e})") inp
  | .groupBy keys aggs input =>
    let inp := emitRel input
    let keyList := String.intercalate ", " (keys.map (fun k => s!"\"{k}\""))
    let aggAssigns := aggs.map (fun (outCol, op, expr) =>
      let eStr := emitSExpr (some "t") expr
      s!"{outCol}=lambda t: ibis.coalesce({emitAggCall op eStr}, {emitAggDefault op})")
    s!"({inp}).group_by([{keyList}]).aggregate({String.intercalate ", " aggAssigns})"
  | .window cols input =>
    let inp := emitRel input
    let winAssigns := cols.map (fun (outCol, op, expr, spec) =>
      let eStr := emitSExpr (some "t") expr
      let wStr := emitWindowSpec spec
      s!"{outCol}=lambda t: ibis.coalesce({emitAggCall op eStr}.over({wStr}), {emitAggDefault op})")
    s!"({inp}).mutate({String.intercalate ", " winAssigns})"

/-- Emit a complete `Program` as a self-contained deterministic Python module using `ibis`. -/
def emitProgram (prog : Program) : String :=
  let tblArgs := prog.tableParams.map (·.1)
  let scalarNames := prog.scalarParams.map (·.1)
  let allArgs := tblArgs ++ scalarNames
  let header := String.intercalate "\n"
    [ "# AUTO-GENERATED BY Lean2Ibis — DO NOT EDIT MANUALLY"
    , "from __future__ import annotations"
    , "import ibis"
    , "from decimal import Decimal"
    , ""
    , s!"def {prog.name}({String.intercalate ", " allArgs}):"
    ]
  let scalarCasts := prog.scalarParams.map (fun (s, ty) =>
    match ty with
    | .rat => s!"    {s} = ibis.literal(Decimal(str({s})), type=\"decimal(38, 12)\")"
    | .bool => s!"    {s} = ibis.literal(bool({s}))"
    | .str => s!"    {s} = ibis.literal(str({s}))"
    | .date => s!"    {s} = ibis.literal(str({s}), type=\"date\")")
  let letLines := prog.lets.map (fun (varName, rhs) =>
    match rhs with
    | .aggSum rel expr =>
      let rStr := emitRel rel
      let eStr := emitSExpr (some "t") expr
      s!"    {varName} = ibis.coalesce(({rStr}).aggregate(val=lambda t: ({eStr}).sum())[\"val\"].as_scalar(), ibis.literal(0, type=\"decimal(38, 12)\"))"
    | .agg op rel expr =>
      let rStr := emitRel rel
      let eStr := emitSExpr (some "t") expr
      s!"    {varName} = ibis.coalesce(({rStr}).aggregate(val=lambda t: {emitAggCall op eStr})[\"val\"].as_scalar(), {emitAggDefault op})"
    | .scalar expr =>
      s!"    {varName} = {emitSExpr none expr}")
  let outDict := prog.outputs.map (fun (k, e) => s!"        \"{k}\": {emitSExpr none e},")
  let footer := String.intercalate "\n" (["    return {"] ++ outDict ++ ["    }"])
  String.intercalate "\n" ([header] ++ scalarCasts ++ letLines ++ [footer, ""])

end Lean2Ibis

import Lean2Sql.DuckDB
import Lean2Ibis.MLHSpec
import Lean2Ibis.LCRSpec
import Lean2Ibis.NSFRSpec

namespace Lean2Sql.Proofs

open Lean2Ibis
open Lean2Sql.DuckDB

def rowToSql (env : Row) : SqlRow :=
  env.map (fun (k, v) => (k, valToSql v))

def tenvToSql (tenv : TableEnv) : SqlTableEnv :=
  tenv.map (fun (t, rows) => (t, rows.map rowToSql))

private theorem sqlRow_get_rowToSql (env : Row) (name : String) :
    (rowToSql env).get name =
      match env.find? (fun (k, _) => k == name) with
      | some (_, v) => valToSql v
      | none => .null := by
  induction env with
  | nil => rfl
  | cons kv rest ih =>
    rcases kv with ⟨k, v⟩
    simp only [rowToSql, List.map_cons, SqlRow.get, List.find?]
    cases (k == name)
    · exact ih
    · rfl

private theorem valToSql_beq (v1 v2 : Val) : (valToSql v1 == valToSql v2) = (v1 == v2) := by
  cases v1 <;> cases v2 <;> simp [valToSql, BEq.beq] <;> rfl

/-- Simple syntactic type system for `SExpr` ensuring no type mismatches (so SQL never produces `NULL`
    due to type errors when all columns/scalars are non-null). -/
inductive Ty where | rat | bool | str | date
  deriving DecidableEq, Repr

def valTy : Val → Ty
  | .rat _ => .rat
  | .bool _ => .bool
  | .str _ => .str
  | .date _ => .date

inductive WellTyped (Γ : String → Ty) : SExpr → Ty → Prop where
  | col {n} : WellTyped Γ (.col n) (Γ n)
  | var {n} : WellTyped Γ (.var n) (Γ n)
  | litRat {n d} : WellTyped Γ (.litRat n d) .rat
  | litBool {b} : WellTyped Γ (.litBool b) .bool
  | litStr {s} : WellTyped Γ (.litStr s) .str
  | not {e} : WellTyped Γ e .bool → WellTyped Γ (.unOp .not e) .bool
  | nonNeg {e} : WellTyped Γ e .rat → WellTyped Γ (.unOp .nonNeg e) .rat
  | arith {op l r} (hop : op = .add ∨ op = .sub ∨ op = .mul ∨ op = .div ∨ op = .min ∨ op = .max) :
      WellTyped Γ l .rat → WellTyped Γ r .rat → WellTyped Γ (.binOp op l r) .rat
  | cmp {op l r} (hop : op = .le ∨ op = .lt ∨ op = .gt) :
      WellTyped Γ l .rat → WellTyped Γ r .rat → WellTyped Γ (.binOp op l r) .bool
  | eq {l r t} : WellTyped Γ l t → WellTyped Γ r t → WellTyped Γ (.binOp .eq l r) .bool
  | logic {op l r} (hop : op = .and ∨ op = .or) :
      WellTyped Γ l .bool → WellTyped Γ r .bool → WellTyped Γ (.binOp op l r) .bool
  | ite {c t f ty} :
      WellTyped Γ c .bool → WellTyped Γ t ty → WellTyped Γ f ty → WellTyped Γ (.ite c t f) ty
  | caseStr {scrut branches dflt ty} :
      WellTyped Γ scrut .str →
      (∀ kv ∈ branches, WellTyped Γ kv.2 ty) →
      WellTyped Γ dflt ty →
      WellTyped Γ (.caseStr scrut branches dflt) ty

/-- Verify `compileProgram` succeeds on `MLH` IR program. -/
theorem mlh_compiles_to_duckdb : (compileProgram MLHSpec.mlhProgram).isSome = true := by
  decide

/-- Environment soundness: every column in `Γ` is bound in `env` with matching `valTy`. -/
def EnvTyped (Γ : String → Ty) (env : Row) : Prop :=
  ∀ k, ∃ v, env.find? (fun (x, _) => x == k) = some (k, v) ∧ valTy v = Γ k

private theorem envTyped_lookup {Γ : String → Ty} {env : Row} (henv : EnvTyped Γ env) (k : String) :
    valTy (env.get k) = Γ k ∧ (rowToSql env).get k = valToSql (env.get k) := by
  rcases henv k with ⟨v, hfind, hty⟩
  have hget : env.get k = v := by simp [Row.get, hfind]
  have hsget : (rowToSql env).get k = valToSql v := by
    rw [sqlRow_get_rowToSql, hfind]
  rw [hget, hsget]
  exact ⟨hty, rfl⟩

mutual
theorem compileSExpr_sound {Γ : String → Ty} {env : Row} (henv : EnvTyped Γ env) :
    ∀ {e : SExpr} {ty : Ty}, WellTyped Γ e ty →
      evalSqlExpr (rowToSql env) (compileSExpr e) = valToSql (evalSExpr env e) ∧
      valTy (evalSExpr env e) = ty
  | .col n, _, hwt => by
    cases hwt
    have ⟨hty, hget⟩ := envTyped_lookup henv n
    simpa [compileSExpr, evalSqlExpr, evalSExpr, hget] using hty
  | .var n, _, hwt => by
    cases hwt
    have ⟨hty, hget⟩ := envTyped_lookup henv n
    simpa [compileSExpr, evalSqlExpr, evalSExpr, hget] using hty
  | .litRat n d, _, hwt => by
    cases hwt; simp [compileSExpr, evalSqlExpr, evalSExpr, valToSql, valTy]
  | .litBool b, _, hwt => by
    cases hwt; simp [compileSExpr, evalSqlExpr, evalSExpr, valToSql, valTy]
  | .litStr s, _, hwt => by
    cases hwt; simp [compileSExpr, evalSqlExpr, evalSExpr, valToSql, valTy]
  | .unOp op e, _, hwt => by
    cases hwt with
    | not h1 =>
      rcases compileSExpr_sound henv h1 with ⟨ih_eq, ih_ty⟩
      cases he : evalSExpr env e with
      | bool b => simp [compileSExpr, evalSqlExpr, sqlNot, evalSExpr, he, ih_eq, valToSql, Val.asBool, valTy]
      | rat _ => simp [he, valTy] at ih_ty
      | str _ => simp [he, valTy] at ih_ty
      | date _ => simp [he, valTy] at ih_ty
    | nonNeg h1 =>
      rcases compileSExpr_sound henv h1 with ⟨ih_eq, ih_ty⟩
      cases he : evalSExpr env e with
      | rat r => simp [compileSExpr, evalSqlExpr, evalSExpr, he, ih_eq, valToSql, Val.asRat, valTy, APS210.nonNeg]
      | bool _ => simp [he, valTy] at ih_ty
      | str _ => simp [he, valTy] at ih_ty
      | date _ => simp [he, valTy] at ih_ty
  | .binOp op l r, _, hwt => by
    cases hwt with
    | arith hop hl hr =>
      rcases compileSExpr_sound henv hl with ⟨ihl_eq, ihl_ty⟩
      rcases compileSExpr_sound henv hr with ⟨ihr_eq, ihr_ty⟩
      cases hl_v : evalSExpr env l <;> simp [hl_v, valTy] at ihl_ty
      cases hr_v : evalSExpr env r <;> simp [hr_v, valTy] at ihr_ty
      rcases hop with rfl | rfl | rfl | rfl | rfl | rfl <;>
      simp [compileSExpr, compileBinOp, evalSqlExpr, evalSqlBinOp, evalSExpr,
        hl_v, hr_v, ihl_eq, ihr_eq, valToSql, Val.asRat, valTy]
      case inr.inr.inr.inl =>
        rename_i a b
        have h0 : (0 : Rat) / 1 = 0 := by native_decide
        simp only [evalSqlCase, evalSqlExpr, evalSqlBinOp]
        rw [ihl_eq, ihr_eq, hl_v, hr_v]
        by_cases hb : b = 0
        · subst hb
          simp [valToSql, h0]
        · have hbeq : (SqlVal.num b == SqlVal.num 0) = false := by
            simp [BEq.beq, hb]
          simp [valToSql, h0, hbeq, hb]
    | cmp hop hl hr =>
      rcases compileSExpr_sound henv hl with ⟨ihl_eq, ihl_ty⟩
      rcases compileSExpr_sound henv hr with ⟨ihr_eq, ihr_ty⟩
      cases hl_v : evalSExpr env l <;> simp [hl_v, valTy] at ihl_ty
      cases hr_v : evalSExpr env r <;> simp [hr_v, valTy] at ihr_ty
      rcases hop with rfl | rfl | rfl <;>
      simp [compileSExpr, compileBinOp, evalSqlExpr, evalSqlBinOp, evalSExpr,
        hl_v, hr_v, ihl_eq, ihr_eq, valToSql, Val.asRat, valTy]
    | eq hl hr =>
      rcases compileSExpr_sound henv hl with ⟨ihl_eq, _⟩
      rcases compileSExpr_sound henv hr with ⟨ihr_eq, _⟩
      have hbeq := valToSql_beq (evalSExpr env l) (evalSExpr env r)
      cases hl_v : evalSExpr env l <;> cases hr_v : evalSExpr env r <;>
      simp [compileSExpr, compileBinOp, evalSqlExpr, evalSqlBinOp, evalSExpr,
        hl_v, hr_v, ihl_eq, ihr_eq, valToSql, valTy] at hbeq ⊢ <;>
      exact hbeq
    | logic hop hl hr =>
      rcases compileSExpr_sound henv hl with ⟨ihl_eq, ihl_ty⟩
      rcases compileSExpr_sound henv hr with ⟨ihr_eq, ihr_ty⟩
      cases hl_v : evalSExpr env l <;> simp [hl_v, valTy] at ihl_ty
      cases hr_v : evalSExpr env r <;> simp [hr_v, valTy] at ihr_ty
      rename_i b1 b2
      rcases hop with rfl | rfl
      · cases b1 <;> cases b2 <;>
        simp [compileSExpr, compileBinOp, evalSqlExpr, evalSqlBinOp, sqlAnd, evalSExpr,
          hl_v, hr_v, ihl_eq, ihr_eq, valToSql, Val.asBool, valTy]
      · cases b1 <;> cases b2 <;>
        simp [compileSExpr, compileBinOp, evalSqlExpr, evalSqlBinOp, sqlOr, evalSExpr,
          hl_v, hr_v, ihl_eq, ihr_eq, valToSql, Val.asBool, valTy]
  | .ite c t f, _, hwt => by
    cases hwt with
    | ite hc ht hf =>
      rcases compileSExpr_sound henv hc with ⟨ihc_eq, ihc_ty⟩
      cases hcv : evalSExpr env c <;> simp [hcv, valTy] at ihc_ty
      rename_i b
      cases b
      · rcases compileSExpr_sound henv hf with ⟨ihf_eq, ihf_ty⟩
        simp [compileSExpr, evalSqlExpr, evalSqlCase, evalSExpr, hcv, ihc_eq, ihf_eq, Val.asBool, valToSql, ihf_ty]
      · rcases compileSExpr_sound henv ht with ⟨iht_eq, iht_ty⟩
        simp [compileSExpr, evalSqlExpr, evalSqlCase, evalSExpr, hcv, ihc_eq, iht_eq, Val.asBool, valToSql, iht_ty]
  | .caseStr scrut branches dflt, ty, hwt => by
    cases hwt with
    | caseStr hs hbs hd =>
      rcases compileSExpr_sound henv hs with ⟨ihs_eq, ihs_ty⟩
      cases hsv : evalSExpr env scrut <;> simp [hsv, valTy] at ihs_ty
      rename_i sv
      simpa [compileSExpr, evalSqlExpr, evalSExpr, hsv, Val.asStr] using
        compileCaseBranches_sound henv (compileSExpr scrut) sv (by simpa [hsv, valToSql] using ihs_eq) hbs hd
  termination_by e _ _ => sizeOf e

theorem compileCaseBranches_sound {Γ : String → Ty} {env : Row} (henv : EnvTyped Γ env)
    (cs : SqlExpr) (sv : String) (hcs : evalSqlExpr (rowToSql env) cs = .str sv) :
    ∀ {branches : List (String × SExpr)} {dflt : SExpr} {ty : Ty},
      (∀ kv ∈ branches, WellTyped Γ kv.2 ty) →
      WellTyped Γ dflt ty →
      evalSqlCase (rowToSql env) (compileCaseBranches cs branches) (compileSExpr dflt) =
        valToSql (evalCaseStr env sv branches dflt) ∧
      valTy (evalCaseStr env sv branches dflt) = ty
  | [], dflt, ty, _, hd => by
    simpa [compileCaseBranches, evalSqlCase, evalCaseStr] using compileSExpr_sound henv hd
  | (k, rhs) :: rest, dflt, ty, hbs, hd => by
    have hrhs : WellTyped Γ rhs ty := hbs (k, rhs) (by simp)
    have hrest : ∀ kv ∈ rest, WellTyped Γ kv.2 ty := fun kv hkv => hbs kv (by simp [hkv])
    simp only [compileCaseBranches, evalSqlCase, evalSqlExpr, evalSqlBinOp, hcs, evalCaseStr]
    by_cases hk : k == sv
    · have hbeq : (SqlVal.str sv == SqlVal.str k) = true := by
        have hks : k = sv := beq_iff_eq.mp hk
        subst hks; simp [BEq.beq]
      simp [hk, hbeq]
      exact compileSExpr_sound henv hrhs
    · have hbeq : (SqlVal.str sv == SqlVal.str k) = false := by
        have hks : k ≠ sv := by intro h; exact hk (beq_iff_eq.mpr h)
        simp [BEq.beq, Ne.symm hks]
      simp [hk, hbeq]
      exact compileCaseBranches_sound henv cs sv hcs hrest hd
  termination_by bs dflt _ _ _ => sizeOf bs + sizeOf dflt
end

/-- Concrete sanity theorem: `sqlSumVals` on non-empty `.num` list equals `.num (sum)`. -/
theorem sqlSumVals_nonnull (xs : List Rat) (h : xs ≠ []) :
    sqlSumVals (xs.map SqlVal.num) = .num (xs.foldl (· + ·) 0) := by
  cases xs with
  | nil => exact absurd rfl h
  | cons x rest =>
    simp only [List.map_cons, sqlSumVals, List.foldl_cons]
    congr 1
    generalize (0 : Rat) + x = acc
    clear h
    induction rest generalizing acc with
    | nil => rfl
    | cons y ys ih =>
      simp only [List.map_cons, List.foldl_cons]
      exact ih (acc + y)

end Lean2Sql.Proofs

import Lean2Ibis.IR
import Lean2Ibis.MLHSpec
import Lean2Ibis.LCRSpec
import Lean2Ibis.NSFRSpec
import Lean2Ibis.ExternalAndComplianceSpec
import Lean2Ibis.UpstreamSpec
import Lean2Ibis.EmitIbis
import Lean2Sql.DuckDB

def main : IO Unit := do
  IO.FS.createDirAll "generated"
  IO.FS.writeFile "generated/aps210_mlh_ibis.py"
    (Lean2Ibis.emitProgram Lean2Ibis.MLHSpec.mlhProgram)
  IO.FS.writeFile "generated/aps210_lcr_ibis.py"
    (Lean2Ibis.emitProgram Lean2Ibis.LCRSpec.lcrProgram)
  IO.FS.writeFile "generated/aps210_nsfr_ibis.py"
    (Lean2Ibis.emitProgram Lean2Ibis.NSFRSpec.nsfrProgram)
  IO.FS.writeFile "generated/aps210_external_ibis.py"
    (Lean2Ibis.emitProgram Lean2Ibis.ExternalAndComplianceSpec.externalProgram)
  IO.FS.writeFile "generated/aps210_compliance_ibis.py"
    (Lean2Ibis.emitProgram Lean2Ibis.ExternalAndComplianceSpec.complianceGateProgram)
  IO.FS.writeFile "generated/aps210_upstream_ibis.py"
    (Lean2Ibis.emitProgram Lean2Ibis.UpstreamSpec.upstreamProgram)

  -- Also emit verified DuckDB SQL directly from Lean2Sql.DuckDB
  if let some sqlProg := Lean2Sql.DuckDB.compileProgram Lean2Ibis.MLHSpec.mlhProgram then
    IO.FS.writeFile "generated/aps210_mlh_duckdb.sql"
      (Lean2Sql.DuckDB.emitDuckDbSql sqlProg
        [ ("placements_by_other_adis", "CAST(:placements_by_other_adis AS DECIMAL(38, 12))")
        , ("required_min_rate", "CAST(:required_min_rate AS DECIMAL(38, 12))")
        , ("trigger_ratio", "CAST(:trigger_ratio AS DECIMAL(38, 12))") ])
  IO.println "Generated all 6 Ibis modules and DuckDB SQL in generated/"

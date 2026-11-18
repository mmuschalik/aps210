// APS 210 Lean 4 Implementation Specification
// Compiles offline with Typst 0.15.x (`typst compile docs/aps210_lean_implementation.typ`)

#set document(
  title: "APRA Prudential Standard APS 210 — Lean 4 Implementation Specification",
  author: "APS 210 Formal Verification & Liquidity Engine",
)

#let navy = rgb("#0F2C59")
#let slate = rgb("#334155")
#let muted = rgb("#64748B")
#let accent = rgb("#1D4ED8")
#let teal = rgb("#0F766E")
#let amber = rgb("#B45309")
#let crimson = rgb("#991B1B")
#let surface = rgb("#F8FAFC")
#let border = rgb("#CBD5E1")
#let headerbg = rgb("#E2E8F0")

#set page(
  paper: "a4",
  margin: (top: 22mm, bottom: 20mm, left: 18mm, right: 18mm),
  header: context {
    if counter(page).get().first() > 1 [
      #set text(size: 8pt, fill: muted)
      #grid(
        columns: (1fr, auto),
        [APRA APS 210 — Lean 4 Implementation Documentation],
        [Scope: Pure Lean Engine (`APS210.*`, excluding Ibis/SQL)],
      )
      #v(-4pt)
      #line(length: 100%, stroke: 0.4pt + border)
    ]
  },
  footer: context {
    if counter(page).get().first() > 1 [
      #line(length: 100%, stroke: 0.4pt + border)
      #v(-2pt)
      #set text(size: 8pt, fill: muted)
      #grid(
        columns: (1fr, auto),
        [Exact Rational (`Rat`) Specification & Pipeline Architecture],
        [Page #counter(page).display("1 of 1", both: true)],
      )
    ]
  },
)

#set text(
  font: ("Helvetica Neue", "Helvetica", "Arial"),
  size: 9.5pt,
  fill: rgb("#1E293B"),
  hyphenate: false,
)

#set par(leading: 0.62em, justify: false)
#set heading(numbering: "1.1")

#show heading.where(level: 1): it => {
  v(10pt)
  block(
    width: 100%,
    below: 7pt,
    stroke: (bottom: 1.5pt + navy),
    inset: (bottom: 4pt),
  )[
    #set text(size: 13.5pt, weight: "bold", fill: navy)
    #it
  ]
}

#show heading.where(level: 2): it => {
  v(7pt)
  block(below: 4pt)[
    #set text(size: 10.8pt, weight: "bold", fill: accent)
    #it
  ]
}

#show heading.where(level: 3): it => {
  v(4pt)
  block(below: 3pt)[
    #set text(size: 9.8pt, weight: "bold", fill: slate)
    #it
  ]
}

#show raw.where(block: false): it => box(
  fill: rgb("#F1F5F9"),
  inset: (x: 2pt, y: 0.6pt),
  outset: (y: 0.8pt),
  radius: 2pt,
  text(font: ("Menlo", "Monaco", "Courier New"), size: 7.8pt, fill: navy, it),
)

// Breakable identifier helper for narrow table cells (wraps cleanly on '.', '_', or camelCase)
#let id(s) = {
  let broken = s
    .replace(".", ".\u{200B}")
    .replace("_", "_\u{200B}")
    .replace("Criteria", "\u{200B}Criteria")
    .replace("CorpSov", "\u{200B}CorpSov")
    .replace("Gt2m", "\u{200B}Gt2m")
    .replace("Managed", "\u{200B}Managed")
    .replace("Collateral", "\u{200B}Collateral")
    .replace("Recognised", "\u{200B}Recognised")
    .replace("recognised", "\u{200B}recognised")
    .replace("Conserves", "\u{200B}Conserves")
    .replace("conserves", "\u{200B}conserves")
    .replace("And", "\u{200B}And")
    .replace("Non", "\u{200B}Non")
    .replace("Excluded", "\u{200B}Excluded")
    .replace("Eligibility", "\u{200B}Eligibility")
    .replace("Assessments", "\u{200B}Assessments")
    .replace("Aps113", "\u{200B}Aps113")
  text(font: ("Menlo", "Monaco", "Courier New"), size: 7.3pt, fill: navy, broken)
}

#show raw.where(block: true): it => block(
  width: 100%,
  fill: surface,
  stroke: (left: 2.5pt + accent, rest: 0.5pt + border),
  inset: 7pt,
  radius: 3pt,
  text(font: ("Menlo", "Monaco", "Courier New"), size: 8.1pt, fill: rgb("#0F172A"), it),
)

#let callout(title, tone: accent, body) = block(
  width: 100%,
  fill: tone.lighten(93%),
  stroke: (left: 3pt + tone, rest: 0.5pt + tone.lighten(60%)),
  inset: 8pt,
  radius: 3pt,
  [
    #text(weight: "bold", fill: tone, size: 9pt)[#title]
    #v(2pt)
    #body
  ],
)

#let stagebox(num, title, subtitle, mod, tone: navy) = rect(
  width: 100%,
  fill: surface,
  stroke: (left: 3.5pt + tone, rest: 0.6pt + border),
  inset: 7pt,
  radius: 3pt,
  [
    #grid(
      columns: (auto, 1fr, auto),
      gutter: 6pt,
      rect(fill: tone, radius: 2pt, inset: (x: 5pt, y: 2pt))[
        #text(fill: white, weight: "bold", size: 8pt)[#num]
      ],
      [
        #text(weight: "bold", fill: navy, size: 9.5pt)[#title] \
        #text(size: 8.4pt, fill: slate)[#subtitle]
      ],
      align(right)[
        #text(font: ("Menlo", "Monaco"), size: 7.8pt, fill: muted)[#mod]
      ],
    )
  ],
)

#let tbl(cols, align-spec: auto, ..rows) = block(
  width: 100%,
  breakable: true,
  table(
    columns: cols,
    align: align-spec,
    stroke: 0.45pt + border,
    inset: (x: 5.5pt, y: 3.8pt),
    fill: (_, y) => if y == 0 { headerbg } else if calc.odd(y) { white } else { surface },
    ..rows,
  ),
)

// ============================================================================
// TITLE PAGE
// ============================================================================
#align(left)[
  #v(14mm)
  #rect(fill: navy, radius: 3pt, inset: (x: 9pt, y: 4pt))[
    #text(fill: white, weight: "bold", size: 8.5pt, tracking: 0.8pt)[TECHNICAL IMPLEMENTATION SPECIFICATION]
  ]
  #v(6mm)
  #text(size: 23pt, weight: "bold", fill: navy)[
    APRA Prudential Standard APS 210 \
    Liquidity Implementation in Lean 4
  ]
  #v(3mm)
  #text(size: 11.5pt, fill: slate)[
    End-to-End Calculation Architecture, Pipeline Stages, Prudential Rules, \
    Cash-Flow Engine, and Kernel-Verified Soundness
  ]
  #v(8mm)
  #line(length: 100%, stroke: 1.5pt + navy)
  #v(6mm)

  #grid(
    columns: (1fr, 1fr),
    gutter: 12pt,
    callout("Scope & Coverage", tone: navy)[
      Covers the complete pure Lean 4 specification and executable calculation library under
      `APS210.lean` and `APS210/**` (*15 files, 4,657 lines of Lean 4*). \
      *Explicitly excluded:* downstream query/SQL/Ibis compilation layers (`Lean2Ibis`, `Lean2Sql`).
    ],
    callout("Verification Baseline", tone: teal)[
      Verified against current workspace build (`lake build APS210`, 32 build jobs passing) with
      *29 exported formal theorems* (`27` in `Theorems.lean`, `2` in `Upstream.lean`) and
      *205 kernel-checked unit & regression examples* in `Tests.lean`.
    ],
  )

  #v(8mm)
  #text(weight: "bold", size: 10pt, fill: navy)[Summary of Covered Prudential Pillars & Pipeline Stages]
  #v(3pt)
  #tbl(
    (auto, 1fr, auto),
    [*Stage*], [*Responsibility*], [*Primary Modules*],
    [Stage 0], [Exact rational core, enumerations, ratio gate, cross-standard APS 110/112/113/120/220 predicates], [`Basic`, `ExternalStandards`],
    [Stage 1], [Calendar math, maturity/option conservatism, retail/SME segmentation, 12m/24m lookbacks, ECAI resolution], [`Upstream`],
    [Stage 2], [Multi-horizon survival ladder, contractual splitting (`NSFR` & `LCR`), MNA/FX derivative netting], [`CashFlows`],
    [Stage 3A], [Liquidity Coverage Ratio (HQLA, SFT unwinds, 15/40 caps, CLF, FCS split, Table 3 outflows, Table 4 inflows)], [`LCR/HQLA`, `Outflows`, `Inflows`, `LCR`],
    [Stage 3B], [Minimum Liquidity Holdings (RBA repo / debt rules, Para 2(f) bilateral netting, APRA trigger gate)], [`MLH`],
    [Stage 3C], [Net Stable Funding Ratio (ASF, RSF, encumbrance, greedy CLF buckets, Para 26 SFT & Para 37 derivative netting, OBS)], [`NSFR`],
    [Stage 4], [Governance/CFP/notifications, consolidation (`level1`/`level2`), `BufferPolicy`, `ApraAdjustmentRule`, continuous check], [`Governance`, `Compliance`],
    [Stage 5], [Closed-form cap bounds, conservation invariants, fail-closed guarantees, exhaustive Table 3/4/ASF/RSF suites], [`Theorems`, `Tests`],
  )

  #v(8mm)
  #outline(title: [Contents], indent: 1.4em, depth: 2)
]

#pagebreak()

// ============================================================================
// SECTION 1: ARCHITECTURE & MODULE INVENTORY
// ============================================================================
= Architecture Overview & Design Principles

The `APS210` Lean 4 library formalises APRA Prudential Standard APS 210 (*Liquidity*)—covering the Main Body, Attachment A (*Liquidity Coverage Ratio*), Attachment B (*Minimum Liquidity Holdings*), Attachment C (*Net Stable Funding Ratio*), and cross-referenced prudential rules from APS 110, APS 112, APS 113, APS 120, and APS 220—as a deterministic, strongly typed functional pipeline.

== Core Engineering & Regulatory Invariants

+ *Exact Rational Arithmetic (`Money := Rat`, `Rate := Rat`):* Every monetary balance, haircut, run-off factor, ASF/RSF weight, and compliance threshold is represented in exact arbitrary-precision rationals (`Rat`). No IEEE-754 floating-point rounding drift exists anywhere in the engine, allowing algebraic identities (such as the `15/85` and `2/3` HQLA cap inequalities) to be proved directly in Lean's kernel.
+ *Fail-Closed Regulatory Defaults:* Opt-in regulatory eligibility flags (`freelyTransferableAtLevel2`, `issuerRestrictionSatisfied`, `recognisedByRegulator`, `intermediatedLookThroughMet`, `hostJurisdictionAppliesBasel`, `hqla2aDiversified`, MLH operational booleans, and `AdiComplianceInput.bufferPolicy`) default to `false` or `none`. Omitting an attestation never silently grants prudential relief; missing `bufferPolicy` fails top-level compliance closed.
+ *Dual-Layer Modularity:* Pure prudential ratio evaluators (`LCR`, `MLH`, `NSFR`) are decoupled from upstream positional transformation (`Upstream`, `CashFlows`), while `Compliance` binds qualitative governance (`Governance`), CLF limit pass-through (`withClfLimit`), management buffer policy (`BufferPolicy`), and supervisory discretion (`ApraAdjustmentRule`) into a single audit verdict.

== Module & Line-Count Inventory

The root aggregator `APS210.lean` imports all 14 functional, compliance, verification, and test modules listed below.

#tbl(
  (1.5fr, auto, 2.3fr),
  align-spec: (left, right, left),
  [*Module Path*], [*Lines*], [*Core Responsibility*],
  [`APS210.lean`], [`14`], [Root module importing the 14 `APS210.*` submodules],
  [`APS210/Basic.lean`], [`119`], [Exact `Rat` types, consolidation basis, currencies, ratings, `checkRatio`],
  [`APS210/ExternalStandards.lean`], [`191`], [APS 112 tables & ECAI resolution, APS 113 SME, APS 120 relief, APS 220 performing status],
  [`APS210/Upstream.lean`], [`498`], [9-stage raw-data & calendar (`EpochDate`) preprocessing pipeline + 2 theorems],
  [`APS210/CashFlows.lean`], [`323`], [Multi-horizon cash-flow ladder, Para 36 buffer, NSFR/LCR splitting, MNA/FX netting],
  [`APS210/LCR/HQLA.lean`], [`408`], [HQLA classifiers, haircuts, SFT unwinds, 15/40 caps, Para 9(e) FX cap, CLF & grace],
  [`APS210/LCR/Outflows.lean`], [`344`], [Scorecard, FCS \$250k retail split, Para 42 tainting, 46-case Table 3 outflows],
  [`APS210/LCR/Inflows.lean`], [`103`], [14-cell Table 4 secured lending, non-HQLA/performing filter, 75% inflow cap],
  [`APS210/LCR.lean`], [`97`], [AUD/All-Currencies LCR evaluator, CLF gating, 50% Foreign Head Office facility cap],
  [`APS210/MLH.lean`], [`120`], [MLH asset qualification, RBA/debt distinction, Para 2(f) netting, trigger ratio gate],
  [`APS210/NSFR.lean`], [`451`], [ASF (19 cases), RSF (23 cases), encumbrance, greedy CLF allocation, derivatives/SFT, OBS],
  [`APS210/Governance.lean`], [`278`], [Board/Level 2 oversight, LRM, CFP, stress tests, notifications, ADI minimum profile],
  [`APS210/Compliance.lean`], [`281`], [Level 1 + Level 2 gate, `BufferPolicy`, `ApraAdjustmentRule`, `withClfLimit`, continuous monitor],
  [`APS210/Theorems.lean`], [`485`], [27 formal proofs covering caps, conservation, fail-closed guards, monotonicity, soundness],
  [`APS210/Tests.lean`], [`945`], [205 kernel-checked unit, matrix, external-standard, and regression tests],
  [*Total (`APS210` Engine)*], [*`4,657`*], [*15 Lean 4 files (excludes `Lean2Ibis` / `Lean2Sql`)*],
)

== End-to-End Execution Pipeline

#figure(
  block(width: 100%, stroke: 0.6pt + border, radius: 4pt, inset: 8pt, fill: white)[
    #stack(
      dir: ttb,
      spacing: 5pt,
      stagebox(
        "STAGE 0",
        "Foundation & Cross-Standard Prudential Predicates",
        "Money/Rate arithmetic, ratings (AAA..belowBMinus), APS 112 risk weights & ECAI second-best rule, APS 113/120/220 predicates",
        "Basic, ExternalStandards",
        tone: slate,
      ),
      align(center)[#text(fill: accent, weight: "bold")[↓]],
      stagebox(
        "STAGE 1",
        "Upstream Data Normalisation & Proleptic Calendar Pipeline",
        "Stages 1–9: maturity/option conservatism, civil calendar addMonthsEpoch, $2m SME & $250k FCS split, Para 42 taint, 12m/24m lookbacks",
        "Upstream",
        tone: accent,
      ),
      align(center)[#text(fill: accent, weight: "bold")[↓]],
      stagebox(
        "STAGE 2",
        "Contractual Cash-Flow Projection & Splitting Engine",
        "6-bucket maturity ladder & survival buffer, conservative option/open-maturity days, NSFR <6m / 6m–<1y / >=1y splits, LCR MNA + FX netting",
        "CashFlows",
        tone: teal,
      ),
      align(center)[#text(fill: accent, weight: "bold")[↓]],
      grid(
        columns: (1fr, 1fr, 1fr),
        gutter: 6pt,
        stagebox(
          "STAGE 3A",
          "LCR Engine",
          "HQLA unwind + 15/40 caps + CLF, retail/Table 3 outflows, Table 4 inflows (75% cap), HO 50% cap",
          "LCR/*",
          tone: navy,
        ),
        stagebox(
          "STAGE 3B",
          "MLH Engine",
          "9% liquid holdings, debt vs non-debt Para 3 criteria, Para 2(f) bilateral netting, trigger > min",
          "MLH",
          tone: navy,
        ),
        stagebox(
          "STAGE 3C",
          "NSFR Engine",
          "100% ASF / RSF, encumbrance rules, greedy CLF buckets, MNA/VM derivatives, Para 26 SFT, OBS",
          "NSFR",
          tone: navy,
        ),
      ),
      align(center)[#text(fill: accent, weight: "bold")[↓]],
      stagebox(
        "STAGE 4",
        "Governance, Management Buffers, APRA Adjustments & Compliance Gate",
        "Level 1 + conditional Level 2 evaluation, withClfLimit auto-wiring, BufferPolicy gate, ApraAdjustmentRule overrides, stress waiver (LCR/NSFR only), continuous history check",
        "Governance, Compliance",
        tone: amber,
      ),
    )
  ],
  caption: [APS 210 Lean 4 multi-stage calculation and compliance architecture.],
)

#pagebreak()

// ============================================================================
// SECTION 2: STAGE 0 - FOUNDATION & EXTERNAL STANDARDS
// ============================================================================
= Stage 0: Foundation & External Prudential Standards

== Core Types & Ratio Checking (`APS210/Basic.lean`)

`Basic.lean` establishes the shared numeric and regulatory vocabulary used by all downstream stages:

- *Numeric helpers:* `Money := Rat`, `Rate := Rat`, `pct (n : Nat) := (n : Rat) / 100`, `nonNeg (x : Rat) := max x 0`, and `sumBy xs f := xs.foldl (fun acc x => acc + f x) 0`.
- *Entity & Basis Types:*
  - `ConsolidationBasis`: `.level1` (licensed ADI standalone) vs `.level2` (consolidated banking group).
  - `AdiIncorporation`: `.locallyIncorporated` vs `.foreign`.
  - `AdiClassification`: `.lcr` vs `.mlh`.
  - `Currency`: `.aud`, `.usd`, `.eur`, `.gbp`, `.jpy`, `.nzd`, `.chf`, `.cad`, `.sgd`, `.other (code : String)`.
  - `CreditRating`: `.aaa`, `.aaPlus`, `.aa`, `.aaMinus`, `.aPlus`, `.a`, `.aMinus`, `.bbbPlus`, `.bbb`, `.bbbMinus`, `.bbPlusOrBelow`, `.bRating`, `.belowBMinus`, `.unrated` with helpers `isAtLeastAA`, `isAtLeastAAMinus`, and `isBetweenAPlusAndBBBMinus`.
  - `CounterpartyKind`: 10 counterparty classes (`.retail`, `.sme`, `.nonFinancialCorporate`, `.sovereign`, `.centralBank`, `.pse`, `.mdb`, `.bank`, `.otherFinancialInstitution`, `.otherLegalEntity`).

=== Ratio Verification Gate (`checkRatio`)

Given numerator $N$, denominator $D$, and minimum required rate $r_min$:

$
"checkRatio"(N, D, r_min)."compliant" =
cases(
  "false" & "if" D <= 0,
  N >= r_min dot D & "if" D > 0
)
$

When $D > 0$, `ratio := some (N / D)`. Zero or negative denominators never produce a spurious division-by-zero pass.

== Cross-Standard Prudential Rules (`APS210/ExternalStandards.lean`)

Rather than duplicating capital or credit-quality logic, `APS210` codifies the referenced APRA standards directly in `ExternalStandards`:

#tbl(
  (0.95fr, 1.35fr, 1.85fr),
  [*External Standard*], [*Lean Function / Structure*], [*Implemented Regulatory Rule*],
  [APS 112 Att F (ECAI)], [#id("CreditRatingGrade.ofCreditRating"), #id("resolveMultipleEcaiAssessments")], [Maps ratings to Grades `1..6` (`bRating -> grade5`, `belowBMinus -> grade6`, `unrated -> unrated`). 1 rating $=>$ that rating; 2 ratings $=>$ worse grade; $>=$3 ratings $=>$ second-best grade (`s.getD 1`).],
  [APS 112 Att B Table 5], [#id("sovereignRiskWeightPct")], [Sovereign/central bank risk weights: `grade1 -> 0%`, `grade2 -> 20%`, `grade3 -> 50%`, `grade4/5 -> 100%`, `grade6 -> 150%`, `unrated -> 100%`.],
  [APS 112 Att B Table 6], [#id("domesticPseRiskWeightPct")], [Domestic PSE risk weights: `grade1 -> 20%`, `grade2 -> 50%`, `grade3/4/5 -> 100%`, `grade6 -> 150%`, `unrated -> 100%`.],
  [APS 112 Att B Table 7], [#id("bankOrNonZeroMdbRiskWeightPct")], [Banks & non-0%-RW MDBs: `grade1 -> 20%`, `grade2/3 -> 50%`, `grade4/5 -> 100%`, `grade6 -> 150%`, `unrated -> 50%`.],
  [APS 112 Mortgages], [#id("StandardResidentialMortgageCriteria.qualifiesFor65PctRsf")], [Requires `lvrPct <= 80`, `legallyEnforceableSecurity`, `firstMortgageOrEquivalent`, and `independentValuationComplete` to qualify for 65% NSFR RSF.],
  [APS 110/112 Netting], [#id("BilateralNettingAgreement.isEligible")], [Requires `writtenAndEnforceable`, `singleLegalObligationOnDefault`, `enforceableAcrossJurisdictions`, `noWalkawayClause`, and `legalReviewCurrent`.],
  [APS 110 Att D (VM)], [#id("CashVariationMarginEligibility.isEligibleForNsfrOffset")], [Requires `markedToMarketDaily`, `cashInSameCurrencyAsSettlement`, `fullAmountTransferredWithZeroThreshold`, `notSegregatedByReceivingParty` (plus APS 210 daily collateral exchange / VM offset conditions).],
  [APS 110 Att D Para 38 (SFT)], [#id("SftNettingCriteria.isEligible")], [Requires `sameExplicitFinalSettlementDate`, `notOpenEndedUnwindable`, `legallyEnforceableSetOffOnDefault`, and `settledViaDvpOrSimultaneousMechanism`.],
  [APS 113 Para 40], [#id("qualifiesAsSmeUnderAps113Para40")], [Non-negative consolidated group exposure strictly below `$1,500,000` (`0 <= exposure && exposure < 1_500_000`).],
  [APS 220 (Credit)], [#id("isPerformingLoanPerAps220")], [`daysPastDue < 90 && !unlikelyToPay`; failing loans are forced into non-performing 100% RSF (`allOtherAssets`).],
  [APS 120 (Securitisation)], [#id("Aps120CapitalReliefCriteria.isMet")], [Requires `significantCreditRiskTransferred`, `aps120OriginatorExclusionMet`, and `noImplicitSupportProvided` for MLH/NSFR deconsolidation.],
)

#pagebreak()

// ============================================================================
// SECTION 3: STAGE 1 - UPSTREAM PREPROCESSING PIPELINE
// ============================================================================
= Stage 1: Upstream Data Preprocessing (`APS210/Upstream.lean`)

`APS210/Upstream.lean` bridges raw transactional books, rating agency feeds, and historical collateral series into validated inputs for the core engines via `evaluateUpstream : UpstreamInput -> UpstreamResult`, structured into *nine discrete deterministic stages* atop a pure proleptic Gregorian calendar engine.

== Calendar Arithmetic (`EpochDate`, `civilFromEpoch`, `addMonthsEpoch`)

To eliminate 30/360 or fixed-365 approximations where exact calendar dates are supplied, `Upstream` implements:
- `civilFromEpoch` / `epochFromCivil`: Exact Howard Hinnant proleptic Gregorian conversion (`z = daysSince1970 + 719468`).
- `daysInMonth y m`: Full leap-year rule (`(y % 4 == 0 && y % 100 != 0) || (y % 400 == 0)`).
- `addMonthsEpoch d months`: Computes target year/month and clamps the day to `min origDay (daysInMonth newYear newMonth)` so adding 6 or 12 months from e.g. 29 Feb or 31 Aug lands cleanly on the true calendar boundary.

== The Nine Upstream Stages

#tbl(
  (0.65fr, 1.3fr, 2.35fr),
  [*Stage*], [*Key Types & Functions*], [*Calculation & Regulatory Logic*],
  [Stage 1], [`PositionRow`, `effectiveMaturityDays`, `classifyNsfrBucketCal`, `classifyNsfrBucket`], [Applies Attachment C/A option conservatism: liabilities (`isLiability = true`) take the *minimum* (earliest) of `contractualMaturityDays`, `earliestInvestorPutOrCallDays`, and `earliestIssuerCallIfExpectedDays`; assets take the *maximum* (latest) with `latestExtensionOptionDays`. When `reportingDate?` is set, NSFR bucketing compares `maturityDate` against `addMonthsEpoch rep 6` and `addMonthsEpoch rep 12`; otherwise falls back to `< 182`, `182..<365`, `>= 365`.],
  [Stage 2], [`DerivativeFxCashflowRow`, `netDerivativeOutflowInflow`], [Aggregates LCR 30-day derivative & FX leg obligations per netting group or transaction, returning bilateral net `(outflow = max (pay - rec) 0, inflow = max (rec - pay) 0)`.],
  [Stage 3], [`CustomerDepositRow`, `evaluateDeposits`, `toHqlaDepositList`], [Qualifies deposits (`retail` or `sme && totalEntityExposureAud < 2_000_000 && managedAsRetail`), splits balances across the `$250,000` FCS guarantee pro-rata per depositor pool, tracks Para 42 term-deposit pool tainting (`poolHasWithdrawalWithoutPenalty`), and produces `LCR.RetailDeposit` rows plus rejected wholesale excess.],
  [Stage 4], [`ForeignSovAssetRow`, `CurrencyOutflowRow`, `cappedForeignSovHqla1`], [Computes foreign-currency sovereign HQLA1 holdings vs foreign-currency net cash outflows per currency (Att A Para 9(e)); takes `min` cap across currency snapshots and restricts recognised value to the net outflow ceiling in that currency.],
  [Stage 5], [`CollateralFlowSample`, `max30dCollateralOutflowCal`], [Validates lookback coverage (`>= 700` days / `>= 23` calendar months and `>= 24` calendar months where dated) and computes the largest absolute 30-day net collateral flow across sliding 30-day windows (`nonneg` guaranteed by `max30dCollateralOutflow_nonneg`).],
  [Stage 6], [`MonthlyTradeFinanceSample`, `avg12mNetOutflowCal`], [Validates a minimum 12-month history and computes the arithmetic mean monthly net outflow for Table 3 trade finance / non-trade guarantee lines (Table 3 notes 7–8).],
  [Stage 7], [`parseCreditRatingStr`, `resolveEcaiByExposure`], [Normalises free-text rating strings (`"AAA"`, `"AA-"`, `"B"`, `"CCC+"` -> `belowBMinus`, etc.) into `CreditRating`, applies `ExternalStandards.resolveMultipleEcaiAssessments` (worse of 2 / second-best of $>=$3), and derives APS 112 Table 5/6/7 risk weights by exposure class.],
  [Stage 8], [`AmortisationCashflowRow`], [Normalises amortising loan and debt-issuance principal/interest cash-flow rows across maturity dates for downstream NSFR maturity splitting (`CashFlows`).],
  [Stage 9], [`SftLegRow`, `ClfSummaryInput`, `toLcrClfInput`], [Maps 30-day repo/reverse-repo legs into `HQLA.ShortTermSftLeg` (capturing exchanged HQLA categories, currency, and residual days) and converts CLF operational/collateral attributes into `HQLA.ClfInput`.],
)

// ============================================================================
// SECTION 4: STAGE 2 - CASH-FLOW ENGINE
// ============================================================================
= Stage 2: Cash-Flow Engine (`APS210/CashFlows.lean`)

`APS210/CashFlows.lean` provides dynamic multi-horizon maturity projection shared across Main Body liquidity risk monitoring (Paragraph 36), LCR derivative/contractual flow extraction, and NSFR amortising balance splitting.

== Multi-Horizon Cash-Flow Ladder & Survival Buffer (Para 36)

Cash flows are slotted into six `CashFlowHorizon` buckets via `horizonOfDays`:

#align(center)[
  `overnight (<= 1d)` | `days2To7 (2–7d)` | `days8To14 (8–14d)` | `days15To30 (15–30d)` | `days31To90 (31–90d)` | `beyond90Days (> 90d)`
]

#v(-4pt)
=== Conservative Effective Maturity (`effectiveOutflowDays` / `effectiveInflowDays`)

- *Outflows:* Uses `min` over `contractualDays`, `earliestOptionDays`, and `latestExtensionDays := none` — any contractual investor put or expected issuer call pulls the outflow *earlier* (and open-maturity cash outflows default to earliest callability).
- *Inflows:* Uses `max` over `contractualDays` and `latestExtensionDays` (extensions push inflows *later*). If `isOpenMaturity && !isMinimumContractualPayment`, the inflow is excluded (`none` / ineligible for LCR 30-day credit).

=== Contractual Cash-Flow Ladder (`projectCashFlowLadder`)

Starting from `openingLiquidBuffer`, at each horizon $h_i$ up to `cutoffHorizon`:
$ "netFlow"(h_i) = "inflows"(h_i) - "outflows"(h_i), quad "cumulativeLiquidity"(h_i) = "openingLiquidBuffer" + sum_(j <= i) "netFlow"(h_j) $
`hasPositiveSurvivalBuffer` returns `true` iff `cumulativeLiquidity >= 0` across every bucket up to and including the cutoff horizon (Main Body Para 36 positive cumulative net surplus requirement).

== NSFR Amortisation Splitting & LCR Derivation

- *NSFR Liability Splitting (`splitLiabilityCashFlowsForAsf` / `...Cal`):* Partitions principal tranches of borrowings/deposits across `<6m`, `6m..<1y`, and `>=1y` using `nsfrBoundaryDays` (exact calendar `addMonthsEpoch rep 6` and `addMonthsEpoch rep 12` when `reportingDate?` is given, else 182/365 days), creating `AsfItem`s per bucket. Proven lossless in `splitLiabilityCashFlowsForAsf_conserves_carrying`.
- *NSFR Amortising Loan Splitting (`splitAmortisingLoanForRsf` / `...Cal`):* Splits principal repayments of amortising loans between `<1y` and `>=1y` `RsfAssetItem`s (`1y` boundary computed via `addMonthsEpoch rep 12` when dated). Proven lossless in `splitAmortisingLoanForRsf_conserves_principal`.
- *LCR 30-Day Cash-Flow & Derivative Extraction (`deriveLcrFromCashFlows`):*
  - Rejects inflows where `countedInHqla == true` (Attachment A Para 31 no double-counting) or `isNonFinancialRevenue == true` (Para 66 exclusion), proven in `cashFlow_hqla_and_nonFinRev_excluded`.
  - Forces securitisation SPV contractual outflows (`source == .securitisationSpv`) into 30-day `.structuredSpvConduitSivLossOfFunding` (Para 60).
  - Nets `LcrDerivativeCashFlowLeg`s via `deriveLcrDerivativeOutflowsAndInflows`: contracts sharing `(counterparty, currency)` with `hasValidMna = true`, or pairs flagged `isSameDayFxPrincipalExchange = true`, net bilaterally (`max (sum pay - sum rec) 0` outflow vs `max (sum rec - sum pay) 0` inflow); un-nettable legs remain gross.

#pagebreak()

// ============================================================================
// SECTION 5: STAGE 3A - LCR ENGINE
// ============================================================================
= Stage 3A: Liquidity Coverage Ratio (`LCR`)

Implemented across `APS210/LCR/HQLA.lean`, `APS210/LCR/Outflows.lean`, `APS210/LCR/Inflows.lean`, and `APS210/LCR.lean`.

$ "LCR" = "Stock of HQLA" / "Total Net Cash Outflows" >= r_"min" $

where $r_"min"$ defaults to `100%` for locally incorporated LCR ADIs (both *All-Currencies* and *AUD-only*) and `40%` for foreign LCR ADIs (*All-Currencies* only), raised by `max` if APRA sets a higher ADI-specific minimum.

== Stock of HQLA (`APS210/LCR/HQLA.lean`)

=== HQLA Categories, Haircuts & Classifiers

#tbl(
  (1.1fr, auto, auto, 2fr),
  [*Category*], [*Haircut*], [*Stress Max Drop*], [*Classifiers & Eligibility Rules*],
  [`hqla1`], [`0%`], [N/A (`100%`)], [`classifySovereignPseMdbHqla`: RBA/Commonwealth/State, `0%` RW sovereign/CB/PSE/MDB (or foreign non-0% RW domestic-currency debt per Para 9(d)/(e)).],
  [`hqla2a`], [`15%`], [`10%`], [`classifyHqla9d9e` / `classifyHqlaDebtSecurity`: `20%` RW sovereign/PSE/MDB or qualifying corporate/covered bonds rated $>=$ `AA-`. Requires `hqla2aDiversified = true` (Para 11).],
  [`hqla2bRmbs`], [`25%`], [`20%`], [`classifyHqla2bRmbs`: RMBS rated $>=$ `AA`, APS 120 compliant, `!containsStructuredProducts` (Para 12(a)(v)), LTV/risk-retention checks.],
  [`hqla2bCorporateDebt`], [`50%`], [`20%`], [`classifyHqlaCorpDebtByIrbPd` / `classifyHqlaDebtSecurity`: non-FI corporate bonds rated `A+..BBB-` (or IRB PD equivalent), excluding non-0%/20%-RW sov/CB/PSE/MDB entities.],
  [`hqla2bEquity`], [`50%`], [`40%`], [`classifyHqla2bEquity`: non-FI common shares in ASX 200 or major foreign index, exchange-traded, AUD/local currency, proven liquid in stress.],
)

*Asset-Level Operational & Consolidation Gating (`eligibleMarketValue`):*
An `HqlaAsset`'s value is zeroed (`0`) unless *all* operational criteria (`unencumberedAndSegregated`, `operationallyUnderTreasuryControl`, `monetisableWithoutLegalImpediment`, `diversifiedAndLowConcentration`, `noWrongWayRiskWithCounterparty`, `activeMarketWithReliablePrices`) are satisfied *or* it is within the Para 25 30-day liquidation grace period (`inThirtyDayLiquidationGracePeriod && daysSinceBecomingIneligible <= 30`). In addition:
- Level 2A assets require `hqla2aDiversified = true` (Para 11).
- Host ALA (`isHostAla = true`) is permitted only at `.level2` (Paras 26–28) and fails closed if `hostJurisdictionHasInsufficientHqla = false`.
- `freelyTransferableAtLevel2`, `issuerRestrictionSatisfied`, and `recognisedByRegulator` default to `false`; at `.level2`, non-transferable holdings are capped by `level2TransferableCap.getD 0` (Para 29).

=== 30-Day SFT Unwinding & Closed-Form 15% / 40% Caps

To prevent temporary window-dressing via short-term repos/collateral swaps (`residualMaturityDays <= 30`), `computeUnwindAdjustments` adjusts each unadjusted HQLA pool by adding back `hqla1/2a/2b` *received/returned on unwind* minus `hqla1/2a/2b` *given away on unwind* (filtered to `.aud` when `isAudOnly = true`):

$ u_1 = max("raw"_1 + Delta_1, 0), quad u_(2a) = max("raw"_(2a) + Delta_(2a), 0), quad u_(2b) = max("raw"_(2b) + Delta_(2b), 0) $

`computeHqlaCaps` then applies the exact Attachment A Paragraphs 6–8 algebraic caps:

$ "cap"_(15) = max(u_(2b) - 15 / 85 (u_1 + u_(2a)), 0) $
$ "cap"_(40) = max((u_(2a) + (u_(2b) - "cap"_(15))) - 2 / 3 u_1, 0) $
$ "stockExClf" = max(u_1 + u_(2a) + u_(2b) - "cap"_(15) - "cap"_(40), 0) $

This guarantees post-cap Level 2B is at most `15%` and combined Level 2A + Level 2B is at most `40%` of capped HQLA (formally proved in `hqla_cap15_bound` and `hqla_cap40_bound`).

=== Committed Liquidity Facility (CLF) & Foreign Sovereign Currency Cap

- *Para 9(e) Foreign Sovereign HQLA1 Cap (`computeCappedForeignSovHqla1`):* For each `(Currency, cap)` entry across `foreignSovCurrencyCaps`, takes the minimum cap for that currency (failing closed to `0` if `foreignSovCurrencyCaps` is empty) so holdings cannot exceed FX net cash outflows in the respective foreign jurisdiction.
- *CLF Recognition (`ClfInput.recognisedAmount`):* Recognised only if `approvedByApra = true` AND `processEligible = true` (requiring `collateralDiversified`, `annualReapprovalCurrent`, `includedInFundingStrategy`, and `balanceSheetEffortsExhausted`). Eligible CLF equals the 3-way minimum:
$ "CLF"_"rec" = min(A_"contract", A_"contract" - "Pledged"_(0%"-runoff") + "Lent"_(0%"-inflow"), C_"RBA" + C_"grace") $
where $C_"grace"$ adds `recentlyIneligibleCollateralAfterRbaMargins` _only_ when `collateralBecameIneligibleWithin30Days` and `collateralDaysSinceIneligible <= 30` (Para 25). CLF is admitted _only_ for locally incorporated ADIs under the *AUD LCR* (`LCR.lean`).

== Total Expected Cash Outflows (`APS210/LCR/Outflows.lean`)

=== Retail & Qualifying SME Deposits (FCS \$250k Split & Scorecard)

- *Table 1 Scorecard (`LessStableScorecardInput`):* Adds `+2` if above `$250,000` / uninsured, `+2` if online-only access, `+1` if no established relationship, `+1` if heavily rate-driven (maximum score `6`, proved by `scorecard_le_six`).
- *Table 2 Rate (`lessStableRunoffRate`):* `10%` if `score <= 2`, else `25%` (`lessStableRunoff_cases`).
- *Attachment A Para 37 FCS Split (`RetailDeposit.outflowAmount`):*
  1. If excluded term deposit (`isExcludedTermDeposit`: `isTermDeposit && (nonWithdrawableWithin30Days || penaltyExceedsInterest)` and `!termDepositCategoryTainted && intermediatedLookThroughOk`), rate is `0%`.
  2. Otherwise, if `basis == .level2 && hostJurisdictionAppliesBasel && hostBaselRate?` is `some r`, apply host rate `r`.
  3. Otherwise, for guaranteed deposits (`coveredByDepositInsurance = true`): the insured tranche $min("bal", 250000)$ receives `5%` (`stable`), while any excess $max("bal" - 250000, 0)$ incurs the automatic `+2` scorecard bump and receives `lessStableRunoffRate(baseScore + 2)` (`10%` or `25%`).
  4. If `intermediatedLookThroughMet = false` (or `notQualifying`), failed look-through forces `100%` outflow.

=== Complete Table 3 Outflow Matrix (`Table3OutflowCategory`, 46 Constructors)

#tbl(
  (1.65fr, auto, 1.65fr, auto),
  align-spec: (left, right, left, right),
  [*Category*], [*Rate*], [*Category*], [*Rate*],
  [#id("retailStable")], [`5%`], [#id("collateralSwapApraMethod")], [`100%`],
  [#id("retailLessStable")], [`10%`], [#id("otherContractualCashOutflows")], [`100%`],
  [#id("retailHigherRunoffLessStable")], [`25%`], [#id("committedFacilityRetailAndSme")], [`5%`],
  [#id("operationalDepositInsured")], [`5%`], [#id("committedCreditNonFinCorpSovCbPse")], [`10%`],
  [#id("operationalDepositUninsured")], [`25%`], [#id("committedLiquidityNonFinCorpSovCbPse")], [`30%`],
  [#id("excessOperationalNonFinInsured")], [`20%`], [#id("committedFacilityBanksAndSupervisedAdis")], [`40%`],
  [#id("excessOperationalNonFinUninsured")], [`40%`], [#id("committedCreditOtherFinancialInstitutions")], [`40%`],
  [#id("excessOperationalFi")], [`100%`], [#id("committedLiquidityOtherFinancialInstitutions")], [`100%`],
  [#id("wholesaleNoticeDepositCalled")], [`100%`], [#id("committedFacilityOtherLegalEntities")], [`100%`],
  [#id("nonOperationalCorpSovCbPseMdbSmeGt2mInsured")], [`20%`], [#id("revocableCreditAndLiquidityFacilities")], [`5%`],
  [#id("nonOperationalCorpSovCbPseMdbSmeGt2mUninsured")], [`40%`], [#id("tradeFinanceObligations") (12m avg)], [`100%`],
  [#id("nonOperationalOther") / #id("unsecuredDebtIssuance")], [`100%`], [#id("guaranteesAndLcsNonTradeFinance") (12m avg)], [`50%`],
  [#id("securedByHqla1") / #id("ClfEligibleWithinLimit")], [`0%`], [#id("buybacksDomesticDebtShortTerm")], [`r` or `10%`],
  [#id("securedByHqla2a")], [`15%`], [#id("buybacksDomesticDebtLongTerm")], [`r` or `5%`],
  [#id("securedDomesticSovPseMdbRwLe20") / #id("Hqla2bRmbs")], [`25%`], [#id("nonContractualStructuredProductsAndManagedFunds")], [`5%`],
  [#id("securedByOtherHqla2b")], [`50%`], [#id("affiliatedDealerOrMarketMaker") `r`], [`nonNeg r`],
  [#id("securedAllOther") / `structured*` / `derivatives*`], [`100%`], [#id("otherNonContractualContingent") `r`], [`nonNeg r`],
  [#id("downgradeThreeNotchCollateral") / #id("excessNonSegregatedCollateralCallable")], [`100%`], [#id("postedNonHqla1CollateralValuationChange")], [`20%`],
  [#id("contractuallyRequiredCollateralNotYetDemanded") / #id("collateralSubstitutionToNonHqla")], [`100%`], [#id("derivativeValuationLargest30dFlow24m") `alt?`], [`100%` / `alt`],
)

*Helpers:* `classifyNonOperationalWholesaleDeposit` & `classifyOperationalDeposit` route qualifying SMEs (`< $2m` & `managedAsRetail`) to retail treatment, while `.otherLegalEntity` excess operational balances always receive `100%` (`.excessOperationalFi`). `substitutableCollateralOutflow` computes `nonNeg ((1 - substitutedHaircut) - (1 - originalHaircut)) * value`.

== Inflows & Net Cash Outflows (`Inflows.lean` & `LCR.lean`)

- *Table 4 Secured Lending (`securedLendingInflowRate`):* Always `0%` if collateral covers a customer short position (`shortPosition_zero_inflow`); otherwise `0%` (`hqla1`), `15%` (`hqla2a`), `25%` (`domesticSovPseMdbRwLe20`, `hqla2bRmbs`), `50%` (`otherHqla2b`, `marginLoansNonHqlaEquityCollateral`), and `100%` (`allOtherCollateral`).
- *75% Inflow Cap (`computeNetCashOutflows`):* Only `isFullyPerforming && !countedInHqla` items enter `grossInflows`. Then:
$ "cappedInflows" = min("grossInflows", min(75% dot "grossOutflows", "overrideCap")), quad "NCO" = max("grossOutflows" - "cappedInflows", 0) $
- *Foreign Head Office Facility (`LCR.lean`):* For `.foreign` ADIs only, `ForeignHeadOfficeFacility` contributes at most `min recognizedAmount (50% * requiredMinRate * NCO)` toward Total HQLA (`foreign_ho_facility_le_50pct`).

#pagebreak()

// ============================================================================
// SECTION 6: STAGE 3B & 3C - MLH & NSFR
// ============================================================================
= Stage 3B & 3C: MLH and NSFR Engines

== Minimum Liquidity Holdings — MLH (`APS210/MLH.lean`)

Applicable to smaller ADIs classified `.mlh` (`requiredMlh >= 9%` unless APRA sets a higher minimum):

$ "MLH Ratio" = "Total Eligible Liquid Assets" / "Net MLH Liability Base" >= r_"min", quad "where triggerRatio" > r_"min" " and MLH Ratio" >= "triggerRatio" $

- *Qualifying Assets (`MlhAssetCategory`):* `cashAndRbaNotes`, `govAndSemiGovSecurities`, `adiDepositsCdsEcp`, `qualifyingInternationalAssets`, and `apraApprovedOtherSecurities`.
- *Attachment B Para 3 Eligibility (`MlhAsset.isEligible`):* Requires `unencumberedOrApraApproved` and `liquidatableWithinTwoBusinessDays` (both default `false`). Under `requiresDebtCriteria`, *debt securities* (`!(.apraApprovedOtherSecurities)` or `isDebtSecurity = true`) additionally require `rbaRepoEligible && nonSubordinated`, whereas non-debt APRA-approved securities bypass those two debt-specific checks.
- *Attachment B Para 2(f) Netting (`MlhInput.liabilityBase`):*
$ "liabilityBase" = max("liabilities" + "offBalanceSheetCommitments" - min("gross2fDeposits", max("placementsByOtherAdis", 0)), 0) $
- *Dual Check (`evaluateMlh`):* Verifies `ratioCheck.compliant` (`>= requiredMinRate`), `triggerRatioValid` (`triggerRatio > requiredMinRate`), and `aboveTriggerRatio` (`ratioCheck(mlhAssets, liabilityBase, triggerRatio)`).

== Net Stable Funding Ratio — NSFR (`APS210/NSFR.lean`)

Applicable exclusively to locally incorporated LCR ADIs (`requiredNsfr >= 100%`):

$ "NSFR" = "Total Available Stable Funding (ASF)" / "Total Required Stable Funding (RSF)" >= r_"min" $

=== Available Stable Funding (`AsfCategory`, 19 Constructors)

#tbl(
  (0.65fr, 3.3fr),
  [*Factor*], [*Categories (Attachment C Paragraphs 11–15 & 38–40)*],
  [`100%`], [`regulatoryCapitalExTier2Lt1Yr`, `otherCapitalGe1YrNoEarlyOption`, `borrowingsAndLiabilitiesGe1Yr`],
  [`95%`], [`stableRetailAndSmeDepositsLt1Yr`, `intermediatedStableRetail12mNotice`],
  [`90%`], [`lessStableRetailAndSmeDepositsLt1Yr`, `intermediatedLessStableRetail12mNotice`],
  [`50%`], [`nonFinCorporateFundingLt1Yr`, `operationalDeposits`, `superannuationOperationalDeposits`, `intermediatedRetail6mNotice`, `sovereignPseMdbNdbFundingLt1Yr`, `otherFunding6mToLt1Yr`],
  [`0%` / var], [`otherFundingLt6m`, `noStatedMaturityShortOrOpen`, `allOtherLiabilitiesAndEquity`, `deferredTaxLiability m` (`>=1y:100%`, `6m..<1y:50%`, `<6m:0%`), `minorityInterest m` (same), `cooperativeNetworkDeposit f` (`min (nonNeg f) 85%`), or `0%` if `isInterdependentApproved`],
)

=== Required Stable Funding (`RsfCategory`, 23 Constructors) & Encumbrance

#tbl(
  (0.65fr, 3.3fr),
  [*Base RSF*], [*Categories (Attachment C Paragraphs 29–37)*],
  [`0%`], [`notesAndCoinsImmediatelyAvailable`, `centralBankReservesAndBalances h?` (`max 0 h`), `centralBankClaimsLt6m`, `tradeDateReceivablesStandardSettlement`, plus customer margin exemption (`onBehalfOfCustomerNoGuarantee = true`) & interdependent/excluded assets],
  [`5%`], [`unencumberedHqla1NonZeroPctRsf`],
  [`10%`], [`loanToFiLt6mSecuredByHqla1Rehypothecatable`, `clfThirdPartyDebtOrHostAla`, `selfSecuritisedClfEligible`],
  [`15%`], [`unencumberedHqla2a`, `otherLoanToFiLt6m`],
  [`50%`], [`unencumberedHqla2b`, `hqlaEncumbered6mToLt1Yr`, `loansToFiAndCentralBanks6mToLt1Yr`, `operationalDepositsAtOtherAdis`, `asx200NonFiEquities`, `otherNonHqlaLt1Yr`],
  [`65%`], [`standardResidentialPropertyLoansLvrLe80Ge1Yr`, `otherLoansNonFiRwLe35Ge1Yr`],
  [`85%`], [`initialMarginAndCcpDefaultFund` (`>=85%` unless customer-exempt), `otherPerformingLoansNonFiGe1Yr`, `nonDefaultedNonHqlaSecuritiesGe1YrAndExchangeEquities`, `physicalTradedCommoditiesInclGold`],
  [`100%`], [`allOtherAssets` (includes APS 220 non-performing loans `dpd >= 90 || unlikelyToPay`)],
)

*Encumbrance Overlay (`applyEncumbranceFactor`):* If `exceptionalCbApraFactor` is given, returns `nonNeg r`; otherwise `.unencumbered` and `.lessThanSixMonths` keep `base`, `.sixMonthsToLessThanOneYear` yields `max 50% base`, and `.oneYearOrMore` forces `100%`.

=== Greedy Order-Independent CLF & Host ALA Allocation

Under Attachment C Paragraphs 32(a), 32(b), and 36(c):
- *Para 32(a) Unencumbered CLF-Eligible Assets* (`clfUnencumberedThirdPartyAssets`, `10%` preferential RSF) and *Para 32(b) Encumbered CLF Collateral* (`clfEncumberedSelfSecuritisedAssets`, `15%` preferential RSF) compete under `recognisedClfLimit`.
- `processClfLimitBucket` sorts assets by their fallback factor *descending* (`sortByHighestFallback`), ensuring deterministic, list-order-independent optimal utilisation of the CLF limit; any balance above `remainingLimit` receives `excessClfFallbackFactor.getD (pct 85)` (defaulting conservatively to `85%`, and never sharing the zeroed CLF cap with `hostAlaInLcrLimit`, which independently receives `50%` up to its LCR cap per Para 33).

=== Derivatives, Cash VM, SFT Netting & Off-Balance-Sheet Exposures

- *Derivatives (`NsfrDerivativeInput`):* Contracts (`DerivativeContractMtM`) net bilaterally only when `nettingAgreement.isEligible && cashVmEligibility.isEligibleForNsfrOffset`; invalid MNA or non-eligible VM forces contract-level gross separation (`grossPos` and `grossNeg`). Para 37(b) computes `netDerivativeRsf = max (nsfrDerivativeAssets - nsfrDerivativeLiabilities) 0`, and Para 37(a)/(c) adds `derivativeLiabilityAddOnRsf = 20% * effectiveGrossDerivativeLiabilities`.
- *SFT Netting (`NsfrSftNettingGroup`):* Nets gross SFT receivables and payables when `criteria.isEligible` (`SftNettingCriteria`); otherwise adds full `grossReceivable` to RSF and `grossPayable` to ASF.
- *Off-Balance-Sheet (`ObsExposureCategory`):* `5%` for revocable/irrevocable credit & liquidity facilities; `1%` for `nonContractualAndOtherApraAdjustedObs` (or APRA override); `100%` for fixed obligations (`otherFixedObsObligation`).

#pagebreak()

// ============================================================================
// SECTION 7: STAGE 4 & STAGE 5 - GOVERNANCE, COMPLIANCE & VERIFICATION
// ============================================================================
= Stage 4 & Stage 5: Governance, Compliance & Formal Verification

== Governance & ADI Profile (`APS210/Governance.lean`)

Quantitative ratio passes alone never suffice for regulatory compliance. `Governance.lean` models all mandatory qualitative frameworks:

- `BoardAndManagementGovernance`: board-approved risk tolerance, annual review, periodic reporting, clear management accountability, and Level 2 group governance alignment when consolidated (`isCompliantFor level2Applicable`).
- `LiquidityFramework`: identification, measurement/projection, scenario monitoring, and internal reporting.
- `LiquidityRiskManagement`: intraday/FX risk controls, collateral management, counterparty diversification, and cash-flow projections.
- `FundingStrategy`: market-access diversification and periodic strategy review.
- `ContingencyFundingPlan` (`ContingencyFundingPlan.isCompliant usesClf`): documented escalation/mitigation actions, annual operational testing, and explicit CLF drawdown procedures whenever `usesClf = true`.
- `StressTesting`: regular idiosyncratic + market-wide scenarios feeding risk limits and CFP triggers.
- `LocalOperationalCapacity`: local operational capability sustaining standalone operations for at least `3` business days (`localOperationalDays >= 3`).
- `NotificationStatus`: prompt APRA notifications on actual/likely LCR breaches (`lcrBreachNotified`), MLH shortfalls (`mlhShortfallNotified`), material adverse events (`materialAdverseEventNotified`), and prior stress notifications (`apraNotifiedBeforeStressUse`).
- `SecuritisationTreatment`: includes SPV 30-day cash flows in LCR and requires `Aps120CapitalReliefCriteria` for MLH/NSFR derecognition.

`AdiProfile` computes baseline statutory minimums (`requiredAudLcr`, `requiredAllCurrenciesLcr`, `requiredMlh`, `requiredNsfr`) taking `max` with any APRA higher minimum.

== Compliance Gate, Buffers & Supervisory Adjustments (`APS210/Compliance.lean`)

`evaluateAdiCompliance : AdiComplianceInput -> AdiComplianceResult` executes the end-to-end compliance decision:

+ *CLF Auto-Wiring (`withClfLimit`):* Reads Level 1 & Level 2 `AUD LCR` `recognisedClf` directly from the LCR inputs and injects it into the corresponding NSFR `recognisedClfLimit` unless an explicit override is already provided.
+ *Mandatory Buffer Gate (`BufferPolicy`):* `bufferPolicy : Option BufferPolicy := none` must be explicitly specified (fails closed if `none`). Supports `.enforceAllBuffers` (checks `lcr >= 100% + lcrInternalBuffer` and `nsfr >= 100% + nsfrInternalBuffer`), `.waivedDuringStress` (waives internal buffer checks during valid stress conditions), and `.customBufferFloor` (enforces explicit LCR/NSFR management floors).
+ *APRA Adjustments (`ApraAdjustmentRule`):* Applies supervisory minimum-rate overrides (`lcrAllCurrenciesOverride`, `lcrAudOverride`, `mlhOverride`, `nsfrOverride`) and verifies `apraAdjustmentsComplied` when prior APRA adjustments or discretion apply.
+ *Scope-Gated Evaluation (`evaluateLcr`, `evaluateMlh`, `evaluateNsfr`):*
  - `.lcr` + `.locallyIncorporated`: evaluates AUD LCR, All-Currencies LCR, and NSFR (no MLH).
  - `.lcr` + `.foreign`: evaluates All-Currencies LCR (with 40% min + 50% HO facility) only.
  - `.mlh`: evaluates MLH only (`9%` min + `triggerRatio > 9%`).
  - *Level 2 Consolidation:* If `hasLevel2Group = true`, both standalone (`level1`) and consolidated (`level2`) checks must succeed (`l2AllOk`).
+ *Scoped Stress Waiver:* Under active `stressedMarketConditions` with compliant `notification` and `cfp`, quantitative LCR and NSFR ratio breaches may be tolerated during drawdown (`stressBreachPermitted`), whereas *MLH ratio and trigger compliance (`mlhOk`, `mlhL2Ok`) are never waived by LCR stress mode*.
+ *Continuous Compliance (`evaluateContinuousCompliance`):* Evaluates a reporting history `List AdiComplianceInput`, requiring a non-empty snapshot sequence in which *every* snapshot is `overallCompliant`.

== Formal Verification & Regression Suite (`Theorems.lean`, `Tests.lean`)

=== Selected Kernel-Verified Theorems (`29` Exported Theorems)

#tbl(
  (1.45fr, 2.35fr),
  [*Theorem (`Theorems` / `Upstream`)*], [*Proved Invariant*],
  [#id("hqla_cap15_bound"), #id("hqla_cap40_bound")], [Exact algebraic proof that `computeHqlaCaps` strictly bounds post-cap HQLA2B `<= 15%` and combined HQLA2A+2B `<= 40%` of total HQLA ex-CLF.],
  [#id("inflow_cap_le_75pct"), #id("netCashOutflows_nonneg")], [`effectiveCappedInflows <= 75% * grossOut` regardless of override caps, and `totalNetCashOutflows >= 0`.],
  [#id("scorecard_le_six"), #id("lessStableRunoff_cases"), #id("lessStableRunoff_monotone")], [Scorecard bounded in `[0, 6]`; exact step threshold at score `2`; higher risk never lowers run-off.],
  [#id("shortPosition_zero_inflow")], [Collateral used to cover customer short positions always yields `0%` LCR inflow.],
  [#id("clf_recognised_bounds"), #id("clf_processEligible_false_recognised_zero"), #id("upstream_clf_requires_*")], [Unapproved CLF, missing annual reapproval, missing APRA approval, or non-diversified collateral always zero out CLF (`= 0`); recognised CLF is non-negative and bounded by contract and RBA collateral.],
  [#id("hqla_operational_false_zero"), #id("hqla_level2_nontransferable_zero_cap")], [Failing operational requirements or omitting Level 2 transfer cap on non-transferable assets forces value `= 0`.],
  [#id("splitLiabilityCashFlowsForAsf_conserves_carrying"), #id("splitAmortisingLoanForRsf_conserves_principal")], [Partitioning amortising liabilities or loans across NSFR maturity buckets conserves `100%` of principal / carrying value.],
  [#id("cashFlow_hqla_and_nonFinRev_excluded")], [Cash inflows from HQLA assets or non-financial revenues are excluded from LCR inflows.],
  [#id("encumbrance_rsf_bounds"), #id("interdependent_zero_factor"), #id("nsfr_netDerivativeRsf_nonneg")], [Encumbrance `>= 1y` forces `100%` RSF without CB exemption; interdependent assets/liabilities get `0%`; derivative net RSF `>= 0`.],
  [#id("foreign_ho_facility_le_50pct"), #id("default_lcr_minimums"), #id("nsfr_only_local_lcr")], [HO facility never exceeds `50% * r_min * NCO`; local vs foreign LCR defaults (`100%`/`40%`); NSFR applies only to local LCR ADIs.],
  [#id("checkRatio_monotone_den"), #id("overallCompliant_implies_both")], [Increasing denominator never creates false compliance; `overallCompliant = true` strictly entails both qualitative and quantitative compliance.],
  [#id("classifyNsfrBucket_exhaustive"), #id("max30dCollateralOutflow_nonneg")], [`Upstream.lean`: NSFR bucketing is total/exhaustive over maturity days; 24-month collateral outflow lookback is always non-negative.],
)

=== Kernel-Checked Regression Coverage (`Tests.lean`, `205` Examples)

Every test in `Tests.lean` compiles inside Lean's kernel via `native_decide` / `decide`, forming a regression barrier across 15 sections:
- Exhaustive verification of *all 46 `Table3OutflowCategory`* outflow rates and *all 14 (`7 x 2`) Table 4* inflow cells.
- Retail scorecard boundaries, `$250k` FCS split, Attachment A Para 42 term-deposit pool tainting, Para 9(e) empty FX cap fail-closed behavior, Para 25 30-day grace check, Para 29 Level 2 transferability, and Para 12(a)(v) structured RMBS exclusion.
- Exhaustive *Attachment C* ASF (`19`), RSF (`23`), and OBS matrix checks, plus order-independent greedy CLF RSF allocation, encumbrance `6m`/`1y` step-ups, and Para 26 SFT / Para 37 derivative netting.
- Cross-standard integration (`APS 110`, `APS 112` Tables 5/6/7 & Att F second-best, `APS 113`, `APS 120`, `APS 220`), `CashFlows` splitting/survival/MNA/FX tests, and `BufferPolicy` / `ApraAdjustmentRule` / `Level 2` compliance failure regressions.

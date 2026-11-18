// APRA APS 210 Mathematical & Calculation Specification
// Compiles offline with Typst (`typst compile docs/aps210_specification.typ`)

#set document(
  title: "APRA Prudential Standard APS 210 — Mathematical & Calculation Specification",
  author: "Prudential Liquidity Calculation Specification",
)

#let navy = rgb("#0F2C59")
#let slate = rgb("#334155")
#let muted = rgb("#64748B")
#let accent = rgb("#1D4ED8")
#let teal = rgb("#0F766E")
#let amber = rgb("#B45309")
#let surface = rgb("#F8FAFC")
#let border = rgb("#CBD5E1")
#let headerbg = rgb("#E2E8F0")

#set page(
  paper: "a4",
  margin: (top: 18mm, bottom: 16mm, left: 17mm, right: 17mm),
  header: context {
    if counter(page).get().first() > 1 [
      #set text(size: 8pt, fill: muted)
      #grid(
        columns: (1fr, auto),
        [APRA Prudential Standard APS 210 — Calculation Specification],
        [Mathematical Formulation & Processing Stages],
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
        [Exact Rational ($QQ$) Liquidity & Funding Risk Specification],
        [Page #counter(page).display("1 of 1", both: true)],
      )
    ]
  },
)

#set text(
  font: ("Helvetica Neue", "Helvetica", "Arial"),
  size: 9.4pt,
  fill: rgb("#1E293B"),
  hyphenate: false,
)

#set par(leading: 0.52em, justify: false)
#set list(spacing: 0.4em)
#set enum(spacing: 0.4em)
#set heading(numbering: "1.1")

#show heading.where(level: 1): it => {
  v(9pt)
  block(
    width: 100%,
    below: 6pt,
    stroke: (bottom: 1.5pt + navy),
    inset: (bottom: 4pt),
  )[
    #set text(size: 13.5pt, weight: "bold", fill: navy)
    #it
  ]
}

#show heading.where(level: 2): it => {
  v(4.5pt)
  block(below: 3pt)[
    #set text(size: 10.6pt, weight: "bold", fill: accent)
    #it
  ]
}

#show heading.where(level: 3): it => {
  v(4pt)
  block(below: 3pt)[
    #set text(size: 9.7pt, weight: "bold", fill: slate)
    #it
  ]
}

#let callout(title, tone: accent, body) = block(
  width: 100%,
  fill: tone.lighten(93%),
  stroke: (left: 3pt + tone, rest: 0.5pt + tone.lighten(60%)),
  inset: 7.5pt,
  radius: 3pt,
  [
    #text(weight: "bold", fill: tone, size: 9pt)[#title]
    #v(2pt)
    #body
  ],
)

#let stagebox(num, title, subtitle, tone: navy) = rect(
  width: 100%,
  fill: surface,
  stroke: (left: 3.5pt + tone, rest: 0.6pt + border),
  inset: 7pt,
  radius: 3pt,
  [
    #grid(
      columns: (auto, 1fr),
      gutter: 7pt,
      rect(fill: tone, radius: 2pt, inset: (x: 5pt, y: 2pt))[
        #text(fill: white, weight: "bold", size: 8pt)[#num]
      ],
      [
        #text(weight: "bold", fill: navy, size: 9.4pt)[#title] \
        #text(size: 8.3pt, fill: slate)[#subtitle]
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
    inset: (x: 5pt, y: 3.1pt),
    fill: (_, y) => if y == 0 { headerbg } else if calc.odd(y) { white } else { surface },
    ..rows,
  ),
)

// ============================================================================
// TITLE PAGE
// ============================================================================
#align(left)[
  #v(12mm)
  #rect(fill: navy, radius: 3pt, inset: (x: 9pt, y: 4pt))[
    #text(fill: white, weight: "bold", size: 8.5pt, tracking: 0.8pt)[PRUDENTIAL CALCULATION SPECIFICATION]
  ]
  #v(5mm)
  #text(size: 23pt, weight: "bold", fill: navy)[
    APRA Prudential Standard APS 210 \
    Mathematical & Pipeline Specification
  ]
  #v(3mm)
  #text(size: 11.2pt, fill: slate)[
    Formal Mathematical Definitions, Piecewise Lookups, Multi-Stage Processing Pipeline, \
    Cash-Flow Projections, and Regulatory Compliance Logic
  ]
  #v(7mm)
  #line(length: 100%, stroke: 1.5pt + navy)
  #v(5mm)

  #grid(
    columns: (1fr, 1fr),
    gutter: 12pt,
    callout("Regulatory & Mathematical Scope", tone: navy)[
      Specifies every calculation across APRA Prudential Standard APS 210 (*Main Body*,
      *Attachment A — LCR*, *Attachment B — MLH*, *Attachment C — NSFR*), together with
      cross-referenced rules from APS 110, APS 112, APS 113, APS 120, and APS 220.
    ],
    callout("Arithmetic & Conservative Semantics", tone: teal)[
      All balances, weights, haircuts, and ratio bounds operate over exact rational numbers ($QQ$)
      with zero rounding drift. Regulatory attestations follow strict *fail-closed* semantics:
      unverified conditions evaluate to zero relief or non-compliance.
    ],
  )

  #v(7mm)
  #text(weight: "bold", size: 10pt, fill: navy)[End-to-End Calculation Pipeline Summary]
  #v(3pt)
  #tbl(
    (auto, 1.3fr, 2fr),
    [*Stage*], [*Domain Scope*], [*Primary Mathematical Responsibilities*],
    [Stage 0], [Foundations & Cross-Standard Rules], [Exact rational operators, credit rating total ordering, denominator-safe ratio gate, APS 112 risk weights & ECAI resolution, APS 113/120/220 predicates],
    [Stage 1], [Upstream Normalisation (9 Sub-Stages)], [Exact Gregorian calendar arithmetic, option-adjusted maturities, bilateral FX netting, \$2m SME & \$250k FCS deposit splitting, Para 42 tainting, 24m/12m historical lookbacks],
    [Stage 2], [Cash-Flow Projection Engine], [6-bucket maturity ladder, Para 36 positive survival buffer, conservative option/open-maturity windows, lossless NSFR liability/loan decomposition, LCR MNA + FX netting],
    [Stage 3A], [Liquidity Coverage Ratio (LCR)], [HQLA haircuts & eligibility filters, 30-day SFT unwinds, closed-form 15% & 40% HQLA caps, CLF 3-way minimum, \$250k FCS split, Table 3 outflows (46 classes), Table 4 inflows (75% cap), 50% Foreign Head Office cap],
    [Stage 3B], [Minimum Liquidity Holdings (MLH)], [2-day liquidation & debt/RBA-repo qualification, Attachment B Para 2(f) bilateral netting, statutory floor + early-warning trigger check],
    [Stage 3C], [Net Stable Funding Ratio (NSFR)], [19 ASF categories, 23 RSF categories, encumbrance step-ups, order-independent greedy CLF RSF allocation, MNA/VM derivatives, Para 26 SFT netting, OBS factors],
    [Stage 4], [Governance, Buffers & Compliance Gate], [Qualitative framework conjunctive check, AUD CLF pass-through to NSFR, internal buffer policies, supervisory overrides, Level 1 & Level 2 consolidation, continuous compliance],
  )

  #v(6mm)
  #outline(title: [Contents], indent: 1.4em, depth: 2)
]

#pagebreak()

// ============================================================================
// SECTION 1: PIPELINE ARCHITECTURE & CONVENTIONS
// ============================================================================
= Calculation Architecture & Mathematical Conventions

The APS 210 calculation model is structured as an ordered, deterministic multi-stage pipeline transforming raw contractual instruments and supervisory metadata into standalone (Level 1) and consolidated (Level 2) liquidity compliance verdicts.

== Fundamental Mathematical Notation & Operators

Throughout this specification, all monetary quantities $x in QQ$ and prudential factors $r in QQ$ obey exact rational arithmetic:

- *Positive-Part Floor:* $[x]_+ := max(x, 0)$.
- *Percentage Operator:* $p\% := p / 100$ for any $p >= 0$.
- *Indicator Function:* $bold(1)_P := 1$ if predicate $P$ holds, and $0$ otherwise.
- *Fail-Closed Parameter Semantics:* An unprovided threshold or missing attestation contributes $0$ (for assets, inflows, or exemptions) or fails the enclosing compliance predicate.
- *Denominator-Safe Ratio Verification Operator:* Given numerator $N in QQ$, denominator $D in QQ$, and required minimum threshold $r_"min" in QQ$:
$
cal(R)(N, D, r_"min") := cases(
  "Non-Compliant" & "if" D <= 0,
  "Compliant iff" N >= r_"min" dot.op D & "if" D > 0
)
$
When $D > 0$, the realised ratio is $rho = N / D$. Zero or negative denominators never yield a pass or division fault.

== Pipeline Flow Diagram

#figure(
  block(width: 100%, stroke: 0.6pt + border, radius: 4pt, inset: 8pt, fill: white)[
    #stack(
      dir: ttb,
      spacing: 4.5pt,
      stagebox(
        "STAGE 0",
        "Prudential Foundations & Cross-Standard Mathematical Lookups",
        "Exact rational operators, rating order (AAA .. below B-), ECAI multi-assessment rule, APS 112 RW tables, APS 113/120/220 predicates",
        tone: slate,
      ),
      align(center)[#text(fill: accent, weight: "bold")[↓]],
      stagebox(
        "STAGE 1",
        "Upstream Normalisation & Proleptic Gregorian Calendar Engine (Steps 1.1 – 1.9)",
        "Exact month-end calendar shifts, option conservatism, $2m SME & $250k FCS splits, Para 42 tainting, Para 9(e) FX caps, 24m/12m historical lookbacks",
        tone: accent,
      ),
      align(center)[#text(fill: accent, weight: "bold")[↓]],
      stagebox(
        "STAGE 2",
        "Multi-Horizon Contractual Cash-Flow & Splitting Engine",
        "6-horizon cash-flow ladder, Para 36 positive survival buffer, conservative effective days, NSFR <6m / 6m–<1y / >=1y splits, LCR MNA + FX netting",
        tone: teal,
      ),
      align(center)[#text(fill: accent, weight: "bold")[↓]],
      grid(
        columns: (1fr, 1fr, 1fr),
        gutter: 6pt,
        stagebox(
          "STAGE 3A",
          "LCR Engine",
          "HQLA unwind + closed-form 15/40 caps + CLF, FCS split, Table 3 outflows (46), Table 4 inflows (75% cap), 50% HO cap",
          tone: navy,
        ),
        stagebox(
          "STAGE 3B",
          "MLH Engine",
          "9% liquid floor, 2-day & debt/RBA-repo filter, Para 2(f) bilateral netting, trigger > min",
          tone: navy,
        ),
        stagebox(
          "STAGE 3C",
          "NSFR Engine",
          "ASF (19) & RSF (23), encumbrance step-up, greedy CLF RSF buckets, MNA/VM derivatives, SFT & OBS",
          tone: navy,
        ),
      ),
      align(center)[#text(fill: accent, weight: "bold")[↓]],
      stagebox(
        "STAGE 4",
        "Qualitative Governance, Management Buffers, APRA Adjustments & Final Gate",
        "Level 1 & Level 2 consolidation, AUD CLF pass-through to NSFR, internal buffer enforcement, supervisory rate overrides, scoped stress waiver (LCR/NSFR only), continuous history verification",
        tone: amber,
      ),
    )
  ],
  caption: [APS 210 end-to-end mathematical pipeline architecture.],
)

#pagebreak()

// ============================================================================
// SECTION 2: STAGE 0 - FOUNDATIONS & EXTERNAL STANDARDS
// ============================================================================
= Stage 0: Prudential Foundations & External Standards

== Institutional Taxonomy & Credit Ordering

Every calculation is parameterised by:
- *Consolidation Basis:* $b in {"Level 1 (Licensed ADI Standalone)", "Level 2 (Consolidated Banking Group)"}$.
- *Incorporation & Regime:* $g in {"Locally Incorporated", "Foreign"}$ and $c in {"LCR", "MLH"}$.
- *Counterparty Set:* $cal(P) = \{$Retail, SME, Non-Financial Corporate, Sovereign, Central Bank, PSE, MDB, Bank, Other Financial Institution, Other Legal Entity$\}$.
- *Credit Quality Scale:* Rated exposures are totally ordered from highest to lowest credit quality across 13 tiers:
$ "AAA" prec "AA+" prec "AA" prec "AA-" prec "A+" prec "A" prec "A-" prec "BBB+" prec "BBB" prec "BBB-" prec "<=BB+" prec "B" prec "<B-" $
plus Unrated ($bot$). From this ordering, each ECAI rating maps to APS 112 Table 21 Credit Rating Grades $gamma in {1, 2, 3, 4, 5, 6, bot}$:

$
"Grade"(r) := cases(
  1 & r in {"AAA", "AA+", "AA", "AA-"},
  2 & r in {"A+", "A", "A-"},
  3 & r in {"BBB+", "BBB", "BBB-"},
  4 & r = "<=BB+",
  5 & r = "B",
  6 & r = "<B-",
  bot & r = "Unrated"
)
$

== Cross-Standard Mathematical Lookups

=== ECAI Multiple-Assessment Resolution (APS 112 Attachment F)

Given a multiset of ECAI assessments ${r_1, dots, r_k}$ sorted in ascending grade order $gamma_((1)) <= gamma_((2)) <= dots <= gamma_((k))$ (excluding unrated entries once rated ones exist):

$
"ECAI"(r_1, dots, r_k) := cases(
  bot & "if" k = 0,
  r_((1)) & "if" k = 1,
  r_((2)) & "if" k = 2 quad ("worse of the two assessments"),
  r_((2)) & "if" k >= 3 quad ("second-best assessment")
)
$

=== APS 112 Attachment B Risk-Weight Lookups (Tables 5, 6, 7)

Given resolved grade $gamma in {1..6, bot}$:

$
"RW"_"Sov"(gamma) = cases(
  0\% & gamma = 1,
  20\% & gamma = 2,
  50\% & gamma = 3,
  100\% & gamma in {4, 5, bot},
  150\% & gamma = 6
) quad
"RW"_"PSE"(gamma) = cases(
  20\% & gamma = 1,
  50\% & gamma = 2,
  100\% & gamma in {3, 4, 5, bot},
  150\% & gamma = 6
) quad
"RW"_"Bank"(gamma) = cases(
  20\% & gamma = 1,
  50\% & gamma in {2, 3, bot},
  100\% & gamma in {4, 5},
  150\% & gamma = 6
)
$

=== Cross-Standard Qualification Indicator Functions

#tbl(
  (0.9fr, 0.8fr, 2.5fr),
  [*Cross-Reference*], [*Indicator*], [*Exact Mathematical / Logical Condition*],
  [APS 112 Mortgages], [$chi_"Mort65"$], [$bold(1)_("LVR" <= 80\%)$ $dot.op$ EnforceableSecurity $dot.op$ FirstMortgage $dot.op$ IndependentValuation],
  [APS 110/112 Netting], [$chi_"MNA"$], [WrittenEnforceable $and$ SingleLegalObligation $and$ CrossJurisdictionEnforceable $and not$ WalkawayClause $and$ CurrentReview],
  [APS 110 Att D (VM)], [$chi_"VM"$], [DailyMTM $and$ SameCurrencyCash $and$ ZeroThresholdFullTransfer $and not$ SegregatedByReceiver],
  [APS 110 Att D Para 38], [$chi_"SFT"$], [SameExplicitFinalSettlement $and not$ OpenEndedUnwindable $and$ EnforceableSetOff $and$ DVPOrSimultaneous],
  [APS 113 Para 40], [$chi_"SME113"(E)$], [$bold(1)_(0 <= E < 1","500","000)$ (consolidated group AUD exposure strictly below $\$1.5"m"$)],
  [APS 220 Credit], [$chi_"Perf"(d_"dpd", "utp")$], [$bold(1)_(d_"dpd" < 90 and not "utp")$ (failing loans become $100\%$ RSF non-performing)],
  [APS 120 Securitisation], [$chi_"APS120"$], [SigCreditRiskTransferred $and$ OriginatorExclusionMet $and not$ ImplicitSupport],
)

#pagebreak()

// ============================================================================
// SECTION 3: STAGE 1 - UPSTREAM PREPROCESSING
// ============================================================================
= Stage 1: Upstream Data Preprocessing (Sub-Stages 1.1 – 1.9)

Stage 1 normalises heterogeneous position tables, deposit accounts, rating feeds, and historical collateral series into canonical cash-flow and prudential inputs.

== Proleptic Gregorian Calendar Algebra

To guarantee exact 6-month, 12-month, and 24-month horizon comparisons across leap years and varying month lengths, dates are represented as integer epoch days $t in ZZ$ paired with an exact proleptic Gregorian bijection $(y, m, d) = cal(C)(t)$:

$ D_"max"(y, m) := cases(
  29 & m = 2 and ((4 divides y and 100 divides.not y) or 400 divides y),
  28 & m = 2 "otherwise",
  30 & m in {4, 6, 9, 11},
  31 & "otherwise"
) $

Shifting a calendar date $t$ with civil representation $(y, m, d)$ forward by $k in NN$ months computes:
$ (y', m') = (y + floor(((m - 1) + k) / 12), ((m - 1 + k) mod 12) + 1), quad "AddMonths"(t, k) := cal(C)^(-1)(y', m', min(d, D_"max"(y', m'))) $

For example, shifting 31 August or 29 February by $6$ or $12$ months clamps cleanly to the true end of the target month.

== The Nine Upstream Mathematical Transformations

#tbl(
  (0.55fr, 1.25fr, 2.4fr),
  [*Step*], [*Transform*], [*Exact Formula / Lookup Rule*],
  [1.1], [Option-Adjusted Maturity & NSFR Bucketing], [Let $d_"base"$ be contractual maturity. For liabilities, $d_"eff" = min(d_"base", d_"investorPut", d_"issuerExpectedCall")$; for assets, $d_"eff" = max(d_"base", d_"extension")$. Given reporting date $t_"rep"$ and maturity date $t_"mat"$, NSFR bucket $cal(B)(t_"mat")$ assigns:
  $<6"m"$ if $t_"mat" < "AddMonths"(t_"rep", 6)$;
  $6"m–"<1"y"$ if $"AddMonths"(t_"rep", 6) <= t_"mat" < "AddMonths"(t_"rep", 12)$;
  $>= 1"y"$ otherwise (or day thresholds $182, 365$ if undated).],
  [1.2], [30-Day Derivative & FX Netting], [For each netting group $g$ with payable legs $P_i$ and receivable legs $R_i$:
  $O_g = [sum_(i in g) P_i - sum_(i in g) R_i]_+, quad I_g = [sum_(i in g) R_i - sum_(i in g) P_i]_+$.],
  [1.3], [Deposit Segmentation, FCS Split & Pool Tainting], [Qualifies deposits where $("Retail" or ("SME" and E_"group" < 2 times 10^6 and "ManagedAsRetail"))$. Across a depositor pool with total guaranteed balance $B_"tot" = sum_k B_k$, each account $i$ receives insured portion $B_i dot.op min(1, 250","000 \/ B_"tot")$ (if $B_"tot" > 0$). Any penalty-free early withdrawal toggles pool-wide tainting $tau_"TD" = 1$, revoking term-deposit exclusions across that pool (Att A Para 42).],
  [1.4], [Foreign Sovereign Currency Cap], [For each foreign currency $c$ with sovereign HQLA1 holding $H_c$ and non-empty outflow cap series ${K_j}_(j in J_c)$, recognised HQLA1 is $min(H_c, min_(j in J_c) K_j)$; if $J_c = emptyset$, cap fails closed to $0$ (Att A Para 9(e)).],
  [1.5], [24-Month Collateral Lookback], [Validates historical sample span ($>= 700"d"$ / $>= 23$ calendar months, and $>= 24$ calendar months when dated) and computes maximum realised absolute 30-day collateral change:
  $V_"24m" = max_(t in "window") |sum_(s in [t-29, t]) Delta C_s| >= 0$.],
  [1.6], [12-Month Average Net Outflow], [Validates minimum $12$-month observation series and computes arithmetic mean monthly net outflow $frac(1, n) sum_(m=1)^n [O_m - I_m]_+$ for Table 3 trade finance and non-trade guarantees.],
  [1.7], [ECAI Rating Normalization], [Maps agency rating tokens into the 13-tier credit order, applies $"ECAI"(R)$ to resolve multi-agency feeds, and outputs APS 112 Table 5/6/7 risk weights by exposure class.],
  [1.8], [Amortisation Decomposition], [Expands scheduled loan and borrowing tranches $(A_k, t_k)$ along exact maturity dates for downstream Stage 2 NSFR bucketing.],
  [1.9], [SFT Leg & CLF Bridging], [Projects 30-day secured funding/lending legs with exchanged collateral types, currency, and residual days alongside RBA-haircut CLF collateral summaries.],
)

#pagebreak()

// ============================================================================
// SECTION 4: STAGE 2 - CASH-FLOW ENGINE
// ============================================================================
= Stage 2: Cash-Flow Projection & Maturity Splitting Engine

Stage 2 models contractual cash flows over dynamic time horizons to support liquidity stress ladders (Main Body Para 36), NSFR contractual maturity decomposition (Att C Paras 8, 11(c), 22), and LCR 30-day derivative & SPV flow extraction.

== Multi-Horizon Ladder & Survival Buffer (Main Body Para 36)

Each contractual cash flow $f$ with contractual days $d_"c"$, earliest call/put option days $d_"opt"$, and latest extension days $d_"ext"$ is assigned a conservative effective day horizon $d_"eff"$:

- *Outflow Horizon Pulling Forward:* $d_"eff"^"out" = min(d_"c", d_"opt")$ — any investor put, issuer expected call, or open-maturity callability accelerates expected cash outflow.
- *Inflow Horizon Pushing Back & Open-Maturity Filter:* If $f$ is open-maturity and is *not* a minimum contractual payment, it is excluded from inflow credit ($I_"eff" = 0$). Otherwise, $d_"eff"^"in" = max(d_"c", d_"ext")$ — borrower extension options delay expected inflows.

Effective days map into six cumulative buckets $cal(H)(d) in {1..6}$:
$ h_1: d <= 1"d", quad h_2: 2"–"7"d", quad h_3: 8"–"14"d", quad h_4: 15"–"30"d", quad h_5: 31"–"90"d", quad h_6: d > 90"d". $

Given opening unencumbered liquid buffer $B_0 >= 0$, per-bucket inflow sum $I_k$ and outflow sum $O_k$, the projected cumulative liquidity surplus at horizon $k$ is:
$ "NetFlow"_k = I_k - O_k, quad quad "CumLiq"_k = B_0 + sum_(j = 1)^k "NetFlow"_j. $
The *Para 36 Survival Buffer Criterion* holds up to cutoff horizon $K$ if and only if:
$ forall k in {1, dots, K}, quad "CumLiq"_k >= 0. $

== NSFR Contractual Decomposition & LCR Flow Extraction

=== Lossless Maturity Partitioning (Att C Paras 8, 11(c), 22)

- *Amortising Liability Decomposition (ASF):* Each principal tranche $(A_i, d_i)$ of a liability with remaining total carrying amount $A = sum_i A_i$ is partitioned using calendar boundaries $T_"6m", T_"12m"$ (or $182"d", 365"d"$ when undated) into exact buckets $A_(<6"m") + A_(6"m–"<1"y") + A_(>= 1"y") = A$, weighting each tranche by its bucket ASF factor.
- *Amortising Loan Decomposition (RSF):* Principal inflows $(P_i, d_i)$ of amortising performing loans are partitioned across $T_"12m"$ into $<1"y"$ ($P_(<1"y")$) and $>= 1"y"$ ($P_(>= 1"y")$) tranches such that $P_(<1"y") + P_(>= 1"y") = P_"total"$, ensuring identical conservation of carrying value under bucket weighting.

=== LCR 30-Day Derivation & Derivative Bilateral Netting

Extracting LCR cash flows over $d_"eff" <= 30$:
+ *Mandatory Exclusions (Att A Paras 31 & 66):* Inflows generated by assets counted in the Stock of HQLA ($I_"HQLA"$) or arising from non-financial operating revenues ($I_"nonFinRev"$) are strictly zeroed ($0$).
+ *Securitisation SPV Pass-Through (Att A Para 60):* Contractual outflows from ADI-sponsored securitisation SPVs map unconditionally to $100\%$ 30-day outflow.
+ *MNA & Same-Day FX Netting:* Derivative legs ${("cp"_j, "ccy"_j, "mna"_j, "fxSameDay"_j, P_j, R_j)}$ are partitioned by netting set. Every set sharing $("cp", "ccy")$ with a valid Master Netting Agreement ($chi_"MNA" = 1$) or qualifying as a same-day FX principal exchange nets bilaterally:
$ O_"net"(S) = [sum_(j in S) P_j - sum_(j in S) R_j]_+, quad quad I_"net"(S) = [sum_(j in S) R_j - sum_(j in S) P_j]_+, $
while un-nettable contracts remain strictly gross ($O = sum P_j, I = sum R_j$).

#pagebreak()

// ============================================================================
// SECTION 5: STAGE 3A - LCR ENGINE
// ============================================================================
= Stage 3A: Liquidity Coverage Ratio (LCR)

For every applicable LCR scope (Consolidated Level 1 / Level 2; All-Currencies and Local AUD-only):
$ "LCR" = frac("Stock of HQLA", "Total Net Cash Outflows") >= r_"min" $
where baseline statutory floors are $r_"min" = 100\%$ for locally incorporated LCR ADIs (both *All-Currencies* and *AUD-only*), and $r_"min" = 40\%$ for foreign LCR ADIs (*All-Currencies* only), increased via $max(r_"min", r_"APRA")$ whenever APRA sets a higher ADI-specific minimum.

== Stock of HQLA

=== Asset Haircuts, Stress Thresholds & Eligibility Filter

For any asset $a$ with market value $V_a >= 0$, category $c_a$, consolidation basis $b$, and currency filter $chi_"ccy"(a)$ (restricting to AUD when evaluating AUD LCR):

#tbl(
  (1.25fr, auto, auto, 2.15fr),
  [*HQLA Class ($c_a$)*], [*Haircut $h(c_a)$*], [*Stress Max Drop*], [*Prudential Qualification Formula*],
  [Level 1 (HQLA1)], [$0\%$], [N/A ($100\%$)], [RBA/Commonwealth/State debt, $0\%$ RW sovereign/CB/PSE/MDB ($"RW" = 0\%$), or domestic-currency non-$0\%$-RW sovereign per Para 9(d)/(e).],
  [Level 2A (HQLA2A)], [$15\%$], [$<= 10\%$], [$"RW" = 20\%$ sovereign/PSE/MDB or non-FI corporate/covered bonds with $r succ.eq "AA-"$; requires diversification indicator $chi_"div2A" = 1$ (Para 11).],
  [Level 2B RMBS], [$25\%$], [$<= 20\%$], [RMBS with $r succ.eq "AA"$, APS 120 compliant, zero structured product exposure ($not chi_"struct"$, Para 12(a)(v)), LVR/retention tests.],
  [Level 2B Corporate Debt], [$50\%$], [$<= 20\%$], [Non-FI corporate debt rated $"A+" dots "BBB-"$ (or equivalent IRB PD); strictly excludes non-$0\%$ or $20\%$ RW sov/CB/PSE/MDB issuers.],
  [Level 2B Equity], [$50\%$], [$<= 40\%$], [Non-FI common equity in ASX 200 or major foreign index, exchange-traded in local/AUD currency.],
)

Eligible market value $tilde(V)_a(b)$ fails closed to $0$ unless *all* gating predicates hold:
$
tilde(V)_a(b) := bold(1)_(chi_"op"(a) or chi_"grace30"(a)) dot.op bold(1)_("issuerOK" and "regRecognised") dot.op bold(1)_(c_a = "2A" arrow.r.double chi_"div2A") dot.op bold(1)_("hostALA" arrow.r.double b = L_2 and chi_"shortage") dot.op T_a(b)
$
where $chi_"op"(a)$ requires unencumbered segregation, treasury control, legal monetisability, diversification, absence of wrong-way risk, and active market pricing; $chi_"grace30"(a)$ admits Para 25 recently-ineligible assets strictly while $"daysSinceIneligible" <= 30$; and the Level 2 transferability term $T_a(b)$ enforces Para 29 fail-closed capping:
$
T_a(b) := cases(
  [V_a]_+ & "if" b = L_1 or "freelyTransferable"_(L_2),
  min([V_a]_+, [overline(T)_(L_2)]_+) & "otherwise (with" overline(T)_(L_2) = 0 "if unspecified)."
)
$
After applying haircuts, unadjusted asset values are summed per tier as $"raw"_k = sum_(a in k) tilde(V)_a(b)(1 - h(c_a))$ for $k in {1, 2a, 2b}$ (applying Para 9(e) currency capping to foreign non-$0\%$-RW sovereign holdings).

=== 30-Day SFT Unwinding & Closed-Form 15% / 40% Caps (Att A Paras 6–8)

To eliminate window-dressing from repos and collateral swaps maturing within $30$ days ($d <= 30$), each tier $k in {1, 2a, 2b}$ adds net cash/HQLA unwind deltas $Delta_k = sum_"legs" ("Returned"_k - "Given"_k)$:
$ u_1 = ["raw"_1 + Delta_1]_+, quad quad u_(2a) = ["raw"_(2a) + Delta_(2a)]_+, quad quad u_(2b) = ["raw"_(2b) + Delta_(2b)]_+. $

Excess Level 2B (at most $15\%$ of final Stock of HQLA) and combined Level 2 (at most $40\%$) are deducted via exact closed-form rational formulas:
$
"cap"_(15) = [u_(2b) - frac(15, 85)(u_1 + u_(2a))]_+, quad quad
"cap"_(40) = [(u_(2a) + (u_(2b) - "cap"_(15))) - frac(2, 3)u_1]_+,
$
$
H_"exCLF" = [u_1 + u_(2a) + u_(2b) - "cap"_(15) - "cap"_(40)]_+.
$
These closed forms algebraically guarantee $u_(2b)^"post" <= 15\% dot.op H_"exCLF"$ and $u_(2a)^"post" + u_(2b)^"post" <= 40\% dot.op H_"exCLF"$.

=== Committed Liquidity Facility (CLF) & Foreign Sovereign Currency Cap

When evaluating *Local AUD LCR* ($g = "Local" and "AUD-only"$), a CLF approved by APRA with satisfied process/diversification/re-approval predicates ($chi_"CLF" = 1$) contributes:
$
"CLF"_"rec" = chi_"CLF" dot.op min([A_"contract"]_+, [A_"contract" - "Pledged"_(0\%) + "Lent"_(0\%)]_+, [C_"RBA"]_+ + chi_"grace30" [C_"recentInelig"]_+),
$
where $chi_"grace30"$ admits recently-ineligible collateral only for $<= 30$ days (Para 25). Total Stock of HQLA for Local AUD LCR is $H_"exCLF" + "CLF"_"rec"$; for all other LCR scopes, $"CLF"_"rec"$ is strictly $0$.

== Total Expected Cash Outflows

=== Retail & Qualifying SME Deposits: Scorecard & FCS \$250,000 Split (Att A Paras 35–42)

For less-stable tranches, Table 1 defines an integer risk score $sigma in {0..6}$:
$ sigma = 2 dot.op bold(1)_(>\$250"k/uninsured") + 2 dot.op bold(1)_("onlineOnly") + 1 dot.op bold(1)_("noEstRel") + 1 dot.op bold(1)_("rateDriven"), quad quad rho_"less"(sigma) = cases(10\% & sigma <= 2, 25\% & sigma > 2.) $

Given retail/SME balance $B >= 0$, deposit outflow $O_"ret"(B, b)$ follows the piecewise hierarchy:
+ *Excluded Term Deposit:* $0$ if term deposit is non-withdrawable within 30d or penalty $>$ interest, pool is untainted ($not tau_"TD"$), and intermediated look-through holds.
+ *Level 2 Host Basel Override:* $r_"host" dot.op B$ if $b = L_2 and "hostBaselApplies"$.
+ *Failed Intermediated Look-Through / Non-Qualifying:* $100\% dot.op B$.
+ *FCS Deposit Split (Para 37):* When covered by the Financial Claims Scheme guarantee ($G = \$250","000$), the deposit splits into insured stable and uninsured excess tranches:
$ O_"ret"(B) = min(B, 250","000) dot.op 5\% + [B - 250","000]_+ dot.op rho_"less"(sigma_"base" + 2), $
or $rho_"less"(sigma_"base") dot.op B$ if uninsured from the first dollar.

=== Complete Table 3 Outflow Matrix (46 Categories)

#tbl(
  (1.65fr, auto, 1.65fr, auto),
  align-spec: (left, right, left, right),
  [*Table 3 Category*], [*Factor*], [*Table 3 Category*], [*Factor*],
  [Retail Stable (row entry)], [$5\%$], [Collateral Swap (APRA method)], [$100\%$],
  [Retail Less Stable ($sigma <= 2$)], [$10\%$], [Other Contractual Cash Outflows], [$100\%$],
  [Retail Higher-Runoff Less Stable ($sigma >= 3$)], [$25\%$], [Committed Credit/Liq: Retail & SME], [$5\%$],
  [Operational Deposit — Insured], [$5\%$], [Committed Credit: Non-Fin Corp/Sov/CB/PSE], [$10\%$],
  [Operational Deposit — Uninsured], [$25\%$], [Committed Liquidity: Non-Fin Corp/Sov/CB/PSE], [$30\%$],
  [Excess Operational Non-Fin — Insured], [$20\%$], [Committed Facility: Banks & Supervised ADIs], [$40\%$],
  [Excess Operational Non-Fin — Uninsured], [$40\%$], [Committed Credit: Other Financial Institutions], [$40\%$],
  [Excess Operational Financial Institution / Other Entity], [$100\%$], [Committed Liquidity: Other Financial Institutions], [$100\%$],
  [Wholesale Notice Deposit Called (APG 210)], [$100\%$], [Committed Facility: Other Legal Entities], [$100\%$],
  [Non-Op Corp/Sov/CB/PSE/MDB/SME$>\$2"m"$ Insured], [$20\%$], [Revocable Credit & Liquidity Facilities], [$5\%$],
  [Non-Op Corp/Sov/CB/PSE/MDB/SME$>\$2"m"$ Uninsured], [$40\%$], [Trade Finance Obligations (12m avg net)], [$100\%$],
  [Non-Op Other / Unsecured Debt Issuance], [$100\%$], [Guarantees & LCs Non-Trade Finance (12m avg)], [$50\%$],
  [Secured Funding: HQLA1 / CLF-Eligible (in limit)], [$0\%$], [Buybacks Domestic Debt — Short-Term], [$r_"APRA"$ or $10\%$],
  [Secured Funding: HQLA2A], [$15\%$], [Buybacks Domestic Debt — Long-Term], [$r_"APRA"$ or $5\%$],
  [Secured Funding: Dom. Sov/PSE/MDB RW$<= 20\%$ / 2B RMBS], [$25\%$], [Non-Contractual Structured Prods & Managed Funds], [$5\%$],
  [Secured Funding: Other HQLA2B], [$50\%$], [Affiliated Dealer / Market Maker], [$[r_"adi/APRA"]_+$],
  [Secured Funding: All Other], [$100\%$], [Other Non-Contractual Contingent], [$[r_"adi/APRA"]_+$],
  [Maturing Structured Notes Issued by ADI / SPV Loss], [$100\%$], [Posted Non-HQLA1 Collateral Valuation Change], [$20\%$],
  [Derivatives Cash Outflows], [$100\%$], [Largest 30d Net Collateral Flow (24m)], [$100\%$ or $A_"alt"$],
  [3-Notch Downgrade Trigger Collateral], [$100\%$], [Collateral Substitution to Non-HQLA], [$100\%$],
  [Excess Non-Segregated Callable Collateral], [$100\%$], [Contractually Required Collateral Not Yet Demanded], [$100\%$],
)
(For substitutable collateral swaps of value $V$, incremental outflow equals $[(1 - h_"sub") - (1 - h_"orig")]_+ dot.op V$.)

== Inflows, 75% Cap & Foreign Head Office Facility

- *Table 4 Secured Lending Inflow Matrix ($7 times 2$):* Any collateral used to cover customer short positions yields *$0\%$* inflow unconditionally. Otherwise, contractual maturing secured lending inflows receive: $0\%$ (HQLA1), $15\%$ (HQLA2A), $25\%$ (Dom. Sov/PSE/MDB RW$<= 20\%$ or HQLA2B RMBS), $50\%$ (Other HQLA2B or margin loans against non-HQLA equities), and $100\%$ (all other collateral).
- *Performing & Non-HQLA Guard:* Only inflows with performing status ($chi_"perfIn" = 1$) and excluded from HQLA ($chi_"nonHQLA" = 1$) enter gross inflows $I_"gross"$.
- *75% Inflow Cap Formula (Att A Para 30):*
$ I_"capped" = min(I_"gross", min(75\% dot.op O_"gross", overline(I)_"override")), quad quad "NCO" = [O_"gross" - I_"capped"]_+ >= 0. $
- *Foreign ADI Head Office Facility Cap (Att A Para 59):* For foreign ADIs ($g = "Foreign"$), a compliant parent head-office committed facility $F_"HO"$ contributes at most $50\%$ of the required HQLA buffer:
$ F_"rec" = min([F_"HO"]_+, 50\% dot.op r_"min" dot.op "NCO"), quad quad "HQLA"_"Foreign" = H_"exCLF" + F_"rec". $

#pagebreak()

// ============================================================================
// SECTION 6: STAGE 3B & 3C - MLH & NSFR
// ============================================================================
= Stage 3B & 3C: MLH and NSFR Engines

== Minimum Liquidity Holdings — MLH (Attachment B)

Applied to smaller ADIs designated under the MLH regime (statutory floor $r_"min" = max(9\%, r_"APRA")$):
$ "MLH" = frac(A_"MLH", L_"MLH") >= r_"min", quad quad "subject to mandatory internal trigger: " r_"trig" > r_"min" and "MLH" >= r_"trig". $

=== Asset Eligibility & Debt vs Non-Debt Rule (Att B Para 3)

Eligible categories comprise: Cash & RBA Notes, Commonwealth/State Gov Securities, ADI Deposits/CDs/ECP, Qualifying International Assets, and APRA-Approved Other Securities. For any asset $a$, eligibility $chi_"MLH"(a)$ requires:
$ chi_"MLH"(a) = bold(1)_("unencumberedOrAPRA" and "liquidatableWithin2BD") dot.op bold(1)_(chi_"debt"(a) arrow.r.double ("RBA-RepoEligible" and "NonSubordinated")), $
where $chi_"debt"(a) = 1$ for all standard categories and APRA-approved debt securities, while non-debt APRA-approved securities bypass the RBA-repo and non-subordination checks. All flags default fail-closed to $0$.

=== Para 2(f) Bilateral Netting Liability Base

Given total balance-sheet liabilities $L$, off-balance-sheet commitments $C_"OBS"$, Para 2(f) deposits held by other ADIs $D_"2f"$, and placements at other ADIs $P_"ADI"$:
$ L_"MLH" = [[L]_+ + [C_"OBS"]_+ - min([D_"2f"]_+, [P_"ADI"]_+)]_+. $

== Net Stable Funding Ratio — NSFR (Attachment C)

Applied exclusively to *locally incorporated LCR ADIs* ($r_"min" = max(100\%, r_"APRA")$):
$ "NSFR" = frac("ASF"_"total", "RSF"_"total") >= r_"min". $

=== Available Stable Funding (ASF) Lookup Matrix (19 Categories)

Unless overridden to $0\%$ by APRA-approved interdependence ($chi_"interdep" = 1$), carrying values receive base ASF factors:

#tbl(
  (0.65fr, 3.35fr),
  [*ASF Factor*], [*Liability / Capital Categories (Att C Paras 11–15)*],
  [$100\%$], [Regulatory capital (excl. Tier 2 $<1"y"$), other capital $>= 1"y"$ without early options, borrowings & liabilities $>= 1"y"$],
  [$95\%$], [Stable retail & qualifying SME deposits $<1"y"$; intermediated stable retail ($12"m"$ notice)],
  [$90\%$], [Less-stable retail & qualifying SME deposits $<1"y"$; intermediated less-stable retail ($12"m"$ notice)],
  [$50\%$], [Non-financial corporate funding $<1"y"$, operational deposits, superannuation operational deposits, intermediated retail ($6"m"$ notice), sovereign/PSE/MDB/NDB funding $<1"y"$, other funding $6"m–"<1"y"$],
  [$0\%$ / var], [Other funding $<6"m"$, no stated maturity / short / open, all other liabilities/equity; DTL & minority interest ($100\%$ if $>= 1"y"$, $50\%$ if $6"m–"<1"y"$, $0\%$ if $<6"m"$); cooperative network deposits $min([f]_+, 85\%)$],
)

=== Required Stable Funding (RSF) Base Lookup (23 Categories) & Encumbrance

Customer-segregated initial margin without ADI guarantee ($chi_"custVM"$), APRA-interdependent assets, and borrowed securities without beneficial ownership receive $0\%$ RSF. Performing loans are classified automatically via $chi_"Perf"$ (APS 220) and $chi_"Mort65"$ (APS 112); failing loans move to $100\%$:

#tbl(
  (0.65fr, 3.35fr),
  [*Base RSF*], [*Asset Categories (Att C Paras 29–37)*],
  [$0\%$], [Notes & coins immediately available, CB reserves/balances ($max(0, r_"APRA")$), CB claims $<6"m"$, standard trade-date receivables],
  [$5\%$], [Unencumbered HQLA1 (non-zero RSF tier)],
  [$10\%$], [Loans to FIs $<6"m"$ secured by rehypothecatable HQLA1; CLF third-party debt / host ALA; self-securitised CLF-eligible],
  [$15\%$], [Unencumbered HQLA2A; other performing loans to FIs $<6"m"$],
  [$50\%$], [Unencumbered HQLA2B, HQLA encumbered $6"m–"<1"y"$, loans to FIs/CBs $6"m–"<1"y"$, operational deposits at ADIs, ASX 200 non-FI equities, other non-HQLA $<1"y"$],
  [$65\%$], [Qualifying standard residential mortgages $"LVR" <= 80\%$ ($>= 1"y"$); other non-FI loans $"RW" <= 35\%$ ($>= 1"y"$)],
  [$85\%$], [Initial margin & CCP default fund ($max(85\%, r_"underlying")$ unless customer-exempt), other performing non-FI loans $>= 1"y"$, non-defaulted non-HQLA securities $>= 1"y"$ & exchange equities, commodities/gold],
  [$100\%$], [All other assets (including non-performing loans where $"DPD" >= 90 or "UTP"$)],
)

*Piecewise Encumbrance Step-Up Function (Paras 23–24):* Given base factor $w_"base"$, encumbrance duration $e$, and exceptional CB liquidity approval factor $r_"CB"$:
$
w_"eff"(w_"base", e, r_"CB") = cases(
  [r_"CB"]_+ & "if" r_"CB" "granted by APRA",
  w_"base" & "if" e in {"Unencumbered", <6"m"},
  max(50\%, w_"base") & "if" e = 6"m–"<1"y",
  100\% & "if" e >= 1"y".
)
$

=== Greedy Order-Independent CLF & Host ALA Bucket Allocation (Paras 32(a), 32(b), 33)

Assets eligible under the CLF limit $L_"CLF" >= 0$ split into two preferential tiers:
- Para 32(a) Unencumbered Third-Party CLF-Eligible Debt: *$10\%$* preferential RSF.
- Para 32(b) Encumbered Self-Securitised CLF Collateral: *$15\%$* preferential RSF.

To make utilisation invariant to input ordering, candidates are sorted *descending by their fallback RSF factor* $w_"fb"$ (which defaults conservatively to $85\%$ per Para 36(c) whenever unspecified). Iterating in sorted order allocates remaining limit $R_i = min(A_i, L_"rem")$ at the preferential factor ($10\%$ or $15\%$) and weights excess $A_i - R_i$ at $w_"fb"$, updating $L_"rem" <- L_"rem" - R_i$. Independently, Host ALA within the LCR recognition limit receives *$50\%$* RSF up to $L_"hostALA"$ (never suppressed when $L_"CLF" = 0$).

=== Derivatives, SFT Netting & Off-Balance-Sheet Exposures

- *Derivatives (Para 37):* Bilateral MNA sets net only if $chi_"MNA" and chi_"VM" = 1$; invalid netting forces gross contract-by-contract positive vs negative MtM separation. Net derivative asset exposure adds $"RSF"_"deriv,net" = [sum A_"deriv" - sum L_"deriv"]_+$, supplemented by the mandatory *$20\%$ gross derivative liability add-on*: $"RSF"_"deriv,20" = 20\% dot.op sum L_"deriv,eff"$.
- *SFT Netting (Para 26):* A repo/reverse-repo group nets $"RSF" = ["Rec" - "Pay"]_+$, $"ASF" = ["Pay" - "Rec"]_+$ iff $chi_"SFT" = 1$; otherwise gross receivables enter RSF and gross payables enter ASF unnetted.
- *Off-Balance-Sheet Table 1:* Undrawn revocable/irrevocable credit & liquidity facilities receive *$5\%$* RSF; non-contractual & other APRA-adjusted OBS exposures receive *$1\%$* (or $r_"APRA"$); fixed OBS commitments receive *$100\%$*.

#pagebreak()

// ============================================================================
// SECTION 7: STAGE 4 & INVARIANTS
// ============================================================================
= Stage 4: Governance, Compliance Gate & Model Invariants

== Qualitative Governance Gate & Statutory Profile

Ratio compliance alone never satisfies APS 210. Full compliance requires a conjunctive pass across eight qualitative obligations:
+ *Board & Management Oversight:* Board-approved liquidity risk tolerance, annual review, periodic reporting, clear accountability, and Level 2 group governance coherence whenever Level 2 applies.
+ *Liquidity Framework & Risk Management:* Intraday, FX, collateral, and counterparty-diversification controls plus dynamic cash-flow projections.
+ *Funding Strategy:* Market-access diversification and periodic formal review.
+ *Contingency Funding Plan (CFP):* Documented escalation triggers, annual operational testing, and explicit CLF drawdown protocols whenever CLF is utilised.
+ *Stress Testing:* Combined idiosyncratic and market-wide stress testing integrated directly into risk limits and CFP triggers.
+ *Local Operational Capacity:* Foreign or domestic local teams able to maintain standalone operations for at least *$3$ business days* ($d_"local" >= 3$).
+ *APRA Notifications:* Prompt reporting of actual/likely LCR breaches, MLH shortfalls, material adverse events, and advance notice prior to stress-buffer drawdown.
+ *Securitisation Relief:* Inclusion of SPV 30-day cash flows in LCR and satisfaction of APS 120 capital-relief criteria ($chi_"APS120" = 1$) for MLH/NSFR derecognition.

== Quantitative Compliance Gate, Buffers & Consolidation

For any ADI compliance snapshot, Stage 4 evaluates an exact 6-step decision procedure:

+ *CLF Limit Pass-Through:* For both Level 1 and Level 2 consolidation bases, the recognised AUD CLF calculated in the LCR engine feeds directly into the NSFR limit $L_"CLF"$ unless explicitly overridden.
+ *Mandatory Buffer Policy Gate:* An explicit internal buffer policy $Pi_"buf"$ must be declared (an unspecified policy fails closed). Under strict enforcement, LCR and NSFR must additionally meet internal management floors: $"LCR" >= 100\% + beta_"LCR"$ and $"NSFR" >= 100\% + beta_"NSFR"$ (or explicit custom management floors); under stress-waiver policy, internal buffers may be relaxed during active compliant stress drawdown.
+ *Supervisory Adjustments:* Applies APRA ADI-specific minimum overrides ($r_"APRA"$) and verifies compliance with all outstanding supervisory adjustments or exclusions.
+ *Consolidation & Classification Routing:*
  - *Locally Incorporated LCR ADI:* Must pass AUD LCR ($>= 100\%$), All-Currencies LCR ($>= 100\%$), and NSFR ($>= 100\%$) at Level 1 (and Level 2 if part of a consolidated group).
  - *Foreign LCR ADI:* Must pass All-Currencies LCR ($>= 40\%$, including $50\%$ Head Office facility cap) at Level 1 (and Level 2 if consolidated).
  - *MLH ADI:* Must pass MLH ratio ($>= 9\%$) and trigger check ($"MLH" >= r_"trig" > r_"min"$) at Level 1 (and Level 2 if consolidated).
+ *Scoped Stress Drawdown Rule:* When APRA has been notified of active market stress with a compliant CFP and notification history, temporary quantitative *LCR and NSFR* breaches during drawdown do not fail top-level compliance; however, *MLH minimums and trigger-ratio gates are never waived by LCR stress mode*.
+ *Continuous-Basis Monitoring (Main Body):* Given an ordered time series of snapshots ${S_1, S_2, dots, S_T}$, continuous compliance holds if and only if $T >= 1$ and $forall t in {1..T}, "OverallCompliant"(S_t) = "True"$.

== Formal Mathematical Properties of the Model

The mathematical definitions above guarantee key structural invariants unconditionally across all valid inputs:

- *Exact HQLA Cap Bounds:* For any non-negative post-unwind asset pools $(u_1, u_(2a), u_(2b))$, the closed-form $"cap"_(15)$ and $"cap"_(40)$ deductions strictly enforce $u_(2b)^"post" <= frac(15, 100) H_"exCLF"$ and $(u_(2a)^"post" + u_(2b)^"post") <= frac(40, 100) H_"exCLF"$.
- *Bounded Inflows & Non-Negative NCO:* Capped LCR inflows never exceed $75\% dot.op O_"gross"$ regardless of custom inflow caps, short-position-covering lending always contributes $0\%$, and $"NCO" >= 0$ always holds.
- *Scorecard Bounds & Monotonicity:* Retail scorecard totals always lie in ${0..6}$; less-stable run-off $rho_"less"(sigma)$ is monotonically non-decreasing in risk score $sigma_1 <= sigma_2 arrow.r.double rho_"less"(sigma_1) <= rho_"less"(sigma_2)$.
- *Denominator Monotonicity:* Under positive denominators $0 < D_1 <= D_2$ and $r_"min" >= 0$, passing at larger denominator $D_2$ implies passing at $D_1$: higher outflows never fabricate LCR compliance, and higher RSF never fabricates NSFR compliance.
- *Lossless Cash-Flow Conservation:* Partitioning amortising loans or liabilities across maturity buckets conserves $100\%$ of principal/carrying balance; inflows from HQLA assets or non-financial revenue never enter LCR inflows.
- *Fail-Closed Safeguards:* Missing APRA CLF approval, missing annual re-approval, missing collateral diversification, failed HQLA operational tests, or missing Level 2 transferability caps deterministically collapse the corresponding liquidity contribution to $0$.
- *Encumbrance & Interdependence Bounds:* Without exceptional CB relief, $>= 1"y"$ encumbrance unconditionally forces $100\%$ RSF; APRA-approved interdependent items always yield $0\%$ ASF/RSF; net NSFR derivative asset RSF is unconditionally non-negative.

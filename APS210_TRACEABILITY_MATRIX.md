# APS 210 Paragraph Traceability Matrix

Generated from `aps-210.html` against `APS210/**/*.lean` (excluding `Tests.lean` and `Theorems.lean`).

## Coverage Summary

| Section | Total Paragraphs | Mapped to Lean | Unmapped | Coverage |
| --- | ---: | ---: | ---: | ---: |
| Main Body | 72 | 60 | 12 | 83.3% |
| Attachment A | 66 | 62 | 4 | 93.9% |
| Attachment B | 6 | 6 | 0 | 100.0% |
| Attachment C | 42 | 37 | 5 | 88.1% |
| **Total** | **186** | **165** | **21** | **88.7%** |

## Main Body

| Para | APS 210 Summary | Lean Mapping |
| ---: | --- | --- |
| 1 | This Prudential Standard is made under section 11AF of the Banking Act 1959 (Banking Act). | **UNMAPPED** |
| 2 | This Prudential Standard applies to all authorised deposit-taking institutions (ADIs) under the Banking Act. | **UNMAPPED** |
| 3 | A reference to an ADI in this Prudential Standard is a reference to: | `ConsolidationBasis` (`APS210/Basic.lean:21`) |
| 4 | If an ADI to which this Prudential Standard applies is: | `BoardAndManagementGovernance.isCompliant` (`APS210/Governance.lean:57`) |
| 5 | If an ADI, or a member of its Level 2 group, is the originating ADI in a securitisation and meets the operatio... | `Aps120CapitalReliefCriteria` (`APS210/ExternalStandards.lean:171`); `SecuritisationTreatment` (`APS210/Governance.lean:6`); `SecuritisationTreatment.mayExcludeFromMlhAndNsfr` (`APS210/Governance.lean:15`) (+1 more) |
| 6 | This Prudential Standard commences on 1 July 2025. | **UNMAPPED** |
| 7 | Terms that are defined in Prudential Standard APS 001 Definitions (APS 001) appear in bold the first time they... | **UNMAPPED** |
| 8 | Where this Prudential Standard provides for APRA to exercise a power or discretion, this power or discretion i... | **UNMAPPED** |
| 9 | In this Prudential Standard, unless the contrary intention appears, a reference to an Act, Regulations or Prud... | **UNMAPPED** |
| 10 | The following definitions apply in this Prudential Standard: | **UNMAPPED** |
| 11 | An ADI is responsible for the sound management of its liquidity risk and must have a robust framework to manag... | **UNMAPPED** |
| 12 | An ADI must at all times maintain sufficient liquidity to meet its obligations as they fall due and hold a min... | **UNMAPPED** |
| 13 | An ADI must ensure that its activities are funded with stable sources of funding on an ongoing basis. | **UNMAPPED** |
| 14 | An ADI must inform APRA as soon as possible of any concerns it has about its current or future liquidity posit... | **UNMAPPED** |
| 15 | An ADI’s Board of directors (Board) is ultimately responsible for the sound and prudent management of the liqu... | `AdiIncorporation` (`APS210/Basic.lean:27`); `BoardAndManagementGovernance` (`APS210/Governance.lean:33`) |
| 16 | The liquidity risk management framework must include, at a minimum: | `BoardAndManagementGovernance` (`APS210/Governance.lean:33`); `BoardAndManagementGovernance` (`APS210/Governance.lean:36`); `BoardAndManagementGovernance` (`APS210/Governance.lean:38`) (+3 more) |
| 17 | The Board must ensure that: | `BoardAndManagementGovernance` (`APS210/Governance.lean:33`); `BoardAndManagementGovernance` (`APS210/Governance.lean:46`); `BoardAndManagementGovernance.isCompliant` (`APS210/Governance.lean:48`) |
| 18 | The Board must review regular reports on the liquidity position of the ADI and, where necessary, information o... | `BoardAndManagementGovernance` (`APS210/Governance.lean:33`); `BoardAndManagementGovernance.isCompliant` (`APS210/Governance.lean:50`) |
| 19 | An ADI’s senior management must, at a minimum: | `BoardAndManagementGovernance` (`APS210/Governance.lean:33`); `BoardAndManagementGovernance.isCompliant` (`APS210/Governance.lean:52`) |
| 20 | Senior management and the Board must be able to demonstrate a thorough understanding of the links between fund... | `BoardAndManagementGovernance` (`APS210/Governance.lean:33`); `BoardAndManagementGovernance.isCompliant` (`APS210/Governance.lean:54`) |
| 21 | An ADI’s liquidity risk tolerance defines the level of liquidity risk that the ADI is willing to assume. An AD... | `LiquidityFramework` (`APS210/Governance.lean:74`) |
| 22 | The liquidity risk tolerance must be reviewed, at least annually, to reflect the ADI’s financial condition and... | `LiquidityFramework` (`APS210/Governance.lean:74`) |
| 23 | In setting the liquidity risk tolerance, the Board and senior management must ensure that the risk tolerance a... | `LiquidityFramework` (`APS210/Governance.lean:74`) |
| 24 | The liquidity risk tolerance must be articulated in such a way that clearly states the trade-off between risks... | `LiquidityFramework` (`APS210/Governance.lean:74`) |
| 25 | An ADI’s liquidity risk management framework must clearly set out the organisational structure as it relates t... | `LiquidityFramework` (`APS210/Governance.lean:74`) |
| 26 | An ADI’s liquidity risk management framework must be formulated to ensure that the ADI maintains sufficient li... | `LiquidityFramework` (`APS210/Governance.lean:74`) |
| 27 | An ADI’s liquidity risk management framework must be well integrated into the ADI’s overall risk management pr... | `LiquidityFramework` (`APS210/Governance.lean:74`) |
| 28 | An ADI’s liquidity risk management oversight function must be operationally independent and staffed with perso... | `LiquidityFramework` (`APS210/Governance.lean:74`) |
| 29 | The liquidity management strategy must include specific policies on liquidity management, such as: | `LiquidityFramework` (`APS210/Governance.lean:74`) |
| 30 | The liquidity management strategy must take account of the ADI’s liquidity needs under normal conditions as we... | `LiquidityFramework` (`APS210/Governance.lean:74`) |
| 31 | The liquidity management strategy must be appropriate for the nature, scale and complexity of the ADI. In form... | `LiquidityFramework` (`APS210/Governance.lean:74`); `LiquidityFramework.isCompliant` (`APS210/Governance.lean:89`) |
| 32 | The liquidity management strategy, key policies for implementing the strategy and the liquidity risk managemen... | `LiquidityFramework` (`APS210/Governance.lean:74`) |
| 33 | An ADI must have adequate policies, procedures and controls in place to ensure that the Board and senior manag... | `LiquidityFramework` (`APS210/Governance.lean:74`) |
| 34 | Senior management must be satisfied that all business units conducting activities that have an impact on liqui... | `LiquidityFramework` (`APS210/Governance.lean:74`); `LiquidityFramework.isCompliant` (`APS210/Governance.lean:91`) |
| 35 | The liquidity risk management framework must be subject to effective review on an ongoing basis, in addition t... | `LiquidityFramework` (`APS210/Governance.lean:74`) |
| 36 | An ADI must have a sound process for identifying, measuring, monitoring and controlling liquidity risk. This p... | `LiquidityRiskManagement` (`APS210/Governance.lean:112`) |
| 37 | An ADI must set limits to control its liquidity risk exposure and vulnerabilities. Limits and corresponding es... | `LiquidityRiskManagement` (`APS210/Governance.lean:112`) |
| 38 | An ADI must actively manage its collateral positions, differentiating between encumbered and unencumbered asse... | `LiquidityRiskManagement` (`APS210/Governance.lean:112`) |
| 39 | An ADI must design a set of early warning indicators to aid its daily liquidity risk management processes in i... | `LiquidityRiskManagement` (`APS210/Governance.lean:112`) |
| 40 | An ADI must have a reliable management information system that provides the Board, senior management and other... | `LiquidityRiskManagement` (`APS210/Governance.lean:112`) |
| 41 | An ADI must actively manage its intraday liquidity positions and risks in order to meet payment and settlement... | `LiquidityRiskManagement` (`APS210/Governance.lean:112`) |
| 42 | An ADI must develop and implement a costs and benefits allocation process for funding and liquidity that appro... | `LiquidityRiskManagement` (`APS210/Governance.lean:112`) |
| 43 | An ADI active in multiple currencies must: | `LiquidityRiskManagement` (`APS210/Governance.lean:112`) |
| 44 | An ADI must: | `FundingStrategy` (`APS210/Governance.lean:135`) |
| 45 | The funding strategy must be reviewed and approved by the Board, at least annually, and supported by robust as... | `FundingStrategy` (`APS210/Governance.lean:135`) |
| 46 | The funding strategy must be reviewed and updated, at least annually, to account for, at a minimum, changed fu... | `FundingStrategy` (`APS210/Governance.lean:135`) |
| 47 | An ADI must have a formal contingency funding plan that clearly sets out the strategies for addressing liquidi... | `ContingencyFundingPlan` (`APS210/Governance.lean:153`) |
| 48 | An ADI’s contingency funding plan must be commensurate with its complexity, risk profile, scope of operations ... | `ContingencyFundingPlan` (`APS210/Governance.lean:153`) |
| 49 | The plan’s design, scope and procedures must be closely integrated with the ADI’s ongoing analysis of liquidit... | `ContingencyFundingPlan` (`APS210/Governance.lean:153`) |
| 50 | For an ADI that has retail deposits, the plan must address a retail deposit run and must include measures to r... | `ContingencyFundingPlan` (`APS210/Governance.lean:153`) |
| 51 | An ADI’s contingency funding plan must be reviewed and tested, at least annually, to ensure its effectiveness ... | `ContingencyFundingPlan` (`APS210/Governance.lean:153`) |
| 52 | For a locally incorporated ADI, APRA will determine whether an ADI is classified as a Liquidity Coverage Ratio... | `AdiClassification` (`APS210/Basic.lean:33`); `AdiProfile` (`APS210/Governance.lean:229`); `AdiProfile.requiredAudLcr` (`APS210/Governance.lean:233`) |
| 53 | A foreign ADI is an LCR ADI, unless APRA determines that the foreign ADI is an MLH ADI. In making a determinat... | `AdiClassification` (`APS210/Basic.lean:33`); `AdiProfile` (`APS210/Governance.lean:229`); `AdiProfile.requiredAudLcr` (`APS210/Governance.lean:234`) |
| 54 | An ADI is required to meet the minimum quantitative requirements associated with its classification on a conti... | `AdiComplianceResult` (`APS210/Compliance.lean:34`); `evaluateContinuousCompliance` (`APS210/Compliance.lean:204`); `AdiProfile` (`APS210/Governance.lean:229`) |
| 55 | An LCR ADI must maintain an adequate level of unencumbered high-quality liquid assets (HQLA) to meet its liqui... | `AdiComplianceResult` (`APS210/Compliance.lean:34`); `AdiProfile` (`APS210/Governance.lean:229`); `AdiProfile.requiredAudLcr` (`APS210/Governance.lean:245`) (+1 more) |
| 56 | APRA may require an LCR ADI to maintain a higher minimum LCR if it has concerns about the ADI’s liquidity risk... | `AdiProfile` (`APS210/Governance.lean:229`); `AdiProfile.requiredAudLcr` (`APS210/Governance.lean:236`) |
| 57 | An MLH ADI must maintain a minimum holding of nine per cent of its liabilities in specified liquid assets, in ... | `AdiProfile` (`APS210/Governance.lean:229`); `AdiProfile.requiredMlh` (`APS210/Governance.lean:261`); `evaluateMlh` (`APS210/MLH.lean:87`) |
| 58 | APRA may require an MLH ADI to maintain higher minimum liquidity holdings if it has concerns about the ADI’s l... | `AdiProfile` (`APS210/Governance.lean:229`); `AdiProfile.requiredAudLcr` (`APS210/Governance.lean:239`); `AdiProfile.requiredMlh` (`APS210/Governance.lean:261`) |
| 59 | A locally incorporated ADI classified as an LCR ADI must meet the NSFR requirements in this Prudential Standar... | `AdiProfile` (`APS210/Governance.lean:229`); `AdiProfile.requiredNsfr` (`APS210/Governance.lean:267`) |
| 60 | A locally incorporated LCR ADI must maintain an NSFR of at least 100 per cent at all times. | `evaluateContinuousCompliance` (`APS210/Compliance.lean:204`); `AdiProfile` (`APS210/Governance.lean:229`); `AdiProfile.requiredNsfr` (`APS210/Governance.lean:267`) (+1 more) |
| 61 | APRA may require an LCR ADI to maintain a higher minimum NSFR where APRA considers it appropriate to do so, in... | `AdiProfile` (`APS210/Governance.lean:229`); `AdiProfile.requiredAudLcr` (`APS210/Governance.lean:241`); `AdiProfile.requiredNsfr` (`APS210/Governance.lean:267`) |
| 62 | In addition to the requirements contained in paragraphs 52 to 56 of this Prudential Standard, an LCR ADI must ... | `StressTesting` (`APS210/Governance.lean:170`) |
| 63 | The stress test outcomes must be used to adjust the ADI’s liquidity management strategy, policies and position... | `StressTesting` (`APS210/Governance.lean:170`) |
| 64 | Stress tests must enable the ADI to analyse the impact of stress scenarios on its Level 1 and Level 2 liquidit... | `StressTesting` (`APS210/Governance.lean:170`) |
| 65 | An LCR ADI’s stress test scenarios and related assumptions must be well documented and reviewed together with ... | `StressTesting` (`APS210/Governance.lean:170`); `StressTesting.isCompliant` (`APS210/Governance.lean:177`) |
| 66 | An LCR ADI must decide how to incorporate the results of stress tests in assessing and planning for related po... | `StressTesting` (`APS210/Governance.lean:170`) |
| 67 | A foreign ADI must perform an assessment of its local operational capacity (LOC) to liquidate assets and make ... | `LocalOperationalCapacity` (`APS210/Governance.lean:189`) |
| 68 | A foreign ADI, in performing a LOC assessment, must ensure at a minimum, it considers a scenario involving a c... | `LocalOperationalCapacity` (`APS210/Governance.lean:189`); `LocalOperationalCapacity.isCompliant` (`APS210/Governance.lean:193`); `LocalOperationalCapacity.isCompliant` (`APS210/Governance.lean:197`) |
| 69 | APRA may impose additional requirements on a foreign ADI if the LOC assessment highlights issues concerning th... | `LocalOperationalCapacity` (`APS210/Governance.lean:189`) |
| 70 | An ADI under severe liquidity stress may request Exceptional Liquidity Assistance (ELA) from the Reserve Bank ... | **UNMAPPED** |
| 71 | APRA may adjust or exclude a specific requirement in this Prudential Standard in relation to an ADI.[1] | `AdiComplianceResult` (`APS210/Compliance.lean:37`) |
| 72 | An ADI must contact APRA if it seeks to place reliance, for the purposes of complying with this Prudential Sta... | `AdiComplianceResult` (`APS210/Compliance.lean:39`) |

## Attachment A

| Para | APS 210 Summary | Lean Mapping |
| ---: | --- | --- |
| 1 | The objective of the LCR is to ensure that an ADI maintains an adequate level of unencumbered HQLA that can be... | **UNMAPPED** |
| 2 | The LCR has two components: | **UNMAPPED** |
| 3 | The LCR is calculated as: | `AdiComplianceInput` (`APS210/Compliance.lean:23`) |
| 4 | An ADI must include an appropriate buffer of HQLA over the LCR requirement in line with its liquidity risk tol... | `LcrResult` (`APS210/LCR.lean:39`) |
| 5 | During a period of financial stress, an ADI may need to liquidate part of its stock of HQLA and/or draw on its... | `NotificationStatus` (`APS210/Governance.lean:204`); `NotificationStatus.isCompliant` (`APS210/Governance.lean:214`) |
| 6 | There are two categories of assets that may be included in the stock of HQLA. Assets that may be included in e... | `NotificationStatus` (`APS210/Governance.lean:204`); `HqlaCategory` (`APS210/LCR/HQLA.lean:6`); `HqlaCapBreakdown` (`APS210/LCR/HQLA.lean:254`) |
| 7 | The calculation of the 40 per cent cap detailed in paragraph 6 of this Attachment, and the 15 per cent cap det... | `HqlaCategory` (`APS210/LCR/HQLA.lean:6`); `HqlaCapBreakdown` (`APS210/LCR/HQLA.lean:254`) |
| 8 | The adjusted amount of HQLA1 is the amount of HQLA1 that would result after unwinding short‑term secured fundi... | `HqlaCategory` (`APS210/LCR/HQLA.lean:6`); `ShortTermSftLeg` (`APS210/LCR/HQLA.lean:220`); `HqlaUnwindAdjustments` (`APS210/LCR/HQLA.lean:228`) (+2 more) |
| 9 | HQLA1 may comprise an unlimited share of the stock of eligible HQLA. HQLA1 are included at market value and ar... | `LcrResult` (`APS210/LCR.lean:30`); `HqlaCategory` (`APS210/LCR/HQLA.lean:6`); `HqlaCategory.haircut` (`APS210/LCR/HQLA.lean:15`) (+8 more) |
| 10 | An ADI may include the following as HQLA2A where these assets have been recognised by APRA or the relevant pru... | `CreditRating` (`APS210/Basic.lean:45`); `isAtLeastAAMinus` (`APS210/Basic.lean:68`); `HqlaCategory` (`APS210/LCR/HQLA.lean:6`) (+5 more) |
| 11 | The portfolio of HQLA2A held by the ADI must be well diversified in terms of type of assets, type of issuer an... | `HqlaCategory` (`APS210/LCR/HQLA.lean:6`); `classifyHqlaDebtSecurity` (`APS210/LCR/HQLA.lean:38`); `HqlaAsset.eligibleMarketValue` (`APS210/LCR/HQLA.lean:188`) |
| 12 | An ADI may include the following as HQLA2B where these assets have been recognised by APRA or the relevant pru... | `isAtLeastAA` (`APS210/Basic.lean:63`); `isBetweenAPlusAndBBBMinus` (`APS210/Basic.lean:73`); `HqlaCategory` (`APS210/LCR/HQLA.lean:6`) (+3 more) |
| 13 | A locally incorporated LCR ADI may establish a CLF with the RBA, sufficient in size to cover any shortfall in ... | `evaluateLcr` (`APS210/LCR.lean:55`); `ClfInput` (`APS210/LCR/HQLA.lean:290`) |
| 14 | The CLF must only be used by an ADI to manage any shortfall in Australian dollars between its holding of HQLA ... | `NotificationStatus` (`APS210/Governance.lean:204`); `evaluateLcr` (`APS210/LCR.lean:55`); `ClfInput` (`APS210/LCR/HQLA.lean:290`) |
| 15 | The amount of HQLA that may be included in the numerator of an ADI’s LCR calculation is the market value of HQ... | `ClfInput` (`APS210/LCR/HQLA.lean:290`); `ClfInput` (`APS210/LCR/HQLA.lean:294`); `ClfInput` (`APS210/LCR/HQLA.lean:296`) (+3 more) |
| 16 | Assets held by an ADI for the purposes of the CLF must: | `ClfInput` (`APS210/LCR/HQLA.lean:290`); `ClfInput` (`APS210/LCR/HQLA.lean:303`); `ClfInput.processEligible` (`APS210/LCR/HQLA.lean:305`) (+1 more) |
| 17 | An ADI that requires a CLF from the RBA for LCR purposes must apply to APRA for approval before establishing t... | `ClfInput` (`APS210/LCR/HQLA.lean:290`); `ClfInput` (`APS210/LCR/HQLA.lean:292`); `ClfInput.processEligible` (`APS210/LCR/HQLA.lean:307`) (+1 more) |
| 18 | To facilitate APRA’s approval process, the ADI’s funding strategy must clearly state the amount of the CLF req... | `ClfInput` (`APS210/LCR/HQLA.lean:290`); `ClfInput` (`APS210/LCR/HQLA.lean:292`); `ClfInput.processEligible` (`APS210/LCR/HQLA.lean:309`) (+1 more) |
| 19 | Following the initial approval, an ADI must apply on an annual basis for APRA’s approval of the size of the CL... | `ClfInput` (`APS210/LCR/HQLA.lean:290`); `ClfInput` (`APS210/LCR/HQLA.lean:292`); `ClfInput.processEligible` (`APS210/LCR/HQLA.lean:311`) (+1 more) |
| 20 | In the case of unforeseen changes in circumstances, an ADI may apply to APRA, at any time, for a change in the... | `ClfInput` (`APS210/LCR/HQLA.lean:290`); `ClfInput` (`APS210/LCR/HQLA.lean:292`); `ClfInput.processEligible` (`APS210/LCR/HQLA.lean:313`) (+1 more) |
| 21 | In the event that an ADI draws funds under its CLF other than for intraday purposes or the funding of end-of-d... | `ClfInput` (`APS210/LCR/HQLA.lean:290`); `ClfInput.processEligible` (`APS210/LCR/HQLA.lean:313`); `ClfInput.processEligible` (`APS210/LCR/HQLA.lean:317`) |
| 22 | All assets in the stock of eligible HQLA must be managed as part of that stock and are subject to the followin... | `HqlaOperationalCompliance` (`APS210/LCR/HQLA.lean:138`); `HqlaOperationalCompliance.isMet` (`APS210/LCR/HQLA.lean:143`); `HqlaOperationalCompliance.isMet` (`APS210/LCR/HQLA.lean:145`) (+2 more) |
| 23 | The assets must not be pledged to secure, collateralise or credit enhance any transactions or be designated to... | `HqlaOperationalCompliance` (`APS210/LCR/HQLA.lean:138`); `HqlaOperationalCompliance.isMet` (`APS210/LCR/HQLA.lean:149`); `HqlaOperationalCompliance.isMet` (`APS210/LCR/HQLA.lean:151`) (+1 more) |
| 24 | At Level 2, an ADI may also include in the stock of HQLA, liquid assets that qualify for inclusion in the nume... | `HqlaOperationalCompliance` (`APS210/LCR/HQLA.lean:138`); `HqlaAsset` (`APS210/LCR/HQLA.lean:172`); `HqlaUnwindAdjustments` (`APS210/LCR/HQLA.lean:228`) |
| 25 | For the purpose of calculating the LCR, if an eligible HQLA or eligible CLF liquid asset becomes ineligible (e... | `HqlaOperationalCompliance` (`APS210/LCR/HQLA.lean:139`); `HqlaAsset` (`APS210/LCR/HQLA.lean:182`); `HqlaAsset.eligibleMarketValue` (`APS210/LCR/HQLA.lean:184`) (+1 more) |
| 26 | The LCR is to be met by an ADI on both a Level 1 and Level 2 basis. Where an ADI has a branch or subsidiary in... | `AdiComplianceInput` (`APS210/Compliance.lean:23`); `HqlaAsset.eligibleMarketValue` (`APS210/LCR/HQLA.lean:186`); `RetailDeposit.isExcludedTermDeposit` (`APS210/LCR/Outflows.lean:55`) |
| 27 | Where an ADI has a branch or subsidiary in an offshore jurisdiction that does not apply the Basel Committee’s ... | **UNMAPPED** |
| 28 | An ADI with a material banking subsidiary in another offshore jurisdiction must ensure that the subsidiary mai... | `AdiComplianceInput` (`APS210/Compliance.lean:21`) |
| 29 | For the calculation of the Level 2 LCR, an ADI must take into account restrictions on the transferability of l... | `HqlaAsset` (`APS210/LCR/HQLA.lean:174`) |
| 30 | Total net cash outflows represent the total expected cash outflows minus total expected cash inflows in the sp... | `NetCashOutflowsResult` (`APS210/LCR/Inflows.lean:75`); `computeTotalExpectedCashOutflows` (`APS210/LCR/Outflows.lean:239`) |
| 31 | An ADI must not double‑count items. That is, if assets are included as part of the stock of HQLA (the numerato... | `InflowItem.inflowAmount` (`APS210/LCR/Inflows.lean:65`) |
| 32 | APRA will assess the suitability of an ADI’s assumptions made for those cash flow items that are derived. An A... | **UNMAPPED** |
| 33 | Where APRA determines that an ADI is actively using collateral swaps for liquidity management purposes, APRA m... | `Table3OutflowCategory.rate` (`APS210/LCR/Outflows.lean:166`) |
| 34 | For the purposes of the LCR, retail deposits are defined as deposits placed with an ADI by a natural person. D... | `RetailDeposit` (`APS210/LCR/Outflows.lean:29`) |
| 35 | Where a person places funds with an intermediary, which then places those funds with an ADI, the deposit with ... | `RetailDeposit` (`APS210/LCR/Outflows.lean:29`); `RetailDeposit` (`APS210/LCR/Outflows.lean:34`) |
| 36 | Retail deposits are divided into stable and less stable portions of funds, as described below. | `RetailDeposit` (`APS210/LCR/Outflows.lean:29`) |
| 37 | Stable deposits are the portion of deposits that are fully covered by the Financial Claims Scheme (or an effec... | `RetailDeposit` (`APS210/LCR/Outflows.lean:29`); `RetailDeposit` (`APS210/LCR/Outflows.lean:42`); `RetailDeposit` (`APS210/LCR/Outflows.lean:45`) (+1 more) |
| 38 | Less stable deposits are the portion of deposits that do not meet the requirements of stable deposits. | `RetailDeposit` (`APS210/LCR/Outflows.lean:29`) |
| 39 | Certain types of deposits are considered more likely to be withdrawn in a time of stress. These include high-v... | `RetailDeposit` (`APS210/LCR/Outflows.lean:29`) |
| 40 | An ADI must assess each less stable account or product balance against the following scorecard: | `LessStableScorecardInput` (`APS210/LCR/Outflows.lean:5`); `RetailDeposit` (`APS210/LCR/Outflows.lean:29`) |
| 41 | Cash outflows related to fixed or term deposits with a residual maturity or withdrawal notice period of greate... | `RetailDeposit` (`APS210/LCR/Outflows.lean:29`); `RetailDeposit` (`APS210/LCR/Outflows.lean:37`); `RetailDeposit.isExcludedTermDeposit` (`APS210/LCR/Outflows.lean:59`) |
| 42 | If an ADI allows a depositor to withdraw such deposits despite a clause that says the depositor has no legal r... | `RetailDeposit` (`APS210/LCR/Outflows.lean:29`); `RetailDeposit` (`APS210/LCR/Outflows.lean:37`); `RetailDeposit` (`APS210/LCR/Outflows.lean:40`) (+1 more) |
| 43 | An ADI may allow depositors experiencing hardship to withdraw their term deposits without changing the treatme... | `RetailDeposit` (`APS210/LCR/Outflows.lean:29`); `RetailDeposit` (`APS210/LCR/Outflows.lean:37`); `RetailDeposit.isExcludedTermDeposit` (`APS210/LCR/Outflows.lean:59`) |
| 44 | For the purposes of the LCR, unsecured wholesale funding is defined as those liabilities and general obligatio... | `Table3OutflowCategory` (`APS210/LCR/Outflows.lean:117`); `classifyNonOperationalWholesaleDeposit` (`APS210/LCR/Outflows.lean:247`) |
| 45 | Wholesale funding included in the LCR is defined as all funding that is callable within the LCR’s horizon of 3... | `Table3OutflowCategory` (`APS210/LCR/Outflows.lean:117`); `classifyNonOperationalWholesaleDeposit` (`APS210/LCR/Outflows.lean:247`) |
| 46 | Unsecured wholesale funding provided by SME customers may be treated as retail deposits where: | `Table3OutflowCategory` (`APS210/LCR/Outflows.lean:117`); `classifyNonOperationalWholesaleDeposit` (`APS210/LCR/Outflows.lean:247`); `classifyNonOperationalWholesaleDeposit` (`APS210/LCR/Outflows.lean:249`) |
| 47 | For the purpose of the LCR, operational deposits are those where customers place, or leave, deposits with an A... | `Table3OutflowCategory` (`APS210/LCR/Outflows.lean:117`) |
| 48 | For the purpose of determining operational deposits, qualifying activities in this context refer to clearing, ... | `Table3OutflowCategory` (`APS210/LCR/Outflows.lean:117`) |
| 49 | Qualifying operational deposits generated by such an activity are those where the deposits are: | `Table3OutflowCategory` (`APS210/LCR/Outflows.lean:117`) |
| 50 | Any excess balances that could be withdrawn without jeopardising these clearing, custody or cash management ac... | `Table3OutflowCategory` (`APS210/LCR/Outflows.lean:117`); `Table3OutflowCategory` (`APS210/LCR/Outflows.lean:120`); `classifyOperationalDeposit` (`APS210/LCR/Outflows.lean:288`) |
| 51 | A liquidity facility is any committed, undrawn back-up facility that would be used to refinance the debt oblig... | `(module-level)` (`APS210/LCR/Outflows.lean:149`); `classifyCommittedFacility` (`APS210/LCR/Outflows.lean:269`); `classifyLiquidityFacilityWithMaturingDebt` (`APS210/LCR/Outflows.lean:310`) |
| 52 | Notwithstanding paragraph 51 of this Attachment, any facilities provided to hedge funds, money market funds an... | `RatioCheck` (`APS210/Basic.lean:93`); `(module-level)` (`APS210/LCR/Outflows.lean:149`); `classifyCommittedFacility` (`APS210/LCR/Outflows.lean:269`) (+1 more) |
| 53 | The following table sets out the run-off rates that must be applied to each liability and off‑balance sheet ca... | `Table3OutflowCategory` (`APS210/LCR/Outflows.lean:110`) |
| 54 | When considering its available cash inflows, an ADI must only include contractual inflows from outstanding exp... | `InflowItem.inflowAmount` (`APS210/LCR/Inflows.lean:63`) |
| 55 | An ADI must assume that maturing reverse repurchase or securities borrowing agreements secured by HQLA1 will b... | `SecuredLendingCollateral` (`APS210/LCR/Inflows.lean:5`) |
| 56 | As an exception to paragraph 55 of this Attachment, if the collateral obtained through reverse repo, securitie... | `SecuredLendingCollateral` (`APS210/LCR/Inflows.lean:5`); `securedLendingInflowRate` (`APS210/LCR/Inflows.lean:17`) |
| 57 | Lines of credit, liquidity facilities and other contingent funding facilities that an ADI holds at other insti... | `InflowCategory` (`APS210/LCR/Inflows.lean:31`) |
| 58 | APRA may allow recognition of access to head office funds via a committed funding facility if the ADI is a for... | `ForeignHeadOfficeFacility` (`APS210/LCR.lean:9`); `evaluateLcr` (`APS210/LCR.lean:57`); `InflowCategory` (`APS210/LCR/Inflows.lean:31`) |
| 59 | A foreign ADI may meet up to 50 per cent of its LCR requirement with a head office committed funding facility.... | `ForeignHeadOfficeFacility` (`APS210/LCR.lean:9`); `evaluateLcr` (`APS210/LCR.lean:57`); `InflowCategory` (`APS210/LCR/Inflows.lean:31`) |
| 60 | All inflows are to be taken only at the latest possible date, based on the contractual rights available to cou... | `InflowCategory` (`APS210/LCR/Inflows.lean:31`) |
| 61 | An ADI is assumed to receive all fully performing contractual inflows from retail and SME[7] customers. At the... | `InflowCategory` (`APS210/LCR/Inflows.lean:31`) |
| 62 | An ADI is assumed to receive all fully performing contractual wholesale cash inflows. In addition, an ADI is a... | `InflowCategory` (`APS210/LCR/Inflows.lean:31`) |
| 63 | A zero per cent inflow rate applies to deposits held at other financial institutions for operational purposes. | `InflowCategory` (`APS210/LCR/Inflows.lean:31`) |
| 64 | Likewise, the depositing ADI must not count any inflow for deposits held at the centralised institution in a c... | `InflowCategory` (`APS210/LCR/Inflows.lean:31`) |
| 65 | These derivative items receive an inflow rate of 100 per cent.Other contractual cash inflows | `InflowCategory` (`APS210/LCR/Inflows.lean:31`) |
| 66 | Other contractual cash inflows receive an inflow rate of 100 per cent. Cash inflows related to non-financial r... | `InflowCategory` (`APS210/LCR/Inflows.lean:31`) |

## Attachment B

| Para | APS 210 Summary | Lean Mapping |
| ---: | --- | --- |
| 1 | For the purpose of this Attachment, liabilities are defined as total on-balance sheet liabilities and irrevoca... | `MlhLiabilityItem` (`APS210/MLH.lean:54`) |
| 2 | For the purpose of the MLH requirement, liquid assets are included at market value. These assets must be free ... | `MlhAssetCategory` (`APS210/MLH.lean:5`); `MlhAsset` (`APS210/MLH.lean:27`); `MlhAsset.isEligible` (`APS210/MLH.lean:32`) (+2 more) |
| 3 | All debt securities must be eligible for repurchase agreement with the RBA and must not be subordinated. | `MlhAssetCategory.isDebtSecurity` (`APS210/MLH.lean:16`); `MlhAsset` (`APS210/MLH.lean:27`); `MlhAsset.isEligible` (`APS210/MLH.lean:34`) (+1 more) |
| 4 | An MLH ADI must ensure it has the operational capacity to liquidate any securities held as liquid assets withi... | `MlhAsset` (`APS210/MLH.lean:27`); `MlhAsset.isEligible` (`APS210/MLH.lean:38`); `MlhAsset.isEligible` (`APS210/MLH.lean:40`) |
| 5 | In order to ensure that the MLH requirement is not breached, absent a situation of financial stress, an ADI mu... | `MlhResult` (`APS210/MLH.lean:74`) |
| 6 | An ADI must inform APRA immediately when it becomes aware that its liquid assets may fall below its MLH requir... | `NotificationStatus.isCompliant` (`APS210/Governance.lean:217`) |

## Attachment C

| Para | APS 210 Summary | Lean Mapping |
| ---: | --- | --- |
| 1 | The objective of the NSFR is to reduce the funding risk of an ADI over a one-year time horizon by requiring an... | **UNMAPPED** |
| 2 | The NSFR has two components: | **UNMAPPED** |
| 3 | The NSFR is calculated as: | `sovereignRiskWeightPct` (`APS210/ExternalStandards.lean:54`) |
| 4 | An ADI must maintain an appropriate buffer above its required NSFR under this Prudential Standard, in line wit... | `NsfrResult` (`APS210/NSFR.lean:315`) |
| 5 | For the purposes of this Attachment, the meanings and definitions in Attachment A apply unless otherwise indic... | **UNMAPPED** |
| 6 | ASF is the portion of capital and liabilities expected to be reliably provided over a one-year time horizon, a... | **UNMAPPED** |
| 7 | An ADI must calculate its ASF as follows: | `AsfItem` (`APS210/NSFR.lean:77`) |
| 8 | In order to determine the maturity of a funding instrument with an option or a deposit with a withdrawal notic... | `NsfrMaturityBucket` (`APS210/NSFR.lean:6`) |
| 9 | An ADI must calculate its derivative liabilities based on the replacement cost for derivative contracts (obtai... | `BilateralNettingAgreement` (`APS210/ExternalStandards.lean:112`); `BilateralNettingAgreement` (`APS210/ExternalStandards.lean:113`); `NsfrDerivativeInput` (`APS210/NSFR.lean:264`) (+1 more) |
| 10 | In calculating NSFR derivative liabilities, an ADI must deduct any collateral posted in the form of variation ... | `BilateralNettingAgreement` (`APS210/ExternalStandards.lean:112`); `NsfrDerivativeInput` (`APS210/NSFR.lean:264`); `NsfrDerivativeInput.nsfrDerivativeLiabilities` (`APS210/NSFR.lean:268`) |
| 11 | The following liabilities and capital instruments must be assigned a 100 per cent ASF factor: | `BilateralNettingAgreement` (`APS210/ExternalStandards.lean:112`); `AsfCategory` (`APS210/NSFR.lean:21`); `AsfCategory` (`APS210/NSFR.lean:23`) (+1 more) |
| 12 | The following liabilities must be assigned a 95 per cent ASF factor: | `AsfCategory` (`APS210/NSFR.lean:21`); `AsfCategory` (`APS210/NSFR.lean:27`); `AsfCategory.factor` (`APS210/NSFR.lean:49`) |
| 13 | The following liabilities must be assigned a 90 per cent ASF factor: | `AsfCategory` (`APS210/NSFR.lean:21`); `AsfCategory` (`APS210/NSFR.lean:30`); `AsfCategory.factor` (`APS210/NSFR.lean:49`) |
| 14 | The following liabilities must be assigned a 50 per cent ASF factor: | `AsfCategory` (`APS210/NSFR.lean:21`); `AsfCategory` (`APS210/NSFR.lean:33`); `AsfCategory.factor` (`APS210/NSFR.lean:49`) |
| 15 | The following liabilities must be assigned a zero per cent ASF factor: | `AsfCategory` (`APS210/NSFR.lean:21`); `AsfCategory.factor` (`APS210/NSFR.lean:40`); `AsfCategory.factor` (`APS210/NSFR.lean:49`) |
| 16 | RSF is the minimum amount of stable funding an ADI is required to hold and is a function of the liquidity char... | **UNMAPPED** |
| 17 | An ADI must calculate its RSF as follows: | `RsfAssetItem` (`APS210/NSFR.lean:168`) |
| 18 | An ADI must allocate assets to the appropriate RSF factor based on the asset’s residual maturity or liquidity ... | `RsfAssetItem` (`APS210/NSFR.lean:168`) |
| 19 | For the purposes of determining its RSF, an ADI must include financial instruments, foreign currencies and com... | `RsfAssetItem` (`APS210/NSFR.lean:168`) |
| 20 | To determine the maturity of an asset, an ADI must, for the purposes of the NSFR, assume the maturity date as ... | `RsfAssetItem` (`APS210/NSFR.lean:168`) |
| 21 | Where an asset does not have a defined maturity or the maturity is subject to periodic review, an ADI must, fo... | `RsfAssetItem` (`APS210/NSFR.lean:168`) |
| 22 | For an amortising loan, or a loan with minimum contractual payments, that portion that falls due within the on... | `RsfAssetItem` (`APS210/NSFR.lean:168`) |
| 23 | Assets on an ADI’s balance sheet that are encumbered[21] for one year or more must be assigned a 100 per cent ... | `CashVariationMarginEligibility` (`APS210/ExternalStandards.lean:127`); `EncumbrancePeriod` (`APS210/NSFR.lean:13`); `applyEncumbranceFactor` (`APS210/NSFR.lean:154`) (+3 more) |
| 24 | Where an asset is encumbered for exceptional[22] central bank liquidity operations, the asset may be given a l... | `applyEncumbranceFactor` (`APS210/NSFR.lean:154`); `RsfAssetItem` (`APS210/NSFR.lean:168`); `RsfAssetItem.effectiveFactor` (`APS210/NSFR.lean:174`) |
| 25 | For the purposes of determining its NSFR, an ADI must exclude from its assets amount, securities borrowed in s... | `RsfAssetItem` (`APS210/NSFR.lean:168`); `RsfAssetItem.effectiveFactor` (`APS210/NSFR.lean:178`) |
| 26 | An ADI may net securities financing transactions with a single counterparty when calculating its NSFR, provide... | `SftNettingCriteria` (`APS210/ExternalStandards.lean:146`); `RsfAssetItem` (`APS210/NSFR.lean:168`) |
| 27 | An ADI must calculate the value of derivative assets based on the replacement cost for derivative contracts (o... | `RsfAssetItem` (`APS210/NSFR.lean:168`); `NsfrDerivativeInput.nsfrDerivativeLiabilities` (`APS210/NSFR.lean:270`) |
| 28 | When calculating NSFR derivative assets, collateral received in connection with derivative contracts must not ... | `CashVariationMarginEligibility` (`APS210/ExternalStandards.lean:127`); `RsfAssetItem` (`APS210/NSFR.lean:168`); `NsfrDerivativeInput.nsfrDerivativeLiabilities` (`APS210/NSFR.lean:272`) |
| 29 | An ADI must assign a zero per cent RSF factor to the following assets: | `RsfCategory` (`APS210/NSFR.lean:92`); `RsfCategory` (`APS210/NSFR.lean:94`); `RsfCategory.baseFactor` (`APS210/NSFR.lean:127`) (+1 more) |
| 30 | An ADI must assign a five per cent RSF factor to the following assets. They include unencumbered HQLA1 assets,... | `RsfCategory` (`APS210/NSFR.lean:92`); `RsfCategory` (`APS210/NSFR.lean:99`); `RsfCategory.baseFactor` (`APS210/NSFR.lean:127`) (+1 more) |
| 31 | An ADI must assign a 10 per cent RSF factor to unencumbered loans to financial institutions with a residual ma... | `RsfCategory` (`APS210/NSFR.lean:92`); `RsfCategory` (`APS210/NSFR.lean:101`); `RsfCategory.baseFactor` (`APS210/NSFR.lean:127`) (+1 more) |
| 32 | An ADI may assign an RSF factor of 10 per cent to: | `RsfCategory` (`APS210/NSFR.lean:92`); `RsfCategory` (`APS210/NSFR.lean:101`); `RsfCategory.baseFactor` (`APS210/NSFR.lean:127`) (+4 more) |
| 33 | An ADI must assign a 15 per cent RSF factor to the following assets: | `RsfCategory` (`APS210/NSFR.lean:92`); `RsfCategory` (`APS210/NSFR.lean:105`); `RsfCategory.baseFactor` (`APS210/NSFR.lean:127`) (+2 more) |
| 34 | An ADI must assign a 50 per cent RSF factor to the following assets: | `RsfCategory` (`APS210/NSFR.lean:92`); `RsfCategory` (`APS210/NSFR.lean:108`); `RsfCategory.baseFactor` (`APS210/NSFR.lean:127`) (+2 more) |
| 35 | An ADI must assign a 65 per cent RSF factor to the following assets: | `StandardResidentialMortgageCriteria` (`APS210/ExternalStandards.lean:97`); `RsfCategory` (`APS210/NSFR.lean:92`); `RsfCategory.baseFactor` (`APS210/NSFR.lean:115`) (+3 more) |
| 36 | An ADI must assign an 85 per cent RSF factor to the following assets: | `RsfCategory` (`APS210/NSFR.lean:92`); `RsfCategory.baseFactor` (`APS210/NSFR.lean:118`); `RsfCategory.baseFactor` (`APS210/NSFR.lean:127`) (+3 more) |
| 37 | An ADI must assign a 100 per cent RSF factor to the following assets: | `RsfCategory` (`APS210/NSFR.lean:92`); `RsfCategory.baseFactor` (`APS210/NSFR.lean:123`); `RsfCategory.baseFactor` (`APS210/NSFR.lean:127`) (+4 more) |
| 38 | APRA may determine that certain asset and liability items, on the basis of contractual arrangements, are inter... | `SftNettingCriteria` (`APS210/ExternalStandards.lean:145`); `AsfItem.effectiveFactor` (`APS210/NSFR.lean:82`); `RsfAssetItem.effectiveFactor` (`APS210/NSFR.lean:176`) |
| 39 | For the purposes of paragraph 38, APRA will only make a determination, on a case-by-case basis, in exceptional... | `AsfItem.effectiveFactor` (`APS210/NSFR.lean:82`); `RsfAssetItem.effectiveFactor` (`APS210/NSFR.lean:176`) |
| 40 | If APRA determines asset and liability items to be interdependent in accordance with paragraph 38 of this Atta... | `qualifiesAsSmeUnderAps113Para40` (`APS210/ExternalStandards.lean:160`); `AsfItem.effectiveFactor` (`APS210/NSFR.lean:82`); `RsfAssetItem.effectiveFactor` (`APS210/NSFR.lean:176`) |
| 41 | An ADI must apply the RSF factor to the RSF category in Table 1 for the OBS exposure categories in the Table. | `ObsExposureCategory` (`APS210/NSFR.lean:237`); `ObsExposureItem` (`APS210/NSFR.lean:253`) |
| 42 | An ADI must apply an RSF factor of 100 per cent to trade finance-related obligations and guarantees and letter... | `ObsExposureCategory` (`APS210/NSFR.lean:237`); `ObsExposureCategory.factor` (`APS210/NSFR.lean:243`); `ObsExposureItem` (`APS210/NSFR.lean:253`) (+1 more) |

# VAULT-CBS — Open Bug Tracker

Last updated: 2026-02-27 | Tests: 606 passing / 30 suites / 0 failures

## Critical (Data Corruption / Money Loss)

### BUG-001: LOANPMT0 — Undefined behavior in leap year DIVIDE
- **File:** `src/LOANPMT0.cob` lines 177-185
- **Status:** Open
- **Description:** The leap year check uses the same variable (`WS-LEAP-YEAR-REM4`) for
  both GIVING and REMAINDER in a DIVIDE statement. Per the COBOL standard this is
  undefined behavior. GnuCOBOL currently writes REMAINDER last (so the check works),
  but a compiler update could reverse the order, causing Feb 29 to be miscalculated
  as Feb 28 in leap years. Same pattern for REM100 and REM400.
- **Impact:** Loan next-payment-date could skip Feb 29 in leap years.
- **Fix:** Use FUNCTION MOD or separate GIVING/REMAINDER variables.

### BUG-002: LOANPMT0 — Rounding can cause negative accrued interest
- **File:** `src/LOANPMT0.cob` lines 105-119
- **Status:** Open
- **Description:** ACCT-ACCRUED-INT (6dp) is rounded to 2dp for the payment split
  comparison. When rounding pushes the 2dp value above the actual 6dp value
  (e.g., 100.005678 rounds to 100.01), a payment of $100.01 subtracts from the
  6dp field, producing -0.004322. This negative accrued interest silently propagates
  to future interest calculations and payoff amounts.
- **Impact:** Negative accrued interest → incorrect payoff quotes, potential
  underpayment on loan payoffs.
- **Fix:** Cap the subtraction: `IF LS-PAYMENT-AMT > ACCT-ACCRUED-INT` then zero
  out ACCT-ACCRUED-INT and route the excess to principal, rather than blindly
  subtracting the rounded amount.

## High (Regulatory / Financial Reporting)

### BUG-003: LOANPMT0 — ACCT-AVAIL-BAL not updated after loan payment
- **File:** `src/LOANPMT0.cob` line 138 (after SUBTRACT)
- **Status:** Open
- **Description:** PROCESS-PAYMENT reduces ACCT-LEDGER-BAL but never recomputes
  ACCT-AVAIL-BAL. Every other balance-mutating module (TXNPOST0, WIREXFR0,
  ACHRECV0, DISPMGT0) updates both. After a loan payment, ACCT-AVAIL-BAL is stale.
- **Impact:** Downstream reads of ACCT-AVAIL-BAL see an overstated value.
- **Fix:** Add `COMPUTE ACCT-AVAIL-BAL = ACCT-LEDGER-BAL - ACCT-HOLD-AMOUNT`
  with ON SIZE ERROR after the principal subtraction.

### BUG-004: ODMGMT0 — NSF fee not added to ACCT-YTD-FEES-CHARGED
- **File:** `src/ODMGMT0.cob` lines 165-178
- **Status:** Open
- **Description:** When an NSF fee is assessed, ODMGMT0 increments NSF count fields
  but never adds the fee to ACCT-YTD-FEES-CHARGED. FEECALC0 correctly tracks its
  fees in this field. OD fees are invisible to YTD reporting and statements.
- **Impact:** Customer statements underreport total fees. Year-end disclosures wrong.
- **Fix:** Add `ADD WS-NSF-FEE-AMOUNT TO ACCT-YTD-FEES-CHARGED` after the NSF
  count increments.

### BUG-005: EOMPROC0 — DD/employee fee waivers never triggered in EOM batch
- **File:** `src/EOMPROC0.cob` lines 153-176
- **Status:** Open
- **Description:** `INITIALIZE FEE-SCHEDULE-RECORD` sets FEE-DD-WAIVER and
  FEE-EMPLOYEE-WAIVER to SPACES. FEECALC0 requires these to be "Y" to trigger
  DD and employee waivers. Since EOMPROC0 never sets them, customers with DD or
  employee waiver codes are incorrectly charged monthly fees through the EOM batch.
- **Impact:** Overcharging customers who qualify for fee waivers.
- **Fix:** After INITIALIZE, set FEE-DD-WAIVER and FEE-EMPLOYEE-WAIVER based on
  the account's waiver code (or load from a product fee schedule).

## Medium (Edge Cases / Consistency)

### BUG-006: WIREXFR0 — Wrong error code for reversal insufficient funds
- **File:** `src/WIREXFR0.cob` line 361
- **Status:** Open
- **Description:** PROCESS-REVERSE returns E0003 (system error) when an incoming
  wire reversal has insufficient funds. Should return E0097 (wire insufficient funds),
  consistent with PROCESS-SEND.
- **Impact:** Callers misclassify NSF on wire reversal as a system error.
- **Fix:** Change E0003 to E0097, update error message.

### BUG-007: WIREXFR0 — MTD-Low-Bal not updated on incoming wire reversal
- **File:** `src/WIREXFR0.cob` ~line 382
- **Status:** Open
- **Description:** Reversing an incoming wire (debit) reduces ACCT-LEDGER-BAL but
  does not update ACCT-MTD-LOW-BAL. PROCESS-SEND correctly tracks this.
- **Impact:** MB fee waiver may be incorrectly granted if the reversal drops the
  balance below the monthly low watermark.
- **Fix:** Add MTD-Low-Bal check after the COMPUTE ACCT-AVAIL-BAL in the "I" branch.

### BUG-008: LOANPMT0 — MTD-Low-Bal not tracked after loan payment
- **File:** `src/LOANPMT0.cob` ~line 144
- **Status:** Open
- **Description:** Same as BUG-007 but for loan payments. ACCT-LEDGER-BAL decreases
  but ACCT-MTD-LOW-BAL is never checked/updated.
- **Impact:** Same as BUG-007 — potential incorrect fee waiver.
- **Fix:** Add `IF ACCT-LEDGER-BAL < ACCT-MTD-LOW-BAL MOVE ... END-IF` after
  balance update.

## Known Limitations (Not Bugs — Require Interface Changes)

- **DATEUTIL holiday table:** Covers 2025-2027 only. BDAY calls past 2027 silently
  treat federal holidays as business days. Needs annual table refresh.
- **FEECALC0 de minimis check:** Needs transaction amount in interface to implement.
- **FEECALC0 age waiver:** Needs CIF-RECORD in interface.
- **APYCALC0 fee-inclusive APR:** Needs separate TILA Reg Z module.
- **INTCALC0 Actual/365:** Uses fixed 365 denominator (Actual/365 Fixed convention).
  Actual/Actual is available via basis "D".

## Completed (Recent Rounds)

| Round | Commit | Bugs Fixed |
|-------|--------|------------|
| R38 | ad9754e | FEECALC0 inactive error code, GLPOST0 negative amount guard, ACCTMGMT pending fix |
| R37 | d758e1f | EOD batch coverage — escheated & restricted account skipping |
| R36 | a40e90d | LOANPMT0 frozen guard, HOLDCALC0 DATEUTIL error propagation |
| R35 | 62cb5e4 | REGDCHK0 status guard, EODPROC0 overflow, dual-control coverage |
| R34 | 7e9339e | CIFMGMT CIF-STATUS validation, APYCALC0 negative rate guard |
| R33 | 76669fc | FEECALC0 status guard, GLPOST0/DATEUTIL hardening |
| R32 | 897e099+ | INTCALC0 escheated check, negative balance close, dormant/restricted coverage |

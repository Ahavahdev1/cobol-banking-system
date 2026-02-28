# VAULT-CBS — Bug Tracker (Closed)

Last updated: 2026-02-27 | Tests: 612 passing / 30 suites / 0 failures

All bugs resolved. Final audit (R41) found and fixed one additional issue.

## Critical (Data Corruption / Money Loss)

### BUG-001: LOANPMT0 — Undefined behavior in leap year DIVIDE
- **File:** `src/LOANPMT0.cob` lines 177-185
- **Status:** Fixed (R40 — c872d19)
- **Fix:** Replaced DIVIDE GIVING/REMAINDER with FUNCTION MOD.

### BUG-002: LOANPMT0 — Rounding can cause negative accrued interest
- **File:** `src/LOANPMT0.cob` lines 105-119
- **Status:** Fixed (R40 — c872d19)
- **Fix:** Cap ACCT-ACCRUED-INT at zero after subtraction. Test LP-046.

## High (Regulatory / Financial Reporting)

### BUG-003: LOANPMT0 — ACCT-AVAIL-BAL not updated after loan payment
- **File:** `src/LOANPMT0.cob` line 138
- **Status:** Fixed (R40 — c872d19)
- **Fix:** Added COMPUTE ACCT-AVAIL-BAL with ON SIZE ERROR. Test LP-047.

### BUG-004: ODMGMT0 — NSF fee not added to ACCT-YTD-FEES-CHARGED
- **File:** `src/ODMGMT0.cob` lines 165-178
- **Status:** Fixed (R40 — c872d19)
- **Fix:** Added ADD WS-NSF-FEE-AMOUNT TO ACCT-YTD-FEES-CHARGED. Test OD-025.

### BUG-005: EOMPROC0 — DD/employee fee waivers never triggered in EOM batch
- **File:** `src/EOMPROC0.cob` lines 153-176
- **Status:** Fixed (R40 — c872d19)
- **Fix:** Set FEE-DD-WAIVER/FEE-EMPLOYEE-WAIVER from ACCT-FEE-WAIVER-CODE. Test EM-023.

## Medium (Edge Cases / Consistency)

### BUG-006: WIREXFR0 — Wrong error code for reversal insufficient funds
- **File:** `src/WIREXFR0.cob` line 361
- **Status:** Fixed (R40 — c872d19)
- **Fix:** Changed E0003 to E0097. Updated test WR-028.

### BUG-007: WIREXFR0 — MTD-Low-Bal not updated on incoming wire reversal
- **File:** `src/WIREXFR0.cob` ~line 382
- **Status:** Fixed (R40 — c872d19)
- **Fix:** Added MTD-Low-Bal watermark check after reversal debit.

### BUG-008: LOANPMT0 — MTD-Low-Bal not tracked after loan payment
- **File:** `src/LOANPMT0.cob` ~line 144
- **Status:** Fixed (R40 — c872d19)
- **Fix:** Added MTD-Low-Bal watermark check after payment. Test LP-048.

### BUG-009: LOANPMT0 — Late fee flag reset on partial payment
- **File:** `src/LOANPMT0.cob` line 232
- **Status:** Fixed (R41)
- **Fix:** Only reset ACCT-LATE-FEE-ASSESSED when ACCT-PAST-DUE-AMT = ZERO. Test LP-049.

## Known Limitations (Not Bugs — Require Interface Changes)

- **DATEUTIL holiday table:** Covers 2025-2027 only. BDAY calls past 2027 silently
  treat federal holidays as business days. Needs annual table refresh.
- **FEECALC0 de minimis check:** Needs transaction amount in interface to implement.
- **FEECALC0 age waiver:** Needs CIF-RECORD in interface.
- **APYCALC0 fee-inclusive APR:** Needs separate TILA Reg Z module.
- **INTCALC0 Actual/365:** Uses fixed 365 denominator (Actual/365 Fixed convention).
  Actual/Actual is available via basis "D".

## Completed Rounds

| Round | Commit | Bugs Fixed |
|-------|--------|------------|
| R41 | (this) | LOANPMT0 late fee flag partial payment |
| R40 | c872d19 | 8 bugs: LOANPMT0 (4), ODMGMT0 (1), EOMPROC0 (1), WIREXFR0 (2) |
| R39 | 8940417 | HOLDCALC0 boundary + DISPMGT0 denied-dispute coverage |
| R38 | ad9754e | FEECALC0 inactive error code, GLPOST0 negative amount guard, ACCTMGMT pending fix |
| R37 | d758e1f | EOD batch coverage — escheated & restricted account skipping |
| R36 | a40e90d | LOANPMT0 frozen guard, HOLDCALC0 DATEUTIL error propagation |
| R35 | 62cb5e4 | REGDCHK0 status guard, EODPROC0 overflow, dual-control coverage |
| R34 | 7e9339e | CIFMGMT CIF-STATUS validation, APYCALC0 negative rate guard |
| R33 | 76669fc | FEECALC0 status guard, GLPOST0/DATEUTIL hardening |
| R32 | 897e099+ | INTCALC0 escheated check, negative balance close, dormant/restricted coverage |

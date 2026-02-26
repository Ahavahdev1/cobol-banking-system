IDENTIFICATION DIVISION.
PROGRAM-ID. TEST-INTCALC.
*> ================================================================
*> TEST-INTCALC - Test suite for INTCALC0 Interest Calculator
*> Tests: Daily interest, accrual bases, tiered rates, edge cases
*> 27 tests (IC-001 to IC-027)
*> Includes P1 audit: year-end rollover + century boundary tests
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-TEST-COUNT          PIC 9(3) VALUE 0.
01  WS-PASS-COUNT          PIC 9(3) VALUE 0.
01  WS-FAIL-COUNT          PIC 9(3) VALUE 0.
01  WS-TEST-NAME           PIC X(60).

*> Account record (matches LINKAGE in INTCALC0)
COPY CPYACCT.

*> Interest calc parameters
01  WS-CALC-DATE            PIC 9(8).
01  WS-INT-RESULT.
    05  WS-INT-RESULT-CODE  PIC X(5).
    05  WS-INT-RESULT-MSG   PIC X(50).
    05  WS-DAILY-INT-AMT    PIC S9(11)V9(6).
    05  WS-NEW-ACCRUED      PIC S9(11)V9(6).
    05  WS-PAYMENT-AMT      PIC S9(11)V99.
    05  WS-PAYMENT-DUE      PIC X(1).

01  WS-EXPECTED-INT         PIC S9(11)V9(6).
01  WS-EXPECTED-ACCRUED     PIC S9(11)V9(6).
01  WS-EXPECTED-PMT         PIC S9(11)V99.

PROCEDURE DIVISION.
MAIN-PROGRAM.
    DISPLAY "========================================".
    DISPLAY "TEST SUITE: INTCALC - Interest Calc".
    DISPLAY "========================================".

    PERFORM TEST-IC-001
    PERFORM TEST-IC-002
    PERFORM TEST-IC-003
    PERFORM TEST-IC-004
    PERFORM TEST-IC-005
    PERFORM TEST-IC-006
    PERFORM TEST-IC-007
    PERFORM TEST-IC-008
    PERFORM TEST-IC-009
    PERFORM TEST-IC-010
    PERFORM TEST-IC-011
    PERFORM TEST-IC-012
    PERFORM TEST-IC-013
    PERFORM TEST-IC-014
    PERFORM TEST-IC-015
    PERFORM TEST-IC-016
    PERFORM TEST-IC-017
    PERFORM TEST-IC-018
    PERFORM TEST-IC-019
    PERFORM TEST-IC-020
    PERFORM TEST-IC-021
    PERFORM TEST-IC-022
    PERFORM TEST-IC-023
    PERFORM TEST-IC-024
    PERFORM TEST-IC-025
    PERFORM TEST-IC-026
    PERFORM TEST-IC-027
    PERFORM TEST-IC-028
    PERFORM TEST-IC-029

    DISPLAY "========================================".
    DISPLAY "RESULTS: " WS-PASS-COUNT "/" WS-TEST-COUNT
            " PASSED".
    DISPLAY "         " WS-FAIL-COUNT " FAILED".
    DISPLAY "========================================".
    MOVE WS-FAIL-COUNT TO RETURN-CODE
    STOP RUN.

*> ---------------------------------------------------------------
*> IC-001: $10,000 @ 5% Actual/365 -> daily = $1.369863
*> ---------------------------------------------------------------
TEST-IC-001.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IC-001: $10K @ 5% Actual/365" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-INT-RESULT
    MOVE 10000.00 TO ACCT-LEDGER-BAL
    MOVE 10000.00 TO ACCT-AVAIL-BAL
    MOVE 10000.00 TO ACCT-COLLECTED-BAL
    MOVE 5.0000000 TO ACCT-INT-RATE
    MOVE "A" TO ACCT-INT-ACCRUAL-BASIS
    MOVE "DB" TO ACCT-INT-CALC-METHOD
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "A" TO ACCT-STATUS
    MOVE "SV" TO ACCT-SUB-TYPE
    MOVE 20260215 TO WS-CALC-DATE
    MOVE 1.369863 TO WS-EXPECTED-INT
    CALL "INTCALC0" USING ACCT-RECORD WS-CALC-DATE WS-INT-RESULT
    IF WS-INT-RESULT-CODE = "E0000"
        IF WS-DAILY-INT-AMT = WS-EXPECTED-INT
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=" WS-EXPECTED-INT
                " actual=" WS-DAILY-INT-AMT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-INT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> IC-002: $10,000 @ 5% Actual/360 -> daily = $1.388888
*> ---------------------------------------------------------------
TEST-IC-002.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IC-002: $10K @ 5% Actual/360" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-INT-RESULT
    MOVE 10000.00 TO ACCT-LEDGER-BAL
    MOVE 10000.00 TO ACCT-AVAIL-BAL
    MOVE 5.0000000 TO ACCT-INT-RATE
    MOVE "B" TO ACCT-INT-ACCRUAL-BASIS
    MOVE "DB" TO ACCT-INT-CALC-METHOD
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "A" TO ACCT-STATUS
    MOVE 20260215 TO WS-CALC-DATE
    MOVE 1.388888 TO WS-EXPECTED-INT
    CALL "INTCALC0" USING ACCT-RECORD WS-CALC-DATE WS-INT-RESULT
    IF WS-INT-RESULT-CODE = "E0000"
        IF WS-DAILY-INT-AMT = WS-EXPECTED-INT
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=" WS-EXPECTED-INT
                " actual=" WS-DAILY-INT-AMT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-INT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> IC-003: $10,000 @ 5% 30/360 -> daily = $1.388888
*> ---------------------------------------------------------------
TEST-IC-003.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IC-003: $10K @ 5% 30/360" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-INT-RESULT
    MOVE 10000.00 TO ACCT-LEDGER-BAL
    MOVE 10000.00 TO ACCT-AVAIL-BAL
    MOVE 5.0000000 TO ACCT-INT-RATE
    MOVE "C" TO ACCT-INT-ACCRUAL-BASIS
    MOVE "DB" TO ACCT-INT-CALC-METHOD
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "A" TO ACCT-STATUS
    MOVE 20260215 TO WS-CALC-DATE
    MOVE 1.388888 TO WS-EXPECTED-INT
    CALL "INTCALC0" USING ACCT-RECORD WS-CALC-DATE WS-INT-RESULT
    IF WS-INT-RESULT-CODE = "E0000"
        IF WS-DAILY-INT-AMT = WS-EXPECTED-INT
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=" WS-EXPECTED-INT
                " actual=" WS-DAILY-INT-AMT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-INT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> IC-004: Zero balance @ 5% Actual/365 -> daily = $0.000000
*> ---------------------------------------------------------------
TEST-IC-004.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IC-004: $0 balance -> no interest" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-INT-RESULT
    MOVE 0.00 TO ACCT-LEDGER-BAL
    MOVE 5.0000000 TO ACCT-INT-RATE
    MOVE "A" TO ACCT-INT-ACCRUAL-BASIS
    MOVE "DB" TO ACCT-INT-CALC-METHOD
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "A" TO ACCT-STATUS
    MOVE 20260215 TO WS-CALC-DATE
    MOVE 0.000000 TO WS-EXPECTED-INT
    CALL "INTCALC0" USING ACCT-RECORD WS-CALC-DATE WS-INT-RESULT
    IF WS-INT-RESULT-CODE = "E0000"
        IF WS-DAILY-INT-AMT = WS-EXPECTED-INT
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=" WS-EXPECTED-INT
                " actual=" WS-DAILY-INT-AMT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-INT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> IC-005: Max balance $9,999,999.99 @ 5% Actual/365
*>         -> daily = $1,369.863012
*> ---------------------------------------------------------------
TEST-IC-005.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IC-005: $9,999,999.99 @ 5% A/365" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-INT-RESULT
    MOVE 9999999.99 TO ACCT-LEDGER-BAL
    MOVE 9999999.99 TO ACCT-AVAIL-BAL
    MOVE 5.0000000 TO ACCT-INT-RATE
    MOVE "A" TO ACCT-INT-ACCRUAL-BASIS
    MOVE "DB" TO ACCT-INT-CALC-METHOD
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "A" TO ACCT-STATUS
    MOVE 20260215 TO WS-CALC-DATE
    MOVE 1369.863012 TO WS-EXPECTED-INT
    CALL "INTCALC0" USING ACCT-RECORD WS-CALC-DATE WS-INT-RESULT
    IF WS-INT-RESULT-CODE = "E0000"
        IF WS-DAILY-INT-AMT = WS-EXPECTED-INT
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=" WS-EXPECTED-INT
                " actual=" WS-DAILY-INT-AMT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-INT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> IC-006: Tiny balance $0.01 @ 5% Actual/365 -> $0.000001
*> ---------------------------------------------------------------
TEST-IC-006.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IC-006: $0.01 @ 5% A/365 -> $0.000001" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-INT-RESULT
    MOVE 0.01 TO ACCT-LEDGER-BAL
    MOVE 0.01 TO ACCT-AVAIL-BAL
    MOVE 5.0000000 TO ACCT-INT-RATE
    MOVE "A" TO ACCT-INT-ACCRUAL-BASIS
    MOVE "DB" TO ACCT-INT-CALC-METHOD
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "A" TO ACCT-STATUS
    MOVE 20260215 TO WS-CALC-DATE
    MOVE 0.000001 TO WS-EXPECTED-INT
    CALL "INTCALC0" USING ACCT-RECORD WS-CALC-DATE WS-INT-RESULT
    IF WS-INT-RESULT-CODE = "E0000"
        IF WS-DAILY-INT-AMT = WS-EXPECTED-INT
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=" WS-EXPECTED-INT
                " actual=" WS-DAILY-INT-AMT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-INT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> IC-007: $10,000 @ 18% Actual/365 -> daily = $4.931506
*> ---------------------------------------------------------------
TEST-IC-007.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IC-007: $10K @ 18% A/365" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-INT-RESULT
    MOVE 10000.00 TO ACCT-LEDGER-BAL
    MOVE 10000.00 TO ACCT-AVAIL-BAL
    MOVE 18.0000000 TO ACCT-INT-RATE
    MOVE "A" TO ACCT-INT-ACCRUAL-BASIS
    MOVE "DB" TO ACCT-INT-CALC-METHOD
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "A" TO ACCT-STATUS
    MOVE 20260215 TO WS-CALC-DATE
    MOVE 4.931506 TO WS-EXPECTED-INT
    CALL "INTCALC0" USING ACCT-RECORD WS-CALC-DATE WS-INT-RESULT
    IF WS-INT-RESULT-CODE = "E0000"
        IF WS-DAILY-INT-AMT = WS-EXPECTED-INT
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=" WS-EXPECTED-INT
                " actual=" WS-DAILY-INT-AMT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-INT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> IC-008: $25,000 @ 4.1234567% Actual/365 -> daily = $2.824285
*> ---------------------------------------------------------------
TEST-IC-008.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IC-008: $25K @ 4.1234567% A/365" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-INT-RESULT
    MOVE 25000.00 TO ACCT-LEDGER-BAL
    MOVE 25000.00 TO ACCT-AVAIL-BAL
    MOVE 4.1234567 TO ACCT-INT-RATE
    MOVE "A" TO ACCT-INT-ACCRUAL-BASIS
    MOVE "DB" TO ACCT-INT-CALC-METHOD
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "A" TO ACCT-STATUS
    MOVE 20260215 TO WS-CALC-DATE
    MOVE 2.824285 TO WS-EXPECTED-INT
    CALL "INTCALC0" USING ACCT-RECORD WS-CALC-DATE WS-INT-RESULT
    IF WS-INT-RESULT-CODE = "E0000"
        IF WS-DAILY-INT-AMT = WS-EXPECTED-INT
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=" WS-EXPECTED-INT
                " actual=" WS-DAILY-INT-AMT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-INT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> IC-009: 30-day accrual at $1.369863/day = $41.095890
*> ---------------------------------------------------------------
TEST-IC-009.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IC-009: 30-day accrual $1.369863/d" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-INT-RESULT
    MOVE 10000.00 TO ACCT-LEDGER-BAL
    MOVE 10000.00 TO ACCT-AVAIL-BAL
    MOVE 5.0000000 TO ACCT-INT-RATE
    MOVE "A" TO ACCT-INT-ACCRUAL-BASIS
    MOVE "DB" TO ACCT-INT-CALC-METHOD
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "A" TO ACCT-STATUS
    MOVE "M" TO ACCT-INT-PAY-FREQ
    *> Set existing accrued to 29 days worth: 29 * 1.369863 = 39.726027
    MOVE 39.726027 TO ACCT-ACCRUED-INT
    MOVE 20260215 TO WS-CALC-DATE
    *> After one more day: 39.726027 + 1.369863 = 41.095890
    MOVE 41.095890 TO WS-EXPECTED-ACCRUED
    CALL "INTCALC0" USING ACCT-RECORD WS-CALC-DATE WS-INT-RESULT
    IF WS-INT-RESULT-CODE = "E0000"
        IF WS-NEW-ACCRUED = WS-EXPECTED-ACCRUED
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=" WS-EXPECTED-ACCRUED
                " actual=" WS-NEW-ACCRUED
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-INT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> IC-010: Payment includes today's interest
*> $10K@5% A/365: daily=1.369863, accrued+daily=42.465753->$42.47
*> ---------------------------------------------------------------
TEST-IC-010.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IC-010: Payment round $41.095890->$42.47"
        TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-INT-RESULT
    MOVE 10000.00 TO ACCT-LEDGER-BAL
    MOVE 10000.00 TO ACCT-AVAIL-BAL
    MOVE 5.0000000 TO ACCT-INT-RATE
    MOVE "A" TO ACCT-INT-ACCRUAL-BASIS
    MOVE "DB" TO ACCT-INT-CALC-METHOD
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "A" TO ACCT-STATUS
    MOVE "M" TO ACCT-INT-PAY-FREQ
    *> Simulate 30 days accrued interest
    MOVE 41.095890 TO ACCT-ACCRUED-INT
    *> Set next pay date to today to trigger payment
    MOVE 20260215 TO ACCT-INT-NEXT-PAY-DATE
    MOVE 20260215 TO WS-CALC-DATE
    MOVE 42.47 TO WS-EXPECTED-PMT
    CALL "INTCALC0" USING ACCT-RECORD WS-CALC-DATE WS-INT-RESULT
    IF WS-INT-RESULT-CODE = "E0000"
        IF WS-PAYMENT-DUE = "Y"
            IF WS-PAYMENT-AMT = WS-EXPECTED-PMT
                ADD 1 TO WS-PASS-COUNT
                DISPLAY "  PASS: " WS-TEST-NAME
            ELSE
                ADD 1 TO WS-FAIL-COUNT
                DISPLAY "  FAIL: " WS-TEST-NAME
                    " expected=" WS-EXPECTED-PMT
                    " actual=" WS-PAYMENT-AMT
            END-IF
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " payment-due=N expected=Y"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-INT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> IC-011: Accrual reset after payment -> ACCRUED = 0
*> ---------------------------------------------------------------
TEST-IC-011.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IC-011: Accrual reset after payment" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-INT-RESULT
    MOVE 10000.00 TO ACCT-LEDGER-BAL
    MOVE 10000.00 TO ACCT-AVAIL-BAL
    MOVE 5.0000000 TO ACCT-INT-RATE
    MOVE "A" TO ACCT-INT-ACCRUAL-BASIS
    MOVE "DB" TO ACCT-INT-CALC-METHOD
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "A" TO ACCT-STATUS
    MOVE "M" TO ACCT-INT-PAY-FREQ
    MOVE 41.095890 TO ACCT-ACCRUED-INT
    MOVE 20260215 TO ACCT-INT-NEXT-PAY-DATE
    MOVE 20260215 TO WS-CALC-DATE
    *> After payment, new accrued should reset
    *> It may include the current day's interest: 1.369863
    *> or be zero depending on implementation
    CALL "INTCALC0" USING ACCT-RECORD WS-CALC-DATE WS-INT-RESULT
    IF WS-INT-RESULT-CODE = "E0000"
        IF WS-PAYMENT-DUE = "Y"
            *> After payment, new accrued resets to current day
            *> interest only (1.369863) or 0
            IF WS-NEW-ACCRUED <= 1.369863
                ADD 1 TO WS-PASS-COUNT
                DISPLAY "  PASS: " WS-TEST-NAME
                    " accrued=" WS-NEW-ACCRUED
            ELSE
                ADD 1 TO WS-FAIL-COUNT
                DISPLAY "  FAIL: " WS-TEST-NAME
                    " accrued=" WS-NEW-ACCRUED
                    " expected<=1.369863"
            END-IF
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " payment-due=N expected=Y"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-INT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> IC-012: Tiered rate tier 1: $5,000 balance, rate 0.50%
*> ---------------------------------------------------------------
TEST-IC-012.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IC-012: Tiered T1 $5K @ 0.50%" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-INT-RESULT
    MOVE 5000.00 TO ACCT-LEDGER-BAL
    MOVE 5000.00 TO ACCT-AVAIL-BAL
    MOVE 0.5000000 TO ACCT-INT-RATE
    MOVE "A" TO ACCT-INT-ACCRUAL-BASIS
    MOVE "DB" TO ACCT-INT-CALC-METHOD
    MOVE "T" TO ACCT-INT-RATE-TYPE
    MOVE "A" TO ACCT-STATUS
    MOVE 20260215 TO WS-CALC-DATE
    *> 5000 * 0.005 / 365 = 0.068493
    MOVE 0.068493 TO WS-EXPECTED-INT
    CALL "INTCALC0" USING ACCT-RECORD WS-CALC-DATE WS-INT-RESULT
    IF WS-INT-RESULT-CODE = "E0000"
        IF WS-DAILY-INT-AMT = WS-EXPECTED-INT
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=" WS-EXPECTED-INT
                " actual=" WS-DAILY-INT-AMT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-INT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> IC-013: Tiered rate tier 2: $25,000 balance, rate 1.00%
*> ---------------------------------------------------------------
TEST-IC-013.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IC-013: Tiered T2 $25K @ 1.00%" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-INT-RESULT
    MOVE 25000.00 TO ACCT-LEDGER-BAL
    MOVE 25000.00 TO ACCT-AVAIL-BAL
    MOVE 1.0000000 TO ACCT-INT-RATE
    MOVE "A" TO ACCT-INT-ACCRUAL-BASIS
    MOVE "DB" TO ACCT-INT-CALC-METHOD
    MOVE "T" TO ACCT-INT-RATE-TYPE
    MOVE "A" TO ACCT-STATUS
    MOVE 20260215 TO WS-CALC-DATE
    *> 25000 * 0.01 / 365 = 0.684931
    MOVE 0.684931 TO WS-EXPECTED-INT
    CALL "INTCALC0" USING ACCT-RECORD WS-CALC-DATE WS-INT-RESULT
    IF WS-INT-RESULT-CODE = "E0000"
        IF WS-DAILY-INT-AMT = WS-EXPECTED-INT
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=" WS-EXPECTED-INT
                " actual=" WS-DAILY-INT-AMT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-INT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> IC-014: Tiered rate tier 3: $75,000 balance, rate 1.50%
*> ---------------------------------------------------------------
TEST-IC-014.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IC-014: Tiered T3 $75K @ 1.50%" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-INT-RESULT
    MOVE 75000.00 TO ACCT-LEDGER-BAL
    MOVE 75000.00 TO ACCT-AVAIL-BAL
    MOVE 1.5000000 TO ACCT-INT-RATE
    MOVE "A" TO ACCT-INT-ACCRUAL-BASIS
    MOVE "DB" TO ACCT-INT-CALC-METHOD
    MOVE "T" TO ACCT-INT-RATE-TYPE
    MOVE "A" TO ACCT-STATUS
    MOVE 20260215 TO WS-CALC-DATE
    *> 75000 * 0.015 / 365 = 3.082191
    MOVE 3.082191 TO WS-EXPECTED-INT
    CALL "INTCALC0" USING ACCT-RECORD WS-CALC-DATE WS-INT-RESULT
    IF WS-INT-RESULT-CODE = "E0000"
        IF WS-DAILY-INT-AMT = WS-EXPECTED-INT
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=" WS-EXPECTED-INT
                " actual=" WS-DAILY-INT-AMT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-INT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> IC-015: Tiered rate tier 4: $150,000 balance, rate 2.00%
*> ---------------------------------------------------------------
TEST-IC-015.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IC-015: Tiered T4 $150K @ 2.00%" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-INT-RESULT
    MOVE 150000.00 TO ACCT-LEDGER-BAL
    MOVE 150000.00 TO ACCT-AVAIL-BAL
    MOVE 2.0000000 TO ACCT-INT-RATE
    MOVE "A" TO ACCT-INT-ACCRUAL-BASIS
    MOVE "DB" TO ACCT-INT-CALC-METHOD
    MOVE "T" TO ACCT-INT-RATE-TYPE
    MOVE "A" TO ACCT-STATUS
    MOVE 20260215 TO WS-CALC-DATE
    *> 150000 * 0.02 / 365 = 8.219178
    MOVE 8.219178 TO WS-EXPECTED-INT
    CALL "INTCALC0" USING ACCT-RECORD WS-CALC-DATE WS-INT-RESULT
    IF WS-INT-RESULT-CODE = "E0000"
        IF WS-DAILY-INT-AMT = WS-EXPECTED-INT
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=" WS-EXPECTED-INT
                " actual=" WS-DAILY-INT-AMT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-INT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> IC-016: $200,000 @ 6.5% Actual/360 -> daily = $36.111111
*> ---------------------------------------------------------------
TEST-IC-016.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IC-016: $200K @ 6.5% A/360" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-INT-RESULT
    MOVE 200000.00 TO ACCT-LEDGER-BAL
    MOVE 200000.00 TO ACCT-AVAIL-BAL
    MOVE 6.5000000 TO ACCT-INT-RATE
    MOVE "B" TO ACCT-INT-ACCRUAL-BASIS
    MOVE "DB" TO ACCT-INT-CALC-METHOD
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "A" TO ACCT-STATUS
    MOVE 20260215 TO WS-CALC-DATE
    MOVE 36.111111 TO WS-EXPECTED-INT
    CALL "INTCALC0" USING ACCT-RECORD WS-CALC-DATE WS-INT-RESULT
    IF WS-INT-RESULT-CODE = "E0000"
        IF WS-DAILY-INT-AMT = WS-EXPECTED-INT
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=" WS-EXPECTED-INT
                " actual=" WS-DAILY-INT-AMT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-INT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> IC-017: Leap year 2024, Actual/365 still uses 365
*>         $10,000 @ 5% -> daily = $1.369863
*> ---------------------------------------------------------------
TEST-IC-017.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IC-017: Leap yr A/365 still /365" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-INT-RESULT
    MOVE 10000.00 TO ACCT-LEDGER-BAL
    MOVE 10000.00 TO ACCT-AVAIL-BAL
    MOVE 5.0000000 TO ACCT-INT-RATE
    MOVE "A" TO ACCT-INT-ACCRUAL-BASIS
    MOVE "DB" TO ACCT-INT-CALC-METHOD
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "A" TO ACCT-STATUS
    MOVE 20240229 TO WS-CALC-DATE
    MOVE 1.369863 TO WS-EXPECTED-INT
    CALL "INTCALC0" USING ACCT-RECORD WS-CALC-DATE WS-INT-RESULT
    IF WS-INT-RESULT-CODE = "E0000"
        IF WS-DAILY-INT-AMT = WS-EXPECTED-INT
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=" WS-EXPECTED-INT
                " actual=" WS-DAILY-INT-AMT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-INT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> IC-018: Actual/Actual basis "D" in 2024 (366 days)
*>         $10,000 @ 5% / 366 = $1.366120
*> ---------------------------------------------------------------
TEST-IC-018.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IC-018: A/A 2024 /366 = $1.366120" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-INT-RESULT
    MOVE 10000.00 TO ACCT-LEDGER-BAL
    MOVE 10000.00 TO ACCT-AVAIL-BAL
    MOVE 5.0000000 TO ACCT-INT-RATE
    MOVE "D" TO ACCT-INT-ACCRUAL-BASIS
    MOVE "DB" TO ACCT-INT-CALC-METHOD
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "A" TO ACCT-STATUS
    MOVE 20240615 TO WS-CALC-DATE
    MOVE 1.366120 TO WS-EXPECTED-INT
    CALL "INTCALC0" USING ACCT-RECORD WS-CALC-DATE WS-INT-RESULT
    IF WS-INT-RESULT-CODE = "E0000"
        IF WS-DAILY-INT-AMT = WS-EXPECTED-INT
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=" WS-EXPECTED-INT
                " actual=" WS-DAILY-INT-AMT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-INT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> IC-019: Negative balance -> daily interest = $0.000000
*> ---------------------------------------------------------------
TEST-IC-019.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IC-019: Negative bal -> no interest" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-INT-RESULT
    MOVE -500.00 TO ACCT-LEDGER-BAL
    MOVE -500.00 TO ACCT-AVAIL-BAL
    MOVE 5.0000000 TO ACCT-INT-RATE
    MOVE "A" TO ACCT-INT-ACCRUAL-BASIS
    MOVE "DB" TO ACCT-INT-CALC-METHOD
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "A" TO ACCT-STATUS
    MOVE 20260215 TO WS-CALC-DATE
    MOVE 0.000000 TO WS-EXPECTED-INT
    CALL "INTCALC0" USING ACCT-RECORD WS-CALC-DATE WS-INT-RESULT
    IF WS-INT-RESULT-CODE = "E0000"
        IF WS-DAILY-INT-AMT = WS-EXPECTED-INT
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=" WS-EXPECTED-INT
                " actual=" WS-DAILY-INT-AMT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-INT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> IC-020: Dormant account (status "D") still earns interest
*> ---------------------------------------------------------------
TEST-IC-020.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IC-020: Dormant acct still earns int" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-INT-RESULT
    MOVE 10000.00 TO ACCT-LEDGER-BAL
    MOVE 10000.00 TO ACCT-AVAIL-BAL
    MOVE 5.0000000 TO ACCT-INT-RATE
    MOVE "A" TO ACCT-INT-ACCRUAL-BASIS
    MOVE "DB" TO ACCT-INT-CALC-METHOD
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "D" TO ACCT-STATUS
    MOVE 20260215 TO WS-CALC-DATE
    MOVE 1.369863 TO WS-EXPECTED-INT
    CALL "INTCALC0" USING ACCT-RECORD WS-CALC-DATE WS-INT-RESULT
    IF WS-INT-RESULT-CODE = "E0000"
        IF WS-DAILY-INT-AMT = WS-EXPECTED-INT
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=" WS-EXPECTED-INT
                " actual=" WS-DAILY-INT-AMT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-INT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> IC-021: Closed account (status "C") -> no interest, E0053
*> ---------------------------------------------------------------
TEST-IC-021.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IC-021: Closed acct -> E0053" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-INT-RESULT
    MOVE 10000.00 TO ACCT-LEDGER-BAL
    MOVE 5.0000000 TO ACCT-INT-RATE
    MOVE "A" TO ACCT-INT-ACCRUAL-BASIS
    MOVE "DB" TO ACCT-INT-CALC-METHOD
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "C" TO ACCT-STATUS
    MOVE 20260215 TO WS-CALC-DATE
    CALL "INTCALC0" USING ACCT-RECORD WS-CALC-DATE WS-INT-RESULT
    IF WS-INT-RESULT-CODE = "E0053"
        IF WS-DAILY-INT-AMT = 0
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " int should be 0, actual="
                WS-DAILY-INT-AMT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected rc=E0053 actual="
            WS-INT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> IC-022: CD accrues until maturity, stops after
*> ---------------------------------------------------------------
TEST-IC-022.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IC-022: CD stops after maturity" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-INT-RESULT
    MOVE 10000.00 TO ACCT-LEDGER-BAL
    MOVE 10000.00 TO ACCT-AVAIL-BAL
    MOVE 5.0000000 TO ACCT-INT-RATE
    MOVE "A" TO ACCT-INT-ACCRUAL-BASIS
    MOVE "DB" TO ACCT-INT-CALC-METHOD
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "A" TO ACCT-STATUS
    MOVE "CD" TO ACCT-SUB-TYPE
    MOVE "CD01" TO ACCT-PRODUCT-CODE
    MOVE 12 TO ACCT-CD-TERM-MONTHS
    *> Maturity date is Feb 1, calc date is Feb 15 (past maturity)
    MOVE 20260201 TO ACCT-MATURITY-DATE
    MOVE 20260215 TO WS-CALC-DATE
    MOVE 0.000000 TO WS-EXPECTED-INT
    CALL "INTCALC0" USING ACCT-RECORD WS-CALC-DATE WS-INT-RESULT
    *> Should either return 0 interest or an error code
    IF WS-DAILY-INT-AMT = WS-EXPECTED-INT
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=0 actual=" WS-DAILY-INT-AMT
    END-IF.

*> ---------------------------------------------------------------
*> IC-023: Interest accrual on Dec 31 (year boundary)
*>         $10K @ 5% Actual/365, calc date 20251231
*>         daily interest = 10000 * 0.05 / 365 = $1.369863
*>         P1 audit: year-end rollover
*> ---------------------------------------------------------------
TEST-IC-023.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IC-023: Year boundary Dec31 $10K@5% A/365"
        TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-INT-RESULT
    MOVE 10000.00 TO ACCT-LEDGER-BAL
    MOVE 10000.00 TO ACCT-AVAIL-BAL
    MOVE 10000.00 TO ACCT-COLLECTED-BAL
    MOVE 5.0000000 TO ACCT-INT-RATE
    MOVE "A" TO ACCT-INT-ACCRUAL-BASIS
    MOVE "DB" TO ACCT-INT-CALC-METHOD
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "A" TO ACCT-STATUS
    MOVE "SV" TO ACCT-SUB-TYPE
    MOVE 20251231 TO WS-CALC-DATE
    MOVE 1.369863 TO WS-EXPECTED-INT
    CALL "INTCALC0" USING ACCT-RECORD WS-CALC-DATE WS-INT-RESULT
    IF WS-INT-RESULT-CODE = "E0000"
        IF WS-DAILY-INT-AMT = WS-EXPECTED-INT
            IF WS-DAILY-INT-AMT > 0
                ADD 1 TO WS-PASS-COUNT
                DISPLAY "  PASS: " WS-TEST-NAME
            ELSE
                ADD 1 TO WS-FAIL-COUNT
                DISPLAY "  FAIL: " WS-TEST-NAME
                    " daily interest should be > 0"
            END-IF
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=" WS-EXPECTED-INT
                " actual=" WS-DAILY-INT-AMT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-INT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> IC-024: Actual/Actual in non-leap year 2025 -> divisor = 365
*>         $10K @ 5% / 365 = $1.369863
*>         P1 audit: century boundary (A/A non-leap)
*> ---------------------------------------------------------------
TEST-IC-024.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IC-024: A/A non-leap 2025 /365=$1.369863"
        TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-INT-RESULT
    MOVE 10000.00 TO ACCT-LEDGER-BAL
    MOVE 10000.00 TO ACCT-AVAIL-BAL
    MOVE 5.0000000 TO ACCT-INT-RATE
    MOVE "D" TO ACCT-INT-ACCRUAL-BASIS
    MOVE "DB" TO ACCT-INT-CALC-METHOD
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "A" TO ACCT-STATUS
    MOVE 20250615 TO WS-CALC-DATE
    MOVE 1.369863 TO WS-EXPECTED-INT
    CALL "INTCALC0" USING ACCT-RECORD WS-CALC-DATE WS-INT-RESULT
    IF WS-INT-RESULT-CODE = "E0000"
        IF WS-DAILY-INT-AMT = WS-EXPECTED-INT
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=" WS-EXPECTED-INT
                " actual=" WS-DAILY-INT-AMT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-INT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> IC-025: Semi-annual payment frequency triggers on pay date
*>         Set ACCT-INT-NEXT-PAY-DATE = calc date
*>         Verify payment is due (WS-PAYMENT-DUE = "Y")
*>         P1 audit: payment schedule edge case
*> ---------------------------------------------------------------
TEST-IC-025.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IC-025: Semi-annual pmt triggers on date"
        TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-INT-RESULT
    MOVE 10000.00 TO ACCT-LEDGER-BAL
    MOVE 10000.00 TO ACCT-AVAIL-BAL
    MOVE 5.0000000 TO ACCT-INT-RATE
    MOVE "A" TO ACCT-INT-ACCRUAL-BASIS
    MOVE "DB" TO ACCT-INT-CALC-METHOD
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "A" TO ACCT-STATUS
    MOVE "S" TO ACCT-INT-PAY-FREQ
    *> Simulate ~182 days of accrued interest (semi-annual)
    *> 182 * 1.369863 = 249.315066
    MOVE 249.315066 TO ACCT-ACCRUED-INT
    *> Set next pay date to calc date to trigger payment
    MOVE 20260630 TO ACCT-INT-NEXT-PAY-DATE
    MOVE 20260630 TO WS-CALC-DATE
    CALL "INTCALC0" USING ACCT-RECORD WS-CALC-DATE WS-INT-RESULT
    IF WS-INT-RESULT-CODE = "E0000"
        IF WS-PAYMENT-DUE = "Y"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " pmt=" WS-PAYMENT-AMT
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " payment-due=N expected=Y"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-INT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> IC-026: Invalid accrual basis "Z" -> E0050
*> ---------------------------------------------------------------
TEST-IC-026.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IC-026: Invalid accrual basis Z -> E0050"
        TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-INT-RESULT
    MOVE 10000.00 TO ACCT-LEDGER-BAL
    MOVE 10000.00 TO ACCT-AVAIL-BAL
    MOVE 5.0000000 TO ACCT-INT-RATE
    MOVE "Z" TO ACCT-INT-ACCRUAL-BASIS
    MOVE "DB" TO ACCT-INT-CALC-METHOD
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "A" TO ACCT-STATUS
    MOVE 20260215 TO WS-CALC-DATE
    CALL "INTCALC0" USING ACCT-RECORD WS-CALC-DATE WS-INT-RESULT
    IF WS-INT-RESULT-CODE = "E0050"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected rc=E0050 actual="
            WS-INT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> IC-027: Payment triggered when batch date > pay date (<= fix)
*> ACCT-INT-NEXT-PAY-DATE = 20260314 (yesterday)
*> LS-CALC-DATE = 20260315 (today, past the pay date)
*> With the <= fix, payment should trigger because pay date <= calc date.
*> $10K @ 5% A/365: daily = 1.369863
*> Accrued = 100.00 + 1.369863 = 101.369863 -> payment = $101.37
*> ---------------------------------------------------------------
TEST-IC-027.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IC-027: Payment when batch date > pay date"
        TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-INT-RESULT
    MOVE 10000.00 TO ACCT-LEDGER-BAL
    MOVE 10000.00 TO ACCT-AVAIL-BAL
    MOVE 5.0000000 TO ACCT-INT-RATE
    MOVE "A" TO ACCT-INT-ACCRUAL-BASIS
    MOVE "DB" TO ACCT-INT-CALC-METHOD
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "A" TO ACCT-STATUS
    MOVE "M" TO ACCT-INT-PAY-FREQ
    MOVE 100.00 TO ACCT-ACCRUED-INT
    *> Pay date is yesterday; calc date is today (past the pay date)
    MOVE 20260314 TO ACCT-INT-NEXT-PAY-DATE
    MOVE 20260315 TO WS-CALC-DATE
    CALL "INTCALC0" USING ACCT-RECORD WS-CALC-DATE WS-INT-RESULT
    IF WS-INT-RESULT-CODE = "E0000"
        IF WS-PAYMENT-DUE = "Y"
            IF WS-PAYMENT-AMT > 0
                ADD 1 TO WS-PASS-COUNT
                DISPLAY "  PASS: " WS-TEST-NAME
                    " pmt=" WS-PAYMENT-AMT
            ELSE
                ADD 1 TO WS-FAIL-COUNT
                DISPLAY "  FAIL: " WS-TEST-NAME
                    " payment-amt=0 expected>0"
            END-IF
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " payment-due=N expected=Y (IC-027)"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-INT-RESULT-CODE " (IC-027)"
    END-IF.

*> ---------------------------------------------------------------
*> IC-028: PTD-INT-EARNED accumulates daily interest
*> After one accrual day, ACCT-PTD-INT-EARNED should equal
*> the daily interest amount (started at 0).
*> ---------------------------------------------------------------
TEST-IC-028.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IC-028: PTD-INT-EARNED accumulates daily int"
        TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-INT-RESULT
    MOVE 10000.00 TO ACCT-LEDGER-BAL
    MOVE 10000.00 TO ACCT-AVAIL-BAL
    MOVE 5.0000000 TO ACCT-INT-RATE
    MOVE "A" TO ACCT-INT-ACCRUAL-BASIS
    MOVE "DB" TO ACCT-INT-CALC-METHOD
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "A" TO ACCT-STATUS
    MOVE 0 TO ACCT-PTD-INT-EARNED
    MOVE 20260315 TO WS-CALC-DATE
    CALL "INTCALC0" USING ACCT-RECORD WS-CALC-DATE WS-INT-RESULT
    IF WS-INT-RESULT-CODE = "E0000"
        IF ACCT-PTD-INT-EARNED = WS-DAILY-INT-AMT
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " ptd=" ACCT-PTD-INT-EARNED
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " ptd=" ACCT-PTD-INT-EARNED
                " expected=" WS-DAILY-INT-AMT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-INT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> IC-029: Escheated account -> E0044 (no interest accrual)
*> Funds turned over to state; interest must stop accruing
*> ---------------------------------------------------------------
TEST-IC-029.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IC-029: Escheated acct -> E0044"
        TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-INT-RESULT
    MOVE 10000.00 TO ACCT-LEDGER-BAL
    MOVE 10000.00 TO ACCT-AVAIL-BAL
    MOVE 5.0000000 TO ACCT-INT-RATE
    MOVE "A" TO ACCT-INT-ACCRUAL-BASIS
    MOVE "DB" TO ACCT-INT-CALC-METHOD
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "E" TO ACCT-STATUS
    MOVE 20260315 TO WS-CALC-DATE
    CALL "INTCALC0" USING ACCT-RECORD WS-CALC-DATE WS-INT-RESULT
    IF WS-INT-RESULT-CODE = "E0044"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-INT-RESULT-CODE " expected=E0044"
    END-IF.

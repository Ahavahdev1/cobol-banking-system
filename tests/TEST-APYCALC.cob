IDENTIFICATION DIVISION.
PROGRAM-ID. TEST-APYCALC.
*> ================================================================
*> TEST-APYCALC - Test suite for APYCALC0 APY/APR Calculator
*> Tests: APY compounding frequencies, edge cases, loan APR
*> 9 tests (AY-001 to AY-009)
*> Regulation DD (Truth in Savings) - 12 CFR 1030
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-TEST-COUNT          PIC 9(3) VALUE 0.
01  WS-PASS-COUNT          PIC 9(3) VALUE 0.
01  WS-FAIL-COUNT          PIC 9(3) VALUE 0.
01  WS-TEST-NAME           PIC X(60).

*> Account record (matches LINKAGE in APYCALC0)
COPY CPYACCT.

*> APY result area
01  WS-APY-RESULT.
    05  WS-APY-RESULT-CODE PIC X(5).
    05  WS-APY-RESULT-MSG  PIC X(50).
    05  WS-APY-VALUE       PIC 9(3)V9(7).
    05  WS-APR-VALUE       PIC 9(3)V9(7).
    05  WS-EFFECTIVE-RATE   PIC 9(3)V9(7).

*> Range check boundaries
01  WS-LOW-BOUND           PIC 9(3)V9(7).
01  WS-HIGH-BOUND          PIC 9(3)V9(7).

PROCEDURE DIVISION.
MAIN-PROGRAM.
    DISPLAY "========================================".
    DISPLAY "TEST SUITE: APYCALC - APY/APR Calc".
    DISPLAY "  Regulation DD (Truth in Savings)".
    DISPLAY "========================================".

    PERFORM TEST-AY-001
    PERFORM TEST-AY-002
    PERFORM TEST-AY-003
    PERFORM TEST-AY-004
    PERFORM TEST-AY-005
    PERFORM TEST-AY-006
    PERFORM TEST-AY-007
    PERFORM TEST-AY-008
    PERFORM TEST-AY-009

    DISPLAY "========================================".
    DISPLAY "RESULTS: " WS-PASS-COUNT "/" WS-TEST-COUNT
            " PASSED".
    DISPLAY "         " WS-FAIL-COUNT " FAILED".
    DISPLAY "========================================".
    MOVE WS-FAIL-COUNT TO RETURN-CODE
    STOP RUN.

*> ---------------------------------------------------------------
*> AY-001: 5% daily compounding -> APY ~5.1267%
*>         APY = (1 + 0.05/365)^365 - 1 = ~5.1267496%
*>         Verify: > 5.12 and < 5.13
*> ---------------------------------------------------------------
TEST-AY-001.
    ADD 1 TO WS-TEST-COUNT
    MOVE "AY-001: 5% daily -> APY ~5.1267%" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-APY-RESULT
    MOVE 5.0000000 TO ACCT-INT-RATE
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "D" TO ACCT-INT-COMPOUND-FREQ
    MOVE "D" TO ACCT-INT-PAY-FREQ
    MOVE "D" TO ACCT-TYPE
    MOVE "A" TO ACCT-STATUS
    CALL "APYCALC0" USING ACCT-RECORD WS-APY-RESULT
    MOVE 5.1200000 TO WS-LOW-BOUND
    MOVE 5.1300000 TO WS-HIGH-BOUND
    IF WS-APY-RESULT-CODE = "E0000"
        IF WS-APY-VALUE > WS-LOW-BOUND
            AND WS-APY-VALUE < WS-HIGH-BOUND
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " APY=" WS-APY-VALUE
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " APY=" WS-APY-VALUE
                " expected 5.12-5.13"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-APY-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> AY-002: 5% monthly compounding -> APY ~5.1162%
*>         APY = (1 + 0.05/12)^12 - 1 = ~5.1161898%
*>         Verify: > 5.11 and < 5.12
*> ---------------------------------------------------------------
TEST-AY-002.
    ADD 1 TO WS-TEST-COUNT
    MOVE "AY-002: 5% monthly -> APY ~5.1162%" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-APY-RESULT
    MOVE 5.0000000 TO ACCT-INT-RATE
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "M" TO ACCT-INT-COMPOUND-FREQ
    MOVE "M" TO ACCT-INT-PAY-FREQ
    MOVE "D" TO ACCT-TYPE
    MOVE "A" TO ACCT-STATUS
    CALL "APYCALC0" USING ACCT-RECORD WS-APY-RESULT
    MOVE 5.1100000 TO WS-LOW-BOUND
    MOVE 5.1200000 TO WS-HIGH-BOUND
    IF WS-APY-RESULT-CODE = "E0000"
        IF WS-APY-VALUE > WS-LOW-BOUND
            AND WS-APY-VALUE < WS-HIGH-BOUND
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " APY=" WS-APY-VALUE
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " APY=" WS-APY-VALUE
                " expected 5.11-5.12"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-APY-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> AY-003: 5% quarterly compounding -> APY ~5.0945%
*>         APY = (1 + 0.05/4)^4 - 1 = ~5.0945337%
*>         Verify: > 5.09 and < 5.10
*> ---------------------------------------------------------------
TEST-AY-003.
    ADD 1 TO WS-TEST-COUNT
    MOVE "AY-003: 5% quarterly -> APY ~5.0945%"
        TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-APY-RESULT
    MOVE 5.0000000 TO ACCT-INT-RATE
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "Q" TO ACCT-INT-COMPOUND-FREQ
    MOVE "Q" TO ACCT-INT-PAY-FREQ
    MOVE "D" TO ACCT-TYPE
    MOVE "A" TO ACCT-STATUS
    CALL "APYCALC0" USING ACCT-RECORD WS-APY-RESULT
    MOVE 5.0900000 TO WS-LOW-BOUND
    MOVE 5.1000000 TO WS-HIGH-BOUND
    IF WS-APY-RESULT-CODE = "E0000"
        IF WS-APY-VALUE > WS-LOW-BOUND
            AND WS-APY-VALUE < WS-HIGH-BOUND
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " APY=" WS-APY-VALUE
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " APY=" WS-APY-VALUE
                " expected 5.09-5.10"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-APY-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> AY-004: 5% annual compounding -> APY = 5.0000000
*>         For annual compounding (1 period), APY = nominal rate
*> ---------------------------------------------------------------
TEST-AY-004.
    ADD 1 TO WS-TEST-COUNT
    MOVE "AY-004: 5% annual -> APY = 5.0000000"
        TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-APY-RESULT
    MOVE 5.0000000 TO ACCT-INT-RATE
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "A" TO ACCT-INT-COMPOUND-FREQ
    MOVE "A" TO ACCT-INT-PAY-FREQ
    MOVE "D" TO ACCT-TYPE
    MOVE "A" TO ACCT-STATUS
    CALL "APYCALC0" USING ACCT-RECORD WS-APY-RESULT
    IF WS-APY-RESULT-CODE = "E0000"
        IF WS-APY-VALUE = 5.0000000
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " APY=" WS-APY-VALUE
                " expected=005.0000000"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-APY-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> AY-005: 0% rate -> APY = 0, APR = 0
*> ---------------------------------------------------------------
TEST-AY-005.
    ADD 1 TO WS-TEST-COUNT
    MOVE "AY-005: 0% rate -> APY = 0" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-APY-RESULT
    MOVE 0.0000000 TO ACCT-INT-RATE
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "M" TO ACCT-INT-COMPOUND-FREQ
    MOVE "M" TO ACCT-INT-PAY-FREQ
    MOVE "D" TO ACCT-TYPE
    MOVE "A" TO ACCT-STATUS
    CALL "APYCALC0" USING ACCT-RECORD WS-APY-RESULT
    IF WS-APY-RESULT-CODE = "E0000"
        IF WS-APY-VALUE = 0
            AND WS-APR-VALUE = 0
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " APY=" WS-APY-VALUE
                " APR=" WS-APR-VALUE
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-APY-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> AY-006: 18% daily compounding -> APY ~19.7164%
*>         APY = (1 + 0.18/365)^365 - 1 = ~19.7164245%
*>         Verify: > 19.71 and < 19.72
*> ---------------------------------------------------------------
TEST-AY-006.
    ADD 1 TO WS-TEST-COUNT
    MOVE "AY-006: 18% daily -> APY ~19.7164%" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-APY-RESULT
    MOVE 18.0000000 TO ACCT-INT-RATE
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "D" TO ACCT-INT-COMPOUND-FREQ
    MOVE "D" TO ACCT-INT-PAY-FREQ
    MOVE "D" TO ACCT-TYPE
    MOVE "A" TO ACCT-STATUS
    CALL "APYCALC0" USING ACCT-RECORD WS-APY-RESULT
    MOVE 19.7100000 TO WS-LOW-BOUND
    MOVE 19.7200000 TO WS-HIGH-BOUND
    IF WS-APY-RESULT-CODE = "E0000"
        IF WS-APY-VALUE > WS-LOW-BOUND
            AND WS-APY-VALUE < WS-HIGH-BOUND
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " APY=" WS-APY-VALUE
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " APY=" WS-APY-VALUE
                " expected 19.71-19.72"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-APY-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> AY-007: Invalid frequency -> E0088
*> ---------------------------------------------------------------
TEST-AY-007.
    ADD 1 TO WS-TEST-COUNT
    MOVE "AY-007: Invalid freq -> E0088" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-APY-RESULT
    MOVE 5.0000000 TO ACCT-INT-RATE
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "X" TO ACCT-INT-COMPOUND-FREQ
    MOVE "X" TO ACCT-INT-PAY-FREQ
    MOVE "D" TO ACCT-TYPE
    MOVE "A" TO ACCT-STATUS
    CALL "APYCALC0" USING ACCT-RECORD WS-APY-RESULT
    IF WS-APY-RESULT-CODE = "E0088"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-APY-RESULT-CODE
            " expected=E0088"
    END-IF.

*> ---------------------------------------------------------------
*> AY-008: 0.5% monthly (low rate precision) -> APY ~0.5011%
*>         APY = (1 + 0.005/12)^12 - 1 = ~0.5011474%
*>         Verify: > 0.50 and < 0.51
*> ---------------------------------------------------------------
TEST-AY-008.
    ADD 1 TO WS-TEST-COUNT
    MOVE "AY-008: 0.5% monthly -> APY ~0.5011%"
        TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-APY-RESULT
    MOVE 0.5000000 TO ACCT-INT-RATE
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "M" TO ACCT-INT-COMPOUND-FREQ
    MOVE "M" TO ACCT-INT-PAY-FREQ
    MOVE "D" TO ACCT-TYPE
    MOVE "A" TO ACCT-STATUS
    CALL "APYCALC0" USING ACCT-RECORD WS-APY-RESULT
    MOVE 0.5000000 TO WS-LOW-BOUND
    MOVE 0.5100000 TO WS-HIGH-BOUND
    IF WS-APY-RESULT-CODE = "E0000"
        IF WS-APY-VALUE > WS-LOW-BOUND
            AND WS-APY-VALUE < WS-HIGH-BOUND
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " APY=" WS-APY-VALUE
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " APY=" WS-APY-VALUE
                " expected 0.50-0.51"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-APY-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> AY-009: Loan APR - 6.5% monthly -> APR = 6.50 (nominal)
*>         For loan accounts, APR currently returns the nominal rate
*>         (TILA Reg Z fee-inclusive APR not yet implemented)
*> ---------------------------------------------------------------
TEST-AY-009.
    ADD 1 TO WS-TEST-COUNT
    MOVE "AY-009: Loan 6.5% monthly -> APR = 6.50"
        TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-APY-RESULT
    MOVE 6.5000000 TO ACCT-INT-RATE
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "M" TO ACCT-INT-COMPOUND-FREQ
    MOVE "M" TO ACCT-INT-PAY-FREQ
    MOVE "L" TO ACCT-TYPE
    MOVE "A" TO ACCT-STATUS
    CALL "APYCALC0" USING ACCT-RECORD WS-APY-RESULT
    IF WS-APY-RESULT-CODE = "E0000"
        IF WS-APR-VALUE = 6.5000000
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " APR=" WS-APR-VALUE
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " APR=" WS-APR-VALUE
                " expected=006.5000000"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-APY-RESULT-CODE
    END-IF.

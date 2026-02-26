IDENTIFICATION DIVISION.
PROGRAM-ID. TEST-LOANPMT.
*> ================================================================
*> TEST-LOANPMT - Test suite for LOANPMT0 Loan Payment Processor
*> Tests: Payments, late checks, payoff, edge cases (29 tests)
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-TEST-COUNT          PIC 9(3) VALUE 0.
01  WS-PASS-COUNT          PIC 9(3) VALUE 0.
01  WS-FAIL-COUNT          PIC 9(3) VALUE 0.
01  WS-TEST-NAME           PIC X(60).

*> LOANPMT0 LINKAGE replicated in working storage
COPY CPYACCT.
01  WS-LOAN-FUNCTION       PIC X(4).
01  WS-PAYMENT-AMT         PIC S9(13)V99.
01  WS-PAYMENT-DATE        PIC 9(8).
01  WS-LOAN-RESULT.
    05  WS-LOAN-RESULT-CODE    PIC X(5).
    05  WS-LOAN-RESULT-MSG     PIC X(50).
    05  WS-LOAN-INT-PORTION    PIC S9(13)V99.
    05  WS-LOAN-PRIN-PORTION   PIC S9(13)V99.
    05  WS-LOAN-NEW-BALANCE    PIC S9(13)V99.

PROCEDURE DIVISION.
MAIN-PROGRAM.
    DISPLAY "========================================"
    DISPLAY "TEST SUITE: LOANPMT0"
    DISPLAY "========================================"

    PERFORM TEST-LP-001
    PERFORM TEST-LP-002
    PERFORM TEST-LP-003
    PERFORM TEST-LP-004
    PERFORM TEST-LP-005
    PERFORM TEST-LP-006
    PERFORM TEST-LP-007
    PERFORM TEST-LP-008
    PERFORM TEST-LP-009
    PERFORM TEST-LP-010
    PERFORM TEST-LP-011
    PERFORM TEST-LP-012
    PERFORM TEST-LP-013
    PERFORM TEST-LP-014
    PERFORM TEST-LP-015
    PERFORM TEST-LP-016
    PERFORM TEST-LP-017
    PERFORM TEST-LP-026
    PERFORM TEST-LP-027
    PERFORM TEST-LP-028
    PERFORM TEST-LP-029
    PERFORM TEST-LP-030
    PERFORM TEST-LP-031
    PERFORM TEST-LP-032
    PERFORM TEST-LP-033
    PERFORM TEST-LP-034
    PERFORM TEST-LP-035
    PERFORM TEST-LP-036
    PERFORM TEST-LP-037

    DISPLAY "========================================"
    DISPLAY "RESULTS: " WS-PASS-COUNT "/" WS-TEST-COUNT
            " PASSED"
    DISPLAY "         " WS-FAIL-COUNT " FAILED"
    DISPLAY "========================================"
    MOVE WS-FAIL-COUNT TO RETURN-CODE
    STOP RUN.

*> ---------------------------------------------------------------
*> Helper: Set up a standard loan account
*> ---------------------------------------------------------------
SETUP-LOAN-ACCOUNT.
    INITIALIZE ACCT-RECORD
    MOVE 000012345678 TO ACCT-NUMBER
    MOVE 2 TO ACCT-CHECK-DIGIT
    MOVE 1000000001 TO ACCT-PRIMARY-CIF
    MOVE "LN01" TO ACCT-PRODUCT-CODE
    MOVE "L" TO ACCT-TYPE
    MOVE "LN" TO ACCT-SUB-TYPE
    MOVE 10000.00 TO ACCT-LEDGER-BAL
    MOVE 0 TO ACCT-AVAIL-BAL
    MOVE 100.000000 TO ACCT-ACCRUED-INT
    MOVE 6.0000000 TO ACCT-INT-RATE
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "A" TO ACCT-INT-ACCRUAL-BASIS
    MOVE "DB" TO ACCT-INT-CALC-METHOD
    MOVE "M" TO ACCT-PAYMENT-FREQ
    MOVE 500.00 TO ACCT-PAYMENT-AMT
    MOVE 36 TO ACCT-REMAINING-TERM
    MOVE 360 TO ACCT-ORIGINAL-TERM
    MOVE 10000.00 TO ACCT-ORIGINAL-AMT
    MOVE 20260315 TO ACCT-NEXT-PMT-DATE
    MOVE "A" TO ACCT-STATUS
    MOVE "N" TO ACCT-LEGAL-HOLD
    MOVE "N" TO ACCT-DECEASED
    MOVE "N" TO ACCT-GARNISHMENT
    MOVE "N" TO ACCT-LATE-FEE-ASSESSED
    MOVE 0 TO ACCT-PAST-DUE-DAYS
    MOVE 0 TO ACCT-PAST-DUE-AMT
    MOVE 20250101 TO ACCT-OPEN-DATE.

*> ---------------------------------------------------------------
*> LP-001: PMNT $500 on $10K loan with $100 accrued interest
*>         int=$100, prin=$400, new bal=$9600
*> ---------------------------------------------------------------
TEST-LP-001.
    ADD 1 TO WS-TEST-COUNT
    MOVE "LP-001: PMNT $500 int=$100 prin=$400"
        TO WS-TEST-NAME
    PERFORM SETUP-LOAN-ACCOUNT
    INITIALIZE WS-LOAN-RESULT
    MOVE "PMNT" TO WS-LOAN-FUNCTION
    MOVE 500.00 TO WS-PAYMENT-AMT
    MOVE 20260315 TO WS-PAYMENT-DATE
    CALL "LOANPMT0" USING WS-LOAN-FUNCTION ACCT-RECORD
                          WS-PAYMENT-AMT WS-PAYMENT-DATE
                          WS-LOAN-RESULT
    IF WS-LOAN-RESULT-CODE = "E0000"
        IF WS-LOAN-INT-PORTION = 100.00
            AND WS-LOAN-PRIN-PORTION = 400.00
            AND WS-LOAN-NEW-BALANCE = 9600.00
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " int=" WS-LOAN-INT-PORTION
                " prin=" WS-LOAN-PRIN-PORTION
                " bal=" WS-LOAN-NEW-BALANCE
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-LOAN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> LP-002: PMNT partial interest payment ($50 on $100 accrued)
*>         all to interest, balance unchanged at $10000
*> ---------------------------------------------------------------
TEST-LP-002.
    ADD 1 TO WS-TEST-COUNT
    MOVE "LP-002: PMNT $50 partial int, bal unchanged"
        TO WS-TEST-NAME
    PERFORM SETUP-LOAN-ACCOUNT
    INITIALIZE WS-LOAN-RESULT
    MOVE "PMNT" TO WS-LOAN-FUNCTION
    MOVE 50.00 TO WS-PAYMENT-AMT
    MOVE 20260315 TO WS-PAYMENT-DATE
    CALL "LOANPMT0" USING WS-LOAN-FUNCTION ACCT-RECORD
                          WS-PAYMENT-AMT WS-PAYMENT-DATE
                          WS-LOAN-RESULT
    IF WS-LOAN-RESULT-CODE = "E0000"
        IF WS-LOAN-INT-PORTION = 50.00
            AND WS-LOAN-PRIN-PORTION = ZERO
            AND WS-LOAN-NEW-BALANCE = 10000.00
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " int=" WS-LOAN-INT-PORTION
                " prin=" WS-LOAN-PRIN-PORTION
                " bal=" WS-LOAN-NEW-BALANCE
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-LOAN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> LP-003: PMNT on deposit account -> E0041
*> ---------------------------------------------------------------
TEST-LP-003.
    ADD 1 TO WS-TEST-COUNT
    MOVE "LP-003: PMNT deposit acct=E0041"
        TO WS-TEST-NAME
    PERFORM SETUP-LOAN-ACCOUNT
    MOVE "D" TO ACCT-TYPE
    MOVE "CH" TO ACCT-SUB-TYPE
    INITIALIZE WS-LOAN-RESULT
    MOVE "PMNT" TO WS-LOAN-FUNCTION
    MOVE 500.00 TO WS-PAYMENT-AMT
    MOVE 20260315 TO WS-PAYMENT-DATE
    CALL "LOANPMT0" USING WS-LOAN-FUNCTION ACCT-RECORD
                          WS-PAYMENT-AMT WS-PAYMENT-DATE
                          WS-LOAN-RESULT
    IF WS-LOAN-RESULT-CODE = "E0041"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0041 actual=" WS-LOAN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> LP-004: PMNT zero amount -> E0002
*> ---------------------------------------------------------------
TEST-LP-004.
    ADD 1 TO WS-TEST-COUNT
    MOVE "LP-004: PMNT zero amount=E0002"
        TO WS-TEST-NAME
    PERFORM SETUP-LOAN-ACCOUNT
    INITIALIZE WS-LOAN-RESULT
    MOVE "PMNT" TO WS-LOAN-FUNCTION
    MOVE ZERO TO WS-PAYMENT-AMT
    MOVE 20260315 TO WS-PAYMENT-DATE
    CALL "LOANPMT0" USING WS-LOAN-FUNCTION ACCT-RECORD
                          WS-PAYMENT-AMT WS-PAYMENT-DATE
                          WS-LOAN-RESULT
    IF WS-LOAN-RESULT-CODE = "E0002"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0002 actual=" WS-LOAN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> LP-005: PMNT on closed loan -> E0011
*> ---------------------------------------------------------------
TEST-LP-005.
    ADD 1 TO WS-TEST-COUNT
    MOVE "LP-005: PMNT closed loan=E0011"
        TO WS-TEST-NAME
    PERFORM SETUP-LOAN-ACCOUNT
    MOVE "C" TO ACCT-STATUS
    INITIALIZE WS-LOAN-RESULT
    MOVE "PMNT" TO WS-LOAN-FUNCTION
    MOVE 500.00 TO WS-PAYMENT-AMT
    MOVE 20260315 TO WS-PAYMENT-DATE
    CALL "LOANPMT0" USING WS-LOAN-FUNCTION ACCT-RECORD
                          WS-PAYMENT-AMT WS-PAYMENT-DATE
                          WS-LOAN-RESULT
    IF WS-LOAN-RESULT-CODE = "E0011"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0011 actual=" WS-LOAN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> LP-006: LATE check past due -> late flag set
*> ---------------------------------------------------------------
TEST-LP-006.
    ADD 1 TO WS-TEST-COUNT
    MOVE "LP-006: LATE past due -> flag=Y"
        TO WS-TEST-NAME
    PERFORM SETUP-LOAN-ACCOUNT
    MOVE 20260315 TO ACCT-NEXT-PMT-DATE
    MOVE "N" TO ACCT-LATE-FEE-ASSESSED
    INITIALIZE WS-LOAN-RESULT
    MOVE "LATE" TO WS-LOAN-FUNCTION
    MOVE ZERO TO WS-PAYMENT-AMT
    *> Payment date is after next-pmt-date
    MOVE 20260401 TO WS-PAYMENT-DATE
    CALL "LOANPMT0" USING WS-LOAN-FUNCTION ACCT-RECORD
                          WS-PAYMENT-AMT WS-PAYMENT-DATE
                          WS-LOAN-RESULT
    IF WS-LOAN-RESULT-CODE = "E0000"
        IF ACCT-LATE-FEE-ASSESSED = "Y"
            AND ACCT-PAST-DUE-DAYS > 0
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " days=" ACCT-PAST-DUE-DAYS
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " flag=" ACCT-LATE-FEE-ASSESSED
                " days=" ACCT-PAST-DUE-DAYS
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-LOAN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> LP-007: LATE check current (not past due) -> no action
*> ---------------------------------------------------------------
TEST-LP-007.
    ADD 1 TO WS-TEST-COUNT
    MOVE "LP-007: LATE current -> no action"
        TO WS-TEST-NAME
    PERFORM SETUP-LOAN-ACCOUNT
    MOVE 20260315 TO ACCT-NEXT-PMT-DATE
    MOVE "N" TO ACCT-LATE-FEE-ASSESSED
    INITIALIZE WS-LOAN-RESULT
    MOVE "LATE" TO WS-LOAN-FUNCTION
    MOVE ZERO TO WS-PAYMENT-AMT
    *> Payment date is before or on next-pmt-date
    MOVE 20260310 TO WS-PAYMENT-DATE
    CALL "LOANPMT0" USING WS-LOAN-FUNCTION ACCT-RECORD
                          WS-PAYMENT-AMT WS-PAYMENT-DATE
                          WS-LOAN-RESULT
    IF WS-LOAN-RESULT-CODE = "E0000"
        IF ACCT-LATE-FEE-ASSESSED = "N"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " flag should be N, actual="
                ACCT-LATE-FEE-ASSESSED
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-LOAN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> LP-008: POFF calculation -> payoff = balance + accrued
*>         $10000 + $100 = $10100
*> ---------------------------------------------------------------
TEST-LP-008.
    ADD 1 TO WS-TEST-COUNT
    MOVE "LP-008: POFF payoff=$10100"
        TO WS-TEST-NAME
    PERFORM SETUP-LOAN-ACCOUNT
    INITIALIZE WS-LOAN-RESULT
    MOVE "POFF" TO WS-LOAN-FUNCTION
    MOVE ZERO TO WS-PAYMENT-AMT
    MOVE 20260315 TO WS-PAYMENT-DATE
    CALL "LOANPMT0" USING WS-LOAN-FUNCTION ACCT-RECORD
                          WS-PAYMENT-AMT WS-PAYMENT-DATE
                          WS-LOAN-RESULT
    IF WS-LOAN-RESULT-CODE = "E0000"
        IF WS-LOAN-NEW-BALANCE = 10100.00
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " payoff=" WS-LOAN-NEW-BALANCE
                " expected=10100.00"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-LOAN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> LP-009: PMNT updates next payment date
*>         20260315 + 1 month = 20260415
*> ---------------------------------------------------------------
TEST-LP-009.
    ADD 1 TO WS-TEST-COUNT
    MOVE "LP-009: PMNT next-pmt-date 0315->0415"
        TO WS-TEST-NAME
    PERFORM SETUP-LOAN-ACCOUNT
    MOVE 20260315 TO ACCT-NEXT-PMT-DATE
    INITIALIZE WS-LOAN-RESULT
    MOVE "PMNT" TO WS-LOAN-FUNCTION
    MOVE 500.00 TO WS-PAYMENT-AMT
    MOVE 20260315 TO WS-PAYMENT-DATE
    CALL "LOANPMT0" USING WS-LOAN-FUNCTION ACCT-RECORD
                          WS-PAYMENT-AMT WS-PAYMENT-DATE
                          WS-LOAN-RESULT
    IF WS-LOAN-RESULT-CODE = "E0000"
        IF ACCT-NEXT-PMT-DATE = 20260415
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " next-pmt=" ACCT-NEXT-PMT-DATE
                " expected=20260415"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-LOAN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> LP-010: PMNT decrements remaining term
*>         36 -> 35
*> ---------------------------------------------------------------
TEST-LP-010.
    ADD 1 TO WS-TEST-COUNT
    MOVE "LP-010: PMNT remaining term 36->35"
        TO WS-TEST-NAME
    PERFORM SETUP-LOAN-ACCOUNT
    MOVE 36 TO ACCT-REMAINING-TERM
    INITIALIZE WS-LOAN-RESULT
    MOVE "PMNT" TO WS-LOAN-FUNCTION
    MOVE 500.00 TO WS-PAYMENT-AMT
    MOVE 20260315 TO WS-PAYMENT-DATE
    CALL "LOANPMT0" USING WS-LOAN-FUNCTION ACCT-RECORD
                          WS-PAYMENT-AMT WS-PAYMENT-DATE
                          WS-LOAN-RESULT
    IF WS-LOAN-RESULT-CODE = "E0000"
        IF ACCT-REMAINING-TERM = 35
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " term=" ACCT-REMAINING-TERM
                " expected=35"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-LOAN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> LP-011: PMNT on fully paid-off loan -> E0042
*> ---------------------------------------------------------------
TEST-LP-011.
    ADD 1 TO WS-TEST-COUNT
    MOVE "LP-011: PMNT paid-off loan=E0042"
        TO WS-TEST-NAME
    PERFORM SETUP-LOAN-ACCOUNT
    MOVE 0 TO ACCT-LEDGER-BAL
    MOVE 0 TO ACCT-ACCRUED-INT
    MOVE "A" TO ACCT-STATUS
    INITIALIZE WS-LOAN-RESULT
    MOVE "PMNT" TO WS-LOAN-FUNCTION
    MOVE 100.00 TO WS-PAYMENT-AMT
    MOVE 20260315 TO WS-PAYMENT-DATE
    CALL "LOANPMT0" USING WS-LOAN-FUNCTION ACCT-RECORD
                          WS-PAYMENT-AMT WS-PAYMENT-DATE
                          WS-LOAN-RESULT
    IF WS-LOAN-RESULT-CODE = "E0042"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0042 actual=" WS-LOAN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> LP-012: PMNT overpayment beyond loan balance -> clamped to 0
*> ---------------------------------------------------------------
TEST-LP-012.
    ADD 1 TO WS-TEST-COUNT
    MOVE "LP-012: PMNT overpayment clamped to 0"
        TO WS-TEST-NAME
    PERFORM SETUP-LOAN-ACCOUNT
    MOVE 100.00 TO ACCT-LEDGER-BAL
    MOVE 0 TO ACCT-ACCRUED-INT
    MOVE 5.0000000 TO ACCT-INT-RATE
    MOVE "A" TO ACCT-STATUS
    MOVE 20260315 TO ACCT-NEXT-PMT-DATE
    MOVE 12 TO ACCT-REMAINING-TERM
    INITIALIZE WS-LOAN-RESULT
    MOVE "PMNT" TO WS-LOAN-FUNCTION
    MOVE 500.00 TO WS-PAYMENT-AMT
    MOVE 20260315 TO WS-PAYMENT-DATE
    CALL "LOANPMT0" USING WS-LOAN-FUNCTION ACCT-RECORD
                          WS-PAYMENT-AMT WS-PAYMENT-DATE
                          WS-LOAN-RESULT
    IF WS-LOAN-RESULT-CODE = "E0000"
        IF WS-LOAN-NEW-BALANCE = ZERO
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " bal=" WS-LOAN-NEW-BALANCE
                " expected=0"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-LOAN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> LP-013: PMNT December -> January next year rollover
*> ---------------------------------------------------------------
TEST-LP-013.
    ADD 1 TO WS-TEST-COUNT
    MOVE "LP-013: PMNT Dec->Jan year rollover"
        TO WS-TEST-NAME
    PERFORM SETUP-LOAN-ACCOUNT
    MOVE 20261215 TO ACCT-NEXT-PMT-DATE
    INITIALIZE WS-LOAN-RESULT
    MOVE "PMNT" TO WS-LOAN-FUNCTION
    MOVE 500.00 TO WS-PAYMENT-AMT
    MOVE 20261215 TO WS-PAYMENT-DATE
    CALL "LOANPMT0" USING WS-LOAN-FUNCTION ACCT-RECORD
                          WS-PAYMENT-AMT WS-PAYMENT-DATE
                          WS-LOAN-RESULT
    IF WS-LOAN-RESULT-CODE = "E0000"
        IF ACCT-NEXT-PMT-DATE = 20270115
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " next-pmt=" ACCT-NEXT-PMT-DATE
                " expected=20270115"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-LOAN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> LP-014: STAT function returns loan info -> E0000
*> ---------------------------------------------------------------
TEST-LP-014.
    ADD 1 TO WS-TEST-COUNT
    MOVE "LP-014: STAT returns loan info"
        TO WS-TEST-NAME
    PERFORM SETUP-LOAN-ACCOUNT
    MOVE 5000.00 TO ACCT-LEDGER-BAL
    MOVE "A" TO ACCT-STATUS
    INITIALIZE WS-LOAN-RESULT
    MOVE "STAT" TO WS-LOAN-FUNCTION
    MOVE ZERO TO WS-PAYMENT-AMT
    MOVE 20260315 TO WS-PAYMENT-DATE
    CALL "LOANPMT0" USING WS-LOAN-FUNCTION ACCT-RECORD
                          WS-PAYMENT-AMT WS-PAYMENT-DATE
                          WS-LOAN-RESULT
    IF WS-LOAN-RESULT-CODE = "E0000"
        IF WS-LOAN-NEW-BALANCE = 5000.00
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " bal=" WS-LOAN-NEW-BALANCE
                " expected=5000.00"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-LOAN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> LP-015: LATE on deposit account -> E0041
*> ---------------------------------------------------------------
TEST-LP-015.
    ADD 1 TO WS-TEST-COUNT
    MOVE "LP-015: LATE deposit acct=E0041"
        TO WS-TEST-NAME
    PERFORM SETUP-LOAN-ACCOUNT
    MOVE "D" TO ACCT-TYPE
    MOVE "A" TO ACCT-STATUS
    INITIALIZE WS-LOAN-RESULT
    MOVE "LATE" TO WS-LOAN-FUNCTION
    MOVE ZERO TO WS-PAYMENT-AMT
    MOVE 20260401 TO WS-PAYMENT-DATE
    CALL "LOANPMT0" USING WS-LOAN-FUNCTION ACCT-RECORD
                          WS-PAYMENT-AMT WS-PAYMENT-DATE
                          WS-LOAN-RESULT
    IF WS-LOAN-RESULT-CODE = "E0041"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0041 actual=" WS-LOAN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> LP-016: POFF on deposit account -> E0041
*> ---------------------------------------------------------------
TEST-LP-016.
    ADD 1 TO WS-TEST-COUNT
    MOVE "LP-016: POFF deposit acct=E0041"
        TO WS-TEST-NAME
    PERFORM SETUP-LOAN-ACCOUNT
    MOVE "D" TO ACCT-TYPE
    MOVE "A" TO ACCT-STATUS
    INITIALIZE WS-LOAN-RESULT
    MOVE "POFF" TO WS-LOAN-FUNCTION
    MOVE ZERO TO WS-PAYMENT-AMT
    MOVE 20260315 TO WS-PAYMENT-DATE
    CALL "LOANPMT0" USING WS-LOAN-FUNCTION ACCT-RECORD
                          WS-PAYMENT-AMT WS-PAYMENT-DATE
                          WS-LOAN-RESULT
    IF WS-LOAN-RESULT-CODE = "E0041"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0041 actual=" WS-LOAN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> LP-017: Invalid function -> E0001
*> ---------------------------------------------------------------
TEST-LP-017.
    ADD 1 TO WS-TEST-COUNT
    MOVE "LP-017: Invalid function -> E0001" TO WS-TEST-NAME
    PERFORM SETUP-LOAN-ACCOUNT
    INITIALIZE WS-LOAN-RESULT
    MOVE "XXXX" TO WS-LOAN-FUNCTION
    MOVE ZERO TO WS-PAYMENT-AMT
    MOVE 20260315 TO WS-PAYMENT-DATE
    CALL "LOANPMT0" USING WS-LOAN-FUNCTION ACCT-RECORD
                          WS-PAYMENT-AMT WS-PAYMENT-DATE
                          WS-LOAN-RESULT
    IF WS-LOAN-RESULT-CODE = "E0001"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0001 actual=" WS-LOAN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> LP-026: Overpayment capped at remaining balance
*>         bal=100, accrued=5, pay=200
*>         int=5, prin=100 (capped), new bal=0
*> ---------------------------------------------------------------
TEST-LP-026.
    ADD 1 TO WS-TEST-COUNT
    MOVE "LP-026: Overpay capped at balance"
        TO WS-TEST-NAME
    PERFORM SETUP-LOAN-ACCOUNT
    MOVE "L" TO ACCT-TYPE
    MOVE "A" TO ACCT-STATUS
    MOVE 100.00 TO ACCT-LEDGER-BAL
    MOVE 5.000000 TO ACCT-ACCRUED-INT
    MOVE 20260315 TO ACCT-NEXT-PMT-DATE
    MOVE 12 TO ACCT-REMAINING-TERM
    INITIALIZE WS-LOAN-RESULT
    MOVE "PMNT" TO WS-LOAN-FUNCTION
    MOVE 200.00 TO WS-PAYMENT-AMT
    MOVE 20260315 TO WS-PAYMENT-DATE
    CALL "LOANPMT0" USING WS-LOAN-FUNCTION ACCT-RECORD
                          WS-PAYMENT-AMT WS-PAYMENT-DATE
                          WS-LOAN-RESULT
    IF WS-LOAN-RESULT-CODE = "E0000"
        IF WS-LOAN-INT-PORTION = 5.00
            AND WS-LOAN-PRIN-PORTION = 100.00
            AND WS-LOAN-NEW-BALANCE = ZERO
            AND ACCT-LEDGER-BAL = ZERO
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " int=" WS-LOAN-INT-PORTION
                " prin=" WS-LOAN-PRIN-PORTION
                " bal=" WS-LOAN-NEW-BALANCE
                " acct-bal=" ACCT-LEDGER-BAL
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-LOAN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> LP-027: Multiple missed payments accumulate past-due
*>         existing past-due=500, scheduled pmt=500
*>         LATE call -> past-due = 500 + 500 + late fee (5%*500=25)
*>         = 1025.00
*> ---------------------------------------------------------------
TEST-LP-027.
    ADD 1 TO WS-TEST-COUNT
    MOVE "LP-027: LATE accumulates past-due+fee"
        TO WS-TEST-NAME
    PERFORM SETUP-LOAN-ACCOUNT
    MOVE "L" TO ACCT-TYPE
    MOVE "A" TO ACCT-STATUS
    MOVE 500.00 TO ACCT-PAST-DUE-AMT
    MOVE 30 TO ACCT-PAST-DUE-DAYS
    MOVE 500.00 TO ACCT-PAYMENT-AMT
    MOVE 20260215 TO ACCT-NEXT-PMT-DATE
    INITIALIZE WS-LOAN-RESULT
    MOVE "LATE" TO WS-LOAN-FUNCTION
    MOVE ZERO TO WS-PAYMENT-AMT
    MOVE 20260401 TO WS-PAYMENT-DATE
    CALL "LOANPMT0" USING WS-LOAN-FUNCTION ACCT-RECORD
                          WS-PAYMENT-AMT WS-PAYMENT-DATE
                          WS-LOAN-RESULT
    IF WS-LOAN-RESULT-CODE = "E0000"
        IF ACCT-PAST-DUE-AMT = 1025.00
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " past-due=" ACCT-PAST-DUE-AMT
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " past-due=" ACCT-PAST-DUE-AMT
                " expected=1025.00"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-LOAN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> LP-028: Jan 31 next-pmt-date caps to Feb 28 (non-leap year)
*>         2026 is not a leap year: 20260131 + 1 month = 20260228
*> ---------------------------------------------------------------
TEST-LP-028.
    ADD 1 TO WS-TEST-COUNT
    MOVE "LP-028: Jan31 next-pmt caps to Feb28 non-leap"
        TO WS-TEST-NAME
    PERFORM SETUP-LOAN-ACCOUNT
    MOVE 20260131 TO ACCT-NEXT-PMT-DATE
    INITIALIZE WS-LOAN-RESULT
    MOVE "PMNT" TO WS-LOAN-FUNCTION
    MOVE 500.00 TO WS-PAYMENT-AMT
    MOVE 20260131 TO WS-PAYMENT-DATE
    CALL "LOANPMT0" USING WS-LOAN-FUNCTION ACCT-RECORD
                          WS-PAYMENT-AMT WS-PAYMENT-DATE
                          WS-LOAN-RESULT
    IF WS-LOAN-RESULT-CODE = "E0000"
        IF ACCT-NEXT-PMT-DATE = 20260228
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " next-pmt=" ACCT-NEXT-PMT-DATE
                " expected=20260228"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-LOAN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> LP-029: Jan 31 next-pmt-date caps to Feb 29 in leap year
*>         2028 is a leap year: 20280131 + 1 month = 20280229
*> ---------------------------------------------------------------
TEST-LP-029.
    ADD 1 TO WS-TEST-COUNT
    MOVE "LP-029: Jan31 next-pmt caps to Feb29 leap yr"
        TO WS-TEST-NAME
    PERFORM SETUP-LOAN-ACCOUNT
    MOVE 20280131 TO ACCT-NEXT-PMT-DATE
    INITIALIZE WS-LOAN-RESULT
    MOVE "PMNT" TO WS-LOAN-FUNCTION
    MOVE 500.00 TO WS-PAYMENT-AMT
    MOVE 20280131 TO WS-PAYMENT-DATE
    CALL "LOANPMT0" USING WS-LOAN-FUNCTION ACCT-RECORD
                          WS-PAYMENT-AMT WS-PAYMENT-DATE
                          WS-LOAN-RESULT
    IF WS-LOAN-RESULT-CODE = "E0000"
        IF ACCT-NEXT-PMT-DATE = 20280229
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " next-pmt=" ACCT-NEXT-PMT-DATE
                " expected=20280229"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-LOAN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> LP-030: POFF on closed account returns E0011
*> ---------------------------------------------------------------
TEST-LP-030.
    ADD 1 TO WS-TEST-COUNT
    MOVE "LP-030: POFF closed account -> E0011"
        TO WS-TEST-NAME
    PERFORM SETUP-LOAN-ACCOUNT
    MOVE "L" TO ACCT-TYPE
    MOVE "C" TO ACCT-STATUS
    INITIALIZE WS-LOAN-RESULT
    MOVE "POFF" TO WS-LOAN-FUNCTION
    MOVE ZERO TO WS-PAYMENT-AMT
    MOVE 20260315 TO WS-PAYMENT-DATE
    CALL "LOANPMT0" USING WS-LOAN-FUNCTION ACCT-RECORD
                          WS-PAYMENT-AMT WS-PAYMENT-DATE
                          WS-LOAN-RESULT
    IF WS-LOAN-RESULT-CODE = "E0011"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0011 actual=" WS-LOAN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> LP-031: PMNT on escheated loan returns E0044
*> ---------------------------------------------------------------
TEST-LP-031.
    ADD 1 TO WS-TEST-COUNT
    MOVE "LP-031: PMNT escheated loan=E0044"
        TO WS-TEST-NAME
    PERFORM SETUP-LOAN-ACCOUNT
    MOVE "L" TO ACCT-TYPE
    MOVE "LN" TO ACCT-SUB-TYPE
    MOVE "E" TO ACCT-STATUS
    MOVE 10000.00 TO ACCT-LEDGER-BAL
    INITIALIZE WS-LOAN-RESULT
    MOVE "PMNT" TO WS-LOAN-FUNCTION
    MOVE 500.00 TO WS-PAYMENT-AMT
    MOVE 20260315 TO WS-PAYMENT-DATE
    CALL "LOANPMT0" USING WS-LOAN-FUNCTION ACCT-RECORD
                          WS-PAYMENT-AMT WS-PAYMENT-DATE
                          WS-LOAN-RESULT
    IF WS-LOAN-RESULT-CODE = "E0044"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0044 actual=" WS-LOAN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> LP-032: LATE on escheated account -> E0044
*> ---------------------------------------------------------------
TEST-LP-032.
    ADD 1 TO WS-TEST-COUNT
    MOVE "LP-032: LATE escheated loan=E0044"
        TO WS-TEST-NAME
    PERFORM SETUP-LOAN-ACCOUNT
    MOVE "L" TO ACCT-TYPE
    MOVE "E" TO ACCT-STATUS
    INITIALIZE WS-LOAN-RESULT
    MOVE "LATE" TO WS-LOAN-FUNCTION
    MOVE ZERO TO WS-PAYMENT-AMT
    MOVE 20260401 TO WS-PAYMENT-DATE
    CALL "LOANPMT0" USING WS-LOAN-FUNCTION ACCT-RECORD
                          WS-PAYMENT-AMT WS-PAYMENT-DATE
                          WS-LOAN-RESULT
    IF WS-LOAN-RESULT-CODE = "E0044"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0044 actual=" WS-LOAN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> LP-033: POFF on escheated account -> E0044
*> ---------------------------------------------------------------
TEST-LP-033.
    ADD 1 TO WS-TEST-COUNT
    MOVE "LP-033: POFF escheated loan=E0044"
        TO WS-TEST-NAME
    PERFORM SETUP-LOAN-ACCOUNT
    MOVE "L" TO ACCT-TYPE
    MOVE "E" TO ACCT-STATUS
    INITIALIZE WS-LOAN-RESULT
    MOVE "POFF" TO WS-LOAN-FUNCTION
    MOVE ZERO TO WS-PAYMENT-AMT
    MOVE 20260315 TO WS-PAYMENT-DATE
    CALL "LOANPMT0" USING WS-LOAN-FUNCTION ACCT-RECORD
                          WS-PAYMENT-AMT WS-PAYMENT-DATE
                          WS-LOAN-RESULT
    IF WS-LOAN-RESULT-CODE = "E0044"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0044 actual=" WS-LOAN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> LP-034: PMNT updates ACCT-LAST-TXN-DATE for dormancy tracking
*> ---------------------------------------------------------------
TEST-LP-034.
    ADD 1 TO WS-TEST-COUNT
    MOVE "LP-034: PMNT sets LAST-TXN-DATE"
        TO WS-TEST-NAME
    PERFORM SETUP-LOAN-ACCOUNT
    MOVE 0 TO ACCT-LAST-TXN-DATE
    INITIALIZE WS-LOAN-RESULT
    MOVE "PMNT" TO WS-LOAN-FUNCTION
    MOVE 500.00 TO WS-PAYMENT-AMT
    MOVE 20260315 TO WS-PAYMENT-DATE
    CALL "LOANPMT0" USING WS-LOAN-FUNCTION ACCT-RECORD
                          WS-PAYMENT-AMT WS-PAYMENT-DATE
                          WS-LOAN-RESULT
    IF WS-LOAN-RESULT-CODE = "E0000"
        IF ACCT-LAST-TXN-DATE = 20260315
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " last-txn=" ACCT-LAST-TXN-DATE
                " expected=20260315"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-LOAN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> LP-035: POFF includes past-due and subtracts escrow
*>         bal=10000, accrued=100, past-due=500, escrow=200
*>         payoff = 10000 + 100 + 500 - 200 = 10400
*> ---------------------------------------------------------------
TEST-LP-035.
    ADD 1 TO WS-TEST-COUNT
    MOVE "LP-035: POFF with past-due and escrow"
        TO WS-TEST-NAME
    PERFORM SETUP-LOAN-ACCOUNT
    MOVE "L" TO ACCT-TYPE
    MOVE "A" TO ACCT-STATUS
    MOVE 10000.00 TO ACCT-LEDGER-BAL
    MOVE 100.000000 TO ACCT-ACCRUED-INT
    MOVE 500.00 TO ACCT-PAST-DUE-AMT
    MOVE 200.00 TO ACCT-ESCROW-BAL
    INITIALIZE WS-LOAN-RESULT
    MOVE "POFF" TO WS-LOAN-FUNCTION
    MOVE ZERO TO WS-PAYMENT-AMT
    MOVE 20260315 TO WS-PAYMENT-DATE
    CALL "LOANPMT0" USING WS-LOAN-FUNCTION ACCT-RECORD
                          WS-PAYMENT-AMT WS-PAYMENT-DATE
                          WS-LOAN-RESULT
    IF WS-LOAN-RESULT-CODE = "E0000"
        IF WS-LOAN-NEW-BALANCE = 10400.00
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " payoff=" WS-LOAN-NEW-BALANCE
                " expected=10400.00"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-LOAN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> LP-036: LATE fee amount reported in INT-PORTION
*>         payment=$500, late fee = 5% * 500 = $25
*>         INT-PORTION should be 25.00
*> ---------------------------------------------------------------
TEST-LP-036.
    ADD 1 TO WS-TEST-COUNT
    MOVE "LP-036: LATE fee in INT-PORTION"
        TO WS-TEST-NAME
    PERFORM SETUP-LOAN-ACCOUNT
    MOVE "L" TO ACCT-TYPE
    MOVE "A" TO ACCT-STATUS
    MOVE 500.00 TO ACCT-PAYMENT-AMT
    MOVE 20260215 TO ACCT-NEXT-PMT-DATE
    MOVE "N" TO ACCT-LATE-FEE-ASSESSED
    MOVE 0 TO ACCT-PAST-DUE-AMT
    MOVE 0 TO ACCT-PAST-DUE-DAYS
    INITIALIZE WS-LOAN-RESULT
    MOVE "LATE" TO WS-LOAN-FUNCTION
    MOVE ZERO TO WS-PAYMENT-AMT
    MOVE 20260401 TO WS-PAYMENT-DATE
    CALL "LOANPMT0" USING WS-LOAN-FUNCTION ACCT-RECORD
                          WS-PAYMENT-AMT WS-PAYMENT-DATE
                          WS-LOAN-RESULT
    IF WS-LOAN-RESULT-CODE = "E0000"
        IF WS-LOAN-INT-PORTION = 25.00
            AND ACCT-YTD-FEES-CHARGED = 25.00
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " int-portion=" WS-LOAN-INT-PORTION
                " ytd-fees=" ACCT-YTD-FEES-CHARGED
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-LOAN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> LP-037: POFF escrow surplus floors at zero
*>         bal=100, accrued=0, past-due=0, escrow=500
*>         raw = 100 + 0 + 0 - 500 = -400, floored to 0
*> ---------------------------------------------------------------
TEST-LP-037.
    ADD 1 TO WS-TEST-COUNT
    MOVE "LP-037: POFF escrow surplus floors at 0"
        TO WS-TEST-NAME
    PERFORM SETUP-LOAN-ACCOUNT
    MOVE "L" TO ACCT-TYPE
    MOVE "A" TO ACCT-STATUS
    MOVE 100.00 TO ACCT-LEDGER-BAL
    MOVE 0 TO ACCT-ACCRUED-INT
    MOVE 0 TO ACCT-PAST-DUE-AMT
    MOVE 500.00 TO ACCT-ESCROW-BAL
    INITIALIZE WS-LOAN-RESULT
    MOVE "POFF" TO WS-LOAN-FUNCTION
    MOVE ZERO TO WS-PAYMENT-AMT
    MOVE 20260315 TO WS-PAYMENT-DATE
    CALL "LOANPMT0" USING WS-LOAN-FUNCTION ACCT-RECORD
                          WS-PAYMENT-AMT WS-PAYMENT-DATE
                          WS-LOAN-RESULT
    IF WS-LOAN-RESULT-CODE = "E0000"
        IF WS-LOAN-NEW-BALANCE = ZERO
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " payoff=" WS-LOAN-NEW-BALANCE
                " expected=0"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-LOAN-RESULT-CODE
    END-IF.

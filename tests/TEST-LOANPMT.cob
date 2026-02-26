IDENTIFICATION DIVISION.
PROGRAM-ID. TEST-LOANPMT.
*> ================================================================
*> TEST-LOANPMT - Test suite for LOANPMT0 Loan Payment Processor
*> Tests: Payments, late checks, payoff, edge cases (10 tests)
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

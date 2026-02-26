IDENTIFICATION DIVISION.
PROGRAM-ID. TEST-BATCH-EOD.
*> ================================================================
*> TEST-BATCH-EOD - Integration test for EODPROC0 End-of-Day Batch
*> Tests: EOD cycle with interest accrual, interest payment,
*>        batch status, multi-account, error paths (12 tests)
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-TEST-COUNT          PIC 9(3) VALUE 0.
01  WS-PASS-COUNT          PIC 9(3) VALUE 0.
01  WS-FAIL-COUNT          PIC 9(3) VALUE 0.
01  WS-TEST-NAME           PIC X(60).

*> EOD batch areas
01  WS-BATCH-DATE          PIC 9(8).
COPY CPYACCT.
COPY CPYBATCH.
01  WS-BATCH-RESULT.
    05  WS-BATCH-RESULT-CODE  PIC X(5).
    05  WS-BATCH-RESULT-MSG   PIC X(50).

01  WS-SAVED-ACCRUED       PIC S9(11)V9(6).
01  WS-SAVED-LEDGER-BAL    PIC S9(13)V99.

PROCEDURE DIVISION.
MAIN-PROGRAM.
    DISPLAY "========================================".
    DISPLAY "TEST SUITE: BATCH-EOD Integration".
    DISPLAY "========================================".

    PERFORM TEST-BE-001
    PERFORM TEST-BE-002
    PERFORM TEST-BE-003
    PERFORM TEST-BE-004
    PERFORM TEST-BE-005
    PERFORM TEST-BE-006
    PERFORM TEST-BE-007
    PERFORM TEST-BE-008
    PERFORM TEST-BE-009
    PERFORM TEST-BE-010
    PERFORM TEST-BE-011
    PERFORM TEST-BE-012

    DISPLAY "========================================".
    DISPLAY "RESULTS: " WS-PASS-COUNT "/" WS-TEST-COUNT
            " PASSED".
    DISPLAY "         " WS-FAIL-COUNT " FAILED".
    DISPLAY "========================================".
    MOVE WS-FAIL-COUNT TO RETURN-CODE
    STOP RUN.

*> ---------------------------------------------------------------
*> Helper: Set up active account with interest params
*> ---------------------------------------------------------------
SETUP-INTEREST-ACCOUNT.
    INITIALIZE ACCT-RECORD
    MOVE 000012345678 TO ACCT-NUMBER
    MOVE 2 TO ACCT-CHECK-DIGIT
    MOVE 1000000001 TO ACCT-PRIMARY-CIF
    MOVE "SAV1" TO ACCT-PRODUCT-CODE
    MOVE "D" TO ACCT-TYPE
    MOVE "SV" TO ACCT-SUB-TYPE
    MOVE 10000.00 TO ACCT-LEDGER-BAL
    MOVE 10000.00 TO ACCT-AVAIL-BAL
    MOVE 0 TO ACCT-HOLD-AMOUNT
    MOVE "A" TO ACCT-STATUS
    MOVE "N" TO ACCT-LEGAL-HOLD
    MOVE "N" TO ACCT-DECEASED
    MOVE "N" TO ACCT-GARNISHMENT
    MOVE 5.0000000 TO ACCT-INT-RATE
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "A" TO ACCT-INT-ACCRUAL-BASIS
    MOVE "M" TO ACCT-INT-PAY-FREQ
    MOVE 0 TO ACCT-INT-NEXT-PAY-DATE
    MOVE 0 TO ACCT-ACCRUED-INT
    MOVE 20200101 TO ACCT-OPEN-DATE.

*> ---------------------------------------------------------------
*> BE-001: EOD batch completes successfully
*> ---------------------------------------------------------------
TEST-BE-001.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BE-001: EOD batch completes -> E0000" TO WS-TEST-NAME
    PERFORM SETUP-INTEREST-ACCOUNT
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260226 TO WS-BATCH-DATE
    CALL "EODPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF WS-BATCH-RESULT-CODE = "E0000"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-BATCH-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> BE-002: EOD batch sets batch status to C (complete)
*> ---------------------------------------------------------------
TEST-BE-002.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BE-002: Batch status = C (complete)" TO WS-TEST-NAME
    PERFORM SETUP-INTEREST-ACCOUNT
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260226 TO WS-BATCH-DATE
    CALL "EODPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF BATCH-STATUS = "C"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " status=" BATCH-STATUS " expected=C"
    END-IF.

*> ---------------------------------------------------------------
*> BE-003: EOD accrues interest on active account
*> ---------------------------------------------------------------
TEST-BE-003.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BE-003: EOD accrues interest" TO WS-TEST-NAME
    PERFORM SETUP-INTEREST-ACCOUNT
    MOVE 0 TO ACCT-ACCRUED-INT
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260226 TO WS-BATCH-DATE
    CALL "EODPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF WS-BATCH-RESULT-CODE = "E0000"
        IF ACCT-ACCRUED-INT > 0
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " accrued=" ACCT-ACCRUED-INT
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " accrued=" ACCT-ACCRUED-INT " expected>0"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-BATCH-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> BE-004: EOD processes 1 account correctly
*> ---------------------------------------------------------------
TEST-BE-004.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BE-004: Batch processed count = 1" TO WS-TEST-NAME
    PERFORM SETUP-INTEREST-ACCOUNT
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260226 TO WS-BATCH-DATE
    CALL "EODPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF BATCH-ACCTS-PROCESSED = 1
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " processed=" BATCH-ACCTS-PROCESSED
    END-IF.

*> ---------------------------------------------------------------
*> BE-005: EOD sets batch type to EOD
*> ---------------------------------------------------------------
TEST-BE-005.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BE-005: Batch type = EOD" TO WS-TEST-NAME
    PERFORM SETUP-INTEREST-ACCOUNT
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260226 TO WS-BATCH-DATE
    CALL "EODPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF BATCH-TYPE = "EOD"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " type=" BATCH-TYPE " expected=EOD"
    END-IF.

*> ---------------------------------------------------------------
*> BE-006: EOD with interest payment on pay date
*> ---------------------------------------------------------------
TEST-BE-006.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BE-006: EOD with interest payment" TO WS-TEST-NAME
    PERFORM SETUP-INTEREST-ACCOUNT
    *> Set accrued interest and payment date = today
    MOVE 41.10 TO ACCT-ACCRUED-INT
    MOVE 20260226 TO ACCT-INT-NEXT-PAY-DATE
    MOVE ACCT-LEDGER-BAL TO WS-SAVED-LEDGER-BAL
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260226 TO WS-BATCH-DATE
    CALL "EODPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF WS-BATCH-RESULT-CODE = "E0000"
        *> Balance should have increased by interest payment
        IF ACCT-LEDGER-BAL > WS-SAVED-LEDGER-BAL
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " bal=" ACCT-LEDGER-BAL
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " bal=" ACCT-LEDGER-BAL
                " prev=" WS-SAVED-LEDGER-BAL
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-BATCH-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> BE-007: EOD sets batch date correctly
*> ---------------------------------------------------------------
TEST-BE-007.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BE-007: Batch date = input date" TO WS-TEST-NAME
    PERFORM SETUP-INTEREST-ACCOUNT
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260226 TO WS-BATCH-DATE
    CALL "EODPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF BATCH-DATE = 20260226
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " date=" BATCH-DATE
    END-IF.

*> ---------------------------------------------------------------
*> BE-008: Closed account skipped (not processed)
*> ---------------------------------------------------------------
TEST-BE-008.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BE-008: Closed acct skipped" TO WS-TEST-NAME
    PERFORM SETUP-INTEREST-ACCOUNT
    MOVE "C" TO ACCT-STATUS
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260226 TO WS-BATCH-DATE
    CALL "EODPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF BATCH-ACCTS-PROCESSED = 0
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " processed=" BATCH-ACCTS-PROCESSED
            " expected=0"
    END-IF.

*> ---------------------------------------------------------------
*> BE-009: Dormant account (status D) still processed
*> ---------------------------------------------------------------
TEST-BE-009.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BE-009: Dormant acct processed -> E0000" TO WS-TEST-NAME
    PERFORM SETUP-INTEREST-ACCOUNT
    MOVE "D" TO ACCT-STATUS
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260226 TO WS-BATCH-DATE
    CALL "EODPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF WS-BATCH-RESULT-CODE = "E0000"
        AND BATCH-ACCTS-PROCESSED = 1
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-BATCH-RESULT-CODE
            " processed=" BATCH-ACCTS-PROCESSED
    END-IF.

*> ---------------------------------------------------------------
*> BE-010: Frozen account (status F) is skipped
*> ---------------------------------------------------------------
TEST-BE-010.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BE-010: Frozen acct skipped" TO WS-TEST-NAME
    PERFORM SETUP-INTEREST-ACCOUNT
    MOVE "F" TO ACCT-STATUS
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260226 TO WS-BATCH-DATE
    CALL "EODPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF BATCH-ACCTS-PROCESSED = 0
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " processed=" BATCH-ACCTS-PROCESSED
            " expected=0"
    END-IF.

*> ---------------------------------------------------------------
*> BE-011: EOD releases hold and recalculates available balance
*> ---------------------------------------------------------------
TEST-BE-011.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BE-011: Hold released, avail=ledger" TO WS-TEST-NAME
    PERFORM SETUP-INTEREST-ACCOUNT
    MOVE 500.00 TO ACCT-HOLD-AMOUNT
    COMPUTE ACCT-AVAIL-BAL =
        ACCT-LEDGER-BAL - ACCT-HOLD-AMOUNT
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260226 TO WS-BATCH-DATE
    CALL "EODPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF WS-BATCH-RESULT-CODE = "E0000"
        IF ACCT-HOLD-AMOUNT = 0
            AND ACCT-AVAIL-BAL = ACCT-LEDGER-BAL
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " hold=" ACCT-HOLD-AMOUNT
                " avail=" ACCT-AVAIL-BAL
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " hold=" ACCT-HOLD-AMOUNT
                " avail=" ACCT-AVAIL-BAL
                " ledger=" ACCT-LEDGER-BAL
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-BATCH-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> BE-012: Batch date stored and status P on error
*> ---------------------------------------------------------------
TEST-BE-012.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BE-012: Batch date set, status=P on err" TO WS-TEST-NAME
    PERFORM SETUP-INTEREST-ACCOUNT
    MOVE "Z" TO ACCT-INT-ACCRUAL-BASIS
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260226 TO WS-BATCH-DATE
    CALL "EODPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF BATCH-DATE = 20260226
        AND BATCH-STATUS = "P"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " date=" BATCH-DATE
            " status=" BATCH-STATUS
            " expected date=20260226 status=P"
    END-IF.

IDENTIFICATION DIVISION.
PROGRAM-ID. TEST-BATCH-EOM.
*> ================================================================
*> TEST-BATCH-EOM - Integration test for EOMPROC0 End-of-Month
*> Tests: EOM cycle with fee assessment, MTD reset, batch status,
*>        closed-account skip (8 tests)
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-TEST-COUNT          PIC 9(3) VALUE 0.
01  WS-PASS-COUNT          PIC 9(3) VALUE 0.
01  WS-FAIL-COUNT          PIC 9(3) VALUE 0.
01  WS-TEST-NAME           PIC X(60).

*> EOM batch areas
01  WS-BATCH-DATE          PIC 9(8).
COPY CPYACCT.
COPY CPYBATCH.
01  WS-BATCH-RESULT.
    05  WS-BATCH-RESULT-CODE  PIC X(5).
    05  WS-BATCH-RESULT-MSG   PIC X(50).

01  WS-SAVED-LEDGER-BAL    PIC S9(13)V99.

PROCEDURE DIVISION.
MAIN-PROGRAM.
    DISPLAY "========================================".
    DISPLAY "TEST SUITE: BATCH-EOM Integration".
    DISPLAY "========================================".

    PERFORM TEST-EM-001
    PERFORM TEST-EM-002
    PERFORM TEST-EM-003
    PERFORM TEST-EM-004
    PERFORM TEST-EM-005
    PERFORM TEST-EM-006
    PERFORM TEST-EM-007
    PERFORM TEST-EM-008

    DISPLAY "========================================".
    DISPLAY "RESULTS: " WS-PASS-COUNT "/" WS-TEST-COUNT
            " PASSED".
    DISPLAY "         " WS-FAIL-COUNT " FAILED".
    DISPLAY "========================================".
    MOVE WS-FAIL-COUNT TO RETURN-CODE
    STOP RUN.

*> ---------------------------------------------------------------
*> Helper: Set up active checking account for EOM testing
*> ---------------------------------------------------------------
SETUP-EOM-ACCOUNT.
    INITIALIZE ACCT-RECORD
    MOVE 000012345678 TO ACCT-NUMBER
    MOVE 2 TO ACCT-CHECK-DIGIT
    MOVE 1000000001 TO ACCT-PRIMARY-CIF
    MOVE "DDA1" TO ACCT-PRODUCT-CODE
    MOVE "D" TO ACCT-TYPE
    MOVE "CH" TO ACCT-SUB-TYPE
    MOVE 5000.00 TO ACCT-LEDGER-BAL
    MOVE 5000.00 TO ACCT-AVAIL-BAL
    MOVE 0 TO ACCT-HOLD-AMOUNT
    MOVE "A" TO ACCT-STATUS
    MOVE "N" TO ACCT-LEGAL-HOLD
    MOVE "N" TO ACCT-DECEASED
    MOVE "N" TO ACCT-GARNISHMENT
    MOVE 12.00 TO ACCT-MONTHLY-FEE
    MOVE "NW" TO ACCT-FEE-WAIVER-CODE
    MOVE 3 TO ACCT-NSF-COUNT-MTD
    MOVE 4500.00 TO ACCT-MTD-AVG-BAL
    MOVE 2000.00 TO ACCT-MTD-LOW-BAL
    MOVE 20200101 TO ACCT-OPEN-DATE.

*> ---------------------------------------------------------------
*> EM-001: EOM batch completes successfully -> E0000
*> ---------------------------------------------------------------
TEST-EM-001.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EM-001: EOM batch completes -> E0000" TO WS-TEST-NAME
    PERFORM SETUP-EOM-ACCOUNT
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260228 TO WS-BATCH-DATE
    CALL "EOMPROC0" USING WS-BATCH-DATE
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
*> EM-002: Batch status = C (complete)
*> ---------------------------------------------------------------
TEST-EM-002.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EM-002: Batch status = C (complete)" TO WS-TEST-NAME
    PERFORM SETUP-EOM-ACCOUNT
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260228 TO WS-BATCH-DATE
    CALL "EOMPROC0" USING WS-BATCH-DATE
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
*> EM-003: Batch type = EOM
*> ---------------------------------------------------------------
TEST-EM-003.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EM-003: Batch type = EOM" TO WS-TEST-NAME
    PERFORM SETUP-EOM-ACCOUNT
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260228 TO WS-BATCH-DATE
    CALL "EOMPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF BATCH-TYPE = "EOM"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " type=" BATCH-TYPE " expected=EOM"
    END-IF.

*> ---------------------------------------------------------------
*> EM-004: Monthly fee is assessed on active account
*> EOMPROC0 initializes FEE-SCHEDULE-RECORD from account data
*> (ACCT-MONTHLY-FEE = 12.00, FEE-WAIVER-CODE = "NW").
*> With no waiver, FEECALC0 should assess the full $12.00 fee.
*> Verify BATCH-FEES-ASSESSED > 0 and balance decreased.
*> ---------------------------------------------------------------
TEST-EM-004.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EM-004: Monthly fee assessed on active acct"
        TO WS-TEST-NAME
    PERFORM SETUP-EOM-ACCOUNT
    MOVE ACCT-LEDGER-BAL TO WS-SAVED-LEDGER-BAL
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260228 TO WS-BATCH-DATE
    CALL "EOMPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF WS-BATCH-RESULT-CODE = "E0000"
        IF BATCH-FEES-ASSESSED > 0
            AND ACCT-LEDGER-BAL < WS-SAVED-LEDGER-BAL
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " fees=" BATCH-FEES-ASSESSED
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " fees=" BATCH-FEES-ASSESSED
                " bal=" ACCT-LEDGER-BAL
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-BATCH-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> EM-005: Batch processed count = 1 for active account
*> ---------------------------------------------------------------
TEST-EM-005.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EM-005: Batch processed count = 1" TO WS-TEST-NAME
    PERFORM SETUP-EOM-ACCOUNT
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260228 TO WS-BATCH-DATE
    CALL "EOMPROC0" USING WS-BATCH-DATE
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
            " expected=1"
    END-IF.

*> ---------------------------------------------------------------
*> EM-006: MTD counters reset after EOM processing
*> ACCT-NSF-COUNT-MTD, ACCT-MTD-AVG-BAL, ACCT-MTD-LOW-BAL
*> should all be zero after EOMPROC0 runs
*> ---------------------------------------------------------------
TEST-EM-006.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EM-006: MTD counters reset to zero" TO WS-TEST-NAME
    PERFORM SETUP-EOM-ACCOUNT
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260228 TO WS-BATCH-DATE
    CALL "EOMPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF ACCT-NSF-COUNT-MTD = 0
        AND ACCT-MTD-AVG-BAL = 0
        AND ACCT-MTD-LOW-BAL = 0
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " nsf=" ACCT-NSF-COUNT-MTD
            " avg=" ACCT-MTD-AVG-BAL
            " low=" ACCT-MTD-LOW-BAL
    END-IF.

*> ---------------------------------------------------------------
*> EM-007: Closed account is skipped (not processed)
*> ---------------------------------------------------------------
TEST-EM-007.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EM-007: Closed acct skipped" TO WS-TEST-NAME
    PERFORM SETUP-EOM-ACCOUNT
    MOVE "C" TO ACCT-STATUS
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260228 TO WS-BATCH-DATE
    CALL "EOMPROC0" USING WS-BATCH-DATE
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
*> EM-008: Batch date = input date
*> ---------------------------------------------------------------
TEST-EM-008.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EM-008: Batch date = input date" TO WS-TEST-NAME
    PERFORM SETUP-EOM-ACCOUNT
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260228 TO WS-BATCH-DATE
    CALL "EOMPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF BATCH-DATE = 20260228
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " date=" BATCH-DATE
    END-IF.

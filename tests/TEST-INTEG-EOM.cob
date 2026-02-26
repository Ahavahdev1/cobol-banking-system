IDENTIFICATION DIVISION.
PROGRAM-ID. TEST-INTEG-EOM.
*> ================================================================
*> TEST-INTEG-EOM - Cross-module integration test for EOM cycle
*> Tests: fee waiver, dormant account, MTD reset, fee posting
*> across EOMPROC0 + FEECALC0 + TXNPOST0 + AUDTLOG0
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
    DISPLAY "TEST SUITE: INTEGRATION - EOM".
    DISPLAY "========================================".

    PERFORM TEST-IE-001
    PERFORM TEST-IE-002
    PERFORM TEST-IE-003
    PERFORM TEST-IE-004

    DISPLAY "========================================".
    DISPLAY "RESULTS: " WS-PASS-COUNT "/" WS-TEST-COUNT
            " PASSED".
    DISPLAY "         " WS-FAIL-COUNT " FAILED".
    DISPLAY "========================================".
    MOVE WS-FAIL-COUNT TO RETURN-CODE
    STOP RUN.

*> ---------------------------------------------------------------
*> Helper: Set up active checking account for integration testing
*> ---------------------------------------------------------------
SETUP-INTEG-ACCOUNT.
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
    MOVE 0 TO ACCT-NSF-COUNT-TODAY
    MOVE 4500.00 TO ACCT-MTD-AVG-BAL
    MOVE 2000.00 TO ACCT-MTD-LOW-BAL
    MOVE 20200101 TO ACCT-OPEN-DATE.

*> ---------------------------------------------------------------
*> IE-001: EOM fee assessment with fee waiver (balance-based)
*> Account has $5000 balance, min-bal waiver at $1500
*> Fee should be waived since balance exceeds threshold
*> ---------------------------------------------------------------
TEST-IE-001.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IE-001: Fee waived when bal > min-bal threshold"
        TO WS-TEST-NAME
    PERFORM SETUP-INTEG-ACCOUNT
    *> Set up balance-based waiver: $5000 bal > $1500 threshold
    MOVE "MB" TO ACCT-FEE-WAIVER-CODE
    MOVE 1500.00 TO ACCT-FEE-WAIVER-AMT
    MOVE 12.00 TO ACCT-MONTHLY-FEE
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260228 TO WS-BATCH-DATE
    CALL "EOMPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF WS-BATCH-RESULT-CODE = "E0000"
        IF BATCH-FEES-ASSESSED = 0
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " fees=" BATCH-FEES-ASSESSED
                " expected=0 (waived)"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-BATCH-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> IE-002: EOM on dormant account still processes
*> Dormant accounts (status "D") should still be processed
*> ---------------------------------------------------------------
TEST-IE-002.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IE-002: Dormant account still processed"
        TO WS-TEST-NAME
    PERFORM SETUP-INTEG-ACCOUNT
    MOVE "D" TO ACCT-STATUS
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
*> IE-003: EOM resets MTD counters
*> Set non-zero MTD values, verify they are zeroed after EOM
*> ---------------------------------------------------------------
TEST-IE-003.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IE-003: MTD counters reset after EOM"
        TO WS-TEST-NAME
    PERFORM SETUP-INTEG-ACCOUNT
    *> Ensure non-zero MTD values (setup already sets these)
    MOVE 5 TO ACCT-NSF-COUNT-MTD
    MOVE 3500.00 TO ACCT-MTD-AVG-BAL
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260228 TO WS-BATCH-DATE
    CALL "EOMPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF ACCT-NSF-COUNT-MTD = 0
        AND ACCT-MTD-AVG-BAL = 0
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " nsf-mtd=" ACCT-NSF-COUNT-MTD
            " avg-bal=" ACCT-MTD-AVG-BAL
    END-IF.

*> ---------------------------------------------------------------
*> IE-004: EOM fee posting debits account
*> $1000 balance, $12 monthly fee, no waiver -> balance decreases
*> ---------------------------------------------------------------
TEST-IE-004.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IE-004: Fee posting debits account balance"
        TO WS-TEST-NAME
    PERFORM SETUP-INTEG-ACCOUNT
    *> Set balance to $1000 with no waiver
    MOVE 1000.00 TO ACCT-LEDGER-BAL
    MOVE 1000.00 TO ACCT-AVAIL-BAL
    MOVE 12.00 TO ACCT-MONTHLY-FEE
    MOVE "NW" TO ACCT-FEE-WAIVER-CODE
    MOVE ACCT-LEDGER-BAL TO WS-SAVED-LEDGER-BAL
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260228 TO WS-BATCH-DATE
    CALL "EOMPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF WS-BATCH-RESULT-CODE = "E0000"
        IF ACCT-LEDGER-BAL < WS-SAVED-LEDGER-BAL
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " bal=" ACCT-LEDGER-BAL
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " bal=" ACCT-LEDGER-BAL
                " expected < " WS-SAVED-LEDGER-BAL
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-BATCH-RESULT-CODE
    END-IF.

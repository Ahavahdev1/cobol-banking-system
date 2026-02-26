IDENTIFICATION DIVISION.
PROGRAM-ID. TEST-DORMANT.
*> ================================================================
*> TEST-DORMANT - Test suite for DORMANT0 Dormancy Management
*> Tests: Dormancy detection, escheatment, edge cases (8 tests)
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-TEST-COUNT          PIC 9(3) VALUE 0.
01  WS-PASS-COUNT          PIC 9(3) VALUE 0.
01  WS-FAIL-COUNT          PIC 9(3) VALUE 0.
01  WS-TEST-NAME           PIC X(60).

*> DORMANT0 interface areas
01  WS-DORM-FUNCTION       PIC X(4).
01  WS-BATCH-DATE          PIC 9(8).
COPY CPYACCT.
01  WS-DORM-RESULT.
    05  WS-DORM-RESULT-CODE    PIC X(5).
    05  WS-DORM-RESULT-MSG     PIC X(50).
    05  WS-DORM-ACTION-TAKEN   PIC X(1).

PROCEDURE DIVISION.
MAIN-PROGRAM.
    DISPLAY "========================================".
    DISPLAY "TEST SUITE: DORMANT - Dormancy Mgmt".
    DISPLAY "========================================".

    PERFORM TEST-DR-001
    PERFORM TEST-DR-002
    PERFORM TEST-DR-003
    PERFORM TEST-DR-004
    PERFORM TEST-DR-005
    PERFORM TEST-DR-006
    PERFORM TEST-DR-007
    PERFORM TEST-DR-008

    DISPLAY "========================================".
    DISPLAY "RESULTS: " WS-PASS-COUNT "/" WS-TEST-COUNT
            " PASSED".
    DISPLAY "         " WS-FAIL-COUNT " FAILED".
    DISPLAY "========================================".
    MOVE WS-FAIL-COUNT TO RETURN-CODE
    STOP RUN.

*> ---------------------------------------------------------------
*> Helper: Set up a basic active account
*> ---------------------------------------------------------------
SETUP-ACTIVE-ACCOUNT.
    INITIALIZE ACCT-RECORD
    MOVE 000012345678 TO ACCT-NUMBER
    MOVE 2 TO ACCT-CHECK-DIGIT
    MOVE 1000000001 TO ACCT-PRIMARY-CIF
    MOVE "DDA1" TO ACCT-PRODUCT-CODE
    MOVE "D" TO ACCT-TYPE
    MOVE "CH" TO ACCT-SUB-TYPE
    MOVE 5000.00 TO ACCT-LEDGER-BAL
    MOVE 5000.00 TO ACCT-AVAIL-BAL
    MOVE "A" TO ACCT-STATUS
    MOVE "N" TO ACCT-LEGAL-HOLD
    MOVE "N" TO ACCT-DECEASED
    MOVE "N" TO ACCT-GARNISHMENT
    MOVE 20200101 TO ACCT-OPEN-DATE.

*> ---------------------------------------------------------------
*> DR-001: Active account, last txn 6 months ago -> no action (N)
*> Batch date 20260226, last txn 20250826 (6 months)
*> Diff = 20260226 - 20250826 = 5400, < 10000 -> no dormancy
*> ---------------------------------------------------------------
TEST-DR-001.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DR-001: Active, 6mo inactivity -> N" TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-ACCOUNT
    MOVE 20250826 TO ACCT-LAST-TXN-DATE
    MOVE 20260226 TO WS-BATCH-DATE
    MOVE "CHKD" TO WS-DORM-FUNCTION
    INITIALIZE WS-DORM-RESULT
    CALL "DORMANT0" USING WS-DORM-FUNCTION
                          ACCT-RECORD
                          WS-BATCH-DATE
                          WS-DORM-RESULT
    IF WS-DORM-RESULT-CODE = "E0000"
        AND WS-DORM-ACTION-TAKEN = "N"
        AND ACCT-STATUS = "A"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-DORM-RESULT-CODE
            " action=" WS-DORM-ACTION-TAKEN
            " status=" ACCT-STATUS
    END-IF.

*> ---------------------------------------------------------------
*> DR-002: Active account, last txn 13 months ago -> dormant (D)
*> Batch date 20260226, last txn 20250101 (about 13.8 months)
*> Diff = 20260226 - 20250101 = 10125, > 10000 -> mark dormant
*> ---------------------------------------------------------------
TEST-DR-002.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DR-002: Active, 13mo inactivity -> D" TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-ACCOUNT
    MOVE 20250101 TO ACCT-LAST-TXN-DATE
    MOVE 20260226 TO WS-BATCH-DATE
    MOVE "CHKD" TO WS-DORM-FUNCTION
    INITIALIZE WS-DORM-RESULT
    CALL "DORMANT0" USING WS-DORM-FUNCTION
                          ACCT-RECORD
                          WS-BATCH-DATE
                          WS-DORM-RESULT
    IF WS-DORM-RESULT-CODE = "E0000"
        AND WS-DORM-ACTION-TAKEN = "D"
        AND ACCT-STATUS = "D"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-DORM-RESULT-CODE
            " action=" WS-DORM-ACTION-TAKEN
            " status=" ACCT-STATUS
    END-IF.

*> ---------------------------------------------------------------
*> DR-003: Dormant account, dormant 2 years -> no escheat (N)
*> Batch date 20260226, last txn 20240301 (about 2 years)
*> Diff = 20260226 - 20240301 = 19925, < 30000 -> no escheat
*> ---------------------------------------------------------------
TEST-DR-003.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DR-003: Dormant 2yr, CHKE -> N" TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-ACCOUNT
    MOVE "D" TO ACCT-STATUS
    MOVE 20240301 TO ACCT-LAST-TXN-DATE
    MOVE 20260226 TO WS-BATCH-DATE
    MOVE "CHKE" TO WS-DORM-FUNCTION
    INITIALIZE WS-DORM-RESULT
    CALL "DORMANT0" USING WS-DORM-FUNCTION
                          ACCT-RECORD
                          WS-BATCH-DATE
                          WS-DORM-RESULT
    IF WS-DORM-RESULT-CODE = "E0000"
        AND WS-DORM-ACTION-TAKEN = "N"
        AND ACCT-STATUS = "D"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-DORM-RESULT-CODE
            " action=" WS-DORM-ACTION-TAKEN
            " status=" ACCT-STATUS
    END-IF.

*> ---------------------------------------------------------------
*> DR-004: Dormant account, dormant 4 years -> escheated (E)
*> Batch date 20260226, last txn 20220101 (about 4 years)
*> Diff = 20260226 - 20220101 = 40125, > 30000 -> escheat
*> ---------------------------------------------------------------
TEST-DR-004.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DR-004: Dormant 4yr, CHKE -> E" TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-ACCOUNT
    MOVE "D" TO ACCT-STATUS
    MOVE 20220101 TO ACCT-LAST-TXN-DATE
    MOVE 20260226 TO WS-BATCH-DATE
    MOVE "CHKE" TO WS-DORM-FUNCTION
    INITIALIZE WS-DORM-RESULT
    CALL "DORMANT0" USING WS-DORM-FUNCTION
                          ACCT-RECORD
                          WS-BATCH-DATE
                          WS-DORM-RESULT
    IF WS-DORM-RESULT-CODE = "E0000"
        AND WS-DORM-ACTION-TAKEN = "E"
        AND ACCT-STATUS = "E"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-DORM-RESULT-CODE
            " action=" WS-DORM-ACTION-TAKEN
            " status=" ACCT-STATUS
    END-IF.

*> ---------------------------------------------------------------
*> DR-005: Already dormant account, CHKD -> no action (N)
*> CHKD only acts on active accounts; dormant should be skipped
*> ---------------------------------------------------------------
TEST-DR-005.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DR-005: Dormant acct, CHKD -> N" TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-ACCOUNT
    MOVE "D" TO ACCT-STATUS
    MOVE 20240101 TO ACCT-LAST-TXN-DATE
    MOVE 20260226 TO WS-BATCH-DATE
    MOVE "CHKD" TO WS-DORM-FUNCTION
    INITIALIZE WS-DORM-RESULT
    CALL "DORMANT0" USING WS-DORM-FUNCTION
                          ACCT-RECORD
                          WS-BATCH-DATE
                          WS-DORM-RESULT
    IF WS-DORM-RESULT-CODE = "E0000"
        AND WS-DORM-ACTION-TAKEN = "N"
        AND ACCT-STATUS = "D"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-DORM-RESULT-CODE
            " action=" WS-DORM-ACTION-TAKEN
            " status=" ACCT-STATUS
    END-IF.

*> ---------------------------------------------------------------
*> DR-006: Closed account -> no action (N)
*> CHKD should not process closed accounts
*> ---------------------------------------------------------------
TEST-DR-006.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DR-006: Closed acct, CHKD -> N" TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-ACCOUNT
    MOVE "C" TO ACCT-STATUS
    MOVE 20220101 TO ACCT-LAST-TXN-DATE
    MOVE 20260226 TO WS-BATCH-DATE
    MOVE "CHKD" TO WS-DORM-FUNCTION
    INITIALIZE WS-DORM-RESULT
    CALL "DORMANT0" USING WS-DORM-FUNCTION
                          ACCT-RECORD
                          WS-BATCH-DATE
                          WS-DORM-RESULT
    IF WS-DORM-RESULT-CODE = "E0000"
        AND WS-DORM-ACTION-TAKEN = "N"
        AND ACCT-STATUS = "C"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-DORM-RESULT-CODE
            " action=" WS-DORM-ACTION-TAKEN
            " status=" ACCT-STATUS
    END-IF.

*> ---------------------------------------------------------------
*> DR-007: Active account, no last txn date (0) -> no action (N)
*> Missing last txn date should not trigger dormancy
*> ---------------------------------------------------------------
TEST-DR-007.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DR-007: No last txn date -> N" TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-ACCOUNT
    MOVE 0 TO ACCT-LAST-TXN-DATE
    MOVE 20260226 TO WS-BATCH-DATE
    MOVE "CHKD" TO WS-DORM-FUNCTION
    INITIALIZE WS-DORM-RESULT
    CALL "DORMANT0" USING WS-DORM-FUNCTION
                          ACCT-RECORD
                          WS-BATCH-DATE
                          WS-DORM-RESULT
    IF WS-DORM-RESULT-CODE = "E0000"
        AND WS-DORM-ACTION-TAKEN = "N"
        AND ACCT-STATUS = "A"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-DORM-RESULT-CODE
            " action=" WS-DORM-ACTION-TAKEN
            " status=" ACCT-STATUS
    END-IF.

*> ---------------------------------------------------------------
*> DR-008: STAT returns current status -> E0000
*> Status inquiry should not change anything
*> ---------------------------------------------------------------
TEST-DR-008.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DR-008: STAT returns status -> E0000" TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-ACCOUNT
    MOVE "D" TO ACCT-STATUS
    MOVE 20240101 TO ACCT-LAST-TXN-DATE
    MOVE 20260226 TO WS-BATCH-DATE
    MOVE "STAT" TO WS-DORM-FUNCTION
    INITIALIZE WS-DORM-RESULT
    CALL "DORMANT0" USING WS-DORM-FUNCTION
                          ACCT-RECORD
                          WS-BATCH-DATE
                          WS-DORM-RESULT
    IF WS-DORM-RESULT-CODE = "E0000"
        AND WS-DORM-ACTION-TAKEN = "N"
        AND ACCT-STATUS = "D"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-DORM-RESULT-CODE
            " action=" WS-DORM-ACTION-TAKEN
            " status=" ACCT-STATUS
    END-IF.

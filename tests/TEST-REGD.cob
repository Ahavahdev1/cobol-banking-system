IDENTIFICATION DIVISION.
PROGRAM-ID. TEST-REGD.
*> ================================================================
*> TEST-REGD - Test suite for REGDCHK0 Regulation D Check
*> Tests: Savings transfer limits, exemptions, MMA, channel types
*> 9 tests (RD-001 to RD-009)
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-TEST-COUNT          PIC 9(3) VALUE 0.
01  WS-PASS-COUNT          PIC 9(3) VALUE 0.
01  WS-FAIL-COUNT          PIC 9(3) VALUE 0.
01  WS-TEST-NAME           PIC X(60).

*> Account record (matches LINKAGE in REGDCHK0)
COPY CPYACCT.

01  WS-REGD-REQUEST.
    05  WS-RD-TXN-CHANNEL   PIC X(2).
    05  WS-RD-TXN-TYPE      PIC X(3).
    05  WS-RD-CURRENT-COUNT PIC 9(3).

01  WS-REGD-RESULT.
    05  WS-RD-RESULT-CODE   PIC X(5).
    05  WS-RD-RESULT-MSG    PIC X(50).
    05  WS-RD-ALLOWED       PIC X(1).
    05  WS-RD-NEW-COUNT     PIC 9(3).
    05  WS-RD-LIMIT-REACHED PIC X(1).

PROCEDURE DIVISION.
MAIN-PROGRAM.
    DISPLAY "========================================".
    DISPLAY "TEST SUITE: REGD - Regulation D".
    DISPLAY "========================================".

    PERFORM TEST-RD-001
    PERFORM TEST-RD-002
    PERFORM TEST-RD-003
    PERFORM TEST-RD-004
    PERFORM TEST-RD-005
    PERFORM TEST-RD-006
    PERFORM TEST-RD-007
    PERFORM TEST-RD-008
    PERFORM TEST-RD-009

    DISPLAY "========================================".
    DISPLAY "RESULTS: " WS-PASS-COUNT "/" WS-TEST-COUNT
            " PASSED".
    DISPLAY "         " WS-FAIL-COUNT " FAILED".
    DISPLAY "========================================".
    MOVE WS-FAIL-COUNT TO RETURN-CODE
    STOP RUN.

*> ---------------------------------------------------------------
*> RD-001: Savings online transfer, count=0 -> allowed, count=1
*> ---------------------------------------------------------------
TEST-RD-001.
    ADD 1 TO WS-TEST-COUNT
    MOVE "RD-001: Savings OL count 0->1 allowed" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-REGD-REQUEST
    INITIALIZE WS-REGD-RESULT
    MOVE "A" TO ACCT-STATUS
    MOVE "SAV1" TO ACCT-PRODUCT-CODE
    MOVE "D" TO ACCT-TYPE
    MOVE "SV" TO ACCT-SUB-TYPE
    MOVE 5000.00 TO ACCT-LEDGER-BAL
    MOVE "OL" TO WS-RD-TXN-CHANNEL
    MOVE "XFR" TO WS-RD-TXN-TYPE
    MOVE 0 TO WS-RD-CURRENT-COUNT
    CALL "REGDCHK0" USING ACCT-RECORD WS-REGD-REQUEST
                          WS-REGD-RESULT
    IF WS-RD-RESULT-CODE = "E0000"
        IF WS-RD-ALLOWED = "Y"
            AND WS-RD-NEW-COUNT = 1
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " allowed=" WS-RD-ALLOWED
                " count=" WS-RD-NEW-COUNT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-RD-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> RD-002: Savings online transfer, count=5 -> allowed, count=6
*> ---------------------------------------------------------------
TEST-RD-002.
    ADD 1 TO WS-TEST-COUNT
    MOVE "RD-002: Savings OL count 5->6 allowed" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-REGD-REQUEST
    INITIALIZE WS-REGD-RESULT
    MOVE "A" TO ACCT-STATUS
    MOVE "SAV1" TO ACCT-PRODUCT-CODE
    MOVE "D" TO ACCT-TYPE
    MOVE "SV" TO ACCT-SUB-TYPE
    MOVE 5000.00 TO ACCT-LEDGER-BAL
    MOVE "OL" TO WS-RD-TXN-CHANNEL
    MOVE "XFR" TO WS-RD-TXN-TYPE
    MOVE 5 TO WS-RD-CURRENT-COUNT
    CALL "REGDCHK0" USING ACCT-RECORD WS-REGD-REQUEST
                          WS-REGD-RESULT
    IF WS-RD-RESULT-CODE = "E0000"
        IF WS-RD-ALLOWED = "Y"
            AND WS-RD-NEW-COUNT = 6
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " allowed=" WS-RD-ALLOWED
                " count=" WS-RD-NEW-COUNT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-RD-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> RD-003: Savings online transfer, count=6 -> DENIED (E0081)
*> ---------------------------------------------------------------
TEST-RD-003.
    ADD 1 TO WS-TEST-COUNT
    MOVE "RD-003: Savings OL count 6 -> denied" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-REGD-REQUEST
    INITIALIZE WS-REGD-RESULT
    MOVE "A" TO ACCT-STATUS
    MOVE "SAV1" TO ACCT-PRODUCT-CODE
    MOVE "D" TO ACCT-TYPE
    MOVE "SV" TO ACCT-SUB-TYPE
    MOVE 5000.00 TO ACCT-LEDGER-BAL
    MOVE "OL" TO WS-RD-TXN-CHANNEL
    MOVE "XFR" TO WS-RD-TXN-TYPE
    MOVE 6 TO WS-RD-CURRENT-COUNT
    CALL "REGDCHK0" USING ACCT-RECORD WS-REGD-REQUEST
                          WS-REGD-RESULT
    IF WS-RD-RESULT-CODE = "E0081"
        IF WS-RD-ALLOWED = "N"
            AND WS-RD-LIMIT-REACHED = "Y"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " allowed=" WS-RD-ALLOWED
                " limit=" WS-RD-LIMIT-REACHED
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-RD-RESULT-CODE " expected=E0081"
    END-IF.

*> ---------------------------------------------------------------
*> RD-004: Checking -> always allowed (Reg D exempt)
*> ---------------------------------------------------------------
TEST-RD-004.
    ADD 1 TO WS-TEST-COUNT
    MOVE "RD-004: Checking always allowed" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-REGD-REQUEST
    INITIALIZE WS-REGD-RESULT
    MOVE "A" TO ACCT-STATUS
    MOVE "DDA1" TO ACCT-PRODUCT-CODE
    MOVE "D" TO ACCT-TYPE
    MOVE "CH" TO ACCT-SUB-TYPE
    MOVE 5000.00 TO ACCT-LEDGER-BAL
    MOVE "OL" TO WS-RD-TXN-CHANNEL
    MOVE "XFR" TO WS-RD-TXN-TYPE
    MOVE 99 TO WS-RD-CURRENT-COUNT
    CALL "REGDCHK0" USING ACCT-RECORD WS-REGD-REQUEST
                          WS-REGD-RESULT
    IF WS-RD-RESULT-CODE = "E0000"
        IF WS-RD-ALLOWED = "Y"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " allowed=" WS-RD-ALLOWED " expected=Y"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-RD-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> RD-005: Savings ATM withdrawal -> allowed (ATM exempt)
*> ---------------------------------------------------------------
TEST-RD-005.
    ADD 1 TO WS-TEST-COUNT
    MOVE "RD-005: Savings ATM exempt" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-REGD-REQUEST
    INITIALIZE WS-REGD-RESULT
    MOVE "A" TO ACCT-STATUS
    MOVE "SAV1" TO ACCT-PRODUCT-CODE
    MOVE "D" TO ACCT-TYPE
    MOVE "SV" TO ACCT-SUB-TYPE
    MOVE 5000.00 TO ACCT-LEDGER-BAL
    MOVE "AT" TO WS-RD-TXN-CHANNEL
    MOVE "WDL" TO WS-RD-TXN-TYPE
    MOVE 6 TO WS-RD-CURRENT-COUNT
    CALL "REGDCHK0" USING ACCT-RECORD WS-REGD-REQUEST
                          WS-REGD-RESULT
    IF WS-RD-RESULT-CODE = "E0000"
        IF WS-RD-ALLOWED = "Y"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " allowed=" WS-RD-ALLOWED " expected=Y"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-RD-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> RD-006: Savings in-person branch -> allowed (exempt)
*> ---------------------------------------------------------------
TEST-RD-006.
    ADD 1 TO WS-TEST-COUNT
    MOVE "RD-006: Savings branch exempt" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-REGD-REQUEST
    INITIALIZE WS-REGD-RESULT
    MOVE "A" TO ACCT-STATUS
    MOVE "SAV1" TO ACCT-PRODUCT-CODE
    MOVE "D" TO ACCT-TYPE
    MOVE "SV" TO ACCT-SUB-TYPE
    MOVE 5000.00 TO ACCT-LEDGER-BAL
    MOVE "BR" TO WS-RD-TXN-CHANNEL
    MOVE "WDL" TO WS-RD-TXN-TYPE
    MOVE 6 TO WS-RD-CURRENT-COUNT
    CALL "REGDCHK0" USING ACCT-RECORD WS-REGD-REQUEST
                          WS-REGD-RESULT
    IF WS-RD-RESULT-CODE = "E0000"
        IF WS-RD-ALLOWED = "Y"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " allowed=" WS-RD-ALLOWED " expected=Y"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-RD-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> RD-007: Savings ACH transfer, count=5 -> allowed, count=6
*> ---------------------------------------------------------------
TEST-RD-007.
    ADD 1 TO WS-TEST-COUNT
    MOVE "RD-007: Savings ACH count 5->6" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-REGD-REQUEST
    INITIALIZE WS-REGD-RESULT
    MOVE "A" TO ACCT-STATUS
    MOVE "SAV1" TO ACCT-PRODUCT-CODE
    MOVE "D" TO ACCT-TYPE
    MOVE "SV" TO ACCT-SUB-TYPE
    MOVE 5000.00 TO ACCT-LEDGER-BAL
    MOVE "AC" TO WS-RD-TXN-CHANNEL
    MOVE "ACH" TO WS-RD-TXN-TYPE
    MOVE 5 TO WS-RD-CURRENT-COUNT
    CALL "REGDCHK0" USING ACCT-RECORD WS-REGD-REQUEST
                          WS-REGD-RESULT
    IF WS-RD-RESULT-CODE = "E0000"
        IF WS-RD-ALLOWED = "Y"
            AND WS-RD-NEW-COUNT = 6
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " allowed=" WS-RD-ALLOWED
                " count=" WS-RD-NEW-COUNT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-RD-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> RD-008: Savings ACH transfer, count=6 -> denied
*> ---------------------------------------------------------------
TEST-RD-008.
    ADD 1 TO WS-TEST-COUNT
    MOVE "RD-008: Savings ACH count 6 denied" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-REGD-REQUEST
    INITIALIZE WS-REGD-RESULT
    MOVE "A" TO ACCT-STATUS
    MOVE "SAV1" TO ACCT-PRODUCT-CODE
    MOVE "D" TO ACCT-TYPE
    MOVE "SV" TO ACCT-SUB-TYPE
    MOVE 5000.00 TO ACCT-LEDGER-BAL
    MOVE "AC" TO WS-RD-TXN-CHANNEL
    MOVE "ACH" TO WS-RD-TXN-TYPE
    MOVE 6 TO WS-RD-CURRENT-COUNT
    CALL "REGDCHK0" USING ACCT-RECORD WS-REGD-REQUEST
                          WS-REGD-RESULT
    IF WS-RD-RESULT-CODE = "E0081"
        IF WS-RD-ALLOWED = "N"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " allowed=" WS-RD-ALLOWED " expected=N"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-RD-RESULT-CODE " expected=E0081"
    END-IF.

*> ---------------------------------------------------------------
*> RD-009: MMA online transfer, count=6 -> denied (subject to D)
*> ---------------------------------------------------------------
TEST-RD-009.
    ADD 1 TO WS-TEST-COUNT
    MOVE "RD-009: MMA OL count 6 denied" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-REGD-REQUEST
    INITIALIZE WS-REGD-RESULT
    MOVE "A" TO ACCT-STATUS
    MOVE "MMA1" TO ACCT-PRODUCT-CODE
    MOVE "D" TO ACCT-TYPE
    MOVE "MM" TO ACCT-SUB-TYPE
    MOVE 50000.00 TO ACCT-LEDGER-BAL
    MOVE "OL" TO WS-RD-TXN-CHANNEL
    MOVE "XFR" TO WS-RD-TXN-TYPE
    MOVE 6 TO WS-RD-CURRENT-COUNT
    CALL "REGDCHK0" USING ACCT-RECORD WS-REGD-REQUEST
                          WS-REGD-RESULT
    IF WS-RD-RESULT-CODE = "E0081"
        IF WS-RD-ALLOWED = "N"
            AND WS-RD-LIMIT-REACHED = "Y"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " allowed=" WS-RD-ALLOWED
                " limit=" WS-RD-LIMIT-REACHED
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-RD-RESULT-CODE " expected=E0081"
    END-IF.

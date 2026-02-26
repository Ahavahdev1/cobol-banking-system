IDENTIFICATION DIVISION.
PROGRAM-ID. TEST-SAR.
*> ================================================================
*> TEST-SAR - Test suite for BSACTRO SAR/Structuring Detection
*> Tests: STRC structuring detection, SARQ pending SAR query
*> 6 tests (SA-001 to SA-006)
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-TEST-COUNT          PIC 9(3) VALUE 0.
01  WS-PASS-COUNT          PIC 9(3) VALUE 0.
01  WS-FAIL-COUNT          PIC 9(3) VALUE 0.
01  WS-TEST-NAME           PIC X(60).

*> BSACTRO parameters (matches LINKAGE)
01  WS-FUNCTION            PIC X(4).

COPY CPYCTR.

01  WS-TXN-INFO.
    05  WS-BSA-CUST-ID      PIC 9(10).
    05  WS-BSA-TXN-DATE     PIC 9(8).
    05  WS-BSA-CASH-AMOUNT  PIC S9(13)V99.
    05  WS-BSA-CASH-DIRECTION PIC X(1).
    05  WS-BSA-IS-CASH      PIC X(1).
    05  WS-BSA-ACCT-NUMBER  PIC 9(12).

01  WS-BSA-RESULT.
    05  WS-BSA-RESULT-CODE   PIC X(5).
    05  WS-BSA-RESULT-MSG    PIC X(50).
    05  WS-BSA-CTR-REQUIRED  PIC X(1).
    05  WS-BSA-CASH-IN-TOTAL PIC S9(13)V99.
    05  WS-BSA-CASH-OUT-TOTAL PIC S9(13)V99.

PROCEDURE DIVISION.
MAIN-PROGRAM.
    DISPLAY "========================================".
    DISPLAY "TEST SUITE: SAR - Structuring/SAR".
    DISPLAY "========================================".

    PERFORM TEST-SA-001
    PERFORM TEST-SA-002
    PERFORM TEST-SA-003
    PERFORM TEST-SA-004
    PERFORM TEST-SA-005
    PERFORM TEST-SA-006

    DISPLAY "========================================".
    DISPLAY "RESULTS: " WS-PASS-COUNT "/" WS-TEST-COUNT
            " PASSED".
    DISPLAY "         " WS-FAIL-COUNT " FAILED".
    DISPLAY "========================================".
    MOVE WS-FAIL-COUNT TO RETURN-CODE
    STOP RUN.

*> ---------------------------------------------------------------
*> SA-001: STRC - 3 deposits of $3,000 = $9,000 -> structuring
*> Total is in $8,000-$9,999.99 range with count > 1
*> ---------------------------------------------------------------
TEST-SA-001.
    ADD 1 TO WS-TEST-COUNT
    MOVE "SA-001: 3x$3K=$9K structuring detected" TO WS-TEST-NAME
    INITIALIZE CTR-RECORD
    INITIALIZE WS-TXN-INFO
    INITIALIZE WS-BSA-RESULT
    *> First deposit: $3,000 cash-in
    MOVE "AGGR" TO WS-FUNCTION
    MOVE 2000000001 TO WS-BSA-CUST-ID
    MOVE 20260215 TO WS-BSA-TXN-DATE
    MOVE 3000.00 TO WS-BSA-CASH-AMOUNT
    MOVE "I" TO WS-BSA-CASH-DIRECTION
    MOVE "Y" TO WS-BSA-IS-CASH
    MOVE 200000000001 TO WS-BSA-ACCT-NUMBER
    MOVE "N" TO CTR-EXEMPT-FLAG
    CALL "BSACTRO" USING WS-FUNCTION CTR-RECORD
                         WS-TXN-INFO WS-BSA-RESULT
    *> Second deposit: $3,000 cash-in (total = $6,000, count = 2)
    INITIALIZE WS-TXN-INFO
    INITIALIZE WS-BSA-RESULT
    MOVE "AGGR" TO WS-FUNCTION
    MOVE 2000000001 TO WS-BSA-CUST-ID
    MOVE 20260215 TO WS-BSA-TXN-DATE
    MOVE 3000.00 TO WS-BSA-CASH-AMOUNT
    MOVE "I" TO WS-BSA-CASH-DIRECTION
    MOVE "Y" TO WS-BSA-IS-CASH
    MOVE 200000000001 TO WS-BSA-ACCT-NUMBER
    CALL "BSACTRO" USING WS-FUNCTION CTR-RECORD
                         WS-TXN-INFO WS-BSA-RESULT
    *> Third deposit: $3,000 cash-in (total = $9,000, count = 3)
    INITIALIZE WS-TXN-INFO
    INITIALIZE WS-BSA-RESULT
    MOVE "AGGR" TO WS-FUNCTION
    MOVE 2000000001 TO WS-BSA-CUST-ID
    MOVE 20260215 TO WS-BSA-TXN-DATE
    MOVE 3000.00 TO WS-BSA-CASH-AMOUNT
    MOVE "I" TO WS-BSA-CASH-DIRECTION
    MOVE "Y" TO WS-BSA-IS-CASH
    MOVE 200000000001 TO WS-BSA-ACCT-NUMBER
    CALL "BSACTRO" USING WS-FUNCTION CTR-RECORD
                         WS-TXN-INFO WS-BSA-RESULT
    *> Now call STRC to check for structuring pattern
    INITIALIZE WS-BSA-RESULT
    MOVE "STRC" TO WS-FUNCTION
    CALL "BSACTRO" USING WS-FUNCTION CTR-RECORD
                         WS-TXN-INFO WS-BSA-RESULT
    IF WS-BSA-RESULT-CODE = "E0082"
        IF WS-BSA-CTR-REQUIRED = "Y"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " CTR=" WS-BSA-CTR-REQUIRED " expected=Y"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-BSA-RESULT-CODE " expected=E0082"
    END-IF.

*> ---------------------------------------------------------------
*> SA-002: STRC - single $9,000 deposit -> no structuring
*> Only 1 transaction (count=1), not a structuring pattern
*> ---------------------------------------------------------------
TEST-SA-002.
    ADD 1 TO WS-TEST-COUNT
    MOVE "SA-002: Single $9K no structuring" TO WS-TEST-NAME
    INITIALIZE CTR-RECORD
    INITIALIZE WS-TXN-INFO
    INITIALIZE WS-BSA-RESULT
    *> Single deposit: $9,000 cash-in (count = 1)
    MOVE "AGGR" TO WS-FUNCTION
    MOVE 2000000002 TO WS-BSA-CUST-ID
    MOVE 20260215 TO WS-BSA-TXN-DATE
    MOVE 9000.00 TO WS-BSA-CASH-AMOUNT
    MOVE "I" TO WS-BSA-CASH-DIRECTION
    MOVE "Y" TO WS-BSA-IS-CASH
    MOVE 200000000002 TO WS-BSA-ACCT-NUMBER
    MOVE "N" TO CTR-EXEMPT-FLAG
    CALL "BSACTRO" USING WS-FUNCTION CTR-RECORD
                         WS-TXN-INFO WS-BSA-RESULT
    *> Now call STRC - should NOT detect structuring (only 1 txn)
    INITIALIZE WS-BSA-RESULT
    MOVE "STRC" TO WS-FUNCTION
    CALL "BSACTRO" USING WS-FUNCTION CTR-RECORD
                         WS-TXN-INFO WS-BSA-RESULT
    IF WS-BSA-RESULT-CODE = "E0000"
        IF WS-BSA-CTR-REQUIRED = "N"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " CTR=" WS-BSA-CTR-REQUIRED " expected=N"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-BSA-RESULT-CODE " expected=E0000"
    END-IF.

*> ---------------------------------------------------------------
*> SA-003: STRC - 3 deposits of $2,000 = $6,000 -> no structuring
*> Below $8,000 threshold
*> ---------------------------------------------------------------
TEST-SA-003.
    ADD 1 TO WS-TEST-COUNT
    MOVE "SA-003: 3x$2K=$6K below threshold" TO WS-TEST-NAME
    INITIALIZE CTR-RECORD
    INITIALIZE WS-TXN-INFO
    INITIALIZE WS-BSA-RESULT
    *> First deposit: $2,000 cash-in
    MOVE "AGGR" TO WS-FUNCTION
    MOVE 2000000003 TO WS-BSA-CUST-ID
    MOVE 20260215 TO WS-BSA-TXN-DATE
    MOVE 2000.00 TO WS-BSA-CASH-AMOUNT
    MOVE "I" TO WS-BSA-CASH-DIRECTION
    MOVE "Y" TO WS-BSA-IS-CASH
    MOVE 200000000003 TO WS-BSA-ACCT-NUMBER
    MOVE "N" TO CTR-EXEMPT-FLAG
    CALL "BSACTRO" USING WS-FUNCTION CTR-RECORD
                         WS-TXN-INFO WS-BSA-RESULT
    *> Second deposit: $2,000 cash-in (total = $4,000, count = 2)
    INITIALIZE WS-TXN-INFO
    INITIALIZE WS-BSA-RESULT
    MOVE "AGGR" TO WS-FUNCTION
    MOVE 2000000003 TO WS-BSA-CUST-ID
    MOVE 20260215 TO WS-BSA-TXN-DATE
    MOVE 2000.00 TO WS-BSA-CASH-AMOUNT
    MOVE "I" TO WS-BSA-CASH-DIRECTION
    MOVE "Y" TO WS-BSA-IS-CASH
    MOVE 200000000003 TO WS-BSA-ACCT-NUMBER
    CALL "BSACTRO" USING WS-FUNCTION CTR-RECORD
                         WS-TXN-INFO WS-BSA-RESULT
    *> Third deposit: $2,000 cash-in (total = $6,000, count = 3)
    INITIALIZE WS-TXN-INFO
    INITIALIZE WS-BSA-RESULT
    MOVE "AGGR" TO WS-FUNCTION
    MOVE 2000000003 TO WS-BSA-CUST-ID
    MOVE 20260215 TO WS-BSA-TXN-DATE
    MOVE 2000.00 TO WS-BSA-CASH-AMOUNT
    MOVE "I" TO WS-BSA-CASH-DIRECTION
    MOVE "Y" TO WS-BSA-IS-CASH
    MOVE 200000000003 TO WS-BSA-ACCT-NUMBER
    CALL "BSACTRO" USING WS-FUNCTION CTR-RECORD
                         WS-TXN-INFO WS-BSA-RESULT
    *> Now call STRC - should NOT detect ($6K < $8K threshold)
    INITIALIZE WS-BSA-RESULT
    MOVE "STRC" TO WS-FUNCTION
    CALL "BSACTRO" USING WS-FUNCTION CTR-RECORD
                         WS-TXN-INFO WS-BSA-RESULT
    IF WS-BSA-RESULT-CODE = "E0000"
        IF WS-BSA-CTR-REQUIRED = "N"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " CTR=" WS-BSA-CTR-REQUIRED " expected=N"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-BSA-RESULT-CODE " expected=E0000"
    END-IF.

*> ---------------------------------------------------------------
*> SA-004: STRC - 3 deposits of $3,500 = $10,500 -> no structuring
*> Above $9,999.99 ceiling (would trigger CTR, not SAR)
*> ---------------------------------------------------------------
TEST-SA-004.
    ADD 1 TO WS-TEST-COUNT
    MOVE "SA-004: 3x$3.5K=$10.5K above ceiling" TO WS-TEST-NAME
    INITIALIZE CTR-RECORD
    INITIALIZE WS-TXN-INFO
    INITIALIZE WS-BSA-RESULT
    *> First deposit: $3,500 cash-in
    MOVE "AGGR" TO WS-FUNCTION
    MOVE 2000000004 TO WS-BSA-CUST-ID
    MOVE 20260215 TO WS-BSA-TXN-DATE
    MOVE 3500.00 TO WS-BSA-CASH-AMOUNT
    MOVE "I" TO WS-BSA-CASH-DIRECTION
    MOVE "Y" TO WS-BSA-IS-CASH
    MOVE 200000000004 TO WS-BSA-ACCT-NUMBER
    MOVE "N" TO CTR-EXEMPT-FLAG
    CALL "BSACTRO" USING WS-FUNCTION CTR-RECORD
                         WS-TXN-INFO WS-BSA-RESULT
    *> Second deposit: $3,500 cash-in (total = $7,000, count = 2)
    INITIALIZE WS-TXN-INFO
    INITIALIZE WS-BSA-RESULT
    MOVE "AGGR" TO WS-FUNCTION
    MOVE 2000000004 TO WS-BSA-CUST-ID
    MOVE 20260215 TO WS-BSA-TXN-DATE
    MOVE 3500.00 TO WS-BSA-CASH-AMOUNT
    MOVE "I" TO WS-BSA-CASH-DIRECTION
    MOVE "Y" TO WS-BSA-IS-CASH
    MOVE 200000000004 TO WS-BSA-ACCT-NUMBER
    CALL "BSACTRO" USING WS-FUNCTION CTR-RECORD
                         WS-TXN-INFO WS-BSA-RESULT
    *> Third deposit: $3,500 cash-in (total = $10,500, count = 3)
    INITIALIZE WS-TXN-INFO
    INITIALIZE WS-BSA-RESULT
    MOVE "AGGR" TO WS-FUNCTION
    MOVE 2000000004 TO WS-BSA-CUST-ID
    MOVE 20260215 TO WS-BSA-TXN-DATE
    MOVE 3500.00 TO WS-BSA-CASH-AMOUNT
    MOVE "I" TO WS-BSA-CASH-DIRECTION
    MOVE "Y" TO WS-BSA-IS-CASH
    MOVE 200000000004 TO WS-BSA-ACCT-NUMBER
    CALL "BSACTRO" USING WS-FUNCTION CTR-RECORD
                         WS-TXN-INFO WS-BSA-RESULT
    *> Now call STRC - should NOT detect ($10,500 > $9,999.99 ceiling)
    INITIALIZE WS-BSA-RESULT
    MOVE "STRC" TO WS-FUNCTION
    CALL "BSACTRO" USING WS-FUNCTION CTR-RECORD
                         WS-TXN-INFO WS-BSA-RESULT
    IF WS-BSA-RESULT-CODE = "E0000"
        IF WS-BSA-CTR-REQUIRED = "N"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " CTR=" WS-BSA-CTR-REQUIRED " expected=N"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-BSA-RESULT-CODE " expected=E0000"
    END-IF.

*> ---------------------------------------------------------------
*> SA-005: SARQ - pending SAR exists (filing status = "P")
*> ---------------------------------------------------------------
TEST-SA-005.
    ADD 1 TO WS-TEST-COUNT
    MOVE "SA-005: SARQ pending SAR exists" TO WS-TEST-NAME
    INITIALIZE CTR-RECORD
    INITIALIZE WS-TXN-INFO
    INITIALIZE WS-BSA-RESULT
    MOVE "SARQ" TO WS-FUNCTION
    MOVE 2000000005 TO WS-BSA-CUST-ID
    MOVE 20260215 TO WS-BSA-TXN-DATE
    MOVE "P" TO CTR-FILING-STATUS
    CALL "BSACTRO" USING WS-FUNCTION CTR-RECORD
                         WS-TXN-INFO WS-BSA-RESULT
    IF WS-BSA-CTR-REQUIRED = "Y"
        IF WS-BSA-RESULT-MSG(1:11) = "Pending SAR"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " msg=" WS-BSA-RESULT-MSG
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " CTR=" WS-BSA-CTR-REQUIRED " expected=Y"
            " rc=" WS-BSA-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> SA-006: SARQ - no pending SAR (filing status = "F")
*> ---------------------------------------------------------------
TEST-SA-006.
    ADD 1 TO WS-TEST-COUNT
    MOVE "SA-006: SARQ no pending SAR" TO WS-TEST-NAME
    INITIALIZE CTR-RECORD
    INITIALIZE WS-TXN-INFO
    INITIALIZE WS-BSA-RESULT
    MOVE "SARQ" TO WS-FUNCTION
    MOVE 2000000006 TO WS-BSA-CUST-ID
    MOVE 20260215 TO WS-BSA-TXN-DATE
    MOVE "F" TO CTR-FILING-STATUS
    CALL "BSACTRO" USING WS-FUNCTION CTR-RECORD
                         WS-TXN-INFO WS-BSA-RESULT
    IF WS-BSA-RESULT-CODE = "E0000"
        IF WS-BSA-CTR-REQUIRED = "N"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " CTR=" WS-BSA-CTR-REQUIRED " expected=N"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-BSA-RESULT-CODE
    END-IF.

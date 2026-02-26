IDENTIFICATION DIVISION.
PROGRAM-ID. TEST-BSAAML.
*> ================================================================
*> TEST-BSAAML - Test suite for BSACTRO BSA/AML CTR Generator
*> Tests: CTR threshold, aggregation, exemptions, cash direction
*> 8 tests (BA-001 to BA-008)
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
    DISPLAY "TEST SUITE: BSAAML - BSA/AML CTR".
    DISPLAY "========================================".

    PERFORM TEST-BA-001
    PERFORM TEST-BA-002
    PERFORM TEST-BA-003
    PERFORM TEST-BA-004
    PERFORM TEST-BA-005
    PERFORM TEST-BA-006
    PERFORM TEST-BA-007
    PERFORM TEST-BA-008

    DISPLAY "========================================".
    DISPLAY "RESULTS: " WS-PASS-COUNT "/" WS-TEST-COUNT
            " PASSED".
    DISPLAY "         " WS-FAIL-COUNT " FAILED".
    DISPLAY "========================================".
    MOVE WS-FAIL-COUNT TO RETURN-CODE
    STOP RUN.

*> ---------------------------------------------------------------
*> BA-001: CHEK - cash $10,000.00 exactly -> CTR required (Y)
*> ---------------------------------------------------------------
TEST-BA-001.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BA-001: Cash $10K exactly -> CTR req" TO WS-TEST-NAME
    INITIALIZE CTR-RECORD
    INITIALIZE WS-TXN-INFO
    INITIALIZE WS-BSA-RESULT
    MOVE "CHEK" TO WS-FUNCTION
    MOVE 1000000001 TO WS-BSA-CUST-ID
    MOVE 20260215 TO WS-BSA-TXN-DATE
    MOVE 10000.00 TO WS-BSA-CASH-AMOUNT
    MOVE "I" TO WS-BSA-CASH-DIRECTION
    MOVE "Y" TO WS-BSA-IS-CASH
    MOVE 100000000001 TO WS-BSA-ACCT-NUMBER
    MOVE "N" TO CTR-EXEMPT-FLAG
    CALL "BSACTRO" USING WS-FUNCTION CTR-RECORD
                         WS-TXN-INFO WS-BSA-RESULT
    IF WS-BSA-RESULT-CODE = "E0000"
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
            " rc=" WS-BSA-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> BA-002: CHEK - cash $9,999.99 -> CTR not required (N)
*> ---------------------------------------------------------------
TEST-BA-002.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BA-002: Cash $9999.99 -> no CTR" TO WS-TEST-NAME
    INITIALIZE CTR-RECORD
    INITIALIZE WS-TXN-INFO
    INITIALIZE WS-BSA-RESULT
    MOVE "CHEK" TO WS-FUNCTION
    MOVE 1000000001 TO WS-BSA-CUST-ID
    MOVE 20260215 TO WS-BSA-TXN-DATE
    MOVE 9999.99 TO WS-BSA-CASH-AMOUNT
    MOVE "I" TO WS-BSA-CASH-DIRECTION
    MOVE "Y" TO WS-BSA-IS-CASH
    MOVE 100000000001 TO WS-BSA-ACCT-NUMBER
    MOVE "N" TO CTR-EXEMPT-FLAG
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

*> ---------------------------------------------------------------
*> BA-003: AGGR - $6K + $5K cash-in same day = $11K -> CTR req
*> ---------------------------------------------------------------
TEST-BA-003.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BA-003: Aggregate $6K+$5K -> CTR req" TO WS-TEST-NAME
    INITIALIZE CTR-RECORD
    INITIALIZE WS-TXN-INFO
    INITIALIZE WS-BSA-RESULT
    *> First aggregation call: $6,000 cash-in
    MOVE "AGGR" TO WS-FUNCTION
    MOVE 1000000001 TO WS-BSA-CUST-ID
    MOVE 20260215 TO WS-BSA-TXN-DATE
    MOVE 6000.00 TO WS-BSA-CASH-AMOUNT
    MOVE "I" TO WS-BSA-CASH-DIRECTION
    MOVE "Y" TO WS-BSA-IS-CASH
    MOVE 100000000001 TO WS-BSA-ACCT-NUMBER
    MOVE "N" TO CTR-EXEMPT-FLAG
    CALL "BSACTRO" USING WS-FUNCTION CTR-RECORD
                         WS-TXN-INFO WS-BSA-RESULT
    *> Second aggregation call: $5,000 cash-in (total = $11,000)
    INITIALIZE WS-TXN-INFO
    INITIALIZE WS-BSA-RESULT
    MOVE "AGGR" TO WS-FUNCTION
    MOVE 1000000001 TO WS-BSA-CUST-ID
    MOVE 20260215 TO WS-BSA-TXN-DATE
    MOVE 5000.00 TO WS-BSA-CASH-AMOUNT
    MOVE "I" TO WS-BSA-CASH-DIRECTION
    MOVE "Y" TO WS-BSA-IS-CASH
    MOVE 100000000001 TO WS-BSA-ACCT-NUMBER
    CALL "BSACTRO" USING WS-FUNCTION CTR-RECORD
                         WS-TXN-INFO WS-BSA-RESULT
    IF WS-BSA-RESULT-CODE = "E0000"
        IF WS-BSA-CTR-REQUIRED = "Y"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " CTR=" WS-BSA-CTR-REQUIRED " expected=Y"
                " in-total=" WS-BSA-CASH-IN-TOTAL
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-BSA-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> BA-004: AGGR - cash-in $6K + cash-out $5K tracked separately
*> ---------------------------------------------------------------
TEST-BA-004.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BA-004: In/out tracked separately" TO WS-TEST-NAME
    INITIALIZE CTR-RECORD
    INITIALIZE WS-TXN-INFO
    INITIALIZE WS-BSA-RESULT
    *> First: $6,000 cash-in
    MOVE "AGGR" TO WS-FUNCTION
    MOVE 1000000002 TO WS-BSA-CUST-ID
    MOVE 20260215 TO WS-BSA-TXN-DATE
    MOVE 6000.00 TO WS-BSA-CASH-AMOUNT
    MOVE "I" TO WS-BSA-CASH-DIRECTION
    MOVE "Y" TO WS-BSA-IS-CASH
    MOVE 100000000001 TO WS-BSA-ACCT-NUMBER
    MOVE "N" TO CTR-EXEMPT-FLAG
    CALL "BSACTRO" USING WS-FUNCTION CTR-RECORD
                         WS-TXN-INFO WS-BSA-RESULT
    *> Second: $5,000 cash-OUT
    INITIALIZE WS-TXN-INFO
    INITIALIZE WS-BSA-RESULT
    MOVE "AGGR" TO WS-FUNCTION
    MOVE 1000000002 TO WS-BSA-CUST-ID
    MOVE 20260215 TO WS-BSA-TXN-DATE
    MOVE 5000.00 TO WS-BSA-CASH-AMOUNT
    MOVE "O" TO WS-BSA-CASH-DIRECTION
    MOVE "Y" TO WS-BSA-IS-CASH
    MOVE 100000000001 TO WS-BSA-ACCT-NUMBER
    CALL "BSACTRO" USING WS-FUNCTION CTR-RECORD
                         WS-TXN-INFO WS-BSA-RESULT
    IF WS-BSA-RESULT-CODE = "E0000"
        *> Neither in nor out should hit $10K threshold
        IF WS-BSA-CTR-REQUIRED = "N"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " in=" WS-BSA-CASH-IN-TOTAL
                " out=" WS-BSA-CASH-OUT-TOTAL
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " CTR=" WS-BSA-CTR-REQUIRED " expected=N"
                " in=" WS-BSA-CASH-IN-TOTAL
                " out=" WS-BSA-CASH-OUT-TOTAL
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-BSA-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> BA-005: CHEK - exempt customer -> not required even >$10K
*> ---------------------------------------------------------------
TEST-BA-005.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BA-005: Exempt customer no CTR" TO WS-TEST-NAME
    INITIALIZE CTR-RECORD
    INITIALIZE WS-TXN-INFO
    INITIALIZE WS-BSA-RESULT
    MOVE "CHEK" TO WS-FUNCTION
    MOVE 1000000003 TO WS-BSA-CUST-ID
    MOVE 20260215 TO WS-BSA-TXN-DATE
    MOVE 50000.00 TO WS-BSA-CASH-AMOUNT
    MOVE "I" TO WS-BSA-CASH-DIRECTION
    MOVE "Y" TO WS-BSA-IS-CASH
    MOVE 100000000001 TO WS-BSA-ACCT-NUMBER
    MOVE "Y" TO CTR-EXEMPT-FLAG
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

*> ---------------------------------------------------------------
*> BA-006: CHEK - non-cash txn $15K -> CTR not required
*> ---------------------------------------------------------------
TEST-BA-006.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BA-006: Non-cash $15K no CTR" TO WS-TEST-NAME
    INITIALIZE CTR-RECORD
    INITIALIZE WS-TXN-INFO
    INITIALIZE WS-BSA-RESULT
    MOVE "CHEK" TO WS-FUNCTION
    MOVE 1000000004 TO WS-BSA-CUST-ID
    MOVE 20260215 TO WS-BSA-TXN-DATE
    MOVE 15000.00 TO WS-BSA-CASH-AMOUNT
    MOVE "I" TO WS-BSA-CASH-DIRECTION
    MOVE "N" TO WS-BSA-IS-CASH
    MOVE 100000000001 TO WS-BSA-ACCT-NUMBER
    MOVE "N" TO CTR-EXEMPT-FLAG
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

*> ---------------------------------------------------------------
*> BA-007: AGGR - cash-in total accumulates correctly
*> ---------------------------------------------------------------
TEST-BA-007.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BA-007: Cash-in accumulates correctly" TO WS-TEST-NAME
    INITIALIZE CTR-RECORD
    INITIALIZE WS-TXN-INFO
    INITIALIZE WS-BSA-RESULT
    *> First deposit: $3,000 cash-in
    MOVE "AGGR" TO WS-FUNCTION
    MOVE 1000000005 TO WS-BSA-CUST-ID
    MOVE 20260215 TO WS-BSA-TXN-DATE
    MOVE 3000.00 TO WS-BSA-CASH-AMOUNT
    MOVE "I" TO WS-BSA-CASH-DIRECTION
    MOVE "Y" TO WS-BSA-IS-CASH
    MOVE 100000000001 TO WS-BSA-ACCT-NUMBER
    MOVE "N" TO CTR-EXEMPT-FLAG
    CALL "BSACTRO" USING WS-FUNCTION CTR-RECORD
                         WS-TXN-INFO WS-BSA-RESULT
    *> Second deposit: $4,000 cash-in (total should be $7,000)
    INITIALIZE WS-TXN-INFO
    INITIALIZE WS-BSA-RESULT
    MOVE "AGGR" TO WS-FUNCTION
    MOVE 1000000005 TO WS-BSA-CUST-ID
    MOVE 20260215 TO WS-BSA-TXN-DATE
    MOVE 4000.00 TO WS-BSA-CASH-AMOUNT
    MOVE "I" TO WS-BSA-CASH-DIRECTION
    MOVE "Y" TO WS-BSA-IS-CASH
    MOVE 100000000001 TO WS-BSA-ACCT-NUMBER
    CALL "BSACTRO" USING WS-FUNCTION CTR-RECORD
                         WS-TXN-INFO WS-BSA-RESULT
    IF WS-BSA-RESULT-CODE = "E0000"
        IF WS-BSA-CASH-IN-TOTAL = 7000.00
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " in-total=" WS-BSA-CASH-IN-TOTAL
                " expected=7000.00"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-BSA-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> BA-008: AGGR - cash-out total accumulates correctly
*> ---------------------------------------------------------------
TEST-BA-008.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BA-008: Cash-out accumulates correctly" TO WS-TEST-NAME
    INITIALIZE CTR-RECORD
    INITIALIZE WS-TXN-INFO
    INITIALIZE WS-BSA-RESULT
    *> First withdrawal: $2,000 cash-out
    MOVE "AGGR" TO WS-FUNCTION
    MOVE 1000000006 TO WS-BSA-CUST-ID
    MOVE 20260215 TO WS-BSA-TXN-DATE
    MOVE 2000.00 TO WS-BSA-CASH-AMOUNT
    MOVE "O" TO WS-BSA-CASH-DIRECTION
    MOVE "Y" TO WS-BSA-IS-CASH
    MOVE 100000000001 TO WS-BSA-ACCT-NUMBER
    MOVE "N" TO CTR-EXEMPT-FLAG
    CALL "BSACTRO" USING WS-FUNCTION CTR-RECORD
                         WS-TXN-INFO WS-BSA-RESULT
    *> Second withdrawal: $3,500 cash-out (total should be $5,500)
    INITIALIZE WS-TXN-INFO
    INITIALIZE WS-BSA-RESULT
    MOVE "AGGR" TO WS-FUNCTION
    MOVE 1000000006 TO WS-BSA-CUST-ID
    MOVE 20260215 TO WS-BSA-TXN-DATE
    MOVE 3500.00 TO WS-BSA-CASH-AMOUNT
    MOVE "O" TO WS-BSA-CASH-DIRECTION
    MOVE "Y" TO WS-BSA-IS-CASH
    MOVE 100000000001 TO WS-BSA-ACCT-NUMBER
    CALL "BSACTRO" USING WS-FUNCTION CTR-RECORD
                         WS-TXN-INFO WS-BSA-RESULT
    IF WS-BSA-RESULT-CODE = "E0000"
        IF WS-BSA-CASH-OUT-TOTAL = 5500.00
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " out-total=" WS-BSA-CASH-OUT-TOTAL
                " expected=5500.00"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-BSA-RESULT-CODE
    END-IF.

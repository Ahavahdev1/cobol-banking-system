IDENTIFICATION DIVISION.
PROGRAM-ID. TEST-AUDTLOG.
*> ================================================================
*> TEST-AUDTLOG - Test suite for AUDTLOG0 Audit Trail Logger
*> Tests: Write audit, read back, timestamp, entity key (9 tests)
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-TEST-COUNT          PIC 9(3) VALUE 0.
01  WS-PASS-COUNT          PIC 9(3) VALUE 0.
01  WS-FAIL-COUNT          PIC 9(3) VALUE 0.
01  WS-TEST-NAME           PIC X(60).

01  WS-AUDIT-FUNCTION      PIC X(4).
COPY CPYAUDT.
01  WS-AUDIT-RESULT.
    05  WS-AUDT-RESULT-CODE  PIC X(5).
    05  WS-AUDT-RESULT-MSG   PIC X(50).

PROCEDURE DIVISION.
MAIN-PROGRAM.
    DISPLAY "========================================".
    DISPLAY "TEST SUITE: AUDTLOG - Audit Trail".
    DISPLAY "========================================".

    PERFORM TEST-AT-001
    PERFORM TEST-AT-002
    PERFORM TEST-AT-003
    PERFORM TEST-AT-004
    PERFORM TEST-AT-005
    PERFORM TEST-AT-006
    PERFORM TEST-AT-007
    PERFORM TEST-AT-008
    PERFORM TEST-AU-009

    DISPLAY "========================================".
    DISPLAY "RESULTS: " WS-PASS-COUNT "/" WS-TEST-COUNT
            " PASSED".
    DISPLAY "         " WS-FAIL-COUNT " FAILED".
    DISPLAY "========================================".
    MOVE WS-FAIL-COUNT TO RETURN-CODE
    STOP RUN.

*> ---------------------------------------------------------------
*> AT-001: WRIT basic audit record -> E0000
*> ---------------------------------------------------------------
TEST-AT-001.
    ADD 1 TO WS-TEST-COUNT
    MOVE "AT-001: WRIT audit record -> E0000" TO WS-TEST-NAME
    INITIALIZE AUDIT-RECORD
    INITIALIZE WS-AUDIT-RESULT
    MOVE "WRIT" TO WS-AUDIT-FUNCTION
    MOVE "TELLER01" TO AUDIT-USER-ID
    MOVE "TERM0001" TO AUDIT-TERMINAL-ID
    MOVE "TXNPOST0" TO AUDIT-PROGRAM-ID
    MOVE "POST" TO AUDIT-FUNCTION
    MOVE "ACCT" TO AUDIT-ENTITY-TYPE
    MOVE "000012345678" TO AUDIT-ENTITY-KEY
    MOVE "LEDGER-BAL" TO AUDIT-FIELD-NAME
    MOVE "1000.00" TO AUDIT-BEFORE-VALUE
    MOVE "1500.00" TO AUDIT-AFTER-VALUE
    MOVE "E0000" TO AUDIT-RESULT-CODE
    MOVE "Transaction posted" TO AUDIT-DESCRIPTION
    CALL "AUDTLOG0" USING WS-AUDIT-FUNCTION AUDIT-RECORD
                          WS-AUDIT-RESULT
    IF WS-AUDT-RESULT-CODE = "E0000"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-AUDT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> AT-002: WRIT sets timestamp (non-zero)
*> ---------------------------------------------------------------
TEST-AT-002.
    ADD 1 TO WS-TEST-COUNT
    MOVE "AT-002: WRIT sets timestamp" TO WS-TEST-NAME
    INITIALIZE AUDIT-RECORD
    INITIALIZE WS-AUDIT-RESULT
    MOVE "WRIT" TO WS-AUDIT-FUNCTION
    MOVE "SYSTEM" TO AUDIT-USER-ID
    MOVE "INTCALC0" TO AUDIT-PROGRAM-ID
    MOVE "CALC" TO AUDIT-FUNCTION
    MOVE "ACCT" TO AUDIT-ENTITY-TYPE
    MOVE "000012345678" TO AUDIT-ENTITY-KEY
    CALL "AUDTLOG0" USING WS-AUDIT-FUNCTION AUDIT-RECORD
                          WS-AUDIT-RESULT
    IF WS-AUDT-RESULT-CODE = "E0000"
        IF AUDIT-TIMESTAMP > 0
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " ts=" AUDIT-TIMESTAMP
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " timestamp is zero"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-AUDT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> AT-003: WRIT assigns audit ID
*> ---------------------------------------------------------------
TEST-AT-003.
    ADD 1 TO WS-TEST-COUNT
    MOVE "AT-003: WRIT assigns audit ID" TO WS-TEST-NAME
    INITIALIZE AUDIT-RECORD
    INITIALIZE WS-AUDIT-RESULT
    MOVE "WRIT" TO WS-AUDIT-FUNCTION
    MOVE "TELLER01" TO AUDIT-USER-ID
    MOVE "ACCTMGMT" TO AUDIT-PROGRAM-ID
    MOVE "OPEN" TO AUDIT-FUNCTION
    MOVE "ACCT" TO AUDIT-ENTITY-TYPE
    MOVE "000098765432" TO AUDIT-ENTITY-KEY
    CALL "AUDTLOG0" USING WS-AUDIT-FUNCTION AUDIT-RECORD
                          WS-AUDIT-RESULT
    IF WS-AUDT-RESULT-CODE = "E0000"
        IF AUDIT-ID > 0
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " id=" AUDIT-ID
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " audit-id is zero"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-AUDT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> AT-004: WRIT assigns incrementing audit IDs
*> ---------------------------------------------------------------
TEST-AT-004.
    ADD 1 TO WS-TEST-COUNT
    MOVE "AT-004: WRIT increments audit ID" TO WS-TEST-NAME
    INITIALIZE AUDIT-RECORD
    INITIALIZE WS-AUDIT-RESULT
    MOVE "WRIT" TO WS-AUDIT-FUNCTION
    MOVE "TELLER02" TO AUDIT-USER-ID
    MOVE "TXNPOST0" TO AUDIT-PROGRAM-ID
    MOVE "POST" TO AUDIT-FUNCTION
    MOVE "ACCT" TO AUDIT-ENTITY-TYPE
    MOVE "000011111111" TO AUDIT-ENTITY-KEY
    CALL "AUDTLOG0" USING WS-AUDIT-FUNCTION AUDIT-RECORD
                          WS-AUDIT-RESULT
    IF WS-AUDT-RESULT-CODE = "E0000"
        *> This should be > previous audit ID (AT-003)
        IF AUDIT-ID > 1
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " id=" AUDIT-ID
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " id not incremented=" AUDIT-ID
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-AUDT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> AT-005: WRIT preserves entity key
*> ---------------------------------------------------------------
TEST-AT-005.
    ADD 1 TO WS-TEST-COUNT
    MOVE "AT-005: WRIT preserves entity key" TO WS-TEST-NAME
    INITIALIZE AUDIT-RECORD
    INITIALIZE WS-AUDIT-RESULT
    MOVE "WRIT" TO WS-AUDIT-FUNCTION
    MOVE "SYSTEM" TO AUDIT-USER-ID
    MOVE "GLPOST0 " TO AUDIT-PROGRAM-ID
    MOVE "POST" TO AUDIT-FUNCTION
    MOVE "GL  " TO AUDIT-ENTITY-TYPE
    MOVE "0000001010    0001" TO AUDIT-ENTITY-KEY
    CALL "AUDTLOG0" USING WS-AUDIT-FUNCTION AUDIT-RECORD
                          WS-AUDIT-RESULT
    IF WS-AUDT-RESULT-CODE = "E0000"
        IF AUDIT-ENTITY-KEY = "0000001010    0001"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " key=" AUDIT-ENTITY-KEY
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-AUDT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> AT-006: WRIT preserves before/after values
*> ---------------------------------------------------------------
TEST-AT-006.
    ADD 1 TO WS-TEST-COUNT
    MOVE "AT-006: WRIT preserves before/after" TO WS-TEST-NAME
    INITIALIZE AUDIT-RECORD
    INITIALIZE WS-AUDIT-RESULT
    MOVE "WRIT" TO WS-AUDIT-FUNCTION
    MOVE "TELLER01" TO AUDIT-USER-ID
    MOVE "TXNPOST0" TO AUDIT-PROGRAM-ID
    MOVE "POST" TO AUDIT-FUNCTION
    MOVE "ACCT" TO AUDIT-ENTITY-TYPE
    MOVE "000012345678" TO AUDIT-ENTITY-KEY
    MOVE "ACCT-LEDGER-BAL" TO AUDIT-FIELD-NAME
    MOVE "+0000000005000.00" TO AUDIT-BEFORE-VALUE
    MOVE "+0000000005500.00" TO AUDIT-AFTER-VALUE
    CALL "AUDTLOG0" USING WS-AUDIT-FUNCTION AUDIT-RECORD
                          WS-AUDIT-RESULT
    IF WS-AUDT-RESULT-CODE = "E0000"
        IF AUDIT-BEFORE-VALUE(1:17) =
            "+0000000005000.00"
            AND AUDIT-AFTER-VALUE(1:17) =
            "+0000000005500.00"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " before=" AUDIT-BEFORE-VALUE(1:17)
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-AUDT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> AT-007: READ with entity key -> E0000
*> ---------------------------------------------------------------
TEST-AT-007.
    ADD 1 TO WS-TEST-COUNT
    MOVE "AT-007: READ with entity key" TO WS-TEST-NAME
    INITIALIZE AUDIT-RECORD
    INITIALIZE WS-AUDIT-RESULT
    MOVE "READ" TO WS-AUDIT-FUNCTION
    MOVE "000012345678" TO AUDIT-ENTITY-KEY
    CALL "AUDTLOG0" USING WS-AUDIT-FUNCTION AUDIT-RECORD
                          WS-AUDIT-RESULT
    IF WS-AUDT-RESULT-CODE = "E0000"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-AUDT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> AT-008: READ without entity key -> E0002
*> ---------------------------------------------------------------
TEST-AT-008.
    ADD 1 TO WS-TEST-COUNT
    MOVE "AT-008: READ no entity key -> E0002" TO WS-TEST-NAME
    INITIALIZE AUDIT-RECORD
    INITIALIZE WS-AUDIT-RESULT
    MOVE "READ" TO WS-AUDIT-FUNCTION
    MOVE SPACES TO AUDIT-ENTITY-KEY
    CALL "AUDTLOG0" USING WS-AUDIT-FUNCTION AUDIT-RECORD
                          WS-AUDIT-RESULT
    IF WS-AUDT-RESULT-CODE = "E0002"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-AUDT-RESULT-CODE " expected=E0002"
    END-IF.

*> ---------------------------------------------------------------
*> AU-009: Invalid function -> E0001
*> ---------------------------------------------------------------
TEST-AU-009.
    ADD 1 TO WS-TEST-COUNT
    MOVE "AU-009: Invalid function -> E0001" TO WS-TEST-NAME
    INITIALIZE AUDIT-RECORD
    INITIALIZE WS-AUDIT-RESULT
    MOVE "XXXX" TO WS-AUDIT-FUNCTION
    CALL "AUDTLOG0" USING WS-AUDIT-FUNCTION AUDIT-RECORD
                          WS-AUDIT-RESULT
    IF WS-AUDT-RESULT-CODE = "E0001"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-AUDT-RESULT-CODE " expected=E0001"
    END-IF.

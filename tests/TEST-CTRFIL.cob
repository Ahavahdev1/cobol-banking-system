IDENTIFICATION DIVISION.
PROGRAM-ID. TEST-CTRFIL.
*> ================================================================
*> TEST-CTRFIL - Test suite for CTRFIL0 CTR Filing Manager
*> Tests: Create, file, query, void CTR records (12 tests)
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-TEST-COUNT          PIC 9(3) VALUE 0.
01  WS-PASS-COUNT          PIC 9(3) VALUE 0.
01  WS-FAIL-COUNT          PIC 9(3) VALUE 0.
01  WS-TEST-NAME           PIC X(60).

01  WS-CTR-FUNCTION        PIC X(4).
COPY CPYCTR.
01  WS-CTR-RESULT.
    05  WS-CTR-RESULT-CODE   PIC X(5).
    05  WS-CTR-RESULT-MSG    PIC X(50).

PROCEDURE DIVISION.
MAIN-PROGRAM.
    DISPLAY "========================================".
    DISPLAY "TEST SUITE: CTRFIL - CTR Filing Manager".
    DISPLAY "========================================".

    PERFORM TEST-CT-001
    PERFORM TEST-CT-002
    PERFORM TEST-CT-003
    PERFORM TEST-CT-004
    PERFORM TEST-CT-005
    PERFORM TEST-CT-006
    PERFORM TEST-CT-007
    PERFORM TEST-CT-008
    PERFORM TEST-CT-009
    PERFORM TEST-CT-010
    PERFORM TEST-CT-011
    PERFORM TEST-CT-012

    DISPLAY "========================================".
    DISPLAY "RESULTS: " WS-PASS-COUNT "/" WS-TEST-COUNT
            " PASSED".
    DISPLAY "         " WS-FAIL-COUNT " FAILED".
    DISPLAY "========================================".
    MOVE WS-FAIL-COUNT TO RETURN-CODE
    STOP RUN.

*> ---------------------------------------------------------------
*> CT-001: CRTE with valid data -> E0000, status=P
*> ---------------------------------------------------------------
TEST-CT-001.
    ADD 1 TO WS-TEST-COUNT
    MOVE "CT-001: CRTE valid data -> E0000, status=P"
        TO WS-TEST-NAME
    INITIALIZE CTR-RECORD
    INITIALIZE WS-CTR-RESULT
    MOVE "CRTE" TO WS-CTR-FUNCTION
    MOVE 1234567890 TO CTR-CUST-ID
    MOVE "JOHN DOE" TO CTR-CUST-NAME
    MOVE 15000.00 TO CTR-CASH-IN-TOTAL
    MOVE 1 TO CTR-CASH-IN-COUNT
    MOVE 20260215 TO CTR-TXN-DATE
    MOVE 1001 TO CTR-BRANCH-ID
    MOVE "TELLER01" TO CTR-TELLER-ID
    CALL "CTRFIL0" USING WS-CTR-FUNCTION CTR-RECORD
                         WS-CTR-RESULT
    IF WS-CTR-RESULT-CODE = "E0000"
        AND CTR-FILING-STATUS = "P"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-CTR-RESULT-CODE
            " status=" CTR-FILING-STATUS
    END-IF.

*> ---------------------------------------------------------------
*> CT-002: CRTE assigns filing ID (non-zero)
*> ---------------------------------------------------------------
TEST-CT-002.
    ADD 1 TO WS-TEST-COUNT
    MOVE "CT-002: CRTE assigns filing ID" TO WS-TEST-NAME
    INITIALIZE CTR-RECORD
    INITIALIZE WS-CTR-RESULT
    MOVE "CRTE" TO WS-CTR-FUNCTION
    MOVE 9876543210 TO CTR-CUST-ID
    MOVE "JANE SMITH" TO CTR-CUST-NAME
    MOVE 25000.00 TO CTR-CASH-OUT-TOTAL
    MOVE 2 TO CTR-CASH-OUT-COUNT
    MOVE 20260215 TO CTR-TXN-DATE
    CALL "CTRFIL0" USING WS-CTR-FUNCTION CTR-RECORD
                         WS-CTR-RESULT
    IF WS-CTR-RESULT-CODE = "E0000"
        IF CTR-FILING-ID > 0
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " id=" CTR-FILING-ID
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " filing-id is zero"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-CTR-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> CT-003: CRTE with zero customer ID -> E0002
*> ---------------------------------------------------------------
TEST-CT-003.
    ADD 1 TO WS-TEST-COUNT
    MOVE "CT-003: CRTE zero customer -> E0002" TO WS-TEST-NAME
    INITIALIZE CTR-RECORD
    INITIALIZE WS-CTR-RESULT
    MOVE "CRTE" TO WS-CTR-FUNCTION
    MOVE 0 TO CTR-CUST-ID
    MOVE 15000.00 TO CTR-CASH-IN-TOTAL
    CALL "CTRFIL0" USING WS-CTR-FUNCTION CTR-RECORD
                         WS-CTR-RESULT
    IF WS-CTR-RESULT-CODE = "E0002"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-CTR-RESULT-CODE " expected=E0002"
    END-IF.

*> ---------------------------------------------------------------
*> CT-004: CRTE stamps filing date (non-zero)
*> ---------------------------------------------------------------
TEST-CT-004.
    ADD 1 TO WS-TEST-COUNT
    MOVE "CT-004: CRTE stamps filing date" TO WS-TEST-NAME
    INITIALIZE CTR-RECORD
    INITIALIZE WS-CTR-RESULT
    MOVE "CRTE" TO WS-CTR-FUNCTION
    MOVE 1111111111 TO CTR-CUST-ID
    MOVE "TEST CUSTOMER" TO CTR-CUST-NAME
    MOVE 12000.00 TO CTR-CASH-IN-TOTAL
    MOVE 1 TO CTR-CASH-IN-COUNT
    CALL "CTRFIL0" USING WS-CTR-FUNCTION CTR-RECORD
                         WS-CTR-RESULT
    IF WS-CTR-RESULT-CODE = "E0000"
        IF CTR-FILING-DATE > 0
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " date=" CTR-FILING-DATE
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " filing date is zero"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-CTR-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> CT-005: FILE pending CTR -> E0000, status=F
*> ---------------------------------------------------------------
TEST-CT-005.
    ADD 1 TO WS-TEST-COUNT
    MOVE "CT-005: FILE pending CTR -> E0000, status=F"
        TO WS-TEST-NAME
    *> First create a CTR
    INITIALIZE CTR-RECORD
    INITIALIZE WS-CTR-RESULT
    MOVE "CRTE" TO WS-CTR-FUNCTION
    MOVE 2222222222 TO CTR-CUST-ID
    MOVE "FILE TEST CUSTOMER" TO CTR-CUST-NAME
    MOVE 20000.00 TO CTR-CASH-IN-TOTAL
    MOVE 1 TO CTR-CASH-IN-COUNT
    CALL "CTRFIL0" USING WS-CTR-FUNCTION CTR-RECORD
                         WS-CTR-RESULT
    *> Now file it
    INITIALIZE WS-CTR-RESULT
    MOVE "FILE" TO WS-CTR-FUNCTION
    CALL "CTRFIL0" USING WS-CTR-FUNCTION CTR-RECORD
                         WS-CTR-RESULT
    IF WS-CTR-RESULT-CODE = "E0000"
        AND CTR-FILING-STATUS = "F"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-CTR-RESULT-CODE
            " status=" CTR-FILING-STATUS
    END-IF.

*> ---------------------------------------------------------------
*> CT-006: FILE already filed CTR -> E0005
*> ---------------------------------------------------------------
TEST-CT-006.
    ADD 1 TO WS-TEST-COUNT
    MOVE "CT-006: FILE already filed -> E0005"
        TO WS-TEST-NAME
    *> CTR-RECORD still has status=F from CT-005
    INITIALIZE WS-CTR-RESULT
    MOVE "FILE" TO WS-CTR-FUNCTION
    CALL "CTRFIL0" USING WS-CTR-FUNCTION CTR-RECORD
                         WS-CTR-RESULT
    IF WS-CTR-RESULT-CODE = "E0005"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-CTR-RESULT-CODE " expected=E0005"
    END-IF.

*> ---------------------------------------------------------------
*> CT-007: QURY returns CTR data -> E0000
*> ---------------------------------------------------------------
TEST-CT-007.
    ADD 1 TO WS-TEST-COUNT
    MOVE "CT-007: QURY returns CTR data -> E0000"
        TO WS-TEST-NAME
    *> Create a fresh CTR for querying
    INITIALIZE CTR-RECORD
    INITIALIZE WS-CTR-RESULT
    MOVE "CRTE" TO WS-CTR-FUNCTION
    MOVE 3333333333 TO CTR-CUST-ID
    MOVE "QUERY TEST CUSTOMER" TO CTR-CUST-NAME
    MOVE 18000.00 TO CTR-CASH-IN-TOTAL
    MOVE 1 TO CTR-CASH-IN-COUNT
    CALL "CTRFIL0" USING WS-CTR-FUNCTION CTR-RECORD
                         WS-CTR-RESULT
    *> Now query it
    INITIALIZE WS-CTR-RESULT
    MOVE "QURY" TO WS-CTR-FUNCTION
    CALL "CTRFIL0" USING WS-CTR-FUNCTION CTR-RECORD
                         WS-CTR-RESULT
    IF WS-CTR-RESULT-CODE = "E0000"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-CTR-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> CT-008: QURY with zero customer -> E0002
*> ---------------------------------------------------------------
TEST-CT-008.
    ADD 1 TO WS-TEST-COUNT
    MOVE "CT-008: QURY zero customer -> E0002"
        TO WS-TEST-NAME
    INITIALIZE CTR-RECORD
    INITIALIZE WS-CTR-RESULT
    MOVE "QURY" TO WS-CTR-FUNCTION
    MOVE 0 TO CTR-CUST-ID
    CALL "CTRFIL0" USING WS-CTR-FUNCTION CTR-RECORD
                         WS-CTR-RESULT
    IF WS-CTR-RESULT-CODE = "E0002"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-CTR-RESULT-CODE " expected=E0002"
    END-IF.

*> ---------------------------------------------------------------
*> CT-009: VOID pending CTR -> E0000, status=V
*> ---------------------------------------------------------------
TEST-CT-009.
    ADD 1 TO WS-TEST-COUNT
    MOVE "CT-009: VOID pending CTR -> E0000, status=V"
        TO WS-TEST-NAME
    *> Create a fresh CTR
    INITIALIZE CTR-RECORD
    INITIALIZE WS-CTR-RESULT
    MOVE "CRTE" TO WS-CTR-FUNCTION
    MOVE 4444444444 TO CTR-CUST-ID
    MOVE "VOID TEST CUSTOMER" TO CTR-CUST-NAME
    MOVE 11000.00 TO CTR-CASH-IN-TOTAL
    MOVE 1 TO CTR-CASH-IN-COUNT
    CALL "CTRFIL0" USING WS-CTR-FUNCTION CTR-RECORD
                         WS-CTR-RESULT
    *> Now void it
    INITIALIZE WS-CTR-RESULT
    MOVE "VOID" TO WS-CTR-FUNCTION
    CALL "CTRFIL0" USING WS-CTR-FUNCTION CTR-RECORD
                         WS-CTR-RESULT
    IF WS-CTR-RESULT-CODE = "E0000"
        AND CTR-FILING-STATUS = "V"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-CTR-RESULT-CODE
            " status=" CTR-FILING-STATUS
    END-IF.

*> ---------------------------------------------------------------
*> CT-010: VOID filed CTR -> E0006
*> ---------------------------------------------------------------
TEST-CT-010.
    ADD 1 TO WS-TEST-COUNT
    MOVE "CT-010: VOID filed CTR -> E0006" TO WS-TEST-NAME
    *> Create and file a CTR
    INITIALIZE CTR-RECORD
    INITIALIZE WS-CTR-RESULT
    MOVE "CRTE" TO WS-CTR-FUNCTION
    MOVE 5555555555 TO CTR-CUST-ID
    MOVE "VOID-FILED TEST" TO CTR-CUST-NAME
    MOVE 30000.00 TO CTR-CASH-OUT-TOTAL
    MOVE 1 TO CTR-CASH-OUT-COUNT
    CALL "CTRFIL0" USING WS-CTR-FUNCTION CTR-RECORD
                         WS-CTR-RESULT
    *> File it
    INITIALIZE WS-CTR-RESULT
    MOVE "FILE" TO WS-CTR-FUNCTION
    CALL "CTRFIL0" USING WS-CTR-FUNCTION CTR-RECORD
                         WS-CTR-RESULT
    *> Now try to void the filed CTR
    INITIALIZE WS-CTR-RESULT
    MOVE "VOID" TO WS-CTR-FUNCTION
    CALL "CTRFIL0" USING WS-CTR-FUNCTION CTR-RECORD
                         WS-CTR-RESULT
    IF WS-CTR-RESULT-CODE = "E0006"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-CTR-RESULT-CODE " expected=E0006"
    END-IF.

*> ---------------------------------------------------------------
*> CT-011: Invalid function -> E0001
*> ---------------------------------------------------------------
TEST-CT-011.
    ADD 1 TO WS-TEST-COUNT
    MOVE "CT-011: Invalid function -> E0001" TO WS-TEST-NAME
    INITIALIZE CTR-RECORD
    INITIALIZE WS-CTR-RESULT
    MOVE "XXXX" TO WS-CTR-FUNCTION
    CALL "CTRFIL0" USING WS-CTR-FUNCTION CTR-RECORD
                         WS-CTR-RESULT
    IF WS-CTR-RESULT-CODE = "E0001"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-CTR-RESULT-CODE " expected=E0001"
    END-IF.

*> ---------------------------------------------------------------
*> CT-012: CRTE with zero cash amounts -> E0003
*> Both CTR-CASH-IN-TOTAL and CTR-CASH-OUT-TOTAL are zero
*> ---------------------------------------------------------------
TEST-CT-012.
    ADD 1 TO WS-TEST-COUNT
    MOVE "CT-012: CRTE zero cash amounts -> E0003"
        TO WS-TEST-NAME
    INITIALIZE CTR-RECORD
    INITIALIZE WS-CTR-RESULT
    MOVE "CRTE" TO WS-CTR-FUNCTION
    MOVE 1000000001 TO CTR-CUST-ID
    MOVE "ZERO CASH TEST" TO CTR-CUST-NAME
    MOVE 0 TO CTR-CASH-IN-TOTAL
    MOVE 0 TO CTR-CASH-OUT-TOTAL
    MOVE 20260215 TO CTR-TXN-DATE
    MOVE 1001 TO CTR-BRANCH-ID
    MOVE "TELLER01" TO CTR-TELLER-ID
    CALL "CTRFIL0" USING WS-CTR-FUNCTION CTR-RECORD
                         WS-CTR-RESULT
    IF WS-CTR-RESULT-CODE = "E0003"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-CTR-RESULT-CODE " expected=E0003"
    END-IF.

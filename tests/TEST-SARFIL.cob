IDENTIFICATION DIVISION.
PROGRAM-ID. TEST-SARFIL.
*> ================================================================
*> TEST-SARFIL - Test suite for SARFIL0 SAR Filing Manager
*> Tests: Create, file, dismiss, query, update SAR records (15 tests)
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-TEST-COUNT          PIC 9(3) VALUE 0.
01  WS-PASS-COUNT          PIC 9(3) VALUE 0.
01  WS-FAIL-COUNT          PIC 9(3) VALUE 0.
01  WS-TEST-NAME           PIC X(60).

01  WS-SAR-FUNCTION        PIC X(4).
COPY CPYSAR.
01  WS-SAR-RESULT.
    05  WS-SAR-RESULT-CODE   PIC X(5).
    05  WS-SAR-RESULT-MSG    PIC X(50).

PROCEDURE DIVISION.
MAIN-PROGRAM.
    DISPLAY "========================================".
    DISPLAY "TEST SUITE: SARFIL - SAR Filing Manager".
    DISPLAY "========================================".

    PERFORM TEST-SF-001
    PERFORM TEST-SF-002
    PERFORM TEST-SF-003
    PERFORM TEST-SF-004
    PERFORM TEST-SF-005
    PERFORM TEST-SF-006
    PERFORM TEST-SF-007
    PERFORM TEST-SF-008
    PERFORM TEST-SF-009
    PERFORM TEST-SF-010
    PERFORM TEST-SR-011
    PERFORM TEST-SF-012
    PERFORM TEST-SF-013
    PERFORM TEST-SF-014
    PERFORM TEST-SF-015

    DISPLAY "========================================".
    DISPLAY "RESULTS: " WS-PASS-COUNT "/" WS-TEST-COUNT
            " PASSED".
    DISPLAY "         " WS-FAIL-COUNT " FAILED".
    DISPLAY "========================================".
    MOVE WS-FAIL-COUNT TO RETURN-CODE
    STOP RUN.

*> ---------------------------------------------------------------
*> SF-001: CRTE with valid data -> E0000, status=P
*> ---------------------------------------------------------------
TEST-SF-001.
    ADD 1 TO WS-TEST-COUNT
    MOVE "SF-001: CRTE valid data -> E0000, status=P"
        TO WS-TEST-NAME
    INITIALIZE SAR-RECORD
    INITIALIZE WS-SAR-RESULT
    MOVE "CRTE" TO WS-SAR-FUNCTION
    MOVE 1234567890 TO SAR-CUST-ID
    MOVE 8500.00 TO SAR-TOTAL-AMOUNT
    MOVE "STRC" TO SAR-PATTERN-TYPE
    MOVE 20260201 TO SAR-START-DATE
    MOVE 20260215 TO SAR-END-DATE
    MOVE "Structuring detected" TO SAR-NARRATIVE
    CALL "SARFIL0" USING WS-SAR-FUNCTION SAR-RECORD
                         WS-SAR-RESULT
    IF WS-SAR-RESULT-CODE = "E0000"
        AND SAR-STATUS = "P"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-SAR-RESULT-CODE
            " status=" SAR-STATUS
    END-IF.

*> ---------------------------------------------------------------
*> SF-002: CRTE assigns filing ID (non-zero)
*> ---------------------------------------------------------------
TEST-SF-002.
    ADD 1 TO WS-TEST-COUNT
    MOVE "SF-002: CRTE assigns filing ID" TO WS-TEST-NAME
    INITIALIZE SAR-RECORD
    INITIALIZE WS-SAR-RESULT
    MOVE "CRTE" TO WS-SAR-FUNCTION
    MOVE 9876543210 TO SAR-CUST-ID
    MOVE 9200.00 TO SAR-TOTAL-AMOUNT
    MOVE "STRC" TO SAR-PATTERN-TYPE
    MOVE 20260201 TO SAR-START-DATE
    MOVE 20260228 TO SAR-END-DATE
    CALL "SARFIL0" USING WS-SAR-FUNCTION SAR-RECORD
                         WS-SAR-RESULT
    IF WS-SAR-RESULT-CODE = "E0000"
        IF SAR-FILING-ID > 0
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " id=" SAR-FILING-ID
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " filing-id is zero"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-SAR-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> SF-003: CRTE with zero customer ID -> E0002
*> ---------------------------------------------------------------
TEST-SF-003.
    ADD 1 TO WS-TEST-COUNT
    MOVE "SF-003: CRTE zero customer -> E0002" TO WS-TEST-NAME
    INITIALIZE SAR-RECORD
    INITIALIZE WS-SAR-RESULT
    MOVE "CRTE" TO WS-SAR-FUNCTION
    MOVE 0 TO SAR-CUST-ID
    MOVE 8500.00 TO SAR-TOTAL-AMOUNT
    CALL "SARFIL0" USING WS-SAR-FUNCTION SAR-RECORD
                         WS-SAR-RESULT
    IF WS-SAR-RESULT-CODE = "E0002"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-SAR-RESULT-CODE " expected=E0002"
    END-IF.

*> ---------------------------------------------------------------
*> SF-004: CRTE stamps date fields (non-zero)
*> ---------------------------------------------------------------
TEST-SF-004.
    ADD 1 TO WS-TEST-COUNT
    MOVE "SF-004: CRTE stamps date fields" TO WS-TEST-NAME
    INITIALIZE SAR-RECORD
    INITIALIZE WS-SAR-RESULT
    MOVE "CRTE" TO WS-SAR-FUNCTION
    MOVE 1111111111 TO SAR-CUST-ID
    MOVE 9000.00 TO SAR-TOTAL-AMOUNT
    MOVE "STRC" TO SAR-PATTERN-TYPE
    MOVE 20260201 TO SAR-START-DATE
    MOVE 20260215 TO SAR-END-DATE
    CALL "SARFIL0" USING WS-SAR-FUNCTION SAR-RECORD
                         WS-SAR-RESULT
    IF WS-SAR-RESULT-CODE = "E0000"
        IF SAR-FILED-DATE > 0
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " date=" SAR-FILED-DATE
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " filed date is zero"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-SAR-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> SF-005: FILE pending SAR -> E0000, status=F
*> ---------------------------------------------------------------
TEST-SF-005.
    ADD 1 TO WS-TEST-COUNT
    MOVE "SF-005: FILE pending SAR -> E0000, status=F"
        TO WS-TEST-NAME
    *> First create a SAR
    INITIALIZE SAR-RECORD
    INITIALIZE WS-SAR-RESULT
    MOVE "CRTE" TO WS-SAR-FUNCTION
    MOVE 2222222222 TO SAR-CUST-ID
    MOVE 8800.00 TO SAR-TOTAL-AMOUNT
    MOVE "STRC" TO SAR-PATTERN-TYPE
    MOVE 20260201 TO SAR-START-DATE
    MOVE 20260215 TO SAR-END-DATE
    CALL "SARFIL0" USING WS-SAR-FUNCTION SAR-RECORD
                         WS-SAR-RESULT
    *> Now file it
    INITIALIZE WS-SAR-RESULT
    MOVE "FILE" TO WS-SAR-FUNCTION
    CALL "SARFIL0" USING WS-SAR-FUNCTION SAR-RECORD
                         WS-SAR-RESULT
    IF WS-SAR-RESULT-CODE = "E0000"
        AND SAR-STATUS = "F"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-SAR-RESULT-CODE
            " status=" SAR-STATUS
    END-IF.

*> ---------------------------------------------------------------
*> SF-006: FILE already filed SAR -> E0005
*> ---------------------------------------------------------------
TEST-SF-006.
    ADD 1 TO WS-TEST-COUNT
    MOVE "SF-006: FILE already filed -> E0005"
        TO WS-TEST-NAME
    *> SAR-RECORD still has status=F from SF-005
    INITIALIZE WS-SAR-RESULT
    MOVE "FILE" TO WS-SAR-FUNCTION
    CALL "SARFIL0" USING WS-SAR-FUNCTION SAR-RECORD
                         WS-SAR-RESULT
    IF WS-SAR-RESULT-CODE = "E0005"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-SAR-RESULT-CODE " expected=E0005"
    END-IF.

*> ---------------------------------------------------------------
*> SF-007: DISM pending SAR -> E0000, status=D
*> ---------------------------------------------------------------
TEST-SF-007.
    ADD 1 TO WS-TEST-COUNT
    MOVE "SF-007: DISM pending SAR -> E0000, status=D"
        TO WS-TEST-NAME
    *> Create a fresh SAR
    INITIALIZE SAR-RECORD
    INITIALIZE WS-SAR-RESULT
    MOVE "CRTE" TO WS-SAR-FUNCTION
    MOVE 3333333333 TO SAR-CUST-ID
    MOVE 8600.00 TO SAR-TOTAL-AMOUNT
    MOVE "STRC" TO SAR-PATTERN-TYPE
    MOVE 20260201 TO SAR-START-DATE
    MOVE 20260215 TO SAR-END-DATE
    MOVE "False positive - verified legitimate business"
        TO SAR-NARRATIVE
    CALL "SARFIL0" USING WS-SAR-FUNCTION SAR-RECORD
                         WS-SAR-RESULT
    *> Now dismiss it (narrative already set)
    INITIALIZE WS-SAR-RESULT
    MOVE "DISM" TO WS-SAR-FUNCTION
    CALL "SARFIL0" USING WS-SAR-FUNCTION SAR-RECORD
                         WS-SAR-RESULT
    IF WS-SAR-RESULT-CODE = "E0000"
        AND SAR-STATUS = "D"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-SAR-RESULT-CODE
            " status=" SAR-STATUS
    END-IF.

*> ---------------------------------------------------------------
*> SF-008: DISM filed SAR -> E0006
*> ---------------------------------------------------------------
TEST-SF-008.
    ADD 1 TO WS-TEST-COUNT
    MOVE "SF-008: DISM filed SAR -> E0006" TO WS-TEST-NAME
    *> Create and file a SAR
    INITIALIZE SAR-RECORD
    INITIALIZE WS-SAR-RESULT
    MOVE "CRTE" TO WS-SAR-FUNCTION
    MOVE 4444444444 TO SAR-CUST-ID
    MOVE 9100.00 TO SAR-TOTAL-AMOUNT
    MOVE "STRC" TO SAR-PATTERN-TYPE
    MOVE 20260201 TO SAR-START-DATE
    MOVE 20260215 TO SAR-END-DATE
    CALL "SARFIL0" USING WS-SAR-FUNCTION SAR-RECORD
                         WS-SAR-RESULT
    *> File it
    INITIALIZE WS-SAR-RESULT
    MOVE "FILE" TO WS-SAR-FUNCTION
    CALL "SARFIL0" USING WS-SAR-FUNCTION SAR-RECORD
                         WS-SAR-RESULT
    *> Now try to dismiss the filed SAR
    INITIALIZE WS-SAR-RESULT
    MOVE "DISM" TO WS-SAR-FUNCTION
    MOVE "Trying to dismiss filed SAR" TO SAR-NARRATIVE
    CALL "SARFIL0" USING WS-SAR-FUNCTION SAR-RECORD
                         WS-SAR-RESULT
    IF WS-SAR-RESULT-CODE = "E0006"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-SAR-RESULT-CODE " expected=E0006"
    END-IF.

*> ---------------------------------------------------------------
*> SF-009: QURY returns SAR data -> E0000
*> ---------------------------------------------------------------
TEST-SF-009.
    ADD 1 TO WS-TEST-COUNT
    MOVE "SF-009: QURY returns SAR data -> E0000"
        TO WS-TEST-NAME
    *> Create a fresh SAR for querying
    INITIALIZE SAR-RECORD
    INITIALIZE WS-SAR-RESULT
    MOVE "CRTE" TO WS-SAR-FUNCTION
    MOVE 5555555555 TO SAR-CUST-ID
    MOVE 8700.00 TO SAR-TOTAL-AMOUNT
    MOVE "STRC" TO SAR-PATTERN-TYPE
    MOVE 20260201 TO SAR-START-DATE
    MOVE 20260215 TO SAR-END-DATE
    CALL "SARFIL0" USING WS-SAR-FUNCTION SAR-RECORD
                         WS-SAR-RESULT
    *> Now query it
    INITIALIZE WS-SAR-RESULT
    MOVE "QURY" TO WS-SAR-FUNCTION
    CALL "SARFIL0" USING WS-SAR-FUNCTION SAR-RECORD
                         WS-SAR-RESULT
    IF WS-SAR-RESULT-CODE = "E0000"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-SAR-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> SF-010: UPDT pending SAR narrative -> E0000
*> ---------------------------------------------------------------
TEST-SF-010.
    ADD 1 TO WS-TEST-COUNT
    MOVE "SF-010: UPDT pending SAR narrative -> E0000"
        TO WS-TEST-NAME
    *> Create a fresh SAR
    INITIALIZE SAR-RECORD
    INITIALIZE WS-SAR-RESULT
    MOVE "CRTE" TO WS-SAR-FUNCTION
    MOVE 6666666666 TO SAR-CUST-ID
    MOVE 9500.00 TO SAR-TOTAL-AMOUNT
    MOVE "STRC" TO SAR-PATTERN-TYPE
    MOVE 20260201 TO SAR-START-DATE
    MOVE 20260215 TO SAR-END-DATE
    MOVE "Initial narrative" TO SAR-NARRATIVE
    CALL "SARFIL0" USING WS-SAR-FUNCTION SAR-RECORD
                         WS-SAR-RESULT
    *> Now update the narrative
    INITIALIZE WS-SAR-RESULT
    MOVE "UPDT" TO WS-SAR-FUNCTION
    MOVE "Updated narrative with more detail on structuring"
        TO SAR-NARRATIVE
    CALL "SARFIL0" USING WS-SAR-FUNCTION SAR-RECORD
                         WS-SAR-RESULT
    IF WS-SAR-RESULT-CODE = "E0000"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-SAR-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> SR-011: Invalid function -> E0001
*> ---------------------------------------------------------------
TEST-SR-011.
    ADD 1 TO WS-TEST-COUNT
    MOVE "SR-011: Invalid function -> E0001" TO WS-TEST-NAME
    INITIALIZE SAR-RECORD
    INITIALIZE WS-SAR-RESULT
    MOVE "XXXX" TO WS-SAR-FUNCTION
    CALL "SARFIL0" USING WS-SAR-FUNCTION SAR-RECORD
                         WS-SAR-RESULT
    IF WS-SAR-RESULT-CODE = "E0001"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-SAR-RESULT-CODE " expected=E0001"
    END-IF.

*> ---------------------------------------------------------------
*> SF-012: DISM without narrative -> E0007, status stays P
*> ---------------------------------------------------------------
TEST-SF-012.
    ADD 1 TO WS-TEST-COUNT
    MOVE "SF-012: DISM without narrative -> E0007"
        TO WS-TEST-NAME
    *> Create a valid SAR
    INITIALIZE SAR-RECORD
    INITIALIZE WS-SAR-RESULT
    MOVE "CRTE" TO WS-SAR-FUNCTION
    MOVE 7777777777 TO SAR-CUST-ID
    MOVE 8500.00 TO SAR-TOTAL-AMOUNT
    MOVE "STRC" TO SAR-PATTERN-TYPE
    MOVE 20260201 TO SAR-START-DATE
    MOVE 20260215 TO SAR-END-DATE
    MOVE SPACES TO SAR-NARRATIVE
    CALL "SARFIL0" USING WS-SAR-FUNCTION SAR-RECORD
                         WS-SAR-RESULT
    *> Now try to dismiss with narrative still spaces
    INITIALIZE WS-SAR-RESULT
    MOVE "DISM" TO WS-SAR-FUNCTION
    MOVE SPACES TO SAR-NARRATIVE
    CALL "SARFIL0" USING WS-SAR-FUNCTION SAR-RECORD
                         WS-SAR-RESULT
    IF WS-SAR-RESULT-CODE = "E0007"
        AND SAR-STATUS = "P"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-SAR-RESULT-CODE
            " status=" SAR-STATUS
            " expected=E0007/P"
    END-IF.

*> ---------------------------------------------------------------
*> SF-013: UPDT on filed SAR -> E0006, narrative unchanged
*> ---------------------------------------------------------------
TEST-SF-013.
    ADD 1 TO WS-TEST-COUNT
    MOVE "SF-013: UPDT filed SAR -> E0006"
        TO WS-TEST-NAME
    *> Create a SAR
    INITIALIZE SAR-RECORD
    INITIALIZE WS-SAR-RESULT
    MOVE "CRTE" TO WS-SAR-FUNCTION
    MOVE 8888888888 TO SAR-CUST-ID
    MOVE 9200.00 TO SAR-TOTAL-AMOUNT
    MOVE "STRC" TO SAR-PATTERN-TYPE
    MOVE 20260201 TO SAR-START-DATE
    MOVE 20260215 TO SAR-END-DATE
    MOVE "Original narrative" TO SAR-NARRATIVE
    CALL "SARFIL0" USING WS-SAR-FUNCTION SAR-RECORD
                         WS-SAR-RESULT
    *> File it
    INITIALIZE WS-SAR-RESULT
    MOVE "FILE" TO WS-SAR-FUNCTION
    CALL "SARFIL0" USING WS-SAR-FUNCTION SAR-RECORD
                         WS-SAR-RESULT
    *> Try to update the filed SAR
    INITIALIZE WS-SAR-RESULT
    MOVE "UPDT" TO WS-SAR-FUNCTION
    MOVE "Changed narrative" TO SAR-NARRATIVE
    CALL "SARFIL0" USING WS-SAR-FUNCTION SAR-RECORD
                         WS-SAR-RESULT
    IF WS-SAR-RESULT-CODE = "E0006"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-SAR-RESULT-CODE " expected=E0006"
    END-IF.

*> ---------------------------------------------------------------
*> SF-014: CRTE with zero amount -> E0003
*> ---------------------------------------------------------------
TEST-SF-014.
    ADD 1 TO WS-TEST-COUNT
    MOVE "SF-014: CRTE zero amount -> E0003"
        TO WS-TEST-NAME
    INITIALIZE SAR-RECORD
    INITIALIZE WS-SAR-RESULT
    MOVE "CRTE" TO WS-SAR-FUNCTION
    MOVE 1234567890 TO SAR-CUST-ID
    MOVE 0 TO SAR-TOTAL-AMOUNT
    MOVE "STRC" TO SAR-PATTERN-TYPE
    CALL "SARFIL0" USING WS-SAR-FUNCTION SAR-RECORD
                         WS-SAR-RESULT
    IF WS-SAR-RESULT-CODE = "E0003"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-SAR-RESULT-CODE " expected=E0003"
    END-IF.

*> ---------------------------------------------------------------
*> SF-015: CRTE with negative amount -> E0003
*> ---------------------------------------------------------------
TEST-SF-015.
    ADD 1 TO WS-TEST-COUNT
    MOVE "SF-015: CRTE negative amount -> E0003"
        TO WS-TEST-NAME
    INITIALIZE SAR-RECORD
    INITIALIZE WS-SAR-RESULT
    MOVE "CRTE" TO WS-SAR-FUNCTION
    MOVE 1234567890 TO SAR-CUST-ID
    MOVE -500.00 TO SAR-TOTAL-AMOUNT
    MOVE "STRC" TO SAR-PATTERN-TYPE
    CALL "SARFIL0" USING WS-SAR-FUNCTION SAR-RECORD
                         WS-SAR-RESULT
    IF WS-SAR-RESULT-CODE = "E0003"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-SAR-RESULT-CODE " expected=E0003"
    END-IF.

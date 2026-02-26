IDENTIFICATION DIVISION.
PROGRAM-ID. TEST-FILEIO.
*> ================================================================
*> TEST-FILEIO - Test suite for FILEIO0 Indexed File I/O Gateway
*> Tests: OPEN/CLOSE/WRITE/READ/REWRITE/DELETE, bad status,
*>        sequential browse, CIF file, audit immutability,
*>        invalid file ID (22 tests FI-001 to FI-022)
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-TEST-COUNT          PIC 9(3) VALUE 0.
01  WS-PASS-COUNT          PIC 9(3) VALUE 0.
01  WS-FAIL-COUNT          PIC 9(3) VALUE 0.
01  WS-TEST-NAME           PIC X(60).

COPY CPYFIO.
01  WS-RECORD-AREA         PIC X(512).
01  WS-RECORD-AREA2        PIC X(512).

*> Account record overlay for test data
01  WS-TEST-ACCT-REC.
    05  WS-TA-ACCT-NUMBER  PIC 9(12).
    05  WS-TA-DATA          PIC X(500).

PROCEDURE DIVISION.
MAIN-PROGRAM.
    DISPLAY "========================================".
    DISPLAY "TEST SUITE: FILEIO - File I/O Gateway".
    DISPLAY "========================================".

    PERFORM TEST-FI-001
    PERFORM TEST-FI-002
    PERFORM TEST-FI-003
    PERFORM TEST-FI-004
    PERFORM TEST-FI-005
    PERFORM TEST-FI-006
    PERFORM TEST-FI-007
    PERFORM TEST-FI-008
    PERFORM TEST-FI-009
    PERFORM TEST-FI-010
    PERFORM TEST-FI-011
    PERFORM TEST-FI-012
    PERFORM TEST-FI-013
    PERFORM TEST-FI-014
    PERFORM TEST-FI-015
    PERFORM TEST-FI-016
    PERFORM TEST-FI-017
    PERFORM TEST-FI-018
    PERFORM TEST-FI-019
    PERFORM TEST-FI-020
    PERFORM TEST-FI-021
    PERFORM TEST-FI-022

    DISPLAY "========================================".
    DISPLAY "RESULTS: " WS-PASS-COUNT "/" WS-TEST-COUNT
            " PASSED".
    DISPLAY "         " WS-FAIL-COUNT " FAILED".
    DISPLAY "========================================".
    MOVE WS-FAIL-COUNT TO RETURN-CODE
    STOP RUN.

*> ---------------------------------------------------------------
*> FI-001: OPEN account file (creates if not exists)
*> ---------------------------------------------------------------
TEST-FI-001.
    ADD 1 TO WS-TEST-COUNT
    MOVE "FI-001: OPEN ACCT file" TO WS-TEST-NAME
    INITIALIZE LS-FILE-REQUEST
    INITIALIZE LS-FILE-RESULT
    MOVE "OPEN" TO LS-FIO-FUNCTION
    MOVE "ACCT" TO LS-FIO-FILE-ID
    CALL "FILEIO0" USING LS-FILE-REQUEST WS-RECORD-AREA
                         LS-FILE-RESULT
    IF LS-FIO-RESULT-CODE = "E0000"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" LS-FIO-RESULT-CODE
            " status=" LS-FIO-STATUS
    END-IF.

*> ---------------------------------------------------------------
*> FI-002: WRITE account record
*> ---------------------------------------------------------------
TEST-FI-002.
    ADD 1 TO WS-TEST-COUNT
    MOVE "FI-002: WRITE ACCT record" TO WS-TEST-NAME
    INITIALIZE WS-RECORD-AREA
    INITIALIZE LS-FILE-REQUEST
    INITIALIZE LS-FILE-RESULT
    *> Set up test account data in record area
    MOVE 100000000001 TO WS-RECORD-AREA(1:12)
    MOVE "TEST ACCOUNT DATA" TO WS-RECORD-AREA(13:50)
    MOVE "WRIT" TO LS-FIO-FUNCTION
    MOVE "ACCT" TO LS-FIO-FILE-ID
    CALL "FILEIO0" USING LS-FILE-REQUEST WS-RECORD-AREA
                         LS-FILE-RESULT
    IF LS-FIO-RESULT-CODE = "E0000"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" LS-FIO-RESULT-CODE
            " status=" LS-FIO-STATUS
    END-IF.

*> ---------------------------------------------------------------
*> FI-003: READ account record back
*> ---------------------------------------------------------------
TEST-FI-003.
    ADD 1 TO WS-TEST-COUNT
    MOVE "FI-003: READ ACCT record" TO WS-TEST-NAME
    INITIALIZE WS-RECORD-AREA2
    INITIALIZE LS-FILE-REQUEST
    INITIALIZE LS-FILE-RESULT
    MOVE "READ" TO LS-FIO-FUNCTION
    MOVE "ACCT" TO LS-FIO-FILE-ID
    MOVE "100000000001" TO LS-FIO-KEY(1:12)
    CALL "FILEIO0" USING LS-FILE-REQUEST WS-RECORD-AREA2
                         LS-FILE-RESULT
    IF LS-FIO-RESULT-CODE = "E0000"
        IF WS-RECORD-AREA2(1:12) = "100000000001"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " key mismatch=" WS-RECORD-AREA2(1:12)
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" LS-FIO-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> FI-004: READ non-existent record -> E0004
*> ---------------------------------------------------------------
TEST-FI-004.
    ADD 1 TO WS-TEST-COUNT
    MOVE "FI-004: READ not found -> E0004" TO WS-TEST-NAME
    INITIALIZE WS-RECORD-AREA2
    INITIALIZE LS-FILE-REQUEST
    INITIALIZE LS-FILE-RESULT
    MOVE "READ" TO LS-FIO-FUNCTION
    MOVE "ACCT" TO LS-FIO-FILE-ID
    MOVE "999999999999" TO LS-FIO-KEY(1:12)
    CALL "FILEIO0" USING LS-FILE-REQUEST WS-RECORD-AREA2
                         LS-FILE-RESULT
    IF LS-FIO-RESULT-CODE = "E0004"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" LS-FIO-RESULT-CODE " expected=E0004"
    END-IF.

*> ---------------------------------------------------------------
*> FI-005: REWRITE existing record
*> ---------------------------------------------------------------
TEST-FI-005.
    ADD 1 TO WS-TEST-COUNT
    MOVE "FI-005: REWRITE ACCT record" TO WS-TEST-NAME
    *> First read to position the file
    INITIALIZE WS-RECORD-AREA
    INITIALIZE LS-FILE-REQUEST
    INITIALIZE LS-FILE-RESULT
    MOVE "READ" TO LS-FIO-FUNCTION
    MOVE "ACCT" TO LS-FIO-FILE-ID
    MOVE "100000000001" TO LS-FIO-KEY(1:12)
    CALL "FILEIO0" USING LS-FILE-REQUEST WS-RECORD-AREA
                         LS-FILE-RESULT
    *> Now rewrite with updated data
    MOVE "UPDATED ACCOUNT DATA" TO WS-RECORD-AREA(13:50)
    INITIALIZE LS-FILE-REQUEST
    INITIALIZE LS-FILE-RESULT
    MOVE "REWT" TO LS-FIO-FUNCTION
    MOVE "ACCT" TO LS-FIO-FILE-ID
    CALL "FILEIO0" USING LS-FILE-REQUEST WS-RECORD-AREA
                         LS-FILE-RESULT
    IF LS-FIO-RESULT-CODE = "E0000"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" LS-FIO-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> FI-006: READ back rewritten record and verify data
*> ---------------------------------------------------------------
TEST-FI-006.
    ADD 1 TO WS-TEST-COUNT
    MOVE "FI-006: Verify rewritten data" TO WS-TEST-NAME
    INITIALIZE WS-RECORD-AREA2
    INITIALIZE LS-FILE-REQUEST
    INITIALIZE LS-FILE-RESULT
    MOVE "READ" TO LS-FIO-FUNCTION
    MOVE "ACCT" TO LS-FIO-FILE-ID
    MOVE "100000000001" TO LS-FIO-KEY(1:12)
    CALL "FILEIO0" USING LS-FILE-REQUEST WS-RECORD-AREA2
                         LS-FILE-RESULT
    IF LS-FIO-RESULT-CODE = "E0000"
        IF WS-RECORD-AREA2(13:20) = "UPDATED ACCOUNT DATA"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " data=" WS-RECORD-AREA2(13:20)
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" LS-FIO-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> FI-007: WRITE second record for sequential browse
*> ---------------------------------------------------------------
TEST-FI-007.
    ADD 1 TO WS-TEST-COUNT
    MOVE "FI-007: WRITE second ACCT record" TO WS-TEST-NAME
    INITIALIZE WS-RECORD-AREA
    INITIALIZE LS-FILE-REQUEST
    INITIALIZE LS-FILE-RESULT
    MOVE 200000000002 TO WS-RECORD-AREA(1:12)
    MOVE "SECOND ACCOUNT" TO WS-RECORD-AREA(13:50)
    MOVE "WRIT" TO LS-FIO-FUNCTION
    MOVE "ACCT" TO LS-FIO-FILE-ID
    CALL "FILEIO0" USING LS-FILE-REQUEST WS-RECORD-AREA
                         LS-FILE-RESULT
    IF LS-FIO-RESULT-CODE = "E0000"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" LS-FIO-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> FI-008: START + READ NEXT sequential browse
*> ---------------------------------------------------------------
TEST-FI-008.
    ADD 1 TO WS-TEST-COUNT
    MOVE "FI-008: START + NEXT sequential browse"
        TO WS-TEST-NAME
    INITIALIZE LS-FILE-REQUEST
    INITIALIZE LS-FILE-RESULT
    MOVE "STRT" TO LS-FIO-FUNCTION
    MOVE "ACCT" TO LS-FIO-FILE-ID
    MOVE "000000000000" TO LS-FIO-KEY(1:12)
    CALL "FILEIO0" USING LS-FILE-REQUEST WS-RECORD-AREA
                         LS-FILE-RESULT
    IF LS-FIO-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME " start failed"
    ELSE
        *> Read first record
        INITIALIZE WS-RECORD-AREA2
        INITIALIZE LS-FILE-REQUEST
        INITIALIZE LS-FILE-RESULT
        MOVE "NEXT" TO LS-FIO-FUNCTION
        MOVE "ACCT" TO LS-FIO-FILE-ID
        CALL "FILEIO0" USING LS-FILE-REQUEST WS-RECORD-AREA2
                             LS-FILE-RESULT
        IF LS-FIO-RESULT-CODE = "E0000"
            AND WS-RECORD-AREA2(1:12) = "100000000001"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " key=" WS-RECORD-AREA2(1:12)
                " rc=" LS-FIO-RESULT-CODE
        END-IF
    END-IF.

*> ---------------------------------------------------------------
*> FI-009: READ NEXT second record
*> ---------------------------------------------------------------
TEST-FI-009.
    ADD 1 TO WS-TEST-COUNT
    MOVE "FI-009: READ NEXT second record" TO WS-TEST-NAME
    INITIALIZE WS-RECORD-AREA2
    INITIALIZE LS-FILE-REQUEST
    INITIALIZE LS-FILE-RESULT
    MOVE "NEXT" TO LS-FIO-FUNCTION
    MOVE "ACCT" TO LS-FIO-FILE-ID
    CALL "FILEIO0" USING LS-FILE-REQUEST WS-RECORD-AREA2
                         LS-FILE-RESULT
    IF LS-FIO-RESULT-CODE = "E0000"
        IF WS-RECORD-AREA2(1:12) = "200000000002"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " key=" WS-RECORD-AREA2(1:12)
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" LS-FIO-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> FI-010: READ NEXT past end -> E0004
*> ---------------------------------------------------------------
TEST-FI-010.
    ADD 1 TO WS-TEST-COUNT
    MOVE "FI-010: READ NEXT past end -> E0004" TO WS-TEST-NAME
    INITIALIZE WS-RECORD-AREA2
    INITIALIZE LS-FILE-REQUEST
    INITIALIZE LS-FILE-RESULT
    MOVE "NEXT" TO LS-FIO-FUNCTION
    MOVE "ACCT" TO LS-FIO-FILE-ID
    CALL "FILEIO0" USING LS-FILE-REQUEST WS-RECORD-AREA2
                         LS-FILE-RESULT
    IF LS-FIO-RESULT-CODE = "E0004"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" LS-FIO-RESULT-CODE " expected=E0004"
    END-IF.

*> ---------------------------------------------------------------
*> FI-011: DELETE record
*> ---------------------------------------------------------------
TEST-FI-011.
    ADD 1 TO WS-TEST-COUNT
    MOVE "FI-011: DELETE ACCT record" TO WS-TEST-NAME
    *> First read to position
    INITIALIZE LS-FILE-REQUEST
    INITIALIZE LS-FILE-RESULT
    MOVE "READ" TO LS-FIO-FUNCTION
    MOVE "ACCT" TO LS-FIO-FILE-ID
    MOVE "200000000002" TO LS-FIO-KEY(1:12)
    CALL "FILEIO0" USING LS-FILE-REQUEST WS-RECORD-AREA
                         LS-FILE-RESULT
    *> Now delete
    INITIALIZE LS-FILE-REQUEST
    INITIALIZE LS-FILE-RESULT
    MOVE "DELT" TO LS-FIO-FUNCTION
    MOVE "ACCT" TO LS-FIO-FILE-ID
    CALL "FILEIO0" USING LS-FILE-REQUEST WS-RECORD-AREA
                         LS-FILE-RESULT
    IF LS-FIO-RESULT-CODE = "E0000"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" LS-FIO-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> FI-012: CLOSE account file
*> ---------------------------------------------------------------
TEST-FI-012.
    ADD 1 TO WS-TEST-COUNT
    MOVE "FI-012: CLOSE ACCT file" TO WS-TEST-NAME
    INITIALIZE LS-FILE-REQUEST
    INITIALIZE LS-FILE-RESULT
    MOVE "CLOS" TO LS-FIO-FUNCTION
    MOVE "ACCT" TO LS-FIO-FILE-ID
    CALL "FILEIO0" USING LS-FILE-REQUEST WS-RECORD-AREA
                         LS-FILE-RESULT
    IF LS-FIO-RESULT-CODE = "E0000"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" LS-FIO-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> FI-013: OPEN CIF file (creates if not exists)
*> ---------------------------------------------------------------
TEST-FI-013.
    ADD 1 TO WS-TEST-COUNT
    MOVE "FI-013: OPEN CIF file" TO WS-TEST-NAME
    INITIALIZE LS-FILE-REQUEST
    INITIALIZE LS-FILE-RESULT
    MOVE "OPEN" TO LS-FIO-FUNCTION
    MOVE "CIF " TO LS-FIO-FILE-ID
    CALL "FILEIO0" USING LS-FILE-REQUEST WS-RECORD-AREA
                         LS-FILE-RESULT
    IF LS-FIO-RESULT-CODE = "E0000"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" LS-FIO-RESULT-CODE
            " status=" LS-FIO-STATUS
    END-IF.

*> ---------------------------------------------------------------
*> FI-014: WRITE CIF record
*> ---------------------------------------------------------------
TEST-FI-014.
    ADD 1 TO WS-TEST-COUNT
    MOVE "FI-014: WRITE CIF record" TO WS-TEST-NAME
    INITIALIZE WS-RECORD-AREA
    INITIALIZE LS-FILE-REQUEST
    INITIALIZE LS-FILE-RESULT
    *> CIF key is CIF-CUST-ID PIC 9(10) at first 10 bytes
    MOVE "0000000001" TO WS-RECORD-AREA(1:10)
    MOVE "SMITH" TO WS-RECORD-AREA(11:30)
    MOVE "JOHN" TO WS-RECORD-AREA(41:20)
    MOVE "WRIT" TO LS-FIO-FUNCTION
    MOVE "CIF " TO LS-FIO-FILE-ID
    CALL "FILEIO0" USING LS-FILE-REQUEST WS-RECORD-AREA
                         LS-FILE-RESULT
    IF LS-FIO-RESULT-CODE = "E0000"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" LS-FIO-RESULT-CODE
            " status=" LS-FIO-STATUS
    END-IF.

*> ---------------------------------------------------------------
*> FI-015: READ CIF record back and verify key
*> ---------------------------------------------------------------
TEST-FI-015.
    ADD 1 TO WS-TEST-COUNT
    MOVE "FI-015: READ CIF record back" TO WS-TEST-NAME
    INITIALIZE WS-RECORD-AREA2
    INITIALIZE LS-FILE-REQUEST
    INITIALIZE LS-FILE-RESULT
    MOVE "READ" TO LS-FIO-FUNCTION
    MOVE "CIF " TO LS-FIO-FILE-ID
    MOVE "0000000001" TO LS-FIO-KEY(1:10)
    CALL "FILEIO0" USING LS-FILE-REQUEST WS-RECORD-AREA2
                         LS-FILE-RESULT
    IF LS-FIO-RESULT-CODE = "E0000"
        IF WS-RECORD-AREA2(1:10) = "0000000001"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " key mismatch=" WS-RECORD-AREA2(1:10)
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" LS-FIO-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> FI-016: CLOSE CIF file
*> ---------------------------------------------------------------
TEST-FI-016.
    ADD 1 TO WS-TEST-COUNT
    MOVE "FI-016: CLOSE CIF file" TO WS-TEST-NAME
    INITIALIZE LS-FILE-REQUEST
    INITIALIZE LS-FILE-RESULT
    MOVE "CLOS" TO LS-FIO-FUNCTION
    MOVE "CIF " TO LS-FIO-FILE-ID
    CALL "FILEIO0" USING LS-FILE-REQUEST WS-RECORD-AREA
                         LS-FILE-RESULT
    IF LS-FIO-RESULT-CODE = "E0000"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" LS-FIO-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> FI-017: OPEN AUDT file (creates if not exists)
*> ---------------------------------------------------------------
TEST-FI-017.
    ADD 1 TO WS-TEST-COUNT
    MOVE "FI-017: OPEN AUDT file" TO WS-TEST-NAME
    INITIALIZE LS-FILE-REQUEST
    INITIALIZE LS-FILE-RESULT
    MOVE "OPEN" TO LS-FIO-FUNCTION
    MOVE "AUDT" TO LS-FIO-FILE-ID
    CALL "FILEIO0" USING LS-FILE-REQUEST WS-RECORD-AREA
                         LS-FILE-RESULT
    IF LS-FIO-RESULT-CODE = "E0000"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" LS-FIO-RESULT-CODE
            " status=" LS-FIO-STATUS
    END-IF.

*> ---------------------------------------------------------------
*> FI-018: WRITE audit record
*> ---------------------------------------------------------------
TEST-FI-018.
    ADD 1 TO WS-TEST-COUNT
    MOVE "FI-018: WRITE AUDT record" TO WS-TEST-NAME
    INITIALIZE WS-RECORD-AREA
    INITIALIZE LS-FILE-REQUEST
    INITIALIZE LS-FILE-RESULT
    *> AUDIT key is AUDIT-ID PIC 9(15) + AUDIT-SEQUENCE PIC 9(5)
    *> = 20 bytes at start of record
    MOVE "000000000000001" TO WS-RECORD-AREA(1:15)
    MOVE "00001" TO WS-RECORD-AREA(16:5)
    MOVE "TESTUSER" TO WS-RECORD-AREA(35:8)
    MOVE "WRIT" TO LS-FIO-FUNCTION
    MOVE "AUDT" TO LS-FIO-FILE-ID
    CALL "FILEIO0" USING LS-FILE-REQUEST WS-RECORD-AREA
                         LS-FILE-RESULT
    IF LS-FIO-RESULT-CODE = "E0000"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" LS-FIO-RESULT-CODE
            " status=" LS-FIO-STATUS
    END-IF.

*> ---------------------------------------------------------------
*> FI-019: REWRITE audit record -> E0005 (immutable)
*> ---------------------------------------------------------------
TEST-FI-019.
    ADD 1 TO WS-TEST-COUNT
    MOVE "FI-019: REWRITE AUDT -> E0005 immutable"
        TO WS-TEST-NAME
    *> First read to position the file
    INITIALIZE LS-FILE-REQUEST
    INITIALIZE LS-FILE-RESULT
    MOVE "READ" TO LS-FIO-FUNCTION
    MOVE "AUDT" TO LS-FIO-FILE-ID
    MOVE "000000000000001" TO LS-FIO-KEY(1:15)
    MOVE "00001" TO LS-FIO-KEY(16:5)
    CALL "FILEIO0" USING LS-FILE-REQUEST WS-RECORD-AREA
                         LS-FILE-RESULT
    *> Now attempt rewrite - should be blocked
    INITIALIZE LS-FILE-REQUEST
    INITIALIZE LS-FILE-RESULT
    MOVE "REWT" TO LS-FIO-FUNCTION
    MOVE "AUDT" TO LS-FIO-FILE-ID
    CALL "FILEIO0" USING LS-FILE-REQUEST WS-RECORD-AREA
                         LS-FILE-RESULT
    IF LS-FIO-RESULT-CODE = "E0005"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" LS-FIO-RESULT-CODE " expected=E0005"
    END-IF.

*> ---------------------------------------------------------------
*> FI-020: DELETE audit record -> E0005 (immutable)
*> ---------------------------------------------------------------
TEST-FI-020.
    ADD 1 TO WS-TEST-COUNT
    MOVE "FI-020: DELETE AUDT -> E0005 immutable"
        TO WS-TEST-NAME
    *> First read to position the file
    INITIALIZE LS-FILE-REQUEST
    INITIALIZE LS-FILE-RESULT
    MOVE "READ" TO LS-FIO-FUNCTION
    MOVE "AUDT" TO LS-FIO-FILE-ID
    MOVE "000000000000001" TO LS-FIO-KEY(1:15)
    MOVE "00001" TO LS-FIO-KEY(16:5)
    CALL "FILEIO0" USING LS-FILE-REQUEST WS-RECORD-AREA
                         LS-FILE-RESULT
    *> Now attempt delete - should be blocked
    INITIALIZE LS-FILE-REQUEST
    INITIALIZE LS-FILE-RESULT
    MOVE "DELT" TO LS-FIO-FUNCTION
    MOVE "AUDT" TO LS-FIO-FILE-ID
    CALL "FILEIO0" USING LS-FILE-REQUEST WS-RECORD-AREA
                         LS-FILE-RESULT
    IF LS-FIO-RESULT-CODE = "E0005"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" LS-FIO-RESULT-CODE " expected=E0005"
    END-IF.

*> ---------------------------------------------------------------
*> FI-021: CLOSE AUDT file
*> ---------------------------------------------------------------
TEST-FI-021.
    ADD 1 TO WS-TEST-COUNT
    MOVE "FI-021: CLOSE AUDT file" TO WS-TEST-NAME
    INITIALIZE LS-FILE-REQUEST
    INITIALIZE LS-FILE-RESULT
    MOVE "CLOS" TO LS-FIO-FUNCTION
    MOVE "AUDT" TO LS-FIO-FILE-ID
    CALL "FILEIO0" USING LS-FILE-REQUEST WS-RECORD-AREA
                         LS-FILE-RESULT
    IF LS-FIO-RESULT-CODE = "E0000"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" LS-FIO-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> FI-022: OPEN with invalid file ID -> error
*> ---------------------------------------------------------------
TEST-FI-022.
    ADD 1 TO WS-TEST-COUNT
    MOVE "FI-022: OPEN invalid file ID -> error"
        TO WS-TEST-NAME
    INITIALIZE LS-FILE-REQUEST
    INITIALIZE LS-FILE-RESULT
    MOVE "OPEN" TO LS-FIO-FUNCTION
    MOVE "XXXX" TO LS-FIO-FILE-ID
    CALL "FILEIO0" USING LS-FILE-REQUEST WS-RECORD-AREA
                         LS-FILE-RESULT
    IF LS-FIO-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
            " rc=" LS-FIO-RESULT-CODE
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected error but got E0000"
    END-IF.

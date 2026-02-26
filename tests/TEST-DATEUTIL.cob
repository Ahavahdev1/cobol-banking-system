IDENTIFICATION DIVISION.
PROGRAM-ID. TEST-DATEUTIL.
*> ================================================================
*> TEST-DATEUTIL - Test suite for DATEUTIL date utility functions
*> Tests: BDAY, WKDY, DIFF, LEAP functions (16 tests)
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-TEST-COUNT          PIC 9(3) VALUE 0.
01  WS-PASS-COUNT          PIC 9(3) VALUE 0.
01  WS-FAIL-COUNT          PIC 9(3) VALUE 0.
01  WS-TEST-NAME           PIC X(40).

*> DATEUTIL LINKAGE replicated in working storage
01  WS-FUNCTION            PIC X(4).
01  WS-DATE-INPUT.
    05  WS-DATE1           PIC 9(8).
    05  WS-DATE2           PIC 9(8).
    05  WS-DAYS-TO-ADD     PIC S9(4).
01  WS-DATE-OUTPUT.
    05  WS-RESULT-DATE     PIC 9(8).
    05  WS-RESULT-DAYS     PIC S9(8).
    05  WS-RESULT-FLAG     PIC X(1).
01  WS-DATE-RESULT.
    05  WS-DATE-RESULT-CODE PIC X(5).
    05  WS-DATE-RESULT-MSG PIC X(50).

PROCEDURE DIVISION.
MAIN-PROGRAM.
    DISPLAY "========================================"
    DISPLAY "TEST SUITE: DATEUTIL"
    DISPLAY "========================================"

    PERFORM TEST-DU-001
    PERFORM TEST-DU-002
    PERFORM TEST-DU-003
    PERFORM TEST-DU-004
    PERFORM TEST-DU-005
    PERFORM TEST-DU-006
    PERFORM TEST-DU-007
    PERFORM TEST-DU-008
    PERFORM TEST-DU-009
    PERFORM TEST-DU-010
    PERFORM TEST-DU-011
    PERFORM TEST-DU-012
    PERFORM TEST-DU-013
    PERFORM TEST-DU-014
    PERFORM TEST-DU-015
    PERFORM TEST-DU-016

    DISPLAY "========================================"
    DISPLAY "RESULTS: " WS-PASS-COUNT "/" WS-TEST-COUNT
            " PASSED"
    DISPLAY "         " WS-FAIL-COUNT " FAILED"
    DISPLAY "========================================"
    MOVE WS-FAIL-COUNT TO RETURN-CODE
    STOP RUN.

*> ---------------------------------------------------------------
*> DU-001: BDAY add 1 business day to Monday -> Tuesday
*> ---------------------------------------------------------------
TEST-DU-001.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DU-001: BDAY add 1 to Monday" TO WS-TEST-NAME
    INITIALIZE WS-DATE-INPUT
    INITIALIZE WS-DATE-OUTPUT
    INITIALIZE WS-DATE-RESULT
    MOVE "BDAY" TO WS-FUNCTION
    MOVE 20260223 TO WS-DATE1
    MOVE 1 TO WS-DAYS-TO-ADD
    CALL "DATEUTIL" USING WS-FUNCTION WS-DATE-INPUT
                          WS-DATE-OUTPUT WS-DATE-RESULT
    IF WS-DATE-RESULT-CODE = "E0000"
        IF WS-RESULT-DATE = 20260224
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=20260224 actual=" WS-RESULT-DATE
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-DATE-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> DU-002: BDAY add 1 biz day to Friday -> Monday (skip weekend)
*> ---------------------------------------------------------------
TEST-DU-002.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DU-002: BDAY add 1 to Friday" TO WS-TEST-NAME
    INITIALIZE WS-DATE-INPUT
    INITIALIZE WS-DATE-OUTPUT
    INITIALIZE WS-DATE-RESULT
    MOVE "BDAY" TO WS-FUNCTION
    MOVE 20260220 TO WS-DATE1
    MOVE 1 TO WS-DAYS-TO-ADD
    CALL "DATEUTIL" USING WS-FUNCTION WS-DATE-INPUT
                          WS-DATE-OUTPUT WS-DATE-RESULT
    IF WS-DATE-RESULT-CODE = "E0000"
        IF WS-RESULT-DATE = 20260223
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=20260223 actual=" WS-RESULT-DATE
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-DATE-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> DU-003: BDAY add 5 biz days to Monday -> next Monday
*> ---------------------------------------------------------------
TEST-DU-003.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DU-003: BDAY add 5 to Monday" TO WS-TEST-NAME
    INITIALIZE WS-DATE-INPUT
    INITIALIZE WS-DATE-OUTPUT
    INITIALIZE WS-DATE-RESULT
    MOVE "BDAY" TO WS-FUNCTION
    MOVE 20260223 TO WS-DATE1
    MOVE 5 TO WS-DAYS-TO-ADD
    CALL "DATEUTIL" USING WS-FUNCTION WS-DATE-INPUT
                          WS-DATE-OUTPUT WS-DATE-RESULT
    IF WS-DATE-RESULT-CODE = "E0000"
        IF WS-RESULT-DATE = 20260302
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=20260302 actual=" WS-RESULT-DATE
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-DATE-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> DU-004: BDAY add 0 biz days -> same date
*> ---------------------------------------------------------------
TEST-DU-004.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DU-004: BDAY add 0 days" TO WS-TEST-NAME
    INITIALIZE WS-DATE-INPUT
    INITIALIZE WS-DATE-OUTPUT
    INITIALIZE WS-DATE-RESULT
    MOVE "BDAY" TO WS-FUNCTION
    MOVE 20260223 TO WS-DATE1
    MOVE 0 TO WS-DAYS-TO-ADD
    CALL "DATEUTIL" USING WS-FUNCTION WS-DATE-INPUT
                          WS-DATE-OUTPUT WS-DATE-RESULT
    IF WS-DATE-RESULT-CODE = "E0000"
        IF WS-RESULT-DATE = 20260223
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=20260223 actual=" WS-RESULT-DATE
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-DATE-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> DU-005: WKDY Monday -> Y (is weekday)
*> ---------------------------------------------------------------
TEST-DU-005.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DU-005: WKDY Monday=Y" TO WS-TEST-NAME
    INITIALIZE WS-DATE-INPUT
    INITIALIZE WS-DATE-OUTPUT
    INITIALIZE WS-DATE-RESULT
    MOVE "WKDY" TO WS-FUNCTION
    MOVE 20260223 TO WS-DATE1
    CALL "DATEUTIL" USING WS-FUNCTION WS-DATE-INPUT
                          WS-DATE-OUTPUT WS-DATE-RESULT
    IF WS-DATE-RESULT-CODE = "E0000"
        IF WS-RESULT-FLAG = "Y"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=Y actual=" WS-RESULT-FLAG
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-DATE-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> DU-006: WKDY Saturday -> N
*> ---------------------------------------------------------------
TEST-DU-006.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DU-006: WKDY Saturday=N" TO WS-TEST-NAME
    INITIALIZE WS-DATE-INPUT
    INITIALIZE WS-DATE-OUTPUT
    INITIALIZE WS-DATE-RESULT
    MOVE "WKDY" TO WS-FUNCTION
    MOVE 20260221 TO WS-DATE1
    CALL "DATEUTIL" USING WS-FUNCTION WS-DATE-INPUT
                          WS-DATE-OUTPUT WS-DATE-RESULT
    IF WS-DATE-RESULT-CODE = "E0000"
        IF WS-RESULT-FLAG = "N"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=N actual=" WS-RESULT-FLAG
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-DATE-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> DU-007: WKDY Sunday -> N
*> ---------------------------------------------------------------
TEST-DU-007.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DU-007: WKDY Sunday=N" TO WS-TEST-NAME
    INITIALIZE WS-DATE-INPUT
    INITIALIZE WS-DATE-OUTPUT
    INITIALIZE WS-DATE-RESULT
    MOVE "WKDY" TO WS-FUNCTION
    MOVE 20260222 TO WS-DATE1
    CALL "DATEUTIL" USING WS-FUNCTION WS-DATE-INPUT
                          WS-DATE-OUTPUT WS-DATE-RESULT
    IF WS-DATE-RESULT-CODE = "E0000"
        IF WS-RESULT-FLAG = "N"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=N actual=" WS-RESULT-FLAG
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-DATE-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> DU-008: DIFF same month (20260201 to 20260215) -> 14 days
*> ---------------------------------------------------------------
TEST-DU-008.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DU-008: DIFF same month=14" TO WS-TEST-NAME
    INITIALIZE WS-DATE-INPUT
    INITIALIZE WS-DATE-OUTPUT
    INITIALIZE WS-DATE-RESULT
    MOVE "DIFF" TO WS-FUNCTION
    MOVE 20260201 TO WS-DATE1
    MOVE 20260215 TO WS-DATE2
    CALL "DATEUTIL" USING WS-FUNCTION WS-DATE-INPUT
                          WS-DATE-OUTPUT WS-DATE-RESULT
    IF WS-DATE-RESULT-CODE = "E0000"
        IF WS-RESULT-DAYS = 14
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=14 actual=" WS-RESULT-DAYS
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-DATE-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> DU-009: DIFF cross month (20260115 to 20260215) -> 31 days
*> ---------------------------------------------------------------
TEST-DU-009.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DU-009: DIFF cross month=31" TO WS-TEST-NAME
    INITIALIZE WS-DATE-INPUT
    INITIALIZE WS-DATE-OUTPUT
    INITIALIZE WS-DATE-RESULT
    MOVE "DIFF" TO WS-FUNCTION
    MOVE 20260115 TO WS-DATE1
    MOVE 20260215 TO WS-DATE2
    CALL "DATEUTIL" USING WS-FUNCTION WS-DATE-INPUT
                          WS-DATE-OUTPUT WS-DATE-RESULT
    IF WS-DATE-RESULT-CODE = "E0000"
        IF WS-RESULT-DAYS = 31
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=31 actual=" WS-RESULT-DAYS
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-DATE-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> DU-010: DIFF cross year (20251215 to 20260115) -> 31 days
*> ---------------------------------------------------------------
TEST-DU-010.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DU-010: DIFF cross year=31" TO WS-TEST-NAME
    INITIALIZE WS-DATE-INPUT
    INITIALIZE WS-DATE-OUTPUT
    INITIALIZE WS-DATE-RESULT
    MOVE "DIFF" TO WS-FUNCTION
    MOVE 20251215 TO WS-DATE1
    MOVE 20260115 TO WS-DATE2
    CALL "DATEUTIL" USING WS-FUNCTION WS-DATE-INPUT
                          WS-DATE-OUTPUT WS-DATE-RESULT
    IF WS-DATE-RESULT-CODE = "E0000"
        IF WS-RESULT-DAYS = 31
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=31 actual=" WS-RESULT-DAYS
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-DATE-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> DU-011: DIFF same date -> 0 days
*> ---------------------------------------------------------------
TEST-DU-011.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DU-011: DIFF same date=0" TO WS-TEST-NAME
    INITIALIZE WS-DATE-INPUT
    INITIALIZE WS-DATE-OUTPUT
    INITIALIZE WS-DATE-RESULT
    MOVE "DIFF" TO WS-FUNCTION
    MOVE 20260215 TO WS-DATE1
    MOVE 20260215 TO WS-DATE2
    CALL "DATEUTIL" USING WS-FUNCTION WS-DATE-INPUT
                          WS-DATE-OUTPUT WS-DATE-RESULT
    IF WS-DATE-RESULT-CODE = "E0000"
        IF WS-RESULT-DAYS = 0
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=0 actual=" WS-RESULT-DAYS
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-DATE-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> DU-012: LEAP 2024 -> Y (divisible by 4)
*> ---------------------------------------------------------------
TEST-DU-012.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DU-012: LEAP 2024=Y" TO WS-TEST-NAME
    INITIALIZE WS-DATE-INPUT
    INITIALIZE WS-DATE-OUTPUT
    INITIALIZE WS-DATE-RESULT
    MOVE "LEAP" TO WS-FUNCTION
    MOVE 20240101 TO WS-DATE1
    CALL "DATEUTIL" USING WS-FUNCTION WS-DATE-INPUT
                          WS-DATE-OUTPUT WS-DATE-RESULT
    IF WS-DATE-RESULT-CODE = "E0000"
        IF WS-RESULT-FLAG = "Y"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=Y actual=" WS-RESULT-FLAG
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-DATE-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> DU-013: LEAP 2025 -> N
*> ---------------------------------------------------------------
TEST-DU-013.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DU-013: LEAP 2025=N" TO WS-TEST-NAME
    INITIALIZE WS-DATE-INPUT
    INITIALIZE WS-DATE-OUTPUT
    INITIALIZE WS-DATE-RESULT
    MOVE "LEAP" TO WS-FUNCTION
    MOVE 20250101 TO WS-DATE1
    CALL "DATEUTIL" USING WS-FUNCTION WS-DATE-INPUT
                          WS-DATE-OUTPUT WS-DATE-RESULT
    IF WS-DATE-RESULT-CODE = "E0000"
        IF WS-RESULT-FLAG = "N"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=N actual=" WS-RESULT-FLAG
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-DATE-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> DU-014: LEAP 2000 -> Y (divisible by 400)
*> ---------------------------------------------------------------
TEST-DU-014.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DU-014: LEAP 2000=Y" TO WS-TEST-NAME
    INITIALIZE WS-DATE-INPUT
    INITIALIZE WS-DATE-OUTPUT
    INITIALIZE WS-DATE-RESULT
    MOVE "LEAP" TO WS-FUNCTION
    MOVE 20000101 TO WS-DATE1
    CALL "DATEUTIL" USING WS-FUNCTION WS-DATE-INPUT
                          WS-DATE-OUTPUT WS-DATE-RESULT
    IF WS-DATE-RESULT-CODE = "E0000"
        IF WS-RESULT-FLAG = "Y"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=Y actual=" WS-RESULT-FLAG
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-DATE-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> DU-015: LEAP 1900 -> N (divisible by 100 but not 400)
*> ---------------------------------------------------------------
TEST-DU-015.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DU-015: LEAP 1900=N" TO WS-TEST-NAME
    INITIALIZE WS-DATE-INPUT
    INITIALIZE WS-DATE-OUTPUT
    INITIALIZE WS-DATE-RESULT
    MOVE "LEAP" TO WS-FUNCTION
    MOVE 19000101 TO WS-DATE1
    CALL "DATEUTIL" USING WS-FUNCTION WS-DATE-INPUT
                          WS-DATE-OUTPUT WS-DATE-RESULT
    IF WS-DATE-RESULT-CODE = "E0000"
        IF WS-RESULT-FLAG = "N"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=N actual=" WS-RESULT-FLAG
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-DATE-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> DU-016: BDAY add 1 biz day to Saturday -> Monday
*> ---------------------------------------------------------------
TEST-DU-016.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DU-016: BDAY Sat+1=Monday" TO WS-TEST-NAME
    INITIALIZE WS-DATE-INPUT
    INITIALIZE WS-DATE-OUTPUT
    INITIALIZE WS-DATE-RESULT
    MOVE "BDAY" TO WS-FUNCTION
    MOVE 20260221 TO WS-DATE1
    MOVE 1 TO WS-DAYS-TO-ADD
    CALL "DATEUTIL" USING WS-FUNCTION WS-DATE-INPUT
                          WS-DATE-OUTPUT WS-DATE-RESULT
    IF WS-DATE-RESULT-CODE = "E0000"
        IF WS-RESULT-DATE = 20260223
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=20260223 actual=" WS-RESULT-DATE
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-DATE-RESULT-CODE
    END-IF.

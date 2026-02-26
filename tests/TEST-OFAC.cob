IDENTIFICATION DIVISION.
PROGRAM-ID. TEST-OFAC.
*> ================================================================
*> TEST-OFAC - Test suite for OFAC Screening Module
*> Tests: CHKN, CHKB, STAT functions (8 tests)
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-TEST-COUNT          PIC 9(3) VALUE 0.
01  WS-PASS-COUNT          PIC 9(3) VALUE 0.
01  WS-FAIL-COUNT          PIC 9(3) VALUE 0.
01  WS-TEST-NAME           PIC X(60).

*> OFAC function code
01  WS-OFAC-FUNCTION       PIC X(4).

*> OFAC check record from copybook
COPY CPYOFAC REPLACING ==OFAC-CHECK-RECORD==
    BY ==WS-OFAC-CHECK-RECORD==.

*> OFAC result
01  WS-OFAC-RESULT.
    05  WS-OFAC-RESULT-CODE    PIC X(5).
    05  WS-OFAC-RESULT-MSG     PIC X(50).

PROCEDURE DIVISION.
MAIN-PROGRAM.
    DISPLAY "========================================".
    DISPLAY "TEST SUITE: OFAC SCREENING".
    DISPLAY "========================================".

    PERFORM TEST-OF-001
    PERFORM TEST-OF-002
    PERFORM TEST-OF-003
    PERFORM TEST-OF-004
    PERFORM TEST-OF-005
    PERFORM TEST-OF-006
    PERFORM TEST-OF-007
    PERFORM TEST-OF-008

    DISPLAY "========================================".
    DISPLAY "RESULTS: " WS-PASS-COUNT "/" WS-TEST-COUNT
            " PASSED".
    DISPLAY "         " WS-FAIL-COUNT " FAILED".
    DISPLAY "========================================".
    MOVE WS-FAIL-COUNT TO RETURN-CODE
    STOP RUN.

*> ---------------------------------------------------------------
*> OF-001: CHKN with safe name -> E0000, no match
*> ---------------------------------------------------------------
TEST-OF-001.
    ADD 1 TO WS-TEST-COUNT
    MOVE "OF-001: CHKN safe name -> E0000 no match"
        TO WS-TEST-NAME
    INITIALIZE WS-OFAC-CHECK-RECORD
    INITIALIZE WS-OFAC-RESULT
    MOVE "CHKN" TO WS-OFAC-FUNCTION
    MOVE "JOHN DOE" TO OFAC-CHECK-NAME
        OF WS-OFAC-CHECK-RECORD
    MOVE "US " TO OFAC-CHECK-COUNTRY
        OF WS-OFAC-CHECK-RECORD
    MOVE "C" TO OFAC-CHECK-TYPE OF WS-OFAC-CHECK-RECORD
    CALL "OFACCHK0" USING WS-OFAC-FUNCTION
                          WS-OFAC-CHECK-RECORD
                          WS-OFAC-RESULT
    IF WS-OFAC-RESULT-CODE = "E0000"
        IF OFAC-MATCH-FOUND OF WS-OFAC-CHECK-RECORD = "N"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " match=" OFAC-MATCH-FOUND
                    OF WS-OFAC-CHECK-RECORD
                " expected=N"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-OFAC-RESULT-CODE
            " expected=E0000"
    END-IF.

*> ---------------------------------------------------------------
*> OF-002: CHKN with "OFAC-TEST-MATCH" -> E0025, match found
*> ---------------------------------------------------------------
TEST-OF-002.
    ADD 1 TO WS-TEST-COUNT
    MOVE "OF-002: CHKN OFAC-TEST-MATCH -> E0025 match"
        TO WS-TEST-NAME
    INITIALIZE WS-OFAC-CHECK-RECORD
    INITIALIZE WS-OFAC-RESULT
    MOVE "CHKN" TO WS-OFAC-FUNCTION
    MOVE "OFAC-TEST-MATCH" TO OFAC-CHECK-NAME
        OF WS-OFAC-CHECK-RECORD
    MOVE "US " TO OFAC-CHECK-COUNTRY
        OF WS-OFAC-CHECK-RECORD
    MOVE "C" TO OFAC-CHECK-TYPE OF WS-OFAC-CHECK-RECORD
    CALL "OFACCHK0" USING WS-OFAC-FUNCTION
                          WS-OFAC-CHECK-RECORD
                          WS-OFAC-RESULT
    IF WS-OFAC-RESULT-CODE = "E0025"
        IF OFAC-MATCH-FOUND OF WS-OFAC-CHECK-RECORD = "Y"
            AND OFAC-MATCH-SCORE OF WS-OFAC-CHECK-RECORD = 100
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " match=" OFAC-MATCH-FOUND
                    OF WS-OFAC-CHECK-RECORD
                " score=" OFAC-MATCH-SCORE
                    OF WS-OFAC-CHECK-RECORD
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-OFAC-RESULT-CODE
            " expected=E0025"
    END-IF.

*> ---------------------------------------------------------------
*> OF-003: CHKB with safe beneficiary, safe country -> E0000
*> ---------------------------------------------------------------
TEST-OF-003.
    ADD 1 TO WS-TEST-COUNT
    MOVE "OF-003: CHKB safe bene+country -> E0000"
        TO WS-TEST-NAME
    INITIALIZE WS-OFAC-CHECK-RECORD
    INITIALIZE WS-OFAC-RESULT
    MOVE "CHKB" TO WS-OFAC-FUNCTION
    MOVE "ACME CORPORATION" TO OFAC-CHECK-NAME
        OF WS-OFAC-CHECK-RECORD
    MOVE "US " TO OFAC-CHECK-COUNTRY
        OF WS-OFAC-CHECK-RECORD
    MOVE "B" TO OFAC-CHECK-TYPE OF WS-OFAC-CHECK-RECORD
    CALL "OFACCHK0" USING WS-OFAC-FUNCTION
                          WS-OFAC-CHECK-RECORD
                          WS-OFAC-RESULT
    IF WS-OFAC-RESULT-CODE = "E0000"
        IF OFAC-MATCH-FOUND OF WS-OFAC-CHECK-RECORD = "N"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " match=" OFAC-MATCH-FOUND
                    OF WS-OFAC-CHECK-RECORD
                " expected=N"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-OFAC-RESULT-CODE
            " expected=E0000"
    END-IF.

*> ---------------------------------------------------------------
*> OF-004: CHKB with sanctioned country "KP" -> E0025
*> ---------------------------------------------------------------
TEST-OF-004.
    ADD 1 TO WS-TEST-COUNT
    MOVE "OF-004: CHKB sanctioned country KP -> E0025"
        TO WS-TEST-NAME
    INITIALIZE WS-OFAC-CHECK-RECORD
    INITIALIZE WS-OFAC-RESULT
    MOVE "CHKB" TO WS-OFAC-FUNCTION
    MOVE "SAFE COMPANY NAME" TO OFAC-CHECK-NAME
        OF WS-OFAC-CHECK-RECORD
    MOVE "KP " TO OFAC-CHECK-COUNTRY
        OF WS-OFAC-CHECK-RECORD
    MOVE "B" TO OFAC-CHECK-TYPE OF WS-OFAC-CHECK-RECORD
    CALL "OFACCHK0" USING WS-OFAC-FUNCTION
                          WS-OFAC-CHECK-RECORD
                          WS-OFAC-RESULT
    IF WS-OFAC-RESULT-CODE = "E0025"
        IF OFAC-MATCH-FOUND OF WS-OFAC-CHECK-RECORD = "Y"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " match=" OFAC-MATCH-FOUND
                    OF WS-OFAC-CHECK-RECORD
                " expected=Y"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-OFAC-RESULT-CODE
            " expected=E0025"
    END-IF.

*> ---------------------------------------------------------------
*> OF-005: CHKB with sanctioned country "IR" -> E0025
*> ---------------------------------------------------------------
TEST-OF-005.
    ADD 1 TO WS-TEST-COUNT
    MOVE "OF-005: CHKB sanctioned country IR -> E0025"
        TO WS-TEST-NAME
    INITIALIZE WS-OFAC-CHECK-RECORD
    INITIALIZE WS-OFAC-RESULT
    MOVE "CHKB" TO WS-OFAC-FUNCTION
    MOVE "ANOTHER SAFE NAME" TO OFAC-CHECK-NAME
        OF WS-OFAC-CHECK-RECORD
    MOVE "IR " TO OFAC-CHECK-COUNTRY
        OF WS-OFAC-CHECK-RECORD
    MOVE "B" TO OFAC-CHECK-TYPE OF WS-OFAC-CHECK-RECORD
    CALL "OFACCHK0" USING WS-OFAC-FUNCTION
                          WS-OFAC-CHECK-RECORD
                          WS-OFAC-RESULT
    IF WS-OFAC-RESULT-CODE = "E0025"
        IF OFAC-MATCH-FOUND OF WS-OFAC-CHECK-RECORD = "Y"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " match=" OFAC-MATCH-FOUND
                    OF WS-OFAC-CHECK-RECORD
                " expected=Y"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-OFAC-RESULT-CODE
            " expected=E0025"
    END-IF.

*> ---------------------------------------------------------------
*> OF-006: CHKB with "OFAC-TEST-MATCH" as beneficiary -> E0025
*> ---------------------------------------------------------------
TEST-OF-006.
    ADD 1 TO WS-TEST-COUNT
    MOVE "OF-006: CHKB OFAC-TEST-MATCH bene -> E0025"
        TO WS-TEST-NAME
    INITIALIZE WS-OFAC-CHECK-RECORD
    INITIALIZE WS-OFAC-RESULT
    MOVE "CHKB" TO WS-OFAC-FUNCTION
    MOVE "OFAC-TEST-MATCH" TO OFAC-CHECK-NAME
        OF WS-OFAC-CHECK-RECORD
    MOVE "US " TO OFAC-CHECK-COUNTRY
        OF WS-OFAC-CHECK-RECORD
    MOVE "B" TO OFAC-CHECK-TYPE OF WS-OFAC-CHECK-RECORD
    CALL "OFACCHK0" USING WS-OFAC-FUNCTION
                          WS-OFAC-CHECK-RECORD
                          WS-OFAC-RESULT
    IF WS-OFAC-RESULT-CODE = "E0025"
        IF OFAC-MATCH-FOUND OF WS-OFAC-CHECK-RECORD = "Y"
            AND OFAC-MATCH-SCORE OF WS-OFAC-CHECK-RECORD
                = 100
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " match=" OFAC-MATCH-FOUND
                    OF WS-OFAC-CHECK-RECORD
                " score=" OFAC-MATCH-SCORE
                    OF WS-OFAC-CHECK-RECORD
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-OFAC-RESULT-CODE
            " expected=E0025"
    END-IF.

*> ---------------------------------------------------------------
*> OF-007: STAT function returns check date
*> ---------------------------------------------------------------
TEST-OF-007.
    ADD 1 TO WS-TEST-COUNT
    MOVE "OF-007: STAT returns check date -> E0000"
        TO WS-TEST-NAME
    INITIALIZE WS-OFAC-CHECK-RECORD
    INITIALIZE WS-OFAC-RESULT
    MOVE "STAT" TO WS-OFAC-FUNCTION
    MOVE 20260215 TO OFAC-CHECK-DATE
        OF WS-OFAC-CHECK-RECORD
    MOVE "P" TO OFAC-CHECK-STATUS OF WS-OFAC-CHECK-RECORD
    CALL "OFACCHK0" USING WS-OFAC-FUNCTION
                          WS-OFAC-CHECK-RECORD
                          WS-OFAC-RESULT
    IF WS-OFAC-RESULT-CODE = "E0000"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-OFAC-RESULT-CODE
            " expected=E0000"
    END-IF.

*> ---------------------------------------------------------------
*> OF-008: Invalid function -> E0001
*> ---------------------------------------------------------------
TEST-OF-008.
    ADD 1 TO WS-TEST-COUNT
    MOVE "OF-008: Invalid function -> E0001"
        TO WS-TEST-NAME
    INITIALIZE WS-OFAC-CHECK-RECORD
    INITIALIZE WS-OFAC-RESULT
    MOVE "XXXX" TO WS-OFAC-FUNCTION
    CALL "OFACCHK0" USING WS-OFAC-FUNCTION
                          WS-OFAC-CHECK-RECORD
                          WS-OFAC-RESULT
    IF WS-OFAC-RESULT-CODE = "E0001"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-OFAC-RESULT-CODE
            " expected=E0001"
    END-IF.

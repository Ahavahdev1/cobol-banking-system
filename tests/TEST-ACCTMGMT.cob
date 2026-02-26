IDENTIFICATION DIVISION.
PROGRAM-ID. TEST-ACCTMGMT.
*> ================================================================
*> TEST-ACCTMGMT - Test suite for ACCTMGMT account management
*> Tests: OPEN, CLOS, CHKD functions (16 tests)
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-TEST-COUNT          PIC 9(3) VALUE 0.
01  WS-PASS-COUNT          PIC 9(3) VALUE 0.
01  WS-FAIL-COUNT          PIC 9(3) VALUE 0.
01  WS-TEST-NAME           PIC X(40).

*> ACCTMGMT LINKAGE replicated in working storage
01  WS-FUNCTION            PIC X(4).
COPY CPYACCT.
COPY CPYCIF.
01  WS-ACCT-RESULT.
    05  WS-ACCT-RESULT-CODE PIC X(5).
    05  WS-ACCT-RESULT-MSG  PIC X(50).

PROCEDURE DIVISION.
MAIN-PROGRAM.
    DISPLAY "========================================"
    DISPLAY "TEST SUITE: ACCTMGMT"
    DISPLAY "========================================"

    PERFORM TEST-AM-001
    PERFORM TEST-AM-002
    PERFORM TEST-AM-003
    PERFORM TEST-AM-004
    PERFORM TEST-AM-005
    PERFORM TEST-AM-006
    PERFORM TEST-AM-007
    PERFORM TEST-AM-008
    PERFORM TEST-AM-009
    PERFORM TEST-AM-010
    PERFORM TEST-AM-011
    PERFORM TEST-AM-012
    PERFORM TEST-AM-013
    PERFORM TEST-AM-014
    PERFORM TEST-AM-015
    PERFORM TEST-AM-016

    DISPLAY "========================================"
    DISPLAY "RESULTS: " WS-PASS-COUNT "/" WS-TEST-COUNT
            " PASSED"
    DISPLAY "         " WS-FAIL-COUNT " FAILED"
    DISPLAY "========================================"
    MOVE WS-FAIL-COUNT TO RETURN-CODE
    STOP RUN.

*> ---------------------------------------------------------------
*> Helper: Set up a valid verified CIF
*> ---------------------------------------------------------------
SETUP-VALID-CIF.
    INITIALIZE CIF-RECORD
    MOVE 1000000001 TO CIF-CUST-ID
    MOVE "DOE" TO CIF-NAME-LAST
    MOVE "JOHN" TO CIF-NAME-FIRST
    MOVE 123456789 TO CIF-SSN-TIN
    MOVE "S" TO CIF-SSN-TYPE
    MOVE 19800115 TO CIF-DOB
    MOVE "I" TO CIF-CUST-TYPE
    MOVE "Y" TO CIF-CIP-VERIFIED
    MOVE 20260101 TO CIF-CIP-VERIFY-DATE
    MOVE "DL" TO CIF-CIP-DOC-TYPE
    MOVE "C" TO CIF-OFAC-STATUS
    MOVE 1 TO CIF-BSA-RISK-RATING
    MOVE "A" TO CIF-STATUS.

*> ---------------------------------------------------------------
*> AM-001: OPEN checking with verified CIF -> E0000
*> ---------------------------------------------------------------
TEST-AM-001.
    ADD 1 TO WS-TEST-COUNT
    MOVE "AM-001: OPEN checking=E0000" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-ACCT-RESULT
    PERFORM SETUP-VALID-CIF
    MOVE "OPEN" TO WS-FUNCTION
    MOVE "DDA1" TO ACCT-PRODUCT-CODE
    MOVE "D" TO ACCT-TYPE
    MOVE "CH" TO ACCT-SUB-TYPE
    MOVE 1000000001 TO ACCT-PRIMARY-CIF
    CALL "ACCTMGMT" USING WS-FUNCTION ACCT-RECORD
                          CIF-RECORD WS-ACCT-RESULT
    IF WS-ACCT-RESULT-CODE = "E0000"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-ACCT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> AM-002: OPEN savings with verified CIF -> E0000
*> ---------------------------------------------------------------
TEST-AM-002.
    ADD 1 TO WS-TEST-COUNT
    MOVE "AM-002: OPEN savings=E0000" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-ACCT-RESULT
    PERFORM SETUP-VALID-CIF
    MOVE "OPEN" TO WS-FUNCTION
    MOVE "SAV1" TO ACCT-PRODUCT-CODE
    MOVE "D" TO ACCT-TYPE
    MOVE "SV" TO ACCT-SUB-TYPE
    MOVE 1000000001 TO ACCT-PRIMARY-CIF
    CALL "ACCTMGMT" USING WS-FUNCTION ACCT-RECORD
                          CIF-RECORD WS-ACCT-RESULT
    IF WS-ACCT-RESULT-CODE = "E0000"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-ACCT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> AM-003: OPEN with CIF not verified -> E0021
*> ---------------------------------------------------------------
TEST-AM-003.
    ADD 1 TO WS-TEST-COUNT
    MOVE "AM-003: OPEN unverif CIF=E0021" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-ACCT-RESULT
    PERFORM SETUP-VALID-CIF
    MOVE "N" TO CIF-CIP-VERIFIED
    MOVE "OPEN" TO WS-FUNCTION
    MOVE "DDA1" TO ACCT-PRODUCT-CODE
    MOVE "D" TO ACCT-TYPE
    MOVE "CH" TO ACCT-SUB-TYPE
    MOVE 1000000001 TO ACCT-PRIMARY-CIF
    CALL "ACCTMGMT" USING WS-FUNCTION ACCT-RECORD
                          CIF-RECORD WS-ACCT-RESULT
    IF WS-ACCT-RESULT-CODE = "E0021"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0021 actual=" WS-ACCT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> AM-004: CLOS with zero balance, no holds -> E0000
*> ---------------------------------------------------------------
TEST-AM-004.
    ADD 1 TO WS-TEST-COUNT
    MOVE "AM-004: CLOS zero bal=E0000" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-ACCT-RESULT
    PERFORM SETUP-VALID-CIF
    MOVE "CLOS" TO WS-FUNCTION
    MOVE "A" TO ACCT-STATUS
    MOVE 0 TO ACCT-LEDGER-BAL
    MOVE 0 TO ACCT-HOLD-AMOUNT
    MOVE "DDA1" TO ACCT-PRODUCT-CODE
    MOVE "D" TO ACCT-TYPE
    MOVE "CH" TO ACCT-SUB-TYPE
    CALL "ACCTMGMT" USING WS-FUNCTION ACCT-RECORD
                          CIF-RECORD WS-ACCT-RESULT
    IF WS-ACCT-RESULT-CODE = "E0000"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-ACCT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> AM-005: CLOS with positive balance ($100) -> E0016
*> ---------------------------------------------------------------
TEST-AM-005.
    ADD 1 TO WS-TEST-COUNT
    MOVE "AM-005: CLOS pos balance=E0016" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-ACCT-RESULT
    PERFORM SETUP-VALID-CIF
    MOVE "CLOS" TO WS-FUNCTION
    MOVE "A" TO ACCT-STATUS
    MOVE 100.00 TO ACCT-LEDGER-BAL
    MOVE 0 TO ACCT-HOLD-AMOUNT
    CALL "ACCTMGMT" USING WS-FUNCTION ACCT-RECORD
                          CIF-RECORD WS-ACCT-RESULT
    IF WS-ACCT-RESULT-CODE = "E0016"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0016 actual=" WS-ACCT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> AM-006: CLOS with active holds -> E0017
*> ---------------------------------------------------------------
TEST-AM-006.
    ADD 1 TO WS-TEST-COUNT
    MOVE "AM-006: CLOS with holds=E0017" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-ACCT-RESULT
    PERFORM SETUP-VALID-CIF
    MOVE "CLOS" TO WS-FUNCTION
    MOVE "A" TO ACCT-STATUS
    MOVE 0 TO ACCT-LEDGER-BAL
    MOVE 50.00 TO ACCT-HOLD-AMOUNT
    CALL "ACCTMGMT" USING WS-FUNCTION ACCT-RECORD
                          CIF-RECORD WS-ACCT-RESULT
    IF WS-ACCT-RESULT-CODE = "E0017"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0017 actual=" WS-ACCT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> AM-007: CHKD calc Luhn for 000012345678 -> digit=2
*> ---------------------------------------------------------------
TEST-AM-007.
    ADD 1 TO WS-TEST-COUNT
    MOVE "AM-007: CHKD Luhn calc=2" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-ACCT-RESULT
    PERFORM SETUP-VALID-CIF
    MOVE "CHKD" TO WS-FUNCTION
    MOVE 000012345678 TO ACCT-NUMBER
    MOVE 0 TO ACCT-CHECK-DIGIT
    CALL "ACCTMGMT" USING WS-FUNCTION ACCT-RECORD
                          CIF-RECORD WS-ACCT-RESULT
    IF WS-ACCT-RESULT-CODE = "E0000"
        IF ACCT-CHECK-DIGIT = 2
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=2 actual=" ACCT-CHECK-DIGIT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-ACCT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> AM-008: CHKD validate correct check digit -> E0000
*> ---------------------------------------------------------------
TEST-AM-008.
    ADD 1 TO WS-TEST-COUNT
    MOVE "AM-008: CHKD valid digit=E0000" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-ACCT-RESULT
    PERFORM SETUP-VALID-CIF
    MOVE "CHKD" TO WS-FUNCTION
    MOVE 000012345678 TO ACCT-NUMBER
    MOVE 2 TO ACCT-CHECK-DIGIT
    CALL "ACCTMGMT" USING WS-FUNCTION ACCT-RECORD
                          CIF-RECORD WS-ACCT-RESULT
    IF WS-ACCT-RESULT-CODE = "E0000"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0000 actual=" WS-ACCT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> AM-009: CHKD validate wrong check digit -> E0019
*> ---------------------------------------------------------------
TEST-AM-009.
    ADD 1 TO WS-TEST-COUNT
    MOVE "AM-009: CHKD wrong digit=E0019" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-ACCT-RESULT
    PERFORM SETUP-VALID-CIF
    MOVE "CHKD" TO WS-FUNCTION
    MOVE 000012345678 TO ACCT-NUMBER
    MOVE 5 TO ACCT-CHECK-DIGIT
    CALL "ACCTMGMT" USING WS-FUNCTION ACCT-RECORD
                          CIF-RECORD WS-ACCT-RESULT
    IF WS-ACCT-RESULT-CODE = "E0019"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0019 actual=" WS-ACCT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> AM-010: OPEN sets ACCT-STATUS to "A"
*> ---------------------------------------------------------------
TEST-AM-010.
    ADD 1 TO WS-TEST-COUNT
    MOVE "AM-010: OPEN sets status=A" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-ACCT-RESULT
    PERFORM SETUP-VALID-CIF
    MOVE "OPEN" TO WS-FUNCTION
    MOVE "DDA1" TO ACCT-PRODUCT-CODE
    MOVE "D" TO ACCT-TYPE
    MOVE "CH" TO ACCT-SUB-TYPE
    MOVE 1000000001 TO ACCT-PRIMARY-CIF
    CALL "ACCTMGMT" USING WS-FUNCTION ACCT-RECORD
                          CIF-RECORD WS-ACCT-RESULT
    IF WS-ACCT-RESULT-CODE = "E0000"
        IF ACCT-STATUS = "A"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " status=" ACCT-STATUS
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-ACCT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> AM-011: OPEN sets ACCT-OPEN-DATE
*> ---------------------------------------------------------------
TEST-AM-011.
    ADD 1 TO WS-TEST-COUNT
    MOVE "AM-011: OPEN sets open date" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-ACCT-RESULT
    PERFORM SETUP-VALID-CIF
    MOVE "OPEN" TO WS-FUNCTION
    MOVE "SAV1" TO ACCT-PRODUCT-CODE
    MOVE "D" TO ACCT-TYPE
    MOVE "SV" TO ACCT-SUB-TYPE
    MOVE 1000000001 TO ACCT-PRIMARY-CIF
    CALL "ACCTMGMT" USING WS-FUNCTION ACCT-RECORD
                          CIF-RECORD WS-ACCT-RESULT
    IF WS-ACCT-RESULT-CODE = "E0000"
        IF ACCT-OPEN-DATE NOT = 0
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " date=" ACCT-OPEN-DATE
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " open-date not set"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-ACCT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> AM-012: OPEN sets ACCT-CREATED-DATE and ACCT-CREATED-USER
*> ---------------------------------------------------------------
TEST-AM-012.
    ADD 1 TO WS-TEST-COUNT
    MOVE "AM-012: OPEN sets audit fields" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-ACCT-RESULT
    PERFORM SETUP-VALID-CIF
    MOVE "OPEN" TO WS-FUNCTION
    MOVE "DDA1" TO ACCT-PRODUCT-CODE
    MOVE "D" TO ACCT-TYPE
    MOVE "CH" TO ACCT-SUB-TYPE
    MOVE 1000000001 TO ACCT-PRIMARY-CIF
    CALL "ACCTMGMT" USING WS-FUNCTION ACCT-RECORD
                          CIF-RECORD WS-ACCT-RESULT
    IF WS-ACCT-RESULT-CODE = "E0000"
        IF ACCT-CREATED-DATE NOT = 0
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " created=" ACCT-CREATED-DATE
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " created-date not set"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-ACCT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> AM-013: CLOS sets ACCT-STATUS=C and ACCT-CLOSE-DATE
*> ---------------------------------------------------------------
TEST-AM-013.
    ADD 1 TO WS-TEST-COUNT
    MOVE "AM-013: CLOS sets status/date" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-ACCT-RESULT
    PERFORM SETUP-VALID-CIF
    MOVE "CLOS" TO WS-FUNCTION
    MOVE "A" TO ACCT-STATUS
    MOVE 0 TO ACCT-LEDGER-BAL
    MOVE 0 TO ACCT-HOLD-AMOUNT
    MOVE "DDA1" TO ACCT-PRODUCT-CODE
    MOVE "D" TO ACCT-TYPE
    MOVE "CH" TO ACCT-SUB-TYPE
    CALL "ACCTMGMT" USING WS-FUNCTION ACCT-RECORD
                          CIF-RECORD WS-ACCT-RESULT
    IF WS-ACCT-RESULT-CODE = "E0000"
        IF ACCT-STATUS = "C" AND ACCT-CLOSE-DATE NOT = 0
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " close=" ACCT-CLOSE-DATE
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " status=" ACCT-STATUS
                " close=" ACCT-CLOSE-DATE
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-ACCT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> AM-014: OPEN with valid product code "DDA1" -> E0000
*> ---------------------------------------------------------------
TEST-AM-014.
    ADD 1 TO WS-TEST-COUNT
    MOVE "AM-014: OPEN valid product DDA1=E0000"
        TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-ACCT-RESULT
    PERFORM SETUP-VALID-CIF
    MOVE "OPEN" TO WS-FUNCTION
    MOVE "DDA1" TO ACCT-PRODUCT-CODE
    MOVE 1000000001 TO ACCT-PRIMARY-CIF
    CALL "ACCTMGMT" USING WS-FUNCTION ACCT-RECORD
                          CIF-RECORD WS-ACCT-RESULT
    IF WS-ACCT-RESULT-CODE = "E0000"
        IF ACCT-TYPE = "D" AND ACCT-SUB-TYPE = "CH"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " type=" ACCT-TYPE
                " sub=" ACCT-SUB-TYPE
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-ACCT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> AM-015: OPEN with invalid product code "XXXX" -> E0018
*> ---------------------------------------------------------------
TEST-AM-015.
    ADD 1 TO WS-TEST-COUNT
    MOVE "AM-015: OPEN invalid product=E0018"
        TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-ACCT-RESULT
    PERFORM SETUP-VALID-CIF
    MOVE "OPEN" TO WS-FUNCTION
    MOVE "XXXX" TO ACCT-PRODUCT-CODE
    MOVE 1000000001 TO ACCT-PRIMARY-CIF
    CALL "ACCTMGMT" USING WS-FUNCTION ACCT-RECORD
                          CIF-RECORD WS-ACCT-RESULT
    IF WS-ACCT-RESULT-CODE = "E0018"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0018 actual=" WS-ACCT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> AM-016: OPEN with CD product "CD06" -> E0000, sub-type=CD
*> ---------------------------------------------------------------
TEST-AM-016.
    ADD 1 TO WS-TEST-COUNT
    MOVE "AM-016: OPEN CD product CD06=E0000"
        TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-ACCT-RESULT
    PERFORM SETUP-VALID-CIF
    MOVE "OPEN" TO WS-FUNCTION
    MOVE "CD06" TO ACCT-PRODUCT-CODE
    MOVE 1000000001 TO ACCT-PRIMARY-CIF
    CALL "ACCTMGMT" USING WS-FUNCTION ACCT-RECORD
                          CIF-RECORD WS-ACCT-RESULT
    IF WS-ACCT-RESULT-CODE = "E0000"
        IF ACCT-TYPE = "D" AND ACCT-SUB-TYPE = "CD"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " type=" ACCT-TYPE
                " sub=" ACCT-SUB-TYPE
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-ACCT-RESULT-CODE
    END-IF.

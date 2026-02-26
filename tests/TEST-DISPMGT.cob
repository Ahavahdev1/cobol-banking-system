IDENTIFICATION DIVISION.
PROGRAM-ID. TEST-DISPMGT.
*> ================================================================
*> TEST-DISPMGT - Test suite for DISPMGT0 Reg E Dispute Management
*> Tests: File, provisional credit, resolve, inquiry (25 tests)
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-TEST-COUNT          PIC 9(3) VALUE 0.
01  WS-PASS-COUNT          PIC 9(3) VALUE 0.
01  WS-FAIL-COUNT          PIC 9(3) VALUE 0.
01  WS-TEST-NAME           PIC X(60).

01  WS-DSP-FUNCTION        PIC X(4).
COPY CPYDSP.
COPY CPYACCT.
01  WS-DSP-RESULT.
    05  WS-DSP-RESULT-CODE PIC X(5).
    05  WS-DSP-RESULT-MSG  PIC X(50).

01  WS-EXPECTED-BAL        PIC S9(13)V99.
01  WS-SAVED-BAL           PIC S9(13)V99.

PROCEDURE DIVISION.
MAIN-PROGRAM.
    DISPLAY "========================================".
    DISPLAY "TEST SUITE: DISPMGT - Reg E Disputes".
    DISPLAY "========================================".

    PERFORM TEST-DP-001
    PERFORM TEST-DP-002
    PERFORM TEST-DP-003
    PERFORM TEST-DP-004
    PERFORM TEST-DP-005
    PERFORM TEST-DP-006
    PERFORM TEST-DP-007
    PERFORM TEST-DP-008
    PERFORM TEST-DP-009
    PERFORM TEST-DP-010
    PERFORM TEST-DP-011
    PERFORM TEST-DP-012
    PERFORM TEST-DP-013
    PERFORM TEST-DP-014
    PERFORM TEST-DP-015
    PERFORM TEST-DP-016
    PERFORM TEST-DP-017
    PERFORM TEST-DP-018
    PERFORM TEST-DP-019
    PERFORM TEST-DP-020
    PERFORM TEST-DP-021
    PERFORM TEST-DP-022
    PERFORM TEST-DP-023
    PERFORM TEST-DP-024
    PERFORM TEST-DP-025

    DISPLAY "========================================".
    DISPLAY "RESULTS: " WS-PASS-COUNT "/" WS-TEST-COUNT
            " PASSED".
    DISPLAY "         " WS-FAIL-COUNT " FAILED".
    DISPLAY "========================================".
    MOVE WS-FAIL-COUNT TO RETURN-CODE
    STOP RUN.

*> ---------------------------------------------------------------
*> DP-001: FILE dispute -> E0000, status=P
*> ---------------------------------------------------------------
TEST-DP-001.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DP-001: FILE dispute -> E0000 status=P"
        TO WS-TEST-NAME
    INITIALIZE DISPUTE-RECORD
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-DSP-RESULT
    MOVE "FILE" TO WS-DSP-FUNCTION
    MOVE 100000000001 TO DSP-ACCT-NUMBER
    MOVE 000000000000100 TO DSP-TXN-ID
    MOVE 250.00 TO DSP-TXN-AMOUNT
    MOVE "UNAU" TO DSP-DISPUTE-TYPE
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    IF WS-DSP-RESULT-CODE = "E0000"
        IF DSP-STATUS = "P"
            AND DSP-DISPUTE-ID > 0
            AND DSP-DISPUTE-DATE > 0
            AND DSP-DEADLINE-DATE > DSP-DISPUTE-DATE
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " id=" DSP-DISPUTE-ID
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " status=" DSP-STATUS
                " id=" DSP-DISPUTE-ID
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-DSP-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> DP-002: FILE with invalid type -> E0085
*> ---------------------------------------------------------------
TEST-DP-002.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DP-002: FILE invalid type -> E0085"
        TO WS-TEST-NAME
    INITIALIZE DISPUTE-RECORD
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-DSP-RESULT
    MOVE "FILE" TO WS-DSP-FUNCTION
    MOVE 100000000001 TO DSP-ACCT-NUMBER
    MOVE 000000000000101 TO DSP-TXN-ID
    MOVE 100.00 TO DSP-TXN-AMOUNT
    MOVE "FAKE" TO DSP-DISPUTE-TYPE
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    IF WS-DSP-RESULT-CODE = "E0085"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-DSP-RESULT-CODE " expected=E0085"
    END-IF.

*> ---------------------------------------------------------------
*> DP-003: PROV provisional credit -> balance increases
*> ---------------------------------------------------------------
TEST-DP-003.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DP-003: PROV credit -> balance increases"
        TO WS-TEST-NAME
    *> First file a dispute to get a valid dispute record
    INITIALIZE DISPUTE-RECORD
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-DSP-RESULT
    MOVE "FILE" TO WS-DSP-FUNCTION
    MOVE 100000000001 TO DSP-ACCT-NUMBER
    MOVE 000000000000102 TO DSP-TXN-ID
    MOVE 500.00 TO DSP-TXN-AMOUNT
    MOVE "UNAU" TO DSP-DISPUTE-TYPE
    *> Set up account with known balance
    MOVE 100000000001 TO ACCT-NUMBER
    MOVE 1000.00 TO ACCT-LEDGER-BAL
    MOVE 1000.00 TO ACCT-AVAIL-BAL
    MOVE 0.00 TO ACCT-HOLD-AMOUNT
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    *> Now issue provisional credit
    MOVE "PROV" TO WS-DSP-FUNCTION
    INITIALIZE WS-DSP-RESULT
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    COMPUTE WS-EXPECTED-BAL = 1000.00 + 500.00
    IF WS-DSP-RESULT-CODE = "E0000"
        IF ACCT-LEDGER-BAL = WS-EXPECTED-BAL
            AND DSP-STATUS = "C"
            AND DSP-PROVISIONAL-AMT = 500.00
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " bal=" ACCT-LEDGER-BAL
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " bal=" ACCT-LEDGER-BAL
                " status=" DSP-STATUS
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-DSP-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> DP-004: PROV on resolved dispute -> E0086
*> ---------------------------------------------------------------
TEST-DP-004.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DP-004: PROV on resolved dispute -> E0086"
        TO WS-TEST-NAME
    INITIALIZE DISPUTE-RECORD
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-DSP-RESULT
    *> Set up a dispute that is already resolved
    MOVE 000000000000099 TO DSP-DISPUTE-ID
    MOVE 100000000001 TO DSP-ACCT-NUMBER
    MOVE 200.00 TO DSP-TXN-AMOUNT
    MOVE "R" TO DSP-STATUS
    MOVE "PROV" TO WS-DSP-FUNCTION
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    IF WS-DSP-RESULT-CODE = "E0086"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-DSP-RESULT-CODE " expected=E0086"
    END-IF.

*> ---------------------------------------------------------------
*> DP-005: RSLV approve -> status=R, credit kept
*> ---------------------------------------------------------------
TEST-DP-005.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DP-005: RSLV approve -> status=R credit kept"
        TO WS-TEST-NAME
    *> File dispute, issue provisional, then resolve with approval
    INITIALIZE DISPUTE-RECORD
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-DSP-RESULT
    MOVE "FILE" TO WS-DSP-FUNCTION
    MOVE 100000000001 TO DSP-ACCT-NUMBER
    MOVE 000000000000103 TO DSP-TXN-ID
    MOVE 300.00 TO DSP-TXN-AMOUNT
    MOVE "ERRO" TO DSP-DISPUTE-TYPE
    MOVE 100000000001 TO ACCT-NUMBER
    MOVE 2000.00 TO ACCT-LEDGER-BAL
    MOVE 2000.00 TO ACCT-AVAIL-BAL
    MOVE 0.00 TO ACCT-HOLD-AMOUNT
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    *> Issue provisional credit
    MOVE "PROV" TO WS-DSP-FUNCTION
    INITIALIZE WS-DSP-RESULT
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    *> Save balance after provisional credit
    MOVE ACCT-LEDGER-BAL TO WS-SAVED-BAL
    *> Resolve with approval
    MOVE "RSLV" TO WS-DSP-FUNCTION
    MOVE "AP" TO DSP-RESOLUTION-CODE
    INITIALIZE WS-DSP-RESULT
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    IF WS-DSP-RESULT-CODE = "E0000"
        IF DSP-STATUS = "R"
            AND ACCT-LEDGER-BAL = WS-SAVED-BAL
            AND DSP-RESOLUTION-DATE > 0
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " bal=" ACCT-LEDGER-BAL
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " status=" DSP-STATUS
                " bal=" ACCT-LEDGER-BAL
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-DSP-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> DP-006: RSLV deny -> status=D, credit reversed
*> ---------------------------------------------------------------
TEST-DP-006.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DP-006: RSLV deny -> status=D credit reversed"
        TO WS-TEST-NAME
    *> File dispute, issue provisional, then deny
    INITIALIZE DISPUTE-RECORD
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-DSP-RESULT
    MOVE "FILE" TO WS-DSP-FUNCTION
    MOVE 100000000002 TO DSP-ACCT-NUMBER
    MOVE 000000000000104 TO DSP-TXN-ID
    MOVE 400.00 TO DSP-TXN-AMOUNT
    MOVE "DUPE" TO DSP-DISPUTE-TYPE
    MOVE 100000000002 TO ACCT-NUMBER
    MOVE 5000.00 TO ACCT-LEDGER-BAL
    MOVE 5000.00 TO ACCT-AVAIL-BAL
    MOVE 0.00 TO ACCT-HOLD-AMOUNT
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    *> Issue provisional credit (bal becomes 5400)
    MOVE "PROV" TO WS-DSP-FUNCTION
    INITIALIZE WS-DSP-RESULT
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    *> Deny the dispute (should reverse back to 5000)
    MOVE "RSLV" TO WS-DSP-FUNCTION
    MOVE "DN" TO DSP-RESOLUTION-CODE
    INITIALIZE WS-DSP-RESULT
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    COMPUTE WS-EXPECTED-BAL = 5000.00
    IF WS-DSP-RESULT-CODE = "E0000"
        IF DSP-STATUS = "D"
            AND ACCT-LEDGER-BAL = WS-EXPECTED-BAL
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " bal=" ACCT-LEDGER-BAL
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " status=" DSP-STATUS
                " bal=" ACCT-LEDGER-BAL
                " expected=" WS-EXPECTED-BAL
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-DSP-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> DP-007: RSLV partial -> adjusted amount
*> ---------------------------------------------------------------
TEST-DP-007.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DP-007: RSLV partial -> adjusted amount"
        TO WS-TEST-NAME
    *> File dispute for $600, issue provisional, then partial $200
    INITIALIZE DISPUTE-RECORD
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-DSP-RESULT
    MOVE "FILE" TO WS-DSP-FUNCTION
    MOVE 100000000003 TO DSP-ACCT-NUMBER
    MOVE 000000000000105 TO DSP-TXN-ID
    MOVE 600.00 TO DSP-TXN-AMOUNT
    MOVE "WRNG" TO DSP-DISPUTE-TYPE
    MOVE 100000000003 TO ACCT-NUMBER
    MOVE 3000.00 TO ACCT-LEDGER-BAL
    MOVE 3000.00 TO ACCT-AVAIL-BAL
    MOVE 0.00 TO ACCT-HOLD-AMOUNT
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    *> Issue provisional credit (bal becomes 3600)
    MOVE "PROV" TO WS-DSP-FUNCTION
    INITIALIZE WS-DSP-RESULT
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    *> Partially approve for $200 (reverse $400, bal becomes 3200)
    MOVE "RSLV" TO WS-DSP-FUNCTION
    MOVE "PA" TO DSP-RESOLUTION-CODE
    MOVE 200.00 TO DSP-TXN-AMOUNT
    INITIALIZE WS-DSP-RESULT
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    *> Expected: 3000 + 600 (provisional) - 400 (reversal) = 3200
    COMPUTE WS-EXPECTED-BAL = 3200.00
    IF WS-DSP-RESULT-CODE = "E0000"
        IF DSP-STATUS = "R"
            AND ACCT-LEDGER-BAL = WS-EXPECTED-BAL
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " bal=" ACCT-LEDGER-BAL
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " status=" DSP-STATUS
                " bal=" ACCT-LEDGER-BAL
                " expected=" WS-EXPECTED-BAL
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-DSP-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> DP-008: INQY returns dispute info
*> ---------------------------------------------------------------
TEST-DP-008.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DP-008: INQY returns dispute info"
        TO WS-TEST-NAME
    *> Set up a dispute record with known values
    INITIALIZE DISPUTE-RECORD
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-DSP-RESULT
    MOVE 000000000000042 TO DSP-DISPUTE-ID
    MOVE 100000000001 TO DSP-ACCT-NUMBER
    MOVE 000000000000200 TO DSP-TXN-ID
    MOVE 750.00 TO DSP-TXN-AMOUNT
    MOVE "NORC" TO DSP-DISPUTE-TYPE
    MOVE "I" TO DSP-STATUS
    MOVE "Charge not received" TO DSP-DESCRIPTION
    MOVE "INQY" TO WS-DSP-FUNCTION
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    IF WS-DSP-RESULT-CODE = "E0000"
        IF DSP-DISPUTE-ID = 000000000000042
            AND DSP-STATUS = "I"
            AND DSP-TXN-AMOUNT = 750.00
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " id=" DSP-DISPUTE-ID
                " status=" DSP-STATUS
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " id=" DSP-DISPUTE-ID
                " status=" DSP-STATUS
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-DSP-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> DP-009: FILE dispute -> deadline date is set (non-zero)
*> ---------------------------------------------------------------
TEST-DP-009.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DP-009: FILE dispute sets deadline date"
        TO WS-TEST-NAME
    INITIALIZE DISPUTE-RECORD
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-DSP-RESULT
    MOVE "FILE" TO WS-DSP-FUNCTION
    MOVE 100000000001 TO DSP-ACCT-NUMBER
    MOVE 000000000000200 TO DSP-TXN-ID
    MOVE 150.00 TO DSP-TXN-AMOUNT
    MOVE "ERRO" TO DSP-DISPUTE-TYPE
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    IF WS-DSP-RESULT-CODE = "E0000"
        IF DSP-DEADLINE-DATE > 0
            AND DSP-DEADLINE-DATE > DSP-DISPUTE-DATE
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " filed=" DSP-DISPUTE-DATE
                " deadline=" DSP-DEADLINE-DATE
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " deadline=" DSP-DEADLINE-DATE
                " dispute-date=" DSP-DISPUTE-DATE
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-DSP-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> DP-010: PROV with dispute ID = 0 -> E0087
*> ---------------------------------------------------------------
TEST-DP-010.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DP-010: PROV dispute-id=0 -> E0087"
        TO WS-TEST-NAME
    INITIALIZE DISPUTE-RECORD
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-DSP-RESULT
    MOVE "PROV" TO WS-DSP-FUNCTION
    MOVE 0 TO DSP-DISPUTE-ID
    MOVE 100000000001 TO DSP-ACCT-NUMBER
    MOVE 200.00 TO DSP-TXN-AMOUNT
    MOVE "P" TO DSP-STATUS
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    IF WS-DSP-RESULT-CODE = "E0087"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-DSP-RESULT-CODE " expected=E0087"
    END-IF.

*> ---------------------------------------------------------------
*> DP-011: RSLV with invalid resolution code -> E0086
*> ---------------------------------------------------------------
TEST-DP-011.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DP-011: RSLV invalid resolution code -> E0086"
        TO WS-TEST-NAME
    *> File a dispute to get a valid dispute record
    INITIALIZE DISPUTE-RECORD
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-DSP-RESULT
    MOVE "FILE" TO WS-DSP-FUNCTION
    MOVE 100000000001 TO DSP-ACCT-NUMBER
    MOVE 000000000000106 TO DSP-TXN-ID
    MOVE 350.00 TO DSP-TXN-AMOUNT
    MOVE "UNAU" TO DSP-DISPUTE-TYPE
    MOVE 100000000001 TO ACCT-NUMBER
    MOVE 2000.00 TO ACCT-LEDGER-BAL
    MOVE 2000.00 TO ACCT-AVAIL-BAL
    MOVE 0.00 TO ACCT-HOLD-AMOUNT
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    *> Issue provisional credit so status allows RSLV
    MOVE "PROV" TO WS-DSP-FUNCTION
    INITIALIZE WS-DSP-RESULT
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    *> Now try RSLV with invalid resolution code "XX"
    MOVE "RSLV" TO WS-DSP-FUNCTION
    MOVE "XX" TO DSP-RESOLUTION-CODE
    INITIALIZE WS-DSP-RESULT
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    IF WS-DSP-RESULT-CODE = "E0086"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-DSP-RESULT-CODE " expected=E0086"
    END-IF.

*> ---------------------------------------------------------------
*> DP-012: RSLV with dispute ID = 0 -> E0087
*> ---------------------------------------------------------------
TEST-DP-012.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DP-012: RSLV dispute-id=0 -> E0087"
        TO WS-TEST-NAME
    INITIALIZE DISPUTE-RECORD
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-DSP-RESULT
    MOVE "RSLV" TO WS-DSP-FUNCTION
    MOVE 0 TO DSP-DISPUTE-ID
    MOVE 100000000001 TO DSP-ACCT-NUMBER
    MOVE "AP" TO DSP-RESOLUTION-CODE
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    IF WS-DSP-RESULT-CODE = "E0087"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-DSP-RESULT-CODE " expected=E0087"
    END-IF.

*> ---------------------------------------------------------------
*> DP-013: FILE with zero amount -> E0031
*> ---------------------------------------------------------------
TEST-DP-013.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DP-013: FILE zero amount -> E0031"
        TO WS-TEST-NAME
    INITIALIZE DISPUTE-RECORD
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-DSP-RESULT
    MOVE "FILE" TO WS-DSP-FUNCTION
    MOVE 100000000001 TO DSP-ACCT-NUMBER
    MOVE 000000000000107 TO DSP-TXN-ID
    MOVE 0.00 TO DSP-TXN-AMOUNT
    MOVE "UNAU" TO DSP-DISPUTE-TYPE
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    IF WS-DSP-RESULT-CODE = "E0031"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-DSP-RESULT-CODE " expected=E0031"
    END-IF.

*> ---------------------------------------------------------------
*> DP-014: FILE with negative amount -> E0031
*> ---------------------------------------------------------------
TEST-DP-014.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DP-014: FILE negative amount -> E0031"
        TO WS-TEST-NAME
    INITIALIZE DISPUTE-RECORD
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-DSP-RESULT
    MOVE "FILE" TO WS-DSP-FUNCTION
    MOVE 100000000001 TO DSP-ACCT-NUMBER
    MOVE 000000000000108 TO DSP-TXN-ID
    MOVE -100.00 TO DSP-TXN-AMOUNT
    MOVE "UNAU" TO DSP-DISPUTE-TYPE
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    IF WS-DSP-RESULT-CODE = "E0031"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-DSP-RESULT-CODE " expected=E0031"
    END-IF.

*> ---------------------------------------------------------------
*> DP-015: PROV on closed account -> E0011
*> ---------------------------------------------------------------
TEST-DP-015.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DP-015: PROV closed account -> E0011"
        TO WS-TEST-NAME
    *> File a dispute first
    INITIALIZE DISPUTE-RECORD
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-DSP-RESULT
    MOVE "FILE" TO WS-DSP-FUNCTION
    MOVE 100000000001 TO DSP-ACCT-NUMBER
    MOVE 000000000000109 TO DSP-TXN-ID
    MOVE 200.00 TO DSP-TXN-AMOUNT
    MOVE "UNAU" TO DSP-DISPUTE-TYPE
    MOVE 100000000001 TO ACCT-NUMBER
    MOVE 1000.00 TO ACCT-LEDGER-BAL
    MOVE 1000.00 TO ACCT-AVAIL-BAL
    MOVE 0.00 TO ACCT-HOLD-AMOUNT
    MOVE "A" TO ACCT-STATUS
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    *> Now set account to closed and try PROV
    MOVE "C" TO ACCT-STATUS
    MOVE "PROV" TO WS-DSP-FUNCTION
    INITIALIZE WS-DSP-RESULT
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    IF WS-DSP-RESULT-CODE = "E0011"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-DSP-RESULT-CODE " expected=E0011"
    END-IF.

*> ---------------------------------------------------------------
*> DP-016: Invalid function code -> E0001
*> ---------------------------------------------------------------
TEST-DP-016.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DP-016: Invalid function -> E0001"
        TO WS-TEST-NAME
    INITIALIZE DISPUTE-RECORD
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-DSP-RESULT
    MOVE "XXXX" TO WS-DSP-FUNCTION
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    IF WS-DSP-RESULT-CODE = "E0001"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-DSP-RESULT-CODE " expected=E0001"
    END-IF.

*> ---------------------------------------------------------------
*> DP-017: PROV on frozen account -> E0012
*> File dispute first, then set account to frozen, call PROV
*> ---------------------------------------------------------------
TEST-DP-017.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DP-017: PROV frozen account -> E0012"
        TO WS-TEST-NAME
    INITIALIZE DISPUTE-RECORD
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-DSP-RESULT
    MOVE "FILE" TO WS-DSP-FUNCTION
    MOVE 100000000001 TO DSP-ACCT-NUMBER
    MOVE 000000000000110 TO DSP-TXN-ID
    MOVE 300.00 TO DSP-TXN-AMOUNT
    MOVE "UNAU" TO DSP-DISPUTE-TYPE
    MOVE 100000000001 TO ACCT-NUMBER
    MOVE 2000.00 TO ACCT-LEDGER-BAL
    MOVE 2000.00 TO ACCT-AVAIL-BAL
    MOVE 0.00 TO ACCT-HOLD-AMOUNT
    MOVE "A" TO ACCT-STATUS
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    *> Now freeze the account and try PROV
    MOVE "F" TO ACCT-STATUS
    MOVE "PROV" TO WS-DSP-FUNCTION
    INITIALIZE WS-DSP-RESULT
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    IF WS-DSP-RESULT-CODE = "E0012"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-DSP-RESULT-CODE " expected=E0012"
    END-IF.

*> ---------------------------------------------------------------
*> DP-018: Overflow on provisional credit -> E0040
*> Set balance to max, file dispute for $1.00, call PROV
*> ---------------------------------------------------------------
TEST-DP-018.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DP-018: PROV overflow -> E0040"
        TO WS-TEST-NAME
    INITIALIZE DISPUTE-RECORD
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-DSP-RESULT
    MOVE "FILE" TO WS-DSP-FUNCTION
    MOVE 100000000001 TO DSP-ACCT-NUMBER
    MOVE 000000000000111 TO DSP-TXN-ID
    MOVE 1.00 TO DSP-TXN-AMOUNT
    MOVE "UNAU" TO DSP-DISPUTE-TYPE
    MOVE 100000000001 TO ACCT-NUMBER
    MOVE 9999999999999.99 TO ACCT-LEDGER-BAL
    MOVE 9999999999999.99 TO ACCT-AVAIL-BAL
    MOVE 0.00 TO ACCT-HOLD-AMOUNT
    MOVE "A" TO ACCT-STATUS
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    *> Now issue provisional credit (should overflow)
    MOVE "PROV" TO WS-DSP-FUNCTION
    INITIALIZE WS-DSP-RESULT
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    IF WS-DSP-RESULT-CODE = "E0040"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-DSP-RESULT-CODE " expected=E0040"
    END-IF.

*> ---------------------------------------------------------------
*> DP-019: RSLV deny without prior provisional credit
*> File dispute (status P), skip PROV, RSLV with "DN"
*> Expect status "D", balance unchanged
*> ---------------------------------------------------------------
TEST-DP-019.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DP-019: RSLV deny no prior PROV"
        TO WS-TEST-NAME
    INITIALIZE DISPUTE-RECORD
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-DSP-RESULT
    MOVE "FILE" TO WS-DSP-FUNCTION
    MOVE 100000000001 TO DSP-ACCT-NUMBER
    MOVE 000000000000112 TO DSP-TXN-ID
    MOVE 500.00 TO DSP-TXN-AMOUNT
    MOVE "UNAU" TO DSP-DISPUTE-TYPE
    MOVE 100000000001 TO ACCT-NUMBER
    MOVE 3000.00 TO ACCT-LEDGER-BAL
    MOVE 3000.00 TO ACCT-AVAIL-BAL
    MOVE 0.00 TO ACCT-HOLD-AMOUNT
    MOVE "A" TO ACCT-STATUS
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    *> Save balance before resolve
    MOVE ACCT-LEDGER-BAL TO WS-SAVED-BAL
    *> Skip PROV, go straight to RSLV deny
    MOVE "RSLV" TO WS-DSP-FUNCTION
    MOVE "DN" TO DSP-RESOLUTION-CODE
    INITIALIZE WS-DSP-RESULT
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    IF WS-DSP-RESULT-CODE = "E0000"
        IF DSP-STATUS = "D"
            AND ACCT-LEDGER-BAL = WS-SAVED-BAL
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " bal=" ACCT-LEDGER-BAL
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " status=" DSP-STATUS
                " bal=" ACCT-LEDGER-BAL
                " expected-bal=" WS-SAVED-BAL
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-DSP-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> DP-020: PROV on closed account -> E0011
*> File dispute first, then close account, call PROV
*> ---------------------------------------------------------------
TEST-DP-020.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DP-020: PROV closed account -> E0011"
        TO WS-TEST-NAME
    INITIALIZE DISPUTE-RECORD
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-DSP-RESULT
    MOVE "FILE" TO WS-DSP-FUNCTION
    MOVE 100000000001 TO DSP-ACCT-NUMBER
    MOVE 000000000000113 TO DSP-TXN-ID
    MOVE 250.00 TO DSP-TXN-AMOUNT
    MOVE "UNAU" TO DSP-DISPUTE-TYPE
    MOVE 100000000001 TO ACCT-NUMBER
    MOVE 1000.00 TO ACCT-LEDGER-BAL
    MOVE 1000.00 TO ACCT-AVAIL-BAL
    MOVE 0.00 TO ACCT-HOLD-AMOUNT
    MOVE "A" TO ACCT-STATUS
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    *> Now set account to closed and try PROV
    MOVE "C" TO ACCT-STATUS
    MOVE "PROV" TO WS-DSP-FUNCTION
    INITIALIZE WS-DSP-RESULT
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    IF WS-DSP-RESULT-CODE = "E0011"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-DSP-RESULT-CODE " expected=E0011"
    END-IF.

*> ---------------------------------------------------------------
*> DP-021: PROV on escheated account -> E0044
*> File dispute first, then set account to escheated, call PROV
*> ---------------------------------------------------------------
TEST-DP-021.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DP-021: PROV escheated account -> E0044"
        TO WS-TEST-NAME
    INITIALIZE DISPUTE-RECORD
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-DSP-RESULT
    MOVE "FILE" TO WS-DSP-FUNCTION
    MOVE 100000000001 TO DSP-ACCT-NUMBER
    MOVE 000000000000114 TO DSP-TXN-ID
    MOVE 100.00 TO DSP-TXN-AMOUNT
    MOVE "UNAU" TO DSP-DISPUTE-TYPE
    MOVE 100000000001 TO ACCT-NUMBER
    MOVE 1000.00 TO ACCT-LEDGER-BAL
    MOVE 1000.00 TO ACCT-AVAIL-BAL
    MOVE 0.00 TO ACCT-HOLD-AMOUNT
    MOVE "A" TO ACCT-STATUS
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    *> Now set account to escheated and try PROV
    MOVE "E" TO ACCT-STATUS
    MOVE "PROV" TO WS-DSP-FUNCTION
    INITIALIZE WS-DSP-RESULT
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    IF WS-DSP-RESULT-CODE = "E0044"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-DSP-RESULT-CODE " expected=E0044"
    END-IF.

*> ---------------------------------------------------------------
*> DP-022: RSLV partial amount exceeds provisional -> E0002
*> File dispute, issue PROV for $500, then RSLV PA with $600
*> ---------------------------------------------------------------
TEST-DP-022.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DP-022: RSLV PA exceeds prov -> E0002"
        TO WS-TEST-NAME
    INITIALIZE DISPUTE-RECORD
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-DSP-RESULT
    *> Step 1: FILE dispute for $500
    MOVE "FILE" TO WS-DSP-FUNCTION
    MOVE 100000000001 TO DSP-ACCT-NUMBER
    MOVE 000000000000115 TO DSP-TXN-ID
    MOVE 500.00 TO DSP-TXN-AMOUNT
    MOVE "UNAU" TO DSP-DISPUTE-TYPE
    MOVE 100000000001 TO ACCT-NUMBER
    MOVE 5000.00 TO ACCT-LEDGER-BAL
    MOVE 5000.00 TO ACCT-AVAIL-BAL
    MOVE 0.00 TO ACCT-HOLD-AMOUNT
    MOVE "A" TO ACCT-STATUS
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    IF WS-DSP-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " FILE rc=" WS-DSP-RESULT-CODE
        GO TO TEST-DP-022-END
    END-IF
    *> Step 2: Issue provisional credit
    MOVE "PROV" TO WS-DSP-FUNCTION
    INITIALIZE WS-DSP-RESULT
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    IF WS-DSP-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " PROV rc=" WS-DSP-RESULT-CODE
        GO TO TEST-DP-022-END
    END-IF
    *> Step 3: RSLV partial with amount > provisional ($600 > $500)
    MOVE "RSLV" TO WS-DSP-FUNCTION
    MOVE "PA" TO DSP-RESOLUTION-CODE
    MOVE 600.00 TO DSP-TXN-AMOUNT
    INITIALIZE WS-DSP-RESULT
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    IF WS-DSP-RESULT-CODE = "E0002"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-DSP-RESULT-CODE " expected=E0002"
    END-IF.
TEST-DP-022-END.
    CONTINUE.

*> ---------------------------------------------------------------
*> DP-023: RSLV past Reg E deadline without PROV -> E0100
*> Dispute filed with deadline in the past, status still "P"
*> (no provisional credit issued) -> must issue PROV first
*> ---------------------------------------------------------------
TEST-DP-023.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DP-023: RSLV past deadline no PROV -> E0100"
        TO WS-TEST-NAME
    INITIALIZE DISPUTE-RECORD
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-DSP-RESULT
    *> Set up dispute in "P" status with a past deadline
    MOVE 1 TO DSP-DISPUTE-ID
    MOVE "P" TO DSP-STATUS
    MOVE "UNAU" TO DSP-DISPUTE-TYPE
    MOVE 250.00 TO DSP-TXN-AMOUNT
    MOVE 20260101 TO DSP-DISPUTE-DATE
    MOVE 20260115 TO DSP-DEADLINE-DATE
    *> Today is past the deadline
    MOVE "AP" TO DSP-RESOLUTION-CODE
    MOVE 100000000001 TO ACCT-NUMBER
    MOVE 5000.00 TO ACCT-LEDGER-BAL
    MOVE 5000.00 TO ACCT-AVAIL-BAL
    MOVE "A" TO ACCT-STATUS
    MOVE "RSLV" TO WS-DSP-FUNCTION
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    IF WS-DSP-RESULT-CODE = "E0100"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-DSP-RESULT-CODE " expected=E0100"
    END-IF.

*> ---------------------------------------------------------------
*> DP-024: RSLV past deadline WITH PROV (status=C) -> allowed
*> Dispute credited (status="C") can be resolved even past deadline
*> ---------------------------------------------------------------
TEST-DP-024.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DP-024: RSLV past deadline w/ PROV -> ok"
        TO WS-TEST-NAME
    INITIALIZE DISPUTE-RECORD
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-DSP-RESULT
    *> Set up dispute in "C" status (provisional credit issued)
    MOVE 2 TO DSP-DISPUTE-ID
    MOVE "C" TO DSP-STATUS
    MOVE "UNAU" TO DSP-DISPUTE-TYPE
    MOVE 250.00 TO DSP-TXN-AMOUNT
    MOVE 250.00 TO DSP-PROVISIONAL-AMT
    MOVE 20260101 TO DSP-DISPUTE-DATE
    MOVE 20260115 TO DSP-DEADLINE-DATE
    MOVE "AP" TO DSP-RESOLUTION-CODE
    MOVE 100000000001 TO ACCT-NUMBER
    MOVE 5000.00 TO ACCT-LEDGER-BAL
    MOVE 5000.00 TO ACCT-AVAIL-BAL
    MOVE "A" TO ACCT-STATUS
    MOVE "RSLV" TO WS-DSP-FUNCTION
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    IF WS-DSP-RESULT-CODE = "E0000"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-DSP-RESULT-CODE " expected=E0000"
    END-IF.

*> ---------------------------------------------------------------
*> DP-025: RSLV past deadline with "I" status -> E0100
*> Investigating disputes also need PROV before RSLV past deadline
*> 12 CFR 1005.11(c)(1) applies to all non-credited disputes
*> ---------------------------------------------------------------
TEST-DP-025.
    ADD 1 TO WS-TEST-COUNT
    MOVE "DP-025: RSLV past deadline status=I -> E0100"
        TO WS-TEST-NAME
    INITIALIZE DISPUTE-RECORD
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-DSP-RESULT
    *> Set up dispute in "I" status with a past deadline
    MOVE 3 TO DSP-DISPUTE-ID
    MOVE "I" TO DSP-STATUS
    MOVE "UNAU" TO DSP-DISPUTE-TYPE
    MOVE 300.00 TO DSP-TXN-AMOUNT
    MOVE 20260101 TO DSP-DISPUTE-DATE
    MOVE 20260115 TO DSP-DEADLINE-DATE
    MOVE "AP" TO DSP-RESOLUTION-CODE
    MOVE 100000000003 TO ACCT-NUMBER
    MOVE 5000.00 TO ACCT-LEDGER-BAL
    MOVE 5000.00 TO ACCT-AVAIL-BAL
    MOVE "A" TO ACCT-STATUS
    MOVE "RSLV" TO WS-DSP-FUNCTION
    CALL "DISPMGT0" USING WS-DSP-FUNCTION DISPUTE-RECORD
                          ACCT-RECORD WS-DSP-RESULT
    IF WS-DSP-RESULT-CODE = "E0100"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-DSP-RESULT-CODE " expected=E0100"
    END-IF.

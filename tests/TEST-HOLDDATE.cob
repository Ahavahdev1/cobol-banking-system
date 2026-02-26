IDENTIFICATION DIVISION.
PROGRAM-ID. TEST-HOLDDATE.
*> ================================================================
*> TEST-HOLDDATE - Test suite for HOLDCALC0 hold release date
*> computation using DATEUTIL BDAY (business day addition)
*> Tests: HD-001 to HD-004 verify release dates across weekdays
*>        and weekends for local, non-local, and treasury checks
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-TEST-COUNT          PIC 9(3) VALUE 0.
01  WS-PASS-COUNT          PIC 9(3) VALUE 0.
01  WS-FAIL-COUNT          PIC 9(3) VALUE 0.
01  WS-TEST-NAME           PIC X(60).

*> Hold request (matches LINKAGE in HOLDCALC0)
01  WS-HOLD-REQUEST.
    05  WS-HR-ACCT-NUMBER   PIC 9(12).
    05  WS-HR-DEPOSIT-AMT   PIC S9(13)V99.
    05  WS-HR-CHECK-TYPE    PIC X(2).
    05  WS-HR-DEPOSIT-DATE  PIC 9(8).
    05  WS-HR-ACCT-OPEN-DATE PIC 9(8).
    05  WS-HR-IS-REDEPOSIT  PIC X(1).
    05  WS-HR-REPEATED-OD   PIC X(1).

COPY CPYHOLD.

01  WS-HOLD-RESULT.
    05  WS-HOLD-RESULT-CODE     PIC X(5).
    05  WS-HOLD-RESULT-MSG      PIC X(50).
    05  WS-HOLD-NEXT-DAY-AMT    PIC S9(13)V99.
    05  WS-HOLD-REMAINING-AMT   PIC S9(13)V99.
    05  WS-HOLD-RELEASE-DT      PIC 9(8).
    05  WS-HOLD-EXCEPTION-FLAG  PIC X(1).

01  WS-EXPECTED-RELEASE    PIC 9(8).

PROCEDURE DIVISION.
MAIN-PROGRAM.
    DISPLAY "========================================".
    DISPLAY "TEST SUITE: HOLDDATE - Release Date Calc".
    DISPLAY "========================================".

    PERFORM TEST-HD-001
    PERFORM TEST-HD-002
    PERFORM TEST-HD-003
    PERFORM TEST-HD-004

    DISPLAY "========================================".
    DISPLAY "RESULTS: " WS-PASS-COUNT "/" WS-TEST-COUNT
            " PASSED".
    DISPLAY "         " WS-FAIL-COUNT " FAILED".
    DISPLAY "========================================".
    MOVE WS-FAIL-COUNT TO RETURN-CODE
    STOP RUN.

*> ---------------------------------------------------------------
*> HD-001: Local check Mon 2/16, 2 biz days -> Wed 2/18
*> ---------------------------------------------------------------
TEST-HD-001.
    ADD 1 TO WS-TEST-COUNT
    MOVE "HD-001: Local chk Mon +2 biz days = Wed"
        TO WS-TEST-NAME
    INITIALIZE WS-HOLD-REQUEST
    INITIALIZE HOLD-RECORD
    INITIALIZE WS-HOLD-RESULT
    MOVE 100000000001 TO WS-HR-ACCT-NUMBER
    MOVE 1000.00 TO WS-HR-DEPOSIT-AMT
    MOVE "LC" TO WS-HR-CHECK-TYPE
    MOVE 20260216 TO WS-HR-DEPOSIT-DATE
    MOVE 20200101 TO WS-HR-ACCT-OPEN-DATE
    MOVE "N" TO WS-HR-IS-REDEPOSIT
    MOVE "N" TO WS-HR-REPEATED-OD
    MOVE 20260218 TO WS-EXPECTED-RELEASE
    CALL "HOLDCALC0" USING WS-HOLD-REQUEST HOLD-RECORD
                           WS-HOLD-RESULT
    IF WS-HOLD-RESULT-CODE = "E0000"
        IF WS-HOLD-RELEASE-DT = WS-EXPECTED-RELEASE
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " release=" WS-HOLD-RELEASE-DT
                " expected=" WS-EXPECTED-RELEASE
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-HOLD-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> HD-002: Non-local check Mon 2/16, 5 biz days -> Mon 2/23
*> ---------------------------------------------------------------
TEST-HD-002.
    ADD 1 TO WS-TEST-COUNT
    MOVE "HD-002: Non-local chk Mon +5 biz days = Mon"
        TO WS-TEST-NAME
    INITIALIZE WS-HOLD-REQUEST
    INITIALIZE HOLD-RECORD
    INITIALIZE WS-HOLD-RESULT
    MOVE 100000000001 TO WS-HR-ACCT-NUMBER
    MOVE 1000.00 TO WS-HR-DEPOSIT-AMT
    MOVE "NL" TO WS-HR-CHECK-TYPE
    MOVE 20260216 TO WS-HR-DEPOSIT-DATE
    MOVE 20200101 TO WS-HR-ACCT-OPEN-DATE
    MOVE "N" TO WS-HR-IS-REDEPOSIT
    MOVE "N" TO WS-HR-REPEATED-OD
    MOVE 20260223 TO WS-EXPECTED-RELEASE
    CALL "HOLDCALC0" USING WS-HOLD-REQUEST HOLD-RECORD
                           WS-HOLD-RESULT
    IF WS-HOLD-RESULT-CODE = "E0000"
        IF WS-HOLD-RELEASE-DT = WS-EXPECTED-RELEASE
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " release=" WS-HOLD-RELEASE-DT
                " expected=" WS-EXPECTED-RELEASE
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-HOLD-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> HD-003: Local check Thu 2/19, 2 biz days -> Mon 2/23
*>         (skips Sat 2/21 and Sun 2/22)
*> ---------------------------------------------------------------
TEST-HD-003.
    ADD 1 TO WS-TEST-COUNT
    MOVE "HD-003: Local chk Thu +2 biz days = Mon (wknd)"
        TO WS-TEST-NAME
    INITIALIZE WS-HOLD-REQUEST
    INITIALIZE HOLD-RECORD
    INITIALIZE WS-HOLD-RESULT
    MOVE 100000000001 TO WS-HR-ACCT-NUMBER
    MOVE 500.00 TO WS-HR-DEPOSIT-AMT
    MOVE "LC" TO WS-HR-CHECK-TYPE
    MOVE 20260219 TO WS-HR-DEPOSIT-DATE
    MOVE 20200101 TO WS-HR-ACCT-OPEN-DATE
    MOVE "N" TO WS-HR-IS-REDEPOSIT
    MOVE "N" TO WS-HR-REPEATED-OD
    MOVE 20260223 TO WS-EXPECTED-RELEASE
    CALL "HOLDCALC0" USING WS-HOLD-REQUEST HOLD-RECORD
                           WS-HOLD-RESULT
    IF WS-HOLD-RESULT-CODE = "E0000"
        IF WS-HOLD-RELEASE-DT = WS-EXPECTED-RELEASE
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " release=" WS-HOLD-RELEASE-DT
                " expected=" WS-EXPECTED-RELEASE
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-HOLD-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> HD-004: Treasury check Wed 2/18 $10K, 2 biz days -> Fri 2/20
*>         (above $5,525 threshold, treasury gets 2 biz day hold)
*> ---------------------------------------------------------------
TEST-HD-004.
    ADD 1 TO WS-TEST-COUNT
    MOVE "HD-004: Treasury chk Wed +2 biz days = Fri"
        TO WS-TEST-NAME
    INITIALIZE WS-HOLD-REQUEST
    INITIALIZE HOLD-RECORD
    INITIALIZE WS-HOLD-RESULT
    MOVE 100000000001 TO WS-HR-ACCT-NUMBER
    MOVE 10000.00 TO WS-HR-DEPOSIT-AMT
    MOVE "TR" TO WS-HR-CHECK-TYPE
    MOVE 20260218 TO WS-HR-DEPOSIT-DATE
    MOVE 20200101 TO WS-HR-ACCT-OPEN-DATE
    MOVE "N" TO WS-HR-IS-REDEPOSIT
    MOVE "N" TO WS-HR-REPEATED-OD
    MOVE 20260220 TO WS-EXPECTED-RELEASE
    CALL "HOLDCALC0" USING WS-HOLD-REQUEST HOLD-RECORD
                           WS-HOLD-RESULT
    IF WS-HOLD-RESULT-CODE = "E0000"
        IF WS-HOLD-RELEASE-DT = WS-EXPECTED-RELEASE
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " release=" WS-HOLD-RELEASE-DT
                " expected=" WS-EXPECTED-RELEASE
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-HOLD-RESULT-CODE
    END-IF.

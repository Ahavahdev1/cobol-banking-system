IDENTIFICATION DIVISION.
PROGRAM-ID. TEST-HOLDCALC.
*> ================================================================
*> TEST-HOLDCALC - Test suite for HOLDCALC0 Reg CC Hold Calc
*> Tests: Local/non-local checks, treasury, large deposits, cash,
*>        wire, new accounts, exceptions (18 tests HC-001 to HC-018)
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

01  WS-EXPECTED-NEXT-DAY   PIC S9(13)V99.
01  WS-EXPECTED-REMAINDER  PIC S9(13)V99.

PROCEDURE DIVISION.
MAIN-PROGRAM.
    DISPLAY "========================================".
    DISPLAY "TEST SUITE: HOLDCALC - Reg CC Holds".
    DISPLAY "========================================".

    PERFORM TEST-HC-001
    PERFORM TEST-HC-002
    PERFORM TEST-HC-003
    PERFORM TEST-HC-004
    PERFORM TEST-HC-005
    PERFORM TEST-HC-006
    PERFORM TEST-HC-007
    PERFORM TEST-HC-008
    PERFORM TEST-HC-009
    PERFORM TEST-HC-010
    PERFORM TEST-HC-011
    PERFORM TEST-HC-012
    PERFORM TEST-HC-013
    PERFORM TEST-HC-014
    PERFORM TEST-HC-015
    PERFORM TEST-HC-016
    PERFORM TEST-HC-017
    PERFORM TEST-HC-018

    DISPLAY "========================================".
    DISPLAY "RESULTS: " WS-PASS-COUNT "/" WS-TEST-COUNT
            " PASSED".
    DISPLAY "         " WS-FAIL-COUNT " FAILED".
    DISPLAY "========================================".
    MOVE WS-FAIL-COUNT TO RETURN-CODE
    STOP RUN.

*> ---------------------------------------------------------------
*> HC-001: Local check $1,000: next-day=$225, remainder=$775,
*>         release=2 biz days
*> ---------------------------------------------------------------
TEST-HC-001.
    ADD 1 TO WS-TEST-COUNT
    MOVE "HC-001: Local chk $1K std hold" TO WS-TEST-NAME
    INITIALIZE WS-HOLD-REQUEST
    INITIALIZE HOLD-RECORD
    INITIALIZE WS-HOLD-RESULT
    MOVE 100000000001 TO WS-HR-ACCT-NUMBER
    MOVE 1000.00 TO WS-HR-DEPOSIT-AMT
    MOVE "LC" TO WS-HR-CHECK-TYPE
    MOVE 20260215 TO WS-HR-DEPOSIT-DATE
    MOVE 20200101 TO WS-HR-ACCT-OPEN-DATE
    MOVE "N" TO WS-HR-IS-REDEPOSIT
    MOVE "N" TO WS-HR-REPEATED-OD
    MOVE 225.00 TO WS-EXPECTED-NEXT-DAY
    MOVE 775.00 TO WS-EXPECTED-REMAINDER
    CALL "HOLDCALC0" USING WS-HOLD-REQUEST HOLD-RECORD
                           WS-HOLD-RESULT
    IF WS-HOLD-RESULT-CODE = "E0000"
        IF WS-HOLD-NEXT-DAY-AMT = WS-EXPECTED-NEXT-DAY
            AND WS-HOLD-REMAINING-AMT = WS-EXPECTED-REMAINDER
            AND WS-HOLD-EXCEPTION-FLAG = "N"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " nxt=" WS-HOLD-NEXT-DAY-AMT
                " rem=" WS-HOLD-REMAINING-AMT
                " exc=" WS-HOLD-EXCEPTION-FLAG
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-HOLD-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> HC-002: Non-local check $1,000: next-day=$225, remainder=$775,
*>         release=5 biz days
*> ---------------------------------------------------------------
TEST-HC-002.
    ADD 1 TO WS-TEST-COUNT
    MOVE "HC-002: Non-local chk $1K 5-day hold" TO WS-TEST-NAME
    INITIALIZE WS-HOLD-REQUEST
    INITIALIZE HOLD-RECORD
    INITIALIZE WS-HOLD-RESULT
    MOVE 100000000001 TO WS-HR-ACCT-NUMBER
    MOVE 1000.00 TO WS-HR-DEPOSIT-AMT
    MOVE "NL" TO WS-HR-CHECK-TYPE
    MOVE 20260215 TO WS-HR-DEPOSIT-DATE
    MOVE 20200101 TO WS-HR-ACCT-OPEN-DATE
    MOVE "N" TO WS-HR-IS-REDEPOSIT
    MOVE "N" TO WS-HR-REPEATED-OD
    MOVE 225.00 TO WS-EXPECTED-NEXT-DAY
    MOVE 775.00 TO WS-EXPECTED-REMAINDER
    CALL "HOLDCALC0" USING WS-HOLD-REQUEST HOLD-RECORD
                           WS-HOLD-RESULT
    IF WS-HOLD-RESULT-CODE = "E0000"
        IF WS-HOLD-NEXT-DAY-AMT = WS-EXPECTED-NEXT-DAY
            AND WS-HOLD-REMAINING-AMT = WS-EXPECTED-REMAINDER
            AND WS-HOLD-EXCEPTION-FLAG = "N"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " nxt=" WS-HOLD-NEXT-DAY-AMT
                " rem=" WS-HOLD-REMAINING-AMT
                " exc=" WS-HOLD-EXCEPTION-FLAG
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-HOLD-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> HC-003: Treasury/cashier check $10,000: next-day=$5,525,
*>         remainder=$4,475, release=next biz day
*> ---------------------------------------------------------------
TEST-HC-003.
    ADD 1 TO WS-TEST-COUNT
    MOVE "HC-003: Treasury chk $10K" TO WS-TEST-NAME
    INITIALIZE WS-HOLD-REQUEST
    INITIALIZE HOLD-RECORD
    INITIALIZE WS-HOLD-RESULT
    MOVE 100000000001 TO WS-HR-ACCT-NUMBER
    MOVE 10000.00 TO WS-HR-DEPOSIT-AMT
    MOVE "TR" TO WS-HR-CHECK-TYPE
    MOVE 20260215 TO WS-HR-DEPOSIT-DATE
    MOVE 20200101 TO WS-HR-ACCT-OPEN-DATE
    MOVE "N" TO WS-HR-IS-REDEPOSIT
    MOVE "N" TO WS-HR-REPEATED-OD
    MOVE 5525.00 TO WS-EXPECTED-NEXT-DAY
    MOVE 4475.00 TO WS-EXPECTED-REMAINDER
    CALL "HOLDCALC0" USING WS-HOLD-REQUEST HOLD-RECORD
                           WS-HOLD-RESULT
    IF WS-HOLD-RESULT-CODE = "E0000"
        IF WS-HOLD-NEXT-DAY-AMT = WS-EXPECTED-NEXT-DAY
            AND WS-HOLD-REMAINING-AMT = WS-EXPECTED-REMAINDER
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " nxt=" WS-HOLD-NEXT-DAY-AMT
                " rem=" WS-HOLD-REMAINING-AMT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-HOLD-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> HC-004: Large deposit >$5,525 ($5,525.01): exception hold
*> ---------------------------------------------------------------
TEST-HC-004.
    ADD 1 TO WS-TEST-COUNT
    MOVE "HC-004: Large deposit $5525.01 exception"
        TO WS-TEST-NAME
    INITIALIZE WS-HOLD-REQUEST
    INITIALIZE HOLD-RECORD
    INITIALIZE WS-HOLD-RESULT
    MOVE 100000000001 TO WS-HR-ACCT-NUMBER
    MOVE 5525.01 TO WS-HR-DEPOSIT-AMT
    MOVE "LC" TO WS-HR-CHECK-TYPE
    MOVE 20260215 TO WS-HR-DEPOSIT-DATE
    MOVE 20200101 TO WS-HR-ACCT-OPEN-DATE
    MOVE "N" TO WS-HR-IS-REDEPOSIT
    MOVE "N" TO WS-HR-REPEATED-OD
    CALL "HOLDCALC0" USING WS-HOLD-REQUEST HOLD-RECORD
                           WS-HOLD-RESULT
    IF WS-HOLD-RESULT-CODE = "E0000"
        IF WS-HOLD-EXCEPTION-FLAG = "Y"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " exception=" WS-HOLD-EXCEPTION-FLAG
                " expected=Y"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-HOLD-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> HC-005: Large deposit boundary $5,525.00 = standard (no exc)
*> ---------------------------------------------------------------
TEST-HC-005.
    ADD 1 TO WS-TEST-COUNT
    MOVE "HC-005: Boundary $5525.00 no exception"
        TO WS-TEST-NAME
    INITIALIZE WS-HOLD-REQUEST
    INITIALIZE HOLD-RECORD
    INITIALIZE WS-HOLD-RESULT
    MOVE 100000000001 TO WS-HR-ACCT-NUMBER
    MOVE 5525.00 TO WS-HR-DEPOSIT-AMT
    MOVE "LC" TO WS-HR-CHECK-TYPE
    MOVE 20260215 TO WS-HR-DEPOSIT-DATE
    MOVE 20200101 TO WS-HR-ACCT-OPEN-DATE
    MOVE "N" TO WS-HR-IS-REDEPOSIT
    MOVE "N" TO WS-HR-REPEATED-OD
    CALL "HOLDCALC0" USING WS-HOLD-REQUEST HOLD-RECORD
                           WS-HOLD-RESULT
    IF WS-HOLD-RESULT-CODE = "E0000"
        IF WS-HOLD-EXCEPTION-FLAG = "N"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " exception=" WS-HOLD-EXCEPTION-FLAG
                " expected=N"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-HOLD-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> HC-006: New account (<30 days old): exception hold
*> ---------------------------------------------------------------
TEST-HC-006.
    ADD 1 TO WS-TEST-COUNT
    MOVE "HC-006: New acct <30 days exception" TO WS-TEST-NAME
    INITIALIZE WS-HOLD-REQUEST
    INITIALIZE HOLD-RECORD
    INITIALIZE WS-HOLD-RESULT
    MOVE 100000000001 TO WS-HR-ACCT-NUMBER
    MOVE 1000.00 TO WS-HR-DEPOSIT-AMT
    MOVE "LC" TO WS-HR-CHECK-TYPE
    MOVE 20260215 TO WS-HR-DEPOSIT-DATE
    *> Opened 15 days ago (< 30 day threshold)
    MOVE 20260201 TO WS-HR-ACCT-OPEN-DATE
    MOVE "N" TO WS-HR-IS-REDEPOSIT
    MOVE "N" TO WS-HR-REPEATED-OD
    CALL "HOLDCALC0" USING WS-HOLD-REQUEST HOLD-RECORD
                           WS-HOLD-RESULT
    IF WS-HOLD-RESULT-CODE = "E0000"
        IF WS-HOLD-EXCEPTION-FLAG = "Y"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " exception=" WS-HOLD-EXCEPTION-FLAG
                " expected=Y"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-HOLD-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> HC-007: Redeposited check: exception hold
*> ---------------------------------------------------------------
TEST-HC-007.
    ADD 1 TO WS-TEST-COUNT
    MOVE "HC-007: Redeposited check exception" TO WS-TEST-NAME
    INITIALIZE WS-HOLD-REQUEST
    INITIALIZE HOLD-RECORD
    INITIALIZE WS-HOLD-RESULT
    MOVE 100000000001 TO WS-HR-ACCT-NUMBER
    MOVE 1000.00 TO WS-HR-DEPOSIT-AMT
    MOVE "LC" TO WS-HR-CHECK-TYPE
    MOVE 20260215 TO WS-HR-DEPOSIT-DATE
    MOVE 20200101 TO WS-HR-ACCT-OPEN-DATE
    MOVE "Y" TO WS-HR-IS-REDEPOSIT
    MOVE "N" TO WS-HR-REPEATED-OD
    CALL "HOLDCALC0" USING WS-HOLD-REQUEST HOLD-RECORD
                           WS-HOLD-RESULT
    IF WS-HOLD-RESULT-CODE = "E0000"
        IF WS-HOLD-EXCEPTION-FLAG = "Y"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " exception=" WS-HOLD-EXCEPTION-FLAG
                " expected=Y"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-HOLD-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> HC-008: Repeated overdraft customer: exception hold
*> ---------------------------------------------------------------
TEST-HC-008.
    ADD 1 TO WS-TEST-COUNT
    MOVE "HC-008: Repeated OD exception" TO WS-TEST-NAME
    INITIALIZE WS-HOLD-REQUEST
    INITIALIZE HOLD-RECORD
    INITIALIZE WS-HOLD-RESULT
    MOVE 100000000001 TO WS-HR-ACCT-NUMBER
    MOVE 1000.00 TO WS-HR-DEPOSIT-AMT
    MOVE "LC" TO WS-HR-CHECK-TYPE
    MOVE 20260215 TO WS-HR-DEPOSIT-DATE
    MOVE 20200101 TO WS-HR-ACCT-OPEN-DATE
    MOVE "N" TO WS-HR-IS-REDEPOSIT
    MOVE "Y" TO WS-HR-REPEATED-OD
    CALL "HOLDCALC0" USING WS-HOLD-REQUEST HOLD-RECORD
                           WS-HOLD-RESULT
    IF WS-HOLD-RESULT-CODE = "E0000"
        IF WS-HOLD-EXCEPTION-FLAG = "Y"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " exception=" WS-HOLD-EXCEPTION-FLAG
                " expected=Y"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-HOLD-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> HC-009: Cash deposit: no hold (next-day = full amount)
*> ---------------------------------------------------------------
TEST-HC-009.
    ADD 1 TO WS-TEST-COUNT
    MOVE "HC-009: Cash deposit no hold" TO WS-TEST-NAME
    INITIALIZE WS-HOLD-REQUEST
    INITIALIZE HOLD-RECORD
    INITIALIZE WS-HOLD-RESULT
    MOVE 100000000001 TO WS-HR-ACCT-NUMBER
    MOVE 5000.00 TO WS-HR-DEPOSIT-AMT
    MOVE "CS" TO WS-HR-CHECK-TYPE
    MOVE 20260215 TO WS-HR-DEPOSIT-DATE
    MOVE 20200101 TO WS-HR-ACCT-OPEN-DATE
    MOVE "N" TO WS-HR-IS-REDEPOSIT
    MOVE "N" TO WS-HR-REPEATED-OD
    MOVE 5000.00 TO WS-EXPECTED-NEXT-DAY
    MOVE 0.00 TO WS-EXPECTED-REMAINDER
    CALL "HOLDCALC0" USING WS-HOLD-REQUEST HOLD-RECORD
                           WS-HOLD-RESULT
    IF WS-HOLD-RESULT-CODE = "E0000"
        IF WS-HOLD-NEXT-DAY-AMT = WS-EXPECTED-NEXT-DAY
            AND WS-HOLD-REMAINING-AMT = WS-EXPECTED-REMAINDER
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " nxt=" WS-HOLD-NEXT-DAY-AMT
                " rem=" WS-HOLD-REMAINING-AMT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-HOLD-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> HC-010: Wire deposit: no hold at all
*> ---------------------------------------------------------------
TEST-HC-010.
    ADD 1 TO WS-TEST-COUNT
    MOVE "HC-010: Wire deposit no hold" TO WS-TEST-NAME
    INITIALIZE WS-HOLD-REQUEST
    INITIALIZE HOLD-RECORD
    INITIALIZE WS-HOLD-RESULT
    MOVE 100000000001 TO WS-HR-ACCT-NUMBER
    MOVE 50000.00 TO WS-HR-DEPOSIT-AMT
    MOVE "WR" TO WS-HR-CHECK-TYPE
    MOVE 20260215 TO WS-HR-DEPOSIT-DATE
    MOVE 20200101 TO WS-HR-ACCT-OPEN-DATE
    MOVE "N" TO WS-HR-IS-REDEPOSIT
    MOVE "N" TO WS-HR-REPEATED-OD
    MOVE 50000.00 TO WS-EXPECTED-NEXT-DAY
    MOVE 0.00 TO WS-EXPECTED-REMAINDER
    CALL "HOLDCALC0" USING WS-HOLD-REQUEST HOLD-RECORD
                           WS-HOLD-RESULT
    IF WS-HOLD-RESULT-CODE = "E0000"
        IF WS-HOLD-NEXT-DAY-AMT = WS-EXPECTED-NEXT-DAY
            AND WS-HOLD-REMAINING-AMT = WS-EXPECTED-REMAINDER
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " nxt=" WS-HOLD-NEXT-DAY-AMT
                " rem=" WS-HOLD-REMAINING-AMT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-HOLD-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> HC-011: Local check $200 (under $225): next-day=$200, no rem
*> ---------------------------------------------------------------
TEST-HC-011.
    ADD 1 TO WS-TEST-COUNT
    MOVE "HC-011: Local chk $200 < $225 all avail"
        TO WS-TEST-NAME
    INITIALIZE WS-HOLD-REQUEST
    INITIALIZE HOLD-RECORD
    INITIALIZE WS-HOLD-RESULT
    MOVE 100000000001 TO WS-HR-ACCT-NUMBER
    MOVE 200.00 TO WS-HR-DEPOSIT-AMT
    MOVE "LC" TO WS-HR-CHECK-TYPE
    MOVE 20260215 TO WS-HR-DEPOSIT-DATE
    MOVE 20200101 TO WS-HR-ACCT-OPEN-DATE
    MOVE "N" TO WS-HR-IS-REDEPOSIT
    MOVE "N" TO WS-HR-REPEATED-OD
    MOVE 200.00 TO WS-EXPECTED-NEXT-DAY
    MOVE 0.00 TO WS-EXPECTED-REMAINDER
    CALL "HOLDCALC0" USING WS-HOLD-REQUEST HOLD-RECORD
                           WS-HOLD-RESULT
    IF WS-HOLD-RESULT-CODE = "E0000"
        IF WS-HOLD-NEXT-DAY-AMT = WS-EXPECTED-NEXT-DAY
            AND WS-HOLD-REMAINING-AMT = WS-EXPECTED-REMAINDER
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " nxt=" WS-HOLD-NEXT-DAY-AMT
                " rem=" WS-HOLD-REMAINING-AMT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-HOLD-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> HC-012: Non-local check exactly $225: next-day=$225, rem=$0
*> ---------------------------------------------------------------
TEST-HC-012.
    ADD 1 TO WS-TEST-COUNT
    MOVE "HC-012: Non-local $225 exact boundary"
        TO WS-TEST-NAME
    INITIALIZE WS-HOLD-REQUEST
    INITIALIZE HOLD-RECORD
    INITIALIZE WS-HOLD-RESULT
    MOVE 100000000001 TO WS-HR-ACCT-NUMBER
    MOVE 225.00 TO WS-HR-DEPOSIT-AMT
    MOVE "NL" TO WS-HR-CHECK-TYPE
    MOVE 20260215 TO WS-HR-DEPOSIT-DATE
    MOVE 20200101 TO WS-HR-ACCT-OPEN-DATE
    MOVE "N" TO WS-HR-IS-REDEPOSIT
    MOVE "N" TO WS-HR-REPEATED-OD
    MOVE 225.00 TO WS-EXPECTED-NEXT-DAY
    MOVE 0.00 TO WS-EXPECTED-REMAINDER
    CALL "HOLDCALC0" USING WS-HOLD-REQUEST HOLD-RECORD
                           WS-HOLD-RESULT
    IF WS-HOLD-RESULT-CODE = "E0000"
        IF WS-HOLD-NEXT-DAY-AMT = WS-EXPECTED-NEXT-DAY
            AND WS-HOLD-REMAINING-AMT = WS-EXPECTED-REMAINDER
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " nxt=" WS-HOLD-NEXT-DAY-AMT
                " rem=" WS-HOLD-REMAINING-AMT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-HOLD-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> HC-013: Large local check $6,000: next-day=$225, rem=$5,775,
*>         exception flag set
*> ---------------------------------------------------------------
TEST-HC-013.
    ADD 1 TO WS-TEST-COUNT
    MOVE "HC-013: Large local $6K exception" TO WS-TEST-NAME
    INITIALIZE WS-HOLD-REQUEST
    INITIALIZE HOLD-RECORD
    INITIALIZE WS-HOLD-RESULT
    MOVE 100000000001 TO WS-HR-ACCT-NUMBER
    MOVE 6000.00 TO WS-HR-DEPOSIT-AMT
    MOVE "LC" TO WS-HR-CHECK-TYPE
    MOVE 20260215 TO WS-HR-DEPOSIT-DATE
    MOVE 20200101 TO WS-HR-ACCT-OPEN-DATE
    MOVE "N" TO WS-HR-IS-REDEPOSIT
    MOVE "N" TO WS-HR-REPEATED-OD
    MOVE 225.00 TO WS-EXPECTED-NEXT-DAY
    MOVE 5775.00 TO WS-EXPECTED-REMAINDER
    CALL "HOLDCALC0" USING WS-HOLD-REQUEST HOLD-RECORD
                           WS-HOLD-RESULT
    IF WS-HOLD-RESULT-CODE = "E0000"
        IF WS-HOLD-NEXT-DAY-AMT = WS-EXPECTED-NEXT-DAY
            AND WS-HOLD-REMAINING-AMT = WS-EXPECTED-REMAINDER
            AND WS-HOLD-EXCEPTION-FLAG = "Y"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " nxt=" WS-HOLD-NEXT-DAY-AMT
                " rem=" WS-HOLD-REMAINING-AMT
                " exc=" WS-HOLD-EXCEPTION-FLAG
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-HOLD-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> HC-014: Multiple exception conditions (new acct + large deposit
*>         + repeated OD) should still compute valid hold
*> ---------------------------------------------------------------
TEST-HC-014.
    ADD 1 TO WS-TEST-COUNT
    MOVE "HC-014: Multi-exception valid hold"
        TO WS-TEST-NAME
    INITIALIZE WS-HOLD-REQUEST
    INITIALIZE HOLD-RECORD
    INITIALIZE WS-HOLD-RESULT
    MOVE 100000000001 TO WS-HR-ACCT-NUMBER
    MOVE 6000.00 TO WS-HR-DEPOSIT-AMT
    MOVE "LC" TO WS-HR-CHECK-TYPE
    MOVE 20260226 TO WS-HR-DEPOSIT-DATE
    *> New account: opened 10 days ago (< 30 day threshold)
    MOVE 20260216 TO WS-HR-ACCT-OPEN-DATE
    MOVE "N" TO WS-HR-IS-REDEPOSIT
    MOVE "Y" TO WS-HR-REPEATED-OD
    CALL "HOLDCALC0" USING WS-HOLD-REQUEST HOLD-RECORD
                           WS-HOLD-RESULT
    IF WS-HOLD-RESULT-CODE = "E0000"
        IF WS-HOLD-EXCEPTION-FLAG = "Y"
            AND WS-HOLD-REMAINING-AMT > 0
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " rem=" WS-HOLD-REMAINING-AMT
                " exc=" WS-HOLD-EXCEPTION-FLAG
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " rem=" WS-HOLD-REMAINING-AMT
                " exc=" WS-HOLD-EXCEPTION-FLAG
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-HOLD-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> HC-015: Zero deposit amount ($0.00, local check)
*>         next-day = $0, remainder = $0
*> ---------------------------------------------------------------
TEST-HC-015.
    ADD 1 TO WS-TEST-COUNT
    MOVE "HC-015: Zero deposit $0 local check"
        TO WS-TEST-NAME
    INITIALIZE WS-HOLD-REQUEST
    INITIALIZE HOLD-RECORD
    INITIALIZE WS-HOLD-RESULT
    MOVE 100000000001 TO WS-HR-ACCT-NUMBER
    MOVE 0.00 TO WS-HR-DEPOSIT-AMT
    MOVE "LC" TO WS-HR-CHECK-TYPE
    MOVE 20260215 TO WS-HR-DEPOSIT-DATE
    MOVE 20200101 TO WS-HR-ACCT-OPEN-DATE
    MOVE "N" TO WS-HR-IS-REDEPOSIT
    MOVE "N" TO WS-HR-REPEATED-OD
    MOVE 0.00 TO WS-EXPECTED-NEXT-DAY
    MOVE 0.00 TO WS-EXPECTED-REMAINDER
    CALL "HOLDCALC0" USING WS-HOLD-REQUEST HOLD-RECORD
                           WS-HOLD-RESULT
    IF WS-HOLD-RESULT-CODE = "E0000"
        IF WS-HOLD-NEXT-DAY-AMT = WS-EXPECTED-NEXT-DAY
            AND WS-HOLD-REMAINING-AMT = WS-EXPECTED-REMAINDER
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " nxt=" WS-HOLD-NEXT-DAY-AMT
                " rem=" WS-HOLD-REMAINING-AMT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-HOLD-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> HC-016: Cashier check "CA" $10,000: same as treasury
*>         next-day=$5,525, remainder=$4,475
*> ---------------------------------------------------------------
TEST-HC-016.
    ADD 1 TO WS-TEST-COUNT
    MOVE "HC-016: Cashier chk $10K same as treasury"
        TO WS-TEST-NAME
    INITIALIZE WS-HOLD-REQUEST
    INITIALIZE HOLD-RECORD
    INITIALIZE WS-HOLD-RESULT
    MOVE 100000000001 TO WS-HR-ACCT-NUMBER
    MOVE 10000.00 TO WS-HR-DEPOSIT-AMT
    MOVE "CA" TO WS-HR-CHECK-TYPE
    MOVE 20260215 TO WS-HR-DEPOSIT-DATE
    MOVE 20200101 TO WS-HR-ACCT-OPEN-DATE
    MOVE "N" TO WS-HR-IS-REDEPOSIT
    MOVE "N" TO WS-HR-REPEATED-OD
    MOVE 5525.00 TO WS-EXPECTED-NEXT-DAY
    MOVE 4475.00 TO WS-EXPECTED-REMAINDER
    CALL "HOLDCALC0" USING WS-HOLD-REQUEST HOLD-RECORD
                           WS-HOLD-RESULT
    IF WS-HOLD-RESULT-CODE = "E0000"
        IF WS-HOLD-NEXT-DAY-AMT = WS-EXPECTED-NEXT-DAY
            AND WS-HOLD-REMAINING-AMT = WS-EXPECTED-REMAINDER
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " nxt=" WS-HOLD-NEXT-DAY-AMT
                " rem=" WS-HOLD-REMAINING-AMT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-HOLD-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> HC-017: Unknown check type "XX" -> WHEN OTHER path
*>         Treated as check deposit: next-day=$225, rem=$775
*> ---------------------------------------------------------------
TEST-HC-017.
    ADD 1 TO WS-TEST-COUNT
    MOVE "HC-017: Unknown type XX -> check deposit"
        TO WS-TEST-NAME
    INITIALIZE WS-HOLD-REQUEST
    INITIALIZE HOLD-RECORD
    INITIALIZE WS-HOLD-RESULT
    MOVE 100000000001 TO WS-HR-ACCT-NUMBER
    MOVE 1000.00 TO WS-HR-DEPOSIT-AMT
    MOVE "XX" TO WS-HR-CHECK-TYPE
    MOVE 20260215 TO WS-HR-DEPOSIT-DATE
    MOVE 20200101 TO WS-HR-ACCT-OPEN-DATE
    MOVE "N" TO WS-HR-IS-REDEPOSIT
    MOVE "N" TO WS-HR-REPEATED-OD
    MOVE 225.00 TO WS-EXPECTED-NEXT-DAY
    MOVE 775.00 TO WS-EXPECTED-REMAINDER
    CALL "HOLDCALC0" USING WS-HOLD-REQUEST HOLD-RECORD
                           WS-HOLD-RESULT
    IF WS-HOLD-RESULT-CODE = "E0000"
        IF WS-HOLD-NEXT-DAY-AMT = WS-EXPECTED-NEXT-DAY
            AND WS-HOLD-REMAINING-AMT = WS-EXPECTED-REMAINDER
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " nxt=" WS-HOLD-NEXT-DAY-AMT
                " rem=" WS-HOLD-REMAINING-AMT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-HOLD-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> HC-018: Treasury check on new account -> exception hold
*> Reg CC 229.13: new account (<30 days) gets +7 exception days
*> Treasury base=2 + exception=7 = 9 business days total hold
*> ---------------------------------------------------------------
TEST-HC-018.
    ADD 1 TO WS-TEST-COUNT
    MOVE "HC-018: Treasury + new acct exception"
        TO WS-TEST-NAME
    INITIALIZE WS-HOLD-REQUEST
    INITIALIZE HOLD-RECORD
    INITIALIZE WS-HOLD-RESULT
    MOVE 100000000001 TO WS-HR-ACCT-NUMBER
    MOVE 10000.00 TO WS-HR-DEPOSIT-AMT
    MOVE "TR" TO WS-HR-CHECK-TYPE
    MOVE 20260215 TO WS-HR-DEPOSIT-DATE
    *> Account opened 10 days ago (< 30 = new account)
    MOVE 20260205 TO WS-HR-ACCT-OPEN-DATE
    MOVE "N" TO WS-HR-IS-REDEPOSIT
    MOVE "N" TO WS-HR-REPEATED-OD
    MOVE 5525.00 TO WS-EXPECTED-NEXT-DAY
    MOVE 4475.00 TO WS-EXPECTED-REMAINDER
    CALL "HOLDCALC0" USING WS-HOLD-REQUEST HOLD-RECORD
                           WS-HOLD-RESULT
    IF WS-HOLD-RESULT-CODE = "E0000"
        IF WS-HOLD-NEXT-DAY-AMT = WS-EXPECTED-NEXT-DAY
            AND WS-HOLD-REMAINING-AMT = WS-EXPECTED-REMAINDER
            AND WS-HOLD-EXCEPTION-FLAG = "Y"
            AND WS-HOLD-RELEASE-DT > 20260215
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " exc=" WS-HOLD-EXCEPTION-FLAG
                " rel=" WS-HOLD-RELEASE-DT
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " exc=" WS-HOLD-EXCEPTION-FLAG
                " rel=" WS-HOLD-RELEASE-DT
                " nxt=" WS-HOLD-NEXT-DAY-AMT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-HOLD-RESULT-CODE
    END-IF.

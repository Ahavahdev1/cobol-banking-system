IDENTIFICATION DIVISION.
PROGRAM-ID. TEST-BATCH-EOM.
*> ================================================================
*> TEST-BATCH-EOM - Integration test for EOMPROC0 End-of-Month
*> Tests: EOM cycle with fee assessment, MTD reset, batch status,
*>        closed-account skip, statement dates, YTD reset (16 tests)
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-TEST-COUNT          PIC 9(3) VALUE 0.
01  WS-PASS-COUNT          PIC 9(3) VALUE 0.
01  WS-FAIL-COUNT          PIC 9(3) VALUE 0.
01  WS-TEST-NAME           PIC X(60).

*> EOM batch areas
01  WS-BATCH-DATE          PIC 9(8).
COPY CPYACCT.
COPY CPYBATCH.
01  WS-BATCH-RESULT.
    05  WS-BATCH-RESULT-CODE  PIC X(5).
    05  WS-BATCH-RESULT-MSG   PIC X(50).

01  WS-SAVED-LEDGER-BAL    PIC S9(13)V99.
01  WS-SAVED-YTD-FEES     PIC S9(9)V99.

PROCEDURE DIVISION.
MAIN-PROGRAM.
    DISPLAY "========================================".
    DISPLAY "TEST SUITE: BATCH-EOM Integration".
    DISPLAY "========================================".

    PERFORM TEST-EM-001
    PERFORM TEST-EM-002
    PERFORM TEST-EM-003
    PERFORM TEST-EM-004
    PERFORM TEST-EM-005
    PERFORM TEST-EM-006
    PERFORM TEST-EM-007
    PERFORM TEST-EM-008
    PERFORM TEST-EM-009
    PERFORM TEST-EM-010
    PERFORM TEST-EM-011
    PERFORM TEST-EM-012
    PERFORM TEST-EM-013
    PERFORM TEST-EM-014
    PERFORM TEST-EM-015
    PERFORM TEST-EM-016

    DISPLAY "========================================".
    DISPLAY "RESULTS: " WS-PASS-COUNT "/" WS-TEST-COUNT
            " PASSED".
    DISPLAY "         " WS-FAIL-COUNT " FAILED".
    DISPLAY "========================================".
    MOVE WS-FAIL-COUNT TO RETURN-CODE
    STOP RUN.

*> ---------------------------------------------------------------
*> Helper: Set up active checking account for EOM testing
*> ---------------------------------------------------------------
SETUP-EOM-ACCOUNT.
    INITIALIZE ACCT-RECORD
    MOVE 000012345678 TO ACCT-NUMBER
    MOVE 2 TO ACCT-CHECK-DIGIT
    MOVE 1000000001 TO ACCT-PRIMARY-CIF
    MOVE "DDA1" TO ACCT-PRODUCT-CODE
    MOVE "D" TO ACCT-TYPE
    MOVE "CH" TO ACCT-SUB-TYPE
    MOVE 5000.00 TO ACCT-LEDGER-BAL
    MOVE 5000.00 TO ACCT-AVAIL-BAL
    MOVE 0 TO ACCT-HOLD-AMOUNT
    MOVE "A" TO ACCT-STATUS
    MOVE "N" TO ACCT-LEGAL-HOLD
    MOVE "N" TO ACCT-DECEASED
    MOVE "N" TO ACCT-GARNISHMENT
    MOVE 12.00 TO ACCT-MONTHLY-FEE
    MOVE "NW" TO ACCT-FEE-WAIVER-CODE
    MOVE 3 TO ACCT-NSF-COUNT-MTD
    MOVE 0 TO ACCT-NSF-COUNT-TODAY
    MOVE 4500.00 TO ACCT-MTD-AVG-BAL
    MOVE 2000.00 TO ACCT-MTD-LOW-BAL
    MOVE 20200101 TO ACCT-OPEN-DATE.

*> ---------------------------------------------------------------
*> EM-001: EOM batch completes successfully -> E0000
*> ---------------------------------------------------------------
TEST-EM-001.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EM-001: EOM batch completes -> E0000" TO WS-TEST-NAME
    PERFORM SETUP-EOM-ACCOUNT
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260228 TO WS-BATCH-DATE
    CALL "EOMPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF WS-BATCH-RESULT-CODE = "E0000"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-BATCH-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> EM-002: Batch status = C (complete)
*> ---------------------------------------------------------------
TEST-EM-002.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EM-002: Batch status = C (complete)" TO WS-TEST-NAME
    PERFORM SETUP-EOM-ACCOUNT
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260228 TO WS-BATCH-DATE
    CALL "EOMPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF BATCH-STATUS = "C"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " status=" BATCH-STATUS " expected=C"
    END-IF.

*> ---------------------------------------------------------------
*> EM-003: Batch type = EOM
*> ---------------------------------------------------------------
TEST-EM-003.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EM-003: Batch type = EOM" TO WS-TEST-NAME
    PERFORM SETUP-EOM-ACCOUNT
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260228 TO WS-BATCH-DATE
    CALL "EOMPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF BATCH-TYPE = "EOM"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " type=" BATCH-TYPE " expected=EOM"
    END-IF.

*> ---------------------------------------------------------------
*> EM-004: Monthly fee is assessed on active account
*> EOMPROC0 initializes FEE-SCHEDULE-RECORD from account data
*> (ACCT-MONTHLY-FEE = 12.00, FEE-WAIVER-CODE = "NW").
*> With no waiver, FEECALC0 should assess the full $12.00 fee.
*> Verify BATCH-FEES-ASSESSED > 0 and balance decreased.
*> ---------------------------------------------------------------
TEST-EM-004.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EM-004: Monthly fee assessed on active acct"
        TO WS-TEST-NAME
    PERFORM SETUP-EOM-ACCOUNT
    MOVE ACCT-LEDGER-BAL TO WS-SAVED-LEDGER-BAL
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260228 TO WS-BATCH-DATE
    CALL "EOMPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF WS-BATCH-RESULT-CODE = "E0000"
        IF BATCH-FEES-ASSESSED > 0
            AND ACCT-LEDGER-BAL < WS-SAVED-LEDGER-BAL
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " fees=" BATCH-FEES-ASSESSED
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " fees=" BATCH-FEES-ASSESSED
                " bal=" ACCT-LEDGER-BAL
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-BATCH-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> EM-005: Batch processed count = 1 for active account
*> ---------------------------------------------------------------
TEST-EM-005.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EM-005: Batch processed count = 1" TO WS-TEST-NAME
    PERFORM SETUP-EOM-ACCOUNT
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260228 TO WS-BATCH-DATE
    CALL "EOMPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF BATCH-ACCTS-PROCESSED = 1
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " processed=" BATCH-ACCTS-PROCESSED
            " expected=1"
    END-IF.

*> ---------------------------------------------------------------
*> EM-006: MTD counters reset after EOM processing
*> ACCT-NSF-COUNT-MTD, ACCT-MTD-AVG-BAL, ACCT-MTD-LOW-BAL
*> should all be zero after EOMPROC0 runs
*> ---------------------------------------------------------------
TEST-EM-006.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EM-006: MTD counters reset to zero" TO WS-TEST-NAME
    PERFORM SETUP-EOM-ACCOUNT
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260228 TO WS-BATCH-DATE
    CALL "EOMPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF ACCT-NSF-COUNT-MTD = 0
        AND ACCT-MTD-AVG-BAL = 0
        AND ACCT-MTD-LOW-BAL = 0
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " nsf=" ACCT-NSF-COUNT-MTD
            " avg=" ACCT-MTD-AVG-BAL
            " low=" ACCT-MTD-LOW-BAL
    END-IF.

*> ---------------------------------------------------------------
*> EM-007: Closed account is skipped (not processed)
*> ---------------------------------------------------------------
TEST-EM-007.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EM-007: Closed acct skipped" TO WS-TEST-NAME
    PERFORM SETUP-EOM-ACCOUNT
    MOVE "C" TO ACCT-STATUS
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260228 TO WS-BATCH-DATE
    CALL "EOMPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF BATCH-ACCTS-PROCESSED = 0
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " processed=" BATCH-ACCTS-PROCESSED
            " expected=0"
    END-IF.

*> ---------------------------------------------------------------
*> EM-008: Batch date = input date
*> ---------------------------------------------------------------
TEST-EM-008.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EM-008: Batch date = input date" TO WS-TEST-NAME
    PERFORM SETUP-EOM-ACCOUNT
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260228 TO WS-BATCH-DATE
    CALL "EOMPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF BATCH-DATE = 20260228
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " date=" BATCH-DATE
    END-IF.

*> ---------------------------------------------------------------
*> EM-009: EOM updates ACCT-LAST-STMT-DATE to batch date
*> ---------------------------------------------------------------
TEST-EM-009.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EM-009: ACCT-LAST-STMT-DATE = batch date"
        TO WS-TEST-NAME
    PERFORM SETUP-EOM-ACCOUNT
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260228 TO WS-BATCH-DATE
    CALL "EOMPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF ACCT-LAST-STMT-DATE = 20260228
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " last-stmt=" ACCT-LAST-STMT-DATE
            " expected=20260228"
    END-IF.

*> ---------------------------------------------------------------
*> EM-010: EOM sets ACCT-NEXT-STMT-DATE to next month
*> ---------------------------------------------------------------
TEST-EM-010.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EM-010: ACCT-NEXT-STMT-DATE = next month"
        TO WS-TEST-NAME
    PERFORM SETUP-EOM-ACCOUNT
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260228 TO WS-BATCH-DATE
    CALL "EOMPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF ACCT-NEXT-STMT-DATE = 20260328
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " next-stmt=" ACCT-NEXT-STMT-DATE
            " expected=20260328"
    END-IF.

*> ---------------------------------------------------------------
*> EM-011: December batch rolls statement date to January next year
*> Batch date = 20261231. Next statement date should become
*> 20270131 (month 12 + 1 = 13 -> month 1, year + 1; day 31
*> stays 31 since January has 31 days).
*> ---------------------------------------------------------------
TEST-EM-011.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EM-011: Dec->Jan year rollover on stmt date"
        TO WS-TEST-NAME
    PERFORM SETUP-EOM-ACCOUNT
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20261231 TO WS-BATCH-DATE
    CALL "EOMPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF WS-BATCH-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-BATCH-RESULT-CODE
            " " WS-BATCH-RESULT-MSG
        GO TO TEST-EM-011-EXIT
    END-IF
    *> Verify LAST-STMT-DATE = batch date (20261231)
    IF ACCT-LAST-STMT-DATE NOT = 20261231
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " last-stmt=" ACCT-LAST-STMT-DATE
            " expected=20261231"
        GO TO TEST-EM-011-EXIT
    END-IF
    *> Verify NEXT-STMT-DATE rolled to January 2027
    *> Dec(12)+1=13 -> month=1, year=2027, day=31 (Jan has 31 days)
    IF ACCT-NEXT-STMT-DATE = 20270131
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " next-stmt=" ACCT-NEXT-STMT-DATE
            " expected=20270131"
    END-IF.
TEST-EM-011-EXIT.
    CONTINUE.

*> ---------------------------------------------------------------
*> EM-012: December EOM resets all YTD counters
*> Year-end processing: batch date = 20261231 (December).
*> EOMPROC0 detects month=12 and zeros all five YTD fields:
*> ACCT-NSF-COUNT-YTD, ACCT-YTD-FEES-CHARGED,
*> ACCT-YTD-FEES-WAIVED, ACCT-YTD-INT-EARNED, ACCT-YTD-INT-PAID.
*> ---------------------------------------------------------------
TEST-EM-012.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EM-012: Dec EOM resets all YTD counters"
        TO WS-TEST-NAME
    PERFORM SETUP-EOM-ACCOUNT
    *> Populate YTD fields with non-zero values
    MOVE 5 TO ACCT-NSF-COUNT-YTD
    MOVE 144.00 TO ACCT-YTD-FEES-CHARGED
    MOVE 48.00 TO ACCT-YTD-FEES-WAIVED
    MOVE 2500.00 TO ACCT-YTD-INT-EARNED
    MOVE 1200.00 TO ACCT-YTD-INT-PAID
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20261231 TO WS-BATCH-DATE
    CALL "EOMPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF WS-BATCH-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-BATCH-RESULT-CODE
        GO TO TEST-EM-012-EXIT
    END-IF
    IF ACCT-NSF-COUNT-YTD = 0
        AND ACCT-YTD-FEES-CHARGED = 0
        AND ACCT-YTD-FEES-WAIVED = 0
        AND ACCT-YTD-INT-EARNED = 0
        AND ACCT-YTD-INT-PAID = 0
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " nsf-ytd=" ACCT-NSF-COUNT-YTD
            " fees-chg=" ACCT-YTD-FEES-CHARGED
            " fees-wv=" ACCT-YTD-FEES-WAIVED
            " int-earn=" ACCT-YTD-INT-EARNED
            " int-paid=" ACCT-YTD-INT-PAID
    END-IF.
TEST-EM-012-EXIT.
    CONTINUE.

*> ---------------------------------------------------------------
*> EM-013: November EOM does NOT reset YTD counters
*> Non-December month: batch date = 20261130 (November).
*> YTD counters must NOT be zeroed (only December resets them).
*> Note: ACCT-YTD-FEES-CHARGED increases by the $12.00 monthly
*> fee assessed during EOM (144.00 + 12.00 = 156.00).
*> ---------------------------------------------------------------
TEST-EM-013.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EM-013: Nov EOM keeps YTD counters intact"
        TO WS-TEST-NAME
    PERFORM SETUP-EOM-ACCOUNT
    *> Populate YTD fields with known non-zero values
    MOVE 5 TO ACCT-NSF-COUNT-YTD
    MOVE 144.00 TO ACCT-YTD-FEES-CHARGED
    MOVE 48.00 TO ACCT-YTD-FEES-WAIVED
    MOVE 2500.00 TO ACCT-YTD-INT-EARNED
    MOVE 1200.00 TO ACCT-YTD-INT-PAID
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20261130 TO WS-BATCH-DATE
    CALL "EOMPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF WS-BATCH-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-BATCH-RESULT-CODE
        GO TO TEST-EM-013-EXIT
    END-IF
    *> YTD-FEES-CHARGED = 144.00 + 12.00 fee assessed = 156.00
    *> All other YTD fields unchanged (not December -> no reset)
    IF ACCT-NSF-COUNT-YTD = 5
        AND ACCT-YTD-FEES-CHARGED = 156.00
        AND ACCT-YTD-FEES-WAIVED = 48.00
        AND ACCT-YTD-INT-EARNED = 2500.00
        AND ACCT-YTD-INT-PAID = 1200.00
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " nsf-ytd=" ACCT-NSF-COUNT-YTD
            " fees-chg=" ACCT-YTD-FEES-CHARGED
            " fees-wv=" ACCT-YTD-FEES-WAIVED
            " int-earn=" ACCT-YTD-INT-EARNED
            " int-paid=" ACCT-YTD-INT-PAID
    END-IF.
TEST-EM-013-EXIT.
    CONTINUE.

*> ---------------------------------------------------------------
*> EM-014: Fee posting failure rollback restores YTD fees
*> Set ACCT-DECEASED = "Y" so TXNPOST0 rejects the fee debit
*> (E0036) while EOMPROC0 still processes the account (status "A").
*> After rollback, ACCT-YTD-FEES-CHARGED should equal pre-fee value
*> and BATCH-ACCTS-ERRORS should be incremented.
*> ---------------------------------------------------------------
TEST-EM-014.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EM-014: Fee post failure rolls back YTD fees"
        TO WS-TEST-NAME
    PERFORM SETUP-EOM-ACCOUNT
    *> Set deceased flag to cause TXNPOST0 to reject the fee posting
    MOVE "Y" TO ACCT-DECEASED
    *> Pre-populate YTD fees to a known value
    MOVE 100.00 TO ACCT-YTD-FEES-CHARGED
    MOVE ACCT-YTD-FEES-CHARGED TO WS-SAVED-YTD-FEES
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260228 TO WS-BATCH-DATE
    CALL "EOMPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    *> EOMPROC0 should complete (possibly with errors)
    *> FEECALC0 succeeds and updates YTD, then TXNPOST0 fails,
    *> triggering rollback of YTD fees and error count increment.
    IF BATCH-ACCTS-ERRORS > 0
        AND ACCT-YTD-FEES-CHARGED = WS-SAVED-YTD-FEES
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " errors=" BATCH-ACCTS-ERRORS
            " ytd-fees=" ACCT-YTD-FEES-CHARGED
            " expected=" WS-SAVED-YTD-FEES
    END-IF.

*> ---------------------------------------------------------------
*> EM-015: Jan 31 2026 -> next stmt date caps to Feb 28 (non-leap)
*> 2026 is not a leap year, so February has 28 days.
*> Day 31 must be capped to 28.
*> ---------------------------------------------------------------
TEST-EM-015.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EM-015: Jan 31 -> Feb 28 (non-leap 2026)"
        TO WS-TEST-NAME
    PERFORM SETUP-EOM-ACCOUNT
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260131 TO WS-BATCH-DATE
    CALL "EOMPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF WS-BATCH-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-BATCH-RESULT-CODE
        GO TO TEST-EM-015-EXIT
    END-IF
    IF ACCT-NEXT-STMT-DATE = 20260228
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " next-stmt=" ACCT-NEXT-STMT-DATE
            " expected=20260228"
    END-IF.
TEST-EM-015-EXIT.
    CONTINUE.

*> ---------------------------------------------------------------
*> EM-016: Jan 31 2024 -> next stmt date caps to Feb 29 (leap year)
*> 2024 is a leap year (divisible by 4, not by 100), so February
*> has 29 days. Day 31 must be capped to 29.
*> ---------------------------------------------------------------
TEST-EM-016.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EM-016: Jan 31 -> Feb 29 (leap 2024)"
        TO WS-TEST-NAME
    PERFORM SETUP-EOM-ACCOUNT
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20240131 TO WS-BATCH-DATE
    CALL "EOMPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF WS-BATCH-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-BATCH-RESULT-CODE
        GO TO TEST-EM-016-EXIT
    END-IF
    IF ACCT-NEXT-STMT-DATE = 20240229
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " next-stmt=" ACCT-NEXT-STMT-DATE
            " expected=20240229"
    END-IF.
TEST-EM-016-EXIT.
    CONTINUE.

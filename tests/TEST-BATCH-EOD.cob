IDENTIFICATION DIVISION.
PROGRAM-ID. TEST-BATCH-EOD.
*> ================================================================
*> TEST-BATCH-EOD - Integration test for EODPROC0 End-of-Day Batch
*> Tests: EOD cycle with interest accrual, interest payment,
*>        batch status, multi-account, error paths (19 tests)
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-TEST-COUNT          PIC 9(3) VALUE 0.
01  WS-PASS-COUNT          PIC 9(3) VALUE 0.
01  WS-FAIL-COUNT          PIC 9(3) VALUE 0.
01  WS-TEST-NAME           PIC X(60).

*> EOD batch areas
01  WS-BATCH-DATE          PIC 9(8).
COPY CPYACCT.
COPY CPYBATCH.
01  WS-BATCH-RESULT.
    05  WS-BATCH-RESULT-CODE  PIC X(5).
    05  WS-BATCH-RESULT-MSG   PIC X(50).

01  WS-SAVED-ACCRUED       PIC S9(11)V9(6).
01  WS-SAVED-LEDGER-BAL    PIC S9(13)V99.
01  WS-SAVED-YTD-INT-PAID  PIC S9(11)V99.
01  WS-SAVED-YTD-INT-EARN  PIC S9(11)V99.

PROCEDURE DIVISION.
MAIN-PROGRAM.
    DISPLAY "========================================".
    DISPLAY "TEST SUITE: BATCH-EOD Integration".
    DISPLAY "========================================".

    PERFORM TEST-BE-001
    PERFORM TEST-BE-002
    PERFORM TEST-BE-003
    PERFORM TEST-BE-004
    PERFORM TEST-BE-005
    PERFORM TEST-BE-006
    PERFORM TEST-BE-007
    PERFORM TEST-BE-008
    PERFORM TEST-BE-009
    PERFORM TEST-BE-010
    PERFORM TEST-BE-011
    PERFORM TEST-BE-012
    PERFORM TEST-BE-013
    PERFORM TEST-BE-014
    PERFORM TEST-BE-015
    PERFORM TEST-BE-016
    PERFORM TEST-BE-017
    PERFORM TEST-BE-018
    PERFORM TEST-BE-019

    DISPLAY "========================================".
    DISPLAY "RESULTS: " WS-PASS-COUNT "/" WS-TEST-COUNT
            " PASSED".
    DISPLAY "         " WS-FAIL-COUNT " FAILED".
    DISPLAY "========================================".
    MOVE WS-FAIL-COUNT TO RETURN-CODE
    STOP RUN.

*> ---------------------------------------------------------------
*> Helper: Set up active account with interest params
*> ---------------------------------------------------------------
SETUP-INTEREST-ACCOUNT.
    INITIALIZE ACCT-RECORD
    MOVE 000012345678 TO ACCT-NUMBER
    MOVE 2 TO ACCT-CHECK-DIGIT
    MOVE 1000000001 TO ACCT-PRIMARY-CIF
    MOVE "SAV1" TO ACCT-PRODUCT-CODE
    MOVE "D" TO ACCT-TYPE
    MOVE "SV" TO ACCT-SUB-TYPE
    MOVE 10000.00 TO ACCT-LEDGER-BAL
    MOVE 10000.00 TO ACCT-AVAIL-BAL
    MOVE 0 TO ACCT-HOLD-AMOUNT
    MOVE "A" TO ACCT-STATUS
    MOVE "N" TO ACCT-LEGAL-HOLD
    MOVE "N" TO ACCT-DECEASED
    MOVE "N" TO ACCT-GARNISHMENT
    MOVE 5.0000000 TO ACCT-INT-RATE
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "A" TO ACCT-INT-ACCRUAL-BASIS
    MOVE "M" TO ACCT-INT-PAY-FREQ
    MOVE 0 TO ACCT-INT-NEXT-PAY-DATE
    MOVE 0 TO ACCT-ACCRUED-INT
    MOVE 20200101 TO ACCT-OPEN-DATE.

*> ---------------------------------------------------------------
*> BE-001: EOD batch completes successfully
*> ---------------------------------------------------------------
TEST-BE-001.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BE-001: EOD batch completes -> E0000" TO WS-TEST-NAME
    PERFORM SETUP-INTEREST-ACCOUNT
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260226 TO WS-BATCH-DATE
    CALL "EODPROC0" USING WS-BATCH-DATE
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
*> BE-002: EOD batch sets batch status to C (complete)
*> ---------------------------------------------------------------
TEST-BE-002.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BE-002: Batch status = C (complete)" TO WS-TEST-NAME
    PERFORM SETUP-INTEREST-ACCOUNT
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260226 TO WS-BATCH-DATE
    CALL "EODPROC0" USING WS-BATCH-DATE
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
*> BE-003: EOD accrues interest on active account
*> ---------------------------------------------------------------
TEST-BE-003.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BE-003: EOD accrues interest" TO WS-TEST-NAME
    PERFORM SETUP-INTEREST-ACCOUNT
    MOVE 0 TO ACCT-ACCRUED-INT
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260226 TO WS-BATCH-DATE
    CALL "EODPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF WS-BATCH-RESULT-CODE = "E0000"
        IF ACCT-ACCRUED-INT > 0
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " accrued=" ACCT-ACCRUED-INT
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " accrued=" ACCT-ACCRUED-INT " expected>0"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-BATCH-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> BE-004: EOD processes 1 account correctly
*> ---------------------------------------------------------------
TEST-BE-004.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BE-004: Batch processed count = 1" TO WS-TEST-NAME
    PERFORM SETUP-INTEREST-ACCOUNT
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260226 TO WS-BATCH-DATE
    CALL "EODPROC0" USING WS-BATCH-DATE
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
    END-IF.

*> ---------------------------------------------------------------
*> BE-005: EOD sets batch type to EOD
*> ---------------------------------------------------------------
TEST-BE-005.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BE-005: Batch type = EOD" TO WS-TEST-NAME
    PERFORM SETUP-INTEREST-ACCOUNT
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260226 TO WS-BATCH-DATE
    CALL "EODPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF BATCH-TYPE = "EOD"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " type=" BATCH-TYPE " expected=EOD"
    END-IF.

*> ---------------------------------------------------------------
*> BE-006: EOD with interest payment on pay date
*> ---------------------------------------------------------------
TEST-BE-006.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BE-006: EOD with interest payment" TO WS-TEST-NAME
    PERFORM SETUP-INTEREST-ACCOUNT
    *> Set accrued interest and payment date = today
    MOVE 41.10 TO ACCT-ACCRUED-INT
    MOVE 20260226 TO ACCT-INT-NEXT-PAY-DATE
    MOVE ACCT-LEDGER-BAL TO WS-SAVED-LEDGER-BAL
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260226 TO WS-BATCH-DATE
    CALL "EODPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF WS-BATCH-RESULT-CODE = "E0000"
        *> Balance should have increased by interest payment
        IF ACCT-LEDGER-BAL > WS-SAVED-LEDGER-BAL
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " bal=" ACCT-LEDGER-BAL
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " bal=" ACCT-LEDGER-BAL
                " prev=" WS-SAVED-LEDGER-BAL
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-BATCH-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> BE-007: EOD sets batch date correctly
*> ---------------------------------------------------------------
TEST-BE-007.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BE-007: Batch date = input date" TO WS-TEST-NAME
    PERFORM SETUP-INTEREST-ACCOUNT
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260226 TO WS-BATCH-DATE
    CALL "EODPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF BATCH-DATE = 20260226
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " date=" BATCH-DATE
    END-IF.

*> ---------------------------------------------------------------
*> BE-008: Closed account skipped (not processed)
*> ---------------------------------------------------------------
TEST-BE-008.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BE-008: Closed acct skipped" TO WS-TEST-NAME
    PERFORM SETUP-INTEREST-ACCOUNT
    MOVE "C" TO ACCT-STATUS
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260226 TO WS-BATCH-DATE
    CALL "EODPROC0" USING WS-BATCH-DATE
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
*> BE-009: Dormant account (status D) still processed
*> ---------------------------------------------------------------
TEST-BE-009.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BE-009: Dormant acct processed -> E0000" TO WS-TEST-NAME
    PERFORM SETUP-INTEREST-ACCOUNT
    MOVE "D" TO ACCT-STATUS
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260226 TO WS-BATCH-DATE
    CALL "EODPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF WS-BATCH-RESULT-CODE = "E0000"
        AND BATCH-ACCTS-PROCESSED = 1
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-BATCH-RESULT-CODE
            " processed=" BATCH-ACCTS-PROCESSED
    END-IF.

*> ---------------------------------------------------------------
*> BE-010: Frozen account still accrues interest via EOD
*> Freeze blocks transactions, not interest entitlement.
*> ---------------------------------------------------------------
TEST-BE-010.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BE-010: Frozen acct processed" TO WS-TEST-NAME
    PERFORM SETUP-INTEREST-ACCOUNT
    MOVE "F" TO ACCT-STATUS
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260226 TO WS-BATCH-DATE
    CALL "EODPROC0" USING WS-BATCH-DATE
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
*> BE-011: EOD releases matured hold (release date <= batch date)
*> ---------------------------------------------------------------
TEST-BE-011.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BE-011: Matured hold released"
        TO WS-TEST-NAME
    PERFORM SETUP-INTEREST-ACCOUNT
    MOVE 500.00 TO ACCT-HOLD-AMOUNT
    MOVE 20260225 TO ACCT-HOLD-RELEASE-DT
    COMPUTE ACCT-AVAIL-BAL =
        ACCT-LEDGER-BAL - ACCT-HOLD-AMOUNT
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260226 TO WS-BATCH-DATE
    CALL "EODPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF WS-BATCH-RESULT-CODE = "E0000"
        IF ACCT-HOLD-AMOUNT = 0
            AND ACCT-AVAIL-BAL = ACCT-LEDGER-BAL
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " hold=" ACCT-HOLD-AMOUNT
                " avail=" ACCT-AVAIL-BAL
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " hold=" ACCT-HOLD-AMOUNT
                " avail=" ACCT-AVAIL-BAL
                " ledger=" ACCT-LEDGER-BAL
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-BATCH-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> BE-012: Batch date stored and status P on error
*> ---------------------------------------------------------------
TEST-BE-012.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BE-012: Batch date set, status=P on err" TO WS-TEST-NAME
    PERFORM SETUP-INTEREST-ACCOUNT
    MOVE "Z" TO ACCT-INT-ACCRUAL-BASIS
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260226 TO WS-BATCH-DATE
    CALL "EODPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF BATCH-DATE = 20260226
        AND BATCH-STATUS = "P"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " date=" BATCH-DATE
            " status=" BATCH-STATUS
            " expected date=20260226 status=P"
    END-IF.

*> ---------------------------------------------------------------
*> BE-013 (EOD-007): Interest payment rollback on posting failure
*> Trigger: Active account with ACCT-DECEASED = "Y". EODPROC0
*> processes status "A", INTCALC0 succeeds and flags payment due,
*> but TXNPOST0 rejects with E0036 (deceased). Rollback restores
*> accrued interest (pre-INTCALC0 + today's accrual) and
*> YTD-INT-PAID to its pre-INTCALC0 value.
*> ---------------------------------------------------------------
TEST-BE-013.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BE-013: Interest payment rollback on post fail"
        TO WS-TEST-NAME
    PERFORM SETUP-INTEREST-ACCOUNT
    *> Active account so EODPROC0 processes it
    MOVE "A" TO ACCT-STATUS
    *> Set deceased flag - TXNPOST0 will reject with E0036
    MOVE "Y" TO ACCT-DECEASED
    *> Pre-existing accrued interest before this EOD run
    MOVE 50.000000 TO ACCT-ACCRUED-INT
    *> Set payment date = today so INTCALC0 flags payment due
    MOVE 20260226 TO ACCT-INT-NEXT-PAY-DATE
    *> Known YTD values before EOD
    MOVE 100.00 TO ACCT-YTD-INT-PAID
    MOVE 200.00 TO ACCT-YTD-INT-EARNED
    *> Save pre-EOD values for verification
    MOVE ACCT-ACCRUED-INT TO WS-SAVED-ACCRUED
    MOVE ACCT-YTD-INT-PAID TO WS-SAVED-YTD-INT-PAID
    MOVE ACCT-LEDGER-BAL TO WS-SAVED-LEDGER-BAL
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260226 TO WS-BATCH-DATE
    CALL "EODPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    *> After rollback:
    *> - ACCT-ACCRUED-INT = pre-INTCALC0 value + today's accrual
    *>   (rollback restores saved accrued then adds daily interest)
    *> - ACCT-YTD-INT-PAID = pre-INTCALC0 value (rollback restores)
    *>   Note: INTCALC0 adds payment to YTD-INT-PAID on payment due,
    *>   but rollback reverts it to saved value
    *> - Ledger balance unchanged (TXNPOST0 never posted)
    *> - Batch has errors (status P)
    IF ACCT-LEDGER-BAL = WS-SAVED-LEDGER-BAL
        AND ACCT-ACCRUED-INT > WS-SAVED-ACCRUED
        AND ACCT-YTD-INT-PAID = WS-SAVED-YTD-INT-PAID
        AND BATCH-ACCTS-ERRORS > 0
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
            " accrued=" ACCT-ACCRUED-INT
            " ytd-paid=" ACCT-YTD-INT-PAID
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " bal=" ACCT-LEDGER-BAL
            " accrued=" ACCT-ACCRUED-INT
            " ytd-paid=" ACCT-YTD-INT-PAID
            " errors=" BATCH-ACCTS-ERRORS
    END-IF.

*> ---------------------------------------------------------------
*> BE-014 (EOD-008): Quarterly payment frequency date advancement
*> Active account with PAY-FREQ = "Q", next pay date = batch date.
*> After EOD: interest paid, next pay date advanced by 3 months.
*> ---------------------------------------------------------------
TEST-BE-014.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BE-014: Quarterly freq advances +3 months"
        TO WS-TEST-NAME
    PERFORM SETUP-INTEREST-ACCOUNT
    MOVE "Q" TO ACCT-INT-PAY-FREQ
    MOVE 20261115 TO ACCT-INT-NEXT-PAY-DATE
    MOVE 41.100000 TO ACCT-ACCRUED-INT
    MOVE ACCT-LEDGER-BAL TO WS-SAVED-LEDGER-BAL
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20261115 TO WS-BATCH-DATE
    CALL "EODPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF WS-BATCH-RESULT-CODE = "E0000"
        IF ACCT-INT-NEXT-PAY-DATE = 20270215
            AND ACCT-LEDGER-BAL > WS-SAVED-LEDGER-BAL
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " next-pay=" ACCT-INT-NEXT-PAY-DATE
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " next-pay=" ACCT-INT-NEXT-PAY-DATE
                " expected=20270215"
                " bal=" ACCT-LEDGER-BAL
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-BATCH-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> BE-015 (EOD-009): Annual payment frequency with year rollover
*> Active account with PAY-FREQ = "A", next pay date = 20260315.
*> After EOD with batch date 20260315: next pay date = 20270315.
*> ---------------------------------------------------------------
TEST-BE-015.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BE-015: Annual freq advances +12 months"
        TO WS-TEST-NAME
    PERFORM SETUP-INTEREST-ACCOUNT
    MOVE "A" TO ACCT-INT-PAY-FREQ
    MOVE 20260315 TO ACCT-INT-NEXT-PAY-DATE
    MOVE 125.500000 TO ACCT-ACCRUED-INT
    MOVE ACCT-LEDGER-BAL TO WS-SAVED-LEDGER-BAL
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260315 TO WS-BATCH-DATE
    CALL "EODPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF WS-BATCH-RESULT-CODE = "E0000"
        IF ACCT-INT-NEXT-PAY-DATE = 20270315
            AND ACCT-LEDGER-BAL > WS-SAVED-LEDGER-BAL
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " next-pay=" ACCT-INT-NEXT-PAY-DATE
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " next-pay=" ACCT-INT-NEXT-PAY-DATE
                " expected=20270315"
                " bal=" ACCT-LEDGER-BAL
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-BATCH-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> BE-016: Failed interest posting still advances next-pay-date
*> Active account with DECEASED=Y. INTCALC0 succeeds, TXNPOST0
*> rejects (E0036). After rollback, next-pay-date should still
*> advance from 20260115 to 20260215 (monthly freq).
*> ---------------------------------------------------------------
TEST-BE-016.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BE-016: Failed post advances next-pay-date"
        TO WS-TEST-NAME
    PERFORM SETUP-INTEREST-ACCOUNT
    *> Active account so EODPROC0 processes it
    MOVE "A" TO ACCT-STATUS
    *> Set deceased flag - TXNPOST0 will reject with E0036
    MOVE "Y" TO ACCT-DECEASED
    *> Pre-existing accrued interest for payment
    MOVE 50.000000 TO ACCT-ACCRUED-INT
    *> Set payment date = batch date so INTCALC0 flags payment due
    MOVE 20260115 TO ACCT-INT-NEXT-PAY-DATE
    MOVE "M" TO ACCT-INT-PAY-FREQ
    *> Known pre-EOD values
    MOVE 100.00 TO ACCT-YTD-INT-PAID
    MOVE 200.00 TO ACCT-YTD-INT-EARNED
    MOVE ACCT-LEDGER-BAL TO WS-SAVED-LEDGER-BAL
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260115 TO WS-BATCH-DATE
    CALL "EODPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    *> After failed posting + rollback:
    *> - Batch has errors
    *> - ACCT-INT-NEXT-PAY-DATE should advance to 20260215
    IF BATCH-ACCTS-ERRORS > 0
        AND ACCT-INT-NEXT-PAY-DATE = 20260215
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
            " next-pay=" ACCT-INT-NEXT-PAY-DATE
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " next-pay=" ACCT-INT-NEXT-PAY-DATE
            " expected=20260215"
            " errors=" BATCH-ACCTS-ERRORS
    END-IF.

*> ---------------------------------------------------------------
*> BE-017: Hold with no release date (0) is NOT released
*> ---------------------------------------------------------------
TEST-BE-017.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BE-017: Hold no release dt preserved"
        TO WS-TEST-NAME
    PERFORM SETUP-INTEREST-ACCOUNT
    MOVE 500.00 TO ACCT-HOLD-AMOUNT
    MOVE 0 TO ACCT-HOLD-RELEASE-DT
    COMPUTE ACCT-AVAIL-BAL =
        ACCT-LEDGER-BAL - ACCT-HOLD-AMOUNT
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260226 TO WS-BATCH-DATE
    CALL "EODPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    IF WS-BATCH-RESULT-CODE = "E0000"
        IF ACCT-HOLD-AMOUNT = 500.00
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
                " hold=" ACCT-HOLD-AMOUNT
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " hold=" ACCT-HOLD-AMOUNT
                " expected=500.00 (not released)"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-BATCH-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> BE-018: At-maturity pay frequency does NOT advance pay date
*> CD with PAY-FREQ="T", next-pay=20260601. After EOD on 20260601
*> the pay date should NOT advance (interest held until maturity).
*> ---------------------------------------------------------------
TEST-BE-018.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BE-018: At-maturity no pay date advance"
        TO WS-TEST-NAME
    PERFORM SETUP-INTEREST-ACCOUNT
    MOVE "T" TO ACCT-INT-PAY-FREQ
    MOVE 20260601 TO ACCT-INT-NEXT-PAY-DATE
    MOVE 50.000000 TO ACCT-ACCRUED-INT
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260601 TO WS-BATCH-DATE
    CALL "EODPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    *> Next-pay-date should NOT have changed from 20260601
    IF ACCT-INT-NEXT-PAY-DATE = 20260601
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
            " next-pay=" ACCT-INT-NEXT-PAY-DATE
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " next-pay=" ACCT-INT-NEXT-PAY-DATE
            " expected=20260601 (unchanged)"
    END-IF.

*> ---------------------------------------------------------------
*> BE-019: Frozen account still accrues daily interest via EOD
*> Freeze blocks transactions, not interest entitlement.
*> ---------------------------------------------------------------
TEST-BE-019.
    ADD 1 TO WS-TEST-COUNT
    MOVE "BE-019: Frozen acct accrues interest"
        TO WS-TEST-NAME
    PERFORM SETUP-INTEREST-ACCOUNT
    MOVE "F" TO ACCT-STATUS
    MOVE 10000.00 TO ACCT-LEDGER-BAL
    MOVE 10000.00 TO ACCT-AVAIL-BAL
    MOVE 0 TO ACCT-ACCRUED-INT
    INITIALIZE BATCH-RECORD
    INITIALIZE WS-BATCH-RESULT
    MOVE 20260226 TO WS-BATCH-DATE
    CALL "EODPROC0" USING WS-BATCH-DATE
                          ACCT-RECORD
                          BATCH-RECORD
                          WS-BATCH-RESULT
    *> Accrued interest should be > 0 (daily interest accrued)
    IF ACCT-ACCRUED-INT > 0
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
            " accrued=" ACCT-ACCRUED-INT
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " accrued=" ACCT-ACCRUED-INT
            " expected>0"
    END-IF.

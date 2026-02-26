IDENTIFICATION DIVISION.
PROGRAM-ID. TEST-INTEG-EOD.
*> ================================================================
*> TEST-INTEG-EOD - Integration test for End-of-Day processing
*> Tests multi-module EOD flows: interest, holds, OD, CTR, GL, fees
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-TEST-COUNT          PIC 9(3) VALUE 0.
01  WS-PASS-COUNT          PIC 9(3) VALUE 0.
01  WS-FAIL-COUNT          PIC 9(3) VALUE 0.
01  WS-TEST-NAME           PIC X(60).

*> --- Account record (used by TXNPOST0, INTCALC0, ODMGMT0, FEECALC0)
COPY CPYACCT REPLACING ==ACCT-RECORD== BY ==WS-ACCT-RECORD==.

*> --- Transaction record (used by TXNPOST0)
COPY CPYTXN REPLACING ==TXN-RECORD== BY ==WS-TXN-RECORD==.

*> --- GL entries for TXNPOST0
01  WS-GL-ENTRIES.
    05  WS-GL-DR-ACCOUNT          PIC 9(10).
    05  WS-GL-CR-ACCOUNT          PIC 9(10).
    05  WS-GL-AMOUNT              PIC S9(13)V99.
    05  WS-GL-POST-FLAG           PIC X(1).

*> --- TXN result for TXNPOST0
01  WS-TXN-RESULT.
    05  WS-TXN-RESULT-CODE        PIC X(5).
    05  WS-TXN-RESULT-MSG         PIC X(50).

*> --- Interest calc result (INTCALC0)
01  WS-CALC-DATE                  PIC 9(8).
01  WS-INT-RESULT.
    05  WS-INT-RESULT-CODE        PIC X(5).
    05  WS-INT-RESULT-MSG         PIC X(50).
    05  WS-DAILY-INT-AMT          PIC S9(11)V9(6).
    05  WS-NEW-ACCRUED            PIC S9(11)V9(6).
    05  WS-PAYMENT-AMT            PIC S9(11)V99.
    05  WS-PAYMENT-DUE            PIC X(1).

*> --- Hold request/result (HOLDCALC0)
01  WS-HOLD-REQUEST.
    05  WS-HR-ACCT-NUMBER         PIC 9(12).
    05  WS-HR-DEPOSIT-AMT         PIC S9(13)V99.
    05  WS-HR-CHECK-TYPE          PIC X(2).
    05  WS-HR-DEPOSIT-DATE        PIC 9(8).
    05  WS-HR-ACCT-OPEN-DATE      PIC 9(8).
    05  WS-HR-IS-REDEPOSIT        PIC X(1).
    05  WS-HR-REPEATED-OD         PIC X(1).
COPY CPYHOLD REPLACING ==HOLD-RECORD== BY ==WS-HOLD-RECORD==.
01  WS-HOLD-RESULT.
    05  WS-HOLD-RESULT-CODE       PIC X(5).
    05  WS-HOLD-RESULT-MSG        PIC X(50).
    05  WS-HOLD-NEXT-DAY-AMT      PIC S9(13)V99.
    05  WS-HOLD-REMAINING-AMT     PIC S9(13)V99.
    05  WS-HOLD-RELEASE-DT        PIC 9(8).
    05  WS-HOLD-EXCEPTION-FLAG    PIC X(1).

*> --- Overdraft request/result (ODMGMT0)
01  WS-OD-REQUEST.
    05  WS-OD-TXN-AMOUNT         PIC S9(13)V99.
    05  WS-OD-TXN-CHANNEL        PIC X(2).
    05  WS-OD-TXN-TYPE           PIC X(3).
    05  WS-OD-CURRENT-DATE        PIC 9(8).
01  WS-OD-RESULT.
    05  WS-OD-RESULT-CODE         PIC X(5).
    05  WS-OD-RESULT-MSG          PIC X(50).
    05  WS-OD-APPROVED            PIC X(1).
    05  WS-OD-FEE-ASSESSED        PIC 9(5)V99.
    05  WS-OD-TRANSFER-AMT        PIC S9(13)V99.
    05  WS-OD-NEW-NSF-COUNT       PIC 9(3).

*> --- CTR record and BSA result (BSACTRO)
01  WS-BSA-FUNCTION               PIC X(4).
COPY CPYCTR REPLACING ==CTR-RECORD== BY ==WS-CTR-RECORD==.
01  WS-TXN-INFO.
    05  WS-BSA-CUST-ID           PIC 9(10).
    05  WS-BSA-TXN-DATE          PIC 9(8).
    05  WS-BSA-CASH-AMOUNT       PIC S9(13)V99.
    05  WS-BSA-CASH-DIRECTION    PIC X(1).
    05  WS-BSA-IS-CASH           PIC X(1).
    05  WS-BSA-ACCT-NUMBER       PIC 9(12).
01  WS-BSA-RESULT.
    05  WS-BSA-RESULT-CODE        PIC X(5).
    05  WS-BSA-RESULT-MSG         PIC X(50).
    05  WS-BSA-CTR-REQUIRED       PIC X(1).
    05  WS-BSA-CASH-IN-TOTAL      PIC S9(13)V99.
    05  WS-BSA-CASH-OUT-TOTAL     PIC S9(13)V99.

*> --- GL posting (GLPOST0)
01  WS-GL-FUNCTION                PIC X(4).
01  WS-GL-ENTRY.
    05  WS-GLE-DR-ACCT            PIC 9(10).
    05  WS-GLE-CR-ACCT            PIC 9(10).
    05  WS-GLE-AMOUNT             PIC S9(15)V99.
    05  WS-GLE-DESCRIPTION        PIC X(40).
    05  WS-GLE-POST-DATE          PIC 9(8).
COPY CPYGL REPLACING ==GL-RECORD== BY ==WS-GL-RECORD==.
01  WS-TRIAL-BAL.
    05  WS-TB-TOTAL-DEBITS        PIC S9(15)V99.
    05  WS-TB-TOTAL-CREDITS       PIC S9(15)V99.
    05  WS-TB-DIFFERENCE          PIC S9(15)V99.
    05  WS-TB-IS-BALANCED         PIC X(1).
01  WS-GL-RESULT.
    05  WS-GL-RESULT-CODE         PIC X(5).
    05  WS-GL-RESULT-MSG          PIC X(50).

*> --- Fee schedule and result (FEECALC0)
COPY CPYFEE REPLACING
    ==FEE-SCHEDULE-RECORD== BY ==WS-FEE-SCHEDULE-RECORD==.
01  WS-FEE-RESULT.
    05  WS-FEE-RESULT-CODE        PIC X(5).
    05  WS-FEE-RESULT-MSG         PIC X(50).
    05  WS-FEE-ASSESSED           PIC 9(5)V99.
    05  WS-FEE-WAIVED-FLAG        PIC X(1).
    05  WS-FEE-WAIVER-REASON      PIC X(2).

*> --- Temporary work fields
01  WS-EXPECTED-DAILY-INT         PIC S9(11)V9(6).
01  WS-EXPECTED-BAL               PIC S9(13)V99.

PROCEDURE DIVISION.
MAIN-PROGRAM.
    DISPLAY "========================================".
    DISPLAY "TEST SUITE: INTEGRATION - EOD".
    DISPLAY "========================================".

    PERFORM TEST-EOD-001 THRU TEST-EOD-001-EXIT
    PERFORM TEST-EOD-002 THRU TEST-EOD-002-EXIT
    PERFORM TEST-EOD-003 THRU TEST-EOD-003-EXIT
    PERFORM TEST-EOD-004 THRU TEST-EOD-004-EXIT
    PERFORM TEST-EOD-005 THRU TEST-EOD-005-EXIT
    PERFORM TEST-EOD-006 THRU TEST-EOD-006-EXIT
    PERFORM TEST-EOD-007 THRU TEST-EOD-007-EXIT

    DISPLAY "========================================".
    DISPLAY "RESULTS: " WS-PASS-COUNT "/" WS-TEST-COUNT
            " PASSED".
    DISPLAY "         " WS-FAIL-COUNT " FAILED".
    DISPLAY "========================================".
    MOVE WS-FAIL-COUNT TO RETURN-CODE
    STOP RUN.

*> ---------------------------------------------------------------
*> EOD-001: Full deposit + interest accrual cycle
*> Post $500 deposit to $10,000 checking, then accrue interest
*> ---------------------------------------------------------------
TEST-EOD-001.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EOD-001: Deposit + interest accrual cycle"
        TO WS-TEST-NAME
    *> --- Step 1: Set up active checking, $10,000, 5% Actual/365
    INITIALIZE WS-ACCT-RECORD
    INITIALIZE WS-TXN-RECORD
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    INITIALIZE WS-INT-RESULT
    MOVE 300000000001 TO ACCT-NUMBER OF WS-ACCT-RECORD
    MOVE 0 TO ACCT-CHECK-DIGIT OF WS-ACCT-RECORD
    MOVE "DDA2" TO ACCT-PRODUCT-CODE OF WS-ACCT-RECORD
    MOVE "D" TO ACCT-TYPE OF WS-ACCT-RECORD
    MOVE "CH" TO ACCT-SUB-TYPE OF WS-ACCT-RECORD
    MOVE 10000.00 TO ACCT-LEDGER-BAL OF WS-ACCT-RECORD
    MOVE 10000.00 TO ACCT-AVAIL-BAL OF WS-ACCT-RECORD
    MOVE 5.0000000 TO ACCT-INT-RATE OF WS-ACCT-RECORD
    MOVE "F" TO ACCT-INT-RATE-TYPE OF WS-ACCT-RECORD
    MOVE "DB" TO ACCT-INT-CALC-METHOD OF WS-ACCT-RECORD
    MOVE "A" TO ACCT-INT-ACCRUAL-BASIS OF WS-ACCT-RECORD
    MOVE "M" TO ACCT-INT-PAY-FREQ OF WS-ACCT-RECORD
    MOVE 20260301 TO ACCT-INT-NEXT-PAY-DATE OF WS-ACCT-RECORD
    MOVE 0 TO ACCT-ACCRUED-INT OF WS-ACCT-RECORD
    MOVE "A" TO ACCT-STATUS OF WS-ACCT-RECORD
    *> --- Step 2: Post $500 cash deposit via TXNPOST0
    MOVE 300000000001 TO TXN-ACCT-NUMBER OF WS-TXN-RECORD
    MOVE "DEP" TO TXN-TYPE OF WS-TXN-RECORD
    MOVE 500.00 TO TXN-AMOUNT OF WS-TXN-RECORD
    MOVE "C" TO TXN-DR-CR OF WS-TXN-RECORD
    MOVE "BR" TO TXN-CHANNEL OF WS-TXN-RECORD
    MOVE "CASH DEPOSIT" TO TXN-DESCRIPTION OF WS-TXN-RECORD
    MOVE 20260226 TO TXN-POST-DATE OF WS-TXN-RECORD
    MOVE 20260226 TO TXN-EFFECTIVE-DATE OF WS-TXN-RECORD
    CALL "TXNPOST0" USING WS-TXN-RECORD
                          WS-ACCT-RECORD
                          WS-GL-ENTRIES
                          WS-TXN-RESULT
    IF WS-TXN-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " TXNPOST0 result=" WS-TXN-RESULT-CODE
        GO TO TEST-EOD-001-EXIT
    END-IF
    *> Verify balance updated to $10,500
    IF ACCT-LEDGER-BAL OF WS-ACCT-RECORD NOT = 10500.00
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " post-deposit bal="
            ACCT-LEDGER-BAL OF WS-ACCT-RECORD
            " expected=10500.00"
        GO TO TEST-EOD-001-EXIT
    END-IF
    *> --- Step 3: Accrue interest via INTCALC0
    MOVE 20260226 TO WS-CALC-DATE
    CALL "INTCALC0" USING WS-ACCT-RECORD
                          WS-CALC-DATE
                          WS-INT-RESULT
    IF WS-INT-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " INTCALC0 result=" WS-INT-RESULT-CODE
        GO TO TEST-EOD-001-EXIT
    END-IF
    *> Expected: 10500 * 0.05 / 365 = 1.438356 (truncated to 6dp)
    MOVE 1.438356 TO WS-EXPECTED-DAILY-INT
    IF WS-DAILY-INT-AMT = WS-EXPECTED-DAILY-INT
        IF ACCT-ACCRUED-INT OF WS-ACCT-RECORD > 0
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " accrued-int not updated"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " daily-int=" WS-DAILY-INT-AMT
            " expected=" WS-EXPECTED-DAILY-INT
    END-IF.
TEST-EOD-001-EXIT.
    CONTINUE.

*> ---------------------------------------------------------------
*> EOD-002: Hold placement and release lifecycle
*> Place hold on local check $1,000, verify amounts, release
*> ---------------------------------------------------------------
TEST-EOD-002.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EOD-002: Hold place/release lifecycle"
        TO WS-TEST-NAME
    INITIALIZE WS-HOLD-REQUEST
    INITIALIZE WS-HOLD-RECORD
    INITIALIZE WS-HOLD-RESULT
    *> Local check deposit of $1,000
    MOVE 400000000001 TO WS-HR-ACCT-NUMBER
    MOVE 1000.00 TO WS-HR-DEPOSIT-AMT
    MOVE "LC" TO WS-HR-CHECK-TYPE
    MOVE 20260226 TO WS-HR-DEPOSIT-DATE
    MOVE 20250101 TO WS-HR-ACCT-OPEN-DATE
    MOVE "N" TO WS-HR-IS-REDEPOSIT
    MOVE "N" TO WS-HR-REPEATED-OD
    CALL "HOLDCALC0" USING WS-HOLD-REQUEST
                           WS-HOLD-RECORD
                           WS-HOLD-RESULT
    IF WS-HOLD-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " HOLDCALC0 result=" WS-HOLD-RESULT-CODE
        GO TO TEST-EOD-002-EXIT
    END-IF
    *> Reg CC: next-day availability = $225, remainder = $775
    IF WS-HOLD-NEXT-DAY-AMT NOT = 225.00
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " next-day=" WS-HOLD-NEXT-DAY-AMT
            " expected=225.00"
        GO TO TEST-EOD-002-EXIT
    END-IF
    IF WS-HOLD-REMAINING-AMT NOT = 775.00
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " remaining=" WS-HOLD-REMAINING-AMT
            " expected=775.00"
        GO TO TEST-EOD-002-EXIT
    END-IF
    *> Simulate hold release
    MOVE "R" TO HOLD-STATUS OF WS-HOLD-RECORD
    IF HOLD-STATUS OF WS-HOLD-RECORD = "R"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " hold-status not released"
    END-IF.
TEST-EOD-002-EXIT.
    CONTINUE.

*> ---------------------------------------------------------------
*> EOD-003: Overdraft management + fee assessment
*> $100 balance, $300 withdrawal, OD opted-in, $500 limit
*> ---------------------------------------------------------------
TEST-EOD-003.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EOD-003: Overdraft approval + fee"
        TO WS-TEST-NAME
    INITIALIZE WS-ACCT-RECORD
    INITIALIZE WS-OD-REQUEST
    INITIALIZE WS-OD-RESULT
    *> Set up checking with $100 balance, OD opted-in
    MOVE 500000000001 TO ACCT-NUMBER OF WS-ACCT-RECORD
    MOVE 0 TO ACCT-CHECK-DIGIT OF WS-ACCT-RECORD
    MOVE "DDA1" TO ACCT-PRODUCT-CODE OF WS-ACCT-RECORD
    MOVE "D" TO ACCT-TYPE OF WS-ACCT-RECORD
    MOVE "CH" TO ACCT-SUB-TYPE OF WS-ACCT-RECORD
    MOVE 100.00 TO ACCT-LEDGER-BAL OF WS-ACCT-RECORD
    MOVE 100.00 TO ACCT-AVAIL-BAL OF WS-ACCT-RECORD
    MOVE "Y" TO ACCT-OD-OPTED-IN OF WS-ACCT-RECORD
    MOVE 500.00 TO ACCT-OD-LIMIT OF WS-ACCT-RECORD
    MOVE "N" TO ACCT-OD-PROTECTION OF WS-ACCT-RECORD
    MOVE 0 TO ACCT-NSF-COUNT-MTD OF WS-ACCT-RECORD
    MOVE 0 TO ACCT-NSF-COUNT-TODAY OF WS-ACCT-RECORD
    MOVE 0 TO ACCT-NSF-COUNT-YTD OF WS-ACCT-RECORD
    MOVE "A" TO ACCT-STATUS OF WS-ACCT-RECORD
    *> OD request: $300 withdrawal via POS
    MOVE 300.00 TO WS-OD-TXN-AMOUNT
    MOVE "PO" TO WS-OD-TXN-CHANNEL
    MOVE "WDL" TO WS-OD-TXN-TYPE
    MOVE 20260226 TO WS-OD-CURRENT-DATE
    CALL "ODMGMT0" USING WS-ACCT-RECORD
                         WS-OD-REQUEST
                         WS-OD-RESULT
    IF WS-OD-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " ODMGMT0 result=" WS-OD-RESULT-CODE
        GO TO TEST-EOD-003-EXIT
    END-IF
    *> Should be approved (within OD limit)
    IF WS-OD-APPROVED NOT = "Y"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " approved=" WS-OD-APPROVED " expected=Y"
        GO TO TEST-EOD-003-EXIT
    END-IF
    *> OD fee should be $36
    IF WS-OD-FEE-ASSESSED NOT = 36.00
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " fee=" WS-OD-FEE-ASSESSED " expected=36.00"
        GO TO TEST-EOD-003-EXIT
    END-IF
    *> NSF count should be incremented
    IF WS-OD-NEW-NSF-COUNT > 0
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " nsf-count=" WS-OD-NEW-NSF-COUNT
            " expected>0"
    END-IF.
TEST-EOD-003-EXIT.
    CONTINUE.

*> ---------------------------------------------------------------
*> EOD-004: CTR aggregation - two cash-in txns exceed $10,000
*> $6,000 + $5,000 = $11,000 -> CTR required
*> ---------------------------------------------------------------
TEST-EOD-004.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EOD-004: CTR aggregation >= $10,000"
        TO WS-TEST-NAME
    INITIALIZE WS-CTR-RECORD
    INITIALIZE WS-TXN-INFO
    INITIALIZE WS-BSA-RESULT
    *> First cash-in transaction: $6,000
    MOVE "AGGR" TO WS-BSA-FUNCTION
    MOVE 1000000001 TO WS-BSA-CUST-ID
    MOVE 20260226 TO WS-BSA-TXN-DATE
    MOVE 6000.00 TO WS-BSA-CASH-AMOUNT
    MOVE "I" TO WS-BSA-CASH-DIRECTION
    MOVE "Y" TO WS-BSA-IS-CASH
    MOVE 600000000001 TO WS-BSA-ACCT-NUMBER
    MOVE 1000000001 TO CTR-CUST-ID OF WS-CTR-RECORD
    MOVE 20260226 TO CTR-TXN-DATE OF WS-CTR-RECORD
    CALL "BSACTRO" USING WS-BSA-FUNCTION
                         WS-CTR-RECORD
                         WS-TXN-INFO
                         WS-BSA-RESULT
    IF WS-BSA-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " BSACTRO AGGR1 result=" WS-BSA-RESULT-CODE
        GO TO TEST-EOD-004-EXIT
    END-IF
    *> Second cash-in transaction: $5,000 (same customer, same day)
    MOVE "AGGR" TO WS-BSA-FUNCTION
    MOVE 5000.00 TO WS-BSA-CASH-AMOUNT
    CALL "BSACTRO" USING WS-BSA-FUNCTION
                         WS-CTR-RECORD
                         WS-TXN-INFO
                         WS-BSA-RESULT
    IF WS-BSA-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " BSACTRO AGGR2 result=" WS-BSA-RESULT-CODE
        GO TO TEST-EOD-004-EXIT
    END-IF
    *> Check threshold: total $11,000 >= $10,000
    MOVE "CHEK" TO WS-BSA-FUNCTION
    CALL "BSACTRO" USING WS-BSA-FUNCTION
                         WS-CTR-RECORD
                         WS-TXN-INFO
                         WS-BSA-RESULT
    IF WS-BSA-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " BSACTRO CHEK result=" WS-BSA-RESULT-CODE
        GO TO TEST-EOD-004-EXIT
    END-IF
    IF WS-BSA-CTR-REQUIRED = "Y"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " ctr-required=" WS-BSA-CTR-REQUIRED
            " expected=Y"
    END-IF.
TEST-EOD-004-EXIT.
    CONTINUE.

*> ---------------------------------------------------------------
*> EOD-005: Interest payment on payment date
*> Accrued $41.095890, pay date = today, verify payment + reset
*> ---------------------------------------------------------------
TEST-EOD-005.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EOD-005: Interest payment on pay date"
        TO WS-TEST-NAME
    INITIALIZE WS-ACCT-RECORD
    INITIALIZE WS-INT-RESULT
    *> Set up account with pre-accrued interest
    MOVE 700000000001 TO ACCT-NUMBER OF WS-ACCT-RECORD
    MOVE 0 TO ACCT-CHECK-DIGIT OF WS-ACCT-RECORD
    MOVE "SAV1" TO ACCT-PRODUCT-CODE OF WS-ACCT-RECORD
    MOVE "D" TO ACCT-TYPE OF WS-ACCT-RECORD
    MOVE "SV" TO ACCT-SUB-TYPE OF WS-ACCT-RECORD
    MOVE 30000.00 TO ACCT-LEDGER-BAL OF WS-ACCT-RECORD
    MOVE 30000.00 TO ACCT-AVAIL-BAL OF WS-ACCT-RECORD
    MOVE 5.0000000 TO ACCT-INT-RATE OF WS-ACCT-RECORD
    MOVE "F" TO ACCT-INT-RATE-TYPE OF WS-ACCT-RECORD
    MOVE "DB" TO ACCT-INT-CALC-METHOD OF WS-ACCT-RECORD
    MOVE "A" TO ACCT-INT-ACCRUAL-BASIS OF WS-ACCT-RECORD
    MOVE "M" TO ACCT-INT-PAY-FREQ OF WS-ACCT-RECORD
    *> Set next pay date to today (calc date)
    MOVE 20260226 TO ACCT-INT-NEXT-PAY-DATE OF WS-ACCT-RECORD
    *> Pre-accrued interest
    MOVE 41.095890 TO ACCT-ACCRUED-INT OF WS-ACCT-RECORD
    MOVE "A" TO ACCT-STATUS OF WS-ACCT-RECORD
    *> Run interest calc on pay date
    MOVE 20260226 TO WS-CALC-DATE
    CALL "INTCALC0" USING WS-ACCT-RECORD
                          WS-CALC-DATE
                          WS-INT-RESULT
    IF WS-INT-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " INTCALC0 result=" WS-INT-RESULT-CODE
        GO TO TEST-EOD-005-EXIT
    END-IF
    *> Verify payment is due
    IF WS-PAYMENT-DUE NOT = "Y"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " payment-due=" WS-PAYMENT-DUE " expected=Y"
        GO TO TEST-EOD-005-EXIT
    END-IF
    *> Payment amount = $41.10 (41.095890 rounded half-up)
    IF WS-PAYMENT-AMT NOT = 41.10
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " payment-amt=" WS-PAYMENT-AMT
            " expected=41.10"
        GO TO TEST-EOD-005-EXIT
    END-IF
    *> Accrual should reset to 0 after payment
    IF ACCT-ACCRUED-INT OF WS-ACCT-RECORD = 0
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " accrued-int="
            ACCT-ACCRUED-INT OF WS-ACCT-RECORD
            " expected=0"
    END-IF.
TEST-EOD-005-EXIT.
    CONTINUE.

*> ---------------------------------------------------------------
*> EOD-006: Multi-account GL trial balance
*> Post $500 checking + $300 savings, verify GL balanced at $800
*> ---------------------------------------------------------------
TEST-EOD-006.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EOD-006: GL trial balance after 2 deposits"
        TO WS-TEST-NAME
    INITIALIZE WS-GL-ENTRY
    INITIALIZE WS-GL-RECORD
    INITIALIZE WS-TRIAL-BAL
    INITIALIZE WS-GL-RESULT
    *> --- Initialize GL via GLPOST0
    MOVE "INIT" TO WS-GL-FUNCTION
    CALL "GLPOST0" USING WS-GL-FUNCTION
                         WS-GL-ENTRY
                         WS-GL-RECORD
                         WS-TRIAL-BAL
                         WS-GL-RESULT
    IF WS-GL-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " GLPOST0 INIT result=" WS-GL-RESULT-CODE
        GO TO TEST-EOD-006-EXIT
    END-IF
    *> --- Post checking deposit $500: DR 1010 (Cash), CR 4010
    MOVE "POST" TO WS-GL-FUNCTION
    MOVE 1010000000 TO WS-GLE-DR-ACCT
    MOVE 4010000000 TO WS-GLE-CR-ACCT
    MOVE 500.00 TO WS-GLE-AMOUNT
    MOVE "CHECKING DEPOSIT" TO WS-GLE-DESCRIPTION
    MOVE 20260226 TO WS-GLE-POST-DATE
    CALL "GLPOST0" USING WS-GL-FUNCTION
                         WS-GL-ENTRY
                         WS-GL-RECORD
                         WS-TRIAL-BAL
                         WS-GL-RESULT
    IF WS-GL-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " GLPOST0 POST1 result=" WS-GL-RESULT-CODE
        GO TO TEST-EOD-006-EXIT
    END-IF
    *> --- Post savings deposit $300: DR 1010, CR 4030
    MOVE "POST" TO WS-GL-FUNCTION
    MOVE 1010000000 TO WS-GLE-DR-ACCT
    MOVE 4030000000 TO WS-GLE-CR-ACCT
    MOVE 300.00 TO WS-GLE-AMOUNT
    MOVE "SAVINGS DEPOSIT" TO WS-GLE-DESCRIPTION
    MOVE 20260226 TO WS-GLE-POST-DATE
    CALL "GLPOST0" USING WS-GL-FUNCTION
                         WS-GL-ENTRY
                         WS-GL-RECORD
                         WS-TRIAL-BAL
                         WS-GL-RESULT
    IF WS-GL-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " GLPOST0 POST2 result=" WS-GL-RESULT-CODE
        GO TO TEST-EOD-006-EXIT
    END-IF
    *> --- Run trial balance
    MOVE "TBAL" TO WS-GL-FUNCTION
    CALL "GLPOST0" USING WS-GL-FUNCTION
                         WS-GL-ENTRY
                         WS-GL-RECORD
                         WS-TRIAL-BAL
                         WS-GL-RESULT
    IF WS-GL-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " GLPOST0 TBAL result=" WS-GL-RESULT-CODE
        GO TO TEST-EOD-006-EXIT
    END-IF
    *> Verify balanced: total DR = total CR = $800
    IF WS-TB-IS-BALANCED = "Y"
        IF WS-TB-TOTAL-DEBITS = 800.00
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " total-dr=" WS-TB-TOTAL-DEBITS
                " expected=800.00"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " is-balanced=" WS-TB-IS-BALANCED
            " expected=Y"
    END-IF.
TEST-EOD-006-EXIT.
    CONTINUE.

*> ---------------------------------------------------------------
*> EOD-007: Fee assessment in EOD
*> $500 balance, $12 fee, $1500 min balance -> fee assessed
*> ---------------------------------------------------------------
TEST-EOD-007.
    ADD 1 TO WS-TEST-COUNT
    MOVE "EOD-007: Monthly fee assessment"
        TO WS-TEST-NAME
    INITIALIZE WS-ACCT-RECORD
    INITIALIZE WS-FEE-SCHEDULE-RECORD
    INITIALIZE WS-FEE-RESULT
    *> Set up account: $500 balance, $12 monthly fee
    MOVE 800000000001 TO ACCT-NUMBER OF WS-ACCT-RECORD
    MOVE 0 TO ACCT-CHECK-DIGIT OF WS-ACCT-RECORD
    MOVE "DDA1" TO ACCT-PRODUCT-CODE OF WS-ACCT-RECORD
    MOVE "D" TO ACCT-TYPE OF WS-ACCT-RECORD
    MOVE "CH" TO ACCT-SUB-TYPE OF WS-ACCT-RECORD
    MOVE 500.00 TO ACCT-LEDGER-BAL OF WS-ACCT-RECORD
    MOVE 500.00 TO ACCT-AVAIL-BAL OF WS-ACCT-RECORD
    MOVE 12.00 TO ACCT-MONTHLY-FEE OF WS-ACCT-RECORD
    MOVE "MB" TO ACCT-FEE-WAIVER-CODE OF WS-ACCT-RECORD
    MOVE 1500.00 TO ACCT-FEE-WAIVER-AMT OF WS-ACCT-RECORD
    MOVE "A" TO ACCT-STATUS OF WS-ACCT-RECORD
    *> Set up fee schedule
    MOVE "DDA1" TO FEE-PRODUCT-CODE OF WS-FEE-SCHEDULE-RECORD
    MOVE "MTH" TO FEE-TYPE OF WS-FEE-SCHEDULE-RECORD
    MOVE 12.00 TO FEE-AMOUNT OF WS-FEE-SCHEDULE-RECORD
    MOVE "MONTHLY MAINTENANCE" TO
        FEE-DESCRIPTION OF WS-FEE-SCHEDULE-RECORD
    MOVE "Y" TO FEE-WAIVER-ELIGIBLE OF WS-FEE-SCHEDULE-RECORD
    MOVE 1500.00 TO
        FEE-MIN-BAL-THRESHOLD OF WS-FEE-SCHEDULE-RECORD
    MOVE "A" TO FEE-STATUS OF WS-FEE-SCHEDULE-RECORD
    CALL "FEECALC0" USING WS-ACCT-RECORD
                          WS-FEE-SCHEDULE-RECORD
                          WS-FEE-RESULT
    IF WS-FEE-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " FEECALC0 result=" WS-FEE-RESULT-CODE
        GO TO TEST-EOD-007-EXIT
    END-IF
    *> Fee should be $12.00 (balance $500 < min $1500)
    IF WS-FEE-ASSESSED = 12.00
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " fee-assessed=" WS-FEE-ASSESSED
            " expected=12.00"
    END-IF.
TEST-EOD-007-EXIT.
    CONTINUE.

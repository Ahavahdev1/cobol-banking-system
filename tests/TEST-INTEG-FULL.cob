IDENTIFICATION DIVISION.
PROGRAM-ID. TEST-INTEG-FULL.
*> ================================================================
*> TEST-INTEG-FULL - Full end-to-end integration tests
*> Exercises complete account lifecycle across multiple modules:
*> TXNPOST0, INTCALC0, FEECALC0, ACHRECV0, WIREXFR0,
*> HOLDCALC0, BSACTRO, OFACCHK0
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-TEST-COUNT          PIC 9(3) VALUE 0.
01  WS-PASS-COUNT          PIC 9(3) VALUE 0.
01  WS-FAIL-COUNT          PIC 9(3) VALUE 0.
01  WS-TEST-NAME           PIC X(60).

*> --- Account record (shared across modules)
COPY CPYACCT REPLACING ==ACCT-RECORD== BY ==WS-ACCT-RECORD==.

*> --- Transaction record (TXNPOST0)
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

*> --- ACH entry and result (ACHRECV0)
01  WS-ACH-ENTRY.
    05  WS-ACH-RECORD-TYPE       PIC X(1).
    05  WS-ACH-TXN-CODE          PIC 9(2).
    05  WS-ACH-ROUTING-NUM       PIC 9(9).
    05  WS-ACH-ACCT-NUMBER       PIC X(17).
    05  WS-ACH-AMOUNT            PIC S9(10)V99.
    05  WS-ACH-INDIV-NAME        PIC X(22).
    05  WS-ACH-TRACE-NUMBER      PIC X(15).
    05  WS-ACH-ADDENDA-FLAG      PIC X(1).
    05  WS-ACH-BATCH-COUNT       PIC 9(6).
    05  WS-ACH-BATCH-DR-TOTAL    PIC S9(12)V99.
    05  WS-ACH-BATCH-CR-TOTAL    PIC S9(12)V99.
    05  WS-ACH-BATCH-HASH        PIC 9(10).
01  WS-ACH-RETURN-INFO.
    05  WS-ACH-RETURN-CODE       PIC X(3).
    05  WS-ACH-RETURN-REASON     PIC X(30).
    05  WS-ACH-RETURN-FLAG       PIC X(1).
01  WS-ACH-RESULT.
    05  WS-ACH-RESULT-CODE        PIC X(5).
    05  WS-ACH-RESULT-MSG         PIC X(50).

*> --- Wire transfer (WIREXFR0)
01  WS-WIRE-FUNCTION              PIC X(4).
COPY CPYWIRE REPLACING ==WIRE-RECORD== BY ==WS-WIRE-RECORD==.
01  WS-WIRE-RESULT.
    05  WS-WIRE-RESULT-CODE       PIC X(5).
    05  WS-WIRE-RESULT-MSG        PIC X(50).

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

*> --- BSA/CTR (BSACTRO)
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
01  WS-EXPECTED-BAL               PIC S9(13)V99.
01  WS-EXPECTED-AVAIL             PIC S9(13)V99.
01  WS-EXPECTED-ACCRUED           PIC S9(11)V9(6).
01  WS-ACCRUED-DIFF               PIC S9(11)V9(6).
01  WS-ABS-DIFF                   PIC S9(11)V9(6).

PROCEDURE DIVISION.
MAIN-PROGRAM.
    DISPLAY "========================================".
    DISPLAY "TEST SUITE: INTEGRATION - FULL E2E".
    DISPLAY "========================================".

    PERFORM TEST-IE-F01 THRU TEST-IE-F01-EXIT
    PERFORM TEST-IE-F02 THRU TEST-IE-F02-EXIT
    PERFORM TEST-IE-F03 THRU TEST-IE-F03-EXIT
    PERFORM TEST-IE-F04 THRU TEST-IE-F04-EXIT
    PERFORM TEST-IE-F05 THRU TEST-IE-F05-EXIT
    PERFORM TEST-IE-F06 THRU TEST-IE-F06-EXIT
    PERFORM TEST-IE-F07 THRU TEST-IE-F07-EXIT
    PERFORM TEST-IE-F08 THRU TEST-IE-F08-EXIT
    PERFORM TEST-IE-F09 THRU TEST-IE-F09-EXIT

    DISPLAY "========================================".
    DISPLAY "RESULTS: " WS-PASS-COUNT "/" WS-TEST-COUNT
            " PASSED".
    DISPLAY "         " WS-FAIL-COUNT " FAILED".
    DISPLAY "========================================".
    MOVE WS-FAIL-COUNT TO RETURN-CODE
    STOP RUN.

*> ---------------------------------------------------------------
*> IE-F01: New checking account deposit and withdrawal cycle
*> Initialize checking $0, deposit $5000, withdraw $1000
*> Verify balances and GL entries at each step
*> ---------------------------------------------------------------
TEST-IE-F01.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IE-F01: Checking deposit/withdrawal cycle"
        TO WS-TEST-NAME

    *> --- Step 1: Initialize checking account at $0
    INITIALIZE WS-ACCT-RECORD
    INITIALIZE WS-TXN-RECORD
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE 100000000001 TO ACCT-NUMBER OF WS-ACCT-RECORD
    MOVE 0 TO ACCT-CHECK-DIGIT OF WS-ACCT-RECORD
    MOVE "DDA1" TO ACCT-PRODUCT-CODE OF WS-ACCT-RECORD
    MOVE "D" TO ACCT-TYPE OF WS-ACCT-RECORD
    MOVE "CH" TO ACCT-SUB-TYPE OF WS-ACCT-RECORD
    MOVE 0 TO ACCT-LEDGER-BAL OF WS-ACCT-RECORD
    MOVE 0 TO ACCT-AVAIL-BAL OF WS-ACCT-RECORD
    MOVE 0 TO ACCT-HOLD-AMOUNT OF WS-ACCT-RECORD
    MOVE "A" TO ACCT-STATUS OF WS-ACCT-RECORD

    *> --- Step 2: Post $5000 deposit via TXNPOST0
    MOVE 100000000001 TO TXN-ACCT-NUMBER OF WS-TXN-RECORD
    MOVE "DEP" TO TXN-TYPE OF WS-TXN-RECORD
    MOVE 5000.00 TO TXN-AMOUNT OF WS-TXN-RECORD
    MOVE "C" TO TXN-DR-CR OF WS-TXN-RECORD
    MOVE "BR" TO TXN-CHANNEL OF WS-TXN-RECORD
    MOVE "INITIAL DEPOSIT" TO TXN-DESCRIPTION OF WS-TXN-RECORD
    MOVE 20260226 TO TXN-POST-DATE OF WS-TXN-RECORD
    MOVE 20260226 TO TXN-EFFECTIVE-DATE OF WS-TXN-RECORD
    CALL "TXNPOST0" USING WS-TXN-RECORD
                          WS-ACCT-RECORD
                          WS-GL-ENTRIES
                          WS-TXN-RESULT
    IF WS-TXN-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " deposit TXNPOST0 result=" WS-TXN-RESULT-CODE
        GO TO TEST-IE-F01-EXIT
    END-IF

    *> Verify balance = $5000
    IF ACCT-LEDGER-BAL OF WS-ACCT-RECORD NOT = 5000.00
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " post-deposit bal="
            ACCT-LEDGER-BAL OF WS-ACCT-RECORD
            " expected=5000.00"
        GO TO TEST-IE-F01-EXIT
    END-IF

    *> --- Step 3: Post $1000 withdrawal via TXNPOST0
    INITIALIZE WS-TXN-RECORD
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE 100000000001 TO TXN-ACCT-NUMBER OF WS-TXN-RECORD
    MOVE "WDL" TO TXN-TYPE OF WS-TXN-RECORD
    MOVE 1000.00 TO TXN-AMOUNT OF WS-TXN-RECORD
    MOVE "D" TO TXN-DR-CR OF WS-TXN-RECORD
    MOVE "BR" TO TXN-CHANNEL OF WS-TXN-RECORD
    MOVE "WITHDRAWAL" TO TXN-DESCRIPTION OF WS-TXN-RECORD
    MOVE 20260226 TO TXN-POST-DATE OF WS-TXN-RECORD
    MOVE 20260226 TO TXN-EFFECTIVE-DATE OF WS-TXN-RECORD
    CALL "TXNPOST0" USING WS-TXN-RECORD
                          WS-ACCT-RECORD
                          WS-GL-ENTRIES
                          WS-TXN-RESULT
    IF WS-TXN-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " withdrawal TXNPOST0 result=" WS-TXN-RESULT-CODE
        GO TO TEST-IE-F01-EXIT
    END-IF

    *> Verify balance = $4000
    IF ACCT-LEDGER-BAL OF WS-ACCT-RECORD NOT = 4000.00
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " post-withdrawal bal="
            ACCT-LEDGER-BAL OF WS-ACCT-RECORD
            " expected=4000.00"
        GO TO TEST-IE-F01-EXIT
    END-IF

    *> Verify GL entries are populated
    IF WS-GL-POST-FLAG NOT = "Y"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " GL post flag=" WS-GL-POST-FLAG
            " expected=Y"
        GO TO TEST-IE-F01-EXIT
    END-IF
    IF WS-GL-AMOUNT NOT = 1000.00
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " GL amount=" WS-GL-AMOUNT
            " expected=1000.00"
        GO TO TEST-IE-F01-EXIT
    END-IF

    ADD 1 TO WS-PASS-COUNT
    DISPLAY "  PASS: " WS-TEST-NAME.
TEST-IE-F01-EXIT.
    CONTINUE.

*> ---------------------------------------------------------------
*> IE-F02: ACH credit followed by wire send
*> Init checking $10000, ACH credit $2000, wire send $3000
*> ---------------------------------------------------------------
TEST-IE-F02.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IE-F02: ACH credit then wire send"
        TO WS-TEST-NAME

    *> --- Step 1: Initialize checking with $10000
    INITIALIZE WS-ACCT-RECORD
    MOVE 200000000001 TO ACCT-NUMBER OF WS-ACCT-RECORD
    MOVE 0 TO ACCT-CHECK-DIGIT OF WS-ACCT-RECORD
    MOVE "DDA1" TO ACCT-PRODUCT-CODE OF WS-ACCT-RECORD
    MOVE "D" TO ACCT-TYPE OF WS-ACCT-RECORD
    MOVE "CH" TO ACCT-SUB-TYPE OF WS-ACCT-RECORD
    MOVE 10000.00 TO ACCT-LEDGER-BAL OF WS-ACCT-RECORD
    MOVE 10000.00 TO ACCT-AVAIL-BAL OF WS-ACCT-RECORD
    MOVE 0 TO ACCT-HOLD-AMOUNT OF WS-ACCT-RECORD
    MOVE "A" TO ACCT-STATUS OF WS-ACCT-RECORD

    *> --- Step 2: Process ACH credit of $2000
    INITIALIZE WS-ACH-ENTRY
    INITIALIZE WS-ACH-RETURN-INFO
    INITIALIZE WS-ACH-RESULT
    MOVE "6" TO WS-ACH-RECORD-TYPE
    MOVE 22 TO WS-ACH-TXN-CODE
    MOVE 123456789 TO WS-ACH-ROUTING-NUM
    MOVE "20000000000100" TO WS-ACH-ACCT-NUMBER
    MOVE 2000.00 TO WS-ACH-AMOUNT
    MOVE "PAYROLL DEPOSIT" TO WS-ACH-INDIV-NAME
    MOVE "TR1000000000001" TO WS-ACH-TRACE-NUMBER
    MOVE "N" TO WS-ACH-ADDENDA-FLAG
    MOVE 1 TO WS-ACH-BATCH-COUNT
    MOVE 0 TO WS-ACH-BATCH-DR-TOTAL
    MOVE 2000.00 TO WS-ACH-BATCH-CR-TOTAL
    MOVE 0 TO WS-ACH-BATCH-HASH
    CALL "ACHRECV0" USING WS-ACH-ENTRY
                          WS-ACCT-RECORD
                          WS-ACH-RETURN-INFO
                          WS-ACH-RESULT
    IF WS-ACH-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " ACHRECV0 result=" WS-ACH-RESULT-CODE
            " " WS-ACH-RESULT-MSG
        GO TO TEST-IE-F02-EXIT
    END-IF
    IF WS-ACH-RETURN-FLAG = "Y"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " ACH returned: " WS-ACH-RETURN-CODE
            " " WS-ACH-RETURN-REASON
        GO TO TEST-IE-F02-EXIT
    END-IF

    *> Verify balance = $12000
    IF ACCT-LEDGER-BAL OF WS-ACCT-RECORD NOT = 12000.00
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " post-ACH bal="
            ACCT-LEDGER-BAL OF WS-ACCT-RECORD
            " expected=12000.00"
        GO TO TEST-IE-F02-EXIT
    END-IF

    *> --- Step 3: Send $3000 wire via WIREXFR0
    INITIALIZE WS-WIRE-RECORD
    INITIALIZE WS-WIRE-RESULT
    MOVE "SEND" TO WS-WIRE-FUNCTION
    MOVE "WR-REF-00000001" TO
        WIRE-REFERENCE-NUM OF WS-WIRE-RECORD
    MOVE 3000.00 TO WIRE-AMOUNT OF WS-WIRE-RECORD
    MOVE "USD" TO WIRE-CURRENCY OF WS-WIRE-RECORD
    MOVE "N" TO WIRE-PRIORITY OF WS-WIRE-RECORD
    MOVE "JOHN DOE" TO WIRE-BENE-NAME OF WS-WIRE-RECORD
    MOVE "US " TO WIRE-BENE-COUNTRY OF WS-WIRE-RECORD
    MOVE "PAYMENT FOR SERVICES"
        TO WIRE-PURPOSE OF WS-WIRE-RECORD
    MOVE "APPROVER" TO WIRE-APPROVED-BY OF WS-WIRE-RECORD
    CALL "WIREXFR0" USING WS-WIRE-FUNCTION
                          WS-WIRE-RECORD
                          WS-ACCT-RECORD
                          WS-WIRE-RESULT
    IF WS-WIRE-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " WIREXFR0 result=" WS-WIRE-RESULT-CODE
            " " WS-WIRE-RESULT-MSG
        GO TO TEST-IE-F02-EXIT
    END-IF

    *> Verify balance = $9000
    IF ACCT-LEDGER-BAL OF WS-ACCT-RECORD NOT = 9000.00
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " post-wire bal="
            ACCT-LEDGER-BAL OF WS-ACCT-RECORD
            " expected=9000.00"
        GO TO TEST-IE-F02-EXIT
    END-IF

    ADD 1 TO WS-PASS-COUNT
    DISPLAY "  PASS: " WS-TEST-NAME.
TEST-IE-F02-EXIT.
    CONTINUE.

*> ---------------------------------------------------------------
*> IE-F03: Interest accrual over multiple days
*> Savings $50000, 3% rate, Actual/365 - accrue 3 days
*> Expected daily = 50000 * 0.03 / 365 = 4.109589
*> After 3 days, accrued ~= 12.328767
*> ---------------------------------------------------------------
TEST-IE-F03.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IE-F03: Multi-day interest accrual"
        TO WS-TEST-NAME

    *> --- Set up savings account at $50000, 3% Actual/365
    INITIALIZE WS-ACCT-RECORD
    INITIALIZE WS-INT-RESULT
    MOVE 300000000002 TO ACCT-NUMBER OF WS-ACCT-RECORD
    MOVE 0 TO ACCT-CHECK-DIGIT OF WS-ACCT-RECORD
    MOVE "SAV1" TO ACCT-PRODUCT-CODE OF WS-ACCT-RECORD
    MOVE "D" TO ACCT-TYPE OF WS-ACCT-RECORD
    MOVE "SV" TO ACCT-SUB-TYPE OF WS-ACCT-RECORD
    MOVE 50000.00 TO ACCT-LEDGER-BAL OF WS-ACCT-RECORD
    MOVE 50000.00 TO ACCT-AVAIL-BAL OF WS-ACCT-RECORD
    MOVE 3.0000000 TO ACCT-INT-RATE OF WS-ACCT-RECORD
    MOVE "F" TO ACCT-INT-RATE-TYPE OF WS-ACCT-RECORD
    MOVE "DB" TO ACCT-INT-CALC-METHOD OF WS-ACCT-RECORD
    MOVE "A" TO ACCT-INT-ACCRUAL-BASIS OF WS-ACCT-RECORD
    MOVE "M" TO ACCT-INT-PAY-FREQ OF WS-ACCT-RECORD
    MOVE 20260331 TO ACCT-INT-NEXT-PAY-DATE OF WS-ACCT-RECORD
    MOVE 0 TO ACCT-ACCRUED-INT OF WS-ACCT-RECORD
    MOVE "A" TO ACCT-STATUS OF WS-ACCT-RECORD

    *> --- Day 1: Accrue interest
    INITIALIZE WS-INT-RESULT
    MOVE 20260301 TO WS-CALC-DATE
    CALL "INTCALC0" USING WS-ACCT-RECORD
                          WS-CALC-DATE
                          WS-INT-RESULT
    IF WS-INT-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " Day1 INTCALC0 result=" WS-INT-RESULT-CODE
        GO TO TEST-IE-F03-EXIT
    END-IF

    *> --- Day 2: Accrue interest
    INITIALIZE WS-INT-RESULT
    MOVE 20260302 TO WS-CALC-DATE
    CALL "INTCALC0" USING WS-ACCT-RECORD
                          WS-CALC-DATE
                          WS-INT-RESULT
    IF WS-INT-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " Day2 INTCALC0 result=" WS-INT-RESULT-CODE
        GO TO TEST-IE-F03-EXIT
    END-IF

    *> --- Day 3: Accrue interest
    INITIALIZE WS-INT-RESULT
    MOVE 20260303 TO WS-CALC-DATE
    CALL "INTCALC0" USING WS-ACCT-RECORD
                          WS-CALC-DATE
                          WS-INT-RESULT
    IF WS-INT-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " Day3 INTCALC0 result=" WS-INT-RESULT-CODE
        GO TO TEST-IE-F03-EXIT
    END-IF

    *> Verify accrued interest ~= 12.328767
    *> Daily = 50000 * 0.03 / 365 = 4.109589 (6dp)
    *> 3 days = 12.328767
    MOVE 12.328767 TO WS-EXPECTED-ACCRUED
    COMPUTE WS-ACCRUED-DIFF =
        ACCT-ACCRUED-INT OF WS-ACCT-RECORD
        - WS-EXPECTED-ACCRUED
    IF WS-ACCRUED-DIFF < 0
        MULTIPLY -1 BY WS-ACCRUED-DIFF
            GIVING WS-ABS-DIFF
    ELSE
        MOVE WS-ACCRUED-DIFF TO WS-ABS-DIFF
    END-IF
    *> Allow tolerance of 0.000002 for rounding
    IF WS-ABS-DIFF <= 0.000002
        *> Also verify it accumulated (not reset)
        IF ACCT-ACCRUED-INT OF WS-ACCT-RECORD > 8.000000
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " accrued too low="
                ACCT-ACCRUED-INT OF WS-ACCT-RECORD
                " (may have reset between calls)"
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " accrued="
            ACCT-ACCRUED-INT OF WS-ACCT-RECORD
            " expected~=" WS-EXPECTED-ACCRUED
    END-IF.
TEST-IE-F03-EXIT.
    CONTINUE.

*> ---------------------------------------------------------------
*> IE-F04: Check deposit with hold, then release
*> Init checking $1000, local check $5000 deposit
*> Verify hold amounts and next-day availability
*> Post deposit, set hold, verify available balance
*> ---------------------------------------------------------------
TEST-IE-F04.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IE-F04: Check deposit with hold"
        TO WS-TEST-NAME

    *> --- Step 1: Initialize checking with $1000
    INITIALIZE WS-ACCT-RECORD
    MOVE 400000000002 TO ACCT-NUMBER OF WS-ACCT-RECORD
    MOVE 0 TO ACCT-CHECK-DIGIT OF WS-ACCT-RECORD
    MOVE "DDA1" TO ACCT-PRODUCT-CODE OF WS-ACCT-RECORD
    MOVE "D" TO ACCT-TYPE OF WS-ACCT-RECORD
    MOVE "CH" TO ACCT-SUB-TYPE OF WS-ACCT-RECORD
    MOVE 1000.00 TO ACCT-LEDGER-BAL OF WS-ACCT-RECORD
    MOVE 1000.00 TO ACCT-AVAIL-BAL OF WS-ACCT-RECORD
    MOVE 0 TO ACCT-HOLD-AMOUNT OF WS-ACCT-RECORD
    MOVE "A" TO ACCT-STATUS OF WS-ACCT-RECORD
    MOVE 20250101 TO ACCT-OPEN-DATE OF WS-ACCT-RECORD

    *> --- Step 2: Calculate hold via HOLDCALC0 for $5000 local check
    INITIALIZE WS-HOLD-REQUEST
    INITIALIZE WS-HOLD-RECORD
    INITIALIZE WS-HOLD-RESULT
    MOVE 400000000002 TO WS-HR-ACCT-NUMBER
    MOVE 5000.00 TO WS-HR-DEPOSIT-AMT
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
        GO TO TEST-IE-F04-EXIT
    END-IF

    *> Reg CC: $225 next-day, remaining = $4775 on hold
    IF WS-HOLD-NEXT-DAY-AMT NOT = 225.00
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " next-day=" WS-HOLD-NEXT-DAY-AMT
            " expected=225.00"
        GO TO TEST-IE-F04-EXIT
    END-IF
    IF WS-HOLD-REMAINING-AMT NOT = 4775.00
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " remaining=" WS-HOLD-REMAINING-AMT
            " expected=4775.00"
        GO TO TEST-IE-F04-EXIT
    END-IF

    *> --- Step 3: Post the $5000 deposit via TXNPOST0
    INITIALIZE WS-TXN-RECORD
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE 400000000002 TO TXN-ACCT-NUMBER OF WS-TXN-RECORD
    MOVE "DEP" TO TXN-TYPE OF WS-TXN-RECORD
    MOVE 5000.00 TO TXN-AMOUNT OF WS-TXN-RECORD
    MOVE "C" TO TXN-DR-CR OF WS-TXN-RECORD
    MOVE "BR" TO TXN-CHANNEL OF WS-TXN-RECORD
    MOVE "CHECK DEPOSIT" TO TXN-DESCRIPTION OF WS-TXN-RECORD
    MOVE 20260226 TO TXN-POST-DATE OF WS-TXN-RECORD
    MOVE 20260226 TO TXN-EFFECTIVE-DATE OF WS-TXN-RECORD
    CALL "TXNPOST0" USING WS-TXN-RECORD
                          WS-ACCT-RECORD
                          WS-GL-ENTRIES
                          WS-TXN-RESULT
    IF WS-TXN-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " deposit TXNPOST0 result=" WS-TXN-RESULT-CODE
        GO TO TEST-IE-F04-EXIT
    END-IF

    *> --- Step 4: Set hold amount on account to remaining amount
    MOVE WS-HOLD-REMAINING-AMT
        TO ACCT-HOLD-AMOUNT OF WS-ACCT-RECORD
    *> Recompute available balance = ledger - hold
    COMPUTE ACCT-AVAIL-BAL OF WS-ACCT-RECORD =
        ACCT-LEDGER-BAL OF WS-ACCT-RECORD
        - ACCT-HOLD-AMOUNT OF WS-ACCT-RECORD

    *> Verify ledger = $6000, available = $6000 - $4775 = $1225
    IF ACCT-LEDGER-BAL OF WS-ACCT-RECORD NOT = 6000.00
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " ledger="
            ACCT-LEDGER-BAL OF WS-ACCT-RECORD
            " expected=6000.00"
        GO TO TEST-IE-F04-EXIT
    END-IF
    MOVE 1225.00 TO WS-EXPECTED-AVAIL
    IF ACCT-AVAIL-BAL OF WS-ACCT-RECORD NOT = WS-EXPECTED-AVAIL
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " avail="
            ACCT-AVAIL-BAL OF WS-ACCT-RECORD
            " expected=1225.00"
        GO TO TEST-IE-F04-EXIT
    END-IF

    ADD 1 TO WS-PASS-COUNT
    DISPLAY "  PASS: " WS-TEST-NAME.
TEST-IE-F04-EXIT.
    CONTINUE.

*> ---------------------------------------------------------------
*> IE-F05: BSA/AML cash aggregation triggers CTR
*> 3 cash deposits ($4000 + $3000 + $4000 = $11000) via AGGR
*> Then CHEK to verify CTR required = Y
*> ---------------------------------------------------------------
TEST-IE-F05.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IE-F05: BSA/AML CTR aggregation"
        TO WS-TEST-NAME

    *> --- Initialize BSA CTR record
    INITIALIZE WS-CTR-RECORD
    INITIALIZE WS-TXN-INFO
    INITIALIZE WS-BSA-RESULT
    MOVE 2000000001 TO CTR-CUST-ID OF WS-CTR-RECORD
    MOVE 20260226 TO CTR-TXN-DATE OF WS-CTR-RECORD
    MOVE "N" TO CTR-EXEMPT-FLAG OF WS-CTR-RECORD

    *> --- Cash deposit 1: $4000
    MOVE "AGGR" TO WS-BSA-FUNCTION
    MOVE 2000000001 TO WS-BSA-CUST-ID
    MOVE 20260226 TO WS-BSA-TXN-DATE
    MOVE 4000.00 TO WS-BSA-CASH-AMOUNT
    MOVE "I" TO WS-BSA-CASH-DIRECTION
    MOVE "Y" TO WS-BSA-IS-CASH
    MOVE 500000000001 TO WS-BSA-ACCT-NUMBER
    CALL "BSACTRO" USING WS-BSA-FUNCTION
                         WS-CTR-RECORD
                         WS-TXN-INFO
                         WS-BSA-RESULT
    IF WS-BSA-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " AGGR1 result=" WS-BSA-RESULT-CODE
        GO TO TEST-IE-F05-EXIT
    END-IF

    *> --- Cash deposit 2: $3000
    MOVE 3000.00 TO WS-BSA-CASH-AMOUNT
    CALL "BSACTRO" USING WS-BSA-FUNCTION
                         WS-CTR-RECORD
                         WS-TXN-INFO
                         WS-BSA-RESULT
    IF WS-BSA-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " AGGR2 result=" WS-BSA-RESULT-CODE
        GO TO TEST-IE-F05-EXIT
    END-IF

    *> --- Cash deposit 3: $4000
    MOVE 4000.00 TO WS-BSA-CASH-AMOUNT
    CALL "BSACTRO" USING WS-BSA-FUNCTION
                         WS-CTR-RECORD
                         WS-TXN-INFO
                         WS-BSA-RESULT
    IF WS-BSA-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " AGGR3 result=" WS-BSA-RESULT-CODE
        GO TO TEST-IE-F05-EXIT
    END-IF

    *> --- Check CTR requirement via CHEK
    MOVE "CHEK" TO WS-BSA-FUNCTION
    INITIALIZE WS-BSA-RESULT
    CALL "BSACTRO" USING WS-BSA-FUNCTION
                         WS-CTR-RECORD
                         WS-TXN-INFO
                         WS-BSA-RESULT
    IF WS-BSA-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " CHEK result=" WS-BSA-RESULT-CODE
        GO TO TEST-IE-F05-EXIT
    END-IF
    IF WS-BSA-CTR-REQUIRED NOT = "Y"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " ctr-required=" WS-BSA-CTR-REQUIRED
            " expected=Y ($11000 aggregate)"
        GO TO TEST-IE-F05-EXIT
    END-IF

    ADD 1 TO WS-PASS-COUNT
    DISPLAY "  PASS: " WS-TEST-NAME.
TEST-IE-F05-EXIT.
    CONTINUE.

*> ---------------------------------------------------------------
*> IE-F06: Overdraft scenario - insufficient funds
*> Init checking $100, no OD protection, try $150 debit
*> Should fail with E0030 insufficient funds, balance unchanged
*> ---------------------------------------------------------------
TEST-IE-F06.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IE-F06: Overdraft - insufficient funds"
        TO WS-TEST-NAME

    *> --- Initialize checking with $100, no OD protection
    INITIALIZE WS-ACCT-RECORD
    INITIALIZE WS-TXN-RECORD
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE 600000000001 TO ACCT-NUMBER OF WS-ACCT-RECORD
    MOVE 0 TO ACCT-CHECK-DIGIT OF WS-ACCT-RECORD
    MOVE "DDA1" TO ACCT-PRODUCT-CODE OF WS-ACCT-RECORD
    MOVE "D" TO ACCT-TYPE OF WS-ACCT-RECORD
    MOVE "CH" TO ACCT-SUB-TYPE OF WS-ACCT-RECORD
    MOVE 100.00 TO ACCT-LEDGER-BAL OF WS-ACCT-RECORD
    MOVE 100.00 TO ACCT-AVAIL-BAL OF WS-ACCT-RECORD
    MOVE 0 TO ACCT-HOLD-AMOUNT OF WS-ACCT-RECORD
    MOVE "N" TO ACCT-OD-PROTECTION OF WS-ACCT-RECORD
    MOVE "Y" TO ACCT-OD-OPTED-IN OF WS-ACCT-RECORD
    MOVE "A" TO ACCT-STATUS OF WS-ACCT-RECORD

    *> --- Try $150 debit - should fail insufficient funds
    MOVE 600000000001 TO TXN-ACCT-NUMBER OF WS-TXN-RECORD
    MOVE "WDL" TO TXN-TYPE OF WS-TXN-RECORD
    MOVE 150.00 TO TXN-AMOUNT OF WS-TXN-RECORD
    MOVE "D" TO TXN-DR-CR OF WS-TXN-RECORD
    MOVE "BR" TO TXN-CHANNEL OF WS-TXN-RECORD
    MOVE "ATM WITHDRAWAL" TO TXN-DESCRIPTION OF WS-TXN-RECORD
    MOVE 20260226 TO TXN-POST-DATE OF WS-TXN-RECORD
    MOVE 20260226 TO TXN-EFFECTIVE-DATE OF WS-TXN-RECORD
    CALL "TXNPOST0" USING WS-TXN-RECORD
                          WS-ACCT-RECORD
                          WS-GL-ENTRIES
                          WS-TXN-RESULT

    *> Verify E0030 insufficient funds
    IF WS-TXN-RESULT-CODE NOT = "E0030"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-TXN-RESULT-CODE
            " expected=E0030"
        GO TO TEST-IE-F06-EXIT
    END-IF

    *> Verify balance unchanged at $100
    IF ACCT-LEDGER-BAL OF WS-ACCT-RECORD NOT = 100.00
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " bal=" ACCT-LEDGER-BAL OF WS-ACCT-RECORD
            " expected=100.00 (unchanged)"
        GO TO TEST-IE-F06-EXIT
    END-IF

    ADD 1 TO WS-PASS-COUNT
    DISPLAY "  PASS: " WS-TEST-NAME.
TEST-IE-F06-EXIT.
    CONTINUE.

*> ---------------------------------------------------------------
*> IE-F07: Fee waiver and non-waiver in sequence
*> Init with $2000, MB waiver at $1500
*> Call 1: balance >= threshold -> fee waived
*> Call 2: balance = $1000 -> fee assessed $12
*> ---------------------------------------------------------------
TEST-IE-F07.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IE-F07: Fee waiver then non-waiver"
        TO WS-TEST-NAME

    *> --- Initialize account with $2000, MB waiver code
    INITIALIZE WS-ACCT-RECORD
    INITIALIZE WS-FEE-SCHEDULE-RECORD
    INITIALIZE WS-FEE-RESULT
    MOVE 700000000002 TO ACCT-NUMBER OF WS-ACCT-RECORD
    MOVE 0 TO ACCT-CHECK-DIGIT OF WS-ACCT-RECORD
    MOVE "DDA1" TO ACCT-PRODUCT-CODE OF WS-ACCT-RECORD
    MOVE "D" TO ACCT-TYPE OF WS-ACCT-RECORD
    MOVE "CH" TO ACCT-SUB-TYPE OF WS-ACCT-RECORD
    MOVE 2000.00 TO ACCT-LEDGER-BAL OF WS-ACCT-RECORD
    MOVE 2000.00 TO ACCT-AVAIL-BAL OF WS-ACCT-RECORD
    MOVE 2000.00 TO ACCT-MTD-LOW-BAL OF WS-ACCT-RECORD
    MOVE "MB" TO ACCT-FEE-WAIVER-CODE OF WS-ACCT-RECORD
    MOVE 12.00 TO ACCT-MONTHLY-FEE OF WS-ACCT-RECORD
    MOVE "A" TO ACCT-STATUS OF WS-ACCT-RECORD

    *> --- Set up fee schedule: $12 monthly, min bal waiver at $1500
    MOVE "DDA1" TO FEE-PRODUCT-CODE OF WS-FEE-SCHEDULE-RECORD
    MOVE "MTH" TO FEE-TYPE OF WS-FEE-SCHEDULE-RECORD
    MOVE 12.00 TO FEE-AMOUNT OF WS-FEE-SCHEDULE-RECORD
    MOVE "MONTHLY MAINTENANCE" TO
        FEE-DESCRIPTION OF WS-FEE-SCHEDULE-RECORD
    MOVE "Y" TO FEE-WAIVER-ELIGIBLE OF WS-FEE-SCHEDULE-RECORD
    MOVE 1500.00 TO
        FEE-MIN-BAL-THRESHOLD OF WS-FEE-SCHEDULE-RECORD
    MOVE "A" TO FEE-STATUS OF WS-FEE-SCHEDULE-RECORD

    *> --- Call 1: Balance $2000 >= $1500 threshold -> waived
    CALL "FEECALC0" USING WS-ACCT-RECORD
                          WS-FEE-SCHEDULE-RECORD
                          WS-FEE-RESULT
    IF WS-FEE-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " Call1 FEECALC0 result=" WS-FEE-RESULT-CODE
        GO TO TEST-IE-F07-EXIT
    END-IF
    IF WS-FEE-WAIVED-FLAG NOT = "Y"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " Call1 waived=" WS-FEE-WAIVED-FLAG
            " expected=Y"
        GO TO TEST-IE-F07-EXIT
    END-IF
    IF WS-FEE-ASSESSED NOT = 0
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " Call1 fee=" WS-FEE-ASSESSED
            " expected=0"
        GO TO TEST-IE-F07-EXIT
    END-IF

    *> --- Call 2: Drop balance to $1000, fee should be assessed
    MOVE 1000.00 TO ACCT-LEDGER-BAL OF WS-ACCT-RECORD
    MOVE 1000.00 TO ACCT-AVAIL-BAL OF WS-ACCT-RECORD
    MOVE 1000.00 TO ACCT-MTD-LOW-BAL OF WS-ACCT-RECORD
    INITIALIZE WS-FEE-RESULT
    CALL "FEECALC0" USING WS-ACCT-RECORD
                          WS-FEE-SCHEDULE-RECORD
                          WS-FEE-RESULT
    IF WS-FEE-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " Call2 FEECALC0 result=" WS-FEE-RESULT-CODE
        GO TO TEST-IE-F07-EXIT
    END-IF
    IF WS-FEE-ASSESSED NOT = 12.00
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " Call2 fee=" WS-FEE-ASSESSED
            " expected=12.00"
        GO TO TEST-IE-F07-EXIT
    END-IF
    IF WS-FEE-WAIVED-FLAG NOT = "N"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " Call2 waived=" WS-FEE-WAIVED-FLAG
            " expected=N"
        GO TO TEST-IE-F07-EXIT
    END-IF

    ADD 1 TO WS-PASS-COUNT
    DISPLAY "  PASS: " WS-TEST-NAME.
TEST-IE-F07-EXIT.
    CONTINUE.

*> ---------------------------------------------------------------
*> IE-F08: Wire transfer with OFAC block
*> Init checking $50000, wire to sanctioned country KP
*> Should be blocked with E0025 (OFAC match), balance unchanged
*> ---------------------------------------------------------------
TEST-IE-F08.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IE-F08: Wire transfer OFAC block"
        TO WS-TEST-NAME

    *> --- Initialize checking with $50000
    INITIALIZE WS-ACCT-RECORD
    MOVE 800000000002 TO ACCT-NUMBER OF WS-ACCT-RECORD
    MOVE 0 TO ACCT-CHECK-DIGIT OF WS-ACCT-RECORD
    MOVE "DDA1" TO ACCT-PRODUCT-CODE OF WS-ACCT-RECORD
    MOVE "D" TO ACCT-TYPE OF WS-ACCT-RECORD
    MOVE "CH" TO ACCT-SUB-TYPE OF WS-ACCT-RECORD
    MOVE 50000.00 TO ACCT-LEDGER-BAL OF WS-ACCT-RECORD
    MOVE 50000.00 TO ACCT-AVAIL-BAL OF WS-ACCT-RECORD
    MOVE 0 TO ACCT-HOLD-AMOUNT OF WS-ACCT-RECORD
    MOVE "A" TO ACCT-STATUS OF WS-ACCT-RECORD

    *> --- Set up wire to sanctioned country "KP" (North Korea)
    INITIALIZE WS-WIRE-RECORD
    INITIALIZE WS-WIRE-RESULT
    MOVE "SEND" TO WS-WIRE-FUNCTION
    MOVE "WR-REF-00000002" TO
        WIRE-REFERENCE-NUM OF WS-WIRE-RECORD
    MOVE 5000.00 TO WIRE-AMOUNT OF WS-WIRE-RECORD
    MOVE "USD" TO WIRE-CURRENCY OF WS-WIRE-RECORD
    MOVE "N" TO WIRE-PRIORITY OF WS-WIRE-RECORD
    MOVE "SOME COMPANY LTD" TO
        WIRE-BENE-NAME OF WS-WIRE-RECORD
    MOVE "KP " TO WIRE-BENE-COUNTRY OF WS-WIRE-RECORD
    MOVE "TRADE PAYMENT" TO WIRE-PURPOSE OF WS-WIRE-RECORD
    MOVE "APPROVER" TO WIRE-APPROVED-BY OF WS-WIRE-RECORD
    CALL "WIREXFR0" USING WS-WIRE-FUNCTION
                          WS-WIRE-RECORD
                          WS-ACCT-RECORD
                          WS-WIRE-RESULT

    *> Verify blocked with E0025 OFAC match
    IF WS-WIRE-RESULT-CODE NOT = "E0025"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-WIRE-RESULT-CODE
            " expected=E0025"
            " msg=" WS-WIRE-RESULT-MSG
        GO TO TEST-IE-F08-EXIT
    END-IF

    *> Verify balance unchanged at $50000
    IF ACCT-LEDGER-BAL OF WS-ACCT-RECORD NOT = 50000.00
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " bal=" ACCT-LEDGER-BAL OF WS-ACCT-RECORD
            " expected=50000.00 (unchanged)"
        GO TO TEST-IE-F08-EXIT
    END-IF

    ADD 1 TO WS-PASS-COUNT
    DISPLAY "  PASS: " WS-TEST-NAME.
TEST-IE-F08-EXIT.
    CONTINUE.

*> ---------------------------------------------------------------
*> IE-F09: Hold blocks over-withdrawal (available balance check)
*> Deposit $6000, place $4775 hold (non-local check), try $2000
*> withdrawal. Available = $6000 - $4775 = $1225 < $2000 -> E0030
*> This tests the critical TXNPOST0 <-> HOLDCALC0 interaction:
*> TXNPOST0 checks ACCT-AVAIL-BAL (not ledger), so holds prevent
*> over-withdrawal even when ledger balance is sufficient.
*> ---------------------------------------------------------------
TEST-IE-F09.
    ADD 1 TO WS-TEST-COUNT
    MOVE "IE-F09: Hold blocks over-withdrawal"
        TO WS-TEST-NAME

    *> --- Step 1: Initialize checking account at $0
    INITIALIZE WS-ACCT-RECORD
    INITIALIZE WS-TXN-RECORD
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE 900000000001 TO ACCT-NUMBER OF WS-ACCT-RECORD
    MOVE 0 TO ACCT-CHECK-DIGIT OF WS-ACCT-RECORD
    MOVE "DDA1" TO ACCT-PRODUCT-CODE OF WS-ACCT-RECORD
    MOVE "D" TO ACCT-TYPE OF WS-ACCT-RECORD
    MOVE "CH" TO ACCT-SUB-TYPE OF WS-ACCT-RECORD
    MOVE 0 TO ACCT-LEDGER-BAL OF WS-ACCT-RECORD
    MOVE 0 TO ACCT-AVAIL-BAL OF WS-ACCT-RECORD
    MOVE 0 TO ACCT-HOLD-AMOUNT OF WS-ACCT-RECORD
    MOVE "N" TO ACCT-OD-PROTECTION OF WS-ACCT-RECORD
    MOVE "N" TO ACCT-OD-OPTED-IN OF WS-ACCT-RECORD
    MOVE "A" TO ACCT-STATUS OF WS-ACCT-RECORD
    MOVE 20250101 TO ACCT-OPEN-DATE OF WS-ACCT-RECORD

    *> --- Step 2: Post $6000 deposit via TXNPOST0
    MOVE 900000000001 TO TXN-ACCT-NUMBER OF WS-TXN-RECORD
    MOVE "DEP" TO TXN-TYPE OF WS-TXN-RECORD
    MOVE 6000.00 TO TXN-AMOUNT OF WS-TXN-RECORD
    MOVE "C" TO TXN-DR-CR OF WS-TXN-RECORD
    MOVE "BR" TO TXN-CHANNEL OF WS-TXN-RECORD
    MOVE "CHECK DEPOSIT" TO TXN-DESCRIPTION OF WS-TXN-RECORD
    MOVE 20260226 TO TXN-POST-DATE OF WS-TXN-RECORD
    MOVE 20260226 TO TXN-EFFECTIVE-DATE OF WS-TXN-RECORD
    CALL "TXNPOST0" USING WS-TXN-RECORD
                          WS-ACCT-RECORD
                          WS-GL-ENTRIES
                          WS-TXN-RESULT
    IF WS-TXN-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " deposit TXNPOST0 result=" WS-TXN-RESULT-CODE
        GO TO TEST-IE-F09-EXIT
    END-IF

    *> Verify ledger = $6000
    IF ACCT-LEDGER-BAL OF WS-ACCT-RECORD NOT = 6000.00
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " post-deposit ledger="
            ACCT-LEDGER-BAL OF WS-ACCT-RECORD
            " expected=6000.00"
        GO TO TEST-IE-F09-EXIT
    END-IF

    *> --- Step 3: Place hold via HOLDCALC0 (non-local check $6000)
    *> Reg CC: $225 next-day avail, remaining $5775 on hold
    *> But we want $4775 hold, so we use the hold result to set it
    INITIALIZE WS-HOLD-REQUEST
    INITIALIZE WS-HOLD-RECORD
    INITIALIZE WS-HOLD-RESULT
    MOVE 900000000001 TO WS-HR-ACCT-NUMBER
    MOVE 6000.00 TO WS-HR-DEPOSIT-AMT
    MOVE "NL" TO WS-HR-CHECK-TYPE
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
        GO TO TEST-IE-F09-EXIT
    END-IF

    *> --- Step 4: Apply hold to account
    *> Set hold amount = remaining amount from HOLDCALC0
    *> For NL (non-local) check $6000: next-day=$225, remaining=$5775
    *> Use the actual hold remaining from HOLDCALC0
    MOVE WS-HOLD-REMAINING-AMT
        TO ACCT-HOLD-AMOUNT OF WS-ACCT-RECORD
    *> Recompute available balance = ledger - hold
    COMPUTE ACCT-AVAIL-BAL OF WS-ACCT-RECORD =
        ACCT-LEDGER-BAL OF WS-ACCT-RECORD
        - ACCT-HOLD-AMOUNT OF WS-ACCT-RECORD

    *> Verify available is less than $2000 (the withdrawal amount)
    IF ACCT-AVAIL-BAL OF WS-ACCT-RECORD >= 2000.00
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " avail=" ACCT-AVAIL-BAL OF WS-ACCT-RECORD
            " should be < 2000.00 for test to be valid"
        GO TO TEST-IE-F09-EXIT
    END-IF

    *> --- Step 5: Attempt $2000 withdrawal - should FAIL
    *> Available = ledger($6000) - hold(remaining) < $2000
    INITIALIZE WS-TXN-RECORD
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE 900000000001 TO TXN-ACCT-NUMBER OF WS-TXN-RECORD
    MOVE "WDL" TO TXN-TYPE OF WS-TXN-RECORD
    MOVE 2000.00 TO TXN-AMOUNT OF WS-TXN-RECORD
    MOVE "D" TO TXN-DR-CR OF WS-TXN-RECORD
    MOVE "BR" TO TXN-CHANNEL OF WS-TXN-RECORD
    MOVE "WITHDRAWAL" TO TXN-DESCRIPTION OF WS-TXN-RECORD
    MOVE 20260226 TO TXN-POST-DATE OF WS-TXN-RECORD
    MOVE 20260226 TO TXN-EFFECTIVE-DATE OF WS-TXN-RECORD
    CALL "TXNPOST0" USING WS-TXN-RECORD
                          WS-ACCT-RECORD
                          WS-GL-ENTRIES
                          WS-TXN-RESULT

    *> Verify E0030 insufficient available funds
    IF WS-TXN-RESULT-CODE NOT = "E0030"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " withdrawal result=" WS-TXN-RESULT-CODE
            " expected=E0030 (insufficient available funds)"
        GO TO TEST-IE-F09-EXIT
    END-IF

    *> Verify ledger balance UNCHANGED at $6000
    IF ACCT-LEDGER-BAL OF WS-ACCT-RECORD NOT = 6000.00
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " ledger=" ACCT-LEDGER-BAL OF WS-ACCT-RECORD
            " expected=6000.00 (unchanged)"
        GO TO TEST-IE-F09-EXIT
    END-IF

    ADD 1 TO WS-PASS-COUNT
    DISPLAY "  PASS: " WS-TEST-NAME.
TEST-IE-F09-EXIT.
    CONTINUE.

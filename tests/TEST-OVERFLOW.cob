IDENTIFICATION DIVISION.
PROGRAM-ID. TEST-OVERFLOW.
*> ================================================================
*> TEST-OVERFLOW - Test suite for arithmetic overflow protection
*> Tests: ON SIZE ERROR handling across TXNPOST0, GLPOST0,
*>        INTCALC0, FEECALC0, BSACTRO (9 tests)
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-TEST-COUNT          PIC 9(3) VALUE 0.
01  WS-PASS-COUNT          PIC 9(3) VALUE 0.
01  WS-FAIL-COUNT          PIC 9(3) VALUE 0.
01  WS-TEST-NAME           PIC X(60).

*> TXNPOST0 linkage
COPY CPYTXN.
COPY CPYACCT.
01  WS-GL-ENTRIES.
    05  WS-GL-DR-ACCOUNT   PIC 9(10).
    05  WS-GL-CR-ACCOUNT   PIC 9(10).
    05  WS-GL-AMOUNT       PIC S9(13)V99.
    05  WS-GL-POST-FLAG    PIC X(1).
01  WS-TXN-RESULT.
    05  WS-TXN-RESULT-CODE PIC X(5).
    05  WS-TXN-RESULT-MSG  PIC X(50).

*> GLPOST0 linkage
01  WS-GL-FUNCTION         PIC X(4).
01  WS-GL-ENTRY.
    05  WS-GLE-DR-ACCT     PIC 9(10).
    05  WS-GLE-CR-ACCT     PIC 9(10).
    05  WS-GLE-AMOUNT      PIC S9(15)V99.
    05  WS-GLE-DESCRIPTION PIC X(40).
    05  WS-GLE-POST-DATE   PIC 9(8).
COPY CPYGL.
01  WS-TRIAL-BAL.
    05  WS-TB-TOTAL-DEBITS   PIC S9(15)V99.
    05  WS-TB-TOTAL-CREDITS  PIC S9(15)V99.
    05  WS-TB-DIFFERENCE     PIC S9(15)V99.
    05  WS-TB-IS-BALANCED    PIC X(1).
01  WS-GL-RESULT.
    05  WS-GL-RESULT-CODE   PIC X(5).
    05  WS-GL-RESULT-MSG    PIC X(50).

*> INTCALC0 linkage
01  WS-CALC-DATE           PIC 9(8).
01  WS-INT-RESULT.
    05  WS-INT-RESULT-CODE PIC X(5).
    05  WS-INT-RESULT-MSG  PIC X(50).
    05  WS-DAILY-INT-AMT   PIC S9(11)V9(6).
    05  WS-NEW-ACCRUED     PIC S9(11)V9(6).
    05  WS-PAYMENT-AMT     PIC S9(11)V99.
    05  WS-PAYMENT-DUE     PIC X(1).

*> FEECALC0 linkage
COPY CPYFEE.
01  WS-FEE-RESULT.
    05  WS-FEE-RESULT-CODE PIC X(5).
    05  WS-FEE-RESULT-MSG  PIC X(50).
    05  WS-FEE-ASSESSED    PIC 9(5)V99.
    05  WS-FEE-WAIVED-FLAG PIC X(1).
    05  WS-FEE-WAIVER-REASON PIC X(2).

*> BSACTRO linkage
01  WS-BSA-FUNCTION        PIC X(4).
01  WS-BSA-TXN-INFO.
    05  WS-BSA-CUST-ID     PIC 9(10).
    05  WS-BSA-TXN-DATE    PIC 9(8).
    05  WS-BSA-CASH-AMOUNT PIC S9(13)V99.
    05  WS-BSA-CASH-DIR    PIC X(1).
    05  WS-BSA-IS-CASH     PIC X(1).
    05  WS-BSA-ACCT-NUMBER PIC 9(12).
01  WS-BSA-RESULT.
    05  WS-BSA-RESULT-CODE    PIC X(5).
    05  WS-BSA-RESULT-MSG     PIC X(50).
    05  WS-BSA-CTR-REQUIRED   PIC X(1).
    05  WS-BSA-CASH-IN-TOTAL  PIC S9(13)V99.
    05  WS-BSA-CASH-OUT-TOTAL PIC S9(13)V99.
COPY CPYCTR.

01  WS-EXPECTED-BAL        PIC S9(13)V99.

PROCEDURE DIVISION.
MAIN-PROGRAM.
    DISPLAY "========================================".
    DISPLAY "TEST SUITE: OVERFLOW - Arithmetic Safety".
    DISPLAY "========================================".

    PERFORM TEST-OV-001
    PERFORM TEST-OV-002
    PERFORM TEST-OV-003
    PERFORM TEST-OV-004
    PERFORM TEST-OV-005
    PERFORM TEST-OV-006
    PERFORM TEST-OV-007
    PERFORM TEST-OV-008
    PERFORM TEST-OV-009

    DISPLAY "========================================".
    DISPLAY "RESULTS: " WS-PASS-COUNT "/" WS-TEST-COUNT
            " PASSED".
    DISPLAY "         " WS-FAIL-COUNT " FAILED".
    DISPLAY "========================================".
    MOVE WS-FAIL-COUNT TO RETURN-CODE
    STOP RUN.

*> ---------------------------------------------------------------
*> Helper: Set up active checking account with given balance
*> ---------------------------------------------------------------
SETUP-ACTIVE-CHECKING.
    INITIALIZE ACCT-RECORD
    MOVE 000012345678 TO ACCT-NUMBER
    MOVE 2 TO ACCT-CHECK-DIGIT
    MOVE 1000000001 TO ACCT-PRIMARY-CIF
    MOVE "DDA1" TO ACCT-PRODUCT-CODE
    MOVE "D" TO ACCT-TYPE
    MOVE "CH" TO ACCT-SUB-TYPE
    MOVE 1000.00 TO ACCT-LEDGER-BAL
    MOVE 1000.00 TO ACCT-AVAIL-BAL
    MOVE 0 TO ACCT-HOLD-AMOUNT
    MOVE "A" TO ACCT-STATUS
    MOVE "N" TO ACCT-LEGAL-HOLD
    MOVE "N" TO ACCT-DECEASED
    MOVE "N" TO ACCT-GARNISHMENT
    MOVE 20260101 TO ACCT-OPEN-DATE.

*> ---------------------------------------------------------------
*> Helper: Set up a basic deposit transaction
*> ---------------------------------------------------------------
SETUP-DEPOSIT-TXN.
    INITIALIZE TXN-RECORD
    MOVE 000000000000001 TO TXN-ID
    MOVE 00001 TO TXN-SEQUENCE
    MOVE 000012345678 TO TXN-ACCT-NUMBER
    MOVE 2 TO TXN-ACCT-CHECK-DIGIT
    MOVE 0001 TO TXN-BRANCH-ID
    MOVE "TELLER01" TO TXN-TELLER-ID
    MOVE "BR" TO TXN-CHANNEL
    MOVE "DEP" TO TXN-TYPE
    MOVE "C" TO TXN-DR-CR
    MOVE 20260226 TO TXN-POST-DATE
    MOVE 120000 TO TXN-POST-TIME
    MOVE 20260226 TO TXN-EFFECTIVE-DATE.

*> ---------------------------------------------------------------
*> Helper: Set up a basic withdrawal transaction
*> ---------------------------------------------------------------
SETUP-WITHDRAWAL-TXN.
    INITIALIZE TXN-RECORD
    MOVE 000000000000002 TO TXN-ID
    MOVE 00001 TO TXN-SEQUENCE
    MOVE 000012345678 TO TXN-ACCT-NUMBER
    MOVE 2 TO TXN-ACCT-CHECK-DIGIT
    MOVE 0001 TO TXN-BRANCH-ID
    MOVE "TELLER01" TO TXN-TELLER-ID
    MOVE "BR" TO TXN-CHANNEL
    MOVE "WDL" TO TXN-TYPE
    MOVE "D" TO TXN-DR-CR
    MOVE 20260226 TO TXN-POST-DATE
    MOVE 120000 TO TXN-POST-TIME
    MOVE 20260226 TO TXN-EFFECTIVE-DATE.

*> ---------------------------------------------------------------
*> OV-001: Deposit that overflows max balance (S9(13)V99)
*> Balance 9999999999999.00 + deposit 1.00 = overflow -> E0040
*> ---------------------------------------------------------------
TEST-OV-001.
    ADD 1 TO WS-TEST-COUNT
    MOVE "OV-001: Deposit overflow max bal -> E0040"
        TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-CHECKING
    MOVE 9999999999999.00 TO ACCT-LEDGER-BAL
    MOVE 9999999999999.00 TO ACCT-AVAIL-BAL
    PERFORM SETUP-DEPOSIT-TXN
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE 1.00 TO TXN-AMOUNT
    MOVE 1.00 TO TXN-CASH-AMOUNT
    CALL "TXNPOST0" USING TXN-RECORD ACCT-RECORD
                          WS-GL-ENTRIES WS-TXN-RESULT
    IF WS-TXN-RESULT-CODE = "E0040"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0040 actual=" WS-TXN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> OV-002: Normal deposit within limits
*> Balance $5000 + deposit $5000 = $10000 -> E0000
*> ---------------------------------------------------------------
TEST-OV-002.
    ADD 1 TO WS-TEST-COUNT
    MOVE "OV-002: Normal deposit within limits"
        TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-CHECKING
    MOVE 5000.00 TO ACCT-LEDGER-BAL
    MOVE 5000.00 TO ACCT-AVAIL-BAL
    PERFORM SETUP-DEPOSIT-TXN
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE 5000.00 TO TXN-AMOUNT
    MOVE 5000.00 TO TXN-CASH-AMOUNT
    COMPUTE WS-EXPECTED-BAL = 5000.00 + 5000.00
    CALL "TXNPOST0" USING TXN-RECORD ACCT-RECORD
                          WS-GL-ENTRIES WS-TXN-RESULT
    IF WS-TXN-RESULT-CODE = "E0000"
        IF ACCT-LEDGER-BAL = WS-EXPECTED-BAL
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " bal=" ACCT-LEDGER-BAL
                " exp=" WS-EXPECTED-BAL
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0000 actual=" WS-TXN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> OV-003: Normal withdrawal within limits
*> Balance $5000 - withdraw $3000 = $2000 -> E0000
*> ---------------------------------------------------------------
TEST-OV-003.
    ADD 1 TO WS-TEST-COUNT
    MOVE "OV-003: Normal withdrawal within limits"
        TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-CHECKING
    MOVE 5000.00 TO ACCT-LEDGER-BAL
    MOVE 5000.00 TO ACCT-AVAIL-BAL
    PERFORM SETUP-WITHDRAWAL-TXN
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE 3000.00 TO TXN-AMOUNT
    MOVE 3000.00 TO TXN-CASH-AMOUNT
    COMPUTE WS-EXPECTED-BAL = 5000.00 - 3000.00
    CALL "TXNPOST0" USING TXN-RECORD ACCT-RECORD
                          WS-GL-ENTRIES WS-TXN-RESULT
    IF WS-TXN-RESULT-CODE = "E0000"
        IF ACCT-LEDGER-BAL = WS-EXPECTED-BAL
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " bal=" ACCT-LEDGER-BAL
                " exp=" WS-EXPECTED-BAL
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0000 actual=" WS-TXN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> OV-004: GL posting overflow
*> GL-CURRENT-BAL near max + large amount -> E0040
*> ---------------------------------------------------------------
TEST-OV-004.
    ADD 1 TO WS-TEST-COUNT
    MOVE "OV-004: GL posting overflow -> E0040"
        TO WS-TEST-NAME
    INITIALIZE WS-GL-ENTRY
    INITIALIZE GL-RECORD
    INITIALIZE WS-TRIAL-BAL
    INITIALIZE WS-GL-RESULT
    MOVE "POST" TO WS-GL-FUNCTION
    MOVE 1010 TO WS-GLE-DR-ACCT
    MOVE 4010 TO WS-GLE-CR-ACCT
    MOVE 1.00 TO WS-GLE-AMOUNT
    MOVE "Overflow test" TO WS-GLE-DESCRIPTION
    MOVE 20260226 TO WS-GLE-POST-DATE
    MOVE "A" TO GL-STATUS
    MOVE "A" TO GL-ACCT-TYPE
    MOVE "D" TO GL-NORMAL-BALANCE
    MOVE 999999999999999.99 TO GL-CURRENT-BAL
    CALL "GLPOST0" USING WS-GL-FUNCTION WS-GL-ENTRY
                         GL-RECORD WS-TRIAL-BAL WS-GL-RESULT
    IF WS-GL-RESULT-CODE = "E0040"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0040 actual=" WS-GL-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> OV-005: Interest calculation overflow with huge balance
*> $9,999,999,999,999.00 @ 500% Actual/365
*> Daily interest = ~136,986,301,369.86 > S9(11)V9(6) max -> E0040
*> ---------------------------------------------------------------
TEST-OV-005.
    ADD 1 TO WS-TEST-COUNT
    MOVE "OV-005: Interest calc overflow -> E0040"
        TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-INT-RESULT
    MOVE 9999999999999.00 TO ACCT-LEDGER-BAL
    MOVE 9999999999999.00 TO ACCT-AVAIL-BAL
    MOVE 9999999999999.00 TO ACCT-COLLECTED-BAL
    MOVE 500.0000000 TO ACCT-INT-RATE
    MOVE "A" TO ACCT-INT-ACCRUAL-BASIS
    MOVE "DB" TO ACCT-INT-CALC-METHOD
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "A" TO ACCT-STATUS
    MOVE "SV" TO ACCT-SUB-TYPE
    MOVE 20260226 TO WS-CALC-DATE
    CALL "INTCALC0" USING ACCT-RECORD WS-CALC-DATE WS-INT-RESULT
    IF WS-INT-RESULT-CODE = "E0040"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0040 actual=" WS-INT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> OV-006: Fee YTD overflow is now fatal (ON SIZE ERROR -> E0040)
*> ACCT-YTD-FEES-CHARGED at max S9(9)V99 = 999999999.99
*> Fee of $12.00 causes YTD overflow -> E0040
*> ---------------------------------------------------------------
TEST-OV-006.
    ADD 1 TO WS-TEST-COUNT
    MOVE "OV-006: Fee YTD overflow -> E0040"
        TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE FEE-SCHEDULE-RECORD
    INITIALIZE WS-FEE-RESULT
    MOVE 5000.00 TO ACCT-LEDGER-BAL
    MOVE 5000.00 TO ACCT-AVAIL-BAL
    MOVE "A" TO ACCT-STATUS
    MOVE "DDA1" TO ACCT-PRODUCT-CODE
    MOVE "CH" TO ACCT-SUB-TYPE
    MOVE 12.00 TO ACCT-MONTHLY-FEE
    MOVE "NW" TO ACCT-FEE-WAIVER-CODE
    MOVE 999999999.99 TO ACCT-YTD-FEES-CHARGED
    MOVE "DDA1" TO FEE-PRODUCT-CODE
    MOVE "MTH" TO FEE-TYPE
    MOVE 12.00 TO FEE-AMOUNT
    MOVE "Y" TO FEE-WAIVER-ELIGIBLE
    MOVE 1500.00 TO FEE-MIN-BAL-THRESHOLD
    MOVE "N" TO FEE-DD-WAIVER
    MOVE "N" TO FEE-EMPLOYEE-WAIVER
    MOVE "A" TO FEE-STATUS
    CALL "FEECALC0" USING ACCT-RECORD FEE-SCHEDULE-RECORD
                          WS-FEE-RESULT
    IF WS-FEE-RESULT-CODE = "E0040"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0040 actual=" WS-FEE-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> OV-007: INTCALC0 YTD interest earned overflow
*> ACCT-YTD-INT-EARNED PIC S9(11)V9(6) max = 99999999999.999999
*> Set near max, small daily interest causes overflow -> E0040
*> ---------------------------------------------------------------
TEST-OV-007.
    ADD 1 TO WS-TEST-COUNT
    MOVE "OV-007: YTD int earned overflow -> E0040"
        TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-INT-RESULT
    MOVE 100000.00 TO ACCT-LEDGER-BAL
    MOVE 100000.00 TO ACCT-AVAIL-BAL
    MOVE 100000.00 TO ACCT-COLLECTED-BAL
    MOVE 5.0000000 TO ACCT-INT-RATE
    MOVE "A" TO ACCT-INT-ACCRUAL-BASIS
    MOVE "DB" TO ACCT-INT-CALC-METHOD
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "A" TO ACCT-STATUS
    MOVE "SV" TO ACCT-SUB-TYPE
    MOVE 99999999999.00 TO ACCT-YTD-INT-EARNED
    MOVE 20260226 TO WS-CALC-DATE
    CALL "INTCALC0" USING ACCT-RECORD WS-CALC-DATE WS-INT-RESULT
    IF WS-INT-RESULT-CODE = "E0040"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0040 actual=" WS-INT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> OV-008: INTCALC0 YTD interest paid overflow
*> ACCT-YTD-INT-PAID PIC S9(11)V99 max = 99999999999.99
*> Set near max, trigger payment via ACCT-INT-NEXT-PAY-DATE = calc
*> date, accrued interest = 500.00 causes YTD paid overflow -> E0040
*> ---------------------------------------------------------------
TEST-OV-008.
    ADD 1 TO WS-TEST-COUNT
    MOVE "OV-008: YTD int paid overflow -> E0040"
        TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE WS-INT-RESULT
    MOVE 100000.00 TO ACCT-LEDGER-BAL
    MOVE 100000.00 TO ACCT-AVAIL-BAL
    MOVE 100000.00 TO ACCT-COLLECTED-BAL
    MOVE 5.0000000 TO ACCT-INT-RATE
    MOVE "A" TO ACCT-INT-ACCRUAL-BASIS
    MOVE "DB" TO ACCT-INT-CALC-METHOD
    MOVE "F" TO ACCT-INT-RATE-TYPE
    MOVE "A" TO ACCT-STATUS
    MOVE "SV" TO ACCT-SUB-TYPE
    MOVE 500.000000 TO ACCT-ACCRUED-INT
    MOVE 99999999999.00 TO ACCT-YTD-INT-PAID
    MOVE 20260226 TO ACCT-INT-NEXT-PAY-DATE
    MOVE 20260226 TO WS-CALC-DATE
    CALL "INTCALC0" USING ACCT-RECORD WS-CALC-DATE WS-INT-RESULT
    IF WS-INT-RESULT-CODE = "E0040"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0040 actual=" WS-INT-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> OV-009: BSACTRO CTR cash-in total overflow
*> CTR-CASH-IN-TOTAL PIC S9(13)V99 max = 9999999999999.99
*> Set near max, add 5000.00 via AGGR -> E0040
*> ---------------------------------------------------------------
TEST-OV-009.
    ADD 1 TO WS-TEST-COUNT
    MOVE "OV-009: CTR cash-in overflow -> E0040"
        TO WS-TEST-NAME
    INITIALIZE CTR-RECORD
    INITIALIZE WS-BSA-TXN-INFO
    INITIALIZE WS-BSA-RESULT
    MOVE "AGGR" TO WS-BSA-FUNCTION
    MOVE 1000000001 TO CTR-CUST-ID
    MOVE 20260226 TO CTR-TXN-DATE
    MOVE 9999999999995.00 TO CTR-CASH-IN-TOTAL
    MOVE 1000000001 TO WS-BSA-CUST-ID
    MOVE 20260226 TO WS-BSA-TXN-DATE
    MOVE 5000.00 TO WS-BSA-CASH-AMOUNT
    MOVE "I" TO WS-BSA-CASH-DIR
    MOVE "Y" TO WS-BSA-IS-CASH
    MOVE 000012345678 TO WS-BSA-ACCT-NUMBER
    CALL "BSACTRO" USING WS-BSA-FUNCTION CTR-RECORD
                         WS-BSA-TXN-INFO WS-BSA-RESULT
    IF WS-BSA-RESULT-CODE = "E0040"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0040 actual=" WS-BSA-RESULT-CODE
    END-IF.

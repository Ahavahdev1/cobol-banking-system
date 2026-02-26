IDENTIFICATION DIVISION.
PROGRAM-ID. INTCALC0.
*> ================================================================
*> INTCALC0 - Daily Interest Accrual Calculator
*> Supports: Actual/365, Actual/360, 30/360, Actual/Actual
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-DAILY-INTEREST          PIC S9(11)V9(6).
01  WS-DAILY-RATE              PIC V9(18).
01  WS-DAYS-IN-YEAR            PIC 9(3).
01  WS-CALC-YEAR               PIC 9(4).
01  WS-REMAINDER-4             PIC 9(4).
01  WS-REMAINDER-100           PIC 9(4).
01  WS-REMAINDER-400           PIC 9(4).
01  WS-IS-LEAP                 PIC 9(1).
01  WS-PAYMENT-FLAG            PIC X(1).

LINKAGE SECTION.
COPY CPYACCT.
01  LS-CALC-DATE                  PIC 9(8).
01  LS-INT-RESULT.
    05  LS-INT-RESULT-CODE        PIC X(5).
    05  LS-INT-RESULT-MSG         PIC X(50).
    05  LS-DAILY-INT-AMT          PIC S9(11)V9(6).
    05  LS-NEW-ACCRUED            PIC S9(11)V9(6).
    05  LS-PAYMENT-AMT            PIC S9(11)V99.
    05  LS-PAYMENT-DUE            PIC X(1).

PROCEDURE DIVISION USING ACCT-RECORD
                         LS-CALC-DATE
                         LS-INT-RESULT.
MAIN-LOGIC.
    MOVE ZEROS TO LS-DAILY-INT-AMT
    MOVE ZEROS TO LS-NEW-ACCRUED
    MOVE ZEROS TO LS-PAYMENT-AMT
    MOVE "N" TO LS-PAYMENT-DUE
    MOVE SPACES TO LS-INT-RESULT-MSG

    *> Check closed account
    IF ACCT-STATUS = "C"
        MOVE "E0053" TO LS-INT-RESULT-CODE
        MOVE "Account is closed" TO LS-INT-RESULT-MSG
        GOBACK
    END-IF

    *> Check CD past maturity
    IF ACCT-SUB-TYPE = "CD"
        AND ACCT-MATURITY-DATE > 0
        AND LS-CALC-DATE > ACCT-MATURITY-DATE
        MOVE "E0000" TO LS-INT-RESULT-CODE
        MOVE "CD past maturity" TO LS-INT-RESULT-MSG
        MOVE ZEROS TO LS-DAILY-INT-AMT
        MOVE ACCT-ACCRUED-INT TO LS-NEW-ACCRUED
        GOBACK
    END-IF

    *> Negative or zero balance -> no interest
    IF ACCT-LEDGER-BAL <= 0
        MOVE "E0000" TO LS-INT-RESULT-CODE
        MOVE "Success" TO LS-INT-RESULT-MSG
        MOVE ZEROS TO LS-DAILY-INT-AMT
        MOVE ACCT-ACCRUED-INT TO LS-NEW-ACCRUED
        GOBACK
    END-IF

    *> Determine days in year based on accrual basis
    EVALUATE ACCT-INT-ACCRUAL-BASIS
        WHEN "A"
            MOVE 365 TO WS-DAYS-IN-YEAR
        WHEN "B"
            MOVE 360 TO WS-DAYS-IN-YEAR
        WHEN "C"
            MOVE 360 TO WS-DAYS-IN-YEAR
        WHEN "D"
            *> Actual/Actual: determine if leap year
            COMPUTE WS-CALC-YEAR =
                FUNCTION INTEGER-PART(LS-CALC-DATE / 10000)
            PERFORM CHECK-LEAP-YEAR
            IF WS-IS-LEAP = 1
                MOVE 366 TO WS-DAYS-IN-YEAR
            ELSE
                MOVE 365 TO WS-DAYS-IN-YEAR
            END-IF
        WHEN OTHER
            MOVE "E0050" TO LS-INT-RESULT-CODE
            MOVE "Invalid accrual basis" TO LS-INT-RESULT-MSG
            GOBACK
    END-EVALUATE

    *> Compute daily interest
    *> Step 1: daily rate = rate / 100 / days_in_year
    COMPUTE WS-DAILY-RATE =
        ACCT-INT-RATE / 100 / WS-DAYS-IN-YEAR
        ON SIZE ERROR
            MOVE "E0040" TO LS-INT-RESULT-CODE
            MOVE "Arithmetic overflow on daily rate"
                TO LS-INT-RESULT-MSG
            GOBACK
    END-COMPUTE
    *> Step 2: daily interest = balance * daily_rate
    COMPUTE WS-DAILY-INTEREST =
        ACCT-LEDGER-BAL * WS-DAILY-RATE
        ON SIZE ERROR
            MOVE "E0040" TO LS-INT-RESULT-CODE
            MOVE "Arithmetic overflow on daily interest"
                TO LS-INT-RESULT-MSG
            GOBACK
    END-COMPUTE

    MOVE WS-DAILY-INTEREST TO LS-DAILY-INT-AMT

    *> Accumulate daily interest into YTD earned
    ADD WS-DAILY-INTEREST TO ACCT-YTD-INT-EARNED
        ON SIZE ERROR
            MOVE "E0040" TO LS-INT-RESULT-CODE
            MOVE "Arithmetic overflow on YTD interest earned"
                TO LS-INT-RESULT-MSG
            GOBACK
    END-ADD

    *> Check if payment is due
    MOVE "N" TO WS-PAYMENT-FLAG
    IF ACCT-INT-NEXT-PAY-DATE > 0
        AND ACCT-INT-NEXT-PAY-DATE <= LS-CALC-DATE
        MOVE "Y" TO WS-PAYMENT-FLAG
    END-IF

    IF WS-PAYMENT-FLAG = "Y"
        *> Payment due: payment = round(current accrued + today) to 2dp
        MOVE "Y" TO LS-PAYMENT-DUE
        COMPUTE LS-PAYMENT-AMT ROUNDED =
            ACCT-ACCRUED-INT + WS-DAILY-INTEREST
            ON SIZE ERROR
                MOVE "E0040" TO LS-INT-RESULT-CODE
                MOVE "Arithmetic overflow on payment amount"
                    TO LS-INT-RESULT-MSG
                GOBACK
        END-COMPUTE
        *> Accumulate payment into YTD interest paid
        ADD LS-PAYMENT-AMT TO ACCT-YTD-INT-PAID
            ON SIZE ERROR
                MOVE "E0040" TO LS-INT-RESULT-CODE
                MOVE "Arithmetic overflow on YTD interest paid"
                    TO LS-INT-RESULT-MSG
                GOBACK
        END-ADD
        *> After payment reset: accrued = 0 (today's interest paid out)
        MOVE ZEROS TO LS-NEW-ACCRUED
    ELSE
        *> Normal accrual: new accrued = existing + today
        COMPUTE LS-NEW-ACCRUED =
            ACCT-ACCRUED-INT + WS-DAILY-INTEREST
            ON SIZE ERROR
                MOVE "E0040" TO LS-INT-RESULT-CODE
                MOVE "Arithmetic overflow on accrual"
                    TO LS-INT-RESULT-MSG
                GOBACK
        END-COMPUTE
    END-IF

    *> Write back accrued interest to account record
    IF WS-PAYMENT-FLAG = "Y"
        MOVE 0 TO ACCT-ACCRUED-INT
    ELSE
        MOVE LS-NEW-ACCRUED TO ACCT-ACCRUED-INT
    END-IF

    MOVE "E0000" TO LS-INT-RESULT-CODE
    MOVE "Success" TO LS-INT-RESULT-MSG
    GOBACK.

CHECK-LEAP-YEAR.
    MOVE 0 TO WS-IS-LEAP
    COMPUTE WS-REMAINDER-4 =
        FUNCTION MOD(WS-CALC-YEAR, 4)
    IF WS-REMAINDER-4 = 0
        COMPUTE WS-REMAINDER-100 =
            FUNCTION MOD(WS-CALC-YEAR, 100)
        IF WS-REMAINDER-100 NOT = 0
            MOVE 1 TO WS-IS-LEAP
        ELSE
            COMPUTE WS-REMAINDER-400 =
                FUNCTION MOD(WS-CALC-YEAR, 400)
            IF WS-REMAINDER-400 = 0
                MOVE 1 TO WS-IS-LEAP
            END-IF
        END-IF
    END-IF.

IDENTIFICATION DIVISION.
PROGRAM-ID. APYCALC0.
*> ================================================================
*> APYCALC0 - APY/APR Calculator
*> Regulation DD (Truth in Savings) - 12 CFR 1030
*>
*> Calculates:
*>   - APY (Annual Percentage Yield) for deposit accounts
*>   - APR (Annual Percentage Rate) for loan accounts
*>   - Effective annual rate via iterative compounding
*>
*> Compounding formula (no native COBOL power function):
*>   APY = ((1 + rate/periods)^periods - 1) * 100
*>   Implemented via iterative multiplication loop
*>
*> Supports: Daily, Monthly, Quarterly, Semi-Annual, Annual
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-PERIOD-RATE            PIC V9(18).
01  WS-COMPOUND-FACTOR        PIC 9(5)V9(15).
01  WS-PERIODS                PIC 9(3).
01  WS-LOOP-IDX               PIC 9(3).
01  WS-COMPUTED-APY            PIC 9(3)V9(7).

LINKAGE SECTION.
COPY CPYACCT.
01  LS-APY-RESULT.
    05  LS-APY-RESULT-CODE     PIC X(5).
    05  LS-APY-RESULT-MSG      PIC X(50).
    05  LS-APY-VALUE           PIC 9(3)V9(7).
    05  LS-APR-VALUE           PIC 9(3)V9(7).
    05  LS-EFFECTIVE-RATE      PIC 9(3)V9(7).

PROCEDURE DIVISION USING ACCT-RECORD
                         LS-APY-RESULT.
MAIN-LOGIC.
    MOVE ZEROS TO LS-APY-VALUE
    MOVE ZEROS TO LS-APR-VALUE
    MOVE ZEROS TO LS-EFFECTIVE-RATE
    MOVE SPACES TO LS-APY-RESULT-MSG

    *> Reject negative interest rates
    IF ACCT-INT-RATE < 0
        MOVE "E0102" TO LS-APY-RESULT-CODE
        MOVE "Negative interest rate not supported"
            TO LS-APY-RESULT-MSG
        GOBACK
    END-IF

    *> Zero rate: APY = 0, APR = 0, success
    IF ACCT-INT-RATE = 0
        MOVE "E0000" TO LS-APY-RESULT-CODE
        MOVE "Success - zero rate" TO LS-APY-RESULT-MSG
        GOBACK
    END-IF

    *> Determine compounding periods from compounding frequency
    EVALUATE ACCT-INT-COMPOUND-FREQ
        WHEN "D"
            MOVE 365 TO WS-PERIODS
        WHEN "M"
            MOVE 12 TO WS-PERIODS
        WHEN "Q"
            MOVE 4 TO WS-PERIODS
        WHEN "S"
            MOVE 2 TO WS-PERIODS
        WHEN "A"
            MOVE 1 TO WS-PERIODS
        WHEN "T"
            MOVE 1 TO WS-PERIODS
        WHEN OTHER
            MOVE "E0088" TO LS-APY-RESULT-CODE
            MOVE "Invalid compounding frequency"
                TO LS-APY-RESULT-MSG
            GOBACK
    END-EVALUATE

    *> For annual compounding, APY = nominal rate
    IF WS-PERIODS = 1
        MOVE ACCT-INT-RATE TO LS-APY-VALUE
        MOVE ACCT-INT-RATE TO LS-EFFECTIVE-RATE
        IF ACCT-TYPE = "L"
            MOVE ACCT-INT-RATE TO LS-APR-VALUE
        END-IF
        MOVE "E0000" TO LS-APY-RESULT-CODE
        MOVE "Success" TO LS-APY-RESULT-MSG
        GOBACK
    END-IF

    *> Compute period rate = nominal_rate / 100 / periods
    COMPUTE WS-PERIOD-RATE =
        ACCT-INT-RATE / 100 / WS-PERIODS
        ON SIZE ERROR
            MOVE "E0040" TO LS-APY-RESULT-CODE
            MOVE "Arithmetic overflow on period rate"
                TO LS-APY-RESULT-MSG
            GOBACK
    END-COMPUTE

    *> Iterative compounding: (1 + period_rate) ^ periods
    MOVE 1 TO WS-COMPOUND-FACTOR
    PERFORM WS-PERIODS TIMES
        COMPUTE WS-COMPOUND-FACTOR =
            WS-COMPOUND-FACTOR * (1 + WS-PERIOD-RATE)
            ON SIZE ERROR
                MOVE "E0040" TO LS-APY-RESULT-CODE
                MOVE "Arithmetic overflow on compounding"
                    TO LS-APY-RESULT-MSG
                GOBACK
        END-COMPUTE
    END-PERFORM

    *> APY = (compound_factor - 1) * 100
    COMPUTE WS-COMPUTED-APY =
        (WS-COMPOUND-FACTOR - 1) * 100
        ON SIZE ERROR
            MOVE "E0040" TO LS-APY-RESULT-CODE
            MOVE "Arithmetic overflow on APY"
                TO LS-APY-RESULT-MSG
            GOBACK
    END-COMPUTE

    MOVE WS-COMPUTED-APY TO LS-APY-VALUE
    MOVE WS-COMPUTED-APY TO LS-EFFECTIVE-RATE

    *> For deposit accounts, APY is the primary disclosure
    *> For loan accounts, APR = nominal rate, effective = APY
    *> NOTE: TILA Reg Z APR requires fee-inclusive calculation.
    *> This module returns nominal rate only. A separate TILA
    *> disclosure module is needed for fee-inclusive APR.
    IF ACCT-TYPE = "L"
        MOVE ACCT-INT-RATE TO LS-APR-VALUE
    END-IF

    MOVE "E0000" TO LS-APY-RESULT-CODE
    MOVE "Success" TO LS-APY-RESULT-MSG
    GOBACK.

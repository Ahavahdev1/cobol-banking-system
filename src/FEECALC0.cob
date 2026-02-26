IDENTIFICATION DIVISION.
PROGRAM-ID. FEECALC0.
*> ================================================================
*> FEECALC0 - Monthly Fee Assessment Engine
*> Assesses fees, checks waiver conditions, tracks YTD
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-ABS-BALANCE             PIC S9(13)V99.
01  WS-FEE-WAIVED              PIC X(1).

LINKAGE SECTION.
COPY CPYACCT.
COPY CPYFEE.
01  LS-FEE-RESULT.
    05  LS-FEE-RESULT-CODE        PIC X(5).
    05  LS-FEE-RESULT-MSG         PIC X(50).
    05  LS-FEE-ASSESSED           PIC 9(5)V99.
    05  LS-FEE-WAIVED-FLAG        PIC X(1).
    05  LS-FEE-WAIVER-REASON      PIC X(2).

PROCEDURE DIVISION USING ACCT-RECORD
                         FEE-SCHEDULE-RECORD
                         LS-FEE-RESULT.
MAIN-LOGIC.
    MOVE "E0000" TO LS-FEE-RESULT-CODE
    MOVE SPACES TO LS-FEE-RESULT-MSG
    MOVE ZEROS TO LS-FEE-ASSESSED
    MOVE "N" TO LS-FEE-WAIVED-FLAG
    MOVE SPACES TO LS-FEE-WAIVER-REASON

    IF FEE-STATUS NOT = "A"
        MOVE "Fee schedule inactive" TO LS-FEE-RESULT-MSG
        GOBACK
    END-IF

    EVALUATE FEE-TYPE
        WHEN "MTH"
            PERFORM PROCESS-MONTHLY-FEE THRU
                    PROCESS-MONTHLY-FEE-EXIT
        WHEN "NSF"
            PERFORM PROCESS-NSF-FEE THRU
                    PROCESS-NSF-FEE-EXIT
        WHEN OTHER
            MOVE "E0001" TO LS-FEE-RESULT-CODE
            MOVE "Unsupported fee type" TO LS-FEE-RESULT-MSG
            GOBACK
    END-EVALUATE

    GOBACK.

PROCESS-MONTHLY-FEE.
    IF FEE-AMOUNT = 0
        MOVE "No fee - zero amount" TO LS-FEE-RESULT-MSG
        GO TO PROCESS-MONTHLY-FEE-EXIT
    END-IF

    MOVE "N" TO WS-FEE-WAIVED

    *> Check waivers in priority order
    *> 1. Employee waiver
    IF ACCT-FEE-WAIVER-CODE = "EM"
        AND FEE-EMPLOYEE-WAIVER = "Y"
        MOVE "Y" TO WS-FEE-WAIVED
        MOVE "Y" TO LS-FEE-WAIVED-FLAG
        MOVE "EM" TO LS-FEE-WAIVER-REASON
        MOVE 0 TO LS-FEE-ASSESSED
        ADD FEE-AMOUNT TO ACCT-YTD-FEES-WAIVED
            ON SIZE ERROR
                MOVE "E0040" TO LS-FEE-RESULT-CODE
                MOVE "YTD fee accumulator overflow"
                    TO LS-FEE-RESULT-MSG
                GOBACK
        END-ADD
        MOVE "Fee waived - employee" TO LS-FEE-RESULT-MSG
        GO TO PROCESS-MONTHLY-FEE-EXIT
    END-IF

    *> 2. Direct deposit waiver
    IF ACCT-FEE-WAIVER-CODE = "DD"
        AND FEE-DD-WAIVER = "Y"
        MOVE "Y" TO WS-FEE-WAIVED
        MOVE "Y" TO LS-FEE-WAIVED-FLAG
        MOVE "DD" TO LS-FEE-WAIVER-REASON
        MOVE 0 TO LS-FEE-ASSESSED
        ADD FEE-AMOUNT TO ACCT-YTD-FEES-WAIVED
            ON SIZE ERROR
                MOVE "E0040" TO LS-FEE-RESULT-CODE
                MOVE "YTD fee accumulator overflow"
                    TO LS-FEE-RESULT-MSG
                GOBACK
        END-ADD
        MOVE "Fee waived - direct deposit" TO LS-FEE-RESULT-MSG
        GO TO PROCESS-MONTHLY-FEE-EXIT
    END-IF

    *> 3. Minimum balance waiver
    IF ACCT-FEE-WAIVER-CODE = "MB"
        AND ACCT-LEDGER-BAL >= FEE-MIN-BAL-THRESHOLD
        MOVE "Y" TO WS-FEE-WAIVED
        MOVE "Y" TO LS-FEE-WAIVED-FLAG
        MOVE "MB" TO LS-FEE-WAIVER-REASON
        MOVE 0 TO LS-FEE-ASSESSED
        ADD FEE-AMOUNT TO ACCT-YTD-FEES-WAIVED
            ON SIZE ERROR
                MOVE "E0040" TO LS-FEE-RESULT-CODE
                MOVE "YTD fee accumulator overflow"
                    TO LS-FEE-RESULT-MSG
                GOBACK
        END-ADD
        MOVE "Fee waived - minimum balance" TO LS-FEE-RESULT-MSG
        GO TO PROCESS-MONTHLY-FEE-EXIT
    END-IF

    *> No waiver applies - charge the fee
    MOVE FEE-AMOUNT TO LS-FEE-ASSESSED
    MOVE "N" TO LS-FEE-WAIVED-FLAG
    ADD FEE-AMOUNT TO ACCT-YTD-FEES-CHARGED
        ON SIZE ERROR
            MOVE "E0040" TO LS-FEE-RESULT-CODE
            MOVE "YTD fee accumulator overflow"
                TO LS-FEE-RESULT-MSG
            GOBACK
    END-ADD
    MOVE "Monthly fee assessed" TO LS-FEE-RESULT-MSG.

PROCESS-MONTHLY-FEE-EXIT.
    EXIT.

PROCESS-NSF-FEE.
    *> De minimis check: if ABS(balance) < de minimis, no fee
    IF ACCT-LEDGER-BAL < 0
        MULTIPLY ACCT-LEDGER-BAL BY -1
            GIVING WS-ABS-BALANCE
    ELSE
        MOVE ACCT-LEDGER-BAL TO WS-ABS-BALANCE
    END-IF

    IF WS-ABS-BALANCE < FEE-NSF-DE-MINIMIS
        MOVE "No fee - de minimis" TO LS-FEE-RESULT-MSG
        GO TO PROCESS-NSF-FEE-EXIT
    END-IF

    *> Daily cap check
    IF ACCT-NSF-COUNT-TODAY >= FEE-NSF-DAILY-MAX
        MOVE "No fee - daily cap reached" TO LS-FEE-RESULT-MSG
        GO TO PROCESS-NSF-FEE-EXIT
    END-IF

    *> Charge NSF fee
    MOVE FEE-AMOUNT TO LS-FEE-ASSESSED
    MOVE "NSF fee assessed" TO LS-FEE-RESULT-MSG.

PROCESS-NSF-FEE-EXIT.
    EXIT.

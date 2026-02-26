IDENTIFICATION DIVISION.
PROGRAM-ID. GLPOST0.
*> ================================================================
*> GLPOST0 - General Ledger Posting and Trial Balance
*> POST = Post entry, TBAL = Trial balance, INIT = Initialize
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.

LINKAGE SECTION.
01  LS-FUNCTION                   PIC X(4).
01  LS-GL-ENTRY.
    05  LS-GLE-DR-ACCT            PIC 9(10).
    05  LS-GLE-CR-ACCT            PIC 9(10).
    05  LS-GLE-AMOUNT             PIC S9(15)V99.
    05  LS-GLE-DESCRIPTION        PIC X(40).
    05  LS-GLE-POST-DATE          PIC 9(8).
COPY CPYGL.
01  LS-TRIAL-BAL.
    05  LS-TB-TOTAL-DEBITS        PIC S9(15)V99.
    05  LS-TB-TOTAL-CREDITS       PIC S9(15)V99.
    05  LS-TB-DIFFERENCE          PIC S9(15)V99.
    05  LS-TB-IS-BALANCED         PIC X(1).
01  LS-GL-RESULT.
    05  LS-GL-RESULT-CODE         PIC X(5).
    05  LS-GL-RESULT-MSG          PIC X(50).

PROCEDURE DIVISION USING LS-FUNCTION
                         LS-GL-ENTRY
                         GL-RECORD
                         LS-TRIAL-BAL
                         LS-GL-RESULT.

    EVALUATE LS-FUNCTION
        WHEN "POST"
            PERFORM POST-GL-ENTRY
        WHEN "TBAL"
            PERFORM TRIAL-BALANCE
        WHEN "INIT"
            PERFORM INIT-GL-RECORD
        WHEN OTHER
            MOVE "E0001" TO LS-GL-RESULT-CODE
            MOVE "Invalid function" TO LS-GL-RESULT-MSG
    END-EVALUATE
    GOBACK.

*> ----------------------------------------------------------------
*> POST-GL-ENTRY - Post a double-entry GL posting
*> ----------------------------------------------------------------
POST-GL-ENTRY.
    *> Check account status
    IF GL-STATUS = "I"
        MOVE "E0061" TO LS-GL-RESULT-CODE
        MOVE "Account inactive" TO LS-GL-RESULT-MSG
        EXIT PARAGRAPH
    END-IF
    IF GL-STATUS = "F"
        MOVE "E0062" TO LS-GL-RESULT-CODE
        MOVE "Account frozen" TO LS-GL-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    *> Check for zero amount
    IF LS-GLE-AMOUNT = ZERO
        MOVE "E0063" TO LS-GL-RESULT-CODE
        MOVE "Zero posting amount" TO LS-GL-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    *> Update GL-RECORD based on normal balance direction
    EVALUATE GL-NORMAL-BALANCE
        WHEN "D"
            *> Debit-normal account (Asset, Expense)
            ADD LS-GLE-AMOUNT TO GL-CURRENT-BAL
                ON SIZE ERROR
                    MOVE "E0040" TO LS-GL-RESULT-CODE
                    MOVE "Arithmetic overflow on GL balance"
                        TO LS-GL-RESULT-MSG
                    EXIT PARAGRAPH
            END-ADD
            ADD LS-GLE-AMOUNT TO GL-MTD-DEBITS
                ON SIZE ERROR CONTINUE
            END-ADD
            ADD LS-GLE-AMOUNT TO GL-YTD-DEBITS
                ON SIZE ERROR CONTINUE
            END-ADD
        WHEN "C"
            *> Credit-normal account (Liability, Income, Equity)
            IF LS-GLE-DR-ACCT > LS-GLE-CR-ACCT
                *> Contra entry: credit-normal on DR side
                SUBTRACT LS-GLE-AMOUNT FROM GL-CURRENT-BAL
                    ON SIZE ERROR
                        MOVE "E0040" TO LS-GL-RESULT-CODE
                        MOVE "Arithmetic overflow on GL balance"
                            TO LS-GL-RESULT-MSG
                        EXIT PARAGRAPH
                END-SUBTRACT
                ADD LS-GLE-AMOUNT TO GL-MTD-DEBITS
                    ON SIZE ERROR CONTINUE
                END-ADD
                ADD LS-GLE-AMOUNT TO GL-YTD-DEBITS
                    ON SIZE ERROR CONTINUE
                END-ADD
            ELSE
                *> Normal entry: credit-normal on CR side
                ADD LS-GLE-AMOUNT TO GL-CURRENT-BAL
                    ON SIZE ERROR
                        MOVE "E0040" TO LS-GL-RESULT-CODE
                        MOVE "Arithmetic overflow on GL balance"
                            TO LS-GL-RESULT-MSG
                        EXIT PARAGRAPH
                END-ADD
                ADD LS-GLE-AMOUNT TO GL-MTD-CREDITS
                    ON SIZE ERROR CONTINUE
                END-ADD
                ADD LS-GLE-AMOUNT TO GL-YTD-CREDITS
                    ON SIZE ERROR CONTINUE
                END-ADD
            END-IF
        WHEN OTHER
            *> Journal mode: track double-entry totals
            ADD LS-GLE-AMOUNT TO GL-MTD-DEBITS
                ON SIZE ERROR CONTINUE
            END-ADD
            ADD LS-GLE-AMOUNT TO GL-MTD-CREDITS
                ON SIZE ERROR CONTINUE
            END-ADD
            ADD LS-GLE-AMOUNT TO GL-YTD-DEBITS
                ON SIZE ERROR CONTINUE
            END-ADD
            ADD LS-GLE-AMOUNT TO GL-YTD-CREDITS
                ON SIZE ERROR CONTINUE
            END-ADD
    END-EVALUATE

    *> Update last posting date
    MOVE LS-GLE-POST-DATE TO GL-LAST-POST-DATE

    MOVE "E0000" TO LS-GL-RESULT-CODE
    MOVE SPACES TO LS-GL-RESULT-MSG
    EXIT PARAGRAPH.

*> ----------------------------------------------------------------
*> TRIAL-BALANCE - Compute trial balance from debits and credits
*> ----------------------------------------------------------------
TRIAL-BALANCE.
    *> If caller hasn't set totals, read from GL-RECORD
    IF LS-TB-TOTAL-DEBITS = ZERO
        AND LS-TB-TOTAL-CREDITS = ZERO
        MOVE GL-MTD-DEBITS TO LS-TB-TOTAL-DEBITS
        MOVE GL-MTD-CREDITS TO LS-TB-TOTAL-CREDITS
    END-IF
    COMPUTE LS-TB-DIFFERENCE =
        LS-TB-TOTAL-DEBITS - LS-TB-TOTAL-CREDITS
        ON SIZE ERROR
            MOVE "E0040" TO LS-GL-RESULT-CODE
            MOVE "Arithmetic overflow on trial balance"
                TO LS-GL-RESULT-MSG
            EXIT PARAGRAPH
    END-COMPUTE
    IF LS-TB-DIFFERENCE = ZERO
        MOVE "Y" TO LS-TB-IS-BALANCED
    ELSE
        MOVE "N" TO LS-TB-IS-BALANCED
    END-IF

    MOVE "E0000" TO LS-GL-RESULT-CODE
    MOVE SPACES TO LS-GL-RESULT-MSG
    EXIT PARAGRAPH.

*> ----------------------------------------------------------------
*> INIT-GL-RECORD - Initialize a GL record with defaults
*> ----------------------------------------------------------------
INIT-GL-RECORD.
    INITIALIZE GL-RECORD
    MOVE "A" TO GL-STATUS

    MOVE "E0000" TO LS-GL-RESULT-CODE
    MOVE SPACES TO LS-GL-RESULT-MSG
    EXIT PARAGRAPH.

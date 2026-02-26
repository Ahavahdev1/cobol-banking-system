IDENTIFICATION DIVISION.
PROGRAM-ID. TXNPOST0.
*> ================================================================
*> TXNPOST0 - Transaction Posting Engine
*> Validates, authorizes, posts transactions, updates GL
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-DEPOSIT-GL            PIC 9(10).

LINKAGE SECTION.
COPY CPYTXN.
COPY CPYACCT.
01  LS-GL-ENTRIES.
    05  LS-GL-DR-ACCOUNT          PIC 9(10).
    05  LS-GL-CR-ACCOUNT          PIC 9(10).
    05  LS-GL-AMOUNT              PIC S9(13)V99.
    05  LS-GL-POST-FLAG           PIC X(1).
01  LS-TXN-RESULT.
    05  LS-TXN-RESULT-CODE        PIC X(5).
    05  LS-TXN-RESULT-MSG         PIC X(50).

PROCEDURE DIVISION USING TXN-RECORD
                         ACCT-RECORD
                         LS-GL-ENTRIES
                         LS-TXN-RESULT.
MAIN-LOGIC.
    PERFORM VALIDATE-AMOUNT
    PERFORM VALIDATE-ACCOUNT-STATUS
    PERFORM CHECK-BALANCE
    PERFORM CAPTURE-BEFORE-SNAPSHOT
    PERFORM POST-TRANSACTION
    PERFORM UPDATE-AVAILABLE-BALANCE
    PERFORM CAPTURE-AFTER-SNAPSHOT
    PERFORM MAP-GL-ENTRIES
    PERFORM CHECK-CTR
    MOVE TXN-POST-DATE TO ACCT-LAST-TXN-DATE
    MOVE "E0000" TO LS-TXN-RESULT-CODE
    MOVE "Transaction posted successfully" TO LS-TXN-RESULT-MSG
    GOBACK.

*> ---------------------------------------------------------------
*> Validate transaction amount
*> ---------------------------------------------------------------
VALIDATE-AMOUNT.
    IF TXN-AMOUNT = ZERO
        MOVE "E0031" TO LS-TXN-RESULT-CODE
        MOVE "Transaction amount is zero" TO LS-TXN-RESULT-MSG
        GOBACK
    END-IF
    IF TXN-AMOUNT < ZERO
        MOVE "E0032" TO LS-TXN-RESULT-CODE
        MOVE "Transaction amount is negative" TO LS-TXN-RESULT-MSG
        GOBACK
    END-IF.

*> ---------------------------------------------------------------
*> Validate account status flags
*> ---------------------------------------------------------------
VALIDATE-ACCOUNT-STATUS.
    IF ACCT-STATUS = "F"
        MOVE "E0033" TO LS-TXN-RESULT-CODE
        MOVE "Account is frozen" TO LS-TXN-RESULT-MSG
        GOBACK
    END-IF
    IF ACCT-STATUS = "C"
        MOVE "E0034" TO LS-TXN-RESULT-CODE
        MOVE "Account is closed" TO LS-TXN-RESULT-MSG
        GOBACK
    END-IF
    IF ACCT-LEGAL-HOLD = "Y" AND TXN-DR-CR = "D"
        MOVE "E0035" TO LS-TXN-RESULT-CODE
        MOVE "Account has legal hold" TO LS-TXN-RESULT-MSG
        GOBACK
    END-IF
    IF ACCT-DECEASED = "Y"
        MOVE "E0036" TO LS-TXN-RESULT-CODE
        MOVE "Account holder is deceased" TO LS-TXN-RESULT-MSG
        GOBACK
    END-IF.

*> ---------------------------------------------------------------
*> Check sufficient funds for withdrawals (debits)
*> ---------------------------------------------------------------
CHECK-BALANCE.
    IF TXN-DR-CR = "D"
        IF TXN-AMOUNT > ACCT-AVAIL-BAL
            MOVE "E0030" TO LS-TXN-RESULT-CODE
            MOVE "Insufficient funds" TO LS-TXN-RESULT-MSG
            GOBACK
        END-IF
    END-IF.

*> ---------------------------------------------------------------
*> Capture balance snapshots before posting
*> ---------------------------------------------------------------
CAPTURE-BEFORE-SNAPSHOT.
    MOVE ACCT-LEDGER-BAL TO TXN-BAL-BEFORE
    MOVE ACCT-AVAIL-BAL TO TXN-AVAIL-BEFORE.

*> ---------------------------------------------------------------
*> Post the transaction to ledger balance
*> ---------------------------------------------------------------
POST-TRANSACTION.
    IF TXN-DR-CR = "C"
        ADD TXN-AMOUNT TO ACCT-LEDGER-BAL
            ON SIZE ERROR
                MOVE "E0040" TO LS-TXN-RESULT-CODE
                MOVE "Arithmetic overflow on deposit"
                    TO LS-TXN-RESULT-MSG
                GOBACK
        END-ADD
    ELSE
        SUBTRACT TXN-AMOUNT FROM ACCT-LEDGER-BAL
            ON SIZE ERROR
                MOVE "E0040" TO LS-TXN-RESULT-CODE
                MOVE "Arithmetic overflow on withdrawal"
                    TO LS-TXN-RESULT-MSG
                GOBACK
        END-SUBTRACT
    END-IF.

*> ---------------------------------------------------------------
*> Update available balance = ledger - holds
*> ---------------------------------------------------------------
UPDATE-AVAILABLE-BALANCE.
    COMPUTE ACCT-AVAIL-BAL =
        ACCT-LEDGER-BAL - ACCT-HOLD-AMOUNT
        ON SIZE ERROR
            MOVE "E0040" TO LS-TXN-RESULT-CODE
            MOVE "Arithmetic overflow on available balance"
                TO LS-TXN-RESULT-MSG
            GOBACK
    END-COMPUTE.

*> ---------------------------------------------------------------
*> Capture balance snapshots after posting
*> ---------------------------------------------------------------
CAPTURE-AFTER-SNAPSHOT.
    MOVE ACCT-LEDGER-BAL TO TXN-BAL-AFTER
    MOVE ACCT-AVAIL-BAL TO TXN-AVAIL-AFTER.

*> ---------------------------------------------------------------
*> Map GL entries based on transaction type and account sub-type
*> ---------------------------------------------------------------
MAP-GL-ENTRIES.
    PERFORM DETERMINE-DEPOSIT-GL
    EVALUATE TXN-TYPE
        WHEN "DEP"
            MOVE 0000001010 TO LS-GL-DR-ACCOUNT
            MOVE WS-DEPOSIT-GL TO LS-GL-CR-ACCOUNT
        WHEN "WDL"
            MOVE WS-DEPOSIT-GL TO LS-GL-DR-ACCOUNT
            MOVE 0000001010 TO LS-GL-CR-ACCOUNT
        WHEN "FEE"
            MOVE WS-DEPOSIT-GL TO LS-GL-DR-ACCOUNT
            MOVE 0000007500 TO LS-GL-CR-ACCOUNT
        WHEN "INT"
            MOVE 0000008010 TO LS-GL-DR-ACCOUNT
            MOVE WS-DEPOSIT-GL TO LS-GL-CR-ACCOUNT
    END-EVALUATE
    MOVE TXN-AMOUNT TO LS-GL-AMOUNT
    MOVE "Y" TO LS-GL-POST-FLAG.

*> ---------------------------------------------------------------
*> Determine the deposit GL account based on account sub-type
*> ---------------------------------------------------------------
DETERMINE-DEPOSIT-GL.
    EVALUATE ACCT-SUB-TYPE
        WHEN "CH"
            MOVE 0000004010 TO WS-DEPOSIT-GL
        WHEN "SV"
            MOVE 0000004030 TO WS-DEPOSIT-GL
        WHEN "MM"
            MOVE 0000004040 TO WS-DEPOSIT-GL
    END-EVALUATE.

*> ---------------------------------------------------------------
*> Check if cash amount triggers CTR reporting
*> ---------------------------------------------------------------
CHECK-CTR.
    IF TXN-CASH-AMOUNT >= 10000.00
        MOVE "Y" TO TXN-CTR-REPORTABLE
    ELSE
        MOVE "N" TO TXN-CTR-REPORTABLE
    END-IF.

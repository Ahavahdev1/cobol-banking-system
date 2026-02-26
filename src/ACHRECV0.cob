IDENTIFICATION DIVISION.
PROGRAM-ID. ACHRECV0.
*> ================================================================
*> ACHRECV0 - ACH Incoming File Processor
*> Processes credits/debits, handles returns (R01-R08)
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-IS-CREDIT           PIC X(1).
01  WS-IS-DEBIT            PIC X(1).

LINKAGE SECTION.
01  LS-ACH-ENTRY.
    05  LS-ACH-RECORD-TYPE       PIC X(1).
    05  LS-ACH-TXN-CODE          PIC 9(2).
    *> 22 = Credit, 27 = Debit,
    *> 32 = Credit savings, 37 = Debit savings
    05  LS-ACH-ROUTING-NUM       PIC 9(9).
    05  LS-ACH-ACCT-NUMBER       PIC X(17).
    05  LS-ACH-AMOUNT            PIC S9(10)V99.
    05  LS-ACH-INDIV-NAME        PIC X(22).
    05  LS-ACH-TRACE-NUMBER      PIC X(15).
    05  LS-ACH-ADDENDA-FLAG      PIC X(1).
    *> Batch control fields
    05  LS-ACH-BATCH-COUNT       PIC 9(6).
    05  LS-ACH-BATCH-DR-TOTAL    PIC S9(12)V99.
    05  LS-ACH-BATCH-CR-TOTAL    PIC S9(12)V99.
    05  LS-ACH-BATCH-HASH        PIC 9(10).
COPY CPYACCT.
01  LS-ACH-RETURN-INFO.
    05  LS-ACH-RETURN-CODE       PIC X(3).
    *> R01 = NSF, R02 = Closed, R03 = No Account,
    *> R08 = Stop Payment
    05  LS-ACH-RETURN-REASON     PIC X(30).
    05  LS-ACH-RETURN-FLAG       PIC X(1).
01  LS-ACH-RESULT.
    05  LS-ACH-RESULT-CODE        PIC X(5).
    05  LS-ACH-RESULT-MSG         PIC X(50).

PROCEDURE DIVISION USING LS-ACH-ENTRY
                         ACCT-RECORD
                         LS-ACH-RETURN-INFO
                         LS-ACH-RESULT.
MAIN-PROCESS.
    *> Initialize outputs
    MOVE SPACES TO LS-ACH-RETURN-CODE
    MOVE SPACES TO LS-ACH-RETURN-REASON
    MOVE "N" TO LS-ACH-RETURN-FLAG
    MOVE "E0000" TO LS-ACH-RESULT-CODE
    MOVE SPACES TO LS-ACH-RESULT-MSG

    *> Determine transaction type
    MOVE "N" TO WS-IS-CREDIT
    MOVE "N" TO WS-IS-DEBIT
    IF LS-ACH-TXN-CODE = 22 OR LS-ACH-TXN-CODE = 32
        MOVE "Y" TO WS-IS-CREDIT
    END-IF
    IF LS-ACH-TXN-CODE = 27 OR LS-ACH-TXN-CODE = 37
        MOVE "Y" TO WS-IS-DEBIT
    END-IF

    *> Step 1: Batch control validation
    PERFORM CHECK-BATCH-TOTALS
    IF LS-ACH-RESULT-CODE NOT = "E0000"
        GOBACK
    END-IF

    *> Step 2: Account validation
    PERFORM CHECK-ACCOUNT
    IF LS-ACH-RETURN-FLAG = "Y"
        GOBACK
    END-IF

    *> Step 3: Funds check for debits
    IF WS-IS-DEBIT = "Y"
        PERFORM CHECK-FUNDS
        IF LS-ACH-RETURN-FLAG = "Y"
            GOBACK
        END-IF
    END-IF

    *> Step 4: Process the transaction
    PERFORM PROCESS-TRANSACTION
    GOBACK.

*> ---------------------------------------------------------------
*> CHECK-BATCH-TOTALS - Validate batch control totals
*> ---------------------------------------------------------------
CHECK-BATCH-TOTALS.
    IF LS-ACH-BATCH-COUNT = 1
        IF WS-IS-CREDIT = "Y"
            IF LS-ACH-AMOUNT NOT = LS-ACH-BATCH-CR-TOTAL
                MOVE "E0094" TO LS-ACH-RESULT-CODE
                MOVE "BATCH CONTROL TOTAL MISMATCH"
                    TO LS-ACH-RESULT-MSG
            END-IF
        END-IF
        IF WS-IS-DEBIT = "Y"
            IF LS-ACH-AMOUNT NOT = LS-ACH-BATCH-DR-TOTAL
                MOVE "E0094" TO LS-ACH-RESULT-CODE
                MOVE "BATCH CONTROL TOTAL MISMATCH"
                    TO LS-ACH-RESULT-MSG
            END-IF
        END-IF
    END-IF.

*> ---------------------------------------------------------------
*> CHECK-ACCOUNT - Validate account exists and is usable
*> ---------------------------------------------------------------
CHECK-ACCOUNT.
    *> Check for non-existent account
    IF ACCT-NUMBER = 0 OR ACCT-STATUS = SPACES
        MOVE "Y" TO LS-ACH-RETURN-FLAG
        MOVE "R03" TO LS-ACH-RETURN-CODE
        MOVE "NO ACCOUNT ON FILE" TO LS-ACH-RETURN-REASON
    *> Check for closed account
    ELSE IF ACCT-STATUS = "C"
        MOVE "Y" TO LS-ACH-RETURN-FLAG
        MOVE "R02" TO LS-ACH-RETURN-CODE
        MOVE "ACCOUNT CLOSED" TO LS-ACH-RETURN-REASON
    *> Check for stop payment on debits
    ELSE IF ACCT-STOP-PAYS-ACTIVE > 0
        AND WS-IS-DEBIT = "Y"
        MOVE "Y" TO LS-ACH-RETURN-FLAG
        MOVE "R08" TO LS-ACH-RETURN-CODE
        MOVE "STOP PAYMENT ON ITEM" TO LS-ACH-RETURN-REASON
    END-IF
    END-IF
    END-IF.

*> ---------------------------------------------------------------
*> CHECK-FUNDS - Verify sufficient funds for debits
*> ---------------------------------------------------------------
CHECK-FUNDS.
    IF LS-ACH-AMOUNT > ACCT-AVAIL-BAL
        MOVE "Y" TO LS-ACH-RETURN-FLAG
        MOVE "R01" TO LS-ACH-RETURN-CODE
        MOVE "INSUFFICIENT FUNDS" TO LS-ACH-RETURN-REASON
    END-IF.

*> ---------------------------------------------------------------
*> PROCESS-TRANSACTION - Apply credit or debit to account
*> ---------------------------------------------------------------
PROCESS-TRANSACTION.
    IF WS-IS-CREDIT = "Y"
        ADD LS-ACH-AMOUNT TO ACCT-LEDGER-BAL
    END-IF
    IF WS-IS-DEBIT = "Y"
        SUBTRACT LS-ACH-AMOUNT FROM ACCT-LEDGER-BAL
    END-IF
    MOVE "N" TO LS-ACH-RETURN-FLAG
    MOVE "E0000" TO LS-ACH-RESULT-CODE
    MOVE "ACH TRANSACTION PROCESSED" TO LS-ACH-RESULT-MSG.

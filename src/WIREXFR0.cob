IDENTIFICATION DIVISION.
PROGRAM-ID. WIREXFR0.
*> ================================================================
*> WIREXFR0 - Wire Transfer Processor
*> Initiates, receives, reverses, and inquires on wire transfers
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-CURRENT-DATE           PIC 9(8).
01  WS-DUAL-CONTROL-THRESHOLD PIC S9(13)V99 VALUE +50000.00.

LINKAGE SECTION.
01  LS-WIRE-FUNCTION           PIC X(4).
COPY CPYWIRE.
COPY CPYACCT.
01  LS-WIRE-RESULT.
    05  LS-WIRE-RESULT-CODE    PIC X(5).
    05  LS-WIRE-RESULT-MSG     PIC X(50).

PROCEDURE DIVISION USING LS-WIRE-FUNCTION
                         WIRE-RECORD
                         ACCT-RECORD
                         LS-WIRE-RESULT.
MAIN-LOGIC.
    MOVE "E0000" TO LS-WIRE-RESULT-CODE
    MOVE SPACES TO LS-WIRE-RESULT-MSG

    EVALUATE LS-WIRE-FUNCTION
        WHEN "SEND"
            PERFORM PROCESS-SEND
        WHEN "RECV"
            PERFORM PROCESS-RECV
        WHEN "RVRS"
            PERFORM PROCESS-REVERSE
        WHEN "INQY"
            PERFORM PROCESS-INQUIRY
        WHEN OTHER
            MOVE "E0001" TO LS-WIRE-RESULT-CODE
            MOVE "Invalid wire function" TO LS-WIRE-RESULT-MSG
    END-EVALUATE
    GOBACK.

*> ---------------------------------------------------------------
*> PROCESS-SEND - Initiate an outgoing wire transfer
*> ---------------------------------------------------------------
PROCESS-SEND.
    *> Validate amount > 0
    IF WIRE-AMOUNT <= ZERO
        MOVE "E0002" TO LS-WIRE-RESULT-CODE
        MOVE "Wire amount must be positive" TO LS-WIRE-RESULT-MSG
        GOBACK
    END-IF

    *> Validate account is active (not closed or frozen)
    IF ACCT-STATUS = "C"
        MOVE "E0011" TO LS-WIRE-RESULT-CODE
        MOVE "Account is closed" TO LS-WIRE-RESULT-MSG
        GOBACK
    END-IF
    IF ACCT-STATUS = "F"
        MOVE "E0012" TO LS-WIRE-RESULT-CODE
        MOVE "Account is frozen" TO LS-WIRE-RESULT-MSG
        GOBACK
    END-IF

    *> Validate sufficient funds
    IF WIRE-AMOUNT > ACCT-AVAIL-BAL
        MOVE "E0097" TO LS-WIRE-RESULT-CODE
        MOVE "Wire: insufficient funds" TO LS-WIRE-RESULT-MSG
        GOBACK
    END-IF

    *> Dual control: amounts >= 50000 require approval
    IF WIRE-AMOUNT >= WS-DUAL-CONTROL-THRESHOLD
        IF WIRE-APPROVED-BY = SPACES
            MOVE "E0098" TO LS-WIRE-RESULT-CODE
            MOVE "Wire: approval required for amount >= 50000"
                TO LS-WIRE-RESULT-MSG
            GOBACK
        END-IF
    END-IF

    *> Debit account
    SUBTRACT WIRE-AMOUNT FROM ACCT-LEDGER-BAL
    COMPUTE ACCT-AVAIL-BAL =
        ACCT-LEDGER-BAL - ACCT-HOLD-AMOUNT

    *> Set wire status and date
    MOVE "PR" TO WIRE-STATUS
    MOVE FUNCTION CURRENT-DATE(1:8) TO WIRE-SEND-DATE
    MOVE "O" TO WIRE-TYPE

    MOVE "E0000" TO LS-WIRE-RESULT-CODE
    MOVE "Wire transfer initiated" TO LS-WIRE-RESULT-MSG.

*> ---------------------------------------------------------------
*> PROCESS-RECV - Process an incoming wire transfer
*> ---------------------------------------------------------------
PROCESS-RECV.
    *> Validate amount > 0
    IF WIRE-AMOUNT <= ZERO
        MOVE "E0002" TO LS-WIRE-RESULT-CODE
        MOVE "Wire amount must be positive" TO LS-WIRE-RESULT-MSG
        GOBACK
    END-IF

    *> Validate account is active (not closed or frozen)
    IF ACCT-STATUS = "C"
        MOVE "E0011" TO LS-WIRE-RESULT-CODE
        MOVE "Account is closed" TO LS-WIRE-RESULT-MSG
        GOBACK
    END-IF
    IF ACCT-STATUS = "F"
        MOVE "E0012" TO LS-WIRE-RESULT-CODE
        MOVE "Account is frozen" TO LS-WIRE-RESULT-MSG
        GOBACK
    END-IF

    *> Credit account
    ADD WIRE-AMOUNT TO ACCT-LEDGER-BAL
    COMPUTE ACCT-AVAIL-BAL =
        ACCT-LEDGER-BAL - ACCT-HOLD-AMOUNT

    *> Set wire status
    MOVE "CP" TO WIRE-STATUS
    MOVE "I" TO WIRE-TYPE

    MOVE "E0000" TO LS-WIRE-RESULT-CODE
    MOVE "Wire transfer received" TO LS-WIRE-RESULT-MSG.

*> ---------------------------------------------------------------
*> PROCESS-REVERSE - Reverse a completed wire transfer
*> ---------------------------------------------------------------
PROCESS-REVERSE.
    *> Validate wire reference exists
    IF WIRE-REFERENCE-NUM = SPACES
        MOVE "E0099" TO LS-WIRE-RESULT-CODE
        MOVE "Wire: invalid wire reference" TO LS-WIRE-RESULT-MSG
        GOBACK
    END-IF

    *> If incoming wire was credited, subtract from balance
    IF WIRE-TYPE = "I"
        SUBTRACT WIRE-AMOUNT FROM ACCT-LEDGER-BAL
        COMPUTE ACCT-AVAIL-BAL =
            ACCT-LEDGER-BAL - ACCT-HOLD-AMOUNT
    END-IF

    *> If outgoing wire was debited, add back to balance
    IF WIRE-TYPE = "O"
        ADD WIRE-AMOUNT TO ACCT-LEDGER-BAL
        COMPUTE ACCT-AVAIL-BAL =
            ACCT-LEDGER-BAL - ACCT-HOLD-AMOUNT
    END-IF

    *> Set wire status to reversed
    MOVE "RV" TO WIRE-STATUS

    MOVE "E0000" TO LS-WIRE-RESULT-CODE
    MOVE "Wire transfer reversed" TO LS-WIRE-RESULT-MSG.

*> ---------------------------------------------------------------
*> PROCESS-INQUIRY - Return wire record as-is
*> ---------------------------------------------------------------
PROCESS-INQUIRY.
    MOVE "E0000" TO LS-WIRE-RESULT-CODE
    MOVE "Wire inquiry successful" TO LS-WIRE-RESULT-MSG.

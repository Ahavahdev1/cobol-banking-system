IDENTIFICATION DIVISION.
PROGRAM-ID. EODPROC0.
*> ================================================================
*> EODPROC0 - End-of-Day Batch Orchestrator
*> Processes all active accounts: interest accrual, hold release,
*> CTR aggregation, trial balance verification
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
COPY CPYCONST.

*> Hold/CTR processing
COPY CPYHOLD.
COPY CPYCTR.

*> INTCALC0 result area
01  WS-INT-RESULT.
    05  WS-INT-RESULT-CODE      PIC X(5).
    05  WS-INT-RESULT-MSG       PIC X(50).
    05  WS-DAILY-INT-AMT        PIC S9(11)V9(6).
    05  WS-NEW-ACCRUED          PIC S9(11)V9(6).
    05  WS-PAYMENT-AMT          PIC S9(11)V99.
    05  WS-PAYMENT-DUE          PIC X(1).

*> TXNPOST0 areas
COPY CPYTXN.
01  WS-GL-ENTRIES.
    05  WS-GL-DR-ACCOUNT       PIC 9(10).
    05  WS-GL-CR-ACCOUNT       PIC 9(10).
    05  WS-GL-AMOUNT           PIC S9(13)V99.
    05  WS-GL-POST-FLAG        PIC X(1).
01  WS-TXN-RESULT.
    05  WS-TXN-RESULT-CODE    PIC X(5).
    05  WS-TXN-RESULT-MSG     PIC X(50).

*> GLPOST0 areas
01  WS-GL-FUNCTION             PIC X(4).
01  WS-GL-ENTRY.
    05  WS-GLE-DR-ACCT         PIC 9(10).
    05  WS-GLE-CR-ACCT         PIC 9(10).
    05  WS-GLE-AMOUNT          PIC S9(15)V99.
    05  WS-GLE-DESCRIPTION     PIC X(40).
    05  WS-GLE-POST-DATE       PIC 9(8).
COPY CPYGL.
01  WS-TRIAL-BAL.
    05  WS-TB-TOTAL-DEBITS     PIC S9(15)V99.
    05  WS-TB-TOTAL-CREDITS    PIC S9(15)V99.
    05  WS-TB-DIFFERENCE       PIC S9(15)V99.
    05  WS-TB-IS-BALANCED      PIC X(1).
01  WS-GL-RESULT.
    05  WS-GL-RESULT-CODE      PIC X(5).
    05  WS-GL-RESULT-MSG       PIC X(50).

*> HOLDCALC0 areas
01  WS-HOLD-REQUEST.
    05  WS-HR-ACCT-NUMBER      PIC 9(12).
    05  WS-HR-DEPOSIT-AMT      PIC S9(13)V99.
    05  WS-HR-CHECK-TYPE        PIC X(2).
    05  WS-HR-DEPOSIT-DATE      PIC 9(8).
    05  WS-HR-ACCT-OPEN-DATE    PIC 9(8).
    05  WS-HR-IS-REDEPOSIT      PIC X(1).
    05  WS-HR-REPEATED-OD       PIC X(1).
01  WS-HOLD-RESULT.
    05  WS-HOLD-RESULT-CODE    PIC X(5).
    05  WS-HOLD-RESULT-MSG     PIC X(50).
    05  WS-HOLD-NEXT-DAY-AMT   PIC S9(13)V99.
    05  WS-HOLD-REMAINING-AMT  PIC S9(13)V99.
    05  WS-HOLD-RELEASE-DT     PIC 9(8).
    05  WS-HOLD-EXCEPTION-FLAG PIC X(1).

*> BSA/AML areas
01  WS-BSA-FUNCTION            PIC X(4).
01  WS-TXN-INFO.
    05  WS-BSA-CUST-ID         PIC 9(10).
    05  WS-BSA-TXN-DATE        PIC 9(8).
    05  WS-BSA-CASH-AMOUNT     PIC S9(13)V99.
    05  WS-BSA-CASH-DIRECTION  PIC X(1).
    05  WS-BSA-IS-CASH         PIC X(1).
    05  WS-BSA-ACCT-NUMBER     PIC 9(12).
01  WS-BSA-RESULT.
    05  WS-BSA-RESULT-CODE     PIC X(5).
    05  WS-BSA-RESULT-MSG      PIC X(50).
    05  WS-BSA-CTR-REQUIRED    PIC X(1).
    05  WS-BSA-CASH-IN-TOTAL   PIC S9(13)V99.
    05  WS-BSA-CASH-OUT-TOTAL  PIC S9(13)V99.

*> Audit areas
01  WS-AUDIT-FUNCTION          PIC X(4).
COPY CPYAUDT.
01  WS-AUDIT-RESULT.
    05  WS-AUDT-RESULT-CODE   PIC X(5).
    05  WS-AUDT-RESULT-MSG    PIC X(50).

*> Batch control
01  WS-ACCTS-PROCESSED        PIC 9(8) VALUE 0.
01  WS-ACCTS-ERRORS           PIC 9(8) VALUE 0.
01  WS-TOTAL-INT-ACCRUED      PIC S9(15)V99 VALUE 0.
01  WS-TOTAL-INT-PAID         PIC S9(15)V99 VALUE 0.
01  WS-HOLDS-RELEASED         PIC 9(8) VALUE 0.

LINKAGE SECTION.
01  LS-BATCH-DATE              PIC 9(8).
COPY CPYACCT.
COPY CPYBATCH.
01  LS-BATCH-RESULT.
    05  LS-BATCH-RESULT-CODE   PIC X(5).
    05  LS-BATCH-RESULT-MSG    PIC X(50).

PROCEDURE DIVISION USING LS-BATCH-DATE
                         ACCT-RECORD
                         BATCH-RECORD
                         LS-BATCH-RESULT.
MAIN-LOGIC.
    MOVE "E0000" TO LS-BATCH-RESULT-CODE
    MOVE SPACES TO LS-BATCH-RESULT-MSG
    MOVE 0 TO WS-ACCTS-PROCESSED
    MOVE 0 TO WS-ACCTS-ERRORS
    MOVE 0 TO WS-TOTAL-INT-ACCRUED
    MOVE 0 TO WS-TOTAL-INT-PAID
    MOVE 0 TO WS-HOLDS-RELEASED

    *> Update batch control record
    MOVE LS-BATCH-DATE TO BATCH-DATE
    MOVE "EOD" TO BATCH-TYPE
    MOVE "S" TO BATCH-STATUS

    *> Process provided account
    IF ACCT-STATUS = "A" OR ACCT-STATUS = "D"
        PERFORM PROCESS-ACCOUNT
    END-IF

    *> Trial balance verification
    INITIALIZE WS-GL-ENTRY
    INITIALIZE GL-RECORD
    INITIALIZE WS-TRIAL-BAL
    INITIALIZE WS-GL-RESULT
    MOVE "TBAL" TO WS-GL-FUNCTION
    CALL "GLPOST0" USING WS-GL-FUNCTION
                         WS-GL-ENTRY
                         GL-RECORD
                         WS-TRIAL-BAL
                         WS-GL-RESULT
    IF WS-GL-RESULT-CODE = "E0000"
        IF WS-TB-IS-BALANCED = "N"
            ADD 1 TO WS-ACCTS-ERRORS
        END-IF
    ELSE
        ADD 1 TO WS-ACCTS-ERRORS
    END-IF

    *> Write batch summary
    MOVE WS-ACCTS-PROCESSED TO BATCH-ACCTS-PROCESSED
    MOVE WS-ACCTS-ERRORS TO BATCH-ACCTS-ERRORS
    MOVE WS-TOTAL-INT-ACCRUED TO BATCH-INT-ACCRUED
    MOVE WS-TOTAL-INT-PAID TO BATCH-INT-PAID
    MOVE WS-HOLDS-RELEASED TO BATCH-HOLDS-RELEASED

    IF WS-ACCTS-ERRORS = 0
        MOVE "C" TO BATCH-STATUS
        MOVE "E0000" TO LS-BATCH-RESULT-CODE
        MOVE "EOD batch completed successfully"
            TO LS-BATCH-RESULT-MSG
    ELSE
        MOVE "P" TO BATCH-STATUS
        MOVE "E0003" TO LS-BATCH-RESULT-CODE
        MOVE "EOD batch completed with errors"
            TO LS-BATCH-RESULT-MSG
    END-IF
    GOBACK.

*> ---------------------------------------------------------------
*> Process a single account for EOD
*> ---------------------------------------------------------------
PROCESS-ACCOUNT.
    ADD 1 TO WS-ACCTS-PROCESSED

    *> Step 1: Accrue daily interest
    PERFORM EOD-ACCRUE-INTEREST

    *> Step 2: Check if interest payment is due
    IF WS-PAYMENT-DUE = "Y"
        PERFORM EOD-POST-INTEREST-PAYMENT
    END-IF

    *> Step 3: Release matured holds
    PERFORM EOD-RELEASE-HOLDS

    *> Step 4: CTR aggregation
    *> Note: CTR aggregation for cash transactions is handled at the
    *> transaction level by TXNPOST0 via CHECK-CTR in BSACTRO.
    *> EOD simply reports batch totals via BATCH-CTR-FILED.

    *> Step 5: Log audit trail
    PERFORM EOD-LOG-AUDIT.

*> ---------------------------------------------------------------
*> Accrue daily interest via INTCALC0
*> ---------------------------------------------------------------
EOD-ACCRUE-INTEREST.
    INITIALIZE WS-INT-RESULT
    CALL "INTCALC0" USING ACCT-RECORD
                          LS-BATCH-DATE
                          WS-INT-RESULT
    IF WS-INT-RESULT-CODE = "E0000"
        ADD WS-DAILY-INT-AMT TO WS-TOTAL-INT-ACCRUED
    ELSE
        ADD 1 TO WS-ACCTS-ERRORS
    END-IF.

*> ---------------------------------------------------------------
*> Post interest payment as a transaction
*> ---------------------------------------------------------------
EOD-POST-INTEREST-PAYMENT.
    INITIALIZE TXN-RECORD
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE ACCT-NUMBER TO TXN-ACCT-NUMBER
    MOVE "INT" TO TXN-TYPE
    MOVE "C" TO TXN-DR-CR
    MOVE WS-PAYMENT-AMT TO TXN-AMOUNT
    MOVE LS-BATCH-DATE TO TXN-POST-DATE
    MOVE LS-BATCH-DATE TO TXN-EFFECTIVE-DATE
    MOVE "SYSTEM" TO TXN-TELLER-ID
    MOVE "BR" TO TXN-CHANNEL
    CALL "TXNPOST0" USING TXN-RECORD
                          ACCT-RECORD
                          WS-GL-ENTRIES
                          WS-TXN-RESULT
    IF WS-TXN-RESULT-CODE = "E0000"
        ADD WS-PAYMENT-AMT TO WS-TOTAL-INT-PAID
    ELSE
        ADD 1 TO WS-ACCTS-ERRORS
    END-IF.

*> ---------------------------------------------------------------
*> Log EOD processing to audit trail
*> ---------------------------------------------------------------
EOD-LOG-AUDIT.
    INITIALIZE AUDIT-RECORD
    INITIALIZE WS-AUDIT-RESULT
    MOVE "WRIT" TO WS-AUDIT-FUNCTION
    MOVE "SYSTEM" TO AUDIT-USER-ID
    MOVE "EODPROC0" TO AUDIT-PROGRAM-ID
    MOVE "EOD " TO AUDIT-FUNCTION
    MOVE "ACCT" TO AUDIT-ENTITY-TYPE
    MOVE ACCT-NUMBER TO AUDIT-ENTITY-KEY
    MOVE "E0000" TO AUDIT-RESULT-CODE
    MOVE "EOD processing complete" TO AUDIT-DESCRIPTION
    CALL "AUDTLOG0" USING WS-AUDIT-FUNCTION
                          AUDIT-RECORD
                          WS-AUDIT-RESULT.

*> ---------------------------------------------------------------
*> Release matured holds on account
*> Simplified: if ACCT-HOLD-AMOUNT > 0, release all holds and
*> recalculate available balance. A production system would iterate
*> individual HOLD-RECORDs and compare HOLD-RELEASE-DATE to batch
*> date for each active hold.
*> ---------------------------------------------------------------
EOD-RELEASE-HOLDS.
    IF ACCT-HOLD-AMOUNT > 0
        SUBTRACT ACCT-HOLD-AMOUNT FROM ACCT-HOLD-AMOUNT
        COMPUTE ACCT-AVAIL-BAL =
            ACCT-LEDGER-BAL - ACCT-HOLD-AMOUNT
            ON SIZE ERROR
                ADD 1 TO WS-ACCTS-ERRORS
        END-COMPUTE
        ADD 1 TO WS-HOLDS-RELEASED
    END-IF
    *> Reset daily NSF counter for next business day
    MOVE 0 TO ACCT-NSF-COUNT-TODAY.

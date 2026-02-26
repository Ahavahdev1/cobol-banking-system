IDENTIFICATION DIVISION.
PROGRAM-ID. EOMPROC0.
*> ================================================================
*> EOMPROC0 - End-of-Month Batch Processor
*> Assesses monthly fees, resets MTD counters, generates statements
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
COPY CPYCONST.

*> Fee calculation areas
COPY CPYFEE.
01  WS-FEE-RESULT.
    05  WS-FEE-RESULT-CODE    PIC X(5).
    05  WS-FEE-RESULT-MSG     PIC X(50).
    05  WS-FEE-ASSESSED       PIC 9(5)V99.
    05  WS-FEE-WAIVED-FLAG    PIC X(1).
    05  WS-FEE-WAIVER-REASON  PIC X(2).

*> Transaction posting areas
COPY CPYTXN.
01  WS-GL-ENTRIES.
    05  WS-GL-DR-ACCOUNT      PIC 9(10).
    05  WS-GL-CR-ACCOUNT      PIC 9(10).
    05  WS-GL-AMOUNT          PIC S9(13)V99.
    05  WS-GL-POST-FLAG       PIC X(1).
01  WS-TXN-RESULT.
    05  WS-TXN-RESULT-CODE   PIC X(5).
    05  WS-TXN-RESULT-MSG    PIC X(50).

*> GL record for MTD reset
COPY CPYGL.

*> Audit
01  WS-AUDIT-FUNCTION         PIC X(4).
COPY CPYAUDT.
01  WS-AUDIT-RESULT.
    05  WS-AUDT-RESULT-CODE  PIC X(5).
    05  WS-AUDT-RESULT-MSG   PIC X(50).

*> Batch counters
01  WS-ACCTS-PROCESSED       PIC 9(8) VALUE 0.
01  WS-ACCTS-ERRORS          PIC 9(8) VALUE 0.
01  WS-TOTAL-FEES-ASSESSED   PIC S9(15)V99 VALUE 0.
01  WS-TOTAL-FEES-WAIVED     PIC S9(15)V99 VALUE 0.

LINKAGE SECTION.
01  LS-BATCH-DATE             PIC 9(8).
COPY CPYACCT.
COPY CPYBATCH.
01  LS-BATCH-RESULT.
    05  LS-BATCH-RESULT-CODE  PIC X(5).
    05  LS-BATCH-RESULT-MSG   PIC X(50).

PROCEDURE DIVISION USING LS-BATCH-DATE
                         ACCT-RECORD
                         BATCH-RECORD
                         LS-BATCH-RESULT.
MAIN-LOGIC.
    MOVE "E0000" TO LS-BATCH-RESULT-CODE
    MOVE SPACES TO LS-BATCH-RESULT-MSG
    MOVE 0 TO WS-ACCTS-PROCESSED
    MOVE 0 TO WS-ACCTS-ERRORS
    MOVE 0 TO WS-TOTAL-FEES-ASSESSED
    MOVE 0 TO WS-TOTAL-FEES-WAIVED

    *> Update batch control record
    MOVE LS-BATCH-DATE TO BATCH-DATE
    MOVE "EOM" TO BATCH-TYPE
    MOVE "S" TO BATCH-STATUS

    *> Process provided account
    IF ACCT-STATUS = "A" OR ACCT-STATUS = "D"
        PERFORM PROCESS-EOM-ACCOUNT
    END-IF

    *> Write batch summary
    MOVE WS-ACCTS-PROCESSED TO BATCH-ACCTS-PROCESSED
    MOVE WS-ACCTS-ERRORS TO BATCH-ACCTS-ERRORS
    MOVE WS-TOTAL-FEES-ASSESSED TO BATCH-FEES-ASSESSED

    IF WS-ACCTS-ERRORS = 0
        MOVE "C" TO BATCH-STATUS
        MOVE "E0000" TO LS-BATCH-RESULT-CODE
        MOVE "EOM batch completed successfully"
            TO LS-BATCH-RESULT-MSG
    ELSE
        MOVE "P" TO BATCH-STATUS
        MOVE "E0003" TO LS-BATCH-RESULT-CODE
        MOVE "EOM batch completed with errors"
            TO LS-BATCH-RESULT-MSG
    END-IF
    GOBACK.

*> ---------------------------------------------------------------
*> Process a single account for EOM
*> ---------------------------------------------------------------
PROCESS-EOM-ACCOUNT.
    ADD 1 TO WS-ACCTS-PROCESSED

    *> Step 1: Assess monthly fee
    PERFORM EOM-ASSESS-FEE

    *> Step 2: Post fee if assessed
    IF WS-FEE-ASSESSED > 0
        PERFORM EOM-POST-FEE
    END-IF

    *> Step 3: Reset MTD counters
    PERFORM EOM-RESET-MTD

    *> Step 4: Log audit
    PERFORM EOM-LOG-AUDIT.

*> ---------------------------------------------------------------
*> Assess monthly fee via FEECALC0
*> ---------------------------------------------------------------
EOM-ASSESS-FEE.
    INITIALIZE WS-FEE-RESULT
    *> Set up fee schedule from account product info
    INITIALIZE FEE-SCHEDULE-RECORD
    MOVE ACCT-PRODUCT-CODE TO FEE-PRODUCT-CODE
    MOVE "MTH" TO FEE-TYPE
    MOVE ACCT-MONTHLY-FEE TO FEE-AMOUNT
    MOVE "Monthly maintenance fee" TO FEE-DESCRIPTION
    MOVE "Y" TO FEE-WAIVER-ELIGIBLE
    MOVE ACCT-FEE-WAIVER-AMT TO FEE-MIN-BAL-THRESHOLD
    MOVE "A" TO FEE-STATUS
    CALL "FEECALC0" USING ACCT-RECORD
                          FEE-SCHEDULE-RECORD
                          WS-FEE-RESULT
    IF WS-FEE-RESULT-CODE = "E0000"
        IF WS-FEE-WAIVED-FLAG = "Y"
            ADD WS-FEE-ASSESSED TO WS-TOTAL-FEES-WAIVED
        ELSE
            ADD WS-FEE-ASSESSED TO WS-TOTAL-FEES-ASSESSED
        END-IF
    ELSE
        ADD 1 TO WS-ACCTS-ERRORS
    END-IF.

*> ---------------------------------------------------------------
*> Post assessed fee as a transaction
*> ---------------------------------------------------------------
EOM-POST-FEE.
    INITIALIZE TXN-RECORD
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE ACCT-NUMBER TO TXN-ACCT-NUMBER
    MOVE "FEE" TO TXN-TYPE
    MOVE "D" TO TXN-DR-CR
    MOVE WS-FEE-ASSESSED TO TXN-AMOUNT
    MOVE LS-BATCH-DATE TO TXN-POST-DATE
    MOVE LS-BATCH-DATE TO TXN-EFFECTIVE-DATE
    MOVE "SYSTEM" TO TXN-TELLER-ID
    MOVE "BR" TO TXN-CHANNEL
    CALL "TXNPOST0" USING TXN-RECORD
                          ACCT-RECORD
                          WS-GL-ENTRIES
                          WS-TXN-RESULT
    IF WS-TXN-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-ACCTS-ERRORS
    END-IF.

*> ---------------------------------------------------------------
*> Reset MTD counters for new month
*> ---------------------------------------------------------------
EOM-RESET-MTD.
    MOVE 0 TO ACCT-NSF-COUNT-MTD
    MOVE 0 TO ACCT-NSF-COUNT-TODAY
    MOVE 0 TO ACCT-MTD-AVG-BAL
    MOVE 0 TO ACCT-MTD-LOW-BAL.

*> ---------------------------------------------------------------
*> Log EOM processing to audit trail
*> ---------------------------------------------------------------
EOM-LOG-AUDIT.
    INITIALIZE AUDIT-RECORD
    INITIALIZE WS-AUDIT-RESULT
    MOVE "WRIT" TO WS-AUDIT-FUNCTION
    MOVE "SYSTEM" TO AUDIT-USER-ID
    MOVE "EOMPROC0" TO AUDIT-PROGRAM-ID
    MOVE "EOM " TO AUDIT-FUNCTION
    MOVE "ACCT" TO AUDIT-ENTITY-TYPE
    MOVE ACCT-NUMBER TO AUDIT-ENTITY-KEY
    MOVE "E0000" TO AUDIT-RESULT-CODE
    MOVE "EOM processing complete" TO AUDIT-DESCRIPTION
    CALL "AUDTLOG0" USING WS-AUDIT-FUNCTION
                          AUDIT-RECORD
                          WS-AUDIT-RESULT.

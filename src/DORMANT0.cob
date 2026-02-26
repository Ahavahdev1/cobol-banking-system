IDENTIFICATION DIVISION.
PROGRAM-ID. DORMANT0.
*> ================================================================
*> DORMANT0 - Dormancy Management Module
*> Detects dormant accounts and processes escheatment
*> Functions: CHKD (check dormancy), CHKE (check escheatment),
*>            STAT (status inquiry)
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-BATCH-YEAR              PIC 9(4).
01  WS-BATCH-MONTH             PIC 9(2).
01  WS-BATCH-DAY               PIC 9(2).
01  WS-TXN-YEAR                PIC 9(4).
01  WS-TXN-MONTH               PIC 9(2).
01  WS-TXN-DAY                 PIC 9(2).
01  WS-DATE-DIFF               PIC S9(9).
01  WS-BATCH-INT-DATE           PIC 9(8).
01  WS-LAST-TXN-INT-DATE        PIC 9(8).

LINKAGE SECTION.
01  LS-DORM-FUNCTION            PIC X(4).
COPY CPYACCT.
01  LS-BATCH-DATE               PIC 9(8).
01  LS-DORM-RESULT.
    05  LS-DORM-RESULT-CODE     PIC X(5).
    05  LS-DORM-RESULT-MSG      PIC X(50).
    05  LS-DORM-ACTION-TAKEN    PIC X(1).
    *> D = marked dormant, E = escheated, N = no action

PROCEDURE DIVISION USING LS-DORM-FUNCTION
                         ACCT-RECORD
                         LS-BATCH-DATE
                         LS-DORM-RESULT.
MAIN-LOGIC.
    MOVE "E0000" TO LS-DORM-RESULT-CODE
    MOVE SPACES TO LS-DORM-RESULT-MSG
    MOVE "N" TO LS-DORM-ACTION-TAKEN

    EVALUATE LS-DORM-FUNCTION
        WHEN "CHKD"
            PERFORM CHECK-DORMANCY
        WHEN "CHKE"
            PERFORM CHECK-ESCHEATMENT
        WHEN "STAT"
            PERFORM STATUS-INQUIRY
        WHEN OTHER
            MOVE "E0001" TO LS-DORM-RESULT-CODE
            MOVE "Invalid function code" TO LS-DORM-RESULT-MSG
    END-EVALUATE

    GOBACK.

*> ---------------------------------------------------------------
*> CHKD - Check if active account should be marked dormant
*> An account is dormant if status is Active and last txn date
*> is more than 1 year (approx 10000 in YYYYMMDD) before batch
*> ---------------------------------------------------------------
CHECK-DORMANCY.
    *> Only process active accounts
    IF ACCT-STATUS NOT = "A"
        MOVE "N" TO LS-DORM-ACTION-TAKEN
        MOVE "Account is not active" TO LS-DORM-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    *> If no last txn date, skip (new account or no activity data)
    IF ACCT-LAST-TXN-DATE = 0
        MOVE "N" TO LS-DORM-ACTION-TAKEN
        MOVE "No last transaction date" TO LS-DORM-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    *> Calculate date difference using intrinsic day counts
    COMPUTE WS-DATE-DIFF =
        FUNCTION INTEGER-OF-DATE(LS-BATCH-DATE)
      - FUNCTION INTEGER-OF-DATE(ACCT-LAST-TXN-DATE)

    IF WS-DATE-DIFF > 366
        MOVE "D" TO ACCT-STATUS
        MOVE "D" TO LS-DORM-ACTION-TAKEN
        MOVE "Account marked dormant - inactive > 1 year"
            TO LS-DORM-RESULT-MSG
    ELSE
        MOVE "N" TO LS-DORM-ACTION-TAKEN
        MOVE "Account activity within 1 year"
            TO LS-DORM-RESULT-MSG
    END-IF.

*> ---------------------------------------------------------------
*> CHKE - Check if dormant account should be escheated
*> Escheatment occurs after 3+ years of inactivity (>= 30000)
*> ---------------------------------------------------------------
CHECK-ESCHEATMENT.
    *> Only process dormant accounts
    IF ACCT-STATUS NOT = "D"
        MOVE "N" TO LS-DORM-ACTION-TAKEN
        MOVE "Account is not dormant" TO LS-DORM-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    *> If no last txn date, skip
    IF ACCT-LAST-TXN-DATE = 0
        MOVE "N" TO LS-DORM-ACTION-TAKEN
        MOVE "No last transaction date" TO LS-DORM-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    *> Calculate date difference using intrinsic day counts
    COMPUTE WS-DATE-DIFF =
        FUNCTION INTEGER-OF-DATE(LS-BATCH-DATE)
      - FUNCTION INTEGER-OF-DATE(ACCT-LAST-TXN-DATE)

    IF WS-DATE-DIFF > 1096
        MOVE "E" TO ACCT-STATUS
        MOVE "E" TO LS-DORM-ACTION-TAKEN
        MOVE "Account escheated - dormant > 3 years"
            TO LS-DORM-RESULT-MSG
    ELSE
        MOVE "N" TO LS-DORM-ACTION-TAKEN
        MOVE "Account dormant < 3 years"
            TO LS-DORM-RESULT-MSG
    END-IF.

*> ---------------------------------------------------------------
*> STAT - Return current dormancy status without changes
*> ---------------------------------------------------------------
STATUS-INQUIRY.
    MOVE "N" TO LS-DORM-ACTION-TAKEN
    EVALUATE ACCT-STATUS
        WHEN "A"
            MOVE "Account is active" TO LS-DORM-RESULT-MSG
        WHEN "D"
            MOVE "Account is dormant" TO LS-DORM-RESULT-MSG
        WHEN "E"
            MOVE "Account is escheated" TO LS-DORM-RESULT-MSG
        WHEN "C"
            MOVE "Account is closed" TO LS-DORM-RESULT-MSG
        WHEN "F"
            MOVE "Account is frozen" TO LS-DORM-RESULT-MSG
        WHEN OTHER
            MOVE "Unknown account status" TO LS-DORM-RESULT-MSG
    END-EVALUATE.

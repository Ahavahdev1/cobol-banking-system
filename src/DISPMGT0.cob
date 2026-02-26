IDENTIFICATION DIVISION.
PROGRAM-ID. DISPMGT0.
*> ================================================================
*> DISPMGT0 - Dispute Management (Regulation E)
*> FILE = File new dispute, PROV = Provisional credit,
*> RSLV = Resolve dispute, INQY = Inquiry
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-CURRENT-DATE-TIME.
    05  WS-CDT-YEAR            PIC 9(4).
    05  WS-CDT-MONTH           PIC 9(2).
    05  WS-CDT-DAY             PIC 9(2).
    05  WS-CDT-HOURS           PIC 9(2).
    05  WS-CDT-MINUTES         PIC 9(2).
    05  WS-CDT-SECONDS         PIC 9(2).
    05  WS-CDT-HUNDREDTHS      PIC 9(2).
    05  WS-CDT-OFFSET-DIR      PIC X(1).
    05  WS-CDT-OFFSET-HOURS    PIC 9(2).
    05  WS-CDT-OFFSET-MINS     PIC 9(2).
01  WS-TODAY-DATE              PIC 9(8).
01  WS-NEXT-DISPUTE-ID         PIC 9(15) VALUE 1.

*> DATEUTIL interface
01  WS-DATE-FUNCTION           PIC X(4).
01  WS-DATE-INPUT.
    05  WS-DATE1               PIC 9(8).
    05  WS-DATE2               PIC 9(8).
    05  WS-DAYS-TO-ADD         PIC S9(4).
01  WS-DATE-OUTPUT.
    05  WS-RESULT-DATE         PIC 9(8).
    05  WS-RESULT-DAYS         PIC S9(8).
    05  WS-RESULT-FLAG         PIC X(1).
01  WS-DATE-RESULT.
    05  WS-DATE-RESULT-CODE    PIC X(5).
    05  WS-DATE-RESULT-MSG     PIC X(50).

*> Working fields for RSLV partial credit adjustment
01  WS-REVERSE-AMT             PIC S9(13)V99.

LINKAGE SECTION.
01  LS-DSP-FUNCTION            PIC X(4).
COPY CPYDSP.
COPY CPYACCT.
01  LS-DSP-RESULT.
    05  LS-DSP-RESULT-CODE     PIC X(5).
    05  LS-DSP-RESULT-MSG      PIC X(50).

PROCEDURE DIVISION USING LS-DSP-FUNCTION
                         DISPUTE-RECORD
                         ACCT-RECORD
                         LS-DSP-RESULT.
MAIN-LOGIC.
    MOVE "E0000" TO LS-DSP-RESULT-CODE
    MOVE SPACES TO LS-DSP-RESULT-MSG

    EVALUATE LS-DSP-FUNCTION
        WHEN "FILE"
            PERFORM DO-FILE-DISPUTE
        WHEN "PROV"
            PERFORM DO-PROVISIONAL-CREDIT
        WHEN "RSLV"
            PERFORM DO-RESOLVE-DISPUTE
        WHEN "INQY"
            PERFORM DO-INQUIRY
        WHEN OTHER
            MOVE "E0001" TO LS-DSP-RESULT-CODE
            MOVE "Invalid dispute function" TO LS-DSP-RESULT-MSG
    END-EVALUATE
    GOBACK.

*> ---------------------------------------------------------------
*> FILE - File a new dispute
*> ---------------------------------------------------------------
DO-FILE-DISPUTE.
    *> Validate dispute type
    IF DSP-DISPUTE-TYPE NOT = "UNAU"
        AND DSP-DISPUTE-TYPE NOT = "ERRO"
        AND DSP-DISPUTE-TYPE NOT = "DUPE"
        AND DSP-DISPUTE-TYPE NOT = "NORC"
        AND DSP-DISPUTE-TYPE NOT = "WRNG"
        MOVE "E0085" TO LS-DSP-RESULT-CODE
        MOVE "Invalid dispute type" TO LS-DSP-RESULT-MSG
        GOBACK
    END-IF

    *> Set status to Pending
    MOVE "P" TO DSP-STATUS

    *> Set dispute date to today
    MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-TIME
    STRING WS-CDT-YEAR WS-CDT-MONTH WS-CDT-DAY
        DELIMITED BY SIZE INTO WS-TODAY-DATE
    END-STRING
    MOVE WS-TODAY-DATE TO DSP-DISPUTE-DATE

    *> Compute deadline = dispute date + 10 business days
    INITIALIZE WS-DATE-INPUT
    INITIALIZE WS-DATE-OUTPUT
    INITIALIZE WS-DATE-RESULT
    MOVE "BDAY" TO WS-DATE-FUNCTION
    MOVE WS-TODAY-DATE TO WS-DATE1
    MOVE +10 TO WS-DAYS-TO-ADD
    CALL "DATEUTIL" USING WS-DATE-FUNCTION
                          WS-DATE-INPUT
                          WS-DATE-OUTPUT
                          WS-DATE-RESULT
    MOVE WS-RESULT-DATE TO DSP-DEADLINE-DATE

    *> Assign dispute ID (auto-increment)
    MOVE WS-NEXT-DISPUTE-ID TO DSP-DISPUTE-ID
    ADD 1 TO WS-NEXT-DISPUTE-ID

    MOVE "E0000" TO LS-DSP-RESULT-CODE
    MOVE "Dispute filed successfully" TO LS-DSP-RESULT-MSG.

*> ---------------------------------------------------------------
*> PROV - Issue provisional credit
*> ---------------------------------------------------------------
DO-PROVISIONAL-CREDIT.
    *> Validate dispute exists
    IF DSP-DISPUTE-ID = 0
        MOVE "E0087" TO LS-DSP-RESULT-CODE
        MOVE "Dispute not found" TO LS-DSP-RESULT-MSG
        GOBACK
    END-IF

    *> Validate status is P or I (not already resolved/denied/credited)
    IF DSP-STATUS NOT = "P" AND DSP-STATUS NOT = "I"
        MOVE "E0086" TO LS-DSP-RESULT-CODE
        MOVE "Invalid dispute status for operation"
            TO LS-DSP-RESULT-MSG
        GOBACK
    END-IF

    *> Set status to Credited
    MOVE "C" TO DSP-STATUS

    *> Set provisional amount and date
    MOVE DSP-TXN-AMOUNT TO DSP-PROVISIONAL-AMT

    MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-TIME
    STRING WS-CDT-YEAR WS-CDT-MONTH WS-CDT-DAY
        DELIMITED BY SIZE INTO WS-TODAY-DATE
    END-STRING
    MOVE WS-TODAY-DATE TO DSP-PROVISIONAL-DATE

    *> Credit account
    ADD DSP-PROVISIONAL-AMT TO ACCT-LEDGER-BAL
        ON SIZE ERROR
            MOVE "E0040" TO LS-DSP-RESULT-CODE
            MOVE "Arithmetic overflow on provisional credit"
                TO LS-DSP-RESULT-MSG
            GOBACK
    END-ADD
    COMPUTE ACCT-AVAIL-BAL =
        ACCT-LEDGER-BAL - ACCT-HOLD-AMOUNT
        ON SIZE ERROR
            MOVE "E0040" TO LS-DSP-RESULT-CODE
            MOVE "Arithmetic overflow on available balance"
                TO LS-DSP-RESULT-MSG
            GOBACK
    END-COMPUTE

    MOVE "E0000" TO LS-DSP-RESULT-CODE
    MOVE "Provisional credit issued" TO LS-DSP-RESULT-MSG.

*> ---------------------------------------------------------------
*> RSLV - Resolve dispute
*> ---------------------------------------------------------------
DO-RESOLVE-DISPUTE.
    *> Validate dispute exists
    IF DSP-DISPUTE-ID = 0
        MOVE "E0087" TO LS-DSP-RESULT-CODE
        MOVE "Dispute not found" TO LS-DSP-RESULT-MSG
        GOBACK
    END-IF

    *> Set resolution date
    MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-TIME
    STRING WS-CDT-YEAR WS-CDT-MONTH WS-CDT-DAY
        DELIMITED BY SIZE INTO WS-TODAY-DATE
    END-STRING
    MOVE WS-TODAY-DATE TO DSP-RESOLUTION-DATE

    EVALUATE DSP-RESOLUTION-CODE
        WHEN "AP"
            *> Approved: keep provisional credit, mark resolved
            MOVE "R" TO DSP-STATUS
        WHEN "DN"
            *> Denied: reverse provisional credit
            IF DSP-PROVISIONAL-AMT > 0
                SUBTRACT DSP-PROVISIONAL-AMT FROM ACCT-LEDGER-BAL
                    ON SIZE ERROR
                        MOVE "E0040" TO LS-DSP-RESULT-CODE
                        MOVE "Arithmetic overflow on credit reversal"
                            TO LS-DSP-RESULT-MSG
                        GOBACK
                END-SUBTRACT
                COMPUTE ACCT-AVAIL-BAL =
                    ACCT-LEDGER-BAL - ACCT-HOLD-AMOUNT
                    ON SIZE ERROR
                        MOVE "E0040" TO LS-DSP-RESULT-CODE
                        MOVE "Arithmetic overflow on available balance"
                            TO LS-DSP-RESULT-MSG
                        GOBACK
                END-COMPUTE
            END-IF
            MOVE "D" TO DSP-STATUS
        WHEN "PA"
            *> Partial: reverse difference between provisional
            *> and the partial amount the caller set in
            *> DSP-TXN-AMOUNT (the approved partial amount)
            IF DSP-PROVISIONAL-AMT > DSP-TXN-AMOUNT
                COMPUTE WS-REVERSE-AMT =
                    DSP-PROVISIONAL-AMT - DSP-TXN-AMOUNT
                SUBTRACT WS-REVERSE-AMT FROM ACCT-LEDGER-BAL
                    ON SIZE ERROR
                        MOVE "E0040" TO LS-DSP-RESULT-CODE
                        MOVE "Arithmetic overflow on partial reversal"
                            TO LS-DSP-RESULT-MSG
                        GOBACK
                END-SUBTRACT
                COMPUTE ACCT-AVAIL-BAL =
                    ACCT-LEDGER-BAL - ACCT-HOLD-AMOUNT
                    ON SIZE ERROR
                        MOVE "E0040" TO LS-DSP-RESULT-CODE
                        MOVE "Arithmetic overflow on available balance"
                            TO LS-DSP-RESULT-MSG
                        GOBACK
                END-COMPUTE
            END-IF
            MOVE "R" TO DSP-STATUS
        WHEN OTHER
            MOVE "E0086" TO LS-DSP-RESULT-CODE
            MOVE "Invalid resolution code" TO LS-DSP-RESULT-MSG
            GOBACK
    END-EVALUATE

    MOVE "E0000" TO LS-DSP-RESULT-CODE
    MOVE "Dispute resolved" TO LS-DSP-RESULT-MSG.

*> ---------------------------------------------------------------
*> INQY - Inquiry (return dispute record as-is)
*> ---------------------------------------------------------------
DO-INQUIRY.
    MOVE "E0000" TO LS-DSP-RESULT-CODE
    MOVE "Dispute inquiry complete" TO LS-DSP-RESULT-MSG.

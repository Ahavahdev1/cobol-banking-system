IDENTIFICATION DIVISION.
PROGRAM-ID. SARFIL0.
*> ================================================================
*> SARFIL0 - Suspicious Activity Report Filing Manager
*> CRTE = Create SAR filing, FILE = File with FinCEN,
*> DISM = Dismiss SAR, QURY = Query SAR, UPDT = Update SAR
*> BSA requires SAR filing when structuring or other suspicious
*> activity is detected
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
*> NOTE: WS-NEXT-SAR-ID resets to 1 on program restart.
*> In production, persist the last-used ID to a file or
*> database and reload it at startup to avoid duplicates.
01  WS-NEXT-SAR-ID            PIC 9(15) VALUE 1.
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
01  WS-DATE-STAMP             PIC 9(8).

LINKAGE SECTION.
01  LS-SAR-FUNCTION            PIC X(4).
COPY CPYSAR.
01  LS-SAR-RESULT.
    05  LS-SAR-RESULT-CODE     PIC X(5).
    05  LS-SAR-RESULT-MSG      PIC X(50).

PROCEDURE DIVISION USING LS-SAR-FUNCTION
                         SAR-RECORD
                         LS-SAR-RESULT.
MAIN-LOGIC.
    MOVE "E0000" TO LS-SAR-RESULT-CODE
    MOVE SPACES TO LS-SAR-RESULT-MSG

    EVALUATE LS-SAR-FUNCTION
        WHEN "CRTE"
            PERFORM DO-CREATE-SAR
        WHEN "FILE"
            PERFORM DO-FILE-SAR
        WHEN "DISM"
            PERFORM DO-DISMISS-SAR
        WHEN "QURY"
            PERFORM DO-QUERY-SAR
        WHEN "UPDT"
            PERFORM DO-UPDATE-SAR
        WHEN OTHER
            MOVE "E0001" TO LS-SAR-RESULT-CODE
            MOVE "Invalid SAR function" TO LS-SAR-RESULT-MSG
    END-EVALUATE
    GOBACK.

*> ---------------------------------------------------------------
*> CRTE - Create a new SAR filing record
*> Validates customer ID and amount, assigns filing ID,
*> sets status to Pending, stamps current date
*> ---------------------------------------------------------------
DO-CREATE-SAR.
    *> Validate customer ID is not zero
    IF SAR-CUST-ID = 0
        MOVE "E0002" TO LS-SAR-RESULT-CODE
        MOVE "Customer ID required for SAR filing"
            TO LS-SAR-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    *> Validate total amount is positive
    IF SAR-TOTAL-AMOUNT <= 0
        MOVE "E0003" TO LS-SAR-RESULT-CODE
        MOVE "SAR amount must be greater than zero"
            TO LS-SAR-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    *> Generate date stamp
    MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-TIME
    STRING WS-CDT-YEAR WS-CDT-MONTH WS-CDT-DAY
        DELIMITED BY SIZE INTO WS-DATE-STAMP
    END-STRING

    *> Assign filing ID and stamp dates
    MOVE WS-NEXT-SAR-ID TO SAR-FILING-ID
    ADD 1 TO WS-NEXT-SAR-ID
        ON SIZE ERROR
            MOVE "E0040" TO LS-SAR-RESULT-CODE
            MOVE "SAR ID overflow - maximum ID exceeded"
                TO LS-SAR-RESULT-MSG
            EXIT PARAGRAPH
    END-ADD
    MOVE WS-DATE-STAMP TO SAR-FILED-DATE

    *> Set status to Pending
    MOVE "P" TO SAR-STATUS

    MOVE "E0000" TO LS-SAR-RESULT-CODE
    MOVE "SAR filing created" TO LS-SAR-RESULT-MSG.

*> ---------------------------------------------------------------
*> FILE - File SAR with FinCEN
*> Validates status is Pending, changes to Filed, stamps date
*> ---------------------------------------------------------------
DO-FILE-SAR.
    *> Already filed SARs cannot be re-filed
    IF SAR-STATUS = "F"
        MOVE "E0005" TO LS-SAR-RESULT-CODE
        MOVE "SAR already filed - duplicate submission"
            TO LS-SAR-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    *> Only pending SARs can be filed
    IF SAR-STATUS NOT = "P"
        MOVE "E0004" TO LS-SAR-RESULT-CODE
        MOVE "SAR must be in pending status to file"
            TO LS-SAR-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    *> Generate date stamp
    MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-TIME
    STRING WS-CDT-YEAR WS-CDT-MONTH WS-CDT-DAY
        DELIMITED BY SIZE INTO WS-DATE-STAMP
    END-STRING

    *> Mark as filed
    MOVE "F" TO SAR-STATUS
    MOVE WS-DATE-STAMP TO SAR-FILED-DATE

    MOVE "E0000" TO LS-SAR-RESULT-CODE
    MOVE "SAR filed with FinCEN" TO LS-SAR-RESULT-MSG.

*> ---------------------------------------------------------------
*> DISM - Dismiss SAR
*> Validates status is Pending, requires narrative reason,
*> changes to Dismissed. Filed SARs cannot be dismissed.
*> ---------------------------------------------------------------
DO-DISMISS-SAR.
    *> Filed SARs cannot be dismissed
    IF SAR-STATUS = "F"
        MOVE "E0006" TO LS-SAR-RESULT-CODE
        MOVE "Cannot dismiss a filed SAR - unauthorized"
            TO LS-SAR-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    *> Only pending SARs can be dismissed
    IF SAR-STATUS NOT = "P"
        MOVE "E0004" TO LS-SAR-RESULT-CODE
        MOVE "SAR must be in pending status to dismiss"
            TO LS-SAR-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    *> Require narrative reason for dismissal
    IF SAR-NARRATIVE = SPACES OR SAR-NARRATIVE = LOW-VALUES
        MOVE "E0007" TO LS-SAR-RESULT-CODE
        MOVE "Narrative required to dismiss SAR"
            TO LS-SAR-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    *> Mark as dismissed
    MOVE "D" TO SAR-STATUS

    MOVE "E0000" TO LS-SAR-RESULT-CODE
    MOVE "SAR dismissed" TO LS-SAR-RESULT-MSG.

*> ---------------------------------------------------------------
*> QURY - Query SAR status
*> Returns current SAR-RECORD data as-is
*> ---------------------------------------------------------------
DO-QUERY-SAR.
    IF SAR-CUST-ID = 0
        MOVE "E0002" TO LS-SAR-RESULT-CODE
        MOVE "Customer ID required for SAR query"
            TO LS-SAR-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    MOVE "E0000" TO LS-SAR-RESULT-CODE
    MOVE "SAR record retrieved" TO LS-SAR-RESULT-MSG.

*> ---------------------------------------------------------------
*> UPDT - Update SAR narrative/details while pending
*> Filed SARs cannot be updated
*> ---------------------------------------------------------------
DO-UPDATE-SAR.
    *> Filed SARs cannot be updated
    IF SAR-STATUS = "F"
        MOVE "E0006" TO LS-SAR-RESULT-CODE
        MOVE "Cannot update a filed SAR - unauthorized"
            TO LS-SAR-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    *> Only pending SARs can be updated
    IF SAR-STATUS NOT = "P"
        MOVE "E0004" TO LS-SAR-RESULT-CODE
        MOVE "SAR must be in pending status to update"
            TO LS-SAR-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    MOVE "E0000" TO LS-SAR-RESULT-CODE
    MOVE "SAR record updated" TO LS-SAR-RESULT-MSG.

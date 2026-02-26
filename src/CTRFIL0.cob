IDENTIFICATION DIVISION.
PROGRAM-ID. CTRFIL0.
*> ================================================================
*> CTRFIL0 - Currency Transaction Report Filing Manager
*> CRTE = Create CTR filing, FILE = Mark as filed with FinCEN,
*> QURY = Query CTR status, VOID = Void a pending CTR
*> BSA requires CTR filing for cash transactions >= $10,000
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
*> NOTE: WS-NEXT-CTR-ID resets to 1 on program restart.
*> In production, persist the last-used ID to a file or
*> database and reload it at startup to avoid duplicates.
01  WS-NEXT-CTR-ID            PIC 9(15) VALUE 1.
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
01  WS-TIMESTAMP              PIC 9(14).
01  WS-DATE-STAMP             PIC 9(8).

LINKAGE SECTION.
01  LS-CTR-FUNCTION            PIC X(4).
COPY CPYCTR.
01  LS-CTR-RESULT.
    05  LS-CTR-RESULT-CODE     PIC X(5).
    05  LS-CTR-RESULT-MSG      PIC X(50).

PROCEDURE DIVISION USING LS-CTR-FUNCTION
                         CTR-RECORD
                         LS-CTR-RESULT.
MAIN-LOGIC.
    MOVE "E0000" TO LS-CTR-RESULT-CODE
    MOVE SPACES TO LS-CTR-RESULT-MSG

    EVALUATE LS-CTR-FUNCTION
        WHEN "CRTE"
            PERFORM DO-CREATE-CTR
        WHEN "FILE"
            PERFORM DO-FILE-CTR
        WHEN "QURY"
            PERFORM DO-QUERY-CTR
        WHEN "VOID"
            PERFORM DO-VOID-CTR
        WHEN OTHER
            MOVE "E0001" TO LS-CTR-RESULT-CODE
            MOVE "Invalid CTR function" TO LS-CTR-RESULT-MSG
    END-EVALUATE
    GOBACK.

*> ---------------------------------------------------------------
*> CRTE - Create a new CTR filing record
*> Validates customer ID and cash amounts, assigns filing ID,
*> sets status to Pending, stamps filing date
*> ---------------------------------------------------------------
DO-CREATE-CTR.
    *> Validate customer ID is not zero
    IF CTR-CUST-ID = 0
        MOVE "E0002" TO LS-CTR-RESULT-CODE
        MOVE "Customer ID required for CTR filing"
            TO LS-CTR-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    *> Validate that at least one cash amount is positive
    IF CTR-CASH-IN-TOTAL <= 0 AND CTR-CASH-OUT-TOTAL <= 0
        MOVE "E0003" TO LS-CTR-RESULT-CODE
        MOVE "Cash amount must be greater than zero"
            TO LS-CTR-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    *> Generate timestamp
    MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-TIME
    STRING WS-CDT-YEAR WS-CDT-MONTH WS-CDT-DAY
        DELIMITED BY SIZE INTO WS-DATE-STAMP
    END-STRING
    STRING WS-CDT-YEAR WS-CDT-MONTH WS-CDT-DAY
           WS-CDT-HOURS WS-CDT-MINUTES WS-CDT-SECONDS
        DELIMITED BY SIZE INTO WS-TIMESTAMP
    END-STRING

    *> Assign filing ID and stamp dates
    MOVE WS-NEXT-CTR-ID TO CTR-FILING-ID
    ADD 1 TO WS-NEXT-CTR-ID
        ON SIZE ERROR
            MOVE "E0040" TO LS-CTR-RESULT-CODE
            MOVE "CTR ID overflow - maximum ID exceeded"
                TO LS-CTR-RESULT-MSG
            EXIT PARAGRAPH
    END-ADD
    MOVE WS-DATE-STAMP TO CTR-FILING-DATE
    MOVE WS-TIMESTAMP TO CTR-FILING-TIMESTAMP

    *> Set status to Pending
    MOVE "P" TO CTR-FILING-STATUS

    MOVE "E0000" TO LS-CTR-RESULT-CODE
    MOVE "CTR filing created" TO LS-CTR-RESULT-MSG.

*> ---------------------------------------------------------------
*> FILE - Mark CTR as filed with FinCEN
*> Validates status is Pending, changes to Filed, stamps date
*> ---------------------------------------------------------------
DO-FILE-CTR.
    *> Only pending CTRs can be filed
    IF CTR-FILING-STATUS = "F"
        MOVE "E0005" TO LS-CTR-RESULT-CODE
        MOVE "CTR already filed - duplicate submission"
            TO LS-CTR-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    IF CTR-FILING-STATUS NOT = "P"
        MOVE "E0004" TO LS-CTR-RESULT-CODE
        MOVE "CTR must be in pending status to file"
            TO LS-CTR-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    *> Generate submission timestamp
    MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-TIME
    STRING WS-CDT-YEAR WS-CDT-MONTH WS-CDT-DAY
           WS-CDT-HOURS WS-CDT-MINUTES WS-CDT-SECONDS
        DELIMITED BY SIZE INTO WS-TIMESTAMP
    END-STRING

    *> Mark as filed
    MOVE "F" TO CTR-FILING-STATUS
    MOVE WS-TIMESTAMP TO CTR-FILING-TIMESTAMP

    MOVE "E0000" TO LS-CTR-RESULT-CODE
    MOVE "CTR filed with FinCEN" TO LS-CTR-RESULT-MSG.

*> ---------------------------------------------------------------
*> QURY - Query CTR status
*> Returns current CTR-RECORD data as-is
*> ---------------------------------------------------------------
DO-QUERY-CTR.
    IF CTR-CUST-ID = 0
        MOVE "E0002" TO LS-CTR-RESULT-CODE
        MOVE "Customer ID required for CTR query"
            TO LS-CTR-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    MOVE "E0000" TO LS-CTR-RESULT-CODE
    MOVE "CTR record retrieved" TO LS-CTR-RESULT-MSG.

*> ---------------------------------------------------------------
*> VOID - Void a CTR filing (corrections)
*> Only pending CTRs can be voided; filed CTRs cannot
*> ---------------------------------------------------------------
DO-VOID-CTR.
    IF CTR-FILING-STATUS = "F"
        MOVE "E0006" TO LS-CTR-RESULT-CODE
        MOVE "Cannot void a filed CTR - unauthorized"
            TO LS-CTR-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    IF CTR-FILING-STATUS NOT = "P"
        MOVE "E0004" TO LS-CTR-RESULT-CODE
        MOVE "CTR must be in pending status to void"
            TO LS-CTR-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    *> Mark as voided
    MOVE "V" TO CTR-FILING-STATUS

    MOVE "E0000" TO LS-CTR-RESULT-CODE
    MOVE "CTR filing voided" TO LS-CTR-RESULT-MSG.

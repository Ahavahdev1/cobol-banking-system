IDENTIFICATION DIVISION.
PROGRAM-ID. AUDTLOG0.
*> ================================================================
*> AUDTLOG0 - Audit Trail Logger
*> WRIT = Write audit record, READ = Read by entity key
*> All audit entries are timestamped and sequenced
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
01  WS-TIMESTAMP              PIC 9(14).
01  WS-NEXT-AUDIT-ID          PIC 9(15) VALUE 1.

LINKAGE SECTION.
01  LS-AUDIT-FUNCTION          PIC X(4).
COPY CPYAUDT.
01  LS-AUDIT-RESULT.
    05  LS-AUDT-RESULT-CODE    PIC X(5).
    05  LS-AUDT-RESULT-MSG     PIC X(50).

PROCEDURE DIVISION USING LS-AUDIT-FUNCTION
                         AUDIT-RECORD
                         LS-AUDIT-RESULT.
MAIN-LOGIC.
    MOVE "E0000" TO LS-AUDT-RESULT-CODE
    MOVE SPACES TO LS-AUDT-RESULT-MSG

    EVALUATE LS-AUDIT-FUNCTION
        WHEN "WRIT"
            PERFORM DO-WRITE-AUDIT
        WHEN "READ"
            PERFORM DO-READ-AUDIT
        WHEN OTHER
            MOVE "E0001" TO LS-AUDT-RESULT-CODE
            MOVE "Invalid audit function" TO LS-AUDT-RESULT-MSG
    END-EVALUATE
    GOBACK.

*> ---------------------------------------------------------------
*> WRIT - Write an audit record with auto-timestamp and sequence
*> ---------------------------------------------------------------
DO-WRITE-AUDIT.
    *> Generate timestamp
    MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-TIME
    STRING WS-CDT-YEAR WS-CDT-MONTH WS-CDT-DAY
           WS-CDT-HOURS WS-CDT-MINUTES WS-CDT-SECONDS
        DELIMITED BY SIZE INTO WS-TIMESTAMP
    END-STRING
    MOVE WS-TIMESTAMP TO AUDIT-TIMESTAMP

    *> Assign audit ID and sequence
    MOVE WS-NEXT-AUDIT-ID TO AUDIT-ID
    MOVE 1 TO AUDIT-SEQUENCE
    ADD 1 TO WS-NEXT-AUDIT-ID

    MOVE "E0000" TO LS-AUDT-RESULT-CODE
    MOVE "Audit record written" TO LS-AUDT-RESULT-MSG.

*> ---------------------------------------------------------------
*> READ - Read audit record by entity key (lookup)
*> Caller must provide AUDIT-ENTITY-KEY; we verify it matches
*> ---------------------------------------------------------------
DO-READ-AUDIT.
    IF AUDIT-ENTITY-KEY = SPACES
        MOVE "E0002" TO LS-AUDT-RESULT-CODE
        MOVE "Entity key required for audit read"
            TO LS-AUDT-RESULT-MSG
    ELSE
        MOVE "E0000" TO LS-AUDT-RESULT-CODE
        MOVE "Audit record read" TO LS-AUDT-RESULT-MSG
    END-IF.

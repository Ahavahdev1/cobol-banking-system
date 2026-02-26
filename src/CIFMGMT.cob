IDENTIFICATION DIVISION.
PROGRAM-ID. CIFMGMT.
*> ================================================================
*> CIFMGMT - Customer Information File Management
*> VALD = Validate CIF, INIT = Initialize new CIF
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-CURRENT-DATE-DATA.
    05  WS-CURRENT-DATE        PIC 9(8).
    05  WS-CURRENT-TIME        PIC 9(6).
    05  FILLER                 PIC X(7).

LINKAGE SECTION.
01  LS-FUNCTION                   PIC X(4).
COPY CPYCIF.
01  LS-CIF-RESULT.
    05  LS-CIF-RESULT-CODE        PIC X(5).
    05  LS-CIF-RESULT-MSG         PIC X(50).

PROCEDURE DIVISION USING LS-FUNCTION
                         CIF-RECORD
                         LS-CIF-RESULT.
    EVALUATE LS-FUNCTION
        WHEN "VALD"
            PERFORM VALIDATE-CIF
        WHEN "INIT"
            PERFORM INIT-CIF
        WHEN OTHER
            MOVE "E0001" TO LS-CIF-RESULT-CODE
            MOVE "Invalid function code" TO LS-CIF-RESULT-MSG
    END-EVALUATE
    GOBACK.

*> ---------------------------------------------------------------
*> VALIDATE-CIF - Validate CIF record fields
*> ---------------------------------------------------------------
VALIDATE-CIF.
    MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA

    IF CIF-SSN-TIN = 0
        MOVE "E0022" TO LS-CIF-RESULT-CODE
        MOVE "Missing SSN/TIN" TO LS-CIF-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    IF CIF-SSN-TYPE NOT = "S"
        AND CIF-SSN-TYPE NOT = "E"
        AND CIF-SSN-TYPE NOT = "I"
        MOVE "E0023" TO LS-CIF-RESULT-CODE
        MOVE "Invalid SSN type" TO LS-CIF-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    IF CIF-CUST-TYPE NOT = "I"
        AND CIF-CUST-TYPE NOT = "J"
        AND CIF-CUST-TYPE NOT = "B"
        AND CIF-CUST-TYPE NOT = "T"
        AND CIF-CUST-TYPE NOT = "E"
        MOVE "E0024" TO LS-CIF-RESULT-CODE
        MOVE "Invalid customer type" TO LS-CIF-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    IF CIF-OFAC-STATUS = "M"
        MOVE "E0025" TO LS-CIF-RESULT-CODE
        MOVE "OFAC match - prohibited" TO LS-CIF-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    IF CIF-BSA-RISK-RATING = 4
        MOVE "E0026" TO LS-CIF-RESULT-CODE
        MOVE "BSA risk rating prohibited" TO LS-CIF-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    IF CIF-CIP-VERIFIED = "N"
        MOVE "E0027" TO LS-CIF-RESULT-CODE
        MOVE "CIP not verified" TO LS-CIF-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    IF CIF-DOB > WS-CURRENT-DATE
        MOVE "E0028" TO LS-CIF-RESULT-CODE
        MOVE "Future date of birth" TO LS-CIF-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    MOVE "E0000" TO LS-CIF-RESULT-CODE
    MOVE "CIF validation successful" TO LS-CIF-RESULT-MSG.

*> ---------------------------------------------------------------
*> INIT-CIF - Initialize a new CIF record
*> ---------------------------------------------------------------
INIT-CIF.
    MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA

    MOVE "A" TO CIF-STATUS
    MOVE WS-CURRENT-DATE TO CIF-OPEN-DATE
    MOVE 0 TO CIF-NUM-ACCOUNTS
    MOVE WS-CURRENT-DATE TO CIF-CREATED-DATE
    MOVE WS-CURRENT-TIME TO CIF-CREATED-TIME
    MOVE "N" TO CIF-CIP-VERIFIED

    MOVE "E0000" TO LS-CIF-RESULT-CODE
    MOVE "CIF initialized successfully" TO LS-CIF-RESULT-MSG.

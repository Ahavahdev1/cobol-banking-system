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
01  WS-MIN-AGE-DATE            PIC 9(8).
01  WS-DOB-YYYY               PIC 9(4).
01  WS-DOB-MMDD               PIC 9(4).

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

    IF CIF-CIP-VERIFIED = "Y"
        AND CIF-CIP-DOC-EXPIRY NOT = 0
        AND CIF-CIP-DOC-EXPIRY < WS-CURRENT-DATE
        MOVE "E0084" TO LS-CIF-RESULT-CODE
        MOVE "CIP document expired" TO LS-CIF-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    IF CIF-DOB = 0
        MOVE "E0030" TO LS-CIF-RESULT-CODE
        MOVE "Invalid or missing date of birth"
            TO LS-CIF-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    IF CIF-DOB > WS-CURRENT-DATE
        MOVE "E0028" TO LS-CIF-RESULT-CODE
        MOVE "Future date of birth" TO LS-CIF-RESULT-MSG
        EXIT PARAGRAPH
    END-IF

    DIVIDE CIF-DOB BY 10000
        GIVING WS-DOB-YYYY REMAINDER WS-DOB-MMDD
    COMPUTE WS-MIN-AGE-DATE =
        (WS-DOB-YYYY + 18) * 10000 + WS-DOB-MMDD
    IF WS-MIN-AGE-DATE > WS-CURRENT-DATE
        MOVE "E0029" TO LS-CIF-RESULT-CODE
        MOVE "Customer under minimum age" TO LS-CIF-RESULT-MSG
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

    *> Initialize regulatory fields to safe defaults
    MOVE 1 TO CIF-BSA-RISK-RATING
    MOVE "C" TO CIF-OFAC-STATUS
    MOVE "N" TO CIF-CTR-EXEMPT
    MOVE "US" TO CIF-COUNTRY

    MOVE "E0000" TO LS-CIF-RESULT-CODE
    MOVE "CIF initialized successfully" TO LS-CIF-RESULT-MSG.

IDENTIFICATION DIVISION.
PROGRAM-ID. OFACCHK0.
*> ================================================================
*> OFACCHK0 - OFAC Sanctions Screening Module
*> Functions:
*>   CHKN - Check name against OFAC watch list
*>   CHKB - Check beneficiary (name + country)
*>   STAT - Check if last OFAC screening is current
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-CURRENT-DATE            PIC 9(8).
01  WS-IDX                     PIC 9(2).
01  WS-NAME-UPPER              PIC X(50).
01  WS-CHECK-UPPER             PIC X(50).
01  WS-COUNTRY-IDX             PIC 9(2).
01  WS-COUNTRY-MATCH           PIC X(1).

*> Internal OFAC watch list for testing
*> In production this would be an external database
01  WS-WATCH-LIST.
    05  WS-WATCH-COUNT         PIC 9(2) VALUE 05.
    05  WS-WATCH-ENTRY.
        10  WS-WATCH-NAME-1    PIC X(50)
            VALUE "OFAC-TEST-MATCH".
        10  WS-WATCH-LIST-1    PIC X(10)
            VALUE "SDN".
        10  WS-WATCH-NAME-2    PIC X(50)
            VALUE "BLOCKED PERSON ONE".
        10  WS-WATCH-LIST-2    PIC X(10)
            VALUE "SDN".
        10  WS-WATCH-NAME-3    PIC X(50)
            VALUE "SANCTIONED ENTITY".
        10  WS-WATCH-LIST-3    PIC X(10)
            VALUE "CONS".
        10  WS-WATCH-NAME-4    PIC X(50)
            VALUE "PROHIBITED TRADING CO".
        10  WS-WATCH-LIST-4    PIC X(10)
            VALUE "NS-PLC".
        10  WS-WATCH-NAME-5    PIC X(50)
            VALUE "TEST SANCTIONS NAME".
        10  WS-WATCH-LIST-5    PIC X(10)
            VALUE "SDN".
    05  WS-WATCH-TABLE REDEFINES WS-WATCH-ENTRY.
        10  WS-WATCH-ITEM OCCURS 5 TIMES.
            15  WS-WATCH-NAME  PIC X(50).
            15  WS-WATCH-LST   PIC X(10).

*> Sanctioned countries list
01  WS-SANCTIONED-COUNTRIES.
    05  WS-SANCT-COUNT         PIC 9(2) VALUE 05.
    05  WS-SANCT-ENTRY.
        10  WS-SANCT-C-1       PIC X(3) VALUE "CU ".
        10  WS-SANCT-C-2       PIC X(3) VALUE "IR ".
        10  WS-SANCT-C-3       PIC X(3) VALUE "KP ".
        10  WS-SANCT-C-4       PIC X(3) VALUE "SY ".
        10  WS-SANCT-C-5       PIC X(3) VALUE "RU ".
    05  WS-SANCT-TABLE REDEFINES WS-SANCT-ENTRY.
        10  WS-SANCT-COUNTRY   PIC X(3) OCCURS 5 TIMES.

*> Internal result for CHKN when called from CHKB
01  WS-INTERNAL-RESULT.
    05  WS-INT-RESULT-CODE     PIC X(5).
    05  WS-INT-RESULT-MSG      PIC X(50).

LINKAGE SECTION.
01  LS-OFAC-FUNCTION           PIC X(4).
COPY CPYOFAC.
01  LS-OFAC-RESULT.
    05  LS-OFAC-RESULT-CODE    PIC X(5).
    05  LS-OFAC-RESULT-MSG     PIC X(50).

PROCEDURE DIVISION USING LS-OFAC-FUNCTION
                         OFAC-CHECK-RECORD
                         LS-OFAC-RESULT.
    EVALUATE LS-OFAC-FUNCTION
        WHEN "CHKN"
            PERFORM DO-CHKN
        WHEN "CHKB"
            PERFORM DO-CHKB
        WHEN "STAT"
            PERFORM DO-STAT
        WHEN OTHER
            MOVE "E0001" TO LS-OFAC-RESULT-CODE
            MOVE "Invalid OFAC function" TO LS-OFAC-RESULT-MSG
    END-EVALUATE
    GOBACK.

*> ---------------------------------------------------------------
*> CHKN - Check name against OFAC watch list
*> Exact match = score 100, no match = score 0
*> ---------------------------------------------------------------
DO-CHKN.
    MOVE "E0000" TO LS-OFAC-RESULT-CODE
    MOVE SPACES TO LS-OFAC-RESULT-MSG
    MOVE "N" TO OFAC-MATCH-FOUND
    MOVE 000 TO OFAC-MATCH-SCORE
    MOVE SPACES TO OFAC-MATCH-LIST
    MOVE FUNCTION CURRENT-DATE(1:8) TO OFAC-CHECK-DATE
    MOVE "P" TO OFAC-CHECK-STATUS

    *> Convert check name to upper for comparison
    MOVE FUNCTION UPPER-CASE(OFAC-CHECK-NAME)
        TO WS-CHECK-UPPER

    PERFORM VARYING WS-IDX FROM 1 BY 1
        UNTIL WS-IDX > WS-WATCH-COUNT
           OR OFAC-MATCH-FOUND = "Y"
        MOVE FUNCTION UPPER-CASE(WS-WATCH-NAME(WS-IDX))
            TO WS-NAME-UPPER
        IF WS-CHECK-UPPER = WS-NAME-UPPER
            MOVE "Y" TO OFAC-MATCH-FOUND
            MOVE 100 TO OFAC-MATCH-SCORE
            MOVE WS-WATCH-LST(WS-IDX) TO OFAC-MATCH-LIST
            MOVE "F" TO OFAC-CHECK-STATUS
            MOVE "E0025" TO LS-OFAC-RESULT-CODE
            MOVE "OFAC match found" TO LS-OFAC-RESULT-MSG
        END-IF
    END-PERFORM

    IF OFAC-MATCH-FOUND = "N"
        MOVE "OFAC check passed" TO LS-OFAC-RESULT-MSG
    END-IF.

*> ---------------------------------------------------------------
*> CHKB - Check beneficiary: name + country screening
*> Calls CHKN internally, then also checks country
*> ---------------------------------------------------------------
DO-CHKB.
    MOVE "E0000" TO LS-OFAC-RESULT-CODE
    MOVE SPACES TO LS-OFAC-RESULT-MSG
    MOVE "N" TO OFAC-MATCH-FOUND
    MOVE 000 TO OFAC-MATCH-SCORE
    MOVE SPACES TO OFAC-MATCH-LIST
    MOVE FUNCTION CURRENT-DATE(1:8) TO OFAC-CHECK-DATE
    MOVE "P" TO OFAC-CHECK-STATUS

    *> First check: name against watch list
    MOVE FUNCTION UPPER-CASE(OFAC-CHECK-NAME)
        TO WS-CHECK-UPPER

    PERFORM VARYING WS-IDX FROM 1 BY 1
        UNTIL WS-IDX > WS-WATCH-COUNT
           OR OFAC-MATCH-FOUND = "Y"
        MOVE FUNCTION UPPER-CASE(WS-WATCH-NAME(WS-IDX))
            TO WS-NAME-UPPER
        IF WS-CHECK-UPPER = WS-NAME-UPPER
            MOVE "Y" TO OFAC-MATCH-FOUND
            MOVE 100 TO OFAC-MATCH-SCORE
            MOVE WS-WATCH-LST(WS-IDX) TO OFAC-MATCH-LIST
            MOVE "F" TO OFAC-CHECK-STATUS
            MOVE "E0025" TO LS-OFAC-RESULT-CODE
            MOVE "OFAC match on beneficiary name"
                TO LS-OFAC-RESULT-MSG
        END-IF
    END-PERFORM

    *> Second check: country against sanctioned list
    IF OFAC-MATCH-FOUND = "N"
        MOVE "N" TO WS-COUNTRY-MATCH
        IF OFAC-CHECK-COUNTRY NOT = SPACES
            PERFORM VARYING WS-COUNTRY-IDX FROM 1 BY 1
                UNTIL WS-COUNTRY-IDX > WS-SANCT-COUNT
                IF OFAC-CHECK-COUNTRY =
                    WS-SANCT-COUNTRY(WS-COUNTRY-IDX)
                    MOVE "Y" TO WS-COUNTRY-MATCH
                END-IF
            END-PERFORM
        END-IF

        IF WS-COUNTRY-MATCH = "Y"
            MOVE "Y" TO OFAC-MATCH-FOUND
            MOVE 100 TO OFAC-MATCH-SCORE
            MOVE "SDN" TO OFAC-MATCH-LIST
            MOVE "F" TO OFAC-CHECK-STATUS
            MOVE "E0025" TO LS-OFAC-RESULT-CODE
            MOVE "OFAC match on beneficiary country"
                TO LS-OFAC-RESULT-MSG
        END-IF
    END-IF

    IF OFAC-MATCH-FOUND = "N"
        MOVE "OFAC beneficiary check passed"
            TO LS-OFAC-RESULT-MSG
    END-IF.

*> ---------------------------------------------------------------
*> STAT - Return status of last OFAC check
*> Checks if OFAC-CHECK-DATE is populated
*> ---------------------------------------------------------------
DO-STAT.
    MOVE "E0000" TO LS-OFAC-RESULT-CODE
    MOVE SPACES TO LS-OFAC-RESULT-MSG

    IF OFAC-CHECK-DATE = ZEROS OR OFAC-CHECK-DATE = SPACES
        MOVE "No prior OFAC check on record"
            TO LS-OFAC-RESULT-MSG
    ELSE
        MOVE "OFAC check on file" TO LS-OFAC-RESULT-MSG
    END-IF.

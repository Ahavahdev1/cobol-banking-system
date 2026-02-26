IDENTIFICATION DIVISION.
PROGRAM-ID. BSACTRO.
*> ================================================================
*> BSACTRO - BSA/AML Currency Transaction Report Generator
*> AGGR = Aggregate, CHEK = Check threshold, FILE = Generate CTR
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
COPY CPYCONST.

LINKAGE SECTION.
01  LS-FUNCTION                   PIC X(4).
COPY CPYCTR.
01  LS-TXN-INFO.
    05  LS-BSA-CUST-ID           PIC 9(10).
    05  LS-BSA-TXN-DATE          PIC 9(8).
    05  LS-BSA-CASH-AMOUNT       PIC S9(13)V99.
    05  LS-BSA-CASH-DIRECTION    PIC X(1).
    *> I = Cash-in, O = Cash-out
    05  LS-BSA-IS-CASH           PIC X(1).
    05  LS-BSA-ACCT-NUMBER       PIC 9(12).
01  LS-BSA-RESULT.
    05  LS-BSA-RESULT-CODE        PIC X(5).
    05  LS-BSA-RESULT-MSG         PIC X(50).
    05  LS-BSA-CTR-REQUIRED       PIC X(1).
    05  LS-BSA-CASH-IN-TOTAL      PIC S9(13)V99.
    05  LS-BSA-CASH-OUT-TOTAL     PIC S9(13)V99.

PROCEDURE DIVISION USING LS-FUNCTION
                         CTR-RECORD
                         LS-TXN-INFO
                         LS-BSA-RESULT.
    EVALUATE LS-FUNCTION
        WHEN "CHEK"
            PERFORM DO-CHEK
        WHEN "AGGR"
            PERFORM DO-AGGR
        WHEN OTHER
            MOVE "E0001" TO LS-BSA-RESULT-CODE
            MOVE "Invalid function" TO LS-BSA-RESULT-MSG
            MOVE "N" TO LS-BSA-CTR-REQUIRED
    END-EVALUATE
    GOBACK.

*> ---------------------------------------------------------------
*> CHEK - Check single transaction against CTR threshold
*> ---------------------------------------------------------------
DO-CHEK.
    MOVE "E0000" TO LS-BSA-RESULT-CODE
    MOVE SPACES TO LS-BSA-RESULT-MSG
    MOVE "N" TO LS-BSA-CTR-REQUIRED
    MOVE ZEROS TO LS-BSA-CASH-IN-TOTAL
    MOVE ZEROS TO LS-BSA-CASH-OUT-TOTAL

    IF CTR-EXEMPT-FLAG = "Y"
        MOVE "Customer exempt from CTR" TO LS-BSA-RESULT-MSG
        GOBACK
    END-IF

    IF LS-BSA-IS-CASH = "N"
        MOVE "Non-cash transaction" TO LS-BSA-RESULT-MSG
        GOBACK
    END-IF

    IF LS-BSA-CASH-AMOUNT >= WS-CTR-THRESHOLD
        MOVE "Y" TO LS-BSA-CTR-REQUIRED
        MOVE "CTR required" TO LS-BSA-RESULT-MSG
    END-IF
    *> Also check aggregated CTR-RECORD totals
    IF CTR-CASH-IN-TOTAL >= WS-CTR-THRESHOLD
        MOVE "Y" TO LS-BSA-CTR-REQUIRED
        MOVE "CTR required - aggregate" TO LS-BSA-RESULT-MSG
    END-IF
    IF CTR-CASH-OUT-TOTAL >= WS-CTR-THRESHOLD
        MOVE "Y" TO LS-BSA-CTR-REQUIRED
        MOVE "CTR required - aggregate" TO LS-BSA-RESULT-MSG
    END-IF
    IF LS-BSA-CTR-REQUIRED = "N"
        MOVE "CTR not required" TO LS-BSA-RESULT-MSG
    END-IF.

*> ---------------------------------------------------------------
*> AGGR - Aggregate cash per customer per day
*> ---------------------------------------------------------------
DO-AGGR.
    MOVE "E0000" TO LS-BSA-RESULT-CODE
    MOVE SPACES TO LS-BSA-RESULT-MSG
    MOVE "N" TO LS-BSA-CTR-REQUIRED

    IF LS-BSA-IS-CASH = "Y"
        IF LS-BSA-CASH-DIRECTION = "I"
            ADD LS-BSA-CASH-AMOUNT TO CTR-CASH-IN-TOTAL
        END-IF
        IF LS-BSA-CASH-DIRECTION = "O"
            ADD LS-BSA-CASH-AMOUNT TO CTR-CASH-OUT-TOTAL
        END-IF
    END-IF

    MOVE CTR-CASH-IN-TOTAL TO LS-BSA-CASH-IN-TOTAL
    MOVE CTR-CASH-OUT-TOTAL TO LS-BSA-CASH-OUT-TOTAL

    IF CTR-EXEMPT-FLAG = "Y"
        MOVE "Customer exempt from CTR" TO LS-BSA-RESULT-MSG
        GOBACK
    END-IF

    IF CTR-CASH-IN-TOTAL >= WS-CTR-THRESHOLD
        MOVE "Y" TO LS-BSA-CTR-REQUIRED
        MOVE "CTR required - cash in threshold" TO LS-BSA-RESULT-MSG
    ELSE IF CTR-CASH-OUT-TOTAL >= WS-CTR-THRESHOLD
        MOVE "Y" TO LS-BSA-CTR-REQUIRED
        MOVE "CTR required - cash out threshold" TO LS-BSA-RESULT-MSG
    ELSE
        MOVE "CTR not required" TO LS-BSA-RESULT-MSG
    END-IF.

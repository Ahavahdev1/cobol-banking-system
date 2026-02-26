IDENTIFICATION DIVISION.
PROGRAM-ID. TEST-GLPOST.
*> ================================================================
*> TEST-GLPOST - Test suite for GLPOST0 General Ledger posting
*> Tests: POST, TBAL, CRPT functions (19 tests)
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-TEST-COUNT          PIC 9(3) VALUE 0.
01  WS-PASS-COUNT          PIC 9(3) VALUE 0.
01  WS-FAIL-COUNT          PIC 9(3) VALUE 0.
01  WS-TEST-NAME           PIC X(40).

*> GLPOST0 LINKAGE replicated in working storage
01  WS-FUNCTION            PIC X(4).
01  WS-GL-ENTRY.
    05  WS-GLE-DR-ACCT     PIC 9(10).
    05  WS-GLE-CR-ACCT     PIC 9(10).
    05  WS-GLE-AMOUNT      PIC S9(15)V99.
    05  WS-GLE-DESCRIPTION PIC X(40).
    05  WS-GLE-POST-DATE   PIC 9(8).
COPY CPYGL REPLACING ==:PREFIX:== BY ==WS==.
01  WS-TRIAL-BAL.
    05  WS-TB-TOTAL-DEBITS   PIC S9(15)V99.
    05  WS-TB-TOTAL-CREDITS  PIC S9(15)V99.
    05  WS-TB-DIFFERENCE     PIC S9(15)V99.
    05  WS-TB-IS-BALANCED    PIC X(1).
01  WS-GL-RESULT.
    05  WS-GL-RESULT-CODE    PIC X(5).
    05  WS-GL-RESULT-MSG     PIC X(50).

*> Second GL record for credit-side in double-entry tests
01  WS-GL-RECORD-CR.
    05  WS-CR-KEY.
        10  WS-CR-ACCOUNT-NUM     PIC 9(10).
        10  WS-CR-COST-CENTER     PIC 9(4).
    05  WS-CR-CLASSIFICATION.
        10  WS-CR-ACCT-TYPE       PIC X(1).
        10  WS-CR-ACCT-SUBTYPE    PIC X(4).
        10  WS-CR-ACCT-NAME       PIC X(40).
        10  WS-CR-NORMAL-BALANCE  PIC X(1).
        10  WS-CR-CALL-RPT-LINE   PIC X(6).
    05  WS-CR-BALANCES.
        10  WS-CR-CURRENT-BAL     PIC S9(15)V99.
        10  WS-CR-MTD-DEBITS      PIC S9(15)V99.
        10  WS-CR-MTD-CREDITS     PIC S9(15)V99.
        10  WS-CR-YTD-DEBITS      PIC S9(15)V99.
        10  WS-CR-YTD-CREDITS     PIC S9(15)V99.
        10  WS-CR-PRIOR-MONTH-BAL PIC S9(15)V99.
        10  WS-CR-PRIOR-YEAR-BAL  PIC S9(15)V99.
        10  WS-CR-BUDGET-BAL      PIC S9(15)V99.
    05  WS-CR-CONTROL-FIELDS.
        10  WS-CR-STATUS          PIC X(1).
        10  WS-CR-AUTO-POST       PIC X(1).
        10  WS-CR-RECONCILE-TYPE  PIC X(1).
        10  WS-CR-LAST-POST-DATE  PIC 9(8).
        10  WS-CR-LAST-RECON-DATE PIC 9(8).

*> Saved values for accumulator tests
01  WS-SAVED-MTD-DR       PIC S9(15)V99.
01  WS-SAVED-MTD-CR       PIC S9(15)V99.
01  WS-SAVED-YTD-DR       PIC S9(15)V99.
01  WS-SAVED-YTD-CR       PIC S9(15)V99.

PROCEDURE DIVISION.
MAIN-PROGRAM.
    DISPLAY "========================================"
    DISPLAY "TEST SUITE: GLPOST0"
    DISPLAY "========================================"

    PERFORM TEST-GL-001
    PERFORM TEST-GL-002
    PERFORM TEST-GL-003
    PERFORM TEST-GL-004
    PERFORM TEST-GL-005
    PERFORM TEST-GL-006
    PERFORM TEST-GL-007
    PERFORM TEST-GL-008
    PERFORM TEST-GL-009
    PERFORM TEST-GL-010
    PERFORM TEST-GL-011
    PERFORM TEST-GL-012
    PERFORM TEST-GL-013
    PERFORM TEST-GL-014
    PERFORM TEST-GL-015
    PERFORM TEST-GL-016
    PERFORM TEST-GL-017
    PERFORM TEST-GL-018
    PERFORM TEST-GL-019

    DISPLAY "========================================"
    DISPLAY "RESULTS: " WS-PASS-COUNT "/" WS-TEST-COUNT
            " PASSED"
    DISPLAY "         " WS-FAIL-COUNT " FAILED"
    DISPLAY "========================================"
    MOVE WS-FAIL-COUNT TO RETURN-CODE
    STOP RUN.

*> ---------------------------------------------------------------
*> GL-001: POST balanced asset/liability entry ($500 DR/CR)
*> ---------------------------------------------------------------
TEST-GL-001.
    ADD 1 TO WS-TEST-COUNT
    MOVE "GL-001: POST balanced asset/liab" TO WS-TEST-NAME
    INITIALIZE WS-GL-ENTRY
    INITIALIZE GL-RECORD
    INITIALIZE WS-TRIAL-BAL
    INITIALIZE WS-GL-RESULT
    MOVE "POST" TO WS-FUNCTION
    MOVE 1010 TO WS-GLE-DR-ACCT
    MOVE 4010 TO WS-GLE-CR-ACCT
    MOVE 500.00 TO WS-GLE-AMOUNT
    MOVE "Cash deposit" TO WS-GLE-DESCRIPTION
    MOVE 20260226 TO WS-GLE-POST-DATE
    MOVE "A" TO GL-STATUS
    MOVE "A" TO GL-ACCT-TYPE
    MOVE "D" TO GL-NORMAL-BALANCE
    CALL "GLPOST0" USING WS-FUNCTION WS-GL-ENTRY
                         GL-RECORD WS-TRIAL-BAL WS-GL-RESULT
    IF WS-GL-RESULT-CODE = "E0000"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-GL-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> GL-002: POST balanced expense/liability ($100 DR/CR)
*> ---------------------------------------------------------------
TEST-GL-002.
    ADD 1 TO WS-TEST-COUNT
    MOVE "GL-002: POST balanced exp/liab" TO WS-TEST-NAME
    INITIALIZE WS-GL-ENTRY
    INITIALIZE GL-RECORD
    INITIALIZE WS-TRIAL-BAL
    INITIALIZE WS-GL-RESULT
    MOVE "POST" TO WS-FUNCTION
    MOVE 8010 TO WS-GLE-DR-ACCT
    MOVE 5500 TO WS-GLE-CR-ACCT
    MOVE 100.00 TO WS-GLE-AMOUNT
    MOVE "Interest expense" TO WS-GLE-DESCRIPTION
    MOVE 20260226 TO WS-GLE-POST-DATE
    MOVE "A" TO GL-STATUS
    MOVE "X" TO GL-ACCT-TYPE
    MOVE "D" TO GL-NORMAL-BALANCE
    CALL "GLPOST0" USING WS-FUNCTION WS-GL-ENTRY
                         GL-RECORD WS-TRIAL-BAL WS-GL-RESULT
    IF WS-GL-RESULT-CODE = "E0000"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-GL-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> GL-003: TBAL after balanced entries -> balanced=Y, diff=0
*> ---------------------------------------------------------------
TEST-GL-003.
    ADD 1 TO WS-TEST-COUNT
    MOVE "GL-003: TBAL balanced=Y diff=0" TO WS-TEST-NAME
    INITIALIZE WS-GL-ENTRY
    INITIALIZE GL-RECORD
    INITIALIZE WS-GL-RESULT
    MOVE "TBAL" TO WS-FUNCTION
    MOVE 500.00 TO WS-TB-TOTAL-DEBITS
    MOVE 500.00 TO WS-TB-TOTAL-CREDITS
    MOVE 0 TO WS-TB-DIFFERENCE
    MOVE "Y" TO WS-TB-IS-BALANCED
    CALL "GLPOST0" USING WS-FUNCTION WS-GL-ENTRY
                         GL-RECORD WS-TRIAL-BAL WS-GL-RESULT
    IF WS-GL-RESULT-CODE = "E0000"
        IF WS-TB-IS-BALANCED = "Y" AND WS-TB-DIFFERENCE = 0
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " balanced=" WS-TB-IS-BALANCED
                " diff=" WS-TB-DIFFERENCE
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-GL-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> GL-004: TBAL with unbalanced state -> balanced=N
*> ---------------------------------------------------------------
TEST-GL-004.
    ADD 1 TO WS-TEST-COUNT
    MOVE "GL-004: TBAL unbalanced=N" TO WS-TEST-NAME
    INITIALIZE WS-GL-ENTRY
    INITIALIZE GL-RECORD
    INITIALIZE WS-GL-RESULT
    MOVE "TBAL" TO WS-FUNCTION
    MOVE 600.00 TO WS-TB-TOTAL-DEBITS
    MOVE 500.00 TO WS-TB-TOTAL-CREDITS
    MOVE 100.00 TO WS-TB-DIFFERENCE
    MOVE "N" TO WS-TB-IS-BALANCED
    CALL "GLPOST0" USING WS-FUNCTION WS-GL-ENTRY
                         GL-RECORD WS-TRIAL-BAL WS-GL-RESULT
    IF WS-GL-RESULT-CODE = "E0000"
        IF WS-TB-IS-BALANCED = "N"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " balanced=" WS-TB-IS-BALANCED
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-GL-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> GL-005: POST updates GL-MTD-DEBITS accumulator
*> ---------------------------------------------------------------
TEST-GL-005.
    ADD 1 TO WS-TEST-COUNT
    MOVE "GL-005: POST updates MTD debits" TO WS-TEST-NAME
    INITIALIZE WS-GL-ENTRY
    INITIALIZE GL-RECORD
    INITIALIZE WS-TRIAL-BAL
    INITIALIZE WS-GL-RESULT
    MOVE "POST" TO WS-FUNCTION
    MOVE 1010 TO WS-GLE-DR-ACCT
    MOVE 4010 TO WS-GLE-CR-ACCT
    MOVE 250.00 TO WS-GLE-AMOUNT
    MOVE "Test MTD debit" TO WS-GLE-DESCRIPTION
    MOVE 20260226 TO WS-GLE-POST-DATE
    MOVE "A" TO GL-STATUS
    MOVE "A" TO GL-ACCT-TYPE
    MOVE "D" TO GL-NORMAL-BALANCE
    MOVE 100.00 TO GL-MTD-DEBITS
    MOVE WS-GLE-AMOUNT TO WS-SAVED-MTD-DR
    ADD GL-MTD-DEBITS TO WS-SAVED-MTD-DR
    CALL "GLPOST0" USING WS-FUNCTION WS-GL-ENTRY
                         GL-RECORD WS-TRIAL-BAL WS-GL-RESULT
    IF WS-GL-RESULT-CODE = "E0000"
        IF GL-MTD-DEBITS = WS-SAVED-MTD-DR
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=" WS-SAVED-MTD-DR
                " actual=" GL-MTD-DEBITS
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-GL-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> GL-006: POST updates GL-MTD-CREDITS accumulator
*> ---------------------------------------------------------------
TEST-GL-006.
    ADD 1 TO WS-TEST-COUNT
    MOVE "GL-006: POST updates MTD credits" TO WS-TEST-NAME
    INITIALIZE WS-GL-ENTRY
    INITIALIZE GL-RECORD
    INITIALIZE WS-TRIAL-BAL
    INITIALIZE WS-GL-RESULT
    MOVE "POST" TO WS-FUNCTION
    MOVE 1010 TO WS-GLE-DR-ACCT
    MOVE 4010 TO WS-GLE-CR-ACCT
    MOVE 300.00 TO WS-GLE-AMOUNT
    MOVE "Test MTD credit" TO WS-GLE-DESCRIPTION
    MOVE 20260226 TO WS-GLE-POST-DATE
    MOVE "A" TO GL-STATUS
    MOVE "L" TO GL-ACCT-TYPE
    MOVE "C" TO GL-NORMAL-BALANCE
    MOVE 200.00 TO GL-MTD-CREDITS
    MOVE WS-GLE-AMOUNT TO WS-SAVED-MTD-CR
    ADD GL-MTD-CREDITS TO WS-SAVED-MTD-CR
    CALL "GLPOST0" USING WS-FUNCTION WS-GL-ENTRY
                         GL-RECORD WS-TRIAL-BAL WS-GL-RESULT
    IF WS-GL-RESULT-CODE = "E0000"
        IF GL-MTD-CREDITS = WS-SAVED-MTD-CR
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=" WS-SAVED-MTD-CR
                " actual=" GL-MTD-CREDITS
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-GL-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> GL-007: POST updates GL-YTD-DEBITS accumulator
*> ---------------------------------------------------------------
TEST-GL-007.
    ADD 1 TO WS-TEST-COUNT
    MOVE "GL-007: POST updates YTD debits" TO WS-TEST-NAME
    INITIALIZE WS-GL-ENTRY
    INITIALIZE GL-RECORD
    INITIALIZE WS-TRIAL-BAL
    INITIALIZE WS-GL-RESULT
    MOVE "POST" TO WS-FUNCTION
    MOVE 1010 TO WS-GLE-DR-ACCT
    MOVE 4010 TO WS-GLE-CR-ACCT
    MOVE 400.00 TO WS-GLE-AMOUNT
    MOVE "Test YTD debit" TO WS-GLE-DESCRIPTION
    MOVE 20260226 TO WS-GLE-POST-DATE
    MOVE "A" TO GL-STATUS
    MOVE "A" TO GL-ACCT-TYPE
    MOVE "D" TO GL-NORMAL-BALANCE
    MOVE 1000.00 TO GL-YTD-DEBITS
    MOVE WS-GLE-AMOUNT TO WS-SAVED-YTD-DR
    ADD GL-YTD-DEBITS TO WS-SAVED-YTD-DR
    CALL "GLPOST0" USING WS-FUNCTION WS-GL-ENTRY
                         GL-RECORD WS-TRIAL-BAL WS-GL-RESULT
    IF WS-GL-RESULT-CODE = "E0000"
        IF GL-YTD-DEBITS = WS-SAVED-YTD-DR
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=" WS-SAVED-YTD-DR
                " actual=" GL-YTD-DEBITS
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-GL-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> GL-008: POST updates GL-YTD-CREDITS accumulator
*> ---------------------------------------------------------------
TEST-GL-008.
    ADD 1 TO WS-TEST-COUNT
    MOVE "GL-008: POST updates YTD credits" TO WS-TEST-NAME
    INITIALIZE WS-GL-ENTRY
    INITIALIZE GL-RECORD
    INITIALIZE WS-TRIAL-BAL
    INITIALIZE WS-GL-RESULT
    MOVE "POST" TO WS-FUNCTION
    MOVE 1010 TO WS-GLE-DR-ACCT
    MOVE 4010 TO WS-GLE-CR-ACCT
    MOVE 350.00 TO WS-GLE-AMOUNT
    MOVE "Test YTD credit" TO WS-GLE-DESCRIPTION
    MOVE 20260226 TO WS-GLE-POST-DATE
    MOVE "A" TO GL-STATUS
    MOVE "L" TO GL-ACCT-TYPE
    MOVE "C" TO GL-NORMAL-BALANCE
    MOVE 2000.00 TO GL-YTD-CREDITS
    MOVE WS-GLE-AMOUNT TO WS-SAVED-YTD-CR
    ADD GL-YTD-CREDITS TO WS-SAVED-YTD-CR
    CALL "GLPOST0" USING WS-FUNCTION WS-GL-ENTRY
                         GL-RECORD WS-TRIAL-BAL WS-GL-RESULT
    IF WS-GL-RESULT-CODE = "E0000"
        IF GL-YTD-CREDITS = WS-SAVED-YTD-CR
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=" WS-SAVED-YTD-CR
                " actual=" GL-YTD-CREDITS
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-GL-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> GL-009: POST to inactive GL account -> E0061
*> ---------------------------------------------------------------
TEST-GL-009.
    ADD 1 TO WS-TEST-COUNT
    MOVE "GL-009: POST inactive acct=E0061" TO WS-TEST-NAME
    INITIALIZE WS-GL-ENTRY
    INITIALIZE GL-RECORD
    INITIALIZE WS-TRIAL-BAL
    INITIALIZE WS-GL-RESULT
    MOVE "POST" TO WS-FUNCTION
    MOVE 1010 TO WS-GLE-DR-ACCT
    MOVE 4010 TO WS-GLE-CR-ACCT
    MOVE 100.00 TO WS-GLE-AMOUNT
    MOVE "Inactive test" TO WS-GLE-DESCRIPTION
    MOVE 20260226 TO WS-GLE-POST-DATE
    MOVE "I" TO GL-STATUS
    CALL "GLPOST0" USING WS-FUNCTION WS-GL-ENTRY
                         GL-RECORD WS-TRIAL-BAL WS-GL-RESULT
    IF WS-GL-RESULT-CODE = "E0061"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0061 actual=" WS-GL-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> GL-010: POST to frozen GL account -> E0062
*> ---------------------------------------------------------------
TEST-GL-010.
    ADD 1 TO WS-TEST-COUNT
    MOVE "GL-010: POST frozen acct=E0062" TO WS-TEST-NAME
    INITIALIZE WS-GL-ENTRY
    INITIALIZE GL-RECORD
    INITIALIZE WS-TRIAL-BAL
    INITIALIZE WS-GL-RESULT
    MOVE "POST" TO WS-FUNCTION
    MOVE 1010 TO WS-GLE-DR-ACCT
    MOVE 4010 TO WS-GLE-CR-ACCT
    MOVE 100.00 TO WS-GLE-AMOUNT
    MOVE "Frozen test" TO WS-GLE-DESCRIPTION
    MOVE 20260226 TO WS-GLE-POST-DATE
    MOVE "F" TO GL-STATUS
    CALL "GLPOST0" USING WS-FUNCTION WS-GL-ENTRY
                         GL-RECORD WS-TRIAL-BAL WS-GL-RESULT
    IF WS-GL-RESULT-CODE = "E0062"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0062 actual=" WS-GL-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> GL-011: POST with zero amount -> E0063
*> ---------------------------------------------------------------
TEST-GL-011.
    ADD 1 TO WS-TEST-COUNT
    MOVE "GL-011: POST zero amount=E0063" TO WS-TEST-NAME
    INITIALIZE WS-GL-ENTRY
    INITIALIZE GL-RECORD
    INITIALIZE WS-TRIAL-BAL
    INITIALIZE WS-GL-RESULT
    MOVE "POST" TO WS-FUNCTION
    MOVE 1010 TO WS-GLE-DR-ACCT
    MOVE 4010 TO WS-GLE-CR-ACCT
    MOVE 0 TO WS-GLE-AMOUNT
    MOVE "Zero amount test" TO WS-GLE-DESCRIPTION
    MOVE 20260226 TO WS-GLE-POST-DATE
    MOVE "A" TO GL-STATUS
    CALL "GLPOST0" USING WS-FUNCTION WS-GL-ENTRY
                         GL-RECORD WS-TRIAL-BAL WS-GL-RESULT
    IF WS-GL-RESULT-CODE = "E0063"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0063 actual=" WS-GL-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> GL-012: POST income account credits increase balance
*> ---------------------------------------------------------------
TEST-GL-012.
    ADD 1 TO WS-TEST-COUNT
    MOVE "GL-012: POST income CR increases" TO WS-TEST-NAME
    INITIALIZE WS-GL-ENTRY
    INITIALIZE GL-RECORD
    INITIALIZE WS-TRIAL-BAL
    INITIALIZE WS-GL-RESULT
    MOVE "POST" TO WS-FUNCTION
    MOVE 1010 TO WS-GLE-DR-ACCT
    MOVE 4010 TO WS-GLE-CR-ACCT
    MOVE 750.00 TO WS-GLE-AMOUNT
    MOVE "Income credit test" TO WS-GLE-DESCRIPTION
    MOVE 20260226 TO WS-GLE-POST-DATE
    MOVE "A" TO GL-STATUS
    MOVE "I" TO GL-ACCT-TYPE
    MOVE "C" TO GL-NORMAL-BALANCE
    MOVE 1000.00 TO GL-CURRENT-BAL
    CALL "GLPOST0" USING WS-FUNCTION WS-GL-ENTRY
                         GL-RECORD WS-TRIAL-BAL WS-GL-RESULT
    IF WS-GL-RESULT-CODE = "E0000"
        IF GL-CURRENT-BAL >= 1750.00
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " balance=" GL-CURRENT-BAL
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-GL-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> GL-013: POST contra acct (liability) debit decreases
*> ---------------------------------------------------------------
TEST-GL-013.
    ADD 1 TO WS-TEST-COUNT
    MOVE "GL-013: POST contra DR decreases" TO WS-TEST-NAME
    INITIALIZE WS-GL-ENTRY
    INITIALIZE GL-RECORD
    INITIALIZE WS-TRIAL-BAL
    INITIALIZE WS-GL-RESULT
    MOVE "POST" TO WS-FUNCTION
    MOVE 4010 TO WS-GLE-DR-ACCT
    MOVE 1010 TO WS-GLE-CR-ACCT
    MOVE 200.00 TO WS-GLE-AMOUNT
    MOVE "Contra debit test" TO WS-GLE-DESCRIPTION
    MOVE 20260226 TO WS-GLE-POST-DATE
    MOVE "A" TO GL-STATUS
    MOVE "L" TO GL-ACCT-TYPE
    MOVE "C" TO GL-NORMAL-BALANCE
    MOVE 1000.00 TO GL-CURRENT-BAL
    CALL "GLPOST0" USING WS-FUNCTION WS-GL-ENTRY
                         GL-RECORD WS-TRIAL-BAL WS-GL-RESULT
    IF WS-GL-RESULT-CODE = "E0000"
        IF GL-CURRENT-BAL <= 800.00
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " balance=" GL-CURRENT-BAL
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-GL-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> GL-014: Double-entry POST both sides, TBAL confirms balance
*> Demonstrates correct two-call pattern: POST debit account,
*> then POST credit account, then TBAL to verify balance.
*> ---------------------------------------------------------------
TEST-GL-014.
    ADD 1 TO WS-TEST-COUNT
    MOVE "GL-014: Double-entry POST + TBAL" TO WS-TEST-NAME

    *> --- Step 1: POST debit side (Asset account, debit-normal) ---
    INITIALIZE WS-GL-ENTRY
    INITIALIZE GL-RECORD
    INITIALIZE WS-TRIAL-BAL
    INITIALIZE WS-GL-RESULT
    MOVE "POST" TO WS-FUNCTION
    MOVE 1010 TO WS-GLE-DR-ACCT
    MOVE 2010 TO WS-GLE-CR-ACCT
    MOVE 1000.00 TO WS-GLE-AMOUNT
    MOVE "Double-entry DR side" TO WS-GLE-DESCRIPTION
    MOVE 20260226 TO WS-GLE-POST-DATE
    *> Set up GL-RECORD as debit-normal asset account
    MOVE 1010 TO GL-ACCOUNT-NUM
    MOVE 1000 TO GL-COST-CENTER
    MOVE "A" TO GL-ACCT-TYPE
    MOVE "D" TO GL-NORMAL-BALANCE
    MOVE "A" TO GL-STATUS
    CALL "GLPOST0" USING WS-FUNCTION WS-GL-ENTRY
                         GL-RECORD WS-TRIAL-BAL WS-GL-RESULT
    IF WS-GL-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " DR POST result=" WS-GL-RESULT-CODE
        EXIT PARAGRAPH
    END-IF
    *> Save debit-side MTD-DEBITS for trial balance
    MOVE GL-MTD-DEBITS TO WS-SAVED-MTD-DR

    *> --- Step 2: POST credit side (Liability account, cr-normal) -
    *> Copy credit-side record into WS-GL-RECORD-CR for reference,
    *> then load GL-RECORD with credit account data for the call.
    INITIALIZE WS-GL-ENTRY
    INITIALIZE GL-RECORD
    INITIALIZE WS-GL-RESULT
    MOVE "POST" TO WS-FUNCTION
    MOVE 1010 TO WS-GLE-DR-ACCT
    MOVE 2010 TO WS-GLE-CR-ACCT
    MOVE 1000.00 TO WS-GLE-AMOUNT
    MOVE "Double-entry CR side" TO WS-GLE-DESCRIPTION
    MOVE 20260226 TO WS-GLE-POST-DATE
    *> Set up GL-RECORD as credit-normal liability account
    MOVE 2010 TO GL-ACCOUNT-NUM
    MOVE 1000 TO GL-COST-CENTER
    MOVE "L" TO GL-ACCT-TYPE
    MOVE "C" TO GL-NORMAL-BALANCE
    MOVE "A" TO GL-STATUS
    CALL "GLPOST0" USING WS-FUNCTION WS-GL-ENTRY
                         GL-RECORD WS-TRIAL-BAL WS-GL-RESULT
    IF WS-GL-RESULT-CODE NOT = "E0000"
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " CR POST result=" WS-GL-RESULT-CODE
        EXIT PARAGRAPH
    END-IF
    *> Save credit-side MTD-CREDITS for trial balance
    MOVE GL-MTD-CREDITS TO WS-SAVED-MTD-CR

    *> --- Step 3: TBAL with both sides -> balanced ---
    INITIALIZE WS-GL-RESULT
    MOVE "TBAL" TO WS-FUNCTION
    MOVE WS-SAVED-MTD-DR TO WS-TB-TOTAL-DEBITS
    MOVE WS-SAVED-MTD-CR TO WS-TB-TOTAL-CREDITS
    MOVE ZERO TO WS-TB-DIFFERENCE
    MOVE SPACES TO WS-TB-IS-BALANCED
    CALL "GLPOST0" USING WS-FUNCTION WS-GL-ENTRY
                         GL-RECORD WS-TRIAL-BAL WS-GL-RESULT
    IF WS-GL-RESULT-CODE = "E0000"
        IF WS-TB-IS-BALANCED = "Y"
            AND WS-TB-DIFFERENCE = ZERO
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " balanced=" WS-TB-IS-BALANCED
                " diff=" WS-TB-DIFFERENCE
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " TBAL result=" WS-GL-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> GL-015: POST to checking deposit (4010) sets CALL-RPT-LINE=2210
*> ---------------------------------------------------------------
TEST-GL-015.
    ADD 1 TO WS-TEST-COUNT
    MOVE "GL-015: POST checking RPT=2210" TO WS-TEST-NAME
    INITIALIZE WS-GL-ENTRY
    INITIALIZE GL-RECORD
    INITIALIZE WS-TRIAL-BAL
    INITIALIZE WS-GL-RESULT
    MOVE "POST" TO WS-FUNCTION
    MOVE 1010 TO WS-GLE-DR-ACCT
    MOVE 4010 TO WS-GLE-CR-ACCT
    MOVE 500.00 TO WS-GLE-AMOUNT
    MOVE "Checking deposit" TO WS-GLE-DESCRIPTION
    MOVE 20260226 TO WS-GLE-POST-DATE
    MOVE 4010 TO GL-ACCOUNT-NUM
    MOVE 1000 TO GL-COST-CENTER
    MOVE "A" TO GL-STATUS
    MOVE "L" TO GL-ACCT-TYPE
    MOVE "C" TO GL-NORMAL-BALANCE
    CALL "GLPOST0" USING WS-FUNCTION WS-GL-ENTRY
                         GL-RECORD WS-TRIAL-BAL WS-GL-RESULT
    IF WS-GL-RESULT-CODE = "E0000"
        IF GL-CALL-RPT-LINE = "2210  "
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=2210 actual=" GL-CALL-RPT-LINE
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-GL-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> GL-016: POST to savings account (4030) sets CALL-RPT-LINE=2213
*> ---------------------------------------------------------------
TEST-GL-016.
    ADD 1 TO WS-TEST-COUNT
    MOVE "GL-016: POST savings RPT=2213" TO WS-TEST-NAME
    INITIALIZE WS-GL-ENTRY
    INITIALIZE GL-RECORD
    INITIALIZE WS-TRIAL-BAL
    INITIALIZE WS-GL-RESULT
    MOVE "POST" TO WS-FUNCTION
    MOVE 1010 TO WS-GLE-DR-ACCT
    MOVE 4030 TO WS-GLE-CR-ACCT
    MOVE 1000.00 TO WS-GLE-AMOUNT
    MOVE "Savings deposit" TO WS-GLE-DESCRIPTION
    MOVE 20260226 TO WS-GLE-POST-DATE
    MOVE 4030 TO GL-ACCOUNT-NUM
    MOVE 1000 TO GL-COST-CENTER
    MOVE "A" TO GL-STATUS
    MOVE "L" TO GL-ACCT-TYPE
    MOVE "C" TO GL-NORMAL-BALANCE
    CALL "GLPOST0" USING WS-FUNCTION WS-GL-ENTRY
                         GL-RECORD WS-TRIAL-BAL WS-GL-RESULT
    IF WS-GL-RESULT-CODE = "E0000"
        IF GL-CALL-RPT-LINE = "2213  "
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=2213 actual=" GL-CALL-RPT-LINE
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-GL-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> GL-017: CRPT maps cash GL (1010) to Call Report line 1110
*> ---------------------------------------------------------------
TEST-GL-017.
    ADD 1 TO WS-TEST-COUNT
    MOVE "GL-017: CRPT cash 1010=line 1110" TO WS-TEST-NAME
    INITIALIZE WS-GL-ENTRY
    INITIALIZE GL-RECORD
    INITIALIZE WS-TRIAL-BAL
    INITIALIZE WS-GL-RESULT
    MOVE "CRPT" TO WS-FUNCTION
    MOVE 1010 TO GL-ACCOUNT-NUM
    CALL "GLPOST0" USING WS-FUNCTION WS-GL-ENTRY
                         GL-RECORD WS-TRIAL-BAL WS-GL-RESULT
    IF WS-GL-RESULT-CODE = "E0000"
        IF GL-CALL-RPT-LINE = "1110  "
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=1110 actual=" GL-CALL-RPT-LINE
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-GL-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> GL-018: POST with zero amount to active GL -> E0063
*> ---------------------------------------------------------------
TEST-GL-018.
    ADD 1 TO WS-TEST-COUNT
    MOVE "GL-018: POST zero amt active=E0063" TO WS-TEST-NAME
    INITIALIZE WS-GL-ENTRY
    INITIALIZE GL-RECORD
    INITIALIZE WS-TRIAL-BAL
    INITIALIZE WS-GL-RESULT
    MOVE "POST" TO WS-FUNCTION
    MOVE 1010 TO WS-GLE-DR-ACCT
    MOVE 4010 TO WS-GLE-CR-ACCT
    MOVE 0 TO WS-GLE-AMOUNT
    MOVE "Zero amount active" TO WS-GLE-DESCRIPTION
    MOVE 20260226 TO WS-GLE-POST-DATE
    MOVE "A" TO GL-STATUS
    MOVE "A" TO GL-ACCT-TYPE
    MOVE "D" TO GL-NORMAL-BALANCE
    MOVE 1000.00 TO GL-CURRENT-BAL
    CALL "GLPOST0" USING WS-FUNCTION WS-GL-ENTRY
                         GL-RECORD WS-TRIAL-BAL WS-GL-RESULT
    IF WS-GL-RESULT-CODE = "E0063"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0063 actual=" WS-GL-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> GL-019: Invalid function -> E0001
*> ---------------------------------------------------------------
TEST-GL-019.
    ADD 1 TO WS-TEST-COUNT
    MOVE "GL-019: Invalid function=E0001" TO WS-TEST-NAME
    INITIALIZE WS-GL-ENTRY
    INITIALIZE GL-RECORD
    INITIALIZE WS-TRIAL-BAL
    INITIALIZE WS-GL-RESULT
    MOVE "XXXX" TO WS-FUNCTION
    CALL "GLPOST0" USING WS-FUNCTION WS-GL-ENTRY
                         GL-RECORD WS-TRIAL-BAL WS-GL-RESULT
    IF WS-GL-RESULT-CODE = "E0001"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0001 actual=" WS-GL-RESULT-CODE
    END-IF.

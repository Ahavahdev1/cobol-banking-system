IDENTIFICATION DIVISION.
PROGRAM-ID. TEST-TXNPOST.
*> ================================================================
*> TEST-TXNPOST - Test suite for TXNPOST0 transaction posting
*> Tests: Deposits, withdrawals, GL mappings, compliance (27 tests)
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-TEST-COUNT          PIC 9(3) VALUE 0.
01  WS-PASS-COUNT          PIC 9(3) VALUE 0.
01  WS-FAIL-COUNT          PIC 9(3) VALUE 0.
01  WS-TEST-NAME           PIC X(40).
01  WS-EXPECTED-BAL        PIC S9(13)V99.

*> TXNPOST0 LINKAGE replicated in working storage
COPY CPYTXN.
COPY CPYACCT.
01  WS-GL-ENTRIES.
    05  WS-GL-DR-ACCOUNT   PIC 9(10).
    05  WS-GL-CR-ACCOUNT   PIC 9(10).
    05  WS-GL-AMOUNT       PIC S9(13)V99.
    05  WS-GL-POST-FLAG    PIC X(1).
01  WS-TXN-RESULT.
    05  WS-TXN-RESULT-CODE PIC X(5).
    05  WS-TXN-RESULT-MSG  PIC X(50).

PROCEDURE DIVISION.
MAIN-PROGRAM.
    DISPLAY "========================================"
    DISPLAY "TEST SUITE: TXNPOST0"
    DISPLAY "========================================"

    PERFORM TEST-TP-001
    PERFORM TEST-TP-002
    PERFORM TEST-TP-003
    PERFORM TEST-TP-004
    PERFORM TEST-TP-005
    PERFORM TEST-TP-006
    PERFORM TEST-TP-007
    PERFORM TEST-TP-008
    PERFORM TEST-TP-009
    PERFORM TEST-TP-010
    PERFORM TEST-TP-011
    PERFORM TEST-TP-012
    PERFORM TEST-TP-013
    PERFORM TEST-TP-014
    PERFORM TEST-TP-015
    PERFORM TEST-TP-016
    PERFORM TEST-TP-017
    PERFORM TEST-TP-018
    PERFORM TEST-TP-019
    PERFORM TEST-TP-020
    PERFORM TEST-TP-021
    PERFORM TEST-TP-022
    PERFORM TEST-TP-023
    PERFORM TEST-TP-024
    PERFORM TEST-TP-025
    PERFORM TEST-TP-026
    PERFORM TEST-TP-027

    DISPLAY "========================================"
    DISPLAY "RESULTS: " WS-PASS-COUNT "/" WS-TEST-COUNT
            " PASSED"
    DISPLAY "         " WS-FAIL-COUNT " FAILED"
    DISPLAY "========================================"
    MOVE WS-FAIL-COUNT TO RETURN-CODE
    STOP RUN.

*> ---------------------------------------------------------------
*> Helper: Set up active checking account with balance
*> ---------------------------------------------------------------
SETUP-ACTIVE-CHECKING.
    INITIALIZE ACCT-RECORD
    MOVE 000012345678 TO ACCT-NUMBER
    MOVE 2 TO ACCT-CHECK-DIGIT
    MOVE 1000000001 TO ACCT-PRIMARY-CIF
    MOVE "DDA1" TO ACCT-PRODUCT-CODE
    MOVE "D" TO ACCT-TYPE
    MOVE "CH" TO ACCT-SUB-TYPE
    MOVE 1000.00 TO ACCT-LEDGER-BAL
    MOVE 1000.00 TO ACCT-AVAIL-BAL
    MOVE 0 TO ACCT-HOLD-AMOUNT
    MOVE "A" TO ACCT-STATUS
    MOVE "N" TO ACCT-LEGAL-HOLD
    MOVE "N" TO ACCT-DECEASED
    MOVE "N" TO ACCT-GARNISHMENT
    MOVE 20260101 TO ACCT-OPEN-DATE.

*> ---------------------------------------------------------------
*> Helper: Set up a basic deposit transaction
*> ---------------------------------------------------------------
SETUP-DEPOSIT-TXN.
    INITIALIZE TXN-RECORD
    MOVE 000000000000001 TO TXN-ID
    MOVE 00001 TO TXN-SEQUENCE
    MOVE 000012345678 TO TXN-ACCT-NUMBER
    MOVE 2 TO TXN-ACCT-CHECK-DIGIT
    MOVE 0001 TO TXN-BRANCH-ID
    MOVE "TELLER01" TO TXN-TELLER-ID
    MOVE "BR" TO TXN-CHANNEL
    MOVE "DEP" TO TXN-TYPE
    MOVE "C" TO TXN-DR-CR
    MOVE 20260226 TO TXN-POST-DATE
    MOVE 120000 TO TXN-POST-TIME
    MOVE 20260226 TO TXN-EFFECTIVE-DATE.

*> ---------------------------------------------------------------
*> Helper: Set up a basic withdrawal transaction
*> ---------------------------------------------------------------
SETUP-WITHDRAWAL-TXN.
    INITIALIZE TXN-RECORD
    MOVE 000000000000002 TO TXN-ID
    MOVE 00001 TO TXN-SEQUENCE
    MOVE 000012345678 TO TXN-ACCT-NUMBER
    MOVE 2 TO TXN-ACCT-CHECK-DIGIT
    MOVE 0001 TO TXN-BRANCH-ID
    MOVE "TELLER01" TO TXN-TELLER-ID
    MOVE "BR" TO TXN-CHANNEL
    MOVE "WDL" TO TXN-TYPE
    MOVE "D" TO TXN-DR-CR
    MOVE 20260226 TO TXN-POST-DATE
    MOVE 120000 TO TXN-POST-TIME
    MOVE 20260226 TO TXN-EFFECTIVE-DATE.

*> ---------------------------------------------------------------
*> TP-001: Cash deposit $500 to active checking -> E0000
*> ---------------------------------------------------------------
TEST-TP-001.
    ADD 1 TO WS-TEST-COUNT
    MOVE "TP-001: Deposit $500 checking" TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-CHECKING
    PERFORM SETUP-DEPOSIT-TXN
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE 500.00 TO TXN-AMOUNT
    MOVE 500.00 TO TXN-CASH-AMOUNT
    COMPUTE WS-EXPECTED-BAL = ACCT-LEDGER-BAL + 500.00
    CALL "TXNPOST0" USING TXN-RECORD ACCT-RECORD
                          WS-GL-ENTRIES WS-TXN-RESULT
    IF WS-TXN-RESULT-CODE = "E0000"
        IF ACCT-LEDGER-BAL = WS-EXPECTED-BAL
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " bal=" ACCT-LEDGER-BAL
                " exp=" WS-EXPECTED-BAL
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-TXN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> TP-002: Cash withdrawal $200 from checking (bal $1000)
*> ---------------------------------------------------------------
TEST-TP-002.
    ADD 1 TO WS-TEST-COUNT
    MOVE "TP-002: Withdraw $200 checking" TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-CHECKING
    PERFORM SETUP-WITHDRAWAL-TXN
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE 200.00 TO TXN-AMOUNT
    MOVE 200.00 TO TXN-CASH-AMOUNT
    COMPUTE WS-EXPECTED-BAL = ACCT-LEDGER-BAL - 200.00
    CALL "TXNPOST0" USING TXN-RECORD ACCT-RECORD
                          WS-GL-ENTRIES WS-TXN-RESULT
    IF WS-TXN-RESULT-CODE = "E0000"
        IF ACCT-LEDGER-BAL = WS-EXPECTED-BAL
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " bal=" ACCT-LEDGER-BAL
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-TXN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> TP-003: Withdrawal exceeding available balance -> E0030
*> ---------------------------------------------------------------
TEST-TP-003.
    ADD 1 TO WS-TEST-COUNT
    MOVE "TP-003: Overdraw=E0030" TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-CHECKING
    PERFORM SETUP-WITHDRAWAL-TXN
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE 2000.00 TO TXN-AMOUNT
    MOVE 2000.00 TO TXN-CASH-AMOUNT
    CALL "TXNPOST0" USING TXN-RECORD ACCT-RECORD
                          WS-GL-ENTRIES WS-TXN-RESULT
    IF WS-TXN-RESULT-CODE = "E0030"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0030 actual=" WS-TXN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> TP-004: Post to frozen account -> E0033
*> ---------------------------------------------------------------
TEST-TP-004.
    ADD 1 TO WS-TEST-COUNT
    MOVE "TP-004: Frozen acct=E0033" TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-CHECKING
    PERFORM SETUP-DEPOSIT-TXN
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE "F" TO ACCT-STATUS
    MOVE 500.00 TO TXN-AMOUNT
    CALL "TXNPOST0" USING TXN-RECORD ACCT-RECORD
                          WS-GL-ENTRIES WS-TXN-RESULT
    IF WS-TXN-RESULT-CODE = "E0033"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0033 actual=" WS-TXN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> TP-005: Post to closed account -> E0034
*> ---------------------------------------------------------------
TEST-TP-005.
    ADD 1 TO WS-TEST-COUNT
    MOVE "TP-005: Closed acct=E0034" TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-CHECKING
    PERFORM SETUP-DEPOSIT-TXN
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE "C" TO ACCT-STATUS
    MOVE 500.00 TO TXN-AMOUNT
    CALL "TXNPOST0" USING TXN-RECORD ACCT-RECORD
                          WS-GL-ENTRIES WS-TXN-RESULT
    IF WS-TXN-RESULT-CODE = "E0034"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0034 actual=" WS-TXN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> TP-006: Post to account with legal hold -> E0035
*> ---------------------------------------------------------------
TEST-TP-006.
    ADD 1 TO WS-TEST-COUNT
    MOVE "TP-006: Legal hold=E0035" TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-CHECKING
    PERFORM SETUP-WITHDRAWAL-TXN
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE "Y" TO ACCT-LEGAL-HOLD
    MOVE 200.00 TO TXN-AMOUNT
    CALL "TXNPOST0" USING TXN-RECORD ACCT-RECORD
                          WS-GL-ENTRIES WS-TXN-RESULT
    IF WS-TXN-RESULT-CODE = "E0035"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0035 actual=" WS-TXN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> TP-007: Post to deceased account -> E0036
*> ---------------------------------------------------------------
TEST-TP-007.
    ADD 1 TO WS-TEST-COUNT
    MOVE "TP-007: Deceased acct=E0036" TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-CHECKING
    PERFORM SETUP-DEPOSIT-TXN
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE "Y" TO ACCT-DECEASED
    MOVE 500.00 TO TXN-AMOUNT
    CALL "TXNPOST0" USING TXN-RECORD ACCT-RECORD
                          WS-GL-ENTRIES WS-TXN-RESULT
    IF WS-TXN-RESULT-CODE = "E0036"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0036 actual=" WS-TXN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> TP-008: Zero amount transaction -> E0031
*> ---------------------------------------------------------------
TEST-TP-008.
    ADD 1 TO WS-TEST-COUNT
    MOVE "TP-008: Zero amount=E0031" TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-CHECKING
    PERFORM SETUP-DEPOSIT-TXN
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE 0 TO TXN-AMOUNT
    CALL "TXNPOST0" USING TXN-RECORD ACCT-RECORD
                          WS-GL-ENTRIES WS-TXN-RESULT
    IF WS-TXN-RESULT-CODE = "E0031"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0031 actual=" WS-TXN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> TP-009: Negative amount transaction -> E0032
*> ---------------------------------------------------------------
TEST-TP-009.
    ADD 1 TO WS-TEST-COUNT
    MOVE "TP-009: Negative amount=E0032" TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-CHECKING
    PERFORM SETUP-DEPOSIT-TXN
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE -100.00 TO TXN-AMOUNT
    CALL "TXNPOST0" USING TXN-RECORD ACCT-RECORD
                          WS-GL-ENTRIES WS-TXN-RESULT
    IF WS-TXN-RESULT-CODE = "E0032"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0032 actual=" WS-TXN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> TP-010: Balance snapshot - BAL-BEFORE and BAL-AFTER
*> ---------------------------------------------------------------
TEST-TP-010.
    ADD 1 TO WS-TEST-COUNT
    MOVE "TP-010: Balance snapshot" TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-CHECKING
    PERFORM SETUP-DEPOSIT-TXN
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE 500.00 TO TXN-AMOUNT
    MOVE 500.00 TO TXN-CASH-AMOUNT
    CALL "TXNPOST0" USING TXN-RECORD ACCT-RECORD
                          WS-GL-ENTRIES WS-TXN-RESULT
    IF WS-TXN-RESULT-CODE = "E0000"
        IF TXN-BAL-BEFORE = 1000.00
            AND TXN-BAL-AFTER = 1500.00
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " before=" TXN-BAL-BEFORE
                " after=" TXN-BAL-AFTER
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-TXN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> TP-011: Available balance snapshot captured
*> ---------------------------------------------------------------
TEST-TP-011.
    ADD 1 TO WS-TEST-COUNT
    MOVE "TP-011: Avail balance snapshot" TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-CHECKING
    PERFORM SETUP-DEPOSIT-TXN
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE 500.00 TO TXN-AMOUNT
    MOVE 500.00 TO TXN-CASH-AMOUNT
    CALL "TXNPOST0" USING TXN-RECORD ACCT-RECORD
                          WS-GL-ENTRIES WS-TXN-RESULT
    IF WS-TXN-RESULT-CODE = "E0000"
        IF TXN-AVAIL-BEFORE = 1000.00
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " avail-before=" TXN-AVAIL-BEFORE
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-TXN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> TP-012: Cash deposit GL mapping: DR 1010, CR 4010
*> ---------------------------------------------------------------
TEST-TP-012.
    ADD 1 TO WS-TEST-COUNT
    MOVE "TP-012: Deposit GL DR1010 CR4010" TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-CHECKING
    PERFORM SETUP-DEPOSIT-TXN
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE 500.00 TO TXN-AMOUNT
    MOVE 500.00 TO TXN-CASH-AMOUNT
    CALL "TXNPOST0" USING TXN-RECORD ACCT-RECORD
                          WS-GL-ENTRIES WS-TXN-RESULT
    IF WS-TXN-RESULT-CODE = "E0000"
        IF WS-GL-DR-ACCOUNT = 1010
            AND WS-GL-CR-ACCOUNT = 4010
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " DR=" WS-GL-DR-ACCOUNT
                " CR=" WS-GL-CR-ACCOUNT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-TXN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> TP-013: Cash withdrawal GL: DR 4010, CR 1010
*> ---------------------------------------------------------------
TEST-TP-013.
    ADD 1 TO WS-TEST-COUNT
    MOVE "TP-013: Withdraw GL DR4010 CR1010" TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-CHECKING
    PERFORM SETUP-WITHDRAWAL-TXN
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE 200.00 TO TXN-AMOUNT
    MOVE 200.00 TO TXN-CASH-AMOUNT
    CALL "TXNPOST0" USING TXN-RECORD ACCT-RECORD
                          WS-GL-ENTRIES WS-TXN-RESULT
    IF WS-TXN-RESULT-CODE = "E0000"
        IF WS-GL-DR-ACCOUNT = 4010
            AND WS-GL-CR-ACCOUNT = 1010
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " DR=" WS-GL-DR-ACCOUNT
                " CR=" WS-GL-CR-ACCOUNT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-TXN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> TP-014: Savings deposit GL: DR 1010, CR 4030
*> ---------------------------------------------------------------
TEST-TP-014.
    ADD 1 TO WS-TEST-COUNT
    MOVE "TP-014: Savings dep GL DR1010" TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-CHECKING
    MOVE "SAV1" TO ACCT-PRODUCT-CODE
    MOVE "SV" TO ACCT-SUB-TYPE
    PERFORM SETUP-DEPOSIT-TXN
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE 300.00 TO TXN-AMOUNT
    MOVE 300.00 TO TXN-CASH-AMOUNT
    CALL "TXNPOST0" USING TXN-RECORD ACCT-RECORD
                          WS-GL-ENTRIES WS-TXN-RESULT
    IF WS-TXN-RESULT-CODE = "E0000"
        IF WS-GL-DR-ACCOUNT = 1010
            AND WS-GL-CR-ACCOUNT = 4030
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " DR=" WS-GL-DR-ACCOUNT
                " CR=" WS-GL-CR-ACCOUNT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-TXN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> TP-015: MMA deposit GL: DR 1010, CR 4040
*> ---------------------------------------------------------------
TEST-TP-015.
    ADD 1 TO WS-TEST-COUNT
    MOVE "TP-015: MMA dep GL DR1010 CR4040" TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-CHECKING
    MOVE "MMA1" TO ACCT-PRODUCT-CODE
    MOVE "MM" TO ACCT-SUB-TYPE
    PERFORM SETUP-DEPOSIT-TXN
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE 400.00 TO TXN-AMOUNT
    MOVE 400.00 TO TXN-CASH-AMOUNT
    CALL "TXNPOST0" USING TXN-RECORD ACCT-RECORD
                          WS-GL-ENTRIES WS-TXN-RESULT
    IF WS-TXN-RESULT-CODE = "E0000"
        IF WS-GL-DR-ACCOUNT = 1010
            AND WS-GL-CR-ACCOUNT = 4040
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " DR=" WS-GL-DR-ACCOUNT
                " CR=" WS-GL-CR-ACCOUNT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-TXN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> TP-016: CTR flag on cash deposit exactly $10,000.00 -> Y
*> ---------------------------------------------------------------
TEST-TP-016.
    ADD 1 TO WS-TEST-COUNT
    MOVE "TP-016: CTR $10000=Y" TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-CHECKING
    PERFORM SETUP-DEPOSIT-TXN
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE 10000.00 TO TXN-AMOUNT
    MOVE 10000.00 TO TXN-CASH-AMOUNT
    CALL "TXNPOST0" USING TXN-RECORD ACCT-RECORD
                          WS-GL-ENTRIES WS-TXN-RESULT
    IF WS-TXN-RESULT-CODE = "E0000"
        IF TXN-CTR-REPORTABLE = "Y"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " CTR=" TXN-CTR-REPORTABLE
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-TXN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> TP-017: No CTR flag on cash deposit $9,999.99 -> N
*> ---------------------------------------------------------------
TEST-TP-017.
    ADD 1 TO WS-TEST-COUNT
    MOVE "TP-017: CTR $9999.99=N" TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-CHECKING
    PERFORM SETUP-DEPOSIT-TXN
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE 9999.99 TO TXN-AMOUNT
    MOVE 9999.99 TO TXN-CASH-AMOUNT
    CALL "TXNPOST0" USING TXN-RECORD ACCT-RECORD
                          WS-GL-ENTRIES WS-TXN-RESULT
    IF WS-TXN-RESULT-CODE = "E0000"
        IF TXN-CTR-REPORTABLE = "N"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " CTR=" TXN-CTR-REPORTABLE
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-TXN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> TP-018: Fee posting GL: DR customer acct, CR 7500
*> ---------------------------------------------------------------
TEST-TP-018.
    ADD 1 TO WS-TEST-COUNT
    MOVE "TP-018: Fee GL DR acct CR 7500" TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-CHECKING
    INITIALIZE TXN-RECORD
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE 000000000000003 TO TXN-ID
    MOVE 00001 TO TXN-SEQUENCE
    MOVE 000012345678 TO TXN-ACCT-NUMBER
    MOVE 2 TO TXN-ACCT-CHECK-DIGIT
    MOVE 0001 TO TXN-BRANCH-ID
    MOVE "SYSTEM" TO TXN-TELLER-ID
    MOVE "BR" TO TXN-CHANNEL
    MOVE "FEE" TO TXN-TYPE
    MOVE "D" TO TXN-DR-CR
    MOVE 25.00 TO TXN-AMOUNT
    MOVE 20260226 TO TXN-POST-DATE
    MOVE 120000 TO TXN-POST-TIME
    MOVE 20260226 TO TXN-EFFECTIVE-DATE
    CALL "TXNPOST0" USING TXN-RECORD ACCT-RECORD
                          WS-GL-ENTRIES WS-TXN-RESULT
    IF WS-TXN-RESULT-CODE = "E0000"
        IF WS-GL-CR-ACCOUNT = 7500
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " CR=" WS-GL-CR-ACCOUNT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-TXN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> TP-019: Interest posting GL: DR 8010, CR customer acct
*> ---------------------------------------------------------------
TEST-TP-019.
    ADD 1 TO WS-TEST-COUNT
    MOVE "TP-019: Int GL DR8010 CR acct" TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-CHECKING
    INITIALIZE TXN-RECORD
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE 000000000000004 TO TXN-ID
    MOVE 00001 TO TXN-SEQUENCE
    MOVE 000012345678 TO TXN-ACCT-NUMBER
    MOVE 2 TO TXN-ACCT-CHECK-DIGIT
    MOVE 0001 TO TXN-BRANCH-ID
    MOVE "SYSTEM" TO TXN-TELLER-ID
    MOVE "BR" TO TXN-CHANNEL
    MOVE "INT" TO TXN-TYPE
    MOVE "C" TO TXN-DR-CR
    MOVE 15.50 TO TXN-AMOUNT
    MOVE 20260226 TO TXN-POST-DATE
    MOVE 120000 TO TXN-POST-TIME
    MOVE 20260226 TO TXN-EFFECTIVE-DATE
    CALL "TXNPOST0" USING TXN-RECORD ACCT-RECORD
                          WS-GL-ENTRIES WS-TXN-RESULT
    IF WS-TXN-RESULT-CODE = "E0000"
        IF WS-GL-DR-ACCOUNT = 8010
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " DR=" WS-GL-DR-ACCOUNT
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-TXN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> TP-020: Large value precision $9,999,999,999.99
*> ---------------------------------------------------------------
TEST-TP-020.
    ADD 1 TO WS-TEST-COUNT
    MOVE "TP-020: Large value precision" TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-CHECKING
    MOVE 0 TO ACCT-LEDGER-BAL
    MOVE 0 TO ACCT-AVAIL-BAL
    PERFORM SETUP-DEPOSIT-TXN
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE 9999999999.99 TO TXN-AMOUNT
    MOVE 9999999999.99 TO TXN-CASH-AMOUNT
    CALL "TXNPOST0" USING TXN-RECORD ACCT-RECORD
                          WS-GL-ENTRIES WS-TXN-RESULT
    IF WS-TXN-RESULT-CODE = "E0000"
        IF ACCT-LEDGER-BAL = 9999999999.99
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " bal=" ACCT-LEDGER-BAL
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-TXN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> TP-021: LAST-TXN-DATE updated to post date
*> ---------------------------------------------------------------
TEST-TP-021.
    ADD 1 TO WS-TEST-COUNT
    MOVE "TP-021: LAST-TXN-DATE updated" TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-CHECKING
    MOVE 20260101 TO ACCT-LAST-TXN-DATE
    PERFORM SETUP-DEPOSIT-TXN
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE 100.00 TO TXN-AMOUNT
    MOVE 100.00 TO TXN-CASH-AMOUNT
    CALL "TXNPOST0" USING TXN-RECORD ACCT-RECORD
                          WS-GL-ENTRIES WS-TXN-RESULT
    IF WS-TXN-RESULT-CODE = "E0000"
        IF ACCT-LAST-TXN-DATE = 20260226
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " date=" ACCT-LAST-TXN-DATE
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-TXN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> TP-022: Deposit to savings (SAV1) -> success
*> ---------------------------------------------------------------
TEST-TP-022.
    ADD 1 TO WS-TEST-COUNT
    MOVE "TP-022: Savings deposit" TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-CHECKING
    MOVE "SAV1" TO ACCT-PRODUCT-CODE
    MOVE "SV" TO ACCT-SUB-TYPE
    PERFORM SETUP-DEPOSIT-TXN
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE 250.00 TO TXN-AMOUNT
    MOVE 250.00 TO TXN-CASH-AMOUNT
    CALL "TXNPOST0" USING TXN-RECORD ACCT-RECORD
                          WS-GL-ENTRIES WS-TXN-RESULT
    IF WS-TXN-RESULT-CODE = "E0000"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-TXN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> TP-023: Withdrawal from MMA (MMA1) -> success
*> ---------------------------------------------------------------
TEST-TP-023.
    ADD 1 TO WS-TEST-COUNT
    MOVE "TP-023: MMA withdrawal" TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-CHECKING
    MOVE "MMA1" TO ACCT-PRODUCT-CODE
    MOVE "MM" TO ACCT-SUB-TYPE
    PERFORM SETUP-WITHDRAWAL-TXN
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE 150.00 TO TXN-AMOUNT
    MOVE 150.00 TO TXN-CASH-AMOUNT
    CALL "TXNPOST0" USING TXN-RECORD ACCT-RECORD
                          WS-GL-ENTRIES WS-TXN-RESULT
    IF WS-TXN-RESULT-CODE = "E0000"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-TXN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> TP-024: Post to dormant account -> still allows (dormant!=frozen)
*> ---------------------------------------------------------------
TEST-TP-024.
    ADD 1 TO WS-TEST-COUNT
    MOVE "TP-024: Dormant acct allows post" TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-CHECKING
    MOVE "D" TO ACCT-STATUS
    PERFORM SETUP-DEPOSIT-TXN
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE 100.00 TO TXN-AMOUNT
    MOVE 100.00 TO TXN-CASH-AMOUNT
    CALL "TXNPOST0" USING TXN-RECORD ACCT-RECORD
                          WS-GL-ENTRIES WS-TXN-RESULT
    IF WS-TXN-RESULT-CODE = "E0000"
        ADD 1 TO WS-PASS-COUNT
        DISPLAY "  PASS: " WS-TEST-NAME
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " expected=E0000 actual=" WS-TXN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> TP-025: Available balance reflects holds
*> ---------------------------------------------------------------
TEST-TP-025.
    ADD 1 TO WS-TEST-COUNT
    MOVE "TP-025: Avail bal reflects holds" TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-CHECKING
    MOVE 1000.00 TO ACCT-LEDGER-BAL
    MOVE 200.00 TO ACCT-HOLD-AMOUNT
    MOVE 800.00 TO ACCT-AVAIL-BAL
    PERFORM SETUP-DEPOSIT-TXN
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE 500.00 TO TXN-AMOUNT
    MOVE 500.00 TO TXN-CASH-AMOUNT
    CALL "TXNPOST0" USING TXN-RECORD ACCT-RECORD
                          WS-GL-ENTRIES WS-TXN-RESULT
    IF WS-TXN-RESULT-CODE = "E0000"
        COMPUTE WS-EXPECTED-BAL =
            ACCT-LEDGER-BAL - ACCT-HOLD-AMOUNT
        IF ACCT-AVAIL-BAL = WS-EXPECTED-BAL
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " avail=" ACCT-AVAIL-BAL
                " exp=" WS-EXPECTED-BAL
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-TXN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> TP-026: Credit to garnished account should succeed
*>         Garnishment blocks debits only via legal hold, not credits
*> ---------------------------------------------------------------
TEST-TP-026.
    ADD 1 TO WS-TEST-COUNT
    MOVE "TP-026: Credit garnished acct=E0000" TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-CHECKING
    PERFORM SETUP-DEPOSIT-TXN
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE "Y" TO ACCT-GARNISHMENT
    MOVE 100.00 TO TXN-AMOUNT
    MOVE 100.00 TO TXN-CASH-AMOUNT
    COMPUTE WS-EXPECTED-BAL = ACCT-LEDGER-BAL + 100.00
    CALL "TXNPOST0" USING TXN-RECORD ACCT-RECORD
                          WS-GL-ENTRIES WS-TXN-RESULT
    IF WS-TXN-RESULT-CODE = "E0000"
        IF ACCT-LEDGER-BAL = WS-EXPECTED-BAL
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " bal=" ACCT-LEDGER-BAL
                " exp=" WS-EXPECTED-BAL
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-TXN-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> TP-027: Deposit to account with unknown sub-type "XX"
*>         GL mapping should handle gracefully; posting succeeds
*> ---------------------------------------------------------------
TEST-TP-027.
    ADD 1 TO WS-TEST-COUNT
    MOVE "TP-027: Unknown sub-type XX=E0000" TO WS-TEST-NAME
    PERFORM SETUP-ACTIVE-CHECKING
    MOVE "XX" TO ACCT-SUB-TYPE
    PERFORM SETUP-DEPOSIT-TXN
    INITIALIZE WS-GL-ENTRIES
    INITIALIZE WS-TXN-RESULT
    MOVE 250.00 TO TXN-AMOUNT
    MOVE 250.00 TO TXN-CASH-AMOUNT
    COMPUTE WS-EXPECTED-BAL = ACCT-LEDGER-BAL + 250.00
    CALL "TXNPOST0" USING TXN-RECORD ACCT-RECORD
                          WS-GL-ENTRIES WS-TXN-RESULT
    IF WS-TXN-RESULT-CODE = "E0000"
        IF ACCT-LEDGER-BAL = WS-EXPECTED-BAL
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " bal=" ACCT-LEDGER-BAL
                " exp=" WS-EXPECTED-BAL
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " result=" WS-TXN-RESULT-CODE
    END-IF.

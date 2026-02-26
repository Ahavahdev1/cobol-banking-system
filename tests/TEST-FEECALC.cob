IDENTIFICATION DIVISION.
PROGRAM-ID. TEST-FEECALC.
*> ================================================================
*> TEST-FEECALC - Test suite for FEECALC0 Fee Assessment Engine
*> Tests: Monthly fees, waivers, NSF fees, de minimis, YTD tracking
*> 15 tests (FE-001 to FE-015)
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-TEST-COUNT          PIC 9(3) VALUE 0.
01  WS-PASS-COUNT          PIC 9(3) VALUE 0.
01  WS-FAIL-COUNT          PIC 9(3) VALUE 0.
01  WS-TEST-NAME           PIC X(60).

*> Account and Fee schedule (matches LINKAGE in FEECALC0)
COPY CPYACCT.
COPY CPYFEE.

01  WS-FEE-RESULT.
    05  WS-FEE-RESULT-CODE  PIC X(5).
    05  WS-FEE-RESULT-MSG   PIC X(50).
    05  WS-FEE-ASSESSED     PIC 9(5)V99.
    05  WS-FEE-WAIVED-FLAG  PIC X(1).
    05  WS-FEE-WAIVER-REASON PIC X(2).

01  WS-EXPECTED-FEE         PIC 9(5)V99.
01  WS-EXPECTED-YTD-CHARGED PIC S9(9)V99.
01  WS-EXPECTED-YTD-WAIVED  PIC S9(9)V99.

PROCEDURE DIVISION.
MAIN-PROGRAM.
    DISPLAY "========================================".
    DISPLAY "TEST SUITE: FEECALC - Fee Assessment".
    DISPLAY "========================================".

    PERFORM TEST-FE-001
    PERFORM TEST-FE-002
    PERFORM TEST-FE-003
    PERFORM TEST-FE-004
    PERFORM TEST-FE-005
    PERFORM TEST-FE-006
    PERFORM TEST-FE-007
    PERFORM TEST-FE-008
    PERFORM TEST-FE-009
    PERFORM TEST-FE-010
    PERFORM TEST-FE-011
    PERFORM TEST-FE-012
    PERFORM TEST-FE-013
    PERFORM TEST-FE-014
    PERFORM TEST-FE-015

    DISPLAY "========================================".
    DISPLAY "RESULTS: " WS-PASS-COUNT "/" WS-TEST-COUNT
            " PASSED".
    DISPLAY "         " WS-FAIL-COUNT " FAILED".
    DISPLAY "========================================".
    MOVE WS-FAIL-COUNT TO RETURN-CODE
    STOP RUN.

*> ---------------------------------------------------------------
*> FE-001: Monthly fee assessed ($12.00, bal $500 < min $1500)
*> ---------------------------------------------------------------
TEST-FE-001.
    ADD 1 TO WS-TEST-COUNT
    MOVE "FE-001: Monthly fee $12 assessed" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE FEE-SCHEDULE-RECORD
    INITIALIZE WS-FEE-RESULT
    MOVE 500.00 TO ACCT-LEDGER-BAL
    MOVE 500.00 TO ACCT-AVAIL-BAL
    MOVE "A" TO ACCT-STATUS
    MOVE "DDA1" TO ACCT-PRODUCT-CODE
    MOVE "CH" TO ACCT-SUB-TYPE
    MOVE 12.00 TO ACCT-MONTHLY-FEE
    MOVE "NW" TO ACCT-FEE-WAIVER-CODE
    MOVE "DDA1" TO FEE-PRODUCT-CODE
    MOVE "MTH" TO FEE-TYPE
    MOVE 12.00 TO FEE-AMOUNT
    MOVE "Y" TO FEE-WAIVER-ELIGIBLE
    MOVE 1500.00 TO FEE-MIN-BAL-THRESHOLD
    MOVE "N" TO FEE-DD-WAIVER
    MOVE "N" TO FEE-EMPLOYEE-WAIVER
    MOVE "A" TO FEE-STATUS
    MOVE 12.00 TO WS-EXPECTED-FEE
    CALL "FEECALC0" USING ACCT-RECORD FEE-SCHEDULE-RECORD
                          WS-FEE-RESULT
    IF WS-FEE-RESULT-CODE = "E0000"
        IF WS-FEE-ASSESSED = WS-EXPECTED-FEE
            AND WS-FEE-WAIVED-FLAG = "N"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " fee=" WS-FEE-ASSESSED
                " waived=" WS-FEE-WAIVED-FLAG
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-FEE-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> FE-002: Monthly fee waived (bal $1500 >= min $1500)
*> ---------------------------------------------------------------
TEST-FE-002.
    ADD 1 TO WS-TEST-COUNT
    MOVE "FE-002: Fee waived bal >= min (MB)" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE FEE-SCHEDULE-RECORD
    INITIALIZE WS-FEE-RESULT
    MOVE 1500.00 TO ACCT-LEDGER-BAL
    MOVE 1500.00 TO ACCT-AVAIL-BAL
    MOVE "A" TO ACCT-STATUS
    MOVE "DDA1" TO ACCT-PRODUCT-CODE
    MOVE 12.00 TO ACCT-MONTHLY-FEE
    MOVE "MB" TO ACCT-FEE-WAIVER-CODE
    MOVE "DDA1" TO FEE-PRODUCT-CODE
    MOVE "MTH" TO FEE-TYPE
    MOVE 12.00 TO FEE-AMOUNT
    MOVE "Y" TO FEE-WAIVER-ELIGIBLE
    MOVE 1500.00 TO FEE-MIN-BAL-THRESHOLD
    MOVE "N" TO FEE-DD-WAIVER
    MOVE "N" TO FEE-EMPLOYEE-WAIVER
    MOVE "A" TO FEE-STATUS
    CALL "FEECALC0" USING ACCT-RECORD FEE-SCHEDULE-RECORD
                          WS-FEE-RESULT
    IF WS-FEE-RESULT-CODE = "E0000"
        IF WS-FEE-WAIVED-FLAG = "Y"
            AND WS-FEE-WAIVER-REASON = "MB"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " waived=" WS-FEE-WAIVED-FLAG
                " reason=" WS-FEE-WAIVER-REASON
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-FEE-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> FE-003: Boundary: bal $1499.99 < min $1500 -> fee charged
*> ---------------------------------------------------------------
TEST-FE-003.
    ADD 1 TO WS-TEST-COUNT
    MOVE "FE-003: Boundary $1499.99 < $1500" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE FEE-SCHEDULE-RECORD
    INITIALIZE WS-FEE-RESULT
    MOVE 1499.99 TO ACCT-LEDGER-BAL
    MOVE 1499.99 TO ACCT-AVAIL-BAL
    MOVE "A" TO ACCT-STATUS
    MOVE "DDA1" TO ACCT-PRODUCT-CODE
    MOVE 12.00 TO ACCT-MONTHLY-FEE
    MOVE "NW" TO ACCT-FEE-WAIVER-CODE
    MOVE "DDA1" TO FEE-PRODUCT-CODE
    MOVE "MTH" TO FEE-TYPE
    MOVE 12.00 TO FEE-AMOUNT
    MOVE "Y" TO FEE-WAIVER-ELIGIBLE
    MOVE 1500.00 TO FEE-MIN-BAL-THRESHOLD
    MOVE "N" TO FEE-DD-WAIVER
    MOVE "N" TO FEE-EMPLOYEE-WAIVER
    MOVE "A" TO FEE-STATUS
    MOVE 12.00 TO WS-EXPECTED-FEE
    CALL "FEECALC0" USING ACCT-RECORD FEE-SCHEDULE-RECORD
                          WS-FEE-RESULT
    IF WS-FEE-RESULT-CODE = "E0000"
        IF WS-FEE-ASSESSED = WS-EXPECTED-FEE
            AND WS-FEE-WAIVED-FLAG = "N"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " fee=" WS-FEE-ASSESSED
                " waived=" WS-FEE-WAIVED-FLAG
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-FEE-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> FE-004: Boundary: bal $1500.00 = min -> fee waived
*> ---------------------------------------------------------------
TEST-FE-004.
    ADD 1 TO WS-TEST-COUNT
    MOVE "FE-004: Boundary $1500 = min -> waived" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE FEE-SCHEDULE-RECORD
    INITIALIZE WS-FEE-RESULT
    MOVE 1500.00 TO ACCT-LEDGER-BAL
    MOVE 1500.00 TO ACCT-AVAIL-BAL
    MOVE "A" TO ACCT-STATUS
    MOVE "DDA1" TO ACCT-PRODUCT-CODE
    MOVE 12.00 TO ACCT-MONTHLY-FEE
    MOVE "MB" TO ACCT-FEE-WAIVER-CODE
    MOVE "DDA1" TO FEE-PRODUCT-CODE
    MOVE "MTH" TO FEE-TYPE
    MOVE 12.00 TO FEE-AMOUNT
    MOVE "Y" TO FEE-WAIVER-ELIGIBLE
    MOVE 1500.00 TO FEE-MIN-BAL-THRESHOLD
    MOVE "N" TO FEE-DD-WAIVER
    MOVE "N" TO FEE-EMPLOYEE-WAIVER
    MOVE "A" TO FEE-STATUS
    CALL "FEECALC0" USING ACCT-RECORD FEE-SCHEDULE-RECORD
                          WS-FEE-RESULT
    IF WS-FEE-RESULT-CODE = "E0000"
        IF WS-FEE-WAIVED-FLAG = "Y"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " waived=" WS-FEE-WAIVED-FLAG
                " fee=" WS-FEE-ASSESSED
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-FEE-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> FE-005: NSF fee = $36.00
*> ---------------------------------------------------------------
TEST-FE-005.
    ADD 1 TO WS-TEST-COUNT
    MOVE "FE-005: NSF fee $36.00" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE FEE-SCHEDULE-RECORD
    INITIALIZE WS-FEE-RESULT
    MOVE 100.00 TO ACCT-LEDGER-BAL
    MOVE "A" TO ACCT-STATUS
    MOVE "DDA1" TO ACCT-PRODUCT-CODE
    MOVE 0 TO ACCT-NSF-COUNT-MTD
    MOVE 0 TO ACCT-NSF-COUNT-TODAY
    MOVE "DDA1" TO FEE-PRODUCT-CODE
    MOVE "NSF" TO FEE-TYPE
    MOVE 36.00 TO FEE-AMOUNT
    MOVE "N" TO FEE-WAIVER-ELIGIBLE
    MOVE 04 TO FEE-NSF-DAILY-MAX
    MOVE 5.00 TO FEE-NSF-DE-MINIMIS
    MOVE "A" TO FEE-STATUS
    MOVE 36.00 TO WS-EXPECTED-FEE
    CALL "FEECALC0" USING ACCT-RECORD FEE-SCHEDULE-RECORD
                          WS-FEE-RESULT
    IF WS-FEE-RESULT-CODE = "E0000"
        IF WS-FEE-ASSESSED = WS-EXPECTED-FEE
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=" WS-EXPECTED-FEE
                " actual=" WS-FEE-ASSESSED
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-FEE-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> FE-006: NSF daily cap: 4th OK, 5th denied
*> ---------------------------------------------------------------
TEST-FE-006.
    ADD 1 TO WS-TEST-COUNT
    MOVE "FE-006: NSF daily cap 5th denied" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE FEE-SCHEDULE-RECORD
    INITIALIZE WS-FEE-RESULT
    MOVE 100.00 TO ACCT-LEDGER-BAL
    MOVE "A" TO ACCT-STATUS
    MOVE "DDA1" TO ACCT-PRODUCT-CODE
    *> Already had 4 NSFs today (at daily max)
    MOVE 4 TO ACCT-NSF-COUNT-MTD
    MOVE 4 TO ACCT-NSF-COUNT-TODAY
    MOVE "DDA1" TO FEE-PRODUCT-CODE
    MOVE "NSF" TO FEE-TYPE
    MOVE 36.00 TO FEE-AMOUNT
    MOVE "N" TO FEE-WAIVER-ELIGIBLE
    MOVE 04 TO FEE-NSF-DAILY-MAX
    MOVE 5.00 TO FEE-NSF-DE-MINIMIS
    MOVE "A" TO FEE-STATUS
    CALL "FEECALC0" USING ACCT-RECORD FEE-SCHEDULE-RECORD
                          WS-FEE-RESULT
    *> 5th NSF should result in no fee (cap reached)
    IF WS-FEE-RESULT-CODE = "E0000"
        IF WS-FEE-ASSESSED = 0
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected fee=0 actual=" WS-FEE-ASSESSED
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-FEE-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> FE-007: De minimis: OD $4.99 -> no NSF fee
*> ---------------------------------------------------------------
TEST-FE-007.
    ADD 1 TO WS-TEST-COUNT
    MOVE "FE-007: De minimis $4.99 no fee" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE FEE-SCHEDULE-RECORD
    INITIALIZE WS-FEE-RESULT
    *> Negative balance of $4.99 (overdrawn by $4.99)
    MOVE -4.99 TO ACCT-LEDGER-BAL
    MOVE "A" TO ACCT-STATUS
    MOVE "DDA1" TO ACCT-PRODUCT-CODE
    MOVE 0 TO ACCT-NSF-COUNT-MTD
    MOVE 0 TO ACCT-NSF-COUNT-TODAY
    MOVE "DDA1" TO FEE-PRODUCT-CODE
    MOVE "NSF" TO FEE-TYPE
    MOVE 36.00 TO FEE-AMOUNT
    MOVE "N" TO FEE-WAIVER-ELIGIBLE
    MOVE 04 TO FEE-NSF-DAILY-MAX
    MOVE 5.00 TO FEE-NSF-DE-MINIMIS
    MOVE "A" TO FEE-STATUS
    CALL "FEECALC0" USING ACCT-RECORD FEE-SCHEDULE-RECORD
                          WS-FEE-RESULT
    IF WS-FEE-RESULT-CODE = "E0000"
        IF WS-FEE-ASSESSED = 0
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected fee=0 actual=" WS-FEE-ASSESSED
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-FEE-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> FE-008: De minimis boundary: OD $5.00 -> NSF fee charged
*> ---------------------------------------------------------------
TEST-FE-008.
    ADD 1 TO WS-TEST-COUNT
    MOVE "FE-008: De minimis boundary $5.00" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE FEE-SCHEDULE-RECORD
    INITIALIZE WS-FEE-RESULT
    MOVE -5.00 TO ACCT-LEDGER-BAL
    MOVE "A" TO ACCT-STATUS
    MOVE "DDA1" TO ACCT-PRODUCT-CODE
    MOVE 0 TO ACCT-NSF-COUNT-MTD
    MOVE 0 TO ACCT-NSF-COUNT-TODAY
    MOVE "DDA1" TO FEE-PRODUCT-CODE
    MOVE "NSF" TO FEE-TYPE
    MOVE 36.00 TO FEE-AMOUNT
    MOVE "N" TO FEE-WAIVER-ELIGIBLE
    MOVE 04 TO FEE-NSF-DAILY-MAX
    MOVE 5.00 TO FEE-NSF-DE-MINIMIS
    MOVE "A" TO FEE-STATUS
    MOVE 36.00 TO WS-EXPECTED-FEE
    CALL "FEECALC0" USING ACCT-RECORD FEE-SCHEDULE-RECORD
                          WS-FEE-RESULT
    IF WS-FEE-RESULT-CODE = "E0000"
        IF WS-FEE-ASSESSED = WS-EXPECTED-FEE
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=" WS-EXPECTED-FEE
                " actual=" WS-FEE-ASSESSED
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-FEE-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> FE-009: Direct deposit waiver (DD) -> fee waived
*> ---------------------------------------------------------------
TEST-FE-009.
    ADD 1 TO WS-TEST-COUNT
    MOVE "FE-009: DD waiver -> fee waived" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE FEE-SCHEDULE-RECORD
    INITIALIZE WS-FEE-RESULT
    MOVE 500.00 TO ACCT-LEDGER-BAL
    MOVE 500.00 TO ACCT-AVAIL-BAL
    MOVE "A" TO ACCT-STATUS
    MOVE "DDA1" TO ACCT-PRODUCT-CODE
    MOVE 12.00 TO ACCT-MONTHLY-FEE
    MOVE "DD" TO ACCT-FEE-WAIVER-CODE
    MOVE "DDA1" TO FEE-PRODUCT-CODE
    MOVE "MTH" TO FEE-TYPE
    MOVE 12.00 TO FEE-AMOUNT
    MOVE "Y" TO FEE-WAIVER-ELIGIBLE
    MOVE 1500.00 TO FEE-MIN-BAL-THRESHOLD
    MOVE "Y" TO FEE-DD-WAIVER
    MOVE "N" TO FEE-EMPLOYEE-WAIVER
    MOVE "A" TO FEE-STATUS
    CALL "FEECALC0" USING ACCT-RECORD FEE-SCHEDULE-RECORD
                          WS-FEE-RESULT
    IF WS-FEE-RESULT-CODE = "E0000"
        IF WS-FEE-WAIVED-FLAG = "Y"
            AND WS-FEE-WAIVER-REASON = "DD"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " waived=" WS-FEE-WAIVED-FLAG
                " reason=" WS-FEE-WAIVER-REASON
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-FEE-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> FE-010: Employee waiver (EM) -> fee waived
*> ---------------------------------------------------------------
TEST-FE-010.
    ADD 1 TO WS-TEST-COUNT
    MOVE "FE-010: Employee waiver -> fee waived" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE FEE-SCHEDULE-RECORD
    INITIALIZE WS-FEE-RESULT
    MOVE 500.00 TO ACCT-LEDGER-BAL
    MOVE 500.00 TO ACCT-AVAIL-BAL
    MOVE "A" TO ACCT-STATUS
    MOVE "DDA1" TO ACCT-PRODUCT-CODE
    MOVE 12.00 TO ACCT-MONTHLY-FEE
    MOVE "EM" TO ACCT-FEE-WAIVER-CODE
    MOVE "DDA1" TO FEE-PRODUCT-CODE
    MOVE "MTH" TO FEE-TYPE
    MOVE 12.00 TO FEE-AMOUNT
    MOVE "Y" TO FEE-WAIVER-ELIGIBLE
    MOVE 1500.00 TO FEE-MIN-BAL-THRESHOLD
    MOVE "N" TO FEE-DD-WAIVER
    MOVE "Y" TO FEE-EMPLOYEE-WAIVER
    MOVE "A" TO FEE-STATUS
    CALL "FEECALC0" USING ACCT-RECORD FEE-SCHEDULE-RECORD
                          WS-FEE-RESULT
    IF WS-FEE-RESULT-CODE = "E0000"
        IF WS-FEE-WAIVED-FLAG = "Y"
            AND WS-FEE-WAIVER-REASON = "EM"
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " waived=" WS-FEE-WAIVED-FLAG
                " reason=" WS-FEE-WAIVER-REASON
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-FEE-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> FE-011: YTD-FEES-CHARGED updated after fee assessment
*> ---------------------------------------------------------------
TEST-FE-011.
    ADD 1 TO WS-TEST-COUNT
    MOVE "FE-011: YTD fees charged updated" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE FEE-SCHEDULE-RECORD
    INITIALIZE WS-FEE-RESULT
    MOVE 500.00 TO ACCT-LEDGER-BAL
    MOVE "A" TO ACCT-STATUS
    MOVE "DDA1" TO ACCT-PRODUCT-CODE
    MOVE 12.00 TO ACCT-MONTHLY-FEE
    MOVE "NW" TO ACCT-FEE-WAIVER-CODE
    MOVE 24.00 TO ACCT-YTD-FEES-CHARGED
    MOVE "DDA1" TO FEE-PRODUCT-CODE
    MOVE "MTH" TO FEE-TYPE
    MOVE 12.00 TO FEE-AMOUNT
    MOVE "Y" TO FEE-WAIVER-ELIGIBLE
    MOVE 1500.00 TO FEE-MIN-BAL-THRESHOLD
    MOVE "N" TO FEE-DD-WAIVER
    MOVE "N" TO FEE-EMPLOYEE-WAIVER
    MOVE "A" TO FEE-STATUS
    *> After $12 fee, YTD should be 24 + 12 = 36
    MOVE 36.00 TO WS-EXPECTED-YTD-CHARGED
    CALL "FEECALC0" USING ACCT-RECORD FEE-SCHEDULE-RECORD
                          WS-FEE-RESULT
    IF WS-FEE-RESULT-CODE = "E0000"
        IF ACCT-YTD-FEES-CHARGED = WS-EXPECTED-YTD-CHARGED
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected YTD=" WS-EXPECTED-YTD-CHARGED
                " actual=" ACCT-YTD-FEES-CHARGED
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-FEE-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> FE-012: YTD-FEES-WAIVED updated after waiver
*> ---------------------------------------------------------------
TEST-FE-012.
    ADD 1 TO WS-TEST-COUNT
    MOVE "FE-012: YTD fees waived updated" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE FEE-SCHEDULE-RECORD
    INITIALIZE WS-FEE-RESULT
    MOVE 1500.00 TO ACCT-LEDGER-BAL
    MOVE 1500.00 TO ACCT-AVAIL-BAL
    MOVE "A" TO ACCT-STATUS
    MOVE "DDA1" TO ACCT-PRODUCT-CODE
    MOVE 12.00 TO ACCT-MONTHLY-FEE
    MOVE "MB" TO ACCT-FEE-WAIVER-CODE
    MOVE 24.00 TO ACCT-YTD-FEES-WAIVED
    MOVE "DDA1" TO FEE-PRODUCT-CODE
    MOVE "MTH" TO FEE-TYPE
    MOVE 12.00 TO FEE-AMOUNT
    MOVE "Y" TO FEE-WAIVER-ELIGIBLE
    MOVE 1500.00 TO FEE-MIN-BAL-THRESHOLD
    MOVE "N" TO FEE-DD-WAIVER
    MOVE "N" TO FEE-EMPLOYEE-WAIVER
    MOVE "A" TO FEE-STATUS
    *> After waiver, YTD waived should be 24 + 12 = 36
    MOVE 36.00 TO WS-EXPECTED-YTD-WAIVED
    CALL "FEECALC0" USING ACCT-RECORD FEE-SCHEDULE-RECORD
                          WS-FEE-RESULT
    IF WS-FEE-RESULT-CODE = "E0000"
        IF ACCT-YTD-FEES-WAIVED = WS-EXPECTED-YTD-WAIVED
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected YTD=" WS-EXPECTED-YTD-WAIVED
                " actual=" ACCT-YTD-FEES-WAIVED
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-FEE-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> FE-013: No fee if FEE-AMOUNT = 0
*> ---------------------------------------------------------------
TEST-FE-013.
    ADD 1 TO WS-TEST-COUNT
    MOVE "FE-013: Zero fee amount -> no fee" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE FEE-SCHEDULE-RECORD
    INITIALIZE WS-FEE-RESULT
    MOVE 500.00 TO ACCT-LEDGER-BAL
    MOVE "A" TO ACCT-STATUS
    MOVE "DDA1" TO ACCT-PRODUCT-CODE
    MOVE "DDA1" TO FEE-PRODUCT-CODE
    MOVE "MTH" TO FEE-TYPE
    MOVE 0.00 TO FEE-AMOUNT
    MOVE "N" TO FEE-WAIVER-ELIGIBLE
    MOVE "A" TO FEE-STATUS
    CALL "FEECALC0" USING ACCT-RECORD FEE-SCHEDULE-RECORD
                          WS-FEE-RESULT
    IF WS-FEE-RESULT-CODE = "E0000"
        IF WS-FEE-ASSESSED = 0
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=0 actual=" WS-FEE-ASSESSED
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-FEE-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> FE-014: Inactive fee schedule (status "I") -> no fee
*> ---------------------------------------------------------------
TEST-FE-014.
    ADD 1 TO WS-TEST-COUNT
    MOVE "FE-014: Inactive schedule -> no fee" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE FEE-SCHEDULE-RECORD
    INITIALIZE WS-FEE-RESULT
    MOVE 500.00 TO ACCT-LEDGER-BAL
    MOVE "A" TO ACCT-STATUS
    MOVE "DDA1" TO ACCT-PRODUCT-CODE
    MOVE "DDA1" TO FEE-PRODUCT-CODE
    MOVE "MTH" TO FEE-TYPE
    MOVE 12.00 TO FEE-AMOUNT
    MOVE "Y" TO FEE-WAIVER-ELIGIBLE
    MOVE 1500.00 TO FEE-MIN-BAL-THRESHOLD
    MOVE "I" TO FEE-STATUS
    CALL "FEECALC0" USING ACCT-RECORD FEE-SCHEDULE-RECORD
                          WS-FEE-RESULT
    IF WS-FEE-RESULT-CODE = "E0000"
        IF WS-FEE-ASSESSED = 0
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " expected=0 actual=" WS-FEE-ASSESSED
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-FEE-RESULT-CODE
    END-IF.

*> ---------------------------------------------------------------
*> FE-015: Multiple fee waivers eligible - employee takes priority
*>         Account has EM waiver code, bal >= min, employee flag Y
*>         Employee waiver should win with reason = "EM"
*> ---------------------------------------------------------------
TEST-FE-015.
    ADD 1 TO WS-TEST-COUNT
    MOVE "FE-015: Multi-waiver EM priority" TO WS-TEST-NAME
    INITIALIZE ACCT-RECORD
    INITIALIZE FEE-SCHEDULE-RECORD
    INITIALIZE WS-FEE-RESULT
    MOVE 5000.00 TO ACCT-LEDGER-BAL
    MOVE 5000.00 TO ACCT-AVAIL-BAL
    MOVE "A" TO ACCT-STATUS
    MOVE "DDA1" TO ACCT-PRODUCT-CODE
    MOVE 12.00 TO ACCT-MONTHLY-FEE
    MOVE "EM" TO ACCT-FEE-WAIVER-CODE
    MOVE "DDA1" TO FEE-PRODUCT-CODE
    MOVE "MTH" TO FEE-TYPE
    MOVE 12.00 TO FEE-AMOUNT
    MOVE "Y" TO FEE-WAIVER-ELIGIBLE
    MOVE 1500.00 TO FEE-MIN-BAL-THRESHOLD
    MOVE "N" TO FEE-DD-WAIVER
    MOVE "Y" TO FEE-EMPLOYEE-WAIVER
    MOVE "A" TO FEE-STATUS
    CALL "FEECALC0" USING ACCT-RECORD FEE-SCHEDULE-RECORD
                          WS-FEE-RESULT
    IF WS-FEE-RESULT-CODE = "E0000"
        IF WS-FEE-WAIVED-FLAG = "Y"
            AND WS-FEE-WAIVER-REASON = "EM"
            AND WS-FEE-ASSESSED = 0
            ADD 1 TO WS-PASS-COUNT
            DISPLAY "  PASS: " WS-TEST-NAME
        ELSE
            ADD 1 TO WS-FAIL-COUNT
            DISPLAY "  FAIL: " WS-TEST-NAME
                " waived=" WS-FEE-WAIVED-FLAG
                " reason=" WS-FEE-WAIVER-REASON
                " fee=" WS-FEE-ASSESSED
        END-IF
    ELSE
        ADD 1 TO WS-FAIL-COUNT
        DISPLAY "  FAIL: " WS-TEST-NAME
            " rc=" WS-FEE-RESULT-CODE
    END-IF.

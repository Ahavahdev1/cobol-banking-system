IDENTIFICATION DIVISION.
PROGRAM-ID. ODMGMT0.
*> ================================================================
*> ODMGMT0 - Overdraft Management
*> Handles Reg E opt-in, OD limits, protection transfers, fees
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
COPY CPYCONST.
COPY CPYERR.
01  WS-EFFECTIVE-BAL          PIC S9(13)V99.
01  WS-OD-AMOUNT              PIC S9(13)V99.
01  WS-SHORTFALL               PIC S9(13)V99.

LINKAGE SECTION.
COPY CPYACCT.
01  LS-OD-REQUEST.
    05  LS-OD-TXN-AMOUNT         PIC S9(13)V99.
    05  LS-OD-TXN-CHANNEL        PIC X(2).
    *> AT = ATM, PO = POS, CK = Check, AC = ACH
    05  LS-OD-TXN-TYPE           PIC X(3).
    05  LS-OD-CURRENT-DATE        PIC 9(8).
01  LS-OD-RESULT.
    05  LS-OD-RESULT-CODE         PIC X(5).
    05  LS-OD-RESULT-MSG          PIC X(50).
    05  LS-OD-APPROVED            PIC X(1).
    05  LS-OD-FEE-ASSESSED        PIC 9(5)V99.
    05  LS-OD-TRANSFER-AMT        PIC S9(13)V99.
    05  LS-OD-NEW-NSF-COUNT       PIC 9(3).

PROCEDURE DIVISION USING ACCT-RECORD
                         LS-OD-REQUEST
                         LS-OD-RESULT.
ODMGMT0-MAIN.
    MOVE "E0000" TO LS-OD-RESULT-CODE
    MOVE SPACES TO LS-OD-RESULT-MSG
    MOVE "N" TO LS-OD-APPROVED
    MOVE ZEROS TO LS-OD-FEE-ASSESSED
    MOVE ZEROS TO LS-OD-TRANSFER-AMT
    MOVE ACCT-NSF-COUNT-TODAY TO LS-OD-NEW-NSF-COUNT

    *> Validate account type: overdraft applies to deposit accounts only
    IF ACCT-TYPE NOT = "D"
        MOVE "E0018" TO LS-OD-RESULT-CODE
        MOVE "Overdraft applies to deposit accounts only"
            TO LS-OD-RESULT-MSG
        GOBACK
    END-IF

    *> Validate account status: reject closed or frozen accounts
    IF ACCT-STATUS = "C"
        MOVE "E0011" TO LS-OD-RESULT-CODE
        MOVE "Account is closed" TO LS-OD-RESULT-MSG
        GOBACK
    END-IF
    IF ACCT-STATUS = "F"
        MOVE "E0012" TO LS-OD-RESULT-CODE
        MOVE "Account is frozen" TO LS-OD-RESULT-MSG
        GOBACK
    END-IF
    IF ACCT-STATUS = "E"
        MOVE "E0050" TO LS-OD-RESULT-CODE
        MOVE "Account is escheated" TO LS-OD-RESULT-MSG
        GOBACK
    END-IF

    *> Determine effective balance
    MOVE ACCT-AVAIL-BAL TO WS-EFFECTIVE-BAL

    *> Step 1: Reg E opt-in check
    *> ATM and POS require opt-in; CK and AC are exempt
    IF ACCT-OD-OPTED-IN = "N"
        IF LS-OD-TXN-CHANNEL = "AT"
            OR LS-OD-TXN-CHANNEL = "PO"
            MOVE "E0095" TO LS-OD-RESULT-CODE
            MOVE "Reg E opt-in required for ATM/POS OD"
                TO LS-OD-RESULT-MSG
            MOVE "N" TO LS-OD-APPROVED
            GOBACK
        END-IF
    END-IF

    *> Step 2: Calculate potential overdraft
    COMPUTE WS-OD-AMOUNT =
        LS-OD-TXN-AMOUNT - WS-EFFECTIVE-BAL
        ON SIZE ERROR
            MOVE "E0040" TO LS-OD-RESULT-CODE
            MOVE "Arithmetic overflow on OD amount"
                TO LS-OD-RESULT-MSG
            GOBACK
    END-COMPUTE

    *> Step 3: Check OD limit
    IF WS-OD-AMOUNT > ACCT-OD-LIMIT
        MOVE "E0096" TO LS-OD-RESULT-CODE
        MOVE "Overdraft would exceed account OD limit"
            TO LS-OD-RESULT-MSG
        MOVE "N" TO LS-OD-APPROVED
        GOBACK
    END-IF

    *> Step 4: OD protection transfer
    IF ACCT-OD-PROTECTION = "T"
        COMPUTE WS-SHORTFALL =
            LS-OD-TXN-AMOUNT - WS-EFFECTIVE-BAL
            ON SIZE ERROR
                MOVE "E0040" TO LS-OD-RESULT-CODE
                MOVE "Arithmetic overflow on OD shortfall"
                    TO LS-OD-RESULT-MSG
                GOBACK
        END-COMPUTE
        IF WS-SHORTFALL > ZEROS
            MOVE WS-SHORTFALL TO LS-OD-TRANSFER-AMT
        END-IF
        MOVE "Y" TO LS-OD-APPROVED
        MOVE "E0000" TO LS-OD-RESULT-CODE
        MOVE "OD approved via protection transfer"
            TO LS-OD-RESULT-MSG
        GOBACK
    END-IF

    *> Step 5: Approve the overdraft
    MOVE "Y" TO LS-OD-APPROVED

    *> Step 6: NSF fee assessment (only when OD amount > 0)
    IF WS-OD-AMOUNT > ZEROS
        *> De minimis check: no fee if OD amount < $5.00
        IF WS-OD-AMOUNT < WS-NSF-DE-MINIMIS
            MOVE ZEROS TO LS-OD-FEE-ASSESSED
        ELSE
            *> Daily cap check
            IF ACCT-NSF-COUNT-TODAY >= WS-NSF-DAILY-MAX
                MOVE ZEROS TO LS-OD-FEE-ASSESSED
            ELSE
                MOVE WS-NSF-FEE-AMOUNT TO LS-OD-FEE-ASSESSED
            END-IF
        END-IF
        *> Increment daily, monthly, and yearly NSF counts
        ADD 1 TO LS-OD-NEW-NSF-COUNT
            ON SIZE ERROR CONTINUE
        END-ADD
        ADD 1 TO ACCT-NSF-COUNT-TODAY
            ON SIZE ERROR CONTINUE
        END-ADD
        ADD 1 TO ACCT-NSF-COUNT-MTD
            ON SIZE ERROR CONTINUE
        END-ADD
        ADD 1 TO ACCT-NSF-COUNT-YTD
            ON SIZE ERROR CONTINUE
        END-ADD
    END-IF

    MOVE "E0000" TO LS-OD-RESULT-CODE
    MOVE "Overdraft transaction approved"
        TO LS-OD-RESULT-MSG
    GOBACK.

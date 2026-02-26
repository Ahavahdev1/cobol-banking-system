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
    MOVE ACCT-NSF-COUNT-MTD TO LS-OD-NEW-NSF-COUNT

    *> Determine effective balance
    IF ACCT-AVAIL-BAL NOT = ZEROS
        MOVE ACCT-AVAIL-BAL TO WS-EFFECTIVE-BAL
    ELSE
        MOVE ACCT-LEDGER-BAL TO WS-EFFECTIVE-BAL
    END-IF

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
            IF ACCT-NSF-COUNT-MTD >= WS-NSF-DAILY-MAX
                MOVE ZEROS TO LS-OD-FEE-ASSESSED
            ELSE
                MOVE WS-NSF-FEE-AMOUNT TO LS-OD-FEE-ASSESSED
            END-IF
        END-IF
        *> Increment NSF count
        ADD 1 TO LS-OD-NEW-NSF-COUNT
    END-IF

    MOVE "E0000" TO LS-OD-RESULT-CODE
    MOVE "Overdraft transaction approved"
        TO LS-OD-RESULT-MSG
    GOBACK.

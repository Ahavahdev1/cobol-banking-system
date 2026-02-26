IDENTIFICATION DIVISION.
PROGRAM-ID. REGDCHK0.
*> ================================================================
*> REGDCHK0 - Regulation D Transfer Limit Check
*> Enforces 6 electronic transfers/month for savings/MMA
*> ================================================================

DATA DIVISION.
WORKING-STORAGE SECTION.
COPY CPYCONST.
01  WS-NEW-COUNT             PIC 9(3).

LINKAGE SECTION.
COPY CPYACCT.
01  LS-REGD-REQUEST.
    05  LS-RD-TXN-CHANNEL        PIC X(2).
    *> OL = Online, AC = ACH, AT = ATM, BR = Branch
    05  LS-RD-TXN-TYPE           PIC X(3).
    05  LS-RD-CURRENT-COUNT      PIC 9(3).
01  LS-REGD-RESULT.
    05  LS-RD-RESULT-CODE         PIC X(5).
    05  LS-RD-RESULT-MSG          PIC X(50).
    05  LS-RD-ALLOWED             PIC X(1).
    05  LS-RD-NEW-COUNT           PIC 9(3).
    05  LS-RD-LIMIT-REACHED       PIC X(1).

PROCEDURE DIVISION USING ACCT-RECORD
                         LS-REGD-REQUEST
                         LS-REGD-RESULT.
MAIN-LOGIC.
    INITIALIZE LS-REGD-RESULT
    MOVE "N" TO LS-RD-LIMIT-REACHED
    MOVE LS-RD-CURRENT-COUNT TO LS-RD-NEW-COUNT

    *> Checking accounts are exempt from Regulation D
    IF ACCT-SUB-TYPE = "CH"
        MOVE "E0000" TO LS-RD-RESULT-CODE
        MOVE "Checking account exempt from Reg D"
            TO LS-RD-RESULT-MSG
        MOVE "Y" TO LS-RD-ALLOWED
        GOBACK
    END-IF

    *> ATM and branch channels are exempt from Reg D limits
    IF LS-RD-TXN-CHANNEL = "AT" OR LS-RD-TXN-CHANNEL = "BR"
        MOVE "E0000" TO LS-RD-RESULT-CODE
        MOVE "Channel exempt from Reg D limits"
            TO LS-RD-RESULT-MSG
        MOVE "Y" TO LS-RD-ALLOWED
        GOBACK
    END-IF

    *> Electronic transfers (OL, AC) for savings/MMA
    *> Check if monthly limit already reached
    IF LS-RD-CURRENT-COUNT >= WS-REGD-MONTHLY-LIMIT
        MOVE "E0081" TO LS-RD-RESULT-CODE
        MOVE "Reg D monthly transfer limit exceeded"
            TO LS-RD-RESULT-MSG
        MOVE "N" TO LS-RD-ALLOWED
        MOVE "Y" TO LS-RD-LIMIT-REACHED
        GOBACK
    END-IF

    *> Transfer allowed - increment count
    ADD 1 TO LS-RD-CURRENT-COUNT GIVING WS-NEW-COUNT
    MOVE WS-NEW-COUNT TO LS-RD-NEW-COUNT
    MOVE "E0000" TO LS-RD-RESULT-CODE
    MOVE "Transfer allowed" TO LS-RD-RESULT-MSG
    MOVE "Y" TO LS-RD-ALLOWED

    *> Check if this transfer reaches the limit
    IF WS-NEW-COUNT >= WS-REGD-MONTHLY-LIMIT
        MOVE "Y" TO LS-RD-LIMIT-REACHED
    END-IF

    GOBACK.

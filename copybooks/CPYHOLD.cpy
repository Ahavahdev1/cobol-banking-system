*> ================================================================
*> CPYHOLD.cpy - Deposit Hold Record (Regulation CC)
*> Used by: Deposit processing, available balance calc
*> ================================================================
01  HOLD-RECORD.
    05  HOLD-KEY.
        10  HOLD-ACCT-NUMBER       PIC 9(12).
        10  HOLD-SEQUENCE          PIC 9(5).
    05  HOLD-DETAIL.
        10  HOLD-TYPE              PIC X(2).
        *> CC = Reg CC, AD = Admin, LG = Legal,
        *> LE = Law Enforcement
        10  HOLD-AMOUNT            PIC S9(13)V99.
        10  HOLD-PLACE-DATE        PIC 9(8).
        10  HOLD-RELEASE-DATE      PIC 9(8).
        10  HOLD-REASON-CODE       PIC X(2).
        *> ND = New Deposit, NW = New Account,
        *> LA = Large Amount, RE = Redeposit,
        *> OD = Overdrawn, RP = Repeated Overdraft
        10  HOLD-STATUS            PIC X(1).
        *> A = Active, R = Released, E = Expired
        10  HOLD-TXN-ID            PIC 9(15).
        10  HOLD-PLACED-BY         PIC X(8).
        10  HOLD-OVERRIDE-USER     PIC X(8).

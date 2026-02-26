*> ================================================================
*> CPYTXN.cpy - Transaction Record Layout
*> Used by: Transaction engine, posting, history, statements
*> ================================================================
01  TXN-RECORD.
    05  TXN-KEY.
        10  TXN-ID                 PIC 9(15).
        10  TXN-SEQUENCE           PIC 9(5).
    05  TXN-ROUTING.
        10  TXN-ACCT-NUMBER        PIC 9(12).
        10  TXN-ACCT-CHECK-DIGIT   PIC 9(1).
        10  TXN-BRANCH-ID          PIC 9(4).
        10  TXN-TELLER-ID          PIC X(8).
        10  TXN-TERMINAL-ID        PIC X(8).
        10  TXN-CHANNEL            PIC X(2).
        *> BR = Branch, AT = ATM, OL = Online,
        *> MB = Mobile, AC = ACH, WR = Wire
    05  TXN-DETAIL.
        10  TXN-TYPE               PIC X(3).
        *> DEP = Deposit, WDL = Withdrawal,
        *> XFR = Transfer, PMT = Payment,
        *> FEE = Fee, INT = Interest,
        *> ADJ = Adjustment, REV = Reversal
        10  TXN-CODE               PIC 9(4).
        10  TXN-DESCRIPTION        PIC X(40).
        10  TXN-AMOUNT             PIC S9(13)V99.
        10  TXN-DR-CR              PIC X(1).
        *> D = Debit, C = Credit
        10  TXN-CHECK-NUMBER       PIC 9(8).
        10  TXN-REF-NUMBER         PIC X(20).
    05  TXN-BALANCE-SNAPSHOT.
        10  TXN-BAL-BEFORE         PIC S9(13)V99.
        10  TXN-BAL-AFTER          PIC S9(13)V99.
        10  TXN-AVAIL-BEFORE       PIC S9(13)V99.
        10  TXN-AVAIL-AFTER        PIC S9(13)V99.
    05  TXN-DATETIME.
        10  TXN-POST-DATE          PIC 9(8).
        10  TXN-POST-TIME          PIC 9(6).
        10  TXN-EFFECTIVE-DATE     PIC 9(8).
        10  TXN-VALUE-DATE         PIC 9(8).
    05  TXN-STATUS-INFO.
        10  TXN-STATUS             PIC X(1).
        *> P = Posted, H = Hold, R = Reversed,
        *> E = Error, A = Authorized
        10  TXN-REVERSAL-FLAG      PIC X(1).
        10  TXN-REVERSAL-TXN-ID    PIC 9(15).
        10  TXN-HOLD-RELEASE-DATE  PIC 9(8).
    05  TXN-GL-ENTRIES.
        10  TXN-GL-DR-ACCT         PIC 9(10).
        10  TXN-GL-CR-ACCT         PIC 9(10).
        10  TXN-GL-POST-STATUS     PIC X(1).
    05  TXN-COMPLIANCE.
        10  TXN-CTR-REPORTABLE     PIC X(1).
        *> Y/N - contributes to $10K+ cash threshold
        10  TXN-SAR-FLAG           PIC X(1).
        10  TXN-CASH-AMOUNT        PIC S9(13)V99.
    05  TXN-AUDIT.
        10  TXN-CREATED-TIMESTAMP  PIC 9(14).
        10  TXN-CREATED-USER       PIC X(8).
        10  TXN-APPROVED-USER      PIC X(8).
    05  TXN-CD-INFO.
        10  TXN-CD-EARLY-WD       PIC X(1).
        *> Y = caller acknowledges early withdrawal penalty
        10  TXN-CD-PENALTY-AMT    PIC S9(9)V99.

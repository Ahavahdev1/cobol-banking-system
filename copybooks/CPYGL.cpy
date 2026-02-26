*> ================================================================
*> CPYGL.cpy - General Ledger Account Record
*> Used by: GL posting, trial balance, Call Reports
*> ================================================================
01  GL-RECORD.
    05  GL-KEY.
        10  GL-ACCOUNT-NUM         PIC 9(10).
        10  GL-COST-CENTER         PIC 9(4).
    05  GL-CLASSIFICATION.
        10  GL-ACCT-TYPE           PIC X(1).
        *> A = Asset, L = Liability, E = Equity,
        *> I = Income, X = Expense
        10  GL-ACCT-SUBTYPE        PIC X(4).
        10  GL-ACCT-NAME           PIC X(40).
        10  GL-NORMAL-BALANCE      PIC X(1).
        *> D = Debit, C = Credit
        10  GL-CALL-RPT-LINE       PIC X(6).
    05  GL-BALANCES.
        10  GL-CURRENT-BAL         PIC S9(15)V99.
        10  GL-MTD-DEBITS          PIC S9(15)V99.
        10  GL-MTD-CREDITS         PIC S9(15)V99.
        10  GL-YTD-DEBITS          PIC S9(15)V99.
        10  GL-YTD-CREDITS         PIC S9(15)V99.
        10  GL-PRIOR-MONTH-BAL     PIC S9(15)V99.
        10  GL-PRIOR-YEAR-BAL      PIC S9(15)V99.
        10  GL-BUDGET-BAL          PIC S9(15)V99.
    05  GL-CONTROL-FIELDS.
        10  GL-STATUS              PIC X(1).
        *> A = Active, I = Inactive, F = Frozen
        10  GL-AUTO-POST           PIC X(1).
        10  GL-RECONCILE-TYPE      PIC X(1).
        10  GL-LAST-POST-DATE      PIC 9(8).
        10  GL-LAST-RECON-DATE     PIC 9(8).

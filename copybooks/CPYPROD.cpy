*> ================================================================
*> CPYPROD.cpy - Product Parameter Table Layout
*> Used by: Account opening, interest calc, fee assessment
*> ================================================================
01  PRODUCT-RECORD.
    05  PROD-CODE                 PIC X(4).
    05  PROD-DESCRIPTION          PIC X(30).
    05  PROD-TYPE                 PIC X(1).
    *> D = Deposit, L = Loan, C = Credit Line
    05  PROD-SUB-TYPE             PIC X(2).
    *> CH = Checking, SV = Savings, MM = Money Mkt
    05  PROD-INT-RATE-DEFAULT     PIC 9(3)V9(7).
    05  PROD-INT-RATE-TYPE        PIC X(1).
    05  PROD-INT-CALC-METHOD      PIC X(2).
    05  PROD-INT-ACCRUAL-BASIS    PIC X(1).
    05  PROD-INT-PAY-FREQ         PIC X(1).
    05  PROD-MONTHLY-FEE          PIC 9(5)V99.
    05  PROD-MIN-OPEN-BAL         PIC S9(13)V99.
    05  PROD-MIN-DAILY-BAL        PIC S9(13)V99.
    05  PROD-REGD-APPLICABLE      PIC X(1).
    05  PROD-OD-ELIGIBLE          PIC X(1).
    05  PROD-GL-DEBIT-ACCT        PIC 9(10).
    05  PROD-GL-CREDIT-ACCT       PIC 9(10).
    05  PROD-STATUS               PIC X(1).

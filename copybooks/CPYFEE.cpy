*> ================================================================
*> CPYFEE.cpy - Fee Schedule Record Layout
*> Used by: FEECALC0, account servicing, statement generation
*> ================================================================
01  FEE-SCHEDULE-RECORD.
    05  FEE-PRODUCT-CODE          PIC X(4).
    05  FEE-TYPE                  PIC X(3).
    *> MTH = Monthly maintenance, NSF = NSF/OD fee,
    *> ATM = ATM surcharge, STM = Statement fee
    05  FEE-AMOUNT                PIC 9(5)V99.
    05  FEE-DESCRIPTION           PIC X(30).
    05  FEE-WAIVER-ELIGIBLE       PIC X(1).
    05  FEE-MIN-BAL-THRESHOLD     PIC S9(13)V99.
    05  FEE-DD-WAIVER             PIC X(1).
    05  FEE-EMPLOYEE-WAIVER       PIC X(1).
    05  FEE-AGE-WAIVER-MIN        PIC 9(2).
    05  FEE-AGE-WAIVER-MAX        PIC 9(2).
    05  FEE-NSF-DAILY-MAX         PIC 9(2).
    05  FEE-NSF-DE-MINIMIS        PIC 9(5)V99.
    05  FEE-GL-DEBIT-ACCT         PIC 9(10).
    05  FEE-GL-CREDIT-ACCT        PIC 9(10).
    05  FEE-EFFECTIVE-DATE        PIC 9(8).
    05  FEE-EXPIRY-DATE           PIC 9(8).
    05  FEE-STATUS                PIC X(1).

*> ================================================================
*> CPYACCT.cpy - Account Master Record Layout
*> Used by: Account servicing, posting, interest calc, statements
*> ================================================================
01  ACCT-RECORD.
    05  ACCT-KEY.
        10  ACCT-NUMBER            PIC 9(12).
        10  ACCT-CHECK-DIGIT       PIC 9(1).
    05  ACCT-OWNERSHIP.
        10  ACCT-PRIMARY-CIF       PIC 9(10).
        10  ACCT-JOINT-CIF         PIC 9(10).
        10  ACCT-OWNERSHIP-TYPE    PIC X(2).
        *> IN = Individual, JT = Joint Tenants,
        *> TC = Tenants in Common, PD = Payable on Death,
        *> TR = Trust, CU = Custodial (UTMA)
    05  ACCT-PRODUCT-INFO.
        10  ACCT-PRODUCT-CODE      PIC X(4).
        *> DDA1 = Regular Checking, DDA2 = Interest Checking
        *> SAV1 = Regular Savings, MMA1 = Money Market
        *> CD01-CD99 = Certificates of Deposit
        10  ACCT-TYPE              PIC X(1).
        *> D = Deposit, L = Loan, C = Credit Line
        10  ACCT-SUB-TYPE          PIC X(2).
        *> CH = Checking, SV = Savings, MM = Money Mkt,
        *> CD = Certificate, LN = Installment
    05  ACCT-BALANCE-INFO.
        10  ACCT-LEDGER-BAL        PIC S9(13)V99.
        10  ACCT-AVAIL-BAL         PIC S9(13)V99.
        10  ACCT-COLLECTED-BAL     PIC S9(13)V99.
        10  ACCT-HOLD-AMOUNT       PIC S9(13)V99.
        10  ACCT-PENDING-DR        PIC S9(13)V99.
        10  ACCT-PENDING-CR        PIC S9(13)V99.
        10  ACCT-ACCRUED-INT       PIC S9(11)V9(6).
        *> 6 decimal places for interest precision
        10  ACCT-YTD-INT-EARNED    PIC S9(11)V99.
        10  ACCT-YTD-INT-PAID      PIC S9(11)V99.
        10  ACCT-PTD-INT-EARNED    PIC S9(11)V99.
        10  ACCT-MTD-AVG-BAL       PIC S9(13)V99.
        10  ACCT-MTD-LOW-BAL       PIC S9(13)V99.
    05  ACCT-INTEREST-PARAMS.
        10  ACCT-INT-RATE          PIC 9(3)V9(7).
        *> 7 decimal places: supports rates like 5.1234567%
        10  ACCT-INT-RATE-TYPE     PIC X(1).
        *> F = Fixed, V = Variable, T = Tiered, I = Indexed
        10  ACCT-INT-INDEX-CODE    PIC X(4).
        10  ACCT-INT-INDEX-MARGIN  PIC S9(3)V9(4).
        10  ACCT-INT-CALC-METHOD   PIC X(2).
        *> DB = Daily Balance, AB = Average Balance
        10  ACCT-INT-ACCRUAL-BASIS PIC X(1).
        *> A = Actual/365, B = Actual/360,
        *> C = 30/360, D = Actual/Actual
        10  ACCT-INT-COMPOUND-FREQ PIC X(1).
        10  ACCT-INT-PAY-FREQ      PIC X(1).
        *> M = Monthly, Q = Quarterly, S = Semi,
        *> A = Annual, T = At Maturity
        10  ACCT-INT-NEXT-PAY-DATE PIC 9(8).
    05  ACCT-OVERDRAFT-INFO.
        10  ACCT-OD-PROTECTION     PIC X(1).
        *> N = None, T = Transfer, L = Line of Credit
        10  ACCT-OD-LINKED-ACCT    PIC 9(12).
        10  ACCT-OD-LIMIT          PIC S9(9)V99.
        10  ACCT-OD-OPTED-IN       PIC X(1).
        *> Y/N - Reg E opt-in for ATM/one-time debit
        10  ACCT-NSF-COUNT-MTD     PIC 9(3).
        10  ACCT-NSF-COUNT-YTD     PIC 9(5).
    05  ACCT-FEE-INFO.
        10  ACCT-MONTHLY-FEE       PIC 9(5)V99.
        10  ACCT-FEE-WAIVER-CODE   PIC X(2).
        *> NW = No Waiver, MB = Min Balance,
        *> DD = Direct Deposit, AG = Age, EM = Employee
        10  ACCT-FEE-WAIVER-AMT    PIC S9(9)V99.
        10  ACCT-YTD-FEES-CHARGED  PIC S9(9)V99.
        10  ACCT-YTD-FEES-WAIVED   PIC S9(9)V99.
    05  ACCT-DATES.
        10  ACCT-OPEN-DATE         PIC 9(8).
        10  ACCT-CLOSE-DATE        PIC 9(8).
        10  ACCT-MATURITY-DATE     PIC 9(8).
        10  ACCT-LAST-TXN-DATE     PIC 9(8).
        10  ACCT-LAST-STMT-DATE    PIC 9(8).
        10  ACCT-NEXT-STMT-DATE    PIC 9(8).
        10  ACCT-STMT-CYCLE        PIC 9(2).
    05  ACCT-STATUS-FLAGS.
        10  ACCT-STATUS            PIC X(1).
        *> A = Active, D = Dormant, F = Frozen,
        *> C = Closed, E = Escheat, R = Restricted
        10  ACCT-LEGAL-HOLD        PIC X(1).
        10  ACCT-GARNISHMENT       PIC X(1).
        10  ACCT-DECEASED          PIC X(1).
        10  ACCT-STOP-PAYS-ACTIVE  PIC 9(2).
    05  ACCT-LOAN-SPECIFIC.
        10  ACCT-ORIGINAL-AMT      PIC S9(13)V99.
        10  ACCT-PAYMENT-AMT       PIC S9(9)V99.
        10  ACCT-PAYMENT-FREQ      PIC X(1).
        10  ACCT-NEXT-PMT-DATE     PIC 9(8).
        10  ACCT-REMAINING-TERM    PIC 9(4).
        10  ACCT-ORIGINAL-TERM     PIC 9(4).
        10  ACCT-PAST-DUE-DAYS     PIC 9(4).
        10  ACCT-PAST-DUE-AMT      PIC S9(9)V99.
        10  ACCT-LATE-FEE-ASSESSED PIC X(1).
        10  ACCT-ESCROW-BAL        PIC S9(9)V99.
        10  ACCT-COLLATERAL-CODE   PIC X(2).
    05  ACCT-CD-SPECIFIC.
        10  ACCT-CD-TERM-MONTHS    PIC 9(3).
        10  ACCT-CD-RENEWAL-TYPE   PIC X(1).
        10  ACCT-CD-EARLY-WD-PEN   PIC 9(3).
        10  ACCT-CD-GRACE-DAYS     PIC 9(2).
    05  ACCT-AUDIT-FIELDS.
        10  ACCT-CREATED-DATE      PIC 9(8).
        10  ACCT-CREATED-USER      PIC X(8).
        10  ACCT-MODIFIED-DATE     PIC 9(8).
        10  ACCT-MODIFIED-TIME     PIC 9(6).
        10  ACCT-MODIFIED-USER     PIC X(8).

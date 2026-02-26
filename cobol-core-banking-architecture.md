# COBOL Core Banking System — Full Architecture

## Project Name: **VAULT-CBS** (Core Banking System)

---

## 1. System Overview

VAULT-CBS is a production-grade core banking system written in COBOL, designed for community banks and credit unions with up to $10B in assets. It handles all critical banking functions: deposit and loan account management, transaction processing, interest accrual, fee assessment, ACH/wire processing, regulatory reporting, and general ledger accounting.

### Design Philosophy

- **Double-entry accounting everywhere** — every transaction touches the GL with balanced debits and credits
- **Fixed-point decimal arithmetic** — `PIC 9` with implied decimals, no floating-point anywhere
- **Batch + Online hybrid** — CICS screens for teller/CSR operations, batch JCL for EOD/EOM/EOY cycles
- **Audit-complete** — every state change is logged with before/after images
- **Regulation-first** — BSA/AML, Reg D, Reg E, Reg CC, Truth in Lending, Truth in Savings baked in

### Technology Stack

| Layer | Technology |
|-------|-----------|
| Language | COBOL 85 / Enterprise COBOL |
| Online Processing | CICS Transaction Server |
| Batch Processing | JCL + JES2 |
| Data Storage | DB2 (primary) + VSAM (indexed files for hot-path lookups) |
| Message Queuing | MQ Series (for ACH/wire/external interfaces) |
| Reporting | COBOL + DFSORT for flat-file reports |
| Security | RACF (mainframe) or equivalent ACL layer |

---

## 2. Module Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                        VAULT-CBS                                │
│                                                                 │
│  ┌──────────┐ ┌──────────┐ ┌──────────┐ ┌──────────┐          │
│  │   CIF    │ │ Deposits │ │  Loans   │ │    GL    │          │
│  │ Module   │ │  Module  │ │  Module  │ │  Module  │          │
│  └────┬─────┘ └────┬─────┘ └────┬─────┘ └────┬─────┘          │
│       │            │            │            │                  │
│  ┌────┴────────────┴────────────┴────────────┴─────┐           │
│  │           TRANSACTION ENGINE (TXN-ENG)          │           │
│  │  ┌─────────┐ ┌─────────┐ ┌─────────┐           │           │
│  │  │ Journal │ │ Posting │ │  Auth   │           │           │
│  │  │  Layer  │ │  Layer  │ │  Layer  │           │           │
│  │  └─────────┘ └─────────┘ └─────────┘           │           │
│  └──────────────────────┬──────────────────────────┘           │
│                         │                                       │
│  ┌──────────┐ ┌─────────┴──┐ ┌──────────┐ ┌──────────┐        │
│  │   ACH    │ │   Wire     │ │   Fee    │ │ Interest │        │
│  │ Processor│ │  Transfer  │ │  Engine  │ │  Engine  │        │
│  └──────────┘ └────────────┘ └──────────┘ └──────────┘        │
│                                                                 │
│  ┌──────────┐ ┌──────────┐ ┌──────────┐ ┌──────────┐          │
│  │  Batch   │ │ Reg/AML  │ │ Stmt Gen │ │  Audit   │          │
│  │ Scheduler│ │ Compliance│ │  Module  │ │  Trail   │          │
│  └──────────┘ └──────────┘ └──────────┘ └──────────┘          │
└─────────────────────────────────────────────────────────────────┘
```

---

## 3. Data Architecture

### 3.1 Copybook Library

Copybooks are the backbone — every data structure is defined once and included everywhere. This is COBOL's version of shared type definitions.

#### CPYCIF.cpy — Customer Information File

```cobol
      *================================================================*
      * CPYCIF.cpy - Customer Information File Record Layout
      * Used by: CIF maintenance, account opening, KYC, BSA/AML
      *================================================================*
       01  CIF-RECORD.
           05  CIF-KEY.
               10  CIF-CUST-ID            PIC 9(10).
           05  CIF-PERSONAL-INFO.
               10  CIF-NAME-LAST          PIC X(30).
               10  CIF-NAME-FIRST         PIC X(20).
               10  CIF-NAME-MIDDLE        PIC X(20).
               10  CIF-NAME-SUFFIX        PIC X(5).
               10  CIF-SSN-TIN            PIC 9(9).
               10  CIF-SSN-TYPE           PIC X(1).
      *            S = SSN, E = EIN, I = ITIN
               10  CIF-DOB                PIC 9(8).
      *            YYYYMMDD
               10  CIF-CUST-TYPE          PIC X(1).
      *            I = Individual, J = Joint, B = Business,
      *            T = Trust, E = Estate
           05  CIF-CONTACT-INFO.
               10  CIF-ADDR-LINE1         PIC X(40).
               10  CIF-ADDR-LINE2         PIC X(40).
               10  CIF-CITY               PIC X(30).
               10  CIF-STATE              PIC X(2).
               10  CIF-ZIP                PIC 9(9).
               10  CIF-COUNTRY            PIC X(3).
      *            ISO 3166-1 alpha-3
               10  CIF-PHONE-PRIMARY      PIC 9(10).
               10  CIF-PHONE-MOBILE       PIC 9(10).
               10  CIF-EMAIL              PIC X(60).
           05  CIF-REGULATORY-INFO.
               10  CIF-CIP-VERIFIED       PIC X(1).
      *            Y/N — Customer Identification Program
               10  CIF-CIP-VERIFY-DATE    PIC 9(8).
               10  CIF-CIP-DOC-TYPE       PIC X(2).
      *            DL = Driver License, PP = Passport,
      *            MI = Military ID, SI = State ID
               10  CIF-CIP-DOC-NUMBER     PIC X(25).
               10  CIF-CIP-DOC-EXPIRY     PIC 9(8).
               10  CIF-OFAC-CHECK-DATE    PIC 9(8).
               10  CIF-OFAC-STATUS        PIC X(1).
      *            C = Clear, H = Hold, M = Match
               10  CIF-BSA-RISK-RATING    PIC 9(1).
      *            1 = Low, 2 = Medium, 3 = High, 4 = Prohibited
               10  CIF-CTR-EXEMPT         PIC X(1).
      *            Y/N — Currency Transaction Report exemption
           05  CIF-RELATIONSHIP-INFO.
               10  CIF-OPEN-DATE          PIC 9(8).
               10  CIF-LAST-CONTACT-DATE  PIC 9(8).
               10  CIF-STATUS             PIC X(1).
      *            A = Active, I = Inactive, C = Closed,
      *            D = Deceased, F = Frozen
               10  CIF-BRANCH-ID          PIC 9(4).
               10  CIF-OFFICER-ID         PIC 9(6).
               10  CIF-NUM-ACCOUNTS       PIC 9(3).
           05  CIF-AUDIT-FIELDS.
               10  CIF-CREATED-DATE       PIC 9(8).
               10  CIF-CREATED-TIME       PIC 9(6).
               10  CIF-CREATED-USER       PIC X(8).
               10  CIF-MODIFIED-DATE      PIC 9(8).
               10  CIF-MODIFIED-TIME      PIC 9(6).
               10  CIF-MODIFIED-USER      PIC X(8).
```

#### CPYACCT.cpy — Account Master Record

```cobol
      *================================================================*
      * CPYACCT.cpy - Account Master Record Layout
      * Used by: Account servicing, posting, interest calc, statements
      *================================================================*
       01  ACCT-RECORD.
           05  ACCT-KEY.
               10  ACCT-NUMBER            PIC 9(12).
               10  ACCT-CHECK-DIGIT       PIC 9(1).
           05  ACCT-OWNERSHIP.
               10  ACCT-PRIMARY-CIF       PIC 9(10).
               10  ACCT-JOINT-CIF         PIC 9(10).
               10  ACCT-OWNERSHIP-TYPE    PIC X(2).
      *            IN = Individual, JT = Joint Tenants,
      *            TC = Tenants in Common, PD = Payable on Death,
      *            TR = Trust, CU = Custodial (UTMA)
           05  ACCT-PRODUCT-INFO.
               10  ACCT-PRODUCT-CODE      PIC X(4).
      *            DDA1 = Regular Checking
      *            DDA2 = Interest Checking
      *            SAV1 = Regular Savings
      *            MMA1 = Money Market
      *            CD01-CD99 = Certificates of Deposit
      *            LOC1 = Line of Credit
      *            AUTO = Auto Loan
      *            MTGE = Mortgage
      *            COMM = Commercial Loan
               10  ACCT-TYPE              PIC X(1).
      *            D = Deposit, L = Loan, C = Credit Line
               10  ACCT-SUB-TYPE          PIC X(2).
      *            CH = Checking, SV = Savings, MM = Money Mkt,
      *            CD = Certificate, LN = Installment,
      *            MG = Mortgage, LC = Line of Credit
           05  ACCT-BALANCE-INFO.
               10  ACCT-LEDGER-BAL        PIC S9(13)V99.
      *            Current book balance
               10  ACCT-AVAIL-BAL         PIC S9(13)V99.
      *            Available balance (after holds)
               10  ACCT-COLLECTED-BAL     PIC S9(13)V99.
      *            Collected (cleared) funds balance
               10  ACCT-HOLD-AMOUNT       PIC S9(13)V99.
      *            Total holds on account
               10  ACCT-PENDING-DR        PIC S9(13)V99.
      *            Pending debits not yet posted
               10  ACCT-PENDING-CR        PIC S9(13)V99.
      *            Pending credits not yet posted
               10  ACCT-ACCRUED-INT       PIC S9(11)V9(6).
      *            6 decimal places for interest precision
               10  ACCT-YTD-INT-EARNED    PIC S9(11)V99.
               10  ACCT-YTD-INT-PAID      PIC S9(11)V99.
               10  ACCT-PTD-INT-EARNED    PIC S9(11)V99.
      *            Period-to-date (for statement cycle)
               10  ACCT-MTD-AVG-BAL       PIC S9(13)V99.
               10  ACCT-MTD-LOW-BAL       PIC S9(13)V99.
           05  ACCT-INTEREST-PARAMS.
               10  ACCT-INT-RATE          PIC 9(3)V9(7).
      *            7 decimal places: supports rates like 5.1234567%
               10  ACCT-INT-RATE-TYPE     PIC X(1).
      *            F = Fixed, V = Variable, T = Tiered, I = Indexed
               10  ACCT-INT-INDEX-CODE    PIC X(4).
      *            PRIM = Prime, SOFR = SOFR, TRES = Treasury
               10  ACCT-INT-INDEX-MARGIN  PIC S9(3)V9(4).
      *            +/- margin over index rate
               10  ACCT-INT-CALC-METHOD   PIC X(2).
      *            DB = Daily Balance, AB = Average Balance,
      *            TB = Tiered Balance, TP = Tiered + Blended
               10  ACCT-INT-ACCRUAL-BASIS PIC X(1).
      *            A = Actual/365, B = Actual/360,
      *            C = 30/360, D = Actual/Actual
               10  ACCT-INT-COMPOUND-FREQ PIC X(1).
      *            D = Daily, M = Monthly, Q = Quarterly,
      *            S = Semi-Annual, A = Annual
               10  ACCT-INT-PAY-FREQ      PIC X(1).
      *            M = Monthly, Q = Quarterly, S = Semi,
      *            A = Annual, T = At Maturity
               10  ACCT-INT-NEXT-PAY-DATE PIC 9(8).
           05  ACCT-OVERDRAFT-INFO.
               10  ACCT-OD-PROTECTION     PIC X(1).
      *            N = None, T = Transfer, L = Line of Credit
               10  ACCT-OD-LINKED-ACCT    PIC 9(12).
               10  ACCT-OD-LIMIT          PIC S9(9)V99.
               10  ACCT-OD-OPTED-IN       PIC X(1).
      *            Y/N — Reg E opt-in for ATM/one-time debit
               10  ACCT-NSF-COUNT-MTD     PIC 9(3).
               10  ACCT-NSF-COUNT-YTD     PIC 9(5).
           05  ACCT-FEE-INFO.
               10  ACCT-MONTHLY-FEE       PIC 9(5)V99.
               10  ACCT-FEE-WAIVER-CODE   PIC X(2).
      *            NW = No Waiver, MB = Min Balance,
      *            DD = Direct Deposit, AG = Age, EM = Employee
               10  ACCT-FEE-WAIVER-AMT    PIC S9(9)V99.
      *            Balance threshold for waiver
               10  ACCT-YTD-FEES-CHARGED  PIC S9(9)V99.
               10  ACCT-YTD-FEES-WAIVED   PIC S9(9)V99.
           05  ACCT-DATES.
               10  ACCT-OPEN-DATE         PIC 9(8).
               10  ACCT-CLOSE-DATE        PIC 9(8).
               10  ACCT-MATURITY-DATE     PIC 9(8).
      *            CDs and loans
               10  ACCT-LAST-TXN-DATE     PIC 9(8).
               10  ACCT-LAST-STMT-DATE    PIC 9(8).
               10  ACCT-NEXT-STMT-DATE    PIC 9(8).
               10  ACCT-STMT-CYCLE        PIC 9(2).
      *            Day of month for statement cutoff
           05  ACCT-STATUS-FLAGS.
               10  ACCT-STATUS            PIC X(1).
      *            A = Active, D = Dormant, F = Frozen,
      *            C = Closed, E = Escheat, R = Restricted
               10  ACCT-LEGAL-HOLD        PIC X(1).
               10  ACCT-GARNISHMENT       PIC X(1).
               10  ACCT-DECEASED          PIC X(1).
               10  ACCT-STOP-PAYS-ACTIVE  PIC 9(2).
           05  ACCT-LOAN-SPECIFIC.
               10  ACCT-ORIGINAL-AMT      PIC S9(13)V99.
               10  ACCT-PAYMENT-AMT       PIC S9(9)V99.
               10  ACCT-PAYMENT-FREQ      PIC X(1).
      *            M = Monthly, B = Bi-Weekly, W = Weekly
               10  ACCT-NEXT-PMT-DATE     PIC 9(8).
               10  ACCT-REMAINING-TERM    PIC 9(4).
      *            Months remaining
               10  ACCT-ORIGINAL-TERM     PIC 9(4).
               10  ACCT-PAST-DUE-DAYS     PIC 9(4).
               10  ACCT-PAST-DUE-AMT      PIC S9(9)V99.
               10  ACCT-LATE-FEE-ASSESSED PIC X(1).
               10  ACCT-ESCROW-BAL        PIC S9(9)V99.
               10  ACCT-COLLATERAL-CODE   PIC X(2).
           05  ACCT-CD-SPECIFIC.
               10  ACCT-CD-TERM-MONTHS    PIC 9(3).
               10  ACCT-CD-RENEWAL-TYPE   PIC X(1).
      *            A = Auto-renew, N = No renew, P = Partial
               10  ACCT-CD-EARLY-WD-PEN   PIC 9(3).
      *            Days of interest penalty
               10  ACCT-CD-GRACE-DAYS     PIC 9(2).
           05  ACCT-AUDIT-FIELDS.
               10  ACCT-CREATED-DATE      PIC 9(8).
               10  ACCT-CREATED-USER      PIC X(8).
               10  ACCT-MODIFIED-DATE     PIC 9(8).
               10  ACCT-MODIFIED-TIME     PIC 9(6).
               10  ACCT-MODIFIED-USER     PIC X(8).
```

#### CPYTXN.cpy — Transaction Record

```cobol
      *================================================================*
      * CPYTXN.cpy - Transaction Record Layout
      * Used by: Transaction engine, posting, history, statements
      *================================================================*
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
      *            BR = Branch, AT = ATM, OL = Online,
      *            MB = Mobile, AC = ACH, WR = Wire,
      *            CK = Check, IN = Internal
           05  TXN-DETAIL.
               10  TXN-TYPE               PIC X(3).
      *            DEP = Deposit, WDL = Withdrawal,
      *            XFR = Transfer, PMT = Payment,
      *            FEE = Fee, INT = Interest,
      *            ADJ = Adjustment, REV = Reversal,
      *            CHK = Check, ACH = ACH,
      *            WIR = Wire, ATM = ATM,
      *            POS = Point of Sale, DDR = Direct Debit
               10  TXN-CODE               PIC 9(4).
      *            Detailed transaction code per institution
               10  TXN-DESCRIPTION        PIC X(40).
               10  TXN-AMOUNT             PIC S9(13)V99.
               10  TXN-DR-CR              PIC X(1).
      *            D = Debit, C = Credit
               10  TXN-CHECK-NUMBER       PIC 9(8).
               10  TXN-REF-NUMBER         PIC X(20).
      *            External reference (ACH trace, wire ref, etc.)
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
      *            For Reg CC hold calculations
           05  TXN-STATUS-INFO.
               10  TXN-STATUS             PIC X(1).
      *            P = Posted, H = Hold, R = Reversed,
      *            E = Error, A = Authorized (not yet posted)
               10  TXN-REVERSAL-FLAG      PIC X(1).
               10  TXN-REVERSAL-TXN-ID    PIC 9(15).
               10  TXN-HOLD-RELEASE-DATE  PIC 9(8).
           05  TXN-GL-ENTRIES.
               10  TXN-GL-DR-ACCT         PIC 9(10).
               10  TXN-GL-CR-ACCT         PIC 9(10).
               10  TXN-GL-POST-STATUS     PIC X(1).
      *            P = Posted, U = Unposted, S = Suspense
           05  TXN-COMPLIANCE.
               10  TXN-CTR-REPORTABLE     PIC X(1).
      *            Y/N — contributes to $10K+ cash threshold
               10  TXN-SAR-FLAG           PIC X(1).
               10  TXN-CASH-AMOUNT        PIC S9(13)V99.
      *            Cash portion of mixed transactions
           05  TXN-AUDIT.
               10  TXN-CREATED-TIMESTAMP  PIC 9(14).
      *            YYYYMMDDHHMMSS
               10  TXN-CREATED-USER       PIC X(8).
               10  TXN-APPROVED-USER      PIC X(8).
      *            Dual control for large transactions
```

#### CPYGL.cpy — General Ledger Record

```cobol
      *================================================================*
      * CPYGL.cpy - General Ledger Account Record
      * Used by: GL posting, trial balance, Call Reports
      *================================================================*
       01  GL-RECORD.
           05  GL-KEY.
               10  GL-ACCOUNT-NUM         PIC 9(10).
               10  GL-COST-CENTER         PIC 9(4).
           05  GL-CLASSIFICATION.
               10  GL-ACCT-TYPE           PIC X(1).
      *            A = Asset, L = Liability, E = Equity,
      *            I = Income, X = Expense
               10  GL-ACCT-SUBTYPE        PIC X(4).
               10  GL-ACCT-NAME           PIC X(40).
               10  GL-NORMAL-BALANCE      PIC X(1).
      *            D = Debit, C = Credit
               10  GL-CALL-RPT-LINE       PIC X(6).
      *            Maps to FFIEC Call Report line item
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
      *            A = Active, I = Inactive, F = Frozen
               10  GL-AUTO-POST           PIC X(1).
      *            Y/N — accepts automated postings
               10  GL-RECONCILE-TYPE      PIC X(1).
      *            N = None, D = Daily, M = Monthly
               10  GL-LAST-POST-DATE      PIC 9(8).
               10  GL-LAST-RECON-DATE     PIC 9(8).
```

#### CPYHOLD.cpy — Hold/Float Record (Reg CC)

```cobol
      *================================================================*
      * CPYHOLD.cpy - Deposit Hold Record (Regulation CC)
      * Used by: Deposit processing, available balance calc
      *================================================================*
       01  HOLD-RECORD.
           05  HOLD-KEY.
               10  HOLD-ACCT-NUMBER       PIC 9(12).
               10  HOLD-SEQUENCE          PIC 9(5).
           05  HOLD-DETAIL.
               10  HOLD-TYPE              PIC X(2).
      *            CC = Reg CC, AD = Admin, LG = Legal,
      *            LE = Law Enforcement
               10  HOLD-AMOUNT            PIC S9(13)V99.
               10  HOLD-PLACE-DATE        PIC 9(8).
               10  HOLD-RELEASE-DATE      PIC 9(8).
               10  HOLD-REASON-CODE       PIC X(2).
      *            ND = New Deposit, NW = New Account,
      *            LA = Large Amount (>$5,525),
      *            RE = Redeposit, OD = Overdrawn,
      *            RP = Repeated Overdraft, EM = Emergency
               10  HOLD-STATUS            PIC X(1).
      *            A = Active, R = Released, E = Expired
               10  HOLD-TXN-ID            PIC 9(15).
               10  HOLD-PLACED-BY         PIC X(8).
               10  HOLD-OVERRIDE-USER     PIC X(8).
```

### 3.2 Chart of Accounts (GL Structure)

A real bank's GL maps to FFIEC Call Report line items. Here's the skeleton:

```
ASSET ACCOUNTS (1000-3999)
├── 1000-1099  Cash and Due From Banks
│   ├── 1010  Cash in Vault
│   ├── 1020  Cash at ATMs
│   ├── 1030  Due from Fed Reserve
│   └── 1040  Due from Correspondent Banks
├── 1100-1199  Securities
│   ├── 1110  US Treasury Securities
│   ├── 1120  Agency Securities
│   └── 1130  Municipal Securities
├── 1200-1299  Federal Funds Sold
├── 2000-2499  Loans
│   ├── 2010  Consumer Loans - Auto
│   ├── 2020  Consumer Loans - Personal
│   ├── 2100  Real Estate - Residential 1-4 Family
│   ├── 2200  Real Estate - Commercial
│   ├── 2300  Commercial & Industrial Loans
│   └── 2400  Agricultural Loans
├── 2500-2599  Allowance for Loan Losses (contra)
├── 3000-3099  Bank Premises & Equipment
└── 3500-3999  Other Assets / Accrued Interest Receivable

LIABILITY ACCOUNTS (4000-5999)
├── 4000-4499  Deposits
│   ├── 4010  DDA - Non-Interest Bearing
│   ├── 4020  DDA - Interest Bearing (NOW)
│   ├── 4030  Savings Deposits
│   ├── 4040  Money Market Deposits
│   └── 4100-4199  Time Deposits (CDs) by maturity band
├── 4500-4599  Federal Funds Purchased
├── 5000-5099  Borrowings (FHLB, etc.)
└── 5500-5999  Other Liabilities / Accrued Interest Payable

EQUITY ACCOUNTS (6000-6999)
├── 6010  Common Stock
├── 6020  Surplus
├── 6030  Retained Earnings
└── 6040  AOCI (Accumulated Other Comprehensive Income)

INCOME ACCOUNTS (7000-7999)
├── 7010  Interest Income - Loans
├── 7020  Interest Income - Securities
├── 7030  Interest Income - Fed Funds
├── 7500  Non-Interest Income - Service Charges
├── 7510  Non-Interest Income - NSF/OD Fees
├── 7520  Non-Interest Income - ATM Fees
└── 7530  Non-Interest Income - Wire Fees

EXPENSE ACCOUNTS (8000-8999)
├── 8010  Interest Expense - Deposits
├── 8020  Interest Expense - Borrowings
├── 8500  Salaries and Benefits
├── 8600  Occupancy Expense
├── 8700  Provision for Loan Losses
└── 8800-8999  Other Operating Expense
```

---

## 4. Core Programs

### 4.1 Transaction Engine (TXN-ENG)

This is the heart of the system. Every financial event flows through this engine.

#### Program: TXNPOST0 — Transaction Posting Master

```
INPUTS:
  - Transaction request (CPYTXN format)
  - Account master (CPYACCT)
  - GL chart of accounts (CPYGL)

PROCESSING FLOW:
  1. VALIDATE-TRANSACTION
     - Account exists and is active
     - Transaction type valid for account type
     - Amount within limits (teller authority, Reg D counts, etc.)
     - Dual control check (amounts > threshold need approval)
     - Check for legal holds, garnishments, deceased flags
     
  2. AUTHORIZE-TRANSACTION
     - For debits: check available balance (NOT ledger balance)
     - For credits: apply Reg CC hold schedule
     - Overdraft logic:
       a. Check OD protection link
       b. If opt-in (Reg E), allow up to OD limit
       c. If no opt-in, decline ATM/POS, allow checks/ACH per policy
     - For Reg D accounts: check 6-transfer limit (savings/MMA)
     
  3. POST-TRANSACTION
     - Update account ledger balance
     - Update available balance (apply/release holds)
     - Update collected balance
     - Write transaction history record
     - Capture before/after balance snapshots
     
  4. POST-GL-ENTRIES
     - Determine GL debit and credit accounts from TXN-CODE
     - Write balanced GL journal entries
     - Example: Cash deposit to checking:
       DR 1010 (Cash in Vault)     $500.00
       CR 4010 (DDA Deposits)      $500.00
     
  5. COMPLIANCE-CHECK
     - Accumulate daily cash totals per customer
     - If cash >= $10,000: flag for CTR generation
     - Pattern detection for structuring (multiple txns just under $10K)
     - SAR trigger rules (unusual activity patterns)
     
  6. COMMIT-OR-ROLLBACK
     - If any step fails, reverse all changes
     - Write to exception/suspense if partial completion

OUTPUTS:
  - Updated account record
  - Transaction history record
  - GL journal entries
  - Compliance flags
  - Audit trail record
```

#### Program: TXNREV0 — Transaction Reversal

```
PURPOSE: Reverse a previously posted transaction
CONSTRAINTS:
  - Must maintain audit trail (never delete, only reverse)
  - Creates new transaction with opposite sign
  - Links reversal TXN-ID to original
  - Reverses GL entries
  - Cannot reverse already-reversed transactions
  - May require supervisor approval based on amount/age
```

### 4.2 Interest Engine

#### Program: INTCALC0 — Daily Interest Accrual

```
RUNS: Nightly batch (EOD cycle)

FOR EACH ACTIVE ACCOUNT WITH INTEREST:
  1. DETERMINE-RATE
     - Fixed: use stored rate directly
     - Variable: fetch current index rate, add margin
     - Tiered: determine tier based on balance
       Tier table example (savings):
         $0.00     - $9,999.99      → 0.50%
         $10,000   - $49,999.99     → 1.00%
         $50,000   - $99,999.99     → 1.50%
         $100,000+                  → 2.00%
     
  2. CALCULATE-DAILY-INTEREST
     Based on ACCT-INT-ACCRUAL-BASIS:
     
     Actual/365:
       Daily Interest = Balance × (Rate / 365)
     
     Actual/360:
       Daily Interest = Balance × (Rate / 360)
     
     30/360:
       Daily Interest = Balance × (Rate / 360)
       (Each month treated as 30 days)
     
     CRITICAL: All calculations use PIC S9(11)V9(6)
     Six decimal places carried until payment rounding
     
  3. DETERMINE-BALANCE-FOR-CALC
     Daily Balance method:
       Use end-of-day ledger balance
     
     Average Daily Balance method:
       Accumulate running balance sum
       At payment: divide by days in period
       
  4. ACCRUE-INTEREST
     - Add daily accrual to ACCT-ACCRUED-INT
     - Update ACCT-PTD-INT-EARNED
     - DO NOT round until payment posting
     
  5. CHECK-PAYMENT-DATE
     If today = ACCT-INT-NEXT-PAY-DATE:
       - Round accrued interest to 2 decimal places
       - For deposits: post credit to account
       - For loans: apply to interest portion of payment
       - Reset accrual accumulator
       - Calculate next payment date
       - Write GL entries:
         Deposit interest payment:
           DR 8010 (Interest Expense - Deposits)
           CR account (customer deposit)
         Loan interest accrual:
           DR 3520 (Accrued Interest Receivable)
           CR 7010 (Interest Income - Loans)
```

#### Program: INTRATE0 — Rate Index Management

```
PURPOSE: Maintain index rates for variable-rate products
INDEXES SUPPORTED:
  - PRIM — Wall Street Journal Prime Rate
  - SOFR — Secured Overnight Financing Rate
  - TRES — Treasury rates by maturity (1mo, 3mo, 6mo, 1yr, 2yr, 5yr, 10yr)
  - LIBR — Legacy LIBOR (sunset references)
  
DAILY UPDATE PROCESS:
  1. Receive rate feed (flat file from data vendor)
  2. Validate rates within tolerance (flag >50bp daily move)
  3. Update rate table
  4. Log rate history for audit
  5. Trigger variable-rate account recalculation if rate changed
```

### 4.3 ACH Processing Module

#### Program: ACHRECV0 — ACH Incoming File Processor

```
INPUT: NACHA-format ACH file from Federal Reserve / correspondent bank

NACHA FILE STRUCTURE:
  File Header (1 record)
  ├── Batch Header (per originator)
  │   ├── Entry Detail Records
  │   │   └── Addenda Records (optional)
  │   └── Batch Control
  └── File Control

PROCESSING:
  1. PARSE-FILE-HEADER
     - Validate immediate origin/destination routing numbers
     - Check file creation date/sequence
     - Reject duplicate files
     
  2. FOR EACH BATCH:
     a. Validate batch header
     b. Determine Standard Entry Class (SEC) code:
        - PPD: Prearranged Payment & Deposit (payroll, bill pay)
        - CCD: Corporate Credit or Debit
        - WEB: Internet-initiated entries
        - TEL: Telephone-initiated entries
        - CTX: Corporate Trade Exchange
        
     c. FOR EACH ENTRY DETAIL:
        - Match routing/account number to internal account
        - Validate account status (active, not frozen)
        - Check for ACH stop payments (originator + amount)
        - Apply Reg E rules for unauthorized debits
        - Post transaction via TXN-ENG
        - Handle return items (R01-R85 return reason codes)
        
     d. Validate batch control totals
     
  3. VALIDATE-FILE-CONTROL
     - Hash totals match
     - Entry/addenda counts match
     - Total debits/credits balance
     
  4. GENERATE-RETURN-FILE
     - Create NACHA return file for rejected entries
     - Return reason codes:
       R01 = Insufficient Funds
       R02 = Account Closed
       R03 = No Account/Unable to Locate
       R04 = Invalid Account Number
       R08 = Payment Stopped
       R10 = Customer Advises Unauthorized
       R29 = Corporate Customer Advises Not Authorized
```

#### Program: ACHSEND0 — ACH Origination

```
PURPOSE: Create outgoing NACHA files for:
  - Direct deposit (payroll clients)
  - Loan payment collection
  - Bill pay origination
  
OUTPUT: NACHA-compliant flat file for Fed submission
```

### 4.4 Wire Transfer Module

#### Program: WIRPROC0 — Wire Transfer Processing

```
PROCESSING:
  1. VALIDATE-WIRE-REQUEST
     - Verify originator account has sufficient collected funds
     - Check OFAC sanctions list (mandatory for all wires)
     - Verify beneficiary info completeness
     - Apply wire transfer limits (per transaction, daily)
     - Dual control: require two approvals for amounts > threshold
     
  2. DEBIT-ORIGINATOR
     - Post debit via TXN-ENG
     - Charge wire fee
     - GL: DR Customer Account / CR Due from Correspondent
     
  3. GENERATE-FEDWIRE-MESSAGE
     - Format per Federal Reserve Fedwire Funds Service specs
     - Include required fields:
       Type/Subtype Code
       Sender ABA, Receiver ABA
       Beneficiary info (name, account, bank)
       Originator info
       OBI (Originator to Beneficiary Information)
     
  4. INCOMING-WIRE-PROCESSING
     - Parse incoming Fedwire message
     - Match beneficiary account
     - Post credit via TXN-ENG (immediate availability)
     - Notify customer (if enabled)
     - CTR check on incoming wire amounts
```

### 4.5 Fee Engine

#### Program: FEECALC0 — Monthly Fee Assessment

```
RUNS: Monthly batch (per account statement cycle date)

FEE TYPES:
  - Monthly maintenance fee
  - Per-check/transaction fees
  - NSF/OD fees (assessed real-time by TXN-ENG)
  - ATM surcharge fees
  - Statement fees (paper statements)
  - Wire transfer fees (assessed real-time)
  - Safe deposit box rental (annual)
  - Account analysis fees (commercial)

WAIVER LOGIC:
  FOR EACH FEE-ELIGIBLE ACCOUNT:
    1. Calculate base fee from product schedule
    2. Check waiver conditions:
       - Minimum balance maintained? (daily or average)
       - Direct deposit received this cycle?
       - Combined relationship balance meets threshold?
       - Age-based waiver (student, senior)?
       - Employee account?
    3. If waiver conditions met:
       - Waive fee
       - Increment ACCT-YTD-FEES-WAIVED
    4. If not waived:
       - Post fee debit via TXN-ENG
       - GL: DR Customer Account / CR 7500 (Service Charges)
       - Increment ACCT-YTD-FEES-CHARGED
       
NSF/OD FEE RULES:
  - Max N fees per day (configurable, e.g., 3-5)
  - Max fees per rolling 12-month period
  - De minimis threshold (no fee if OD amount < $5)
  - Grace period for end-of-day cure
  - Daily OD fee after N consecutive days overdrawn
```

---

## 5. Batch Processing Framework

### 5.1 End-of-Day (EOD) Cycle

```
JOB STREAM: VAULT-EOD (runs nightly after business close)

STEP 01: EODPREP0 — Preparation
  - Set processing date
  - Verify all online transactions committed
  - Take backup checkpoint

STEP 02: TXNPOST0 — Post Pending Transactions
  - Post all authorized-but-unposted transactions
  - Process memo-post to hard-post conversion

STEP 03: ACHRECV0 — Process Incoming ACH
  - Parse Fed ACH file
  - Post credits and debits
  - Generate return items

STEP 04: HOLDREL0 — Release Expired Holds
  - Check all active holds against Reg CC schedule
  - Release holds past release date
  - Recalculate available balances

STEP 05: INTCALC0 — Daily Interest Accrual
  - Calculate and accrue interest on all eligible accounts
  - Post interest payments where due

STEP 06: ODMGMT0 — Overdraft Management
  - Identify accounts overdrawn at EOD
  - Attempt OD protection transfers
  - Assess OD fees (per daily limits)
  - Generate OD notices

STEP 07: GLPOST0 — GL Posting & Balancing
  - Post all unposted GL journal entries
  - Verify trial balance (total debits = total credits)
  - If out of balance: HALT and alert — manual intervention required

STEP 08: CTRCALC0 — CTR Aggregation
  - Sum daily cash transactions per customer
  - Generate CTR filings for amounts >= $10,000
  - Flag suspicious patterns for SAR review

STEP 09: EODCOMP0 — Completion
  - Update processing date
  - Archive transaction logs
  - Generate EOD summary reports
  - Release checkpoint

TOTAL ESTIMATED JCL STEPS: 25-30 (including sorts, file copies, backups)
```

### 5.2 End-of-Month (EOM) Cycle

```
JOB STREAM: VAULT-EOM (runs after last EOD of month)

STEP 01: STMTGEN0 — Statement Generation
  - FOR EACH ACCOUNT with statement cycle ending this month:
    - Gather all transactions for the period
    - Calculate period summary (opening bal, credits, debits, 
      fees, interest, ending bal)
    - Calculate average daily balance
    - Generate statement image (print file or PDF data)
    - Update ACCT-LAST-STMT-DATE / ACCT-NEXT-STMT-DATE

STEP 02: FEECALC0 — Monthly Fee Assessment
  - Assess maintenance fees
  - Apply waivers
  - Post fee transactions

STEP 03: DORMCHK0 — Dormancy Check
  - Flag accounts with no customer-initiated activity > 12 months
  - Change status to Dormant
  - Generate dormancy notices
  - Begin escheatment countdown per state requirements

STEP 04: GLCLOSE0 — Monthly GL Close
  - Generate trial balance report
  - Calculate MTD income/expense totals
  - Roll MTD accumulators to YTD
  - Reset MTD fields
  - Store prior month balances

STEP 05: REGDCHK0 — Reg D Monitoring
  - Reset monthly transfer counters for savings/MMA
  - Flag accounts that exceeded 6-transfer limit
  - Generate conversion notices if applicable

STEP 06: RPTGEN0 — Management Reports
  - Deposit composition report
  - Loan delinquency report
  - Concentration report (large deposits)
  - Rate sensitivity / GAP analysis data
  - Branch profitability
```

### 5.3 End-of-Year (EOY) Cycle

```
JOB STREAM: VAULT-EOY

STEP 01: TAXRPT0 — Tax Reporting
  - Generate 1099-INT for accounts earning > $10 interest
  - Generate 1098 for mortgage interest paid
  - Generate 1099-R for IRA distributions
  - Create IRS transmittal files

STEP 02: GLANNUAL0 — Annual GL Close
  - Close income/expense accounts to Retained Earnings
  - Roll YTD accumulators
  - Store prior year comparison balances
  - Reset all YTD fields

STEP 03: CALLRPT0 — Call Report Data
  - Extract data for FFIEC Call Report filing
  - Map GL balances to Call Report line items
  - Calculate regulatory ratios:
    - Tier 1 Capital Ratio
    - Total Capital Ratio
    - Leverage Ratio
    - Liquidity Coverage Ratio

STEP 04: YLDCALC0 — APY Verification
  - Recalculate effective APY for all deposit products
  - Verify against disclosed rates (Truth in Savings)
  - Flag discrepancies for compliance review
```

---

## 6. Regulatory Compliance Programs

### 6.1 BSA/AML Module

```
Program: BSACTRO — Currency Transaction Report Generator
  - Aggregate cash transactions per customer per day
  - Generate CTR (FinCEN Form 112) for cash >= $10,000
  - File electronically via BSA E-Filing

Program: BSASAR0 — Suspicious Activity Report
  - Rule-based detection:
    - Structuring: multiple cash transactions just under $10K
    - Rapid movement: large deposits followed by immediate withdrawal
    - Unusual patterns for account type/customer profile
    - Round-dollar transactions in unusual amounts
  - Queue alerts for BSA officer review
  - Generate SAR filing (FinCEN Form 111) upon approval

Program: BSAOFAC0 — OFAC Screening
  - Screen new customers against SDN list
  - Screen wire beneficiaries against SDN list
  - Fuzzy name matching algorithm
  - Generate 314(a) information sharing responses
```

### 6.2 Regulation E (Electronic Fund Transfers)

```
Program: REGEDSP0 — Reg E Dispute Processing
  - Accept customer disputes for electronic transactions
  - Calculate provisional credit timeline:
    - 10 business days for investigation
    - Provisional credit if not resolved in 10 days
    - 45 calendar days total investigation window
    - 90 days for POS/foreign transactions
  - Track investigation status
  - Generate required customer notifications
  - Post provisional credits and reversals
```

### 6.3 Regulation CC (Expedited Funds Availability)

```
HOLD SCHEDULE (encoded in HOLDCALC0):

CHECK TYPE                    NEXT-DAY    FULL AVAILABILITY
─────────────────────────────────────────────────────────────
US Treasury check              $5,525*     Next business day
Government checks              $5,525*     Next business day
Cashier's/certified checks     $5,525*     Next business day
Local checks                   $225        2 business days
Non-local checks               $225        5 business days

* First $225 available next business day

EXCEPTION HOLDS (extended to 7+ business days):
  - New accounts (open < 30 days)
  - Large deposits (> $5,525)
  - Redeposited checks
  - Repeated overdrafts
  - Reasonable cause to doubt collectibility

Program: HOLDCALC0
  - Determine check type from routing number
  - Apply appropriate hold schedule
  - Handle exception conditions
  - Generate hold notices (required by Reg CC)
  - Schedule hold release
```

---

## 7. CICS Online Screens (Teller/CSR Interface)

### Screen Map Layout

```
SCREEN ID    PURPOSE                    TRANSACTION CODE
──────────────────────────────────────────────────────────
VCIF01       Customer Inquiry           CINQ
VCIF02       Customer Maintenance       CMNT
VACCT01      Account Inquiry            AINQ
VACCT02      Account Opening            AOPN
VACCT03      Account Maintenance        AMNT
VACCT04      Account Closing            ACLS
VTXN01       Transaction Entry          TENT
VTXN02       Transaction Inquiry        TINQ
VTXN03       Transaction History        THST
VTXN04       Transaction Reversal       TREV
VDEP01       Deposit Entry              DENR
VWDL01       Withdrawal Entry           WENR
VXFR01       Transfer Entry             XENR
VCHK01       Check Inquiry              CINQ
VSTP01       Stop Payment Entry         SENR
VWIR01       Wire Transfer Entry        WENT
VWIR02       Wire Transfer Approval     WAPV
VHLD01       Hold Inquiry/Override      HINQ
VBAL01       Balance Inquiry (all accts) BINQ
VRPT01       Teller Balancing           TBAL
VRPT02       Branch Daily Summary       BSUM
```

### Example Screen — VACCT01 (Account Inquiry)

```
────────────────────────────────────────────────────────────────────────
 VAULT CORE BANKING SYSTEM              ACCOUNT INQUIRY        VACCT01
 BRANCH: 0001  USER: JTELLER1           02/23/2026 14:30:15

 ACCT NUMBER: 000012345678-9            STATUS: ACTIVE
 PRODUCT: DDA1 - REGULAR CHECKING       OPENED: 01/15/2020

 PRIMARY: SMITH, JOHN M                 CIF: 0000012345
 JOINT:   SMITH, JANE A                 CIF: 0000012346
 OWNERSHIP: JT - JOINT TENANTS

 ─── BALANCES ──────────────────────────────────────────────────────
 LEDGER BALANCE:         $12,450.33     AVAILABLE BALANCE:    $11,950.33
 COLLECTED BALANCE:      $12,200.33     HOLDS:                   $500.00
 PENDING DEBITS:            $125.00     PENDING CREDITS:          $0.00

 ─── INTEREST ──────────────────────────────────────────────────────
 RATE: 0.050%  TYPE: FIXED              METHOD: DAILY BALANCE
 ACCRUED (PTD):              $0.52      YTD EARNED:              $5.87
 LAST PAYMENT:           01/31/2026     NEXT PAYMENT:         02/28/2026

 ─── ACTIVITY SUMMARY (CURRENT PERIOD) ────────────────────────────
 DEPOSITS:     5   $8,250.00            WITHDRAWALS:  12  $6,325.67
 CHECKS:       8   $3,200.00            FEES:          0      $0.00

 ─── HOLDS ─────────────────────────────────────────────────────────
 CHECK DEP 02/21  $500.00  RELEASE: 02/25   TYPE: CC-LOCAL

 F1=HELP  F3=EXIT  F5=TXN HIST  F7=HOLDS  F9=MAINT  F12=RETURN
────────────────────────────────────────────────────────────────────────
```

---

## 8. File Organization & Program Inventory

### Directory Structure

```
VAULT-CBS/
├── COPYLIB/                    Copybook library
│   ├── CPYCIF.cpy             Customer Information File
│   ├── CPYACCT.cpy            Account Master
│   ├── CPYTXN.cpy             Transaction Record
│   ├── CPYGL.cpy              General Ledger
│   ├── CPYHOLD.cpy            Hold/Float Record
│   ├── CPYLOAN.cpy            Loan Payment Schedule
│   ├── CPYPROD.cpy            Product Parameter Table
│   ├── CPYRATE.cpy            Interest Rate Table
│   ├── CPYFEE.cpy             Fee Schedule Table
│   ├── CPYACH.cpy             ACH Record Layout
│   ├── CPYWIRE.cpy            Wire Transfer Record
│   ├── CPYAUDT.cpy            Audit Trail Record
│   ├── CPYCTR.cpy             CTR Filing Record
│   ├── CPYSAR.cpy             SAR Filing Record
│   ├── CPYSTMT.cpy            Statement Record
│   ├── CPYERR.cpy             Error Code Table
│   └── CPYCONST.cpy           System Constants
│
├── ONLINE/                     CICS online programs
│   ├── CIFMAINT.cbl           CIF inquiry/maintenance
│   ├── ACCTINQ.cbl            Account inquiry
│   ├── ACCTOPEN.cbl           Account opening
│   ├── ACCTMNT.cbl            Account maintenance
│   ├── ACCTCLS.cbl            Account closing
│   ├── TXNENTRY.cbl           Transaction entry
│   ├── TXNINQ.cbl             Transaction inquiry/history
│   ├── TXNREV.cbl             Transaction reversal
│   ├── DEPOSIT.cbl            Deposit entry
│   ├── WITHDRAW.cbl           Withdrawal entry
│   ├── TRANSFER.cbl           Internal transfer
│   ├── STOPPAY.cbl            Stop payment
│   ├── WIREENT.cbl            Wire transfer entry
│   ├── WIREAPV.cbl            Wire transfer approval
│   ├── HOLDMGMT.cbl           Hold inquiry/override
│   ├── TELLBAL.cbl            Teller balancing
│   └── BRCHSUM.cbl            Branch daily summary
│
├── BATCH/                      Batch processing programs
│   ├── TXNPOST0.cbl           Transaction posting engine
│   ├── TXNREV0.cbl            Transaction reversal
│   ├── INTCALC0.cbl           Interest calculation/accrual
│   ├── INTRATE0.cbl           Rate index maintenance
│   ├── FEECALC0.cbl           Fee assessment
│   ├── HOLDREL0.cbl           Hold release processing
│   ├── ACHRECV0.cbl           ACH incoming processor
│   ├── ACHSEND0.cbl           ACH origination
│   ├── ACHRETN0.cbl           ACH return processing
│   ├── WIRPROC0.cbl           Wire processing
│   ├── GLPOST0.cbl            GL posting & balancing
│   ├── GLCLOSE0.cbl           GL period close
│   ├── STMTGEN0.cbl           Statement generation
│   ├── DORMCHK0.cbl           Dormancy check
│   ├── ODMGMT0.cbl            Overdraft management
│   ├── LNPMT0.cbl             Loan payment processing
│   ├── LNDELQ0.cbl            Loan delinquency management
│   ├── LNAMORT0.cbl           Loan amortization schedule
│   ├── CDMAT0.cbl             CD maturity processing
│   ├── CDRENEW0.cbl           CD auto-renewal
│   └── REGDCHK0.cbl           Reg D limit check
│
├── COMPLIANCE/                 Regulatory programs
│   ├── BSACTRO.cbl            CTR generation
│   ├── BSASAR0.cbl            SAR detection & filing
│   ├── BSAOFAC0.cbl           OFAC screening
│   ├── REGEDSP0.cbl           Reg E dispute processing
│   ├── HOLDCALC0.cbl          Reg CC hold calculation
│   ├── CALLRPT0.cbl           Call Report data extraction
│   └── TAXRPT0.cbl            1099/1098 tax reporting
│
├── REPORTS/                    Report programs
│   ├── RPTDAILY.cbl           Daily activity summary
│   ├── RPTTBAL.cbl            Trial balance
│   ├── RPTDEPO.cbl            Deposit composition
│   ├── RPTLOAN.cbl            Loan portfolio
│   ├── RPTDELQ.cbl            Delinquency report
│   ├── RPTCONC.cbl            Large balance concentration
│   ├── RPTRATE.cbl            Rate sensitivity analysis
│   └── RPTBRCH.cbl            Branch performance
│
├── JCL/                        Job Control Language
│   ├── VEOD.jcl               End-of-Day job stream
│   ├── VEOM.jcl               End-of-Month job stream
│   ├── VEOY.jcl               End-of-Year job stream
│   ├── VACH.jcl               ACH processing jobs
│   ├── VWIRE.jcl              Wire processing jobs
│   └── VBACKUP.jcl            Backup/recovery jobs
│
├── BMS/                        CICS screen maps
│   ├── VCIF01.bms             CIF inquiry screen
│   ├── VACCT01.bms            Account inquiry screen
│   ├── VTXN01.bms             Transaction entry screen
│   └── ... (all screen maps)
│
├── DDL/                        Database definitions
│   ├── VAULT_TABLES.sql       DB2 table definitions
│   ├── VAULT_INDEXES.sql      DB2 index definitions
│   ├── VAULT_VIEWS.sql        DB2 view definitions
│   └── VAULT_PROCS.sql        DB2 stored procedures
│
├── TESTDATA/                   Test data generators
│   ├── GENACCT.cbl            Generate test accounts
│   ├── GENTXN.cbl             Generate test transactions
│   └── GENACH.cbl             Generate test ACH files
│
└── DOCS/
    ├── DATA_DICTIONARY.md     Complete field reference
    ├── GL_CHART.md            Chart of accounts
    ├── TXN_CODES.md           Transaction code reference
    ├── ERROR_CODES.md         Error/exception codes
    ├── BATCH_SCHEDULE.md      Batch job dependencies
    └── REG_MATRIX.md          Regulatory requirement mapping
```

### Program Count Estimate

| Category | Programs | Estimated LOC |
|----------|----------|---------------|
| Copybooks | 17 | ~3,000 |
| Online (CICS) | 17 | ~15,000 |
| Batch | 21 | ~18,000 |
| Compliance | 7 | ~6,000 |
| Reports | 8 | ~5,000 |
| JCL | 6 | ~2,000 |
| Screen Maps | 17 | ~3,000 |
| DDL/SQL | 4 | ~1,500 |
| Test Data | 3 | ~1,500 |
| **TOTAL** | **~100** | **~55,000** |

---

## 9. Implementation Roadmap

### Phase 1: Foundation (Weeks 1-4)
- All copybooks
- GL chart of accounts and GL posting engine
- CIF maintenance (add/update/inquiry)
- Account master (open/close/inquiry)
- Basic transaction posting engine (deposits, withdrawals)
- DB2 table definitions

### Phase 2: Core Banking (Weeks 5-10)
- Interest calculation engine (all methods)
- Fee assessment engine
- Transfer processing
- Check processing
- Hold management (Reg CC)
- EOD batch cycle
- Teller balancing

### Phase 3: Payments (Weeks 11-16)
- ACH incoming/outgoing processor
- Wire transfer processing
- Stop payment processing
- Overdraft management (Reg E opt-in)

### Phase 4: Lending (Weeks 17-22)
- Loan account servicing
- Payment application (principal/interest/escrow split)
- Amortization schedule generation
- CD maturity and renewal processing
- Delinquency tracking and management
- Late fee assessment

### Phase 5: Compliance & Reporting (Weeks 23-28)
- BSA/AML (CTR, SAR, OFAC screening)
- Reg E dispute processing
- Statement generation
- Tax reporting (1099-INT, 1098)
- Call Report data extraction
- All management reports

### Phase 6: Polish & Testing (Weeks 29-34)
- CICS screen development (all screens)
- Integration testing across all modules
- Regulatory scenario testing
- Performance testing (batch window optimization)
- Documentation completion

---

## 10. Key Design Decisions & Trade-offs

### Why DB2 + VSAM (Hybrid Storage)
DB2 provides relational integrity and SQL reporting capability. VSAM provides raw I/O speed for hot-path batch processing (interest calc touching every account). In production, you'd read from VSAM for batch throughput and sync to DB2 for online queries and reporting.

### Why 6 Decimal Places for Interest Accrual
Banks accrue interest daily but pay monthly/quarterly. Over 90 days, rounding errors at 2 decimal places compound. Six decimal places ensures the final rounded payment matches the disclosed APY within regulatory tolerance. This is a real production requirement.

### Why Balance Snapshots in Every Transaction
Regulators and auditors need point-in-time balance reconstruction. By storing before/after balances on every transaction, you can prove the account state at any moment without replaying the entire transaction history. This is non-negotiable for examination readiness.

### Why Separate Ledger/Available/Collected Balances
These serve different purposes: ledger balance is the accounting truth, available balance is what the customer can spend (ledger minus holds), and collected balance is what's backed by cleared funds. Reg CC compliance requires tracking all three independently.

### Why Transaction Codes Instead of Free-Text Types
A `PIC 9(4)` transaction code maps to a lookup table that controls GL account mapping, statement descriptions, fee applicability, and compliance reporting flags. This is how real core systems work — the code drives all downstream behavior.

---

## 11. Competitive Positioning

When presenting this project, the narrative is:

> "Every major bank in the world still runs on COBOL — JPMorgan processes $10 trillion daily on COBOL systems. I didn't just learn the language, I built the entire system those programs run inside. This demonstrates I understand not just syntax, but the full domain: double-entry accounting, federal banking regulations, payment network protocols, and batch processing architecture at institutional scale."

This project demonstrates mastery of:
- Financial domain knowledge (banking operations, regulations, accounting)
- Enterprise architecture (batch/online hybrid, mainframe patterns)
- Data architecture (copybook design, normalization, file organization)
- Systems thinking (EOD cycles, GL balancing, audit trails)
- Regulatory compliance (BSA/AML, Reg CC, Reg D, Reg E)
- The actual technology stack that processes the world's money

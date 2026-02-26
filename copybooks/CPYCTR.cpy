*> ================================================================
*> CPYCTR.cpy - Currency Transaction Report Record Layout
*> Used by: BSACTRO, BSA/AML compliance
*> ================================================================
01  CTR-RECORD.
    05  CTR-KEY.
        10  CTR-FILING-ID         PIC 9(15).
        10  CTR-FILING-DATE       PIC 9(8).
    05  CTR-CUSTOMER-INFO.
        10  CTR-CUST-ID           PIC 9(10).
        10  CTR-CUST-NAME         PIC X(50).
        10  CTR-CUST-SSN-TIN      PIC 9(9).
        10  CTR-CUST-DOB          PIC 9(8).
        10  CTR-CUST-ADDRESS      PIC X(80).
        10  CTR-CUST-ID-TYPE      PIC X(2).
        10  CTR-CUST-ID-NUMBER    PIC X(25).
    05  CTR-TRANSACTION-SUMMARY.
        10  CTR-TXN-DATE          PIC 9(8).
        10  CTR-CASH-IN-TOTAL     PIC S9(13)V99.
        10  CTR-CASH-OUT-TOTAL    PIC S9(13)V99.
        10  CTR-CASH-IN-COUNT     PIC 9(5).
        10  CTR-CASH-OUT-COUNT    PIC 9(5).
    05  CTR-ACCOUNT-INFO.
        10  CTR-NUM-ACCOUNTS      PIC 9(3).
        10  CTR-ACCOUNTS OCCURS 10 TIMES.
            15  CTR-ACCT-NUMBER   PIC 9(12).
            15  CTR-ACCT-TYPE     PIC X(1).
    05  CTR-FILING-INFO.
        10  CTR-BRANCH-ID         PIC 9(4).
        10  CTR-TELLER-ID         PIC X(8).
        10  CTR-EXEMPT-FLAG       PIC X(1).
        10  CTR-FILING-STATUS     PIC X(1).
        *> P = Pending, F = Filed, E = Error
        10  CTR-FILING-TIMESTAMP  PIC 9(14).

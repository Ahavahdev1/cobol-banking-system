*> ================================================================
*> CPYWIRE.cpy - Wire Transfer Record Layout
*> Used by: Wire transfer processing (WIREXFR0)
*> ================================================================
01  WIRE-RECORD.
    05  WIRE-REFERENCE-NUM    PIC X(16).
    05  WIRE-TYPE             PIC X(1).
        *> I=Incoming, O=Outgoing, R=Return
    05  WIRE-PRIORITY         PIC X(1).
        *> N=Normal, U=Urgent
    05  WIRE-AMOUNT           PIC S9(13)V99.
    05  WIRE-CURRENCY         PIC X(3) VALUE "USD".
    05  WIRE-SEND-DATE        PIC 9(8).
    05  WIRE-VALUE-DATE       PIC 9(8).
    05  WIRE-STATUS           PIC X(2).
        *> PE=Pending, AP=Approved, PR=Processing,
        *> CP=Complete, RJ=Rejected, RV=Reversed
    05  WIRE-ORIGINATOR.
        10  WIRE-ORIG-NAME    PIC X(35).
        10  WIRE-ORIG-ACCT    PIC 9(12).
        10  WIRE-ORIG-BANK    PIC 9(9).
    05  WIRE-BENEFICIARY.
        10  WIRE-BENE-NAME    PIC X(35).
        10  WIRE-BENE-ACCT    PIC X(34).
        10  WIRE-BENE-BANK    PIC 9(9).
        10  WIRE-BENE-COUNTRY PIC X(3).
    05  WIRE-INTERMEDIARY.
        10  WIRE-INTM-BANK    PIC 9(9).
    05  WIRE-PURPOSE          PIC X(40).
    05  WIRE-APPROVED-BY      PIC X(8).
    05  WIRE-CREATED-BY       PIC X(8).

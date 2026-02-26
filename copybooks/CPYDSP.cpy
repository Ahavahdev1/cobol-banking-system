*> ================================================================
*> CPYDSP.cpy - Dispute Record Layout (Regulation E)
*> Used by: Dispute management, EFT error resolution
*> ================================================================
01  DISPUTE-RECORD.
    05  DSP-DISPUTE-ID        PIC 9(15).
    05  DSP-ACCT-NUMBER       PIC 9(12).
    05  DSP-TXN-ID            PIC 9(15).
    05  DSP-TXN-AMOUNT        PIC S9(13)V99.
    05  DSP-DISPUTE-DATE      PIC 9(8).
    05  DSP-DISPUTE-TYPE      PIC X(4).
        *> UNAU=Unauthorized, ERRO=Error, DUPE=Duplicate
        *> NORC=Not received, WRNG=Wrong amount
    05  DSP-STATUS            PIC X(1).
        *> P=Pending, I=Investigating, R=Resolved,
        *> D=Denied, C=Credited (provisional)
    05  DSP-PROVISIONAL-AMT   PIC S9(13)V99.
    05  DSP-PROVISIONAL-DATE  PIC 9(8).
    05  DSP-RESOLUTION-DATE   PIC 9(8).
    05  DSP-RESOLUTION-CODE   PIC X(2).
        *> AP=Approved, DN=Denied, PA=Partial
    05  DSP-INVESTIGATOR      PIC X(8).
    05  DSP-DESCRIPTION       PIC X(80).
    05  DSP-DEADLINE-DATE     PIC 9(8).

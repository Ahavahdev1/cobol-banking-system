*> ================================================================
*> CPYBATCH.cpy - Batch Control Record Layout
*> Used by: EODPROC0, EOMPROC0 for batch run tracking
*> ================================================================
01  BATCH-RECORD.
    05  BATCH-KEY.
        10  BATCH-DATE              PIC 9(8).
        10  BATCH-TYPE              PIC X(3).
        *> EOD = End-of-Day, EOM = End-of-Month,
        *> EOY = End-of-Year
    05  BATCH-DETAIL.
        10  BATCH-STATUS            PIC X(1).
        *> S = Started, C = Complete, F = Failed, P = Partial
        10  BATCH-ACCTS-PROCESSED   PIC 9(8).
        10  BATCH-ACCTS-ERRORS      PIC 9(8).
        10  BATCH-START-TIME        PIC 9(14).
        10  BATCH-END-TIME          PIC 9(14).
    05  BATCH-TOTALS.
        10  BATCH-INT-ACCRUED       PIC S9(15)V99.
        10  BATCH-INT-PAID          PIC S9(15)V99.
        10  BATCH-FEES-ASSESSED     PIC S9(15)V99.
        10  BATCH-HOLDS-RELEASED    PIC 9(8).
        10  BATCH-CTR-FILED         PIC 9(5).

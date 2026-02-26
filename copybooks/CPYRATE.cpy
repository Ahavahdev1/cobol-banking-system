*> ================================================================
*> CPYRATE.cpy - Interest Rate Table Layout
*> Used by: INTCALC0, INTRATE0, variable rate recalculation
*> ================================================================
01  RATE-TABLE-RECORD.
    05  RATE-INDEX-CODE           PIC X(4).
    05  RATE-MATURITY-CODE        PIC X(4).
    05  RATE-CURRENT-VALUE        PIC 9(3)V9(7).
    05  RATE-PRIOR-VALUE          PIC 9(3)V9(7).
    05  RATE-EFFECTIVE-DATE       PIC 9(8).
    05  RATE-PRIOR-DATE           PIC 9(8).
    05  RATE-SOURCE               PIC X(10).
    05  RATE-LAST-UPDATE          PIC 9(14).

*> Tiered rate sub-table (for tiered interest products)
01  TIER-RATE-TABLE.
    05  TIER-PRODUCT-CODE         PIC X(4).
    05  TIER-NUM-TIERS            PIC 9(2).
    05  TIER-ENTRY OCCURS 10 TIMES.
        10  TIER-MIN-BAL          PIC S9(13)V99.
        10  TIER-MAX-BAL          PIC S9(13)V99.
        10  TIER-RATE             PIC 9(3)V9(7).

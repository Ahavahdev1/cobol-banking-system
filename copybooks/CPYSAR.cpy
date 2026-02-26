*> ================================================================
*> CPYSAR.cpy - Suspicious Activity Report Record Layout
*> Used by: BSACTRO (SAR detection), AUDTLOG0, EODPROC0
*> ================================================================
01  SAR-RECORD.
    05  SAR-KEY.
        10  SAR-FILING-ID            PIC 9(15).
    05  SAR-DETAIL.
        10  SAR-CUST-ID             PIC 9(10).
        10  SAR-START-DATE          PIC 9(8).
        10  SAR-END-DATE            PIC 9(8).
        10  SAR-TOTAL-AMOUNT        PIC S9(13)V99.
        10  SAR-PATTERN-TYPE        PIC X(6).
        *> STRC = Structuring, RNDTRP = Round-trip,
        *> RAPID = Rapid movement
        10  SAR-NARRATIVE           PIC X(200).
    05  SAR-STATUS-INFO.
        10  SAR-STATUS              PIC X(1).
        *> P = Pending, F = Filed, D = Dismissed
        10  SAR-FILED-DATE          PIC 9(8).
        10  SAR-FILED-BY            PIC X(8).

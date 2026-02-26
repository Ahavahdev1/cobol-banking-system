*> ================================================================
*> CPYFIO.cpy - File I/O Request/Response Linkage
*> Used by: FILEIO0 for unified indexed file CRUD operations
*> ================================================================
01  LS-FILE-REQUEST.
    05  LS-FIO-FUNCTION          PIC X(4).
    *> OPEN, CLOS, READ, WRIT, REWT, DELT, NEXT, STRT
    05  LS-FIO-FILE-ID           PIC X(4).
    *> ACCT, CIF, TXN, GL, HOLD, AUDT
    05  LS-FIO-KEY               PIC X(20).
01  LS-FILE-RESULT.
    05  LS-FIO-STATUS            PIC X(2).
    05  LS-FIO-RESULT-CODE       PIC X(5).
    05  LS-FIO-RESULT-MSG        PIC X(50).

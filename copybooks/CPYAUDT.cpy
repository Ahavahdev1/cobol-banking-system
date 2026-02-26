*> ================================================================
*> CPYAUDT.cpy - Audit Trail Record Layout
*> Used by: All modules for audit logging
*> ================================================================
01  AUDIT-RECORD.
    05  AUDIT-KEY.
        10  AUDIT-ID              PIC 9(15).
        10  AUDIT-SEQUENCE        PIC 9(5).
    05  AUDIT-DETAIL.
        10  AUDIT-TIMESTAMP       PIC 9(14).
        10  AUDIT-USER-ID         PIC X(8).
        10  AUDIT-TERMINAL-ID     PIC X(8).
        10  AUDIT-PROGRAM-ID      PIC X(8).
        10  AUDIT-FUNCTION        PIC X(4).
        10  AUDIT-ENTITY-TYPE     PIC X(4).
        10  AUDIT-ENTITY-KEY      PIC X(20).
    05  AUDIT-CHANGES.
        10  AUDIT-FIELD-NAME      PIC X(30).
        10  AUDIT-BEFORE-VALUE    PIC X(50).
        10  AUDIT-AFTER-VALUE     PIC X(50).
    05  AUDIT-STATUS.
        10  AUDIT-RESULT-CODE     PIC X(5).
        10  AUDIT-DESCRIPTION     PIC X(40).

*> ================================================================
*> CPYOFAC.cpy - OFAC Screening Record Layout
*> Used by: OFACCHK0 for sanctions list checking
*> ================================================================
01  OFAC-CHECK-RECORD.
    05  OFAC-CHECK-NAME        PIC X(50).
    05  OFAC-CHECK-COUNTRY     PIC X(3).
    05  OFAC-CHECK-ID-NUMBER   PIC X(20).
    05  OFAC-CHECK-TYPE        PIC X(1).
    *> C = Customer, B = Beneficiary, O = Originator
    05  OFAC-MATCH-FOUND       PIC X(1).
    *> Y = Match, N = No match
    05  OFAC-MATCH-SCORE       PIC 9(3).
    *> 000-100 confidence score
    05  OFAC-MATCH-LIST        PIC X(10).
    *> SDN, CONS, NS-PLC, etc.
    05  OFAC-CHECK-DATE        PIC 9(8).
    05  OFAC-CHECK-STATUS      PIC X(1).
    *> P = Passed, F = Failed, R = Review Required

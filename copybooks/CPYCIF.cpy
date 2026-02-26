*> ================================================================
*> CPYCIF.cpy - Customer Information File Record Layout
*> Used by: CIF maintenance, account opening, KYC, BSA/AML
*> ================================================================
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
        *> S = SSN, E = EIN, I = ITIN
        10  CIF-DOB                PIC 9(8).
        *> YYYYMMDD
        10  CIF-CUST-TYPE          PIC X(1).
        *> I = Individual, J = Joint, B = Business,
        *> T = Trust, E = Estate
    05  CIF-CONTACT-INFO.
        10  CIF-ADDR-LINE1         PIC X(40).
        10  CIF-ADDR-LINE2         PIC X(40).
        10  CIF-CITY               PIC X(30).
        10  CIF-STATE              PIC X(2).
        10  CIF-ZIP                PIC 9(9).
        10  CIF-COUNTRY            PIC X(3).
        *> ISO 3166-1 alpha-3
        10  CIF-PHONE-PRIMARY      PIC 9(10).
        10  CIF-PHONE-MOBILE       PIC 9(10).
        10  CIF-EMAIL              PIC X(60).
    05  CIF-REGULATORY-INFO.
        10  CIF-CIP-VERIFIED       PIC X(1).
        *> Y/N - Customer Identification Program
        10  CIF-CIP-VERIFY-DATE    PIC 9(8).
        10  CIF-CIP-DOC-TYPE       PIC X(2).
        *> DL = Driver License, PP = Passport,
        *> MI = Military ID, SI = State ID
        10  CIF-CIP-DOC-NUMBER     PIC X(25).
        10  CIF-CIP-DOC-EXPIRY     PIC 9(8).
        10  CIF-OFAC-CHECK-DATE    PIC 9(8).
        10  CIF-OFAC-STATUS        PIC X(1).
        *> C = Clear, H = Hold, M = Match
        10  CIF-BSA-RISK-RATING    PIC 9(1).
        *> 1 = Low, 2 = Medium, 3 = High, 4 = Prohibited
        10  CIF-CTR-EXEMPT         PIC X(1).
        *> Y/N - Currency Transaction Report exemption
    05  CIF-RELATIONSHIP-INFO.
        10  CIF-OPEN-DATE          PIC 9(8).
        10  CIF-LAST-CONTACT-DATE  PIC 9(8).
        10  CIF-STATUS             PIC X(1).
        *> A = Active, I = Inactive, C = Closed,
        *> D = Deceased, F = Frozen
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

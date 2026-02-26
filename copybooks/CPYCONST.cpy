*> ================================================================
*> CPYCONST.cpy - System Constants
*> Used by: All modules for regulatory and system thresholds
*> ================================================================
01  SYSTEM-CONSTANTS.
    *> BSA/AML Thresholds
    05  WS-CTR-THRESHOLD          PIC S9(13)V99
                                  VALUE +10000.00.
    05  WS-CTR-THRESHOLD-EXACT    PIC S9(13)V99
                                  VALUE +10000.00.
    *> Regulation CC - Funds Availability
    05  WS-REGCC-NEXT-DAY-AMT     PIC S9(13)V99
                                   VALUE +225.00.
    05  WS-REGCC-LARGE-DEP-AMT    PIC S9(13)V99
                                   VALUE +5525.00.
    05  WS-REGCC-TREASURY-AMT     PIC S9(13)V99
                                   VALUE +5525.00.
    05  WS-REGCC-LOCAL-DAYS       PIC 9(2) VALUE 02.
    05  WS-REGCC-NONLOCAL-DAYS    PIC 9(2) VALUE 05.
    05  WS-REGCC-NEW-ACCT-DAYS    PIC 9(2) VALUE 30.
    05  WS-REGCC-EXCEPTION-DAYS   PIC 9(2) VALUE 07.
    *> Regulation D - Transfer Limits
    05  WS-REGD-MONTHLY-LIMIT     PIC 9(2) VALUE 06.
    *> NSF/OD Fee Limits
    05  WS-NSF-FEE-AMOUNT         PIC 9(5)V99 VALUE 36.00.
    05  WS-NSF-DAILY-MAX          PIC 9(2) VALUE 04.
    05  WS-NSF-DE-MINIMIS         PIC 9(5)V99 VALUE 5.00.
    *> Account Management
    05  WS-DORMANCY-MONTHS        PIC 9(3) VALUE 012.
    *> System Limits
    05  WS-MAX-BALANCE            PIC S9(13)V99
                                  VALUE +9999999999999.99.
    05  WS-MIN-BALANCE            PIC S9(13)V99
                                  VALUE -9999999999999.99.

      * TEST CODE INSERTED BY ZUTZCPC
       01  UT-FIELDS.
           05  UT-CONST-ES               PIC X(02) VALUE 'ES'.
           05  UT-LABEL-EXPECTED-ACCESS.
               10  FILLER                PIC X(06) VALUE 'ACCESS'.
               10  UT-LABEL-EXPECTED-ACCESS-PLURAL 
                                         PIC X(02) VALUE SPACES.
           05  UT-DISPLAY-MESSAGE        PIC X(256) VALUE SPACES.
           05  FILLER                    PIC X(01) VALUE SPACES.
               88  UT-REVERSE-COMPARE    VALUE 'Y', FALSE SPACES.
           05  FILLER                    PIC X(01) VALUE '1'.
               88  UT-VERIFY-EXACT                 VALUE '1'.
               88  UT-VERIFY-AT-LEAST              VALUE '2'.
               88  UT-VERIFY-NO-MORE-THAN          VALUE '3'.   
           05  FILLER                    PIC X(01) VALUE SPACES.
               88  UT-VERIFY-PASSED      VALUE 'Y', FALSE SPACES.     
           05  FILLER                    PIC X(01) VALUE SPACES.
               88  UT-COMPARE-DEFAULT    VALUE SPACE.
               88  UT-COMPARE-NUMERIC    VALUE 'N', FALSE SPACES.
           05  FILLER                    PIC X(01) VALUE SPACES.
               88  UT-COMPARE-PASSED     VALUE 'Y', FALSE SPACES.    
           05  UT-EXPECTED               PIC X(60) VALUE SPACES.
           05  UT-ACTUAL                 PIC X(60) VALUE SPACES.
           05  UT-EXPECTED-NUMERIC       PIC S9(08) VALUE ZERO.
           05  UT-ACTUAL-NUMERIC         PIC S9(08) VALUE ZERO.
           05  UT-EXPECTED-ACCESSES      PIC 9(04) VALUE ZERO.
           05  UT-ACTUAL-ACCESSES        PIC 9(04) VALUE ZERO.
           05  UT-EXPECTED-ACCESSES-FMT  PIC Z,ZZ9.
           05  UT-ACTUAL-ACCESSES-FMT    PIC Z,ZZ9.
           05  UT-FAILED                 PIC X(11)  VALUE "**** FAIL: ".
           05  UT-PASSED                 PIC X(11)  VALUE "     PASS: ".
           05  UT-TEST-CASE-NAME         PIC X(80)  VALUE SPACES.
           05  UT-TEST-CASE-NUMBER       PIC ZZ9.
           05  UT-RETCODE                PIC 9(4)   VALUE ZERO.
           05  UT-TEST-CASE-COUNT        PIC 9(4)   VALUE ZERO.
           05  UT-NUMBER-PASSED          PIC 9(4)   VALUE ZERO.
           05  UT-NUMBER-FAILED          PIC 9(4)   VALUE ZERO.
       01  UT-MOCKS.
           05  FILLER                   PIC X(01) VALUE SPACES.
               88  UT-MOCK-FOUND        VALUE 'Y', FALSE SPACES.
           05  UT-MOCK-FIND-TYPE         PIC X(04).
               88  UT-FIND-FILE-MOCK     VALUE 'FILE'.
               88  UT-FIND-CICS-MOCK     VALUE 'CICS'.
               88  UT-FIND-SQL-MOCK      VALUE 'SQL'.
           05  UT-MOCK-FIND-FILENAME     PIC X(31).
           05  UT-MOCK-FIND-OPERATION    PIC X(04).
           05  UT-MOCK-SET-RECORD        PIC X(2048).
           05  UT-MOCK-SET-FILE-STATUS   PIC X(02).
           05  UT-MOCK-MAX               PIC 9(02) VALUE 10.
           05  UT-MOCK-COUNT             PIC 9(02) VALUE ZERO.
           05  UT-MOCK OCCURS 10 INDEXED BY UT-MOCK-IX.
               10  UT-MOCK-TYPE          PIC X(04).
                   88  UT-MOCK-FILE          VALUE 'FILE', FALSE SPACES.
                   88  UT-MOCK-CICS          VALUE 'CICS', FALSE SPACES.
                   88  UT-MOCK-SQL           VALUE 'SQL',  FALSE SPACES.
               10  UT-MOCK-ACCESS-COUNT    PIC 9(04) VALUE ZERO.
               10  UT-MOCK-RECORD        PIC X(2048).    
               10  UT-MOCK-DATA          PIC X(2048).
               10  UT-MOCK-FILE-DATA REDEFINES UT-MOCK-DATA.        
                   15  UT-MOCK-FILENAME      PIC X(31).    
                   15  UT-MOCK-OPERATION     PIC X(20).  
                   15  UT-MOCK-FILE-STATUS   PIC X(02).
                   15  FILLER             PIC X(104).
               10  UT-MOCK-CICS-DATA REDEFINES UT-MOCK-DATA.
                   15  UT-MOCK-CICS-RESOURCE PIC X(08).
                   15  UT-MOCK-CICS-COMMAND  PIC X(30).
                   15  UT-MOCK-CICS-RIDFLD   PIC X(100).    
                   15  UT-MOCK-EIBRESP       PIC S9(08) COMP.
                   15  UT-MOCK-EIBRESP2      PIC S9(08) COMP.
               10  UT-MOCK-SQL-DATA REDEFINES UT-MOCK-DATA.
                   15  UT-MOCK-SQL-COMMAND   PIC X(20).
                   15  UT-MOCK-SQL-CODE      PIC S9(08) COMP.
                   15  UT-MOCK-SQL-STATE     PIC S9(08) COMP.
                   15  FILLER             PIC X(118).
       01  UT-FILES.
           05  UT-FILE-MAX                   PIC 9(02) VALUE 10.
           05  UT-FILE-COUNT                 PIC 9(02) VALUE ZERO.
           05  FILLER                        PIC X(01) VALUE SPACE.
               88  UT-FILENAME-MATCHED       VALUE 'Y', FALSE SPACE.
           05  UT-FILE-INFORMATION.
               10  FILLER OCCURS 10 INDEXED BY UT-FILE-IX.
                   15  UT-INTERNAL-FILENAME      PIC X(31).
                   15  UT-RECORD-FIELD-NAME      PIC X(31).
                   15  UT-FILE-STATUS-FIELD-NAME PIC X(31).

      * END OF TEST CODE 
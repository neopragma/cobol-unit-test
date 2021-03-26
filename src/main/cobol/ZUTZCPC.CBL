       IDENTIFICATION DIVISION.
       PROGRAM-ID.  ZUTZCPC.
      *****************************************************************
      * This work is licensed under a Creative Commons
      * Attribution-ShareAlike 4.0 International license.
      * See http://creativecommons.org/licenses/by-sa/4.0/.
      *
      * Precompiler to copy a source file to a temporary test file and
      * insert unit test code into the copy.
      *
      * -- Input --
      * ORIGINAL-SOURCE  The program to be tested
      * TEST-CASES       Unit test cases
      * UNIT-TEST-CONFIG Names of files containing standard test code
      *
      * -- Output --
      * TEST-SOURCE    Copy of ORIGINAL-SOURCE with test code inserted
      *****************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ORIGINAL-SOURCE
               ASSIGN TO SRCPRG
               FILE STATUS IS ORIGINAL-SOURCE-STATUS
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT UNIT-TEST-CONFIG
               ASSIGN TO UTSTCFG
               FILE STATUS IS UNIT-TEST-CONFIG-STATUS
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TEST-SOURCE
               ASSIGN TO TESTPRG
               FILE STATUS IS TEST-SOURCE-STATUS
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TEST-CASES ASSIGN TO UTESTS
               FILE STATUS IS TEST-CASES-STATUS
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  UNIT-TEST-CONFIG
           DATA RECORD IS UNIT-TEST-CONFIG-REC.
       01  UNIT-TEST-CONFIG-REC.
           05 COPYBOOK-NAME   PIC X(80).
       FD  ORIGINAL-SOURCE
           DATA RECORD IS ORIGINAL-LINE.
       01  ORIGINAL-LINE PIC X(80).
      
       FD  TEST-SOURCE
           DATA RECORD IS TEST-LINE.
       01  TEST-LINE                     PIC X(80).
       FD  TEST-CASES
           DATA RECORD IS TEST-CASE-LINE.
       01  TEST-CASE-LINE.
           05  FILLER                    PIC X(06).
           05  FILLER                    PIC X(01).
               88  TEST-CASE-COMMENT                VALUE '*'.
           05  FILLER                    PIC X(249).
      
       WORKING-STORAGE SECTION.
       01  FILLER PIC X(16) VALUE '******* ZUTZCPC'.
       01  FILLER PIC X(16) VALUE '******* 0.1.0'.
      *****************************************************************
      * Saved information about files that are accessed by the program
      * under test. If any MOCKs are declared in the test cases, we
      * will need this information to generate test code. We save the
      * information in the first pass of ORIGINAL-SOURCE so that we
      * won't have to pass the file multiple times.
      *
      * INTERNAL-FILENAME is declared in the SELECT statement.
      * RECORD-FIELD-NAME contains the name of the first record area
      * declared under the FD statement.
      * FILE-STATUS-FIELD-NAME contains the name of the FILE STATUS
      * field declared in the SELECT statement.
      *****************************************************************
       01  FILLER.
           05  THIS-PROGRAM PIC X(08) VALUE 'ZUTZCPC'.
           05  FILLER PIC X(16) VALUE 'WORKING-STORAGE'.
      
       01 TEST-CASE-UPPERED-LINE PIC X(256).
      
       01 ORIGINAL-UPPERED-LINE.
         03  ORIGINAL-LINE-FOR-REDEFINE.
           05  FILLER                    PIC X(06).
           05  COMMENT-CHAR              PIC X(1).
               88 COMMENTED-LINE VALUE '*'.
           05  SECTION-HEADER            PIC X(10).
               88  DATA-DIVISION-HEADER      VALUE 'DATA DIVIS'.
               88  FILE-CONTROL-HEADER       VALUE 'FILE-CONTR'.
               88  FILE-SECTION-HEADER       VALUE 'FILE SECTI'.
               88  WORKING-STORAGE-HEADER    VALUE 'WORKING-ST'.
               88  PROCEDURE-DIVISION-HEADER VALUE 'PROCEDURE '.
           05  FILLER REDEFINES SECTION-HEADER.
               10  FILLER                PIC X(03).
                   88  FD-STATEMENT          VALUE 'FD '.
               10  FILLER                PIC X(07).
           05  FILLER REDEFINES SECTION-HEADER.
               10  PARA-HEADER-AREA      PIC X(04).
               10  FILLER                PIC X(06).
           05  FILLER                    PIC X(63).
         03  FILLER REDEFINES ORIGINAL-LINE-FOR-REDEFINE.
           05  FILLER                    PIC X(07).
           05  PARAGRAPH-NAME            PIC X(31).
           05  FILLER                    PIC X(42).
      
       01  FILLER.
           05  MAX-FILES                     PIC 9(02) VALUE 10.
       01  FILLER                            PIC X(01) VALUE 'N'.
           88  FILENAME-MATCHED              VALUE 'Y'.
           88  FILENAME-NOT-MATCHED          VALUE 'N'.
       01  FILE-INFORMATION.
           05  FILE-INF OCCURS 10 INDEXED BY FILE-IX.
               10  INTERNAL-FILENAME         PIC X(31).
               10  RECORD-FIELD-NAME         PIC X(31).
               10  FILE-STATUS-FIELD-NAME    PIC X(31).
      
      *****************************************************************
      * Specifications for mocks.
      * This table is populated based on MOCK keywords found in the
      * unit TEST-CASES file. The data are then used to substitute
      * fake code in the TEST-SOURCE file.
      *****************************************************************
       01  FILLER.
           05  MOCK-STMT       PIC X(320).
           05  MOCK-SET-STMT   PIC X(320).
           05  MOCK-LOOKUP-KEY PIC X(320).
           05  MOCK-LOOKUP-KEYWORDS.
               10  MOCK-LOOKUP-KEYWORD
                   OCCURS 25 TIMES
                   INDEXED BY MOCK-LOOKUP-IX PIC X(31).
       01  FILLER.
           05  MOCK-FILE-STATUS-WORK      PIC X(02).
           05  FILLER                     PIC X.
               88  MOCK-SYNTAX-ERROR      VALUE 'Y'.
               88  NO-MOCK-SYNTAX-ERROR   VALUE 'N'.
           05  FILLER                     PIC X.
               88  MOCK-MATCHED           VALUE 'Y'.
               88  MOCK-NOT-MATCHED       VALUE 'N'.
           05  FILLER                     PIC X.
               88  COMMENT-THE-LINES      VALUE 'Y'.
               88  COPY-THE-LINES         VALUE 'N'.
           05  MAX-MOCKS                  PIC 9(02) VALUE 50.
           05  MAX-CICS-KEYWORDS          PIC 9(02) VALUE 25.
           05  MAX-MOCK-REPLACEMENT-STMTS   PIC 9(03) VALUE 250.
       01  MOCKS.
           05  MOCK-TEST-COUNT            PIC 9(04).
           05  MOCK-COUNT                 PIC S9(08) COMP.
           05  MOCK-LOOKUP.
               10  MOCK-LOOKUP-TYPE       PIC X(04).
               10  MOCK-LOOKUP-FILENAME   PIC X(31).
               10  MOCK-LOOKUP-OPERATION  PIC X(20).
               10  MOCK-LOOKUP-PARA-NAME  PIC X(31).
               10  MOCK-LOOKUP-SECT-NAME  PIC X(31).
           05  MOCK OCCURS 50 INDEXED BY MOCK-IX.
               10  MOCK-TYPE              PIC X(04).
                   88  MOCK-CALL          VALUE 'CALL'.
                   88  MOCK-CICS          VALUE 'CICS'.
                   88  MOCK-FILE          VALUE 'FILE'.
                   88  MOCK-PARAGRAPH     VALUE 'PARA'.
                   88  MOCK-SECTION       VALUE 'SECT'.
                   88  MOCK-SQL           VALUE 'SQL'.
               10  MOCK-RECORD            PIC X(2048).
               10  MOCK-DATA.
                   15  MOCK-KEY           PIC X(320).
                   15  FILLER             PIC X(20000).
               10  MOCK-FILE-DATA REDEFINES MOCK-DATA.
                   15  MOCK-FILENAME      PIC X(31).
                   15  MOCK-OPERATION     PIC X(20).
                   15  MOCK-FILE-STATUS   PIC X(02).
                   15  FILLER             PIC X(20267).
               10  MOCK-CALL-DATA REDEFINES MOCK-DATA.
                   15  MOCK-CALL-TOKENS-KEY PIC X(320).
                   15  FILLER              PIC X(20000).
               10  MOCK-CICS-DATA REDEFINES MOCK-DATA.
                   15  MOCK-CICS-KEYWORDS-KEY  PIC X(040).
                   15  MOCK-CICS-KEYWORDS-DATA PIC X(280).
                   15  FILLER              PIC X(20000).
               10  MOCK-PARA-DATA REDEFINES MOCK-DATA.
                   15  MOCK-PARA-NAME     PIC X(31).
                   15  FILLER             PIC X(20289).
               10  MOCK-SECT-DATA REDEFINES MOCK-DATA.
                   15  MOCK-SECT-NAME     PIC X(31).
                   15  FILLER             PIC X(20289).
               10  MOCK-SQL-DATA REDEFINES MOCK-DATA.
                   15  FILLER             PIC X(20320).
               10  FILLER REDEFINES MOCK-DATA.
                   15  FILLER             PIC X(320).
                   15  MOCK-REPLACEMENT-STMTS.
                       20  MOCK-REPLACEMENT-STMT
                           OCCURS 250 TIMES
                           INDEXED BY MOCK-REPLACEMENT-STMTS-IX
                           PIC X(80).
      
       01  FIND-SECTION.
           05  SECTION-NAME               PIC X(31).
           05  SECTION-FIND-KEYWORD       PIC X(8).
           05  SECTION-REMAINDER          PIC X(50).
      *****************************************************************
      * This area is used to handle file status values for file mocks.
      *****************************************************************
       01  FILLER.
           05  MAX-STATUS-MNEMONICS   PIC 9(02) VALUE 4.
           05  TEMP-SUB               PIC S9(08) COMP.
           05  SAVE-SUB               PIC S9(08) COMP.
           05  SAVE-OFFSET            PIC 9(04) VALUE 0 COMP.
           05  TEMP-TOKEN             PIC X(80).
           05  MOCK-FILE-STATUS-MNEMONICS.
               10  FILLER PIC X(20) VALUE 'OK'.
               10  FILLER PIC X(02) VALUE '00'.
               10  FILLER PIC X(20) VALUE 'END-OF-FILE'.
               10  FILLER PIC X(02) VALUE '10'.
               10  FILLER PIC X(20) VALUE 'EOF'.
               10  FILLER PIC X(02) VALUE '10'.
               10  FILLER PIC X(20) VALUE 'FILE-NOT-FOUND'.
               10  FILLER PIC X(02) VALUE '35'.
           05  FILLER REDEFINES MOCK-FILE-STATUS-MNEMONICS.
               10  FILLER OCCURS 4 INDEXED BY MOCK-STATUS-IX.
                   15  MOCK-FILE-STATUS-MNEMONIC  PIC X(20).
                   15  MOCK-FILE-STATUS-VALUE     PIC X(02).
      
      *****************************************************************
      * Specifications for parsing I/O statements from the
      * ORIGINAL-SOURCE file.
      *
      * VALID-FILE-OPERATION identifies an operation keyword such
      *     as READ or CLOSE.
      * NEXT-TOKEN-TO-GET gives the number of tokens to extract
      *     after the I/O verb to find the next interesting item in
      *     the source statement.
      *****************************************************************
       01  MAX-VALID-FILE-OPERATIONS      PIC 9(02) VALUE 6.
       01  VALID-FILE-OPERATIONS.
           05  FILLER PIC X(07) VALUE 'CLOSE'.
           05  FILLER PIC X(01) VALUE '1'.
           05  FILLER PIC X(07) VALUE 'DELETE'.
           05  FILLER PIC X(01) VALUE '1'.
           05  FILLER PIC X(07) VALUE 'OPEN'.
           05  FILLER PIC X(01) VALUE '2'.
           05  FILLER PIC X(07) VALUE 'READ'.
           05  FILLER PIC X(01) VALUE '1'.
           05  FILLER PIC X(07) VALUE 'REWRITE'.
           05  FILLER PIC X(01) VALUE '1'.
           05  FILLER PIC X(07) VALUE 'WRITE'.
           05  FILLER PIC X(01) VALUE '1'.
       01  FILLER REDEFINES VALID-FILE-OPERATIONS.
           05  FILLER OCCURS 6 TIMES
                      INDEXED BY VALID-FILE-OPERATION-IX.
               10  VALID-FILE-OPERATION PIC X(07).
               10  NEXT-TOKEN-TO-GET    PIC 9(01).
      
       01  FILLER.
           05  TEMP-STATEMENT               PIC X(80).
           05  TEMP-VALUE                   PIC X(2048).
           05  TEMP-VALUE-OFFSET            PIC 9(04) COMP.
           05  SAVE-VALUE                   PIC X(2048).
      
       01  MAX-SAVED-LINES                  PIC S9(03) VALUE 250.
       01  TALLY-FOR-SAVED-LINES            PIC S9(03) COMP-3.
       01  SAVED-LINES.
           05  SAVED-LINE
               OCCURS 250
               INDEXED BY SAVED-LINE-IX     PIC X(80).
       01 LAST-SAVED-LINE PIC X(80).
      
      *****************************************************************
      * These statements are generated by default for every mocked
      * EXEC CICS command. They are inserted in TEST-SOURCE prior to
      * any Cobol statements specified in the MOCK definition.
      *****************************************************************
       01  FILLER.
           05  DEFAULT-CICS-STATEMENT-COUNT PIC S9(02) VALUE 3.
           05  DEFAULT-CICS-STATEMENT-VALUES.
               10  FILLER PIC X(80) VALUE
               '           ADD 1 TO UT-MOCK-ACCESS-COUNT(UT-MOCK-IX)'.
               10  FILLER PIC X(80) VALUE
               '           MOVE ZERO TO EIBRESP'.
               10  FILLER PIC X(80) VALUE
               '           MOVE ZERO TO EIBRESP2'.
           05  DEFAULT-CICS-STATEMENTS REDEFINES
               DEFAULT-CICS-STATEMENT-VALUES.
               10  DEFAULT-CICS-STATEMENT
                   OCCURS 3
                   INDEXED BY DEFAULT-CICS-STATEMENT-IX
                   PIC X(80).
      
      *****************************************************************
      * Accumulate source statements that will be inserted into the
      * TEST-SOURCE file.
      *****************************************************************
       01  GENERATED-STATEMENT-LENGTH PIC 9(04) VALUE 1. 
       01  GENERATED-STATEMENT-INPUT PIC X(80).          
       01  MAX-GENERATED-STATEMENTS PIC S9(04) VALUE 250.
       01  GENERATED-SOURCE-STATEMENTS.
           05  GENERATED-SOURCE-STATEMENT
               OCCURS 250
               INDEXED BY STATEMENT-IX   PIC X(160) VALUE SPACES.
      
      *****************************************************************
      * Accumulate source statements that will be inserted into the
      * UT-INITIALIZE paragraph in the TEST-SOURCE file.
      *****************************************************************
       01  MAX-INITIALIZATION-STATEMENTS PIC S9(04) VALUE 250.
       01  INITIALIZATION-STATEMENTS.
           05  INITIALIZATION-STATEMENT
               OCCURS 250
               INDEXED BY INITIALIZATION-IX PIC X(80) VALUE SPACES.
      
      *****************************************************************
      * Accumulate source statements that will be part of the of the
      * UT-INITIALIZE-MOCK paragraph in the TEST-SOURCE file.
      *****************************************************************
       01  INITIALIZE-MOCK-STATEMENTS.
           05  INITIALIZE-MOCK-STATEMENT-MAX PIC 9(4) COMP VALUE 250.
           05  INITIALIZE-MOCK-STATEMENT
               OCCURS 250
               INDEXED BY INITIALIZE-MOCK-IX PIC X(80) VALUE SPACES.
      
      *****************************************************************
      * Accumulate source statements that will be inserted into the
      * UT-BEFORE paragraph in the TEST-SOURCE file.
      *****************************************************************
       01  MAX-BEFORE-EACH-STATEMENTS        PIC S9(04) VALUE 250.
       01  BEFORE-EACH-STATEMENTS.
           05  BEFORE-EACH-MAX PIC 9(4) COMP VALUE 0.
           05  BEFORE-EACH-STATEMENT
               OCCURS 250
               INDEXED BY BEFORE-EACH-IX PIC X(80) VALUE SPACES.
      
      *****************************************************************
      * Accumulate source statements that will be inserted into the
      * UT-AFTER paragraph in the TEST-SOURCE file.
      *****************************************************************
       01  MAX-AFTER-EACH-STATEMENTS         PIC S9(04) VALUE 250. 
       01  AFTER-EACH-STATEMENTS.
           05  AFTER-EACH-MAX PIC 9(4) COMP VALUE 0.
           05  AFTER-EACH-STATEMENT
               OCCURS 250
               INDEXED BY AFTER-EACH-IX PIC X(80) VALUE SPACES.
      
      *****************************************************************
      * This is a work area used when tokenizing a line of input from
      * the ORIGINAL-SOURCE file.
      *****************************************************************
       01  TOKENS.
            05                        PIC 9(1).
              88 NOT-IN-SINGLE-QUOTE        VALUE 0.
              88     IN-SINGLE-QUOTE        VALUE 1.
            05                        PIC 9(1).
              88 NOT-IN-DOUBLE-QUOTE        VALUE 0.
              88     IN-DOUBLE-QUOTE        VALUE 1.
           05  WS-COUNT               PIC S9(04) COMP.
           05  CURRENT-TOKEN-OFFSET   PIC 9(04) VALUE 0.
           05  TOKEN-TYPE             PIC X(8).
             88 UNKNOWN-TOKEN VALUE 'UNKNOWN'.
             88 IN-TOKEN      VALUE 'IN'.
             88 NOT-TOKEN     VALUE 'NOT'.
             88 TO-TOKEN      VALUE 'TO'.
             88 BE-TOKEN      VALUE 'BE'.
           05  FILLER OCCURS 100 TIMES INDEXED BY TOKEN-IX.
               10  TOKEN              PIC X(120).
               10  ORIGINAL-TOKEN     PIC X(120).
               10  TOKEN-LENGTH       PIC 9(4) COMP.
               10  FILLER             PIC X(01).
                   88  TOKEN-HAS-PERIOD    VALUE 'Y'.
                   88  TOKEN-HAS-NO-PERIOD VALUE 'N'.
      
      *****************************************************************
      * This area is used for parsing keywords out of input records.
      *****************************************************************
       01  FILLER.
           05  RAW-VALUE.
               10  RAW-VALUE-1        PIC X(72).
               10  RAW-VALUE-2        PIC X(1976).
           05  QUOTED-VALUE           PIC X(2048).
           05  LINE-TO-PARSE          PIC X(256).
           05  LINE-TO-PARSE-END      PIC 9(02) COMP.
           05  KEYWORD-MATCHED-INDICATOR PIC X(01) VALUE 'N'.
               88  KEYWORD-MATCHED              VALUE 'Y'.
               88  KEYWORD-NOT-MATCHED          VALUE 'N'.
           05  WS-CHARS               PIC 9(04) VALUE ZERO COMP.
           05  WS-NEXT                PIC 9(04) VALUE ZERO COMP.
           05  WS-LEFT                PIC 9(04) COMP-5 VALUE ZERO.
           05  WS-IX                  PIC 9(04) COMP-5 VALUE ZERO.
           05  WS-SUBSCRIPT           PIC X(80) VALUE SPACE.
           05  WS-SUB-PTR             PIC 9(04) COMP-5 VALUE ZERO.
           05  WS-LEADING             PIC 9(04) COMP-5 VALUE ZERO.
           05  WS-STRING-DELIMITER    PIC X(01) VALUE SPACE.
           05  KEYWORD-TO-MATCH       PIC X(20) VALUE SPACES.
           05  KEYWORD-MATCH-LENGTH   PIC 9(04) VALUE ZERO.
           05  KEYWORD-OFFSET         PIC 9(04) VALUE 0 COMP.
           05  KEYWORD-BEGIN          PIC 9(04) VALUE 0 COMP.
           05  KEYWORD-SEARCH-LIMIT   PIC 9(04) VALUE 0.
           05  KEYWORD-END            PIC 9(04) VALUE 72 COMP.
           05  KEYWORD-END-TEST       PIC 9(04) VALUE 256 COMP.
           05  KEYWORD-END-SRC        PIC 9(04) VALUE 72 COMP.
           05  KEYWORD-START          PIC 9(04) VALUE 12 COMP.
           05  MAX-KEYWORDS           PIC 9(02) VALUE 8.
           05  KEYWORD-VALUES.
               10  FILLER PIC X(20) VALUE 'EXPECT'.
               10  FILLER PIC 9(02) VALUE 6 COMP.
               10  FILLER PIC X(20) VALUE 'TESTCASE'.
               10  FILLER PIC 9(02) VALUE 8 COMP.
               10  FILLER PIC X(20) VALUE 'TESTSUITE'.
               10  FILLER PIC 9(02) VALUE 9 COMP.
               10  FILLER PIC X(20) VALUE 'BEFORE-EACH'.
               10  FILLER PIC 9(02) VALUE 11 COMP.
               10  FILLER PIC X(20) VALUE 'AFTER-EACH'.
               10  FILLER PIC 9(02) VALUE 10 COMP.
               10  FILLER PIC X(20) VALUE 'MOCK'.
               10  FILLER PIC 9(02) VALUE 4 COMP.
               10  FILLER PIC X(20) VALUE 'VERIFY'.
               10  FILLER PIC 9(02) VALUE 6 COMP.
               10  FILLER PIC X(20) VALUE 'IGNORE'.
               10  FILLER PIC 9(02) VALUE 6 COMP.
           05  KEYWORDS REDEFINES KEYWORD-VALUES.
               10  KEYWORD OCCURS 8 INDEXED BY KEYWORD-IX.
                   15  KEYWORD-VALUE      PIC X(20).
                   15  KEYWORD-LENGTH     PIC 9(02) COMP.
           05  CANDIDATE-TEST-KEYWORD     PIC X(20).
               88  IS-A-TEST-KEYWORD      VALUE 'EXPECT',
                                                'TESTCASE',
                                                'TESTSUITE',
                                                'BEFORE-EACH',
                                                'AFTER-EACH',
                                                'MOCK',
                                                'VERIFY',
                                                'IGNORE',
                                                '.'.
      
       01  FILLER.
           05  IGNORE-MAX PIC S9(02) VALUE 2.
           05  KEYWORDS-TO-IGNORE-VALUES.
               10  FILLER PIC X(80) VALUE ' TESTSUITE '.
               10  FILLER PIC S9(02) VALUE 11.
               10  FILLER PIC X(80) VALUE ' TESTCASE '.
               10  FILLER PIC S9(02) VALUE 10.
           05  KEYWORDS-TO-IGNORE REDEFINES KEYWORDS-TO-IGNORE-VALUES.
               10  FILLER OCCURS 2 INDEXED BY IGNORE-IX.
                   15  KEYWORD-TO-IGNORE PIC X(80).
                   15  KEYWORD-TO-IGNORE-LENGTH PIC S9(02).
      
      *****************************************************************
      * Track current input phase for better error messages
      *****************************************************************
       01  FILLER.
           05  CURRENT-PHASE                PIC X(09) VALUE SPACES.
               88 PARSE-FILE-CONTROLL       VALUE 'FILE-CONT'.
               88 PARSE-WORKING-STORAGE     VALUE 'WS-HEADER'.
               88 PARSE-PROCEDURE-DIVISION  VALUE 'PROC-DIV'.
               88 PARSE-SELECT-STATEMENT    VALUE 'SELECT'.
               88 PARSE-FD-STATEMENT        VALUE 'FD'.
               88 PARSE-CHECK-FOR-MOCKABLES VALUE 'MOCKS'.
               88 PARSE-OTHER               VALUE 'MAIN CODE'.
      
      *****************************************************************
      * Constants
      *****************************************************************
       01  FILLER.
           05  STATEMENT-OFFSET             PIC 9(02) VALUE 12.
           05  STATEMENT-LENGTH             PIC 9(02) VALUE 70.
           05  STATEMENT-CONTINUATION PIC X(12) VALUE '      -    '''.
           05  PROCEDURE-DIVISION-COPYBOOK  PIC X(08) VALUE 'ZUTZCPD'.
           05  CALL-KEYWORD                 PIC X(04) VALUE 'CALL'.
           05  CICS-KEYWORD                 PIC X(04) VALUE 'CICS'.
           05  COPY-KEYWORD                 PIC X(06) VALUE ' COPY '.
           05  ELSE-KEYWORD                 PIC X(04) VALUE 'ELSE'.
           05  END-CALL-KEYWORD             PIC X(08) VALUE 'END-CALL'.
           05  END-EXEC-KEYWORD             PIC X(08) VALUE 'END-EXEC'.
           05  END-MOCK-KEYWORD             PIC X(08) VALUE 'END-MOCK'.
           05  ENDIF-KEYWORD                PIC X(06) VALUE 'END-IF'.
           05  EXEC-KEYWORD-VARIANTS.
               10  FILLER                   PIC X(07) VALUE 'EXEC   '.
               10  FILLER                   PIC S9(02) VALUE 5 COMP.
               10  FILLER                   PIC S9(02) VALUE 4 COMP.
               10  FILLER                   PIC X(07) VALUE 'EXECUTE'.
               10  FILLER                   PIC S9(02) VALUE 7 COMP.
               10  FILLER                   PIC S9(02) VALUE 7 COMP.
           05  EXEC-KEYWORDS REDEFINES EXEC-KEYWORD-VARIANTS.
               10  FILLER OCCURS 2 INDEXED BY EXEC-KEYWORD-IX.
                   15  EXEC-KEYWORD         PIC X(07).
                   15  EXEC-KEYWORD-LENGTH  PIC S9(02) COMP.
                   15  EXEC-KEYWORD-ADJUST  PIC S9(02) COMP.
           05  FALSE-KEYWORD                PIC X(05) VALUE 'FALSE'.
           05  FILE-KEYWORD                 PIC X(04) VALUE 'FILE'.
           05  HAPPENED-KEYWORD             PIC X(08) VALUE 'HAPPENED'.
           05  IS-KEYWORD                   PIC X(04) VALUE ' IS '.
           05  MOCK-KEYWORD                 PIC X(04) VALUE 'MOCK'.
           05  NEVER-KEYWORD                PIC X(05) VALUE 'NEVER'.
           05  NUMERIC-KEYWORD              PIC X(07) VALUE 'NUMERIC'.
           05  ONCE-KEYWORD                 PIC X(04) VALUE 'ONCE'.
           05  PARA-KEYWORD                 PIC X(04) VALUE 'PARA'.
           05  PARAGRAPH-KEYWORD            PIC X(09) VALUE 'PARAGRAPH'.
           05  SECT-KEYWORD                 PIC X(04) VALUE 'SECT'.
           05  SECTION-KEYWORD              PIC X(07) VALUE 'SECTION'.
           05  SELECT-KEYWORD               PIC X(06) VALUE 'SELECT'.
           05  SQL-KEYWORD                  PIC X(03) VALUE 'SQL'.
           05  TO-KEYWORD                   PIC X(04) VALUE ' TO '.
           05  TRUE-KEYWORD                 PIC X(04) VALUE 'TRUE'.
           05  ZERO-KEYWORD                 PIC X(06) VALUE ' ZERO '.
      
           05  IF-VERB                      PIC X(03) VALUE 'IF '.
           05  MOVE-VERB                    PIC X(05) VALUE 'MOVE '.
           05  TO-VERB                      PIC X(05) VALUE '  TO '.
           05  OPEN-VERB                    PIC X(04) VALUE 'OPEN'.
           05  PERFORM-VERB                 PIC X(08) VALUE 'PERFORM '.
           05  READ-VERB                    PIC X(04) VALUE 'READ'.
           05  SET-VERB                     PIC X(03) VALUE 'SET'.
           05  WRITE-VERB                   PIC X(05) VALUE 'WRITE'.
           05  DISPLAY-VERB                 PIC X(08) VALUE 'DISPLAY '.
      
           05  SINGLE-QUOTE                 PIC X(01) VALUE "'".
           05  COMMENT-MARKER               PIC X(05) VALUE '*ZUT>'.
           05  PERIOD                       PIC X(01) VALUE '.'.
           05  STATEMENT-SPACER             PIC X(11) VALUE SPACES.
           05  PARAGRAPH-END-MARKER         PIC X(12)
                                            VALUE '           .'.
           05  PARAGRAPH-EMPTY-PREVENTER    PIC X(19)
                                            VALUE '           CONTINUE'.
           05  BEFORE-PARAGRAPH-HEADER      PIC X(17)
                                            VALUE '       UT-BEFORE.'.
           05  AFTER-PARAGRAPH-HEADER       PIC X(17)
                                            VALUE '       UT-AFTER.'.
           05  INIT-PARAGRAPH-HEADER        PIC X(22)
                                        VALUE '       UT-INITIALIZE.'.
           05  INIT-MOCK-PARAGRAPH-HEADER   PIC X(27)
                                    VALUE '       UT-INITIALIZE-MOCK.'.
           05  END-PARAGRAPH-HEADER         PIC X(22)
                                        VALUE '       UT-END.'.
           05  INCR-TESTCASE-COUNT-STMT     PIC X(80) VALUE
               '           ADD 1 TO UT-TEST-CASE-COUNT'.
           05  INCR-MOCK-ACCESS-COUNT-STMT  PIC X(80) VALUE
               '           ADD 1 TO UT-MOCK-ACCESS-COUNT(UT-MOCK-IX)'.
           05  SET-REVERSE-TRUE-STMT        PIC X(80) VALUE
               '           SET UT-REVERSE-COMPARE TO TRUE'.
           05  SET-NORMAL-TRUE-STMT        PIC X(80) VALUE
               '           SET UT-NORMAL-COMPARE TO TRUE'.
           05  SET-MOCK-STMT                PIC X(80) VALUE
               '           PERFORM UT-SET-MOCK'.
           05  ENDIF-STMT                   PIC X(80) VALUE
               '           END-IF'.
           05  SET-VERIFY-EXACT-STMT        PIC X(80) VALUE
               '           SET UT-VERIFY-EXACT TO TRUE'.
           05  SET-VERIFY-AT-LEAST-STMT     PIC X(80) VALUE
               '           SET UT-VERIFY-AT-LEAST TO TRUE'.
           05  SET-VERIFY-NO-MORE-THAN-STMT PIC X(80) VALUE
               '           SET UT-VERIFY-NO-MORE-THAN TO TRUE'.
           05  SET-FIND-FILE-MOCK-STMT      PIC X(80) VALUE
               '           SET UT-FIND-FILE-MOCK TO TRUE'.
           05  SET-FIND-CALL-MOCK-STMT      PIC X(80) VALUE
               '           SET UT-FIND-CALL-MOCK TO TRUE'.
           05  SET-FIND-CICS-MOCK-STMT      PIC X(80) VALUE
               '           SET UT-FIND-CICS-MOCK TO TRUE'.
           05  SET-FIND-PARA-MOCK-STMT      PIC X(80) VALUE
               '           SET UT-FIND-PARA-MOCK TO TRUE'.
           05  SET-FIND-SECT-MOCK-STMT      PIC X(80) VALUE
               '           SET UT-FIND-SECT-MOCK TO TRUE'.
           05  ZUT-MARKERVALUE-BEGIN        PIC X(80) VALUE
               '      * ZUTEYECATCHERBEGIN'.
           05  ZUT-MARKERVALUE-END          PIC X(80) VALUE
               '      * ZUTEYECATCHEREND'.
           05  UT-MOCK-FIND-CALL-TOKENS-STMT PIC X(80) VALUE
               '           UT-MOCK-FIND-CALL-TOKENS'.
           05  UT-MOCK-FIND-CICS-KEYWO-STMT    PIC X(80) VALUE
               '           UT-MOCK-FIND-CICS-KEYWORDS'.
           05  UT-MOCK-FIND-SECT-NAME-STMT     PIC X(80) VALUE
               '           UT-MOCK-FIND-SECT-NAME'.
           05  UT-MOCK-FIND-PARA-NAME-STMT     PIC X(80) VALUE
               '           UT-MOCK-FIND-PARA-NAME'.
      
      *****************************************************************
      * These definitions are used to identify COBOL primary keywords
      * to end a CALL.
      *****************************************************************
       01  COBOL-PRIMARY-RESERVED-KEYWORD   PIC X(31) VALUE SPACES.
           88  KEYWORD-IS-COBOL-RESERVED  VALUE 'ACCEPT',
                                                'ADD',
                                                'CALL',
                                                'CLOSE',
                                                'COMPUTE',
                                                'CONTINUE',
                                                'COPY',
                                                'DISPLAY',
                                                'DIVIDE',
                                                'ELSE',
                                                'END-ADD',
                                                'END-CALL',
                                                'END-COMPUTE',
                                                'END-DELETE',
                                                'END-DIVIDE',
                                                'END-EVALUATE',
                                                'END-IF',
                                                'END-MULTIPLY',
                                                'END-READ',
                                                'END-STRING',
                                                'END-SUBTRACT',
                                                'END-UNSTRING',
                                                'END-WRITE',
                                                'EVALUATE',
                                                'EXEC',
                                                'EXIT',
                                                'GO',
                                                'GOBACK',
                                                'IF',
                                                'INITIALIZE',
                                                'INSPECT',
                                                'MOVE',
                                                'MULTIPLY',
                                                'OPEN',
                                                'PERFORM',
                                                'READ',
                                                'SET',
                                                'STOP',
                                                'STRING',
                                                'SUBTRACT',
                                                'UNSTRING',
                                                'WHEN',
                                                'WRITE'.
      *****************************************************************
      * These definitions are used to identify CICS reserved words when
      * parsing a MOCK specification.
      *****************************************************************
       01  CANDIDATE-TOKEN                  PIC X(31).
       01  CANDIDATE-CICS-RESERVED-WORD     PIC X(31) VALUE SPACES.
           88  TOKEN-IS-CICS-RESERVED-WORD  VALUE 'ABCODE',
                                                  'ABEND',
                                                  'ABSTIME',
                                                  'ACCUM',
                                                  'ACEE',
                                                  'ACTION',
                                                  'ADDRESS',
                                                  'AFTER',
                                                  'AID',
                                                  'ALARM',
                                                  'ALLOCATE',
                                                  'APPLID',
                                                  'ASKTIME',
                                                  'ASIS',
                                                  'ASSIGN',
                                                  'AT',
                                                  'ATTACH',
                                                  'ATTACHID',
                                                  'ATTRIBUTES',
                                                  'AUTOPAGE',
                                                  'AUXILIARY',
                                                  'BELOW',
                                                  'BIF',
                                                  'BUILD',
                                                  'CANCEL',
                                                  'CHANGE',
                                                  'CICSKEY',
                                                  'COMMAREA',
                                                  'COMPLETE',
                                                  'CONDITION',
                                                  'CONFIRM',
                                                  'CONNECT',
                                                  'CONTROL',
                                                  'CONV',
                                                  'CONVERSE',
                                                  'CONVID',
                                                  'CRITICAL',
                                                  'CSA',
                                                  'CTLCHAR',
                                                  'CURSOR',
                                                  'CWA',
                                                  'DATA',
                                                  'DATAONLY',
                                                  'DATAPOINTER',
                                                  'DATASET',
                                                  'DATASTR',
                                                  'DATE',
                                                  'DATEFORM',
                                                  'DATESEP',
                                                  'DAYCOUNT',
                                                  'DAYOFMONTH',
                                                  'DAYOFWEEK',
                                                  'DCT',
                                                  'DDMMYY',
                                                  'DEEDIT',
                                                  'DEFRESP',
                                                  'DELAY',
                                                  'DELETE',
                                                  'DELETEQ',
                                                  'DEQ',
                                                  'DEST',
                                                  'DUMP',
                                                  'DUMPCODE',
                                                  'ECADDR',
                                                  'EIB',
                                                  'ENDBR',
                                                  'ENQ',
                                                  'ENTER',
                                                  'ENTRY',
                                                  'EQUAL',
                                                  'ERASE',
                                                  'ERASEAUP',
                                                  'ERRTERM',
                                                  'ESMRESP',
                                                  'EVENT',
                                                  'EVENTUAL',
                                                  'EXCEPTION',
                                                  'EXTRACT',
                                                  'FCT',
                                                  'FIELD',
                                                  'FILE',
                                                  'FOR',
                                                  'FORMATTIME',
                                                  'FORMFEED',
                                                  'FREE',
                                                  'FREEKB',
                                                  'FREEMAIN',
                                                  'FROM',
                                                  'FROMFLENGTH',
                                                  'FROMLENGTH',
                                                  'FRSET',
                                                  'GENERIC',
                                                  'GETMAIN',
                                                  'GTEQ',
                                                  'HANDLE',
                                                  'HEADER',
                                                  'HOLD',
                                                  'HONEOM',
                                                  'HOURS',
                                                  'IGNORE',
                                                  'IMMEDIATE',
                                                  'INITIMG',
                                                  'INPUTMSG',
                                                  'INPUTMSGLEN',
                                                  'INTERVAL',
                                                  'INTO',
                                                  'INVITE',
                                                  'ITEM',
                                                  'IUTYPE',
                                                  'JOURNALNUM',
                                                  'JTYPEID',
                                                  'JUSFIRST',
                                                  'JUSTIFY',
                                                  'JUSLAST',
                                                  'KEYLENGTH',
                                                  'L40',
                                                  'L64',
                                                  'L80',
                                                  'LABEL',
                                                  'LAST',
                                                  'LENGTH',
                                                  'LENGTHLIST',
                                                  'LINK',
                                                  'LIST',
                                                  'LOAD',
                                                  'LUW',
                                                  'MAIN',
                                                  'MAP',
                                                  'MAPONLY',
                                                  'MAPSET',
                                                  'MASSINSERT',
                                                  'MAXFLENGTH',
                                                  'MAXLENGTH',
                                                  'MAXLIFETIME',
                                                  'MAXPROCLEN',
                                                  'MESSAGE',
                                                  'MINUTES',
                                                  'MMDDYY',
                                                  'MONTHOFYEAR',
                                                  'NATLANG',
                                                  'NEWPASSWORD',
                                                  'NEXT',
                                                  'NLEOM',
                                                  'NOAUTOPAGE',
                                                  'NOCHECK',
                                                  'NODUMP',
                                                  'NOEDIT',
                                                  'NOHANDLE',
                                                  'NOQUEUE',
                                                  'NOSUSPEND',
                                                  'NOTRUNCATE',
                                                  'NUMITEMS',
                                                  'NUMREC',
                                                  'NUMROUTES',
                                                  'NUMSEGMENTS',
                                                  'OIDCARD',
                                                  'OPCLASS',
                                                  'OPERATOR',
                                                  'OPERPURGE',
                                                  'PAGE',
                                                  'PAGING',
                                                  'PASSWORD',
                                                  'PCT',
                                                  'PFXLENG',
                                                  'PIPLENGTH',
                                                  'PIPLIST',
                                                  'POP',
                                                  'POST',
                                                  'PPT',
                                                  'PREFIX',
                                                  'PRINT',
                                                  'PRIORITY',
                                                  'PROCESS',
                                                  'PROCLENGTH',
                                                  'PROCNAME',
                                                  'PROFILE',
                                                  'PROGRAM',
                                                  'PROTECT',
                                                  'PURGE',
                                                  'PUSH',
                                                  'QUEUE',
                                                  'RBA',
                                                  'READ',
                                                  'READNEXT',
                                                  'READPREV',
                                                  'READQ',
                                                  'RECEIVE',
                                                  'RECFM',
                                                  'RELEASE',
                                                  'REPLY',
                                                  'REPLYLENGTH',
                                                  'REQID',
                                                  'RESET',
                                                  'RESETBR',
                                                  'RESOURCE',
                                                  'RETAIN',
                                                  'RETRIEVE',
                                                  'RETURN',
                                                  'REWRITE',
                                                  'RIDFLD',
                                                  'ROLLBACK',
                                                  'ROUTE',
                                                  'ROUTECODES',
                                                  'RPROCESS',
                                                  'RRESOURCE',
                                                  'RRN',
                                                  'RTERMID',
                                                  'RTRANSID',
                                                  'SECONDS',
                                                  'SEGMENTLIST',
                                                  'SEND',
                                                  'SESSION',
                                                  'SET',
                                                  'SHARED',
                                                  'SIGNOFF',
                                                  'SIGNON',
                                                  'SIT',
                                                  'START',
                                                  'STARTBR',
                                                  'STARTIO',
                                                  'STATE',
                                                  'STORAGE',
                                                  'STRFIELD',
                                                  'SUSPEND',
                                                  'SYNCLEVEL',
                                                  'SYNCONRETURN',
                                                  'SYNCPOINT',
                                                  'SYSID',
                                                  'TABLES',
                                                  'TASK',
                                                  'TCT',
                                                  'TCTUA',
                                                  'TD',
                                                  'TERMINAL',
                                                  'TEXT',
                                                  'TEXTLENGTH',
                                                  'TIME',
                                                  'TIMEOUT',
                                                  'TIMESEP',
                                                  'TITLE',
                                                  'TOFLENGTH',
                                                  'TOLENGTH',
                                                  'TRACEID',
                                                  'TRACENUM',
                                                  'TRAILER',
                                                  'TRANSACTION',
                                                  'TRANSID',
                                                  'TRT',
                                                  'TS',
                                                  'TWA',
                                                  'UNLOCK',
                                                  'UNTIL',
                                                  'UPDATE',
                                                  'USERID',
                                                  'USERKEY',
                                                  'WAIT',
                                                  'WRITE',
                                                  'WRITEQ',
                                                  'XCTL',
                                                  'YYDDD',
                                                  'YYDDMM',
                                                  'YYMMDD',
      
      * Condition names
                                                  'ALLOCERR',
                                                  'CBIDERR',
                                                  'CHANNELERR',
                                                  'DISABLED',
                                                  'DSSTAT',
                                                  'DUPKEY',
                                                  'DUPREC',
                                                  'ENDDATA',
                                                  'ENDFILE',
                                                  'ENDINPT',
                                                  'ENQBUSY',
                                                  'ENVDEFERR',
                                                  'EOC',
                                                  'EODS',
                                                  'EOF',
                                                  'EXPIRED',
                                                  'FILENOTFOUND',
                                                  'FUNCERR',
                                                  'IGREQCD',
                                                  'IGREQID',
                                                  'ILLOGIC',
                                                  'INBFMH',
                                                  'INVERRTERM',
                                                  'INVEXITREQ',
                                                  'INVLDC',
                                                  'INVMPSZ',
                                                  'INVPARTN',
                                                  'INVPARTNSET',
                                                  'INVREQ',
                                                  'IOERR',
                                                  'ISCINVREQ',
                                                  'ITEMERR',
                                                  'JIDERR',
                                                  'LENGERR',
                                                  'LOADING',
                                                  'LOCKED',
                                                  'MAPFAIL',
                                                  'NETNAMEIDERR',
                                                  'NODEIDERR',
                                                  'NOJBUFSP',
                                                  'NONVAL',
                                                  'NOPASSBKRD',
                                                  'NOPASSBKWR',
                                                  'NOSPACE',
                                                  'NOSPOOL',
                                                  'NOSTART',
                                                  'NOSTG',
                                                  'NOTALLOC',
                                                  'NOTAUTH',
                                                  'NOTFND',
                                                  'NOTOPEN',
                                                  'OPENERR',
                                                  'OVERFLOW',
                                                  'PARTNERIDERR',
                                                  'PARTNFAIL',
                                                  'PGMIDERR',
                                                  'QBUSY',
                                                  'QIDERR',
                                                  'QZERO',
                                                  'RDATT',
                                                  'RESUNAVAIL',
                                                  'RETPAGE',
                                                  'ROLLEDBACK',
                                                  'RTEFAIL',
                                                  'RTESOME',
                                                  'SELNERR',
                                                  'SESSBUSY',
                                                  'SESSIONERR',
                                                  'SIGNAL',
                                                  'SPOLBUSY',
                                                  'SPOLERR',
                                                  'STRELERR',
                                                  'SUPPRESSED',
                                                  'SYSBUSY',
                                                  'SYSIDERR',
                                                  'TERMERR',
                                                  'TERMIDERR',
                                                  'TRANSIDERR',
                                                  'TSIOERR',
                                                  'UNEXPIN',
                                                  'USERIDERR',
                                                  'WRBRK'.
      
      
      *****************************************************************
      * Used by 7970-INSPECT-REPLACE.
      * Workaround for lack of support for INSPECT REPLACING in older
      * versions of mainframe Cobol.
      *****************************************************************
       01  INSPECT-REPLACING-FIELDS.
           05  FILLER.
               10  TARGET-STRING          PIC X(80).
               10  TEMP-STRING            PIC X(80).
               10  SEARCH-STRING          PIC X(80).
               10  REPLACE-STRING         PIC X(80).
           05  FILLER USAGE BINARY.
               10  TARGET-STRING-LENGTH   PIC S9(04).
               10  SEARCH-STRING-LENGTH   PIC S9(04).
               10  REPLACE-STRING-LENGTH  PIC S9(04).
               10  INSPECT-MAX            PIC S9(04).
               10  INSPECT-SUB            PIC S9(04).
               10  CUT-LENGTH             PIC S9(04).
               10  CONCAT-OFFSET          PIC S9(04).
               10  CONCAT-LENGTH          PIC S9(04).
      
      *****************************************************************
      * File status codes
      *****************************************************************
       01  FILLER.
           05  UNIT-TEST-CONFIG-STATUS       PIC 9(02) VALUE ZERO.
               88  UNIT-TEST-CONFIG-STATUS-OK         VALUE ZERO.
               88  UNIT-TEST-CONFIG-END-OF-FILE       VALUE 10.
               88  UNIT-TEST-CONFIG-NOT-FOUND         VALUE 35.
               88  UNIT-TEST-CONFIG-NOT-READABLE      VALUE 37.
           05  ORIGINAL-SOURCE-STATUS        PIC 9(2) VALUE ZERO.
               88  ORIGINAL-SOURCE-STATUS-OK          VALUE ZERO.
               88  ORIGINAL-SOURCE-END-OF-FILE        VALUE 10.
               88  ORIGINAL-SOURCE-NOT-FOUND          VALUE 35.
               88  ORIGINAL-SOURCE-NOT-READABLE       VALUE 37.
               88  ORIGINAL-SOURCE-EXHAUSTED          VALUE 46.
           05  TEST-CASES-STATUS             PIC 9(2) VALUE ZERO.
               88  TEST-CASES-STATUS-OK               VALUE ZERO.
               88  TEST-CASES-END-OF-FILE             VALUE 10.
               88  TEST-CASES-FILE-NOT-FOUND          VALUE 35.
               88  TEST-CASES-NOT-READABLE            VALUE 37.
               88  TEST-CASES-EXHAUSTED               VALUE 46.
           05  TEST-SOURCE-STATUS            PIC 9(2) VALUE ZERO.
               88  TEST-SOURCE-STATUS-OK              VALUE ZERO.
               88  TEST-SOURCE-FILE-NOT-FOUND         VALUE 35.
               88  TEST-SOURCE-NOT-WRITABLE           VALUE 37.
      
      *****************************************************************
      * Indicators, flags, switches, etc.
      *****************************************************************
       01  FILLER.
           05  FILLER                    PIC X VALUE SPACE.
               88  DATATYPE-X                  VALUE SPACE.
               88  DATATYPE-9                  VALUE '9'.
               88  DATATYPE-88                 VALUE 'B'.
           05  FILLER                    PIC X VALUE SPACE.
               88  NORMAL-COMPARE              VALUE 'N'.
               88  REVERSE-COMPARE             VALUE 'Y'.
           05  FILLER                    PIC X VALUE '1'.
               88  VERIFY-EXACT                VALUE '1'.
               88  VERIFY-AT-LEAST             VALUE '2'.
               88  VERIFY-NO-MORE-THAN         VALUE '3'.
           05  FILLER                    PIC X VALUE SPACE.
               88  NOT-IGNORE-TESTCASE         VALUE 'N'.
               88  IGNORE-TESTCASE             VALUE 'Y'.
           05  FILLER                    PIC X VALUE SPACE.
               88  NOT-1ST-TEST-AFTER-IGNORE VALUE 'N'.
               88  1ST-TEST-AFTER-IGNORE VALUE 'Y'.
           05  FILLER                    PIC X VALUE SPACE.
               88  NOT-BEYOND-PROCEDURE-HEADER VALUE 'N'.
               88  BEYOND-PROCEDURE-HEADER     VALUE 'Y'.
           05  FILLER                    PIC X VALUE SPACE.
               88  NOT-FIRST-TOKEN             VALUE 'N'.
               88  IS-FIRST-TOKEN              VALUE 'Y'.
           05  FILLER                    PIC X VALUE SPACE.
               88  MORE-TOKENS                 VALUE 'N'.
               88  NO-MORE-TOKENS              VALUE 'Y'.
           05  FILLER                    PIC X VALUE SPACE.
               88  END-OF-FILE                 VALUE 'Y'.
           05  FILLER                    PIC X VALUE SPACE.
               88  SPACE-FOUND                 VALUE 'N'.
               88  NON-SPACE-FOUND             VALUE 'Y'.
           05  FILLER                    PIC X VALUE SPACE.
               88  NOT-END-OF-STATEMENT        VALUE 'N'.
               88  END-OF-STATEMENT            VALUE 'Y'.
           05  FILLER                    PIC X VALUE SPACE.
               88  END-OF-TEST-CASES           VALUE 'Y'.
           05  COPY-SOURCE-INDICATOR     PIC X VALUE SPACE.
               88  COPY-SOURCE                 VALUE 'N'.
               88  SUPPRESS-COPY-SOURCE        VALUE 'Y'.
           05  FILLER                    PIC X VALUE SPACE.
               88  GENERATE-PERFORM-AFTER      VALUE 'Y'.
           05  FILLER                    PIC X VALUE SPACE.
               88  NOT-SCANNING-TEST-CASES     VALUE 'N'.
               88  SCANNING-TEST-CASES         VALUE 'Y'.
           05  FILLER                    PIC X VALUE SPACE.
               88  NOT-SCANNING-ORIGINAL-SOURCE VALUE 'N'.
               88  SCANNING-ORIGINAL-SOURCE    VALUE 'Y'.
           05  FILLER                    PIC X VALUE SPACE.
               88  NO-CHECK-FOR-MOCKABLES      VALUE 'N'.
               88  CHECK-FOR-MOCKABLES         VALUE 'Y'.
           05  FILLER                    PIC X VALUE SPACE.
               88  MORE-RESERVED-WORDS         VALUE 'N'.
               88  NO-MORE-RESERVED-WORDS      VALUE 'Y'.
           05  FILLER                    PIC X VALUE SPACE.
               88  NO-SAVE-ORIGINAL-LINES      VALUE 'N'.
               88  SAVE-ORIGINAL-LINES         VALUE 'Y'.
           05  FILLER                    PIC X VALUE SPACE.
               88  NO-SUPPRESS-TEST-CASE-READ  VALUE 'N'.
               88  SUPPRESS-TEST-CASE-READ     VALUE 'Y'.
           05  FILLER                    PIC X(01) VALUE 'N'.
               88  STRING-FOUND                VALUE 'Y'.
               88  STRING-NOT-FOUND            VALUE 'N'.
           05  FILLER                    PIC X(01) VALUE 'N'.
               88  MOCKABLE-FOUND              VALUE 'Y'.
               88  MOCKABLE-NOT-FOUND          VALUE 'N'.
           05  FILLER                    PIC X(01) VALUE 'N'.
               88  SELECT-STATEMENT              VALUE 'Y'.
               88  NOT-SELECT-STATEMENT          VALUE 'N'.
      
      *****************************************************************
      * Areas to build COPY statements to insert in the test program.
      *****************************************************************
       01  FILLER.
           05  WS-WORKING-STORAGE-COPY   PIC X(12).
           05  WS-PROCEDURE-COPY         PIC X(12).
       01  WS-COPY-LINE.
           05  FILLER                    PIC X(17)
                                         VALUE '            COPY '.
           05  WS-COPY-NAME              PIC X(12).
           05  FILLER                    PIC X(51) VALUE '.'.
      
      *****************************************************************
      * Error messages
      *****************************************************************
       01  ERROR-MESSAGES.
      
           05  WS-CURRENT-TESTCASE             PIC X(80) VALUE SPACES.
           05  ERROR-MESSAGE-LINE              PIC X(80) VALUE SPACES.
      
           05  MSG-ORIG-SOURCE-NOT-FOUND.
               10  FILLER PIC X(06) VALUE '001-E '.
               10  FILLER PIC X(74) VALUE
                   'ORIGINAL SOURCE FILE SRCPRG NOT FOUND'.
      
           05  MSG-ORIG-SOURCE-UNREADABLE.
               10  FILLER PIC X(06) VALUE '002-E '.
               10  FILLER PIC X(76) VALUE
               'ORIGINAL SOURCE FILE SRCPRG CAN''T BE OPENED FOR INPUT'.
      
           05  MSG-ORIG-SOURCE-ERROR.
               10  FILLER PIC X(06) VALUE '003-E '.
               10  FILLER PIC X(36) VALUE
                   'ORIGINAL SOURCE FILE SRCPRG STATUS '.
               10  MSG-ORIG-SOURCE-STATUS    PIC 9(02).
               10  FILLER                    PIC X(04) VALUE ' ON '.
               10  MSG-ORIG-SOURCE-OPERATION PIC X(04).
               10  FILLER PIC X(10) VALUE
                   ' IN PHASE '.
               10  MSG-ORIG-SOURCE-PHASE     PIC X(09).
               10  FILLER                    PIC X(09) VALUE SPACES.
      
           05  MSG-TEST-CASES-NOT-FOUND.
               10  FILLER PIC X(06) VALUE '004-E '.
               10  FILLER PIC X(76) VALUE
               'UNIT TEST FILE UTESTS NOT FOUND'.
      
           05  MSG-TEST-CASES-UNREADABLE.
               10  FILLER PIC X(06) VALUE '005-E '.
               10  FILLER PIC X(80) VALUE
               'UNIT TEST FILE UTESTS CAN''T BE OPENED FOR INPUT'.
      
           05  MSG-TEST-CASES-ERROR.
               10  FILLER PIC X(06) VALUE '006-E '.
               10  FILLER                   PIC X(30) VALUE
                   'UNIT TEST FILE UTESTS STATUS '.
               10  MSG-TEST-CASES-STATUS    PIC 9(02).
               10  FILLER                   PIC X(04) VALUE ' ON '.
               10  MSG-TEST-CASES-OPERATION PIC X(04).
               10  FILLER                   PIC X(34) VALUE SPACES.
      
           05  MSG-MOCK-SYNTAX-1.
               10  FILLER PIC X(06) VALUE '007-E '.
               10  FILLER PIC X(76) VALUE
               'UNABLE TO PARSE MOCK STATEMENT (UNIT TEST FILE UTESTS)'.
           05  MSG-MOCK-SYNTAX-2              PIC X(80) VALUE
               '    MOCK FILE [filename]'.
           05  MSG-MOCK-SYNTAX-3              PIC X(80) VALUE
               '      ON [operation] RETURN|STATUS [value]'.
           05  MSG-MOCK-SYNTAX-4              PIC X(80) VALUE
               '    END-MOCK'.
      
           05  MSG-VERIFY-COUNT PIC S9(02) VALUE 10.
           05  MSG-VERIFY-SYNTAX-ERROR-TEXT.
               10  FILLER.
                   15  FILLER PIC X(06) VALUE '008-E '.
                   15  FILLER PIC X(40) VALUE
                   'UNABLE TO PARSE VERIFY STATEMENT (UNIT T'.
                   15  FILLER PIC X(34) VALUE
                   'EST FILE UTESTS)'.
               10  FILLER.
                   15  FILLER PIC X(40) VALUE
                   '    VERIFY mock-type mock-ident-values H'.
                   15  FILLER PIC X(40) VALUE
                   'APPENED count TIME[S]'.
               10  FILLER.
                   15  FILLER PIC X(80) VALUE 'Examples:'.
               10  FILLER.
                   15  FILLER PIC X(80) VALUE
                   '    VERIFY SECTION 010-INIT HAPPENED 3 TIMES'.     
               10  FILLER.
                   15  FILLER PIC X(80) VALUE
                   '    VERIFY SECTION 010-INIT HAPPENED ONCE'.
               10  FILLER.
                   15  FILLER PIC X(80) VALUE
                   '    VERIFY SECTION 010-INIT NEVER HAPPENED'.
               10  FILLER.
                   15  FILLER PIC X(40) VALUE
                   '    VERIFY SECTION 010-INIT HAPPENED'.
                   15  FILLER PIC X(40) VALUE
                   ' AT LEAST ONCE'.
               10  FILLER.
                   15  FILLER PIC X(40) VALUE
                   '    VERIFY CALL SUBPROC HAPPENED'.
                   15  FILLER PIC X(40) VALUE
                   ' NO MORE THAN 5 TIMES'.
               10  FILLER.
                   15  FILLER PIC X(40) VALUE
                   '    VERIFY CICS READ DATASET(WS-FILENAME'.
                   15  FILLER PIC X(40) VALUE
                   ') RIDFLD(WS-KEY) INTO(WS-RECORD)'.
               10  FILLER.
                   15  FILLER PIC X(80) VALUE
                   '        HAPPENED ONCE'.
           05  MSG-VERIFY-SYNTAX-ERRORS
               REDEFINES MSG-VERIFY-SYNTAX-ERROR-TEXT.
               10  MSG-VERIFY-SYNTAX
                   OCCURS 10
                   INDEXED BY MSG-VERIFY-IX
                   PIC X(80).
      
           05  MSG-FILE-MOCK-NOMATCH-1.
               10  FILLER PIC X(06) VALUE '009-W'.
               10  FILLER PIC X(40) VALUE
                   'A FILE MOCK WAS SPECIFIED THAT DOESN''T M'.
               10  FILLER PIC X(36) VALUE
                   'ATCH ANY SELECT STATEMENT'.
           05  MSG-FILE-MOCK-NOMATCH-2.
               10  FILLER PIC X(15) VALUE '    FILENAME: '.
               10  MSG-MOCK-NOMATCH-FILENAME PIC X(65).
           05  MSG-FILE-MOCK-NOMATCH-3.
               10  FILLER PIC X(15) VALUE '    OPERATION: '.
               10  MSG-MOCK-MOMATCH-OPERATION PIC X(64).
      
           05  MSG-CONFIG-FILE-NOT-FOUND.
               10  FILLER PIC X(06) VALUE '010-E '.
               10  FILLER PIC X(74) VALUE
               'CONFIGURATION FILE UTSTCFG NOT FOUND'.
      
           05  MSG-CONFIG-FILE-UNREADABLE.
               10  FILLER PIC X(06) VALUE '011-E '.
               10  FILLER PIC X(74) VALUE
               'CONFIGURATION FILE UTSTCFG CAN''T BE OPENED FOR INPUT'.
      
           05  MSG-CONFIG-FILE-ERROR.
               10  FILLER PIC X(06) VALUE '012-E '.
               10  FILLER                   PIC X(35) VALUE
                   'CONFIGURATION FILE UTSTCFG STATUS '.
               10  MSG-CONFIG-FILE-STATUS    PIC 9(02).
               10  FILLER                   PIC X(04) VALUE ' ON '.
               10  MSG-CONFIG-FILE-OPERATION PIC X(04).
               10  FILLER                   PIC X(29) VALUE SPACES.
      
           05  MSG-TEST-SOURCE-NOT-FOUND.
               10  FILLER PIC X(06) VALUE '013-E '.
               10  FILLER PIC X(76) VALUE
               'TEST SOURCE FILE TESTPRG NOT FOUND'.
      
           05  MSG-TEST-SOURCE-UNWRITABLE.
               10  FILLER PIC X(06) VALUE '014-E '.
               10  FILLER PIC X(80) VALUE
               'TEST SOURCE FILE TESTPRG CAN''T BE OPENED FOR OUTPUT'.
      
           05  MSG-TEST-SOURCE-ERROR.
               10  FILLER PIC X(06) VALUE '015-E '.
               10  FILLER                   PIC X(33) VALUE
                   'TEST SOURCE FILE TESTPRG STATUS '.
               10  MSG-TEST-SOURCE-STATUS    PIC 9(02).
               10  FILLER                   PIC X(04) VALUE ' ON '.
               10  MSG-TEST-SOURCE-OPERATION PIC X(04).
               10  FILLER                   PIC X(31) VALUE SPACES.
      
           05  MSG-EXPECT-SYNTAX-1.
               10  FILLER PIC X(06) VALUE '016-E '.
               10  FILLER PIC X(40) VALUE
               'UNABLE TO PARSE EXPECT STATEMENT (UNIT T'.
               10  FILLER PIC X(36) VALUE
               'EST FILE UTESTS)'.
           05  MSG-EXPECT-SYNTAX-2.
               10  FILLER PIC X(80) VALUE 'SYNTAX IS:'.
           05  MSG-EXPECT-SYNTAX-3.
               10  FILLER PIC X(80) VALUE
               '    EXPECT [actual] TO BE [expected]'.
      
           05  MSG-TEST-UNEXPECTED-END.
               10  FILLER PIC X(06) VALUE '017-E '.
               10  FILLER                   PIC X(53) VALUE
                   'UNEXPECTED END OF ORIGINAL SOURCE IN PHASE '.
               10  MSG-ORIG-SOURCE-PHASE    PIC X(09).
               10  FILLER                   PIC X(12) VALUE
                   ' IN SECTION '.
               10  MSG-CURRENT-SECTION       PIC X(09).
      
       LINKAGE SECTION.
      
       PROCEDURE DIVISION.
      *****************************************************************
      * Paragraph organization:
      *   Open - Mainline
      *   1xxx - Input processing loop
      *   2xxx - Process ORIGINAL-SOURCE file
      *   3xxx - Process ORIGINAL-SOURCE file
      *   4xxx - Process TEST-CASES file
      *   5xxx - Process TEST-CASES file
      *   6xxx - Source statement generation routines
      *   7xxx - Utility routines
      *   8xxx - Initialization routines
      *   9xxx - Input/Output routines
      *****************************************************************
           PERFORM 8000-INITIALIZE
           PERFORM 1000-PROCESS-INPUT
           GOBACK
           .
      
       1000-PROCESS-INPUT.
           PERFORM 9210-OPEN-ORIGINAL-SOURCE
           PERFORM 9410-OPEN-TEST-SOURCE
           PERFORM UNTIL NOT ORIGINAL-SOURCE-STATUS-OK
               PERFORM 9250-READ-ORIGINAL-SOURCE
               IF ORIGINAL-SOURCE-STATUS-OK
                   PERFORM 2000-PARSE-ORIGINAL-SOURCE
               END-IF
           END-PERFORM
           CLOSE TEST-SOURCE
           PERFORM 9290-CLOSE-ORIGINAL-SOURCE
           .
      
       2000-PARSE-ORIGINAL-SOURCE.
      *****************************************************************
      * Copy the source statements from ORIGINAL-SOURCE to TEST-SOURCE
      * and insert unit test code at appropriate points in the program.
      *****************************************************************
      
           IF PARSE-FILE-CONTROLL
             PERFORM 2600-IS-THIS-A-SELECT-STMT
           END-IF
      
           EVALUATE TRUE
               WHEN DATA-DIVISION-HEADER
                    SET PARSE-FILE-CONTROLL TO TRUE
                    PERFORM 9460-COPY-ORIGINAL-LINE
               WHEN WORKING-STORAGE-HEADER
                    SET PARSE-WORKING-STORAGE     TO TRUE
                    PERFORM 2700-INSERT-WORKING-STG-CODE
               WHEN PROCEDURE-DIVISION-HEADER AND
                    NOT COMMENTED-LINE
                    SET PARSE-PROCEDURE-DIVISION  TO TRUE
                    PERFORM 3000-INSERT-PROC-DIV-CODE
               WHEN SELECT-STATEMENT
                    SET PARSE-SELECT-STATEMENT    TO TRUE
                    PERFORM 2800-PROCESS-SELECT
               WHEN FD-STATEMENT
                    SET PARSE-FD-STATEMENT        TO TRUE
                    PERFORM 2900-PROCESS-FD
               WHEN CHECK-FOR-MOCKABLES
                    SET PARSE-CHECK-FOR-MOCKABLES TO TRUE
                    PERFORM 3100-CHECK-FOR-MOCKABLES
               WHEN OTHER
                    SET PARSE-OTHER               TO TRUE
                    PERFORM 9460-COPY-ORIGINAL-LINE
           END-EVALUATE
           .
      
       2600-IS-THIS-A-SELECT-STMT.
      *****************************************************************
      * See if the first token on the current original source line is
      * 'SELECT'.
      *****************************************************************
           SET NOT-SELECT-STATEMENT TO TRUE
      
           MOVE ZERO TO WS-CHARS
           INSPECT ORIGINAL-UPPERED-LINE
               TALLYING WS-CHARS FOR CHARACTERS BEFORE INITIAL 'SELECT'
      
           IF WS-CHARS IS LESS THAN KEYWORD-END
               SET SELECT-STATEMENT TO TRUE
           END-IF
           .
      
       2700-INSERT-WORKING-STG-CODE.
      *****************************************************************
      * Insert a COPY statement at the top of the WORKING-STORAGE
      * section to bring in the common unit test definitions.
      *****************************************************************
           PERFORM 9460-COPY-ORIGINAL-LINE
           MOVE WS-WORKING-STORAGE-COPY TO WS-COPY-NAME
           PERFORM 9450-WRITE-COPY-LINE
           .
      
       2800-PROCESS-SELECT.
      *****************************************************************
      * Parse a SELECT statement from the ORIGINAL-SOURCE file to
      * capture information that will be needed if the user has
      * specified a MOCK for the file.
      *****************************************************************
           PERFORM 9460-COPY-ORIGINAL-LINE
           SET TOKEN-IX TO 1
           MOVE 14 TO KEYWORD-OFFSET
           MOVE KEYWORD-END TO KEYWORD-SEARCH-LIMIT
      
      *    Next token is the internal filename in a SELECT statement
      
           PERFORM 7200-NEXT-TOKEN-ORIG-SOURCE
      
      *    Save the filename for future reference during parsing
      
           MOVE TOKEN(TOKEN-IX) TO INTERNAL-FILENAME(FILE-IX)
           SET TOKEN-IX UP BY 1
      
      *    Gen code in TEST-SOURCE to manage the filename at runtime
      
           SET INITIALIZATION-IX UP BY 1
           MOVE '           ADD 1 TO UT-FILE-COUNT'
               TO INITIALIZATION-STATEMENT(INITIALIZATION-IX)
           SET INITIALIZATION-IX UP BY 1
           MOVE '           SET UT-FILE-IX TO UT-FILE-COUNT'
               TO INITIALIZATION-STATEMENT(INITIALIZATION-IX)
      
           MOVE INTERNAL-FILENAME(FILE-IX) TO RAW-VALUE
           PERFORM 7950-ENCLOSE-IN-QUOTES
           SET INITIALIZATION-IX UP BY 1
           STRING
               STATEMENT-SPACER   DELIMITED BY SIZE
               MOVE-VERB          DELIMITED BY SIZE
               QUOTED-VALUE       DELIMITED BY SPACE
               TO-KEYWORD         DELIMITED BY SIZE
               'UT-INTERNAL-FILENAME(UT-FILE-IX)' DELIMITED BY SIZE
               INTO INITIALIZATION-STATEMENT(INITIALIZATION-IX)
           END-STRING
      
           SET TOKEN-IX UP BY 1
      
      *    Optional FILE STATUS [IS] may be on the current line or on
      *    any subsequent line up to the next period.
      
           PERFORM UNTIL END-OF-STATEMENT
               PERFORM 7700-PERIOD-ON-THIS-LINE
               IF END-OF-STATEMENT
                   CONTINUE
               ELSE
                   PERFORM 7500-INIT-KEYWORD-SEARCH
                   MOVE 'FILE STATUS' TO KEYWORD-TO-MATCH
                   MOVE 11 TO KEYWORD-MATCH-LENGTH
                   PERFORM 7400-MATCH-KEYWORD
                   IF KEYWORD-MATCHED
                       COMPUTE KEYWORD-OFFSET =
                           KEYWORD-OFFSET + 11
                       END-COMPUTE
                       COMPUTE KEYWORD-SEARCH-LIMIT =
                           KEYWORD-END - KEYWORD-OFFSET
                       END-COMPUTE
                       PERFORM 7200-NEXT-TOKEN-ORIG-SOURCE
                       IF TOKEN(TOKEN-IX) IS EQUAL TO 'IS'
                           PERFORM 7200-NEXT-TOKEN-ORIG-SOURCE
                       END-IF
                       PERFORM 7800-STRIP-PERIOD
                       MOVE TOKEN(TOKEN-IX) TO RAW-VALUE
                       PERFORM 7950-ENCLOSE-IN-QUOTES
      
      * Save FILE STATUS field name for future reference during parsing
      
                       MOVE TOKEN(TOKEN-IX)
                            TO FILE-STATUS-FIELD-NAME(FILE-IX)
      
      * Gen code in TEST-SOURCE to use the field name at runtime
      
                       SET INITIALIZATION-IX UP BY 1
                       STRING
                           STATEMENT-SPACER     DELIMITED BY SIZE
                           MOVE-VERB            DELIMITED BY SIZE
                           QUOTED-VALUE         DELIMITED BY SPACE
                           TO-KEYWORD           DELIMITED BY SIZE
                          INTO INITIALIZATION-STATEMENT
                               (INITIALIZATION-IX)
                       END-STRING
                       SET INITIALIZATION-IX UP BY 1
                       STRING
                           STATEMENT-SPACER     DELIMITED BY SIZE
                           STATEMENT-SPACER     DELIMITED BY SIZE
                           'UT-FILE-STATUS-FIELD-NAME(UT-FILE-IX)'
                                                DELIMITED BY SIZE
                           INTO INITIALIZATION-STATEMENT
                                (INITIALIZATION-IX)
                       END-STRING
                   ELSE
                       PERFORM 9240-READ-AND-COPY-ORIG-SOURCE
                       IF NOT ORIGINAL-SOURCE-STATUS-OK
                           MOVE CURRENT-PHASE
                             TO MSG-ORIG-SOURCE-PHASE
                                IN MSG-TEST-UNEXPECTED-END
                           MOVE '2800'
                             TO MSG-CURRENT-SECTION
                           DISPLAY MSG-TEST-UNEXPECTED-END
                           GOBACK
                       END-IF
                       MOVE ORIGINAL-UPPERED-LINE TO LINE-TO-PARSE
                   END-IF
               END-IF
               MOVE 7 TO KEYWORD-OFFSET
           END-PERFORM
           SET FILE-IX UP BY 1
           .
      
       2900-PROCESS-FD.
      *****************************************************************
      * Parse an FD and its first 01 level item from the ORIGINAL-SOURCE
      * file to capture information that will be needed if the user has
      * specified a MOCK for the file.
      *****************************************************************
           PERFORM 9460-COPY-ORIGINAL-LINE
           SET TOKEN-IX TO 1
           MOVE 11 TO KEYWORD-OFFSET
           MOVE KEYWORD-END TO KEYWORD-SEARCH-LIMIT
      
      * Look for file name to match with the SELECT file name
      
           PERFORM 7200-NEXT-TOKEN-ORIG-SOURCE
           PERFORM 7800-STRIP-PERIOD
      
      * Find the entry in the file information table for this file
      
           PERFORM VARYING FILE-IX FROM 1 BY 1
               UNTIL FILE-IX GREATER THAN MAX-FILES
               OR INTERNAL-FILENAME(FILE-IX) EQUAL TOKEN(TOKEN-IX)
           END-PERFORM
      
      * Generate code in TEST-SOURCE to position the indices into the
      * tables of mock and file information.
      
           SET INITIALIZATION-IX UP BY 1
           MOVE SET-FIND-FILE-MOCK-STMT
               TO INITIALIZATION-STATEMENT(INITIALIZATION-IX)
      
           PERFORM 6300-GEN-SET-FILENAME-TO-FIND
           SET INITIALIZATION-IX UP BY 1
           MOVE TEMP-STATEMENT
               TO INITIALIZATION-STATEMENT(INITIALIZATION-IX)
      
           PERFORM 6100-GEN-LOOKUP-MOCK
           SET INITIALIZATION-IX UP BY 1
           MOVE TEMP-STATEMENT
               TO INITIALIZATION-STATEMENT(INITIALIZATION-IX)
      
           PERFORM 6200-GEN-LOOKUP-FILE
           SET INITIALIZATION-IX UP BY 1
           MOVE TEMP-STATEMENT
               TO INITIALIZATION-STATEMENT(INITIALIZATION-IX)
      
      * Find the name of the default record field on the next line
      
           PERFORM 9240-READ-AND-COPY-ORIG-SOURCE
           IF NOT ORIGINAL-SOURCE-STATUS-OK
               MOVE CURRENT-PHASE
                 TO MSG-ORIG-SOURCE-PHASE
                    IN MSG-TEST-UNEXPECTED-END
               MOVE '2900'
                 TO MSG-CURRENT-SECTION
               DISPLAY MSG-TEST-UNEXPECTED-END
               GOBACK
           END-IF
           MOVE ORIGINAL-UPPERED-LINE TO LINE-TO-PARSE
           MOVE 11 TO KEYWORD-OFFSET
           MOVE KEYWORD-END TO KEYWORD-SEARCH-LIMIT
           PERFORM 7200-NEXT-TOKEN-ORIG-SOURCE
           PERFORM 7800-STRIP-PERIOD
      
      * Save the field name for future reference during parsing
      
           MOVE TOKEN(TOKEN-IX) TO RECORD-FIELD-NAME(FILE-IX)
      
      * Generate code in TEST-SOURCE to set the field name on the mock
      
           MOVE TOKEN(TOKEN-IX) TO RAW-VALUE
           PERFORM 7950-ENCLOSE-IN-QUOTES
      
           SET INITIALIZATION-IX UP BY 1
           STRING
               STATEMENT-SPACER        DELIMITED BY SIZE
               MOVE-VERB               DELIMITED BY SIZE
               QUOTED-VALUE            DELIMITED BY SPACE
               TO-KEYWORD              DELIMITED BY SIZE
               'UT-RECORD-FIELD-NAME(UT-FILE-IX)' DELIMITED BY SIZE
               INTO INITIALIZATION-STATEMENT(INITIALIZATION-IX)
           END-STRING
           .
      
       3000-INSERT-PROC-DIV-CODE.
      *****************************************************************
      * Insert the unit test cases and standard unit test code at the
      * top of the PROCEDURE DIVISION.
      *****************************************************************
           PERFORM 9460-COPY-ORIGINAL-LINE
           PERFORM 4000-INSERT-TEST-CODE
      
      * Insert a call to UT-AFTER (AFTER-EACH code) after the last
      * test case.
      
           INITIALIZE GENERATED-SOURCE-STATEMENTS
           SET STATEMENT-IX TO 1
           MOVE 'PERFORM UT-AFTER' TO
               GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                   (STATEMENT-OFFSET:STATEMENT-LENGTH)
           PERFORM 9440-WRITE-GENERATED-STMTS
      
      * Insert a COPY for boilerplate unit test code and write
      * generated paragraphs just after it.
      
           MOVE PROCEDURE-DIVISION-COPYBOOK TO WS-COPY-NAME
           PERFORM 9450-WRITE-COPY-LINE
           PERFORM 9470-WRITE-BEFORE-PARAGRAPH
           PERFORM 9480-WRITE-AFTER-PARAGRAPH
           PERFORM 9490-WRITE-INIT-MOCK-PARAGRAPH
           PERFORM 9485-WRITE-INIT-PARAGRAPH
      
      * Enable checking for IO statements in subsequent PROCEDURE
      * DIVISION code.
      
           SET CHECK-FOR-MOCKABLES TO TRUE
      
           PERFORM 9417-WRITE-ZUTEND-LINE
           .
      
       3100-CHECK-FOR-MOCKABLES.
      *****************************************************************
      * See if the current original source statement looks like
      * something that can be mocked, and handle it accordingly.
      *****************************************************************
           PERFORM 3200-CHECK-FOR-IO-STATEMENT
           IF KEYWORD-MATCHED
               PERFORM 3220-IO-KEYWORD-MATCHED
           ELSE
               PERFORM 3400-CHECK-FOR-CALL-STATEMENT
               IF MOCKABLE-FOUND
                   SET SUPPRESS-COPY-SOURCE TO TRUE
               ELSE
                   PERFORM 3500-CHECK-FOR-EXEC-COMMAND
                   IF MOCKABLE-FOUND
                       SET COPY-SOURCE TO TRUE
                   ELSE
                       PERFORM 3700-CHECK-FOR-PARA-HEADER
                       IF MOCKABLE-FOUND
                           SET COPY-SOURCE TO TRUE
                       ELSE
                           PERFORM 3750-CHECK-FOR-SECT-HEADER
                           SET COPY-SOURCE TO TRUE
                       END-IF
                   END-IF
               END-IF
           END-IF
      
           IF SUPPRESS-COPY-SOURCE
               SET COPY-SOURCE TO TRUE
           ELSE
               PERFORM 9460-COPY-ORIGINAL-LINE
           END-IF
           .
      
       3200-CHECK-FOR-IO-STATEMENT.
      *****************************************************************
      * First check to see if the current source line is potentially a
      * mocked file I/O statement. If not then see if it is potentially
      * a mocked EXEC command. If not then see if it is potentially a
      * mocked paragraph header.
      *****************************************************************
           SET COPY-SOURCE TO TRUE
           SET TOKEN-IX TO 1
           PERFORM 7500-INIT-KEYWORD-SEARCH
           PERFORM 7200-NEXT-TOKEN-ORIG-SOURCE
           PERFORM VARYING VALID-FILE-OPERATION-IX FROM 1 BY 1
                   UNTIL VALID-FILE-OPERATION-IX IS GREATER THAN
                         MAX-VALID-FILE-OPERATIONS
                      OR KEYWORD-MATCHED
               IF TOKEN(TOKEN-IX) IS EQUAL TO
                       VALID-FILE-OPERATION(VALID-FILE-OPERATION-IX)
                   SET KEYWORD-MATCHED TO TRUE
      * Keep index set to this entry for subsequent processing
                   SET VALID-FILE-OPERATION-IX DOWN BY 1
               END-IF
           END-PERFORM
           .
      
       3220-IO-KEYWORD-MATCHED.
      *****************************************************************
      * An I/O verb was found in ORIGINAL-SOURCE.
      *****************************************************************
      
      * Filename is next
      
           SET TOKEN-IX UP BY 1
      
           PERFORM
               NEXT-TOKEN-TO-GET(VALID-FILE-OPERATION-IX) TIMES
               PERFORM 7200-NEXT-TOKEN-ORIG-SOURCE
           END-PERFORM
      
           PERFORM 3240-FIND-CORR-FILE-MOCK
           IF MOCK-MATCHED
               PERFORM 3260-FIND-CORR-FILE-INFO
           ELSE
               SET COPY-SOURCE TO TRUE
           END-IF
           .
      
       3240-FIND-CORR-FILE-MOCK.
      *****************************************************************
      * Found an I/O statement in ORIGINAL-SOURCE. See if a MOCK was
      * specified for this filename.
      *****************************************************************
           SET MOCK-NOT-MATCHED TO TRUE
           PERFORM VARYING MOCK-IX FROM 1 BY 1
                   UNTIL MOCK-IX IS GREATER THAN MAX-MOCKS
                      OR MOCK-MATCHED
      
               IF MOCK-FILE(MOCK-IX)
               AND MOCK-OPERATION(MOCK-IX) EQUAL TOKEN(1)
               AND MOCK-FILENAME(MOCK-IX) EQUAL TOKEN(2)
                   SET MOCK-MATCHED TO TRUE
               ELSE
                   SET COPY-SOURCE TO TRUE
               END-IF
           END-PERFORM
           SET MOCK-IX DOWN BY 1
           .
      
       3260-FIND-CORR-FILE-INFO.
      *****************************************************************
      * Found an I/O statement for which a MOCK was specified.
      * Generate code in TEST-SOURCE to substitute mock functionality
      * for the real I/O operation.
      * MOCK-IX is already set.
      *****************************************************************
           SET FILENAME-NOT-MATCHED TO TRUE
           PERFORM 6400-GET-FILE-INFO-FOR-MOCK
           IF FILENAME-MATCHED
               PERFORM 3300-GEN-MOCK-REPLACE-STMTS
           ELSE
               SET COPY-SOURCE TO TRUE
               MOVE MOCK-FILENAME(MOCK-IX)
                    TO MSG-MOCK-NOMATCH-FILENAME
               MOVE MOCK-OPERATION(MOCK-IX)
                    TO MSG-MOCK-MOMATCH-OPERATION
               DISPLAY THIS-PROGRAM MSG-FILE-MOCK-NOMATCH-1
               DISPLAY THIS-PROGRAM MSG-FILE-MOCK-NOMATCH-2
               DISPLAY THIS-PROGRAM MSG-FILE-MOCK-NOMATCH-3
           END-IF
           .
      
       3300-GEN-MOCK-REPLACE-STMTS.
      *****************************************************************
      * Generate code in TEST-SOURCE to substitute mock behavior for
      * the real I/O functionality.
      *****************************************************************
           MOVE SPACES TO GENERATED-SOURCE-STATEMENTS
      
           PERFORM 6300-GEN-SET-FILENAME-TO-FIND
           SET STATEMENT-IX TO 1
           MOVE TEMP-STATEMENT
               TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
      
           PERFORM 6100-GEN-LOOKUP-MOCK
           SET STATEMENT-IX UP BY 1
           MOVE TEMP-STATEMENT
               TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
      
           SET STATEMENT-IX UP BY 1
           MOVE '           IF UT-MOCK-FOUND'
               TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
      
           SET STATEMENT-IX UP BY 1
           MOVE INCR-MOCK-ACCESS-COUNT-STMT
               TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
      
           PERFORM 6300-GEN-SET-FILENAME-TO-FIND
           SET STATEMENT-IX UP BY 1
           MOVE TEMP-STATEMENT
               TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
      
           PERFORM 6200-GEN-LOOKUP-FILE
           SET STATEMENT-IX UP BY 1
           MOVE TEMP-STATEMENT
               TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           SET STATEMENT-IX UP BY 1
      
           MOVE ENDIF-STMT TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
      
           SET STATEMENT-IX UP BY 1
           MOVE '            IF UT-MOCK-RECORD(UT-MOCK-IX)'
               TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
      
           SET STATEMENT-IX UP BY 1
           MOVE '                NOT EQUAL SPACES'
               TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
      
           SET STATEMENT-IX UP BY 1
           MOVE '            MOVE UT-MOCK-RECORD(UT-MOCK-IX)'
               TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
      
           SET STATEMENT-IX UP BY 1
           STRING
               '                TO '     DELIMITED BY SIZE
               RECORD-FIELD-NAME(FILE-IX) DELIMITED BY SPACE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           END-STRING
      
           SET STATEMENT-IX UP BY 1
           MOVE ENDIF-STMT
               TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
      
           SET STATEMENT-IX UP BY 1
           MOVE '            IF UT-MOCK-FILE-STATUS(UT-MOCK-IX)'
               TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
      
           SET STATEMENT-IX UP BY 1
           MOVE '                NOT EQUAL SPACES'
               TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
      
           SET STATEMENT-IX UP BY 1
           MOVE '            MOVE UT-MOCK-FILE-STATUS(UT-MOCK-IX)'
               TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
      
           SET STATEMENT-IX UP BY 1
           STRING
               '                TO '          DELIMITED BY SIZE
               FILE-STATUS-FIELD-NAME(FILE-IX) DELIMITED BY SPACE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           END-STRING
      
           SET STATEMENT-IX UP BY 1
           MOVE ENDIF-STMT
               TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
      
           PERFORM 9465-COMMENT-ORIGINAL-LINE
      
           PERFORM 9415-WRITE-ZUTBEGIN-LINE
      
           PERFORM 9440-WRITE-GENERATED-STMTS
      
           PERFORM 9417-WRITE-ZUTEND-LINE
           .
       3400-CHECK-FOR-CALL-STATEMENT.
      *****************************************************************
      * See if the current original source line contains a CALL
      * statement. If the CALL is to be mocked, then comment out the
      * lines in the program under test and substitute the code
      * defined for the mock. Otherwise, copy the original lines.
      *****************************************************************
           SET MOCKABLE-NOT-FOUND TO TRUE
           MOVE SPACES TO MOCK-LOOKUP-KEYWORDS
           IF TOKEN(TOKEN-IX) IS EQUAL TO CALL-KEYWORD
             SET MOCKABLE-FOUND TO TRUE
             PERFORM 7640-SET-SAVE-ORIG-LINES
             PERFORM 7650-SAVE-ORIG-LINE
      
             SET MORE-TOKENS TO TRUE
             SET MOCK-LOOKUP-IX TO 1
             PERFORM UNTIL NO-MORE-TOKENS
               PERFORM 7200-NEXT-TOKEN-ORIG-SOURCE
               PERFORM 7800-STRIP-PERIOD
               MOVE TOKEN(TOKEN-IX)
                 TO COBOL-PRIMARY-RESERVED-KEYWORD
               IF KEYWORD-IS-COBOL-RESERVED OR
                  TOKEN-HAS-PERIOD(TOKEN-IX)
                 SET NO-MORE-TOKENS TO TRUE
               ELSE
                 PERFORM 7900-STRIP-QUOTES
                 MOVE TOKEN(TOKEN-IX) TO MOCK-LOOKUP-KEYWORD
                                         (MOCK-LOOKUP-IX)
                 SET MOCK-LOOKUP-IX UP BY 1
               END-IF
             END-PERFORM
      
             PERFORM 5080-STRINGIFY-KEYWORDS
             PERFORM 3900-LOOKUP-MOCK
      
             IF MOCK-MATCHED
                 SET COMMENT-THE-LINES TO TRUE
                 PERFORM 3960-COPY-OR-COMMENT-LINES
      
                 MOVE UT-MOCK-FIND-CALL-TOKENS-STMT TO MOCK-STMT
                 PERFORM 3770-WRITE-MOCK-ACCESS-STMT
                 PERFORM 6030-WRITE-GENERATED-STMTS
             ELSE
                 SET COPY-THE-LINES TO TRUE
                 PERFORM 3960-COPY-OR-COMMENT-LINES
                 SET SUPPRESS-COPY-SOURCE TO TRUE
             END-IF
           END-IF
           .
      
       3500-CHECK-FOR-EXEC-COMMAND.
      *****************************************************************
      * See if the current original source line contains an EXEC
      * command.
      *****************************************************************
           SET TOKEN-IX TO 1
           PERFORM VARYING EXEC-KEYWORD-IX FROM 1 BY 1
               UNTIL EXEC-KEYWORD-IX IS GREATER THAN 2
                  OR KEYWORD-MATCHED
      
               PERFORM 7500-INIT-KEYWORD-SEARCH
               MOVE EXEC-KEYWORD(EXEC-KEYWORD-IX) TO KEYWORD-TO-MATCH
               MOVE EXEC-KEYWORD-LENGTH(EXEC-KEYWORD-IX)
                    TO KEYWORD-MATCH-LENGTH
               PERFORM 7450-FIND-KEYWORD-BEGIN
               IF LINE-TO-PARSE(KEYWORD-BEGIN:8) = END-EXEC-KEYWORD
                 CONTINUE
               ELSE
                 PERFORM 7400-MATCH-KEYWORD
               END-IF
           END-PERFORM
      
      * If EXEC or EXECUTE was found, next token will be CICS or SQL
      
           SET EXEC-KEYWORD-IX DOWN BY 1
           ADD EXEC-KEYWORD-ADJUST(EXEC-KEYWORD-IX) TO KEYWORD-OFFSET
      
           IF KEYWORD-MATCHED
               SET MOCKABLE-FOUND TO TRUE
               PERFORM 7640-SET-SAVE-ORIG-LINES
               PERFORM 7650-SAVE-ORIG-LINE
      
               PERFORM 7200-NEXT-TOKEN-ORIG-SOURCE
      
               EVALUATE TRUE
                   WHEN TOKEN(TOKEN-IX) IS EQUAL TO CICS-KEYWORD
                        PERFORM 3600-PROCESS-CICS-COMMAND
                   WHEN TOKEN(TOKEN-IX) IS EQUAL TO SQL-KEYWORD
                        PERFORM 3800-PROCESS-SQL-COMMAND
                   WHEN OTHER
                        CONTINUE
               END-EVALUATE
           ELSE
               SET MOCKABLE-NOT-FOUND TO TRUE
           END-IF
           SET NO-SAVE-ORIGINAL-LINES TO TRUE
           .
      
       3600-PROCESS-CICS-COMMAND.
      *****************************************************************
      * EXEC CICS was found. See the command is to be mocked, comment
      * out the original lines and substitute the code specified in
      * the mock. Otherwise, copy the original lines.
      *****************************************************************
      
           SET MOCK-LOOKUP-IX TO 1
           MOVE SPACES TO MOCK-LOOKUP-KEY
           MOVE SPACES TO MOCK-LOOKUP-KEYWORDS
           PERFORM VARYING KEYWORD-OFFSET
                   FROM KEYWORD-OFFSET BY 1
                   UNTIL TOKEN(TOKEN-IX) IS EQUAL TO END-EXEC-KEYWORD
      
               PERFORM 7200-NEXT-TOKEN-ORIG-SOURCE
               IF TOKEN(TOKEN-IX) IS EQUAL TO END-EXEC-KEYWORD
                   CONTINUE
               ELSE
                   MOVE TOKEN(TOKEN-IX) TO CANDIDATE-TOKEN
                   INSPECT CANDIDATE-TOKEN REPLACING ALL '(' BY SPACE
                   MOVE SPACES TO CANDIDATE-CICS-RESERVED-WORD
                   STRING CANDIDATE-TOKEN
                       DELIMITED BY SPACE
                       INTO CANDIDATE-CICS-RESERVED-WORD
                   END-STRING
                   MOVE TOKEN(TOKEN-IX) TO MOCK-LOOKUP-KEYWORD
                           (MOCK-LOOKUP-IX)
                   SET MOCK-LOOKUP-IX UP BY 1
               END-IF
           END-PERFORM
      
           PERFORM 5080-STRINGIFY-KEYWORDS
      
           PERFORM 4925-GENERATE-CICS-KEYWORDS
           MOVE MOCK-CICS-KEYWORDS-DATA(MOCK-IX)
             TO MOCK-LOOKUP-KEY
             (LENGTH OF MOCK-CICS-KEYWORDS-KEY(MOCK-IX) + 1:)
      
           PERFORM 3900-LOOKUP-MOCK
           IF MOCK-MATCHED
               SET COMMENT-THE-LINES TO TRUE
           ELSE
               SET COPY-THE-LINES TO TRUE
           END-IF
      
           PERFORM 3960-COPY-OR-COMMENT-LINES
           MOVE UT-MOCK-FIND-CICS-KEYWO-STMT TO MOCK-STMT
           MOVE SPACES
             TO MOCK-LOOKUP-KEY
             (LENGTH OF MOCK-CICS-KEYWORDS-KEY(MOCK-IX) + 1:)
           PERFORM 3770-WRITE-MOCK-ACCESS-STMT
           PERFORM 6030-WRITE-GENERATED-STMTS
           .
      
       3700-CHECK-FOR-PARA-HEADER.
      *****************************************************************
      * See if we have read a paragraph header line from
      * ORIGINAL-SOURCE.
      *****************************************************************
           SET MOCKABLE-NOT-FOUND TO TRUE
           IF PARA-HEADER-AREA IS EQUAL TO SPACES
               CONTINUE
           ELSE
               PERFORM 7640-SET-SAVE-ORIG-LINES
               PERFORM 3720-CHECK-FOR-MOCKED-PARA
           END-IF
           .
      
       3720-CHECK-FOR-MOCKED-PARA.
      *****************************************************************
      * If this paragraph is to be mocked, substitute the code
      * specified in the mock. Otherwise, copy the original lines.
      *****************************************************************
           MOVE PARAGRAPH-NAME TO MOCK-LOOKUP-KEY
           PERFORM 3900-LOOKUP-MOCK
           IF MOCK-MATCHED
               PERFORM 3740-PROCESS-MOCKED-PARA-SECT
           ELSE
               SET MOCK-NOT-MATCHED TO TRUE
               SET MOCKABLE-NOT-FOUND TO TRUE
               SET COPY-THE-LINES TO TRUE
               PERFORM 3960-COPY-OR-COMMENT-LINES
           END-IF
           .
      
       3740-PROCESS-MOCKED-PARA-SECT.
      *****************************************************************
      * Comment the original source lines and substitute the source
      * lines specified for this mocked paragraph.
      *****************************************************************
           SET MOCK-MATCHED TO TRUE
           SET MOCKABLE-FOUND TO TRUE
      
      * Consume original source lines through the end of the mocked
      * paragraph. Echo the paragraph header and ending period line
      * (if any) and comment the rest of the lines. We currently have
      * the paragraph header line in the input buffer.
      
           WRITE TEST-LINE FROM PARAGRAPH-END-MARKER
           WRITE TEST-LINE FROM ORIGINAL-LINE
           PERFORM 7640-SET-SAVE-ORIG-LINES
      
           MOVE SPACES TO ORIGINAL-LINE
                          ORIGINAL-UPPERED-LINE
           PERFORM UNTIL ORIGINAL-SOURCE-END-OF-FILE
                      OR (PARA-HEADER-AREA NOT EQUAL SPACES
                      AND ORIGINAL-UPPERED-LINE (1:7) NOT = '      *')
                      OR ORIGINAL-UPPERED-LINE (1:12) EQUAL
                         PARAGRAPH-END-MARKER
               PERFORM 9250-READ-ORIGINAL-SOURCE
           END-PERFORM
      
      * Comment out the original behavior of the mocked paragraph.
      
           SET COMMENT-THE-LINES TO TRUE
           PERFORM 3960-COPY-OR-COMMENT-LINES
      
      * Replace the original behavior with the mocked behavior.
      
           IF MOCK-PARAGRAPH(MOCK-IX)
             MOVE UT-MOCK-FIND-PARA-NAME-STMT TO MOCK-STMT
           ELSE
             MOVE UT-MOCK-FIND-SECT-NAME-STMT TO MOCK-STMT
           END-IF
           PERFORM 3770-WRITE-MOCK-ACCESS-STMT
           PERFORM 6030-WRITE-GENERATED-STMTS
           .
      
       3750-CHECK-FOR-SECT-HEADER.
      *****************************************************************
      * See if we have read a section header line from
      * ORIGINAL-SOURCE.
      *****************************************************************
           SET MOCKABLE-NOT-FOUND TO TRUE
           UNSTRING ORIGINAL-UPPERED-LINE (8:)
             DELIMITED BY ALL SPACE
               INTO SECTION-NAME
                    SECTION-FIND-KEYWORD
                    SECTION-REMAINDER
           END-UNSTRING
           IF SECTION-FIND-KEYWORD NOT = 'SECTION.'
               CONTINUE
           ELSE
               PERFORM 7640-SET-SAVE-ORIG-LINES
               PERFORM 3760-CHECK-FOR-MOCKED-SECT
           END-IF
           .
      
       3760-CHECK-FOR-MOCKED-SECT.
      *****************************************************************
      * If this section is to be mocked, substitute the code
      * specified in the mock. Otherwise, copy the original lines.
      *****************************************************************
      
           MOVE SECTION-NAME TO MOCK-LOOKUP-KEY
           PERFORM 3900-LOOKUP-MOCK
           IF MOCK-MATCHED
               PERFORM 3740-PROCESS-MOCKED-PARA-SECT
           ELSE
               SET MOCKABLE-NOT-FOUND TO TRUE
               SET COPY-THE-LINES TO TRUE
               PERFORM 3960-COPY-OR-COMMENT-LINES
           END-IF
           .
      
      *****************************************************************
       3770-WRITE-MOCK-ACCESS-STMT.
      *****************************************************************
           PERFORM 9415-WRITE-ZUTBEGIN-LINE
      
           MOVE MOCK-LOOKUP-KEY TO RAW-VALUE
           PERFORM 7950-ENCLOSE-IN-QUOTES
      
           STRING
               STATEMENT-SPACER             DELIMITED BY SIZE
               MOVE-VERB                    DELIMITED BY SIZE
               QUOTED-VALUE                 DELIMITED BY SIZE
               INTO TEST-LINE
           END-STRING
           WRITE TEST-LINE
      
           MOVE SPACES TO TEST-LINE
           STRING
               STATEMENT-SPACER             DELIMITED BY SIZE
               TO-VERB                      DELIMITED BY SIZE
               MOCK-STMT                    DELIMITED BY SIZE
               INTO TEST-LINE
           WRITE TEST-LINE
           MOVE '           PERFORM UT-LOOKUP-MOCK'
               TO TEST-LINE
           WRITE TEST-LINE
      
           MOVE INCR-MOCK-ACCESS-COUNT-STMT
               TO TEST-LINE
           WRITE TEST-LINE
           PERFORM 9417-WRITE-ZUTEND-LINE
           .
      
       3800-PROCESS-SQL-COMMAND.
      *****************************************************************
      * EXEC SQL was found. Future implementation.
      *****************************************************************
           CONTINUE
           .
      
       3900-LOOKUP-MOCK.
      *****************************************************************
      * Find the mock that matches MOCK-LOOKUP-KEY, if any.
      *****************************************************************
           SET MOCK-NOT-MATCHED TO TRUE
           PERFORM VARYING MOCK-IX FROM 1 BY 1
                   UNTIL MOCK-IX IS GREATER THAN MAX-MOCKS
                      OR MOCK-DATA(MOCK-IX) IS EQUAL TO SPACES
                      OR MOCK-MATCHED
               IF MOCK-KEY(MOCK-IX) EQUAL MOCK-LOOKUP-KEY
                   SET MOCK-MATCHED TO TRUE
               END-IF
           END-PERFORM
           SET MOCK-IX DOWN BY 1
           .
      
       3960-COPY-OR-COMMENT-LINES.
      *****************************************************************
      * If current statement is to be mocked, comment the original
      * source lines in the output file; otherwise, copy them.
      *****************************************************************
           SET NO-SAVE-ORIGINAL-LINES TO TRUE
           PERFORM VARYING SAVED-LINE-IX FROM 1 BY 1
                   UNTIL SAVED-LINE-IX
                         IS GREATER THAN MAX-SAVED-LINES
                      OR SAVED-LINE-IX
                         IS EQUAL TO TALLY-FOR-SAVED-LINES
               MOVE SAVED-LINE(SAVED-LINE-IX) TO ORIGINAL-LINE
               PERFORM 9240-UPPER-CASE-ORIGINAL-LINE
               IF COMMENT-THE-LINES
                   PERFORM 9465-COMMENT-ORIGINAL-LINE
               ELSE
                   PERFORM 9460-COPY-ORIGINAL-LINE
               END-IF
           END-PERFORM
           .
      
      *****************************************************************
      * 4xxx- 5xxx-
      * Parse the TEST-CASES file and insert test code into the
      * PROCEDURE DIVISION of TEST-SOURCE.
      *****************************************************************
      
       4000-INSERT-TEST-CODE.
      *****************************************************************
      * Input processing loop for the TEST-CASES file.
      *****************************************************************
           PERFORM 9415-WRITE-ZUTBEGIN-LINE
           PERFORM 4050-INSERT-PERFORM-INIT
           PERFORM 9310-OPEN-TEST-CASES
           PERFORM 4100-PARSE-TEST-CASES
               UNTIL NOT TEST-CASES-STATUS-OK
           PERFORM 9390-CLOSE-TEST-CASES
           .
      
       4050-INSERT-PERFORM-INIT.
      *****************************************************************
      * Insert a PERFORM for the initialization paragraph in the test
      * program.
      *****************************************************************
           MOVE '           PERFORM UT-INITIALIZE' TO TEST-LINE
           PERFORM 9420-WRITE-TEST-LINE
           .
      
       4100-PARSE-TEST-CASES.
      *****************************************************************
      * Copy unit test code from TEST-CASES to TEST-SOURCE, parsing
      * keywords and substituting appropriate code.
      *****************************************************************
           PERFORM 9350-READ-TEST-CASES
           IF TEST-CASES-STATUS-OK
               PERFORM 4200-PROCESS-KEYWORDS
               IF SUPPRESS-COPY-SOURCE OR IGNORE-TESTCASE
                   CONTINUE
               ELSE
                   MOVE TEST-CASE-LINE TO TEST-LINE
                   PERFORM 9420-WRITE-TEST-LINE
               END-IF
           END-IF
           .
      
       4200-PROCESS-KEYWORDS.
      *****************************************************************
      * Look for keywords specific to this unit testing framework and
      * substitute valid COBOL source statements corresponding to the
      * specifications.
      *****************************************************************
           INITIALIZE COPY-SOURCE-INDICATOR
           PERFORM VARYING KEYWORD-IX FROM 1 BY 1
                   UNTIL KEYWORD-IX IS GREATER THAN MAX-KEYWORDS
               COMPUTE KEYWORD-SEARCH-LIMIT =
                   KEYWORD-END - KEYWORD-LENGTH(KEYWORD-IX)
               END-COMPUTE
               MOVE KEYWORD-START TO KEYWORD-OFFSET
               MOVE KEYWORD-VALUE(KEYWORD-IX) TO KEYWORD-TO-MATCH
               MOVE KEYWORD-LENGTH(KEYWORD-IX) TO KEYWORD-MATCH-LENGTH
               INITIALIZE TOKENS
               INITIALIZE KEYWORD-MATCHED-INDICATOR
      
               MOVE TEST-CASE-UPPERED-LINE TO LINE-TO-PARSE
               SET SCANNING-TEST-CASES TO TRUE
               SET NOT-SCANNING-ORIGINAL-SOURCE TO TRUE
               PERFORM 7400-MATCH-KEYWORD
      
               IF KEYWORD-MATCHED
                   MOVE KEYWORD-TO-MATCH TO TOKEN(1)
                   SET TOKEN-IX TO 2
                   EVALUATE TRUE
                       WHEN TOKEN(1) IS EQUAL TO 'EXPECT'
                            IF IGNORE-TESTCASE
                                SET SUPPRESS-COPY-SOURCE TO TRUE
                            ELSE
                                PERFORM 4300-KEYWORD-EXPECT
                            END-IF
                            EXIT PERFORM
                       WHEN TOKEN(1) IS EQUAL TO 'VERIFY'
                            IF IGNORE-TESTCASE
                                SET SUPPRESS-COPY-SOURCE TO TRUE
                            ELSE
                                PERFORM 5000-KEYWORD-VERIFY
                            END-IF
                       WHEN TOKEN(1) IS EQUAL TO 'IGNORE'
                            SET IGNORE-TESTCASE TO TRUE
                            SET 1ST-TEST-AFTER-IGNORE TO TRUE
                            SET SUPPRESS-COPY-SOURCE TO TRUE
                       WHEN TOKEN(1) IS EQUAL TO 'TESTCASE'
                            IF IGNORE-TESTCASE
                                IF 1ST-TEST-AFTER-IGNORE
                                    SET NOT-1ST-TEST-AFTER-IGNORE
                                        TO TRUE
                                ELSE
                                    SET NOT-IGNORE-TESTCASE TO TRUE
                                PERFORM 4400-KEYWORD-TESTCASE
                                END-IF
                            ELSE
                                PERFORM 4400-KEYWORD-TESTCASE
                            END-IF
                       WHEN TOKEN(1) IS EQUAL TO 'TESTSUITE'
                            PERFORM 4500-KEYWORD-TESTSUITE
                       WHEN TOKEN(1) IS EQUAL TO 'BEFORE-EACH'
                            PERFORM 4600-KEYWORD-BEFORE
                       WHEN TOKEN(1) IS EQUAL TO 'AFTER-EACH'
                            PERFORM 4700-KEYWORD-AFTER
                       WHEN TOKEN(1) IS EQUAL TO 'MOCK'
                            IF IGNORE-TESTCASE
                                SET SUPPRESS-COPY-SOURCE TO TRUE
                            ELSE
                                PERFORM 4800-KEYWORD-MOCK
                            END-IF
                   END-EVALUATE
               END-IF
           END-PERFORM
           .
      
       4300-KEYWORD-EXPECT.
      *****************************************************************
      * EXPECT keyword was found.
      *
      * Generate assertion code in TEST-SOURCE.
      *
      * EXPECT [actual-value] TO BE [expected-value]
      *****************************************************************
      
           MOVE TEST-CASE-UPPERED-LINE TO TARGET-STRING
           MOVE 80 TO TARGET-STRING-LENGTH
           PERFORM 7980-CHECK-FOR-IGNORE
           IF STRING-NOT-FOUND
               PERFORM 4310-KEYWORD-EXPECT
           END-IF
           .
      
       4310-KEYWORD-EXPECT.
           PERFORM 7600-ADJUST-OFFSETS
           PERFORM 7100-NEXT-TOKEN-TEST-CASES
           MOVE SPACES TO CANDIDATE-TEST-KEYWORD
           SET UNKNOWN-TOKEN TO TRUE
           SET NORMAL-COMPARE TO TRUE
      
           PERFORM UNTIL IS-A-TEST-KEYWORD
                      OR TOKEN-IX > 100
                      OR END-OF-TEST-CASES
             EVALUATE TRUE
                 WHEN TOKEN(TOKEN-IX) = 'IN'
                      SET IN-TOKEN TO TRUE
                 WHEN TOKEN(TOKEN-IX) = 'TO'
                      SET TO-TOKEN TO TRUE
                 WHEN TOKEN(TOKEN-IX) = 'BE'
                  AND TO-TOKEN
                      MOVE ' TO BE ' TO TOKEN (TOKEN-IX - 1)
                      MOVE 7 TO TOKEN-LENGTH (TOKEN-IX - 1)
                      PERFORM 4312-RESET-CURRENT-TOKEN
                      SET TOKEN-IX DOWN BY 1
                 WHEN TOKEN(TOKEN-IX) = 'NOT'
                      SET REVERSE-COMPARE TO TRUE
                      PERFORM 4312-RESET-CURRENT-TOKEN
                      SET TOKEN-IX DOWN BY 1
                 WHEN IN-TOKEN
                      STRING TOKEN (TOKEN-IX - 2) DELIMITED BY '  '
                             ' IN '  DELIMITED BY SIZE
                             TOKEN (TOKEN-IX) DELIMITED BY SPACE
                        INTO TOKEN (TOKEN-IX - 2)
                      ADD TOKEN-LENGTH (TOKEN-IX) 4
                       TO TOKEN-LENGTH (TOKEN-IX - 2)
                      PERFORM 4312-RESET-CURRENT-TOKEN
                      SET TOKEN-IX DOWN BY 1
                      PERFORM 4312-RESET-CURRENT-TOKEN
                      SET TOKEN-IX DOWN BY 1
                      SET UNKNOWN-TOKEN TO TRUE
             END-EVALUATE
      
             SET TOKEN-IX UP BY 1
             PERFORM 7100-NEXT-TOKEN-TEST-CASES
             MOVE TOKEN(TOKEN-IX) TO CANDIDATE-TEST-KEYWORD
           END-PERFORM
      
           PERFORM 4315-EXTRACT-SUBSCRIPT
      
           IF TOKEN-IX <= 100
           AND TOKEN(3) = ' TO BE '
             PERFORM 4320-EXPECT-EQUALITY
             SET SUPPRESS-TEST-CASE-READ TO TRUE
           ELSE
             DISPLAY MSG-EXPECT-SYNTAX-1
             DISPLAY MSG-EXPECT-SYNTAX-2
             DISPLAY MSG-EXPECT-SYNTAX-3
           END-IF
           .
      
      *****************************************************************
       4312-RESET-CURRENT-TOKEN.
      *****************************************************************
           MOVE SPACES TO TOKEN(TOKEN-IX)
           MOVE ZERO TO TOKEN-LENGTH (TOKEN-IX)
           .
      
       4315-EXTRACT-SUBSCRIPT.
      *****************************************************************
      * Extract and remove subscript from token list and put it
      * into WS-SUBSCRIPT
      *****************************************************************
      
           MOVE 1 TO WS-SUB-PTR
           MOVE SPACE TO WS-SUBSCRIPT
           PERFORM VARYING TALLY FROM 3 BY 1
              UNTIL TALLY = TOKEN-IX
                 OR TOKEN (TALLY) = ' TO BE '
              MOVE 1 TO WS-LEADING
              INSPECT TOKEN(TALLY) TALLYING WS-LEFT FOR ALL '('
                                    WS-LEADING FOR LEADING SPACES
              IF WS-LEFT > 0
                STRING TOKEN (TALLY) (WS-LEADING:) DELIMITED BY SPACE
                  INTO WS-SUBSCRIPT WITH POINTER WS-SUB-PTR
                ADD 1 TO WS-SUB-PTR
              END-IF
           END-PERFORM
      
           IF WS-SUBSCRIPT NOT = SPACE AND TOKEN (TALLY) = ' TO BE '
             MOVE 3 TO WS-IX
             PERFORM VARYING TALLY FROM TALLY BY 1
               UNTIL TALLY > TOKEN-IX
               MOVE TOKEN (TALLY) TO TOKEN (WS-IX)
               MOVE ORIGINAL-TOKEN(TALLY) TO ORIGINAL-TOKEN(WS-IX)
               ADD 1 TO WS-IX
             END-PERFORM
           END-IF
           .
       4320-EXPECT-EQUALITY.
      *****************************************************************
      * Generate code in TEST-SOURCE to assert expected and actual
      * results are equal.
      *****************************************************************
           SET DATATYPE-X TO TRUE
      
           IF TOKEN(4) IS EQUAL TO NUMERIC-KEYWORD
               SET DATATYPE-9 TO TRUE
           END-IF
      
           IF TOKEN(4) IS EQUAL TO TRUE-KEYWORD OR FALSE-KEYWORD
               SET DATATYPE-88 TO TRUE
           END-IF
      
           INITIALIZE GENERATED-SOURCE-STATEMENTS
           SET STATEMENT-IX TO 1
      
           MOVE INCR-TESTCASE-COUNT-STMT
               TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           SET STATEMENT-IX UP BY 1
      
           PERFORM 4360-GENERATE-SET-REVERSE
      
           EVALUATE TRUE
               WHEN DATATYPE-9
                    PERFORM 4330-GENERATE-NUMERIC-COMPARE
               WHEN DATATYPE-88
                    PERFORM 4340-GENERATE-88-COMPARE
               WHEN OTHER
                    PERFORM 4350-GENERATE-PIC-X-COMPARE
           END-EVALUATE
      
           PERFORM 9440-WRITE-GENERATED-STMTS
           SET SUPPRESS-COPY-SOURCE TO TRUE
           .
      
       4330-GENERATE-NUMERIC-COMPARE.
      * This sections creates the following code lines:
      * IF x =
      *    y
      *   SET UT-COMPARE-PASSED TO TRUE
      * ELSE
      *   SET UT-COMPARE-FAILED TO TRUE
      * END-IF
      * PERFORM UT-NUMERIC-COMPARE
      * IF UT-COMPARE-FAILED
      *   DISPLAY '    EXPECTED ' y
      *             ' , WAS ' x
      * END-IF
      
           MOVE SPACES TO GENERATED-STATEMENT-INPUT         
           STRING "'"                DELIMITED BY SIZE      
                TOKEN(5)             DELIMITED BY SPACE     
                  "'"                DELIMITED BY SIZE      
             INTO GENERATED-STATEMENT-INPUT                 
           PERFORM 4390-PREPARE-NICE-EXPECT                 
       
           STRING
               IF-VERB              DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                         (STATEMENT-OFFSET:STATEMENT-LENGTH)
           END-STRING
           SET STATEMENT-IX UP BY 1
           PERFORM 4370-WRITE-TOKEN-2
      
           STRING
               ' = '                DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                         (STATEMENT-OFFSET:STATEMENT-LENGTH)
           END-STRING
           SET STATEMENT-IX UP BY 1
      
           STRING
               TOKEN(5)             DELIMITED BY '  '
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                         (STATEMENT-OFFSET:STATEMENT-LENGTH)
           END-STRING
      
           SET STATEMENT-IX UP BY 1
           STRING '  '               DELIMITED BY SIZE
               SET-VERB              DELIMITED BY SIZE
               ' UT-COMPARE-PASSED'  DELIMITED BY SIZE
               TO-KEYWORD            DELIMITED BY SIZE
               TRUE-KEYWORD          DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                         (STATEMENT-OFFSET:STATEMENT-LENGTH)
           END-STRING
      
           SET STATEMENT-IX UP BY 1
           MOVE 'ELSE'
               TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                   (STATEMENT-OFFSET:STATEMENT-LENGTH)
      
           SET STATEMENT-IX UP BY 1
           STRING '  '               DELIMITED BY SIZE
               SET-VERB              DELIMITED BY SIZE
               ' UT-COMPARE-FAILED'  DELIMITED BY SIZE
               TO-KEYWORD            DELIMITED BY SIZE
               TRUE-KEYWORD          DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                         (STATEMENT-OFFSET:STATEMENT-LENGTH)
           END-STRING
           SET STATEMENT-IX UP BY 1
           MOVE 'END-IF'
               TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                   (STATEMENT-OFFSET:STATEMENT-LENGTH)
      
           SET STATEMENT-IX UP BY 1
           STRING
               PERFORM-VERB          DELIMITED BY SIZE
               ' UT-NUMERIC-COMPARE' DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                         (STATEMENT-OFFSET:STATEMENT-LENGTH)
           END-STRING
      
           SET STATEMENT-IX UP BY 1
           STRING
               IF-VERB              DELIMITED BY SIZE
               'UT-COMPARE-FAILED'  DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                         (STATEMENT-OFFSET:STATEMENT-LENGTH)
           END-STRING
      
           SET STATEMENT-IX UP BY 1
           STRING '  '               DELIMITED BY SIZE
               DISPLAY-VERB          DELIMITED BY SIZE
               "'    EXPECTED '"     DELIMITED BY SIZE
               TOKEN(5)             DELIMITED BY '  '
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                         (STATEMENT-OFFSET:STATEMENT-LENGTH)
           END-STRING
      
           SET STATEMENT-IX UP BY 1
           STRING '  '              DELIMITED BY SIZE
               "' , WAS '"          DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                         (STATEMENT-OFFSET:STATEMENT-LENGTH)
           END-STRING
      
           SET STATEMENT-IX UP BY 1
           PERFORM 4370-WRITE-TOKEN-2
      
           MOVE 'END-IF'
               TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                   (STATEMENT-OFFSET:STATEMENT-LENGTH)
           .
      
       4340-GENERATE-88-COMPARE.
      
           MOVE SPACES TO GENERATED-STATEMENT-INPUT     
           STRING "'"                DELIMITED BY SIZE  
                TOKEN(4)             DELIMITED BY SPACE 
                  "'"                DELIMITED BY SIZE  
             INTO GENERATED-STATEMENT-INPUT             
           PERFORM 4390-PREPARE-NICE-EXPECT             
      
      * EXPECT foo TO BE TRUE
           STRING
               MOVE-VERB             DELIMITED BY SIZE
               'UT-TEST-CASE-COUNT'  DELIMITED BY SPACE
               TO-KEYWORD            DELIMITED BY SIZE
               'UT-TEST-CASE-NUMBER' DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                         (STATEMENT-OFFSET:STATEMENT-LENGTH)
           END-STRING
           SET STATEMENT-IX UP BY 1
      
           STRING
               SET-VERB              DELIMITED BY SIZE
               ' UT-COMPARE-DEFAULT' DELIMITED BY SIZE
               TO-KEYWORD            DELIMITED BY SIZE
               TRUE-KEYWORD          DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                         (STATEMENT-OFFSET:STATEMENT-LENGTH)
           END-STRING
           SET STATEMENT-IX UP BY 1
      
      * TOKEN(2) is the field name
      * TOKEN(4) is the truth value (TRUE or FALSE)
      
           MOVE SPACES TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           STRING
               IF-VERB              DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                         (STATEMENT-OFFSET:STATEMENT-LENGTH)
           END-STRING
           SET STATEMENT-IX UP BY 1
           PERFORM 4370-WRITE-TOKEN-2
      
           MOVE SPACES TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           STRING
               MOVE-VERB            DELIMITED BY SIZE
               " 'TRUE' "           DELIMITED BY SIZE
               TO-KEYWORD           DELIMITED BY SIZE
               'UT-ACTUAL'          DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                         (STATEMENT-OFFSET:STATEMENT-LENGTH)
           END-STRING
           SET STATEMENT-IX UP BY 1
      
           MOVE SPACES TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           STRING
               ELSE-KEYWORD           DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                         (STATEMENT-OFFSET:STATEMENT-LENGTH)
           END-STRING
           SET STATEMENT-IX UP BY 1
      
           MOVE SPACES TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           STRING
               MOVE-VERB            DELIMITED BY SIZE
               " 'FALSE' "           DELIMITED BY SIZE
               TO-KEYWORD           DELIMITED BY SIZE
               'UT-ACTUAL'          DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                         (STATEMENT-OFFSET:STATEMENT-LENGTH)
           END-STRING
           SET STATEMENT-IX UP BY 1
      
           MOVE SPACES TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           STRING
               ENDIF-KEYWORD           DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                         (STATEMENT-OFFSET:STATEMENT-LENGTH)
           END-STRING
           SET STATEMENT-IX UP BY 1
      
           MOVE TOKEN(4) TO RAW-VALUE
           PERFORM 7950-ENCLOSE-IN-QUOTES
           STRING
               MOVE-VERB             DELIMITED BY SIZE
               QUOTED-VALUE          DELIMITED BY SPACE
               TO-KEYWORD            DELIMITED BY SIZE
               'UT-EXPECTED'         DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                         (STATEMENT-OFFSET:STATEMENT-LENGTH)
           END-STRING
           SET STATEMENT-IX UP BY 1
      
           STRING
               PERFORM-VERB          DELIMITED BY SIZE
               ' UT-ASSERT-EQUAL'    DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                         (STATEMENT-OFFSET:STATEMENT-LENGTH)
           END-STRING
           SET STATEMENT-IX UP BY 1
           .
      
       4350-GENERATE-PIC-X-COMPARE.
           MOVE SPACES TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           
           MOVE ORIGINAL-TOKEN(4)           
             TO GENERATED-STATEMENT-INPUT   
           PERFORM 4390-PREPARE-NICE-EXPECT 
      
           STRING
               MOVE-VERB            DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                         (STATEMENT-OFFSET:STATEMENT-LENGTH)
           END-STRING
           SET STATEMENT-IX UP BY 1
           PERFORM 4370-WRITE-TOKEN-2
           STRING
               STATEMENT-SPACER     DELIMITED BY SIZE
               TO-KEYWORD           DELIMITED BY SIZE
               'UT-ACTUAL'          DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                         (STATEMENT-OFFSET:STATEMENT-LENGTH)
           END-STRING
           SET STATEMENT-IX UP BY 1
           STRING
               MOVE-VERB             DELIMITED BY SIZE
               ORIGINAL-TOKEN(4)     DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                         (STATEMENT-OFFSET:STATEMENT-LENGTH)
           END-STRING
           SET STATEMENT-IX UP BY 1
           STRING
               STATEMENT-SPACER      DELIMITED BY SIZE
               TO-KEYWORD            DELIMITED BY SIZE
               'UT-EXPECTED'         DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                         (STATEMENT-OFFSET:STATEMENT-LENGTH)
           END-STRING
           SET STATEMENT-IX UP BY 1
           STRING
               SET-VERB              DELIMITED BY SIZE
               ' UT-COMPARE-DEFAULT' DELIMITED BY SIZE
               TO-KEYWORD            DELIMITED BY SIZE
               TRUE-KEYWORD          DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                         (STATEMENT-OFFSET:STATEMENT-LENGTH)
           END-STRING
           SET STATEMENT-IX UP BY 1
      
           MOVE 'PERFORM UT-ASSERT-EQUAL'
               TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                   (STATEMENT-OFFSET:STATEMENT-LENGTH)
           .
      
       4360-GENERATE-SET-REVERSE.
           IF REVERSE-COMPARE
               MOVE SET-REVERSE-TRUE-STMT
                    TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           ELSE
               MOVE SET-NORMAL-TRUE-STMT
                    TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           END-IF
           SET STATEMENT-IX UP BY 1
           .
      
      *****************************************************************
       4370-WRITE-TOKEN-2.
      *****************************************************************
           IF TOKEN-LENGTH (2) > 57
             PERFORM 4380-FIND-MIDDLE-IN-POINT
             STRING
                 TOKEN(2)(1:WS-IX)    DELIMITED BY '  '
                 INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                           (STATEMENT-OFFSET:STATEMENT-LENGTH)
             END-STRING
             SET STATEMENT-IX UP BY 1
             STRING
                 TOKEN(2)(WS-IX + 1:) DELIMITED BY '  '
                 WS-SUBSCRIPT         DELIMITED BY '  '
                 INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                           (STATEMENT-OFFSET:STATEMENT-LENGTH)
             END-STRING
           ELSE
             STRING
                 TOKEN(2)             DELIMITED BY '  '
                 WS-SUBSCRIPT         DELIMITED BY '  '
                 INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                           (STATEMENT-OFFSET:STATEMENT-LENGTH)
             END-STRING
           END-IF
           SET STATEMENT-IX UP BY 1
           .
      
      *****************************************************************
       4375-WRITE-TOKEN-2-WITH-QUOTE.                                  
      *****************************************************************
           MOVE 1 TO GENERATED-STATEMENT-LENGTH                        
           IF TOKEN-LENGTH (2) > 57                                    
             PERFORM 4380-FIND-MIDDLE-IN-POINT                         
             STRING                   
                 "'"                  DELIMITED BY SIZE                
                 TOKEN(2)(1:WS-IX)    DELIMITED BY '  '                
                 "'"                  DELIMITED BY SIZE                
                 INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)         
                           (STATEMENT-OFFSET:STATEMENT-LENGTH)         
                 POINTER GENERATED-STATEMENT-LENGTH                    
             END-STRING                                                
           ELSE                                                        
             STRING                                                    
                 "'"                  DELIMITED BY SIZE                
                 TOKEN(2)             DELIMITED BY '  '                
                 WS-SUBSCRIPT         DELIMITED BY '  '                
                 "'"                  DELIMITED BY SIZE                
                 INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)         
                           (STATEMENT-OFFSET:STATEMENT-LENGTH)         
                 POINTER GENERATED-STATEMENT-LENGTH                    
             END-STRING                                                
           END-IF                                                      
           SET STATEMENT-IX UP BY 1                                    
           .                                                           
                                                                 
      *****************************************************************
       4380-FIND-MIDDLE-IN-POINT.
      *****************************************************************
           DIVIDE TOKEN-LENGTH (2) BY 3
           GIVING WS-IX
           PERFORM VARYING WS-IX FROM WS-IX BY 1
             UNTIL WS-IX >= TOKEN-LENGTH (2)
                OR TOKEN(2)(WS-IX:2) = 'IN'
           END-PERFORM
           SUBTRACT 1 FROM WS-IX
           .                                                           
                                                                 
      *****************************************************************
       4390-PREPARE-NICE-EXPECT.                                       
      *****************************************************************
           STRING                                                      
               MOVE-VERB            DELIMITED BY SIZE                  
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)           
                         (STATEMENT-OFFSET:STATEMENT-LENGTH)           
           END-STRING                                                  
           SET STATEMENT-IX UP BY 1                                    
           PERFORM 4375-WRITE-TOKEN-2-WITH-QUOTE                       
           STRING                                                      
               TO-KEYWORD           DELIMITED BY SIZE                  
               'UT-ACTUAL-VARIABLE' DELIMITED BY SIZE                  
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)           
                         (STATEMENT-OFFSET:STATEMENT-LENGTH)           
           END-STRING                                                  
           SET STATEMENT-IX UP BY 1                                    
           MOVE SPACES TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)     
           SUBTRACT 2 FROM GENERATED-STATEMENT-LENGTH
           STRING                                                      
               MOVE-VERB            DELIMITED BY SIZE                  
               GENERATED-STATEMENT-LENGTH DELIMITED BY SIZE            
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)           
                         (STATEMENT-OFFSET:STATEMENT-LENGTH)           
           END-STRING                                        
           SET STATEMENT-IX UP BY 1                          
           STRING                                            
               TO-KEYWORD           DELIMITED BY SIZE        
               'UT-ACTUAL-VARIABLE-LENGTH' DELIMITED BY SIZE 
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX) 
                         (STATEMENT-OFFSET:STATEMENT-LENGTH) 
           END-STRING                                        
           SET STATEMENT-IX UP BY 1                      
           MOVE 1 TO GENERATED-STATEMENT-LENGTH   
           STRING                                            
               MOVE-VERB             DELIMITED BY SIZE       
               GENERATED-STATEMENT-INPUT DELIMITED BY SIZE   
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX) 
                         (STATEMENT-OFFSET:STATEMENT-LENGTH) 
               POINTER GENERATED-STATEMENT-LENGTH 
           END-STRING                                        
           SET STATEMENT-IX UP BY 1                          
           STRING                                            
               TO-KEYWORD            DELIMITED BY SIZE       
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX) 
                         (STATEMENT-OFFSET:STATEMENT-LENGTH) 
           END-STRING                                        
           SET STATEMENT-IX UP BY 1                          
           STRING                                            
               'UT-EXPECT-VALUE'     DELIMITED BY SIZE       
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX) 
                         (STATEMENT-OFFSET:STATEMENT-LENGTH) 
           END-STRING                                        
           SET STATEMENT-IX UP BY 1                          
           STRING                                            
               MOVE-VERB            DELIMITED BY SIZE        
               GENERATED-STATEMENT-LENGTH DELIMITED BY SIZE  
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX) 
                         (STATEMENT-OFFSET:STATEMENT-LENGTH) 
           END-STRING                                        
           SET STATEMENT-IX UP BY 1                          
           STRING                                            
               TO-KEYWORD           DELIMITED BY SIZE        
               'UT-EXPECT-VALUE-LENGTH' DELIMITED BY SIZE    
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX) 
                         (STATEMENT-OFFSET:STATEMENT-LENGTH) 
           END-STRING                                        
           SET STATEMENT-IX UP BY 1                          
           .
      
       4400-KEYWORD-TESTCASE.
      *****************************************************************
      * TESTCASE keyword was found.
      *
      * Convert
      *
      *     TESTCASE [description]
      *
      * into
      *     PERFORM UT-AFTER (unless this is the first test case)
      *     MOVE [description] TO UT-TEST-CASE-NAME
      *     PERFORM UT-BEFORE
      *****************************************************************
           ADD 8 TO KEYWORD-OFFSET
           MOVE TEST-CASE-UPPERED-LINE (KEYWORD-OFFSET:72) TO TOKEN(2)
           INITIALIZE GENERATED-SOURCE-STATEMENTS
           SET STATEMENT-IX TO 1
           MOVE TOKEN(2) TO WS-CURRENT-TESTCASE
      
           IF GENERATE-PERFORM-AFTER
               MOVE 'PERFORM UT-AFTER' TO
                   GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                       (STATEMENT-OFFSET:STATEMENT-LENGTH)
               SET STATEMENT-IX UP BY 1
           ELSE
               SET GENERATE-PERFORM-AFTER TO TRUE
           END-IF
      
           STRING                                    
               DISPLAY-VERB        DELIMITED BY SIZE 
               "' '"               DELIMITED BY SIZE               
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)       
                   (STATEMENT-OFFSET:STATEMENT-LENGTH)             
           END-STRING                                              
           SET STATEMENT-IX UP BY 1                                
           MOVE SPACES TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX) 
           STRING                                                  
               DISPLAY-VERB        DELIMITED BY SIZE               
               "'RUNNING TESTCASE: '"                              
               TOKEN(2)            DELIMITED BY SIZE               
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)       
                   (STATEMENT-OFFSET:)                             
           END-STRING                                              
           SET STATEMENT-IX UP BY 1                                
      
           STRING
               MOVE-VERB           DELIMITED BY SIZE
               TOKEN(2)            DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                   (STATEMENT-OFFSET:STATEMENT-LENGTH)
           END-STRING
           SET STATEMENT-IX UP BY 1
      
           MOVE '    TO UT-TEST-CASE-NAME' TO
               GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                   (STATEMENT-OFFSET:STATEMENT-LENGTH)
           SET STATEMENT-IX UP BY 1
      
           MOVE 'PERFORM UT-BEFORE' TO
               GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                   (STATEMENT-OFFSET:STATEMENT-LENGTH)
           SET STATEMENT-IX UP BY 1
      
           MOVE 'PERFORM UT-INITIALIZE-MOCK' TO
               GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                   (STATEMENT-OFFSET:STATEMENT-LENGTH)
      
           PERFORM 9440-WRITE-GENERATED-STMTS
           SET SUPPRESS-COPY-SOURCE TO TRUE
           .
      
       4500-KEYWORD-TESTSUITE.
      *****************************************************************
      * TESTSUITE keyword was found.
      *
      * Convert
      *
      *     TESTSUITE [description]
      *
      * into
      *     DISPLAY SPACE
      *     DISPLAY 'TEST SUITE:'
      *     DISPLAY [description]
      *     DISPLAY SPACE
      *****************************************************************
           ADD 9 TO KEYWORD-OFFSET
           MOVE TEST-CASE-UPPERED-LINE (KEYWORD-OFFSET:72) TO TOKEN(2)
           INITIALIZE GENERATED-SOURCE-STATEMENTS
           MOVE 'DISPLAY SPACE' TO GENERATED-SOURCE-STATEMENT(1)
                   (STATEMENT-OFFSET:STATEMENT-LENGTH)
           MOVE 'DISPLAY "TEST SUITE:"' TO GENERATED-SOURCE-STATEMENT(2)
                   (STATEMENT-OFFSET:STATEMENT-LENGTH)
           MOVE 'DISPLAY' TO GENERATED-SOURCE-STATEMENT(3)
                   (STATEMENT-OFFSET:STATEMENT-LENGTH)
           MOVE TOKEN(2) TO GENERATED-SOURCE-STATEMENT(4)
                   (STATEMENT-OFFSET:STATEMENT-LENGTH)
           MOVE 'DISPLAY SPACE' TO GENERATED-SOURCE-STATEMENT(5)
                   (STATEMENT-OFFSET:STATEMENT-LENGTH)
           PERFORM 9440-WRITE-GENERATED-STMTS
           SET SUPPRESS-COPY-SOURCE TO TRUE
           .
      
       4600-KEYWORD-BEFORE.
      *****************************************************************
      * BEFORE-EACH keyword was found.
      *
      *     BEFORE-EACH [description]
      *         [COBOL statement]
      *         [COBOL statement]
      *     END-BEFORE
      *
      * Save the COBOL statements between BEFORE-EACH and END-BEFORE.
      * They will be written to TEST-SOURCE at the appropriate point.
      *****************************************************************
           INITIALIZE KEYWORD-MATCHED-INDICATOR
           PERFORM VARYING BEFORE-EACH-IX FROM 1 BY 1
                   UNTIL TEST-CASES-END-OF-FILE
                      OR KEYWORD-MATCHED
                      OR BEFORE-EACH-IX IS GREATER THAN 
                                             MAX-BEFORE-EACH-STATEMENTS
               PERFORM 9350-READ-TEST-CASES
               MOVE 'END-BEFORE' TO KEYWORD-TO-MATCH
               MOVE 10 TO KEYWORD-MATCH-LENGTH
               PERFORM 7500-INIT-KEYWORD-SEARCH
      
               MOVE TEST-CASE-UPPERED-LINE TO LINE-TO-PARSE
               SET SCANNING-TEST-CASES TO TRUE
               SET NOT-SCANNING-ORIGINAL-SOURCE TO TRUE
               PERFORM 7400-MATCH-KEYWORD
      
               IF KEYWORD-MATCHED
                   CONTINUE
               ELSE
                   MOVE TEST-CASE-LINE TO BEFORE-EACH-STATEMENT
                                          (BEFORE-EACH-IX)
               END-IF
           END-PERFORM
           SET SUPPRESS-COPY-SOURCE TO TRUE
           .
      
       4700-KEYWORD-AFTER.
      *****************************************************************
      * AFTER-EACH keyword was found.
      *
      *     AFTER-EACH [description]
      *         [COBOL statement]
      *         [COBOL statement]
      *     END-AFTER
      *
      * Save the COBOL statements between AFTER-EACH and END-AFTER.
      * They will be written to TEST-SOURCE at the appropriate point.
      *****************************************************************
           INITIALIZE KEYWORD-MATCHED-INDICATOR
           PERFORM VARYING AFTER-EACH-IX FROM 1 BY 1
                  UNTIL TEST-CASES-END-OF-FILE
                  OR KEYWORD-MATCHED
                  OR AFTER-EACH-IX IS GREATER THAN 
                                        MAX-AFTER-EACH-STATEMENTS
               PERFORM 9350-READ-TEST-CASES
               MOVE 'END-AFTER' TO KEYWORD-TO-MATCH
               MOVE 9 TO KEYWORD-MATCH-LENGTH
               PERFORM 7500-INIT-KEYWORD-SEARCH
      
               MOVE TEST-CASE-UPPERED-LINE TO LINE-TO-PARSE
               SET SCANNING-TEST-CASES TO TRUE
               SET NOT-SCANNING-ORIGINAL-SOURCE TO TRUE
               PERFORM 7400-MATCH-KEYWORD
      
               IF KEYWORD-MATCHED
                   CONTINUE
               ELSE
                   MOVE TEST-CASE-LINE TO AFTER-EACH-STATEMENT
                                          (AFTER-EACH-IX)
               END-IF
           END-PERFORM
           SET SUPPRESS-COPY-SOURCE TO TRUE
           .
      
       4800-KEYWORD-MOCK.
      *****************************************************************
      * If the word 'MOCK' is not the first token on the line, then it
      * is in the middle of a description (e.g. TESTCASE, TESTSUITE).
      * In that case, ignore it.
      *****************************************************************
           SET NOT-FIRST-TOKEN TO TRUE
           PERFORM VARYING TEMP-SUB FROM 1 BY 1
                   UNTIL TEMP-SUB IS GREATER THAN KEYWORD-END
               IF TEST-CASE-UPPERED-LINE (TEMP-SUB:1) IS NOT EQUAL
                  TO SPACE
                   IF TEST-CASE-UPPERED-LINE (TEMP-SUB:4)
                          IS EQUAL TO MOCK-KEYWORD
                      SET IS-FIRST-TOKEN TO TRUE
                   END-IF
                   MOVE KEYWORD-END TO TEMP-SUB
               END-IF
           END-PERFORM
           IF IS-FIRST-TOKEN
               PERFORM 4810-PROCESS-MOCK-SPEC
           END-IF
           .
      
       4810-PROCESS-MOCK-SPEC.
      *****************************************************************
      * MOCK keyword was found.
      *
      * Save information about the MOCK so that code can be
      * substituted to simulate interaction with the mocked item
      * when references are encountered in ORIGINAL-SOURCE.
      *****************************************************************
           SET NO-MOCK-SYNTAX-ERROR TO TRUE
      
      * Next token will be the type of mock
      
           ADD 4 TO KEYWORD-OFFSET
           PERFORM 7100-NEXT-TOKEN-TEST-CASES
           ADD 1 TO MOCK-COUNT
           SET MOCK-IX TO MOCK-COUNT
           EVALUATE TRUE
               WHEN TOKEN(TOKEN-IX) IS EQUAL TO FILE-KEYWORD
                    PERFORM 4900-PROCESS-MOCK-FILE-SPEC
               WHEN TOKEN(TOKEN-IX) IS EQUAL TO CALL-KEYWORD
                    PERFORM 4910-PROCESS-MOCK-CALL-SPEC
               WHEN TOKEN(TOKEN-IX) IS EQUAL TO CICS-KEYWORD
                    PERFORM 4920-PROCESS-MOCK-CICS-SPEC
               WHEN TOKEN(TOKEN-IX) IS EQUAL TO SECTION-KEYWORD
                    PERFORM 4930-PROCESS-MOCK-SECT-SPEC
               WHEN TOKEN(TOKEN-IX) IS EQUAL TO PARA-KEYWORD
                                             OR PARAGRAPH-KEYWORD
                    PERFORM 4940-PROCESS-MOCK-PARA-SPEC
               WHEN OTHER
                    SET MOCK-SYNTAX-ERROR TO TRUE
           END-EVALUATE
           IF NO-MOCK-SYNTAX-ERROR
               PERFORM 9440-WRITE-GENERATED-STMTS
           ELSE
               DISPLAY THIS-PROGRAM MSG-MOCK-SYNTAX-1
               DISPLAY THIS-PROGRAM MSG-MOCK-SYNTAX-2
               DISPLAY THIS-PROGRAM MSG-MOCK-SYNTAX-3
               DISPLAY THIS-PROGRAM MSG-MOCK-SYNTAX-4
           END-IF
           SET SUPPRESS-COPY-SOURCE TO TRUE
           .
      
       4900-PROCESS-MOCK-FILE-SPEC.
      *****************************************************************
      * Process a specification to mock access to a file.
      *
      *     MOCK FILE [internal-name]
      *         ON [io-operation] RETURN [record-value]
      *     END-MOCK
      *
      *     MOCK FILE [internal-name]
      *         ON [io-operation] STATUS [IS] [file status]
      *     END-MOCK
      *
      * By the time this routine is performed, the keywords MOCK and
      * FILE have already been processed. The next expected token is
      * the filename.
      *****************************************************************
           SET MOCK-FILE(MOCK-IX) TO TRUE
           MOVE SPACES TO GENERATED-SOURCE-STATEMENTS
           SET STATEMENT-IX TO 1
           MOVE SET-FIND-FILE-MOCK-STMT
               TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
      
           ADD 1 TO KEYWORD-OFFSET
           PERFORM 7100-NEXT-TOKEN-TEST-CASES
           MOVE TOKEN(TOKEN-IX) TO MOCK-FILENAME(MOCK-IX)
      
           MOVE TOKEN(TOKEN-IX) TO RAW-VALUE
           PERFORM 7950-ENCLOSE-IN-QUOTES
           SET STATEMENT-IX UP BY 1
           STRING
               STATEMENT-SPACER        DELIMITED BY SIZE
               MOVE-VERB               DELIMITED BY SIZE
               QUOTED-VALUE            DELIMITED BY SPACE
               TO-KEYWORD              DELIMITED BY SIZE
               'UT-MOCK-FIND-FILENAME' DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           END-STRING
      
      * Next token must be 'ON', as in 'ON REWRITE do such-and-such'
      
           SET TOKEN-IX UP BY 1
           PERFORM 9350-READ-TEST-CASES
           MOVE KEYWORD-START TO KEYWORD-OFFSET
           PERFORM 7100-NEXT-TOKEN-TEST-CASES
           IF TOKEN(TOKEN-IX) IS NOT EQUAL TO 'ON'
               SET MOCK-SYNTAX-ERROR TO TRUE
           END-IF
      
      * Next token must be an I/O operation such as READ or WRITE.
      
           ADD 1 TO KEYWORD-OFFSET
           PERFORM 7100-NEXT-TOKEN-TEST-CASES
      
           SET MOCK-SYNTAX-ERROR TO TRUE
           PERFORM VARYING VALID-FILE-OPERATION-IX FROM 1 BY 1
                   UNTIL VALID-FILE-OPERATION-IX IS GREATER THAN
                   MAX-VALID-FILE-OPERATIONS
               IF TOKEN(TOKEN-IX) IS EQUAL TO
                       VALID-FILE-OPERATION(VALID-FILE-OPERATION-IX)
                   SET NO-MOCK-SYNTAX-ERROR TO TRUE
                   MOVE TOKEN(TOKEN-IX) TO MOCK-OPERATION(MOCK-IX)
      
                   MOVE TOKEN(TOKEN-IX) TO RAW-VALUE
                   PERFORM 7950-ENCLOSE-IN-QUOTES
                   SET STATEMENT-IX UP BY 1
                   MOVE SPACES TO GENERATED-SOURCE-STATEMENT
                                             (STATEMENT-IX)
                   STRING
                       STATEMENT-SPACER         DELIMITED BY SIZE
                       MOVE-VERB                DELIMITED BY SIZE
                       QUOTED-VALUE             DELIMITED BY SPACE
                       TO-KEYWORD               DELIMITED BY SIZE
                       'UT-MOCK-FIND-OPERATION' DELIMITED BY SIZE
                       INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                   END-STRING
               END-IF
           END-PERFORM
      
      * Keyword 'STATUS' is next.
      
           SET TOKEN-IX UP BY 1
           PERFORM 7100-NEXT-TOKEN-TEST-CASES
      
      * Optional syntactic sugar 'IS' may or may not be next.
      
           SET TOKEN-IX UP BY 1
           PERFORM 7100-NEXT-TOKEN-TEST-CASES
           IF TOKEN(TOKEN-IX) IS EQUAL TO 'IS'
               PERFORM 7100-NEXT-TOKEN-TEST-CASES
           END-IF
      
           SET STATEMENT-IX UP BY 1
           PERFORM 4960-GEN-INIT-ACCESS-COUNT
           EVALUATE TRUE
      
      * If the last token was RETURN, then the current token is a
      * record value.
      
               WHEN TOKEN(TOKEN-IX - 1) IS EQUAL TO 'RETURN'
                    MOVE TOKEN(TOKEN-IX)
                      TO MOCK-RECORD(MOCK-IX)
      
                    SET STATEMENT-IX UP BY 1
                    STRING
                        STATEMENT-SPACER     DELIMITED BY SIZE
                        MOVE-VERB            DELIMITED BY SIZE
                        TOKEN(TOKEN-IX)      DELIMITED BY SPACE
                        TO-KEYWORD           DELIMITED BY SIZE
                        'UT-MOCK-SET-RECORD' DELIMITED BY SIZE
                        INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                    END-STRING
      
      * If the last token was STATUS, then the current token is a
      * FILE STATUS value.
      
               WHEN TOKEN(TOKEN-IX - 1) IS EQUAL TO 'STATUS'
                    PERFORM VARYING MOCK-STATUS-IX FROM 1 BY 1
                            UNTIL MOCK-STATUS-IX IS GREATER THAN
                                  MAX-STATUS-MNEMONICS
                               OR TOKEN(TOKEN-IX) IS EQUAL TO
                                  MOCK-FILE-STATUS-MNEMONIC
                                      (MOCK-STATUS-IX)
                    END-PERFORM
                    IF MOCK-STATUS-IX IS GREATER THAN
                            MAX-STATUS-MNEMONICS
                        PERFORM 7900-STRIP-QUOTES
                        MOVE TOKEN(TOKEN-IX)(1:2) TO
                            MOCK-FILE-STATUS(MOCK-IX)
                    ELSE
                        MOVE MOCK-FILE-STATUS-VALUE(MOCK-STATUS-IX)
                            TO MOCK-FILE-STATUS(MOCK-IX)
                    END-IF
      
                    SET STATEMENT-IX UP BY 1
                    STRING
                        STATEMENT-SPACER          DELIMITED BY SIZE
                        MOVE-VERB                 DELIMITED BY SIZE
                        MOCK-FILE-STATUS(MOCK-IX) DELIMITED BY SPACE
                        TO-KEYWORD                DELIMITED BY SIZE
                        'UT-MOCK-SET-FILE-STATUS' DELIMITED BY SIZE
                        INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                    END-STRING
      
               WHEN OTHER
                    SET MOCK-SYNTAX-ERROR TO TRUE
           END-EVALUATE
      
      * MOCK specifications must end with END-MOCK
      
           SET TOKEN-IX UP BY 1
           PERFORM 7100-NEXT-TOKEN-TEST-CASES
           IF TOKEN(TOKEN-IX) IS NOT EQUAL TO 'END-MOCK'
               SET MOCK-SYNTAX-ERROR TO TRUE
           END-IF
      
           SET STATEMENT-IX UP BY 1
           MOVE SET-FIND-FILE-MOCK-STMT
                TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
      
           SET STATEMENT-IX UP BY 1
           MOVE SET-MOCK-STMT
                TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           .
      
       4910-PROCESS-MOCK-CALL-SPEC.
      *****************************************************************
      * Process a specification to mock a CALL statement.
      *
      *     MOCK CALL
      *             [tokens]
      *             END-CALL
      *         [any Cobol statements]
      *     END-MOCK
      *
      * By the time this routine is performed, the keyword CALL has
      * already been processed.
      *****************************************************************
           SET MOCK-CALL(MOCK-IX) TO TRUE
           PERFORM 4955-PREPARE-MOCK-LOOKUP-KEY
      
           PERFORM 4912-FIND-CALL-TOKENS
      
           MOVE UT-MOCK-FIND-CALL-TOKENS-STMT TO MOCK-STMT
           MOVE SET-FIND-CALL-MOCK-STMT TO MOCK-SET-STMT
      
           PERFORM 4950-MAKE-INITIALIZE-MOCK-STMT
      * Save the Cobol statements that provide the behavior for this
      * mock (if any). We assume all statements between END-CALL and
      * END-MOCK are to be substituted in the program under test for
      * the original CALL statement.
      
           PERFORM 4980-SAVE-REPL-STMTS-FOR-MOCK
           .
      *****************************************************************
       4912-FIND-CALL-TOKENS.
      *
      * Current token is 'CALL'. Collect tokens until we reach an
      * END-CALL statement. END-CALL must be in the MOCK specification
      * even if it is not used with the corresponding CALL statement
      * in the program under test.
      *****************************************************************
      
           SET MORE-TOKENS TO TRUE
           PERFORM VARYING KEYWORD-OFFSET
                   FROM KEYWORD-OFFSET BY 1
                   UNTIL NO-MORE-TOKENS
                  OR NOT TEST-CASES-STATUS-OK
      
               PERFORM 7100-NEXT-TOKEN-TEST-CASES
               IF TOKEN(TOKEN-IX) IS EQUAL TO END-CALL-KEYWORD
                   SET NO-MORE-TOKENS TO TRUE
               ELSE
                   PERFORM 7900-STRIP-QUOTES
                   MOVE TOKEN(TOKEN-IX) TO MOCK-LOOKUP-KEYWORD
                                           (MOCK-LOOKUP-IX)
                   SET MOCK-LOOKUP-IX UP BY 1
               END-IF
      
           END-PERFORM
      
           PERFORM 5080-STRINGIFY-KEYWORDS
           MOVE MOCK-LOOKUP-KEY TO MOCK-CALL-TOKENS-KEY(MOCK-IX)
           .
      
       4920-PROCESS-MOCK-CICS-SPEC.
      *****************************************************************
      * Process a specification to mock an EXEC CICS command.
      *
      *     MOCK CICS [keywords]
      *         [any Cobol statements]
      *     END-MOCK
      *
      * By the time this routine is performed, the keywords MOCK and
      * CICS have already been processed.
      *****************************************************************
           SET MOCK-CICS(MOCK-IX) TO TRUE
           PERFORM 4955-PREPARE-MOCK-LOOKUP-KEY
      
           PERFORM 4922-FIND-CICS-MOCK-KEYWORDS
      
           MOVE UT-MOCK-FIND-CICS-KEYWO-STMT    TO MOCK-STMT
           MOVE SET-FIND-CICS-MOCK-STMT TO MOCK-SET-STMT
      
           PERFORM 4950-MAKE-INITIALIZE-MOCK-STMT
      
      * Save the Cobol statements that provide the behavior for this
      * mock (if any). We assume all statements following the last
      * CICS keyword and before the END-MOCK statement are Cobol
      * statements that are to be executed when the mocked CICS
      * command is encountered in the program under test.
      
           PERFORM 4980-SAVE-REPL-STMTS-FOR-MOCK
           .
      
      *****************************************************************
       4922-FIND-CICS-MOCK-KEYWORDS.
      *****************************************************************
      
      * Collect the tokens that are part of the EXEC CICS mock spec.
      * When we hit something that is not a CICS reserved word, we have
      * reached the first Cobol statement to be executed to mock the
      * command's behavior at run time.
      
           SET MORE-RESERVED-WORDS TO TRUE
           PERFORM VARYING KEYWORD-OFFSET
                   FROM KEYWORD-OFFSET BY 1
                   UNTIL TOKEN(TOKEN-IX) IS EQUAL TO END-MOCK-KEYWORD
                      OR NO-MORE-RESERVED-WORDS
      
               PERFORM 7100-NEXT-TOKEN-TEST-CASES
      
               MOVE TOKEN(TOKEN-IX) TO CANDIDATE-TOKEN
               INSPECT CANDIDATE-TOKEN REPLACING ALL '(' BY SPACE
               MOVE SPACES TO CANDIDATE-CICS-RESERVED-WORD
               STRING CANDIDATE-TOKEN
                   DELIMITED BY SPACE
                   INTO CANDIDATE-CICS-RESERVED-WORD
               END-STRING
      
               IF TOKEN-IS-CICS-RESERVED-WORD
                   MOVE TOKEN(TOKEN-IX)
                       TO MOCK-LOOKUP-KEYWORD
                           (MOCK-LOOKUP-IX)
                   SET MOCK-LOOKUP-IX UP BY 1
               ELSE
                   SET NO-MORE-RESERVED-WORDS TO TRUE
               END-IF
      
           END-PERFORM
      
           PERFORM 5080-STRINGIFY-KEYWORDS
      
           PERFORM 4925-GENERATE-CICS-KEYWORDS
           .
      
      *****************************************************************
       4925-GENERATE-CICS-KEYWORDS.
      *****************************************************************
           MOVE MOCK-LOOKUP-KEY TO MOCK-CICS-KEYWORDS-DATA(MOCK-IX)
           STRING MOCK-LOOKUP-KEYWORD(1) DELIMITED BY SPACE
              ' ' MOCK-LOOKUP-KEYWORD(2)
             DELIMITED BY SIZE INTO MOCK-CICS-KEYWORDS-KEY(MOCK-IX)
           MOVE MOCK-CICS-KEYWORDS-KEY(MOCK-IX)
             TO MOCK-LOOKUP-KEY
           .
      
       4930-PROCESS-MOCK-SECT-SPEC.
      *****************************************************************
      * Process a specification to mock a section.
      *
      *     MOCK SECTION
      *         [any Cobol statements]
      *     END-MOCK
      *
      * By the time this routine is performed, the keywords MOCK and
      * SECTION have already been processed.
      *****************************************************************
           SET MOCK-SECTION(MOCK-IX) TO TRUE
           PERFORM 4955-PREPARE-MOCK-LOOKUP-KEY
      
           PERFORM 4932-FIND-SECT-MOCK-KEYWORDS
      
           MOVE UT-MOCK-FIND-SECT-NAME-STMT TO MOCK-STMT
           MOVE SET-FIND-SECT-MOCK-STMT TO MOCK-SET-STMT
      
           PERFORM 4950-MAKE-INITIALIZE-MOCK-STMT
      
           SET TOKEN-IX UP BY 1
           PERFORM 4980-SAVE-REPL-STMTS-FOR-MOCK
           .
      
      *****************************************************************
       4932-FIND-SECT-MOCK-KEYWORDS.
      *****************************************************************
      * Next token is the paragraph name
      
           ADD 1 TO KEYWORD-OFFSET
           PERFORM 7100-NEXT-TOKEN-TEST-CASES
           STRING
               TOKEN(TOKEN-IX)          DELIMITED BY SPACE
               INTO MOCK-LOOKUP-KEYWORD (MOCK-LOOKUP-IX)
           END-STRING
      
           PERFORM 5080-STRINGIFY-KEYWORDS
      
           MOVE MOCK-LOOKUP-KEY TO MOCK-SECT-NAME(MOCK-IX)
           .
      
       4940-PROCESS-MOCK-PARA-SPEC.
      *****************************************************************
      * Process a specification to mock a paragraph.
      *
      *     MOCK PARA[GRAPH]
      *         [any Cobol statements]
      *     END-MOCK
      *
      * By the time this routine is performed, the keywords MOCK and
      * PARA or PARAGRAPH have already been processed.
      *****************************************************************
           SET MOCK-PARAGRAPH(MOCK-IX) TO TRUE
           PERFORM 4955-PREPARE-MOCK-LOOKUP-KEY
      
           PERFORM 4942-FIND-PARA-MOCK-KEYWORDS
      
           MOVE UT-MOCK-FIND-PARA-NAME-STMT TO MOCK-STMT
           MOVE SET-FIND-PARA-MOCK-STMT TO MOCK-SET-STMT
      
           PERFORM 4950-MAKE-INITIALIZE-MOCK-STMT
      
           SET TOKEN-IX UP BY 1
           PERFORM 4980-SAVE-REPL-STMTS-FOR-MOCK
           .
      
      *****************************************************************
       4942-FIND-PARA-MOCK-KEYWORDS.
      *****************************************************************
      * Next token is the paragraph name
      
           ADD 1 TO KEYWORD-OFFSET
           PERFORM 7100-NEXT-TOKEN-TEST-CASES
           STRING
               TOKEN(TOKEN-IX)          DELIMITED BY SPACE
               PERIOD                   DELIMITED BY SIZE
               INTO MOCK-LOOKUP-KEYWORD (MOCK-LOOKUP-IX)
           END-STRING
      
           PERFORM 5080-STRINGIFY-KEYWORDS
      
           MOVE MOCK-LOOKUP-KEY TO MOCK-PARA-NAME(MOCK-IX)
           .
      
      *****************************************************************
       4950-MAKE-INITIALIZE-MOCK-STMT.
      *****************************************************************
      
           MOVE MOCK-LOOKUP-KEY TO RAW-VALUE
           PERFORM 7950-ENCLOSE-IN-QUOTES
      
           STRING
               STATEMENT-SPACER             DELIMITED BY SIZE
               MOVE-VERB                    DELIMITED BY SIZE
               QUOTED-VALUE                 DELIMITED BY SIZE
               INTO INITIALIZE-MOCK-STATEMENT(INITIALIZE-MOCK-IX)
           END-STRING
           SET INITIALIZE-MOCK-IX UP BY 1
           STRING
               STATEMENT-SPACER             DELIMITED BY SIZE
               TO-KEYWORD                   DELIMITED BY SIZE
               MOCK-STMT                    DELIMITED BY SIZE
               INTO INITIALIZE-MOCK-STATEMENT(INITIALIZE-MOCK-IX)
           END-STRING
           SET INITIALIZE-MOCK-IX UP BY 1
      
           MOVE MOCK-SET-STMT
               TO INITIALIZE-MOCK-STATEMENT(INITIALIZE-MOCK-IX)
           SET INITIALIZE-MOCK-IX UP BY 1
      
           PERFORM 4970-GEN-INIT-MOCK-COUNT
           .
      
      *****************************************************************
       4955-PREPARE-MOCK-LOOKUP-KEY.
      *****************************************************************
           SET MOCK-LOOKUP-IX TO 1
           MOVE SPACES TO MOCK-LOOKUP-KEY
           MOVE SPACES TO MOCK-LOOKUP-KEYWORDS
           .
      
       4960-GEN-INIT-ACCESS-COUNT.
      *****************************************************************
      * Generate code for TEST-SOURCE to initialize the access count
      * for a mock.
      *****************************************************************
           STRING
               STATEMENT-SPACER               DELIMITED BY SIZE
               MOVE-VERB                      DELIMITED BY SIZE
               ZERO-KEYWORD                   DELIMITED BY SIZE
               TO-KEYWORD                     DELIMITED BY SIZE
               'UT-MOCK-ACCESS-COUNT(UT-MOCK-IX)' DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           END-STRING
           .
      
       4970-GEN-INIT-MOCK-COUNT.
      *****************************************************************
      * Generate code for TEST-SOURCE to initialize the access count
      * for a mock.
      *****************************************************************
      
           MOVE SET-MOCK-STMT
                TO INITIALIZE-MOCK-STATEMENT(INITIALIZE-MOCK-IX)
           SET INITIALIZE-MOCK-IX UP BY 1
      
           STRING
               STATEMENT-SPACER               DELIMITED BY SIZE
               MOVE-VERB                      DELIMITED BY SIZE
               ZERO-KEYWORD                   DELIMITED BY SIZE
               TO-KEYWORD                     DELIMITED BY SIZE
               'UT-MOCK-ACCESS-COUNT(UT-MOCK-IX)' DELIMITED BY SIZE
               INTO INITIALIZE-MOCK-STATEMENT(INITIALIZE-MOCK-IX)
           END-STRING
           SET INITIALIZE-MOCK-IX UP BY 1
           .
      
       4980-SAVE-REPL-STMTS-FOR-MOCK.
      *****************************************************************
      * Generate the replacement COBOL source statements for a MOCK.
      *
      * Assumptions:
      *
      * This is called during processing of a MOCK specification that
      * allows replacement COBOL statements to be inserted into the
      * test program, such as MOCK CICS or MOCK PARAGRAPH.
      *
      * All input statements from the unit test file up to the next
      * END-MOCK statement will be inserted into the test program.
      *
      * Be sure TOKEN-IX and MOCK-REPLACEMENT-STMTS-IX are initialized
      * prior to performing this paragraph.
      *****************************************************************
      
      * If last token was END-MOCK, then there are no substitution
      * source lines to be written to the output file.
           SET MOCK-REPLACEMENT-STMTS-IX TO 1
           MOVE SPACES TO MOCK-REPLACEMENT-STMTS(MOCK-IX)
      
           IF TOKEN(TOKEN-IX - 1) IS EQUAL TO END-MOCK-KEYWORD
               CONTINUE
           ELSE
               PERFORM 4990-EXTRACT-REPL-STMTS
           END-IF
           .
      
       4990-EXTRACT-REPL-STMTS.
           PERFORM VARYING MOCK-REPLACEMENT-STMTS-IX
                   FROM MOCK-REPLACEMENT-STMTS-IX BY 1
                   UNTIL MOCK-REPLACEMENT-STMTS-IX IS GREATER THAN
                         MAX-MOCK-REPLACEMENT-STMTS
                      OR TOKEN(TOKEN-IX) IS EQUAL TO END-MOCK-KEYWORD
                      OR END-OF-TEST-CASES
                      OR SUPPRESS-TEST-CASE-READ
      
               PERFORM 7100-NEXT-TOKEN-TEST-CASES
               IF TOKEN(TOKEN-IX) IS EQUAL TO END-MOCK-KEYWORD
                   CONTINUE
               ELSE
                   MOVE TOKEN(TOKEN-IX) TO CANDIDATE-TEST-KEYWORD
                   IF IS-A-TEST-KEYWORD
                       SET SUPPRESS-TEST-CASE-READ TO TRUE
                   ELSE
                       INSPECT TEST-CASE-LINE
                           REPLACING ALL SINGLE-QUOTE BY '"'
                       MOVE TEST-CASE-LINE TO RAW-VALUE
                       PERFORM 7960-FIND-LENGTH-OF-RAW-VALUE
                       MOVE RAW-VALUE(1:TEMP-SUB)
                           TO MOCK-REPLACEMENT-STMT
                           (MOCK-IX, MOCK-REPLACEMENT-STMTS-IX)
                       PERFORM 9350-READ-TEST-CASES
                       MOVE KEYWORD-START TO KEYWORD-OFFSET
                   END-IF
               END-IF
           END-PERFORM
           .
      
       5000-KEYWORD-VERIFY.
      *****************************************************************
      * VERIFY keyword was found.
      *
      * Generate assertion code in TEST-SOURCE.
      *
      * VERIFY FILE filename READ HAPPENED times TIMES
      * VERIFY FILE filename WRITE NEVER HAPPENED
      * VERIFY CICS [keywords] HAPPENED ONCE
      * VERIFY PARA [keywords] HAPPENED ONCE
      * VERIFY SECT [keywords] HAPPENED ONCE
      *****************************************************************
           MOVE TEST-CASE-UPPERED-LINE TO TARGET-STRING
           MOVE 80 TO TARGET-STRING-LENGTH
           PERFORM 7980-CHECK-FOR-IGNORE
           IF STRING-NOT-FOUND
               PERFORM 5001-IDENT-MOCK-TO-VERIFY
           END-IF
           .
      
       5001-IDENT-MOCK-TO-VERIFY.
      *****************************************************************
      * Identify the type of mock for the VERIFY.
      *****************************************************************
           PERFORM 7600-ADJUST-OFFSETS
           PERFORM 7100-NEXT-TOKEN-TEST-CASES
      
           INITIALIZE KEYWORD-MATCHED-INDICATOR
           MOVE KEYWORD-OFFSET TO TEMP-SUB
           PERFORM 5002-CHECK-VERIFY-FILE
           IF KEYWORD-MATCHED
             PERFORM 5010-VERIFY-FILE-MOCK
           ELSE
             PERFORM 5003-CHECK-VERIFY-CALL
             IF KEYWORD-MATCHED
               PERFORM 5020-VERIFY-CALL-MOCK
             ELSE
               PERFORM 5004-CHECK-VERIFY-CICS
               IF KEYWORD-MATCHED
                 PERFORM 5030-VERIFY-CICS-MOCK
               ELSE
                 PERFORM 5005-CHECK-VERIFY-PARA
                 IF KEYWORD-MATCHED
                   PERFORM 5040-VERIFY-PARA-MOCK
                 ELSE
                   PERFORM 5006-CHECK-VERIFY-SECT
                   IF KEYWORD-MATCHED
                     PERFORM 5045-VERIFY-SECT-MOCK
                   ELSE
                     PERFORM 5060-DISPLAY-VERIFY-ERROR
                   END-IF
                 END-IF
               END-IF
             END-IF
           END-IF
           .
      
       5002-CHECK-VERIFY-FILE.
      *****************************************************************
      * Are we verifying a mocked batch file access?
      *****************************************************************
           IF TOKEN (TOKEN-IX) = 'FILE'
             SET KEYWORD-MATCHED TO TRUE
           END-IF
           .
      
       5003-CHECK-VERIFY-CALL.
      *****************************************************************
      * Are we verifying a mocked CALL statement?
      *****************************************************************
           IF TOKEN (TOKEN-IX) = 'CALL'
             SET KEYWORD-MATCHED TO TRUE
           END-IF
           .
      
       5004-CHECK-VERIFY-CICS.
      *****************************************************************
      * Are we verifying a mocked EXEC CICS command?
      *****************************************************************
           IF TOKEN (TOKEN-IX) = 'CICS'
             SET KEYWORD-MATCHED TO TRUE
           END-IF
           .
      
       5005-CHECK-VERIFY-PARA.
      *****************************************************************
      * Are we verifying a mocked paragraph?
      *****************************************************************
           IF TOKEN (TOKEN-IX) = 'PARA' OR 'PARAGRAPH'
             SET KEYWORD-MATCHED TO TRUE
           END-IF
           .
      
       5006-CHECK-VERIFY-SECT.
      *****************************************************************
      * Are we verifying a mocked paragraph?
      *****************************************************************
           IF TOKEN (TOKEN-IX) = 'SECT' OR 'SECTION'
             SET KEYWORD-MATCHED TO TRUE
           END-IF
           .
      
       5010-VERIFY-FILE-MOCK.
      *****************************************************************
      * Generate code for TEST-SOURCE to verify that a given file MOCK
      * was accessed the expected number of times.
      *****************************************************************
           MOVE FILE-KEYWORD TO MOCK-LOOKUP-TYPE
      
      * Expect next tokens to be 'FILE filename operation'
      
           SUBTRACT 7 FROM KEYWORD-OFFSET
           PERFORM 7100-NEXT-TOKEN-TEST-CASES
           MOVE TOKEN(TOKEN-IX) TO MOCK-LOOKUP-FILENAME
           PERFORM 7100-NEXT-TOKEN-TEST-CASES
           MOVE TOKEN(TOKEN-IX) TO MOCK-LOOKUP-OPERATION
           PERFORM 6500-LOOK-UP-FILE-MOCK
           PERFORM 6400-GET-FILE-INFO-FOR-MOCK
           PERFORM 7100-NEXT-TOKEN-TEST-CASES
           PERFORM 5050-PARSE-HAPPENED-SPEC
      
      * Generate statements in TEST-SOURCE to do the verification
      
           MOVE SPACES TO GENERATED-SOURCE-STATEMENTS
           SET STATEMENT-IX TO 1
           MOVE SET-FIND-FILE-MOCK-STMT
                TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           SET STATEMENT-IX UP BY 1
      
           MOVE SPACES TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           MOVE MOCK-LOOKUP-FILENAME TO RAW-VALUE
           PERFORM 7950-ENCLOSE-IN-QUOTES
           STRING
               STATEMENT-SPACER        DELIMITED BY SIZE
               MOVE-VERB               DELIMITED BY SIZE
               QUOTED-VALUE            DELIMITED BY SPACE
               TO-KEYWORD              DELIMITED BY SIZE
               'UT-MOCK-FIND-FILENAME' DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           END-STRING
           SET STATEMENT-IX UP BY 1
      
           MOVE SPACES TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           MOVE MOCK-LOOKUP-OPERATION TO RAW-VALUE
           PERFORM 7950-ENCLOSE-IN-QUOTES
           STRING
               STATEMENT-SPACER         DELIMITED BY SIZE
               MOVE-VERB                DELIMITED BY SIZE
               QUOTED-VALUE             DELIMITED BY SPACE
               TO-KEYWORD               DELIMITED BY SIZE
               'UT-MOCK-FIND-OPERATION' DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           END-STRING
           SET STATEMENT-IX UP BY 1
      
           PERFORM 6100-GEN-LOOKUP-MOCK
           MOVE TEMP-STATEMENT
               TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           SET STATEMENT-IX UP BY 1
      
           MOVE '           IF UT-MOCK-FOUND'
               TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           SET STATEMENT-IX UP BY 1
      
           PERFORM 6200-GEN-LOOKUP-FILE
           MOVE TEMP-STATEMENT
               TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           SET STATEMENT-IX UP BY 1
      
           MOVE ENDIF-STMT TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           SET STATEMENT-IX UP BY 1
      
           PERFORM 5055-GEN-VERIFY-CODE
      
           PERFORM 9440-WRITE-GENERATED-STMTS
           SET SUPPRESS-COPY-SOURCE TO TRUE
           .
      
       5020-VERIFY-CALL-MOCK.
      *****************************************************************
      * Generate code for TEST-SOURCE to verify that a given CALL MOCK
      * was accessed the expected number of times.
      *****************************************************************
           MOVE CALL-KEYWORD TO MOCK-LOOKUP-TYPE
      
      * Expect next tokens to be the name of the called routine and
      * the argument list.
      * When we encounter HAPPENED or NEVER it's the end of the tokens.
      
           SET MOCK-LOOKUP-IX TO 1
           MOVE SPACES TO MOCK-LOOKUP-KEY
           MOVE SPACES TO MOCK-LOOKUP-KEYWORDS
           SET MORE-TOKENS TO TRUE
           PERFORM UNTIL NO-MORE-TOKENS
               PERFORM 7100-NEXT-TOKEN-TEST-CASES
               PERFORM 7900-STRIP-QUOTES
               IF TOKEN(TOKEN-IX) IS EQUAL TO
                  HAPPENED-KEYWORD OR NEVER-KEYWORD
                  SET NO-MORE-TOKENS TO TRUE
               ELSE
                  MOVE TOKEN(TOKEN-IX)
                      TO MOCK-LOOKUP-KEYWORD
                          (MOCK-LOOKUP-IX)
                  SET MOCK-LOOKUP-IX UP BY 1
               END-IF
           END-PERFORM
      
      * This will put a single string containing the command keywords
      * separated by single spaces into MOCK-LOOKUP-KEY.
      
           PERFORM 5080-STRINGIFY-KEYWORDS
      
      * Look up the mock
      
           SET MOCK-NOT-MATCHED TO TRUE
           PERFORM VARYING MOCK-IX FROM 1 BY 1
                   UNTIL MOCK-IX IS GREATER THAN MAX-MOCKS
                      OR MOCK-MATCHED
               IF MOCK-CALL(MOCK-IX)
               AND MOCK-LOOKUP-KEY IS EQUAL TO
                       MOCK-CALL-TOKENS-KEY(MOCK-IX)
                   SET MOCK-MATCHED TO TRUE
               END-IF
           END-PERFORM
      
           PERFORM 5050-PARSE-HAPPENED-SPEC
      
      * Generate statements in TEST-SOURCE to do the verification
      
           MOVE SPACES TO GENERATED-SOURCE-STATEMENTS
           SET STATEMENT-IX TO 1
           MOVE SET-FIND-CALL-MOCK-STMT
                TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           SET STATEMENT-IX UP BY 1
      
           MOVE MOCK-LOOKUP-KEY TO RAW-VALUE
           PERFORM 7960-FIND-LENGTH-OF-RAW-VALUE
      
           MOVE SPACES TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           STRING
               STATEMENT-SPACER             DELIMITED BY SIZE
               MOVE-VERB                    DELIMITED BY SIZE
               SINGLE-QUOTE                 DELIMITED BY SIZE
               RAW-VALUE(1:TEMP-SUB)        DELIMITED BY SIZE
               SINGLE-QUOTE                 DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           END-STRING
           SET STATEMENT-IX UP BY 1
      
           MOVE SPACES TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           STRING
               STATEMENT-SPACER             DELIMITED BY SIZE
               TO-KEYWORD                   DELIMITED BY SIZE
              UT-MOCK-FIND-CALL-TOKENS-STMT DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           END-STRING
           SET STATEMENT-IX UP BY 1
      
           PERFORM 6100-GEN-LOOKUP-MOCK
           MOVE TEMP-STATEMENT
               TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           SET STATEMENT-IX UP BY 1
      
      *    MOVE '           IF UT-MOCK-FOUND'
      *        TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
      *    SET STATEMENT-IX UP BY 1
      *
      *    MOVE INCR-MOCK-ACCESS-COUNT-STMT
      *        TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
      *    SET STATEMENT-IX UP BY 1
      *
      *    MOVE ENDIF-STMT TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
      *    SET STATEMENT-IX UP BY 1
      
           PERFORM 5055-GEN-VERIFY-CODE
      
           PERFORM 9440-WRITE-GENERATED-STMTS
      
           SET SUPPRESS-COPY-SOURCE TO TRUE
           .
      
       5030-VERIFY-CICS-MOCK.
      *****************************************************************
      * Generate code for TEST-SOURCE to verify that a given CICS MOCK
      * was accessed the expected number of times.
      *****************************************************************
           MOVE CICS-KEYWORD TO MOCK-LOOKUP-TYPE
      
      * Expect next tokens to be the CICS command keywords that
      * identify the MOCK (e.g., 'READ DATASET(foo) RIDFLD(bar)', etc.)
      * When we encounter HAPPENED or NEVER it's the end of the tokens.
      
           ADD 1 TO KEYWORD-OFFSET
           SET MOCK-LOOKUP-IX TO 1
           MOVE SPACES TO MOCK-LOOKUP-KEY
           MOVE SPACES TO MOCK-LOOKUP-KEYWORDS
           SET MORE-TOKENS TO TRUE
           PERFORM UNTIL NO-MORE-TOKENS
               PERFORM 7100-NEXT-TOKEN-TEST-CASES
               IF TOKEN(TOKEN-IX) IS EQUAL TO
                  HAPPENED-KEYWORD OR NEVER-KEYWORD
                  SET NO-MORE-TOKENS TO TRUE
               ELSE
                  MOVE TOKEN(TOKEN-IX)
                      TO MOCK-LOOKUP-KEYWORD
                          (MOCK-LOOKUP-IX)
                  SET MOCK-LOOKUP-IX UP BY 1
               END-IF
           END-PERFORM
      
      * This will put a single string containing the command keywords
      * separated by single spaces into MOCK-LOOKUP-KEY.
      
           PERFORM 5080-STRINGIFY-KEYWORDS
      
           PERFORM 4925-GENERATE-CICS-KEYWORDS
      
           SET MOCK-NOT-MATCHED TO TRUE
           PERFORM VARYING MOCK-IX FROM 1 BY 1
                   UNTIL MOCK-IX IS GREATER THAN MAX-MOCKS
                      OR MOCK-MATCHED
               IF MOCK-CICS(MOCK-IX)
               AND MOCK-LOOKUP-KEY IS EQUAL TO
                       MOCK-CICS-KEYWORDS-KEY(MOCK-IX)
                   SET MOCK-MATCHED TO TRUE
               END-IF
           END-PERFORM
      
           PERFORM 5050-PARSE-HAPPENED-SPEC
      
      * Generate statements in TEST-SOURCE to do the verification
      
           MOVE SPACES TO GENERATED-SOURCE-STATEMENTS
           SET STATEMENT-IX TO 1
           MOVE SET-FIND-CICS-MOCK-STMT
                TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           SET STATEMENT-IX UP BY 1
      
           MOVE MOCK-LOOKUP-KEY TO RAW-VALUE
           PERFORM 7960-FIND-LENGTH-OF-RAW-VALUE
      
           MOVE SPACES TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           STRING
               STATEMENT-SPACER             DELIMITED BY SIZE
               MOVE-VERB                    DELIMITED BY SIZE
               SINGLE-QUOTE                 DELIMITED BY SIZE
               RAW-VALUE(1:TEMP-SUB)        DELIMITED BY SIZE
               SINGLE-QUOTE                 DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           END-STRING
           SET STATEMENT-IX UP BY 1
      
           MOVE SPACES TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           STRING
               STATEMENT-SPACER             DELIMITED BY SIZE
               TO-KEYWORD                   DELIMITED BY SIZE
               'UT-MOCK-FIND-CICS-KEYWORDS' DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           END-STRING
           SET STATEMENT-IX UP BY 1
      
           PERFORM 6100-GEN-LOOKUP-MOCK
           MOVE TEMP-STATEMENT
               TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           SET STATEMENT-IX UP BY 1
      
           PERFORM 5055-GEN-VERIFY-CODE
      
           PERFORM 9440-WRITE-GENERATED-STMTS
           SET SUPPRESS-COPY-SOURCE TO TRUE
           .
      
       5040-VERIFY-PARA-MOCK.
      *****************************************************************
      * Generate code for TEST-SOURCE to verify that a given paragraph
      * mock was accessed the expected number of times.
      *****************************************************************
           MOVE PARA-KEYWORD TO MOCK-LOOKUP-TYPE
      
      * Expect next token to be the mock paragraph name
      
           PERFORM 7320-FIND-NEXT-SPACE
           PERFORM 7100-NEXT-TOKEN-TEST-CASES
           MOVE TOKEN(TOKEN-IX) TO MOCK-LOOKUP-PARA-NAME
      
      * Find this mock in the local table
      
           SET MOCK-NOT-MATCHED TO TRUE
           PERFORM VARYING MOCK-IX FROM 1 BY 1
                   UNTIL MOCK-IX IS GREATER THAN MAX-MOCKS
                      OR MOCK-MATCHED
               IF MOCK-PARAGRAPH(MOCK-IX)
               AND MOCK-LOOKUP-PARA-NAME IS EQUAL TO
                       MOCK-PARA-NAME(MOCK-IX)
                   SET MOCK-MATCHED TO TRUE
               END-IF
           END-PERFORM
      
      * Expect next token to be HAPPENED, or NEVER
      * Need to skip past variable-length paragraph name
      
           PERFORM 7100-NEXT-TOKEN-TEST-CASES
      
           PERFORM 5050-PARSE-HAPPENED-SPEC
      
      * Generate statements in TEST-SOURCE to do the verification
      
           MOVE SPACES TO GENERATED-SOURCE-STATEMENTS
           SET STATEMENT-IX TO 1
      
           MOVE SET-FIND-PARA-MOCK-STMT
                TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           SET STATEMENT-IX UP BY 1
      
           MOVE MOCK-LOOKUP-PARA-NAME TO RAW-VALUE
           PERFORM 7960-FIND-LENGTH-OF-RAW-VALUE
      
           MOVE SPACES TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           STRING
               STATEMENT-SPACER             DELIMITED BY SIZE
               MOVE-VERB                    DELIMITED BY SIZE
               SINGLE-QUOTE                 DELIMITED BY SIZE
               RAW-VALUE(1:TEMP-SUB)        DELIMITED BY SIZE
               PERIOD                       DELIMITED BY SIZE
               SINGLE-QUOTE                 DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           END-STRING
           SET STATEMENT-IX UP BY 1
      
           MOVE SPACES TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           STRING
               STATEMENT-SPACER             DELIMITED BY SIZE
               TO-KEYWORD                   DELIMITED BY SIZE
               'UT-MOCK-FIND-PARA-NAME'     DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           END-STRING
           SET STATEMENT-IX UP BY 1
      
           PERFORM 6100-GEN-LOOKUP-MOCK
           MOVE TEMP-STATEMENT
               TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           SET STATEMENT-IX UP BY 1
      
           MOVE '           IF UT-MOCK-FOUND'
               TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           SET STATEMENT-IX UP BY 1
      
           PERFORM 5055-GEN-VERIFY-CODE
           SET STATEMENT-IX UP BY 1
      
           MOVE ENDIF-STMT TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           SET STATEMENT-IX UP BY 1
      
           PERFORM 9440-WRITE-GENERATED-STMTS
           SET SUPPRESS-COPY-SOURCE TO TRUE
           .
      
       5045-VERIFY-SECT-MOCK.
      *****************************************************************
      * Generate code for TEST-SOURCE to verify that a given section
      * mock was accessed the expected number of times.
      *****************************************************************
           MOVE SECT-KEYWORD TO MOCK-LOOKUP-TYPE
      
      * Expect next token to be the mock paragraph name
      
           PERFORM 7320-FIND-NEXT-SPACE
           PERFORM 7100-NEXT-TOKEN-TEST-CASES
           MOVE TOKEN(TOKEN-IX) TO MOCK-LOOKUP-SECT-NAME
      
      * Find this mock in the local table
      
           SET MOCK-NOT-MATCHED TO TRUE
           PERFORM VARYING MOCK-IX FROM 1 BY 1
                   UNTIL MOCK-IX IS GREATER THAN MAX-MOCKS
                      OR MOCK-MATCHED
               IF MOCK-SECTION(MOCK-IX)
               AND MOCK-LOOKUP-SECT-NAME IS EQUAL TO
                       MOCK-SECT-NAME(MOCK-IX)
                   SET MOCK-MATCHED TO TRUE
               END-IF
           END-PERFORM
      
      * Expect next token to be HAPPENED, or NEVER
      * Need to skip past variable-length paragraph name
      
           PERFORM 7100-NEXT-TOKEN-TEST-CASES
      
           PERFORM 5050-PARSE-HAPPENED-SPEC
      
      * Generate statements in TEST-SOURCE to do the verification
      
           MOVE SPACES TO GENERATED-SOURCE-STATEMENTS
           SET STATEMENT-IX TO 1
      
           MOVE SET-FIND-SECT-MOCK-STMT
                TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           SET STATEMENT-IX UP BY 1
      
           MOVE MOCK-LOOKUP-SECT-NAME TO RAW-VALUE
           PERFORM 7960-FIND-LENGTH-OF-RAW-VALUE
      
           MOVE SPACES TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           STRING
               STATEMENT-SPACER             DELIMITED BY SIZE
               MOVE-VERB                    DELIMITED BY SIZE
               SINGLE-QUOTE                 DELIMITED BY SIZE
               RAW-VALUE(1:TEMP-SUB)        DELIMITED BY SIZE
               SINGLE-QUOTE                 DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           END-STRING
           SET STATEMENT-IX UP BY 1
      
           MOVE SPACES TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           STRING
               STATEMENT-SPACER             DELIMITED BY SIZE
               TO-KEYWORD                   DELIMITED BY SIZE
               'UT-MOCK-FIND-SECT-NAME'     DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           END-STRING
           SET STATEMENT-IX UP BY 1
      
           PERFORM 6100-GEN-LOOKUP-MOCK
           MOVE TEMP-STATEMENT
               TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           SET STATEMENT-IX UP BY 1
      
           MOVE '           IF UT-MOCK-FOUND'
               TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           SET STATEMENT-IX UP BY 1
      
           PERFORM 5055-GEN-VERIFY-CODE
           SET STATEMENT-IX UP BY 1
      
           MOVE ENDIF-STMT TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           SET STATEMENT-IX UP BY 1
      
           PERFORM 9440-WRITE-GENERATED-STMTS
           SET SUPPRESS-COPY-SOURCE TO TRUE
           .
      
       5050-PARSE-HAPPENED-SPEC.
      *****************************************************************
      * On entry to this paragraph, the current token is HAPPENED or
      * NEVER. Expect next tokens to be one of the following sequences:
      *      HAPPENED              (if the spec reads NEVER HAPPENED)
      *      ONCE                  (if the spec reads HAPPENED ONCE)
      *      n TIME[S]             (if the spec reads HAPPENED n TIMES)
      *      AT LEAST ONCE
      *      AT LEAST n TIME[S]
      *      NO MORE THAN ONCE
      *      NO MORE THAN n TIME[S]
      *****************************************************************
      
           IF TOKEN(TOKEN-IX) IS EQUAL TO NEVER-KEYWORD
               PERFORM 7100-NEXT-TOKEN-TEST-CASES
               MOVE ZERO TO MOCK-TEST-COUNT
           ELSE
               IF TOKEN(TOKEN-IX)
                   IS EQUAL TO HAPPENED-KEYWORD
                   SET KEYWORD-NOT-MATCHED TO TRUE
                   MOVE KEYWORD-OFFSET TO SAVE-OFFSET
                   MOVE 'NO MORE THAN ' TO KEYWORD-TO-MATCH
                   MOVE 13 TO KEYWORD-MATCH-LENGTH
                   PERFORM 7400-MATCH-KEYWORD
                   IF KEYWORD-MATCHED
                       SET VERIFY-NO-MORE-THAN TO TRUE
                       ADD 12 TO KEYWORD-OFFSET
                   ELSE
                       MOVE SAVE-OFFSET TO KEYWORD-OFFSET
                       MOVE 'AT LEAST ' TO KEYWORD-TO-MATCH
                       MOVE 9 TO KEYWORD-MATCH-LENGTH
                       PERFORM 7400-MATCH-KEYWORD
                       IF KEYWORD-MATCHED
                           SET VERIFY-AT-LEAST TO TRUE
                           ADD 8 TO KEYWORD-OFFSET
                       ELSE
                           MOVE SAVE-OFFSET TO KEYWORD-OFFSET
                           SET VERIFY-EXACT TO TRUE
                       END-IF
                   END-IF
                   PERFORM 7100-NEXT-TOKEN-TEST-CASES
               END-IF
               IF TOKEN(TOKEN-IX) IS EQUAL TO ONCE-KEYWORD
                   MOVE 1 TO MOCK-TEST-COUNT
               ELSE
                   IF TOKEN(TOKEN-IX) = 'ZERO'
                     MOVE 0 TO MOCK-TEST-COUNT
                   ELSE
                     PERFORM 5051-TOKEN-AS-MOCK-TEST-COUNT
                   END-IF
                   PERFORM 7100-NEXT-TOKEN-TEST-CASES
                   IF TOKEN(TOKEN-IX) IS EQUAL TO 'TIME' OR 'TIMES'
                       CONTINUE
                   ELSE
                       PERFORM 5060-DISPLAY-VERIFY-ERROR
                   END-IF
               END-IF
           END-IF
           .

       5051-TOKEN-AS-MOCK-TEST-COUNT.                                   
      ******************************************************************
      * use the current token as a numeric and use it as mock-test-count
      ******************************************************************
           if FUNCTION TEST-NUMVAL(TOKEN(TOKEN-IX)) = 0                 
             COMPUTE MOCK-TEST-COUNT                                    
                   = FUNCTION NUMVAL(TOKEN(TOKEN-IX))                   
           else                                                         
             STRING "TESTCASE:"                                         
                    WS-CURRENT-TESTCASE DELIMITED BY SIZE               
               INTO ERROR-MESSAGE-LINE                                  
             PERFORM 5052-PUSH-ERROR-MESSAGE-LINE                       
                                                                  
             STRING "   TOKEN: '"         DELIMITED BY SIZE             
                    TOKEN(TOKEN-IX)       DELIMITED BY '  '             
                    "' IS NOT NUMERIC."   DELIMITED BY SIZE             
               INTO ERROR-MESSAGE-LINE                                  
             PERFORM 5052-PUSH-ERROR-MESSAGE-LINE                       
             PERFORM 5060-DISPLAY-VERIFY-ERROR                          
           END-IF                                                       
           .                                                            
                                                                  
       5052-PUSH-ERROR-MESSAGE-LINE.                                    
      ***************************************************************** 
      * PUt error message in buffer                                     
      ***************************************************************** 
            SET MSG-VERIFY-IX UP BY 1                                   
           MOVE ERROR-MESSAGE-LINE                                      
             TO MSG-VERIFY-SYNTAX(MSG-VERIFY-IX)                        
           MOVE SPACES TO ERROR-MESSAGE-LINE                            
           .                                                            
                                                                  
       5055-GEN-VERIFY-CODE.
      *****************************************************************
      * Generate VERIFY code common to all types of mocks.
      *****************************************************************
           STRING
               STATEMENT-SPACER                      DELIMITED BY SIZE
               MOVE-VERB                             DELIMITED BY SIZE
               MOCK-TEST-COUNT                       DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           END-STRING
           SET STATEMENT-IX UP BY 1
      
           STRING
               STATEMENT-SPACER                      DELIMITED BY SIZE
               TO-KEYWORD                            DELIMITED BY SIZE
               'UT-EXPECTED-ACCESSES'                DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           END-STRING
           SET STATEMENT-IX UP BY 1
      
           STRING
               STATEMENT-SPACER                      DELIMITED BY SIZE
               MOVE-VERB                             DELIMITED BY SIZE
               'UT-MOCK-ACCESS-COUNT(UT-MOCK-IX)'    DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           END-STRING
           SET STATEMENT-IX UP BY 1
      
           STRING
               STATEMENT-SPACER                      DELIMITED BY SIZE
               TO-KEYWORD                            DELIMITED BY SIZE
               'UT-ACTUAL-ACCESSES'                  DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           END-STRING
           SET STATEMENT-IX UP BY 1
      
           MOVE INCR-TESTCASE-COUNT-STMT
               TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           SET STATEMENT-IX UP BY 1
      
           EVALUATE TRUE
               WHEN VERIFY-AT-LEAST
                    MOVE SET-VERIFY-AT-LEAST-STMT
                         TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
               WHEN VERIFY-NO-MORE-THAN
                    MOVE SET-VERIFY-NO-MORE-THAN-STMT
                         TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
               WHEN OTHER
                    MOVE SET-VERIFY-EXACT-STMT
                         TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           END-EVALUATE
           SET STATEMENT-IX UP BY 1
      
           STRING
               STATEMENT-SPACER                      DELIMITED BY SIZE
               PERFORM-VERB                          DELIMITED BY SIZE
               'UT-ASSERT-ACCESSES'                  DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           END-STRING
           .
      
       5060-DISPLAY-VERIFY-ERROR.
      *****************************************************************
      * Display an informative error message when the program cannot
      * understand a VERIFY specification.
      *****************************************************************
           PERFORM VARYING MSG-VERIFY-IX FROM 1 BY 1
                   UNTIL MSG-VERIFY-IX IS GREATER THAN MSG-VERIFY-COUNT
               DISPLAY THIS-PROGRAM MSG-VERIFY-SYNTAX(MSG-VERIFY-IX)
           END-PERFORM
           DISPLAY THIS-PROGRAM 'INPUT LINE: ' TEST-CASE-LINE
           .
      
       5080-STRINGIFY-KEYWORDS.
      *****************************************************************
      * Format the list of keywords as a single string with the
      * keywords separated by spaces. This will be the key for looking
      * up the mock during test program execution.
      *****************************************************************
           MOVE SPACES TO TEMP-VALUE
           MOVE 1 TO TEMP-VALUE-OFFSET
           PERFORM VARYING MOCK-LOOKUP-IX FROM 1 BY 1
                   UNTIL MOCK-LOOKUP-IX
                         IS GREATER THAN MAX-CICS-KEYWORDS
                      OR MOCK-LOOKUP-KEYWORD (MOCK-LOOKUP-IX)
                         IS EQUAL TO SPACES
      
               INSPECT MOCK-LOOKUP-KEYWORD(MOCK-LOOKUP-IX)
                   REPLACING ALL SINGLE-QUOTE BY '"'
      
               MOVE MOCK-LOOKUP-KEYWORD(MOCK-LOOKUP-IX)
                   TO RAW-VALUE
               PERFORM 7960-FIND-LENGTH-OF-RAW-VALUE
      
               MOVE MOCK-LOOKUP-KEYWORD(MOCK-LOOKUP-IX)
                 TO TEMP-VALUE(TEMP-VALUE-OFFSET:TEMP-SUB)
               ADD TEMP-SUB 1 TO TEMP-VALUE-OFFSET
           END-PERFORM
           MOVE TEMP-VALUE TO MOCK-LOOKUP-KEY
           .
      
       6030-WRITE-GENERATED-STMTS.
      *****************************************************************
      * Write the source statements that express the defined behavior
      * of a mock into TEST-SOURCE.
      *****************************************************************
           IF MOCK-MATCHED
             PERFORM 9415-WRITE-ZUTBEGIN-LINE
             PERFORM VARYING MOCK-REPLACEMENT-STMTS-IX FROM 1 BY 1
                     UNTIL MOCK-REPLACEMENT-STMTS-IX IS GREATER THAN
                           MAX-MOCK-REPLACEMENT-STMTS
                        OR MOCK-REPLACEMENT-STMT
                           (MOCK-IX, MOCK-REPLACEMENT-STMTS-IX)
                           IS EQUAL TO SPACES
                 SET STATEMENT-IX TO MOCK-REPLACEMENT-STMTS-IX
                 MOVE MOCK-REPLACEMENT-STMT
                           (MOCK-IX, MOCK-REPLACEMENT-STMTS-IX)
                     TO GENERATED-SOURCE-STATEMENT
                           (STATEMENT-IX)
             END-PERFORM
             PERFORM 9440-WRITE-GENERATED-STMTS
             PERFORM 9417-WRITE-ZUTEND-LINE
           END-IF
           .
      
       6100-GEN-LOOKUP-MOCK.
      *****************************************************************
      * Generate code for TEST-SOURCE to look up a mock specification
      *****************************************************************
           MOVE SPACES TO TEMP-STATEMENT
           STRING
               STATEMENT-SPACER        DELIMITED BY SIZE
               PERFORM-VERB            DELIMITED BY SIZE
               'UT-LOOKUP-MOCK'        DELIMITED BY SIZE
                INTO TEMP-STATEMENT
           END-STRING
           .
      
       6200-GEN-LOOKUP-FILE.
      *****************************************************************
      * Generate code for TEST-SOURCE to look up file information
      * originally parsed from a SELECT statement based on filename.
      * Assumes the filename is currently in TOKEN(TOKEN-IX).
      *****************************************************************
           MOVE SPACES TO TEMP-STATEMENT
           STRING
               STATEMENT-SPACER        DELIMITED BY SIZE
               PERFORM-VERB            DELIMITED BY SIZE
               'UT-LOOKUP-FILE'        DELIMITED BY SIZE
                INTO TEMP-STATEMENT
           END-STRING
           .
      
       6300-GEN-SET-FILENAME-TO-FIND.
           MOVE SPACES TO TEMP-STATEMENT
           MOVE TOKEN(TOKEN-IX) TO RAW-VALUE
           PERFORM 7950-ENCLOSE-IN-QUOTES
           STRING
               STATEMENT-SPACER        DELIMITED BY SIZE
               MOVE-VERB               DELIMITED BY SIZE
               QUOTED-VALUE            DELIMITED BY SPACE
               TO-KEYWORD              DELIMITED BY SIZE
               'UT-MOCK-FIND-FILENAME' DELIMITED BY SIZE
                INTO TEMP-STATEMENT
           END-STRING
           .
      
       6400-GET-FILE-INFO-FOR-MOCK.
      *****************************************************************
      * MOCK-IX is pointing to a MOCK entry and we need to find the
      * corresponding file information.
      *****************************************************************
           SET FILENAME-NOT-MATCHED TO TRUE
           PERFORM VARYING FILE-IX FROM 1 BY 1
                   UNTIL FILE-IX IS GREATER THAN MAX-FILES
                      OR FILENAME-MATCHED
      
               IF INTERNAL-FILENAME(FILE-IX) IS GREATER THAN SPACES
               AND INTERNAL-FILENAME(FILE-IX) IS EQUAL TO
                       MOCK-FILENAME(MOCK-IX)
                   SET FILENAME-MATCHED TO TRUE
               END-IF
           END-PERFORM
           SET FILE-IX DOWN BY 1
           .
      
       6500-LOOK-UP-FILE-MOCK.
      *****************************************************************
      * Set MOCK-IX to the entry that matches the type (FILE), the
      * filename, and the I/O operation.
      *****************************************************************
           SET MOCK-NOT-MATCHED TO TRUE
           PERFORM VARYING MOCK-IX FROM 1 BY 1
                   UNTIL MOCK-IX IS GREATER THAN MAX-MOCKS
                      OR MOCK-MATCHED
      
               IF MOCK-LOOKUP-TYPE EQUAL MOCK-TYPE(MOCK-IX)
               AND MOCK-LOOKUP-FILENAME EQUAL MOCK-FILENAME(MOCK-IX)
               AND MOCK-LOOKUP-OPERATION EQUAL MOCK-OPERATION(MOCK-IX)
                   SET MOCK-MATCHED TO TRUE
                   SET MOCK-IX DOWN BY 1
               END-IF
           END-PERFORM
           .
      
      *****************************************************************
      * 7xxx - Utility routines
      *****************************************************************
      
       7100-NEXT-TOKEN-TEST-CASES.
      *****************************************************************
      * Extract the next token from TEST-CASES.
      *****************************************************************
           MOVE TEST-CASE-UPPERED-LINE TO LINE-TO-PARSE
           SET SCANNING-TEST-CASES TO TRUE
           SET NOT-SCANNING-ORIGINAL-SOURCE TO TRUE
           SET SPACE-FOUND TO TRUE
           PERFORM 7312-FIND-NEXT-NON-SPACE-TEST UNTIL NON-SPACE-FOUND
                                     OR NOT TEST-CASES-STATUS-OK
           IF TEST-CASES-STATUS-OK
               PERFORM 7300-EXTRACT-NEXT-TOKEN
               MOVE TEST-CASE-LINE
                   (CURRENT-TOKEN-OFFSET:TOKEN-LENGTH(TOKEN-IX))
                 TO ORIGINAL-TOKEN(TOKEN-IX)
           END-IF
           .
      
       7200-NEXT-TOKEN-ORIG-SOURCE.
      *****************************************************************
      * Extract the next token from ORIGINAL-SOURCE.
      *****************************************************************
           MOVE ORIGINAL-UPPERED-LINE TO LINE-TO-PARSE
           SET SCANNING-ORIGINAL-SOURCE TO TRUE
           SET NOT-SCANNING-TEST-CASES TO TRUE
           SET SPACE-FOUND TO TRUE
           PERFORM 7310-FIND-NEXT-NON-SPACE-SRC UNTIL NON-SPACE-FOUND
                                     OR NOT ORIGINAL-SOURCE-STATUS-OK
           IF ORIGINAL-SOURCE-STATUS-OK
               PERFORM 7300-EXTRACT-NEXT-TOKEN
           ELSE
               SET NO-MORE-TOKENS TO TRUE
           END-IF
           .
      
       7300-EXTRACT-NEXT-TOKEN.
      *****************************************************************
      * Find the next token in either ORIGINAL-SOURCE or TEST-CASES
      * and place it in TOKEN(TOKEN-IX).
      *****************************************************************
           IF LINE-TO-PARSE(KEYWORD-OFFSET:1) IS EQUAL TO '''' OR '"'
               MOVE LINE-TO-PARSE(KEYWORD-OFFSET:1)
                    TO WS-STRING-DELIMITER
               COMPUTE WS-NEXT =
                   KEYWORD-OFFSET + 1
               END-COMPUTE
               PERFORM VARYING WS-NEXT FROM WS-NEXT BY 1
                   UNTIL LINE-TO-PARSE(WS-NEXT:1)
                   EQUAL WS-STRING-DELIMITER
               END-PERFORM
               COMPUTE WS-CHARS =
                   WS-NEXT - KEYWORD-OFFSET + 1
               END-COMPUTE
           ELSE
               MOVE ZERO TO WS-CHARS
               INSPECT LINE-TO-PARSE
                       (KEYWORD-OFFSET:KEYWORD-SEARCH-LIMIT)
                   TALLYING WS-CHARS FOR CHARACTERS BEFORE INITIAL SPACE
           END-IF
      
           MOVE LINE-TO-PARSE(KEYWORD-OFFSET:WS-CHARS)
               TO TOKEN(TOKEN-IX)
           MOVE WS-CHARS
               TO TOKEN-LENGTH(TOKEN-IX)
           MOVE KEYWORD-OFFSET TO CURRENT-TOKEN-OFFSET
      
           COMPUTE KEYWORD-OFFSET =
               KEYWORD-OFFSET + WS-CHARS
           END-COMPUTE
           .
      
       7310-FIND-NEXT-NON-SPACE-SRC.
           PERFORM 7313-FIND-NEXT-NON-SPACE
           IF SPACE-FOUND
               PERFORM 9460-COPY-ORIGINAL-LINE
               PERFORM 9250-READ-ORIGINAL-SOURCE
               MOVE ORIGINAL-UPPERED-LINE TO LINE-TO-PARSE
               MOVE KEYWORD-START TO KEYWORD-OFFSET
           END-IF
           .
      
       7312-FIND-NEXT-NON-SPACE-TEST.
           MOVE KEYWORD-END-TEST TO KEYWORD-END
           PERFORM 7313-FIND-NEXT-NON-SPACE
           IF SPACE-FOUND
               PERFORM 9350-READ-TEST-CASES
               MOVE TEST-CASE-UPPERED-LINE TO LINE-TO-PARSE
               MOVE KEYWORD-START TO KEYWORD-OFFSET
           END-IF
           MOVE KEYWORD-END-SRC TO KEYWORD-END
           .
      
       7313-FIND-NEXT-NON-SPACE.
           PERFORM VARYING KEYWORD-OFFSET FROM KEYWORD-OFFSET BY 1
             UNTIL KEYWORD-OFFSET IS GREATER THAN KEYWORD-END
                OR LINE-TO-PARSE(KEYWORD-OFFSET:1)
                     IS NOT EQUAL TO SPACE
           END-PERFORM
           IF LINE-TO-PARSE(KEYWORD-OFFSET:1) IS NOT EQUAL TO SPACE
           AND KEYWORD-OFFSET <= KEYWORD-END
               SET NON-SPACE-FOUND TO TRUE
           END-IF
           .
      
       7320-FIND-NEXT-SPACE.
           IF LINE-TO-PARSE(KEYWORD-OFFSET:1) IS EQUAL TO SPACE
               SET SPACE-FOUND TO TRUE
           ELSE
               SET NON-SPACE-FOUND TO TRUE
           END-IF
           PERFORM UNTIL SPACE-FOUND
               PERFORM VARYING KEYWORD-OFFSET FROM KEYWORD-OFFSET BY 1
                   UNTIL KEYWORD-OFFSET IS GREATER THAN KEYWORD-END
                      OR LINE-TO-PARSE(KEYWORD-OFFSET:1)
                         IS EQUAL TO SPACE
               END-PERFORM
               IF LINE-TO-PARSE(KEYWORD-OFFSET:1) IS EQUAL TO SPACE
                   SET SPACE-FOUND TO TRUE
               END-IF
           END-PERFORM
           .
      
       7400-MATCH-KEYWORD.
      *****************************************************************
      * Look for a specific keyword on the current input line.
      *****************************************************************
           SET NOT-IN-SINGLE-QUOTE TO TRUE
           SET NOT-IN-DOUBLE-QUOTE TO TRUE
      
           PERFORM VARYING KEYWORD-OFFSET FROM KEYWORD-OFFSET BY 1
                   UNTIL KEYWORD-OFFSET IS GREATER THAN
                         KEYWORD-END
                      OR KEYWORD-MATCHED
      
             EVALUATE TRUE
               WHEN LINE-TO-PARSE(KEYWORD-OFFSET:1) = '"'
                AND IN-DOUBLE-QUOTE
                  SET NOT-IN-DOUBLE-QUOTE TO TRUE
               WHEN LINE-TO-PARSE(KEYWORD-OFFSET:1) = "'"
                AND IN-SINGLE-QUOTE
                  SET NOT-IN-SINGLE-QUOTE TO TRUE
               WHEN NOT (IN-SINGLE-QUOTE OR IN-DOUBLE-QUOTE )
                 EVALUATE LINE-TO-PARSE(KEYWORD-OFFSET:1)
                   WHEN "'" SET IN-SINGLE-QUOTE TO TRUE
                   WHEN '"' SET IN-DOUBLE-QUOTE TO TRUE
                   WHEN OTHER
                     IF KEYWORD-TO-MATCH IS EQUAL TO
                     LINE-TO-PARSE(KEYWORD-OFFSET:KEYWORD-MATCH-LENGTH)
                       SET KEYWORD-MATCHED TO TRUE
                     END-IF
                 END-EVALUATE
             END-EVALUATE
      
           END-PERFORM
           .
      
       7450-FIND-KEYWORD-BEGIN.
      *****************************************************************
      * Position pointers for first keyword/token match on a line.
      *****************************************************************
           PERFORM VARYING KEYWORD-BEGIN FROM KEYWORD-OFFSET BY 1
                   UNTIL KEYWORD-BEGIN IS GREATER THAN
                         KEYWORD-END
                      OR LINE-TO-PARSE(KEYWORD-BEGIN:1) NOT = SPACES
           END-PERFORM
           .
      
       7500-INIT-KEYWORD-SEARCH.
      *****************************************************************
      * Position pointers for first keyword/token search on a line.
      *****************************************************************
           SET KEYWORD-NOT-MATCHED TO TRUE
           MOVE KEYWORD-START TO KEYWORD-OFFSET
           MOVE KEYWORD-END TO KEYWORD-SEARCH-LIMIT
           .
      
       7600-ADJUST-OFFSETS.
      *****************************************************************
      * Position pointers for next keyword/token search.
      *****************************************************************
           COMPUTE KEYWORD-OFFSET =
               KEYWORD-OFFSET + KEYWORD-LENGTH(KEYWORD-IX) - 1
           END-COMPUTE
      
           COMPUTE KEYWORD-SEARCH-LIMIT =
               KEYWORD-END - KEYWORD-OFFSET
           END-COMPUTE
           .
      
       7640-SET-SAVE-ORIG-LINES.
      *****************************************************************
      * Initialize WORKING-STORAGE to start saving lines from the
      * ORIGINAL-SOURCE file.
      *****************************************************************
           SET SAVE-ORIGINAL-LINES TO TRUE
           MOVE SPACES TO SAVED-LINES
           SET SAVED-LINE-IX TO 1
           MOVE 1 TO TALLY-FOR-SAVED-LINES
           .
      
       7650-SAVE-ORIG-LINE.
      *****************************************************************
      * Save the current line from ORIGINAL-SOURCE because we don't
      * know what to do with it yet.
      *****************************************************************
           MOVE ORIGINAL-LINE TO SAVED-LINE(SAVED-LINE-IX)
           SET SAVED-LINE-IX UP BY 1
           ADD 1 TO TALLY-FOR-SAVED-LINES
           .
      
       7700-PERIOD-ON-THIS-LINE.
      *****************************************************************
      * See if there is a period on the current input line.
      *****************************************************************
           MOVE '.' TO KEYWORD-TO-MATCH
           PERFORM 7400-MATCH-KEYWORD
           IF KEYWORD-MATCHED
             SET END-OF-STATEMENT TO TRUE
           ELSE
             SET NOT-END-OF-STATEMENT TO TRUE
           END-IF
           .
      
       7800-STRIP-PERIOD.
      *****************************************************************
      * Remove period-space from the current token.
      *****************************************************************
           MOVE ZERO TO WS-COUNT
           INSPECT TOKEN(TOKEN-IX) TALLYING WS-COUNT FOR ALL '.'
           IF WS-COUNT IS GREATER THAN ZERO
             SET TOKEN-HAS-PERIOD(TOKEN-IX) TO TRUE
             INSPECT TOKEN(TOKEN-IX) REPLACING ALL '. ' BY '  '
           ELSE
             SET TOKEN-HAS-NO-PERIOD(TOKEN-IX) TO TRUE
           END-IF
           .
      
       7900-STRIP-QUOTES.
      *****************************************************************
      * Remove single and double quotes from the current token.
      *****************************************************************
           IF TOKEN(TOKEN-IX)(1:1) IS EQUAL TO '"' OR "'"
               INSPECT TOKEN(TOKEN-IX) REPLACING ALL '"' BY SPACE
               INSPECT TOKEN(TOKEN-IX) REPLACING ALL "'" BY SPACE
               MOVE TOKEN(TOKEN-IX)(2:79) TO TOKEN(TOKEN-IX)(1:79)
           END-IF
           .
      
       7950-ENCLOSE-IN-QUOTES.
      *****************************************************************
      * Enclose value in single quotes.
      *****************************************************************
           PERFORM 7960-FIND-LENGTH-OF-RAW-VALUE
           MOVE SPACES TO QUOTED-VALUE
           STRING
               SINGLE-QUOTE          DELIMITED BY SIZE
               RAW-VALUE(1:TEMP-SUB) DELIMITED BY SIZE
               SINGLE-QUOTE          DELIMITED BY SIZE
               INTO QUOTED-VALUE
           END-STRING
           .
      
       7960-FIND-LENGTH-OF-RAW-VALUE.
      *****************************************************************
      * Find the length of the value in RAW-VALUE including any
      * embedded spaces.
      *****************************************************************
           PERFORM VARYING TEMP-SUB FROM LENGTH OF RAW-VALUE BY -1
                   UNTIL TEMP-SUB IS EQUAL TO ZERO
                      OR RAW-VALUE(TEMP-SUB:1) IS NOT EQUAL TO SPACE
           END-PERFORM
           .
      
       7980-CHECK-FOR-IGNORE.
      *****************************************************************
      * Check for keywords to be ignored in the current context.
      * For example: Ignore EXPECT keyword when processing a
      * TESTSUITE specification.
      *****************************************************************
           SET STRING-NOT-FOUND TO TRUE
           PERFORM VARYING IGNORE-IX FROM 1 BY 1
                   UNTIL IGNORE-IX IS GREATER THAN IGNORE-MAX
                      OR STRING-FOUND
               MOVE KEYWORD-TO-IGNORE(IGNORE-IX)
                   TO SEARCH-STRING
               MOVE KEYWORD-TO-IGNORE-LENGTH(IGNORE-IX)
                   TO SEARCH-STRING-LENGTH
               PERFORM 7990-FIND-STRING
           END-PERFORM
           .
      
       7990-FIND-STRING.
      *****************************************************************
      * If SEARCH-STRING exists anywhere in TARGET-STRING, set
      * STRING-FOUND to TRUE.
      *****************************************************************
           SET STRING-NOT-FOUND TO TRUE
           COMPUTE INSPECT-MAX =
               TARGET-STRING-LENGTH - SEARCH-STRING-LENGTH
           END-COMPUTE
           IF SEARCH-STRING-LENGTH IS GREATER THAN ZERO
           AND TARGET-STRING-LENGTH IS GREATER THAN ZERO
             IF TARGET-STRING-LENGTH
                   IS LESS THAN OR EQUAL TO SEARCH-STRING-LENGTH
               IF TARGET-STRING(1:TARGET-STRING-LENGTH) IS EQUAL TO
                  SEARCH-STRING(1:TARGET-STRING-LENGTH)
                   SET STRING-FOUND TO TRUE
               END-IF
             ELSE
               IF INSPECT-MAX IS GREATER THAN ZERO
                 PERFORM VARYING INSPECT-SUB FROM 1 BY 1
                       UNTIL INSPECT-SUB IS GREATER THAN INSPECT-MAX
                       OR STRING-FOUND
                   IF TARGET-STRING(INSPECT-SUB:SEARCH-STRING-LENGTH)
                       IS EQUAL TO SEARCH-STRING(1:SEARCH-STRING-LENGTH)
                       SET STRING-FOUND TO TRUE
                   END-IF
                 END-PERFORM
               END-IF
             END-IF
           END-IF
           .
      
      *****************************************************************
      * 8xxx - Initialization
      * 8100 - Load configuration settings
      * 8200 - Initialize MOCK statement handling
      *****************************************************************
      
       8000-INITIALIZE.
           PERFORM 8100-LOAD-CONFIG-SETTINGS
           PERFORM 8200-INITIALIZE-MOCK-HANDLING
           SET NOT-BEYOND-PROCEDURE-HEADER TO TRUE
           SET NO-SAVE-ORIGINAL-LINES TO TRUE
           .
      
       8100-LOAD-CONFIG-SETTINGS.
           PERFORM 9110-OPEN-UNIT-TEST-CONFIG
           PERFORM 9150-READ-UNIT-TEST-CONFIG
           MOVE COPYBOOK-NAME TO WS-WORKING-STORAGE-COPY
           PERFORM 9150-READ-UNIT-TEST-CONFIG
           MOVE COPYBOOK-NAME TO WS-PROCEDURE-COPY
           PERFORM 9190-CLOSE-UNIT-TEST-CONFIG
           .
      
       8200-INITIALIZE-MOCK-HANDLING.
           INITIALIZE MOCKS
           SET NO-CHECK-FOR-MOCKABLES TO TRUE
           MOVE ZERO TO MOCK-COUNT
           SET MOCK-IX TO 1
           SET FILE-IX TO 1
           MOVE SPACES TO INITIALIZATION-STATEMENTS
           SET INITIALIZATION-IX TO 1
           SET INITIALIZE-MOCK-IX TO 1
           .
      
      *****************************************************************
      * 9xxx - Input/Output
      *     91xx - Configuration file (input)
      *     92xx - Original source file (input)
      *     93xx - Test cases file (input)
      *     94xx - Test source file (output)
      * 9x10 - OPEN
      * 9x50 - READ / WRITE
      * 9x90 - CLOSE
      * 9xxx - other
      *****************************************************************
      
      *****************************************************************
      * 91xx - UNIT-TEST-CONFIG file I/O routines.
      *****************************************************************
       9110-OPEN-UNIT-TEST-CONFIG.
           OPEN INPUT UNIT-TEST-CONFIG
           EVALUATE TRUE
               WHEN UNIT-TEST-CONFIG-STATUS-OK
                    CONTINUE
               WHEN UNIT-TEST-CONFIG-NOT-FOUND
                    DISPLAY THIS-PROGRAM MSG-CONFIG-FILE-NOT-FOUND
               WHEN UNIT-TEST-CONFIG-NOT-READABLE
                    DISPLAY THIS-PROGRAM MSG-CONFIG-FILE-UNREADABLE
               WHEN OTHER
                    MOVE UNIT-TEST-CONFIG-STATUS TO
                         MSG-CONFIG-FILE-STATUS
                    MOVE OPEN-VERB TO MSG-CONFIG-FILE-OPERATION
                    DISPLAY THIS-PROGRAM MSG-CONFIG-FILE-ERROR
           END-EVALUATE
           .
      
       9150-READ-UNIT-TEST-CONFIG.
           READ UNIT-TEST-CONFIG
           IF UNIT-TEST-CONFIG-STATUS-OK OR UNIT-TEST-CONFIG-END-OF-FILE
               CONTINUE
           ELSE
               MOVE UNIT-TEST-CONFIG-STATUS TO MSG-CONFIG-FILE-STATUS
               MOVE READ-VERB TO MSG-CONFIG-FILE-OPERATION
               DISPLAY THIS-PROGRAM MSG-CONFIG-FILE-ERROR
           END-IF
           .
      
       9190-CLOSE-UNIT-TEST-CONFIG.
           CLOSE UNIT-TEST-CONFIG
           .
      
      *****************************************************************
      * 92xx - ORIGINAL-SOURCE file I/O routines.
      *****************************************************************
       9210-OPEN-ORIGINAL-SOURCE.
           OPEN INPUT ORIGINAL-SOURCE
           EVALUATE TRUE
               WHEN ORIGINAL-SOURCE-STATUS-OK
                    CONTINUE
               WHEN ORIGINAL-SOURCE-NOT-FOUND
                    DISPLAY THIS-PROGRAM MSG-ORIG-SOURCE-NOT-FOUND
               WHEN ORIGINAL-SOURCE-NOT-READABLE
                    DISPLAY THIS-PROGRAM MSG-ORIG-SOURCE-UNREADABLE
               WHEN OTHER
                    MOVE ORIGINAL-SOURCE-STATUS
                         TO MSG-ORIG-SOURCE-STATUS
                    MOVE OPEN-VERB TO MSG-ORIG-SOURCE-OPERATION
                    MOVE CURRENT-PHASE TO MSG-ORIG-SOURCE-PHASE
                                          IN MSG-ORIG-SOURCE-ERROR
                    DISPLAY THIS-PROGRAM MSG-ORIG-SOURCE-ERROR
           END-EVALUATE
           .
      
       9240-READ-AND-COPY-ORIG-SOURCE.
           PERFORM 9250-READ-ORIGINAL-SOURCE
           IF ORIGINAL-SOURCE-STATUS-OK
               PERFORM 9460-COPY-ORIGINAL-LINE
           END-IF
           .
      
       9250-READ-ORIGINAL-SOURCE.
           READ ORIGINAL-SOURCE
           IF ORIGINAL-LINE IS EQUAL TO LOW-VALUES
               MOVE SPACES TO ORIGINAL-LINE
           END-IF
           PERFORM 9240-UPPER-CASE-ORIGINAL-LINE
           EVALUATE TRUE
               WHEN ORIGINAL-SOURCE-STATUS-OK
                    IF SAVE-ORIGINAL-LINES
                      PERFORM 7650-SAVE-ORIG-LINE
                    END-IF
               WHEN ORIGINAL-SOURCE-END-OF-FILE
                       WRITE TEST-LINE FROM PARAGRAPH-END-MARKER
               WHEN OTHER
                    MOVE ORIGINAL-SOURCE-STATUS
                         TO MSG-ORIG-SOURCE-STATUS
                    MOVE READ-VERB TO MSG-ORIG-SOURCE-OPERATION
                    MOVE CURRENT-PHASE TO MSG-ORIG-SOURCE-PHASE
                                          IN MSG-ORIG-SOURCE-ERROR
                    DISPLAY MSG-ORIG-SOURCE-ERROR
                    PERFORM 9290-CLOSE-ORIGINAL-SOURCE
                    PERFORM 9495-CLOSE-TEST-SOURCE
                    GOBACK
           END-EVALUATE
           .
      
       9240-UPPER-CASE-ORIGINAL-LINE.
           MOVE FUNCTION UPPER-CASE(ORIGINAL-LINE)
             TO ORIGINAL-UPPERED-LINE
           .
      
       9290-CLOSE-ORIGINAL-SOURCE.
           CLOSE ORIGINAL-SOURCE
           .
      
      *****************************************************************
      * 93xx - TEST-CASES file I/O routines.
      *****************************************************************
       9310-OPEN-TEST-CASES.
           OPEN INPUT TEST-CASES
           EVALUATE TRUE
               WHEN TEST-CASES-STATUS-OK
                    CONTINUE
               WHEN TEST-CASES-FILE-NOT-FOUND
                    DISPLAY THIS-PROGRAM MSG-TEST-CASES-NOT-FOUND
               WHEN TEST-CASES-NOT-READABLE
                    DISPLAY THIS-PROGRAM MSG-TEST-CASES-UNREADABLE
               WHEN OTHER
                    MOVE TEST-CASES-STATUS TO MSG-TEST-CASES-STATUS
                    MOVE OPEN-VERB TO MSG-TEST-CASES-OPERATION
                    DISPLAY MSG-TEST-CASES-ERROR
           END-EVALUATE
           .
      
       9350-READ-TEST-CASES.
           IF NOT END-OF-TEST-CASES
               IF NO-SUPPRESS-TEST-CASE-READ
                   PERFORM 9360-NEXT-TEST-CASE-RECORD
               ELSE
                   SET NO-SUPPRESS-TEST-CASE-READ TO TRUE
               END-IF
           END-IF
           .
      
       9360-NEXT-TEST-CASE-RECORD.
           PERFORM WITH TEST AFTER
                   UNTIL NOT TEST-CASE-COMMENT
               READ TEST-CASES
               IF TEST-CASES-END-OF-FILE
                   SET END-OF-TEST-CASES TO TRUE
                   CONTINUE
               END-IF
           END-PERFORM
           IF TEST-CASES-STATUS-OK OR END-OF-TEST-CASES
               MOVE FUNCTION UPPER-CASE(TEST-CASE-LINE)
                 TO TEST-CASE-UPPERED-LINE
           ELSE
               MOVE TEST-CASES-STATUS TO MSG-TEST-CASES-STATUS
               MOVE READ-VERB TO MSG-TEST-CASES-OPERATION
               DISPLAY MSG-TEST-CASES-ERROR
           END-IF
           .
      
       9390-CLOSE-TEST-CASES.
           CLOSE TEST-CASES
           .
      
      *****************************************************************
      * 94xx - TEST-SOURCE file I/O routines.
      *****************************************************************
      
       9410-OPEN-TEST-SOURCE.
           OPEN OUTPUT TEST-SOURCE
           EVALUATE TRUE
               WHEN TEST-SOURCE-STATUS-OK
                    CONTINUE
               WHEN TEST-SOURCE-FILE-NOT-FOUND
                    DISPLAY THIS-PROGRAM MSG-TEST-SOURCE-NOT-FOUND
               WHEN TEST-SOURCE-NOT-WRITABLE
                    DISPLAY THIS-PROGRAM MSG-TEST-SOURCE-UNWRITABLE
               WHEN OTHER
                    MOVE TEST-SOURCE-STATUS TO MSG-TEST-SOURCE-STATUS
                    MOVE WRITE-VERB TO MSG-TEST-SOURCE-OPERATION
                    DISPLAY MSG-TEST-SOURCE-ERROR
           END-EVALUATE
           .
            
       9415-WRITE-ZUTBEGIN-LINE.
           MOVE ZUT-MARKERVALUE-BEGIN TO TEST-LINE
           PERFORM 9420-WRITE-TEST-LINE
           .
      
       9417-WRITE-ZUTEND-LINE.
           MOVE ZUT-MARKERVALUE-END TO TEST-LINE
           PERFORM 9420-WRITE-TEST-LINE
           .
      
       9420-WRITE-TEST-LINE.
           IF SAVE-ORIGINAL-LINES
               CONTINUE
           ELSE
             PERFORM 9425-WRITE-TEST-LINE
           END-IF
           .
      
       9425-WRITE-TEST-LINE.
           IF TEST-LINE IS EQUAL TO LOW-VALUES
             MOVE SPACES TO TEST-LINE
           END-IF
      
           IF BEYOND-PROCEDURE-HEADER
             PERFORM 9430-CHECK-FOR-COPY-STMT
             IF IS-FIRST-TOKEN
               CONTINUE
             ELSE
      * Not a paragraph header
               IF TEST-LINE(8:4) IS EQUAL TO SPACES
                 MOVE 1  TO KEYWORD-OFFSET
                 MOVE 72 TO KEYWORD-END
                 MOVE TEST-LINE TO LINE-TO-PARSE
                 PERFORM 7450-FIND-KEYWORD-BEGIN
                 IF LINE-TO-PARSE(KEYWORD-BEGIN:1) = '.'
                 AND LINE-TO-PARSE(KEYWORD-BEGIN + 1:) = SPACES
                   EXIT PARAGRAPH
                 END-IF
               ELSE
      * A paragraph header
                 MOVE ZERO TO WS-COUNT
                 INSPECT TEST-LINE(8:5)
      -                  TALLYING WS-COUNT FOR ALL '*>'
                 IF TEST-LINE(7:1) = '*'
                    OR WS-COUNT > 0
                   CONTINUE
                 ELSE
                   MOVE TEST-LINE TO TEMP-STATEMENT
                   WRITE TEST-LINE FROM PARAGRAPH-END-MARKER
                   MOVE TEMP-STATEMENT TO TEST-LINE
                 END-IF
               END-IF
             END-IF
           END-IF
           IF TEST-LINE(8:13) IS EQUAL TO 'PROCEDURE DIV'
             SET BEYOND-PROCEDURE-HEADER TO TRUE
           END-IF
      
           MOVE TEST-LINE TO TEMP-STATEMENT
           WRITE TEST-LINE
           MOVE TEMP-STATEMENT TO TEST-LINE
      
           EVALUATE TRUE
               WHEN TEST-SOURCE-STATUS-OK
                    CONTINUE
               WHEN OTHER
                    MOVE TEST-SOURCE-STATUS TO MSG-TEST-SOURCE-STATUS
                    MOVE WRITE-VERB TO MSG-TEST-SOURCE-OPERATION
                    DISPLAY MSG-TEST-SOURCE-ERROR
           END-EVALUATE
           .
      
       9430-CHECK-FOR-COPY-STMT.
           SET NOT-FIRST-TOKEN TO TRUE
           PERFORM VARYING TEMP-SUB FROM 11 BY 1
                   UNTIL TEMP-SUB IS GREATER THAN KEYWORD-END
               IF TEST-LINE(TEMP-SUB + 1:1) IS NOT EQUAL TO SPACE
                   IF TEST-LINE(TEMP-SUB:6)
                          IS EQUAL TO COPY-KEYWORD
                      SET IS-FIRST-TOKEN TO TRUE
                   END-IF
                   MOVE KEYWORD-END TO TEMP-SUB
               END-IF
           END-PERFORM
           .
      
       9440-WRITE-GENERATED-STMTS.
      *****************************************************************
      * Insert test code resulting from parsing unit test keywords.
      *****************************************************************
           PERFORM VARYING STATEMENT-IX FROM 1 BY 1
               UNTIL STATEMENT-IX > 250
                  OR GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                     IS EQUAL TO SPACES
      
               MOVE GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                   TO RAW-VALUE
               PERFORM 7960-FIND-LENGTH-OF-RAW-VALUE
      
      * multiple continued lines? PERFORM UNTIL ???
                   IF TEMP-SUB IS GREATER THAN 72
                       MOVE RAW-VALUE-1 TO TEST-LINE
                       PERFORM 9420-WRITE-TEST-LINE
                       STRING
                           STATEMENT-CONTINUATION DELIMITED BY SIZE
                           RAW-VALUE-2            DELIMITED BY SIZE
                           INTO RAW-VALUE
                       END-STRING
                       PERFORM 7960-FIND-LENGTH-OF-RAW-VALUE
      
                       MOVE RAW-VALUE-1 TO TEST-LINE
                       PERFORM 9420-WRITE-TEST-LINE
                   ELSE
                       MOVE RAW-VALUE TO TEST-LINE
                       PERFORM 9420-WRITE-TEST-LINE
                   END-IF
      *         END-PERFORM
           END-PERFORM
           INITIALIZE GENERATED-SOURCE-STATEMENTS
           .
      
       9450-WRITE-COPY-LINE.
      *****************************************************************
      * Insert a line into TEST-SOURCE containing a COPY statement.
      *****************************************************************
           MOVE WS-COPY-LINE TO TEST-LINE
           PERFORM 9420-WRITE-TEST-LINE
           .
      
       9460-COPY-ORIGINAL-LINE.
      *****************************************************************
      * Copy a line from ORIGINAL-SOURCE to TEST-SOURCE unchanged.
      *****************************************************************
           MOVE ORIGINAL-LINE TO TEST-LINE
           PERFORM 9420-WRITE-TEST-LINE
           .
      
       9465-COMMENT-ORIGINAL-LINE.
      *****************************************************************
      * Comment the current ORIGINAL-SOURCE line and copy it.
      *****************************************************************
           IF ORIGINAL-LINE(7:1) NOT = '*'         
             MOVE ORIGINAL-LINE TO LINE-TO-PARSE   
             PERFORM 7500-INIT-KEYWORD-SEARCH      
             MOVE '. ' TO KEYWORD-TO-MATCH         
             MOVE 2 TO KEYWORD-MATCH-LENGTH        
             PERFORM 7400-MATCH-KEYWORD            
             IF KEYWORD-NOT-MATCHED                
               MOVE COMMENT-MARKER TO ORIGINAL-LINE(7:5)
             END-IF
           END-IF
           MOVE ORIGINAL-LINE TO TEST-LINE
           PERFORM 9420-WRITE-TEST-LINE
           .
      
       9470-WRITE-BEFORE-PARAGRAPH.
      *****************************************************************
      * Write the UT-BEFORE paragraph into the TEST-SOURCE file.
      * This contains the statements specified in the BEFORE-EACH
      * block of the TEST-CASES file, if any.
      *****************************************************************
           MOVE BEFORE-PARAGRAPH-HEADER TO TEST-LINE
           PERFORM 9420-WRITE-TEST-LINE
           SET BEFORE-EACH-MAX TO BEFORE-EACH-IX
           PERFORM VARYING BEFORE-EACH-IX FROM 1 BY 1
                     UNTIL BEFORE-EACH-IX > BEFORE-EACH-MAX
                        OR BEFORE-EACH-IX > MAX-BEFORE-EACH-STATEMENTS
               MOVE BEFORE-EACH-STATEMENT(BEFORE-EACH-IX) TO TEST-LINE
               PERFORM 9420-WRITE-TEST-LINE
           END-PERFORM
           MOVE PARAGRAPH-EMPTY-PREVENTER TO TEST-LINE
           PERFORM 9420-WRITE-TEST-LINE
           MOVE PARAGRAPH-END-MARKER TO TEST-LINE
           PERFORM 9420-WRITE-TEST-LINE
           .
      
       9480-WRITE-AFTER-PARAGRAPH.
      *****************************************************************
      * Write the UT-AFTER paragraph into the TEST-SOURCE file.
      * This contains the statements specified in the AFTER-EACH
      * block of the TEST-CASES file, if any.
      *****************************************************************
           MOVE AFTER-PARAGRAPH-HEADER TO TEST-LINE
           PERFORM 9420-WRITE-TEST-LINE
           SET AFTER-EACH-MAX TO AFTER-EACH-IX
           PERFORM VARYING AFTER-EACH-IX FROM 1 BY 1
                     UNTIL AFTER-EACH-IX > AFTER-EACH-MAX
                        OR AFTER-EACH-IX > MAX-AFTER-EACH-STATEMENTS
               MOVE AFTER-EACH-STATEMENT(AFTER-EACH-IX) TO TEST-LINE
               PERFORM 9420-WRITE-TEST-LINE
           END-PERFORM
           MOVE PARAGRAPH-EMPTY-PREVENTER TO TEST-LINE
           PERFORM 9420-WRITE-TEST-LINE
           MOVE PARAGRAPH-END-MARKER TO TEST-LINE
           PERFORM 9420-WRITE-TEST-LINE
           .
      
       9485-WRITE-INIT-PARAGRAPH.
      *****************************************************************
      * Write the UT-INITIALIZE paragraph into the TEST-SOURCE file.
      * This contains the statements generated as a result of parsing
      * the SELECT and FD entries in the program under test.
      *****************************************************************
           MOVE INIT-PARAGRAPH-HEADER TO TEST-LINE
           PERFORM 9420-WRITE-TEST-LINE
           MOVE '           MOVE SPACES TO UT-FILE-INFORMATION'
               TO TEST-LINE
           PERFORM 9420-WRITE-TEST-LINE
      
           PERFORM VARYING INITIALIZATION-IX FROM 1 BY 1
                   UNTIL INITIALIZATION-IX > MAX-INITIALIZATION-STATEMENTS
                   OR INITIALIZATION-STATEMENT(INITIALIZATION-IX)
                   IS EQUAL TO SPACES
               MOVE INITIALIZATION-STATEMENT(INITIALIZATION-IX)
                   TO TEST-LINE
               PERFORM 9420-WRITE-TEST-LINE
           END-PERFORM
           MOVE PARAGRAPH-END-MARKER TO TEST-LINE
           PERFORM 9420-WRITE-TEST-LINE
      
           MOVE END-PARAGRAPH-HEADER TO TEST-LINE
           PERFORM 9420-WRITE-TEST-LINE
           MOVE PARAGRAPH-END-MARKER TO TEST-LINE
           PERFORM 9420-WRITE-TEST-LINE
           .
      
       9490-WRITE-INIT-MOCK-PARAGRAPH.
      *****************************************************************
      * Write the UT-INITIALIZE paragraph into the TEST-SOURCE file.
      * This contains the statements generated as a result of parsing
      * the SELECT and FD entries in the program under test.
      *****************************************************************
           MOVE INIT-MOCK-PARAGRAPH-HEADER TO TEST-LINE
           PERFORM 9420-WRITE-TEST-LINE
           SET MAX-GENERATED-STATEMENTS
            TO INITIALIZE-MOCK-IX
      
           PERFORM VARYING INITIALIZE-MOCK-IX FROM 1 BY 1
                   UNTIL INITIALIZE-MOCK-IX                        
                      IS GREATER THAN INITIALIZE-MOCK-STATEMENT-MAX
                   OR INITIALIZE-MOCK-IX = MAX-GENERATED-STATEMENTS
               MOVE INITIALIZE-MOCK-STATEMENT(INITIALIZE-MOCK-IX)
                   TO TEST-LINE
               PERFORM 9420-WRITE-TEST-LINE
           END-PERFORM
      
           MOVE PARAGRAPH-EMPTY-PREVENTER TO TEST-LINE
           PERFORM 9420-WRITE-TEST-LINE
           MOVE PARAGRAPH-END-MARKER TO TEST-LINE
           PERFORM 9420-WRITE-TEST-LINE
           .
      
       9495-CLOSE-TEST-SOURCE.
           CLOSE TEST-SOURCE
           .
      
       9999-END.
           CONTINUE
           .
      

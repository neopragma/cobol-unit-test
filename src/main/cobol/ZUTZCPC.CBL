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
               ASSIGN TO EXTERNAL SRCPRG
               FILE STATUS IS ORIGINAL-SOURCE-STATUS
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT UNIT-TEST-CONFIG
               ASSIGN TO EXTERNAL UTSTCFG
               FILE STATUS IS UNIT-TEST-CONFIG-STATUS
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TEST-SOURCE
               ASSIGN TO EXTERNAL TESTPRG
               FILE STATUS IS TEST-SOURCE-STATUS
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TEST-CASES ASSIGN TO EXTERNAL UTESTS
               FILE STATUS IS TEST-CASES-STATUS
               ORGANIZATION IS LINE SEQUENTIAL.    
       DATA DIVISION.
       FILE SECTION.
       FD  UNIT-TEST-CONFIG
           DATA RECORD IS UNIT-TEST-CONFIG-REC.
       01  UNIT-TEST-CONFIG-REC.
           05  COPYBOOK-NAME   PIC X(12).
       FD  ORIGINAL-SOURCE
           DATA RECORD IS ORIGINAL-LINE.
       01  ORIGINAL-LINE.
           05  FILLER                    PIC X(07).
           05  SECTION-HEADER            PIC X(10).
               88  DATA-DIVISION-HEADER      VALUE 'DATA DIVIS'.
               88  FILE-CONTROL-HEADER       VALUE 'FILE-CONTR'.
               88  FILE-SECTION-HEADER       VALUE 'FILE SECTI'.
               88  WORKING-STORAGE-HEADER    VALUE 'WORKING-ST'.
               88  PROCEDURE-DIVISION-HEADER VALUE 'PROCEDURE '.
           05  FILLER REDEFINES SECTION-HEADER.
               10  FILLER                PIC X(06).
                   88  SELECT-STATEMENT      VALUE 'SELECT'.
               10  FILLER                PIC X(04).
           05  FILLER REDEFINES SECTION-HEADER.
               10  FILLER                PIC X(03).
                   88  FD-STATEMENT          VALUE 'FD '.
               10  FILLER                PIC X(07).           
           05  FILLER                    PIC X(63).

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
           05  MAX-FILES                     PIC 9(02) VALUE 10.
       01  FILLER                            PIC X(01) VALUE SPACE.
           88  FILENAME-MATCHED              VALUE 'Y', FALSE SPACE.
       01  FILE-INFORMATION.
           05  FILE-INF OCCURS 10 INDEXED BY FILE-IX.
               10  INTERNAL-FILENAME         PIC X(31).
               10  RECORD-FIELD-NAME         PIC X(31).
               10  FILE-STATUS-FIELD-NAME    PIC X(31).

      *****************************************************************
      * Specifications for mocking file accesses, CICS commands, and
      * SQL commands. This table is populated based on MOCK keywords
      * found in the unit TEST-CASES file. The data are then used to
      * substitute fake code in the TEST-SOURCE file.
      *****************************************************************
       01  FILLER.
           05  MOCK-FILE-STATUS-WORK      PIC X(02).
           05  FILLER                     PIC X.
               88  MOCK-SYNTAX-ERROR      VALUE 'Y', FALSE SPACE.
           05  FILLER                     PIC X.
               88  MOCK-MATCHED           VALUE 'Y', FALSE SPACE.
           05  MAX-MOCKS                  PIC 9(02) VALUE 10.
       01  MOCKS.
           05  MOCK-TEST-COUNT            PIC 9(04).
           05  MOCK-LOOKUP.
               10  MOCK-LOOKUP-TYPE       PIC X(04).
               10  MOCK-LOOKUP-FILENAME   PIC X(31).
               10  MOCK-LOOKUP-OPERATION  PIC X(20).
           05  MOCK OCCURS 10 INDEXED BY MOCK-IX.
               10  MOCK-TYPE              PIC X(04).
                   88  MOCK-FILE          VALUE 'FILE', FALSE SPACES.
                   88  MOCK-CICS          VALUE 'CICS', FALSE SPACES.
                   88  MOCK-SQL           VALUE 'SQL',  FALSE SPACES.
               10  MOCK-RECORD            PIC X(2048).    
               10  MOCK-DATA              PIC X(146).
               10  MOCK-FILE-DATA REDEFINES MOCK-DATA.        
                   15  MOCK-FILENAME      PIC X(31).    
                   15  MOCK-OPERATION     PIC X(20).  
                   15  MOCK-FILE-STATUS   PIC X(02).
                   15  FILLER             PIC X(93).
               10  MOCK-CICS-DATA REDEFINES MOCK-DATA.
                   15  MOCK-CICS-RESOURCE PIC X(08).
                   15  MOCK-CICS-COMMAND  PIC X(30).
                   15  MOCK-CICS-RIDFLD   PIC X(100).    
                   15  MOCK-EIBRESP       PIC S9(08) COMP.
                   15  MOCK-EIBRESP2      PIC S9(08) COMP.
               10  MOCK-SQL-DATA REDEFINES MOCK-DATA.
                   15  MOCK-SQL-COMMAND   PIC X(20).
                   15  MOCK-SQL-CODE      PIC S9(08) COMP.
                   15  MOCK-SQL-STATE     PIC S9(08) COMP.
                   15  FILLER             PIC X(118).
       01  FILLER.            
           05  MAX-STATUS-MNEMONICS       PIC 9(02) VALUE 4.
           05  PREVIOUS-ENTRY             PIC 9(02) VALUE 0.
           05  TEMP-SUB                   PIC 9(02) VALUE 0.
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
                   
       01  TEMP-STATEMENT               PIC X(80).            
      *****************************************************************
      * Accumulate source statements that will be inserted into the 
      * TEST-SOURCE file. 
      *****************************************************************
       01  GENERATED-SOURCE-STATEMENTS.
           05  GENERATED-SOURCE-STATEMENT 
               OCCURS 100 
               INDEXED BY STATEMENT-IX   PIC X(80).
                   
      *****************************************************************
      * Accumulate source statements that will be inserted into the 
      * UT-INITIALIZE paragraph in the TEST-SOURCE file. 
      *****************************************************************
       01  INITIALIZATION-STATEMENTS.
           05  INITIALIZATION-STATEMENT 
               OCCURS 50 
               INDEXED BY INITIALIZATION-IX PIC X(80).

      *****************************************************************
      * Accumulate source statements that will be inserted into the 
      * UT-BEFORE paragraph in the TEST-SOURCE file. 
      *****************************************************************
       01  BEFORE-EACH-STATEMENTS.
           05  BEFORE-EACH-STATEMENT
               OCCURS 50
               INDEXED BY BEFORE-EACH-IX PIC X(80).        

      *****************************************************************
      * Accumulate source statements that will be inserted into the 
      * UT-AFTER paragraph in the TEST-SOURCE file. 
      *****************************************************************
       01  AFTER-EACH-STATEMENTS.
           05  AFTER-EACH-STATEMENT
               OCCURS 50
               INDEXED BY AFTER-EACH-IX PIC X(80).        

      *****************************************************************
      * Build source statements for the UT-MOCK-INFO table to be
      * inserted into WORKING-STORAGE SECTION of the TEST-SOURCE file.
      *****************************************************************
       01  MOCK-INFO-LINES.
           05  MOCK-INFO-LINE
               OCCURS 50
               INDEXED BY MOCK-INFO-IX PIC X(80).

      *****************************************************************
      * Build source statements for the UT-LOOKUP-MOCK paragraph to be
      * inserted into the PROCEDURE-DIVISION of TEST-SOURCE file.
      *****************************************************************
       01  LOOKUP-MOCK-STATEMENTS.
           05  LOOKUP-MOCK-STATEMENT
               OCCURS 50
               INDEXED BY LOOKUP-MOCK-IX PIC X(80).

      *****************************************************************
      * This is a work area used when tokenizing a line of input from
      * the ORIGINAL-SOURCE file.
      *****************************************************************
       01  TOKENS.
           05  TOKEN OCCURS 20 TIMES INDEXED BY TOKEN-IX PIC X(80).

      *****************************************************************
      * This area is used for parsing keywords out of input records.
      *****************************************************************
       01  FILLER.    
           05  RAW-VALUE              PIC X(80).
           05  QUOTED-VALUE           PIC X(80).
           05  LINE-TO-PARSE          PIC X(80).
           05  KEYWORD-MATCHED-INDICATOR PIC X(01) VALUE SPACE.
               88  KEYWORD-MATCHED              VALUE 'Y', FALSE SPACE.
           05  WS-CHARS               PIC 9(02) VALUE ZERO.  
           05  WS-NEXT                PIC 9(02) VALUE ZERO.
           05  WS-STRING-DELIMITER    PIC X(01) VALUE SPACE.  
           05  KEYWORD-TO-MATCH       PIC X(20) VALUE SPACES.
           05  KEYWORD-MATCH-LENGTH   PIC 9(02) VALUE ZERO.
           05  KEYWORD-OFFSET         PIC 9(02) VALUE 0.
           05  KEYWORD-SEARCH-LIMIT   PIC 9(02) VALUE 0.
           05  KEYWORD-END            PIC 9(02) VALUE 72.
           05  KEYWORD-START          PIC 9(02) VALUE 12.
           05  MAX-KEYWORDS           PIC 9(02) VALUE 7.
           05  KEYWORD-VALUES.
               10  FILLER PIC X(20) VALUE 'EXPECT'.
               10  FILLER PIC 9(02) VALUE 6.
               10  FILLER PIC X(20) VALUE 'TESTCASE'.
               10  FILLER PIC 9(02) VALUE 8.
               10  FILLER PIC X(20) VALUE 'TESTSUITE'.
               10  FILLER PIC 9(02) VALUE 9.
               10  FILLER PIC X(20) VALUE 'BEFORE-EACH'.
               10  FILLER PIC 9(02) VALUE 11.     
               10  FILLER PIC X(20) VALUE 'AFTER-EACH'.
               10  FILLER PIC 9(02) VALUE 10.     
               10  FILLER PIC X(20) VALUE 'MOCK'.
               10  FILLER PIC 9(02) VALUE 4.     
               10  FILLER PIC X(20) VALUE 'VERIFY'.
               10  FILLER PIC 9(02) VALUE 6.     
           05  KEYWORDS REDEFINES KEYWORD-VALUES.
               10  KEYWORD OCCURS 7 INDEXED BY KEYWORD-IX.
                   15  KEYWORD-VALUE      PIC X(20).
                   15  KEYWORD-LENGTH     PIC 9(02).

      *****************************************************************
      * Constants
      *****************************************************************
       01  FILLER.
           05  STATEMENT-OFFSET             PIC 9(02) VALUE 12.
           05  STATEMENT-LENGTH             PIC 9(02) VALUE 70.
           05  PROCEDURE-DIVISION-COPYBOOK  PIC X(08) VALUE 'ZUTZCPD'.
           05  FILE-KEYWORD                 PIC X(04) VALUE 'FILE'.
           05  HAPPENED-KEYWORD             PIC X(08) VALUE 'HAPPENED'.
           05  ONCE-KEYWORD                 PIC X(04) VALUE 'ONCE'.
           05  OPEN-VERB                    PIC X(04) VALUE 'OPEN'.
           05  READ-VERB                    PIC X(04) VALUE 'READ'.
           05  WRITE-VERB                   PIC X(05) VALUE 'WRITE'.
           05  MOVE-VERB                    PIC X(05) VALUE 'MOVE '.
           05  PERFORM-VERB                 PIC X(08) VALUE 'PERFORM '.
           05  TO-KEYWORD                   PIC X(04) VALUE ' TO '.
           05  ZERO-KEYWORD                 PIC X(06) VALUE ' ZERO '.
           05  SINGLE-QUOTE                 PIC X(01) VALUE "'".
           05  COMMENT-MARKER               PIC X(01) VALUE '*'.
           05  STATEMENT-SPACER             PIC X(11) VALUE SPACES.
           05  PARAGRAPH-END-MARKER         PIC X(12) 
                                            VALUE '           .'.
           05  BEFORE-PARAGRAPH-HEADER      PIC X(17) 
                                            VALUE '       UT-BEFORE.'.
           05  AFTER-PARAGRAPH-HEADER       PIC X(17) 
                                            VALUE '       UT-AFTER.'.
           05  INIT-PARAGRAPH-HEADER        PIC X(22) 
                                        VALUE '       UT-INITIALIZE.'.
           05  END-PARAGRAPH-HEADER         PIC X(22) 
                                        VALUE '       UT-END.'.
           05  INCR-TESTCASE-COUNT-STMT     PIC X(80) VALUE
               '           ADD 1 TO UT-TEST-CASE-COUNT'.
           05  SET-MOCK-STMT                PIC X(80) VALUE
               '           PERFORM UT-SET-MOCK'.    
           05  ENDIF-STMT                   PIC X(18) VALUE
               '           END-IF'.    

      *****************************************************************
      * File status codes
      *****************************************************************
       01  FILLER.
           05  UNIT-TEST-CONFIG-STATUS       PIC X(02) VALUE ZERO.
               88  UNIT-TEST-CONFIG-STATUS-OK         VALUE ZERO.
               88  UNIT-TEST-CONFIG-END-OF-FILE       VALUE 10.
               88  UNIT-TEST-CONFIG-NOT-FOUND         VALUE 35.
               88  UNIT-TEST-CONFIG-NOT-READABLE      VALUE 37.
           05  ORIGINAL-SOURCE-STATUS        PIC 9(2) VALUE ZERO.
               88  ORIGINAL-SOURCE-STATUS-OK          VALUE ZERO.
               88  ORIGINAL-SOURCE-END-OF-FILE        VALUE 10.
               88  ORIGINAL-SOURCE-NOT-FOUND          VALUE 35.
               88  ORIGINAL-SOURCE-NOT-READABLE       VALUE 37.
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
               88  END-OF-FILE                 VALUE 'Y'.   
           05  FILLER                    PIC X VALUE SPACE.
               88  NON-SPACE-FOUND             VALUE 'Y', FALSE SPACE.
           05  FILLER                    PIC X VALUE SPACE.
               88  END-OF-STATEMENT            VALUE 'Y', FALSE SPACE.  
           05  FILLER                    PIC X VALUE SPACE.
               88  NOT-END-OF-TEST-CASES       VALUE SPACE.
               88  END-OF-TEST-CASES           VALUE 'Y'.   
           05  COPY-SOURCE-INDICATOR     PIC X VALUE SPACE.
               88  SUPPRESS-COPY-SOURCE        VALUE 'Y', FALSE SPACE.   
           05  FILLER                    PIC X VALUE SPACE.
               88  GENERATE-PERFORM-AFTER      VALUE 'Y'.    
           05  FILLER                    PIC X VALUE SPACE.
               88  SCANNING-TEST-CASES         VALUE 'Y', FALSE SPACE.    
           05  FILLER                    PIC X VALUE SPACE.
               88  SCANNING-ORIGINAL-SOURCE    VALUE 'Y', FALSE SPACE.    
           05  FILLER                    PIC X VALUE SPACE.
               88  CHECK-FOR-IO-STATEMENTS     VALUE 'Y', FALSE SPACE.    

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
           05  TEXT-UNIT-TEST-CONFIG-FILE      PIC X(27)
               VALUE 'CONFIGURATION FILE UTSTCFG '.
           05  TEXT-ORIGINAL-SOURCE-FILE       PIC X(21)
               VALUE 'ORIGINAL SOURCE FILE '.
           05  TEXT-TEST-CASES-FILE            PIC X(23)
               VALUE 'TEST CASES FILE UTESTS '.
           05  TEXT-TEST-SOURCE-FILE           PIC X(24)
               VALUE 'TEST SOURCE OUTPUT FILE '.
           05  TEXT-NOT-FOUND                  PIC X(09)
               VALUE 'NOT FOUND'.    
           05  TEXT-UNREADABLE                 PIC X(26)
               VALUE 'CAN''T BE OPENED FOR INPUT'.    
           05  TEXT-UNWRITABLE                 PIC X(27)
               VALUE 'CAN''T BE OPENED FOR OUTPUT'.    
           05  TEXT-IO-ERROR.
               10  FILLER                      PIC X(23)
               VALUE 'I/O ERROR, FILE STATUS '.
               10  MSG-FILE-STATUS             PIC X(02) VALUE '??'.
           05  TEXT-ON                         PIC X(04) VALUE ' ON '.

           05  ERROR-MESSAGE-LINE              PIC X(80) VALUE SPACES.

           05  MSG-ORIGINAL-SOURCE-NOT-FOUND  PIC X(80) VALUE
               'ORIGINAL SOURCE FILE NOT FOUND'.
           05  MSG-ORIGINAL-SOURCE-UNREADABLE PIC X(80) VALUE
               'ORIGINAL SOURCE FILE CAN''T BE OPENED FOR INPUT'.
           05  MSG-ORIGINAL-SOURCE-FILE-ERROR.
               10  FILLER                   PIC X(34) VALUE
                   'ORIGINAL SOURCE FILE STATUS '.
               10  MSG-ORIGINAL-SOURCE-STATUS    PIC 9(02).
               10  FILLER                   PIC X(04) VALUE ' ON '.
               10  MSG-ORIGINAL-SOURCE-OPERATION PIC X(04).
               10  FILLER                   PIC X(36) VALUE SPACES.

           05  MSG-TEST-CASES-NOT-FOUND  PIC X(80) VALUE
               'UNIT TEST COPY FILE UTESTS NOT FOUND'.
           05  MSG-TEST-CASES-UNREADABLE PIC X(80) VALUE
               'UNIT TEST COPY FILE UTESTS CAN''T BE OPENED FOR INPUT'.
           05  MSG-TEST-CASES-FILE-ERROR.
               10  FILLER                   PIC X(34) VALUE
                   'UNIT TEST COPY FILE UTESTS STATUS '.
               10  MSG-TEST-CASES-STATUS    PIC 9(02).
               10  FILLER                   PIC X(04) VALUE ' ON '.
               10  MSG-TEST-CASES-OPERATION PIC X(04).
               10  FILLER                   PIC X(36) VALUE SPACES.

           05  MSG-MOCK-SYNTAX-1              PIC X(80) VALUE
               'MOCK FILE [filename]'. 
           05  MSG-MOCK-SYNTAX-2              PIC X(80) VALUE
               '  ON [operation] RETURN|STATUS [value]'. 
           05  MSG-MOCK-SYNTAX-3              PIC X(80) VALUE
               'END-MOCK'. 

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
           EVALUATE TRUE
               WHEN WORKING-STORAGE-HEADER
                    PERFORM 2100-INSERT-WORKING-STG-CODE
               WHEN PROCEDURE-DIVISION-HEADER
                    PERFORM 2400-INSERT-PROCEDURE-DIV-CODE
               WHEN SELECT-STATEMENT
                    PERFORM 2200-PROCESS-SELECT
               WHEN FD-STATEMENT
                    PERFORM 2300-PROCESS-FD
               WHEN CHECK-FOR-IO-STATEMENTS
                    PERFORM 2500-CHECK-FOR-IO-STATEMENT
               WHEN OTHER
                    PERFORM 9460-COPY-ORIGINAL-LINE         
           END-EVALUATE         
           .

       2100-INSERT-WORKING-STG-CODE.
      *****************************************************************
      * Insert a COPY statement at the top of the WORKING-STORAGE
      * section to bring in the common unit test definitions.
      *****************************************************************
           PERFORM 9460-COPY-ORIGINAL-LINE
           MOVE WS-WORKING-STORAGE-COPY TO WS-COPY-NAME
           PERFORM 9450-WRITE-COPY-LINE
           .    

       2200-PROCESS-SELECT.
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

           PERFORM 7200-NEXT-TOKEN-ORIGINAL-SOURCE

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
               PERFORM 7500-INITIALIZE-KEYWORD-SEARCH
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
                   PERFORM 7200-NEXT-TOKEN-ORIGINAL-SOURCE
                   IF TOKEN(TOKEN-IX) IS EQUAL TO 'IS'
                       PERFORM 7200-NEXT-TOKEN-ORIGINAL-SOURCE
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
                      INTO INITIALIZATION-STATEMENT(INITIALIZATION-IX)
                   END-STRING                         
                   SET INITIALIZATION-IX UP BY 1
                   STRING
                       STATEMENT-SPACER     DELIMITED BY SIZE
                       STATEMENT-SPACER     DELIMITED BY SIZE
                       'UT-FILE-STATUS-FIELD-NAME(UT-FILE-IX)'
                                            DELIMITED BY SIZE
                       INTO INITIALIZATION-STATEMENT(INITIALIZATION-IX)
                   END-STRING                         
               ELSE
                   PERFORM 9240-READ-AND-COPY-ORIG-SOURCE
                   MOVE ORIGINAL-LINE TO LINE-TO-PARSE
               END-IF                           
           END-PERFORM
           SET FILE-IX UP BY 1      
           .

       2300-PROCESS-FD.
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

           PERFORM 7200-NEXT-TOKEN-ORIGINAL-SOURCE
           PERFORM 7800-STRIP-PERIOD

      * Find the entry in the file information table for this file

           PERFORM VARYING FILE-IX FROM 1 BY 1 
               UNTIL FILE-IX GREATER THAN MAX-FILES
               OR INTERNAL-FILENAME(FILE-IX) EQUAL TOKEN(TOKEN-IX)
           END-PERFORM           

      * Generate code in TEST-SOURCE to position the indices into the 
      * tables of mock and file information.

           PERFORM 6300-GEN-SET-FILENAME-TO-FIND
           SET STATEMENT-IX UP BY 1
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
           MOVE ORIGINAL-LINE TO LINE-TO-PARSE
           MOVE 11 TO KEYWORD-OFFSET
           MOVE KEYWORD-END TO KEYWORD-SEARCH-LIMIT
           PERFORM 7200-NEXT-TOKEN-ORIGINAL-SOURCE
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

       2400-INSERT-PROCEDURE-DIV-CODE.
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
           PERFORM 9440-WRITE-GENERATED-STATEMENTS
                   
      * Insert a COPY for boilerplate unit test code and write
      * generated paragraphs just after it.

           MOVE PROCEDURE-DIVISION-COPYBOOK TO WS-COPY-NAME
           PERFORM 9450-WRITE-COPY-LINE
           PERFORM 9470-WRITE-BEFORE-PARAGRAPH
           PERFORM 9480-WRITE-AFTER-PARAGRAPH
           PERFORM 9485-WRITE-INIT-PARAGRAPH

      * Enable checking for IO statements in subsequent PROCEDURE
      * DIVISION code.

           SET CHECK-FOR-IO-STATEMENTS TO TRUE
           .

       2500-CHECK-FOR-IO-STATEMENT.
      *****************************************************************
      * Replace an IO statement for a mocked file with the code
      * corresponding to the MOCK specification.
      * If the file is not mocked, copy the IO statement as-is.
      *
      * File definition information has already been parsed from the
      * ORIGINAL-SOURCE file, and MOCK specifications have been
      * collected from the TEST-CASES file, before this paragraph is
      * performed.
      *
      * Limitations of the current implementation: 
      * - It will identify an I/O statement only if the I/O verb is the 
      *   first token on the line.
      * - It doesn't handle I/O statements that span more than one
      *   line in the source file.
      * - It doesn't handle READ INTO.
      * - It doesn't handle WRITE FROM. 
      * - It should automatically mock OPEN and CLOSE statements for 
      *   any file that has other mocks specified; it currently handles
      *   only the statement that it has just found.
      *****************************************************************
           SET SUPPRESS-COPY-SOURCE TO FALSE
           SET TOKEN-IX TO 1
           PERFORM 7500-INITIALIZE-KEYWORD-SEARCH
           PERFORM 7200-NEXT-TOKEN-ORIGINAL-SOURCE
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

           IF KEYWORD-MATCHED 
               PERFORM 2510-IO-KEYWORD-MATCHED
           ELSE
               SET SUPPRESS-COPY-SOURCE TO FALSE   
           END-IF

           IF SUPPRESS-COPY-SOURCE
               SET SUPPRESS-COPY-SOURCE TO FALSE
           ELSE
               PERFORM 9460-COPY-ORIGINAL-LINE
           END-IF        
           .

       2510-IO-KEYWORD-MATCHED.
      *****************************************************************
      * An I/O verb was found in ORIGINAL-SOURCE.
      *****************************************************************

      * Filename is next

           SET TOKEN-IX UP BY 1

           PERFORM 
               NEXT-TOKEN-TO-GET(VALID-FILE-OPERATION-IX) TIMES
               PERFORM 7200-NEXT-TOKEN-ORIGINAL-SOURCE
           END-PERFORM

           PERFORM 2520-FIND-CORR-FILE-MOCK
           IF MOCK-MATCHED
               PERFORM 2540-FIND-CORR-FILE-INFO
           ELSE
               SET SUPPRESS-COPY-SOURCE TO FALSE    
           END-IF        
           .

       2520-FIND-CORR-FILE-MOCK.
      *****************************************************************
      * Found an I/O statement in ORIGINAL-SOURCE. See if a MOCK was
      * specified for this filename.
      *****************************************************************
           SET MOCK-MATCHED TO FALSE
           PERFORM VARYING MOCK-IX FROM 1 BY 1
                   UNTIL MOCK-IX IS GREATER THAN MAX-MOCKS
                      OR MOCK-MATCHED

               IF MOCK-FILE(MOCK-IX) 
               AND MOCK-OPERATION(MOCK-IX) EQUAL TOKEN(1)
               AND MOCK-FILENAME(MOCK-IX) EQUAL TOKEN(2)
                   SET MOCK-MATCHED TO TRUE
                   SET MOCK-IX UP BY 1
               ELSE
                   SET SUPPRESS-COPY-SOURCE TO FALSE
               END-IF               

           END-PERFORM            
           .

       2540-FIND-CORR-FILE-INFO.
      *****************************************************************
      * Found an I/O statement for which a MOCK was specified.
      * Generate code in TEST-SOURCE to substitute mock functionality
      * for the real I/O operation.
      * MOCK-IX is already set.
      *****************************************************************
           PERFORM 6400-LOOK-UP-FILE-INFO-FOR-MOCK
           IF FILENAME-MATCHED
               PERFORM 2550-GEN-MOCK-STATEMENTS
           ELSE
               SET SUPPRESS-COPY-SOURCE TO FALSE

      * TO DO: display warning message here. MOCK was specified that
      *        doesn't correspond with any SELECT statement in the
      *        program. Not an error, because if they're doing TDD
      *        they might write the test case before writing the
      *        SELECT statement. But display a warning in case it's
      *        a typo.
           END-IF        
           .

       2550-GEN-MOCK-STATEMENTS.
      *****************************************************************
      * Generate code in TEST-SOURCE to substitute mock behavior for
      * the real I/O functionality.
      *****************************************************************
           MOVE SPACES TO GENERATED-SOURCE-STATEMENTS
           SET STATEMENT-IX TO ZERO

           PERFORM 6300-GEN-SET-FILENAME-TO-FIND
           SET STATEMENT-IX UP BY 1
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
           MOVE '           ADD 1 TO UT-MOCK-ACCESS-COUNT(UT-MOCK-IX)'   
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
           PERFORM 9440-WRITE-GENERATED-STATEMENTS
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
               IF SUPPRESS-COPY-SOURCE
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

               MOVE TEST-CASE-LINE TO LINE-TO-PARSE
               SET SCANNING-TEST-CASES TO TRUE
               SET SCANNING-ORIGINAL-SOURCE TO FALSE
               PERFORM 7400-MATCH-KEYWORD

               IF KEYWORD-MATCHED
                   MOVE KEYWORD-TO-MATCH TO TOKEN(1)
                   SET TOKEN-IX TO 2
                   EVALUATE TRUE
                       WHEN TOKEN(1) IS EQUAL TO 'EXPECT'
                            PERFORM 4300-PROCESS-KEYWORD-EXPECT
                       WHEN TOKEN(1) IS EQUAL TO 'VERIFY'
                            PERFORM 5000-PROCESS-KEYWORD-VERIFY     
                       WHEN TOKEN(1) IS EQUAL TO 'TESTCASE'
                            PERFORM 4400-PROCESS-KEYWORD-TESTCASE  
                       WHEN TOKEN(1) IS EQUAL TO 'TESTSUITE'
                            PERFORM 4500-PROCESS-KEYWORD-TESTSUITE        
                       WHEN TOKEN(1) IS EQUAL TO 'BEFORE-EACH'
                            PERFORM 4600-PROCESS-KEYWORD-BEFORE       
                       WHEN TOKEN(1) IS EQUAL TO 'AFTER-EACH'
                            PERFORM 4700-PROCESS-KEYWORD-AFTER      
                       WHEN TOKEN(1) IS EQUAL TO 'MOCK'
                            PERFORM 4800-PROCESS-KEYWORD-MOCK      
                   END-EVALUATE
               END-IF   
           END-PERFORM
           .    

       4300-PROCESS-KEYWORD-EXPECT.
      *****************************************************************
      * EXPECT keyword was found. 
      *
      * Generate assertion code in TEST-SOURCE.
      *
      * EXPECT [actual-value] TO BE [expected-value]
      *****************************************************************
           PERFORM 7600-ADJUST-OFFSETS
           PERFORM 7100-NEXT-TOKEN-TEST-CASES

           INITIALIZE KEYWORD-MATCHED-INDICATOR
           MOVE 'TO BE ' TO KEYWORD-TO-MATCH
           MOVE 6 TO KEYWORD-MATCH-LENGTH
           PERFORM 7400-MATCH-KEYWORD
           IF KEYWORD-MATCHED
               PERFORM 4320-EXPECT-EQUALITY
           ELSE
               DISPLAY 'SYNTAX IS:'
               DISPLAY '    EXPECT [actual] TO BE [expected]'
           END-IF
           .

       4320-EXPECT-EQUALITY.
      *****************************************************************     
      * Generate code in TEST-SOURCE to assert expected and actual
      * results are equal.
      *****************************************************************     
               SET TOKEN-IX UP BY 1
               MOVE KEYWORD-TO-MATCH TO TOKEN(TOKEN-IX)
               SUBTRACT 1 FROM KEYWORD-OFFSET
               SET TOKEN-IX UP BY 1
               PERFORM 7600-ADJUST-OFFSETS
               PERFORM 7100-NEXT-TOKEN-TEST-CASES

           INITIALIZE GENERATED-SOURCE-STATEMENTS
           SET STATEMENT-IX TO 1

           STRING 
                   MOVE-VERB           DELIMITED BY SIZE
                   TOKEN(2)          DELIMITED BY SPACE
                   ' TO UT-ACTUAL'   DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                   (STATEMENT-OFFSET:STATEMENT-LENGTH)
           END-STRING
           SET STATEMENT-IX UP BY 1

           STRING 
                   MOVE-VERB           DELIMITED BY SIZE
                   TOKEN(4)          DELIMITED BY SIZE
                   ' TO UT-EXPECTED'   DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                   (STATEMENT-OFFSET:STATEMENT-LENGTH)
           END-STRING
           SET STATEMENT-IX UP BY 1

           STRING
                   '    TO UT-EXPECTED' DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                   (STATEMENT-OFFSET:STATEMENT-LENGTH)
           END-STRING
           SET STATEMENT-IX UP BY 1

           MOVE 'PERFORM UT-ASSERT-EQUAL'
               TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                   (STATEMENT-OFFSET:STATEMENT-LENGTH)

           PERFORM 9440-WRITE-GENERATED-STATEMENTS
           SET SUPPRESS-COPY-SOURCE TO TRUE
           .

       4400-PROCESS-KEYWORD-TESTCASE.
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
           MOVE TEST-CASE-LINE(KEYWORD-OFFSET:72) TO TOKEN(2)
           INITIALIZE GENERATED-SOURCE-STATEMENTS
           SET STATEMENT-IX TO 1

           IF GENERATE-PERFORM-AFTER
               MOVE 'PERFORM UT-AFTER' TO        
                   GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                       (STATEMENT-OFFSET:STATEMENT-LENGTH)
               SET STATEMENT-IX UP BY 1
           ELSE
               SET GENERATE-PERFORM-AFTER TO TRUE
           END-IF

           MOVE INCR-TESTCASE-COUNT-STMT
               TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           SET STATEMENT-IX UP BY 1    



           STRING 
                   MOVE-VERB           DELIMITED BY SIZE
                   TOKEN(2)          DELIMITED BY SIZE
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

           PERFORM 9440-WRITE-GENERATED-STATEMENTS
           SET SUPPRESS-COPY-SOURCE TO TRUE
           .

       4500-PROCESS-KEYWORD-TESTSUITE.
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
           MOVE TEST-CASE-LINE(KEYWORD-OFFSET:72) TO TOKEN(2)
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
           PERFORM 9440-WRITE-GENERATED-STATEMENTS
           SET SUPPRESS-COPY-SOURCE TO TRUE        
           .

       4600-PROCESS-KEYWORD-BEFORE.
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
                      OR BEFORE-EACH-IX IS GREATER THAN 50
               PERFORM 9350-READ-TEST-CASES
               MOVE 'END-BEFORE' TO KEYWORD-TO-MATCH
               MOVE 10 TO KEYWORD-MATCH-LENGTH
               PERFORM 7500-INITIALIZE-KEYWORD-SEARCH

               MOVE TEST-CASE-LINE TO LINE-TO-PARSE
               SET SCANNING-TEST-CASES TO TRUE
               SET SCANNING-ORIGINAL-SOURCE TO FALSE
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

       4700-PROCESS-KEYWORD-AFTER.
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
                  OR AFTER-EACH-IX IS GREATER THAN 50
               PERFORM 9350-READ-TEST-CASES
               MOVE 'END-AFTER' TO KEYWORD-TO-MATCH
               MOVE 9 TO KEYWORD-MATCH-LENGTH
               PERFORM 7500-INITIALIZE-KEYWORD-SEARCH

               MOVE TEST-CASE-LINE TO LINE-TO-PARSE
               SET SCANNING-TEST-CASES TO TRUE
               SET SCANNING-ORIGINAL-SOURCE TO FALSE
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

       4800-PROCESS-KEYWORD-MOCK.
      *****************************************************************
      * MOCK keyword was found. 
      *
      * Save information about the MOCK so that code can be  
      * substituted to simulate interaction with the mocked item
      * when references are encountered in ORIGINAL-SOURCE.
      *****************************************************************
           SET MOCK-IX UP BY 1
           SET MOCK-SYNTAX-ERROR TO FALSE

      * Next token will be the type of mock

           ADD 4 TO KEYWORD-OFFSET
           PERFORM 7100-NEXT-TOKEN-TEST-CASES
           EVALUATE TRUE
               WHEN TOKEN(TOKEN-IX) IS EQUAL TO 'FILE'
                    PERFORM 4900-PROCESS-MOCK-FILE-SPEC
           END-EVALUATE
           SET SUPPRESS-COPY-SOURCE TO TRUE
           IF MOCK-SYNTAX-ERROR
               DISPLAY MSG-MOCK-SYNTAX-1    
               DISPLAY MSG-MOCK-SYNTAX-2   
               DISPLAY MSG-MOCK-SYNTAX-3
           END-IF
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
           MOVE '           SET UT-FIND-FILE-MOCK TO TRUE'
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
                   SET MOCK-SYNTAX-ERROR TO FALSE
                   MOVE TOKEN(TOKEN-IX) TO MOCK-OPERATION(MOCK-IX)

                   MOVE TOKEN(TOKEN-IX) TO RAW-VALUE
                   PERFORM 7950-ENCLOSE-IN-QUOTES
                   SET STATEMENT-IX UP BY 1    
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

      * Optional syntactic sugar 'IS' may or may not be next.

           SET TOKEN-IX UP BY 1
           PERFORM 7100-NEXT-TOKEN-TEST-CASES
           SET TOKEN-IX UP BY 1           
           PERFORM 7100-NEXT-TOKEN-TEST-CASES
           IF TOKEN(TOKEN-IX) IS EQUAL TO 'IS'
               PERFORM 7100-NEXT-TOKEN-TEST-CASES
           END-IF    

           COMPUTE PREVIOUS-ENTRY = 
               TOKEN-IX - 1 
           END-COMPUTE

      * Reset the access count for the 'new' mock

           SET STATEMENT-IX UP BY 1
           STRING
               STATEMENT-SPACER               DELIMITED BY SIZE
               MOVE-VERB                      DELIMITED BY SIZE
               ZERO-KEYWORD                   DELIMITED BY SIZE
               TO-KEYWORD                     DELIMITED BY SIZE
               'UT-MOCK-ACCESS-COUNT(UT-MOCK-IX)' DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           END-STRING    

           EVALUATE TRUE

      * If the last token was RETURN, then the current token is a
      * record value.

               WHEN TOKEN(PREVIOUS-ENTRY) IS EQUAL TO 'RETURN'
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

               WHEN TOKEN(PREVIOUS-ENTRY) IS EQUAL TO 'STATUS'
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
                        STRING 
                            TOKEN(TOKEN-IX) DELIMITED BY SPACE
                            INTO MOCK-FILE-STATUS(MOCK-IX)
                        END-STRING
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
           MOVE SET-MOCK-STMT
                TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)

           PERFORM 9440-WRITE-GENERATED-STATEMENTS
           .    

       5000-PROCESS-KEYWORD-VERIFY.
      *****************************************************************
      * VERIFY keyword was found. 
      *
      * Generate assertion code in TEST-SOURCE.
      *
      * VERIFY FILE filename READ HAPPENED times TIMES
      * VERIFY FILE filename WRITE NEVER HAPPENED
      *****************************************************************
           PERFORM 7600-ADJUST-OFFSETS
           PERFORM 7100-NEXT-TOKEN-TEST-CASES

           INITIALIZE KEYWORD-MATCHED-INDICATOR
           MOVE 'FILE ' TO KEYWORD-TO-MATCH
           MOVE 5 TO KEYWORD-MATCH-LENGTH
           PERFORM 7400-MATCH-KEYWORD
           IF KEYWORD-MATCHED
               PERFORM 5020-VERIFY-FILE-MOCK
           ELSE
               DISPLAY 'SYNTAX IS:'
               DISPLAY '    VERIFY FILE filename operation [HAPPENED]'
                       ' count [TIME[S]]'
               DISPLAY '    VERIFY FILE filename operation [HAPPENED]'
                       ' ONCE'        
               DISPLAY '    VERIFY FILE filename operation NEVER'
                       ' [HAPPENED]'
           END-IF
           .

       5020-VERIFY-FILE-MOCK.
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
           PERFORM 6400-LOOK-UP-FILE-INFO-FOR-MOCK

           PERFORM 7100-NEXT-TOKEN-TEST-CASES
           IF TOKEN(TOKEN-IX) IS EQUAL TO HAPPENED-KEYWORD
               PERFORM 7100-NEXT-TOKEN-TEST-CASES
           END-IF
           IF TOKEN(TOKEN-IX) IS EQUAL TO ONCE-KEYWORD
               MOVE 1 TO MOCK-TEST-COUNT
           ELSE     
               MOVE TOKEN(TOKEN-IX) TO MOCK-TEST-COUNT    
               PERFORM 7100-NEXT-TOKEN-TEST-CASES
               IF TOKEN(TOKEN-IX) IS EQUAL TO 'TIME' OR 'TIMES'
                   CONTINUE
      *         ELSE
      * Display syntax error message here
               END-IF
           END-IF    

      * Generate statements in TEST-SOURCE to do the verification

           MOVE SPACES TO GENERATED-SOURCE-STATEMENTS
           SET STATEMENT-IX TO ZERO
           
           PERFORM 6100-GEN-LOOKUP-MOCK
           SET STATEMENT-IX UP BY 1
           MOVE TEMP-STATEMENT 
               TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)

           SET STATEMENT-IX UP BY 1
           MOVE '           IF UT-MOCK-FOUND' 
               TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)

           PERFORM 6200-GEN-LOOKUP-FILE    
           SET STATEMENT-IX UP BY 1
           MOVE TEMP-STATEMENT 
               TO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)       

           SET STATEMENT-IX UP BY 1
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
           STRING
               STATEMENT-SPACER                      DELIMITED BY SIZE
               PERFORM-VERB                          DELIMITED BY SIZE
               'UT-ASSERT-ACCESSES'                  DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
           END-STRING    

           PERFORM 9440-WRITE-GENERATED-STATEMENTS
           SET SUPPRESS-COPY-SOURCE TO TRUE        
           .


       6100-GEN-LOOKUP-MOCK.
      *****************************************************************
      * Generate code for TEST-SOURCE to look up a mock specification
      * based on its filename.
      * Assumes the filename is currently in TOKEN(TOKEN-IX).
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

       6400-LOOK-UP-FILE-INFO-FOR-MOCK.     
      *****************************************************************
      * MOCK-IX is pointing to a MOCK entry and we need to find the
      * corresponding file information.
      *****************************************************************
           SET FILENAME-MATCHED TO FALSE
           PERFORM VARYING FILE-IX FROM 1 BY 1
                   UNTIL FILE-IX IS GREATER THAN MAX-FILES
                      OR FILENAME-MATCHED

               IF INTERNAL-FILENAME(FILE-IX) IS EQUAL TO
                       MOCK-FILENAME(MOCK-IX)
                   SET FILENAME-MATCHED TO TRUE
                   SET FILE-IX DOWN BY 1      
               END-IF
           END-PERFORM                       
           .

       6500-LOOK-UP-FILE-MOCK.
      *****************************************************************
      * Set MOCK-IX to the entry that matches the type (FILE), the
      * filename, and the I/O operation.
      *****************************************************************
           SET MOCK-MATCHED TO FALSE
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
           MOVE TEST-CASE-LINE TO LINE-TO-PARSE
           SET SCANNING-TEST-CASES TO TRUE
           SET SCANNING-ORIGINAL-SOURCE TO FALSE
           PERFORM 7300-EXTRACT-NEXT-TOKEN
           .

       7200-NEXT-TOKEN-ORIGINAL-SOURCE.
      *****************************************************************
      * Extract the next token from ORIGINAL-SOURCE.
      *****************************************************************
           MOVE ORIGINAL-LINE TO LINE-TO-PARSE
           SET SCANNING-ORIGINAL-SOURCE TO TRUE
           SET SCANNING-TEST-CASES TO FALSE
           PERFORM 7300-EXTRACT-NEXT-TOKEN
           .

       7300-EXTRACT-NEXT-TOKEN.
      *****************************************************************
      * Find the next token in either ORIGINAL-SOURCE or TEST-CASES
      * and place it in TOKEN(TOKEN-IX).
      *****************************************************************
           PERFORM 7310-FIND-NEXT-NON-SPACE
           IF LINE-TO-PARSE(KEYWORD-OFFSET:1) IS EQUAL TO '''' OR '"'
               MOVE LINE-TO-PARSE(KEYWORD-OFFSET:1) 
                    TO WS-STRING-DELIMITER
               COMPUTE WS-NEXT =
                   KEYWORD-OFFSET + 1
               END-COMPUTE    
               PERFORM VARYING WS-NEXT FROM WS-NEXT BY 1
                   UNTIL LINE-TO-PARSE(WS-NEXT:1) EQUAL '''' OR '"'
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

           COMPUTE KEYWORD-OFFSET =
               KEYWORD-OFFSET + WS-CHARS
           END-COMPUTE               
           .

       7310-FIND-NEXT-NON-SPACE.
           SET NON-SPACE-FOUND TO FALSE
           PERFORM UNTIL NON-SPACE-FOUND
               PERFORM VARYING KEYWORD-OFFSET FROM KEYWORD-OFFSET BY 1
                   UNTIL KEYWORD-OFFSET IS GREATER THAN KEYWORD-END
                      OR LINE-TO-PARSE(KEYWORD-OFFSET:1) 
                         IS NOT EQUAL TO SPACE
               END-PERFORM  
               IF LINE-TO-PARSE(KEYWORD-OFFSET:1) IS NOT EQUAL TO SPACE
                   SET NON-SPACE-FOUND TO TRUE
               ELSE
                   IF SCANNING-TEST-CASES
                       PERFORM 9350-READ-TEST-CASES
                       MOVE TEST-CASE-LINE TO LINE-TO-PARSE   
                   ELSE
                       PERFORM 9460-COPY-ORIGINAL-LINE
                       PERFORM 9250-READ-ORIGINAL-SOURCE
                       MOVE ORIGINAL-LINE TO LINE-TO-PARSE
                   END-IF    
                   MOVE KEYWORD-START TO KEYWORD-OFFSET
               END-IF            
           END-PERFORM
           .

       7400-MATCH-KEYWORD.
      *****************************************************************
      * Look for a specific keyword on the current input line.
      *****************************************************************
           PERFORM VARYING KEYWORD-OFFSET FROM KEYWORD-OFFSET BY 1
                   UNTIL KEYWORD-OFFSET IS GREATER THAN 
                         KEYWORD-SEARCH-LIMIT
                      OR KEYWORD-MATCHED  
               IF KEYWORD-TO-MATCH IS EQUAL TO
                   LINE-TO-PARSE(KEYWORD-OFFSET:KEYWORD-MATCH-LENGTH)
                   SET KEYWORD-MATCHED TO TRUE
               END-IF
           END-PERFORM    
           .    

       7500-INITIALIZE-KEYWORD-SEARCH.
      *****************************************************************
      * Position pointers for first keyword/token search on a line.
      *****************************************************************
           SET KEYWORD-MATCHED TO FALSE
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

       7700-PERIOD-ON-THIS-LINE.
      *****************************************************************
      * See if there is a period on the current input line.
      *****************************************************************
           MOVE '.' TO KEYWORD-TO-MATCH
           PERFORM 7400-MATCH-KEYWORD
           IF KEYWORD-MATCHED
               SET END-OF-STATEMENT TO TRUE
           ELSE
               SET END-OF-STATEMENT TO FALSE
           END-IF
           .            

       7800-STRIP-PERIOD.
      *****************************************************************
      * Remove periods from the current token.
      *****************************************************************
           INSPECT TOKEN(TOKEN-IX) REPLACING ALL '.' BY ' '
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
      * Assumption: The value to be quoted contains no embedded spaces.
      *****************************************************************
           MOVE SPACES TO QUOTED-VALUE
           STRING
               SINGLE-QUOTE      DELIMITED BY SIZE
               RAW-VALUE         DELIMITED BY SPACE
               SINGLE-QUOTE      DELIMITED BY SIZE
               INTO QUOTED-VALUE
           END-STRING    
           .      

      *****************************************************************
      * 8xxx - Initialization
      * 8100 - Load configuration settings
      * 8200 - Initialize MOCK statement handling
      *****************************************************************

       8000-INITIALIZE.
           PERFORM 8100-LOAD-CONFIG-SETTINGS
           PERFORM 8200-INITIALIZE-MOCK-HANDLING
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
           SET CHECK-FOR-IO-STATEMENTS TO FALSE
           SET MOCK-IX TO 0
           SET FILE-IX TO 1
           MOVE SPACES TO INITIALIZATION-STATEMENTS
           SET INITIALIZATION-IX TO 0
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
                    STRING 
                        TEXT-UNIT-TEST-CONFIG-FILE DELIMITED BY SIZE
                        TEXT-NOT-FOUND             DELIMITED BY SIZE
                        TEXT-ON                    DELIMITED BY SIZE
                        OPEN-VERB                    DELIMITED BY SIZE
                        INTO ERROR-MESSAGE-LINE
                    END-STRING
                    DISPLAY ERROR-MESSAGE-LINE
               WHEN UNIT-TEST-CONFIG-NOT-READABLE
                    STRING 
                        TEXT-UNIT-TEST-CONFIG-FILE DELIMITED BY SIZE
                        TEXT-UNREADABLE            DELIMITED BY SIZE
                        TEXT-ON                    DELIMITED BY SIZE
                        OPEN-VERB                    DELIMITED BY SIZE
                        INTO ERROR-MESSAGE-LINE
                    END-STRING
                    DISPLAY ERROR-MESSAGE-LINE
               WHEN OTHER
                    MOVE UNIT-TEST-CONFIG-STATUS TO MSG-FILE-STATUS
                    STRING 
                        TEXT-UNIT-TEST-CONFIG-FILE DELIMITED BY SIZE
                        TEXT-IO-ERROR              DELIMITED BY SIZE
                        TEXT-ON                    DELIMITED BY SIZE
                        OPEN-VERB                    DELIMITED BY SIZE
                        INTO ERROR-MESSAGE-LINE
                    END-STRING
                    DISPLAY ERROR-MESSAGE-LINE
           END-EVALUATE
           .

       9150-READ-UNIT-TEST-CONFIG.
           READ UNIT-TEST-CONFIG
           IF UNIT-TEST-CONFIG-STATUS-OK OR UNIT-TEST-CONFIG-END-OF-FILE
               CONTINUE
           ELSE
               MOVE UNIT-TEST-CONFIG-STATUS TO MSG-FILE-STATUS
               STRING 
                   TEXT-UNIT-TEST-CONFIG-FILE DELIMITED BY SIZE
                   TEXT-IO-ERROR              DELIMITED BY SIZE
                   TEXT-ON                    DELIMITED BY SIZE
                   READ-VERB                    DELIMITED BY SIZE
                   INTO ERROR-MESSAGE-LINE
               END-STRING
               DISPLAY ERROR-MESSAGE-LINE
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
                    STRING 
                        TEXT-ORIGINAL-SOURCE-FILE DELIMITED BY SIZE
                        TEXT-NOT-FOUND            DELIMITED BY SIZE
                        TEXT-ON                   DELIMITED BY SIZE
                        OPEN-VERB                   DELIMITED BY SIZE
                        INTO ERROR-MESSAGE-LINE
                    END-STRING
                    DISPLAY ERROR-MESSAGE-LINE
               WHEN ORIGINAL-SOURCE-NOT-READABLE
                    STRING 
                        TEXT-ORIGINAL-SOURCE-FILE DELIMITED BY SIZE
                        TEXT-UNREADABLE           DELIMITED BY SIZE
                        TEXT-ON                   DELIMITED BY SIZE
                        OPEN-VERB                   DELIMITED BY SIZE
                        INTO ERROR-MESSAGE-LINE
                    END-STRING
                    DISPLAY ERROR-MESSAGE-LINE
               WHEN OTHER
                    MOVE ORIGINAL-SOURCE-STATUS TO MSG-FILE-STATUS
                    STRING 
                        TEXT-ORIGINAL-SOURCE-FILE DELIMITED BY SIZE
                        TEXT-IO-ERROR             DELIMITED BY SIZE
                        TEXT-ON                   DELIMITED BY SIZE
                        OPEN-VERB                   DELIMITED BY SIZE
                        INTO ERROR-MESSAGE-LINE
                    END-STRING
                    DISPLAY ERROR-MESSAGE-LINE
           END-EVALUATE
           .

       9240-READ-AND-COPY-ORIG-SOURCE.
           PERFORM 9250-READ-ORIGINAL-SOURCE
           PERFORM 9460-COPY-ORIGINAL-LINE
           .    

       9250-READ-ORIGINAL-SOURCE.
           READ ORIGINAL-SOURCE
           IF ORIGINAL-SOURCE-STATUS-OK OR ORIGINAL-SOURCE-END-OF-FILE
               CONTINUE
           ELSE
               MOVE ORIGINAL-SOURCE-STATUS TO MSG-FILE-STATUS
               STRING 
                   TEXT-ORIGINAL-SOURCE-FILE DELIMITED BY SIZE
                   TEXT-IO-ERROR             DELIMITED BY SIZE
                   TEXT-ON                   DELIMITED BY SIZE
                   READ-VERB                   DELIMITED BY SIZE
                   INTO ERROR-MESSAGE-LINE
               END-STRING
               DISPLAY ERROR-MESSAGE-LINE
           END-IF
           .

       9290-CLOSE-ORIGINAL-SOURCE.
           CLOSE ORIGINAL-SOURCE.
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
                    STRING 
                        TEXT-TEST-CASES-FILE      DELIMITED BY SIZE
                        TEXT-NOT-FOUND            DELIMITED BY SIZE
                        TEXT-ON                   DELIMITED BY SIZE
                        OPEN-VERB                   DELIMITED BY SIZE
                        INTO ERROR-MESSAGE-LINE
                    END-STRING
                    DISPLAY ERROR-MESSAGE-LINE
               WHEN TEST-CASES-NOT-READABLE
                    STRING 
                        TEXT-TEST-CASES-FILE      DELIMITED BY SIZE
                        TEXT-UNREADABLE           DELIMITED BY SIZE
                        TEXT-ON                   DELIMITED BY SIZE
                        OPEN-VERB                   DELIMITED BY SIZE
                        INTO ERROR-MESSAGE-LINE
                    END-STRING
                    DISPLAY ERROR-MESSAGE-LINE
               WHEN OTHER
                    MOVE TEST-CASES-STATUS TO MSG-FILE-STATUS
                    STRING 
                        TEXT-TEST-CASES-FILE      DELIMITED BY SIZE
                        TEXT-IO-ERROR             DELIMITED BY SIZE
                        TEXT-ON                   DELIMITED BY SIZE
                        OPEN-VERB                   DELIMITED BY SIZE
                        INTO ERROR-MESSAGE-LINE
                    END-STRING
                    DISPLAY ERROR-MESSAGE-LINE
           END-EVALUATE
           .

       9350-READ-TEST-CASES.
           PERFORM WITH TEST AFTER UNTIL NOT TEST-CASE-COMMENT
               READ TEST-CASES
           END-PERFORM    
           IF TEST-CASES-STATUS-OK 
           OR TEST-CASES-END-OF-FILE
           OR TEST-CASES-EXHAUSTED
               CONTINUE
           ELSE
               STRING 
                   TEXT-TEST-CASES-FILE      DELIMITED BY SIZE
                   TEXT-IO-ERROR             DELIMITED BY SIZE
                   TEXT-ON                   DELIMITED BY SIZE
                   READ-VERB                   DELIMITED BY SIZE
                   INTO ERROR-MESSAGE-LINE
               END-STRING
               DISPLAY ERROR-MESSAGE-LINE
           END-IF
           .

       9390-CLOSE-TEST-CASES.
           CLOSE TEST-CASES.
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
                    STRING 
                        TEXT-TEST-SOURCE-FILE     DELIMITED BY SIZE
                        TEXT-NOT-FOUND            DELIMITED BY SIZE
                        TEXT-ON                   DELIMITED BY SIZE
                        OPEN-VERB                   DELIMITED BY SIZE
                        INTO ERROR-MESSAGE-LINE
                    END-STRING
                    DISPLAY ERROR-MESSAGE-LINE
               WHEN TEST-SOURCE-NOT-WRITABLE
                    STRING 
                        TEXT-TEST-SOURCE-FILE     DELIMITED BY SIZE
                        TEXT-UNWRITABLE           DELIMITED BY SIZE
                        TEXT-ON                   DELIMITED BY SIZE
                        OPEN-VERB                   DELIMITED BY SIZE
                        INTO ERROR-MESSAGE-LINE
                    END-STRING
                    DISPLAY ERROR-MESSAGE-LINE
               WHEN OTHER
                    MOVE TEST-SOURCE-STATUS TO MSG-FILE-STATUS
                    STRING 
                        TEXT-TEST-SOURCE-FILE     DELIMITED BY SIZE
                        TEXT-IO-ERROR             DELIMITED BY SIZE
                        TEXT-ON                   DELIMITED BY SIZE
                        OPEN-VERB                   DELIMITED BY SIZE
                        INTO ERROR-MESSAGE-LINE
                    END-STRING
                    DISPLAY ERROR-MESSAGE-LINE
           END-EVALUATE
           .

       9420-WRITE-TEST-LINE.
           WRITE TEST-LINE
           EVALUATE TRUE
               WHEN TEST-SOURCE-STATUS-OK
                    CONTINUE
               WHEN OTHER
                    MOVE TEST-SOURCE-STATUS TO MSG-FILE-STATUS
                    STRING 
                        TEXT-TEST-SOURCE-FILE     DELIMITED BY SIZE
                        TEXT-IO-ERROR             DELIMITED BY SIZE
                        TEXT-ON                   DELIMITED BY SIZE
                        WRITE-VERB                  DELIMITED BY SIZE
                        INTO ERROR-MESSAGE-LINE
                    END-STRING
                    DISPLAY ERROR-MESSAGE-LINE
           END-EVALUATE
           .    

      *****************************************************************
      * Insert test code resulting from parsing unit test keywords.
      *****************************************************************
       9440-WRITE-GENERATED-STATEMENTS.    
           PERFORM VARYING STATEMENT-IX FROM 1 BY 1
               UNTIL STATEMENT-IX IS GREATER THAN 100
                  OR GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                     IS EQUAL TO SPACES
               MOVE GENERATED-SOURCE-STATEMENT(STATEMENT-IX) 
                   TO TEST-LINE      
               PERFORM 9420-WRITE-TEST-LINE
           END-PERFORM        
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
           MOVE COMMENT-MARKER TO ORIGINAL-LINE(7:1)
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
           PERFORM VARYING BEFORE-EACH-IX FROM 1 BY 1
                   UNTIL BEFORE-EACH-IX IS GREATER THAN 50
                   OR BEFORE-EACH-STATEMENT(BEFORE-EACH-IX)
                   IS EQUAL TO SPACES
               MOVE BEFORE-EACH-STATEMENT(BEFORE-EACH-IX) TO TEST-LINE
               PERFORM 9420-WRITE-TEST-LINE    
           END-PERFORM
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
           PERFORM VARYING AFTER-EACH-IX FROM 1 BY 1
                   UNTIL AFTER-EACH-IX IS GREATER THAN 50
                   OR AFTER-EACH-STATEMENT(AFTER-EACH-IX)
                   IS EQUAL TO SPACES
               MOVE AFTER-EACH-STATEMENT(AFTER-EACH-IX) TO TEST-LINE    
               PERFORM 9420-WRITE-TEST-LINE
           END-PERFORM
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
                   UNTIL INITIALIZATION-IX IS GREATER THAN 50
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

       9490-CLOSE-TEST-SOURCE.
           .    

       9999-END.
           .

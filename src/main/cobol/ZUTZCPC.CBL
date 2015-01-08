       IDENTIFICATION DIVISION.
       PROGRAM-ID.  ZUTZCPC.
      *****************************************************************
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

       01  WS-TRAP PIC S9(04) VALUE ZERO.

      *****************************************************************
      * Saved information about files that are accessed by the program
      * under test. If any MOCKs are declared in the test cases, we
      * will need this information to generate test code. We save the
      * information in the first pass of ORIGINAL-SOURCE so that we
      * won't have to pass the file multiple times.
      *
      * WS-INTERNAL-FILE-NAME is declared in the SELECT statement.
      * WS-RECORD-FIELD contains the name of the first record area
      * declared under the FD statement.
      * WS-FILE-STATUS-FIELD contains the name of the FILE STATUS
      * field declared in the SELECT statement.
      *****************************************************************
       01  WS-FILE-INFORMATION.
           05  WS-MAX-FILES                  PIC 9(02) VALUE 10.
           05  WS-FILE-INFO OCCURS 10 INDEXED BY FILE-IX.
               10  WS-INTERNAL-FILE-NAME     PIC X(31).
               10  WS-RECORD-FIELD           PIC X(31).
               10  WS-FILE-STATUS-FIELD      PIC X(31).

      *****************************************************************
      * Specifications for mocking file accesses, CICS commands, and
      * SQL commands. This table is populated based on MOCK keywords
      * found in the unit TEST-CASES file. The data are then used to
      * substitute fake code in the TEST-SOURCE file.
      *****************************************************************
       01  WS-MOCKS.
           05  FILLER                    PIC X.
               88  MOCK-SYNTAX-ERROR          VALUE 'Y', FALSE SPACE.
           05  WS-MAX-MOCKS              PIC 9(02) VALUE 10.
           05  WS-MOCK OCCURS 10 INDEXED BY MOCK-IX.
               10  WS-MOCK-TYPE          PIC X(04).
                   88  MOCK-FILE         VALUE 'FILE', FALSE SPACES.
                   88  MOCK-CICS         VALUE 'CICS', FALSE SPACES.
                   88  MOCK-SQL          VALUE 'SQL',  FALSE SPACES.
               10  WS-MOCK-RECORD            PIC X(2048).    
               10  WS-MOCK-DATA              PIC X(146).
               10  WS-MOCK-FILE-DATA REDEFINES WS-MOCK-DATA.        
                   15  WS-MOCK-FILENAME      PIC X(20).    
                   15  WS-MOCK-OPERATION     PIC X(20).  
                   15  WS-MOCK-FILE-STATUS   PIC 9(02).
                   15  FILLER                PIC X(104).
               10  WS-MOCK-CICS-DATA REDEFINES WS-MOCK-DATA.
                   15  WS-MOCK-CICS-RESOURCE PIC X(08).
                   15  WS-MOCK-CICS-COMMAND  PIC X(30).
                   15  WS-MOCK-CICS-RIDFLD   PIC X(100).    
                   15  WS-MOCK-EIBRESP       PIC S9(08) COMP.
                   15  WS-MOCK-EIBRESP2      PIC S9(08) COMP.
               10  WS-MOCK-SQL-DATA REDEFINES WS-MOCK-DATA.
                   15  WS-MOCK-SQL-COMMAND   PIC X(20).
                   15  WS-MOCK-SQL-CODE      PIC S9(08) COMP.
                   15  WS-MOCK-SQL-STATE     PIC S9(08) COMP.
                   15  FILLER                PIC X(118).
           05  WS-MAX-STATUS-MNEMONICS       PIC 9(02) VALUE 4.
           05  WS-MOCK-FILE-STATUS-MNEMONICS.
               10  FILLER PIC X(20) VALUE 'OK'.
               10  FILLER PIC X(02) VALUE '00'.
               10  FILLER PIC X(20) VALUE 'END-OF-FILE'.
               10  FILLER PIC X(02) VALUE '10'.
               10  FILLER PIC X(20) VALUE 'EOF'.
               10  FILLER PIC X(02) VALUE '10'.
               10  FILLER PIC X(20) VALUE 'FILE-NOT-FOUND'.
               10  FILLER PIC X(02) VALUE '35'.
           05  FILLER REDEFINES WS-MOCK-FILE-STATUS-MNEMONICS.
               10  FILLER OCCURS 4 INDEXED BY MOCK-STATUS-IX.
                   15  WS-MOCK-FILE-STATUS-MNEMONIC  PIC X(20).
                   15  WS-MOCK-FILE-STATUS-VALUE     PIC X(02).        


       01  WS-MAX-VALID-FILE-OPERATIONS      PIC 9(02) VALUE 6.
       01  WS-VALID-FILE-OPERATIONS.
           05  FILLER PIC X(07) VALUE 'CLOSE'.
           05  FILLER PIC X(07) VALUE 'DELETE'.
           05  FILLER PIC X(07) VALUE 'OPEN'.
           05  FILLER PIC X(07) VALUE 'READ'.
           05  FILLER PIC X(07) VALUE 'REWRITE'.
           05  FILLER PIC X(07) VALUE 'WRITE'.
       01  FILLER REDEFINES WS-VALID-FILE-OPERATIONS.
           05  WS-VALID-FILE-OPERATION 
               OCCURS 6 TIMES 
               INDEXED BY VALID-FILE-OPERATION-IX PIC X(07). 
                   
      *****************************************************************
      * Accumulate source statements that will be inserted into the 
      * TEST-SOURCE file. 
      *****************************************************************
       01  GENERATED-SOURCE-STATEMENTS.
           05  GENERATED-SOURCE-STATEMENT 
               OCCURS 10 
               INDEXED BY STATEMENT-IX   PIC X(80).

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
      * This is a work area used when tokenizing a line of input from
      * the ORIGINAL-SOURCE file.
      *****************************************************************
       01  TOKENS.
           05  TOKEN OCCURS 20 TIMES INDEXED BY TOKEN-IX PIC X(80).

      *****************************************************************
      * This area is used for parsing keywords out of input records.
      *****************************************************************
       01  FILLER.    
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
           05  MAX-KEYWORDS           PIC 9(02) VALUE 6.
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
           05  KEYWORDS REDEFINES KEYWORD-VALUES.
               10  KEYWORD OCCURS 6 INDEXED BY KEYWORD-IX.
                   15  KEYWORD-VALUE      PIC X(20).
                   15  KEYWORD-LENGTH     PIC 9(02).

      *****************************************************************
      * Constants
      *****************************************************************
       01  FILLER.
           05  STATEMENT-OFFSET          PIC 9(02) VALUE 12.
           05  STATEMENT-LENGTH          PIC 9(02) VALUE 70.
           05  WS-STANDARD-TEST-CODE     PIC X(08) VALUE 'ZUTZCPD'.
           05  WS-OPEN                   PIC X(04) VALUE 'OPEN'.
           05  WS-READ                   PIC X(04) VALUE 'READ'.
           05  WS-WRITE                  PIC X(05) VALUE 'WRITE'.
           05  WS-MOVE                   PIC X(05) VALUE 'MOVE '.
           05  WS-PARA-END               PIC X(12) 
                                         VALUE '           .'.
           05  WS-BEFORE-PARA-HEADER     PIC X(17) 
                                         VALUE '       UT-BEFORE.'.
           05  WS-AFTER-PARA-HEADER      PIC X(17) 
                                         VALUE '       UT-AFTER.'.

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
       01  WS-INDICATORS.
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
       01  WS-MESSAGES.
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

           05  WS-MESSAGE-LINE                 PIC X(80) VALUE SPACES.


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

      * refactor
      * 5. organize paragraphs in top-down functional sequence
      * 6. separate setup for token parsing for each input file


       PROCEDURE DIVISION.
      *****************************************************************
      * Paragraph organization:
      *   Open - Mainline
      *   1xxx - Input processing loop
      *   2xxx - Process ORIGINAL-SOURCE file
      *   3xxx - Process ORIGINAL-SOURCE file
      *   4xxx - Process TEST-CASES file
      *   5xxx - Process TEST-CASES file
      *   6xxx - 
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

      *    Look for SELECT INTERNAL-FILE-NAME     

           PERFORM 7200-NEXT-TOKEN-ORIGINAL-SOURCE
           MOVE TOKEN(TOKEN-IX) TO WS-INTERNAL-FILE-NAME(FILE-IX)
           SET TOKEN-IX UP BY 1

      *    Optional FILE STATUS [IS] may be on the current line or on
      *    any subsequent line up to the next period.

           PERFORM UNTIL END-OF-STATEMENT
               PERFORM 7800-PERIOD-ON-THIS-LINE
               IF END-OF-STATEMENT
                   CONTINUE
               ELSE
               MOVE KEYWORD-START TO KEYWORD-OFFSET
               MOVE KEYWORD-END TO KEYWORD-SEARCH-LIMIT
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
                   PERFORM 7900-STRIP-PERIOD    
                   MOVE TOKEN(TOKEN-IX) TO WS-FILE-STATUS-FIELD(FILE-IX)
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

      *    Look for file name to match with the SELECT file name     

           PERFORM 7200-NEXT-TOKEN-ORIGINAL-SOURCE
           PERFORM 7900-STRIP-PERIOD

      *    Find the entry in the file information table for this file

           PERFORM VARYING FILE-IX FROM 1 BY 1 
               UNTIL FILE-IX GREATER THAN WS-MAX-FILES
               OR WS-INTERNAL-FILE-NAME(FILE-IX) EQUAL TOKEN(TOKEN-IX)
           END-PERFORM
           
      *    Find the name of the default record field on the next line

           PERFORM 9240-READ-AND-COPY-ORIG-SOURCE
           MOVE ORIGINAL-LINE TO LINE-TO-PARSE
           MOVE 11 TO KEYWORD-OFFSET
           MOVE KEYWORD-END TO KEYWORD-SEARCH-LIMIT
           PERFORM 7200-NEXT-TOKEN-ORIGINAL-SOURCE
           PERFORM 7900-STRIP-PERIOD
           MOVE TOKEN(TOKEN-IX) TO WS-RECORD-FIELD(FILE-IX)
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
                   
      * Insert a COPY for boilerplate unit test code and write the
      * UT-BEFORE and UT-AFTER paragraphs just after it.

           MOVE WS-STANDARD-TEST-CODE TO WS-COPY-NAME
           PERFORM 9450-WRITE-COPY-LINE
           PERFORM 9470-WRITE-BEFORE-PARAGRAPH
           PERFORM 9480-WRITE-AFTER-PARAGRAPH

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
      *****************************************************************
           SET SUPPRESS-COPY-SOURCE TO FALSE
           SET TOKEN-IX TO 1
           SET KEYWORD-MATCHED TO FALSE
           MOVE KEYWORD-START TO KEYWORD-OFFSET
           MOVE KEYWORD-END TO KEYWORD-SEARCH-LIMIT

           MOVE ORIGINAL-LINE TO LINE-TO-PARSE
           SET SCANNING-ORIGINAL-SOURCE TO TRUE
           SET SCANNING-TEST-CASES TO FALSE


      *     PERFORM VARYING VALID-FILE-OPERATION-IX FROM 1 BY 1
      *             UNTIL VALID-FILE-OPERATION-IX IS GREATER THAN
      *                   WS-MAX-VALID-FILE-OPERATIONS
      *                OR KEYWORD-MATCHED   
      *         PERFORM 7200-NEXT-TOKEN-ORIGINAL-SOURCE
      *         IF TOKEN(TOKEN-IX) IS EQUAL TO
      *                 WS-VALID-FILE-OPERATION(VALID-FILE-OPERATION-IX)
      *             SET KEYWORD-MATCHED TO TRUE    
      *         END-IF                    
      *     END-PERFORM

           IF SUPPRESS-COPY-SOURCE
               CONTINUE
           ELSE
               PERFORM 9460-COPY-ORIGINAL-LINE
           END-IF  
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
           PERFORM 9310-OPEN-TEST-CASES
           PERFORM 4100-PARSE-TEST-CASES
               UNTIL NOT TEST-CASES-STATUS-OK 
           PERFORM 9390-CLOSE-TEST-CASES
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

      *****************************************************************
      * EXPECT keyword was found. 
      *
      * Convert
      *
      *     EXPECT [actual] TO BE [expected]
      *
      * into
      *
      *     MOVE [actual] TO UT-ACTUAL
      *     MOVE [expected] TO UT-EXPECTED
      *     PERFORM UT-ASSERT-EQUAL
      *****************************************************************
       4300-PROCESS-KEYWORD-EXPECT.
           PERFORM 7700-ADJUST-OFFSETS
           PERFORM 7100-NEXT-TOKEN-TEST-CASES
           INITIALIZE KEYWORD-MATCHED-INDICATOR
           MOVE 'TO BE ' TO KEYWORD-TO-MATCH
           MOVE 6 TO KEYWORD-MATCH-LENGTH
           PERFORM 7400-MATCH-KEYWORD
           IF KEYWORD-MATCHED
               SET TOKEN-IX UP BY 1
               MOVE KEYWORD-TO-MATCH TO TOKEN(TOKEN-IX)
               SUBTRACT 1 FROM KEYWORD-OFFSET
               SET TOKEN-IX UP BY 1
               PERFORM 7700-ADJUST-OFFSETS
               PERFORM 7100-NEXT-TOKEN-TEST-CASES
           ELSE
               DISPLAY 'SYNTAX IS:'
               DISPLAY '    EXPECT [actual] TO BE [expected]'
           END-IF

           INITIALIZE GENERATED-SOURCE-STATEMENTS
           SET STATEMENT-IX TO 1

           STRING 
                   WS-MOVE           DELIMITED BY SIZE
                   TOKEN(2)          DELIMITED BY SPACE
                   ' TO UT-ACTUAL'   DELIMITED BY SIZE
               INTO GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                   (STATEMENT-OFFSET:STATEMENT-LENGTH)
           END-STRING
           SET STATEMENT-IX UP BY 1

           STRING 
                   WS-MOVE           DELIMITED BY SIZE
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

           STRING 
                   WS-MOVE           DELIMITED BY SIZE
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
               MOVE KEYWORD-START TO KEYWORD-OFFSET
               MOVE KEYWORD-END TO KEYWORD-SEARCH-LIMIT

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
               MOVE KEYWORD-START TO KEYWORD-OFFSET
               MOVE KEYWORD-END TO KEYWORD-SEARCH-LIMIT

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
           SET MOCK-SYNTAX-ERROR TO FALSE
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
           ELSE
               SET MOCK-IX UP BY 1    
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
           ADD 1 TO KEYWORD-OFFSET
           PERFORM 7100-NEXT-TOKEN-TEST-CASES
           MOVE TOKEN(TOKEN-IX) TO WS-MOCK-FILENAME(MOCK-IX)

      *---------------------------------------------------------------
           DISPLAY '4900-PROCESS-MOCK-FILE-SPEC: FILENAME ' 
                WS-MOCK-FILENAME(MOCK-IX)
      *---------------------------------------------------------------

           SET TOKEN-IX UP BY 1
           PERFORM 9350-READ-TEST-CASES
           MOVE KEYWORD-START TO KEYWORD-OFFSET
           PERFORM 7100-NEXT-TOKEN-TEST-CASES

      *---------------------------------------------------------------
           DISPLAY '4900-PROCESS-MOCK-FILE-SPEC: WANT ON, GOT <' 
               TOKEN(TOKEN-IX) '>'
      *---------------------------------------------------------------

           IF TOKEN(TOKEN-IX) IS NOT EQUAL TO 'ON'
               SET MOCK-SYNTAX-ERROR TO TRUE
           END-IF    
           ADD 1 TO KEYWORD-OFFSET
           PERFORM 7100-NEXT-TOKEN-TEST-CASES

      *---------------------------------------------------------------
           DISPLAY '4900-PROCESS-MOCK-FILE-SPEC: WANT OPERATION, GOT <'
               TOKEN(TOKEN-IX) '>'
      *---------------------------------------------------------------

           SET MOCK-SYNTAX-ERROR TO TRUE
           PERFORM VARYING VALID-FILE-OPERATION-IX FROM 1 BY 1
                   UNTIL VALID-FILE-OPERATION-IX IS GREATER THAN
                   WS-MAX-VALID-FILE-OPERATIONS
               IF TOKEN(TOKEN-IX) IS EQUAL TO
                       WS-VALID-FILE-OPERATION(VALID-FILE-OPERATION-IX)
                   SET MOCK-SYNTAX-ERROR TO FALSE
                   MOVE TOKEN(TOKEN-IX) TO WS-MOCK-OPERATION(MOCK-IX)
               END-IF            
           END-PERFORM        

      *---------------------------------------------------------------
           IF MOCK-SYNTAX-ERROR
               DISPLAY '4900-PROCESS-MOCK-FILE-SPEC: OPERATION ' 
                   TOKEN(TOKEN-IX) ' WAS NOT RECOGNIZED'
           ELSE
               DISPLAY '4900-PROCESS-MOCK-FILE-SPEC: OPERATION ' 
                   TOKEN(TOKEN-IX) ' WAS RECOGNIZED AS VALID'
           END-IF                                       
      *---------------------------------------------------------------

           SET TOKEN-IX UP BY 1
           PERFORM 7100-NEXT-TOKEN-TEST-CASES

      *---------------------------------------------------------------
           DISPLAY '4900-PROCESS-MOCK-FILE-SPEC: '
               'WANT RETURN OR STATUS, GOT <'
               TOKEN(TOKEN-IX) '>'
      *---------------------------------------------------------------

           SET TOKEN-IX UP BY 1           
           PERFORM 7100-NEXT-TOKEN-TEST-CASES

      *---------------------------------------------------------------
           DISPLAY '4900-PROCESS-MOCK-FILE-SPEC: '
               'MAY BE OPTIONAL "IS", GOT <'
               TOKEN(TOKEN-IX) '>'
      *---------------------------------------------------------------

           IF TOKEN(TOKEN-IX) IS EQUAL TO 'IS'
               PERFORM 7100-NEXT-TOKEN-TEST-CASES
           END-IF    

      *---------------------------------------------------------------
           DISPLAY '4900-PROCESS-MOCK-FILE-SPEC: '
               'SHOULD BE PAST "IS", GOT <'
               TOKEN(TOKEN-IX) '>'
      *---------------------------------------------------------------

           EVALUATE TRUE
               WHEN TOKEN(TOKEN-IX - 1) IS EQUAL TO 'RETURN'
                    MOVE TOKEN(TOKEN-IX) 
                      TO WS-MOCK-RECORD(MOCK-IX)
               WHEN TOKEN(TOKEN-IX - 1) IS EQUAL TO 'STATUS'
                    PERFORM VARYING MOCK-STATUS-IX FROM 1 BY 1
                            UNTIL MOCK-STATUS-IX IS GREATER THAN
                                  WS-MAX-STATUS-MNEMONICS 
                               OR TOKEN(TOKEN-IX) IS EQUAL TO
                                  WS-MOCK-FILE-STATUS-MNEMONIC
                                      (MOCK-STATUS-IX)
                    END-PERFORM
                    IF MOCK-STATUS-IX IS GREATER THAN 
                            WS-MAX-STATUS-MNEMONICS                   
                        MOVE TOKEN(TOKEN-IX)
                            TO WS-MOCK-FILE-STATUS(MOCK-IX)
                    ELSE
                        MOVE WS-MOCK-FILE-STATUS-VALUE(MOCK-STATUS-IX)
                            TO WS-MOCK-FILE-STATUS(MOCK-IX)
                    END-IF        
               WHEN OTHER              
                    SET MOCK-SYNTAX-ERROR TO TRUE
           END-EVALUATE      

      *---------------------------------------------------------------
           DISPLAY 'WS-MOCK-FILENAME(MOCK-IX) <' 
                    WS-MOCK-FILENAME(MOCK-IX) '>'
           DISPLAY 'WS-MOCK-OPERATION(MOCK-IX) <' 
                    WS-MOCK-OPERATION(MOCK-IX) '>'
           DISPLAY 'WS-MOCK-FILE-STATUS(MOCK-IX) <'
                    WS-MOCK-FILE-STATUS(MOCK-IX) '>'   
           DISPLAY 'WS-MOCK-RECORD(MOCK-IX) <'
                    WS-MOCK-RECORD(MOCK-IX) '>'                
      *---------------------------------------------------------------
                        
           SET TOKEN-IX UP BY 1
           PERFORM 7100-NEXT-TOKEN-TEST-CASES
           IF TOKEN(TOKEN-IX) IS NOT EQUAL TO 'END-MOCK'
               SET MOCK-SYNTAX-ERROR TO TRUE
           END-IF    
           .    

      *****************************************************************
      * 7xxx - Utility routines
      *****************************************************************

       7100-NEXT-TOKEN-TEST-CASES.
           MOVE TEST-CASE-LINE TO LINE-TO-PARSE
           SET SCANNING-TEST-CASES TO TRUE
           SET SCANNING-ORIGINAL-SOURCE TO FALSE
           PERFORM 7300-EXTRACT-NEXT-TOKEN
           .

       7200-NEXT-TOKEN-ORIGINAL-SOURCE.
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

       7700-ADJUST-OFFSETS.
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

       7800-PERIOD-ON-THIS-LINE.
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

       7900-STRIP-PERIOD.
      *****************************************************************
      * Remove periods from the current token.
      *****************************************************************
           INSPECT TOKEN(TOKEN-IX) REPLACING ALL '.' BY ' '
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
           INITIALIZE WS-MOCKS
           SET CHECK-FOR-IO-STATEMENTS TO FALSE
           SET MOCK-IX TO 1
           SET FILE-IX TO 1
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
                        WS-OPEN                    DELIMITED BY SIZE
                        INTO WS-MESSAGE-LINE
                    END-STRING
                    DISPLAY WS-MESSAGE-LINE
               WHEN UNIT-TEST-CONFIG-NOT-READABLE
                    STRING 
                        TEXT-UNIT-TEST-CONFIG-FILE DELIMITED BY SIZE
                        TEXT-UNREADABLE            DELIMITED BY SIZE
                        TEXT-ON                    DELIMITED BY SIZE
                        WS-OPEN                    DELIMITED BY SIZE
                        INTO WS-MESSAGE-LINE
                    END-STRING
                    DISPLAY WS-MESSAGE-LINE
               WHEN OTHER
                    MOVE UNIT-TEST-CONFIG-STATUS TO MSG-FILE-STATUS
                    STRING 
                        TEXT-UNIT-TEST-CONFIG-FILE DELIMITED BY SIZE
                        TEXT-IO-ERROR              DELIMITED BY SIZE
                        TEXT-ON                    DELIMITED BY SIZE
                        WS-OPEN                    DELIMITED BY SIZE
                        INTO WS-MESSAGE-LINE
                    END-STRING
                    DISPLAY WS-MESSAGE-LINE
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
                   WS-READ                    DELIMITED BY SIZE
                   INTO WS-MESSAGE-LINE
               END-STRING
               DISPLAY WS-MESSAGE-LINE
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
                        WS-OPEN                   DELIMITED BY SIZE
                        INTO WS-MESSAGE-LINE
                    END-STRING
                    DISPLAY WS-MESSAGE-LINE
               WHEN ORIGINAL-SOURCE-NOT-READABLE
                    STRING 
                        TEXT-ORIGINAL-SOURCE-FILE DELIMITED BY SIZE
                        TEXT-UNREADABLE           DELIMITED BY SIZE
                        TEXT-ON                   DELIMITED BY SIZE
                        WS-OPEN                   DELIMITED BY SIZE
                        INTO WS-MESSAGE-LINE
                    END-STRING
                    DISPLAY WS-MESSAGE-LINE
               WHEN OTHER
                    MOVE ORIGINAL-SOURCE-STATUS TO MSG-FILE-STATUS
                    STRING 
                        TEXT-ORIGINAL-SOURCE-FILE DELIMITED BY SIZE
                        TEXT-IO-ERROR             DELIMITED BY SIZE
                        TEXT-ON                   DELIMITED BY SIZE
                        WS-OPEN                   DELIMITED BY SIZE
                        INTO WS-MESSAGE-LINE
                    END-STRING
                    DISPLAY WS-MESSAGE-LINE
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
                   WS-READ                   DELIMITED BY SIZE
                   INTO WS-MESSAGE-LINE
               END-STRING
               DISPLAY WS-MESSAGE-LINE
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
                        WS-OPEN                   DELIMITED BY SIZE
                        INTO WS-MESSAGE-LINE
                    END-STRING
                    DISPLAY WS-MESSAGE-LINE
               WHEN TEST-CASES-NOT-READABLE
                    STRING 
                        TEXT-TEST-CASES-FILE      DELIMITED BY SIZE
                        TEXT-UNREADABLE           DELIMITED BY SIZE
                        TEXT-ON                   DELIMITED BY SIZE
                        WS-OPEN                   DELIMITED BY SIZE
                        INTO WS-MESSAGE-LINE
                    END-STRING
                    DISPLAY WS-MESSAGE-LINE
               WHEN OTHER
                    MOVE TEST-CASES-STATUS TO MSG-FILE-STATUS
                    STRING 
                        TEXT-TEST-CASES-FILE      DELIMITED BY SIZE
                        TEXT-IO-ERROR             DELIMITED BY SIZE
                        TEXT-ON                   DELIMITED BY SIZE
                        WS-OPEN                   DELIMITED BY SIZE
                        INTO WS-MESSAGE-LINE
                    END-STRING
                    DISPLAY WS-MESSAGE-LINE
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
                   WS-READ                   DELIMITED BY SIZE
                   INTO WS-MESSAGE-LINE
               END-STRING
               DISPLAY WS-MESSAGE-LINE
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
                        WS-OPEN                   DELIMITED BY SIZE
                        INTO WS-MESSAGE-LINE
                    END-STRING
                    DISPLAY WS-MESSAGE-LINE
               WHEN TEST-SOURCE-NOT-WRITABLE
                    STRING 
                        TEXT-TEST-SOURCE-FILE     DELIMITED BY SIZE
                        TEXT-UNWRITABLE           DELIMITED BY SIZE
                        TEXT-ON                   DELIMITED BY SIZE
                        WS-OPEN                   DELIMITED BY SIZE
                        INTO WS-MESSAGE-LINE
                    END-STRING
                    DISPLAY WS-MESSAGE-LINE
               WHEN OTHER
                    MOVE TEST-SOURCE-STATUS TO MSG-FILE-STATUS
                    STRING 
                        TEXT-TEST-SOURCE-FILE     DELIMITED BY SIZE
                        TEXT-IO-ERROR             DELIMITED BY SIZE
                        TEXT-ON                   DELIMITED BY SIZE
                        WS-OPEN                   DELIMITED BY SIZE
                        INTO WS-MESSAGE-LINE
                    END-STRING
                    DISPLAY WS-MESSAGE-LINE
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
                        WS-WRITE                  DELIMITED BY SIZE
                        INTO WS-MESSAGE-LINE
                    END-STRING
                    DISPLAY WS-MESSAGE-LINE
           END-EVALUATE
           .    

      *****************************************************************
      * Insert test code resulting from parsing unit test keywords.
      *****************************************************************
       9440-WRITE-GENERATED-STATEMENTS.    
           PERFORM VARYING STATEMENT-IX FROM 1 BY 1
               UNTIL STATEMENT-IX IS GREATER THAN 10
                  OR GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                     IS EQUAL TO SPACES
               MOVE GENERATED-SOURCE-STATEMENT(STATEMENT-IX) 
                   TO TEST-LINE      
               PERFORM 9420-WRITE-TEST-LINE
           END-PERFORM        
           .

      *****************************************************************
      * Insert a line into TEST-SOURCE containing a COPY statement.
      *****************************************************************
       9450-WRITE-COPY-LINE.
           MOVE WS-COPY-LINE TO TEST-LINE
           PERFORM 9420-WRITE-TEST-LINE
           .

      *****************************************************************
      * Copy a line from ORIGINAL-SOURCE to TEST-SOURCE unchanged.
      *****************************************************************
       9460-COPY-ORIGINAL-LINE.
           MOVE ORIGINAL-LINE TO TEST-LINE
           PERFORM 9420-WRITE-TEST-LINE
           .

      *****************************************************************
      * Write the UT-BEFORE paragraph into the TEST-SOURCE file.
      * This contains the statements specified in the BEFORE-EACH
      * block of the TEST-CASES file, if any.
      *****************************************************************
       9470-WRITE-BEFORE-PARAGRAPH.
           MOVE WS-BEFORE-PARA-HEADER TO TEST-LINE
           PERFORM 9420-WRITE-TEST-LINE
           PERFORM VARYING BEFORE-EACH-IX FROM 1 BY 1
                   UNTIL BEFORE-EACH-IX IS GREATER THAN 50
                   OR BEFORE-EACH-STATEMENT(BEFORE-EACH-IX)
                   IS EQUAL TO SPACES
               MOVE BEFORE-EACH-STATEMENT(BEFORE-EACH-IX) TO TEST-LINE
               PERFORM 9420-WRITE-TEST-LINE    
           END-PERFORM
           MOVE WS-PARA-END TO TEST-LINE
           PERFORM 9420-WRITE-TEST-LINE        
           .

      *****************************************************************
      * Write the UT-AFTER paragraph into the TEST-SOURCE file.
      * This contains the statements specified in the AFTER-EACH
      * block of the TEST-CASES file, if any.
      *****************************************************************
       9480-WRITE-AFTER-PARAGRAPH.
           MOVE WS-AFTER-PARA-HEADER TO TEST-LINE
           PERFORM 9420-WRITE-TEST-LINE
           PERFORM VARYING AFTER-EACH-IX FROM 1 BY 1
                   UNTIL AFTER-EACH-IX IS GREATER THAN 50
                   OR AFTER-EACH-STATEMENT(AFTER-EACH-IX)
                   IS EQUAL TO SPACES
               MOVE AFTER-EACH-STATEMENT(AFTER-EACH-IX) TO TEST-LINE    
               PERFORM 9420-WRITE-TEST-LINE
           END-PERFORM
           MOVE WS-PARA-END TO TEST-LINE
           PERFORM 9420-WRITE-TEST-LINE        
           .

       9490-CLOSE-TEST-SOURCE.
           .    

       9999-END.
           .

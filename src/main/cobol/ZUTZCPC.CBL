       IDENTIFICATION DIVISION.
       PROGRAM-ID.  ZUTZCPC.
      *****************************************************************
      * PRECOMPILER TO COPY A SOURCE FILE TO A TEMPORARY TEST FILE AND
      * INSERT UNIT TEST CODE INTO THE COPY.
      *****************************************************************       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ORIGINAL-SOURCE
               ASSIGN TO EXTERNAL SRCPRG
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT UNIT-TEST-CONFIG
               ASSIGN TO EXTERNAL UTSTCFG
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TEST-SOURCE
               ASSIGN TO EXTERNAL TESTPRG
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
           05  FILLER                    PIC X(73).       

       WORKING-STORAGE SECTION.
       01  FILE-NAMES-TO-BE-MOCKED.
           05  FILE-NAME OCCURS 20 INDEXED BY FILE-IX PIC X(31).
       01  GENERATED-SOURCE-STATEMENTS.
           05  GENERATED-SOURCE-STATEMENT 
               OCCURS 10 
               INDEXED BY STATEMENT-IX   PIC X(80).
       01  BEFORE-EACH-STATEMENTS.
           05  BEFORE-EACH-STATEMENT
               OCCURS 50
               INDEXED BY BEFORE-EACH-IX PIC X(80).        
       01  AFTER-EACH-STATEMENTS.
           05  AFTER-EACH-STATEMENT
               OCCURS 50
               INDEXED BY AFTER-EACH-IX PIC X(80).        
       01  TOKENS.
           05  TOKEN OCCURS 20 TIMES INDEXED BY TOKEN-IX PIC X(80).
       01  FILLER.    
           05  KEYWORD-MATCHED-INDICATOR PIC X(01) VALUE SPACE.
               88  KEYWORD-NOT-MATCHED          VALUE SPACE.
               88  KEYWORD-MATCHED              VALUE 'Y'.
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
               10  FILLER PIC 9(02) VALUE 7.
               10  FILLER PIC X(20) VALUE 'TESTCASE'.
               10  FILLER PIC 9(02) VALUE 9.
               10  FILLER PIC X(20) VALUE 'TESTSUITE'.
               10  FILLER PIC 9(02) VALUE 10.
               10  FILLER PIC X(20) VALUE 'BEFORE-EACH'.
               10  FILLER PIC 9(02) VALUE 12.     
               10  FILLER PIC X(20) VALUE 'AFTER-EACH'.
               10  FILLER PIC 9(02) VALUE 11.     
               10  FILLER PIC X(20) VALUE 'MOCK'.
               10  FILLER PIC 9(02) VALUE 5.     
           05  KEYWORDS REDEFINES KEYWORD-VALUES.
               10  KEYWORD OCCURS 6 INDEXED BY KEYWORD-IX.
                   15  KEYWORD-VALUE      PIC X(20).
                   15  KEYWORD-LENGTH     PIC 9(02).
       01  WS-CONSTANTS.
           05  STATEMENT-OFFSET          PIC 9(02) VALUE 12.
           05  STATEMENT-LENGTH          PIC 9(02) VALUE 70.
           05  WS-STANDARD-TEST-CODE     PIC X(8) VALUE 'ZUTZCPD'.
           05  WS-OPEN                   PIC X(4) VALUE 'OPEN'.
           05  WS-READ                   PIC X(4) VALUE 'READ'.
           05  WS-MOVE                   PIC X(5) VALUE 'MOVE '.
           05  WS-PARA-END               PIC X(12) 
                                         VALUE '           .'.
           05  WS-BEFORE-PARA-HEADER     PIC X(17) 
                                         VALUE '       UT-BEFORE.'.
           05  WS-AFTER-PARA-HEADER      PIC X(17) 
                                         VALUE '       UT-AFTER.'.
       01  TEST-CASES-STATUS             PIC 9(2) VALUE ZERO.
           88  TEST-CASES-STATUS-OK               VALUE ZERO.
           88  TEST-CASES-END-OF-FILE             VALUE 10.
           88  TEST-CASES-FILE-NOT-FOUND          VALUE 35.
           88  TEST-CASES-NOT-READABLE            VALUE 37.
       01  FILLER                        PIC X VALUE SPACE.
           88  END-OF-FILE                     VALUE 'Y'.     
       01  FILLER                        PIC X VALUE SPACE.
           88  NOT-END-OF-TEST-CASES           VALUE SPACE.
           88  END-OF-TEST-CASES               VALUE 'Y'.   
       01  COPY-SOURCE-INDICATOR         PIC X VALUE SPACE.
           88  SUPPRESS-COPY-SOURCE            VALUE 'Y'.    
       01  FILLER                        PIC X VALUE SPACE.
           88  GENERATE-PERFORM-AFTER          VALUE 'Y'.    
       01  FILLER.
           05  WS-WORKING-STORAGE-COPY   PIC X(12).
           05  WS-PROCEDURE-COPY         PIC X(12).
       01  WS-COPY-LINE.
           05  FILLER                    PIC X(17)
                                         VALUE '            COPY '.
           05  WS-COPY-NAME              PIC X(12).
           05  FILLER                    PIC X(51) VALUE '.'.  
       01  WS-MESSAGES.
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
       LINKAGE SECTION.
       PROCEDURE DIVISION.
           PERFORM 0100-INITIALIZE
           PERFORM 1000-PROCESS-INPUT
           GOBACK
           .

       0100-INITIALIZE.
           OPEN INPUT UNIT-TEST-CONFIG
           READ UNIT-TEST-CONFIG
           MOVE COPYBOOK-NAME TO WS-WORKING-STORAGE-COPY
           READ UNIT-TEST-CONFIG
           MOVE COPYBOOK-NAME TO WS-PROCEDURE-COPY
           CLOSE UNIT-TEST-CONFIG
           INITIALIZE FILE-NAMES-TO-BE-MOCKED
           SET FILE-IX TO 1
           .

       1000-PROCESS-INPUT.
           OPEN INPUT ORIGINAL-SOURCE
           OPEN OUTPUT TEST-SOURCE
           PERFORM UNTIL END-OF-FILE
               READ ORIGINAL-SOURCE
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END    
                       PERFORM 2100-INSERT-TEST-CODE
               END-READ
           END-PERFORM
           CLOSE TEST-SOURCE
           CLOSE ORIGINAL-SOURCE
           .

       2100-INSERT-TEST-CODE.
           PERFORM 3000-COPY-ORIGINAL-LINE
           EVALUATE TRUE
               WHEN WORKING-STORAGE-HEADER
                    PERFORM 2200-INS-WORKING-STORAGE-CODE
               WHEN PROCEDURE-DIVISION-HEADER
                    PERFORM 2300-INS-PROCEDURE-DIV-CODE
           END-EVALUATE         
           .

       2200-INS-WORKING-STORAGE-CODE.
           MOVE WS-WORKING-STORAGE-COPY TO WS-COPY-NAME
           PERFORM 3100-WRITE-COPY-LINE
           .    

       2300-INS-PROCEDURE-DIV-CODE.
           PERFORM 3200-INSERT-TEST-CASES 

           INITIALIZE GENERATED-SOURCE-STATEMENTS
           SET STATEMENT-IX TO 1
           MOVE 'PERFORM UT-AFTER' TO        
               GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                   (STATEMENT-OFFSET:STATEMENT-LENGTH)
           PERFORM 4100-WRITE-GENERATED-STATEMENTS
                   
           MOVE WS-STANDARD-TEST-CODE TO WS-COPY-NAME
           PERFORM 3100-WRITE-COPY-LINE
           PERFORM 5000-WRITE-BEFORE-PARAGRAPH
           PERFORM 5100-WRITE-AFTER-PARAGRAPH
           .

       3000-COPY-ORIGINAL-LINE.
           WRITE 
               TEST-LINE FROM ORIGINAL-LINE
           END-WRITE
           .

       3100-WRITE-COPY-LINE.
           WRITE 
               TEST-LINE FROM WS-COPY-LINE
           END-WRITE
           .

       3200-INSERT-TEST-CASES.
           PERFORM 3210-OPEN-TEST-CASES
           PERFORM 3220-COPY-TEST-CODE
               UNTIL NOT TEST-CASES-STATUS-OK 
           PERFORM 3230-CLOSE-TEST-CASES
           .

       3210-OPEN-TEST-CASES.
           OPEN INPUT TEST-CASES
           EVALUATE TRUE
               WHEN TEST-CASES-STATUS-OK
                    CONTINUE
               WHEN TEST-CASES-FILE-NOT-FOUND
                    DISPLAY MSG-TEST-CASES-NOT-FOUND
               WHEN TEST-CASES-NOT-READABLE
                    DISPLAY MSG-TEST-CASES-UNREADABLE
               WHEN OTHER
                    MOVE WS-OPEN TO MSG-TEST-CASES-OPERATION
                    MOVE TEST-CASES-STATUS TO MSG-TEST-CASES-STATUS
                    DISPLAY MSG-TEST-CASES-FILE-ERROR           
           END-EVALUATE
           .

       3220-COPY-TEST-CODE.
           PERFORM 3230-READ-TEST-CASES
           IF TEST-CASES-STATUS-OK
               PERFORM 4000-PROCESS-KEYWORDS
               IF SUPPRESS-COPY-SOURCE
                   CONTINUE
               ELSE    
                   WRITE 
                       TEST-LINE FROM TEST-CASE-LINE
                   END-WRITE
               END-IF                
           END-IF
           .    

       3230-READ-TEST-CASES.
           PERFORM WITH TEST AFTER UNTIL NOT TEST-CASE-COMMENT
               READ TEST-CASES
           END-PERFORM    
           IF TEST-CASES-STATUS-OK OR TEST-CASES-END-OF-FILE
               CONTINUE
           ELSE
               MOVE WS-READ TO MSG-TEST-CASES-OPERATION
               MOVE TEST-CASES-STATUS TO MSG-TEST-CASES-STATUS
               DISPLAY MSG-TEST-CASES-FILE-ERROR               
           END-IF
           .

       3230-CLOSE-TEST-CASES.
           CLOSE TEST-CASES.
           .

       4000-PROCESS-KEYWORDS.
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
               PERFORM 4900-MATCH-KEYWORD
               IF KEYWORD-MATCHED
                   MOVE KEYWORD-TO-MATCH TO TOKEN(1)
                   SET TOKEN-IX TO 2
                   EVALUATE TRUE
                       WHEN TOKEN(1) IS EQUAL TO 'EXPECT'
                            PERFORM 4010-PROCESS-KEYWORD-EXPECT
                       WHEN TOKEN(1) IS EQUAL TO 'TESTCASE'
                            PERFORM 4020-PROCESS-KEYWORD-TESTCASE  
                       WHEN TOKEN(1) IS EQUAL TO 'TESTSUITE'
                            PERFORM 4030-PROCESS-KEYWORD-TESTSUITE        
                       WHEN TOKEN(1) IS EQUAL TO 'BEFORE-EACH'
                            PERFORM 4040-PROCESS-KEYWORD-BEFORE       
                       WHEN TOKEN(1) IS EQUAL TO 'AFTER-EACH'
                            PERFORM 4050-PROCESS-KEYWORD-AFTER      
                       WHEN TOKEN(1) IS EQUAL TO 'MOCK'
                            PERFORM 4060-PROCESS-KEYWORD-MOCK      
                   END-EVALUATE
               END-IF   
           END-PERFORM
           .    

       4010-PROCESS-KEYWORD-EXPECT.
           PERFORM 4790-ADJUST-OFFSETS
           PERFORM 4800-EXTRACT-NEXT-TOKEN
           INITIALIZE KEYWORD-MATCHED-INDICATOR
           MOVE 'TO BE ' TO KEYWORD-TO-MATCH
           MOVE 6 TO KEYWORD-MATCH-LENGTH
           PERFORM 4900-MATCH-KEYWORD
           IF KEYWORD-MATCHED
               SET TOKEN-IX UP BY 1
               MOVE KEYWORD-TO-MATCH TO TOKEN(TOKEN-IX)
               SUBTRACT 1 FROM KEYWORD-OFFSET
               SET TOKEN-IX UP BY 1
               PERFORM 4790-ADJUST-OFFSETS
               PERFORM 4800-EXTRACT-NEXT-TOKEN
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

           PERFORM 4100-WRITE-GENERATED-STATEMENTS
           SET SUPPRESS-COPY-SOURCE TO TRUE
           .

       4020-PROCESS-KEYWORD-TESTCASE.
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

           PERFORM 4100-WRITE-GENERATED-STATEMENTS
           SET SUPPRESS-COPY-SOURCE TO TRUE
           .

       4030-PROCESS-KEYWORD-TESTSUITE.
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
           PERFORM 4100-WRITE-GENERATED-STATEMENTS
           SET SUPPRESS-COPY-SOURCE TO TRUE        
           .

       4040-PROCESS-KEYWORD-BEFORE.
           INITIALIZE KEYWORD-MATCHED-INDICATOR
           PERFORM VARYING BEFORE-EACH-IX FROM 1 BY 1
                   UNTIL END-OF-FILE 
                      OR KEYWORD-MATCHED
                      OR BEFORE-EACH-IX IS GREATER THAN 50
               PERFORM 3230-READ-TEST-CASES
               MOVE 'END-BEFORE' TO KEYWORD-TO-MATCH
               MOVE 10 TO KEYWORD-MATCH-LENGTH
               MOVE KEYWORD-START TO KEYWORD-OFFSET
               MOVE KEYWORD-END TO KEYWORD-SEARCH-LIMIT
               PERFORM 4900-MATCH-KEYWORD
               IF KEYWORD-MATCHED
                   CONTINUE
               ELSE
                   MOVE TEST-CASE-LINE TO BEFORE-EACH-STATEMENT
                                          (BEFORE-EACH-IX)
               END-IF                           
           END-PERFORM
           SET SUPPRESS-COPY-SOURCE TO TRUE        
           .

       4050-PROCESS-KEYWORD-AFTER.
           INITIALIZE KEYWORD-MATCHED-INDICATOR
           PERFORM VARYING AFTER-EACH-IX FROM 1 BY 1
                   UNTIL END-OF-FILE 
                      OR KEYWORD-MATCHED
                      OR AFTER-EACH-IX IS GREATER THAN 50
               PERFORM 3230-READ-TEST-CASES
               MOVE 'END-AFTER' TO KEYWORD-TO-MATCH
               MOVE 9 TO KEYWORD-MATCH-LENGTH
               MOVE KEYWORD-START TO KEYWORD-OFFSET
               MOVE KEYWORD-END TO KEYWORD-SEARCH-LIMIT
               PERFORM 4900-MATCH-KEYWORD
               IF KEYWORD-MATCHED
                   CONTINUE
               ELSE
                   MOVE TEST-CASE-LINE TO AFTER-EACH-STATEMENT
                                          (AFTER-EACH-IX)
               END-IF                           
           END-PERFORM
           SET SUPPRESS-COPY-SOURCE TO TRUE        
           .

       4060-PROCESS-KEYWORD-MOCK.
           DISPLAY 'IN 4060-PROCESS-KEYWORD-MOCK'
           ADD 4 TO KEYWORD-OFFSET
           PERFORM 4800-EXTRACT-NEXT-TOKEN

           EVALUATE TRUE
               WHEN TOKEN(TOKEN-IX) IS EQUAL TO 'FILE'
                    ADD 1 TO KEYWORD-OFFSET
                    PERFORM 4800-EXTRACT-NEXT-TOKEN 
                    MOVE TOKEN(TOKEN-IX) TO FILE-NAME(FILE-IX)

           DISPLAY 'FILE-NAME(1) IS <' FILE-NAME(1) '>'
           DISPLAY 'FILE-NAME(2) IS <' FILE-NAME(2) '>'

                    SET FILE-IX UP BY 1                   
           END-EVALUATE
           SET SUPPRESS-COPY-SOURCE TO TRUE
           .


       4100-WRITE-GENERATED-STATEMENTS.    
           PERFORM VARYING STATEMENT-IX FROM 1 BY 1
               UNTIL STATEMENT-IX IS GREATER THAN 10
                  OR GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
                     IS EQUAL TO SPACES
               WRITE TEST-LINE
                   FROM GENERATED-SOURCE-STATEMENT(STATEMENT-IX)
               END-WRITE    
           END-PERFORM        
           .

       4780-FIND-NEXT-NON-SPACE.
           PERFORM VARYING KEYWORD-OFFSET FROM KEYWORD-OFFSET BY 1
               UNTIL TEST-CASE-LINE(KEYWORD-OFFSET:1)
                     IS NOT EQUAL TO SPACE
           END-PERFORM          
           .

       4790-ADJUST-OFFSETS.
           COMPUTE KEYWORD-OFFSET = 
               KEYWORD-OFFSET + KEYWORD-LENGTH(KEYWORD-IX) - 1
           END-COMPUTE

           COMPUTE KEYWORD-SEARCH-LIMIT =
               KEYWORD-END - KEYWORD-OFFSET
           END-COMPUTE    
           .

       4800-EXTRACT-NEXT-TOKEN.

           PERFORM 4780-FIND-NEXT-NON-SPACE

           IF TEST-CASE-LINE(KEYWORD-OFFSET:1) IS EQUAL TO '''' OR '"'
               MOVE TEST-CASE-LINE(KEYWORD-OFFSET:1) 
                    TO WS-STRING-DELIMITER
               COMPUTE WS-NEXT =
                   KEYWORD-OFFSET + 1
               END-COMPUTE    
               PERFORM VARYING WS-NEXT FROM WS-NEXT BY 1
                   UNTIL TEST-CASE-LINE(WS-NEXT:1) EQUAL '''' OR '"'
               END-PERFORM
               COMPUTE WS-CHARS =
                   WS-NEXT - KEYWORD-OFFSET + 1
               END-COMPUTE        
           ELSE
               MOVE ZERO TO WS-CHARS
               INSPECT TEST-CASE-LINE
                       (KEYWORD-OFFSET:KEYWORD-SEARCH-LIMIT)
                   TALLYING WS-CHARS FOR CHARACTERS BEFORE INITIAL SPACE
           END-IF    

           MOVE TEST-CASE-LINE(KEYWORD-OFFSET:WS-CHARS) 
               TO TOKEN(TOKEN-IX)    

           COMPUTE KEYWORD-OFFSET =
               KEYWORD-OFFSET + WS-CHARS
           END-COMPUTE               
           .

       4900-MATCH-KEYWORD.
           PERFORM VARYING KEYWORD-OFFSET FROM KEYWORD-OFFSET BY 1
                   UNTIL KEYWORD-OFFSET IS GREATER THAN 
                         KEYWORD-SEARCH-LIMIT
                      OR KEYWORD-MATCHED  
               IF KEYWORD-TO-MATCH IS EQUAL TO
                  TEST-CASE-LINE
                          (KEYWORD-OFFSET:KEYWORD-MATCH-LENGTH)
                   SET KEYWORD-MATCHED TO TRUE
               END-IF
           END-PERFORM    
           .    

       5000-WRITE-BEFORE-PARAGRAPH.
           MOVE WS-BEFORE-PARA-HEADER TO TEST-LINE
           WRITE TEST-LINE
           PERFORM VARYING BEFORE-EACH-IX FROM 1 BY 1
                   UNTIL BEFORE-EACH-IX IS GREATER THAN 50
                   OR BEFORE-EACH-STATEMENT(BEFORE-EACH-IX)
                   IS EQUAL TO SPACES
               WRITE TEST-LINE
                   FROM BEFORE-EACH-STATEMENT(BEFORE-EACH-IX)
               END-WRITE
           END-PERFORM
           MOVE WS-PARA-END TO TEST-LINE
           WRITE TEST-LINE        
           .

       5100-WRITE-AFTER-PARAGRAPH.
           MOVE WS-AFTER-PARA-HEADER TO TEST-LINE
           WRITE TEST-LINE
           PERFORM VARYING AFTER-EACH-IX FROM 1 BY 1
                   UNTIL AFTER-EACH-IX IS GREATER THAN 50
                   OR AFTER-EACH-STATEMENT(AFTER-EACH-IX)
                   IS EQUAL TO SPACES
               WRITE TEST-LINE
                   FROM AFTER-EACH-STATEMENT(AFTER-EACH-IX)
               END-WRITE
           END-PERFORM
           MOVE WS-PARA-END TO TEST-LINE
           WRITE TEST-LINE        
           .

       9999-END.
           .

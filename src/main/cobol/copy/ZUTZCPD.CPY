      * ZUTZCPD.CPY
           DISPLAY SPACE
           MOVE UT-TEST-CASE-COUNT TO UT-TEST-CASE-NUMBER
           DISPLAY UT-TEST-CASE-NUMBER ' TEST CASES WERE EXECUTED'
           MOVE UT-NUMBER-PASSED TO UT-TEST-CASE-NUMBER
           DISPLAY UT-TEST-CASE-NUMBER ' PASSED'
           MOVE UT-NUMBER-FAILED TO UT-TEST-CASE-NUMBER
           DISPLAY UT-TEST-CASE-NUMBER ' FAILED'
           DISPLAY "================================================="
           MOVE UT-RETCODE TO RETURN-CODE
           GOBACK.

       UT-ASSERT-EQUAL.
      *****************************************************************
      * COMPARE EXPECTED AND ACTUAL VALUES FOR EQUALITY.
      *****************************************************************
           MOVE UT-TEST-CASE-COUNT TO UT-TEST-CASE-NUMBER
           PERFORM UT-COMPARE
           IF UT-COMPARE-PASSED
               PERFORM UT-DISPLAY-PASSED
           ELSE 
               PERFORM UT-DISPLAY-FAILED
           END-IF
           .

       UT-COMPARE.
           SET UT-COMPARE-FAILED TO TRUE
           IF UT-COMPARE-NUMERIC
               IF UT-ACTUAL-NUMERIC IS EQUAL TO UT-EXPECTED-NUMERIC
                   SET UT-COMPARE-PASSED TO TRUE
               END-IF    
           ELSE 
               IF UT-ACTUAL IS EQUAL TO UT-EXPECTED
                   SET UT-COMPARE-PASSED TO TRUE
               END-IF    
           END-IF        
           PERFORM UT-REVERSE-RESULT
           .

       UT-REVERSE-RESULT.
           IF UT-REVERSE-COMPARE
               IF UT-COMPARE-PASSED
                   SET UT-COMPARE-FAILED TO TRUE
               ELSE
                   SET UT-COMPARE-PASSED TO TRUE
               END-IF
           END-IF
           .                

       UT-DISPLAY-PASSED.
           ADD 1 TO UT-NUMBER-PASSED
           DISPLAY UT-PASSED 
                   UT-TEST-CASE-NUMBER '. ' 
                   UT-TEST-CASE-NAME
           .              

       UT-DISPLAY-FAILED.
           ADD 1 TO UT-NUMBER-FAILED
           DISPLAY UT-FAILED 
                   UT-TEST-CASE-NUMBER '. ' 
                   UT-TEST-CASE-NAME
           IF UT-COMPARE-NUMERIC
               DISPLAY '    EXPECTED ' UT-EXPECTED-NUMERIC
                       ', WAS ' UT-ACTUAL-NUMERIC
           ELSE                     
               DISPLAY '    EXPECTED <' UT-EXPECTED 
                       '>, WAS <' UT-ACTUAL '>'
           END-IF            
           MOVE 4 TO UT-RETCODE
           .

       UT-ASSERT-ACCESSES.
      *****************************************************************
      * COMPARE THE NUMBER OF ACCESSES TO A MOCK WITH THE EXPECTED
      * NUMBER OF ACCESSES.
      *****************************************************************
           MOVE UT-TEST-CASE-COUNT TO UT-TEST-CASE-NUMBER
           MOVE UT-ACTUAL-ACCESSES TO UT-ACTUAL-ACCESSES-FMT
           MOVE UT-EXPECTED-ACCESSES TO UT-EXPECTED-ACCESSES-FMT
           IF UT-EXPECTED-ACCESSES IS EQUAL TO 1
              MOVE SPACES TO UT-LABEL-EXPECTED-ACCESS-PL
           ELSE
              MOVE UT-CONST-ES TO UT-LABEL-EXPECTED-ACCESS-PL
           END-IF      

           SET UT-VERIFY-FAILED TO TRUE
           EVALUATE TRUE
               WHEN UT-VERIFY-AT-LEAST
                    IF UT-ACTUAL-ACCESSES IS GREATER THAN OR EQUAL TO 
                            UT-EXPECTED-ACCESSES
                        SET UT-VERIFY-PASSED TO TRUE
                    END-IF    
               WHEN UT-VERIFY-NO-MORE-THAN
                    IF UT-ACTUAL-ACCESSES IS LESS THAN OR EQUAL TO
                            UT-EXPECTED-ACCESSES
                        SET UT-VERIFY-PASSED TO TRUE
                    END-IF        
               WHEN OTHER
                    IF UT-ACTUAL-ACCESSES IS EQUAL TO
                            UT-EXPECTED-ACCESSES
                        SET UT-VERIFY-PASSED TO TRUE
                    END-IF        
           END-EVALUATE    

           IF UT-VERIFY-PASSED
               ADD 1 TO UT-NUMBER-PASSED
               DISPLAY UT-PASSED 
                       UT-TEST-CASE-NUMBER '. ' 
                      'VERIFY ' UT-EXPECTED-ACCESSES-FMT SPACE
                      UT-LABEL-EXPECTED-ACCESS
           ELSE 
               ADD 1 TO UT-NUMBER-FAILED
               MOVE SPACES TO UT-DISPLAY-MESSAGE
               IF UT-MOCK-FILE(UT-MOCK-IX)
                   STRING 
                       UT-FAILED                      DELIMITED BY SIZE
                       UT-TEST-CASE-NUMBER            DELIMITED BY SIZE
                       '. VERIFY ACCESSES TO '        DELIMITED BY SIZE
                       UT-MOCK-OPERATION(UT-MOCK-IX)  DELIMITED BY SPACE
                       ' ON '                         DELIMITED BY SIZE
                       UT-MOCK-FILENAME(UT-MOCK-IX)   DELIMITED BY SPACE
                       ' | EXPECTED '                 DELIMITED BY SIZE
                       UT-EXPECTED-ACCESSES-FMT       DELIMITED BY SIZE
                       SPACE                          DELIMITED BY SIZE
                       UT-LABEL-EXPECTED-ACCESS       DELIMITED BY SPACE
                       ', WAS '                       DELIMITED BY SIZE
                       UT-ACTUAL-ACCESSES-FMT         DELIMITED BY SIZE
                       INTO UT-DISPLAY-MESSAGE
                   END-STRING
               ELSE
                   STRING 
                       UT-FAILED                      DELIMITED BY SIZE
                       UT-TEST-CASE-NUMBER            DELIMITED BY SIZE
                       '. VERIFY ACCESSES TO '        DELIMITED BY SIZE
                       UT-MOCK-CICS-KEYWORDS-KEY(UT-MOCK-IX)
                                                      DELIMITED BY SIZE
                       INTO UT-DISPLAY-MESSAGE
                   END-STRING     
                   DISPLAY UT-DISPLAY-MESSAGE
                   MOVE SPACES TO UT-DISPLAY-MESSAGE
                   STRING 
                       '   EXPECTED '                 DELIMITED BY SIZE
                       UT-EXPECTED-ACCESSES-FMT       DELIMITED BY SIZE
                       SPACE                          DELIMITED BY SIZE
                       UT-LABEL-EXPECTED-ACCESS       DELIMITED BY SPACE
                       ', WAS '                       DELIMITED BY SIZE
                       UT-ACTUAL-ACCESSES-FMT         DELIMITED BY SIZE
                       INTO UT-DISPLAY-MESSAGE
                   END-STRING                                  
               END-IF    
               DISPLAY UT-DISPLAY-MESSAGE
               MOVE 4 TO UT-RETCODE
           END-IF              
           . 

       UT-SET-MOCK.
      *****************************************************************
      * CREATE OR UPDATE A MOCK SPECIFICATION.
      *****************************************************************
           EVALUATE TRUE
               WHEN UT-FIND-FILE-MOCK
                    PERFORM UT-SET-FILE-MOCK
               WHEN UT-FIND-CALL-MOCK
                    PERFORM UT-SET-CALL-MOCK
               WHEN UT-FIND-CICS-MOCK
                    PERFORM UT-SET-CICS-MOCK
               WHEN UT-FIND-PARA-MOCK
                    PERFORM UT-SET-PARA-MOCK          
           END-EVALUATE
           .

       UT-SET-FILE-MOCK.
           PERFORM UT-LOOKUP-MOCK
           IF UT-MOCK-FOUND
               CONTINUE
           ELSE    
               ADD 1 TO UT-MOCK-COUNT
               SET UT-MOCK-IX TO UT-MOCK-COUNT
               SET UT-MOCK-FILE(UT-MOCK-IX) TO TRUE
               MOVE UT-MOCK-FIND-FILENAME 
                    TO UT-MOCK-FILENAME(UT-MOCK-IX)
               MOVE UT-MOCK-FIND-OPERATION
                    TO UT-MOCK-OPERATION(UT-MOCK-IX)
           END-IF
           MOVE UT-MOCK-SET-RECORD 
                TO UT-MOCK-RECORD(UT-MOCK-IX)
           MOVE UT-MOCK-SET-FILE-STATUS 
                TO UT-MOCK-FILE-STATUS(UT-MOCK-IX)
           .    

       UT-SET-CALL-MOCK.
           PERFORM UT-LOOKUP-MOCK
           IF UT-MOCK-FOUND
               CONTINUE
           ELSE    
               ADD 1 TO UT-MOCK-COUNT
               SET UT-MOCK-IX TO UT-MOCK-COUNT
               MOVE UT-MOCK-FIND-CALL-TOKENS
                   TO UT-MOCK-CALL-TOKENS-KEY(UT-MOCK-IX)
           END-IF
           .    

       UT-SET-CICS-MOCK.
           PERFORM UT-LOOKUP-MOCK
           IF UT-MOCK-FOUND
               CONTINUE
           ELSE    
               ADD 1 TO UT-MOCK-COUNT
               SET UT-MOCK-IX TO UT-MOCK-COUNT
               MOVE UT-MOCK-FIND-CICS-KEYWORDS
                   TO UT-MOCK-CICS-KEYWORDS-KEY(UT-MOCK-IX)
           END-IF
           .    

       UT-SET-PARA-MOCK.
           PERFORM UT-LOOKUP-MOCK
           IF UT-MOCK-FOUND
               CONTINUE
           ELSE
               ADD 1 TO UT-MOCK-COUNT
               SET UT-MOCK-IX TO UT-MOCK-COUNT
               MOVE UT-MOCK-FIND-PARA-NAME
                   TO UT-MOCK-PARA-NAME(UT-MOCK-IX)
           END-IF            
           .

       UT-LOOKUP-MOCK.
      *****************************************************************
      * LOOK UP A MOCK SPECIFICATION.
      *****************************************************************
           SET UT-MOCK-NOT-FOUND TO TRUE
           PERFORM VARYING UT-MOCK-IX FROM 1 BY 1
               UNTIL UT-MOCK-IX IS GREATER THAN UT-MOCK-MAX
                  OR UT-MOCK-FOUND
               EVALUATE TRUE
                   WHEN UT-FIND-FILE-MOCK   
                       IF UT-MOCK-FIND-FILENAME IS EQUAL TO
                              UT-MOCK-FILENAME(UT-MOCK-IX)
                       AND UT-MOCK-FIND-OPERATION IS EQUAL TO
                              UT-MOCK-OPERATION(UT-MOCK-IX)
                           SET UT-MOCK-FOUND TO TRUE
                           CONTINUE
                       END-IF
                   WHEN UT-FIND-CALL-MOCK
                       IF UT-MOCK-FIND-CALL-TOKENS IS EQUAL TO
                              UT-MOCK-CALL-TOKENS-KEY(UT-MOCK-IX)
                           SET UT-MOCK-FOUND TO TRUE
                           CONTINUE
                       END-IF         
                   WHEN UT-FIND-CICS-MOCK
                       IF UT-MOCK-FIND-CICS-KEYWORDS IS EQUAL TO
                              UT-MOCK-CICS-KEYWORDS-KEY(UT-MOCK-IX)
                           SET UT-MOCK-FOUND TO TRUE
                           CONTINUE
                       END-IF         
                   WHEN UT-FIND-PARA-MOCK
                       IF UT-MOCK-FIND-PARA-NAME IS EQUAL TO
                              UT-MOCK-PARA-NAME(UT-MOCK-IX)
                           SET UT-MOCK-FOUND TO TRUE
                           CONTINUE
                       END-IF               
               END-EVALUATE
           END-PERFORM    
           SET UT-MOCK-IX DOWN BY 1
           .    

       UT-LOOKUP-FILE.
      *****************************************************************
      * LOOK UP A FILE SPECIFICATION.
      *****************************************************************
           PERFORM VARYING UT-FILE-IX FROM 1 BY 1
               UNTIL UT-FILE-IX GREATER UT-FILE-MAX
               OR UT-INTERNAL-FILENAME(UT-FILE-IX)
                EQUAL UT-MOCK-FIND-FILENAME
             CONTINUE    
           END-PERFORM
           SET UT-FILE-IX DOWN BY 1
           .

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

      *****************************************************************
      * COMPARE TWO PIC X FIELDS FOR EQUALITY.
      *****************************************************************
       UT-ASSERT-EQUAL.
           MOVE UT-TEST-CASE-COUNT TO UT-TEST-CASE-NUMBER
           IF UT-ACTUAL IS EQUAL TO UT-EXPECTED
               ADD 1 TO UT-NUMBER-PASSED
               DISPLAY UT-PASSED 
                       UT-TEST-CASE-NUMBER '. ' 
                      UT-TEST-CASE-NAME
           ELSE 
               ADD 1 TO UT-NUMBER-FAILED
               DISPLAY UT-FAILED 
                       UT-TEST-CASE-NUMBER '. ' 
                       UT-TEST-CASE-NAME 
                   "| EXPECTED <" UT-EXPECTED ">, GOT <" UT-ACTUAL ">"
               MOVE 4 TO UT-RETCODE
           END-IF
           .

      *****************************************************************
      * CREATE OR UPDATE A MOCK SPECIFICATION.
      *****************************************************************
       UT-SET-MOCK.
           IF UT-FIND-FILE-MOCK
               PERFORM UT-SET-FILE-MOCK
           END-IF    
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

       UT-LOOKUP-MOCK.
      *****************************************************************
      * LOOK UP A MOCK SPECIFICATION.
      *****************************************************************
           SET UT-MOCK-FOUND TO FALSE
           PERFORM VARYING UT-MOCK-IX FROM 1 BY 1
               UNTIL UT-MOCK-IX IS GREATER THAN UT-MOCK-MAX
                  OR UT-MOCK-FOUND
               IF UT-MOCK-FIND-FILENAME IS EQUAL TO
                          UT-MOCK-FILENAME(UT-MOCK-IX)
               AND UT-MOCK-FIND-OPERATION IS EQUAL TO
                          UT-MOCK-OPERATION(UT-MOCK-IX)
                   SET UT-MOCK-FOUND TO TRUE
                   CONTINUE
               END-IF               
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
           END-PERFORM
           SET UT-FILE-IX DOWN BY 1
           .

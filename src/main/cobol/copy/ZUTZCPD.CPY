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

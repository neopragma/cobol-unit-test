      * UNIT TESTS FOR SAMPLE.CBL
           
           DISPLAY SPACES
           DISPLAY '==================================================='
           DISPLAY 'UNIT TESTS FOR SAMPLE.CBL'

           MOVE 'IT RETURNS HELLO, WORLD! AS GREETING' 
               TO UT-TEST-CASE-NAME
           MOVE 'HELLO, WORLD!' TO UT-EXPECTED
           MOVE 'GREETING' TO WS-MESSAGE-TYPE
           PERFORM 2000-SPEAK
           PERFORM 2000-SPEAK
           MOVE WS-MESSAGE TO UT-ACTUAL
           PERFORM UT-CHECK

           MOVE 'IT RETURNS GOODBYE, CRUEL WORLD! AS FAREWELL' 
               TO UT-TEST-CASE-NAME
           MOVE 'GOODBYE, CRUEL WORLD!' TO UT-EXPECTED
           MOVE 'FAREWELL' TO WS-MESSAGE-TYPE
           PERFORM 2000-SPEAK
           MOVE WS-MESSAGE TO UT-ACTUAL
           PERFORM UT-CHECK

           DISPLAY '==================================================='

           MOVE UT-RETCODE TO RETURN-CODE
           GOBACK
           .
       UT-CHECK.
           IF UT-ACTUAL IS EQUAL TO UT-EXPECTED
               DISPLAY UT-PASSED UT-TEST-CASE-NAME
           ELSE 
               DISPLAY UT-FAILED UT-TEST-CASE-NAME 
               DISPLAY '    EXPECTED <' UT-EXPECTED '>'
               DISPLAY '      ACTUAL <' UT-ACTUAL '>'
               MOVE 4 TO UT-RETCODE
           END-IF             
           .     
       UT-CHECK-END.
           .       

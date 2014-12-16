      * UNIT TESTS FOR VSAMMOCK.CBL
           
           DISPLAY SPACES
           DISPLAY '==================================================='
           DISPLAY 'UNIT TESTS FOR VSAMMOCK.CBL'

           MOVE 'IT INITIALIZES FILE METADATA TO SPACES' 
               TO UT-TEST-CASE-NAME
           MOVE SPACES TO UT-EXPECTED
           SET VMF-FUNCTION-INITIALIZE TO TRUE
           PERFORM 0100-INITIALIZE
           MOVE FILE-METADATA TO UT-ACTUAL
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

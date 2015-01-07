       IDENTIFICATION DIVISION.
       PROGRAM-ID.  SAMPLE.
      *****************************************************************
      * TRIVIAL PROGRAM TO EXERCISE ZUTZCPC.
      *****************************************************************       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FILLER.
           05  WS-MESSAGE-TYPE          PIC X(08) VALUE SPACES.
           05  WS-MESSAGE               PIC X(40) VALUE SPACES.
       PROCEDURE DIVISION.

       2000-SPEAK.
           IF WS-MESSAGE-TYPE IS EQUAL TO 'GREETING'
               MOVE 'HELLO, WORLD!' TO WS-MESSAGE
           END-IF  
           IF WS-MESSAGE-TYPE IS EQUAL TO 'FAREWELL'
               MOVE 'SEE YOU LATER, ALLIGATOR!' TO WS-MESSAGE  
           END-IF    
           .    

       9999-END.
           .

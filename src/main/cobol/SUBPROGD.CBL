      **********************************************************************
      * AUTHOR:    DAVE NICOLETTE
      * DATE:      21 JAN 2015
      * PURPOSE:   TEST DRIVER TO TEST A CALLED SUBPROGRAM
      **********************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUBPROGD.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-ARGUMENTS.
           05  WS-ARG-1      PIC X(80).
           05  WS-ARG-2      PIC X(80).
       PROCEDURE DIVISION.
           MOVE 'Argument 1 initial value' TO WS-ARG-1
           MOVE 'Argument 2 initial value' TO WS-ARG-2
           CALL 'SUBPROG' USING
               BY REFERENCE WS-ARGUMENTS
           END-CALL    
           .
       9999-END.
           .

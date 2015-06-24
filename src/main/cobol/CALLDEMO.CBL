      **********************************************************************
      * AUTHOR:    DAVE NICOLETTE
      * DATE:      25 JUN 2015
      * PURPOSE:   DEMONSTRATE MOCKING OF CALL STATEMENTS.
      **********************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALLDEMO.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WS-ALPHA                PIC X.
       77  WS-OMEGA                PIC X.
       77  WS-SUBPROGRAM-NAME      PIC X(08).
       PROCEDURE DIVISION.
           GOBACK
           .

       1000-BASIC-CALL.
           MOVE 'A' TO WS-ALPHA
           MOVE 'Z' TO WS-OMEGA
           CALL 'NOTREALNAME' 
               USING WS-ALPHA WS-OMEGA
           END-CALL
           .
 
       2000-CLASSIC-CALL.
           MOVE 'A' TO WS-ALPHA.
           MOVE 'Z' TO WS-OMEGA.
           CALL 'NOTREALNAME' 
               USING WS-ALPHA WS-OMEGA.
           .

       3000-DYNAMIC-CALL.
           MOVE 'A' TO WS-ALPHA.
           MOVE 'Z' TO WS-OMEGA.
           CALL WS-SUBPROGRAM-NAME 
               USING WS-ALPHA WS-OMEGA.
           .



       9999-END.
           .
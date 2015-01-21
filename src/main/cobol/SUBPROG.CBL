      **********************************************************************
      * AUTHOR:    DAVE NICOLETTE
      * DATE:      21 JAN 2015
      * PURPOSE:   DEMONSTRATE UNIT TESTING A CALLED SUBPROGRAM
      **********************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUBPROG.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       01  LS-ARGUMENTS.
           05  LS-ARG-1      PIC X(80).
           05  LS-ARG-2      PIC X(80).
       PROCEDURE DIVISION USING BY REFERENCE LS-ARGUMENTS.
           DISPLAY 'Enter SUBPROG'
           MOVE 'Production run' TO LS-ARG-1
           GOBACK
           .
       1000-RETURN-VALUE-A.
           MOVE 'This is value A' TO LS-ARG-2
           .           
       1000-RETURN-VALUE-B.
           MOVE 'This is value B' TO LS-ARG-2
           .           
       9999-END.
           .
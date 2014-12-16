      **********************************************************************
      * AUTHOR:    DAVE NICOLETTE
      * DATE:      07 AUG 2014
      * PURPOSE:   SAMPLE PROGRAM THAT WORKS WITH DATES.
      *
      * USAGE:     INVDATE
      **********************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INVDATE.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           COPY DATETIME.
       01  WS-NEXT-INVOICE-DATE PIC X(8).   
       01  WS-QUOTIENT          PIC S9(4) COMP.
       01  WS-REMAINDER         PIC S9(4) COMP. 
       PROCEDURE DIVISION.

       0000-MAIN.

           PERFORM 0500-INITIALIZE
           PERFORM 1000-PROCESS-INVOICES
           GOBACK
           .

       0500-INITIALIZE.
           .

       1000-PROCESS-INVOICES.
           PERFORM 2000-NEXT-INVOICE-DATE
           .

       2000-NEXT-INVOICE-DATE.  
           EVALUATE TRUE
               WHEN FEBRUARY 
                    PERFORM 2100-HANDLE-FEBRUARY
               WHEN 30-DAY-MONTH
                    MOVE 30 TO WS-CURRENT-DAY
               WHEN OTHER 
                    MOVE 31 TO WS-CURRENT-DAY
           END-EVALUATE              
           MOVE WS-CURRENT-DATE TO WS-NEXT-INVOICE-DATE
           .

       2100-HANDLE-FEBRUARY.
           DIVIDE 4 INTO WS-CURRENT-YEAR
               GIVING WS-QUOTIENT
               REMAINDER WS-REMAINDER
           END-DIVIDE
           IF  WS-REMAINDER EQUAL ZERO
               MOVE 29 TO WS-CURRENT-DAY
           ELSE
               MOVE 28 TO WS-CURRENT-DAY
           END-IF            
           .

       9999-END.
           .     

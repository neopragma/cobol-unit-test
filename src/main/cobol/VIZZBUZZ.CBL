      ******************************************************************
      * AUTHOR:    DAVE NICOLETTE
      * DATE:      23 JAN 2015
      * PURPOSE:   "CLASSIC" COBOL CODING STYLE VERSION OF FIZZBUZZ.
      *
      * USAGE:     VIZZBUZZ FROM TO
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VIZZBUZZ.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ARGS PIC X(120).
       01  ARGUMENTS.
           05  STARTING-NUMBER    PIC 9(4).
           05  ENDING-NUMBER      PIC 9(4).

       01  BINARY.    
           05  THE-REMAINDER      PIC S9(4).
           05  THE-QUOTIENT       PIC S9(4).
           05  THE-DIVISOR        PIC S9(4).
           05  OFFSET             PIC S9(4).

       01  FILLER.
           05  CURRENT-NUMBER     PIC 9(4).
           05  CURRENT-RESULT     PIC X(12).
           05  FIZZ-OUT           PIC X(4) VALUE SPACES.
           05  BUZZ-OUT           PIC X(4) VALUE SPACES.
           05  FILLER             PIC X VALUE 'N'.
               88  TEST-88 VALUE 'Y', FALSE 'N'.

       01  RETURN-VALUE.
           05  RETURN-LINE OCCURS 1 TO 10000 
                                  DEPENDING ON ENDING-NUMBER 
                                  PIC X(60).    

       PROCEDURE DIVISION.

       0000-MAIN.

           PERFORM 0500-INITIALIZE.
           PERFORM
               VARYING CURRENT-NUMBER 
               FROM STARTING-NUMBER BY 1
               UNTIL CURRENT-NUMBER GREATER THAN ENDING-NUMBER

               PERFORM 1000-PROCESS-NUMBER
               DISPLAY 'Result for ' CURRENT-NUMBER
                       ' is ' CURRENT-RESULT
           END-PERFORM.            
           GOBACK.

       0500-INITIALIZE.
           ACCEPT ARGS FROM COMMAND-LINE.

           UNSTRING ARGS DELIMITED BY SPACE
               INTO STARTING-NUMBER, ENDING-NUMBER.

       1000-PROCESS-NUMBER.
           MOVE SPACES TO CURRENT-RESULT FIZZ-OUT BUZZ-OUT.
           MOVE 15 TO THE-DIVISOR.
           PERFORM 2000-DIVIDE.
           IF THE-REMAINDER EQUAL ZERO
               MOVE "FIZZBUZZ" TO CURRENT-RESULT.
           IF CURRENT-RESULT EQUAL SPACES
               MOVE 3 TO THE-DIVISOR
               PERFORM 2000-DIVIDE
               IF THE-REMAINDER EQUAL 0
                   MOVE "FIZZ" TO CURRENT-RESULT.
           IF CURRENT-RESULT EQUAL SPACES        
               MOVE 5 TO THE-DIVISOR
               PERFORM 2000-DIVIDE
               IF THE-REMAINDER EQUAL 0
                   MOVE "BUZZ" TO CURRENT-RESULT.
           IF CURRENT-RESULT EQUAL SPACES 
               MOVE CURRENT-NUMBER TO CURRENT-RESULT.

       2000-DIVIDE.
           DIVIDE THE-DIVISOR INTO CURRENT-NUMBER 
               GIVING THE-QUOTIENT 
               REMAINDER THE-REMAINDER. 

       9999-END.
           .
                 
   
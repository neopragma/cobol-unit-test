      ******************************************************************
      * AUTHOR:    DAVE NICOLETTE
      * DATE:      18 JUL 2014
      * PURPOSE:   FUNCTIONAL TEST DRIVER FOR CONVERT.CBL.
      *            READS SEQUENTIAL FILE WITH EXPECTED RECORDS AND 
      *            SEQUENTIAL OUTPUT FILE FROM 'CONVERT' AND COMPARES
      *            THEM RECORD BY RECORD.
      *
      * USAGE:     CONVERT-TEST EXPECTED-OUTPUT-FILE ACTUAL-OUTPUT-FILE
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONVERT-TEST.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
  
           SELECT EXPECTED-RESULT-FILE 
               ASSIGN TO EXPECTED-RESULT-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ACTUAL-RESULT-FILE 
               ASSIGN TO ACTUAL-RESULT-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  EXPECTED-RESULT-FILE.
       01  EXPECTED-RESULT-RECORD.
           COPY OUTPUT.

       FD  ACTUAL-RESULT-FILE.
       01  ACTUAL-RESULT-RECORD.
           COPY OUTPUT.

       WORKING-STORAGE SECTION.

       01  ARGS PIC X(500).

       01  ARG-VALUES.
           05  EXPECTED-RESULT-FILENAME PIC X(120).
           05  ACTUAL-RESULT-FILENAME   PIC X(120).
 
       01  EOF-EXPECTED PIC X VALUE SPACES.
           88  END-OF-EXPECTED          VALUE "Y".
       01  EOF-ACTUAL PIC X VALUE SPACES.
           88  END-OF-ACTUAL            VALUE "Y".
       01  TEST-STATUS                  PIC X(11) VALUE "     PASS: ".
           88  TEST-PASS                VALUE "     PASS: ".
           88  TEST-FAIL                VALUE "**** FAIL: ".
       01  EXPECTED-LINE-NUMBER         PIC 9(6) VALUE ZERO.
       01  ACTUAL-LINE-NUMBER           PIC 9(6) VALUE ZERO.
       01  STATUS-MESSAGE               PIC X(100) VALUE SPACES.

       PROCEDURE DIVISION.

       0000-MAIN.

           PERFORM 0500-INITIALIZE
           PERFORM 1000-COMPARE-FILES
           GOBACK
           .

       0500-INITIALIZE.

           ACCEPT ARGS FROM COMMAND-LINE END-ACCEPT
           UNSTRING ARGS DELIMITED BY SPACE
               INTO EXPECTED-RESULT-FILENAME ACTUAL-RESULT-FILENAME
           END-UNSTRING

           IF  EXPECTED-RESULT-FILENAME = SPACES 
           OR ACTUAL-RESULT-FILENAME = SPACES
               DISPLAY 'USAGE: CONVERT EXPECTED-RESULT-FILENAME' 
                   ' ACTUAL-RESULT-FILENAME'
               GOBACK
           END-IF
           .

       1000-COMPARE-FILES.
           OPEN INPUT EXPECTED-RESULT-FILE
           OPEN INPUT ACTUAL-RESULT-FILE

           DISPLAY "==================================================="
           DISPLAY " FUNCTIONAL TESTS FOR CONVERT.CBL"
           DISPLAY " EXPECTED OUTPUT FILE: " EXPECTED-RESULT-FILENAME
           DISPLAY " ACTUAL OUTPUT FILE:   " ACTUAL-RESULT-FILENAME

           PERFORM 2000-COMPARE-RECORDS
               UNTIL TEST-FAIL OR END-OF-EXPECTED OR END-OF-ACTUAL

           IF  TEST-FAIL
               DISPLAY TEST-STATUS 
               FUNCTION SUBSTITUTE
                   (STATUS-MESSAGE; "LINE"; EXPECTED-LINE-NUMBER)
           ELSE
               DISPLAY TEST-STATUS "ACTUAL RESULT MATCHES"
                   " EXPECTED RESULT"    
           END-IF

           DISPLAY "==================================================="

           CLOSE EXPECTED-RESULT-FILE
           CLOSE ACTUAL-RESULT-FILE
           .

       2000-COMPARE-RECORDS.

           IF  NOT END-OF-EXPECTED
               READ EXPECTED-RESULT-FILE
               AT END
                   SET END-OF-EXPECTED TO TRUE
               NOT AT END
                   ADD 1 TO EXPECTED-LINE-NUMBER    
               END-READ
           END-IF

           IF  NOT END-OF-ACTUAL
               READ ACTUAL-RESULT-FILE
               AT END
                   SET END-OF-ACTUAL TO TRUE
               NOT AT END
                   ADD 1 TO ACTUAL-LINE-NUMBER
               END-READ
           END-IF

           IF  END-OF-EXPECTED AND NOT END-OF-ACTUAL
               SET TEST-FAIL TO TRUE
               MOVE "MORE RECORDS THAN EXPECTED WERE WRITTEN" 
                 TO STATUS-MESSAGE
           END-IF

           IF  NOT END-OF-EXPECTED AND END-OF-ACTUAL
               SET TEST-FAIL TO TRUE
               MOVE "FEWER RECORDS THAN EXPECTED WERE WRITTEN" 
                 TO STATUS-MESSAGE
           END-IF

           IF  EXPECTED-RESULT-RECORD NOT = ACTUAL-RESULT-RECORD
               SET TEST-FAIL TO TRUE
               MOVE "FILES DO NOT MATCH STARTING AT LINE LINE" 
                 TO STATUS-MESSAGE
           END-IF
           .
 
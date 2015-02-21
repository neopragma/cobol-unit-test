      **********************************************************************
      * AUTHOR:    DAVE NICOLETTE
      * DATE:      18 JUL 2014
      * PURPOSE:   CONVERT A COMMA DELIMITED FILE INTO A FIXED FORMAT FILE.
      *
      * USAGE:     CONVERT INPUT-FILENAME OUTPUT-FILENAME
      **********************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONVERT.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT INPUT-FILE ASSIGN TO INPUT-FILENAME
           FILE STATUS IS WS-INPUT-FILE-STATUS
           ORGANIZATION IS LINE SEQUENTIAL.

       SELECT OUTPUT-FILE ASSIGN TO OUTPUT-FILENAME
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD INPUT-FILE.
       01 INPUT-RECORD PIC X(200).

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD.
          COPY OUTPUT.

       WORKING-STORAGE SECTION.

       01  ARGS PIC X(500).

       01  ARG-VALUES.
           05  INPUT-FILENAME   PIC X(120).
           05  OUTPUT-FILENAME  PIC X(120).
 
       01  WS-FILE-STATUS.
           05  WS-INPUT-FILE-STATUS  PIC 9(02).
       01  EOF      PIC X VALUE SPACES.
           88  END-OF-FILE          VALUE 'N'.
 
       01  INPUT-VALUES.
           05  TEXT-VALUE-1     PIC X(12).
           05  STATE-CODE-IN    PIC X(02).
           05  TEXT-VALUE-2     PIC X(24).
           05  DECIMAL-VALUE-1-X.
               10  DECIMAL-VALUE-1  PIC 9(3)V9(4). 

       01  TO-UPPER-CASE        PIC X(30).

       01  STATE-VALUES.
           05  FILLER           PIC X(32) VALUE "AKALASKA".
           05  FILLER           PIC X(32) VALUE "ARARKANSAS".
           05  FILLER           PIC X(32) VALUE "AZARIZONA".

       01  STATE-TABLE REDEFINES STATE-VALUES.
           05  STATE-TABLE-DATA OCCURS 3 TIMES 
                          ASCENDING KEY STATE-TABLE-CODE
                          INDEXED BY STATE-INDEX.
               10  STATE-TABLE-CODE   PIC X(02).
               10  STATE-TABLE-NAME   PIC X(30).

       PROCEDURE DIVISION.

       0000-MAIN.

           PERFORM 0500-INITIALIZE
           PERFORM 1000-PROCESS-INPUT UNTIL END-OF-FILE
           GOBACK
           .

       0500-INITIALIZE.

           ACCEPT ARGS FROM COMMAND-LINE END-ACCEPT
           UNSTRING ARGS DELIMITED BY SPACE
               INTO INPUT-FILENAME OUTPUT-FILENAME
           END-UNSTRING

           IF  INPUT-FILENAME = SPACES OR OUTPUT-FILENAME = SPACES
               DISPLAY 'USAGE: CONVERT INPUT-FILENAME OUTPUT-FILENAME'
               GOBACK
           END-IF
           .

       1000-PROCESS-INPUT.
           OPEN OUTPUT OUTPUT-FILE
           OPEN INPUT INPUT-FILE
           PERFORM UNTIL END-OF-FILE
               READ INPUT-FILE 
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       PERFORM 2000-CONVERT-RECORD
                       PERFORM 3000-WRITE-OUTPUT
               END-READ
           END-PERFORM
           CLOSE OUTPUT-FILE
           CLOSE INPUT-FILE
           .

       2000-CONVERT-RECORD.
           MOVE SPACES TO OUTPUT-RECORD
           PERFORM 2050-BREAK-OUT-DELIMITED-VALUES
           PERFORM 2100-CONVERT-TEXT-FIELD-1
           PERFORM 2200-LOOKUP-STATE-NAME
           PERFORM 2300-CONVERT-TEXT-FIELD-2
           MOVE DECIMAL-VALUE-1 TO DECIMAL-OUT-1
           .

       2050-BREAK-OUT-DELIMITED-VALUES.
           UNSTRING INPUT-RECORD DELIMITED BY ','
               INTO TEXT-VALUE-1
                   STATE-CODE-IN
                   TEXT-VALUE-2
                   DECIMAL-VALUE-1
           END-UNSTRING
           . 

       2100-CONVERT-TEXT-FIELD-1.
           IF  TEXT-VALUE-1 = LOW-VALUES
               MOVE SPACES TO TEXT-OUT-1
           ELSE
               MOVE TEXT-VALUE-1 TO TO-UPPER-CASE
               PERFORM 9000-TO-UPPER-CASE
               MOVE TO-UPPER-CASE TO TEXT-OUT-1
           END-IF    
           . 

       2200-LOOKUP-STATE-NAME.
           MOVE STATE-CODE-IN TO TO-UPPER-CASE
           PERFORM 9000-TO-UPPER-CASE
           MOVE TO-UPPER-CASE TO STATE-CODE-IN

           SEARCH ALL STATE-TABLE-DATA
               AT END
                   MOVE SPACES TO STATE-NAME-OUT
               WHEN STATE-TABLE-CODE (STATE-INDEX) = STATE-CODE-IN
                   MOVE STATE-TABLE-NAME (STATE-INDEX) TO STATE-NAME-OUT
           END-SEARCH
           .

       2300-CONVERT-TEXT-FIELD-2.
           IF  TEXT-VALUE-2 = LOW-VALUES
               MOVE SPACES TO TEXT-VALUE-2
           ELSE    
               CALL "C$TOUPPER" 
                   USING TEXT-VALUE-2
                   BY VALUE 
                   LENGTH 1
               END-CALL

               CALL "C$JUSTIFY" 
                   USING TEXT-VALUE-2
                   "C"
               END-CALL
           END-IF    

           MOVE TEXT-VALUE-2 TO TEXT-OUT-2
           . 

       3000-WRITE-OUTPUT.
           WRITE 
               OUTPUT-RECORD FROM OUTPUT-RECORD
           END-WRITE
           .

       9000-TO-UPPER-CASE.
           CALL "C$TOUPPER" 
               USING TO-UPPER-CASE
               BY VALUE 
               LENGTH TO-UPPER-CASE
           END-CALL
           .     

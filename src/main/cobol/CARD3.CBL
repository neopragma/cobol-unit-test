      **********************************************************************
      * AUTHOR:    DAVE NICOLETTE
      * DATE:      13 APR 2017
      * PURPOSE:   CREDIT CARD IDENTIFIER - BEFORE REFACTORING.
      **********************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CARD3.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT INPUT-FILE ASSIGN TO INPUT-FILENAME
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-INPUT-STATUS.       

       SELECT OUTPUT-FILE ASSIGN TO OUTPUT-FILENAME
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-OUTPUT-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD INPUT-FILE.
       01 INPUT-RECORD PIC X(200).

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD PIC X(200).      

       WORKING-STORAGE SECTION.

       01  ARGS PIC X(500).

       01  ARG-VALUES.
           05  INPUT-FILENAME PIC X(120).
           05  OUTPUT-FILENAME   PIC X(120).

       01  WS-FILE-STATUS.
           05  WS-INPUT-STATUS        PIC X(02).
               88  INPUT-OK           VALUE '00'.
               88  INPUT-EOF          VALUE '10'.
           05  WS-OUTPUT-STATUS       PIC X(02).
               88  OUTPUT-OK          VALUE '00'.
        01 WS-ALL-FILE-STATUS REDEFINES WS-FILE-STATUS PIC X(04).
               88  FILES-OK           VALUE '0000'.

       01  WS-RECORD-AREAS.
           05  WS-INPUT-RECORD.
               10  FILLER             PIC X(10).
               10  IN-ACCOUNT-NUMBER  PIC X(16).
               10  FILLER             PIC X(174).
           05  WS-OUTPUT-RECORD.
               10  OUT-ACCOUNT-NUMBER PIC X(16).
               10  OUT-MESSAGE        PIC X(184).    

       01  WS-MESSAGE.
           05  FILLER                 PIC X(04) VALUE ' IS '.  
           05  WS-CARD-TYPE           PIC X(60). 
               88  UNKNOWN-CARD          VALUE 'UNKNOWN'.
               88  AMERICAN-EXPRESS-CARD VALUE 'AMERICAN EXPRESS'.   
               88  DISCOVER-CARD         VALUE 'DISCOVER'.
               88  MASTERCARD-CARD       VALUE 'MASTERCARD'.
               88  VISA-CARD             VALUE 'VISA'.
               88  DINERS-CLUB-CARD      VALUE 'DINERS CLUB'.

       PROCEDURE DIVISION.
          
           PERFORM 9100-OPEN-FILES
           IF INPUT-OK
               PERFORM 2000-IDENTIFY-CARD-TYPES UNTIL INPUT-EOF
               PERFORM 9400-CLOSE-FILES
           END-IF    
           . 

       2000-IDENTIFY-CARD-TYPES.
           PERFORM 9200-READ-INPUT-FILE.
           IF FILES-OK
               PERFORM 2200-IDENTIFY-CARD-TYPE.
           PERFORM 9300-WRITE-OUTPUT-FILE.

       2200-IDENTIFY-CARD-TYPE.

           SET UNKNOWN-CARD TO TRUE. 
           IF IN-ACCOUNT-NUMBER(1:2) = '34'
           OR IN-ACCOUNT-NUMBER(1:2) = '37'
               SET AMERICAN-EXPRESS-CARD TO TRUE
               PERFORM 2290-POPULATE-MESSAGE
           ELSE        
               IF IN-ACCOUNT-NUMBER(1:2) = '65'
               OR IN-ACCOUNT-NUMBER(1:4) = '6011'
                   SET DISCOVER-CARD TO TRUE
                   PERFORM 2290-POPULATE-MESSAGE
               ELSE       
                   IF IN-ACCOUNT-NUMBER(1:2) >= '51'
                   AND IN-ACCOUNT-NUMBER(1:2) <= '55'
                       SET MASTERCARD-CARD TO TRUE
                       PERFORM 2290-POPULATE-MESSAGE
                   ELSE       
                       IF IN-ACCOUNT-NUMBER(1:1) = '4'
                           SET VISA-CARD TO TRUE
                           PERFORM 2290-POPULATE-MESSAGE
                       ELSE    
                           IF IN-ACCOUNT-NUMBER(1:2) = '36'
                           OR IN-ACCOUNT-NUMBER(1:2) = '38'
                               SET DINERS-CLUB-CARD TO TRUE
                               PERFORM 2290-POPULATE-MESSAGE
                           ELSE       
                               PERFORM 2290-POPULATE-MESSAGE.

       2290-POPULATE-MESSAGE.    
           MOVE IN-ACCOUNT-NUMBER TO OUT-ACCOUNT-NUMBER.
           MOVE WS-MESSAGE TO OUT-MESSAGE.

           DISPLAY 'OUT-ACCOUNT-NUMBER ' OUT-ACCOUNT-NUMBER.
           DISPLAY 'WS-MESSAGE ' WS-MESSAGE.
           DISPLAY 'WS-CARD-TYPE ' WS-CARD-TYPE.

       9100-OPEN-FILES.

           ACCEPT ARGS FROM COMMAND-LINE END-ACCEPT
           UNSTRING ARGS DELIMITED BY SPACE
               INTO INPUT-FILENAME OUTPUT-FILENAME
           END-UNSTRING

           IF  INPUT-FILENAME = SPACES 
           OR OUTPUT-FILENAME = SPACES
               DISPLAY 'USAGE: CARD1 INPUT-FILENAME' 
                   ' OUTPUT-FILENAME'
               GOBACK
           END-IF

           OPEN INPUT INPUT-FILE 
           OPEN OUTPUT OUTPUT-FILE
           .

       9200-READ-INPUT-FILE.
           READ INPUT-FILE
           .    

       9300-WRITE-OUTPUT-FILE.
           WRITE OUTPUT-RECORD FROM WS-OUTPUT-RECORD
           .    

       9400-CLOSE-FILES.
           CLOSE OUTPUT-FILE
           CLOSE INPUT-FILE
           .

       9999-END.
           .
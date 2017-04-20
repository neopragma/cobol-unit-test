      **********************************************************************
      * AUTHOR:    DAVE NICOLETTE
      * DATE:      13 APR 2017
      * PURPOSE:   CREDIT CARD IDENTIFIER - AFTER REFACTORING.
      **********************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CARD2.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-MESSAGE.
           05  FILLER                 PIC X(04) VALUE ' IS '.  
           05  WS-CARD-TYPE           PIC X(60).    

       LINKAGE SECTION.

       01  WS-PARAMETERS.
           05  WS-ARGUMENTS.
               10  FILLER             PIC X(10).
               10  IN-ACCOUNT-NUMBER  PIC X(16).
               10  FILLER             PIC X(174).
           05  WS-RETURN-VALUES.
               10  OUT-ACCOUNT-NUMBER PIC X(16).
               10  OUT-MESSAGE        PIC X(184).    

       PROCEDURE DIVISION USING WS-PARAMETERS.

           PERFORM 2200-IDENTIFY-CARD-TYPE
           MOVE ZERO TO RETURN-CODE
           GOBACK
           .

       2200-IDENTIFY-CARD-TYPE.

           EVALUATE TRUE
               WHEN IN-ACCOUNT-NUMBER(1:1) IS EQUAL TO '4'
                   MOVE 'VISA' TO WS-CARD-TYPE
               WHEN IN-ACCOUNT-NUMBER(1:2) IS >= '51' 
               AND IN-ACCOUNT-NUMBER(1:2) IS < '56' 
                   MOVE 'MASTERCARD' TO WS-CARD-TYPE
               WHEN IN-ACCOUNT-NUMBER(1:2) = '36'
               WHEN IN-ACCOUNT-NUMBER(1:2) = '38'
                   MOVE 'DINERS CLUB' TO WS-CARD-TYPE
               WHEN IN-ACCOUNT-NUMBER(1:4) = '6011'
               WHEN IN-ACCOUNT-NUMBER(1:2) = '65'    
                   MOVE 'DISCOVER' TO WS-CARD-TYPE 
               WHEN IN-ACCOUNT-NUMBER(1:2) = '34'
               WHEN IN-ACCOUNT-NUMBER(1:2) = '37'
                   MOVE 'AMERICAN EXPRESS' TO WS-CARD-TYPE
               WHEN IN-ACCOUNT-NUMBER(1:2) = '35'
                   MOVE 'JAPAN CREDIT BUREAU' TO WS-CARD-TYPE    
               WHEN OTHER   
                   MOVE 'UNKNOWN' TO WS-CARD-TYPE
           END-EVALUATE

           MOVE IN-ACCOUNT-NUMBER TO OUT-ACCOUNT-NUMBER
           MOVE WS-MESSAGE TO OUT-MESSAGE
           .    

       9999-END.
           .
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  ZUTZCPC.
      *****************************************************************
      * PRECOMPILER TO COPY A SOURCE FILE TO A TEMPORARY TEST FILE AND
      * INSERT UNIT TEST CODE INTO THE COPY.
      *****************************************************************       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ORIGINAL-SOURCE
               ASSIGN TO EXTERNAL SRCPRG
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT UNIT-TEST-CONFIG
               ASSIGN TO EXTERNAL UTSTCFG
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TEST-SOURCE
               ASSIGN TO EXTERNAL TESTPRG
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  UNIT-TEST-CONFIG
           DATA RECORD IS UNIT-TEST-CONFIG-REC.
       01  UNIT-TEST-CONFIG-REC.
           05  COPYBOOK-NAME   PIC X(12).
       FD  ORIGINAL-SOURCE
           DATA RECORD IS ORIGINAL-LINE.
       01  ORIGINAL-LINE.
           05  FILLER                    PIC X(07).
           05  SECTION-HEADER            PIC X(04).
               88  WORKING-STORAGE-HEADER    VALUE 'WORK'.
               88  PROCEDURE-DIVISION-HEADER VALUE 'PROC'.
           05  FILLER                    PIC X(69).
       FD  TEST-SOURCE
           DATA RECORD IS TEST-LINE.
       01  TEST-LINE                     PIC X(80).
       WORKING-STORAGE SECTION.
       01  FILLER                        PIC X VALUE SPACE.
           88  END-OF-FILE                     VALUE 'Y'.   
       01  FILLER.
           05  WS-WORKING-STORAGE-HEADER PIC X(4) VALUE
               'WORK'.
           05  WS-PROCEDURE-DIVISION-HEADER PIC X(4) VALUE
               'PROC'.    
           05  WS-WORKING-STORAGE-COPY   PIC X(12).
           05  WS-PROCEDURE-COPY         PIC X(12).
       01  WS-COPY-LINE.
           05  FILLER                    PIC X(17)
                                         VALUE '            COPY '.
           05  WS-COPY-NAME              PIC X(12).
           05  FILLER                    PIC X(51) VALUE '.'.                         
       LINKAGE SECTION.
       PROCEDURE DIVISION.
           PERFORM 0100-INITIALIZE
           PERFORM 1000-PROCESS-INPUT
           GOBACK
           .

       0100-INITIALIZE.
           OPEN INPUT UNIT-TEST-CONFIG
           READ UNIT-TEST-CONFIG
           MOVE COPYBOOK-NAME TO WS-WORKING-STORAGE-COPY
           READ UNIT-TEST-CONFIG
           MOVE COPYBOOK-NAME TO WS-PROCEDURE-COPY
           CLOSE UNIT-TEST-CONFIG
           .

       1000-PROCESS-INPUT.
           OPEN INPUT ORIGINAL-SOURCE
           OPEN OUTPUT TEST-SOURCE
           PERFORM UNTIL END-OF-FILE
               READ ORIGINAL-SOURCE
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END    
                       PERFORM 2000-INSERT-TEST-CODE
               END-READ
           END-PERFORM
           CLOSE TEST-SOURCE
           CLOSE ORIGINAL-SOURCE
           .

       2000-INSERT-TEST-CODE.
           WRITE 
               TEST-LINE FROM ORIGINAL-LINE
           END-WRITE

           EVALUATE TRUE
               WHEN WORKING-STORAGE-HEADER
                    MOVE WS-WORKING-STORAGE-COPY TO WS-COPY-NAME
                    PERFORM 3000-WRITE-COPY-LINE
               WHEN PROCEDURE-DIVISION-HEADER
                    MOVE WS-PROCEDURE-COPY TO WS-COPY-NAME        
                    PERFORM 3000-WRITE-COPY-LINE
           END-EVALUATE         
           .
 
       3000-WRITE-COPY-LINE.
           WRITE 
               TEST-LINE FROM WS-COPY-LINE
           END-WRITE
           .

       9999-END.
           .

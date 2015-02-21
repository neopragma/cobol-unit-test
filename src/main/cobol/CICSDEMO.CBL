      **********************************************************************
      * AUTHOR:    DAVE NICOLETTE
      * DATE:      10 JAN 2015
      * PURPOSE:   DEMONSTRATE MOCKING FEATURES OF ZUTZCPC.
      **********************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CICSDEMO.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-RECORD.
           05  WS-KEY        PIC X(5).
           05  WS-FIELD-2    PIC X(5).
           05  WS-FIELD-3    PIC X(5).
           05  WS-FIELD-4    PIC X(5).

       LINKAGE SECTION.
           COPY DFHEIBLK.
       01  DFHCOMMAREA PIC X.    

       PROCEDURE DIVISION USING DFHEIBLK, DFHCOMMAREA.

       0100-READ-DATASET.

           EXEC CICS READ 
                DATASET('MYFILE') 
                RIDFLD('AAAAA') 
                INTO(WS-RECORD)
           END-EXEC    
           .

       0200-WRITE-DATASET.

           EXEC CICS WRITE 
                DATASET('YOURFILE') 
                RIDFLD('AAAAA') 
                FROM(WS-RECORD)
           END-EXEC    
           .

       9999-END.
           .
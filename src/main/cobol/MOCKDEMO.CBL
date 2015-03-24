      **********************************************************************
      * AUTHOR:    DAVE NICOLETTE
      * DATE:      10 JAN 2015
      * PURPOSE:   DEMONSTRATE MOCKING FEATURES OF ZUTZCPC.
      **********************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MOCKDEMO.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT INPUT-FILE ASSIGN TO INPUT-FILENAME
           ORGANIZATION IS LINE SEQUENTIAL       
           FILE STATUS IS WS-INPUT-FILE-STATUS.

       SELECT OUTPUT-FILE ASSIGN TO OUTPUT-FILENAME
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD INPUT-FILE.
       01 INPUT-RECORD PIC X(200).

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD PIC X(200).      

       WORKING-STORAGE SECTION.

       01  WS-FILE-STATUS.
           05  WS-INPUT-FILE-STATUS   PIC X(02).
           05  WS-OUTPUT-FILE-STATUS  PIC X(02).
           05  WS-OTHER-FILE-STATUS   PIC X(02).

       01  WS-RECORD-AREAS.
           05  WS-INPUT-RECORD        PIC X(200).
           05  WS-OUTPUT-RECORD       PIC X(200).    

       01  WS-TEST-AREAS.
           05  WS-FIELD-A             PIC X(04) VALUE 'ORIG'.    
           05  WS-FIELD-B             PIC X(04) VALUE 'ORIG'.    

       PROCEDURE DIVISION.
           

       0100-OPEN-INPUT.
           OPEN INPUT INPUT-FILE  
           .

       0200-READ-INPUT-FILE.
           READ INPUT-FILE
           .    

       1000-PARA-A.
           MOVE 'A1' TO WS-FIELD-A
           .

       2000-PARA-B.
           MOVE 'B1' TO WS-FIELD-B
           .


       9999-END.
           .
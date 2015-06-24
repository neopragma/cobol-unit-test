      **********************************************************************
      * AUTHOR:    DAVE NICOLETTE
      * DATE:      30 JUN 2015
      * PURPOSE:   DEMONSTRATE FILE MOCKING FEATURES OF ZUTZCPC.
      **********************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILEDEMO.
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

       PROCEDURE DIVISION.           

       0100-OPEN-INPUT.
           OPEN INPUT INPUT-FILE  
           .

       0200-READ-INPUT-FILE.
           READ INPUT-FILE
           .    

       9999-END.
           .
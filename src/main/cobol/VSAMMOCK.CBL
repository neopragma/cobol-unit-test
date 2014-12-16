       IDENTIFICATION DIVISION.
       PROGRAM-ID.  VSAMMOCK.
      *****************************************************************
      * MOCK VSAM FILE ACCESS FOR ISOLATED UNIT TESTING.              *
      *****************************************************************       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  FILE-METADATA.
           05  METADATA OCCURS 10 INDEXED BY METADATA-INDEX.
               10  FILE-NAME              PIC  X(44).
               10  FILE-TYPE              PIC  X(01).
               10  FILE-ACCESS            PIC  X(01).
               10  FILE-RECORD-TYPE       PIC  X(01).
               10  FILE-KEY-LENGTH        PIC S9(08) COMP.
               10  FILE-STATE             PIC  X(02).
       LINKAGE SECTION.
       01  VMF-BLOCK.
           COPY VMFBLOCK.
       PROCEDURE DIVISION USING VMF-BLOCK.


           DISPLAY '**** IN VSAMMOCK ****'

           EVALUATE TRUE
               WHEN VMF-ACTION-INITIALIZE
                    PERFORM 0100-INITIALIZE
           END-EVALUATE

           GOBACK
           .

       0100-INITIALIZE.
           INITIALIZE FILE-METADATA
           .             

       9999-END.
           .

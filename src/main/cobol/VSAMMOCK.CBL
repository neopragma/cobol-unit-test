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
               10  FILE-ACTION            PIC  X(02).
               10  FILE-STATUS            PIC  X(02).
               10  FILE-RECORD            PIC  X(8192).
       LINKAGE SECTION.
       01  VMF-BLOCK.
           COPY VMFBLOCK.
       PROCEDURE DIVISION USING VMF-BLOCK.


           DISPLAY '**** IN VSAMMOCK ****'

           EVALUATE TRUE
               WHEN VMF-FUNCTION-INITIALIZE
                    PERFORM 0100-INITIALIZE
           END-EVALUATE

           GOBACK
           .

       0100-INITIALIZE.
           INITIALIZE FILE-METADATA
           .             

       9999-END.
           .

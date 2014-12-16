       IDENTIFICATION DIVISION.
       PROGRAM-ID.  VSAMTEST.
      *****************************************************************
      * UNIT TEST DRIVER FOR VSAMMOCK SUBPROGRAM.                     *
      *****************************************************************       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  VMF-BLOCK.
           COPY VMFBLOCK.
       PROCEDURE DIVISION.


       0100-INVOKE-VSAMMOCK.
           CALL 
               'VSAMMOCK' USING VMF-BLOCK
           END-CALL    


           DISPLAY '**** RETURNED FROM VSAMMOCK ****'
           .             

       9999-END.
           .

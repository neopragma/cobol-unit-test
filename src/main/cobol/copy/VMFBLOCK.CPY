      *****************************************************************
      * VMFBLOCK
      * VSAM MOCKING FRAMEWORK CALL BLOCK
      *****************************************************************
           05  FILLER                    PIC  X(08) VALUE 'VMFBLOCK'. 
           05  VMF-RETURN-CODE                PIC  X(02) VALUE SPACES.
               88  VMF-RETURN-CODE-UNDEFINED  VALUE SPACES.
               88  VMF-OK                     VALUE '00'.
               88  VMF-UNKNOWN-ACTION         VALUE '01'.
               88  VMF-UNKNOWN-FILE-TYPE      VALUE '02'.
               88  VMF-UNKNOWN-ACCESS         VALUE '03'.
               88  VMF-UNKNOWN-RECORD-TYPE    VALUE '04'.
               88  VMF-MISSING-FILE-NAME      VALUE '05'.
               88  VMF-MISSING-KEY-VALUE      VALUE '06'.
               88  VMF-INTERNAL-ERROR         VALUE '99'.
           05  VMF-ACTION                     PIC  X(02) VALUE SPACES.
               88  VMF-ACTION-UNDEFINED       VALUE SPACES.
               88  VMF-ACTION-INITIALIZE      VALUE '00'.
               88  VMF-ACTION-OPEN-INPUT      VALUE '01'.
               88  VMF-ACTION-OPEN-OUTPUT     VALUE '02'.
               88  VMF-ACTION-OPEN-IO         VALUE '03'.
               88  VMF-ACTION-OPEN-EXTEND     VALUE '04'.
               88  VMF-ACTION-CLOSE           VALUE '09'.
               88  VMF-ACTION-READ-SEQUENTIAL VALUE '11'.
               88  VMF-ACTION-READ-RANDOM     VALUE '12'.
               88  VMF-ACTION-WRITE           VALUE '21'.
               88  VMF-ACTION-REWRITE         VALUE '22'.
               88  VMF-ACTION-DELETE          VALUE '23'.
           05  VMF-FILE-TYPE                  PIC  X(01) VALUE SPACES.
               88  VMF-FILE-TYPE-UNDEFINED    VALUE SPACES.
               88  VMF-ESDS                   VALUE 'E'.
               88  VMF-KSDS                   VALUE 'K'.
               88  VMF-LDS                    VALUE 'L'.
               88  VMF-RRDS                   VALUE 'R'.
           05  VMF-ACCESS                     PIC  X(01) VALUE SPACES.
               88  VMF-ACCESS-UNDEFINED       VALUE SPACES.
               88  VMF-ACCESS-RANDOM          VALUE 'R'.   
               88  VMF-ACCESS-SEQUENTIAL      VALUE 'S'. 
           05  VMF-RECORD-TYPE                PIC  X(01) VALUE SPACES.
               88  VMF-RECORD-TYPE-UNDEFINED  VALUE SPACES.
               88  VMF-RECORD-TYPE-FIXED      VALUE 'F'.
               88  VMF-RECORD-TYPE-VARIABLE   VALUE 'V'.    
           05  VMF-FILE-STATE                 PIC  X(01) VALUE SPACES.
               88  VMF-FILE-STATE-UNDEFINED   VALUE SPACES.
               88  VMF-STATE-CLOSED           VALUE '0'.
               88  VMF-STATE-OPEN             VALUE '1' '2' '3'.
               88  VMF-STATE-OPEN-INPUT       VALUE '1'.
               88  VMF-STATE-OPEN-OUTPUT      VALUE '2'.
               88  VMF-STATE-OPEN-IO          VALUE '3'.
               88  VMF-STATE-OPEN-EXTEND      VALUE '4'.
               88  VMF-STATE-ERROR            VALUE '9'.
           05  FILLER                    PIC  X(08) VALUE 'FILENAME'.               
           05  VMF-FILE-NAME                  PIC  X(44).   
           05  FILLER                    PIC  X(08) VALUE 'KEYVALUE'.               
           05  VMF-KEY-VALUE                  PIC  X(44). 
           05  FILLER                    PIC  X(08) VALUE 'KEYLENG '.
           05  VMF-KEY-LENGTH                 PIC S9(08) COMP.
      * RETURNED TO CALLER     
           05  FILLER                    PIC  X(08) VALUE 'VSAMSTAT'.    
           05  VMF-VSAM-STATUS                PIC  X(02) VALUE SPACES.
               88  VMF-FILE-STATUS-UNDEFINED     VALUE SPACES.
               88  VMF-STATUS-OK                 VALUE '00' '97'.
               88  VMF-STATUS-DUPLICATE-KEY      VALUE '02' '22'.
               88  VMF-STATUS-WRONG-LENGTH-REC   VALUE '04'.
               88  VMF-STATUS-FILE-NOT-FOUND     VALUE '05' '35'.
               88  VMF-STATUS-END-OF-FILE        VALUE '10'.
               88  VMF-STATUS-INVALID-KEY        VALUE '20'.
               88  VMF-STATUS-SEQUENCE-ERROR     VALUE '21'.
               88  VMF-STATUS-RECORD-NOT-FOUND   VALUE '23'.
               88  VMF-STATUS-LOGIC-ERROR        VALUE '39' '92'.
               88  VMF-STATUS-ALREADY-OPEN       VALUE '41'.
               88  VMF-STATUS-ALREADY-CLOSED     VALUE '42'.
               88  VMF-STATUS-NO-REC-TO-DELETE   VALUE '43'.
               88  VMF-STATUS-SEQ-READ-NO-POS    VALUE '46'.
               88  VMF-STATUS-NOT-OPEN-FOR-READ  VALUE '47'.
               88  VMF-STATUS-NOT-OPEN-FOR-WRITE VALUE '48'.
               88  VMF-STATUS-NOT-OPEN-FOR-REWR  VALUE '49'.
               88  VMF-STATUS-READ-AFTER-EOF     VALUE '94'.
               88  VMF-STATUS-MISSING-DD         VALUE '96'.
               88  VMF-STATUS-INTEGRITY-VERIFIED VALUE '97'.
           05  FILLER                    PIC  X(08) VALUE 'EXTNSTAT'.    
           05  VMF-VSAM-EXTENDED-STATUS       COMP.
               10  VMF-VSAM-RETURN-CODE       PIC S9(04).
                   88  VMF-EXT-OK             VALUE 0.
                   88  VMF-EXT-REQUEST-ACTIVE VALUE 4.
                   88  VMF-EXT-LOGICAL-ERROR  VALUE 8.
                   88  VMF-EXT-PHYSICAL-ERROR VALUE 12.
               10  VMF-VSAM-FUNCTION-CODE     PIC S9(04).
                   88  VMF-BASE-CLUSTER-OK    VALUE 0.
                   88  VMF-BASE-CLUSTER-ERROR VALUE 1.
                   88  VMF-ALT-INDEX-OK       VALUE 2.
                   88  VMF-ALT-INDEX-ERROR    VALUE 3.
                   88  VMF-UPGRADE-OK         VALUE 4.
                   88  VMF-UPGRADE-ERROR      VALUE 5.
      *            VALUES DEPEND ON VMF-VSAM-RETURN-CODE                   
               10  VMF-VSAM-FEEDBACK-CODE     PIC S9(04).
           05  FILLER                    PIC  X(08) VALUE '=RECORD='.
           05  VMF-RECORD                PIC  X(8192).

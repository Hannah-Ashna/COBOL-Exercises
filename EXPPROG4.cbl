       ID DIVISION.
       PROGRAM-ID. EXPPROG4.
       AUTHOR. HANNAH JACOB.
       INSTALLATION. RSM.
       DATE-WRITTEN. 23RD JULY 2021.
       DATE-COMPILED.
      *
       FILE-CONTROL.
           SELECT VOTESINPUT    ASSIGN TO FILEINPUT
           FILE STATUS IS WS-INPUT-STATUS.
           SELECT RESULTSOUTPUT ASSIGN TO FILEOUTPUT
           FILE STATUS IS WS-OUTPUT-STATUS.

       DATA DIVISION.
      *
       FILE SECTION.
      *------------------------------------------
      * VOTES INPUT FILE
      *------------------------------------------
       FD VOTESINPUT BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01 VOTES-RECORD.
           03 CONSTITUENCY-ID  PIC X(4).
           03 VOTE-VALUE OCCURS 72 TIMES INDEXED BY VOTE-ID PIC 9(1).
           03 SPACE-FILLER     PIC X(4).

      *------------------------------------------
      * RESULTS OUTPUT FILE
      *------------------------------------------
       FD RESULTSOUTPUT BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01 RESULTS-RECORD       PIC X(80).

      *------------------------------------------
      * WORKING STORAGE SECTION
      *------------------------------------------
       WORKING-STORAGE SECTION.

       01 WS-INPUT-STATUS      PIC X(2).
           88 INPUT-OK         VALUE "00".
           88 INPUT-EOF        VALUE "10".
           88 INPUT-VALID      VALUE "00", "10".
       
       01 WS-OUTPUT-STATUS     PIC X(2).
           88 OUTPUT-OK        VALUE "00".
       
       01 WS-REC-VALID         PIC X(1).
           88 REC-VALID        VALUE "Y".
           88 REC-INVALID      VALUE "N".

       01 WS-RLP-COUNT         PIC 9(4).
       01 WS-CON-COUNT         PIC 9(4).
       01 WS-PC-COUNT          PIC 9(4).
       01 WS-FC-COUNT          PIC 9(4).
       01 WS-LAB-COUNT         PIC 9(4).
       01 WS-LIB-COUNT         PIC 9(4).
       01 WS-SC-COUNT          PIC 9(4).
       01 WS-SPOILT-COUNT      PIC 9(4).

       01 WS-OUTPUT-DATA.
           03 PARTY-NAME       PIC X(20).
           03 FILLER           PIC X(30) VALUE " ".
           03 PARTY-VOTES      PIC 9(4).
           03 FILLER           PIC X(5)  VALUE " ".
           03 PARTY-WINNER     PIC X(3).
           03 FILLER           PIC X(18) VALUE " ".

       01 WS-WIN-STATUS        PIC X(3)  VALUE "***".
 
      *------------------------------------------
      * LINKAGE SECTION
      *------------------------------------------
       LINKAGE SECTION.
       
       01 INPUT-PARM           PIC X(4).

      *------------------------------------------
      * Program Logic
      *------------------------------------------
       PROCEDURE DIVISION USING INPUT-PARM.

       A000-MAIN-LOGIC         SECTION.
           DISPLAY "Status - Program Starting..."
           PERFORM B000-INIT-CODE
           PERFORM C000-PROCESS UNTIL INPUT-EOF
      *    PERFORM MAIN LOGIC FOR THE LOOP HERE     
      *    PERFORM X000-WRITE-OUTPUT
           PERFORM E000-CLOSE-FILE
           DISPLAY "Status - Program Complete"
           STOP RUN.
       
       B000-INIT-CODE          SECTION.
           OPEN INPUT  VOTESINPUT
           OPEN OUTPUT RESULTSOUTPUT
           DISPLAY "Status - Files Opened"

           PERFORM C000-READ-FILE
           .
       
       C000-PROCESS            SECTION.
           PERFORM F000-CHECK-CONST

           IF REC-VALID
               PERFORM G000-VALIDATE-VOTE
               PERFORM D000-READ-FILE
           ELSE
               PERFORM D000-READ-FILE
           END-IF
           .

       D000-READ-FILE          SECTION.
           READ VOTESINPUT
           DISPLAY "New Record: " VOTES-RECORD
           .

       E000-CLOSE-FILE         SECTION.
           CLOSE   VOTESINPUT
                   RESULTSOUTPUT
           
           DISPLAY "Status - Files Closed"
           .
       
       F000-CHECK-CONST        SECTION.
           SET REC-VALID TO TRUE

           EVALUATE CONSTITUENCY-ID
               WHEN INPUT-PARM
                   DISPLAY "Status - Valid Constituency"
                   
               WHEN OTHER
                   DISPLAY "Status - Invalid Constituency"
                   SET REC-INVALID TO FALSE
           END-EVALUATE
           .

       G000-VALIDATE-VOTE      SECTION.
           EVALUATE VOTE-VALUE
               WHEN 0
                   DISPLAY "+1 Vote for Raving Loony Party"
                   ADD 1 TO WS-RLP-COUNT
               WHEN 1
                   DISPLAY "+1 Vote for Conservative"
                   ADD 1 TO WS-CON-COUNT
               WHEN 2
                   DISPLAY "+1 Vote for Plaid Cymru"
                   ADD 1 TO WS-PC-COUNT                          
               WHEN 3
                   DISPLAY "+1 Vote for Free Cleethorpes"
                   ADD 1 TO WS-FC-COUNT
               WHEN 4
                   DISPLAY "+1 Vote for Labour"
                   ADD 1 TO WS-LAB-COUNT
               WHEN 5
                    DISPLAY "+1 Vote for Liberal"
                    ADD 1 TO WS-LIB-COUNT                        
               WHEN 6
                   DISPLAY "+1 Vote for Scottish National"
                   ADD 1 TO WS-SC-COUNT    
               WHEN OTHER
                   DISPLAY "+1 for Spoilt Vote"
                   ADD 1 TO WS-SPOILT-COUNT
           END-EVALUATE
           .

       W000-WRITE-lOSER        SECTION.
      *    PUT FORMATTING HERE?
           WRITE RESULTS-RECORD

           IF NOT OUTPUT-OK
               DISPLAY "Error - Cannot write to Output File"
           ENDIF
           .
       
       W001-WRITE-WINNER       SECTION.
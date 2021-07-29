       ID DIVISION.
       PROGRAM-ID. EXPPROG5.
       AUTHOR. HANNAH JACOB.
       DATE-WRITTEN. 30TH JULY 2021.
       DATE-COMPILED.
      
      *---------------------
       ENVIRONMENT DIVISION.
      
      *---------------------
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT FILE-INPUT   ASSIGN TO FILEIN
           FILE STATUS IS WS-FILE-STATUS.
           SELECT FILE-OUTPUT  ASSIGN TO FILEOUT.
       DATA DIVISION.
      *
       FILE SECTION.
      *
       FD FILE-INPUT BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01 INPUT-RECORDS.
           03 ACCOUNT-NUM          PIC 9(5).
           03 INITIAL-VAL          PIC X(1).
           03 SURNAME              PIC X(20).
           03 MARKER               PIC X(1).
           03 TRANS-AMOUNT         PIC 9(5).
           03 TRANS-DATE           PIC 9(8).
           03 TRANS-DETAILS        PIC X(20).
           03 FILLER               PIC X(20).
      
       FD FILE-OUTPUT BLOCK CONTAINS 0 RECORDS
            RECORDING MODE IS F.
       01 OUTPUT-RECORDS.
           03 ACCOUNT-NUM          PIC 9(5).
           03 MARKER               PIC X(1).
           03 TRANS-AMOUNT         PIC S9(5) COMP-3.
           03 TRANS-DATE           PIC 9(8).
           03 INITIAL-VAL          PIC X(1).
           03 SURNAME              PIC X(20).
           03 NOT-USED             PIC X(40) VALUE SPACES. 
        
       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS           PIC X(2).
           88 FILE-OK              VALUE "00".
           88 FILE-EOF             VALUE "01".
           88 FILE-VALID           VALUE "00", "01".
      
      *---------------------
       PROCEDURE DIVISION.
       A100-MAIN-LOGIC             SECTION.
           DISPLAY "STATUS - STARTING PROGRAM"
           PERFORM B100-INIT
      * DO STUFF HERE???
           PERFORM T100-TERMINATE
           DISPLAY "STATUS - PROGRAM DONE"
           STOP RUN
           .
      
       B100-INIT                   SECTION.
           OPEN INPUT   FILE-INPUT
                OUTPUT  FILE-OUTPUT
          
           IF FILE-OK
               DISPLAY "STATUS - FILES OK"
           ELSE
               DISPLAY "ERROR - COULD NOT OPEN FILE"
           END-IF
      
           DISPLAY "STATUS - FILES OPENED"
           .
      
       T100-TERMINATE              SECTION.
           CLOSE FILE-INPUT
                 FILE-OUTPUT
      
           DISPLAY "STATUS - FILES CLOSED"
           .

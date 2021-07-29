 ID DIVISION.
       PROGRAM-ID. EXPPROG5.
       AUTHOR. HANNAH JACOB.
       DATE-WRITTEN. 30TH JULY 2021.
       DATE-COMPILED.
      *
       ENVIRONMENT DIVISION.
      *
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
           03 ACCOUNT-NUM       PIC 9(5).
           03 INITIAL-VAL       PIC X(1).
           03 SURNAME           PIC X(20).
           03 MARKER            PIC X(1).
           03 TRANS-AMOUNT      PIC 9(5).
           03 TRANS-DATE        PIC 9(8).
           03 TRANS-DETAILS     PIC X(20).
           03 FILLER            PIC X(20).
      
      FD FILE-OUTPUT BLOCK CONTAINS 0 RECORDS
          RECORDING MODE IS F.
      01 OUTPUT-RECORDS.
           03 ACCOUNT-NUM       PIC 9(5).
           03 MARKER            PIC X(1).
           03 TRANS-AMOUNT      PIC S9(5) COMP-3.
           03 TRANS-DATE        PIC 9(8).
           03 INITIAL-VAL       PIC X(1).
           03 SURNAME           PIC X(20).
           03 NOT-USED          PIC X(40) VALUE SPACES. 

           03 TRANS-DETAILS     PIC X(20).
           03 FILLER            PIC X(20). 
     *
      PROCEDURE DIVISION.
      A100-MAIN-LOGIC           SECTION.
     * DO STUFF HERE???

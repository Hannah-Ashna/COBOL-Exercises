       ID DIVISION.
       PROGRAM-ID. EXPPROG4.
       AUTHOR. HANNAH JACOB.
       INSTALLATION. RSM.
       DATE-WRITTEN. 23RD JULY 2021.
       DATE-COMPILED.
      *
       FILE-CONTROL.
           SELECT VOTESINPUT    ASSIGN TO FILEINPUT.
           SELECT RESULTSOUTPUT ASSIGN TO FILEOUTPUT.

       DATA DIVISION.
      *
       FILE SECTION.

      *------------------------------------------
      * VOTES INPUT FILE
      *------------------------------------------
       FD VOTESINPUT BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01 VOTES-RECORD.
           03 CONSTITUENCY-ID      PIC X(4).
           03 VOTE-VALUE OCCURS 72 TIMES INDEXED BY VOTE-ID PIC 9(1).
           03 SPACE-FILLER         PIC X(4).

      *------------------------------------------
      * RESULTS OUTPUT FILE
      *------------------------------------------
       FD RESULTSOUTPUT BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01 RESULTS-RECORD           PIC X(80).

      *------------------------------------------
      * WORKING STORAGE SECTION
      *------------------------------------------
       WORKING-STORAGE SECTION.

       01 WS-INPUT-EOF          PIC X(1) VALUE "N".

       01 WS-OUTPUT-DATA.
           03 PARTY-NAME        PIC X(20).
           03 FILLER            PIC X(30) VALUE " ".
           03 PARTY-VOTES       PIC 9(4).
           03 FILLER            PIC X(5)  VALUE " ".
           03 PARTY-WINNER      PIC X(3).
           03 FILLER            PIC X(18) VALUE " ".

       01 WS-WIN-STATUS         PIC X(3)  VALUE "***".
 

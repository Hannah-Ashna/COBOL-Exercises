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

       01 WS-OUTPUT-DATA.
           03 PARTY-NAME       PIC X(20).
           03 FILLER           PIC X(30) VALUE " ".
           03 PARTY-VOTES      PIC 9(4).
           03 FILLER           PIC X(5)  VALUE " ".
           03 PARTY-WINNER     PIC X(3).
           03 FILLER           PIC X(18) VALUE " ".

       01 WS-WIN-STATUS        PIC X(3)  VALUE "***".
 
      *------------------------------------------
      * Program Logic
      *------------------------------------------
       PROCEDURE DIVISION.

       A100-MAIN-LOGIC         SECTION.
           DISPLAY "Status - Program Starting..."
           PERFORM B100-INIT-CODE
           PERFORM C100-READ-FILE UNTIL INPUT-EOF
      *    PERFORM MAIN LOGIC FOR THE LOOP HERE     
           PERFORM X100-WRITE-OUTPUT
           PERFORM D100-CLOSE-FILE
           DISPLAY "Status - Program Complete"
           STOP RUN.
       
       B100-INIT-CODE          SECTION.
           OPEN INPUT  VOTESINPUT
           OPEN OUTPUT RESULTSOUTPUT
           DISPLAY "Status - Files Opened"

           PERFORM C100-READ-FILE
           .
       
       C100-READ-FILE          SECTION.
           READ VOTESINPUT
           DISPLAY "New Record: " VOTES-RECORD
           .

       D100-CLOSE-FILE         SECTION.
           CLOSE   VOTESINPUT
                   RESULTSOUTPUT
           
           DISPLAY "Status - Files Closed"
           .

       X100-WRITE-OUTPUT       SECTION.
      *    PUT FORMATTING HERE?
           WRITE RESULTS-RECORD

           IF NOT OUTPUT-OK
               DISPLAY "Error - Cannot write to Output File"
           ENDIF
           .
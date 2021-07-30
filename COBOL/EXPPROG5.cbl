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
       01 INPUT-RECORD.
           03 ACCOUNT-NUM          PIC 9(5).
           03 INITIAL-VAL          PIC X(1).
           03 SURNAME              PIC X(20).
           03 MARKER               PIC X(1).
           03 TRANS-AMOUNT         PIC 9(5).
           03 TRANS-DATE.
               05 IN-DAY           PIC 9(2).
               05 IN-MONTH         PIC 9(2).
               05 IN-YEAR          PIC 9(4).
           03 TRANS-DETAILS        PIC X(20).
           03 FILLER-IN            PIC X(20).

       FD FILE-OUTPUT BLOCK CONTAINS 0 RECORDS
            RECORDING MODE IS F.
       01 OUTPUT-RECORD.
           03 OUT-ACCOUNT-NUM      PIC 9(5).
           03 OUT-MARKER           PIC X(1).
           03 OUT-TRANS-AMOUNT     PIC S9(9) COMP-3.
           03 OUT-TRANS-DATE.
               05 OUT-YEAR         PIC 9(4).
               05 OUT-MONTH        PIC 9(2).
               05 OUT-DAY          PIC 9(2).
           03 OUT-INITIAL-VAL      PIC X(1).
           03 OUT-SURNAME          PIC X(20).
           03 FILLER-OUT           PIC X(40).

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS           PIC X(2).
           88 FILE-OK              VALUE "00".
           88 FILE-EOF             VALUE "01".
           88 FILE-VALID           VALUE "00", "01".

       01 WS-TEMP                  PIC S9(5).

      *---------------------
       PROCEDURE DIVISION.
       A100-MAIN-LOGIC             SECTION.
           DISPLAY "STATUS - STARTING PROGRAM"
           PERFORM B100-INIT
           PERFORM R100-READ-FILE
           PERFORM C100-PROCESS UNTIL FILE-EOF
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

       C100-PROCESS                SECTION.
           PERFORM W100-WRITE-FILE
           PERFORM R100-READ-FILE
           .

       W100-WRITE-FILE             SECTION.
           MOVE ACCOUNT-NUM TO OUT-ACCOUNT-NUM
           MOVE INITIAL-VAL TO OUT-INITIAL-VAL
           MOVE SURNAME     TO OUT-SURNAME
           MOVE MARKER      TO OUT-MARKER
           MOVE IN-DAY      TO OUT-DAY
           MOVE IN-MONTH    TO OUT-MONTH
           MOVE IN-YEAR     TO OUT-YEAR
           MOVE SPACES      TO FILLER-OUT

           IF MARKER = "D"
               MULTIPLY TRANS-AMOUNT BY -1 GIVING WS-TEMP
               MOVE WS-TEMP      TO OUT-TRANS-AMOUNT
           ELSE
               MOVE TRANS-AMOUNT TO OUT-TRANS-AMOUNT
           END-IF
           WRITE OUTPUT-RECORD
           .

       R100-READ-FILE              SECTION.
           READ FILE-INPUT AT END SET FILE-EOF TO TRUE
           DISPLAY "RECORD DATA: " INPUT-RECORD
           .

       T100-TERMINATE              SECTION.
           CLOSE FILE-INPUT
                 FILE-OUTPUT

           DISPLAY "STATUS - FILES CLOSED"
           . 

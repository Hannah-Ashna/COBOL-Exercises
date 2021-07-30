       ID DIVISION.
       PROGRAM-ID. EXPPROG6.
       AUTHOR. HANNAH JACOB.
       DATE-WRITTEN. 30TH JULY 2021.
       DATE-COMPILED.

      *---------------------
       ENVIRONMENT DIVISION.

      *---------------------
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT GOODFILE     ASSIGN TO GOOD-FILE
           FILE STATUS IS WS-GOOD-STATUS.
           SELECT MASTERFILE   ASSIGN TO MASTER-FILE
           FILE STATUS IS WS-MASTER-STATUS.
           SELECT OUTPUT-FILE  ASSIGN TO FILEOUT.
      *
       DATA DIVISION.
      *
       FILE SECTION.
      *
       FD GOODFILE BLOCK CONTAINS 0 RECORDS
            RECORDING MODE IS F.
       01 GOOD-RECORD.
           03 G-ACCOUNT-NUM        PIC 9(5).
           03 G-MARKER             PIC X(1).
           03 G-TRANS-AMOUNT       PIC S9(9) COMP-3.
           03 G-TRANS-DATE.
               05 OUT-YEAR         PIC 9(4).
               05 OUT-MONTH        PIC 9(2).
               05 OUT-DAY          PIC 9(2).
           03 G-INITIAL-VAL        PIC X(1).
           03 G-SURNAME            PIC X(20).
           03 G-FILLER             PIC X(40) VALUE SPACES.

       FD MASTERFILE BLOCK CONTAINS 0 RECORDS
            RECORDING MODE IS F.
       01 MASTER-RECORD.
           03 M-ACCOUNT-NUM        PIC 9(5).
           03 M-INITIAL-VAL        PIC X(1).
           03 M-SURNAME            PIC X(20).
           03 M-BALANCE            PIC S9(9) COMP-3.
           03 M-HIST-TRANS OCCURS 5 TIMES.
           03 M-MARKER             PIC X(1).
           03 M-TRANS-AMOUNT       PIC S9(9) COMP-3.
           03 M-FILLER             PIC X(35) VALUE SPACES.

       FD OUTPUT-FILE BLOCK CONTAINS 0 RECORDS
            RECORDING MODE IS F.
       01 OUTPUT-RECORD.
           03 O-ACCOUNT-NUM        PIC 9(5).
           03 O-INITIAL-VAL        PIC X(1).
           03 O-SURNAME            PIC X(20).
           03 O-BALANCE            PIC S9(9) COMP-3.
           03 O-HIST-TRANS OCCURS 5 TIMES.
           03 O-MARKER             PIC X(1).
           03 O-TRANS-AMOUNT       PIC S9(9) COMP-3.
           03 O-FILLER             PIC X(35) VALUE SPACES.

       WORKING-STORAGE SECTION.
       01 WS-GOOD-STATUS           PIC X(2).
           88 GOOD-OK              VALUE "00".
           88 GOOD-EOF             VALUE "01".
           88 GOOD-VALID           VALUE "00", "01".

       01 WS-MASTER-STATUS         PIC X(2).
           88 MASTER-OK            VALUE "00".
           88 MASTER-EOF           VALUE "01".
           88 MASTER-VALID         VALUE "00", "01".

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

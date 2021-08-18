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
           SELECT GOODFILE     ASSIGN TO GOODIN
           FILE STATUS IS WS-GOOD-STATUS.
           SELECT MASTERFILE   ASSIGN TO MASTIN
           FILE STATUS IS WS-MAST-STATUS.

           SELECT OUTPUTFILE   ASSIGN TO FILEOUT.
           SELECT REPORTFILE   ASSIGN TO REPOUT.
      *
       DATA DIVISION.
      *
       FILE SECTION.
      *
       FD GOODFILE BLOCK CONTAINS 0 RECORDS
            RECORDING MODE IS F.
       01 GOOD-RECORD.
           03 G-ACCOUNT            PIC X(5).
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
           03 M-ACCOUNT            PIC X(5).
           03 M-INITIAL-VAL        PIC X(1).
           03 M-SURNAME            PIC X(20).
           03 M-BALANCE            PIC S9(9) COMP-3.
           03 M-HIST-TRANS OCCURS 5 TIMES INDEXED BY M-IDX.
               05 M-MARKER         PIC X(1).
               05 M-TRANS-AMOUNT   PIC S9(9) COMP-3.
           03 M-FILLER             PIC X(19) VALUE SPACES.

       FD OUTPUTFILE BLOCK CONTAINS 0 RECORDS
            RECORDING MODE IS F.
       01 OUTPUT-RECORD.
           03 O-ACCOUNT-NUM        PIC X(5).
           03 O-INITIAL-VAL        PIC X(1).
           03 O-SURNAME            PIC X(20).
           03 O-BALANCE            PIC S9(9) COMP-3.
           03 O-HIST-TRANS OCCURS 5 TIMES INDEXED BY O-IDX.
               05 O-MARKER         PIC X(1).
               05 O-TRANS-AMOUNT   PIC S9(9) COMP-3.
           03 O-FILLER             PIC X(19) VALUE SPACES.

       FD REPORTFILE BLOCK CONTAINS 0 RECORDS
            RECORDING MODE IS F.
       01 REPORT-RECORD            PIC X(80).

       WORKING-STORAGE SECTION.
       01 WS-GOOD-STATUS           PIC X(2).
           88 G-OK                 VALUE "00".
           88 G-EOF                VALUE "01".
           88 G-VALID              VALUE "00", "01".

       01 WS-MAST-STATUS           PIC X(2).
           88 M-OK                 VALUE "00".
           88 M-EOF                VALUE "01".
           88 M-VALID              VALUE "00", "01".

       01 REPORT-FORMAT.
           03 RECORD-TYPE          PIC X(20).
           03 FILLER               PIC X(20)   VALUE SPACES.
           03 TOTAL-VAL            PIC Z(3)9   VALUE ZERO.
           03 FILLER               PIC X(20)   VALUE SPACES.
           03 PERCENTAGE-VAL       PIC ZZ9.99  VALUE ZERO.

       01 PRNT-COUNT               PIC 9(1).

       01 TOTAL-REC.
           03 TOT-REC OCCURS 4 TIMES.
               05 TOT-RECS         PIC 9(7) VALUE 0.

       01 PERCENT-REC.
           03 PERC-REC OCCURS 4 TIMES.
               05 PERC-RECS        PIC 9(3)V9999 VALUE 0.

       01 DATA-REC.
           03 FILLER               PIC X(20) VALUE "UNCHANGED RECORDS".
           03 FILLER               PIC X(20) VALUE "NEW RECORDS".
           03 FILLER               PIC X(20) VALUE "UPDATED RECORDS".
           03 FILLER               PIC X(20) VALUE "ALL RECORD TYPES".

       01 DATA-REC-REDEF REDEFINES DATA-REC.
          03 RECORD-IN OCCURS 4 TIMES.
              05 RECORD-DATA       PIC X(20).

      *---------------------
       PROCEDURE DIVISION.
       A100-MAIN-LOGIC             SECTION.
           DISPLAY "STATUS - STARTING PROGRAM"
           PERFORM B100-INIT-STAGE
           PERFORM R100-READ-GOOD
           PERFORM R200-READ-MASTER
           PERFORM C100-MAIN-PROCESS UNTIL G-ACCOUNT = HIGH-VALUES AND
                                           M-ACCOUNT = HIGH-VALUES.
           PERFORM W400-WRITE-REPORT
           PERFORM T100-TERMINATE
           DISPLAY "STATUS - PROGRAM DONE"
           STOP RUN
           .

       B100-INIT-STAGE             SECTION.
           OPEN INPUT   GOODFILE
                        MASTERFILE

           OPEN OUTPUT  OUTPUTFILE
                        REPORTFILE
           DISPLAY "STATUS - INIT STAGE DONE"
           .


       C100-MAIN-PROCESS           SECTION.
            EVALUATE TRUE
                WHEN G-ACCOUNT < M-ACCOUNT
                     ADD 1 TO TOT-RECS(2) TOT-RECS(4)
                     DISPLAY "PERFORMING " G-ACCOUNT " < " M-ACCOUNT
                     PERFORM W300-WRITE-GOOD

                WHEN G-ACCOUNT > M-ACCOUNT
                     ADD 1 TO TOT-RECS(1) TOT-RECS(4)
                     DISPLAY "PERFORMING " G-ACCOUNT " > " M-ACCOUNT
                     PERFORM W200-WRITE-MASTER

                WHEN G-ACCOUNT = M-ACCOUNT
                     ADD 1 TO TOT-RECS(3) TOT-RECS(4)
                     DISPLAY "PERFORMING " G-ACCOUNT " = " M-ACCOUNT
                     PERFORM W100-WRITE-MATCH
            END-EVALUATE
            .

       W100-WRITE-MATCH            SECTION.
           MOVE M-ACCOUNT          TO O-ACCOUNT-NUM
           MOVE M-INITIAL-VAL      TO O-INITIAL-VAL
           MOVE M-SURNAME          TO O-SURNAME
           PERFORM VARYING M-IDX FROM 1 BY 1 UNTIL M-IDX > 4
               MOVE M-MARKER(M-IDX)       TO O-MARKER(M-IDX + 1)
               MOVE M-TRANS-AMOUNT(M-IDX) TO O-TRANS-AMOUNT(M-IDX + 1)
               DISPLAY "MARKER " O-MARKER(M-IDX + 1)
           END-PERFORM

           MOVE G-MARKER        TO O-MARKER(1)
           MOVE G-TRANS-AMOUNT  TO O-TRANS-AMOUNT(1)
           ADD  G-TRANS-AMOUNT  TO M-BALANCE GIVING O-BALANCE
           DISPLAY "W100 OUTPUT RECORD: " OUTPUT-RECORD
           WRITE OUTPUT-RECORD

           PERFORM R100-READ-GOOD
           PERFORM R200-READ-MASTER
           .

       W200-WRITE-MASTER           SECTION.
           MOVE MASTER-RECORD TO OUTPUT-RECORD
           DISPLAY "W200 OUTPUT RECORD: " OUTPUT-RECORD
           WRITE OUTPUT-RECORD

           PERFORM R200-READ-MASTER
           .

       W300-WRITE-GOOD             SECTION.
           MOVE G-ACCOUNT          TO O-ACCOUNT-NUM
           MOVE G-INITIAL-VAL      TO O-INITIAL-VAL
           MOVE G-SURNAME          TO O-SURNAME

           MOVE G-MARKER           TO O-MARKER(1)
           MOVE G-TRANS-AMOUNT     TO O-TRANS-AMOUNT(1)
           PERFORM VARYING O-IDX FROM 2 BY 1 UNTIL O-IDX > 4
               MOVE SPACES         TO O-MARKER(O-IDX)
               MOVE ZEROES         TO O-TRANS-AMOUNT(O-IDX)
           END-PERFORM

           MOVE G-TRANS-AMOUNT     TO O-BALANCE
           DISPLAY "W300 OUTPUT RECORD: " OUTPUT-RECORD
           WRITE OUTPUT-RECORD

           PERFORM R100-READ-GOOD
           .

       W400-WRITE-REPORT           SECTION.
           PERFORM VARYING PRNT-COUNT FROM 1 BY 1 UNTIL PRNT-COUNT > 4
               MOVE RECORD-DATA(PRNT-COUNT)  TO RECORD-TYPE
               MOVE TOT-RECS(PRNT-COUNT)     TO TOTAL-VAL
               COMPUTE  PERC-RECS(PRNT-COUNT) =
                        (TOT-RECS(PRNT-COUNT)/TOT-RECS(4))
               MULTIPLY PERC-RECS(PRNT-COUNT) BY 100
                        GIVING PERC-RECS(PRNT-COUNT)
               MOVE PERC-RECS(PRNT-COUNT)     TO PERCENTAGE-VAL
               WRITE REPORT-RECORD         FROM REPORT-FORMAT
           END-PERFORM
           .

       R100-READ-GOOD              SECTION.
           READ GOODFILE AT END MOVE HIGH-VALUES TO G-ACCOUNT.
           DISPLAY "HIGH-VALUES: "     G-ACCOUNT
           DISPLAY "GOOD FILE DATA: "  GOOD-RECORD
           .

       R200-READ-MASTER            SECTION.
           READ MASTERFILE AT END MOVE HIGH-VALUES TO M-ACCOUNT.
           DISPLAY "HIGH-VALUES: " M-ACCOUNT
           DISPLAY "MASTER FILE DATA: " MASTER-RECORD
           .

       T100-TERMINATE              SECTION.
           CLOSE GOODFILE
                 MASTERFILE
                 OUTPUTFILE
                 REPORTFILE
           DISPLAY "STATUS - FILES CLOSED"
           . 

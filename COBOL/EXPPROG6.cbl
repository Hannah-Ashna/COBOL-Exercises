       ID DIVISION.
       PROGRAM-ID. DTBPROG6.
       AUTHOR. HANNAH JACOB.
       DATE-WRITTEN. 16TH SEPT 2021.
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
           03 M-HIST-TRANS OCCURS 5 TIMES.
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
           03 O-HIST-TRANS OCCURS 5 TIMES.
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

       01 M-IDX                    PIC 9(1).
       01 O-IDX                    PIC 9(1).

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
       DT  MAIN-LOGIC
       ACTIONS
       01  DISPLAY  "STATUS - STARTING PROGRAM"
           PERFORMX B100-INIT-STAGE
       02  PERFORMX R100-READ-GOOD
       03  PERFORMX R200-READ-MASTER
       04  PERFORMX C100-MAIN-PROCESS
           DISPLAY  "MAIN PROCESS COMPLETE"
       05  PERFORMX W400-WRITE-REPORT
       06  PERFORMX T100-TERMINATE
           DISPLAY  "STATUS - PROGRAM DONE"
           STOP     RUN

       DT  B100-INIT-STAGE
       ACTIONS
       01  OPEN INPUT
                    GOODFILE
                    MASTERFILE
           OPEN OUTPUT
                    OUTPUTFILE
                    REPORTFILE
           DISPLAY  "STATUS - INIT STAGE DONE"

       DT  C100-MAIN-PROCESS
       CONDITIONS                                  1 2
       01           G-ACCOUNT                      Y N
           =        HIGH-VALUES
           AND      M-ACCOUNT
           =        HIGH-VALUES
       ACTIONS
       01  PERFORMX C100-MAIN-PROCESS-2            - X
           DISPLAY  G-ACCOUNT
           DISPLAY  M-ACCOUNT
       02  REPEAT                                  - X



       DT  C100-MAIN-PROCESS-2
       CONDITIONS                                  1 2 3
       01           G-ACCOUNT                      Y N N
           <        M-ACCOUNT
       02           G-ACCOUNT                      N Y N
           >        M-ACCOUNT
       03           G-ACCOUNT                      N N Y
           =        M-ACCOUNT
       ACTIONS
       01  ADD      1                              X - -
             TO     TOT-RECS(2) TOT-RECS(4)
           DISPLAY  "PERFORMING G < M"
           PERFORMX W300-WRITE-GOOD
       02  ADD      1                              - X -
             TO     TOT-RECS(1) TOT-RECS(4)
           DISPLAY  "PERFORMING G > M"
           PERFORMX W200-WRITE-MASTER
       03  ADD      1                              - - X
             TO     TOT-RECS(3) TOT-RECS(4)
           DISPLAY  "PERFORMING G = M"
           PERFORMX W100-WRITE-MATCH

       DT  W100-WRITE-MATCH
       ACTIONS
       01  DISPLAY  "WRITE MATCH"
           MOVE     M-ACCOUNT
             TO     O-ACCOUNT-NUM
           MOVE     M-INITIAL-VAL
             TO     O-INITIAL-VAL
           MOVE     M-SURNAME
             TO     O-SURNAME
           PERFORMX W100-WRITE-MATCH-LOOP
           MOVE     G-MARKER
             TO     O-MARKER(1)
           MOVE     G-TRANS-AMOUNT
             TO     O-TRANS-AMOUNT(1)
           ADD      G-TRANS-AMOUNT
             TO     M-BALANCE
             GIVING O-BALANCE
           MOVE     SPACES
             TO     O-FILLER
           WRITE    OUTPUT-RECORD
           DISPLAY  OUTPUT-RECORD
           PERFORMX R100-READ-GOOD
           PERFORMX R200-READ-MASTER

       DT  W100-WRITE-MATCH-LOOP
       CONDITIONS                                  0 1 2
       01           M-IDX                            Y N
           >        4
       ACTIONS
       01  MOVE     1                              X - -
             TO     M-IDX
       02  MOVE     M-MARKER(M-IDX)                - - X
             TO     O-MARKER(M-IDX + 1)
           MOVE     M-TRANS-AMOUNT(M-IDX)
             TO     O-TRANS-AMOUNT(M-IDX + 1)
           ADD      1
             TO     M-IDX
       03  REPEAT                                  - - X

       DT  W200-WRITE-MASTER
       ACTIONS
       01  DISPLAY  "WRITE MASTER"
           MOVE     MASTER-RECORD
             TO     OUTPUT-RECORD
           MOVE     SPACES
             TO     O-FILLER
           WRITE    OUTPUT-RECORD
           DISPLAY  OUTPUT-RECORD
           PERFORMX R200-READ-MASTER

       DT  W300-WRITE-GOOD
       ACTIONS
       01  DISPLAY  "WRITE GOOD"
           MOVE     G-ACCOUNT
             TO     O-ACCOUNT-NUM
           MOVE     G-INITIAL-VAL
             TO     O-INITIAL-VAL
           MOVE     G-SURNAME
             TO     O-SURNAME
           MOVE     G-MARKER
             TO     O-MARKER(1)
           MOVE     G-TRANS-AMOUNT
             TO     O-TRANS-AMOUNT(1)
           PERFORMX W300-WRITE-GOOD-LOOP
           MOVE     G-TRANS-AMOUNT
             TO     O-BALANCE
           MOVE     SPACES
             TO     O-FILLER
           WRITE    OUTPUT-RECORD
           DISPLAY  OUTPUT-RECORD
           PERFORMX R100-READ-GOOD

       DT  W300-WRITE-GOOD-LOOP
       CONDITIONS                                  0 1 2
       01           O-IDX                            Y N
           >        5
       ACTIONS
       01  MOVE     1                              X - -
             TO     O-IDX
       02  MOVE     SPACES                         - - X
             TO     O-MARKER(O-IDX)
           MOVE     ZEROES
             TO     O-TRANS-AMOUNT(O-IDX)
           ADD      1
             TO     O-IDX
       03  REPEAT                                  - - X

       DT  W400-WRITE-REPORT
       CONDITIONS                                  0 1 2
       01           PRNT-COUNT                       Y N
           >        4
       ACTIONS
       01  MOVE     1                              X - -
             TO     PRNT-COUNT
           DISPLAY  "WRITE REPORT"
       02  MOVE     RECORD-DATA(PRNT-COUNT)        - - X
             TO     RECORD-TYPE
           MOVE     TOT-RECS(PRNT-COUNT)
             TO     TOTAL-VAL
           COMPUTE  PERC-RECS(PRNT-COUNT)
            ROUNDED = (TOT-RECS(PRNT-COUNT)/
                    TOT-RECS(4))
           MULTIPLY PERC-RECS(PRNT-COUNT)
             BY     100
             GIVING PERC-RECS(PRNT-COUNT)
           MOVE     PERC-RECS(PRNT-COUNT)
             TO     PERCENTAGE-VAL
           WRITE    REPORT-RECORD
             FROM   REPORT-FORMAT
           ADD      1
             TO     PRNT-COUNT
       03  REPEAT                                  - - X

       DT  R100-READ-GOOD
       ACTIONS
       01  READ     GOODFILE
           AT END MOVE HIGH-VALUES
           TO       G-ACCOUNT

       DT  R200-READ-MASTER
       ACTIONS
       01  READ     MASTERFILE
           AT END MOVE HIGH-VALUES
           TO       M-ACCOUNT

       DT  T100-TERMINATE
       ACTIONS
       01  CLOSE    GOODFILE
                    MASTERFILE
                    OUTPUTFILE
                    REPORTFILE
           DISPLAY  "STATUS - FILES CLOSED" 

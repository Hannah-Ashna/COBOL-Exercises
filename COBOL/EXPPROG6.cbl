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
           FILE STATUS IS WS-MASTER-STATUS.
           SELECT OUTPUTFILE  ASSIGN TO FILEOUT.
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
           03 M-HIST-TRANS OCCURS 5 TIMES INDEXED BY M-IDX.
               05 M-MARKER         PIC X(1).
               05 M-TRANS-AMOUNT   PIC S9(9) COMP-3.
           03 M-FILLER             PIC X(11) VALUE SPACES.

       FD OUTPUTFILE BLOCK CONTAINS 0 RECORDS
            RECORDING MODE IS F.
       01 OUTPUT-RECORD.
           03 O-ACCOUNT-NUM        PIC 9(5).
           03 O-INITIAL-VAL        PIC X(1).
           03 O-SURNAME            PIC X(20).
           03 O-BALANCE            PIC S9(9) COMP-3.
           03 O-HIST-TRANS OCCURS 5 TIMES INDEXED BY O-IDX.
               05 O-MARKER         PIC X(1).
               05 O-TRANS-AMOUNT   PIC S9(9) COMP-3.
           03 O-FILLER             PIC X(11) VALUE SPACES.

       WORKING-STORAGE SECTION.
       01 WS-GOOD-STATUS           PIC X(2).
           88 GOOD-OK              VALUE "00".
           88 GOOD-EOF             VALUE "01".
           88 GOOD-VALID           VALUE "00", "01".

       01 WS-MASTER-STATUS         PIC X(2).
           88 MASTER-OK            VALUE "00".
           88 MASTER-EOF           VALUE "01".
           88 MASTER-VALID         VALUE "00", "01".

       01 WS-MASTER                PIC X(1).
           88 M-MATCH              VALUE "Y".
           88 M-NO-MATCH           VALUE "N".

       01 WS-GOOD                  PIC X(1).
           88 G-MATCH              VALUE "Y".
           88 G-NO-MATCH           VALUE "N".

       01 WS-TEMP-IDX              PIC 9(1).

      *---------------------
       PROCEDURE DIVISION.
       A100-MAIN-LOGIC             SECTION.
           DISPLAY "STATUS - STARTING PROGRAM"
           PERFORM B100-INIT-STAGE1
           PERFORM C100-MASTER-MATCH-PROCESS UNTIL MASTER-EOF

           PERFORM B200-INIT-STAGE2
           PERFORM C200-GOOD-MATCH-PROCESS UNTIL GOOD-EOF

           PERFORM T100-TERMINATE
           DISPLAY "STATUS - PROGRAM DONE"
           STOP RUN
           .

       B100-INIT-STAGE1            SECTION.
           OPEN INPUT   GOODFILE
           OPEN OUTPUT  OUTPUTFILE
           DISPLAY "STATUS - INIT STAGE 1 DONE"
           .

       B200-INIT-STAGE2            SECTION.
           OPEN INPUT MASTERFILE
           CLOSE GOODFILE
           DISPLAY "STATUS - INIT STAGE 2 DONE"
           .

       C100-MASTER-MATCH-PROCESS   SECTION.
           SET MASTER-OK TO TRUE
           PERFORM R200-READ-GOOD

           OPEN INPUT MASTERFILE
           PERFORM D100-MASTER-MATCH-CHECK UNTIL MASTER-EOF

           IF M-MATCH
               DISPLAY "STATUS - M-MATCH"
               PERFORM W100-WRITE-MATCH
           ELSE
               DISPLAY "STATUS - M-NO-MATCH"
               PERFORM W200-WRITE-MASTER
           END-IF
           CLOSE MASTERFILE
           .

       C200-GOOD-MATCH-PROCESS     SECTION.
           SET GOOD-OK TO TRUE
           PERFORM R100-READ-MASTER

           OPEN INPUT GOODFILE
           PERFORM D200-GOOD-MATCH-CHECK UNTIL GOOD-EOF

           IF G-MATCH
               DISPLAY "STATUS - G-MATCH"
               PERFORM W100-WRITE-MATCH
           ELSE
               DISPLAY "STATUS - G-NO-MATCH"
               PERFORM W300-WRITE-GOOD
           END-IF
           CLOSE GOODFILE
           .

       D100-MASTER-MATCH-CHECK     SECTION.
           PERFORM R100-READ-MASTER
           IF M-ACCOUNT-NUM = G-ACCOUNT-NUM
               SET M-MATCH    TO TRUE
           ELSE
               SET M-NO-MATCH TO TRUE
           END-IF
           .

       D200-GOOD-MATCH-CHECK       SECTION.
           PERFORM R200-READ-GOOD
           IF G-ACCOUNT-NUM = M-ACCOUNT-NUM
               SET G-MATCH    TO TRUE
           ELSE
               SET G-NO-MATCH TO TRUE
           END-IF
           .

       W100-WRITE-MATCH            SECTION.
           MOVE M-ACCOUNT-NUM    TO O-ACCOUNT-NUM
           MOVE M-INITIAL-VAL    TO O-INITIAL-VAL
           MOVE M-SURNAME        TO O-SURNAME
           MOVE 5 TO WS-TEMP-IDX
           PERFORM VARYING M-IDX FROM 4 BY -1 UNTIL M-IDX = 1
               SET  O-IDX TO WS-TEMP-IDX
               MOVE M-MARKER(M-IDX)       TO O-MARKER(O-IDX)
               MOVE M-TRANS-AMOUNT(M-IDX) TO O-TRANS-AMOUNT(O-IDX)
               SUBTRACT 1 FROM WS-TEMP-IDX
           END-PERFORM

           MOVE G-MARKER        TO O-MARKER(1)
           MOVE G-TRANS-AMOUNT  TO O-TRANS-AMOUNT(1)

           ADD  G-TRANS-AMOUNT  TO M-BALANCE GIVING O-BALANCE
           DISPLAY "OUTPUT RECORD: " OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           .

       W200-WRITE-MASTER           SECTION.
           MOVE MASTER-RECORD TO OUTPUT-RECORD
           DISPLAY "OUTPUT RECORD: " OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           .

       W300-WRITE-GOOD             SECTION.
           MOVE G-ACCOUNT-NUM    TO O-ACCOUNT-NUM
           MOVE G-INITIAL-VAL    TO O-INITIAL-VAL
           MOVE G-SURNAME        TO O-SURNAME

           MOVE G-MARKER        TO O-MARKER(1)
           MOVE G-TRANS-AMOUNT  TO O-TRANS-AMOUNT(1)

           MOVE G-TRANS-AMOUNT  TO O-BALANCE
           DISPLAY "OUTPUT RECORD: " OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           .

       R100-READ-MASTER            SECTION.
           READ MASTERFILE AT END SET MASTER-EOF TO TRUE
           DISPLAY "MASTER FILE DATA: " MASTER-RECORD
           .

       R200-READ-GOOD              SECTION.
           READ GOODFILE AT END SET GOOD-EOF TO TRUE
           DISPLAY "GOOD FILE DATA: " GOOD-RECORD
           .

       T100-TERMINATE              SECTION.
           CLOSE GOODFILE
                 MASTERFILE
                 OUTPUTFILE
           DISPLAY "STATUS - FILES CLOSED"
           . 

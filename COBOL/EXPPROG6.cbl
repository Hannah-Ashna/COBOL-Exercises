       ID DIVISION.
       PROGRAM-ID. EXPPROG6.
       AUTHOR. HANNAH JACOB.
       DATE-WRITTEN. 12TH AUG 2021.
       DATE-COMPILED.

      *---------------------
       ENVIRONMENT DIVISION.

      *---------------------
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT MASTERFILE ASSIGN TO MASTIN
           ACCESS            IS DYNAMIC
           ORGANIZATION      IS INDEXED
           RECORD KEY        IS ACCOUNT-NUM
           FILE STATUS       IS WS-MAST-STATUS.

           SELECT PRINTFILE  ASSIGN TO REPOUT
           FILE STATUS       IS WS-REPORT-STATUS.
      *
       DATA DIVISION.
      *
       FILE SECTION.
      *
       FD MASTERFILE
            LABEL RECORDS ARE STANDARD
            BLOCK CONTAINS 0 RECORDS.
       01 MASTER-RECORD.
           03 ACCOUNT-NUM          PIC X(5).
           03 INITIAL-VAL          PIC X(1).
           03 SURNAME              PIC X(20).
           03 BALANCE              PIC S9(9) COMP-3.
           03 HIST-TRANS OCCURS 5 TIMES.
               05 MARKER           PIC X(1).
               05 TRANS-AMOUNT     PIC S9(9) COMP-3.
           03 FILLER               PIC X(19) VALUE SPACES.

       FD PRINTFILE BLOCK CONTAINS 0 RECORDS
            RECORDING MODE IS F.
       01 PRINTLINE                PIC X(132).

       WORKING-STORAGE SECTION.
       01 WS-MAST-STATUS           PIC X(2).
           88 M-OK                 VALUE "00".
           88 M-EOF                VALUE "01".
           88 M-VALID              VALUE "00", "01".

       01 WS-REPORT-STATUS         PIC X(2).
           88 R-OK                 VALUE "00".
           88 R-EOF                VALUE "01".
           88 R-VALID              VALUE "00", "01".

       01 M-IDX                    PIC 9(1).

       01 WS-LINE-COUNT            PIC 9(2).

       01 REPORT-HEADER.
           03 FILLER               PIC X(63) VALUE SPACES.
           03 FILLER               PIC X(6)  VALUE "REPORT".
           03 FILLER               PIC X(4)  VALUE SPACES.
           03 PAGE-COUNT           PIC 9(2).
           03 FILLER               PIC X(57) VALUE SPACES.

       01 REPORT-COL-1.
           03 FILLER               PIC X(7)  VALUE "ACCOUNT".
           03 FILLER               PIC X(2)  VALUE SPACES.
           03 FILLER               PIC X(7)  VALUE "INITIAL".
           03 FILLER               PIC X(2)  VALUE SPACES.
           03 FILLER               PIC X(11) VALUE "SURNAME".
           03 FILLER               PIC X(2)  VALUE SPACES.
           03 FILLER               PIC X(7)  VALUE "BALANCE".
           03 FILLER               PIC X(2)  VALUE SPACES.
           03 HIST-COL OCCURS 5 TIMES.
               05 FILLER           PIC X(3)  VALUE "C/D".
               05 FILLER           PIC X(1)  VALUE SPACES.
               05 FILLER           PIC X(6)  VALUE "TRANS ".
               05 FILLER           PIC X(2)  VALUE SPACES.
           03 FILLER               PIC X(32) VALUE SPACES.

       01 REPORT-COL-2.
           03 FILLER               PIC X(7)  VALUE "NUMBER ".
           03 FILLER               PIC X(33) VALUE SPACES.
           03 HIST-COL-2 OCCURS 5 TIMES.
               05 FILLER           PIC X(3)  VALUE "MKR".
               05 FILLER           PIC X(1)  VALUE SPACES.
               05 FILLER           PIC X(6)  VALUE "AMOUNT".
               05 FILLER           PIC X(2)  VALUE SPACES.
           03 FILLER               PIC X(32) VALUE SPACES.

       01 REPORT-RECORD.
           03 FILLER              PIC X VALUE SPACES.
           03 ACCOUNT-OUT         PIC X(5).
           03 FILLER              PIC X(5) VALUE SPACES.
           03 INITIAL-OUT         PIC X(1).
           03 FILLER              PIC X(6) VALUE SPACES.
           03 SURNAME-OUT         PIC X(13).
           03 FILLER              PIC X(1) VALUE SPACES.
           03 BALANCE-OUT         PIC -(5)9(1).
           03 FILLER              PIC X(3) VALUE SPACES.
           03 HISTPRINT OCCURS 5 TIMES.
               05 MARKER-OUT       PIC X(1).
               05 FILLER           PIC X(1).
               05 TRANS-AMOUNT-OUT PIC -(6)9(1).
               05 FILLER           PIC X(3) VALUE SPACES.
           03 FILLER               PIC X(26) VALUE SPACES.
      *---------------------
       PROCEDURE DIVISION.
       A100-MAIN-LOGIC             SECTION.
           DISPLAY "STATUS - STARTING PROGRAM"
           PERFORM B100-INIT-STAGE
           PERFORM R100-READ-MASTER UNTIL M-EOF
           PERFORM T100-TERMINATE
           DISPLAY "STATUS - PROGRAM DONE"
           STOP RUN
           .

       B100-INIT-STAGE             SECTION.
           OPEN INPUT   MASTERFILE
           OPEN OUTPUT  PRINTFILE
           MOVE  1      TO PAGE-COUNT
           MOVE  5      TO WS-LINE-COUNT
           WRITE PRINTLINE FROM REPORT-HEADER AFTER ADVANCING PAGE
           WRITE PRINTLINE FROM REPORT-COL-1  AFTER ADVANCING 1 LINE
           WRITE PRINTLINE FROM REPORT-COL-2  AFTER ADVANCING 2 LINE
           DISPLAY "STATUS - INIT STAGE DONE"
           .

       W100-WRITE-REPORT           SECTION.
           MOVE ACCOUNT-NUM TO ACCOUNT-OUT
           MOVE INITIAL-VAL TO INITIAL-OUT
           MOVE SURNAME     TO SURNAME-OUT
           MOVE BALANCE     TO BALANCE-OUT
           PERFORM VARYING M-IDX FROM 1 BY 1 UNTIL M-IDX > 5
               MOVE MARKER(M-IDX)       TO MARKER-OUT(M-IDX)
               MOVE TRANS-AMOUNT(M-IDX) TO TRANS-AMOUNT-OUT(M-IDX)
           END-PERFORM

           IF WS-LINE-COUNT < 54
               ADD 2 TO WS-LINE-COUNT
               WRITE PRINTLINE FROM REPORT-RECORD
               AFTER ADVANCING 2 LINE
           ELSE
               MOVE  0 TO WS-LINE-COUNT
               ADD   7 TO WS-LINE-COUNT
               ADD   1 TO PAGE-COUNT
               WRITE PRINTLINE FROM REPORT-HEADER AFTER ADVANCING PAGE
               WRITE PRINTLINE FROM REPORT-COL-1  AFTER ADVANCING 1 LINE
               WRITE PRINTLINE FROM REPORT-COL-2  AFTER ADVANCING 2 LINE
               WRITE PRINTLINE FROM REPORT-RECORD AFTER ADVANCING 2 LINE
           END-IF
           .

       R100-READ-MASTER            SECTION.
           READ MASTERFILE NEXT
           AT END SET M-EOF TO TRUE
           NOT AT END PERFORM W100-WRITE-REPORT
           .

       T100-TERMINATE              SECTION.
           CLOSE MASTERFILE
                 PRINTFILE
           . 

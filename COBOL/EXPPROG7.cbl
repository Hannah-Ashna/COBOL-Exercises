       ID DIVISION.
       PROGRAM-ID. EXPPROG7.
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
           03 HIST-TRANS OCCURS 5 TIMES INDEXED BY M-IDX.
               05 MARKER           PIC X(1).
               05 TRANS-AMOUNT     PIC S9(9) COMP-3.
           03 FILLER               PIC X(19) VALUE SPACES.

       FD PRINTFILE BLOCK CONTAINS 0 RECORDS
            RECORDING MODE IS F.
       01 PRINTLINE                PIC X(100).

       WORKING-STORAGE SECTION.
       01 WS-MAST-STATUS           PIC X(2).
           88 M-OK                 VALUE "00".
           88 M-EOF                VALUE "01".
           88 M-VALID              VALUE "00", "01".

       01 WS-REPORT-STATUS         PIC X(2).
           88 R-OK                 VALUE "00".
           88 R-EOF                VALUE "01".
           88 R-VALID              VALUE "00", "01".

       01 WS-LINE-COUNT            PIC 9(2).

       01 WS-PAGE-COUNT            PIC 9(2).

       01 REPORT-HEADER.
           03 FILLER               PIC X(47) VALUE SPACES.
           03 REPORT-TITLE         PIC X(6)  VALUE "REPORT".
           03 FILLER               PIC X(47) VALUE SPACES.

       01 REPORT-COL-1.
           03 ACCOUNT-COL          PIC X(7)  VALUE "ACCOUNT".
           03 FILLER               PIC X(2)  VALUE SPACES.
           03 INITIAL-COL          PIC X(7)  VALUE "INITIAL".
           03 FILLER               PIC X(2)  VALUE SPACES.
           03 SURNAME-COL          PIC X(11) VALUE "SURNAME".
           03 FILLER               PIC X(2)  VALUE SPACES.
           03 BALANCE-COL          PIC X(7)  VALUE "BALANCE".
           03 FILLER               PIC X(2)  VALUE SPACES.
           03 HIST-COL OCCURS 5 TIMES.
               05 MARKER-COL       PIC X(3)  VALUE "C/D".
               05 FILLER           PIC X(1)  VALUE SPACES.
               05 AMOUNT-COL       PIC X(6)  VALUE "TRANS ".
               05 FILLER           PIC X(2)  VALUE SPACES.

       01 REPORT-COL-2.
           03 ACCOUNT-COL-2        PIC X(7)  VALUE "NUMBER ".
           03 FILLER               PIC X(33) VALUE SPACES.
           03 HIST-COL-2 OCCURS 5 TIMES.
               05 MARKER-COL-2     PIC X(3)  VALUE "MKR".
               05 FILLER           PIC X(1)  VALUE SPACES.
               05 AMOUNT-COL-2     PIC X(6)  VALUE "AMOUNT".
               05 FILLER           PIC X(2)  VALUE SPACES.

       01 REPORT-RECORD.
           03 ACCOUNT-OUT          PIC X(5).
           03 FILLER               PIC X(6)  VALUE SPACES.
           03 INITIAL-OUT          PIC X(1).
           03 FILLER               PIC X(6)  VALUE SPACES.
           03 SURNAME-OUT          PIC X(12).
           03 FILLER               PIC X(1)  VALUE SPACES.
           03 BALANCE-OUT          PIC ++(9)9.
           03 FILLER               PIC X(2)  VALUE SPACES.
           03 HIST-TRANS OCCURS 5 TIMES INDEXED BY R-IDX.
               05 MARKER-OUT       PIC X(1).
               05 FILLER           PIC X(8)  VALUE SPACES.
               05 TRANS-AMOUNT-OUT PIC ++(9)9.
               05 FILLER           PIC X(16) VALUE SPACES.
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
           WRITE PRINTLINE FROM REPORT-HEADER AFTER ADVANCING PAGE
           WRITE PRINTLINE FROM REPORT-COL-1  AFTER ADVANCING 1 LINE
           WRITE PRINTLINE FROM REPORT-COL-2  AFTER ADVANCING 2 LINE
           MOVE  1      TO WS-PAGE-COUNT
           MOVE  5      TO WS-LINE-COUNT
           MOVE 'A0001' TO ACCOUNT-NUM
           START MASTERFILE
                 KEY GREATER THAN OR EQUAL TO ACCOUNT-NUM
           END-START
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
               AFTER ADVANCING 1 LINE
           ELSE
               MOVE  0 TO WS-LINE-COUNT
               ADD   7 TO WS-LINE-COUNT
               ADD   1 TO WS-PAGE-COUNT
               WRITE PRINTLINE FROM REPORT-HEADER AFTER ADVANCING PAGE
               WRITE PRINTLINE FROM REPORT-COL-1  AFTER ADVANCING 1 LINE
               WRITE PRINTLINE FROM REPORT-COL-2  AFTER ADVANCING 2 LINE
               WRITE PRINTLINE FROM REPORT-RECORD AFTER ADVANCING 1 LINE
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

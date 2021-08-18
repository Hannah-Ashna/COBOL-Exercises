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

           SELECT REPORTFILE ASSIGN TO REPOUT
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
           03 ACCOUNT-NUM        PIC X(5).
           03 INITIAL-VAL        PIC X(1).
           03 SURNAME            PIC X(20).
           03 BALANCE            PIC S9(9) COMP-3.
           03 HIST-TRANS OCCURS 5 TIMES INDEXED BY M-IDX.
               05 MARKER         PIC X(1).
               05 TRANS-AMOUNT   PIC S9(9) COMP-3.
           03 FILLER             PIC X(19) VALUE SPACES.

       FD REPORTFILE BLOCK CONTAINS 0 RECORDS
            RECORDING MODE IS F.
       01 REPORT-RECORD            PIC X(80).

       WORKING-STORAGE SECTION.
       01 WS-MAST-STATUS           PIC X(2).
           88 M-OK                 VALUE "00".
           88 M-EOF                VALUE "01".
           88 M-VALID              VALUE "00", "01".

       01 WS-REPORT-STATUS         PIC X(2).
           88 R-OK                 VALUE "00".
           88 R-EOF                VALUE "01".
           88 R-VALID              VALUE "00", "01".
      *---------------------
       PROCEDURE DIVISION.
       A100-MAIN-LOGIC             SECTION.
           DISPLAY "STATUS - STARTING PROGRAM"
           PERFORM B100-INIT-STAGE
           PERFORM R100-READ-MASTER UNTIL M-EOF
           PERFORM W100-WRITE-REPORT
           PERFORM T100-TERMINATE
           DISPLAY "STATUS - PROGRAM DONE"
           STOP RUN
           .

       B100-INIT-STAGE             SECTION.
           OPEN INPUT   MASTERFILE
           OPEN OUTPUT  REPORTFILE
           MOVE 'A0001' TO ACCOUNT-NUM
           START MASTERFILE
                 KEY GREATER THAN OR EQUAL TO ACCOUNT-NUM
           END-START
           DISPLAY "STATUS - INIT STAGE DONE"
           .

       W100-WRITE-REPORT           SECTION.
           DISPLAY "DOING MY REPORT WRITING HERE..."
           .

       R100-READ-MASTER            SECTION.
           READ MASTERFILE NEXT
           AT END SET M-EOF TO TRUE
           NOT AT END DISPLAY MASTER-RECORD

           .

       T100-TERMINATE              SECTION.
           CLOSE MASTERFILE
                 REPORTFILE
           . 

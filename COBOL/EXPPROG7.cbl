       ID DIVISION.
       PROGRAM-ID. EXPPROG7.
       AUTHOR. HANNAH JACOB.
       DATE-WRITTEN. 20TH SEPT 2021.
       DATE-COMPILED.

      *---------------------
       ENVIRONMENT DIVISION.

      *---------------------
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
      *VSAM File Stuff - Use this once everything else works
      *    SELECT INPUT-FILE  ASSIGN TO FILEIN
      *    ACCESS             IS DYNAMIC
      *    ORGANIZATION       IS INDEXED
      *    RECORD KEY         IS ACCOUNT
      *    FILE STATUS        IS WS-INPUT-STATUS.

           SELECT INPUT-FILE ASSIGN TO FILEIN
           FILE STATUS        IS WS-INPUT-STATUS.
           SELECT OUTPUT-FILE ASSIGN TO FILEOUT
           FILE STATUS        IS WS-OUTPUT-STATUS.
      *
       DATA DIVISION.
      *
       FILE SECTION.
      *
       FD INPUT-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01 INPUT-RECORD.
           03 ACCOUNT          PIC X(5).
           03 FILLER           PIC X(1).
           03 ITEM-CODES OCCURS 5 TIMES INDEXED BY I-IDX.
               05 ITEM-CODE    PIC X(3) VALUE ZEROES.
               05 ITEM-VALUE   PIC 9(3) VALUE ZEROES.
           03 FILLER           PIC X(1).
           03 DISC-CODES OCCURS 5 TIMES INDEXED BY D-IDX.
               05 DISC-CODE    PIC X(3) VALUE ZEROES.
               05 DISC-COUNT   PIC 9(1) VALUE ZEROES.
               05 DISC-MARKER  PIC X(1) VALUE ZEROES.
               05 DISC-VALUE   PIC 9(2) VALUE ZEROES.
           03 FILLER           PIC X(78).

       FD OUTPUT-FILE BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01 OUTPUT-RECORD.
           03 ACCOUNT-OUT      PIC X(5).
           03 FILLER           PIC X(4).
           03 TOTAL-OUT        PIC 9(3).99.
           03 FILLER           PIC X(135).
      *
       WORKING-STORAGE SECTION.
       01 WS-INPUT-STATUS      PIC X(2).
           88 I-OK             VALUE "00".
           88 I-EOF            VALUE "01".
           88 I-VALID          VALUE "00", "01".

       01 WS-OUTPUT-STATUS     PIC X(2).
           88 O-OK             VALUE "00".
           88 O-EOF            VALUE "01".
           88 O-VALID          VALUE "00", "01".

      *TEMP VARIABLES FOR CALCULATIONS
       01 DISC-COMP            PIC 9(2).
       01 ITEM-COUNT           PIC 9(2).
       01 ITEM-TOTAL           PIC 9(3).
       01 TEMP                 PIC 9(3)V99.
       01 TEMP-VALUE           PIC 9(3)V99.
       01 RESULT               PIC 9(3)V99.
       01 BAG-TOTAL            PIC 9(3)V99.
       01 ITEM-TEMP            PIC X(6).
       01 DISC-TEMP            PIC X(7).
       01 I                    PIC 9.
       01 J                    PIC 9.

       01 WS-ALTERNATIVE       PIC X(2).
           88 DISC-ALT         VALUE "Y".
           88 DISC-NO-ALT      VALUE "X".

      *---------------------
       PROCEDURE DIVISION.
       A100-MAIN-LOGIC             SECTION.
           DISPLAY "STATUS - PROGRAM HAS BEGUN"
           PERFORM B100-INIT-STAGE
           PERFORM R100-READ-INPUT
           PERFORM C100-PROCESS UNTIL I-EOF
           PERFORM T100-TERMINATE
           DISPLAY "STATUS - PROGRAM DONE"
           STOP    RUN
           .

       B100-INIT-STAGE             SECTION.
           OPEN INPUT   INPUT-FILE
           OPEN OUTPUT  OUTPUT-FILE
           SET DISC-NO-ALT TO TRUE
           DISPLAY "STATUS - FILES OPENED"
           .

       C100-PROCESS                SECTION.
           DISPLAY "NEW DATA:" INPUT-RECORD
           PERFORM D100-SORT-ITEMS
           PERFORM D200-SORT-DISC
           PERFORM C200-PROCESS
           PERFORM R100-READ-INPUT
           DISPLAY "BAG-TOTAL: " BAG-TOTAL
           .

       C200-PROCESS                SECTION.
           MOVE 0 TO BAG-TOTAL
           PERFORM VARYING D-IDX FROM 1 BY 1 UNTIL D-IDX > 5
               MOVE 0 TO ITEM-TOTAL
               MOVE 0 TO ITEM-COUNT
               MOVE 0 TO RESULT

               PERFORM VARYING I-IDX FROM 1 BY 1 UNTIL I-IDX > 5
                   IF ITEM-CODE(I-IDX) EQUAL TO DISC-CODE(D-IDX)
                       ADD ITEM-VALUE(I-IDX) TO ITEM-TOTAL
                       ADD 1                 TO ITEM-COUNT
                   END-IF
               END-PERFORM

               IF ITEM-COUNT >= DISC-COUNT(D-IDX)
                   EVALUATE DISC-MARKER(D-IDX)
                       WHEN "P"
                           DISPLAY  "PERCENTAGE DISCOUNT SELECTED"
                           COMPUTE  TEMP = FUNCTION MOD (ITEM-COUNT,
                                                    DISC-COUNT(D-IDX))
                           COMPUTE  RESULT =  ITEM-TOTAL * TEMP
                           DIVIDE   RESULT BY ITEM-COUNT GIVING RESULT
                           SUBTRACT RESULT FROM ITEM-TOTAL
                           COMPUTE  TEMP = (ITEM-TOTAL - ((ITEM-TOTAL *
                                           DISC-VALUE(D-IDX)) / 100))
                           ADD      TEMP TO RESULT
                       WHEN "C"
                           DISPLAY  "CASH DISCOUNT SELECTED"
                           SUBTRACT DISC-VALUE(D-IDX) FROM ITEM-TOTAL
                                    GIVING RESULT
                       WHEN OTHER
                           DISPLAY  "NO DISCOUNT AVAILABLE"
                           MOVE     ITEM-TOTAL TO RESULT
                   END-EVALUATE

               ELSE
                   DISPLAY  "NO DISCOUNT AVAILABLE ITEM COUNT < DISC"
                   MOVE     ITEM-TOTAL TO RESULT
               END-IF

               DISPLAY DISC-CODE(D-IDX) " RESULT: " RESULT

               IF D-IDX > 1
                   PERFORM C300-PROCESS
               ELSE
                   MOVE RESULT TO DISC-COMP
                   SET DISC-ALT TO TRUE
               END-IF

               IF RESULT >= 0
                   IF NOT DISC-ALT
                       MOVE DISC-COMP TO RESULT
                   END-IF
                   ADD RESULT TO BAG-TOTAL
               END-IF

           END-PERFORM
           .

       C300-PROCESS                SECTION.
           IF (DISC-CODE(D-IDX) = DISC-CODE(D-IDX - 1) AND RESULT > 0
               AND RESULT < DISC-COMP)
              SUBTRACT DISC-COMP FROM BAG-TOTAL
              MOVE RESULT TO DISC-COMP
              SET DISC-ALT TO TRUE
           ELSE IF (DISC-CODE(D-IDX) = DISC-CODE(D-IDX - 1) AND
                    RESULT > 0 AND RESULT > DISC-COMP)
               MOVE 0 TO DISC-COMP
               SET DISC-NO-ALT TO TRUE
           ELSE IF (DISC-CODE(D-IDX) NOT EQUAL TO DISC-CODE(D-IDX - 1)
                    AND RESULT > 0 AND RESULT < DISC-COMP)
               MOVE RESULT TO DISC-COMP
               SET DISC-ALT TO TRUE
           ELSE IF (DISC-CODE(D-IDX) NOT EQUAL TO DISC-CODE(D-IDX - 1)
                    AND RESULT > 0 AND RESULT > DISC-COMP)
               MOVE RESULT TO DISC-COMP
               SET DISC-NO-ALT TO TRUE
           ELSE
               MOVE RESULT TO DISC-COMP
               SET DISC-NO-ALT TO TRUE
           END-IF
           .

       D100-SORT-ITEMS             SECTION.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 4
               ADD 1 TO I GIVING J
               PERFORM UNTIL J > 5
                   IF ITEM-CODES(I) > ITEM-CODES(J)
                       MOVE ITEM-CODES(I) TO ITEM-TEMP
                       MOVE ITEM-CODES(J) TO ITEM-CODES(I)
                       MOVE ITEM-TEMP     TO ITEM-CODES(J)
                   END-IF
                   ADD 1 TO J
               END-PERFORM
           END-PERFORM
           DISPLAY "STATUS - SORTED ITEMS"
           .

       D200-SORT-DISC              SECTION.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 4
               ADD 1 TO I GIVING J
               PERFORM UNTIL J > 5
                   IF DISC-CODES(I) > DISC-CODES(J)
                       MOVE DISC-CODES(I) TO DISC-TEMP
                       MOVE DISC-CODES(J) TO DISC-CODES(I)
                       MOVE DISC-TEMP     TO DISC-CODES(J)
                   END-IF
                   ADD 1 TO J
               END-PERFORM
           END-PERFORM
           DISPLAY "STATUS - SORTED DISCOUNTS"
           .

       R100-READ-INPUT             SECTION.
           READ INPUT-FILE AT END SET I-EOF TO TRUE
           .

       T100-TERMINATE              SECTION.
           CLOSE INPUT-FILE
                 OUTPUT-FILE
           DISPLAY "STATUS - FILES CLOSED"
           . 

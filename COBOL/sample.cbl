       ID DIVISION.
       PROGRAM-ID. CBLPROG6.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
      ******************************************************************
      * 2 FILE MATCH PROGRAM INSERTING NEW INPUT 'MASTER FILE' *
      * COMPARING IT WITH THE SORTED GOOD FILE FROM BHEXPEXT *
      * TO PRODUCE NEW MASTER FILE AND COUNT OF THE RECORDS. *
      ******************************************************************
       FILE-CONTROL.
           SELECT MASTER-FILE ASSIGN TO MASTIN.
           SELECT EXT-GOOD-FILE ASSIGN TO EXTIN.
           SELECT NEW-MASTER-FILE ASSIGN TO MASTOUT.
           SELECT PRINTOUT ASSIGN TO PROUT.
       DATA DIVISION.
       FILE SECTION.
       FD EXT-GOOD-FILE BLOCK CONTAINS 0 RECORDS
                        RECORDING MODE IS F.
       01 EXT-GOOD.
          03 ACCNUM-EXT            PIC X(5).
          03 CDMKR-EXT             PIC X(1).
          03 TRANAMT-EXT           PIC S9(9) COMP-3.
          03 TRANDATE-EXT.
             05 EXT-YYYY           PIC 9(4).
             05 EXT-MM             PIC 9(2).
             05 EXT-DD             PIC 9(2).
          03 INITIAL-EXT           PIC X(1).
          03 SURNAME-EXT           PIC X(20).
          03 FILLER                PIC X(40).
       FD MASTER-FILE BLOCK CONTAINS 0 RECORDS
                      RECORDING MODE IS F.
       01 MASTER-REC.
          03 ACCNUM-M              PIC X(5).
          03 INITIAL-M             PIC X(1).
          03 SURNAME-M             PIC X(20).
          03 BALANCE-M             PIC S9(9) COMP-3.
          03 HISTORICAL-M OCCURS 5 TIMES.
             05 CDMKR-M            PIC X(1).
             05 TRANAMT-M          PIC S9(9) COMP-3.
          03 FILLER                PIC X(19).
       FD NEW-MASTER-FILE BLOCK CONTAINS 0 RECORDS
                          RECORDING MODE IS F.
       01 NEW-MASTER-REC.
          03 ACCNUM-NM             PIC X(5).
          03 INITIAL-NM            PIC X(1).
          03 SURNAME-NM            PIC X(20).
          03 BALANCE-NM            PIC S9(9) COMP-3.
          03 HISTORICAL-NM OCCURS 5 TIMES.
             05 CDMKR-NM           PIC X(1).
             05 TRANAMT-NM         PIC S9(9) COMP-3.
          03 FILLER-NM             PIC X(19).

       FD PRINTOUT BLOCK CONTAINS 0 RECORDS
                   RECORDING MODE IS F.
       01 PRINTLINE                PIC X(132).
       WORKING-STORAGE SECTION.
       01 HIS-COUNT PIC 9(1).
       01 PR-CNT    PIC 9(1).
       01 TEMP-PERC PIC 9(9).
       01 PAGE-NUM-E PIC 9(4).
       01 LINE-COUNT PIC 9(4) VALUE 0.


       01 HEAD-1.
          03 FILLER              PIC X(20) VALUE SPACES.
          03 FILLER              PIC X(32) VALUE 'BHEXPUPD REPORT'.
          03 FILLER              PIC X(80) VALUE SPACES.
       01 BLANKLINE.
          03 BLANK-LINE          PIC X(132) VALUE SPACES.
       01 HEAD-2.
          03 FILLER              PIC X(20) VALUE 'TYPE OF RECORD'.
          03 FILLER              PIC X(20) VALUE SPACES.
          03 FILLER              PIC X(15) VALUE 'TOTAL'.
          03 FILLER              PIC X(10) VALUE SPACES.
          03 FILLER              PIC X(15) VALUE 'PERCENTAGE'.
          03 FILLER              PIC X(52) VALUE SPACES.
       01 DETAIL-LINE.
          03 TYPE-OF-RECORD-1    PIC X(20).
          03 FILLER              PIC X(20) VALUE SPACES.
          03 TOTAL-1             PIC Z(3)9 VALUE ZERO.
          03 FILLER              PIC X(20) VALUE SPACES.
          03 PERCENTAGE-1        PIC ZZ9.99 VALUE ZERO.
       01 TOTAL-REC.
          03 TOT-REC OCCURS 4 TIMES.
             05 TOT-RECS         PIC 9(7) VALUE 0.
        01 PERCENTAGE-REC.
           03 PERCENT-REC OCCURS 4 TIMES.
              05 PERC-REC         PIC 9(3)V9999 VALUE 0.
        01 LOOKUP-REC.
           03 FILLER              PIC X(20) VALUE 'UNCHANGED RECORDS'.
           03 FILLER              PIC X(20) VALUE 'NEW RECORDS'.
           03 FILLER              PIC X(20) VALUE 'UPDATED RECORDS'.
           03 FILLER              PIC X(20) VALUE 'ALL RECORDS TYPES'.
        01 RECORD-LIST REDEFINES LOOKUP-REC.
           03 RECORD-IN OCCURS 4 TIMES.
              05 RECORD-DATA      PIC X(20).
       PROCEDURE DIVISION.
       START-UP SECTION.
             OPEN INPUT MASTER-FILE
                        EXT-GOOD-FILE
                 OUTPUT NEW-MASTER-FILE
                        PRINTOUT.
               READ EXT-GOOD-FILE AT END MOVE HIGH-VALUES TO ACCNUM-EXT.
               READ MASTER-FILE AT END MOVE HIGH-VALUES TO ACCNUM-M.
             PERFORM MAIN-PROC UNTIL ACCNUM-EXT = HIGH-VALUES AND
                                     ACCNUM-M = HIGH-VALUES.
             PERFORM PRINT-WRITE.
             CLOSE MASTER-FILE
                   EXT-GOOD-FILE
                   NEW-MASTER-FILE
                   PRINTOUT.
             STOP RUN.
      * MAIN LOOP SECTION TO DETERMINE WHETHER THE FILE IS AN UPDATE *
      * A FILE THAT ONLY APPEARS ON THE MASTER INPUT OR A FILE ONLY *
      * ON THE SORTED GOOD FILE *
       MAIN-PROC SECTION.
             EVALUATE TRUE
      *         WHEN ACCNUM-EXT < ACCNUM-M
                WHEN ACCNUM-EXT > ACCNUM-M
                 ADD 1 TO TOT-RECS(2) TOT-RECS(4)
                 DISPLAY 'EXT-MATCH' ACCNUM-EXT ' ' ACCNUM-M
                    PERFORM EXT-MATCH
      *        WHEN ACCNUM-EXT > ACCNUM-M
               WHEN ACCNUM-EXT < ACCNUM-M
                 ADD 1 TO TOT-RECS(1) TOT-RECS(4)
                 DISPLAY 'MAST-MATCH' ACCNUM-EXT ' ' ACCNUM-M
                    PERFORM MASTER-MOVE
               WHEN ACCNUM-EXT = ACCNUM-M
                 ADD 1 TO TOT-RECS(3) TOT-RECS(4)
                 DISPLAY 'MATCH' ACCNUM-EXT ' ' ACCNUM-M
                    PERFORM MATCH
            END-EVALUATE.
      * EXTRACTED FILE WITH NO MATCH ON THE MASTER EXTRACTED MOVES *
      * TO NEW MASTER PRODUCING AN UNCHANGED RECORD *
       EXT-MATCH SECTION.
            MOVE ACCNUM-EXT TO ACCNUM-NM
            MOVE INITIAL-EXT TO INITIAL-NM
            MOVE SURNAME-EXT TO SURNAME-NM
            MOVE SPACES TO FILLER-NM
            MOVE TRANAMT-EXT TO BALANCE-NM
            MOVE CDMKR-EXT TO CDMKR-NM(1)
            MOVE TRANAMT-EXT TO TRANAMT-NM(1)
               PERFORM VARYING HIS-COUNT FROM 2 BY 1
               UNTIL HIS-COUNT > 4
                 MOVE SPACES TO CDMKR-NM(HIS-COUNT)
                 MOVE ZERO TO TRANAMT-NM(HIS-COUNT)
               END-PERFORM
            WRITE NEW-MASTER-REC.
            READ EXT-GOOD-FILE AT END MOVE HIGH-VALUES TO ACCNUM-EXT.
      * MATCHING ACCOUNT NUMBER RECORD PRODUCING AN UPDATED RECORD ON *
      * THE NEW MASTER FILE *
       MATCH SECTION.
             MOVE ACCNUM-M TO ACCNUM-NM
             MOVE INITIAL-M TO INITIAL-NM
             MOVE SURNAME-M TO SURNAME-NM
             MOVE SPACES TO FILLER-NM
                ADD TRANAMT-EXT TO BALANCE-M
             MOVE BALANCE-M TO BALANCE-NM
             MOVE CDMKR-M(1) TO CDMKR-NM(1)
             MOVE TRANAMT-M(1) TO TRANAMT-NM(1)
                PERFORM VARYING HIS-COUNT FROM 1 BY 1
                UNTIL HIS-COUNT > 4
             MOVE CDMKR-M(HIS-COUNT) TO CDMKR-NM(HIS-COUNT + 1 )
             MOVE TRANAMT-M(HIS-COUNT)
                TO TRANAMT-NM(HIS-COUNT + 1 )
             END-PERFORM
             WRITE NEW-MASTER-REC.
             READ EXT-GOOD-FILE AT END MOVE HIGH-VALUES TO ACCNUM-EXT.
             READ MASTER-FILE AT END MOVE HIGH-VALUES TO ACCNUM-M.


      * NO MATCH BETWEEN SORTED GOOD FILE AND MASTER FILE *
      * MASTER FILE MOVES TO NEW MASTER CREATING A NEW RECORD *
       MASTER-MOVE SECTION.
            MOVE MASTER-REC TO NEW-MASTER-REC.
               WRITE NEW-MASTER-REC.
             READ MASTER-FILE AT END MOVE HIGH-VALUES TO ACCNUM-M.
      * PRINT SECTION PRODUCING RECORD COUNT REPORT CALCULATING *
      * PERCENTAGE OF EACH RECORD TYPE *
       PRINT-WRITE SECTION.
           WRITE PRINTLINE FROM HEAD-1
           WRITE PRINTLINE FROM HEAD-2 AFTER ADVANCING 2
             PERFORM VARYING PR-CNT FROM 1 BY 1 UNTIL PR-CNT > 4
               MOVE RECORD-DATA(PR-CNT) TO TYPE-OF-RECORD-1
               MOVE TOT-RECS(PR-CNT) TO TOTAL-1
            COMPUTE PERC-REC(PR-CNT) ROUNDED =
            (TOT-RECS(PR-CNT) / TOT-RECS(4))
           MULTIPLY PERC-REC(PR-CNT) BY 100 GIVING PERC-REC(PR-CNT)
               MOVE PERC-REC(PR-CNT) TO PERCENTAGE-1
           WRITE PRINTLINE FROM DETAIL-LINE AFTER ADVANCING 1
           END-PERFORM. 

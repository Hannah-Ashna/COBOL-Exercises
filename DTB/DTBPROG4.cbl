       ID DIVISION.
       PROGRAM-ID. DTBPROG4.
       AUTHOR. HANNAH JACOB.
       DATE-WRITTEN. 1ST OCT 2021.
       DATE-COMPILED.
      *
       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT VOTESINPUT    ASSIGN TO FILEIN
           FILE STATUS IS WS-INPUT-STATUS.
           SELECT RESULTSOUTPUT ASSIGN TO FILEOUT
           FILE STATUS IS WS-OUTPUT-STATUS.
       DATA DIVISION.
      *
       FILE SECTION.

      *------------------------------------------
      * VOTES INPUT FILE
      *------------------------------------------
       FD VOTESINPUT BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01 VOTES-RECORD.
           03 CONSTITUENCY-ID  PIC X(4).
           03 VOTE-VALUE OCCURS 72 TIMES PIC 9(1).

           03 SPACE-FILLER     PIC X(4).

      *------------------------------------------
      * RESULTS OUTPUT FILE
      *------------------------------------------
       FD RESULTSOUTPUT BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01 RESULTS-RECORD       PIC X(80).

      *------------------------------------------
      * WORKING STORAGE SECTION
      *------------------------------------------
       WORKING-STORAGE SECTION.
       01 WS-INPUT-STATUS      PIC X(2).
           88 INPUT-OK         VALUE "00".
           88 INPUT-EOF        VALUE "10".
           88 INPUT-VALID      VALUE "00", "10".

       01 WS-OUTPUT-STATUS     PIC X(2).
           88 OUTPUT-OK        VALUE "00".

       01 WS-REC-VALID         PIC X(1).
           88 REC-VALID        VALUE "Y".
           88 REC-INVALID      VALUE "N".

       01 WS-VOTE-END          PIC X(1).
           88 VOTE-OK          VALUE "Y".
           88 VOTE-END         VALUE "N".

       01 WS-COUNTER           PIC 9(2).
       01 WS-RES               PIC 9(4).

       01 WS-TOTAL-VOTE.
         03 VOTE-COUNT OCCURS 8 TIMES PIC 9(4).

       01 WS-OUTPUT-DATA.
           03 P-NAME           PIC X(20).
           03 FILLER           PIC X(30) VALUE " ".
           03 P-VOTES          PIC 9(4).
           03 FILLER           PIC X(5)  VALUE " ".
           03 P-WINNER         PIC X(3).
           03 FILLER           PIC X(18) VALUE " ".

       01 WS-WIN-STATUS        PIC X(3)  VALUE "***".

       01 WS-DETAIL.
           03 FILLER           PIC X(20)  VALUE
              'RAVING LOONY PARTY'.
           03 FILLER           PIC X(20)  VALUE
              'CONSERVATIVE'.
           03 FILLER           PIC X(20)  VALUE
              'PLAID CYMRU'.
           03 FILLER           PIC X(20)  VALUE
              'FREE CLEETHORPES'.
           03 FILLER           PIC X(20)  VALUE
              'LABOUR'.
           03 FILLER           PIC X(20)  VALUE
              'LIBERAL'.
           03 FILLER           PIC X(20)  VALUE
              'SCOTTISH NAT.'.
           03 FILLER           PIC X(20)  VALUE
              'SPOILT'.

       01 DETAILS-REDEF REDEFINES WS-DETAIL.
           03 PARTY-NAME OCCURS 8 TIMES.
               05 PARTY-DATA          PIC X(20).

       01 PARTY-IDX            PIC 9(2).
       01 VOTE-IDX             PIC 9(2).

      *------------------------------------------
      * LINKAGE SECTION
      *------------------------------------------
       LINKAGE SECTION.

       01 INPUT-PARM.
           03 PARM-LENGTH      PIC S9(04) COMP.
           03 PARM-DATA        PIC X(4).

      *------------------------------------------
      * PROGRAM LOGIC
      *------------------------------------------
       PROCEDURE DIVISION USING INPUT-PARM.
       DT  A000-MAIN-LOGIC                         0 1 2
       CONDITIONS
       01           INPUT-EOF                        Y N
       ACTIONS
       01  DISPLAY  "STATUS - PROGRAM STARTING"    X - -
           DISPLAY  "CHOSEN CONSTITUENCY: "
           DISPLAY  INPUT-PARM
       02  PERFORMX B000-INIT-CODE                 X - -
       03  PERFORMX C000-PROCESS                   - - X
       04  REPEAT                                  - - X
       05  PERFORMX G001-FIND-WINNER               - X -
           PERFORMX X000-CLOSE-FILE
           DISPLAY  "STATUS - PROGRAM COMPLETE"
           STOP     RUN

       DT  B000-INIT-CODE
       CONDITIONS                                  0 1 2
       01           VOTE-IDX                         Y N
           >        8
       ACTIONS
       01  OPEN INPUT                              X - -
                    VOTESINPUT
           OPEN OUTPUT
                    RESULTSOUTPUT
           DISPLAY  "STATUS - FILES OPENED"
           MOVE     1
             TO     VOTE-IDX
       02  MOVE     0                              - - X
             TO     VOTE-COUNT(VOTE-IDX)
           ADD      1
             TO     VOTE-IDX
       03  PERFORMX D000-READ-FILE                 - X -
       04  REPEAT                                  - - X

       C000-PROCESS            SECTION.
           PERFORM E000-CHECK-CONST
           SET VOTE-OK TO TRUE

           IF REC-VALID
               MOVE 1 TO WS-COUNTER
               DISPLAY "Status - Adding valid const votes"
               PERFORM G000-VALIDATE-VOTE UNTIL VOTE-END
               PERFORM D000-READ-FILE
           END-IF
           IF REC-INVALID
               MOVE 1 TO WS-COUNTER
               DISPLAY "Status - Adding invalid const votes"
               PERFORM G010-COUNT-SPOILT UNTIL VOTE-END
               PERFORM D000-READ-FILE
           END-IF
           .

       DT  X000-CLOSE-FILE
       ACTIONS
       01  CLOSE    VOTESINPUT
                    RESULTSOUTPUT
           DISPLAY  "STATUS - FILES CLOSED"

       DT  E000-CHECK-CONST
       CONDITIONS                                  1 2
       01           CONSTITUENCY-ID                Y N
           =        PARM-DATA
       ACTIONS
       01  DISPLAY  "STATUS - VALID CONST"         X -
           SET      REC-VALID
            TO      TRUE
       02  DISPLAY  "STATUS - INVALID CONST"       - X
           SET      REC-INVALID
            TO      TRUE

       G000-VALIDATE-VOTE      SECTION.
           SET VOTE-OK TO TRUE
           IF VOTE-VALUE(WS-COUNTER) NOT = " " AND WS-COUNTER < 77
               EVALUATE VOTE-VALUE(WS-COUNTER)
                   WHEN 0
                       DISPLAY "+1 Vote for Raving Loony Party"
                       ADD 1 TO VOTE-COUNT(1)
                   WHEN 1
                       DISPLAY "+1 Vote for Conservative"
                       ADD 1 TO VOTE-COUNT(2)
                   WHEN 2
                       DISPLAY "+1 Vote for Plaid Cymru"
                       ADD 1 TO VOTE-COUNT(3)
                   WHEN 3
                       DISPLAY "+1 Vote for Free Cleethorpes"
                       ADD 1 TO VOTE-COUNT(4)
                   WHEN 4
                       DISPLAY "+1 Vote for Labour"
                       ADD 1 TO VOTE-COUNT(5)
                   WHEN 5
                       DISPLAY "+1 Vote for Liberal"
                       ADD 1 TO VOTE-COUNT(6)
                   WHEN 6
                       DISPLAY "+1 Vote for Scottish National"
                       ADD 1 TO VOTE-COUNT(7)
                   WHEN OTHER
                       DISPLAY "+1 for Spoilt Vote"
                       ADD 1 TO VOTE-COUNT(8)
               END-EVALUATE
           ELSE
               DISPLAY "Status - Vote Validation Complete"
               SET VOTE-END TO TRUE
           END-IF
           ADD 1 TO WS-COUNTER
           .











       G001-FIND-WINNER        SECTION.
           COMPUTE WS-RES = FUNCTION MAX(VOTE-COUNT(1) VOTE-COUNT(2)
                                         VOTE-COUNT(3) VOTE-COUNT(4)
                                         VOTE-COUNT(4) VOTE-COUNT(5)
                                         VOTE-COUNT(6) VOTE-COUNT(7))
           DISPLAY "HIGHEST VOTE COUNT IS: " WS-RES

           PERFORM VARYING PARTY-IDX FROM 1 BY 1 UNTIL PARTY-IDX > 8
               SET  VOTE-IDX              TO PARTY-IDX
               MOVE VOTE-COUNT(VOTE-IDX)  TO P-VOTES
               MOVE PARTY-DATA(PARTY-IDX) TO P-NAME

               IF VOTE-COUNT(VOTE-IDX) = WS-RES AND VOTE-IDX < 8
                   MOVE WS-WIN-STATUS  TO P-WINNER
               ELSE
                   MOVE SPACES            TO P-WINNER
               END-IF

               WRITE RESULTS-RECORD FROM WS-OUTPUT-DATA
           END-PERFORM
           .

       DT  G010-COUNT-SPOILT
       CONDITIONS                                  0 1 2 3 4
       01           VOTE-VALUE(WS-COUNTER)           Y N Y N
           =        " "
       02           WS-COUNTER                       Y Y N N
           >        76
       ACTIONS
       01  SET      VOTE-OK                        X - - - -
            TO      TRUE
       02  SET      VOTE-END                       - X X X -
            TO      TRUE
       03  ADD      1                              - - - - X
            TO      VOTE-COUNT(8)
       04  ADD      1                              - X X X X
            TO      WS-COUNTER

       DT  D000-READ-FILE
       ACTIONS
       01  READ     VOTESINPUT
           DISPLAY  "NEW RECORD: "
           DISPLAY  VOTES-RECORD
 

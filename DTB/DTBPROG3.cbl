       ID DIVISION.
       PROGRAM-ID. DTBPROG3.
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
       02  PERFORMX B000-INIT-CODE                 X - -
       03  DISPLAY  "C-PROCESS STARTING"           - - X
           PERFORMX C000-PROCESS
       04  REPEAT                                  - - X
       05  DISPLAY  "WRITING OUTPUT"               - X -
           PERFORMX G001-FIND-WINNER
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

       DT  C000-PROCESS
       CONDITIONS                                  0 1 2 3 4
       01           REC-VALID                        Y N Y N
       02           VOTE-END                         Y Y N N
       ACTIONS
       01  PERFORMX E000-CHECK-CONST               X - - - -
           SET      VOTE-OK
            TO      TRUE
           MOVE     1
             TO     WS-COUNTER
       02  PERFORMX G000-VALIDATE-VOTE             - - - X -
       03  PERFORMX G010-COUNT-SPOILT              - - - - X
       04  PERFORMX D000-READ-FILE                 - X X - -
           DISPLAY  "READ NEW LINE"
       05  REPEAT                                  - - - X X

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

       DT  G000-VALIDATE-VOTE
       CONDITIONS                                  0 1 2
       01           VOTE-VALUE(WS-COUNTER)           Y N
           NOT =    " "
           AND      WS-COUNTER
           <        77
       ACTIONS
       01  SET      VOTE-OK                        X - -
            TO      TRUE
       02  PERFORMX G001-VALIDATE-VOTE             - X -
       03  DISPLAY  "STATUS - VALIDATION DONE"     - - X
           SET      VOTE-END
            TO      TRUE
       04  ADD      1                              - X X
            TO      WS-COUNTER

       DT  G001-VALIDATE-VOTE
       CONDITIONS                                  1 2 3 4 5 6 7 8
       01           VOTE-VALUE(WS-COUNTER)         Y N N N N N N N
           =        0
       02           VOTE-VALUE(WS-COUNTER)         N Y N N N N N N
           =        1
       03           VOTE-VALUE(WS-COUNTER)         N N Y N N N N N
           =        2
       04           VOTE-VALUE(WS-COUNTER)         N N N Y N N N N
           =        3
       05           VOTE-VALUE(WS-COUNTER)         N N N N Y N N N
           =        4
       06           VOTE-VALUE(WS-COUNTER)         N N N N N Y N N
           =        5
       07           VOTE-VALUE(WS-COUNTER)         N N N N N N Y N
           =        6
       ACTIONS
       01  ADD      1                              X - - - - - - -
            TO      VOTE-COUNT(1)
       02  ADD      1                              - X - - - - - -
            TO      VOTE-COUNT(2)
       03  ADD      1                              - - X - - - - -
            TO      VOTE-COUNT(3)
       04  ADD      1                              - - - X - - - -
            TO      VOTE-COUNT(4)
       05  ADD      1                              - - - - X - - -
            TO      VOTE-COUNT(5)
       06  ADD      1                              - - - - - X - -
            TO      VOTE-COUNT(6)
       07  ADD      1                              - - - - - - X -
            TO      VOTE-COUNT(7)
       08  ADD      1                              - - - - - - - X
            TO      VOTE-COUNT(8)

       DT  G001-FIND-WINNER
       CONDITIONS                                  0 1 2 3 4
       01           PARTY-IDX                        Y N Y N
           >        8
       02           VOTE-COUNT(VOTE-IDX)             Y Y N N
           =        WS-RES
           AND      VOTE-IDX
           <        8
       ACTIONS
       01  COMPUTE  WS-RES                         X - - - -
           = FUNCTION MAX
                    (VOTE-COUNT(1) VOTE-COUNT(2)
                    VOTE-COUNT(3) VOTE-COUNT(4)
                    VOTE-COUNT(4) VOTE-COUNT(5)
                    VOTE-COUNT(6) VOTE-COUNT(7))
           MOVE     1
             TO     PARTY-IDX
       02  MOVE     PARTY-IDX                      - - X - X
             TO     VOTE-IDX
           MOVE     VOTE-COUNT(VOTE-IDX)
             TO     P-VOTES
           MOVE     PARTY-DATA(PARTY-IDX)
             TO     P-NAME
       03  MOVE     WS-WIN-STATUS                  - - X - -
             TO     P-WINNER
       04  MOVE     SPACES                         - - - - X
             TO     P-WINNER
       05  WRITE    RESULTS-RECORD                 - - X - X
            FROM    WS-OUTPUT-DATA
           DISPLAY  "WRITE RECORD"
           ADD      1
             TO     PARTY-IDX
       06  REPEAT                                  - - X - X

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
           AT END SET
                    INPUT-EOF
           TO TRUE 

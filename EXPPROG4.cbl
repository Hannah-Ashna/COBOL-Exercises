       ID DIVISION.                                                     00000108
       PROGRAM-ID. EXPPROG4.                                            00000208
       AUTHOR. HANNAH JACOB.                                            00000308
       DATE-WRITTEN. 23RD JULY 2021.                                    00000408
       DATE-COMPILED.                                                   00000508
      *                                                                 00000608
       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.                                                    00000708
           SELECT VOTESINPUT    ASSIGN TO FILEIN                        00000808
           FILE STATUS IS WS-INPUT-STATUS.                              00000908
           SELECT RESULTSOUTPUT ASSIGN TO FILEOUT                       00001008
           FILE STATUS IS WS-OUTPUT-STATUS.                             00001108
       DATA DIVISION.                                                   00001208
      *                                                                 00001308
       FILE SECTION.                                                    00001408
      *------------------------------------------                       00001508
      * VOTES INPUT FILE                                                00001608
      *------------------------------------------                       00001708
       FD VOTESINPUT BLOCK CONTAINS 0 RECORDS                           00001808
           RECORDING MODE IS F.                                         00001908
       01 VOTES-RECORD.                                                 00002008
           03 CONSTITUENCY-ID  PIC X(4).                                00002108
           03 VOTE-VALUE OCCURS 72 TIMES PIC 9(1).                      00002208

           03 SPACE-FILLER     PIC X(4).                                00002308
      *------------------------------------------                       00002408
      * RESULTS OUTPUT FILE                                             00002508
      *------------------------------------------                       00002608
       FD RESULTSOUTPUT BLOCK CONTAINS 0 RECORDS                        00002708
           RECORDING MODE IS F.                                         00002808
       01 RESULTS-RECORD       PIC X(80).                               00002908
      *------------------------------------------                       00003008
      * WORKING STORAGE SECTION                                         00003108
      *------------------------------------------                       00003208
       WORKING-STORAGE SECTION.                                         00003308
       01 WS-INPUT-STATUS      PIC X(2).                                00003408
           88 INPUT-OK         VALUE "00".                              00003508
           88 INPUT-EOF        VALUE "10".                              00003608
           88 INPUT-VALID      VALUE "00", "10".                        00003708
                                                                        00003808
       01 WS-OUTPUT-STATUS     PIC X(2).                                00003908
           88 OUTPUT-OK        VALUE "00".                              00004008
                                                                        00004108
       01 WS-REC-VALID         PIC X(1).                                00004208
           88 REC-VALID        VALUE "Y".                               00004308
           88 REC-INVALID      VALUE "N".                               00004408

       01 WS-VOTE-END          PIC X(1).                                00004508
           88 VOTE-OK          VALUE "Y".                               00004608
           88 VOTE-END         VALUE "N".                               00004708
                                                                        00004808
       01 WS-COUNTER           PIC 9(2).                                00004908
       01 WS-RES               PIC 9(4).

       01 WS-TOTAL-VOTE.                                                00005408
         03 VOTE-COUNT OCCURS 8 TIMES INDEXED BY VOTE-IDX PIC 9(4).

       01 WS-OUTPUT-DATA.                                               00005508
           03 P-NAME           PIC X(20).                               00005608
           03 FILLER           PIC X(30) VALUE " ".                     00005708
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
           03 PARTY-NAME OCCURS 8 TIMES INDEXED BY PARTY-IDX.
               05 PARTY-DATA   PIC X(20).
      *------------------------------------------
      * LINKAGE SECTION
      *------------------------------------------
       LINKAGE SECTION.
                                                                        00006508
       01 INPUT-PARM.                                                   00006608
           03 PARM-LENGTH      PIC S9(04) COMP.
           03 PARM-DATA        PIC X(4).
      *------------------------------------------                       00006708
      * Program Logic                                                   00006808
      *------------------------------------------                       00006908
       PROCEDURE DIVISION USING INPUT-PARM.                             00007608
       A000-MAIN-LOGIC         SECTION.                                 00007708
           DISPLAY "Status - Program Starting..."                       00007808
           DISPLAY "CHOSEN CONSTITUENCY: " INPUT-PARM

           PERFORM B000-INIT-CODE                                       00007908
           PERFORM C000-PROCESS UNTIL INPUT-EOF                         00008008
           PERFORM G001-FIND-WINNER
           PERFORM X000-CLOSE-FILE                                      00008308

           DISPLAY "Status - Program Complete"                          00008408
           STOP RUN.                                                    00008508
                                                                        00008608
       B000-INIT-CODE          SECTION.                                 00008708
           OPEN INPUT  VOTESINPUT                                       00008808
           OPEN OUTPUT RESULTSOUTPUT                                    00008908
           DISPLAY "Status - Files Opened"                              00009008

           PERFORM VARYING VOTE-IDX FROM 1 BY 1 UNTIL VOTE-IDX > 8
               MOVE 0 TO VOTE-COUNT(VOTE-IDX)
           END-PERFORM

           PERFORM D000-READ-FILE                                       00009108
           .                                                            00009208
                                                                        00009308
       C000-PROCESS            SECTION.                                 00009408
           PERFORM E000-CHECK-CONST                                     00009508
           SET VOTE-OK TO TRUE

           IF REC-VALID                                                 00009608
               MOVE 1 TO WS-COUNTER                                     00009708
               DISPLAY "Status - Adding valid const votes"
               PERFORM G000-VALIDATE-VOTE UNTIL VOTE-END                00009808
               PERFORM D000-READ-FILE                                   00009908
           END-IF
           IF REC-INVALID                                               00010008
               MOVE 1 TO WS-COUNTER
               DISPLAY "Status - Adding invalid const votes"
               PERFORM G010-COUNT-SPOILT UNTIL VOTE-END
               PERFORM D000-READ-FILE                                   00010108
           END-IF                                                       00010208
           .                                                            00010308
                                                                        00010908
       X000-CLOSE-FILE         SECTION.                                 00011008
           CLOSE   VOTESINPUT                                           00011108
                   RESULTSOUTPUT                                        00011208
                                                                        00011308
           DISPLAY "Status - Files Closed"                              00011408
           .                                                            00011508
                                                                        00011608
       E000-CHECK-CONST        SECTION.                                 00011708
           EVALUATE TRUE                                                00011908
               WHEN CONSTITUENCY-ID = PARM-DATA                         00012008
                   DISPLAY "Status - Valid Constituency"                00012108
                   SET REC-VALID TO TRUE                                00012208
               WHEN OTHER                                               00012308
                   DISPLAY "Status - Invalid Constituency"              00012408
                   SET REC-INVALID TO TRUE                              00012508
           END-EVALUATE                                                 00012608
           .                                                            00012708
                                                                        00012808
       G000-VALIDATE-VOTE      SECTION.                                 00012908
           SET VOTE-OK TO TRUE                                          00013008
           IF VOTE-VALUE(WS-COUNTER) NOT = " " AND WS-COUNTER < 77      00013108
               EVALUATE VOTE-VALUE(WS-COUNTER)                          00013208
                   WHEN 0                                               00013308
                       DISPLAY "+1 Vote for Raving Loony Party"         00013408
                       ADD 1 TO VOTE-COUNT(1)                           00013508
                   WHEN 1                                               00013608
                       DISPLAY "+1 Vote for Conservative"               00013708
                       ADD 1 TO VOTE-COUNT(2)                           00013808
                   WHEN 2                                               00013908
                       DISPLAY "+1 Vote for Plaid Cymru"                00014008
                       ADD 1 TO VOTE-COUNT(3)                           00014108
                   WHEN 3                                               00014208
                       DISPLAY "+1 Vote for Free Cleethorpes"           00014308
                       ADD 1 TO VOTE-COUNT(4)                           00014408
                   WHEN 4                                               00014508
                       DISPLAY "+1 Vote for Labour"                     00014608
                       ADD 1 TO VOTE-COUNT(5)                           00014708
                   WHEN 5                                               00014808
                       DISPLAY "+1 Vote for Liberal"                    00014908
                       ADD 1 TO VOTE-COUNT(6)                           00015008
                   WHEN 6                                               00015108
                       DISPLAY "+1 Vote for Scottish National"          00015208
                       ADD 1 TO VOTE-COUNT(7)                           00015308
                   WHEN OTHER                                           00015408
                       DISPLAY "+1 for Spoilt Vote"                     00015508
                       ADD 1 TO VOTE-COUNT(8)                           00015608
               END-EVALUATE                                             00015708
           ELSE                                                         00015808
               DISPLAY "Status - Vote Validation Complete"              00015908
               SET VOTE-END TO TRUE                                     00016008
           END-IF                                                       00016108
           ADD 1 TO WS-COUNTER                                          00016208
           .                                                            00016308
                                                                        00016408
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

       G010-COUNT-SPOILT       SECTION.
           SET VOTE-OK TO TRUE

           IF VOTE-VALUE(WS-COUNTER) =  " " OR WS-COUNTER > 76
               SET VOTE-END TO TRUE
           ELSE
               ADD 1 TO VOTE-COUNT(8)
           END-IF

           ADD 1 TO WS-COUNTER
           .

       D000-READ-FILE          SECTION.
           READ VOTESINPUT
           DISPLAY "New Record: " VOTES-RECORD
           . 

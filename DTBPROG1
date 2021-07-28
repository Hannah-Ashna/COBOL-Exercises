       ID DIVISION.
       PROGRAM-ID. DTBPROG1.
       AUTHOR. HANNAH JACOB.
       DATE-WRITTEN. 28TH JULY 2021.
       DATE-COMPILED.
      *
       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT ACC-FILEIN   ASSIGN TO CDIN
           FILE STATUS IS WS-FILE-STATUS.
           SELECT ACC-FILEOUT  ASSIGN TO CDOUT.
       DATA DIVISION.
      *
       FILE SECTION.
      *
       FD ACC-FILEIN BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01 ACC-RECORDA.
           03 ACCOUNT-A              PIC X(8).
           03 ACCOUNT-NAME.
               05 USER-TITLE         PIC X(3).
               05 INITIALS.
                   07 FIRST-INITIAL  PIC X(1).
                   07 MIDDLE-INITIAL PIC X(1).
               05 SURNAME            PIC X(30).
           03 DATE-OF-ISSUE          PIC X(8).
           03 DATE-OF-RECEIPT        PIC X(8).
           03 BALANCE                PIC 9(10).
           03 FILLER-A               PIC X(10).
           03 TYPE-A                 PIC X(1).

       01 ACC-RECORDB.
           03 ACCOUNT-B              PIC X(8).
           03 USER-ADDRESS.
               05 ADDRESS-NUM        PIC 9(4).
               05 STREET             PIC X(20).
               05 TOWN               PIC X(20).
               05 COUNTY             PIC X(10).
               05 POST-CODE          PIC X(10).
               05 FILLER-B           PIC X(7).
               05 TYPE-B             PIC X(1).

       FD ACC-FILEOUT BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01 ACC-RECORDOUT.
           03 ACCOUNT-NUM            PIC X(8).
           03 COUNTY-OUT             PIC X(10).
           03 BALANCE-OUT            PIC 9(10).
           03 UNUSED                 PIC X(51).
           03 TYPE-OUT               PIC X(1).

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS             PIC X(2).
           88 FILE-OK                VALUE "00".
           88 FILE-EOF               VALUE "01".
           88 FILE-VALID             VALUE "00", "01".

      *
       PROCEDURE DIVISION.
       DT  MAIN-LOGIC                              0 1 2
       CONDITIONS
       01           FILE-EOF                         Y N

       ACTIONS
       01  DISPLAY  "PROGRAM START"                X - -
           PERFORMX A100-INITIALISE
       02  PERFORMX C100-READ-DATA                 X - -
       03  PERFORMX B100-MOVE-DATA                 - - X
           PERFORMX C100-READ-DATA
           REPEAT
       04  PERFORMX T100-TERMINATE                 - X -
           DISPLAY  "PROGRAM END"
           STOP     RUN

       DT  A100-INITIALISE
       ACTIONS
       01  OPEN INPUT
                    ACC-FILEIN
           OPEN OUTPUT
                    ACC-FILEOUT
           DISPLAY  "STATUS - FILES OPEN"

       DT  T100-TERMINATE
       ACTIONS
       01  CLOSE    ACC-FILEIN
           CLOSE    ACC-FILEOUT
           DISPLAY  "STATUS - FILES CLOSED"

       DT  B100-MOVE-DATA
       ACTIONS
       01  MOVE     ACCOUNT-A
             TO     ACCOUNT-NUM
           MOVE     BALANCE
             TO     BALANCE-OUT
           READ     ACC-FILEIN
           MOVE     COUNTY
             TO     COUNTY-OUT
           MOVE     '0'
             TO     TYPE-OUT
           MOVE     SPACES
             TO     UNUSED
           DISPLAY  ACC-RECORDOUT
           WRITE    ACC-RECORDOUT

       DT  C100-READ-DATA
       ACTIONS
       01  READ     ACC-FILEIN
           AT END SET
                    FILE-EOF
           TO TRUE 

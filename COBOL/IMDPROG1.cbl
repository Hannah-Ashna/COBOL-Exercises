       ID DIVISION.
       PROGRAM-ID. EXPPROG3.
       AUTHOR. HANNAH JACOB.
       DATE-WRITTEN. 11TH AUG 2021.
       DATE-COMPILED.
      *
       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
       DATA DIVISION.
      *
       FILE SECTION.
      *
       WORKING-STORAGE SECTION.
       01 WS-GENERAL-FILLER.
           03 WS-WRITE               PIC X(4) VALUE 'WS  '.
           03 WS-OPEN-INPUT          PIC X(4) VALUE 'OI  '.
           03 WS-OPEN-OUTPUT         PIC X(4) VALUE 'OO  '.
           03 WS-READ-INPUT          PIC X(4) VALUE 'RS  '.
               88 IS-EOF                      VALUE 'RSEF'.
           03 WS-CLOSE               PIC X(4) VALUE 'C   '.
           03 WS-FILE-01             PIC X(8) VALUE 'FILE1   '.
           03 WS-FILE-02             PIC X(8) VALUE 'FILE2   '.

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

       01 ACC-RECORDOUT.
           03 ACCOUNT-NUM            PIC X(8).
           03 COUNTY-OUT             PIC X(10).
           03 BALANCE-OUT            PIC 9(10).
           03 UNUSED                 PIC X(51).
           03 TYPE-OUT               PIC X(1).

       PROCEDURE DIVISION.
       A100-BEGIN     SECTION.
           CALL    'IOMOD'
             USING WS-OPEN-INPUT
                   WS-FILE-01
           CALL    'IOMOD'
             USING WS-OPEN-OUTPUT
                   WS-FILE-02
           CALL 'IOMOD'
             USING WS-READ-INPUT
                   WS-FILE-01
                   ACC-RECORDA

           PERFORM B100-MOVE-DATA UNTIL IS-EOF

           CALL    'IOMOD'
             USING WS-CLOSE
                   WS-FILE-01
           CALL    'IOMOD'
             USING WS-CLOSE
                   WS-FILE-02

           STOP RUN.

       B100-MOVE-DATA     SECTION.
           MOVE ACCOUNT-A TO ACCOUNT-NUM
           MOVE BALANCE   TO BALANCE-OUT
           CALL    'IOMOD'
             USING WS-READ-INPUT
                   WS-FILE-01
                   ACC-RECORDB
           DISPLAY ACC-RECORDB
           MOVE COUNTY    TO COUNTY-OUT
           MOVE '0'       TO TYPE-OUT
           MOVE SPACES    TO UNUSED
           CALL     'IOMOD'
              USING WS-WRITE
                    WS-FILE-02 ACC-RECORDOUT
           CALL    'IOMOD'
             USING WS-READ-INPUT
                   WS-FILE-01
                   ACC-RECORDA
           DISPLAY ACC-RECORDA
           . 

       ID DIVISION.
       PROGRAM-ID. EXPPROG1.
       AUTHOR. HANNAH JACOB.
       INSTALLATION. RSM.
       DATE-WRITTEN. 19TH JULY 2021.
       DATE-COMPILED.
      *
       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT ACC-FILEIN ASSIGN TO CDIN.
           SELECT ACC-FILEOUT ASSIGN TO CDOUT.
       DATA DIVISION.
      *
       FILE SECTION.
      *
       FD ACC-FILEIN BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01 ACC-RECORDA.
           03 ACCOUNT-A PIC X(8).
           03 ACCOUNT-NAME.
               05 USER-TITLE PIC X(3).
               05 INITIALS.
                   07 FIRST-INITIAL PIC X(1).
                   07 MIDDLE-INITIAL PIC X(1).
               05 SURNAME PIC X(30).
           03 DATE-OF-ISSUE PIC X(8).
           03 DATE-OF-RECEIPT PIC X(8).
           03 BALANCE PIC 9(10).
           03 FILLER-A PIC X(10).
           03 TYPE-A PIC X(1).
       01 ACC-RECORDB.
           03 ACCOUNT-B PIC X(8).
           03 USER-ADDRESS.
               05 ADDRESS-NUM PIC 9(4).
               05 STREET PIC X(20).
               05 TOWN PIC X(20).
               05 COUNTY PIC X(10).
               05 POST-CODE PIC X(10).
               05 FILLER-B PIC X(7).
               05 TYPE-B PIC X(1).
       FD ACC-FILEOUT BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01 ACC-RECORDOUT.
           03 ACCOUNT-NUM PIC X(8).
           03 COUNTY-OUT PIC X(10).
           03 BALANCE-OUT PIC 9(10).
           03 UNUSED PIC X(51).
           03 TYPE-OUT PIC X(1).
       WORKING-STORAGE SECTION.
       01 EOF-POINT PIC X    VALUE 'N'.
      *
       PROCEDURE DIVISION.
       BEGIN.
           OPEN INPUT ACC-FILEIN
                OUTPUT ACC-FILEOUT.
           READ ACC-FILEIN AT END MOVE 'Y' TO EOF-POINT.
           PERFORM MOVE-DATA UNTIL EOF-POINT = 'Y'.
           CLOSE ACC-FILEIN
                 ACC-FILEOUT.
           STOP RUN.
       MOVE-DATA.
           MOVE ACCOUNT-A TO ACCOUNT-NUM.
           MOVE BALANCE TO BALANCE-OUT.
      * MOVE TO THE NEXT LINE (DATA ALTERNATES E.G. ABAB)
           READ ACC-FILEIN.
           MOVE COUNTY TO COUNTY-OUT.
           MOVE '0' TO TYPE-OUT.
           MOVE SPACES TO UNUSED.
      * WRITE TO OUTPUT FILE
           WRITE ACC-RECORDOUT.
      * MOVE TO THE NEXT LINE TO GET THE NEXT CUSTOMER'S RECORDS
           READ ACC-FILEIN AT END MOVE 'Y' TO EOF-POINT.
        ID DIVISION.
       PROGRAM-ID. EXPPROG1.
       AUTHOR. HANNAH JACOB.
       INSTALLATION. RSM.
       DATE-WRITTEN. 19TH JULY 2021.
       DATE-COMPILED.
      *
       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT ACC-FILEIN ASSIGN TO CDIN.
           SELECT ACC-FILEOUT ASSIGN TO CDOUT.
       DATA DIVISION.
      *
       FILE SECTION.
      *
       FD ACC-FILEIN BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01 ACC-RECORDA.
           03 ACCOUNT-A PIC X(8).
           03 ACCOUNT-NAME.
               05 USER-TITLE PIC X(3).
               05 INITIALS.
                   07 FIRST-INITIAL PIC X(1).
                   07 MIDDLE-INITIAL PIC X(1).
               05 SURNAME PIC X(30).
           03 DATE-OF-ISSUE PIC X(8).
           03 DATE-OF-RECEIPT PIC X(8).
           03 BALANCE PIC 9(10).
           03 FILLER-A PIC X(10).
           03 TYPE-A PIC X(1).
       01 ACC-RECORDB.
           03 ACCOUNT-B PIC X(8).
           03 USER-ADDRESS.
               05 ADDRESS-NUM PIC 9(4).
               05 STREET PIC X(20).
               05 TOWN PIC X(20).
               05 COUNTY PIC X(10).
               05 POST-CODE PIC X(10).
               05 FILLER-B PIC X(7).
               05 TYPE-B PIC X(1).
       FD ACC-FILEOUT BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01 ACC-RECORDOUT.
           03 ACCOUNT-NUM PIC X(8).
           03 COUNTY-OUT PIC X(10).
           03 BALANCE-OUT PIC 9(10).
           03 UNUSED PIC X(51).
           03 TYPE-OUT PIC X(1).
       WORKING-STORAGE SECTION.
       01 EOF-POINT PIC X    VALUE 'N'.
      *
       PROCEDURE DIVISION.
       BEGIN.
           OPEN INPUT ACC-FILEIN
                OUTPUT ACC-FILEOUT.
           READ ACC-FILEIN AT END MOVE 'Y' TO EOF-POINT.
           PERFORM MOVE-DATA UNTIL EOF-POINT = 'Y'.
           CLOSE ACC-FILEIN
                 ACC-FILEOUT.
           STOP RUN.
       MOVE-DATA.
           MOVE ACCOUNT-A TO ACCOUNT-NUM.
           MOVE BALANCE TO BALANCE-OUT.
      * MOVE TO THE NEXT LINE (DATA ALTERNATES E.G. ABAB)
           READ ACC-FILEIN.
           MOVE COUNTY TO COUNTY-OUT.
           MOVE '0' TO TYPE-OUT.
           MOVE SPACES TO UNUSED.
      * WRITE TO OUTPUT FILE
           WRITE ACC-RECORDOUT.
      * MOVE TO THE NEXT LINE TO GET THE NEXT CUSTOMER'S RECORDS
           READ ACC-FILEIN AT END MOVE 'Y' TO EOF-POINT.
        ID DIVISION.
       PROGRAM-ID. EXPPROG1.
       AUTHOR. HANNAH JACOB.
       INSTALLATION. RSM.
       DATE-WRITTEN. 19TH JULY 2021.
       DATE-COMPILED.
      *
       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT ACC-FILEIN ASSIGN TO CDIN.
           SELECT ACC-FILEOUT ASSIGN TO CDOUT.
       DATA DIVISION.
      *
       FILE SECTION.
      *
       FD ACC-FILEIN BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01 ACC-RECORDA.
           03 ACCOUNT-A PIC X(8).
           03 ACCOUNT-NAME.
               05 USER-TITLE PIC X(3).
               05 INITIALS.
                   07 FIRST-INITIAL PIC X(1).
                   07 MIDDLE-INITIAL PIC X(1).
               05 SURNAME PIC X(30).
           03 DATE-OF-ISSUE PIC X(8).
           03 DATE-OF-RECEIPT PIC X(8).
           03 BALANCE PIC 9(10).
           03 FILLER-A PIC X(10).
           03 TYPE-A PIC X(1).
       01 ACC-RECORDB.
           03 ACCOUNT-B PIC X(8).
           03 USER-ADDRESS.
               05 ADDRESS-NUM PIC 9(4).
               05 STREET PIC X(20).
               05 TOWN PIC X(20).
               05 COUNTY PIC X(10).
               05 POST-CODE PIC X(10).
               05 FILLER-B PIC X(7).
               05 TYPE-B PIC X(1).
       FD ACC-FILEOUT BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01 ACC-RECORDOUT.
           03 ACCOUNT-NUM PIC X(8).
           03 COUNTY-OUT PIC X(10).
           03 BALANCE-OUT PIC 9(10).
           03 UNUSED PIC X(51).
           03 TYPE-OUT PIC X(1).
       WORKING-STORAGE SECTION.
       01 EOF-POINT PIC X    VALUE 'N'.
      *
       PROCEDURE DIVISION.
       BEGIN.
           OPEN INPUT ACC-FILEIN
                OUTPUT ACC-FILEOUT.
           READ ACC-FILEIN AT END MOVE 'Y' TO EOF-POINT.
           PERFORM MOVE-DATA UNTIL EOF-POINT = 'Y'.
           CLOSE ACC-FILEIN
                 ACC-FILEOUT.
           STOP RUN.
       MOVE-DATA.
           MOVE ACCOUNT-A TO ACCOUNT-NUM.
           MOVE BALANCE TO BALANCE-OUT.
      * MOVE TO THE NEXT LINE (DATA ALTERNATES E.G. ABAB)
           READ ACC-FILEIN.
           MOVE COUNTY TO COUNTY-OUT.
           MOVE '0' TO TYPE-OUT.
           MOVE SPACES TO UNUSED.
      * WRITE TO OUTPUT FILE
           WRITE ACC-RECORDOUT.
      * MOVE TO THE NEXT LINE TO GET THE NEXT CUSTOMER'S RECORDS
           READ ACC-FILEIN AT END MOVE 'Y' TO EOF-POINT.
 
ID DIVISION.
       PROGRAM-ID. EXPPROG1.
       AUTHOR. HANNAH JACOB.
       INSTALLATION. RSM.
       DATE-WRITTEN. 19TH JULY 2021.
       DATE-COMPILED.
      *
       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT ACC-FILEIN ASSIGN TO CDIN.
           SELECT ACC-FILEOUT ASSIGN TO CDOUT.
       DATA DIVISION.
      *
       FILE SECTION.
      *
       FD ACC-FILEIN BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01 ACC-RECORDA.
           03 ACCOUNT-A PIC X(8).
           03 ACCOUNT-NAME.
               05 USER-TITLE PIC X(3).
               05 INITIALS.
                   07 FIRST-INITIAL PIC X(1).
                   07 MIDDLE-INITIAL PIC X(1).
               05 SURNAME PIC X(30).
           03 DATE-OF-ISSUE PIC X(8).
           03 DATE-OF-RECEIPT PIC X(8).
           03 BALANCE PIC 9(10).
           03 FILLER-A PIC X(10).
           03 TYPE-A PIC X(1).
       01 ACC-RECORDB.
           03 ACCOUNT-B PIC X(8).
           03 USER-ADDRESS.
               05 ADDRESS-NUM PIC 9(4).
               05 STREET PIC X(20).
               05 TOWN PIC X(20).
               05 COUNTY PIC X(10).
               05 POST-CODE PIC X(10).
               05 FILLER-B PIC X(7).
               05 TYPE-B PIC X(1).
       FD ACC-FILEOUT BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01 ACC-RECORDOUT.
           03 ACCOUNT-NUM PIC X(8).
           03 COUNTY-OUT PIC X(10).
           03 BALANCE-OUT PIC 9(10).
           03 UNUSED PIC X(51).
           03 TYPE-OUT PIC X(1).
       WORKING-STORAGE SECTION.
       01 EOF-POINT PIC X    VALUE 'N'.
      *
       PROCEDURE DIVISION.
       BEGIN.
           OPEN INPUT ACC-FILEIN
                OUTPUT ACC-FILEOUT.
           READ ACC-FILEIN AT END MOVE 'Y' TO EOF-POINT.
           PERFORM MOVE-DATA UNTIL EOF-POINT = 'Y'.
           CLOSE ACC-FILEIN
                 ACC-FILEOUT.
           STOP RUN.
       MOVE-DATA.
           MOVE ACCOUNT-A TO ACCOUNT-NUM.
           MOVE BALANCE TO BALANCE-OUT.
      * MOVE TO THE NEXT LINE (DATA ALTERNATES E.G. ABAB)
           READ ACC-FILEIN.
           MOVE COUNTY TO COUNTY-OUT.
           MOVE '0' TO TYPE-OUT.
           MOVE SPACES TO UNUSED.
      * WRITE TO OUTPUT FILE
           WRITE ACC-RECORDOUT.
      * MOVE TO THE NEXT LINE TO GET THE NEXT CUSTOMER'S RECORDS
           READ ACC-FILEIN AT END MOVE 'Y' TO EOF-POINT.
 

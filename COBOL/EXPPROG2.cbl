       ID DIVISION.
       PROGRAM-ID. EXPPROG2.
       AUTHOR. HANNAH JACOB.
       DATE-WRITTEN. 20TH JULY 2021.
       DATE-COMPILED.
      *
       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT FILE-IN ASSIGN TO FILEIN
           FILE STATUS IS WS-FILEIN-STATUS.

           SELECT GOOD-OUT ASSIGN TO GOODOUT
           FILE STATUS IS WS-GOODOUT-STATUS.

           SELECT BAD-OUT ASSIGN TO BADOUT
           FILE STATUS IS WS-BADOUT-STATUS.
      *
       DATA DIVISION.

       FILE SECTION.

      *INPUT FILE - SALES DATA
        FD FILE-IN BLOCK CONTAINS 0 RECORDS
            RECORDING MODE IS F.
        01 FILE-IN-REC.
            03 DEPARTMENT     PIC 9(1).
            03 QUANTITY       PIC 9(4).
            03 RETAIL-PRICE   PIC 9(5).
            03 SALE-PRICE     PIC 9(5).
            03 INVOICE-NUM.
               05 INVCHAR-1   PIC X(1).
                  88 INVCHAR-VAL VALUE 'A' THRU 'R'.
               05 INVCHAR-4   PIC 9(4).
            03 FILLER         PIC X(59) VALUE SPACES.
            03 SALE-INDICATOR PIC X(1).
      
      *GOOD OUTPUT DATA
        FD GOOD-OUT BLOCK CONTAINS 0 RECORDS
            RECORDING MODE IS F.
        01 GOOD-REC.
            03 DEPT-1         PIC 9(1).
            03 TOTAL-1        PIC 9(7) COMP-3.
            03 DEPT-2         PIC 9(1).
            03 TOTAL-2        PIC 9(7) COMP-3.
            03 DEPT-3         PIC 9(1).
            03 TOTAL-3        PIC 9(7) COMP-3.
            03 DEPT-4         PIC 9(1).
            03 TOTAL-4        PIC 9(7) COMP-3.
            03 DEPT-5         PIC 9(1).
            03 TOTAL-5        PIC 9(7) COMP-3.
            03 FILLER         PIC X(55).
      
      *BAD OUTPUT DATA
        FD BAD-OUT BLOCK CONTAINS 0 RECORDS
            RECORDING MODE IS F.
        01 BAD-REC.
            03 DEPT           PIC 9(1).
            03 ERROR-DEPT REDEFINES DEPT PIC X(1).
            03 ERROR-VAL      PIC X(5).
            03 INV-NUM        PIC X(5).
            03 ERROR-TEXT     PIC X(69).
      
      *WORKING STORAGE
       WORKING-STORAGE SECTION.
        01 WS-TOTAL-1          PIC 9(7) COMP-3.
        01 WS-TOTAL-2          PIC 9(7) COMP-3.
        01 WS-TOTAL-3          PIC 9(7) COMP-3.
        01 WS-TOTAL-4          PIC 9(7) COMP-3.
        01 WS-TOTAL-5          PIC 9(7) COMP-3.

        01 WS-FILEIN-STATUS    PIC X(2).
           88 FILEIN-OK        VALUE "00".
           88 FILEIN-EOF       VALUE "10".
           88 FILEIN-VALID     VALUE "00", "10".
        01 WS-GOODOUT-STATUS   PIC X(2).
           88 GOODOUT-OK       VALUE "00".
        01 WS-BADOUT-STATUS    PIC X(2).
           88 BADOUT-OK        VALUE "00".
        01 WS-REC-VALID        PIC X(1).
           88 REC-VALID        VALUE "Y".
           88 REC-INVALID      VALUE "N".
      *
       PROCEDURE DIVISION.
       MAIN-LOGIC             SECTION.
           PERFORM INIT
           PERFORM TASK-PROCESS UNTIL FILEIN-EOF
           PERFORM TASK-WRITE-GOODOUT
           PERFORM TASK-CLOSE-FILES
           DISPLAY "PROGRAM COMPLETE!"
           STOP RUN.
      
      *------------------------------------
      *INITIALISE THE FILES
       INIT                   SECTION.
           OPEN INPUT  FILE-IN
           OPEN OUTPUT GOOD-OUT
           OPEN OUTPUT BAD-OUT
      
      *SET ALL TOTALS TO ZERO
           MOVE 0 TO TOTAL-1
           MOVE 0 TO TOTAL-2
           MOVE 0 TO TOTAL-3
           MOVE 0 TO TOTAL-4
           MOVE 0 TO TOTAL-5
      
      *CHECK IF ALL FILES ARE FUNCTIONAL
           IF FILEIN-OK AND GOODOUT-OK AND BADOUT-OK
               CONTINUE
           ELSE
               DISPLAY "ERROR INITIALISING FILES"
               PERFORM TASK-ERRORS
           END-IF
           
           PERFORM TASK-READ
           IF FILEIN-EOF
               DISPLAY "ERROR - FILE IS EMPTY"
               MOVE    8 TO RETURN-CODE
           END-IF.
      
      *------------------------------------
      *THE MAIN BIT OF PROCESSING
       TASK-PROCESS           SECTION.
           PERFORM TASK-VALIDATION
               IF REC-VALID
                   PERFORM TASK-MULTIPLY
                   PERFORM TASK-READ
               ELSE
                   PERFORM TASK-READ
               END-IF.

      *------------------------------------
      *CLOSE FILES
       TASK-CLOSE-FILES        SECTION.
           CLOSE FILE-IN
           CLOSE GOOD-OUT
           CLOSE BAD-OUT
           IF FILEIN-OK AND GOODOUT-OK AND BADOUT-OK
               CONTINUE
           ELSE
               DISPLAY "ERROR CLOSING FILES"
               PERFORM TASK-ERRORS
           END-IF.
      
      *------------------------------------
      *CARRY OUT VALIDATION CHECKS FOR SALES DATA
       TASK-VALIDATION        SECTION.
           SET REC-VALID TO TRUE
           DISPLAY "VALIDATING RECORD..." FILE-IN-REC

           IF DEPARTMENT NOT = 1 AND 2 AND 3 AND 4 AND 5
               MOVE " ERROR - INVALID DEPT. NUMBER" TO ERROR-TEXT
               DISPLAY "ERROR - INVALID DEPT. NUMBER"
               PERFORM TASK-WRITE-BADOUT
               SET REC-INVALID TO TRUE
           END-IF

           IF DEPARTMENT NOT NUMERIC
               MOVE " ERROR - INVALID DEPT. NUMBER" TO ERROR-TEXT
               DISPLAY "ERROR - INVALID DEPT. NUMBER"
               PERFORM TASK-WRITE-BADOUT
               SET REC-INVALID TO TRUE
           END-IF

           IF SALE-INDICATOR NOT = 'S' AND ' '
               MOVE " ERROR - INVALID SALE INDICATOR" TO ERROR-TEXT
               DISPLAY "ERROR - INVALID SALE INDICATOR"
               PERFORM TASK-WRITE-BADOUT
               SET REC-INVALID TO TRUE
           END-IF

           IF QUANTITY NOT NUMERIC
               MOVE " ERROR - QUANTITY NOT NUMERIC" TO ERROR-TEXT
               DISPLAY "ERROR - QUANTITY NOT NUMERIC"
               PERFORM TASK-WRITE-BADOUT
               SET REC-INVALID TO TRUE
           END-IF

           IF RETAIL-PRICE NOT NUMERIC
               MOVE " ERROR - RETAIL PRICE NOT NUMERIC" TO ERROR-TEXT
               DISPLAY  "ERROR - RETAIL PRICE NOT NUMERIC"
               PERFORM TASK-WRITE-BADOUT
               SET REC-INVALID TO TRUE
           END-IF

           IF SALE-PRICE NOT NUMERIC
               MOVE " ERROR - SALE PRICE NOT NUMERIC" TO ERROR-TEXT
               DISPLAY "ERROR - SALE PRICE NOT NUMERIC"
               PERFORM TASK-WRITE-BADOUT
               SET REC-INVALID TO TRUE
           END-IF

           IF QUANTITY NOT > 0
               MOVE " ERROR - QUANTITY IS 0" TO ERROR-TEXT
               DISPLAY "ERROR - QUANTITY IS 0"
               PERFORM TASK-WRITE-BADOUT
               SET REC-INVALID TO TRUE
           END-IF

           IF RETAIL-PRICE NOT > 0
               MOVE " ERROR - RETAIL PRICE IS 0" TO ERROR-TEXT
               DISPLAY "ERROR - RETAIL PRICE IS 0"
               PERFORM TASK-WRITE-BADOUT
               SET REC-INVALID TO TRUE
           END-IF

           IF SALE-INDICATOR = 'S' AND SALE-PRICE = 0
               MOVE " ERROR - SALE PRICE IS 0" TO ERROR-TEXT
               DISPLAY "ERROR - SALE PRICE IS 0"
               PERFORM TASK-WRITE-BADOUT
               SET REC-INVALID TO TRUE
           END-IF

           IF SALE-INDICATOR = ' ' AND SALE-PRICE NOT = 0
               MOVE " ERROR - SALE PRICE IS NOT 0" TO ERROR-TEXT
               DISPLAY "ERROR - SALE PRICE IS NOT 0"
               PERFORM TASK-WRITE-BADOUT
               SET REC-INVALID TO TRUE
           END-IF

           IF SALE-INDICATOR = 'S' AND SALE-PRICE > RETAIL-PRICE
               MOVE " ERROR - SP > RP" TO ERROR-TEXT
               DISPLAY  "ERROR - SP > RP"
               PERFORM TASK-WRITE-BADOUT
               SET REC-INVALID TO TRUE
           END-IF

           IF INVCHAR-VAL
               MOVE INVOICE-NUM TO INV-NUM
           ELSE
               MOVE " ERROR - INVOICE CHAR NOT A-R" TO ERROR-TEXT
               DISPLAY "ERROR - INVOICE CHAR NOT A-R"
               PERFORM TASK-WRITE-BADOUT
               SET REC-INVALID TO TRUE
           END-IF

           IF INVCHAR-1 NOT ALPHABETIC
               MOVE " ERROR - INVOICE NOT ALPHABETIC" TO ERROR-TEXT
               DISPLAY "ERROR - CHAR IS A NUMBER"
               PERFORM TASK-WRITE-BADOUT
               SET REC-INVALID TO TRUE
           END-IF

           IF INVCHAR-4 NOT NUMERIC
               MOVE " ERROR - INVOICE NOT NUMERIC" TO ERROR-TEXT
               DISPLAY "ERROR - CHAR IS NOT A NUMBER"
               PERFORM TASK-WRITE-BADOUT
               SET REC-INVALID TO TRUE
           END-IF.

      *------------------------------------
      *MULTIPLY VALUES TO ADD TO THE TOTAL
       TASK-MULTIPLY           SECTION.
           EVALUATE TRUE
               WHEN DEPARTMENT = 1 AND SALE-INDICATOR = " "
                    MULTIPLY RETAIL-PRICE BY QUANTITY
                    GIVING WS-TOTAL-1
                    ADD WS-TOTAL-1 TO TOTAL-1
               WHEN DEPARTMENT = 1 AND SALE-INDICATOR = "S"
                    MULTIPLY SALE-PRICE BY QUANTITY
                    GIVING WS-TOTAL-1
                    ADD WS-TOTAL-1 TO TOTAL-1
               WHEN DEPARTMENT = 2 AND SALE-INDICATOR = " "
                    MULTIPLY RETAIL-PRICE BY QUANTITY
                    GIVING WS-TOTAL-2
                    ADD WS-TOTAL-2 TO TOTAL-2
               WHEN DEPARTMENT = 2 AND SALE-INDICATOR = "S"
                    MULTIPLY SALE-PRICE BY QUANTITY
                    GIVING WS-TOTAL-2
                    ADD WS-TOTAL-2 TO TOTAL-2
               WHEN DEPARTMENT = 3 AND SALE-INDICATOR = " "
                    MULTIPLY RETAIL-PRICE BY QUANTITY
                    GIVING WS-TOTAL-3
                    ADD WS-TOTAL-3 TO TOTAL-3
               WHEN DEPARTMENT = 3 AND SALE-INDICATOR = "S"
                    MULTIPLY SALE-PRICE BY QUANTITY
                    GIVING WS-TOTAL-3
                    ADD WS-TOTAL-3 TO TOTAL-3
               WHEN DEPARTMENT = 4 AND SALE-INDICATOR = " "
                    MULTIPLY RETAIL-PRICE BY QUANTITY
                    GIVING WS-TOTAL-4
                    ADD WS-TOTAL-4 TO TOTAL-4
               WHEN DEPARTMENT = 4 AND SALE-INDICATOR = "S"
                    MULTIPLY SALE-PRICE BY QUANTITY
                    GIVING WS-TOTAL-4
                    ADD WS-TOTAL-4 TO TOTAL-4
               WHEN DEPARTMENT = 5 AND SALE-INDICATOR = " "
                    MULTIPLY RETAIL-PRICE BY QUANTITY
                    GIVING WS-TOTAL-5
                    ADD WS-TOTAL-5 TO TOTAL-5
               WHEN DEPARTMENT = 5 AND SALE-INDICATOR = "S"
                    MULTIPLY SALE-PRICE BY QUANTITY
                    GIVING WS-TOTAL-5
                    ADD WS-TOTAL-5 TO TOTAL-5
           END-EVALUATE
           DISPLAY "EVALUATION COMPLETE".
      
      *------------------------------------
      *READ FROM THE INPUT FILE
       TASK-READ              SECTION.
           READ FILE-IN
           DISPLAY "NEW DATA: " FILE-IN-REC.
      *    IF NOT FILEIN-OK
      *        DISPLAY "ERROR READING FILE"
      *        PERFORM TASK-ERRORS
      *    END-IF.

      *------------------------------------
      *OUTPUT TO GOODOUT FILE
       TASK-WRITE-GOODOUT     SECTION.
           MOVE 1 TO DEPT-1
           MOVE 2 TO DEPT-2
           MOVE 3 TO DEPT-3
           MOVE 4 TO DEPT-4
           MOVE 5 TO DEPT-5
           WRITE GOOD-REC
           IF NOT GOODOUT-OK
              DISPLAY "ERROR WRITING GOOD OUTPUT FILE"
              PERFORM TASK-ERRORS
           END-IF
           DISPLAY "OUTPUT TO FILE - GOOD DATA".
      
      *------------------------------------
      *OUTPUT TO BADOUT FILE
       TASK-WRITE-BADOUT      SECTION.
           IF DEPARTMENT NUMERIC
              MOVE DEPARTMENT TO DEPT
           ELSE
              MOVE DEPARTMENT TO ERROR-DEPT
           END-IF

           MOVE INVOICE-NUM TO INV-NUM
           MOVE "ERROR" TO ERROR-VAL
           WRITE BAD-REC

           IF NOT BADOUT-OK
              DISPLAY "ERROR WRITING BAD OUTPUT FILE"
              PERFORM TASK-ERRORS
           END-IF

           DISPLAY "OUTPUT TO FILE - BAD DATA".

      *------------------------------------
      *HANDLE FILE ERRORS
       TASK-ERRORS            SECTION.
           DISPLAY "SALE STATUS IS "    WS-FILEIN-STATUS
           DISPLAY "GOODOUT STATUS IS " WS-GOODOUT-STATUS
           DISPLAY "ERROR STATUS IS"    WS-BADOUT-STATUS
           DISPLAY "TERMINATING PROGRAM"
           MOVE    13 TO RETURN-CODE
           STOP RUN.
 

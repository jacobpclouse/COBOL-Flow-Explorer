      **************************************************************
      *
      *  PROGRAM ID ORDERSYS
      *  DATE CREATED:  03MAR2026
      *
      * TAKE INPUT DATA FROM A FILE, FORMAT IT FOR 
      * OUTPUT AND CALCULATE NUMBER OF ITEMS SOLD AND 
      * TALLY THE TOTAL KINDS OF EACH TRANSACTION
      *
      *  CHANGE LOG
      *  USER ID     DATE     CHANGE DESCRIPTION
      * ---------   ------    -------------------------------------
      *  DAN BIEG   03MAR2026 CODE PROG
      **************************************************************
       IDENTIFICATION DIVISION.                               
      **************************************************************
       PROGRAM-ID. ORDERSYS.  

      **************************************************************
       ENVIRONMENT DIVISION.
      **************************************************************

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370. 

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT FILE-SHIPIN ASSIGN TO UT-S-SHIPIN.
           SELECT FILE-SHIPOUT ASSIGN TO UT-S-SHIPOUT.

      **************************************************************
       DATA DIVISION.
      **************************************************************

       FILE SECTION.

      * IF THERE IS A PROBLEM, MOST LIKELY ITS HOW I'M
      * READING IN THE FILE

      *READ IN
       FD  FILE-SHIPIN
              LABEL RECORDS ARE OMITTED       
              BLOCK CONTAINS 0 RECORDS            
              DATA RECORD IS FIL-SHIPIN. 

       01  FIL-SHIPIN         PIC X(80). 

      * ---

      *WRITE OUT
       FD  FILE-SHIPOUT
              LABEL RECORDS ARE OMITTED   
              BLOCK CONTAINS 0 RECORDS       
              DATA RECORD IS FIL-SHIPOUT. 

       01  FIL-SHIPOUT       PIC X(80).

      **************************************************************
       WORKING-STORAGE SECTION.
      **************************************************************

      * USE THE COPYBOOKS

      *    USE TO READ IN DATA
       01  SHIP-INPUT-REC COPY SHIPREC.

      *    TOTAL VARS + USE TO WRITE TO OUTPUT
       01  SHIP-OUTPUT-REC COPY OUTREC.

      * SEPERATORS AND MESSAGES
       01  WS-BREAKPT     PIC X(25) VALUE '-=-=-=-=-=-=-=-=-=-=-=-=-'.
       01  WS-MESSAGE     PIC X(25) VALUE 'ORDER SUMMARY REPORT GEN!'.
       01  WS-LINE-SPACE  PIC X(25) VALUE SPACES.

      * FLAGS
       01  WS-VAL.
           02  WS-EOF-SHIPIN     PIC X VALUE 'N'.

      **************************************************************
       PROCEDURE DIVISION.
      **************************************************************

           DISPLAY WS-BREAKPT.
           DISPLAY WS-MESSAGE.
           DISPLAY WS-BREAKPT.

      *    OPEN FILES AND READ FIRST RECORD
           PERFORM R1000-OPEN-DATASETS.

      *    ADD DATA HEADER

      *    START MAIN LOOP 
           PERFORM R2000-PROCESS-RECORD
              UNTIL WS-EOF-SHIPIN = 'Y'.

           PERFORM R3000-CRAFT-SUMMARY.

      *    CLOSE OUT FILES
           PERFORM R4000-CLOSE-DATASETS.

           STOP RUN.

      *  ------
        R1000-OPEN-DATASETS.
      *  ------
           DISPLAY '  R1000 OPEN DATASETS'.
           OPEN INPUT  FILE-SHIPIN.
           OPEN OUTPUT FILE-SHIPOUT.


      *  ------
        R1100-READ-LIC-ENTRY.
      *  ------
           DISPLAY '  R1100 READ ENTRY'.
           READ FILE-SHIPIN INTO SHIP-INPUT-REC 
                 AT END MOVE 'Y' TO WS-EOF-SHIPIN.

      *  ------
        R2000-PROCESS-RECORD.
      *  ------
           DISPLAY '  R2000 PROCESS REC'.
           PERFORM R1100-READ-LIC-ENTRY.

      *    IN CASE OF ABEND, MOST LIKELY DUE TO MISALIGNED DATA
           DISPLAY ' -- START DEBUG COPYBOOK DATA --'.  
           DISPLAY '    SI-ORDER-NO: ' SI-ORDER-NO.     
           DISPLAY '   SI-CUST-NAME: ' SI-CUST-NAME.    
           DISPLAY '   SI-ITEM-CODE: ' SI-ITEM-CODE.    
           DISPLAY '    SI-QUANTITY: ' SI-QUANTITY.     
           DISPLAY '  SI-UNIT-PRICE: ' SI-UNIT-PRICE.   
           DISPLAY '  SI-TRANS-TYPE: ' SI-TRANS-TYPE.   
           DISPLAY ' -- END DEBUG COPYBOOK DATA --'.    
           

      **    LV1 IF 
           IF WS-EOF-SHIPIN = 'N'
      *    USING SUBPROGRAM TO CALC OUT THE VALUES
              CALL 'SUBCALC' USING SI-ORDER-NO, SI-CUST-NAME,
                 SI-ITEM-CODE, SI-QUANTITY, SI-UNIT-PRICE,
                 SI-TRANS-TYPE, WS-EXTENDED-AMT, WS-TOTAL-SALES
                 WS-TOTAL-ITEMS, WS-TOTAL-SALES-AMT, 
                 WS-TOTAL-RETURNS, WS-TOTAL-CANCELS.  

      
      *       COMPUTE WS-EXTENDED-AMT =
      *             SI-QUANTITY * SI-UNIT-PRICE.


      **    LV2 IF - IF S
      *       IF SI-TRANS-TYPE = 'S'
      *          ADD 1 TO WS-TOTAL-SALES
      *          ADD SI-QUANTITY TO WS-TOTAL-ITEMS
      *          ADD WS-EXTENDED-AMT TO WS-TOTAL-SALES-AMT
      **CHECK IF ABOVE VALID IN COBOL 68
      *       .


      **    LV2 IF - IF R
      *       IF SI-TRANS-TYPE = 'R'
      *          ADD 1 TO WS-TOTAL-RETURNS
      *       .


      **    LV2 IF - IF C
      *       IF SI-TRANS-TYPE = 'C'
      *          ADD 1 TO WS-TOTAL-CANCELS
      *       .

              DISPLAY 'PROCESSED ORDER: ' SI-ORDER-NO.
              DISPLAY WS-BREAKPT.
              DISPLAY WS-LINE-SPACE.

      *    LV1 IF END
           .


      *  ------
        R3000-CRAFT-SUMMARY.
      *  ------
           DISPLAY '  R3000 CRAFT SUMMARY'. 
           MOVE SPACES TO FIL-SHIPOUT.

      *    USE COPYBOOK TO WRITE TO OUTPUT LINE BY LINE
           MOVE OUT-PT0 TO FIL-SHIPOUT.
           WRITE FIL-SHIPOUT.
           MOVE SPACES TO FIL-SHIPOUT.

           MOVE OUT-PT1 TO FIL-SHIPOUT.
           WRITE FIL-SHIPOUT.
           MOVE SPACES TO FIL-SHIPOUT.

           MOVE OUT-PT2 TO FIL-SHIPOUT.
           WRITE FIL-SHIPOUT.
           MOVE SPACES TO FIL-SHIPOUT.

           MOVE OUT-PT3 TO FIL-SHIPOUT.
           WRITE FIL-SHIPOUT.
           MOVE SPACES TO FIL-SHIPOUT.


      * ------------------
        R4000-CLOSE-DATASETS.
      * ------------------
           DISPLAY '  R4000 CLOSE DATA'.
           CLOSE FILE-SHIPIN.
           CLOSE FILE-SHIPOUT.


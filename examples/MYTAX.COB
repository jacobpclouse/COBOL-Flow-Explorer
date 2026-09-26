      **************************************************************
      *
      *  PROGRAM ID MYTAX
      *  DATE CREATED:  21MAR2026
      *
      *  TAX RETURN / TAX FILING PROG 
      *  USING JAY JAY MOSELEY PROGRAM + CODE SNIPPETS
      *  (DONT NEED TO DIRECTLY IMPORT VSAM IN FILE CONTROL, JAYS
      *  ASSEMBLER VSAMIO WILL PULL IT FOR YOU)
      *
      *  1) WE ARE ASSUMING ALL RECORDS IN SEQUENCTIAL RETURNS FILE
      *    ARE IN THE VSAM DATABASE (FOR NOW)
      *
      *  CHANGE LOG
      *  USER ID     DATE     CHANGE DESCRIPTION
      * ---------   ------    -------------------------------------
      *  DAN BIEG   21MAR2026 CODE PROG
      **************************************************************
       IDENTIFICATION DIVISION.   
      **************************************************************

       PROGRAM-ID. MYTAX.  

      **************************************************************
       ENVIRONMENT DIVISION.
      **************************************************************
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370. 

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.            
           SELECT INPUT-RETURNS-SQ  ASSIGN TO UT-S-RETURNS.
           SELECT OUTPUT-REPORT-SQ  ASSIGN TO UT-S-REPORT.
           SELECT OUTPUT-DISPERSE-SQ  ASSIGN TO UT-S-DISPERSE.
           SELECT OUTPUT-AUDIT-SQ  ASSIGN TO UT-S-AUDIT.


      **************************************************************
       DATA DIVISION.       
      **************************************************************
       FILE SECTION. 

      * INPUT SEQ FILE:

       FD  INPUT-RETURNS-SQ        
           LABEL RECORDS ARE OMITTED            
           BLOCK CONTAINS 0 RECORDS      
           DATA RECORD IS FIL-INP-RETURNS. 
       
       01  FIL-INP-RETURNS                   PIC X(80).

      * -- -- --

      * OUTPUT SEQ FILES:

       FD  OUTPUT-REPORT-SQ        
           LABEL RECORDS ARE OMITTED            
           BLOCK CONTAINS 0 RECORDS      
           DATA RECORD IS FIL-OUT-REPORT. 
       
       01  FIL-OUT-REPORT                   PIC X(132).
      * DOUBLE CHECK THAT THE FILE WE CREATE IS BIG ENOUGHT TO HOLD THIS

      * -- 

       FD  OUTPUT-DISPERSE-SQ        
           LABEL RECORDS ARE OMITTED            
           BLOCK CONTAINS 0 RECORDS      
           DATA RECORD IS FIL-OUT-DISPERSE. 
       
       01  FIL-OUT-DISPERSE                   PIC X(80).

      * -- 

       FD  OUTPUT-AUDIT-SQ        
           LABEL RECORDS ARE OMITTED            
           BLOCK CONTAINS 0 RECORDS      
           DATA RECORD IS FIL-OUT-AUDIT. 
       
       01  FIL-OUT-AUDIT                   PIC X(80).

      **************************************************************
       WORKING-STORAGE SECTION. 
      **************************************************************

      * SEPERATORS AND MESSAGES
       01  WS-BREAKPT     PIC X(25) VALUE '-=-=-=-=-=-=-=-=-=-=-=-=-'.
       01  WS-MESSAGE     PIC X(25) VALUE 'BATCH TAX RETURN PROCESNG'.
       01  WS-LINE-SPACE  PIC X(25) VALUE SPACES.
       01  WS-FULL-SEPERATOR    PIC X(50) VALUE ALL '*'.

      * CURRENT DATE - FOR DEMO -- ASSUME 1978
       01  WS-CURRENT-DATE.
            02 WS-CURRENT-MM    PIC 9(2) VALUE 04.
            02 FILLER           PIC X(1) VALUE '-'.
            02 WS-CURRENT-DD    PIC 9(2) VALUE 17.
            02 FILLER           PIC X(1) VALUE '-'.
            02 WS-CURRENT-YY    PIC 9(4) VALUE 1978.


      * FLAGS
       01  WS-VAL.
           02 WS-EOF-RETURNS            PIC X VALUE 'N'.
           02  WS-AUDIT-FLAG            PIC X VALUE 'N'.
              88  NEEDS-AUDIT                 VALUE 'Y'.
           02  WS-MASTER-FOUND          PIC X VALUE 'N'.
              88  MASTER-FOUND                VALUE 'Y'.

      * COUNTERS
       01  WS-COUNTERS.
           05  WS-RETURNS-READ     PIC 9(06)  VALUE ZEROES.
           05  WS-RETURNS-PROC     PIC 9(06)  VALUE ZEROES.
           05  WS-RETURNS-ERR      PIC 9(06)  VALUE ZEROES.
           05  WS-REFUNDS-ISSUED   PIC 9(06)  VALUE ZEROES.
           05  WS-TAXES-OWED       PIC 9(06)  VALUE ZEROES.
           05  WS-AUDITS-FLAGGED   PIC 9(06)  VALUE ZEROES.
           05  WS-TOTAL-REFUNDS    PIC S9(09)V99 VALUE ZEROES.
           05  WS-TOTAL-COLLECTED  PIC S9(09)V99 VALUE ZEROES.

      * TAX CALC WORK FIELDS
       01  WS-TAX-CALC.
           05  WS-AGI              PIC S9(08)V99 VALUE ZEROES.
           05  WS-STD-DEDUCTION    PIC S9(07)V99 VALUE ZEROES.
           05  WS-TAXABLE-INCOME   PIC S9(08)V99 VALUE ZEROES.
           05  WS-GROSS-TAX        PIC S9(07)V99 VALUE ZEROES.
           05  WS-TOTAL-CREDITS    PIC S9(06)V99 VALUE ZEROES.
           05  WS-NET-TAX          PIC S9(07)V99 VALUE ZEROES.
           05  WS-BALANCE-DUE      PIC S9(07)V99 VALUE ZEROES.
           05  WS-EXEMPTION-AMT    PIC S9(06)V99 VALUE ZEROES.
           05  WS-DED-RATIO        PIC S9(03)V99 VALUE ZEROES.
           05  WS-WORK-AMT         PIC S9(09)V99 VALUE ZEROES.

      * STANDARD DEDUCTIONS
       01  WS-STD-DED-TABLE.
           05  WS-STD-DED-SINGLE   PIC 9(05)V99  VALUE 02300.00.
           05  WS-STD-DED-MFJ      PIC 9(05)V99  VALUE 03400.00.
           05  WS-STD-DED-MFS      PIC 9(05)V99  VALUE 01700.00.
           05  WS-STD-DED-HH       PIC 9(05)V99  VALUE 02300.00.
           05  WS-EXEMPTION-VALUE  PIC 9(04)V99  VALUE 1000.00.

      * AUDIT THRESHOLDS
       01  WS-AUDIT-LIMITS.
           05  WS-HIGH-INCOME-LMT  PIC 9(08)V99  VALUE 00100000.00.
           05  WS-HIGH-DED-PCT     PIC V99        VALUE .40.
           05  WS-MAX-AUDIT-CNT    PIC 9(02)      VALUE 03.


      * TAX COPYBOOKS
      *  - INPUT COPYBOOKS
       01  TAX-RETURN-REC                 COPY RETURREC.
       01  TAXPAYER-MASTER-REC            COPY MASTRREC.
      *  - OUTPUT COPYBOKKS
       01  REPORT-REC                    COPY REPORTO.
       01  DISBURSEMENT-REC              COPY DISPRSEO.
       01  AUDIT-FLAG-REC                COPY AUDITO.

      * VSAM IO SETUP COPYBOOKS
       01  VSIO-PARAMETER-VALUES         COPY VSAMIO.
       01  MASTERDB                      COPY VSAMIOFB.
       01  KSDS-RECORD.
           02  KR-KEY                    PIC X(09).
           02  FILLER                    PIC X(71).
      
      **************************************************************
       PROCEDURE DIVISION.                                    
      **************************************************************
           DISPLAY WS-FULL-SEPERATOR.
           DISPLAY WS-MESSAGE.
           PERFORM R9999-PRINT-LOGO.
           DISPLAY WS-FULL-SEPERATOR.

      *    SET DATE - YOU WOULD GET THIS FROM SYSTEM NORMALLY
      *    BUT EASIER HERE TO JUST PRETEND
           MOVE WS-CURRENT-DATE TO WS-RPT-DATE.

      *    OPEN FILES
           PERFORM R1000-OPEN-DATASETS.
           PERFORM R2000-READ-REC-ENTRY.

      *    START MAIN LOOP
           PERFORM R3000-PROCESS-FILE
              UNTIL WS-EOF-RETURNS = 'Y'.

      *    PRINT OUT RESULTS
           PERFORM R4000-FINISH-UP-REPORTS.

      *    CLOSE OUT FILES
           PERFORM R5000-CLOSE-DATASETS.

           STOP RUN.
      
      *  ------
        R1000-OPEN-DATASETS.
      *  ------
           DISPLAY ' -- OPENING DATASETS -- '. 
           OPEN INPUT INPUT-RETURNS-SQ.   
           OPEN OUTPUT OUTPUT-REPORT-SQ.     
           OPEN OUTPUT OUTPUT-DISPERSE-SQ.
           OPEN OUTPUT OUTPUT-AUDIT-SQ.
           DISPLAY ' - OPENED SEQ - '.         

      *    BELOW THIS IS USED TO ACCESS VSAM ON MVS 3.8
           MOVE 'MASTERDB' TO VSIO-DDNAME.
           MOVE VSIO-KSDS TO VSIO-ORGANIZATION.
           MOVE VSIO-DIRECT TO VSIO-ACCESS.
           MOVE VSIO-INPUT-OUTPUT TO VSIO-MODE.
           MOVE +80 TO VSIO-RECORD-LENGTH.
           MOVE +0 TO VSIO-KEY-POSITION.
           MOVE +9 TO VSIO-KEY-LENGTH.
      *    MOVE +10 TO VSIO-KEY-LENGTH.
           MOVE VSIO-OPEN TO VSIO-COMMAND.
           CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, MASTERDB,
                               KSDS-RECORD.
           IF NOT VSIO-SUCCESS
               DISPLAY 'VSAMIO ERROR OCCURRED DURING '
                       VSIO-COMMAND
               EXHIBIT NAMED VSIO-RETURN-CODE,
               EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
                             VSIO-VSAM-FUNCTION-CODE,
                             VSIO-VSAM-FEEDBACK-CODE
               STOP RUN
           ELSE    
               DISPLAY ' - OPENED VSAM - '.        

      *    WRITE REPORT OUTFILE HEADERS
      *       REPORT LOG HEADERS
           MOVE SPACES TO FIL-OUT-REPORT.
           MOVE WS-RPT-HEADER-1 TO FIL-OUT-REPORT.
           WRITE FIL-OUT-REPORT.

           MOVE SPACES TO FIL-OUT-REPORT.
           MOVE WS-RPT-HEADER-2 TO FIL-OUT-REPORT.
           WRITE FIL-OUT-REPORT.

           MOVE SPACES TO FIL-OUT-REPORT.
           MOVE WS-RPT-HEADER-3 TO FIL-OUT-REPORT.
           WRITE FIL-OUT-REPORT.

      *       AUDIT LOG HEADERS
           MOVE SPACES TO FIL-OUT-AUDIT.
           MOVE WS-FULL-SEPERATOR TO FIL-OUT-AUDIT.
           WRITE FIL-OUT-AUDIT.

           MOVE SPACES TO FIL-OUT-AUDIT.
           MOVE ' -- -- TAX AUDIT LOG -- -- ' TO FIL-OUT-AUDIT.
           WRITE FIL-OUT-AUDIT.

           MOVE SPACES TO FIL-OUT-AUDIT.
           MOVE WS-FULL-SEPERATOR TO FIL-OUT-AUDIT.
           WRITE FIL-OUT-AUDIT.

      *       DISPERSE LOG HEADERS
           MOVE SPACES TO FIL-OUT-DISPERSE.
           MOVE WS-FULL-SEPERATOR 
              TO FIL-OUT-DISPERSE.
           WRITE FIL-OUT-DISPERSE.

           MOVE SPACES TO FIL-OUT-DISPERSE.
           MOVE ' -- -- TAX DISPERSEMENT LOG -- -- ' 
              TO FIL-OUT-DISPERSE.
           WRITE FIL-OUT-DISPERSE.

           MOVE SPACES TO FIL-OUT-DISPERSE.
           MOVE WS-FULL-SEPERATOR 
              TO FIL-OUT-DISPERSE.
           WRITE FIL-OUT-DISPERSE.
               


      *  ------
        R2000-READ-REC-ENTRY.
      *  ------
           READ INPUT-RETURNS-SQ INTO TAX-RETURN-REC 
                 AT END MOVE 'Y' TO WS-EOF-RETURNS.
            
           IF WS-EOF-RETURNS = 'N'
               ADD 1 TO WS-RETURNS-READ.


      *  ------
        R3000-PROCESS-FILE.
      *  ------
           MOVE 'N' TO WS-AUDIT-FLAG.
           MOVE 'N' TO WS-MASTER-FOUND.
           MOVE SPACES TO AF-AUDIT-REASON.

           MOVE ZEROES TO WS-AGI.
           MOVE ZEROES TO WS-TAXABLE-INCOME
           MOVE ZEROES TO WS-GROSS-TAX.
           MOVE ZEROES TO WS-NET-TAX.
           MOVE ZEROES TO WS-BALANCE-DUE.
           MOVE ZEROES TO WS-TOTAL-CREDITS.

      *    ONLY EXECUTE IF NOT AT END.
           IF WS-EOF-RETURNS = 'N'

              DISPLAY WS-LINE-SPACE.
              DISPLAY WS-BREAKPT.

      * ATTEMPT TO LOOKUP RECORD FROM SEQ IN DB
              MOVE TAX-RETURN-REC TO KSDS-RECORD.
              DISPLAY 'ATTEMPTING TO READ: ' KR-KEY.
              MOVE VSIO-READ TO VSIO-COMMAND.
              CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, MASTERDB,
                                  KSDS-RECORD.
              IF NOT VSIO-SUCCESS
                  IF VSIO-LOGIC-ERROR AND VSIO-RECORD-NOT-FOUND
                     NEXT SENTENCE
                  ELSE
                      DISPLAY 'VSAMIO ERROR OCCURRED DURING '
                              VSIO-COMMAND
                      EXHIBIT NAMED VSIO-RETURN-CODE,
                      EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
                                    VSIO-VSAM-FUNCTION-CODE,
                                    VSIO-VSAM-FEEDBACK-CODE
                      STOP RUN.
      *
      *     DISPLAY IF FOUND + GO ON WITH CALC
              IF VSIO-LOGIC-ERROR AND VSIO-RECORD-NOT-FOUND
                 DISPLAY '          *** RECORD NOT FOUND'
                 DISPLAY WS-LINE-SPACE
                 ADD 1 TO WS-RETURNS-ERR 
                 STOP RUN
              ELSE
      *          MOVE VSAM DATA INTO COPYBOOK SO WE CAN USE IT
                 MOVE KSDS-RECORD TO TAXPAYER-MASTER-REC

                 DISPLAY '   > RECORD FOUND: ' KSDS-RECORD
                 DISPLAY '   >    FIRSTNAME: ' TM-FIRST-NAME
                 DISPLAY '   >     LASTNAME: ' TM-LAST-NAME
                 DISPLAY '   >          SSN: ' TM-SSN           
                 DISPLAY '   > FILING STATS: ' TM-FILING-STATUS 
                 DISPLAY '   > PRIOR BALANC: ' TM-PRIOR-BALANCE 
                 DISPLAY '   > YTD WITHHELD: ' TM-YTD-WITHHELD  
                 DISPLAY '   >  AUDIT COUNT: ' TM-AUDIT-COUNT   
                 DISPLAY '   > LAST FILE YR: ' TM-LAST-FILING-YR
                 DISPLAY '   >  ADDR LINE 1: ' TM-ADDR-LINE1    
                 DISPLAY '   >         CITY: ' TM-CITY          
                 DISPLAY '   >        STATE: ' TM-STATE         

      *          NOW SETUP AND PROCESS CALCULATIONS
                 PERFORM R3100-TAX-CALCULATIONS
                 PERFORM R3200-AUDIT-CHECK
                 PERFORM R3300-WRITE-RETURN-OUTPUT
                 PERFORM R3400-UPDATE-MASTER-VSAM
                 
                 ADD 1 TO WS-RETURNS-PROC
                 DISPLAY WS-LINE-SPACE   

      *    READ AT END SO DONT GET OFF BY ONE LOOP
                 PERFORM R2000-READ-REC-ENTRY.           
           
      * 

      *  ------
        R3100-TAX-CALCULATIONS.
      *  ------
      * CALC AVERAGE INCOME
           DISPLAY '   > CALC AVERAGE INCOME'.
           COMPUTE WS-AGI = TR-WAGES + TR-INTEREST-INCOME +
                 TR-DIVIDENDS.


      * CALC DEDUCTIONS
           DISPLAY '   > CALC DEDUCTIONS'.
           MOVE ZEROES TO WS-STD-DEDUCTION
      *    MAYBE FOR DEDUCTIONS, DO IF TR DEDUCT GREATEER FIRST, THEN
      *    DO THE REST AFTER, SAVE PROCESS CYCLES THAT WAY

      *    FIND GREATER DEDUCTION AND USE THAT
           IF TR-DEDUCTIONS > WS-STD-DEDUCTION
              MOVE TR-DEDUCTIONS TO WS-STD-DEDUCTION
              DISPLAY '      TR-DEDUCTIONS GREATER'
           ELSE
      *       SET SINGLE AS DEFAULT
              MOVE WS-STD-DED-SINGLE TO WS-STD-DEDUCTION

              IF TM-MARRIED-JOINT
                 MOVE ZEROES TO WS-STD-DEDUCTION
                 MOVE WS-STD-DED-MFJ TO WS-STD-DEDUCTION

              IF TM-MARRIED-SEP
                 MOVE ZEROES TO WS-STD-DEDUCTION
                 MOVE WS-STD-DED-MFS TO WS-STD-DEDUCTION

              IF TM-HEAD-HOUSEHOLD
                 MOVE ZEROES TO WS-STD-DEDUCTION
                 MOVE WS-STD-DED-HH TO WS-STD-DEDUCTION
              
              DISPLAY '      WS-STD-DEDUCTION GREATER'.
      *    I ANTICIPATE THESE NESTED IF AND ELSE STATEMENTS BE
      *    TROUBLESOME TO SAY THE LEAST, ADD PRINT OUTS


      * CALC TAXABLE INCOME
           DISPLAY '   > CALC TAXABLE INCOME'.
      *    EXECEPTION CALC
           COMPUTE WS-EXEMPTION-AMT =
              TR-EXEMPTIONS * WS-EXEMPTION-VALUE.

           COMPUTE WS-TAXABLE-INCOME =
               WS-AGI - WS-STD-DEDUCTION - WS-EXEMPTION-AMT.

      * CALC GROSS TAX -- TAX BRACKETS BELOW
           DISPLAY '   > CALC GROSS TAX'.
           IF WS-TAXABLE-INCOME < 1
               MOVE ZEROS TO WS-TAXABLE-INCOME.
               MOVE ZEROS TO WS-GROSS-TAX.
      *        IF THEIR INCOME IS LESS THAN 1, THEY HAVE BIGGER
      *        PROBLEMS THAN THE IRS
           
           IF WS-TAXABLE-INCOME < 2100.00 AND WS-TAXABLE-INCOME > 0
                 COMPUTE WS-GROSS-TAX = WS-TAXABLE-INCOME * .14.

           IF WS-TAXABLE-INCOME < 4200.00 AND 
              WS-TAXABLE-INCOME > 2099.99
                 COMPUTE WS-GROSS-TAX = 294.00 +
                    (WS-TAXABLE-INCOME - 2100.00) * .15.

           IF WS-TAXABLE-INCOME < 6500.00 AND 
              WS-TAXABLE-INCOME > 4199.99
                 COMPUTE WS-GROSS-TAX =
                  609.00 + (WS-TAXABLE-INCOME - 4200.00) * .16.

           IF WS-TAXABLE-INCOME < 8200.00 AND
              WS-TAXABLE-INCOME > 6499.99
                 COMPUTE WS-GROSS-TAX =
                  977.00 + (WS-TAXABLE-INCOME - 6500.00) * .17.

           IF WS-TAXABLE-INCOME < 10800.00 AND
              WS-TAXABLE-INCOME > 8199.99
                 COMPUTE WS-GROSS-TAX =
                  1266.00 + (WS-TAXABLE-INCOME - 8200.00) * .19.

           IF WS-TAXABLE-INCOME > 10799.99
                 COMPUTE WS-GROSS-TAX = 1760.00 +
                  (WS-TAXABLE-INCOME - 10800.00) * .22.
           

      * CALC CREDITS TO APPLY
           DISPLAY '   > CALC CREDITS TO APPLY'.
      *    LIKE CHILD TAX CREDITS
           MOVE TR-CHILD-TAX-CRED TO WS-TOTAL-CREDITS.

           COMPUTE WS-NET-TAX = WS-GROSS-TAX - WS-TOTAL-CREDITS.

           IF WS-NET-TAX < 0
               MOVE ZERO TO WS-NET-TAX.


      * CALC BALANCE
           DISPLAY '   > CALC BALANCE'.
      *    POSITIVE = TAX OWED TO IRS
      *    NEGATIVE = REFUND DUE TO TAXPAYER
           COMPUTE WS-BALANCE-DUE = WS-NET-TAX - TR-WITHHELD.

           IF WS-BALANCE-DUE < 0
               ADD 1 TO WS-REFUNDS-ISSUED
               COMPUTE WS-WORK-AMT = WS-BALANCE-DUE * -1
               ADD WS-WORK-AMT TO WS-TOTAL-REFUNDS
           ELSE
               ADD 1 TO WS-TAXES-OWED
               ADD WS-BALANCE-DUE TO WS-TOTAL-COLLECTED.

           DISPLAY '   > COMPLETED R3100-TAX-CALCULATIONS!'.


      *  ------
        R3200-AUDIT-CHECK.
      *  ------
           DISPLAY '   ~ R3200-AUDIT-CHECK'.
      * CHECK TO SEE IF WE NEED TO HAVE AN AUDIT ON THE PERSON
           MOVE SPACES TO AF-AUDIT-REASON.
           
      *    OVER $100,000 - YES
           IF WS-AGI > WS-HIGH-INCOME-LMT
               MOVE 'Y' TO WS-AUDIT-FLAG
               MOVE 'HIGH INCOME OVER $100K'
                   TO AF-AUDIT-REASON.

      *    EXCESSIVE DEDUCTIONS -- OVER 40% OF AGI - YES
           IF WS-AGI > 0
               COMPUTE WS-DED-RATIO = TR-DEDUCTIONS / WS-AGI
               IF WS-DED-RATIO > WS-HIGH-DED-PCT AND 
                 WS-AUDIT-FLAG = 'N'
                   MOVE 'Y' TO WS-AUDIT-FLAG
                   MOVE 'EXCESSIVE DEDUCTION RATIO'
                       TO AF-AUDIT-REASON.

      *    3 OR MORE PRIOR AUDITS - YES
           IF TM-AUDIT-COUNT > WS-MAX-AUDIT-CNT AND 
              WS-AUDIT-FLAG = 'N'
               MOVE 'Y' TO WS-AUDIT-FLAG
               MOVE 'PRIOR AUDIT HISTORY - 3 OR MORE'
                   TO AF-AUDIT-REASON.

      *    PRIOR YEAR BALANCE - YES
           IF TM-PRIOR-BALANCE > 0 AND 
              WS-AUDIT-FLAG = 'N'
               MOVE 'Y' TO WS-AUDIT-FLAG
               MOVE 'OUTSTANDING PRIOR YEAR BALANCE'
                   TO AF-AUDIT-REASON.

           IF NEEDS-AUDIT
               ADD 1 TO WS-AUDITS-FLAGGED.


      *  ------
        R3300-WRITE-RETURN-OUTPUT.
      *  ------
           DISPLAY '   ^ R3300-WRITE-RETURN-OUTPUT'.
      *    WRITE REFUND DISBURSEMENT RECORD IF DUE
           IF WS-BALANCE-DUE < 0
               MOVE TR-SSN TO DB-SSN
               MOVE TM-LAST-NAME TO DB-LAST-NAME
               MOVE TM-FIRST-NAME TO DB-FIRST-NAME
               COMPUTE DB-REFUND-AMOUNT = WS-BALANCE-DUE * -1
               MOVE TR-TAX-YEAR TO DB-TAX-YEAR
               MOVE TM-ADDR-LINE1 TO DB-ADDR-LINE1
               MOVE TM-CITY TO DB-CITY

               MOVE SPACES TO FIL-OUT-DISPERSE.
               MOVE DISBURSEMENT-REC TO FIL-OUT-DISPERSE.
               WRITE FIL-OUT-DISPERSE.

      *    WRITE AUDIT FLAG RECORD IF FLAGGED
           IF NEEDS-AUDIT
               MOVE TR-SSN TO AF-SSN
               MOVE TM-LAST-NAME TO AF-LAST-NAME
               MOVE TR-TAX-YEAR TO AF-TAX-YEAR
               MOVE WS-AGI TO AF-GROSS-INCOME
               MOVE WS-NET-TAX TO AF-CALC-TAX

               MOVE SPACES TO FIL-OUT-AUDIT.
               MOVE AUDIT-FLAG-REC TO FIL-OUT-AUDIT.
               WRITE FIL-OUT-AUDIT.

      *    WRITE DETAIL LINE TO PROCESSING REPORT
           MOVE SPACES TO WS-RPT-DETAIL.
           MOVE TR-SSN TO WS-RPT-SSN.
           MOVE TM-LAST-NAME TO WS-RPT-NAME.
           MOVE TR-TAX-YEAR TO WS-RPT-YEAR.
           MOVE WS-AGI TO WS-RPT-INCOME.
           MOVE WS-NET-TAX TO WS-RPT-TAX.
           MOVE TR-WITHHELD TO WS-RPT-WITHHELD.
           MOVE WS-BALANCE-DUE TO WS-RPT-BALANCE.

      *    DONT THINK THE COMPILER IS GOING TO THESE NESTED STATEMENTS
           IF WS-BALANCE-DUE < 0
               IF NEEDS-AUDIT
                   MOVE 'REFUND/AUD' TO WS-RPT-DISP-STAT
               ELSE
                   MOVE 'REFUND    ' TO WS-RPT-DISP-STAT.

           IF WS-BALANCE-DUE > -1
               IF NEEDS-AUDIT
                   MOVE 'OWED/AUDIT' TO WS-RPT-DISP-STAT
               ELSE
                   MOVE 'TAX OWED  ' TO WS-RPT-DISP-STAT.

           MOVE SPACES TO FIL-OUT-REPORT.
           MOVE WS-RPT-DETAIL TO FIL-OUT-REPORT.
           WRITE FIL-OUT-REPORT.


      *  ------
        R3400-UPDATE-MASTER-VSAM.
      *  ------
           DISPLAY '   < R3400-UPDATE-MASTER-VSAM'.

           MOVE TR-TAX-YEAR  TO TM-LAST-FILING-YR.
           MOVE TR-WITHHELD  TO TM-YTD-WITHHELD.

           IF NEEDS-AUDIT
               ADD 1 TO TM-AUDIT-COUNT.

      *    UPDATE VSAM WITH VALUES FROM SEQ FILE USING VSAMIO COMMANDS
      *    MOVE RI-IMAGE TO KSDS-RECORD.
           MOVE VSIO-REWRITE TO VSIO-COMMAND.
           CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, MASTERDB,
                               KSDS-RECORD.
           IF VSIO-SUCCESS
               DISPLAY KSDS-RECORD
               DISPLAY '               RECORD AFTER CHANGE'
           ELSE
               DISPLAY 'VSAMIO ERROR OCCURRED DURING '
                       VSIO-COMMAND
               EXHIBIT NAMED VSIO-RETURN-CODE,
               EXHIBIT NAMED VSIO-VSAM-RETURN-CODE,
                             VSIO-VSAM-FUNCTION-CODE,
                             VSIO-VSAM-FEEDBACK-CODE
               STOP RUN.

      *  ------
        R4000-FINISH-UP-REPORTS.
      *  ------
           DISPLAY 'FINALLY: R4000-FINISH-UP-REPORTS.'
           DISPLAY WS-LINE-SPACE.

           MOVE SPACES TO FIL-OUT-REPORT.
           MOVE WS-RPT-HEADER-3 TO FIL-OUT-REPORT.
           WRITE FIL-OUT-REPORT.

           MOVE SPACES TO FIL-OUT-REPORT.
           MOVE WS-TOTAL-REFUNDS    TO WS-TOT-REFUNDS.
           MOVE WS-TOTAL-COLLECTED  TO WS-TOT-COLLECT.
           MOVE WS-RPT-TOTALS TO FIL-OUT-REPORT.
           WRITE FIL-OUT-REPORT.

           MOVE SPACES TO FIL-OUT-REPORT.
           MOVE WS-RETURNS-PROC     TO WS-CNT-PROC.
           MOVE WS-REFUNDS-ISSUED   TO WS-CNT-REFUND.
           MOVE WS-AUDITS-FLAGGED   TO WS-CNT-AUDIT.
           MOVE WS-RETURNS-ERR      TO WS-CNT-ERR.
           MOVE WS-RPT-COUNTS TO FIL-OUT-REPORT.
           WRITE FIL-OUT-REPORT.

           DISPLAY WS-FULL-SEPERATOR.
           DISPLAY 'TAXPROC: PROCESSING COMPLETE.'.
           DISPLAY 'TAXPROC: RETURNS READ    = ' WS-RETURNS-READ.
           DISPLAY 'TAXPROC: RETURNS PROC    = ' WS-RETURNS-PROC.
           DISPLAY 'TAXPROC: REFUNDS ISSUED  = ' WS-REFUNDS-ISSUED.
           DISPLAY 'TAXPROC: AUDITS FLAGGED  = ' WS-AUDITS-FLAGGED.
           DISPLAY 'TAXPROC: ERRORS/REJECTS  = ' WS-RETURNS-ERR.
           DISPLAY WS-FULL-SEPERATOR.


      * ------------------
        R5000-CLOSE-DATASETS.
      * ------------------
           CLOSE INPUT-RETURNS-SQ.
           CLOSE OUTPUT-REPORT-SQ.
           CLOSE OUTPUT-DISPERSE-SQ.
           CLOSE OUTPUT-AUDIT-SQ.
           
           MOVE VSIO-CLOSE TO VSIO-COMMAND.
           CALL 'VSAMIO' USING VSIO-PARAMETER-BLOCK, MASTERDB,
                               KSDS-RECORD.
                               
           DISPLAY ' -- CLOSED DATASETS -- '.

      * ------------------
       R9999-PRINT-LOGO.
      * ------------------
        DISPLAY 
        ' __  __   __                                                 '. 
        DISPLAY 
        '|  |/  `.*  `..-.          .-                                '. 
        DISPLAY 
        '|   .-.  .-   *\ \        / /  .|                            '. 
        DISPLAY 
        '|  |  |  | |  | \ \      / / .* |_     __     ____     ____  '. 
        DISPLAY 
        '|  |  |  | |  |  \ \    / /.*     | .:--.*.  `.   \  .*   /  '. 
        DISPLAY 
        '|  |  |  | |  |   \ \  / /*--.  .-*/ |   \ |   `.  `*   .*   '. 
        DISPLAY 
        '|  |  |  | |  |    \ `  /    |  |  `" __ | |     *.    .     '. 
        DISPLAY 
        '|__|  |__| |__|     \  /     |  |   .*.**| |     .*     .    '. 
        DISPLAY 
        '                    / /      |  *.*/ /   | |_  .*  .*`.  `.  '. 
        DISPLAY 
        '                |`-* /       |   / \ \._,\ */.*   /    `   `.'. 
        DISPLAY 
        '                 *..*        `*-*   `--*  `"*----*      *----'. 
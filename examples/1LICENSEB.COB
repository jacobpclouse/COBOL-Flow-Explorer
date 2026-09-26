      **************************************************************
      *
      *  PROGRAM ID LICENSEB
      *  DATE CREATED:  22FEB2026
      *
      *  DEMO PROG SHOWS YOU HOW TO 
      *  INPUT AND OUTPUT FILES FROM COBOL
      *  AND USE THE DATA IN BETWEEN
      *
      *  CHANGE LOG
      *  USER ID     DATE     CHANGE DESCRIPTION
      * ---------   ------    -------------------------------------
      *  DAN BIEG   22FEB2026 CODE PROG
      **************************************************************
       IDENTIFICATION DIVISION.   
      **************************************************************

       PROGRAM-ID. LICENSEB.  

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370. 

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.                                                    
           SELECT FILE-LIC-TRNS ASSIGN TO UT-S-LICTRNS.  
           SELECT FILE-LIC-OUT ASSIGN TO UT-S-LICOUT.  
           SELECT FILE-ERR-LOG ASSIGN TO UT-S-ERRLOG.  


      **************************************************************
       DATA DIVISION.       
      **************************************************************

       FILE SECTION.                                                    
       FD     FILE-LIC-TRNS                                            
              LABEL RECORDS ARE OMITTED                                 
              BLOCK CONTAINS 0 RECORDS                                  
              DATA RECORD IS FIL-LICENSE-TRNS. 

       01  FIL-LICENSE-TRNS     PIC X(80).  
      * --

       FD     FILE-LIC-OUT        
              LABEL RECORDS ARE OMITTED            
              BLOCK CONTAINS 0 RECORDS               
              DATA RECORD IS FIL-LICENSE-OUT. 
       
       01  FIL-LICENSE-OUT      PIC X(80).
      *--

       FD     FILE-ERR-LOG
              LABEL RECORDS ARE OMITTED
              BLOCK CONTAINS 0 RECORDS
              DATA RECORD IS FIL-ERR-LOG.

       01  FIL-ERR-LOG            PIC X(80).

      **************************************************************
       WORKING-STORAGE SECTION. 
      **************************************************************


      * USE THE COPYBOOK
       01  DMV-LICENSE-REC COPY LICCOPY.
       01  DMV-OUT-REC COPY RPRTCOPY.
       01  DMV-ERR-REC COPY ERRCOPY.


      * CURRENT DATE - FOR DEMO -- ADD 5 TO THIS FOR SENIORS,
      *      8 FOR EVERYONE ELSE
       01  WS-CURRENT-DATE.
            02 WS-CURRENT-YY    PIC 9(2) VALUE 26.
            02 WS-CURRENT-MM    PIC 9(2) VALUE 02.
            02 WS-CURRENT-DD    PIC 9(2) VALUE 18.

      * COMPARE IN 260218 AGAINST EXPIRE DATE TO SEE IF LICENSE
      * IS OVERDUE, THEIR DATE SHOULD BE LARGER IF NOT OVERDUE

       01 WS-RENEWAL-DATES.
            02 WS-RENEW-MM      PIC 9(2) VALUE ZEROS.
            02 FILLER            PIC X VALUE '-'.
            02 WS-RENEW-DD      PIC 9(2) VALUE ZEROS.
            02 FILLER            PIC X VALUE '-'.
            02 WS-RENEW-YY      PIC 9(2) VALUE ZEROS.

       01 WS-RENEWAL-YR-SENIOR PIC 9(2) VALUE 05.
       01 WS-RENEWAL-YR-ADULT  PIC 9(2) VALUE 08.


      * SEPERATORS AND MESSAGES
       01  WS-BREAKPT     PIC X(25) VALUE '-=-=-=-=-=-=-=-=-=-=-=-=-'.
       01  WS-MESSAGE     PIC X(25) VALUE 'BATCH LICENSE PROCESSING!'.
       01  WS-LINE-SPACE  PIC X(25) VALUE SPACES.
       01  WS-ERROR-MSG   PIC X(50) VALUE SPACES.

      * FEES
       01 WS-FEES.
           02  WS-INT-FEE       PIC 9(5)V99 VALUE 0003000.
           02  WS-RENEW-FEE     PIC 9(5)V99 VALUE 0007500.
           02  WS-OVERDUE-FEE   PIC 9(5)V99 VALUE 0021500.
           02  WS-TOTAL-FEE     PIC 9(5)V99 VALUE ZEROS.
           02  WS-TEMP-FEE      PIC 9(5)V99 VALUE ZEROS.

      * FLAGS
       01  WS-VAL.
           02 WS-EOF-LICFILE            PIC X VALUE 'N'.
           02 WS-ERROR-MSG-SET          PIC X VALUE 'N'.
           02 WS-SHORT-RENEWAL          PIC X VALUE 'N'.

      * LICENSE MINIMUM AGE NEEDED TO OBTAIN
       01  WS-LICENSE-AGE.
           02 WS-A-LICENSE-AGE          PIC 9(3) VALUE 021.
           02 WS-B-LICENSE-AGE          PIC 9(3) VALUE 018.
           02 WS-C-LICENSE-AGE          PIC 9(3) VALUE 018.
           02 WS-D-LICENSE-AGE          PIC 9(3) VALUE 018.
           02 WS-M-LICENSE-AGE          PIC 9(3) VALUE 018.

      * AGE LIMIT FOR SHORTER LICENSE RENEWAL (2 YEARS INSTEAD OF 5)
       01  WS-SHORT-RENEWAL-AGE         PIC 9(3) VALUE 070.

      * MAX AGE FOR LICENSE RENEWAL (NO MORE DRIVING AFTER THIS)
       01  WS-MAX-RENEWAL-AGE           PIC 9(3) VALUE 100.

      * LICENSE TYPES
       01  WS-LICENSE-TYPES.
           02 WS-A-LICENSE-TYPE PIC X(26)
                     VALUE 'A COMMERCIAL LARGE COMBO  '.
           02 WS-B-LICENSE-TYPE PIC X(26)
                     VALUE 'B COMMERCIAL BUS          '.
           02 WS-C-LICENSE-TYPE PIC X(26)
                     VALUE 'C COMMERCIAL MULT PASSNGER'.
           02 WS-D-LICENSE-TYPE PIC X(26)
                     VALUE 'D STANDARD DRIVER         '.
           02 WS-M-LICENSE-TYPE PIC X(26)
                     VALUE 'M MOTORCYCLE              '.


      **************************************************************
       PROCEDURE DIVISION.                                    
      **************************************************************
          
           DISPLAY WS-BREAKPT.
           DISPLAY WS-MESSAGE.
           DISPLAY WS-BREAKPT.

      *    OPEN FILES AND READ FIRST RECORD
           PERFORM R1000-OPEN-DATASETS.

      *    START PROCESSING LICENSE FILES
           MOVE SPACES TO FIL-LICENSE-OUT.
           MOVE '== START OF DMV PROCESS REPORT =='
                 TO FIL-LICENSE-OUT.
           WRITE FIL-LICENSE-OUT.

           MOVE SPACES TO FIL-ERR-LOG.
           MOVE '## START OF DMV ERROR LOG ##'
                 TO FIL-ERR-LOG.
           WRITE FIL-ERR-LOG.

      *    START MAIN LOOP
           PERFORM R3000-PROCESS-FILE
              UNTIL WS-EOF-LICFILE = 'Y'.

           MOVE SPACES TO FIL-LICENSE-OUT.
           MOVE '== END OF DMV PROCESS REPORT =='
                 TO FIL-LICENSE-OUT.
           WRITE FIL-LICENSE-OUT.

           MOVE SPACES TO FIL-ERR-LOG.
           MOVE '## END OF DMV ERROR LOG ##'
                 TO FIL-ERR-LOG.
           WRITE FIL-ERR-LOG.

      *    CLOSE OUT FILES
           PERFORM R4000-CLOSE-DATASETS.

           STOP RUN.


      *  ------
        R1000-OPEN-DATASETS.
      *  ------
           OPEN INPUT FILE-LIC-TRNS.
           OPEN OUTPUT FILE-LIC-OUT.
           OPEN OUTPUT FILE-ERR-LOG.

      *  ------
        R2000-READ-LIC-ENTRY.
      *  ------
           READ FILE-LIC-TRNS INTO DMV-LICENSE-REC 
                 AT END MOVE 'Y' TO WS-EOF-LICFILE.

      * ------------------
        R2100-CLEAR-OUT-REC.
      * ------------------
      * CLEAR OUT RECORD FIELDS AFTER EVER RUN
           DISPLAY '> CLEARING OUTPUT + ERROR LOG FIELDS...'.
           MOVE SPACES TO OUT-NAME-VAL.
           MOVE SPACES TO OUT-LICTYPE-VAL.
           MOVE SPACES TO OUT-EYECOL-VAL.
           MOVE SPACES TO OUT-HAIRCOL-VAL.
           MOVE ZEROS  TO OUT-AGE-VAL.
           MOVE SPACES TO OUT-CORLENS-VAL.
           MOVE SPACES TO OUT-CARTP-VAL.
           MOVE SPACES TO OUT-RECIP-VAL.
           MOVE ZEROES TO OUT-EXPDT-YY.
           MOVE ZEROES TO OUT-EXPDT-MM.
           MOVE ZEROES TO OUT-EXPDT-DD.
           MOVE ZEROES TO OUT-FEE-VAL.

           MOVE SPACES TO ERR-NAME-VAL.
           MOVE SPACES TO ERR-LICTYPE-VAL.
           MOVE SPACES TO ERR-REASON-VAL.
 



      *  ------
        R3000-PROCESS-FILE.
      *  ------
      *NEXT STEP: GO DOWN THE LIST OF CRITERIA AND CHECK EACH ONE
      * RECOMMEND WEEDING OUT IF THEY HAD TO IDENTITY DOCS
      * THEN AGE RESTRICTIONS
      * THEN TESTS, PAYMENT 

           PERFORM R2000-READ-LIC-ENTRY.
           PERFORM R2100-CLEAR-OUT-REC.

           MOVE 'N' TO WS-ERROR-MSG-SET.
           MOVE SPACES TO WS-ERROR-MSG.
           MOVE 'N' TO WS-SHORT-RENEWAL.
           MOVE ZEROES TO WS-TOTAL-FEE.
           MOVE ZEROES TO WS-TEMP-FEE.
           MOVE ZEROES TO WS-RENEWAL-DATES.

           IF WS-EOF-LICFILE = 'N'

              DISPLAY WS-LINE-SPACE.
              DISPLAY WS-BREAKPT.
              DISPLAY 'PROCESSING DMV LICENSE FOR:'.
              DISPLAY DMV-NAME.

      *       1 CHECK AGES.
              PERFORM R3100-CHECK-AGE.

      *       2 CHECK TESTS AND FEES
              PERFORM R3200-CHK-TESTS-FEES-INSUR.

      *       3 CALCULATE FEES 
              PERFORM R3300-CALC-FEES.

              DISPLAY 'IS ERROR? ' WS-ERROR-MSG-SET.
             
   *  *  * IF THEY PASS ALL CRITERIA, WRITE OUT OTHERWISE DONT
                 IF WS-ERROR-MSG-SET = 'N'
                       DISPLAY WS-BREAKPT
                       DISPLAY '--- LICENSE APPROVED! ---'
                       DISPLAY WS-BREAKPT

                       MOVE SPACES TO FIL-LICENSE-OUT

                       MOVE DMV-NAME TO OUT-NAME-VAL
                       MOVE DMV-EYE-COLOR TO OUT-EYECOL-VAL
                       MOVE DMV-HAIR-COLOR TO OUT-HAIRCOL-VAL
                       MOVE DMV-AGE TO OUT-AGE-VAL
                       MOVE DMV-CORRECTIVE-LENS TO OUT-CORLENS-VAL
                       MOVE DMV-CAR-TYPE  TO OUT-CARTP-VAL
                       MOVE DMV-RECIPROCITY TO OUT-RECIP-VAL
                       MOVE WS-TOTAL-FEE TO OUT-FEE-VAL

                       MOVE OUT-PT1 TO FIL-LICENSE-OUT
                       WRITE FIL-LICENSE-OUT
                       MOVE SPACES TO FIL-LICENSE-OUT

                       MOVE OUT-PT2 TO FIL-LICENSE-OUT
                       WRITE FIL-LICENSE-OUT
                       MOVE SPACES TO FIL-LICENSE-OUT

                       MOVE OUT-PT3 TO FIL-LICENSE-OUT
                       WRITE FIL-LICENSE-OUT
                       MOVE SPACES TO FIL-LICENSE-OUT

                       MOVE OUT-PT4 TO FIL-LICENSE-OUT
                       WRITE FIL-LICENSE-OUT
                       MOVE SPACES TO FIL-LICENSE-OUT


      *     *     * SEPERATOR FOR NEXT RECORD
                       DISPLAY WS-BREAKPT
                 .

                 IF WS-ERROR-MSG-SET = 'Y'
                       DISPLAY '--- ERROR! REQUEST DENIED ---'
                       DISPLAY WS-LINE-SPACE
                       DISPLAY ' USER NAME: ' DMV-NAME
                       DISPLAY WS-LINE-SPACE
                       DISPLAY ' LICENSE TYPE: ' DMV-LICENSE-TYPE
                       DISPLAY WS-LINE-SPACE
                       DISPLAY ' REASON FOR DENIAL: ' WS-ERROR-MSG
                       DISPLAY WS-LINE-SPACE
                       DISPLAY '--- --- --- '

                       MOVE SPACES TO FIL-ERR-LOG
                       MOVE DMV-NAME TO ERR-NAME-VAL
                       MOVE WS-ERROR-MSG TO ERR-REASON-VAL

                       MOVE ERR-PT1 TO FIL-ERR-LOG
                       WRITE FIL-ERR-LOG
                       MOVE SPACES TO FIL-ERR-LOG

                       MOVE ERR-PT2 TO FIL-ERR-LOG
                       WRITE FIL-ERR-LOG
                       MOVE SPACES TO FIL-ERR-LOG

                       MOVE ERR-PT3 TO FIL-ERR-LOG
                       WRITE FIL-ERR-LOG
                       MOVE SPACES TO FIL-ERR-LOG


                       DISPLAY WS-BREAKPT
                  .
       .

       .


      * ------------------
        R3100-CHECK-AGE.
      * ------------------
           DISPLAY '> CHECKING AGE CRITERIA...'.
           DISPLAY '   STRT R3100 ERROR SET? ' WS-ERROR-MSG-SET.
           DISPLAY '   STRT R3100 ERROR MSG: ' WS-ERROR-MSG.
           DISPLAY WS-LINE-SPACE.

           IF DMV-AGE > WS-MAX-RENEWAL-AGE
              MOVE '-NA-' TO ERR-LICTYPE-VAL
              MOVE 'ERROR: TOO OLD FOR LICENSE RENEWAL'
                    TO WS-ERROR-MSG
              MOVE 'Y' TO WS-ERROR-MSG-SET
              DISPLAY WS-ERROR-MSG
           .

           IF DMV-AGE < 018 AND WS-ERROR-MSG-SET = 'N'
              MOVE '-NA-' TO ERR-LICTYPE-VAL
              MOVE 'ERROR: TOO YOUNG FOR ANY LICENSE'
                    TO WS-ERROR-MSG
              MOVE 'Y' TO WS-ERROR-MSG-SET
              DISPLAY WS-ERROR-MSG
           .

           IF WS-ERROR-MSG-SET = 'N'
              IF DMV-LICENSE-TYPE = 'A'
                 DISPLAY ' TYPE A'
                 MOVE WS-A-LICENSE-TYPE TO OUT-LICTYPE-VAL
                 MOVE WS-A-LICENSE-TYPE TO ERR-LICTYPE-VAL
                 IF DMV-AGE < WS-A-LICENSE-AGE
                    MOVE 'ERROR: TOO YOUNG FOR TYPE A LICENSE'
                          TO WS-ERROR-MSG
                    MOVE 'Y' TO WS-ERROR-MSG-SET
                    DISPLAY WS-ERROR-MSG
           .

           IF WS-ERROR-MSG-SET = 'N'
              IF DMV-LICENSE-TYPE = 'B'
                 DISPLAY ' TYPE B'
                 MOVE WS-B-LICENSE-TYPE TO OUT-LICTYPE-VAL
                 MOVE WS-B-LICENSE-TYPE TO ERR-LICTYPE-VAL
                 IF DMV-AGE < WS-B-LICENSE-AGE
                    MOVE 'ERROR: TOO YOUNG FOR TYPE B LICENSE'
                          TO WS-ERROR-MSG
                    MOVE 'Y' TO WS-ERROR-MSG-SET
                    DISPLAY WS-ERROR-MSG
           .

           IF WS-ERROR-MSG-SET = 'N'
              IF DMV-LICENSE-TYPE = 'C'
                 DISPLAY ' TYPE C'
                 MOVE WS-C-LICENSE-TYPE TO OUT-LICTYPE-VAL
                 MOVE WS-C-LICENSE-TYPE TO ERR-LICTYPE-VAL
                 IF DMV-AGE < WS-C-LICENSE-AGE
                    MOVE 'ERROR: TOO YOUNG FOR TYPE C LICENSE'
                          TO WS-ERROR-MSG
                    MOVE 'Y' TO WS-ERROR-MSG-SET
                    DISPLAY WS-ERROR-MSG
           .

           IF WS-ERROR-MSG-SET = 'N'
              IF DMV-LICENSE-TYPE = 'D'
                 DISPLAY ' TYPE D'
                 MOVE WS-D-LICENSE-TYPE TO OUT-LICTYPE-VAL
                 MOVE WS-D-LICENSE-TYPE TO ERR-LICTYPE-VAL
                 IF DMV-AGE < WS-D-LICENSE-AGE
                    MOVE 'ERROR: TOO YOUNG FOR TYPE D LICENSE'
                          TO WS-ERROR-MSG
                    MOVE 'Y' TO WS-ERROR-MSG-SET
                    DISPLAY WS-ERROR-MSG
           .

           IF WS-ERROR-MSG-SET = 'N'
              IF DMV-LICENSE-TYPE = 'M'
                 DISPLAY ' TYPE M'
                 MOVE WS-M-LICENSE-TYPE TO OUT-LICTYPE-VAL
                 MOVE WS-M-LICENSE-TYPE TO ERR-LICTYPE-VAL
                 IF DMV-AGE < WS-M-LICENSE-AGE
                    MOVE 'ERROR: TOO YOUNG FOR TYPE M LICENSE'
                          TO WS-ERROR-MSG
                    MOVE 'Y' TO WS-ERROR-MSG-SET
                    DISPLAY WS-ERROR-MSG
           .

           DISPLAY ' CHECK SHORT RENEWAL (5 YEARS INSTEAD OF 8)'.
           IF DMV-AGE > WS-SHORT-RENEWAL-AGE
              MOVE 'Y' TO WS-SHORT-RENEWAL
              DISPLAY 'SHORT RENEWAL, 5 YEAR RENEWAL INSTEAD OF 8'
              COMPUTE WS-RENEW-YY = WS-CURRENT-YY
                      + WS-RENEWAL-YR-SENIOR
              MOVE WS-CURRENT-MM TO WS-RENEW-MM
              MOVE WS-CURRENT-DD TO WS-RENEW-DD
           ELSE
              DISPLAY 'NOT A SHORT RENEWAL, 8 YEAR RENEWAL'
              COMPUTE WS-RENEW-YY = WS-CURRENT-YY
                      + WS-RENEWAL-YR-ADULT
              MOVE WS-CURRENT-MM TO WS-RENEW-MM
              MOVE WS-CURRENT-DD TO WS-RENEW-DD
           .

           MOVE WS-RENEW-DD TO OUT-EXPDT-DD.
           MOVE WS-RENEW-MM TO OUT-EXPDT-MM.
           MOVE WS-RENEW-YY TO OUT-EXPDT-YY.

           DISPLAY '   END R3100 ERROR SET? ' WS-ERROR-MSG-SET.
           DISPLAY '   END R3100 ERROR MSG: ' WS-ERROR-MSG.
           DISPLAY '  RENEWAL DATE CALCULATED AS: ' WS-RENEWAL-DATES.
           DISPLAY WS-LINE-SPACE.


      * ------------------
        R3200-CHK-TESTS-FEES-INSUR.
      * ------------------
           DISPLAY '> CHECKING TESTS, FEES, INSURANCE...'.
           DISPLAY '   STRT R3200 ERROR SET? ' WS-ERROR-MSG-SET.
           DISPLAY '   STRT R3200 ERROR MSG: ' WS-ERROR-MSG.
           DISPLAY WS-LINE-SPACE.

           IF WS-ERROR-MSG-SET = 'N'
              IF DMV-ROAD-TEST NOT = 'Y'
                 MOVE 'ERROR: DID NOT PASS ROAD TEST'
                       TO WS-ERROR-MSG
                 MOVE 'Y' TO WS-ERROR-MSG-SET
           .

           IF WS-ERROR-MSG-SET = 'N'
              IF DMV-WRITTEN-TEST NOT = 'Y'
                 MOVE 'ERROR: DID NOT PASS WRITTEN TEST'
                       TO WS-ERROR-MSG
                 MOVE 'Y' TO WS-ERROR-MSG-SET
           .

           IF WS-ERROR-MSG-SET = 'N'
              IF DMV-PAID-FEE NOT = 'Y'
                 MOVE 'ERROR: DID NOT PAY REQUIRED FEE'
                       TO WS-ERROR-MSG
                 MOVE 'Y' TO WS-ERROR-MSG-SET
           .

           IF WS-ERROR-MSG-SET = 'N'
              IF DMV-INSURANCE NOT = 'Y'
                 MOVE 'ERROR: DID NOT PROVIDE PROOF OF INSURANCE'
                       TO WS-ERROR-MSG
                 MOVE 'Y' TO WS-ERROR-MSG-SET
           .

           IF WS-ERROR-MSG-SET = 'N'
              IF DMV-IDENTITY-DOC NOT = 'Y'
                 MOVE 'ERROR: DID NOT PROVIDE IDENTITY DOC'
                       TO WS-ERROR-MSG
                 MOVE 'Y' TO WS-ERROR-MSG-SET
           .

           DISPLAY '   END R3200 ERROR SET? ' WS-ERROR-MSG-SET.
           DISPLAY '   END R3200 ERROR MSG: ' WS-ERROR-MSG.
           DISPLAY WS-LINE-SPACE.

      * ------------------
        R3300-CALC-FEES.
      * ------------------
           DISPLAY '> CALCULATING FEES OWED...'.
           MOVE ZEROS TO WS-TOTAL-FEE.
           MOVE ZEROS TO WS-TEMP-FEE.

           IF DMV-EXPIRE-DATE < WS-CURRENT-DATE
              DISPLAY 'LICENSE IS OVERDUE, ADDING OVERDUE FEE'
              COMPUTE WS-TEMP-FEE = WS-INT-FEE + WS-OVERDUE-FEE
           ELSE
              DISPLAY 'LICENSE IS NOT OVERDUE, NO EXTRA FEE'
              COMPUTE WS-TEMP-FEE = WS-INT-FEE
           .

           IF DMV-RENEWAL = 'Y'
              DISPLAY 'THIS WAS A RENEWAL, HIGHER FEE'
              COMPUTE WS-TOTAL-FEE = WS-TEMP-FEE + WS-RENEW-FEE
           ELSE
              DISPLAY 'THIS WAS NOT A RENEWAL, JUST INITIAL FEE'
              COMPUTE WS-TOTAL-FEE = WS-TEMP-FEE
           .


      * ------------------
        R4000-CLOSE-DATASETS.
      * ------------------
           CLOSE FILE-LIC-TRNS.
           CLOSE FILE-LIC-OUT.
           CLOSE FILE-ERR-LOG.
 
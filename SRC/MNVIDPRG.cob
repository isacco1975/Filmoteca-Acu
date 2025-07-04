      ******************************************************************
      * FILE NAME   : MNVIDPRG                                         *
      * DATE        : 2025-06-07                                       *
      * AUTHOR      : FABIO MARQUES (FMARQUES@FMARQUES.ETI.BR)         *
      * DATA CENTER : COMPANY.EDUC360                                  *
      * PURPOSE     : MAIN MENU OF VIDEOTECA PROGRAM                   *
      ******************************************************************
      * ACUCOBOL CONVERSION AND OPTIMIZATION BY:                       *
      *          ISAAC GARCIA PEVERI (IGP TECH BLOG [YOUTUBE CHANNEL]) *
      *          JULY 2025                                             *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MNVIDPRG.
       AUTHOR. FABIO MARQUES.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY 'CPVIDFCV.cpy'. *> MOVIES DAT WORKBOOK
      *
       DATA DIVISION.
       FILE SECTION.
       FD MOVIES.
           COPY 'CPVIDDAT.cpy'.
      *
       WORKING-STORAGE SECTION.
           COPY 'CPVIDMAN.cpy'. *> MAIN SCREEN
           COPY 'CPVIDMNU.cpy'. *> MAIN MENU
           COPY 'CPVIDMSG.cpy'. *> MESSAGES
           COPY 'CPVIDFCW.cpy'. *> MOVIES DAT WORKBOOK
           COPY 'CPVIDABE.cpy'. *> ABEND
       77  WRK-DATFILE-CREATED      PIC X VALUE 'N'.
      *
       SCREEN SECTION.
           COPY 'SCVIDBAN.cpy'. *> BANNER - ASCII ART
           COPY 'SCVIDMAN.cpy'. *> MAIN SCREEN
           COPY 'SCVIDMNU.cpy'. *> MAIN MENU
           COPY 'SCVIDMSG.cpy'. *> MESSAGES
      *
       PROCEDURE DIVISION.
       0000-MAIN SECTION.
           DISPLAY SCREEN-ASCII-ART.
           ACCEPT OMITTED
      *
           PERFORM 0100-OPEN-DATA.
           PERFORM 0300-PROCESS-DATA UNTIL WRK-OPTION EQUAL ZEROES.
           PERFORM 0700-END-PROGRAM.
       0000-MAIN-END. EXIT.
      *
       0100-OPEN-DATA.
           OPEN I-O MOVIES.
      *
           IF FS-MOVIES EQUAL "35" THEN
               OPEN OUTPUT MOVIES
               MOVE 'S' TO WRK-DATFILE-CREATED
           END-IF.
      *
           CLOSE MOVIES.
       0100-OPEN-DATA-END. EXIT.
      *
       0300-PROCESS-DATA.
           COPY 'CPVIDDTE.cpy'. *> DATE/TIME PROCEDURE
           MOVE "     * * * * M A I N  M E N U * * *" TO WRK-TITLE.
           MOVE "PF3=EXIT"                            TO WRK-KEYS.
           MOVE '0'                                   TO WRK-OPTION.
      *
           DISPLAY SCREEN-MAIN.
           DISPLAY SCREEN-MENU
           ACCEPT SCREEN-MENU.
      *
           EVALUATE WRK-OPTION
             WHEN '1' CALL 'INVIDPRG'
             WHEN '2' CALL 'ALVIDPRG'
             WHEN '3' CALL 'EXVIDPRG'
             WHEN '4' CALL 'LSVIDPRG'
             WHEN '5' CALL 'IMVIDPRG'
             WHEN '6' CALL 'XPVIDPRG'
             WHEN '7' CALL 'JSVIDPRG'
             WHEN '0' CONTINUE
             WHEN OTHER PERFORM 0310-OTHER
           END-EVALUATE.
       0300-PROCESS-DATA-END. EXIT.

       0310-OTHER.
           INITIALIZE WRK-MSG.
           MOVE '20INVALID OTPION.' TO WRK-MSG.
           DISPLAY SCREEN-MSG.
           ACCEPT SCREEN-WAIT.
       0310-OTHER-END. EXIT.

       0600-ROT-ABEND.
       0600-ROT-ABEND-END. EXIT.

       0700-END-PROGRAM.
           GOBACK.
       0700-END-PROGRAM-END. EXIT.

       END PROGRAM MNVIDPRG.

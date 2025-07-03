      ******************************************************************
      * FILE NAME   : ALVIDPRG                                         *
      * DATE        : 2025-06-07                                       *
      * AUTHOR      : FABIO MARQUES (FMARQUES@FMARQUES.ETI.BR)         *
      * DATA CENTER : COMPANY.EDUC360                                  *
      * PURPOSE     : CHANGE ROUTINE OF VIDEOTECA PROGRAM              *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALVIDPRG.
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
           COPY 'CPVIDEDT.cpy'. *> EDITION SCREEN
           COPY 'CPVIDMSG.cpy'. *> MESSAGES
           COPY 'CPVIDFCW.cpy'. *> MOVIES DAT WORKBOOK
           COPY 'CPVIDABE.cpy'. *> ABEND
           COPY 'CPVIDSRC.cpy'. *> SEARCH RECORD GENRES
      *
       SCREEN SECTION.
           COPY 'SCVIDMAN.cpy'. *> MAIN SCREEN
           COPY 'SCVIDMNU.cpy'. *> MAIN MENU
           COPY 'SCVIDMSG.cpy'. *> MESSAGES
           COPY 'SCVIDEDT.cpy'. *> EDITION SCREEN
      *
       PROCEDURE DIVISION.
       0000-MAIN SECTION.
           INITIALIZE WRK-CONTINUE.
           PERFORM 0100-OPEN-DATA.
           PERFORM 0300-PROCESS-DATA UNTIL WRK-CONTINUE EQUAL 'N'
                                        OR WRK-CONTINUE EQUAL 'n'.
           PERFORM 0500-CLOSE-DATA.
           PERFORM 0700-END-PROGRAM.
       0000-MAIN-END. EXIT.

       0100-OPEN-DATA SECTION.
           OPEN I-O MOVIES.
      *
           IF FS-MOVIES NOT EQUAL ZERO
               MOVE '46ERROR OPENING MOVIES FILE.'
                   TO WRK-MSG
               DISPLAY SCREEN-MSG
               ACCEPT SCREEN-WAIT
      *
               MOVE FS-MOVIES TO WS-ABEND-CODE
               MOVE 'ERROR OPENING MOVIES FILE'
                   TO WS-ABEND-MESSAGE
               PERFORM 0600-ROT-ABEND
           END-IF.
       0100-OPEN-DATA-END. EXIT.

       0200-VALIDATE-DATA SECTION.
           COPY 'CPVIDRVD.cpy'. *> VALIDATION EDIT DATA ROUTINE
       0200-VALIDATE-DATA-END. EXIT.

       0300-PROCESS-DATA SECTION.
           COPY 'CPVIDDTE.cpy'. *> DATE/TIME PROCEDURE
           MOVE "   * * * *   EDIT MOVIE FORM  * * * *"    TO WRK-TITLE.
           MOVE "PF3=EXT  TAB=NEXT FIELD  ENTER=CONFIRM/SELECT GENRE"
             TO WRK-KEYS.
           MOVE 9                                           TO WRK-LINE.
           INITIALIZE REG-FIL WRK-MSG WRK-IS-VALID.
      *
           DISPLAY SCREEN-MAIN.
           DISPLAY SCREEN-MENU.
           DISPLAY SCREEN-EDIT.
           ACCEPT  SCREEN-EDIT-KEY.

           READ MOVIES
               INVALID KEY
                   MOVE '48MOVIE CODE NOT FOUND! NEW SEARCH (Y/N)?'
                       TO WRK-MSG
               NOT INVALID KEY
                   PERFORM 0310-SRC-GENRE
                   ACCEPT  SCREEN-EDIT-DDS
                   PERFORM 0310-SRC-GENRE
      *
                   PERFORM 0200-VALIDATE-DATA
      *
                   IF WRK-IS-VALID = 'N'
                       MOVE WRK-VALID-MESSAGE TO WRK-MSG
                       DISPLAY SCREEN-MSG
                       ACCEPT SCREEN-WAIT
                   ELSE
                       MOVE '42CONFIRM RECORDING THE ENTRY (Y/N)?'
                           TO WRK-MSG
      *
                           DISPLAY SCREEN-CONFIRMATION
                           ACCEPT SCREEN-CONFIRMATION-WAIT
      *
                           IF WRK-AWAIT EQUAL 'Y' OR EQUAL 'y'
                           INITIALIZE WRK-MSG
                           REWRITE REG-FIL
                               INVALID KEY
                                   MOVE
                               '42EDIT ERROR! NEW ENTRY (Y/N)?'
                                   TO WRK-MSG
                               NOT INVALID KEY
                                   MOVE
                    '54RECORD CHANGED SUCCESSFULLY. NEW ENTRY (Y/N)?'
                                   TO WRK-MSG
                           END-REWRITE
                       ELSE
                           INITIALIZE WRK-MSG
                           MOVE '27NEW ENTRY (Y/N)?'
                           TO WRK-MSG
                       END-IF
                   END-IF
           END-READ.
      *
           IF WRK-IS-VALID NOT EQUAL 'N'
               DISPLAY SCREEN-CONFIRMATION
               ACCEPT SCREEN-CONFIRMATION-WAIT
               MOVE WRK-AWAIT TO WRK-CONTINUE
           END-IF.
       0300-PROCESS-DATA-END. EXIT.

       0310-SRC-GENRE SECTION.
           COPY 'CPVIDGSE.cpy'. *> SEARCH GENRE ROUTINE
       0310-SRC-GENRE-END. EXIT.

       0400-PRINT-RESULTS SECTION.
       0400-PRINT-RESULTS-END. EXIT.

       0500-CLOSE-DATA SECTION.
           CLOSE MOVIES.
      *
           IF FS-MOVIES NOT EQUAL ZERO
               MOVE '47ERROR FETCHING MOVIES RECORD.'
                   TO WRK-MSG
               DISPLAY SCREEN-MSG
               ACCEPT SCREEN-WAIT
      *
               MOVE FS-MOVIES TO WS-ABEND-CODE
               MOVE 'ERROR FETCHING MOVIES RECORD.'
                   TO WS-ABEND-MESSAGE
               PERFORM 0600-ROT-ABEND
           END-IF.

       0500-CLOSE-DATA-END. EXIT.

       0600-ROT-ABEND SECTION.
           COPY 'CPVIDRAB.cpy'. *> ABEND ROUTINE.
      *
           PERFORM 0700-END-PROGRAM.
       0600-ROT-ABEND-END. EXIT.

       0700-END-PROGRAM SECTION.
           GOBACK.
       0700-END-PROGRAM-END. EXIT.

       END PROGRAM ALVIDPRG.

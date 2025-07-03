      ******************************************************************
      * FILE NAME   : EXVIDPRG                                         *
      * DATE        : 2025-06-07                                       *
      * AUTHOR      : FABIO MARQUES (FMARQUES@FMARQUES.ETI.BR)         *
      * DATA CENTER : COMPANY.EDUC360                                  *
      * PURPOSE     : EXCLUSION ROUTINE OF VIDEOTECA PROGRAM           *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXVIDPRG.
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
       FD  MOVIES.
           COPY 'CPVIDDAT.cpy'. *> MOVIES DATA FILE
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
           INITIALIZE WRK-CONTINUE LNK-GENRES.
           PERFORM 0100-OPEN-DATA.
      *     PERFORM 0200-VALIDATE-DATA.
           PERFORM 0300-PROCESS-DATA UNTIL WRK-CONTINUE EQUAL 'N'
                                        OR WRK-CONTINUE EQUAL 'n'.
      *     PERFORM 0400-PRINT-RESULTS.
           PERFORM 0500-CLOSE-DATA.
           PERFORM 0700-END-PROGRAM.
       0000-MAIN-END. EXIT.

       0100-OPEN-DATA.
           OPEN I-O MOVIES.
           IF FS-MOVIES NOT EQUAL "00"
               MOVE '46ERROR OPENING MOVIES FILE.'
                   TO WRK-MSG
               DISPLAY SCREEN-MSG
               ACCEPT SCREEN-WAIT
      *
               MOVE FS-MOVIES TO WS-ABEND-CODE
               MOVE 'ERROR OPENING MOVIES FILE.'
                   TO WS-ABEND-MESSAGE
               PERFORM 0600-ROT-ABEND
           END-IF.
       0100-OPEN-DATA-END. EXIT.

       0200-VALIDATE-DATA.
       0200-VALIDATE-DATA-END. EXIT.

       0300-PROCESS-DATA.
           COPY 'CPVIDDTE.cpy'. *> DATE/TIME PROCEDURE
           MOVE "    * * * *  MOVIE   REMOVAL  * * * *"    TO WRK-TITLE.
           MOVE "PF3=EXU  TAB=NEXT FIELD  ENTER=CONFIRM/SELECT GENREO"
             TO WRK-KEYS.
           MOVE 10                                         TO WRK-LINE.
           INITIALIZE REG-FIL WRK-MSG WRK-IS-VALID.
      *
           DISPLAY SCREEN-MAIN.
           DISPLAY SCREEN-MENU.
           DISPLAY SCREEN-EDIT.
           ACCEPT  SCREEN-EDIT-KEY.

           READ MOVIES
               INVALID KEY
                   MOVE '48MOVIE NOT LOCALIZED! ADD A NEW ENTRY(Y/N)?'
                       TO WRK-MSG
               NOT INVALID KEY
      *
                   PERFORM 0310-SRC-GENRE
      *
                   INITIALIZE WRK-MSG
                   DISPLAY SCREEN-EDIT-DDS
                   MOVE '41CONFIRMA A EXCLUSAO DO FILME (Y/N)?'
                       TO WRK-MSG
                   DISPLAY SCREEN-CONFIRMATION
                   ACCEPT SCREEN-CONFIRMATION-WAIT
      *
                   IF WRK-AWAIT EQUAL 'Y' OR EQUAL 'y'
                       INITIALIZE WRK-MSG
                       DELETE MOVIES
                           INVALID KEY
                               MOVE
                               '43DELETE ERROR! NEW ENTRY? (Y/N)?'
                                   TO WRK-MSG
                           NOT INVALID KEY
                               MOVE
                    '54MOVIE DELETED SUCCESSFULLY! NEW DELETION (Y/N)?'
                                   TO WRK-MSG
                       END-DELETE
                   ELSE
                       INITIALIZE WRK-MSG
                       MOVE '27NEW DELETION (Y/N)?'
                       TO WRK-MSG
                   END-IF
           END-READ.

           DISPLAY SCREEN-CONFIRMATION.
           ACCEPT SCREEN-CONFIRMATION-WAIT.
           MOVE WRK-AWAIT TO WRK-CONTINUE.
       0300-PROCESS-DATA-END. EXIT.

       0310-SRC-GENRE.
           MOVE GENERO TO LNK-GEN-COD.
           ADD 3 TO WRK-LINE GIVING LNK-GEN-DESC-LINE.
           CALL 'SRVIDPRG' USING LNK-GENRES.
       0310-SRC-GENRE-END. EXIT.

       0400-PRINT-RESULTS.
       0400-PRINT-RESULTS-END. EXIT.

       0500-CLOSE-DATA.
           CLOSE MOVIES.
           IF FS-MOVIES NOT = ZERO
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

       0600-ROT-ABEND.
           COPY 'CPVIDRAB.cpy'. *> ABEND ROUTINE.
      *
           PERFORM 0700-END-PROGRAM.
       0600-ROT-ABEND-END. EXIT.

       0700-END-PROGRAM.
           GOBACK.
       0700-END-PROGRAM-END. EXIT.

       END PROGRAM EXVIDPRG.

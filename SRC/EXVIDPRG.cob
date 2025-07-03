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

       0100-OPEN-DATA SECTION.
           OPEN I-O MOVIES.
           IF FS-MOVIES NOT EQUAL "00"
               MOVE '46ERRO AO ABRIR ARQUIVO DE DADOS DE FILMES.'
                   TO WRK-MSG
               DISPLAY SCREEN-MSG
               ACCEPT SCREEN-WAIT
      *
               MOVE FS-MOVIES TO WS-ABEND-CODE
               MOVE 'ERRO AO ABRIR ARQUIVO DE DADOS DE FILMES'
                   TO WS-ABEND-MESSAGE
               PERFORM 0600-ROT-ABEND
           END-IF.
       0100-OPEN-DATA-END. EXIT.

       0200-VALIDATE-DATA SECTION.
       0200-VALIDATE-DATA-END. EXIT.

       0300-PROCESS-DATA SECTION.
           COPY 'CPVIDDTE.cpy'. *> DATE/TIME PROCEDURE
           MOVE "    * * * * EXCLUSAO DE FILME * * * *"    TO WRK-TITLE.
           MOVE "PF3=FIM  TAB=PROX CAMPO  ENTER=CONFIRMA/ESCOLHE GENERO"
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
                   MOVE '48FILME NAO LOCALIZADO! NOVA EXCLUSAO (S/N)?'
                       TO WRK-MSG
               NOT INVALID KEY
      *
                   PERFORM 0310-SRC-GENRE
      *
                   INITIALIZE WRK-MSG
                   DISPLAY SCREEN-EDIT-DDS
                   MOVE '41CONFIRMA A EXCLUSAO DO FILME (S/N)?'
                       TO WRK-MSG
                   DISPLAY SCREEN-CONFIRMATION
                   ACCEPT SCREEN-CONFIRMATION-WAIT
      *
                   IF WRK-AWAIT EQUAL 'S' OR EQUAL 's'
                       INITIALIZE WRK-MSG
                       DELETE MOVIES
                           INVALID KEY
                               MOVE
                               '43ERRO AO EXCLUIR! NOVA EXCLUSAO (S/N)?'
                                   TO WRK-MSG
                           NOT INVALID KEY
                               MOVE
                    '54FILME EXCLUIDO COM SUCESSO! NOVA EXCLUSAO (S/N)?'
                                   TO WRK-MSG
                       END-DELETE
                   ELSE
                       INITIALIZE WRK-MSG
                       MOVE '27NOVA EXCLUSAO (S/N)?'
                       TO WRK-MSG
                   END-IF
           END-READ.

           DISPLAY SCREEN-CONFIRMATION.
           ACCEPT SCREEN-CONFIRMATION-WAIT.
           MOVE WRK-AWAIT TO WRK-CONTINUE.
       0300-PROCESS-DATA-END. EXIT.

       0310-SRC-GENRE SECTION.
           MOVE GENERO TO LNK-GEN-COD.
           ADD 3 TO WRK-LINE GIVING LNK-GEN-DESC-LINE.
           CALL 'SRVIDPRG' USING LNK-GENRES.
       0310-SRC-GENRE-END. EXIT.

       0400-PRINT-RESULTS SECTION.
       0400-PRINT-RESULTS-END. EXIT.

       0500-CLOSE-DATA SECTION.
           CLOSE MOVIES.
           IF FS-MOVIES NOT = ZERO
               MOVE '47ERRO AO FECHAR ARQUIVO DE DADOS DE FILMES.'
                   TO WRK-MSG
               DISPLAY SCREEN-MSG
               ACCEPT SCREEN-WAIT
      *
               MOVE FS-MOVIES TO WS-ABEND-CODE
               MOVE 'ERRO AO FECHAR ARQ DE DADOS DE FILMES.'
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

       END PROGRAM EXVIDPRG.

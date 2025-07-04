      ******************************************************************
      * FILE NAME   : INVIDPRG                                         *
      * DATE        : 2025-06-07                                       *
      * AUTHOR      : FABIO MARQUES (FMARQUES@FMARQUES.ETI.BR)         *
      * DATA CENTER : COMPANY.EDUC360                                  *
      * PURPOSE     : LIST ON SCREEN ROUTINE OF VIDEOTECA PROGRAM      *
      ******************************************************************
      * ACUCOBOL CONVERSION AND OPTIMIZATION BY:                       *
      *          ISAAC GARCIA PEVERI (IGP TECH BLOG [YOUTUBE CHANNEL]) *
      *          JULY 2025                                             *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LSVIDPRG.
       AUTHOR. FABIO MARQUES.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY 'CPVIDSEQ.cpy'. *> MOVIES DAT WORKBOOK SEQUENTIAL
           COPY 'CPVIDGES.cpy'. *> GENRES DAT WORKBOOK
      *
       DATA DIVISION.

       FILE SECTION.
       FD MOVIES
           RECORDING MODE IS F.
           COPY 'CPVIDDAT.cpy'.

       FD  GENRES.
           COPY 'CPVIDGEN.cpy'. *> GENRE DATA FILE
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
       77  WRK-LST-LINE             PIC 9(02) USAGE COMP-3 VALUE 14.
       77  WRK-LST-PAGE             PIC 9(02) USAGE COMP-3 VALUE 01.
       77  WRK-LST-COUNT            PIC 9(02) USAGE COMP-3 VALUE 01.
       77  FS-GENRES                PIC XX.
       77  IDX PIC S9(4) COMP VALUE ZERO.
       
       01 TAB-GEN.
          05 OCC-GEN      OCCURS 10 TIMES.
             10 EL-GEN-COD  PIC X(2).
             10 EL-GEN-DESC PIC X(8).
      *
       SCREEN SECTION.
           COPY 'SCVIDMAN.cpy'. *> MAIN SCREEN
           COPY 'SCVIDMNU.cpy'. *> MAIN MENU
           COPY 'SCVIDMSG.cpy'. *> MESSAGES
           COPY 'SCVIDLST.cpy'. *> LIST SCREEN
      *
       PROCEDURE DIVISION.
       0000-MAIN.
           INITIALIZE WRK-CONTINUE.
           PERFORM 0100-OPEN-DATA.
           PERFORM 0300-PROCESS-DATA UNTIL WRK-CONTINUE EQUAL 'N'
                                        OR WRK-CONTINUE EQUAL 'n'.
           PERFORM 0500-CLOSE-DATA.
           PERFORM 0700-END-PROGRAM.
       0000-MAIN-END. EXIT.

       0100-OPEN-DATA.
           OPEN INPUT MOVIES.
           OPEN INPUT GENRES.
      *
           IF FS-GENRES NOT EQUAL "00"
               MOVE '47ERROR OPENING GENRE FILE.'
                   TO WRK-MSG
               DISPLAY SCREEN-MSG
               ACCEPT SCREEN-WAIT
      *
               MOVE FS-GENRES TO WS-ABEND-CODE
               MOVE 'ERRO OPENING GENRE FILE'
                   TO WS-ABEND-MESSAGE
               PERFORM 0600-ROT-ABEND
           END-IF.
      *
           READ GENRES AT END CONTINUE END-READ  
           
           IF FS-GENRES = ZERO   
              PERFORM GENRE-TO-MEMORY
           END-IF   
      
           IF FS-MOVIES NOT EQUAL ZERO
               MOVE '46ERROR DURING OPENING MOVIES FILE.'
                   TO WRK-MSG
               DISPLAY SCREEN-MSG
               ACCEPT SCREEN-WAIT
      *
               MOVE FS-MOVIES TO WS-ABEND-CODE
               MOVE 'ERROR DURING OPENING MOVIES FILE'
                   TO WS-ABEND-MESSAGE
               PERFORM 0600-ROT-ABEND
           END-IF.
      *
           MOVE LOW-VALUES TO CODIGO
           START MOVIES KEY >= CODIGO END-START
           READ MOVIES NEXT AT END CONTINUE END-READ
      *
           IF FS-MOVIES NOT EQUAL "00"
               MOVE '46ERROR FETCHING MOVIES FIRST ENTRY.'
                   TO WRK-MSG
               DISPLAY SCREEN-MSG
               ACCEPT SCREEN-WAIT
      *
               MOVE FS-MOVIES TO WS-ABEND-CODE
               MOVE 'ERROR FETCHING MOVIES FIRST ENTRY.'
                   TO WS-ABEND-MESSAGE
               PERFORM 0600-ROT-ABEND
           END-IF.
       0100-OPEN-DATA-END. EXIT.

       GENRE-TO-MEMORY.
           PERFORM UNTIL FS-GENRES NOT = ZERO
              ADD 1 TO IDX
              MOVE CODIGO-GEN TO EL-GEN-COD (IDX)
              MOVE DESC-GEN   TO EL-GEN-DESC(IDX)
              READ GENRES AT END CONTINUE END-READ
           END-PERFORM   
           .

       0200-VALIDATE-DATA.
       0200-VALIDATE-DATA-END. EXIT.

       0300-PROCESS-DATA.
           COPY 'CPVIDDTE.cpy'. *> DATE/TIME PROCEDURE
           MOVE "   * * * * M O V I E  L I S T * * * *" TO WRK-TITLE.
           MOVE "PF3=EXT   ANY KEY TO ADVANCE PAGE" TO WRK-KEYS.
           MOVE 8 TO WRK-LINE.
      *
           DISPLAY SCREEN-MENU.
           DISPLAY SCREEN-LIST.
      *
           PERFORM VARYING WRK-LST-COUNT
             FROM 1 BY 1 UNTIL WRK-LST-COUNT EQUAL 08
                            OR FS-MOVIES     EQUAL 10
      *
               PERFORM 0310-SRC-GENRE
      *
               DISPLAY CODIGO       AT LINE WRK-LST-LINE COLUMN 14
               COLOR 4 HIGHLIGHT             
               DISPLAY TITULO       AT LINE WRK-LST-LINE COLUMN 23
               COLOR 11 HIGHLIGHT 
               DISPLAY LNK-GEN-DESC AT LINE WRK-LST-LINE COLUMN 56
               COLOR 11 HIGHLIGHT 
               DISPLAY NOTA         AT LINE WRK-LST-LINE COLUMN 68
               COLOR 11 HIGHLIGHT 
      *
               ADD 1 TO WRK-LST-LINE
               READ MOVIES NEXT AT END CONTINUE END-READ
           END-PERFORM.
      *
           MOVE 14 TO WRK-LST-LINE.
           ADD   1 TO WRK-LST-PAGE.
           INITIALIZE WRK-MSG.

           IF FS-MOVIES EQUAL 10
               MOVE '20END OF FILE.' TO WRK-MSG
      *
               DISPLAY SCREEN-MSG
               ACCEPT SCREEN-WAIT
               MOVE 'N' TO WRK-CONTINUE
           ELSE
               MOVE '31SHOW MORE ENRTRIES (Y/N)?' TO WRK-MSG
      *
               DISPLAY SCREEN-CONFIRMATION
               ACCEPT SCREEN-CONFIRMATION-WAIT
      *
               IF WRK-AWAIT EQUAL 'Y' OR EQUAL 'y'
                   MOVE WRK-AWAIT TO WRK-CONTINUE
               ELSE
                   MOVE 'N' TO WRK-CONTINUE
               END-IF
           END-IF.
       0300-PROCESS-DATA-END. EXIT.

       0310-SRC-GENRE.
           MOVE GENERO TO LNK-GEN-COD.
           MOVE 0      TO LNK-GEN-DESC-LINE.
           CALL 'SRVIDPRG' USING LNK-GENRES, TAB-GEN.
       0310-SRC-GENRE-END. EXIT.

       0400-PRINT-RESULTS.
       0400-PRINT-RESULTS-END. EXIT.

       0500-CLOSE-DATA.
           CLOSE MOVIES.
      *
           IF FS-MOVIES NOT EQUAL "00"
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
           CLOSE GENRES
           GOBACK.
       0700-END-PROGRAM-END. EXIT.

       END PROGRAM LSVIDPRG.

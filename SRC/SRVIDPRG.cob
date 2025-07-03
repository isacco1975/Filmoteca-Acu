      ******************************************************************
      * FILE NAME   : SRVIDPRG                                         *
      * DATE        : 2025-06-07                                       *
      * AUTHOR      : FABIO MARQUES (FMARQUES@FMARQUES.ETI.BR)         *
      * DATA CENTER : COMPANY.EDUC360                                  *
      * PURPOSE     : SEARCH RECORD OF GENRE ROUTINE OF VIDEOTECA PROG *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SRVIDPRG.
       AUTHOR. FABIO MARQUES.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY 'CPVIDGES.cpy'. *> GENRES DAT WORKBOOK
      *
       DATA DIVISION.
       FILE SECTION.
       FD  GENRES.
           COPY 'CPVIDGEN.cpy'. *> GENRE DATA FILE
      *
       WORKING-STORAGE SECTION.
           COPY 'CPVIDEDT.cpy'. *> EDITION SCREEN       
           COPY 'CPVIDMSG.cpy'. *> MESSAGES
           COPY 'CPVIDGEW.cpy'. *> GENRES DAT WORKBOOK
           COPY 'CPVIDABE.cpy'. *> ABEND
      *
       77  WRK-LST-LINE             PIC 9(02) USAGE COMP-3 VALUE 14.
       77  WRK-LST-PAGE             PIC 9(02) USAGE COMP-3 VALUE 01.
       77  WRK-LST-COUNT            PIC 9(02) USAGE COMP-3 VALUE 01.
       77  WRK-GENRE                PIC X(02)              VALUE SPACES.
      *
       LINKAGE SECTION.
           COPY 'CPVIDSRC.cpy'. *> SEARCH RECORD GENRES
      *
       SCREEN SECTION.
           COPY 'SCVIDGEN.cpy'. *> GENRE SELECTION SCREEN
           COPY 'SCVIDMSG.cpy'. *> MESSAGES
      *
       PROCEDURE DIVISION USING LNK-GENRES.
       0000-MAIN SECTION.
           INITIALIZE LNK-GEN-DESC LNK-GEN-STATUS WRK-CONTINUE.
           MOVE 1 TO WRK-LST-PAGE.
      *
           IF (LNK-GEN-COD) EQUAL '+'
               PERFORM 0100-OPEN-DATA
               PERFORM 0150-SELECT-GENRE UNTIL WRK-CONTINUE EQUAL 'N'
                                            OR WRK-CONTINUE EQUAL 'n'
               PERFORM 0500-CLOSE-DATA
           END-IF.
      *
      *    PERFORM 0200-VALIDATE-DATA.
           PERFORM 0100-OPEN-DATA.
           PERFORM 0300-PROCESS-DATA UNTIL FS-GENRES EQUAL "10".
           PERFORM 0400-PRINT-RESULTS.
           PERFORM 0500-CLOSE-DATA.
           PERFORM 0700-END-PROGRAM.
       0000-MAIN-END. EXIT.

       0100-OPEN-DATA SECTION.
           OPEN INPUT GENRES.
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
           READ GENRES
      *
           IF FS-GENRES NOT EQUAL "00"
               MOVE '48ERROR FETCHING GENRE FIRST ENTRY.'
                   TO WRK-MSG
               DISPLAY SCREEN-MSG
               ACCEPT SCREEN-WAIT
      *
               MOVE FS-GENRES TO WS-ABEND-CODE
               MOVE 'ERROR FETCHING GENRE FIRST ENTRY'
                   TO WS-ABEND-MESSAGE
               PERFORM 0600-ROT-ABEND
           END-IF.
       0100-OPEN-DATA-END. EXIT.

       0150-SELECT-GENRE SECTION.
           DISPLAY 'PF3=EXT   ANY KEY TO FORWARD PAGES'
             AT LINE 24 COLUMN 03.
           
           DISPLAY SCREEN-GENRE
           MOVE 9 TO WRK-LST-LINE

           PERFORM VARYING WRK-LST-COUNT
             FROM 1 BY 1 UNTIL WRK-LST-COUNT EQUAL 10
                            OR FS-GENRES     EQUAL 10
      *
              DISPLAY CODIGO-GEN AT LINE WRK-LST-LINE COLUMN 60
              COLOR 3 HIGHLIGHT
              DISPLAY DESC-GEN   AT LINE WRK-LST-LINE COLUMN 69
              COLOR 7 HIGHLIGHT
      *
               ADD 1 TO WRK-LST-LINE
               READ GENRES
           END-PERFORM
      *
           IF FS-GENRES EQUAL 10
             MOVE 'N'       TO WRK-CONTINUE
             DISPLAY '* EXT *' AT LINE WRK-LST-LINE COLUMN 69
           END-IF.
      *
           DISPLAY 'CODE...: [  ]' AT LINE 19 COLUMN 58.
           ACCEPT WRK-GENRE        AT LINE 19 COLUMN 68.
      *
           IF(WRK-GENRE) NOT EQUAL SPACES
             MOVE 'N'       TO WRK-CONTINUE
             MOVE WRK-GENRE TO LNK-GEN-COD
           END-IF.
      *
           MOVE 14 TO WRK-LST-LINE.
           ADD   1 TO WRK-LST-PAGE.
       0150-SELECT-GENRE-END. EXIT.

       0200-VALIDATE-DATA SECTION.
       0200-VALIDATE-DATA-END. EXIT.

       0300-PROCESS-DATA SECTION.
           IF LNK-GEN-COD EQUAL CODIGO-GEN
               MOVE DESC-GEN TO LNK-GEN-DESC
               MOVE 1        TO LNK-GEN-STATUS
           END-IF.
      *
           READ GENRES AT END CONTINUE END-READ.
       0300-PROCESS-DATA-END. EXIT.

       0400-PRINT-RESULTS SECTION.
           IF LNK-GEN-DESC EQUAL SPACES
               MOVE 'N LOCALI' TO LNK-GEN-DESC
           END-IF.
      *
           IF LNK-GEN-DESC NOT = SPACES
              DISPLAY LNK-GEN-DESC AT LINE LNK-GEN-DESC-LINE + 1 
              COLOR 3 HIGHLIGHT
              COLUMN 36
           END-IF.
       0400-PRINT-RESULTS-END. EXIT.

       0500-CLOSE-DATA SECTION.
           CLOSE GENRES.
           IF FS-GENRES NOT EQUAL "00"
               MOVE '48ERRO FETCHING GENRE RECORD'
                   TO WRK-MSG
               DISPLAY SCREEN-MSG
               ACCEPT SCREEN-WAIT
      *
               MOVE FS-GENRES TO WS-ABEND-CODE
               MOVE 'ERRO FETCHING GENRE RECORD'
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

       END PROGRAM SRVIDPRG.

      ******************************************************************
      * FILE NAME   : JSVIDPRG                                         *
      * DATE        : 2025-06-07                                       *
      * AUTHOR      : FABIO MARQUES (FMARQUES@FMARQUES.ETI.BR)         *
      * DATA CENTER : COMPANY.EDUC360                                  *
      * PURPOSE     : EXPORT TO JSON ROUTINE OF VIDEOTECA PROGRAM      *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. JSVIDPRG.
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

           SELECT MOVIES-EXP
              ASSIGN       TO 'MOVIES-EXP.json'
              ORGANIZATION IS LINE SEQUENTIAL
              ACCESS MODE  IS SEQUENTIAL
              FILE STATUS  IS FS-MOVIES-EXP.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  MOVIES
           RECORDING MODE IS F.
           COPY 'CPVIDDAT.cpy'.

       FD  MOVIES-EXP.
       01 FIL-EXP          PIC X(500).
      *
       WORKING-STORAGE SECTION.
           COPY 'CPVIDMSG.cpy'. *> MESSAGES
           COPY 'CPVIDFCW.cpy'. *> MOVIES DAT WORKBOOK
           COPY 'CPVIDABE.cpy'. *> ABEND
       77  JSON-OBJECT              PIC X(500).
      *
       77  FS-MOVIES-EXP            PIC X(02).
      *
       01  WRK-ESTATISTICA.
           05 WRK-READ-LINES        PIC 9(02) USAGE COMP-3 VALUE ZEROES.
           05 WRK-WRITE-LINES       PIC 9(02) USAGE COMP-3 VALUE ZEROES.
           05 WRK-READ-LINES-EDIT   PIC Z9                 VALUE ZEROES.
           05 WRK-WRITE-LINES-EDIT  PIC Z9                 VALUE ZEROES.
      *
       SCREEN SECTION.
           COPY 'SCVIDMSG.cpy'. *> MESSAGES
      *
       PROCEDURE DIVISION.
       0000-MAIN.
           INITIALIZE WRK-READ-LINES WRK-WRITE-LINES.
      *
           PERFORM 0100-OPEN-DATA.
      *     PERFORM 0200-VALIDATE-DATA.
           PERFORM 0300-PROCESS-DATA UNTIL FS-MOVIES EQUAL "10".
           PERFORM 0400-PRINT-RESULTS.
           PERFORM 0500-CLOSE-DATA.
           PERFORM 0700-END-PROGRAM.
       0000-MAIN-END. EXIT.

       0100-OPEN-DATA.
           OPEN OUTPUT MOVIES-EXP
                INPUT  MOVIES.
      *
           IF FS-MOVIES-EXP NOT EQUAL ZERO
               PERFORM 0500-CLOSE-DATA
               MOVE '45ERROR OPENING OUTPUT FILE (JSON).'
                   TO WRK-MSG
               DISPLAY SCREEN-MSG
               ACCEPT SCREEN-WAIT
      *
               MOVE FS-MOVIES TO WS-ABEND-CODE
               MOVE 'ERROR OPENING OUTPUT FILE (JSON).'
                   TO WS-ABEND-MESSAGE
               PERFORM 0600-ROT-ABEND
           END-IF.
      *
           IF FS-MOVIES NOT EQUAL "00"
               PERFORM 0500-CLOSE-DATA
               MOVE '53ERROR OPENING MOVIES FILE.'
                   TO WRK-MSG
               DISPLAY SCREEN-MSG
               ACCEPT SCREEN-WAIT
      *
               MOVE FS-MOVIES TO WS-ABEND-CODE
               MOVE 'ERROR OPENIONG MOVIES FILE.'
                   TO WS-ABEND-MESSAGE
               PERFORM 0600-ROT-ABEND
           END-IF.

           READ MOVIES NEXT AT END CONTINUE.
      *
           IF FS-MOVIES NOT EQUAL ZERO
               PERFORM 0500-CLOSE-DATA
      *
               MOVE '36ERROR READING THE FIRST ENTRY.'
                   TO WRK-MSG
               DISPLAY SCREEN-MSG
               ACCEPT SCREEN-WAIT
      *
               MOVE FS-MOVIES TO WS-ABEND-CODE
               MOVE 'ERROR READING THE FIRST ENTRY.'
                   TO WS-ABEND-MESSAGE
               PERFORM 0600-ROT-ABEND
           END-IF.

      *    HEAD OF JSON FILE
           INITIALIZE FIL-EXP.
           MOVE '{"MOVIES": [' TO FIL-EXP.
           WRITE FIL-EXP.
       0100-OPEN-DATA-END. EXIT.

       0200-VALIDATE-DATA.
       0200-VALIDATE-DATA-END. EXIT.

       0300-PROCESS-DATA.
           ADD 1 TO WRK-READ-LINES.
           INITIALIZE FIL-EXP.
      
           STRING
               CODIGO ';'
               TITULO ';'
               GENERO ';'
               DURACAO ';'
               DISTRIB ';'
               NOTA
               INTO FIL-EXP.
      
      *        JSON GENERATE FIL-EXP FROM REG-FIL.

               IF WRK-WRITE-LINES GREATER ZERO
                 STRING ',' DELIMITED BY SIZE
                        FIL-EXP DELIMITED BY SIZE
                   INTO JSON-OBJECT

                 MOVE JSON-OBJECT TO FIL-EXP
               END-IF               

               WRITE FIL-EXP.
               IF FS-MOVIES-EXP EQUAL "00"
                   ADD 1 TO WRK-WRITE-LINES
               END-IF.
      *
               READ MOVIES NEXT AT END EXIT PARAGRAPH.
       0300-PROCESS-DATA-END. EXIT.

       0400-PRINT-RESULTS.
           MOVE WRK-READ-LINES  TO WRK-READ-LINES-EDIT.
           MOVE WRK-WRITE-LINES TO WRK-WRITE-LINES-EDIT.
      *
           STRING '41STATISTICS: READ ' DELIMITED BY SIZE
                  WRK-READ-LINES-EDIT DELIMITED BY SIZE
                  ', RECORDED ' DELIMITED BY SIZE
                  WRK-WRITE-LINES-EDIT DELIMITED BY SIZE
                  '.' DELIMITED BY SIZE
                  INTO WRK-MSG.
      *
           DISPLAY SCREEN-MSG.
           ACCEPT SCREEN-WAIT.
       0400-PRINT-RESULTS-END. EXIT.

       0500-CLOSE-DATA.
      *    FOOTER OF JSON FILE
           MOVE ']}' TO FIL-EXP.
           WRITE FIL-EXP.
      *
           CLOSE MOVIES-EXP MOVIES.
      *
           IF FS-MOVIES-EXP NOT EQUAL ZERO
               MOVE '44ERROR FETCHING OUTPUT FILE (JSON).'
                   TO WRK-MSG
               DISPLAY SCREEN-MSG
               ACCEPT SCREEN-WAIT
      *
               MOVE FS-MOVIES TO WS-ABEND-CODE
               MOVE 'ERROR FETCHING OUTPUT FILE (JSON)'
                   TO WS-ABEND-MESSAGE
               PERFORM 0600-ROT-ABEND
           END-IF.
      *
           IF FS-MOVIES NOT EQUAL ZERO
               MOVE '47ERROR FETCHING MOVIES RECORD.'
                   TO WRK-MSG
               DISPLAY SCREEN-MSG
               ACCEPT SCREEN-WAIT
      *
               MOVE FS-MOVIES TO WS-ABEND-CODE
               MOVE 'ERROR FETCHING MOVIES RECORD'
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

       END PROGRAM JSVIDPRG.

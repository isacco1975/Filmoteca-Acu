      ******************************************************************
      * FILE NAME   : IMVIDPRG                                         *
      * DATE        : 2025-06-07                                       *
      * AUTHOR      : FABIO MARQUES (FMARQUES@FMARQUES.ETI.BR)         *
      * DATA CENTER : COMPANY.EDUC360                                  *
      * PURPOSE     : IMPORT FROM CSV ROUTINE OF VIDEOTECA PROGRAM     *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IMVIDPRG.
       AUTHOR. FABIO MARQUES.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY 'CPVIDFCV.cpy'. *> MOVIES DAT WORKBOOK DINAMIC

           SELECT MOVIES-IMP
              ASSIGN       TO 'MOVIES-IMP.csv'
              ORGANIZATION IS LINE SEQUENTIAL
              ACCESS MODE  IS SEQUENTIAL
              FILE STATUS  IS FS-MOVIES-SEQ.

           SELECT RELATO-IMP
              ASSIGN       TO 'RELATO-IMP.txt'
              ORGANIZATION IS LINE SEQUENTIAL
              ACCESS MODE  IS SEQUENTIAL
              FILE STATUS  IS FS-RELATO-IMP.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  MOVIES
           RECORDING MODE IS F.
           COPY 'CPVIDDAT.cpy'.

       FD  MOVIES-IMP.
       01 FIL-IMP               PIC X(200).

       FD  RELATO-IMP.
       01 REL-IMP.              *> LRECL 100
          03 REL-IMP-CODIGO     PIC 9(05).
          03 REL-IMP-TITULO     PIC X(30).
          03 REL-IMP-MENSAGEM   PIC X(65).
      *
       WORKING-STORAGE SECTION.
           COPY 'CPVIDMSG.cpy'. *> MESSAGES
           COPY 'CPVIDFCW.cpy'. *> MOVIES DAT WORKBOOK
           COPY 'CPVIDABE.cpy'. *> ABEND
           COPY 'CPVIDMAN.cpy'. *> DATE/TIME 
      *
       77  FS-MOVIES-SEQ             PIC X(02).
       77  FS-RELATO-IMP             PIC X(02).

       01  WRK-ESTATISTICA.
           05 WRK-READ-LINES        PIC 9(02) USAGE COMP-3 VALUE ZEROES.
           05 WRK-WRITE-LINES       PIC 9(02) USAGE COMP-3 VALUE ZEROES.
           05 WRK-SKIPPED-LINES     PIC 9(02) USAGE COMP-3 VALUE ZEROES.
      *
           05 WRK-READ-LINES-EDIT    PIC Z9                VALUE ZEROES.
           05 WRK-WRITE-LINES-EDIT   PIC Z9                VALUE ZEROES.
           05 WRK-SKIPPED-LINES-EDIT PIC Z9                VALUE ZEROES.
      *
       SCREEN SECTION.
           COPY 'SCVIDMSG.cpy'. *> MESSAGES
      *
       PROCEDURE DIVISION.
       0000-MAIN SECTION.
           INITIALIZE WRK-READ-LINES WRK-WRITE-LINES WRK-SKIPPED-LINES.
      *
           PERFORM 0100-OPEN-DATA.
           PERFORM 0110-DATE-TIME.
      *     PERFORM 0200-VALIDATE-DATA.
           PERFORM 0300-PROCESS-DATA UNTIL FS-MOVIES-SEQ EQUAL "10".
           PERFORM 0400-PRINT-RESULTS.
           PERFORM 0500-CLOSE-DATA.
           PERFORM 0700-END-PROGRAM.
       0000-MAIN-END. EXIT.

       0100-OPEN-DATA SECTION.
           OPEN INPUT  MOVIES-IMP
                I-O    MOVIES
                OUTPUT RELATO-IMP.
      *
           IF FS-MOVIES-SEQ NOT EQUAL "00"
               PERFORM 0500-CLOSE-DATA
               MOVE '45ERRO AO ABRIR ARQUIVO DE ENTRADA (CSV).'
                   TO WRK-MSG
               DISPLAY SCREEN-MSG
               ACCEPT SCREEN-WAIT
      *
               MOVE FS-MOVIES-SEQ TO WS-ABEND-CODE
               MOVE 'ERRO AO ABRIR ARQUIVO DE DADOS DE FILMES'
                   TO WS-ABEND-MESSAGE
               PERFORM 0600-ROT-ABEND
           END-IF.
      *
           IF FS-MOVIES NOT EQUAL "00"
               PERFORM 0500-CLOSE-DATA
               MOVE '53ERRO AO ABRIR ARQUIVO DE DADOS DE FILMES.'
                   TO WRK-MSG
               DISPLAY SCREEN-MSG
               ACCEPT SCREEN-WAIT
      *
               MOVE FS-MOVIES TO WS-ABEND-CODE
               MOVE 'ERRO AO ABRIR ARQUIVO DE DADOS DE FILMES'
                   TO WS-ABEND-MESSAGE
               PERFORM 0600-ROT-ABEND
           END-IF.
      *
           IF FS-RELATO-IMP NOT EQUAL "00"
               OPEN OUTPUT RELATO-IMP
      *        CLOSE RELATO-IMP
      *        OPEN I-O RELATO-IMP

               IF FS-RELATO-IMP NOT EQUAL "00"
                   PERFORM 0500-CLOSE-DATA
                   MOVE '53ERRO AO ABRIR ARQUIVO DE RELATORIO.'
                     TO WRK-MSG
                   DISPLAY SCREEN-MSG
                   ACCEPT SCREEN-WAIT
      *
                   MOVE FS-RELATO-IMP TO WS-ABEND-CODE
                   MOVE 'ERRO AO ABRIR ARQUIVO DE RELATORIO.'
                     TO WS-ABEND-MESSAGE
                   PERFORM 0600-ROT-ABEND
               END-IF
           END-IF.
      *
           READ MOVIES-IMP. *> HEADER
      *
           IF FS-MOVIES NOT EQUAL "00"
               PERFORM 0500-CLOSE-DATA
      *
               MOVE '26ERRO AO LER CABECALHO.'
                   TO WRK-MSG
               DISPLAY SCREEN-MSG
               ACCEPT SCREEN-WAIT
      *
               MOVE FS-MOVIES TO WS-ABEND-CODE
               MOVE 'ERRO AO LER CABECALHO.'
                   TO WS-ABEND-MESSAGE
               PERFORM 0600-ROT-ABEND
           END-IF.
      *
           READ MOVIES-IMP. *> FIRST RECORD LINE
      *
           IF FS-MOVIES NOT EQUAL "00"
               PERFORM 0500-CLOSE-DATA
      *
               MOVE '36ERRO AO LER O PRIMEIRO REGISTRO.'
                   TO WRK-MSG
               DISPLAY SCREEN-MSG
               ACCEPT SCREEN-WAIT
      *
               MOVE FS-MOVIES TO WS-ABEND-CODE
               MOVE 'ERRO AO LER O PRIMEIRO REGISTRO.'
                   TO WS-ABEND-MESSAGE
               PERFORM 0600-ROT-ABEND
           END-IF.
       0100-OPEN-DATA-END. EXIT.

       0110-DATE-TIME SECTION.
           COPY 'CPVIDDTE.cpy'. *> DATE/TIME ROUTINE
       0110-DATE-TIME-END. EXIT.

       0200-VALIDATE-DATA SECTION.
       0200-VALIDATE-DATA-END. EXIT.

       0300-PROCESS-DATA SECTION.
           ADD 1 TO WRK-READ-LINES.
      *
           UNSTRING FIL-IMP DELIMITED BY ";"
               INTO CODIGO,
                    TITULO,
                    GENERO,
                    DURACAO,
                    DISTRIB,
                    NOTA.
      *
               WRITE REG-FIL.
               IF FS-MOVIES EQUAL "00"
                   ADD 1 TO WRK-WRITE-LINES
               ELSE
                   IF WRK-SKIPPED-LINES EQUAL 0
                      INITIALIZE REL-IMP
                      STRING
                        '***************************************** '
                        WRK-DATE ' ' WRK-TIME
                        ' ****************************************'
                        INTO REL-IMP
                      WRITE REL-IMP
                   END-IF
      *
                   INITIALIZE REL-IMP
                   MOVE CODIGO                TO REL-IMP-CODIGO
                   MOVE TITULO                TO REL-IMP-TITULO
                   MOVE 'CODIGO JA EXISTENTE' TO REL-IMP-MENSAGEM
                   WRITE REL-IMP

                   ADD 1 TO WRK-SKIPPED-LINES
               END-IF
      *
               READ MOVIES-IMP.
       0300-PROCESS-DATA-END. EXIT.

       0400-PRINT-RESULTS SECTION.
           MOVE WRK-READ-LINES    TO WRK-READ-LINES-EDIT.
           MOVE WRK-WRITE-LINES   TO WRK-WRITE-LINES-EDIT.
           MOVE WRK-SKIPPED-LINES TO WRK-SKIPPED-LINES-EDIT.
      *
           STRING '39ESTATISTICAS: LD ' DELIMITED BY SIZE
                  WRK-READ-LINES-EDIT DELIMITED BY SIZE
                  ', GR ' DELIMITED BY SIZE
                  WRK-WRITE-LINES-EDIT DELIMITED BY SIZE
                  ', IG ' DELIMITED BY SIZE
                  WRK-SKIPPED-LINES-EDIT DELIMITED BY SIZE
                  '.' DELIMITED BY SIZE
                  INTO WRK-MSG.
      *
           DISPLAY SCREEN-MSG.
           ACCEPT SCREEN-WAIT.
       0400-PRINT-RESULTS-END. EXIT.

       0500-CLOSE-DATA SECTION.
           CLOSE MOVIES-IMP MOVIES RELATO-IMP.
      *
           IF FS-MOVIES-SEQ NOT EQUAL "00"
               PERFORM 0500-CLOSE-DATA
               MOVE '46ERRO AO FECHAR ARQUIVO DE ENTRADA (CSV).'
                   TO WRK-MSG
               DISPLAY SCREEN-MSG
               ACCEPT SCREEN-WAIT
      *
               MOVE FS-MOVIES-SEQ TO WS-ABEND-CODE
               MOVE 'ERRO AO FECHAR ARQUIVO DE ENTRADA (CSV)'
                   TO WS-ABEND-MESSAGE
               PERFORM 0600-ROT-ABEND
           END-IF.
      *
           IF FS-MOVIES NOT EQUAL "00"
               MOVE '47ERRO AO FECHAR ARQUIVO DE DADOS DE FILMES.'
                   TO WRK-MSG
               DISPLAY SCREEN-MSG
               ACCEPT SCREEN-WAIT
      *
               MOVE FS-MOVIES TO WS-ABEND-CODE
               MOVE 'ERRO AO FECHAR ARQ DE DADOS DE FILMES'
                   TO WS-ABEND-MESSAGE
               PERFORM 0600-ROT-ABEND
           END-IF.
      *
           IF FS-RELATO-IMP NOT EQUAL "00"
      *       MOVE '42ERRO AO FECHAR ARQUIVO DE RELATORIO.'
      *         TO WRK-MSG
              MOVE 42 TO WRK-MSG-LEN
              MOVE FS-RELATO-IMP TO WRK-MSG-TEXT

               DISPLAY SCREEN-MSG
               ACCEPT SCREEN-WAIT
      *
               MOVE FS-RELATO-IMP TO WS-ABEND-CODE
               MOVE 'ERRO AO FECHAR ARQ DE RELATORIO'
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

       END PROGRAM IMVIDPRG.

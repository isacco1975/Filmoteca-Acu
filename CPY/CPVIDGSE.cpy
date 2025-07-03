      **
      * SEARCH GENRE ROUTINE
      **
           MOVE GENERO TO LNK-GEN-COD.
           ADD 6 TO WRK-LINE GIVING LNK-GEN-DESC-LINE.
           CALL 'SRVIDPRG' USING LNK-GENRES.
      *    
           IF LNK-GEN-STATUS EQUAL 1
               MOVE LNK-GEN-COD TO GENERO
           END-IF.
      *    
           DISPLAY SCREEN-MAIN.
           DISPLAY SCREEN-MENU.
           DISPLAY SCREEN-EDIT.
           DISPLAY SCREEN-EDIT-KEY.
           DISPLAY SCREEN-EDIT-DDS.
           DISPLAY LNK-GEN-DESC AT LINE LNK-GEN-DESC-LINE COLUMN 36
           COLOR 3 HIGHLIGHT.

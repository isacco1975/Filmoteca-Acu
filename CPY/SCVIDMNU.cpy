      **
      * COPY BOOK SCREEN SECTION OF MAIN MENU
      **
       01  SCREEN-MENU.
           05 LINE 06 COLUMN 10 VALUE "+---------------+".
           05 LINE 07 COLUMN 10 VALUE "| 1. INCLUIR    |".
           05 LINE 08 COLUMN 10 VALUE "| 2. ALTERAR    |".
           05 LINE 09 COLUMN 10 VALUE "| 3. EXCLUIR    |".
           05 LINE 10 COLUMN 10 VALUE "| 4. LISTAR     |".
           05 LINE 11 COLUMN 10 VALUE "| 5. RESTAURAR  |".
           05 LINE 12 COLUMN 10 VALUE "| 6. BACKUP     |".
           05 LINE 13 COLUMN 10 VALUE "| 7. EXPORTAR   |".
           05 LINE 14 COLUMN 10 VALUE "| 0. SAIR       |".
           05 LINE 15 COLUMN 10 VALUE "|               |".
           05 LINE 16 COLUMN 10 VALUE "| OPCAO...: [ ] |".
           05 LINE 17 COLUMN 10 VALUE "+---------------+".
           05 LINE 16 COLUMN 23 PIC X(01) USING WRK-OPTION.

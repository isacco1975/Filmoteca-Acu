      **
      * COPY BOOK SCREEN SECTION OF MAIN MENU
      **
       01  SCREEN-MENU.
           05 LINE 06 COLUMN 10 VALUE "+---------------+".
           05 LINE 07 COLUMN 10 VALUE "| 1. ADD        |".
           05 LINE 08 COLUMN 10 VALUE "| 2. MODIFY     |".
           05 LINE 09 COLUMN 10 VALUE "| 3. DELETE     |".
           05 LINE 10 COLUMN 10 VALUE "| 4. LIST ALL   |".
           05 LINE 11 COLUMN 10 VALUE "| 5. REBUILD    |".
           05 LINE 12 COLUMN 10 VALUE "| 6. BACKUP     |".
           05 LINE 13 COLUMN 10 VALUE "| 7. EXPORT     |".
           05 LINE 14 COLUMN 10 VALUE "| 0. EXIT APP   |".
           05 LINE 15 COLUMN 10 VALUE "|               |".
           05 LINE 16 COLUMN 10 VALUE "| OPTION..: [ ] |".
           05 LINE 17 COLUMN 10 VALUE "+---------------+".
           05 LINE 16 COLUMN 23 PIC X(01) USING WRK-OPTION.

      **
      * COPY BOOK SCREEN SECTION OF EDIT SCREEN
      **
       01 SCREEN-EDIT.
           05 LINE WRK-LINE COLUMN 12 VALUE
               "+---------------------------------------------------+"
               .
           05 LINE PLUS 1 COLUMN 12 VALUE
               "| CODE...........: [     ]                          |"
               .
           05 LINE PLUS 1 COLUMN 12 VALUE
               "| MOVIE NAME.....: [                              ] |" 
               .
           05 LINE PLUS 1 COLUMN 12 VALUE
               "| DURATION.......: [   ]                            |" 
               .
           05 LINE PLUS 1 COLUMN 12 VALUE
               "| DISTRIBUTED BY.: [               ]                |" 
               .
           05 LINE PLUS 1 COLUMN 12 VALUE
               "| MY GRADE.......: [  ]                             |" 
               .
           05 LINE PLUS 1 COLUMN 12 VALUE
               "| GENRE..........: [  ][        ] (+) SEARCH        |" 
               .
           05 LINE PLUS 1 COLUMN 12 VALUE
               "+---------------------------------------------------+" 
               .
           05 LINE 20 COLUMN 23 PIC X(01) USING WRK-OPTION.
      *
       01 SCREEN-EDIT-KEY.
           05 LINE WRK-LINE COLUMN 2  VALUE ' '.
           05 LINE PLUS 1   COLUMN 32 PIC 9(05) USING CODIGO
             COLOR 6 HIGHLIGHT.
      *
       01 SCREEN-EDIT-DDS.
           05 LINE WRK-LINE COLUMN 2  VALUE ' '.
           05 LINE PLUS 1   COLUMN 2  VALUE ' '.
           05 LINE PLUS 1   COLUMN 32 PIC X(30) USING TITULO
              COLOR 7 HIGHLIGHT.
           05 LINE PLUS 1   COLUMN 32 PIC 9(03) USING DURACAO 
              COLOR 7 HIGHLIGHT.
           05 LINE PLUS 1   COLUMN 32 PIC X(15) USING DISTRIB 
              COLOR 7 HIGHLIGHT.
           05 LINE PLUS 1   COLUMN 32 PIC 9(02) USING NOTA 
              COLOR 7 HIGHLIGHT.
           05 LINE PLUS 1   COLUMN 32 PIC X(02) USING GENERO 
              COLOR 7 HIGHLIGHT.

      **
      * COPY BOOK DESCRIPTION DATA FILES SEQUENTIAL
      **
           SELECT MOVIES
              ASSIGN       TO 'MOVIES.dat'
              ORGANIZATION IS INDEXED
              ACCESS MODE  IS DYNAMIC *> DYNAMIC
              FILE STATUS  IS FS-MOVIES
              RECORD KEY   IS CODIGO.

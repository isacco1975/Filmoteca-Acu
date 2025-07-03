      **
      * COPY BOOK DESCRIPTION DATA FILES GENRES
      **
           SELECT GENRES
              ASSIGN       TO 'GENRE.dat'
              ORGANIZATION IS LINE SEQUENTIAL
              ACCESS MODE  IS SEQUENTIAL
              FILE STATUS  IS FS-GENRES.

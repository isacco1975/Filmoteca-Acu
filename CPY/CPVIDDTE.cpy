      **
      * COPY BOOK DATE/TIME PROCEDURE
      **
           MOVE FUNCTION CURRENT-DATE TO WRK-NOW.
      *
           MOVE WRK-NOW(01:04) TO WRK-YEAR.
           MOVE WRK-NOW(05:02) TO WRK-MONTH.
           MOVE WRK-NOW(07:02) TO WRK-DAY.
           MOVE WRK-NOW(09:02) TO WRK-HOUR.
           MOVE WRK-NOW(11:02) TO WRK-MINUTE.

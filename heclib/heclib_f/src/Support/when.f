      SUBROUTINE WHEN (CDAT, CTIM)
C
C       This subroutine, when called, returns the current date and
C  time.  The format is shown in the following example:
C            Date: '05MAR85 '
C            Time: '07:35:10'
C
C       Output:  CDAT  (--- Date)
C                CTIM  (--- Time)
C
C
      CHARACTER CDAT*(*), CTIM*(*)
      CHARACTER CD*30, CT*20
C
C     CDAT(1:8) = '01JAN85 '                                            D
C     CTIM(1:8) = '12:34:56'                                            D
C

      CDAT = ' '
      CTIM = ' '

      CALL getcurrentdatetimestring(CD, 30, 114, CT, 20)
      CDAT = CD
      CTIM = CT(1:8)
C
      RETURN
      END


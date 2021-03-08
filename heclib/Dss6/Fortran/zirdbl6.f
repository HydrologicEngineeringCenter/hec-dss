      SUBROUTINE zirdbl6 (IFROM, ITO, LTODBL)
C
C
C     Convert a single value to a double value, or visa-versa
C     Where the values are passed in the form of an
C     integer array.
C     If LTODBL is true, convert the single IFROM into the
C     double ITO
C     If LTODBL is false, convert the double IFROM into the
C     single ITO
C
      INTEGER IFROM(*), ITO(*)
      LOGICAL LTODBL
C
      DOUBLEPRECISION DEQUIV
      REAL            SEQUIV, TEMP
      INTEGER         IEQUIV(2)
      EQUIVALENCE (IEQUIV, DEQUIV), (IEQUIV, SEQUIV)
C
C
      IF (LTODBL) THEN
         IEQUIV(1) = IFROM(1)
         TEMP = SEQUIV
         DEQUIV = DBLE (TEMP)
         ITO(1) = IEQUIV(1)
         ITO(2) = IEQUIV(2)
      ELSE
         IEQUIV(1) = IFROM(1)
         IEQUIV(2) = IFROM(2)
         TEMP = SNGL (DEQUIV)
         SEQUIV = TEMP
         ITO(1) = IEQUIV(1)
      ENDIF
C
      RETURN
      END


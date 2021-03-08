      SUBROUTINE zckts6(IFLTAB, CPATH, IDTYPE, LEMPTY)
C
C
C     Look for empty DSS records
C     If empty (no data), delete the record
C     Written by Bill Charley
C
C
      INTEGER IFLTAB(*), ISTAT, IOFSET
      CHARACTER CPATH*(*), CUNITS*12, CTYPE*12
      LOGICAL LDELETE, LEMPTY
      logical external lismissingf
C
      integer nvalues
      PARAMETER (NVALUES=1500)
      REAL VALUES(NVALUES)
      INTEGER ITIMES(NVALUES)
C
C
      LEMPTY = .FALSE.
C
C     Read the record and see if there is data in it.
      IF (IDTYPE.LT.110) THEN
         LDELETE = .FALSE.
         NVALS = NVALUES
         ISTAT = -1
         CALL zrrts(IFLTAB, CPATH, ' ', ' ', NVALS, VALUES,
     *   CUNITS, CTYPE, IOFSET, ISTAT)
         IF (ISTAT.EQ.4) LEMPTY = .TRUE.
      ELSE
        ISTIME = -2
        CALL zrits (IFLTAB, CPATH, JULS, ISTIME, JULE, IETIME,
     *  ITIMES, VALUES, NVALUES, NVALS, IBDATE, CUNITS, CTYPE, ISTAT)
        LEMPTY = .TRUE.
        DO 120, I=1,NVALS
           IF (lismissingf(VALUES(I)).eq.false) LEMPTY = .FALSE.
 120    CONTINUE
      ENDIF
C
 800  CONTINUE
      RETURN
C
C
      END


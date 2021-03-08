      SUBROUTINE zsrtsc6 ( IFLTAB, CPATH, CDATE, CTIME, NVALS,
     * LDOUBLE, VALUES, DVALUES, JQUAL, LQUAL, CUNITS, CTYPE,
     * COORDS, NCOORDS, ICDESC, NCDESC, CSUPP, ITZONE,
     * CTZONE, IPLAN, JCOMP, BASEV, LBASEV, LDHIGH, NPREC, ISTAT)
C
C
C     Z-Store Regular interval Time-Series data
C     Data is stored according to the
C     time window set from CDATE/CTIME and NVALS.
C

C     Argument Dimensions
      CHARACTER CPATH*(*), CTYPE*(*), CUNITS*(*), CDATE*(*), CTIME*(*)
      CHARACTER CTZONE*(*), CSUPP*(*)
      INTEGER IFLTAB(*), JQUAL(*)
      INTEGER IPLAN, ISTAT, NVALS, ITZONE, MAXHEAD, NUHEAD
      LOGICAL LQUAL, LBASEV, LDHIGH, LDOUBLE
C
      REAL VALUES(*)
C
      DOUBLE PRECISION DVALUES(*), COORDS(*)
      INTEGER NCOORDS, ICDESC(*), NCDESC
C
      INCLUDE 'zdssts.h'
C
C
      CWTZONE = CTZONE
      IWTZONE = ITZONE
C
      NUHEAD = 0
      JCOMP = 0
      NPREC = -1
      if (len(csupp).gt.1) then
          CALL CHRLNB(CSUPP, NSUPP)
          IF (NSUPP.GT.1) THEN
             NUHEAD = ((NSUPP - 1) / 4) + 1
             NUHEAD = MIN(NUHEAD, NIBUFF)
             MAXHEAD = NUHEAD * 4
	       IF (NSUPP.GE.MAXHEAD) THEN
                CALL CH2HOL(CSUPP, INTBUF, NUHEAD)
             ELSE
C               Be sure the last word is blank filled
                CALL CH2HOL('    ', INTBUF(NUHEAD), 1)
                CALL CHRHOL(CSUPP, 1, NSUPP, INTBUF, 1)
             ENDIF
          endif
      ENDIF
C
C
      CALL zsrtsi6 (IFLTAB, CPATH, CDATE, CTIME, NVALS,
     * LDOUBLE, VALUES, DVALUES, JQUAL, LQUAL, CUNITS, CTYPE,
     * INTBUF, NUHEAD, COORDS, NCOORDS, ICDESC, NCDESC,
     * IPLAN, JCOMP, BASEV, LBASEV, LDHIGH, NPREC, ISTAT)
C
      RETURN
C
      END


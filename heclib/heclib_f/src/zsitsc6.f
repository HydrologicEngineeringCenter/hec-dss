      SUBROUTINE zsitsc6(IFLTAB, CPATH, ITIMES,  VALUES, DVALUES,
     * LDOUBLE, NVALUE, IBDATE, JQUAL, LSQUAL, CUNITS, CTYPE,
     * COORDS, NCOORDS, ICDESC, NCDESC, CSUPP, ITZONE,
     * CTZONE, INFLAG, ISTAT)
C
C
C     Z - Store - IRregular - Time - Series  data into DSS database
C     (Complete version)
C
C     Written by Bill Charley
C
C
C     --- Arguments ---
C     Input:
C         IFLTAB -  File table array used in zopen6 CALL (DIM to 1200)
C         CPATH  - Pathname
C         VALUES - The data values to be stored by the subroutine.
C         ITIMES - An array containing the minutes of each data value,
C                  from the base date, IBDATE
C         NVALS -  Number if date/value pairs to store.
C         IBDATE -  Base date - real julian date of starting time; add
C                  ITIMES(I) to this to get the date for (I).
C         CUNITS - Units of data, CHARACTER*8
C         CTYPE -  Type of data, CHARACTER*8
C         INFLAG - Flag to indicate whether to replace or merge new data
C                  with old data. Replace for editing/changing data,
C                  merge for adding data.
C                  = 0  FOR MERGE
C                  = 1  FOR REPLACE
C
C         ISTAT -  Status parameter on operation
C                  = 0 IF OK
C                  = 4 IF NO DATA FOUND
C                  .GE. 10 IF A 'FATAL' ERROR OCCURED
C
C
      INTEGER IFLTAB(*), JQUAL(*), ITIMES(*)
      REAL VALUES(*)
      CHARACTER CPATH*(*), CUNITS*(*), CTYPE*(*)
      CHARACTER CTZONE*(*), CSUPP*(*)
      LOGICAL LSQUAL, LDOUBLE
      INTEGER NVALUE, IBDATE, NSUPP, ITZONE
      INTEGER INFLAG, ISTAT, MAXHEAD, NUHEAD
      DOUBLE PRECISION DVALUES(*), COORDS(*)
      INTEGER NCOORDS, ICDESC(*), NCDESC
C
      INCLUDE 'zdssts.h'
C
      INTEGER IUHEAD(NIBUFF)
C
C
      CWTZONE = CTZONE
      IWTZONE = ITZONE
C
C
      NUHEAD = 0
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
      CALL zsitsi6(IFLTAB, CPATH, ITIMES, VALUES, DVALUES,
     * LDOUBLE, NVALUE, IBDATE, JQUAL, LSQUAL, CUNITS, CTYPE,
     * IUHEAD, NUHEAD, COORDS, NCOORDS, ICDESC, NCDESC, INFLAG, ISTAT)
C
      RETURN
C
      END


      SUBROUTINE zsitsx (IFLTAB, CPATH, ITIMES, VALUES, NVALUE, IBDATE,
     * JQUAL, LSQUAL, CUNITS, CTYPE, IUHEAD, NUHEAD, INFLAG, ISTAT)
C
      implicit none
      integer nvalue,ibdate,nuhead,inflag,istat
C
C     Z - Store - IRregular - Time - Series  data into DSS database
C     (Extended version)
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
      INTEGER IFLTAB(*), JQUAL(*), ITIMES(*), IUHEAD(*)
      REAL VALUES(*)
      CHARACTER CPATH*(*), CUNITS*(*), CTYPE*(*)
      LOGICAL LSQUAL
C
      DOUBLE PRECISION DVALUES(1), COORDS(1)
      INTEGER NCOORDS, ICDESC(1), NCDESC
C
      NCOORDS = 0
      NCDESC = 0
C
C
      CALL zsitsi(IFLTAB, CPATH, ITIMES, VALUES, DVALUES,
     * .FALSE., NVALUE, IBDATE, JQUAL, LSQUAL, CUNITS, CTYPE,
     * IUHEAD, NUHEAD, COORDS, NCOORDS, ICDESC, NCDESC, INFLAG, ISTAT)
C
      RETURN
C
C
      END


      SUBROUTINE ztssrch (IFLTAB, CINPATH, IUNIT, COUTPATH, NFOUND)
C
      implicit none
      integer zdssversion
C
C     Time Series Search.
C     Given a DSS file IFLTAB and a time series pathname,
C     search the database for data sets with the same pathname,
C     except for the "D" (Date) part.  Either the first pathname
C     found (same except for the D part) can be returned (FAST),
C     or a list of all pathnames that match can be written to
C     a file (complete but slow, depending on the file size).
C     This routine is typically used when you have parts of the pathname
C     but you do not know when the data occurs.
C
C     IFLTAB - (input) Array from the zopen6 call.
C     CINPATH - (input) Character string containing the pathname to
C               search.  The D part is ignored (usually ts blank).
C     IUNIT - (input)  If a complete list is to be found, this should
C               be the unit number of the file to write the list to.
C               If only the first pathname is to be returned, set this
C               to zero.
C     COUTPATH - (output) Character string containing the pathname
C               of the first matching path, if IUNIT is zero.
C               If IUNIT is positive, this will be blank.
C     NFOUND - (output)  The number of pathnames found.  -1 if an error
C               occurred, zero if none.
C
C
C     Argument Dimensions
      CHARACTER CINPATH*(*), COUTPATH*(*)
      INTEGER IFLTAB(*)
      INTEGER IUNIT, NFOUND
C
      IF (zdssVersion(IFLTAB).EQ.6) THEN
         CALL ztssrch6 (IFLTAB, CINPATH, IUNIT, COUTPATH, NFOUND)
      ELSE
         CALL ztssrch7 (IFLTAB, CINPATH, IUNIT, COUTPATH, NFOUND)
      ENDIF
C
      RETURN
      END


      SUBROUTINE zrtxta (IFLTAB, CPATH, CARRAY, KARRAY, NLINES,
     * IUHEAD, KUHEAD, NUHEAD, ISTAT)
C
      implicit none
C
C     Retrieve text data and place into character array CARRAY.
C     CARRAY should be large enough to hold the text data
C     (Its dimension is KARRAY)
C     For writing text data to a unit (e.g., file) use zrtext6
C
C     Written by Bill Charley at HEC, 1990
C     Last modified, March 1995.  Added call to zrdbuf6
C
C
      INTEGER IFLTAB(*), IUHEAD(*),zdssVersion,KARRAY
      CHARACTER CARRAY(KARRAY)*(*)
      CHARACTER CPATH*(*)
      INTEGER  NLINES, KUHEAD, NUHEAD, ISTAT
C

C
C
      IF (zdssVersion(IFLTAB).EQ.6) THEN
         CALL zrtxta6 (IFLTAB, CPATH, CARRAY, KARRAY, NLINES,
     *                 IUHEAD, KUHEAD, NUHEAD, ISTAT)
      ELSE
         CALL ztextretrievearray (IFLTAB, CPATH, CARRAY, KARRAY, NLINES,
     *                 IUHEAD, KUHEAD, NUHEAD, ISTAT)
      ENDIF
C
      END


      SUBROUTINE ZRTXTS (IFLTAB, CPATH, CSTRING, KSTRING,
     * NSTRING, IUHEAD, KUHEAD, NUHEAD, ISTAT)
C
      implicit none
C
C     Retrieve text data and place into the character string CSTRING.
C     CSTRING should be large enough to hold the text data
C     (Its dimension is KSTRING)
C     For writing text data to a unit (e.g., file) use ZRTEXT
C
C     Written by Bill Charley at HEC, 2000
C
C
      INTEGER IFLTAB(*), IUHEAD(*),zdssVersion
      INTEGER KSTRING, NSTRING, KUHEAD, NUHEAD, ISTAT
C
      CHARACTER CSTRING*(*)
      CHARACTER CPATH*(*)
C
C
      IF (zdssVersion(IFLTAB).EQ.6) THEN
         CALL ZRTXTS6 (IFLTAB, CPATH, CSTRING, KSTRING,
     *          NSTRING, IUHEAD, KUHEAD, NUHEAD, ISTAT)
      ELSE
         CALL ztextretrievestring (IFLTAB, CPATH, CSTRING, KSTRING,
     *          NSTRING, IUHEAD, KUHEAD, NUHEAD, ISTAT)
      ENDIF
C
      END


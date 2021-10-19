      SUBROUTINE zselca(CPATH, NPATH, CCTAG, CCPROG, CCDATE, CCTIME,
     * CDTYPE, IDTYPE, IRVERS, NDATA, NHEAD, JNPATH, LSELCA, LMATCH,
     * LCDCAT, LERR)
C
C
C     Selective Catalog.  Used by zcat6, which is called by zcatlg6
C     If the catalog is to be sorted, or pathnames selectively chosen,
C     this subroutine Unforms them, then either writes them to a scratch
C     file for sorting, or to the catalog file (if not to be sorted)
C
C     Written by Bill Charley at HEC, 1988.
C
      CHARACTER CCTAG*(*), CCPROG*(*), CCDATE*(*), CCTIME*(*)
      CHARACTER CDTYPE*(*), CPATH*(*)
      CHARACTER CPATH2*392, CPATH3*392, CTPATH*392, CLINE*400
      LOGICAL LSELCA, LMATCH, LCDCAT, LERR
      INTEGER IBPART(6), IEPART(6), ILPART(6)
C
C
C
      INCLUDE 'zdssca.h'
C
      INCLUDE 'zdsscc.h'
C
      INCLUDE 'zdsscm.h'
C
      INCLUDE 'zdssmz.h'
C
C
C
      IF (MLEVEL.GE.12) WRITE (MUNIT,20) NPATH, CPATH(1:NPATH)
 20   FORMAT (T8,'-----DSS---Debug:  Enter zselca,  NPATH:',I5,/,
     * T13,'Path: ',A)
      LERR = .FALSE.
C
      IF (NDATA.GT.9999) NDATA = 9999
C
C     Scan (index) for a string in the path
      IF (NSINDEX.GT.0) THEN
         IF (INDEX(CPATH(1:NPATH), CSINDEX(1:NSINDEX)).EQ.0) GO TO 800
      ENDIF
C
      IF (LSORT.OR.LSELCA.OR.LTWCAT) THEN
C     Unform the Pathname
      CALL zupath (CPATH(1:NPATH), IBPART, IEPART, ILPART, ISTAT)
      DO 30 I=1,6
      IF (IEPART(I).LT.IBPART(I)) IEPART(I) = IBPART(I)
 30   CONTINUE
C
C     If a condensed catalog is being generated, get the maximum
C     lengths of each part.
      IF (LCDCAT) THEN
      DO 40 I=1,6
      IF (MAXPRT(I).LT.ILPART(I) ) MAXPRT(I) = ILPART(I)
 40   CONTINUE
      CALL CHRLNB (CCTAG(1:8), ILAST)
      IF (MAXPRT(7).LT.ILAST) MAXPRT(7) = ILAST
      ENDIF
      ENDIF
C
C     If the selective catalog feature is used, check if the
C     proper parts match (or don't, as the case may be)
C
      IF (LSELCA) THEN
      CALL zmatca(CPATH,IBPART, IEPART, ILPART, CCDATE, CCPROG, LMATCH)
      IF (.NOT.LMATCH) GO TO 800
      ELSE
      LMATCH = .TRUE.
      ENDIF
C
      NOPTHS = NOPTHS + 1
      CTPATH = CPATH
C
C     Check to see if the data is time series.  If so,
C     use a julian date for the D part, and a time in
C     minutes for the E part (or similar number for Irregular TS)
C
      IF (LSORT) THEN
C
C     If we don't know the data type, we might try time series
C     (most common).
      IF (IDTYPE.EQ.-1) THEN
         IF (ILPART(4).EQ.9) IDTYPE = 100
      ENDIF
C
      IF ((IDTYPE.GE.100).AND.(IDTYPE.LT.200)) THEN
         JST = 1
         CALL zgintl6 (INTL, CPATH(IBPART(5):IEPART(5)), NV, JST)
         CALL DATJUL (CTPATH(IBPART(4):IEPART(4)), JUL, IERR)
         IF ((IERR.EQ.0).AND.(JST.NE.-1)) THEN
            JUL = JUL + 100000
            WRITE (CTPATH(IBPART(4):IEPART(4)), 100) JUL
            IBPART(5) = IBPART(5) - 1
            IEPART(5) = IEPART(5) + 1
C           Be sure INTL is positive (irregular int t.s. is negative)
            INTL = INTL + 100
            WRITE (CTPATH(IBPART(5):IEPART(5)), 100) INTL
 100        FORMAT (I6.6)
         ENDIF
      ELSE IF ((IDTYPE.GE.400).AND.(IDTYPE.LT.500)) THEN
C        Gridded data
         DO 140 I=4,5
C           Convert the dates to day count
            CALL DATJUL (CTPATH(IBPART(I):IBPART(I)+8), JUL, IERR)
            IF (IERR.EQ.0) THEN
              JUL = JUL + 100000
               WRITE (CTPATH(IBPART(I):IBPART(I)+8), 120) JUL
 120           FORMAT(I9.9)
            ENDIF
 140     CONTINUE
      ENDIF
C
      ENDIF
C
C
C
C     Write the pathname parts for sorting, and the direct
C     access file record number
      IF (LSORT) THEN
C     Write pathname parts for sorting
      DO 160 I=1,6
      CLINE(IBPMAX(I):IEPMAX(I)) =
     * CTPATH(IBPART(IORDER(I)):IEPART(IORDER(I)))
 160  CONTINUE
      WRITE (ISUNIT(1), 180, ERR=900) CLINE(1:MTOTAL), NOPTHS
 180  FORMAT (A,I7)   !**********************
      ENDIF
C
C     Extended Form of Catalog
      IF (LEXTND) THEN
C
      IF (LSORT) THEN
C     Write the catalog line to the temporary direct access file
      WRITE (CLINE, 200, ERR=900)
     * CCTAG, CCPROG, CCDATE, CCTIME, CDTYPE, IRVERS, NDATA,
     * CPATH(1:NPATH)
 200  FORMAT (A8, 2X, A6, 2X, A7, 1X, A5, 2X, A3, I4, I5, 3X, A)
      WRITE (ISUNIT(3), REC=NOPTHS, ERR=900) CLINE(1:400)
C
C     Write the catalog line to sort input file
C
      ELSE
C     Write info and Pathname directly to the catalog
      IF (NOPTHS.GE.1000000) THEN
      WRITE (JCUNIT,221,ERR=900) NOPTHS, CCTAG, CCPROG,
     * CCDATE, CCTIME, CDTYPE, IRVERS, NDATA, CPATH(1:NPATH)
 221  FORMAT (I7, 1X, A8, 2X, A6, 2X, A7, 1X, A5, 2X, A3, I4, I5, 3X, A)
      ELSE
      WRITE (JCUNIT,220,ERR=900) NOPTHS, CCTAG, CCPROG,
     * CCDATE, CCTIME, CDTYPE, IRVERS, NDATA, CPATH(1:NPATH)
 220  FORMAT (I6, 2X, A8, 2X, A6, 2X, A7, 1X, A5, 2X, A3, I4, I5, 3X, A)
      ENDIF
      IF (LMAP) WRITE (MAPUNT,240,ERR=900) CPATH(1:NPATH)
      ENDIF
 240  FORMAT (A)
C
      ELSE
C
C     Short Form of Catalog
C
      IF (LSORT) THEN
C     Write pathname parts and pathname for sorting
      WRITE (CLINE, 260, ERR=900) CCTAG, CPATH(1:NPATH)
 260  FORMAT (A8,2X,A)
      WRITE (ISUNIT(3), REC=NOPTHS, ERR=900) CLINE(1:400)
C
C
C     No sort, write directly to the catalog
      ELSE
C
C     If the calling program has a time window set (LTWCAT),
C     keep only one pathname for which have all
C     but the date part different
      IF (LTWCAT) THEN
      J = 1
      CALL zgintl6 (INTL, CPATH(IBPART(5):IEPART(5)), NV, J)
      IF ((J.NE.0).AND.(J.NE.1))  GO TO 300
C
      CPATH2 = ' '
      WRITE (CPATH2(1:2), '(I2)') NPATH
      CPATH2(3:) = CPATH(1:IBPART(4)-1) // CPATH(IBPART(5):NPATH)
C
      IF (NTWPAT.GT.0) THEN
      DO 280 I=1,NTWPAT
      READ (UNIT=68,REC=I) CPATH3
      READ (CPATH3(1:2), '(I2)') N
      IF (N.EQ.NPATH) THEN
      IF (CPATH2.EQ.CPATH3) GO TO 800
      ENDIF
 280  CONTINUE
      ENDIF
      NTWPAT = NTWPAT + 1
      WRITE (UNIT=68,REC=NTWPAT) CPATH2
      ENDIF
C
C
 300  CONTINUE
      JP = NOPTHS
C     Use JNPATH as counter if from subroutine zrdcat6
      IF (JNUNIT.NE.0) JP = JNPATH
      IF (JCUNIT.EQ.0) GO TO 800
      IF (JP.GE.1000000) THEN
      WRITE (JCUNIT, 321, ERR=900) JP, CCTAG, CPATH(1:NPATH)
 321  FORMAT (I7,1X,A,2X,A)
      ELSE
      WRITE (JCUNIT, 320, ERR=900) JP, CCTAG, CPATH(1:NPATH)
 320  FORMAT (I6,2X,A,2X,A)
      ENDIF
      IF (LMAP) WRITE (MAPUNT, 340, ERR=900) CPATH(1:NPATH)
 340  FORMAT (A)
      ENDIF
C
      ENDIF
C
C
C
 800  CONTINUE
      IF (MLEVEL.GE.12) WRITE (MUNIT,820)
 820  FORMAT (T8,'-----DSS---Debug:  Exit ZSELCA')
      RETURN
C
C
C     Error during write
 900  CONTINUE
      WRITE (MUNIT,901)
 901  FORMAT (/' **** zcatlg6 - Error During Catalog Write ****',/,
     * ' Unable to Create Catalog',/)
      LERR = .TRUE.
      GO TO 800
C
      END


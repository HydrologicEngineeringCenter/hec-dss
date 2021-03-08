      SUBROUTINE zcatit (IFLTAB, IUNIT, LSELCA, LCDCAT, CINSTR, LERR)
C
C
C     Write the Catalog Title Lines to the Catalog File
C
C     Written by Bill Charley at HEC, 1988.
C
C
      INTEGER IFLTAB(*)
      LOGICAL LCDCAT, LSELCA, LERR
      CHARACTER CINSTR*(*)
C
      CHARACTER CNAME*64, CATDAT*15, CATIME*4, CFILDA*12
      CHARACTER CLINE*392
      CHARACTER CVERS*4, CFVERS*4, CSORT*6, CALPHA*6
C
      INCLUDE 'zdssca.h'
C
C
      LERR = .FALSE.
C
      CNAME = ' '
      CALL zinqir (IFLTAB, 'NAME',   CNAME,    ILARGE)
      CALL zinqir (IFLTAB, 'FDATE', CFILDA    ,ILARGE)
      CALL zinqir (IFLTAB, 'NREC',   CSORT,      NREC)
      CALL zinqir (IFLTAB, 'FVERS', CFVERS,    ILARGE)
      CALL zinqir (IFLTAB, 'VERS',   CVERS,    ILARGE)
      IF (NREC.LE.0) GO TO 900
      CALL CHRLNB (CNAME, NNAME)
      IF (NNAME.EQ.0) NNAME = 1
C
      CALL CURTIM (JUL, ITIME)
      I = M2IHM (ITIME, CATIME)
      IF (CATIME(1:1).EQ.'0') CATIME(1:1) = ' '
      CALL JULDAT (JUL, 1, CATDAT, NCDATE)
C
      CALL DATJUL (CFILDA, JUL, IERR)
      IF (IERR.EQ.0) THEN
      CFILDA = ' '
      CALL JULDAT (JUL, 1, CFILDA, NFILDA)
      ELSE
      NFILDA = 8
      ENDIF
C
      IF (LSORT) THEN
      CALPHA = 'ABCDEF'
      DO 20 I=1,6
      J = IORDER(I)
      CSORT(I:I) = CALPHA(J:J)
 20   CONTINUE
      ENDIF
C
C
      REWIND (IUNIT)
C
      IF (LCDCAT) THEN
      WRITE (IUNIT, 100, ERR=900) 'Condensed', CNAME(1:NNAME)
      ELSE IF (LSELCA) THEN
      WRITE (IUNIT, 100, ERR=900) 'Partial ', CNAME(1:NNAME)
      ELSE
      WRITE (IUNIT, 100, ERR=900) 'Complete', CNAME(1:NNAME)
      ENDIF
 100  FORMAT (/,T6,'HECDSS ',A,' Catalog of Record Pathnames in File ',
     * A)
C
      WRITE (IUNIT, 140, ERR=900) CATDAT(1:NCDATE), CATIME(1:2),
     * CATIME(3:4), CFILDA(1:NFILDA)
 140  FORMAT (/,T6,'Catalog Created on ',A,' at ',A,':',A,
     * T50,'File Created on ',A)
C
      WRITE (IUNIT, 160, ERR=900) NREC, CVERS, CFVERS
 160  FORMAT (T6,'Number of Records:',I7,T50,'DSS Version ',A,
     *        ',  File ',A)
C
      CALL CHRFLB (CINSTR, JINSTR, NINSTR)
      IF (NINSTR.GT.0) THEN
      IF (LSORT) THEN
      WRITE (IUNIT, 200, ERR=900) CSORT, CINSTR(JINSTR:NINSTR)
 200  FORMAT (T6,'Sort Order: ',A,T30,A)
      ELSE
      WRITE (IUNIT, 220, ERR=900) CINSTR(JINSTR:NINSTR)
 220  FORMAT (T6,'Pathnames Not Sorted',T30,A)
      ENDIF
      ELSE
      IF (LSORT) THEN
      WRITE (IUNIT, 240, ERR=900) CSORT
 240  FORMAT (T6,'Sort Order: ',A)
      ELSE
      WRITE (IUNIT, 260, ERR=900)
 260  FORMAT (T6,'Pathnames Not Sorted')
      ENDIF
      ENDIF
C
      IF (LCDCAT) THEN
      CLINE = 'Tag'
      IEND = MAXPRT(7)
      DO 270 I=1,6
      J = IORDER(I)
      IBEG = IEND + 3
      IEND = IBEG + MAXPRT(J) - 1
      CLINE(IBEG:IEND+1) = CSORT(I:I) // ' Part'
 270  CONTINUE
C
      CALL CHRLNB(CLINE,N)
      WRITE (IUNIT, 280, ERR=900) CLINE(1:N)
 280  FORMAT (/,1X,A,/)
C
      ELSE IF (LEXTND) THEN
      WRITE (IUNIT, 300, ERR=900)
 300  FORMAT (/,T3,'Ref.',T28,'Last Written',/,
     * T2,'Number',T11,'Tag',T19,'Program',T29,'Date',T35,'Time',
     * T41,'Type',T46,'Vers',T51,'Data',T63,'Record Pathname',/)
      ELSE
      WRITE (IUNIT, 320, ERR=900)
 320  FORMAT (/,T3,'Ref.',/,
     * T2,'Number',T11,'Tag',T25,'Record Pathname',/)
      ENDIF
C
C
 800  CONTINUE
      RETURN
C
 900  CONTINUE
      LERR = .TRUE.
      GO TO 800
C
      END


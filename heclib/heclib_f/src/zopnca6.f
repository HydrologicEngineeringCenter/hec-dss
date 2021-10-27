      SUBROUTINE zopnca (CDSSFI, ICUNIT, LGENCA, LOPNCA,
     * LCATCA, ICDUNT, LGENCD, LOPNCD, LCATCD, NRECS)
C
C     Generates a Catalog file name (from a DSS file name),
C     then opens the file.  The file is checked to see if
C     it contains a Catalog.
C
C     This routine is for use by those programs accessing
C     the Catalog (see zcatlg6)
C
C     The catalog file name is the name of the DSS file
C     with a "C" added on at the end.  On the PC, the
C     "C" replaces the last character.  E.g.,
C       DSS: datab.dss   Catalog: datab.dsc
C
C
C     Written by Bill Charley at HEC, 1990.
C
C
      CHARACTER CDSSFI*(*), CCHAR*2, CATFIL*256, CLINE*60, CCOND*11
      LOGICAL LOPEN, LCATLG, LEXIST, LOPNCD, LCATCD, LCREAT
      LOGICAL LGENCA, LGENCD, LOPNCA, LCATCA, LGEN
C
      INCLUDE 'zdssmz.h'
C
      DATA CCOND /' Condensed '/
C
C
C
      IF (MLEVEL.GE.11) THEN
      WRITE (MUNIT,20) CDSSFI, ICUNIT, ICDUNT, LGENCA, LGENCD
 20   FORMAT (T6,'-----DSS---Debug:  Enter zopnca;  DSS File: ',A,/,
     * T17,'Catalog Unit:',I4,' Condensed Unit:',I4,/,
     * T17,'Gen Catalog :',L4,' Gen Condensed: ',L4)
      ENDIF
C
      IF (LOPNCA) THEN
      CLOSE (UNIT=ICUNIT)
      LOPEN = .FALSE.
      ENDIF
C
      IF (LOPNCD) THEN
      CLOSE (UNIT=ICDUNT)
      LOPNCD = .FALSE.
      ENDIF
C
      LCATCA = .FALSE.
      LCATCD = .FALSE.
      NS = NRECS
      NRECS = -1
      CCHAR = 'cd'
C
C
      DO 200 J=1,2
C
      CATFIL = CDSSFI
      LGEN = .FALSE.
      LOPEN = .FALSE.
      LCATLG = .FALSE.
C
      IF (J.EQ.1) THEN
      IUNIT = ICUNIT
      LCREAT = LGENCA
      NCOND = 1
      ELSE
      IUNIT = ICDUNT
      LCREAT = LGENCD
      NCOND = 11
      ENDIF
      IF (IUNIT.LE.0) GO TO 180
C
      CALL CHRLNB (CATFIL, ILAST)
      ILAST = ILAST + 1
      CATFIL(ILAST:ILAST) = CCHAR(J:J)
C     I = INDEX(CATFIL,'.')
C     IF (I.EQ.0) I = ILAST+1
C     CATFIL(I:I+3) = '.DS' // CCHAR(J:J)
C
C
C     See if the file exists
      CALL CHRLNB (CATFIL, ILAST)
      INQUIRE ( FILE=CATFIL(1:ILAST), EXIST=LEXIST)
      IF (MLEVEL.GE.12) WRITE (MUNIT, 25) LEXIST, CATFIL(1:ILAST)
 25   FORMAT (T17,'File Exists: ',L1,'  Name: -',A,'-')
C
      IF (.NOT.LEXIST) THEN
         CATFIL(ILAST-1:ILAST-1) = CATFIL(ILAST:ILAST)
         ILAST = ILAST -1
         INQUIRE ( FILE=CATFIL(1:ILAST), EXIST=LEXIST)
      ENDIF
C
C     If it exists, open it
      IF (LEXIST) THEN
      OPEN ( UNIT=IUNIT, FILE=CATFIL(1:ILAST), IOSTAT=IERR)
C     OPEN ( UNIT=IUNIT, FILE=CATFIL(1:ILAST), IOSTAT=IERR,
C    * SHARE='DENYNONE')
C     Test for read access only
      INQUIRE (UNIT=IUNIT, OPENED=LOPEN)
      IF (.NOT.LOPEN) THEN
         OPEN ( UNIT=IUNIT, FILE=CATFIL(1:ILAST),
     *   IOSTAT=IERR, ACTION='READ')
         INQUIRE (UNIT=IUNIT, OPENED=LOPEN)
      ENDIF
C
      IF ((IERR.NE.0).OR.(.NOT.LOPEN)) THEN
      IF (MLEVEL.GE.1) WRITE (MUNIT,30) CATFIL(1:ILAST), CCOND(1:NCOND),
     * IERR
 30   FORMAT (/,' **** zopnca - ERROR:  Unable to Access the Catalog',
     * ' File ****',/,' Catalog File: ',A,',  Condensed Catalog: ',A,
     * /,' Error: ',I6)
      IF (.NOT.LCREAT) GO TO 180
      GO TO 180
      ELSE
      LOPEN = .TRUE.
C
C     Check to see if it is already cataloged
      DO 80 K=1,20
      READ ( IUNIT, 40, END=180) CLINE
 40   FORMAT (A)
      CALL UPCASE (CLINE)
      I = INDEX (CLINE,'HECDSS')
      IF (I.GT.0) THEN
      M = INDEX (CLINE,'CATALOG')
      IF (M.GT.0) THEN
C     LCATLG = .TRUE.
C
      DO 60 N=1,10
      READ ( IUNIT, 40, END=180) CLINE
      CALL UPCASE (CLINE)
C     Check version 6 style catalog
      I = INDEX (CLINE,'OF RECORDS:')
      IF (I.GT.0) THEN
      M = I + 11
      NREC = INTCHR (CLINE(M:M+6))
      IF (NREC.EQ.-1) GO TO 60
C     Look for a pathanme slash within the next few lines
      DO 50 I=1,10
      READ (IUNIT, 40, END=180) CLINE(1:30)
      CALL UPCASE (CLINE(1:30))
      IF (INDEX(CLINE(1:30),'TAG').GT.0) THEN
      LCATLG = .TRUE.
      GO TO 180
      ENDIF
 50   CONTINUE
      GO TO 180
      ENDIF
C     Check for version 4 style of catalog
      I = INDEX (CLINE,'OF RECORDS =')
      IF (I.GT.0) THEN
      M = I + 12
      NREC = INTCHR (CLINE(M:M+4))
      IF (NREC.EQ.-1) GO TO 60
      DO 55 I=1,10
      READ (IUNIT, 40, END=180) CLINE
      IF (INDEX(CLINE,'/').GT.0) THEN
      LCATLG = .TRUE.
      GO TO 180
      ENDIF
 55   CONTINUE
      GO TO 180
      ENDIF
 60   CONTINUE
C
      ENDIF
      ENDIF
 80   CONTINUE
      ENDIF
C
      ELSE
      IF (.NOT.LCREAT) GO TO 180
C     CREATE THE CATALOG FILE
      IF (MLEVEL.GE.2) WRITE (MUNIT,90) CCOND(1:NCOND), CATFIL(1:ILAST)
 90   FORMAT (A,'Catalog File Does Not Exist: ',A)
C
C     OPEN THE FILE
      OPEN ( UNIT=IUNIT, FILE=CATFIL(1:ILAST), IOSTAT=IERR)
C     OPEN ( UNIT=IUNIT, FILE=CATFIL(1:ILAST), IOSTAT=IERR,
C    * SHARE='DENYNONE')
      IF (IERR.NE.0) THEN
         IF (MLEVEL.GE.1) WRITE (MUNIT,120) CATFIL(1:ILAST), IERR
         LOPEN = .FALSE.
         IF (MLEVEL.GE.1) WRITE ( MUNIT, 120) CCOND(1:NCOND),
     *                    CATFIL(1:ILAST), IERR
 120     FORMAT ( ' --Unable to Create',A,'Catalog File: ',A,I8)
         GO TO 800
      ELSE
         IF (MLEVEL.GE.2) WRITE (MUNIT,100) CATFIL(1:ILAST)
 100     FORMAT ( ' Created Catalog File: ',A)
         LGEN = .TRUE.
      LOPEN = .TRUE.
      ENDIF
      ENDIF
C
C
      IF (IERR.NE.0) THEN
      IF (MLEVEL.GE.1) WRITE (MUNIT,120) CATFIL, IERR
      GO TO 800
      ELSE
      LOPEN = .TRUE.
      ENDIF
C
C
 180  CONTINUE
      IF (LOPEN) REWIND (UNIT=IUNIT)
      IF ((MLEVEL.GE.3).AND.(.NOT.LGEN).AND.LOPEN) THEN
      WRITE (MUNIT, 181) CCOND(1:NCOND), CATFIL(1:ILAST)
 181  FORMAT (A,'Catalog File: ',A)
      ENDIF
C
      IF (J.EQ.1) THEN
      LCATCA = LCATLG
      LOPNCA = LOPEN
      NRECS = NREC
      ELSE
      LCATCD = LCATLG
      LOPNCD = LOPEN
      ENDIF
C
 200  CONTINUE
C
C
 800  CONTINUE
      IF (MLEVEL.GE.11) WRITE (MUNIT,820) NRECS, LOPNCA, LCATCA,
     * LOPNCD, LCATCD
 820  FORMAT (T6,'-----DSS---Debug:  Exit zopnca;  NRECS:',I7,/,
     * T11,'Catalog File Opened, Cataloged:      ',2L4,/
     * T11,'Condensed File Opened, Condensed Cat:',2L4)
      RETURN
C
      END


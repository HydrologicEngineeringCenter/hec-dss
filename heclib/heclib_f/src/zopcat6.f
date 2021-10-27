      SUBROUTINE zopcat6 ( CDSSFI, CATFIL, ICUNIT, LOPEN, LCATLG,
     * LCREAT, NREC)
C
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
C     Written by Bill Charley at HEC, 1983.
C
C
      CHARACTER CDSSFI*(*), CATFIL*(*), CLINE*60
      LOGICAL LOPEN, LCATLG, LCREAT, LEXIST
C
      INCLUDE 'zdssmz.h'
C
C
      NREC = -1
      LCATLG = .FALSE.
      NCF = LEN ( CATFIL)
C
      IF (LOPEN) THEN
      CLOSE (UNIT=ICUNIT)
      LOPEN = .FALSE.
      ENDIF
C
      CATFIL = CDSSFI
      CALL CHRLNB (CATFIL, ILAST)
      I = INDEX(CATFIL,'.')
      IF (I.EQ.0) I = ILAST+1
      IF (I+3.LE.NCF) THEN
      CATFIL(I:I+3) = '.dsc'
      ENDIF
C
C
C     See if the file exists
      CALL CHRLNB (CATFIL, ILAST)
      INQUIRE ( FILE=CATFIL(1:ILAST), EXIST=LEXIST)
C
C     If it exists, open it
      IF (LEXIST) THEN
      OPEN ( UNIT=ICUNIT, FILE=CATFIL(1:ILAST), IOSTAT=IERR)
      IF (IERR.NE.0) THEN
      IF (MLEVEL.GE.1) WRITE (MUNIT,20) CATFIL(1:ILAST), IERR
 20   FORMAT (/,' **** zopcat6 - ERROR:  Unable to Access the Catalog',
     * ' File ****',/,' Catalog File: ',A)
      IF (.NOT.LCREAT) GO TO 820
      GO TO 820
      ELSE
      LOPEN = .TRUE.
C
C     Check to see if it is already cataloged
      DO 80 K=1,20
      READ ( ICUNIT, 40, END=820) CLINE
 40   FORMAT (A)
      CALL UPCASE (CLINE)
      I = INDEX (CLINE,'HECDSS')
      IF (I.GT.0) THEN
      J = INDEX (CLINE,'CATALOG')
      IF (J.GT.0) THEN
C     LCATLG = .TRUE.
C
      DO 60 N=1,10
      READ ( ICUNIT, 40, END=820) CLINE
      CALL UPCASE (CLINE)
C     Check version 6 style catalog
      I = INDEX (CLINE,'OF RECORDS:')
      IF (I.GT.0) THEN
      J = I + 11
      NREC = INTCHR (CLINE(J:J+6))
      IF (NREC.EQ.-1) GO TO 60
C     Look for a pathanme slash within the next few lines
      DO 50 I=1,10
      READ (ICUNIT, 40, END=800) CLINE
      IF (INDEX(CLINE,'/').GT.0) THEN
      LCATLG = .TRUE.
      GO TO 800
      ENDIF
 50   CONTINUE
      GO TO 800
      ENDIF
C     Check for version 4 style of catalog
      I = INDEX (CLINE,'OF RECORDS =')
      IF (I.GT.0) THEN
      J = I + 12
      NREC = INTCHR (CLINE(J:J+4))
      IF (NREC.EQ.-1) GO TO 60
      DO 55 I=1,10
      READ (ICUNIT, 40, END=800) CLINE
      IF (INDEX(CLINE,'/').GT.0) THEN
      LCATLG = .TRUE.
      GO TO 800
      ENDIF
 55   CONTINUE
      GO TO 800
      ENDIF
 60   CONTINUE
C
      ENDIF
      ENDIF
 80   CONTINUE
      ENDIF
C
      ELSE
C     CREATE THE CATALOG FILE
      IF (MLEVEL.GE.2) WRITE (MUNIT,90) CATFIL(1:ILAST)
 90   FORMAT ( ' Catalog File Does Not Exist: ',A)
      IF (.NOT.LCREAT) GO TO 820
C
C     OPEN THE FILE
      OPEN ( UNIT=ICUNIT, FILE=CATFIL(1:ILAST), IOSTAT=IERR)
      IF (IERR.EQ.0) THEN
         IF (MLEVEL.GE.2) WRITE (MUNIT,100) CATFIL(1:ILAST)
 100     FORMAT ( ' Created Catalog File: ',A)
         LOPEN = .TRUE.
      ELSE
         IF (MLEVEL.GE.1) WRITE ( MUNIT, 120) CATFIL(1:ILAST), IERR
 120     FORMAT ( ' --Unable to Create Catalog File: ',A,I8)
         LOPEN = .FALSE.
         GO TO 820
      ENDIF
      ENDIF
C
C
C
 800  CONTINUE
      REWIND ICUNIT
      IF (MLEVEL.GE.3) WRITE ( MUNIT, 801) CATFIL(1:ILAST)
 801  FORMAT ( ' Catalog File: ',A)
C
 820  CONTINUE
      RETURN
C
      END


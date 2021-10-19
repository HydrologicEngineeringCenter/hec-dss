      SUBROUTINE zprtc6 (MUNIT, IBUFF, NEXT)
C
C     Print Data Compression information (for time series data
C
C     Written by Bill Charley at HEC, 1989
C
C
      INTEGER IBUFF(*)
      CHARACTER CLINE*400
C
C
C     Get compression scheme, base, etc. for this set
      CALL CHGTYP (IBUFF(NEXT+1), DBASE)
      CALL GETHOL (IBUFF(NEXT+2), 1, IMETH)
      CALL GETHOL (IBUFF(NEXT+2), 2, IBASE)
      CALL GETHOL (IBUFF(NEXT+2), 3, NBYTES)
      CALL GETHOL (IBUFF(NEXT+2), 4, NPRE)
      CALL GETHOL (IBUFF(NEXT+2), 5, NPARTS)
C     (NPRE is offset by 50 so that negative
C     numbers can be stored in one byte)
      NPRE = NPRE - 50
C
C     Get required parts and their lengths
      IPOS = 6
      JLINE = 1
      CLINE = ' '
      DO 80 I=1,NPARTS
      CALL GETHOL (IBUFF(NEXT+2), IPOS, IPART)
      IPOS = IPOS + 1
      CALL GETHOL (IBUFF(NEXT+2), IPOS, ILEN)
      IPOS = IPOS + 1
      CLINE(JLINE:JLINE+3) = '  ' // CHAR(64+IPART) // '='
      IF (JLINE.GT.1) CLINE(JLINE:JLINE) = ','
      JLINE = JLINE + 4
      IF (ILEN.GT.0) THEN
      CALL HOLCHR (IBUFF(NEXT+2), IPOS, ILEN, CLINE(JLINE:), 1)
      JLINE = JLINE + ILEN
      IPOS = IPOS + ILEN
      ENDIF
 80   CONTINUE
C
      WRITE (MUNIT, 100) CLINE(1:JLINE)
 100  FORMAT (/,' Pathname Parts:',A)
C
      CLINE = ' '
      IF (IMETH.EQ.1) CLINE = 'Repeat'
      IF (IMETH.EQ.2) CLINE = 'Delta'
      IF (IMETH.EQ.3) CLINE = 'Repeat + Delta'
      IF (IMETH.EQ.4) CLINE = 'Significant Digits'
      IF (IMETH.EQ.5) CLINE = 'Repeat + Significant Digits'
      WRITE (MUNIT,120) IMETH, CLINE(1:30)
 120  FORMAT (' Compression Method:',I3,';  ',A)
C
      IF ((IMETH.EQ.2).OR.(IMETH.EQ.3)) THEN
      WRITE (MUNIT, 150) NPRE
 150  FORMAT (' Precision:',I3)
      IF (IBASE.GT.0) WRITE (MUNIT,140) DBASE
 140  FORMAT (' Base Value Set to:',F8.3)
      IF (NBYTES.EQ.2) THEN
      WRITE (MUNIT, 160)
 160  FORMAT (' Two bytes allocated for each value.')
      ELSE
      WRITE (MUNIT, 180)
 180  FORMAT (' Compression Software selects allocation space',
     * ' for the Delta scheme.')
      ENDIF
      ENDIF
C
      RETURN
      END


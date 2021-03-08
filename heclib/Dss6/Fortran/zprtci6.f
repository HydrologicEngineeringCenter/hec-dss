      SUBROUTINE zprtci6 (IFLTAB, LALL, CPARTS)
C
C
C     Print Data Compression Infromation to unit MUNIT
C
C     Written by Bill Charley at HEC, 1989
C
C
      INTEGER IFLTAB(*)
      CHARACTER CPARTS(*)*(*), CLINE*400
      LOGICAL LALL, L
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdssts.h'
C
      INCLUDE 'zdssmz.h'
C
C
      IF (IFLTAB(KCOMPN).EQ.0) THEN
C
      WRITE (MUNIT, 20)
 20   FORMAT (/,' No Default Compression Methods have been',
     * ' set for this DSS file.',/)
C
      ELSE
C
      JSIZE = IFLTAB(KCOMPN)
      CALL zgtrec6(IFLTAB, ILBUFF(1), JSIZE, IFLTAB(KCOMPI), .FALSE.)
C
C
      JNEXT = 1
C
C     Loop through each set, comparing pathname parts
C     LOOP
 40   CONTINUE
C     IF (JNEXT.GT.IFLTAB(KCOMPN)) EXIT LOOP
      IF ((JNEXT.GT.IFLTAB(KCOMPN)).OR.(JNEXT.LE.0)) GO TO 800
C
      IF (LALL) THEN
      NEXT = JNEXT
      IF (ILBUFF(NEXT).LE.0) GO TO 800
      ELSE
      NEXT = -99
      CALL zgetci6 (IFLTAB, CPARTS, IMETH, D, L, L, NEXT)
      IF (IMETH.LE.0) GO TO 900
      ENDIF
C
      CALL zprtc6 (MUNIT, ILBUFF, NEXT)
C
C     ENDLOOP
      JNEXT = ILBUFF(NEXT)
      IF (LALL) GO TO 40
C
      ENDIF
C
 800  CONTINUE
      RETURN
C
C     No compression method found for this set of pathname parts
 900  CONTINUE
      JLINE = 1
      CLINE = ' '
      DO 910 I=1,6
      CALL CHRLNB (CPARTS(I), N)
      IF (N.GT.0) THEN
      CLINE(JLINE:JLINE+2+N) = ' ' // CHAR(64+I) // '=' //
     * CPARTS(I)(1:N)
      JLINE = JLINE + N + 3
      ENDIF
 910  CONTINUE
      CALL CHRLNB(CLINE,N)
      WRITE (MUNIT, 920) CLINE(1:N)
 920  FORMAT (/,' Pathname Parts: ',A)
      WRITE (MUNIT, 930)
 930  FORMAT (' No Data Compression Method Set.',/)
      GO TO 800
C
      END
      SUBROUTINE zprtci(IFLTAB, LALL, CPARTS)
      INTEGER IFLTAB(*)
      CHARACTER CPARTS(*)*(*)
      LOGICAL LALL
      call zGetVersion(IFLTAB, iversion)
      IF (iversion.EQ.6) THEN
      call zprtci6 (IFLTAB, LALL, CPARTS)
      endif
      return
      end


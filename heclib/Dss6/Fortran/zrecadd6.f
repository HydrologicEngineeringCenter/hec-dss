      SUBROUTINE ZRECADD6(IFLTAB, CPATH, ILADD, ISTAT)
C
C
C     Return internal record addresses for debugging purposes (only)
C     ISTAT is returned as:
C        0  Record found, ok to read/write
C       -1  Record not found
C        1  Record found, user does not have delete access
C           (but does have read/write access)
C        2  Record found, user has only read access
C        3  Record found, user does not have access
C
C     Written by Bill Charley at HEC, 1989.
C
      INTEGER IFLTAB(*)
      CHARACTER CPATH*(*)
      INTEGER*8 ILADD(9)
      INTEGER ISTAT
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdsslz.h'
C
      INCLUDE 'zdssmz.h'
C
C
      IF (MLEVEL.GE.11) WRITE ( MUNIT, 20) IFLTAB(KUNIT), CPATH
 20   FORMAT (T6,'-----DSS---Debug: Enter ZRECADD',/,T10,
     * 'UNIT =',I5,'  PATH: ',A)
C
C
C     Check that IFLTAB is valid (e.g., the DSS file is open)
      IF (IFLTAB(1).NE.6) CALL ZERROR6(IFLTAB, 5, 'ZRECADD',
     * 0, IFLTAB, ' ', 0, ' ',0)
C
      CALL ZRDINF6(IFLTAB, CPATH, NHEAD, NDATA, ISTAT)
      ILADD(1) = 0                       !  tableHash - DSS Version 7
      ILADD(2) = IHASH                   !  hashCode
      ILADD(3) = IHASH + NPERM           !  hashAddress
      ILADD(4) = IFLTAB(KBNSIZ)          !  binLength
      ILADD(5) = IFLTAB(KPADD)           !  binAddress
      ILADD(6) = NINFO + NPPWRD          !  infoLength
      ILADD(7) = IFLTAB(KAINFO)          !  infoAddress
      IF (ISTAT.NE.-1) THEN
         ILADD(8) = INFO(NPPWRD+KINDAT)  !  dataLength
         ILADD(9) = INFO(NPPWRD+KIADAT)  !  dataAddress
      ENDIF
C
      IF (MLEVEL.GE.11) WRITE (MUNIT, 820) ISTAT
 820  FORMAT (T6,'-----DSS--Debug: EXIT ZRECADD;  Status:',I3)
C
      RETURN
C
C
 900  CONTINUE
      IF (.NOT.LNOABT) CALL ZDEBUG (MUNIT, INFO, 1, 30)
      NP = INFO(KINPAT)
      CALL ZERROR6(IFLTAB, 11, 'ZRECADD', 0, NADD, CPATH, NPATH, CTPATH,
     * NP)
      ISTAT = -1
      RETURN
C
      END


      SUBROUTINE zcutsz6(IFLTAB, CPATH, ISIZE, ISTATUS)
C
C
C     Get the size of buffer needed to do a cut/copy operation (zcut6)
C
C     Written by Bill Charley, HEC, 1999.
C
C
      INTEGER IFLTAB(*)
      CHARACTER CPATH*(*)
C
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdssmz.h'
C
C
      IF (MLEVEL.GE.11) THEN
         CALL CHRLNB(CPATH,NPATH)
         WRITE (MUNIT, 20) IFLTAB(KUNIT), CPATH(1:NPATH)
 20      FORMAT (T6,'-----DSS---Debug: Enter zcutsz6',/,
     *   T5,'Unit:',I5,'  Path: ',A)
      ENDIF
C
C
C     Read in the main information block
      ISIZE = 0
      CALL zrdinf6(IFLTAB, CPATH, JHEAD, JDATA, ISTATUS)
      IF (ISTATUS.NE.0) GO TO 800
C
C     Get the sizes of the headers and data blocks
      NIHEAD = INFO(NPPWRD+KINIHE)
      NCHEAD = INFO(NPPWRD+KINCHE)
      NUHEAD = INFO(NPPWRD+KINUHE)
      NDATA  = INFO(NPPWRD+KINDAT)
C
      ISIZE = 4 + NIHEAD + NCHEAD + NUHEAD + NDATA + NINFO + NPPWRD
C
C
C
 800  CONTINUE
      IF (MLEVEL.GE.11) WRITE (MUNIT, 820) ISIZE, ISTATUS
 820  FORMAT (T6,'-----DSS---Debug: Exit zcutsz6, Size:',I5,
     * ', Status:',I4)
      RETURN
C
      END


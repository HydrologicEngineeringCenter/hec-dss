      SUBROUTINE zreadx6 (IFLTAB, CPATH, IIHEAD, KIHEAD, NIHEAD,
     * ICHEAD, KCHEAD, NCHEAD, IUHEAD, KUHEAD, NUHEAD, IDATA,
     * KDATA, NDATA, IPLAN, LFOUND)
C
C
C     Main routine for retrieving data (zrdbuf6 may also be used)
C
C     Written by Bill Charley at HEC, 1989
C
      INTEGER IFLTAB(*), IIHEAD(*), ICHEAD(*), IUHEAD(*), IDATA(*)
      CHARACTER CPATH*(*)
      LOGICAL LFOUND, LEND
      integer KIHEAD,KCHEAD,KUHEAD,KDATA,IPLAN,N,J,JSIZE
      integer NIHEAD,NCHEAD,NUHEAD,NDATA
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdssmz.h'
C
C
      IF (MLEVEL.GE.11) WRITE ( MUNIT, 20) IFLTAB(KUNIT), CPATH,
     * KIHEAD, KCHEAD, KUHEAD, KDATA, IPLAN
 20   FORMAT (T6,'-----DSS---Debug: Enter zreadx6',/,T10,
     * 'UNIT =',I5,'  PATH: ',A,/,T10,'KIHEAD:',I8,',  KCHEAD:',I8,
     * ',  KUHEAD:',I8,',  KDATA:',I8,',  IPLAN:',I4)
C
C
C
C     Get the info block, and read the first portion of the header
      IFLTAB(KRBNPA) = -1
      CALL zrdbuf6 (IFLTAB, CPATH, IIHEAD, 0, N, IDATA, 0, J,
     * LEND, IPLAN, LFOUND)
      IF (IFLTAB(KSTAT).NE.0) RETURN
C
      IF (LFOUND) THEN
C
C     Get get any internal header area
      JSIZE = INFO(NPPWRD+KINIHE)
      NIHEAD = JSIZE
      JSIZE = MIN0 (JSIZE, KIHEAD)
      IF (JSIZE.GT.0)
     * CALL zgtrec6(IFLTAB, IIHEAD, JSIZE, INFO(NPPWRD+KIAIHE), .FALSE.)
C
C     Get get the compression header area
      JSIZE = INFO(NPPWRD+KINCHE)
      NCHEAD = JSIZE
      JSIZE = MIN0 (JSIZE, KCHEAD)
      IF (JSIZE.GT.0)
     * CALL zgtrec6(IFLTAB, ICHEAD, JSIZE, INFO(NPPWRD+KIACHE), .FALSE.)
C
C     Get get any user header area
      JSIZE = INFO(NPPWRD+KINUHE)
      NUHEAD = JSIZE
      JSIZE = MIN0 (JSIZE, KUHEAD)
      IF (JSIZE.GT.0)
     * CALL zgtrec6(IFLTAB, IUHEAD, JSIZE, INFO(NPPWRD+KIAUHE), .FALSE.)
      IF (IFLTAB(KSTAT).NE.0) RETURN
C
C     Get the data
      JSIZE = INFO(NPPWRD+KINDAT)
      NDATA = JSIZE
      JSIZE = MIN0 (JSIZE, KDATA)
      IF (JSIZE.GT.0)
     * CALL zgtrec6 (IFLTAB, IDATA, JSIZE, INFO(NPPWRD+KIADAT), .FALSE.)
C
      ELSE
C
      NIHEAD = 0
      NCHEAD = 0
      NUHEAD = 0
      NDATA = 0
C
      ENDIF
C
C
      IF (MLEVEL.GE.11) WRITE ( MUNIT,820) NIHEAD, NCHEAD, NUHEAD,
     * NDATA
 820  FORMAT (T6,'-----DSS---Debug: Exit  zreadx6',/,T10,
     * 'NIHEAD:',I5,',  NCHEAD:',I5,',  NUHEAD:',I5,',  NDATA:',I5)
C
      RETURN
C
      END


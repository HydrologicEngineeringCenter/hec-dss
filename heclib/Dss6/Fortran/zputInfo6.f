      SUBROUTINE zputInfo6 (IFLTAB, CPATH, IBUFF, ISTAT)
C
C
C     Get record information
C     Written by Bill Charley at HEC, 2010.
C
C     Input:   IFLTAB  - DSS file table
C              CPATH   - Pathname
C
C
      INTEGER IFLTAB(*), IBUFF(*), ISTAT
      CHARACTER CPATH*(*)
C
      INTEGER NUHEAD, NDATA, JUL, IERR, IHR, IMIN, ISECS, NADD
      CHARACTER CSCRAT*30
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdsslz.h'
C
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdsscz.h'
C
      INCLUDE 'zdssmz.h'
C
C
C     ibuff[1] = data type
C     ibuff[2] = version number (number of writes)
C     ibuff[3] = expansion number (number of time expanded)
C     ibuff[4] = expansion flag
C     ibuff[5] = compression
C     ibuff[6] = percision
C     ibuff[7] = last written date in julian (since 1900)
C     ibuff[8] = last written time in seconds past midnight
C     ibuff[9,10,11,12] = last write program (16 characters long)
C     ibuff[13,14] = record password (8 characters long)
C
C
      IF (MLEVEL.GE.11) WRITE ( MUNIT, 20) IFLTAB(KUNIT), CPATH
 20   FORMAT (T6,'-----DSS---Debug: Enter zputInfo6',/,T10,
     * 'UNIT =',I5,'  PATH: ',A)
C
      CALL zrdinf6 (IFLTAB, CPATH, NUHEAD, NDATA, ISTAT)
      IF (ISTAT.EQ.0) THEN
C
        INFO(NPPWRD+KITYPE) = IBUFF(1)
        INFO(NPPWRD+KIVER) = IBUFF(2)
        INFO(NPPWRD+KIPREC) = IBUFF(6)
C
        CSCRAT = ' '
        JUL = IBUFF(7)
        CALL JULDAT(JUL, 114, CSCRAT, N)
        CALL CHRHOL (CSCRAT, 1, NDATEC, INFO(NPPWRD+KIDATE), 1)
C
        CSCRAT = ' '
        ISECS = IBUFF(8)
        IHR = ISECS/3600
        IMIN = (ISECS - (IHR * 3600)) / 60
        ISEC = ISECS - (IHR * 3600) - (IMIN * 60)
        WRITE (CSCRAT,30) IHR,IMIN,ISEC
 30     FORMAT (I2.2,':',I2.2,':',I2.2)
        CALL CHRHOL (CSCRAT, 1, NTIMEC, INFO(NPPWRD+KITIME), 1)
C
        INFO(NPPWRD+KIPROG) = IBUFF(9)
        INFO(NPPWRD+KIPROG+1) = IBUFF(10)
C       Ignore last 8 here
        INFO(NPPWRD+KIPASS) = IBUFF(13)
        INFO(NPPWRD+KIPASS+1) = IBUFF(14)
C
        NADD = IPNBIN(JPNBIN+NPPWRD+KBAINF)
        CALL zptrec6 (IFLTAB, INFO, NINFO+NPPWRD, NADD, .FALSE.)
C
      ENDIF
C
C
C
 800  CONTINUE
      IF (MLEVEL.GE.11) WRITE ( MUNIT,820)
 820  FORMAT (T6,'-----DSS---Debug: Exit  zputInfo6')
      RETURN
C
      END


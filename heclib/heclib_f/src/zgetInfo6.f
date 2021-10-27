      SUBROUTINE zgetInfo6 (IFLTAB, CPATH, IBUFF, ISTAT)
C
C
C     Get record information
C     Written by Bill Charley at HEC, 2010.
C
C     Input:   IFLTAB  - DSS file table
C              CPATH   - Pathname
C
C
      INTEGER IFLTAB(*), IBUFF(*), ISTAT, I
      CHARACTER CPATH*(*)
C
      INTEGER NUHEAD, NDATA, JUL, IERR, IHR,IMIN,ISEC
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
 20   FORMAT (T6,'-----DSS---Debug: Enter zgetInfo6',/,T10,
     * 'UNIT =',I5,'  PATH: ',A)
C
      CALL zrdinf6 (IFLTAB, CPATH, NUHEAD, NDATA, ISTAT)
      IF (ISTAT.NE.0) THEN
         IF (MLEVEL.GE.3) WRITE ( MUNIT, 24) IFLTAB(KUNIT), CPATH
 24      FORMAT ('-----DSS---zgetInfo6, failed to find record',/,T10,
     *   'UNIT =',I5,'  PATH: ',A)
        GO TO 800
      ENDIF
C
      DO 25 I=1, 14
        IBUFF(I) = 0
 25   CONTINUE

C
        IBUFF(1) = INFO(NPPWRD+KITYPE)
        IBUFF(2) = INFO(NPPWRD+KIVER)
        IBUFF(3) = 0  ! Not recorded in DSS-6
        IBUFF(4) = 0  ! Not recorded in DSS-6
        IBUFF(5) = 0  ! Not recorded in DSS-6
        IBUFF(6) = INFO(NPPWRD+KIPREC)
C
        CSCRAT = ' '
        CALL HOLCHR (INFO(NPPWRD+KIDATE), 1, NDATEC, CSCRAT, 1)
        CALL DATJUL(CSCRAT(1:NDATEC), JUL, IERR)
        IF (IERR.NE.0) JUL = 0;
        IBUFF(7) = JUL
        CSCRAT = ' '
        CALL HOLCHR (INFO(NPPWRD+KITIME), 1, NTIMEC, CSCRAT, 1)
        !MIN = IHM2M(CSCRAT(1:NTIMEC))
        !IBUFF(4) = MIN * 60
        READ (CSCRAT,30) IHR,IMIN,ISEC
 30     FORMAT (I2,1X,I2,1X,I2)
        IBUFF(8) = (IHR * 3600) + (IMIN * 60) + ISEC
C
        CALL HOLCHR (INFO(NPPWRD+KIPROG), 1, NPROGC, CSCRAT, 1)
        CALL CHRHOL (CSCRAT, 1, NPROGC, IBUFF(9), 1)
C
        CALL HOLCHR (INFO(NPPWRD+KIPASS), 1, NPASSC, CSCRAT, 1)
        CALL CHRHOL (CSCRAT, 1, NPASSC, IBUFF(13), 1)
C
C
C
C
 800  CONTINUE
      IF (MLEVEL.GE.11) WRITE ( MUNIT,820)
 820  FORMAT (T6,'-----DSS---Debug: Exit  zgetInfo6')
      RETURN
C
      END


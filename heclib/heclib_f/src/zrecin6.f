      SUBROUTINE zrecin6 (IFLTAB, IOUT, ILEVEL, CPATH, IBUFF, KBUFF,
     * LFOUND)
C
C     Print out record information
C     Written by Bill Charley at HEC, 1990.
C
C     Example output:
C
C  Record Found:
C  /MUSKINGUM/ZANF5/PRECIP-INC/01JAN1970/12HOUR/OBS/
C  Regular-interval time series; Tag: T14; Precision: 2; Password Applie
C  Last Written on 02JAN85,  at 11:19  by Program:  NONE
C  Version:   8;  Number of Data:   60;  Space Allocated:  20
C  Compressed to 43%
C  Compression Method:  3;  Delta + Repeat
C  Precision:  -2;  Base:  0.000;  Size: 2
C
C  Record Found:
C  /MUSKINGUM/ZANF5/FLOW/01JAN1990/1HOUR/OBS/
C  Regular-interval time series; Tag: T14; Precision: 2; Password Applie
C  Last Written on 02JAN85,  at 11:19  by Program:  NONE
C  Version:   6;  Number of Data:  744;  Space Allocated: 1440
C  Data qualilty flags set
C
C
      INTEGER IFLTAB(*), IBUFF(*)
      INTEGER IOUT, ILEVEL, KBUFF
      CHARACTER CPATH*(*)
      LOGICAL LFOUND
C
      CHARACTER CSCRAT*392
      LOGICAL LBASE, LRPEAT, LDELTA, LSIGDT
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
      INCLUDE 'zdssShared.h'
C
C
C
      IF (MLEVEL.GE.11) WRITE ( MUNIT, 20) IFLTAB(KUNIT), CPATH
 20   FORMAT (T6,'-----DSS---Debug: Enter zrecin6',/,T10,
     * 'UNIT =',I5,'  PATH: ',A)
C
C
C
      CALL zrdinf6 (IFLTAB, CPATH, NUHEAD, NDATA, ISTAT)
      IF (ISTAT.EQ.0) THEN
      LFOUND = .TRUE.
      ELSE
      LFOUND = .FALSE.
      ENDIF
C
      IF (LFOUND) THEN
C
      CALL CHRLNB(CPATH, N)
      WRITE (IOUT,100) CPATH(1:N)
 100  FORMAT (' Record Found:',/,1X,A)
C
      IF (ILEVEL.GE.2) THEN
C
      NIHEAD = INFO(NPPWRD+KINIHE)
      NCHEAD = INFO(NPPWRD+KINCHE)
      NUHEAD = INFO(NPPWRD+KINUHE)
      NDATA = INFO(NPPWRD+KINDAT)
C
      ITYPE = INFO(NPPWRD+KITYPE)
      CALL HOLCHR (INFO(NPPWRD+KITAG), 1, NTAGC, CTAG, 1)
C
C Regular-interval time series; Tag: T14; Precision: 2; Password Applied
C
      DO 110 I=1,NRTYPE
         IF (ITYPE.EQ.IRTYPE(I)) THEN
            NT = I
            GO TO 120
         ENDIF
 110  CONTINUE
      NT = 18
C
 120  CONTINUE
C
!      CALL CHRLNB (CT(NT), N)
!      WRITE (CSCRAT, 130) CT(NT)(1:N), CTAG(1:NTAGC)
      CALL CHRLNB (CRDESC(NT), N)
      WRITE (CSCRAT, 130) CRDESC(NT)(1:N), CTAG(1:NTAGC)
 130  FORMAT (1X,A,';  Tag: ',A)
C
C
      IF ((INFO(NPPWRD+KIPREC).GT.0).AND.(INFO(NPPWRD+KIPREC).LE.8))THEN
      CALL CHRLNB (CSCRAT, N)
      WRITE (CSCRAT(N+1:), 160) INFO(NPPWRD+KIPREC)
 160  FORMAT (';  Precision:',I2)
      ENDIF
C
      IF (INFO(NPPWRD+KIPASS).GT.0) THEN
      CALL CHRLNB (CSCRAT, N)
C!!!! WRITE (CSCRAT(N+1:), 170)
 170  FORMAT (';  Password Applied')
      ENDIF
C
      CALL CHRLNB(CSCRAT,N)
      WRITE (IOUT, 180) CSCRAT(1:N)
 180  FORMAT (A)
C
      CSCRAT = ' '
      CALL HOLCHR (INFO(NPPWRD+KIDATE), 1, NDATEC, CSCRAT, 1)
      CALL HOLCHR (INFO(NPPWRD+KITIME), 1, NTIMEC, CSCRAT, 11)
      CALL HOLCHR (INFO(NPPWRD+KIPROG), 1, NPROGC, CSCRAT, 21)
      WRITE (IOUT,220) CSCRAT(1:7), CSCRAT(11:15), CSCRAT(21:26)
 220  FORMAT (' Last Written on ',A,',  at ',A,'  by Program:  ',A)
C
      WRITE (CSCRAT, 240) INFO(NPPWRD+KIVER), INFO(NPPWRD+KILNDA)
 240  FORMAT ('Version:',I4,';  Number of data:',I5)
      IF (INFO(NPPWRD+KILNDA).NE.INFO(NPPWRD+KINDAT)) THEN
      WRITE (CSCRAT(36:), 260) INFO(NPPWRD+KINDAT)
 260  FORMAT (';  Space Allocated:',I5)
      ENDIF
      CALL CHRLNB(CSCRAT,N)
      WRITE (IOUT, 300) CSCRAT(1:N)
 300  FORMAT (1X,A)
C
      IF (INFO(NPPWRD+KIQUAL).GT.0) THEN
      WRITE (IOUT, 320)
 320  FORMAT (' Data flags set.')
C
      ELSE IF (INFO(NPPWRD+KICOMP).GT.0) THEN
      PREC = ( (REAL(INFO(NPPWRD+KINDAT)) + REAL(NCHEAD)) /
     * REAL(INFO(NPPWRD+KILNDA)) ) * 100.
      WRITE (IOUT, 400) PREC
 400  FORMAT (' Compressed to',F5.1,'%')
C
      IMETH = INFO(NPPWRD+KICOMP)
      CSCRAT = ' '
      IF (IMETH.EQ.1) CSCRAT = 'Repeat'
      IF (IMETH.EQ.2) CSCRAT = 'Delta'
      IF (IMETH.EQ.3) CSCRAT = 'Repeat + Delta'
      IF (IMETH.EQ.4) CSCRAT = 'Significant Digits'
      IF (IMETH.EQ.5) CSCRAT = 'Repeat + Significant Digits'
      WRITE (IOUT,420) IMETH, CSCRAT(1:30)
 420  FORMAT (' Compression Method:',I3,';  ',A)
C
      JSIZE = MIN0 (NCHEAD, KBUFF)
      IF (JSIZE.GT.0)
     * CALL zgtrec6 (IFLTAB, IBUFF, JSIZE, INFO(NPPWRD+KIACHE), .FALSE.)
C
      CALL DHINFO (IBUFF, JSIZE, IMETH, N, BASE, LBASE, NELMS, ISIZE,
     * IDCPRE, LRPEAT, LDELTA, LSIGDT, IST)
C
      IF (LDELTA) THEN
      WRITE (IOUT, 440) IDCPRE, ISIZE, BASE, LBASE
 440  FORMAT(' Precision:',I3,';  Element Size:',I2,';  Base:',F12.2,
     * ';  User set base:',L2)
      ENDIF
C
C
      ENDIF
      ENDIF
C
      ELSE
C
      CALL CHRLNB (CPATH, N)
      WRITE (IOUT,500) CPATH(1:N)
 500  FORMAT (' Record Does Not Exist: ',/,1X,A,/)
C
      ENDIF
C
C
 800  CONTINUE
      IF (MLEVEL.GE.11) WRITE ( MUNIT,820)
 820  FORMAT (T6,'-----DSS---Debug: Exit  zrecin6')
      RETURN
C
      END
      SUBROUTINE zrecin(IFLTAB, IOUT, ILEVEL, CPATH, IBUFF, KBUFF,
     * LFOUND)
      INTEGER IFLTAB(*), IBUFF(*)
      INTEGER IOUT, ILEVEL, KBUFF
      CHARACTER CPATH*(*)
      LOGICAL LFOUND
      call zGetVersion(IFLTAB, iversion)
      IF (iversion.EQ.6) THEN
      call zrecin6 (IFLTAB, IOUT, ILEVEL, CPATH, IBUFF, KBUFF,
     * LFOUND)
      endif
      return
      end


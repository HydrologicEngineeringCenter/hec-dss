      SUBROUTINE RT2CT( LFNOUT,ITIME,NTIMES,CTIME,NCT )
C
C     GENERATE A SERIES OF SHEF DATE AND TIME STAMPS FROM
C     A SERIES OF DSS/HECLIB STYLE TIMES <TIME>.  THE ROUTINE
C     GENERATES NEARLY THE MINIMAL SIZE SHEF TIME STAMPS.
C
      CHARACTER CTIME(*)*(*)
      DIMENSION ITIME(*),NCT(*)
      LOGICAL LPOPT
      INTEGER JUL,  JULN                                         
C
      JUL = ITIME(1)/1440
      JDUM =  JLIYMD( JUL,IYR,IMON,IDAY )
C
C
      DO 1000 N=1,NTIMES
        CTIME(N) = ' '
	    JULN = ITIME(N) / 1440
	IM = ITIME(N) - (JULN * 1440)
C
         ITADJ=0
         IF(IM .EQ. 0) THEN
            IM = 1440
            ITADJ=-1
         ENDIF
         JHR = IM / 60
         JMIN = MOD(IM,60)
         JULN = JULN +ITADJ
         JDUM = JLIYMD( JULN,JYR,JMON,JDAY )
C
C
         IF ( JULN.GT.JUL ) THEN
            IF ( JYR.NE.IYR ) THEN
               CTIME(N)(1:2) = 'DY'
               JYR2 = MOD(JYR,100)
               WRITE ( CTIME(N)(3:12),100 ) JYR2,JMON,JDAY,JHR,JMIN
  100          FORMAT ( 5I2 )
               NCT(N) = 12
            ELSE IF ( JMON.NE.IMON ) THEN
               CTIME(N)(1:2) = 'DM'
               WRITE ( CTIME(N)(3:10),100 ) JMON,JDAY,JHR,JMIN
               NCT(N) = 10
            ELSE IF ( JDAY.NE.IDAY ) THEN
               CTIME(N)(1:2) = 'DD'
               WRITE ( CTIME(N)(3:8),100 ) JDAY,JHR,JMIN
               NCT(N) = 8
            ELSE
            ENDIF
         ELSE IF ( JHR.NE.IHR ) THEN
            CTIME(N)(1:2) = 'DH'
            WRITE ( CTIME(N)(3:6),100 ) JHR,JMIN
            NCT(N) = 6
         ELSE
            CTIME(N)(1:2) = 'DN'
            WRITE ( CTIME(N)(3:4),100 ) JMIN
            NCT(N) = 4
         ENDIF
C        CHECK FOR REPEATED DATE/TIME ELEMENTS FROM RIGHT TO LEFT
         IF ( N.EQ.1 ) GO TO 400
            IF ( CTIME(N)(1:2).EQ.'DN'.OR.JMIN.NE.IMIN ) GOTO 400
            NCT(N) = NCT(N) - 2
            IF ( CTIME(N)(1:2).EQ.'DH'.OR.JHR.NE.IHR ) GOTO 400
            NCT(N) = NCT(N) - 2
            IF ( CTIME(N)(1:2).EQ.'DD'.OR.JDAY.NE.IDAY ) GOTO 400
            NCT(N) = NCT(N) - 2
            IF ( CTIME(N)(1:2).EQ.'DM'.OR.JMON.NE.IMON ) GOTO 400
            NCT(N) = NCT(N) - 2
  400    CONTINUE
         DO 500 L=1,NCT(N)
            IF ( CTIME(N)(L:L).EQ.' ') CTIME(N)(L:L)='0'
  500    CONTINUE
         IMIN = JMIN
         IHR  = JHR
         IDAY = JDAY
         IMON = JMON
         IYR  = JYR
         JUL  = JULN
 1000 CONTINUE
      RETURN
      END

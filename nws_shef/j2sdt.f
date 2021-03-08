      SUBROUTINE J2SDT ( JUL,ITIME,YYMMDD,icen)
C
      CHARACTER*(*) YYMMDD
      CHARACTER*4 CHHMM
      INTEGER*4 JUL, JULX                                               M
      integer icen
C
      JULX=JUL
	IXTIME=ITIME
      IF(ITIME .EQ. 0) THEN
         JULX=JULX-1
         IXTIME=1440
      ENDIF
C
      YYMMDD = ' '
      JDUM = JLIYMD ( JULX,IYR,IMON,IDAY )
      icen=iyr/100
      IYR=MOD(IYR,100)
      WRITE ( YYMMDD,'(3I2)') IYR,IMON,IDAY
      LNN = LEN(YYMMDD)
      IF ( LNN.EQ.6 ) THEN
      ELSE IF ( LNN.EQ.8 ) THEN
         MTIM = M2IHM(IXTIME,CHHMM )
         YYMMDD(7:8) = CHHMM(1:2)
      ELSE IF (LNN.EQ.10 ) THEN
         MTIM = M2IHM(IXTIME,CHHMM )
         YYMMDD(7:10) = CHHMM(1:4)
      ELSE
         YYMMDD = '???????'
      ENDIF
      DO 10 I = 1,LNN
         IF ( YYMMDD(I:I).EQ.' ') YYMMDD(I:I)='0'
   10 CONTINUE
      RETURN
      END

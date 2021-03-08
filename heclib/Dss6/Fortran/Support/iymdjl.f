      FUNCTION IYMDJL (IYR,IMON,IDAY)
C
C     CONVERT FROM INTEGER IYR IMON IDAY DATE TO DAY COUNT
C
C     ASSUME AT LEAST 20 BIT INTEGER
C     USE BASE AS  1900 (01 JAN 1900 IS DAY 1)
C
C     This code includes conversion of a abbrievated 2 digit year
C     to a 4 digit year
C
      COMMON /UNDEF_TIME/ ITIME_UNDEF
      INTEGER ITIME_UNDEF
C      
      INTEGER IBASE, IYMDJL
      INTEGER NDAY(12)
C
      DATA NDAY /0,31,59,90,120,151,181,212,243,273,304,334/
      DATA IBASE /693960/
C
      NYR=IYR
C
C     Fix up months, if off
      JMON = IMON
      IF (JMON.GT.12) THEN
      NYR = NYR + 1
      JMON = JMON - 12
      ENDIF
      IF (JMON.LT.1) THEN
      NYR = NYR - 1
      JMON = JMON + 12
      ENDIF
C
C     Is this an abbrievated year?  Add the century if so.
      IF (NYR.LT.100) CALL ADDCENTURY (NYR)
C
      JYR=NYR-1
      NLEAP=JYR/4+JYR/400-JYR/100
C
      MLEAP=0
      IF (JMON.GE.3) THEN
      IF (MOD(NYR,4).EQ.0 .AND. (MOD(NYR,100).NE.0 .OR. (MOD(NYR,100)
     .    .EQ.0 .AND. MOD(NYR,400).EQ.0))) MLEAP=1
      ENDIF
      IF ((JMON.LE.0).OR.(JMON.GE.13)) THEN
      IYMDJL = ITIME_UNDEF
      ELSE
      IYMDJL=NYR*365+NLEAP+NDAY(JMON)+IDAY+MLEAP-IBASE                  
      ENDIF
C
      RETURN
      END


      SUBROUTINE ROUND(LFUOUT,DATA,NDATA,NSIG,IRND)
C
      INTEGER LFUOUT,NDATA,NSIG,IRND
      REAL DATA(*)
C
C     ROUND VALUES IN A TIME SERIES TO LESSER OF SIGNIFICANT DIGITS
C     OR PLACE
C
C
C     WRITTEN FOR SHFDSS & DSSSHF ROUTINES
C
C
C     ******************************************************************
C
C     SUBROUTINE AUTHOR:  PAUL E. PUGNER
C     CESPK-ED-D RESERVOIR CONTROL SECTION
C     DEVELOPMENT DATE:  01 JAN 90
C
C
C     SUBROUTINE PARAMETERS:
C
C     -----INPUT------
C
C     LFUOUT   -  OUTPUT LOGICAL FILE FOR ERROR MESSAGES & COMMENTS
C     DATA     -  DATA TO BE ROUNDED
C     NDATA    -  NUMBER OF DATA TO BE ROUNDED
C     NSIG     -  NUMBER OF SIGNIFICANT DIGITS TO CARRY
C     IRND     -  PLACE TO ROUND TO (-2 = ROUND TO 1/100 THS)
C
C     -----OUTPUT-----
C
C     DATA     -  THE ROUNDED DATA
C
C
C
      CHARACTER CBUFF*16,FMT1*7,FMT2*7
      LOGICAL LPOPT
C
C
      DATA FMT1 /'(E16.3)'/,FMT2 /'(E16.3)'/
C
C
C
C ESTABLISH NUMBER OF SIGNIFICANT DIGITS
C
      IF ( NSIG.LT.2 ) THEN
         NSIG = 2
         WRITE ( LFUOUT,* ) 'NSIG ADJUSTED TO MIN OF 2'
      ELSE IF ( NSIG.GT.8 ) THEN
         NSIG = 8
         WRITE ( LFUOUT,* ) 'NSIG ADJUSTED TO MAX OF 8'
      ENDIF
C
      IF(IRND.LT.-10) THEN
         IRND=-10
         WRITE ( LFUOUT,* ) 'IRND ADJUSTED TO MIN OF -10'
      ENDIF
C
      FMT1(6:6)=CHAR(NSIG+48)
C ******************************************************************************
C  BEGIN LOOP ON DATA VALUES
C ******************************************************************************
      DO 900 N=1,NDATA
C        CREATE CHARACTER OUTPUT BUFFER
         NDIGIT=NSIG
         WRITE ( CBUFF,FMT1 ) DATA(N)
         READ (CBUFF(14:16),'(I3)') MAG
         MAG = MAG
         NDIGIT = MAG - IRND
         IF(LPOPT('D'))THEN
            WRITE ( LFUOUT,* ) 'NSIG=',NSIG,'   IRND=',IRND
            WRITE ( LFUOUT,* ) 'MAG= ',MAG,'  NDIGIT=',NDIGIT
            WRITE ( LFUOUT,* ) 'CBUFF...'
            WRITE ( LFUOUT,* )       '12345678901234567890'
            WRITE ( LFUOUT,* ) CBUFF
         ENDIF
         IF ( NDIGIT.GT.NSIG ) THEN
            NDIGIT = NSIG
         ELSE IF ( NDIGIT.GT.0 ) THEN
            FMT2(6:6) = CHAR(NDIGIT+48)
            WRITE ( CBUFF,FMT2 ) DATA(N)
            IF(LPOPT('D'))THEN
               WRITE ( LFUOUT,* ) 'AFTER ROUNDING ...'
               WRITE ( LFUOUT,* ) 'CBUFF...'
               WRITE ( LFUOUT,* )       '12345678901234567890'
               WRITE ( LFUOUT,* ) CBUFF
            ENDIF
         ELSE IF ( NDIGIT.EQ.0 ) THEN
            FMT1(6:6) = '8'
            WRITE ( CBUFF,FMT1 ) DATA(N)
            IDEC = INDEX(CBUFF,'.')
C           IF ( CBUFF(IDEC+1:IDEC+8).LGE.'50000000') THEN              H
            IF ( LGE(CBUFF(IDEC+1:IDEC+8), '50000000')) THEN            M
               CBUFF(IDEC-1:IDEC+8) = '1.00000000'
            ELSE
               CBUFF = '0.0'
            ENDIF
         ELSE
            CBUFF = '0.0'
         ENDIF
         READ ( CBUFF,'(E16.3)') DATA(N)
  900 CONTINUE
      RETURN
      END

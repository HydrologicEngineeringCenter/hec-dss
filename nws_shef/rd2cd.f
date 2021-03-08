      SUBROUTINE RD2CD ( LFNOUT,DATA,NDATA,CDATA,NC,NSIG,RMAG )
C
      CHARACTER CBUFF*16,CDATA(*)*(*),CZERO*30,FMT1*7
      DIMENSION DATA(*)
      INTEGER RMAG,DMAG,NC(*)
C
      DATA FMT1 /'(E16.3)'/
      DATA CZERO /'000000000000000000000000000000'/
C
C ESTABLISH NUMBER OF SIGNIFICANT DIGITS
C
      IF ( NSIG.LT.3 ) THEN
         NSIG = 3
         WRITE ( LFNOUT,* ) 'NSIG ADJUSTED TO MIN OF 3'
      ELSE IF ( NSIG.GT.8 ) THEN
         NSIG = 8
         WRITE ( LFNOUT,* ) 'NSIG ADJUSTED TO MAX OF 8'
      ENDIF
C ******************************************************************************
C  BEGIN LOOP ON DATA VALUES
C ******************************************************************************
      DO 900 N=1,NDATA
C CREATE CHARACTER OUTPUT BUFFER
         NDIGIT=NSIG
         FMT1(6:6)=CHAR(NDIGIT+48)
         WRITE ( CBUFF,FMT1 ) DATA(N)
         READ (CBUFF(14:16),'(I3)') MAG
C     WRITE ( LFNOUT,* ) 'MAG=',MAG
C     WRITE ( LFNOUT,* ) 'CBUFF...'
C     WRITE ( LFNOUT,* )       '12345678901234567890'
C     WRITE ( LFNOUT,* ) CBUFF
         IF ( RMAG.GT.(MAG-NDIGIT)) THEN
            NDIGIT = MAG - RMAG
            IF ( NDIGIT.LT.2 ) NDIGIT = 2
            FMT1(6:6) = CHAR(NDIGIT+48)
            WRITE ( CBUFF,FMT1 ) DATA(N)
            !-----------------------------------------------------!
            ! Don't forget to re-read the exponent because it may !
            ! have changed.                       MDP 28 Feb 2007 !
            !-----------------------------------------------------!
            READ (CBUFF(14:16),'(I3)') MAG
C        WRITE ( LFNOUT,* ) 'AFTER ROUNDING ...'
C        WRITE ( LFNOUT,* ) 'CBUFF...'
C        WRITE ( LFNOUT,* )       '12345678901234567890'
C        WRITE ( LFNOUT,* ) CBUFF
         ENDIF
c
      do 850 j=1,12                                                     M
      if ( CBUFF(j:j+1) .eq. ' .' ) CBUFF(j:j+1) = '0.'                 M
      if ( CBUFF(j:j+2) .eq. ' -.' ) CBUFF(j:j+2) = '-0.'               M
  850 continue                                                          M
c
         IDEC = 12 - NDIGIT
         LDIGIT = 12
         CDATA(N) =' '
         NXTCH = 1
C PICK UP SIGN
      IF ( CBUFF(IDEC-2:IDEC-2).EQ.'-' ) THEN
         CDATA(N)(NXTCH:NXTCH)='-'
         NXTCH=NXTCH+1
      ENDIF
C SHIFT DECIMAL TO LEFT
      IF ( MAG.LT.0 ) THEN
         CDATA(N)(NXTCH:)='0.'
         NXTCH = NXTCH + 2
         NZ = -MAG-1
         IF ( NZ.GT.0 ) THEN
            CDATA(N)(NXTCH:)=CZERO(1:NZ)
            NXTCH = NXTCH + NZ
         ENDIF
         CDATA(N)(NXTCH:)=CBUFF(IDEC-1:IDEC-1)
         NXTCH = NXTCH + 1
         CDATA(N)(NXTCH:)=CBUFF(IDEC+1:LDIGIT)
         NXTCH = NXTCH + NDIGIT
C SHIFT DECIMAL RIGHT
      ELSE IF ( MAG.GT.0 ) THEN
C        IF(CBUFF(IDEC-1:IDEC-1).LGT.'0') THEN                          H
         IF(LGT(CBUFF(IDEC-1:IDEC-1),'0')) THEN                         M
            CDATA(N)(NXTCH:) = CBUFF(IDEC-1:IDEC-1)
            NXTCH = NXTCH + 1
         ENDIF
         NR = MAG
         IF ( NR.LE.NDIGIT ) THEN
            CDATA(N)(NXTCH:)=CBUFF(IDEC+1:IDEC+NR)
            NXTCH = NXTCH + NR
            CDATA(N)(NXTCH:)='.'
            NXTCH = NXTCH + 1
            IF ( NR.LT.NDIGIT ) THEN
               CDATA(N)(NXTCH:)=CBUFF(IDEC+NR+1:LDIGIT)
               NXTCH = NXTCH + NDIGIT - NR
            ENDIF
         ELSE
            CDATA(N)(NXTCH:)=CBUFF(IDEC+1:LDIGIT)
            NXTCH = NXTCH + NDIGIT
            NZ = NR - NDIGIT
            CDATA(N)(NXTCH:)=CZERO(1:NZ)
            NXTCH = NXTCH + NZ
            CDATA(N)(NXTCH:)='.'
            NXTCH = NXTCH + 1
         ENDIF
C NO DECIMAL SHIFT
      ELSE
         CDATA(N)(NXTCH:)=CBUFF(IDEC-1:LDIGIT)
         NXTCH = NXTCH + NDIGIT + 2
      ENDIF
C UPDATE POINTERS
      NC(N) = NXTCH - 1
C STRIP TRAILING ZEROES
   80 CONTINUE
      IF ( CDATA(N)(NC(N):NC(N)).EQ.'0' ) THEN
         NC(N) = NC(N) - 1
         IF ( NC(N).GT.1 ) GO TO 80
      ENDIF
C     WRITE ( 3,* ) 'N=',N,'   NC=',NC(N)
C     WRITE ( LFNOUT,* ) 'N=',N,'   NC=',NC(N)
C     WRITE ( 3,* ) CDATA(N)(1:NC(N))
C     WRITE ( LFNOUT,* ) CDATA(N)(1:NC(N))
  900 CONTINUE
C ******************************************************************************
C  END LOOP ON DATA VALUES
C ******************************************************************************
      RETURN
      END

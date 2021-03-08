      SUBROUTINE ADDSEN ( LINE,TINTL,ISTAT )
C
C      ADD A SENSOR TO THE SENSOR TABLES.  IF A SENSOR IS FOUND OUT OF
C      ORDER, PUT IT IN THE RIGHT PLACE AND SET ISTAT = -1.
C
CADD C.SDCNTL                                                           H
      INCLUDE 'sdcntl.h'                                                MLlg
CADD C.SENSRS                                                           H
      INCLUDE 'sensrs.h'                                                MLlg
C
C     LOCAL VARIABLES
      CHARACTER LINE*(*),CRLF*2
      INTEGER INTGR
C
      CRLF = CHAR(13)//CHAR(10)
      ISTAT = 0
      NSEN = NSEN + 1
      ISEN = NSEN
   15 CONTINUE
C     IF ( ISEN.GT.1.AND.LINE(1:10).LLT.SENSOR(ISEN-1) ) THEN           H
      IF ( ISEN .GT. 1 .AND. LLT( LINE(1:10), SENSOR(ISEN-1) ) ) THEN   M
         ISTAT = -1
         SENSOR(ISEN) = SENSOR(ISEN-1)
         SINTL(ISEN) = SINTL(ISEN-1)
         GROUP(ISEN) = GROUP(ISEN-1)
         LOCAT(ISEN) = LOCAT(ISEN-1)
         CFPRT(ISEN) = CFPRT(ISEN-1)
         UID(ISEN)   = UID(ISEN-1)
         ISEN = ISEN - 1
         GO TO 15
      ENDIF
      SENSOR(ISEN)=LINE(1:10)
      SINTL(ISEN) = TINTL
      GROUP(ISEN) = LINE(17:32)
      IF ( NINDX(LINE(34:49),' ').EQ.0 ) THEN
         LOCAT(ISEN) = LINE(1:8)
      ELSE
         LOCAT(ISEN) = LINE(34:49)
      ENDIF
      CFPRT(ISEN) = LINE(51:66)
      UID(ISEN) = LINE(68:75)
C     CHECK FOR DURACTION CODE
      IF(LINE(77:80).EQ.'    ') THEN
        IERR=-1
      ELSE
        NNN=INTGR(LINE,77,4,IERR)
      ENDIF
      IF(IERR.LT.0.OR.NNN.LT.0.OR.NNN.GE.7000) THEN
       IDURAC(ISEN)=-1
      ELSE
       IDURAC(ISEN)=NNN
      ENDIF
C
c     write (21,*) 'sintl(isen): ', (sintl(i),i=1,9)
      RETURN
C
      END

      SUBROUTINE SDMAIN
C
      INCLUDE 'sdcntl.h'                                                MLlg
      INCLUDE 'shefitc.h'                                               MLlg
      INCLUDE 'sddata.h'                                                MLlg
      INCLUDE 'sensrs.h'                                                MLlg
      INCLUDE 'pstbls.h'                                                MLlg
      INCLUDE 'sdmisc.h'                                                MLlg
C
      CHARACTER GROUP0*16,LOCAT0*16,CPART*25
C     INTEGER SINTL0                                                    H
      INTEGER*4 SINTL0, INTL                                            M
      LOGICAL DEBUG
C
      CHARACTER CPARAM*32
	CHARACTER CUNITS*12
	CHARACTER CTYPE*12
	REAL DFACTOR
	INTEGER ISIGDIG
	INTEGER IROUND
C
      DEBUG = IDEBUG.EQ.1
C
C *****************************************************************************
C     LOOP ON HEADS
C *****************************************************************************
      IF ( DEBUG ) THEN
         WRITE ( LFNOUT,* )' NHEAD=',NHEAD,'  NDATA=',NDATA
         CALL DBGOUT ( LFNOUT,HEAD,IFIRST,ILAST,MAXHD,NHEAD,
     1                  JDATE,MTIME,DATA,DQ,NXTDAT,MAXDAT,NDATA )
      ENDIF
      IHEAD = 1
   10 CONTINUE
      IF ( IOLVL.EQ.1 ) THEN
         WRITE ( LFNOUT,* )
     1      '----------------------------------------------'
         WRITE ( LFNOUT,* ) 'ID= ',HEAD(IHEAD)(1:10)
      ENDIF
      IF ( DEBUG ) WRITE ( LFNOUT,* ) 'HEAD = ',HEAD(IHEAD)
C
C     SORT INTO CHRONOLOGICAL ORDER
      CALL CHRONO ( IFIRST(IHEAD),JDATE,MTIME,NXTDAT )
      IF ( DEBUG ) THEN
         CALL DBGOUT ( LFNOUT,HEAD,IFIRST,ILAST,MAXHD,NHEAD,
     1                  JDATE,MTIME,DATA,DQ,NXTDAT,MAXDAT,NDATA )
      ENDIF
C
C     REMOVE REPEATS OF OBSERVATIONS
      CALL RPTSCR( HEAD(IHEAD)(17:17),IFIRST(IHEAD),JDATE,MTIME,NXTDAT )
C
C     FIND POINTER TO SENSOR IN SENSOR TABLE
      idur=intgr(head(ihead),11,4,ierr)
      CALL GETSEN ( HEAD(IHEAD)(1:10),ISEN,idur )
c     write (*,*) 'head: ', head(ihead)(1:10), isen
      IF ( ISEN.EQ.0 ) THEN
         IF ( IADHOC.EQ.0 ) THEN
            WRITE ( LFNOUT,* )
     1       'INCOMING SENSOR=',HEAD(IHEAD)(1:10),
     2       ' NOT RECOGNIZED ... DATA IGNORED'
            GO TO 900
         ELSE
            GROUP0 = ' '
            LOCAT0 = HEAD(IHEAD)(1:8)
            SINTL0 = 0
         ENDIF
      ELSE
         IF ( CPSWD.EQ.UID(ISEN) ) THEN
            GROUP0 = GROUP(ISEN)
            LOCAT0 = LOCAT(ISEN)
      write (lfnout,*) 'sdmain - isen: ', isen
      write (lfnout,*) 'sdmain - sintl(isen): ', sintl(isen)
            SINTL0 = SINTL(ISEN)
      write (lfnout,*) 'sdmain1 - sintl0: ', sintl0
         ELSE
            WRITE( LFNOUT,* )
     1      'PASSWORD DOES NOT MATCH ... ',HEAD(IHEAD)(1:10),
     2      ' DATA IGNORED'
            GO TO 900
         ENDIF
      ENDIF
      IF ( DEBUG )WRITE ( LFNOUT,* ) ISEN,GROUP0,LOCAT0
c     write (lfnout,*) 'sdmain2 - sintl0: ', sintl0
C
C     FIND POINTER TO PE IN PARAMETER TABLE
      IPAR = 1
   20 CONTINUE
      IF ( HEAD(IHEAD)(9:10).NE.PE(IPAR) ) THEN
         IF ( IPAR.LT.NPAR ) THEN
            IPAR = IPAR + 1
            GO TO 20
         ELSE
	   IF (LDOALL) THEN
            CPARAM = HEAD(IHEAD)(9:10)
	      CUNITS = ' '
	      CTYPE = 'INST-VAL'
	      DFACTOR = 1.0
	      ISIGDIG = -1
	      IROUND = -999
            WRITE ( LFNOUT,* )
     1       'INCOMING PE=',HEAD(IHEAD)(9:10),
     2       ' NOT RECOGNIZED ... USING DEFAULT'
            GO TO 40
	   ELSE
            WRITE ( LFNOUT,* )
     1       'INCOMING PE=',HEAD(IHEAD)(9:10),
     2       ' NOT RECOGNIZED ... IGNORING DATA'
            GO TO 900
         ENDIF
	ENDIF
      ELSE
	   CPARAM = PARAM(IPAR)
	   CUNITS = UNITS(IPAR)
	   CTYPE = TYPE(IPAR)
	   DFACTOR = FACTOR(IPAR)
	   ISIGDIG = NSIG(IPAR)
	   IROUND = IRND(IPAR)
      ENDIF
 40   CONTINUE
      IF ( DEBUG )WRITE ( LFNOUT,* ) 'PARAM =',CPARAM
C
C     DETERMINE DATA INTERVAL
c     write (lfnout,*) 'sdmain3 - sintl0: ', sintl0
c     write (lfnout,*) 'sdmain3 - intle(ihead): ',intle(ihead)
       tempint=sintl0
      IF ( INTLE(IHEAD).GT.0.and.intle(ihead).ne.999 ) then
         SINTL0 = INTLE(IHEAD)
      else if(intle(ihead).eq.999.and.sintl0.le.0) then
       j=ifirst(ihead)
       if(nxtdat(j).gt.0) then
        k=nxtdat(j)
        minu=(jdate(k)-jdate(j))*1440+(mtime(k)-mtime(j))
c       write(lfnout,*)'j,k,jdate,mtime',j,k,jdate(j),mtime(j),
c    .  jdate(k),mtime(k)
c       write(lfnout,*) 'sdmain4 - minu:  ',minu
        sintl0=minu
       endif
      endif
c     write (lfnout,*) 'sdmain4 - head:   ', head(ihead)(11:14)
c     write (lfnout,*) 'sdmain4 - sintl0: ', sintl0
c     write (lfnout,*) 'sdmain4 - type:   ', type(ipar)
c     write (lfnout,*) 'sdmain4 - tstype: ', tstype
c     write (lfnout,*) 'sdmain4 - intl:   ', intl
c     write (lfnout,*) 'sdmain4 - ztype:  ', ztype
c     write (lfnout,*) 'sdmain4 - errmsg: ', errmsg
      CALL GETINT ( HEAD(IHEAD)(11:14),SINTL0,CTYPE,TSTYPE,
     1              INTL,ZTYPE,ERRMSG )
c     check to see if the interval is being forced
      if(lforce.and.tempint.gt.0) then
       intl=tempint
       sintl0=intl
      endif
c     write(lfnout,*)' sintl0 ',sintl0
c     write ( lfnout,* ) 'sdmain4 - intl: ', intl
      IF ( ERRMSG.NE.' ' ) THEN
         WRITE ( LFNOUT,* ) ERRMSG
         GO TO 900
      ENDIF
      IF ( DEBUG )WRITE ( LFNOUT,* ) TSTYPE,UNITS(IPAR),ZTYPE,INTL
      IF ( ISEN.GT.0.AND.CFPRT(ISEN).NE.'                ' ) THEN
         DES = CFPRT(ISEN)
C
C        force f-part to null if "null" in field
C
         IF(CFPRT(ISEN) .EQ. 'NULL' .OR. CFPRT(ISEN) .EQ. 'null')
     .                               DES=' '
C
C
      ELSE IF ( CFDEF(1:5).NE.'     ' ) THEN
         DES = CFDEF
      ELSE
         CALL GETDES ( HEAD(IHEAD)(15:16),DES )
      ENDIF
C
      CPART=CPARAM
      IF ( HEAD(IHEAD)(18:18).NE.'Z' ) THEN
         CALL GETEXT(HEAD(IHEAD)(18:18),CPARAM,CPART,INTL,
     .                TSTYPE,ERRMSG)
         IF ( ERRMSG.NE.' ' ) THEN
            WRITE ( LFNOUT,* ) ERRMSG
            WRITE ( LFNOUT,* ) 'DATA IGNORED'
            GO TO 900
         ENDIF
      ENDIF
C     TIME ZONE SHIFT
      IF ( IDTZ.NE.0 ) THEN
         CALL TZSHFT ( IFIRST(IHEAD),JDATE,MTIME,NXTDAT,IDTZ*60 )
         IF ( DEBUG ) CALL DBGOUT( LFNOUT,HEAD,IFIRST,ILAST,MAXHD,NHEAD,
     1                  JDATE,MTIME,DATA,DQ,NXTDAT,MAXDAT,NDATA )
      ELSE
      ENDIF
      CALL PUTDSS ( GROUP0,LOCAT0,CPART,INTL,TSTYPE,DES,
     .     HEAD(IHEAD)(15:15),CIRBLK,ZTYPE,CUNITS,DFACTOR,ISIGDIG,
     .     IROUND,IFIRST(IHEAD),NXTDAT,DATA,DQ,
     .     JDATE,MTIME,HEAD(IHEAD)(17:17),LFNOUT,IOLVL, 
     .     %VAL(IfltabAddress), NTOTAL )
       NSETS = NSETS + 1
  900 CONTINUE
      IF ( IHEAD.LT.NHEAD ) THEN
         IHEAD = IHEAD + 1
         GO TO 10
      ENDIF
C ******************************************************************************
C END LOOPS ON HEADS
C ******************************************************************************
      RETURN
      END

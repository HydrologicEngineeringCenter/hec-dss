      SUBROUTINE PUTSHF( LFNOUT,LFNSHF,CPATH,
     1  DATA,NDATA,ITIME,INTL,
     2  TZONE,NSIG0,IRND0,PCTS,REPLCE,CUNITS)
C
      INCLUDE 'pstbls.h'
      INCLUDE 'shfids.h'
      INCLUDE 'century.h'
C
      REAL DATA(NDATA)
      INTEGER ITIME(*)
      CHARACTER CUNITS*(*)
C
C DSS DECLARATIONS
      CHARACTER CPATH*(*),PCTS*(*), CTYPE*8
      CHARACTER*64 CA,CB,CC,CD,CE,CF
C
C DECLARATIONS FOR MISC.
      CHARACTER TZONE*(*)      
      PARAMETER (NCBUFF=160)
      CHARACTER PC*7,STAID*8,DVCODE*5,DYCODE*12,DICODE*5,CBUFF*(NCBUFF),
     &   CBUF2*80,SFMT*3, cdote*80, csep*1
	 CHARACTER CSDT*10
      PARAMETER (MAXCD=100)
      CHARACTER CDATA(MAXCD)*10,CTIME(MAXCD)*12
      INTEGER RMAG,NCH(MAXCD),NCT(MAXCD)
      LOGICAL LF,LPOPT,AFMT,EFMT,REPLCE
C
      INTEGER INTL,  JULF, jul                                   
      integer icen
      character*14 ctemp
c
c	LFNOUT - unit connecte to standard out, for messages
c	LFNSHF - unit connected to the shef output data file
C	CPAHT, NPATH - DSS pathname and length
C	DATA, NDATA - data to write in shef format and number
C	ITIME - minutes since 01Jan1900
C	INTL - time interval, in minutes
C	TZON
C	NSIGO
C	IRNDO	
C	PCTS
C	REPLCE
C	CUNITS - Units of the data
C
      CSDT = ' '
      ccen = 'YES'
	CALL CURTIM(J, M)
	CALL J2SDT ( J,M,CSDT,icen)
*      if(ccen.eq.'YES') then
        ctemp=csdt
        csdt(3:)=ctemp(1:8)
        write(csdt(1:2),'(I2)')icen
*       endif
C
      CALL CHRLNB(CPATH, NPATH)
C
      CALL ZUFPN ( CA,NA,CB,NB,CC,NC,CD,ND,CE,NE,CF,NF,
     1        CPATH,NPATH,ISTAT )    
C
      JULF = ITIME(1)/1440
      IFTIME = ITIME(1) - (JULF * 1440)
      IF (IFTIME.EQ.0) THEN
	   JULF = JULF - 1
		IFTIME = 1440
	ENDIF
C
C     DETERMINE SHEF FORMAT
      IF ( CE(1:2).EQ.'IR'.OR.NDATA.EQ.1 ) THEN
         SFMT='.A '
         AFMT = .TRUE.
      ELSE
         SFMT='.E '
         AFMT = .FALSE.
      ENDIF
CT(3:3)='R'
C
      WRITE(LFNOUT,*)'REPLCE=',REPLCE,' SFMT=',SFMT
C
      EFMT = .NOT.AFMT
C     FIND 8-CHAR STATION ID
      CALL BSRCH ( CB,DSSID,NSTA,ISTA )
      IF ( ISTA.EQ.0 ) THEN
         STAID = CB(1:8)
      ELSE
         STAID = SHEFID(ISTA)
      ENDIF
      LCOUNT = 1
C
      CBUFF=SFMT
      INXT = 4
      CALL APLNB ( CBUFF,INXT,NCBUFF,' ',STAID,8,ISTAT )
      if(ccen.eq.'YES') then
      CALL APLNB ( CBUFF,INXT,NCBUFF,' ',CSDT,8,ISTAT )
      else
      CALL APLNB ( CBUFF,INXT,NCBUFF,' ',CSDT,6,ISTAT )
      endif
      CALL APLNB ( CBUFF,INXT,NCBUFF,' ',TZONE,3,ISTAT )
C SHEF STARTING DATE AND TIME
      DYCODE = 'DY'
      CALL J2SDT ( JULF,IFTIME,DYCODE(3:12),icen)
      idycode = inxt + 1
      nn=12
      ctemp=dycode(1:12)
      if(ccen.eq.'YES') then
        nn=14
        write(ctemp(3:4),'(I2)')icen
        ctemp(1:2)='DT'
        ctemp(5:)=dycode(3:12)
       endif
c      CALL APLNB ( CBUFF,INXT,NCBUFF,' ',DYCODE,12,ISTAT )
       CALL APLNB ( CBUFF,INXT,NCBUFF,' ',ctemp,nn,ISTAT )
C SHEF PARAMETER CODE
      PC = '       '
      CALL LSRCH ( CC,PARAM,NPAR,IPAR )
      IF ( IPAR.EQ.0 ) THEN
         PC(1:2) = CC(1:2)
         FAC = 1.
         NSIGU = NSIG0
         IRNDU = IRND0
      ELSE
         PC(1:2) = PE(IPAR)
         FAC = FACTOR(IPAR)
         IF ( NSIG(IPAR).LT.0 ) THEN
            NSIGU = NSIG0
         ELSE
            NSIGU = NSIG(IPAR)
         ENDIF
         IF ( IRND(IPAR).LT.IRND0) THEN
            IRNDU = IRND0
         ELSE
            IRNDU = IRND(IPAR)
         ENDIF
      ENDIF
C SHEF DURATION CODE
          DVCODE = 'DV'
      CALL SHFDUR ( INTL,CTYPE,PC,DVCODE )
      IF ( PC(3:3).EQ.'X' ) THEN
         WRITE ( LFNOUT,* ) '***CAUTION:  DURATION CODE NOT DEFINED***'
      ELSE IF ( PC(3:3).EQ.'V' ) THEN
         CALL APLNB ( CBUFF,INXT,NCBUFF,'/',DVCODE,5,ISTAT )
      ENDIF
      IF ( PCTS.NE.'  ' ) THEN
         IF ( PC(3:3).EQ.' ' ) PC(3:3) = 'Z'
         PC(4:5) = PCTS
      ENDIF
C     WRITE ( 34,* ) 'PC=',PC,'   FAC=',FAC,'   NSIG=',NSIG0,'   IRND=',
C    1                IRND0
C APPLY SCALING FACTOR
      IF ( ABS(FAC-1.).GT.1.E-6 ) THEN
         DO 10 N=1,NDATA
            IF ( DATA(N).GT.-900.95.OR.DATA(N).LT.-910.05 ) THEN
               DATA(N) =  DATA(N) / FAC
            ENDIF
  10     CONTINUE
         IRNDU = IRNDU - MAGN(FAC)
C         WRITE ( 3,* ) 'FAC=',FAC,'   IRNDU=',IRNDU
          MAGX = MAGN(FAC)
C         WRITE ( 3,* ) 'MAGX=',MAGX
      ENDIF
C SHEF TIME INTERVAL CODE
      IF ( EFMT ) THEN
         CALL APLNB ( CBUFF,INXT,NCBUFF,'/',PC,7,ISTAT )
         DICODE='DIHHH'
         CALL SHFDI ( INTL,DICODE )
         CALL APLNB ( CBUFF,INXT,NCBUFF,'/',DICODE,5,ISTAT )
C Save the beginning of the .E line for use with additional
C lines when continuation is not used.  Date will by changed.
         if ( lpopt('J') ) then
            ndote = inxt - 1
            cdote = cbuff(1:ndote)
         endif
      ENDIF
C LOAD THE OUTPUT CHARACTER DATA BUFFER
      N1 = 1
      LDATA = NDATA
      NCD=MAXCD
      ntot = 0
   20 CONTINUE
C       WRITE ( 34,* ) 'N1=',N1,'   NCD=',NCD,'   LDATA=',
C    1   LDATA
      IF ( NCD.GT.LDATA ) NCD = LDATA
      CALL RD2CD ( LFNOUT,DATA(N1),NCD,CDATA,NCH,NSIGU,IRNDU )
      IF ( AFMT ) THEN
          CALL RT2CT ( LFNOUT,ITIME(N1),NCD,CTIME,NCT )
          IF ( N1.EQ.1 ) THEN
              CTIME(1) = DYCODE
              NCT(1) = 12
          ENDIF
C         WRITE ( 34,'(7A14)') (CTIME(I),I=1,NCD )
      ENDIF
      CALL CKMISS ( DATA(N1),CDATA,NCH,NCD )
C     WRITE ( 34,'(10(1X,A))') (CDATA(I),I=1,NCD)
C     WRITE ( 34,'(   10I10))') (NCH(I),I=1,NCD)
C WRITE THE OUTPUT BUFFER TO THE OUTPUT FILE
      DO 30 N=1,NCD
         ntot = ntot + 1
         IF ( AFMT ) THEN
            IF ( N1.NE.1.OR.(N1.EQ.1.AND.N.GT.1)) THEN
               CALL APLNB( CBUFF,INXT,NCBUFF,'/',CTIME(N),NCT(N),
     1                     ISTAT )
            ENDIF
            CALL APLNB( CBUFF,INXT,NCBUFF,'/',PC,7,ISTAT )
            CALL APLNB( CBUFF,INXT,NCBUFF,' ',CDATA(N),NCH(N),ISTAT )
         ELSE IF ( EFMT ) THEN
            CALL APLNB( CBUFF,INXT,NCBUFF,'/',CDATA(N),NCH(N),ISTAT )
         ENDIF
         IF ( INXT.GT.80 ) THEN
C           FIND THE LAST '/' BEFORE 80TH COLUMN
            IP = 80
   25       CONTINUE
            IF ( IP.GT.1.AND.CBUFF(IP:IP).NE.'/' )THEN
               IP = IP - 1
               GO TO 25
            ENDIF
            WRITE ( LFNSHF,'(A)') CBUFF(1:IP-1)
            NMOV = INXT-IP-1
            CBUF2 = CBUFF(IP+1:INXT-1)
            CBUFF = SFMT
            WRITE ( CBUFF(3:3),'(I1)') LCOUNT
C Place saved beginning of .E line in buffer, then update time
            inext = 4
            if ( lpopt('J') .and. efmt ) then
               cbuff(1:len(cdote)) = cdote(1:ndote)
               idum = inctim(intl,0,ntot-1,julf,iftime,jul,itime)
               if(ccen.eq.'YES') then
                 call j2sdt(jul,itime,cbuff(idycode+4:idycode+13),i)
                 write (cbuff(idycode+2:idycode+3), '(I2.2)') i
               else
                 call j2sdt (jul, itime, cbuff(idycode+2:idycode+11),i)
               endif
               inext = ndote + 1
               csep = '/'
            else
               csep = ' '
            endif
            CBUFF(inext:NMOV+inext)= csep // CBUF2(1:NMOV)
            INXT = NMOV + inext + 1
            LCOUNT = MOD(LCOUNT,9) + 1
         ENDIF
  30  CONTINUE
      LDATA = LDATA - NCD
      N1 = N1 + NCD
      IF ( LDATA.GT.0 ) GO TO 20
      IF ( INXT.GT.5 ) THEN         
      WRITE ( LFNSHF,'(A)') CBUFF
      ENDIF
      WRITE ( LFNOUT,800 ) CPATH,STAID,PC,NSIGU,IRNDU,NDATA
  800 FORMAT ( 1X,A/' SHEFID=',A,'  PC=',A,'  NO. SIG. DIG.=',I1,
     1         '  ROUND TO 10**',I3/I5,' DATA VALUES')
      RETURN
      END

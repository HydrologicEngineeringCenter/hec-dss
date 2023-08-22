      SUBROUTINE PUTDSS ( GROUP,LOCAT,PARAM,INTL,TSTYPE,DES,CFSCT,
     . CIRBLK,ZTYPE,UNITS,FACTOR,NSIG,IRND,IFIRST,NXTDAT,DATA,DQ,
     . JDATE,MTIME,IREV,LFNOUT,IOLVL,IFLTAB, NTOTAL )
C
C     PUTDSS ASSEMBLES THE PARTICULAR DATA NEEDED TO WRITE TO DSS
C     CALLS THE APPROPRIATE DSS TIME-SERIES ENTRY ROUTINE TO
C     POST THE DATA TO THE DSS FILE.
C
C     EXTERNAL VARIABLES
      CHARACTER GROUP*(*),LOCAT*(*),TSTYPE*(*),DES*(*),CIRBLK*(*),IREV*1
      CHARACTER DQ(*)*(*),PARAM*(*),ZTYPE*(*),UNITS*(*), CFSCT*(*)
      INTEGER  NXTDAT(*),MTIME(*)
C     INTEGER JDATE(*)                                                  H
      INTEGER*4 JDATE(*), JULS, JULE, INTL                              M
      INTEGER*4 NPERS, NOPERS, inctim, idummy, NVALS
      REAL DATA(*)
C
C     LOCAL VARIABLES
      CHARACTER CBUFF*80
      PARAMETER ( MAXBLK=3000 )
      INTEGER IHEAD(40)
      CHARACTER CPATH*80,CD*32,CE*32
      DIMENSION BUFFER(2*MAXBLK+100),ZDATA(MAXBLK),ZTIME(MAXBLK)
      LOGICAL LPOPT,DEBUG
      character alpha(16)
      integer   mlvl
C
C
C ****************************************************************************
C INITIALIZATION
C ****************************************************************************
C
      call zquery("MLVL", alpha, mlvl)
      DEBUG = (LPOPT('D').and.(mlvl.gt.0))
C
C     COMMON PATHNAME PARTS
      CALL LFLNB( GROUP,1,LEN(GROUP),IBEG,NA )
      IF ( NA.EQ.0 ) THEN
         NA = 1
      ELSE IF (IBEG.GT.1) THEN
         CBUFF = GROUP
         GROUP = ' '
         GROUP(1:NA) = CBUFF(IBEG:IBEG+NA-1)
      ELSE
      ENDIF
      CALL LFLNB( LOCAT,1,LEN(LOCAT),IBEG,NB )
      IF ( NB.EQ.0 ) THEN
         NB = 1
      ELSE IF (IBEG.GT.1) THEN
         CBUFF = LOCAT
         LOCAT = ' '
         LOCAT(1:NB) = CBUFF(IBEG:IBEG+NB-1)
      ELSE
      ENDIF
      CALL LFLNB( PARAM,1,LEN(PARAM),IBEG,NC )
      IF ( NC.EQ.0 ) THEN
         NC = 1
      ELSE IF (IBEG.GT.1) THEN
         CBUFF = PARAM
          PARAM =  ' '
         PARAM(1:NC) = CBUFF(IBEG:IBEG+NC-1)
      ELSE
      ENDIF
      CALL LFLNB( DES,1,LEN(DES),IBEG,NF )
      IF ( NF.EQ.0 ) THEN
         NF = 1
      ELSE IF (IBEG.GT.1) THEN
         CBUFF = DES
         DES = ' '
         DES(1:NF) = CBUFF(IBEG:IBEG+NF-1)
      ELSE
      ENDIF
C
      IF (DES(1:1).EQ.'*') THEN
         CALL SHSAVK('G',0,KYR,KMO,KDA,KHR,KMN,KSE)
         KMN = 0
         WRITE (DES, 9) CFSCT,KYR,KMO,KDA,KHR,KMN
 9       FORMAT(A,': ',I4.4, '.'I2.2,'.',I2.2,'-',2I2.2)
         CALL CHRLNB(DES, NF)
      ENDIF      
C
C     REGULAR INTERVAL TIME-SERIES DATA
      IF ( TSTYPE.EQ.'RG' ) THEN
         J = IFIRST
   10    CONTINUE
         ncount=0
         DO 20 I=1,MAXBLK
            ZDATA(I) = -901.
   20    CONTINUE
         ISTIME = MTIME(J)
         JULS   = JDATE(J)
         IF (DEBUG) WRITE ( LFNOUT,* ) 'JULS =',JULS,'  ISTIME =',ISTIME
   30    CONTINUE
         IF (DEBUG) WRITE ( LFNOUT,* ) 'J =',J,'  JDATE =',JDATE(J),
     1   '   MTIME = ',MTIME(J)
c        write (lfnout,*) 'putdss - intl:     ', intl
         NPERS = NOPERS ( INTL,0,JULS,ISTIME,JDATE(J),MTIME(J) )
         if(debug)write (lfnout,*) 'npers: ', npers
         IF ( NPERS.LT.0 ) THEN
            WRITE ( LFNOUT,*) '*** ERROR *** DATA OUT OF ORDER'
            CBUFF = '    OBSERVATION ON           AT      HRS '
            CALL JULDAT (JDATE(J), 104, CBUFF(20:28), N)
            MI = M2IHM ( MTIME(J), CBUFF(33:36) )
            WRITE ( LFNOUT,* ) CBUFF
            GO TO 999
         ENDIF
         NVALS = 1 + NPERS
         ncount=ncount+1
         if(nvals.lt.ncount) nvals=ncount
         ncount=nvals
         IF ( DEBUG ) WRITE ( LFNOUT,* ) 'NVALS =',NVALS
         IF ( NVALS.LE.MAXBLK ) THEN
            IF ((NSIG.NE.-1).AND.(IRND.NE.-999)) THEN
               CALL ROUND (LFNOUT,DATA(J),1,NSIG,IRND)
            ENDIF
            IF ( ABS(DATA(J)+9999.).GT.1.0E-5 ) THEN
               ZDATA(NVALS) = DATA(J) * FACTOR
            ELSE
            ENDIF
            IETIME = MTIME(J)
            JULE   = JDATE(J)
            J = NXTDAT(J)
            NDATA = NVALS
         ENDIF
         IF ( NVALS.GE.MAXBLK.OR.J.EQ.0 ) THEN
            IF (DEBUG)WRITE (LFNOUT,* ) 'NDATA=',NDATA,'  JULE =',JULE,
     1                 '  IETIME=',IETIME
            IF ( DEBUG ) WRITE (LFNOUT,'(8F10.1)') (ZDATA(I),I=1,NDATA)
            NDATA = MAXBLK
            IF ( IREV.EQ.'R'.OR.LPOPT('R') ) THEN
               ISTAT = 0
            ELSE
               ISTAT = -1
            ENDIF
c
            idum = 0
            dum1 = 0.
            dum2 = 0.
c
c     write (*,*) ' '
c     write (*,*) 'juls:   ', juls
c     write (*,*) 'jule:   ', jule
c     write (*,*) 'istime: ', istime
c     write (*,*) 'ietime: ', ietime
c     write (*,*) 'intl:   ', intl
c     write (*,*) 'ndata:  ', ndata
c
c     do 47 ic=1,3
c        write (*,*) 'zdata(',ic,'): ', zdata(ic)
c  47 continue
c
c     call zset ( 'MLVL', ' ', 9 )
c
            IDUM = 0
      iflag=0
      if(j.eq.0) then
        idummy=inctim(intl,iflag,ncount-1,juls,istime,jule,ietime)
      endif
      if(debug) then
      write(lfnout,*)'j ncount nvals ndata',j,ncount,nvals,ndata
      write(lfnout,*)'juls,istime,jule,ietime',juls,istime,jule,ietime
      endif
            CALL ZPTDTS ( IFLTAB, GROUP, NA, LOCAT, NB, PARAM, NC,
     .                    DES, NF, JULS, ISTIME, JULE, IETIME, INTL,
     .                    DUM1, IDUM, DUM2, ZDATA, NDATA, UNITS,
     .                    ZTYPE, ISTAT )
c
            IF ( ISTAT.GE.10 ) THEN
               WRITE ( LFNOUT,* ) '*** COULD NOT WRITE TO DSS ',
     1            ' DSS ERROR CODE =', ISTAT
            ELSE IF ( ISTAT.GT.0.AND.IOLVL.EQ.1 ) THEN
               WRITE ( LFNOUT,* ) '*** WARNING -- ',
     1         ' DSS ERROR CODE =', ISTAT
            ENDIF
            NTOTAL =NTOTAL + NDATA
         ELSE
            GO TO 30
         ENDIF
         IF ( J.GT.0 ) GO TO 10
C
C     IRREGULAR INTERVAL DATA
C
      ELSE IF ( TSTYPE.EQ.'IR' ) THEN
         NVALS = 1
         J = IFIRST
         BDATE = FLOAT(JDATE(J))
         JULS  = JDATE(J)
            IF (DEBUG) WRITE (LFNOUT,* ) 'JULS =', JULS, '  BDATE = ',
     1     BDATE
   50    CONTINUE
         IF ((NSIG.NE.-1).AND.(IRND.NE.-999)) THEN
            CALL ROUND(LFNOUT,DATA(J),1,NSIG,IRND)
         ENDIF
         IF ( DEBUG ) WRITE (LFNOUT,* ) J,DATA(J)
         IF ( ABS(DATA(J)+9999.).GT.1.0E-5 ) THEN
            ZDATA(NVALS) = DATA(J) * FACTOR
         ELSE
            ZDATA(NVALS) = -901.
         ENDIF
         ZTIME(NVALS)=FLOAT(JDATE(J)-JULS)+FLOAT(MTIME(J))*.0006944444
C        IF ( DEBUG ) WRITE ( LFNOUT,* ) N,ZDATA(N),ZTIME(N)
         IF ( NVALS.LT.MAXBLK.AND.NXTDAT(J).GT.0 ) THEN
            J = NXTDAT(J)
            NVALS = NVALS + 1
            GO TO 50
         ELSE
         ENDIF
            IF ( IREV.EQ.'R'.OR.LPOPT('R') ) THEN
               INFLAG = 1
            ELSE
               INFLAG = 0
            ENDIF
            IF ( DEBUG ) WRITE (LFNOUT,* ) 'INFLAG =',INFLAG
            CALL JULDAT (JULS, 104, CD, N)
            CD(1:2)='01'
            IF ( DEBUG ) WRITE ( LFNOUT,'(1X,20A4)') CD
            CE = CIRBLK
            CALL LFLNB( CIRBLK,1,10,IE,NE )
            IF ( DEBUG ) WRITE ( LFNOUT,'(1X,20A4)') CE
         CPATH=' '
         CALL ZFPN ( GROUP,NA,LOCAT,NB,PARAM,NC,CD,9,CE,NE,DES,NF,
     1               CPATH,NPATH )
         IF ( DEBUG ) WRITE (LFNOUT,'(1X,20A4)') CPATH
         IF ( DEBUG ) WRITE ( LFNOUT,'(1X,4(F10.5,F10.2 ))') ( ZTIME(I),
     1      ZDATA(I),I = 1,NVALS )
         NDATA = MAXBLK
         NBUFF = 2*MAXBLK+100
         IT = 0
         CALL ZPIRTS ( IFLTAB,CPATH,NPATH,BUFFER,NBUFF,IHEAD,IT,ZTIME,
     1   ZDATA,NVALS,BDATE,UNITS,ZTYPE,INFLAG,ISTAT )
         IF ( ISTAT.GE.10 ) THEN
            WRITE ( LFNOUT,* ) '*** COULD NOT WRITE TO DSS ',
     1         ' DSS ERROR CODE =', ISTAT
         ELSE IF ( ISTAT.GT.0.AND.IOLVL.EQ.1 ) THEN
            WRITE ( LFNOUT,* ) '*** WARNING -- ',
     1         ' DSS ERROR CODE =', ISTAT
         ELSE
         NTOTAL = NTOTAL + NVALS
         ENDIF
            IF ( NXTDAT(J).GT.0 ) THEN
               NVALS = 1
               J = NXTDAT(J)
               GO TO 50
            ELSE
            ENDIF
      ELSE
      ENDIF
C
  999 CONTINUE
      RETURN
C
      END

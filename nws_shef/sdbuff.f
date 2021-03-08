      SUBROUTINE SDBUFF ( IDSTN,NYEAR,NMON,NDAY,NHOUR,NMIN,
     1                        NSEC,KYEAR,KMON,KDAY,KHOUR,KMIN,KSEC,
     2                        KODP,KODE,IDUR,KODT,KODS,KODEX,CODP,
     3                        VALUE,LWAL,IREV,MSRCE,KINTL )
C
C     SUBROUTINE SDBUFF ASSEMBLES DATA IN THE DSS OUTPUT BUFFER
C     IN AN  FORM FOR PROCESSING AND LOADING INTO DSS.
C                   DENNIS HUFF    22JAN88
C
CADD C.SDCNTL                                                           H
      INCLUDE 'sdcntl.h'                                                MLlg
CADD C.SDDATA                                                           H
      INCLUDE 'sddata.h'                                                MLlg
C
      DOUBLE PRECISION VALUE
c     DIMENSION IDSTN(8),MSRCE(8)
      character*8 idstn,msrce
      character*1 kodp,kode,kodt,kods,kodex,lwal
C     INTEGER   IFACT(7)                                                H
      INTEGER*4 IFACT(7)                                                M
      CHARACTER THEAD*18
      INTEGER*4 IYMDJL                                                  M
C
      DATA IFACT /    1,  60, 1440, 43200, 525600,   -1,    -1 /
C
c     open(unit=95,file='debug.out')
c     write(95,*)' ksec ',ksec
c     write(95,*)'idstn nyear nmon nday nhour nmin nsec'
c     write(95,'(a8,9i5)')idstn,nyear,nmon,nday,nhour,nmin,nsec
c     write(95,*)'kyear,kmon,kday,khour,kmin,ksec'
c     write(95,*) kyear,kmon,kday,khour,kmin,ksec
c     write(95,*)'kodp,kode,idur,kodt,kods,kodex,codp'
c     write(95,'(2a2,i5,3a2,f8.2)') kodp,kode,idur,kodt,kods,kodex,codp
c     write(95,*)'value,lwal,irev,msrce,kintl'
c     write(95,'(f8.2,a21,i2,a8,i5)') value,lwal,irev,msrce,kintl
c     write(95,*)'-----------------------------------------------'
c     WRITE ( THEAD(1:8),'(8A1)') IDSTN
      thead(1:8)=idstn
c     WRITE (THEAD(9:9),'(A1)') KODP
      thead(9:9)=kodp
c     WRITE (THEAD(10:10),'(A1)') KODE
      thead(10:10)=kode
C     CALL INTC9 ( IDUR,THEAD,11,4 )
      WRITE ( THEAD(11:14),'(I4)') IDUR
c     WRITE (THEAD(15:15),'(A1)') KODT
      thead(15:15)=kodt
c     WRITE (THEAD(16:16),'(A1)') KODS
      thead(16:16)=kods
      IF ( IREV.EQ.1 ) THEN
         THEAD(17:17) = 'R'
      ELSE
         THEAD(17:17) = ' '
      ENDIF
c     WRITE (THEAD(18:18),'(A1)') KODEX
      thead(18:18)=kodex
C     NOTE:   PROBABILILTY CODES IGNORED, AND MESSAGE
C              SOURCE ID IS IGNORED.  WHETHER THESE WOULD BE
C              CONSISTANTLY USED OR HOW THEY WOULD BE USED BY
C              DISSEMINATING AGENCIES AND OFFICES IS NOT CLEAR.
C              THESE COULD BE IMPLEMENTED AS PART OF THE THEAD
C              LATER.
c     JDATE(NDATA+1) = IYMDJL ( NYEAR+1900,NMON,NDAY )
      JDATE(NDATA+1) = IYMDJL ( NYEAR,NMON,NDAY )
      MTIME(NDATA+1) = NHOUR*60 + NMIN
      DATA(NDATA+1) = VALUE
c     WRITE (DQ(NDATA+1),'(A1)')  LWAL
      dq(ndata+1)=lwal
C     UPDATE HEADS AND RELATED POINTERS
      NDATA = NDATA + 1
      NXTDAT(NDATA) = 0
      IHEAD = 1
   30 IF ( IHEAD.LE.NHEAD ) THEN
         IF ( THEAD.EQ.HEAD(IHEAD) ) THEN
            NXTDAT(ILAST(IHEAD)) = NDATA
            ILAST(IHEAD) = NDATA
         ELSE
            IHEAD = IHEAD + 1
            GO TO 30
         ENDIF
      ELSE
         NHEAD = NHEAD + 1
         HEAD(NHEAD) = THEAD
         IFIRST(NHEAD) = NDATA
         ILAST(NHEAD) = NDATA
c        write (21,*) 'sdbuff - kintl: ', kintl
         IF ( KINTL.GT.0 ) THEN
            IUNITS = 1 + KINTL / 1000
c           write (21,*) 'sdbuff - mod:   ', mod(kintl,1000)
c           write (21,*) 'sdbuff - ifact: ', ifact(iunits)
            INTLE(NHEAD) = MOD(KINTL,1000) * IFACT(IUNITS)
c           write (21,*) 'sdbuff - intle: ', intle(nhead)
c           write (21,*) ' '
         else                                                           aem
            intle(nhead) = 0                                            aem
c           write(lfnout,*)' kintl is ',kintl
c           write(lfnout,*)' iunits ',iunits
c           write(lfnout,*)' intle(nhead) ',intle(nhead)
         ENDIF
      ENDIF
      IF ( NDATA.GE.MAXDAT.OR.NHEAD.GE.MAXHD ) THEN
c        write (21,*) 'sdbuff - intle: ', (intle(jj),jj=1,10)
         CALL SDMAIN
         NDATA = 0
         HEAD(1)=' '
         NHEAD = 0
      ENDIF
  999 CONTINUE
      RETURN
      END

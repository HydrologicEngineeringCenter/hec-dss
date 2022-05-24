      SUBROUTINE RUNSHEFDSS(IFLTAB, ILUNIT, CINPUT, CSHEFPARM, 
     * CSENSOR, CPARAM, LSTOREALL, NTOTAL1, NSETS1, ISTATUS)
C
C     SHFDSS READS DATA FROM A FILE CREATED BY THE NWS SHEF
C     DECODER "SYS*SHEF" AND POSTS THE DATA TO A DSS FILE
C     WITH THE NAMES, UNITS, ETC., EQUIVALENT IN DSS.
C
C     VERSION 2.x.x -- combines SHFDSS VERSION 1.x.x and
C     SHEFRD into one program.  Begin tracking 2.x.x code
C     with version 2.0.1.  Last of 1.x.x source code in
C     file 2000SHEF*O.SHFDSS.
C                                 Dennis Huff   4/5/88
C
C     Clean up LODPAR routine & make common with DSSSHF &
C     add IRND & NSIG parameters from SHFDSSP file.
C               Version 2.2.0    Paul E. Pugner 01/01/90
C
*      USE IFPORT 
C
      INCLUDE 'sdcntl.h'                                                MLlg
      INCLUDE 'shefitc.h'                                               MLlg
	INCLUDE 'sdmisc.h'
	INCLUDE 'sddata.h' 
C
      COMMON /LUNS/   LCHN,JCHN,KCHN,MCHN,MREC,ICHER
C
      INTEGER IFLTAB(*)
	CHARACTER CINPUT*(*)
	CHARACTER CSHEFPARM*(*)
	CHARACTER CSENSOR*(*)
	CHARACTER CPARAM*(*)
	LOGICAL LSTOREALL
	INTEGER ISTATUS,NTOTAL1,NSETS1
C
C
      CHARACTER*80 CIFILE,COFILE,CDOTB
c     CHARACTER*80 CDSFIL,CSFILE,CPFILE,CPEFIL
      CHARACTER*256 CDSFIL,CSFILE,CPFILE
      CHARACTER CVERNO*10,CVERDT*9,DATE*7,TIME*8,CDTZ*4
      LOGICAL LPOPT
C
c     COMMONT /CCENTURY/ CCENT,LCCENT
c     CHARACTER CCENT*6
c     LOGICAL LCCENT
      character fsh*132,cerror*132,CSHEFIT*132,cforce*8
      logical exs
c     DATA LCCENT /.FALSE./
C
      DATA CVERNO /'2.0.02'/,CVERDT/'11DEC2009'/
C
      CIRBLK = 'IR-'
      LFNOUT = ILUNIT
      LCHN = 20
      KCHN = 7
      MCHN = 19
      LUOUT = 11
      LUPARM = 12
      LUERR = ILUNIT
c     LUCPY = 13
      LUCPY = -1
      ICHER = LFNOUT
      fsh=' '
      ishefitc=99
      lforce=.false.
      NTOTAL = 0
      NSETS = 0
      NDATA = 0
      HEAD(1)=' '
      NHEAD = 0

c
      IfltabAddress = LOC(IFLTAB)
	LDOALL = LSTOREALL
C
C
C
C
C
C
*      CALL ATTSET( 'SHEFDSS '//' '//cverno//'   '//CVERDT )
*	CALL ATTACH( LCHN,'INPUT', 'tempin1', ' ', CIFILE, ISTAT )
*      CALL ATTACH( LCHN,'INPUT', 'STDIN', ' ', CIFILE, ISTAT )
*      CALL ATTACH( LFNOUT, 'OUTPUT', 'STDOUT', ' ', COFILE, ISTAT )
*      CALL ATTACH( MCHN,'DOTB','SCRATCH','A=S,F=U,P=N,S=S',CDOTB,ISTAT ) M
*      CALL ATTACH( 999,'DSSFILE','  ','NOP',CDSFIL,ISTAT )
*      CALL ATTACH( 999,'PFILE','shfdssp','NOP',CPFILE,ISTAT )           M
*      CALL ATTACH( 999,'SFILE','shfdsss','NOP',CSFILE,ISTAT )           M
*      CALL ATTACH( 999,'PEFILE','-NONE-','NOP',CPEFIL,ISTAT )
*      CALL ATTACH( 999,'CENTURY','-NONE-','NOP',CCENT,ISTAT )
*       CALL ATTACH( 999,'DTZ',' ','NOP',CDTZ,ISTAT )
*       CALL ATTACH( 999,'FPART',' ','NOP',CFDEF,ISTAT )
*       CALL ATTACH( 999,'IRBLK','MONTH  ','NOP',CIRBLK(4:10),ISTAT )
*       CALL ATTACH( 999,'PSWD','        ','NOP',CPSWD,ISTAT )
c     CALL ATTACH( 999,'SHEFPARM','rfs_sys_dir','NOP',fsh,ISTAT )
c      CALL ATTACH( 999,'SHEFPARM','./SHEFPARM',
c     .'NOP',fsh,ISTAT )
*      CALL ATTACH( 999,'ERRORLOG','Error.log','NOP',cerror,ISTAT )
*      CALL ATTACH( 999,'SHEFIT','-NONE-','NOP',CSHEFIT,ISTAT )
*      CALL ATTACH( 999,'FORCE','NO','NOP',Cforce,ISTAT )
*      CALL ATTEND
*****************************
      Cforce = 'YES'
      CSHEFIT = '-NONE-'
      CPSWD = '   '
      CIRBLK(4:10) = 'MONTH  '
      CFDEF = ' '
      CDTZ = ' '
	OPEN (UNIT=MCHN, STATUS='SCRATCH', FORM='UNFORMATTED') 
*****************************
      fsh = CSHEFPARM 
      CPFILE = CPARAM
      CSFILE = CSENSOR
	CIFILE = CINPUT
	COFILE = ''
C
*      write(*,*) fsh
* 	write(*,*) CPFILE
* 	write(*,*) CSFILE
* 	write(*,*) CINPUT
*	read (*,*)ncerr
*****************************
	OPEN (UNIT=LCHN, FILE=CINPUT)

c
      if(cforce(1:1).eq.'y'.or.cforce(1:1).eq.'Y') lforce=.true.
c         OPEN(LUout,FILE='shefout',IOSTAT=IC,STATUS='OLD')
c         IF (IC.ne.0) OPEN(LUOUT,FILE='shefout',IOSTAT=IC,
c    $                      FORM='UNFORMATTED',STATUS='NEW')
c
c       open(unit=95,file='debug.out')
        call chrlnb(fsh,nfsh)
        if(fsh(1:nfsh).eq.'rfs_sys_dir') then
          FSH = 'SHEFPARM'
          nfsh=8
        endif
        INQUIRE(FILE=FSH(1:nfsh),IOSTAT=IE,EXIST=EXS)
        IF (IE.EQ.0 .AND. .NOT.EXS) THEN
           write(luerr,*)' Fatal - Error: Unable to open SHEFPARM'
           write(luerr,*)' Aborting '
           go to 900
        else
          open(unit=LUPARM,file=FSH(1:nfsh))
        endif
c     IF(CCENT(1:6).NE.'-NONE-') LCCENT=.TRUE.
C
      IF(CSHEFIT.NE.'-NONE-') THEN
       lshefitc=.true.
       call chrlnb(cshefit,ncshefit)
       open(unit=ishefitc,file=cshefit(1:ncshefit))
      else
       lshefitc=.false.
      ENDIF
C
      IF ( LPOPT('O').AND..NOT.(LPOPT('B').OR.LPOPT('D')) ) THEN
         IOLVL = 0
      ELSE
         IOLVL = 1
      ENDIF
C
      IF (CDTZ.NE.'    ') THEN
         CALL LFLNB ( CDTZ,1,4,IC,NC )
         IDTZ = INTGR ( CDTZ,IC,NC,ISTAT )
         IF ( ISTAT.NE.0 ) THEN
            WRITE ( LFNOUT,15 ) CDTZ
   15       FORMAT ( ' ERROR IN DTZ = ',A )
            RETURN
         ENDIF
      ENDIF
C
      IF ( IOLVL.EQ.1 ) THEN
         CALL WHEN ( DATE,TIME )
c
c        WRITE ( LFNOUT,10 ) CVERNO,CVERDT,DATE,TIME,CIFILE,COFILE,
c    1     CDSFIL,CSFILE,CPFILE,IDTZ
c
         WRITE ( LFNOUT, 10 ) CVERNO, CVERDT, DATE, TIME,
     .     CIFILE(1:18), COFILE(1:18), CDSFIL(1:18),
     .     CSFILE(1:18), CPFILE(1:18), IDTZ
c
   10    FORMAT ( '1',80('*') /
     1    '   S H E F D S S    ',A,'---',A,
     2   /'   THIS RUN ON ',A,' AT ',A
     3   /'   INPUT=',A,'   OUTPUT=',A, '   DSSFILE=',A,
     4   /'   SFILE=', A ,'    PFILE=', A ,'   DTZ =',I2,' HRS'
     5   /80('*')/)
      ENDIF
C
      IADHOC = 0
      IF ( LSTOREALL) IADHOC = 1
      IF ( IADHOC.EQ.1.AND.IOLVL.EQ.1 ) THEN
         WRITE ( LFNOUT,* ) '   AD HOC ENTRIES ( A OPTION ON )'
      ENDIF
C
      IF ( CFDEF.NE.' '.AND.IOLVL.EQ.1 ) THEN
          WRITE ( LFNOUT, * ) '   DEFAULT PATHNAME F-PART = ', CFDEF
          WRITE ( LFNOUT, * )
      ELSE
      ENDIF
C
C
C     <LODPAR> READS INFORMATION LINKING SHEF PHYSICAL ELEMENTS <PE> TO
C     APPROPRIATE DSS PARAMTER INFORMATION <PARAM>,
C     AND <TYPE>.
C
      CALL LODPAR( CPFILE,LFNOUT)
C
C
C      <LODSEN>  READS INFORMATION ABOUT FIELD SENSORS OF INTEREST WHICH
C      IS NECESSARY FOR FORMATING AND LABELING DATA FROM THOSE SENSORS
C      INTO DSS.  <SENSOR> IS A LIST OF SENSOR LABELS FORMED BY A
C      CONCATENATION OF THE SHEF STATION IDENTIFIER AND PHYSICAL
C      ELEMENT CODE.  EACH ENTRY IN SENSOR HAS, OPTIONALLY, A
C      CORRESPONDING REGULAR TIME INTERVAL OF REPORTING AND DSS
C      PATHNAME LABEL PARTS <GROUP> AND <LOCATION> FOR PARTS
C      A AND B,RESPECTIVELY.
C
      CALL LODSEN( CSFILE,ISTAT )
      IF ( ISTAT.NE.0 ) GO TO 900
C
c     IF ( CPEFIL.NE.'-NONE-' ) THEN
C
C     CREATE THE SHEF PARAMETER FILE 'SHEFPARM' FROM THE
C     THE FILE 'INPUTPARM'.
C
c        CALL LODPE ( CPEFIL,ISTAT )
c     ENDIF
C
C     Open the DSS file
*      CALL SDINIT( CDSFIL,ISTAT )
*     IF ( ISTAT.NE.0 ) GO TO 900
C
C      THIS ROUTINE IS THE DRIVER FOR PARSING SHEF.
C
c     CALL SHDRIVE
      call shdriv(lchn,luout,luparm,luerr,lucpy)
C
      CALL SDFLSH
C
  900 CONTINUE
      NTOTAL1 = NTOTAL
      NSETS1 = NSETS
      CLOSE (UNIT=LCHN)
*      CLOSE (UNIT=LFNOUT)
      CLOSE (UNIT=ishefitc)
      CLOSE (UNIT=MCHN,STATUS='DELETE')
C
      RETURN
      END

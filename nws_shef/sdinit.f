      SUBROUTINE SDINIT( CDSFIL,ISTAT )
C
      CHARACTER CDSFIL*(*)
C
CADD C.SDCNTL                                                           H
      INCLUDE 'sdcntl.h'                                                MLlg
CADD C.SDDATA                                                           H
      INCLUDE 'sddata.h'                                                MLlg
CADD C.SENSRS                                                           H
      INCLUDE 'sensrs.h'                                                MLlg
CADD C.PSTBLS                                                           H
      INCLUDE 'pstbls.h'                                                MLlg
CADD C.SDMISC                                                           H
      INCLUDE 'sdmisc.h'                                                MLlg
C
      LOGICAL LPOPT
C
      ISTAT = 0
C
C *****************************************************************************
C     INITIALIZE KEY VARIABLES
C *****************************************************************************
C
      IF ( IOLVL.EQ.1 ) THEN
         WRITE ( LFNOUT,100 )
  100    FORMAT ( 45('-')/ )
      ENDIF
      NDATA = 0
      HEAD(1)=' '
      NHEAD = 0
      IF ( LPOPT('D') ) IDEBUG = 1
  900 CONTINUE
      RETURN
      END

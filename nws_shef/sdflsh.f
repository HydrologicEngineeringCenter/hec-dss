      SUBROUTINE SDFLSH
C
CADD C.SDCNTL                                                           H
      INCLUDE 'sdcntl.h'                                                MLlg
CADD C.SDMISC                                                           H
      INCLUDE 'sdmisc.h'                                                MLlg
CADD C.SDDATA                                                           H
      INCLUDE 'sddata.h'                                                MLlg
C
      IF ( NHEAD.GT.0.AND.NDATA.GT.0 ) THEN
         CALL SDMAIN
      ENDIF
C
*      IF ( IOLVL.EQ.0 ) CALL ZSET( 'MLVL',' ',0 )
*      CALL ZCLOSE( IFLTAB )
C
      RETURN
      END

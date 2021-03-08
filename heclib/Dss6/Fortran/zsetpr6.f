      SUBROUTINE zsetpr6 (IFLTAB, CFLG, CSTR, INUMB)
C
C
C     Sets Items in the Permanent section of a DSS file
C     This routine is to be called only by DSSUTL and
C     internal DSS subroutines
C
C     Written by Bill Charley at HEC, January 1990.
C
C
      INTEGER IFLTAB(*)
      CHARACTER CFLAG*4, CFLG*(*), CSTR*(*)
C
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdssts.h'
C
      INCLUDE 'zdssmz.h'
C
C
C
      CFLAG = CFLG
      IF (MLEVEL.GE.12) WRITE ( MUNIT, 20) CFLAG
 20   FORMAT (T2,'-----DSS---Debug:  Enter zsetpr6;  Flag: ',A)
C
C     Loc file and get the root section
      CALL zmultu6 (IFLTAB, .TRUE., .TRUE.)
C
C
      IF (CFLAG.EQ.'SEQN') THEN
      IFLTAB(KSEQNO) = INUMB
C
      ELSE
      GO TO 820
      ENDIF
C
C
 800  CONTINUE
C     Store new information
      IADD = 1
      CALL zptrec6 (IFLTAB, IFLTAB(KPERM), NPERM, IADD, .FALSE.)
C
 820  CONTINUE
C     Dump buffers and unlock file
      CALL zmultu6 (IFLTAB, .FALSE., .TRUE.)
C
      RETURN
      END
      SUBROUTINE zsetpr (IFLTAB, CFLG, CSTR, INUMB)
      INTEGER IFLTAB(*)
      CHARACTER CFLG*(*), CSTR*(*)
      call zGetVersion(IFLTAB, iversion)
      IF (iversion.EQ.6) THEN
      call zsetpr6 (IFLTAB, CFLG, CSTR, INUMB)
      endif
      return
      end


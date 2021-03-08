      SUBROUTINE zcut6(IFLTAB,CPATH, LDELETE, IBUFFER, MAXBUFF, ISTATUS)
C
C
C     Copy a single record into a buffer area.  (copy)
C     If LDELETE is true, delete the record afterwards (cut).
C     Can use zpaste6 to paste the record to another dss file
C
C     Written by Bill Charley, HEC, 1999.
C
C
      INTEGER IFLTAB(*), IBUFFER(MAXBUFF), ISTATUS
      CHARACTER CPATH*(*)
      LOGICAL LDELETE
      LOGICAL LFOUND
C
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdssmz.h'
C
C
C     IBUFFER will contain:
C        1.  Key (-24689)
C        2.  Size of buffer (includes Keys)
C        3.  Swap (Big - Little Endian) flag
C        4.  Info block
C        5.  Internal header
C        6.  Compression Header
C        7.  User Header
C        8.  Data Block
C        9.  Key (-24688)
C
C
C
C
      CALL CHRLNB(CPATH,NPATH)
      IF (MLEVEL.GE.11) THEN
         WRITE (MUNIT, 20) IFLTAB(KUNIT), CPATH(1:NPATH)
 20      FORMAT (T6,'-----DSS---Debug: Enter zcut6',/,
     *   T5,'Unit:',I5,'  Path: ',A)
      ENDIF
C
      CALL zcutsz6 (IFLTAB, CPATH, ISIZE, ISTATUS)
      IF (ISTATUS.NE.0) GO TO 900
      IF (ISIZE.GT.MAXBUFF) GO TO 920
C
      IBUFFER(1) = -24689
      IBUFFER(2) = ISIZE
      IBUFFER(3) = IFLTAB(KSWAP)
      IBUFFER(ISIZE) = -24688
C
C     Read in the main information block
      CALL zrdinf6(IFLTAB, CPATH, JHEAD, JDATA, ISTATUS)
      IF (ISTATUS.NE.0) GO TO 900
C
C     Get the sizes of the headers and data blocks
      NIHEAD = INFO(NPPWRD+KINIHE)
      NCHEAD = INFO(NPPWRD+KINCHE)
      NUHEAD = INFO(NPPWRD+KINUHE)
      NDATA  = INFO(NPPWRD+KINDAT)
C
C     Copy the info block to the buffer
      ILOC = 4
      NSIZE = NINFO + NPPWRD
      DO 100 I=1,NSIZE
         IBUFFER(ILOC) = INFO(I)
         ILOC = ILOC + 1
 100  CONTINUE
C
C     Read the internal header
      IF (NIHEAD.GT.0) THEN
         NADD = INFO(NPPWRD+KIAIHE)
         CALL zgtrec6 (IFLTAB, IBUFFER(ILOC), NIHEAD, NADD, .FALSE.)
         ILOC = ILOC + NIHEAD
      ENDIF
C
C     Read the Compression header
      IF (NCHEAD.GT.0) THEN
         NADD = INFO(NPPWRD+KIACHE)
         CALL zgtrec6 (IFLTAB, IBUFFER(ILOC), NCHEAD, NADD, .FALSE.)
         ILOC = ILOC + NCHEAD
      ENDIF
C
C     Read the User header
      IF (NUHEAD.GT.0) THEN
         NADD = INFO(NPPWRD+KIAUHE)
         CALL zgtrec6 (IFLTAB, IBUFFER(ILOC), NUHEAD, NADD, .FALSE.)
         ILOC = ILOC + NUHEAD
      ENDIF
C
C     Read the Data block
      IF (NDATA.GT.0) THEN
         NADD = INFO(NPPWRD+KIADAT)
         CALL zgtrec6 (IFLTAB, IBUFFER(ILOC), NDATA, NADD, .FALSE.)
         ILOC = ILOC + NDATA
      ENDIF
C
C
C     Write the message that the data was read
      IF (MLEVEL.GE.4) THEN
         IF (L80COL) THEN
            WRITE ( MUNIT, 120) CPATH(1:NPATH)
 120        FORMAT(' --ZREAD:  ',A)
         ELSE
            WRITE ( MUNIT, 140) IFLTAB(KUNIT), INFO(NPPWRD+KIVER),
     *                         CPATH(1:NPATH)
 140        FORMAT(' -----DSS---ZREAD Unit',I5,'; Vers.',I5,':',2X,A)
         ENDIF
      ENDIF
C
C
C     Delete the record if this is a cut, not a copy
      IF (LDELETE) THEN
         CALL zdelet6 (IFLTAB, CPATH, NPATH, LFOUND)
      ENDIF
C
C
C
 800  CONTINUE
      IF (MLEVEL.GE.11) WRITE (MUNIT, 820) ISTATUS
 820  FORMAT (T6,'-----DSS---Debug: Exit zcut6, Status:',I4)
*      CALL FLUSH(MUNIT)                                         Mu
      RETURN
C
 900  CONTINUE
      CALL CHRLNB (CPATH,NPATH)
      IF (MLEVEL.GE.1) WRITE (MUNIT, 901) CPATH(1:NPATH)
 901  FORMAT (/,' -----DSS---zcut6:  ERROR;  Record does not exist: ',A)
      GO TO 800
C
 920  CONTINUE
      ISTAT = -2
      CALL CHRLNB (CPATH,NPATH)
      IF (MLEVEL.GE.1) WRITE (MUNIT, 921) CPATH(1:NPATH), ISIZE, MAXBUFF
 921  FORMAT (/,' -----DSS---zcut6:  ERROR;  Insufficient buffer',
     * ' supplied for copy',/,' Pathname: ',A,/,
     * '    Needed:',I9,',  Supplied:',I9)
      GO TO 800
C
      END


      SUBROUTINE zpaste6 (IFLTAB, IBUFFER, ISTATUS)
      implicit none
C
C     Paste a single record (from zcut6) into a dss file.
C
C     Written by Bill Charley, HEC, 1999.
C
C
      INTEGER IFLTAB(*), IBUFFER(*)
      CHARACTER CPATH*392
      integer ISIZE,NPATH,ILOC,I,NIHEAD,NCHEAD,NUHEAD
      integer NDATA,JHEAD,JDATA,NLEN,NADD,ISTATUS,ISTAT,J
C
C
      INCLUDE 'zdsscz.h'
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdsslz.h'
C
      INCLUDE 'zdssmz.h'
C
      LOGICAL LFOUND
C
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
      IF (MLEVEL.GE.11) THEN
         WRITE (MUNIT, 20)
 20      FORMAT (T6,'-----DSS---Debug: Enter zpaste6')
      ENDIF
C
C     Is this a valid cut buffer block?
      IF (IBUFFER(1).NE.-24689) GO TO 900
      ISIZE = IBUFFER(2)
      IF (ISIZE.LE.0) GO TO 900
      IF (IBUFFER(ISIZE).NE.-24688) GO TO 900
C
C     Yes - a valid buffer
C     Is the  Swap (Big - Little Endian) flag the same?
      IF (IBUFFER(3).NE.IFLTAB(KSWAP)) GO TO 910
C
C     Are we in a read only state?
      IF (IFLTAB(KREADO).EQ.1) GO TO 920
C
C     Get multiple user access
      CALL zmultu6 ( IFLTAB, .TRUE., .TRUE.)
C
C     Everything is OK, extract the INFO block and pathname
      NPATH = IBUFFER(3+KINPAT)
      NPPWRD = (NPATH - 1)/NCPW + 1
      NPMWRD = (NPATH - 1) / NCMW + 1
      ILOC = 4
      NSIZE = NINFO + NPPWRD
      DO 100 I=1,NSIZE
         INFO(I) = IBUFFER(ILOC)
         ILOC = ILOC + 1
 100  CONTINUE
C
      CPATH = ' '
      CALL HOL2CH (INFO(KIPATH), CPATH, NPMWRD)
C
      IF (MLEVEL.GE.11) WRITE (MUNIT,120) CPATH(1:NPATH)
 120  FORMAT (T6,'Pathname: ',A)
C
C     Get the sizes of the headers and data blocks
      NIHEAD = INFO(NPPWRD+KINIHE)
      NCHEAD = INFO(NPPWRD+KINCHE)
      NUHEAD = INFO(NPPWRD+KINUHE)
      NDATA  = INFO(NPPWRD+KINDAT)
C
C
C     Check if new record exists
      LWRITE = .TRUE.
      CALL zcheck6 (IFLTAB, CPATH, NPATH, JHEAD, JDATA, LFOUND)
C
C     Set the type, tag, and number of data
      ITYPE = INFO(NPPWRD+KITYPE)
      NLDATA = INFO(NPPWRD+KILNDA)
      CTAG = ' '
      CALL HOLCHR (INFO(NPPWRD+KITAG), 1, NTAGC, CTAG, 1)
C
C
C     If the pathname was not found by zcheck6 write new pointers
      IF (.NOT.LFOUND) THEN
         CALL znwrit6 (IFLTAB, CPATH, NPATH, NIHEAD, NCHEAD, NUHEAD,
     *                NDATA)
      ELSE
         IF (LPROTC) GO TO 930
         CALL zowrit6 (IFLTAB, CPATH, NPATH, NIHEAD, NCHEAD, NUHEAD,
     *                NDATA)
      ENDIF
C
      IF (IFLTAB(KSTAT).NE.0) GO TO 940
C
C     Restore non address settings in the information block
      NLEN = KIQUAL - KILNDA + 1
      DO 140 I=1,NLEN
         J = I + NPPWRD + KILNDA - 1
         INFO(J) = IBUFFER(J+3)
 140  CONTINUE
      DO 160 I=1,NPASS
         J = I + NPPWRD + KIPASS - 1
         INFO(J) = IBUFFER(J+3)
 160  CONTINUE
C
C     Store the information block
      CALL zptrec6 (IFLTAB, INFO, NSIZE, IPNBIN(JPNBIN+NPPWRD+KBAINF),
     * .FALSE.)
C
C     Now store the headers and data set
      ILOC = NSIZE + 4
      IF (NIHEAD.GT.0) THEN
         NADD = INFO(NPPWRD+KIAIHE)
         CALL zptrec6 (IFLTAB, IBUFFER(ILOC), NIHEAD, NADD, .FALSE.)
         ILOC = ILOC + NIHEAD
      ENDIF
C
      IF (NCHEAD.GT.0) THEN
         NADD = INFO(NPPWRD+KIACHE)
         CALL zptrec6 (IFLTAB, IBUFFER(ILOC), NCHEAD, NADD, .FALSE.)
         ILOC = ILOC + NCHEAD
      ENDIF
C
      IF (NUHEAD.GT.0) THEN
         NADD = INFO(NPPWRD+KIAUHE)
         CALL zptrec6 (IFLTAB, IBUFFER(ILOC), NUHEAD, NADD, .FALSE.)
         ILOC = ILOC + NUHEAD
      ENDIF
C
      IF (NDATA.GT.0) THEN
         NADD = INFO(NPPWRD+KIADAT)
         CALL zptrec6 (IFLTAB, IBUFFER(ILOC), NDATA, NADD, .FALSE.)
         ILOC = ILOC + NDATA
      ENDIF
C
      IF (MLEVEL.GE.3) THEN
         IF (L80COL) THEN
            WRITE ( MUNIT,200) CPATH(1:NPATH)
 200        FORMAT('    --ZWRITE: ',A)
         ELSE
            WRITE (MUNIT,220) IFLTAB(KUNIT), INFO(NPPWRD+KIVER),
     *                       CPATH(1:NPATH)
 220        FORMAT('    -----DSS---ZWRITE Unit',I5,'; Vers.',
     *             I5,':',2X,A)
         ENDIF
      ENDIF
C
C
C
 800  CONTINUE
C     Release multiple user access
      CALL zmultu6 ( IFLTAB, .FALSE., .TRUE.)
      LWRITE = .FALSE.
C
      IF (MLEVEL.GE.11) WRITE (MUNIT, 820) ISTATUS
 820  FORMAT (T6,'-----DSS---Debug: Exit zpaste6, Status:',I4)
*      CALL FLUSH(MUNIT)                                    Mu
      RETURN
C
C
 900  CONTINUE
      ISTATUS = -1
      IF (MLEVEL.GE.1) WRITE (MUNIT, 901)
 901  FORMAT (/' -----DSS---zpaste6: ERROR; Invalid paste buffer block')
      GO TO 800
C
 910  CONTINUE
      ISTAT = -5
      WRITE (MUNIT, 911)
 911  FORMAT (/' -----DSS---zpaste6:  ERROR;  Files are of different',
     * ' endian type.'/' Can only copy files with the same byte order')
      GO TO 800
C
 920  CONTINUE
       ISTAT = -2
      CALL CHRLNB (CPATH,NPATH)
      IF (MLEVEL.GE.1) WRITE (MUNIT, 921) CPATH(1:NPATH)
 921  FORMAT (' -----DSS---zpaste6:  ERROR;  File has Read Access Only',
     * /,' Pathname: ',A)
      GO TO 800
C
 930  CONTINUE
      ISTAT = 2
      CALL CHRLNB (CPATH,NPATH)
      IF (MLEVEL.GE.2) WRITE (MUNIT, 931) CPATH(1:NPATH)
 931  FORMAT (' -----DSS---zpaste6:  Write Protection for Existing',
     * ' Record (no data written)',/,
     * ' Pathname: ',A)
C
 940  CONTINUE
      ISTATUS = IFLTAB(KSTAT)
      WRITE (MUNIT, 941) ISTATUS
 941  FORMAT (/,' *****DSS*** zpaste6:  ERROR  - UNABLE TO ',
     * ' COPY DATA',/,' Status: ',I8)
      GO TO 800
C
C
      END


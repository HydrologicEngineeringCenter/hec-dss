      SUBROUTINE zcopyfile6 (ifltabFrom, ifltabTo, ISTAT)
C
      IMPLICIT NONE
C
C
      integer, PARAMETER:: KARRAY=5000
      INTEGER ifltabFrom(*), ifltabTo(*)
      CHARACTER CPATH*392
      INTEGER ISTAT, count
      INTEGER IARRAY(KARRAY)
      integer IADD,NRECS,IW,IREC,IWRD,IPSTAT,NPATH
      integer N,NERROR,MAXERROR
C
C
C     Z-Copy-File.
C
C     zcofil6 copies via a brute-force approach;  Thus it
C     will copy all vaild data from a damaged file
C     (restoring it as best as possible).
C
C     Written by Bill Charley at HEC, 1988.
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdsscz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdsslz.h'
C
      INCLUDE 'zdssmz.h'
C
      INCLUDE 'zdssnz.h'
C
      COMMON /ZSTATUS/ TOTAL_NUMB,  CURRENT_NUMB,
     *                 INTERRUPT, NERROR, MAXERROR
      INTEGER TOTAL_NUMB,  CURRENT_NUMB,  INTERRUPT
C
      SAVE 

      integer vFrom, vTo
C
C
      call zGetVersion(ifltabFrom, vFrom)
      if (vFrom.eq.7) then
         call zcopyfile7(ifltabFrom, ifltabTo, ISTAT)
         return
      endif
      if (vFrom.ne.6) go to 910

C
      CPATH = ' '
      count = 0
C
C
C     The follow is if another thread interrupts us to stop the process
      INTERRUPT = 0
      IF (MLEVEL.GE.11) WRITE (MUNIT,20) ifltabFrom(KUNIT)
 20   FORMAT (T6,'-----DSS---Debug:  Enter zcofil6;  Unit:',I5)
C
C
C     Get the number of physical records in file (not data recs)
      IADD = ifltabFrom(KFSIZE) - 1
      CALL zgetrw6 (IADD, NRECS, IW)
C
      TOTAL_NUMB = ifltabFrom(KNRECS)
      CURRENT_NUMB = 0
C
      DO 200 IREC=1,NRECS
C
C     If another thead set the interrupt flag, return
      IF (INTERRUPT.NE.0) GO TO 840
C
C
C     Read physical record IREC from file
      CALL zgetad6 (IADD, IREC, 1)
      CALL zgtrec6 (ifltabFrom, IARRAY, NBSIZE, IADD, .FALSE.)
C
C     Did we hit end of file?
      IF (IADD.LT.0) THEN
      WRITE (MUNIT, 28) IREC
 28   FORMAT (//, ' **** Caution:  Reached End of File at Physical',
     * ' Record:',I8,/,' **** Probable Incomplete File Copy',/)
      GO TO 800
      ENDIF
C
C     Search through this record, looking for pathname flags
C
      DO 200 IWRD=1,NBSIZE
C
      IF (IARRAY(IWRD).EQ.NPFLAG) THEN
C
C     Found a flag - Get the first three words of the information
C     block to see if this indeed is the start of a data record
      CALL zgetad6 (IADD, IREC, IWRD)
      CALL zgtrec6 ( ifltabFrom, INFO, 3, IADD, .FALSE.)
      IF (ifltabFrom(KSTAT).NE.0) GO TO 940
C
      IF (MLEVEL.GE.10) WRITE (MUNIT,30)IADD, INFO(KISTAT), INFO(KINPAT)
 30   FORMAT (T5,'Found a INFO Flag at address',I13,/,
     * T5,'Status:',I5,';  Pathname Length:',I5)
      IPSTAT = INFO(KISTAT)
      IF (IPSTAT.GT.10) IPSTAT = IPSTAT - 10
C     Is this an undelete of a renamed or deleted record?
      IF (IPSTAT.EQ.2) THEN
          INFO(KISTAT) = INFO(KISTAT) - 1
      ENDIF
C     Check for a valid status flag (1)
      IF (IPSTAT.EQ.1) THEN
C     Check for a valid pathname length
      NPATH = INFO(KINPAT)
      IF ((NPATH.GT.0).AND.(NPATH.LE.392)) THEN
C
C     Passed, therefore a valid record - Get the full information block
      NPPWRD = (NPATH - 1)/NCPW + 1
      NPMWRD = (NPATH - 1)/NCMW + 1
      CALL zgtrec6 ( ifltabFrom, INFO, NINFO+NPPWRD, IADD, .FALSE.)
      IF (ifltabFrom(KSTAT).NE.0) GO TO 940
C
C     Get the pathname
      CPATH = ' '
      CALL HOLCHR (INFO(KIPATH), 1, NPATH, CPATH, 1)
      CURRENT_NUMB = CURRENT_NUMB + 1
      count = count + 1
      call chrlnb(CPATH, n)
      IF (MLEVEL.GT.3) WRITE (MUNIT,190) CPATH(1:n)
 190  FORMAT (' -----DSS--- zcofil6;  Record: ',A)
      call zcopyRecord(ifltabFrom, ifltabTo, CPATH(1:n),
     * CPATH(1:n), ISTAT)
C
      if (istat.lt.0) then
        write (*,*)'Error in copy at record ', count
        write (*,*)'Pathname: ', cpath(1:n)
        write (*,*)'Status: ', ISTAT
        return
      endif
C
      ENDIF

C
      ENDIF
      ENDIF
C
 200  CONTINUE

 800  CONTINUE

      IF (MLEVEL.GE.11) WRITE (MUNIT,820)
 820  FORMAT (T6,'-----DSS---Debug:  Exit zcofil6')
      CALL FLUSH(MUNIT)                                                 Mu
C
      RETURN
C
C
 840  CONTINUE
      IF (MLEVEL.GE.4) WRITE (MUNIT, 850)
 850  FORMAT (' ----- DSS --- zcofil6:  Copy interrupted.')
      GO TO 800
C
C
C
 910  CONTINUE
      WRITE (MUNIT, 911) ifltabFrom(1), ifltabTo(1)
 911  FORMAT (/,' ----- DSS --- zcofil6:  ERROR;  DSS File(s) not',
     * ' Version 6.',/,' Versions:',2I8,/)
      RETURN
C
 940  CONTINUE
      WRITE (MUNIT, 941)
 941  FORMAT (/' *****DSS*** zcofil6:  ERROR  - UNABLE TO ',
     * ' COPY DATA')
      RETURN
C
      END


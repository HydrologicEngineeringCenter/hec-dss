      SUBROUTINE zundel6 ( IFLTAB, CPATH, NPATH, ISTAT)
C
C
C     Undeletes records in a DSS previously deleted by zdelet6.
C     zdelet6 sets a flag in the pathname bin and data area
C     indicating that the record has been deleted.  The
C     data is not really deleted until a squeez by DSSUTL.
C
C     Written by Bill Charley at HEC, 1988.
C
C
      INTEGER IFLTAB(*)
      CHARACTER CPATH*(*), CTPATH*392
      LOGICAL LFOUND
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
C
      IF (MLEVEL.GE.12) WRITE ( MUNIT, 20)
 20   FORMAT (T8,'-----DSS---Debug: Enter zundel6')
C
C
C
      ISTAT = 0
      LUNDEL = .TRUE.
C
C
C     Should all records in the file be undeleted??
      IF (CPATH(1:NPATH).EQ.'ALL') THEN
      CALL zudall6 (IFLTAB, 0)
      ELSE
C
C     Just undelete a single  record
C
C     Lock file for multiple User Access
      CALL zmultu6 ( IFLTAB, .TRUE., .TRUE.)
      LWRITE = .TRUE.
C
C     Find the pathname bin block location
      IFLTAB(KLPATL) = -1
      CALL zcheck6 ( IFLTAB, CPATH, NPATH, NHEAD, NDATA, LFOUND)
C
      IF (LFOUND) THEN
C
C     Make sure the status flag is set to delete
      IPSTAT = IPNBIN(JPNBIN+KBSTAT)
      IF (IPSTAT.EQ.12) IPSTAT = 2
      IF (IPSTAT.NE.2) THEN
      IF (MLEVEL.GE.2) WRITE (MUNIT,80) IFLTAB(KUNIT), CPATH(1:NPATH)
 80   FORMAT (' -----DSS***zundel6 Unit',I5,':  Record Already Exists;',/
     * ' Pathname: ',A)
      ISTAT = 2
      GO TO 200
      ENDIF
C
C     Reset the status flag in the pathname bin to OK
      IPNBIN(JPNBIN+KBSTAT) = IPNBIN(JPNBIN+KBSTAT) - 1
      I = IFLTAB(KBNSIZ)
      CALL zptrec6 (IFLTAB, IPNBIN, I, IPBADD, .FALSE.)
C
C     Get the record information area
      NPPWRD = (NPATH-1)/NCPW + 1
      NPMWRD = (NPATH-1)/NCMW + 1
      IADD = IPNBIN(JPNBIN+NPPWRD+KBAINF)
      CALL zgtrec6 (IFLTAB, INFO, NINFO+NPPWRD, IADD, .FALSE.)
C
C     Double Check that this is the correct pathname
      IF (NPATH.NE.INFO(KINPAT)) GO TO 900
      CALL HOL2CH ( INFO(KIPATH), CTPATH, NPMWRD)
      IF (CPATH(1:NPATH).NE.CTPATH(1:NPATH)) GO TO 900
C
C     Reset record information block status flag
      INFO(KISTAT) = IPNBIN(JPNBIN+KBSTAT)
      CALL zptrec6 (IFLTAB, INFO, NINFO+NPPWRD, IADD, .FALSE.)
C
C     Decrement dead space
      IFLTAB(KNRECS) = IFLTAB(KNRECS) + 1
      IFLTAB(KDEAD) = IFLTAB(KDEAD) - (NINFO + NPPWRD + NHEAD + NDATA)
      IADD = 1
      CALL zptrec6 (IFLTAB, IFLTAB(KPERM), NPERM, IADD, .FALSE.)
C
C     Write informative message
C
      IF (MLEVEL.GE.3) WRITE (MUNIT,100) IFLTAB(KUNIT), CPATH(1:NPATH)
 100  FORMAT (1X,'-----DSS---zundel6 Unit',I5,':  ',A)
C
C
      ELSE
C
C     That record was not found!
      IF (MLEVEL.GE.2) WRITE (MUNIT,160) IFLTAB(KUNIT), CPATH(1:NPATH)
 160  FORMAT (' -----DSS***zundel6 Unit',I5,':  Unable to Find Record;',/
     * ' Pathname: ',A)
      ISTAT = 1
C
      ENDIF
C
      ENDIF
C
 200  CONTINUE
C     Release Multiple User Acess
      CALL zmultu6 ( IFLTAB, .FALSE., .TRUE.)
      IFLTAB(KLPATL) = -1
      LUNDEL = .FALSE.
      LWRITE = .FALSE.
C
 800  CONTINUE
      IF (IFLTAB(KSTAT).NE.0) GO TO 900
      IF (MLEVEL.GE.12) WRITE ( MUNIT,820)
 820  FORMAT (T8,'-----DSS---Debug:  Exit zundel6')
*      CALL FLUSH(MUNIT)                                      Mu
C
      RETURN
C
C
 900  CONTINUE
      NP = INFO(KINPAT)
      CALL zerror6 (IFLTAB, 11, 'zundel6', NP, IADD, CPATH, NPATH,
     * CTPATH, NP)
      ISTAT = -3
      RETURN
C
      END


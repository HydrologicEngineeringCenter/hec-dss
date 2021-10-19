      SUBROUTINE zowrit6 (IFLTAB, CPATH, NPATH, NIHEAD, NCHEAD, NUHEAD,
     * NDATA)
C
C
C     Main routine for re-writing an old record to a DSS file
C     Not user callable (Use zwritex6)
C
C     Written by Bill Charley, HEC, June 1989
C
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdssbz.h'
C
      INCLUDE 'zdsscz.h'
C
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdsslz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdssmz.h'
C
      INTEGER IFLTAB(*)
      CHARACTER CTPATH*400, CPATH*(*)
      LOGICAL LWBIN, LONGPA
C
C
      IF (MLEVEL.GE.12) WRITE (MUNIT,20) IFLTAB(KUNIT), CPATH(1:NPATH),
     * NIHEAD, NCHEAD, NUHEAD, NDATA
 20   FORMAT (/,T10,'-----DSS---Debug: Enter zowrit6,  Unit:',I5,/,
     * /,T12,'Pathname: ',A,/,T12,'NIHEAD:',I5,', NCHEAD:',I5,
     * ', NUHEAD:',I5,', NDATA:',I5)
C
      IFLTAB(KBSADD) = IFLTAB(KFSIZE)
      CTPATH = CPATH
      LWBIN = .FALSE.
C
C
C     Update the current date and time
      CALL WHEN (CDATE, CTIME)
C
C
C     Save the pathname bin in memory, in case it needs to be rewritten
      LSBUFF(JBUFF) = .TRUE.
C
C     Read the information block
      NADD  = IPNBIN(JPNBIN+NPPWRD+KBAINF)
      CALL zgtrec6 (IFLTAB, INFO, NINFO+NPPWRD, NADD, .TRUE.)
      IF (INFO(KISTAT).EQ.11) THEN
         LONGPA = .TRUE.
      ELSE
         LONGPA = .FALSE.
      ENDIF
C
C     Double check to see that this is the correct pathname
      IF (NPATH.NE.INFO(KINPAT)) GO TO 900
      CALL HOL2CH ( INFO(KIPATH), CTPATH, NPMWRD)
      IF (CPATH(1:NPATH).NE.CTPATH(1:NPATH)) GO TO 900
C
C     Get Size information
      JDATA  = INFO(NPPWRD+KINDAT)
      JIHEAD = INFO(NPPWRD+KINIHE)
      JCHEAD = INFO(NPPWRD+KINCHE)
      JUHEAD = INFO(NPPWRD+KINUHE)
      JUNUSE = INFO(NPPWRD+KIUNUS)
C
C     See if old size is same as new
      NSIZE = NDATA + NIHEAD + NCHEAD + NUHEAD
      JSIZE = JDATA + JIHEAD + JCHEAD + JUHEAD + JUNUSE
C
C     If the new size is greater than the old, write the new data
C     at the end of the file, and mark the old as dead space.
      IF (NSIZE.GT.JSIZE) THEN
C     Mark and store old info block as unusable
      INFO(KISTAT) = -1
C     Store old information block
      IF(MLEVEL.GE.14)WRITE(MUNIT,*)'-zowrit6: Store old obsolete info'
      CALL zptrec6 (IFLTAB, INFO, NINFO+NPPWRD, NADD, .FALSE.)
C
      IFLTAB(KDEAD) = IFLTAB(KDEAD) + JSIZE + NPPWRD + NINFO
C     Update the size and locations in the pathname bin
      IPNBIN(JPNBIN+NPPWRD+KBAINF) = IFLTAB(KFSIZE)
      IPNBIN(JPNBIN+NPPWRD+KBNHEA) = NUHEAD
      IPNBIN(JPNBIN+NPPWRD+KBNDAT) = NDATA
      IPNBIN(JPNBIN+NPPWRD+KBTYPE) = ITYPE
      IF (NLDATA.GE.0) THEN
      IPNBIN(JPNBIN+NPPWRD+KBLNDA) = NLDATA
      ELSE
      IPNBIN(JPNBIN+NPPWRD+KBLNDA) = NDATA
      ENDIF
C     Flag that this bin and the permanent sections need to be updated
      LWBIN = .TRUE.
      IFLTAB(KLPATL) = 0
C
      ELSE
C     New size equal or less than old.
C     If sizes have changed, update pathname bin
      IF ((JDATA.NE.NDATA).OR.(JUHEAD.NE.NUHEAD)) THEN
      IPNBIN(JPNBIN+NPPWRD+KBNHEA) = NUHEAD
      IPNBIN(JPNBIN+NPPWRD+KBNDAT) = NDATA
      IPNBIN(JPNBIN+NPPWRD+KBTYPE) = ITYPE
      IFLTAB(KLPATL) = 0
      IF (NLDATA.GE.0) THEN
      IPNBIN(JPNBIN+NPPWRD+KBLNDA) = NLDATA
      ELSE
      IPNBIN(JPNBIN+NPPWRD+KBLNDA) = NDATA
      ENDIF
      LWBIN = .TRUE.
      ENDIF
      ENDIF
C
C     The Pathname bin has been changed - store new copy on disk
      IF (LWBIN) THEN
      IF(MLEVEL.GE.14)WRITE(MUNIT,*)'-zowrit6:  new size, update bin'
      I = INT(IFLTAB(KBNSIZ))
      CALL zptrec6 (IFLTAB, IPNBIN, I, IPBADD, .TRUE.)
      ENDIF
C
C     If the new size is less, update the amount of dead space
C     in the file
      IF (NSIZE.LT.JSIZE) THEN
      IFLTAB(KDEAD) = IFLTAB(KDEAD) + JSIZE - NSIZE
      ELSE IF (JUNUSE.GT.0) THEN
      IFLTAB(KDEAD) = IFLTAB(KDEAD) + NSIZE - JSIZE - JUNUSE
      IF (IFLTAB(KDEAD).LT.0) IFLTAB(KDEAD) = 0
      ENDIF
C
C
      IF (NSIZE.GT.JSIZE) THEN
C
C     Header and data arrays larger than old - need to write new
C     one at end of file (along with header and data).
      IF (LONGPA) THEN
         INFO(KISTAT) = 11
      ELSE
         INFO(KISTAT) = 1
      ENDIF
      IFLTAB(KFSIZE) = IFLTAB(KFSIZE) + NINFO + NPPWRD
C     Update block with new location of data and header
C     Store the header array location and length
      INFO(NPPWRD+KIAIHE) = IFLTAB(KFSIZE)
      INFO(NPPWRD+KINIHE) = NIHEAD
      IFLTAB(KFSIZE) = IFLTAB(KFSIZE) + NIHEAD
      INFO(NPPWRD+KIACHE) = IFLTAB(KFSIZE)
      INFO(NPPWRD+KINCHE) = NCHEAD
      IFLTAB(KFSIZE) = IFLTAB(KFSIZE) + NCHEAD
      INFO(NPPWRD+KIAUHE) = IFLTAB(KFSIZE)
      INFO(NPPWRD+KINUHE) = NUHEAD
      INFO(NPPWRD+KIUNUS) = 0
      IFLTAB(KFSIZE) = IFLTAB(KFSIZE) + NUHEAD
C     Store the data array location and length
      INFO(NPPWRD+KIADAT) = IFLTAB(KFSIZE)
      INFO(NPPWRD+KINDAT) = NDATA
      IF (NLDATA.GE.0) THEN
      INFO(NPPWRD+KILNDA) = NLDATA
      ELSE
      INFO(NPPWRD+KILNDA) = NDATA
      ENDIF
      IFLTAB(KFSIZE) = IFLTAB(KFSIZE) + NDATA
C
      ELSE
C
C     Save header and data location in case they have changed
      INFO(NPPWRD+KINIHE) = NIHEAD
      INFO(NPPWRD+KIACHE) = INFO(NPPWRD+KIAIHE) + NIHEAD
      INFO(NPPWRD+KINCHE) = NCHEAD
      INFO(NPPWRD+KIAUHE) = INFO(NPPWRD+KIACHE) + NCHEAD
      INFO(NPPWRD+KINUHE) = NUHEAD
      INFO(NPPWRD+KIADAT) = INFO(NPPWRD+KIAUHE) + NUHEAD
      INFO(NPPWRD+KINDAT) = NDATA
      INFO(NPPWRD+KIUNUS) = JSIZE - NSIZE
C
      ENDIF
C
C     Update last written date, time, version, and program
      CALL CHRHOL (CDATE, 1, NDATEC, INFO(NPPWRD+KIDATE), 1)
      CALL CHRHOL (CTIME, 1, NTIMEC, INFO(NPPWRD+KITIME), 1)
      INFO(NPPWRD+KIVER) = INFO(NPPWRD+KIVER) + 1
      IF (INFO(NPPWRD+KIVER).GT.999) INFO(NPPWRD+KIVER) = 999
      INFO(NPPWRD+KITYPE) = ITYPE
      INFO(NPPWRD+KICOMP) = ICOMP
      INFO(NPPWRD+KIQUAL) = IQUAL
      INFO(NPPWRD+KIPREC) = IPREC
      CALL CHRHOL (CPROG, 1, NPROGC, INFO(NPPWRD+KIPROG), 1)
C
C     Store the information block
      ISIZE = NPPWRD + NINFO
      IF(MLEVEL.GE.14)WRITE(MUNIT,*)'-zowrit6:  Store info block'
      CALL zptrec6 (IFLTAB, INFO, ISIZE, IPNBIN(JPNBIN+NPPWRD+KBAINF),
     * .TRUE.)
C
C     If the permanent section of the file has changed, store it
      NADD = 1
      IF(MLEVEL.GE.14)WRITE(MUNIT,*)'-zowrit6:  Update root'
      CALL zptrec6 (IFLTAB, IFLTAB(KPERM), NPERM, NADD, .TRUE.)
C
C
 800  CONTINUE
      NLDATA = -1
      ICOMP = 0
      IQUAL = 0
      ITYPE = 0
      CTAG = ' '
      IRENAM = 0
      IF (MLEVEL.GE.12) WRITE (MUNIT,820)
 820  FORMAT (T8,'-----DSS---Debug: EXIT zowrit6')
      RETURN
C
C
 900  CONTINUE
C     Temporary debug statements
      WRITE (MUNIT, 902) JPNBIN, IHASH, IPBADD
 902  FORMAT (/' DSS Debug Information:',/'  JPNBIN:',I5,'  Hash:',I6,
     * '  Bin Address:',I9,/' INFO Block:')
      IZERO = 1
      CALL ZDEBUG ( MUNIT, INFO, IZERO, NINFO+NPPWRD)
      WRITE (MUNIT, 903)
 903  FORMAT (/,' Pathname Bin:')
      IZERO = JPNBIN
      CALL ZDEBUG ( MUNIT, IPNBIN, IZERO, NPPWRD+KBPINTL)
      NP = INFO(KINPAT)
      CALL zerror6 (IFLTAB, 11, 'zrdinf6', 0, NADD, CPATH, NPATH, CTPATH,
     * NP)
C
      END


      SUBROUTINE zcatfi6 (IFLTAB, LEXTND, LSELCA, LCDCAT, LTONLY, LERR)
C
C
C     Generate a catalog from a DSS file.
C     This may either be an extended version or a short one.
C     The extended one includes the last written date and time,
C     the record version number, and the program that
C     last wrote that record.
C
C     The extended version takes substantially longer than the short.
C
C     Written by Bill Charley at HEC, 1988.
C
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdsslz.h'
C
      INCLUDE 'zdsscz.h'
C
      INCLUDE 'zdsscm.h'
C
      INCLUDE 'zdssbz.h'
C
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdssmz.h'
       INCLUDE 'zdssShared.h'
C
C
      INTEGER IFLTAB(*)
      CHARACTER CPATH*400, CCPROG*10, CCDATE*10, CCTIME*10, CCTAG*10
      CHARACTER CDTYPE*3
      LOGICAL LMATCH, LEXTND, LSELCA, LCDCAT, LTONLY, LERR
C
      COMMON /ZSTATUS/ TOTAL_NUMB,  CURRENT_NUMB,
     *                 INTERRUPT, NERROR, MAXERROR
      INTEGER TOTAL_NUMB,  CURRENT_NUMB,  INTERRUPT
C
C
C
      IF (MLEVEL.GE.11) WRITE (MUNIT,20) IFLTAB(KUNIT)
 20   FORMAT (T6,'-----DSS---Debug:  Enter zcatfi6;  Unit:',I5)
      LTWCAT = .FALSE.
c     The follow is if another thread interrupts us to stop the process
      INTERRUPT = 0
C
C     Read the permanent section of the file
      CALL zrdprm6 (IFLTAB, .FALSE.)
      NRECS = IFLTAB(KNRECS)
      IADD = IFLTAB(KFSIZE) - 1
      CALL zgetrw6 (IADD, NR, IW)
      CCTAG = ' '
      JNPATH = 0
C
      TOTAL_NUMB = IFLTAB(KNRECS)
      CURRENT_NUMB = 0
C
C     Get the first pathname bin address
      IF (IFLTAB(KTABLE).EQ.1) THEN
      NBIN = IFLTAB(KBNBLK)
      ELSE IF (IFLTAB(KTABLE).EQ.2) THEN
      NBIN = IFLTAB(KHASH)
      ELSE
C     We should never get here
      GO TO 900
      ENDIF
C
C
C
C     Get the address of the first bin
      IADD = IFLTAB(KAFBIN)
      JJBUFF = 1
      JJREC = -2
C
C     Now read all pathname bins from the file
 40   CONTINUE
C
C     Read the pathname bin
C     Release the previous record
      IF (JCREC(JJBUFF).EQ.JJREC) LSBUFF(JJBUFF) = .FALSE.
      NBNSIZ = IFLTAB(KBNSIZ)
      CALL zgtrec6 (IFLTAB, IPNBIN, NBNSIZ, IADD, .TRUE.)
C     This next lines keeps that area in memory while we read elsewhere
      JJBUFF = JBUFF
      JJREC = JCREC(JBUFF)
      JPNBIN = 1
C
C     Loop through bin, looking for pathnames
 100  CONTINUE
C     Any more pathnames left?
      IF ((JPNBIN.LT.1).OR.(JPNBIN.GT.NBNSIZ)) GO TO 900
      IF (IPNBIN(JPNBIN).LE.0) GO TO 200
C     Yes - Compute the number of integer words in the pathname
      NBPWPA = ((IPNBIN(JPNBIN+KBNPAT)-1) / NCPW) + 1
*      IF ((NBPWPA.LT.0).OR.(NBPWPA.GT.200)) GO TO 900
      NBMWPA = ((IPNBIN(JPNBIN+KBNPAT)-1) / NCMW) + 1
*      IF ((NBMWPA.LT.0).OR.(NBMWPA.GT.200)) GO TO 900
C
C     Record status good? (not deleted or renamed?)
      IF ((IPNBIN(JPNBIN).EQ.1).OR.(IPNBIN(JPNBIN).EQ.11)) THEN
C
      JNPATH = JNPATH + 1
C
C     Yes.  Save this tag in the tag-hash code block
      IF (JNPATH.LE.NRECS) THEN
      IF (JNPATH.LT.NRECS) THEN
      IFUN = 0
      ELSE
      IFUN = 1
      ENDIF
      ENDIF
C
C
C     If another thead set the interrupt flag, return
      IF (INTERRUPT.NE.0) GO TO 840
*      CURRENT_NUMB = IREC
      CURRENT_NUMB = JNPATH
*      write(*,*)'current = ',CURRENT_NUMB
C
C     Is a short or extended version asked for
      IF (.NOT.LEXTND) THEN
C
C     Get pathname and pathname length
      NPATH = IPNBIN(JPNBIN+KBNPAT)
      CALL HOL2CH (IPNBIN(JPNBIN+KBPATH), CPATH, NBMWPA)
C     Should we show a pseudo regular interval pathname?
      IF (LPSEUDO.AND.(IPNBIN(JPNBIN+NBPWPA+KBPINTL).GT.0)) THEN
         CALL ZPseudoRTS6(CPATH(1:NPATH), CKPATH,
     *              IPNBIN(JPNBIN+NBPWPA+KBPINTL), 2, ISTAT)
         IF (ISTAT.EQ.0) THEN
            CPATH = CKPATH
            CALL CHRLNB(CPATH, NPATH)
         ENDIF
      ENDIF
C
C     Pick up the number of data and header words,
C     the data type, and the catalog tag identifier.
      NHEAD = IPNBIN(JPNBIN+KBNHEA+NBPWPA)
      NDATA = IPNBIN(JPNBIN+KBNDAT+NBPWPA)
      IDTYPE = IPNBIN(JPNBIN+KBTYPE+NBPWPA)
      CALL HOLCHR (IPNBIN(JPNBIN+KBTAG+NBPWPA), 1, NTAGC, CCTAG, 1)
C
      CALL ZSELCA(CPATH, NPATH, CCTAG(1:NTAGC),CCPROG, CCDATE, CCTIME,
     * CDTYPE, IDTYPE, IRVERS, NDATA, NHEAD, JNPATH, LSELCA, LMATCH,
     * LCDCAT, LERR)
      IF (LERR) GO TO 800
C
      ELSE
C
C     Extended Catalog
C
C     Get Information Block
      NADD = IPNBIN(JPNBIN+NBPWPA+KBAINF)
      CALL zgtrec6 (IFLTAB, INFO, NINFO+NBPWPA, NADD, .FALSE.)
C
C     Extract information from this array
      IRSTAT = INFO(KISTAT)
C     If this is an long pathname, set the status to 1
      IF (IRSTAT.EQ.11) IRSTAT = 1
      IF (IRSTAT.GT.1) GO TO 180
      IF (INFO(KIFLAG).NE.NPFLAG) GO TO 180
C     Get pathname and pathname length
      NPATH = INFO(KINPAT)
      CALL HOL2CH (INFO(KIPATH), CPATH, NBMWPA)
C
      NHEAD = INFO(NBPWPA+KINUHE)
      NDATA = INFO(NBPWPA+KILNDA)
      IDTYPE = INFO(NBPWPA+KITYPE)
      IRVERS = INFO(NBPWPA+KIVER)
      CALL HOLCHR (INFO(NBPWPA+KIPROG), 1, NPROGC, CCPROG, 1)
      CALL HOLCHR (INFO(NBPWPA+KIDATE), 1, NDATEC, CCDATE, 1)
      CALL HOLCHR (INFO(NBPWPA+KITIME), 1, NTIMEC, CCTIME, 1)
      CALL HOLCHR (INFO(NBPWPA+KITAG), 1, NTAGC, CCTAG, 1)
C
C     Get the record data type
      DO 140 I=1,NRTYPE
         IF (IDTYPE.EQ.IRTYPE(I)) THEN
            CDTYPE = CRTYPE(I)
            GO TO 150
         ENDIF
 140  CONTINUE
      CDTYPE = 'UND'
C
 150  CONTINUE
      CALL ZSELCA(CPATH, NPATH, CCTAG(1:NTAGC),CCPROG, CCDATE, CCTIME,
     * CDTYPE, IDTYPE, IRVERS, NDATA, NHEAD, JNPATH, LSELCA, LMATCH,
     * LCDCAT, LERR)
      IF (LERR) GO TO 800
C
      ENDIF
C
C     Have we reached the number of records in the file?
      IF (JNPATH.GE.NRECS) GO TO 800
C
C
      ELSE IF (IPNBIN(JPNBIN).EQ.-1) THEN
C     No more space in this block, read the next one
      GO TO 200
      ENDIF
C
C     Update the bin pointer (to next possible pathname location
C     within this block).
 180  CONTINUE
      JPNBIN = JPNBIN + NBPWPA + NLBIN
      IF ((JPNBIN.LT.1)) THEN
	GO TO 900
	ENDIF
C     Is that pointer too large?
      IF (JPNBIN.GT.(IFLTAB(KBNSIZ)-2)) GO TO 200
C     Go back and look for next pathnme within this bin
      GO TO 100
C
C
 200  CONTINUE
C     At this point, there are no more pathnames in the current bin
C     (or it is full and extends into another block).
C     Read the next pathname bin
      NBIN = NBIN - 1
C     Any more bins in this block?
      IF (NBIN.LE.0) THEN
C     No - Get pointer to next bin block (section).
C     Get location of pointers in bin
      I = IFLTAB(KBNSIZ)
C     Any more bins in the DSS file? (Exit to 800 if no more).
      IF (IPNBIN(I).EQ.0) GO TO 800
      IADD = IPNBIN(I)
      NBIN = IFLTAB(KBNBLK)
      ELSE
C     More bins available within this block - get next one.
      IADD = IADD + IFLTAB(KBNSIZ)
      ENDIF
C
C     Go back up and read next bin
      GO TO 40
C
C
C     No more bins or pathnames left.  All done
 800  CONTINUE
C
      IF (MLEVEL.GE.11) WRITE (MUNIT,820)
 820  FORMAT (T6,'-----DSS---Debug:  Exit  zcatfi6')
      RETURN
C
 840  CONTINUE
      IF (MLEVEL.GE.4) WRITE (MUNIT, 850)
 850  FORMAT (' ----- DSS --- zcatfi6:  Catalog interrupted.')
      GO TO 800
C
 900  CONTINUE
      CALL zerror6 (IFLTAB, 100, 'zcatfi6', 0, IFLTAB(KTABLE), ' ', 0,
     * ' ', 0)
C
      END


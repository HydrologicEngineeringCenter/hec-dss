      SUBROUTINE zuprts6 (IFLTAB, JULS, ISTIME, INTL, JULSD,
     * NSTART, NDATA, NVALS, VALUES, JQUAL, LQUAL, IPLAN, LERR)
C
C
C     Update Regular Interval Time-Series Data
C     Accomplishes essentially a zwrite6
C
C     This is an internal subroutine called by zsrts6 and zptdts6
C
C     Written by Bill Charley at HEC, 1988.
C
C
C     Passed arguments
      INTEGER IFLTAB(*), JQUAL(*)
      REAL VALUES(*)
      LOGICAL LEQNER, LERR, LQUAL
C
      CHARACTER CDATE1*9, CDATE2*9
C
      INCLUDE 'zdsskz.h'
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
      INCLUDE 'zdssts.h'
C
C
C
      IF (MLEVEL.GE.9) WRITE (MUNIT,20) IFLTAB(KUNIT)
 20   FORMAT (T8,'----DSS---Debug:  Enter zuprts6;  Unit:',I5)
C
      LERR = .FALSE.
C
C     This record has already been checked, and there is no
C     increase in the size of the data.
C
C     Update last written date, time, version, and program
      CALL CHRHOL (CDATE, 1, NDATEC, INFO(NPPWRD+KIDATE), 1)
      CALL CHRHOL (CTIME, 1, NTIMEC, INFO(NPPWRD+KITIME), 1)
      INFO(NPPWRD+KIVER) = INFO(NPPWRD+KIVER) + 1
      IF (INFO(NPPWRD+KIVER).GT.999) INFO(NPPWRD+KIVER) = 999
      INFO(NPPWRD+KIPREC) = IPREC
      CALL CHRHOL (CPROG, 1, NPROGC, INFO(NPPWRD+KIPROG), 1)
C
C     Now store the updated information block
      CALL zptrec6 (IFLTAB, INFO, NPPWRD+NINFO,
     * IPNBIN(JPNBIN+NPPWRD+KBAINF), .TRUE.)
C
C     Read the first word of the header array to check
C     for the same time offset
C?    CALL zgtrec6 (IFLTAB, IOFF, 1, INFO(NPPWRD+KIAIHE))
C?    IF ((IOFF.NE.ISOFF).AND.(MLEVEL.GE.3))
C?   * WRITE (MUNIT,50) IOFF, ISOFF
C50   FORMAT (' -----DSS--- zsrts6:  Caution - Writing to a'
C?   * ' Record with a different time offset',/,
C?   * ' Current Offset:',I8,'  minutes;  New Offset:',I8,' minutes')
C
C     Compute the location of the data that we need to store
C
C     Obtain the number of periods from the start of the stored
C     record to the current data position
      NPOS = NOPERS (INTL, 0, JULSD, 0, JULS, ISTIME) + NSTART - 1
      IF (NPOS.LT.1) GO TO 910
C
C     Find number of data to store for this record (NSTORE)
C
C     End of stored record controling
      NSTORE = NDATA - NPOS + 1
C     End of data to store controling
      J = NVALS - NSTART + 1
      IF (J.LT.NSTORE) NSTORE = J
C
      IF (MLEVEL.GE.9) THEN
      CALL JULDAT (JULS, 114, CDATE1, N)
      CALL JULDAT (JULSD, 114, CDATE2, N)
      WRITE (MUNIT,40) NSTART, NVALS, JULS, ISTIME, NDATA, JULSD,
     * CDATE1, CDATE2, LQUAL, LTOL, IPLAN
 40   FORMAT (T10,'NSTART:',I5,T25,'NVALS:',I5,T40,'JULS:',I8,
     * T55,'ISTIME:',I6,/,T10,'NDATA:',I5,T25,'JULSD:',I8,
     */,T10,'JULS: ',A,T30,'JULSD: ',A,/,
     * T10, 'Quality Flag: ',L1,'   Tol: ',L1,'   Plan:',I3)
C
      WRITE (MUNIT,45) NPOS, NDATA, NSTORE
 45   FORMAT (T10,' --- zuprts6 Calculations:   ',
     *'NPOS:',I5,'   NDATA:',I5,'   NSTORE:',I5)
      ENDIF
C
C
C     If we don't have data quality flags, store data directly
C
      IF (.NOT.LQUAL) THEN
C     Compute starting address of data to store
      IADD = INFO(NPPWRD+KIADAT) + NPOS - 1
C
C     Does IPLAN require us the read the current data first?
      IF (((IPLAN.EQ.0).OR.(IPLAN.EQ.2)).AND.(.NOT.LTOL)) THEN
C     No - just store the data
      CALL zptrec6 (IFLTAB, VALUES(NSTART), NSTORE, IADD, .FALSE.)
      ELSE
      CALL zgtrec6 (IFLTAB, BUFF, NSTORE, IADD, .TRUE.)
      J = NSTART - 1
      IF (IPLAN.EQ.1) THEN
      DO 60 I=1,NSTORE
      J = J + 1
      IF ((BUFF(I).EQ.-901.).OR.(BUFF(I).EQ.-902.)) BUFF(I) = VALUES(J)
 60   CONTINUE
      ELSE IF (IPLAN.EQ.4) THEN
      DO 80 I=1,NSTORE
      J = J + 1
      IF ((VALUES(J).NE.-901.).AND.(VALUES(J).NE.-902.))
     *    BUFF(I) = VALUES(J)
 80   CONTINUE
      ELSE IF (LTOL) THEN
      DO 90 I=1,NSTORE
      J = J + 1
      IF (.NOT.(LEQNER (BUFF(I), VALUES(J), TOL))) BUFF(I) = VALUES(J)
 90   CONTINUE
      ELSE
      DO 95 I=1,NSTORE
      J = J + 1
      BUFF(I) = VALUES(J)
 95   CONTINUE
      ENDIF
      CALL zptrec6 (IFLTAB, BUFF, NSTORE, IADD, .FALSE.)
      ENDIF
C
      ELSE
C
C     We have data quality flags
C     Compute starting address of data to store
      IADD = INFO(NPPWRD+KIADAT) + ((NPOS-1) * 2)
      ISTORE = NSTORE * 2
C
C     Does IPLAN require us the read the current data first?
      IF ((IPLAN.EQ.0).OR.(IPLAN.EQ.2)) THEN
C     No - just store the data
C     Prepare the array to store
      DO 100 I=1,NSTORE
      J = NSTART + I - 1
      K = ((I - 1) * 2) + 1
      BUFF(K) = VALUES(J)
      K = K + 1
      ILBUFF(K) = JQUAL(J)
 100  CONTINUE
      CALL zptrec6 (IFLTAB, BUFF, ISTORE, IADD, .FALSE.)
C
      ELSE
      CALL zgtrec6 (IFLTAB, BUFF, ISTORE, IADD, .TRUE.)
C
      IF (IPLAN.EQ.1) THEN
      DO 120 I=1,NSTORE
      J = NSTART + I - 1
      K = ((I - 1) * 2) + 1
      IF (BUFF(K).EQ.-901.) THEN
      BUFF(K) = VALUES(J)
      K = K + 1
      ILBUFF(K) = JQUAL(J)
      ENDIF
 120  CONTINUE
C
      ELSE
      DO 140 I=1,NSTORE
      J = NSTART + I - 1
      K = ((I - 1) * 2) + 1
      IF (VALUES(J).NE.-901.) THEN
      BUFF(K) = VALUES(J)
      K = K + 1
      ILBUFF(K) = JQUAL(J)
      ENDIF
 140  CONTINUE
      ENDIF
C
      CALL zptrec6 (IFLTAB, BUFF, ISTORE, IADD, .FALSE.)
      ENDIF
C
      ENDIF
C
C
C     Update the position of VALUES for the next record
      NSTART = NSTART + NSTORE
C
C
 800  CONTINUE
      IF (MLEVEL.GE.12) WRITE (MUNIT,820)
 820  FORMAT (T8,'----DSS---Debug:  Exit  zuprts6')
      RETURN
C
C
C     Probably will never get here - a terse error message is ok
C     (This error will be picked up by zmovbk6!)
 910  CONTINUE
      WRITE (MUNIT,911) JULS, ISTIME
 911  FORMAT (/,' ****ERROR - zuprts6:  Illegal starting date/time',/,
     * ' Julian date:',I8,'  Time:',I8,/)
      LERR = .TRUE.
      GO TO 800
C
      END


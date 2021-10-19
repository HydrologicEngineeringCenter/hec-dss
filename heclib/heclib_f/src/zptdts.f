      SUBROUTINE zptdts (IFLTAB, CA, NA, CB, NB, CC, NC, CF, NF,
     * JULS, ISTIME, JULE, IETIME, INTL, IHEAD, NHEAD, BUFF,
     * VALUES, NVALS, CUNITS, CTYPE, ISTAT)
C
C
C     Replaced by zsrts6
C
C
      INTEGER IFLTAB(*), IHEAD(*)
      REAL VALUES(*)
      CHARACTER CA*(*), CB*(*), CC*(*), CF*(*), CUNITS*(*), CTYPE*(*)
      CHARACTER CPATH*392, CE*12, CDATE*9, CTIME*4
C
C
C
      IPLAN = 0
      IF ((ISTAT.LT.0).AND.(ISTAT.GT.-5)) IPLAN = IABS(ISTAT)
      CE = ' '
      I = 2
      CALL zgintl6 (INTL, CE, N, I)
      IF  (I.NE.0) GO TO 900
C
      ND = 0
      NE = 12
      CPATH = ' '
      CALL zfpn (CA, NA, CB, NB, CC, NC, ' ', ND, CE, NE,
     * CF, NF, CPATH, NPATH)
C
      CALL DATCLN (JULS, ISTIME, JUL, IT)
      CALL JULDAT (JUL, 104, CDATE, N)
      N = M2IHM (IT, CTIME)
C
      NVALS = NOPERS (INTL,0,JULS,ISTIME,JULE,IETIME) + 1
      IF (NVALS.LE.0) GO TO 910
C
C
      BASEV = 0.0
      NPREC = 0
      IF (NHEAD.GT.21) THEN
         JHEAD = NHEAD - 21
      ELSE
         JHEAD = 0
      ENDIF
      CALL zsrtsx ( IFLTAB, CPATH(1:NPATH), CDATE, CTIME, NVALS,
     * VALUES, IQUAL, .FALSE., CUNITS, CTYPE, IHEAD(22), JHEAD, IPLAN,
     * 0, BASEV, .FALSE., .FALSE., NPREC, ISTAT)
C
 800  CONTINUE
      RETURN
C
 900  CONTINUE
      !WRITE (MUNIT, 901) INTL
 901  FORMAT (/,' -----DSS---zptdts6;  ERROR:  Unrecognizable Time',
     * ' Interval',/' Interval Specified:',I6,/)
      ISTAT = 12
      GO TO 800
C
 910  CONTINUE
      !WRITE (MUNIT, 911) NVALS
 911  FORMAT (/,' -----DSS---zptdts6;  ERROR:  Illegal Time Window',
     * ' Given',/' Number of Values:',I6,/)
      !WRITE (MUNIT, 912) CDATE, CTIME, JULS
 912  FORMAT (' Starting Date: ',A,',  Time: ',A,I8)
      CALL DATCLN (JULE, IETIME, JUL, IT)
      CALL JULDAT (JUL, 104, CDATE, N)
      N = M2IHM (IT, CTIME)
      !WRITE (MUNIT, 913) CDATE, CTIME, JULE
 913  FORMAT (' Ending Date: ',A,',  Time: ',A,I8,/)
      ISTAT = 15
      GO TO 800
C
      END


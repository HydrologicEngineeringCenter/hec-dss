      SUBROUTINE zrrtsb6 (IFLTAB, JULS, ISTIME, INTL, JULSD, NSTART,
     * NLDATA, NADATA, NVALS,  LGETDOB, LFILDOB, SVALUES, DVALUES,
     * JQUAL, LQBLOK, LQUAL, JCOMP, LFOUND, ISTAT)
C
C
C     Get Regular Interval Time-Series Data Block
C     Accomplishes essentially a zread6
C
C     This is an internal subroutine called by zrrtsx6
C
C     Written by Bill Charley at HEC, 1989.
C
C
C     Passed arguments
      INTEGER IFLTAB(*), JQUAL(*)
      REAL SVALUES(*)
      DOUBLE PRECISION DVALUES(*)
      LOGICAL LGETDOB, LFILDOB, LFOUND, LQUAL, LQBLOK
C
      CHARACTER CDATE1*15, CDATE2*15
C
      COMMON /WORDS/ IWORD(10)
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdssmz.h'
C
      INCLUDE 'zdssts.h'
C
      INCLUDE 'zdssdc.h'
C
C
C
      IF (MLEVEL.GE.9) WRITE (MUNIT,20) IFLTAB(KUNIT)
 20   FORMAT (T8,'----DSS---Debug:  Enter zrrtsb6;  Unit:',I5)
C
C
C
C     Obtain the number of periods from the start of the stored
C     record to the current data position
      NPOS = NOPERS (INTL, 0, JULSD, 0, JULS, ISTIME) + NSTART - 1
      IF (NPOS.LT.1) GO TO 900
C
C     Find number of data to read for this record (NREAD)
C
C     End of stored record controlling
      NREAD = NLDATA - NPOS + 1
C     End of data desired controlling
      J = NVALS - NSTART + 1
      IF (J.LT.NREAD) NREAD = J
      ILIM = NSTART + NREAD - 1
C
      IF (MLEVEL.GE.9) THEN
      CALL JULDAT (JULS, 104, CDATE1, N)
      CALL JULDAT (JULSD, 104, CDATE2, N)
      WRITE (MUNIT,40) NSTART, NVALS, JULS, ISTIME, NLDATA, JULSD,
     * CDATE1, CDATE2, LQBLOK, LQUAL
 40   FORMAT (T10,'NSTART:',I9,5X,'NVALS:',I9,5X,'JULS:',I8,
     * 5X,'ISTIME:',I6,/,T10,'NLDATA:',I8,5X,'JULSD:',I8,
     */,T10,'JULS: ',A,5X,'JULSD: ',A,/,
     * T10, 'Quality Read: ',L1,',  Quality Requested:',L2)
C
      WRITE (MUNIT,45) NPOS, NLDATA, NREAD, ILIM
 45   FORMAT (T10,'---zrrtsb6 Calculations:  ',
     *'NPOS:',I8,'  NDATA:',I8,'  NREAD:',I8,'  ILIM:',I8)
      ENDIF
C
C
      IF (LFOUND) THEN
C
C     If the compression flag was set, uncompress the data
C     (If data compression used, record cannot be double)
C
      IF (INFO(NPPWRD+KICOMP).GT.0) THEN
      JCOMP = INFO(NPPWRD+KICOMP)
C     Get the compression header
      NDCH = INFO(NPPWRD+KINCHE)
      NDCH = MIN0 (NDCH, KDCH)
      CALL zgtrec6 (IFLTAB, IDCH, NDCH, INFO(NPPWRD+KIACHE), .FALSE.)
      IF (IFLTAB(KSTAT).NE.0) GO TO 800
C
C     Don't uncompress the data if we do not have enough room
      IF (INFO(NPPWRD+KILNDA).GT.KSBUFF) THEN
C        Try and use the integer buffer, if we have room!
         IF (NADATA.GT.NIBUFF) GO TO 910
         CALL zgtrec6 (IFLTAB, INTBUF, NADATA, INFO(NPPWRD+KIADAT),
     *    .FALSE.)
         IF (IFLTAB(KSTAT).NE.0) GO TO 800
C        Uncompress the data, with new data in buffer
         NELMS = -1
         CALL DUREAL(BUFF, KLBUFF, 1, NELMS, INTBUF, NADATA, NIBUFF,
     *    IDCH, NDCH, IST)
         IF (IST.NE.0) GO TO 920
      ELSE
C
C        Get the data block
         CALL zgtrec6 (IFLTAB, SBUFF2, NADATA, INFO(NPPWRD+KIADAT),
     *    .FALSE.)
         IF (IFLTAB(KSTAT).NE.0) GO TO 800
C        Uncompress the data, with new data in buffer
         NELMS = -1
         CALL DUREAL(SBUFF1, KSBUFF, 1, NELMS, SBUFF2, NADATA, KSBUFF,
     *    IDCH, NDCH, IST)
         IF (IST.NE.0) GO TO 910
C
      ENDIF
C
      M = NPOS - 1
      IF (LGETDOB) THEN
         DO 60 I=NSTART, ILIM
            M = M + 1
            DVALUES(I) = BUFF(M)
 60      CONTINUE
      ELSE
         DO 70 I=NSTART, ILIM
            M = M + 1
            SVALUES(I) = BUFF(M)
 70      CONTINUE
      ENDIF
C
      ELSE
C
C     No data compression...
C     If we don't have data quality flags for single data sets,
C     or we have double data, read the data directly
C
      IF ((.NOT.LQBLOK).OR.(LFILDOB)) THEN
C        Compute starting address of data
         IF (LGETDOB.EQV.LFILDOB) THEN
            IF (LGETDOB) THEN
C              Read doubles
               IADD = INFO(NPPWRD+KIADAT) + (NPOS*2) - 2
               N = NREAD * IWORD(10)
               CALL zgtrec6 (IFLTAB, DVALUES(NSTART), N, IADD, .FALSE.)
               IF (IFLTAB(KDSWAP).NE.0) CALL zdswap6(DVALUES(NSTART), N)
            ELSE
               IADD = INFO(NPPWRD+KIADAT) + NPOS - 1
               CALL zgtrec6(IFLTAB, SVALUES(NSTART), NREAD,IADD,.FALSE.)
            ENDIF
         ELSE
C           Need to translate word types
            IF (LGETDOB) THEN
C              Translate single (file) into double (requested)
               IF (NREAD.GT.KSBUFF) GO TO 910
               IADD = INFO(NPPWRD+KIADAT) + NPOS - 1
               CALL zgtrec6 (IFLTAB, BUFF, NREAD, IADD, .FALSE.)
               M = 0
               DO 80 I=NSTART, ILIM
                  M = M + 1
                  DVALUES(I) = BUFF(M)
 80            CONTINUE
            ELSE
C              Translate doubles (file) into singles (requested)
               IF (NREAD.GT.KDBUFF) GO TO 910
               IADD = INFO(NPPWRD+KIADAT) + (NPOS*2) - 2
               N = NREAD * IWORD(10)
               CALL zgtrec6 (IFLTAB, DBUFF, N, IADD, .FALSE.)
               IF (IFLTAB(KDSWAP).NE.0) CALL zdswap6(DBUFF, N)
               M = 0
               DO 100 I=NSTART, ILIM
                  M = M + 1
                  SVALUES(I) = DBUFF(M)
 100           CONTINUE
            ENDIF
         ENDIF
         IF (IFLTAB(KSTAT).NE.0) GO TO 800
C
         IF (LQUAL) THEN
C           Data quality requested.
C           If double data, and we have it, read it directly
C           If single data, we don't have it.  Fill with zeros
            IF (LQBLOK.AND.LFILDOB) THEN
C              flags live after the data block for doubles
               IADD = INFO(NPPWRD+KIADAT) + NPOS - 1 +
     *                (INFO(NPPWRD+KILNDA) * 2)
               CALL zgtrec6(IFLTAB, JQUAL(NSTART), NREAD, IADD, .FALSE.)
            ELSE
               DO 160 I=NSTART,ILIM
                  JQUAL(I) = 0
 160           CONTINUE
            ENDIF
         ENDIF
C
      ELSE
C
C        We have single data with data quality flags
C
C        Compute starting address of data
         IADD = INFO(NPPWRD+KIADAT) + ((NPOS-1) * 2)
         IREAD = NREAD * 2
         CALL zgtrec6 (IFLTAB, BUFF, IREAD, IADD, .FALSE.)
         IF (IFLTAB(KSTAT).NE.0) GO TO 800
C
         IF (LGETDOB) THEN
C           We have single data on disk, but requested doubles
            DO 180 I=1,NREAD
               J = NSTART + I - 1
               K = ((I - 1) * 2) + 1
               DVALUES(J) = DBLE(BUFF(K))
               IF (LQUAL) THEN
                  K = K + 1
                  JQUAL(J) = ILBUFF(K)
               ENDIF
 180        CONTINUE
         ELSE
            DO 190 I=1,NREAD
               J = NSTART + I - 1
               K = ((I - 1) * 2) + 1
               SVALUES(J) = BUFF(K)
               IF (LQUAL) THEN
                  K = K + 1
                  JQUAL(J) = ILBUFF(K)
               ENDIF
 190        CONTINUE
         ENDIF
C
      ENDIF
C
      ENDIF
C
      ELSE
C
C     Record not found.  Fill in -902.'s
      IF (LGETDOB) THEN
         DO 200 I=NSTART,ILIM
            DVALUES(I) = -902.0
            IF (LQUAL) JQUAL(I) = 0
 200     CONTINUE
      ELSE
         DO 220 I=NSTART,ILIM
            SVALUES(I) = -902.0
            IF (LQUAL) JQUAL(I) = 0
 220     CONTINUE
      ENDIF
      ENDIF
C
C
C     Update the position of VALUES for the next record
      NSTART = NSTART + NREAD
C
C
 800  CONTINUE
      IF (MLEVEL.GE.12) WRITE (MUNIT,820)
 820  FORMAT (T8,'----DSS---Debug:  Exit  zrrtsb6')
      RETURN
C
C
C     Probably will never get here - a terse error message is ok
 900  CONTINUE
      WRITE (MUNIT,901) JULS, ISTIME
 901  FORMAT (/,' **** ERROR - zrrtsb6:  Illegal starting date/time',/,
     * ' Julian date:',I8,'  Time:',I8,/)
      ISTAT = 15
      GO TO 800
C
 910  CONTINUE
      WRITE (MUNIT,911) NADATA, NIBUFF
 911  FORMAT (/,' **** ERROR - zrrtsb6:  Insufficient internal memory',
     * ' to Uncompress Data',/,
     * ' Amount required:',I5,',  Amount available:',I5)
      ISTAT = 54
      GO TO 800
C
 920  CONTINUE
      WRITE (MUNIT,921) IST
 921  FORMAT (/,' **** ERROR - zrrtsb6:  Unable to Uncompress Data',/,
     * ' Status:',I6,/)
      ISTAT = 53
      GO TO 800
C
      END


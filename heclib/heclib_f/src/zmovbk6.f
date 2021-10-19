      SUBROUTINE zmovbk6 (SVALUES, DVALUES, LDOUBLE, LFILDOB,
     * JQUAL, LQVALS, LQBLOK, NSTART, NVALS,
     * JULS, ISTIME, INTL, BLOCK, DBLOCK, IBLOCK, NDATA,
     * JULSD, ISTAT, IFLAG)
C
C
C     INTERNAL SUBROUTINE FOR zptdts6 AND zgtdts6
C     GIVEN A BUFFER OF INFO, WHICH STARTS AT PERIOD NSTART,
C     AND ENDS AT NVALS, WITH THE TIME FOR PERIOD 1 OF JULS
C     DAYS AND ISTIME MINUTES.
C
C     PUT INFO IN ARRAY BLOCK AND UP TO MAX OF NDATA, WITH
C     TIME FOR LOCATION 1 OF JULSD.  INTERVAL OF VALUES
C     IS INTL IN MINUTES.
C
C     VALUES is the array passed back to the users routine
C     BLOCK is the array read or written to the DSS file
C
C     IF IFLAG = 1, MOVE VALUES INTO BLOCK
C     IF IFLAG = -1, UPDATE VALUES INTO BLOCK
C
C
      REAL SVALUES(*), BLOCK(*)
      INTEGER JQUAL(*), IBLOCK(*)
      DOUBLE PRECISION DVALUES(*), DBLOCK(*)
C
      CHARACTER CDATE1*9, CDATE2*9
      LOGICAL LQVALS, LQBLOK, LEQNER, LDOUBLE, LFILDOB, LEQNERD
      LOGICAL ztstqp6
C
      INCLUDE 'zdssmz.h'
C
      INCLUDE 'zdsslz.h'
C
      INCLUDE 'zdssnz.h'
C
      external lismissingf
      logical lismissingf
      external lismissingd
      logical lismissingd
C
C
      ISTAT = 0
      IF (MLEVEL.GE.9) THEN
      CALL JULDAT (JULS, 114, CDATE1, N)
      CALL JULDAT (JULSD, 114, CDATE2, N)
      WRITE (MUNIT,10) NSTART, NVALS, JULS, ISTIME, NDATA, JULSD,
     * CDATE1, CDATE2, LQVALS, LQBLOK
 10   FORMAT (T10,'----- ENTERING zmovbk6 ',
     */,T5,'NSTART:',I5,T20,'NVALS:',I5,/,T5,'JULS:',I5,
     *T20,'ISTIME:',I5,/,T5,'NDATA:',I5,T20,'JULSD:',I5,
     */,T5,'JULS:',A,T25,'JULSD:',A,/,T5,
     * 'Value Quality Flag: ',L1,',  Block Qualtiy Flag: ',L1)
      ENDIF
C
C
C     GET THE NUMBER OF PERIODS FROM START OF BLOCK TO VALUES
      NPOS = NOPERS (INTL, 0, JULSD, 0, JULS, ISTIME) + NSTART - 1
      IF (NPOS.LT.1) GO TO 910
C
C     Find Upper Limit
C
      NSTORE = NDATA - NPOS + 1
      J = NVALS - NSTART + 1
      IF (J.LT.NSTORE) NSTORE = J
      ILIM = NSTART + NSTORE - 1
C
      IF  (MLEVEL.GE.9) WRITE (MUNIT,25) NPOS, NDATA, NSTORE, ILIM
 25   FORMAT (T10,' --- zmovbk6 CALCULATIONS --',
     */,T5,'NPOS:',I5,T20,'NDATA:',I5,
     */,T5,'NSTORE:',I5,T20,'ILIM:',I5)
C
C     Now move block
C     Prepare block to be stored (called by zsrts6)
C
C     Is data quality used?
      IF ((.NOT.LQVALS).AND.(.NOT.LQBLOK)) THEN
C        No quality
         M = NPOS - 1
C
         IF (LDOUBLE.EQV.LFILDOB) THEN
            IF (.NOT.LDOUBLE) THEN
C              Copy single array to single
               DO 100 I=NSTART,ILIM
                  M = M + 1
                  IF (IFLAG.LT.0) THEN
                     IF (IFLAG.EQ.-1) THEN
                        IF (.NOT.lismissingf(BLOCK(M)))go to 100
                     ELSE IF (IFLAG.EQ.-4) THEN
                        IF (lismissingf(SVALUES(I)))GO TO 100
                     ENDIF
                  ENDIF
                  IF (LTOL) THEN
                     IF (.NOT.(LEQNER(BLOCK(M), SVALUES(I),
     *               TOL))) BLOCK(M) = SVALUES(I)
                  ELSE
                     BLOCK(M) = SVALUES(I)
                  ENDIF
 100           CONTINUE
C
            ELSE
C              Copy double array to double
               DO 120 I=NSTART,ILIM
                  M = M + 1
                  IF (IFLAG.LT.0) THEN
                     IF (IFLAG.EQ.-1) THEN
                        IF (.NOT.lismissingd(DBLOCK(M))) GO TO 120
                     ELSE IF (IFLAG.EQ.-4) THEN
                        IF (lismissingd(DVALUES(I))) GO TO 120
                     ENDIF
                  ENDIF
                  IF (LTOL) THEN
                     IF (.NOT.(LEQNERD(DBLOCK(M), DVALUES(I), TOL)))
     *                   DBLOCK(M) = DVALUES(I)
                  ELSE
                     DBLOCK(M) = DVALUES(I)
                  ENDIF
 120           CONTINUE
            ENDIF
C
         ELSE
C
            IF (LFILDOB) THEN
C              Double (DBLOCK) on disk, inputing single (SVALUES)
C              Copy single array to double
               DO 140 I=NSTART,ILIM
                  M = M + 1
                  IF (IFLAG.LT.0) THEN
                     IF (IFLAG.EQ.-1) THEN
                        IF (.NOT.lismissingd(DBLOCK(M))) GO TO 140
                     ELSE IF (IFLAG.EQ.-4) THEN
                        IF (lismissingd(SVALUES(I))) GO TO 140
                     ENDIF
                  ENDIF
                  IF (LTOL) THEN
                     IF (.NOT.(LEQNER(SNGL(DBLOCK(M)), SVALUES(I),
     *               TOL))) DBLOCK(M) = DBLE (SVALUES(I))
                  ELSE
                     DBLOCK(M) = DBLE (SVALUES(I))
                  ENDIF
 140           CONTINUE
            ELSE
C
C              Single (SBLOCK) on disk, inputing doubles (DVALUES)
C              This is not leagl, and is taken care of in zsrtsi6
               GO TO 920
            ENDIF
C
         ENDIF
C
      ELSE
C
C        Data quality used
         IF (.NOT.LDOUBLE) THEN
C           Copy single array to single
            M = ((NPOS - 1) * 2) - 1
            DO 160 I=NSTART,ILIM
               M = M + 2
               IF (IFLAG.LT.0) THEN
                  IF (IFLAG.EQ.-1) THEN
                      IF (.NOT.lismissingf(BLOCK(M))) GO TO 160
                  ELSE IF (IFLAG.EQ.-4) THEN
                     IF (.NOT.lismissingf(SVALUES(I))) GO TO 160
                  ENDIF
               ENDIF
C              Check for quality protection bit
               IF (LQPBIT.AND.LQVALS) THEN
C                 If protection bit on, and
                  IF (ztstqp6(IBLOCK(M+1)).AND.(.NOT.ztstqp6(JQUAL(I))))
     *                GO TO 160
               ENDIF
C              Copy data value
               IF (LTOL) THEN
                  IF (.NOT.(LEQNER(BLOCK(M), SVALUES(I), TOL)))
     *            BLOCK(M) = SVALUES(I)
               ELSE
                  BLOCK(M) = SVALUES(I)
               ENDIF
C              Copy quality value
               IF (LQVALS) THEN
                  IBLOCK(M+1) = JQUAL(I)
               ELSE
                  IBLOCK(M+1) = 0
               ENDIF
 160        CONTINUE
C
         ELSE
C
C           Copy double array to double
            M = NPOS - 1
            DO 180 I=NSTART,ILIM
               M = M + 1
               IF (IFLAG.LT.0) THEN
                  IF (IFLAG.EQ.-1) THEN
                     IF (.NOT.lismissingd(DBLOCK(M))) GO TO 180
                  ELSE IF (IFLAG.EQ.-4) THEN
                     IF (lismissingd(DVALUES(I))) GO TO 180
                  ENDIF
               ENDIF
C              Check for quality protection bit
               IF (LQPBIT.AND.LQVALS) THEN
C                 If protection bit on, and
                  IF (ztstqp6(IBLOCK((NDATA*2)+M)).AND.
     *               (.NOT.ztstqp6(JQUAL(I)))) GO TO 180
               ENDIF
C              Copy data value
               IF (LTOL) THEN
                  IF (.NOT.(LEQNER(SNGL(DBLOCK(M)), SNGL(DVALUES(I)),
     *            TOL))) DBLOCK(M) = DVALUES(I)
               ELSE
                  DBLOCK(M) = DVALUES(I)
               ENDIF
C              Copy quality value
               IF (LQVALS) THEN
                  IBLOCK((NDATA*2)+M) = JQUAL(I)
               ELSE
                  IBLOCK((NDATA*2)+M) = 0
               ENDIF
 180        CONTINUE
         ENDIF
      ENDIF
C
C
C     Update pointers
      NSTART = ILIM + 1
      RETURN
C
C     ------ERROR CONDITIONS-------
C
 910  CONTINUE
      CALL JULDAT (JULS, 114, CDATE1, N)
      CALL JULDAT (JULSD, 114, CDATE2, N)
      WRITE (MUNIT, 911) JULS, ISTIME, NPOS, JULSD, CDATE1, CDATE2
 911  FORMAT (//,' ***** ERROR - zmovbk6 - Illegal Starting Date',
     +4(2X,I6)/,' Start Date: ',A,'  Block Date: ',A/)
      ISTAT = 21
      RETURN
C
 920  CONTINUE
C     Probably cannot get here
      WRITE (MUNIT, 921)
 921  FORMAT (/, ' ***** ERROR - zmovbk6 - Illegal operation')
      ISTAT = 22
      RETURN
C
      END


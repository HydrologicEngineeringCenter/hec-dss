      SUBROUTINE zsrst(
       !------------------!
       ! file/record info !
       !------------------!
     * IFLTAB, ! file table
     * CPATH,  ! pathname
       !--------------!
       ! general info !
       !--------------!
     * CLOC,   ! location string
     * CATIM,  ! activated time string
     * CHPARM, ! height parameter string
     * CHUNIT, ! height units string
     * CFPARM, ! flow parameter string
     * CFUNIT, ! flow units string
     * CCMT,   ! comment string
     * COPT,   ! option string
     * LSNUM,  ! flag specifying if rating has serial number
     * ISNUM,  ! rating serial number
     * LDATUM, ! flag specifying if rating has datum elevation
     * DATUM,  ! rating datum elevation
       !------------------!
       ! base rating info !
       !------------------!
     * LSTAGE, ! flag for stage heights (vs elevation heights)
     * IBHI,   ! height interpolation type for base curve
     * IBHU,   ! height underflow handling type for base curve
     * HUV,    ! height underflow value (may or may not be used)
     * IBHO,   ! height overflow handling type for base curve
     * HOV,    ! height overflow value (may or may not be used)
     * IBFI,   ! flow interpolation type for base curve
     * IBFU,   ! flow underflow handling type for base curve
     * FUV,    ! flow underflow value (may or may not be used)
     * IBFO,   ! flow overflow handling type for base curve
     * FOV,    ! flow overflow value (may or may not be used)
     * NBASE,  ! number of points in base curve
     * HEIGHT, ! stages or elevations
     * FLOW,   ! flows
       !------------!
       ! shift info !
       !------------!
     * NSHIFT, ! number of constant or variable shifts
     * ISTI,   ! interpolation type for shift effective times
     * ISTU,   ! underflow handling type for shift effective times
     * ISTO,   ! overflow handling type for shift effective times
     * ISBI,   ! interpolation type for shift breakpoints for each
               ! shift
     * ISBU,   ! underflow handling type for shift breakpoints for each
               ! shift
     * SBUV,   ! shift breakpoints underflow values for each shift
     * ISBO,   ! overflow handling type for shift breakpoints for each
               ! shift
     * SBOV,   ! shift breakpoints overflow values for each shift
     * ISHI,   ! interpolation type for shift heights for each shift
     * ISHU,   ! underflow handling type for shift heights for each
               ! shift
     * SHUV,   ! shift heights underflow values for each shift
     * ISHO,   ! overflow handling type for shift heights for each shift
     * SHOV,   ! shift heights overflow values for each shift
     * LATMSK, ! masks specifying whether each shift has activated time
     * JBDATE, ! julian base date for shift times
     * IETIME, ! effective times for shifts (minutes relative to jbdate)
     * IATIME, ! activated times for shifts (minutes relative to jbdate)
     * NSHFTP, ! number of variable shift breakpoints for each shift
               ! (0 = constant shift)
     * SHIFTS, ! shift points (for each shift, constant shift or
               ! break(1), height(1),...break(n),height(n))
       !-------------!
       ! offset info !
       !-------------!
     * LCOFF,  ! flag specifying whether a constant offset is used
     * COFVAL, ! constant offset value
     * NOFF,   ! number of variable offset breakpoint, height pairs
     * IOBI,   ! interpolation type for variable offset breakpoints
     * IOBU,   ! underflow handling type for variable offset breakpoints
     * OBUV,   ! offset breakpoint underflow value (may/may not be used)
     * IOBO,   ! overflow handling type for variable offset breakpoints
     * OBOV,   ! offset breakpoint oferflow value (may/may not be used)
     * IOHI,   ! interpolation type for variable offset heights
     * IOHU,   ! underflow handling type for variable offset heights
     * OHUV,   ! offset height underflow value (may or may not be used)
     * IOHO,   ! overflow handling type for variable offset heights
     * OHOV,   ! offset height oferflow value (may or may not be used)
     * OFFSET, ! variable offset values (break(1), height(1),...
               ! break(n),height(n))
       !-----------!
       ! misc info !
       !-----------!
     * IHORIZ, ! horizontal axis
     * IPLAN,  ! overwrite plan
     * ISTAT   ! success/failure status
     * )
c
c     Store STREAM RATING
c
c     Mike Perryman
c     Nov, 2003
c
      PARAMETER (KUHEAD=400)

      INTEGER*4 IFLTAB(*), ISNUM, IBHI, IBHO, IBHU, IBFI, IBFO, IBFU,
     * NBASE, NSHIFT, ISTI, ISTU, ISTO, ISBI(*), ISBU(*), ISBO(*),
     * ISHI(*), ISHU(*), ISHO(*), JBDATE, IETIME(*), IATIME(*),
     * NSHFTP(*), NOFF, IOBI, IOBU, IOBO, IOHI, IOHU, IOHO, IHORIZ,
     * IPLAN, ISTAT, NPATH, NLOC, NATIM, NHPARM, NHUNIT, NFPARM,
     * NFUNIT, NCMT, EFFTIM, TMPTIM, NA, NB, NC, ND, NE, NF, NUHEAD,
     * IUHEAD(KUHEAD)

      CHARACTER CPATH*(*), CLOC*(*), CATIM*(*), CHPARM*(*), CHUNIT*(*),
     * CFPARM*(*), CFUNIT*(*), CCMT*(*), COPT*(*), CA*80, CB*80, CC*80,
     * CD*80, CE*80, CF*80, CTEMP1*80, CTEMP2*80

      LOGICAL*4 LSNUM, LDATUM, LSTAGE, LATMSK(*), LCOFF, LFOUND

      REAL*4 DATUM, HUV, HOV, FUV, FOV, HEIGHT(*), FLOW(*), SBUV(*),
     * SBOV(*), SHUV(*), SHOV(*), SHIFTS(*), COFVAL, OBUV, OBOV, OHUV,
     * OHOV, OFFSET(*), RUHEAD(KUHEAD)

      EQUIVALENCE (IUHEAD, RUHEAD)

      INCLUDE 'zdsskz.h'
      INCLUDE 'zdssiz.h'
      INCLUDE 'zdssmz.h'
      INCLUDE 'zdssbf.h'
      INCLUDE 'zdssrst.h'

      CALL CHRLNB(CPATH, NPATH)
      CALL CHRLNB(CLOC, NLOC)
      CALL CHRLNB(CATIM, NATIM)
      CALL CHRLNB(CHPARM, NHPARM)
      CALL CHRLNB(CHUNIT, NHUNIT)
      CALL CHRLNB(CFPARM, NFPARM)
      CALL CHRLNB(CFUNIT, NFUNIT)
      CALL CHRLNB(CCMT, NCMT)
      CALL CHRLNB(COPT, NOPT)

      !---------------------!
      ! debug entry message !
      !---------------------!
      IF (MLEVEL .GE. 7) THEN
         WRITE(MUNIT, 10) NBASE, NSHIFT, IHORIZ, LSTAGE, IPLAN,
     *    CPATH(1:NPATH)
      END IF
   10 FORMAT (T5, '----- Enter zsrst -----', /,
     * T11, 'NBASE:', I6, ', NSHIFT:', I4, ', IHORIZ:', I4, /,
     * T11, 'LSTAGE:', L1, ', IPLAN:', I3, /, T11, 'Pathname: ', A)

      !--------------------------------------------------------!
      ! get the effective time from the d part of the pathname !
      !--------------------------------------------------------!
      CALL zufpn(CA, NA, CB, NB, CC, NC, CD, ND, CE, NE, CF, NF,
     * CPATH, NPATH, ISTAT)
      IF (ISTAT .NE. 0 .OR. ND .LT. 7) GO TO 9880
      CALL RATTIM(CD, ND, EFFTIM, ISTAT)
      IF (ISTAT .NE. 0) GO TO 9880

      !----------------------------!
      ! check for error conditions !
      !----------------------------!
      IF (NBASE .LT. 1) GO TO 9990
C      IF (UBOUND(HEIGHT) .LT. NBASE) GO TO 9980
C      IF (UBOUND(FLOW) .LT. NBASE) GO TO 9970

      LFOUND = .FALSE.
      I = 1
      DO WHILE (.NOT. LFOUND .AND. (I .LE. NINTYP))
         IF (IBHI .EQ. IINTYP(I)) LFOUND = .TRUE.
         I = I + 1
      END DO
      IF (.NOT. LFOUND) GO TO 9960

      LFOUND = .FALSE.
      I = 1
      DO WHILE (.NOT. LFOUND .AND. (I .LE. NEXTYP))
         IF (IBHU .EQ. IEXTYP(I)) LFOUND = .TRUE.
         I = I + 1
      END DO
      IF (.NOT. LFOUND) GO TO 9950
      IF (IBHU .GE. 16) GO TO 9950 ! time-only extrapolation

      LFOUND = .FALSE.
      I = 1
      DO WHILE (.NOT. LFOUND .AND. (I .LE. NEXTYP))
         IF (IBHO .EQ. IEXTYP(I)) LFOUND = .TRUE.
         I = I + 1
      END DO
      IF (.NOT. LFOUND) GO TO 9940
      IF (IBHO .GE. 16) GO TO 9940 ! time-only extrapolation

      LFOUND = .FALSE.
      I = 1
      DO WHILE (.NOT. LFOUND .AND. (I .LE. NINTYP))
         IF (IBFI .EQ. IINTYP(I)) LFOUND = .TRUE.
         I = I + 1
      END DO
      IF (.NOT. LFOUND) GO TO 9960

      LFOUND = .FALSE.
      I = 1
      DO WHILE (.NOT. LFOUND .AND. (I .LE. NEXTYP))
         IF (IBFO .EQ. IEXTYP(I)) LFOUND = .TRUE.
         I = I + 1
      END DO
      IF (.NOT. LFOUND) GO TO 9930
      IF (IBFO .GE. 16) GO TO 9930 ! time-only extrapolation

      LFOUND = .FALSE.
      I = 1
      DO WHILE (.NOT. LFOUND .AND. (I .LE. NEXTYP))
         IF (IBFU .EQ. IEXTYP(I)) LFOUND = .TRUE.
         I = I + 1
      END DO
      IF (.NOT. LFOUND) GO TO 9920
      IF (IBFU .GE. 16) go to 9920 ! time-only extrapolation

      NSHVAL = 0
      IF (NSHIFT .LT. 0 .OR. NSHIFT .GT. MXSHF) GO TO 9910
      IF (NSHIFT .GT. 0) THEN
C         IF (UBOUND(NSHFTP) .LT. NSHIFT) GO TO 9900
C         IF (UBOUND(LATMSK) .LT. NSHIFT) GO TO 9890
C         IF (UBOUND(IETIME) .LT. NSHIFT) GO TO 9870
         DO I = 1, NSHIFT
            IF (NSHFTP(I) .LT. 0) GO TO 9860
            IF (NSHFTP(I) .EQ. 0) THEN
               NSHVAL = NSHVAL + 1
            ELSE
               NSHVAL = NSHVAL + 2 * NSHFTP(I)
            END IF
C            IF (LATMSK(I) .AND. UBOUND(IATIME) .LT. I) GO TO 9850
            TMPTIM = JBDATE + IETIME(I)
            IF (TMPTIM .LT. EFFTIM) GO TO 9840
         END DO
C         IF (NSHVAL .GT. UBOUND(SHIFTS)) GO TO 9830

         LFOUND = .FALSE.
         I = 1
         DO WHILE (.NOT. LFOUND .AND. (I .LE. NINTYP))
            IF (ISTI .EQ. IINTYP(I)) LFOUND = .TRUE.
            I = I + 1
         END DO
         IF (.NOT. LFOUND) GO TO 9820

         LFOUND = .FALSE.
         I = 1
         DO WHILE (.NOT. LFOUND .AND. (I .LE. NEXTYP))
            IF (ISTU .EQ. IEXTYP(I)) LFOUND = .TRUE.
            I = I + 1
         END DO
         IF (.NOT. LFOUND) GO TO 9810

         LFOUND = .FALSE.
         I = 1
         DO WHILE (.NOT. LFOUND .AND. (I .LE. NEXTYP))
            IF (ISTO .EQ. IEXTYP(I)) LFOUND = .TRUE.
            I = I + 1
         END DO
         IF (.NOT. LFOUND) GO TO 9800

         DO I = 1, NSHIFT
            LFOUND = .FALSE.
            J = 1
            DO WHILE (.NOT. LFOUND .AND. (J .LE. NINTYP))
               IF (ISBI(I) .EQ. IINTYP(J)) LFOUND = .TRUE.
               J = J + 1
            END DO
            IF (.NOT. LFOUND) GO TO 9790

            LFOUND = .FALSE.
            J = 1
            DO WHILE (.NOT. LFOUND .AND. (J .LE. NEXTYP))
               IF (ISBU(I) .EQ. IEXTYP(J)) THEN
                  LFOUND = .TRUE.
                  IF (ISBU(I) .GE. 16) GO TO 9780 ! time-only extrap
               END IF
               J = J + 1
            END DO
            IF (.NOT. LFOUND) GO TO 9780

            LFOUND = .FALSE.
            J = 1
            DO WHILE (.NOT. LFOUND .AND. (J .LE. NEXTYP))
               IF (ISBO(I) .EQ. IEXTYP(J)) THEN
                  LFOUND = .TRUE.
                  IF (ISBO(I) .GE. 16) GO TO 9770 ! time-only extrap
               END IF
               J = J + 1
            END DO
            IF (.NOT. LFOUND) GO TO 9770

            LFOUND = .FALSE.
            J = 1
            DO WHILE (.NOT. LFOUND .AND. (J .LE. NINTYP))
               IF (ISHI(I) .EQ. IINTYP(J)) LFOUND = .TRUE.
               J = J + 1
            END DO
            IF (.NOT. LFOUND) GO TO 9760

            LFOUND = .FALSE.
            J = 1
            DO WHILE (.NOT. LFOUND .AND. (J .LE. NEXTYP))
               IF (ISHU(I) .EQ. IEXTYP(J)) THEN
                  LFOUND = .TRUE.
                  IF (ISHU(I) .GE. 16) GO TO 9750 ! time-only extrap
               END IF
               J = J + 1
            END DO
            IF (.NOT. LFOUND) GO TO 9750

            LFOUND = .FALSE.
            J = 1
            DO WHILE (.NOT. LFOUND .AND. (J .LE. NEXTYP))
               IF (ISHO(I) .EQ. IEXTYP(J)) THEN
                  LFOUND = .TRUE.
                  IF (ISHO(I) .GE. 16) GO TO 9740 ! time-only extrap
               END IF
               J = J + 1
            END DO
            IF (.NOT. LFOUND) GO TO 9740
         END DO

      END IF

      !------------------------------!
      ! stuff the record user header !
      !------------------------------!
      NUHEAD = 0

      IF (NLOC .GT. 0) THEN
         CALL STUFFUSERHEADER('LOC', CLOC, RUHEAD, KUHEAD, NUHEAD,
     *        ISTAT)
      END IF
      IF (NATIM .GT. 0) THEN
         CALL STUFFUSERHEADER('ATIM', CATIM, RUHEAD, KUHEAD, NUHEAD,
     *        ISTAT)
      END IF

      IF (NHPARM .GT. 0) THEN
         CALL STUFFUSERHEADER('HPARM', CHPARM, RUHEAD, KUHEAD, NUHEAD,
     *        ISTAT)
      END IF

      IF (NHUNIT .GT. 0) THEN
         CALL STUFFUSERHEADER('HUNIT', CHUNIT, RUHEAD, KUHEAD, NUHEAD,
     *        ISTAT)
      END IF

      IF (NFPARM .GT. 0) THEN
         CALL STUFFUSERHEADER('FPARM', CFPARM, RUHEAD, KUHEAD, NUHEAD,
     *        ISTAT)
      END IF

      IF (NFUNIT .GT. 0) THEN
         CALL STUFFUSERHEADER('FUNIT', CFUNIT, RUHEAD, KUHEAD, NUHEAD,
     *        ISTAT)
      END IF

      IF (NCMT .GT. 0) THEN
         CALL STUFFUSERHEADER('COMMENT', CCMT, RUHEAD, KUHEAD, NUHEAD,
     *        ISTAT)
      END IF

      IF (NOPT .GT. 0) THEN
         CALL STUFFUSERHEADER('OPTION', COPT, RUHEAD, KUHEAD, NUHEAD,
     *        ISTAT)
      END IF

      IF (LSNUM) THEN
         CTEMP1 = ''
         WRITE(CTEMP1, *) ISNUM
         CALL STUFFUSERHEADER('SERIAL', CTEMP1, RUHEAD, KUHEAD, NUHEAD,
     *        ISTAT)
      END IF

      IF (LDATUM) THEN
         CTEMP1 = ''
         WRITE(CTEMP1, *) DATUM
         CALL CHRLNB(CTEMP1, NTEMP1)
         IPOINT = INDEX(CTEMP1, '.')
         ILAST = NINDXR(CTEMP1(IPOINT+1:NTEMP1), '0')
         CALL STUFFUSERHEADER('DATUM', CTEMP1(1:IPOINT+ILAST), RUHEAD,
     *        KUHEAD, NUHEAD, ISTAT)
      END IF

      IF (LCOFF) THEN
         CTEMP1 = ''
         WRITE(CTEMP1, *) COFVAL
         CALL CHRLNB(CTEMP1, NTEMP1)
         IPOINT = INDEX(CTEMP1, '.')
         ILAST = NINDXR(CTEMP1(IPOINT+1:NTEMP1), '0')
         CALL STUFFUSERHEADER('OFFSET', CTEMP1(1:IPOINT+ILAST), RUHEAD,
     *        KUHEAD, NUHEAD, ISTAT)
      END IF

      !----------------------------------!
      ! stuff the record internal header !
      !----------------------------------!
      IGBUFF(1) = NBASE
      IGBUFF(2) = NSHIFT
      ICOUNT = 0
      ILAST = -1
      DO I = 1, NSHIFT
         IBIT = I - 1
         IDX = IBIT / 32
         NATMSK = IDX + 1
         IF (IDX .NE. ILAST) THEN
            IATMSK(NATMSK) = 0
            ILAST = IDX
         END IF
         IBIT = IBIT - IDX * 32
         IF (LATMSK(I)) THEN
            ICOUNT = ICOUNT + 1
            IATIMS(ICOUNT) = IATIME(I)
            IATMSK(NATMSK) = IOR(IATMSK(NATMSK), 2 ** IBIT)
         END IF
      END DO
      IGBUFF(3) = ICOUNT
      IF (IHORIZ .EQ. 2) THEN
         IGBUFF(4) = 2
      ELSE
         IGBUFF(4) = 1
      END IF
      IF (LSTAGE) THEN
         IGBUFF(5) = 1
      ELSE
         IGBUFF(5) = 0
      END IF
      IGBUFF(6) = IBHI
      IGBUFF(7) = IBHO
      IGBUFF(8) = IBHU
      IGBUFF(9) = IBFI
      IGBUFF(10) = IBFO
      IGBUFF(11) = IBFU
      IGBUFF(12) = ISTI
      IGBUFF(13) = ISTU
      IGBUFF(14) = ISTO
      IGBUFF(15) = JBDATE
      IF (.NOT. LCOFF .AND. (NOFF .GT. 0)) THEN
         IGBUFF(16) = NOFF
      ELSE
         IGBUFF(16) = 0
      END IF

      !--------------------------------!
      ! arrange the record data values !
      !--------------------------------!
      NVALS = 7 + NBASE * 2 + NATMSK + IGBUFF(3) + NSHIFT * 12 + NSHVAL
      IF (IGBUFF(16) .GT. 0) THEN
         NVALS = NVALS + 10 + IGBUFF(16) * 2
      END IF

      RVALS(1) = HUV
      RVALS(2) = HOV
      RVALS(3) = FUV
      RVALS(4) = FOV
      IF (LSNUM)  IVALS(5) = ISNUM
      IF (LDATUM) RVALS(6) = DATUM
      IF (LCOFF)  RVALS(7) = COFVAL
      IDX = 8

      IF (NVALS .GT. MXVAL) GO TO 9730
      DO I = 1, NBASE
         RVALS(IDX+2*(I-1)+0) = HEIGHT(I)
         RVALS(IDX+2*(I-1)+1) = FLOW(I)
      END DO
      IDX = IDX + NBASE * 2

      DO I = 1, NATMSK
         IVALS(IDX+I-1) = IATMSK(I)
      END DO
      IDX = IDX + NATMSK

      DO I = 1, IGBUFF(3)
         IVALS(IDX+I-1) = IATIMS(I)
      END DO
      IDX = IDX + IGBUFF(3)

      DO I = 1, NSHIFT
         IVALS(IDX+12*(I-1)+0)  = IETIME(I)
         IVALS(IDX+12*(I-1)+1)  = NSHFTP(I)
         IVALS(IDX+12*(I-1)+2)  = ISBI(I)
         IVALS(IDX+12*(I-1)+3)  = ISBU(I)
         RVALS(IDX+12*(I-1)+4)  = SBUV(I)
         IVALS(IDX+12*(I-1)+5)  = ISBO(I)
         RVALS(IDX+12*(I-1)+6)  = SBOV(I)
         IVALS(IDX+12*(I-1)+7)  = ISHI(I)
         IVALS(IDX+12*(I-1)+8)  = ISHU(I)
         RVALS(IDX+12*(I-1)+9)  = SHUV(I)
         IVALS(IDX+12*(I-1)+10) = ISHO(I)
         RVALS(IDX+12*(I-1)+11) = SHOV(I)
      END DO
      IDX = IDX + NSHIFT * 12

      DO I = 1, NSHVAL
         RVALS(IDX+I-1) = SHIFTS(I)
      END DO
      IDX = IDX + NSHVAL

      IF (IGBUFF(16) .GT. 0) THEN
         IVALS(IDX+0) = IOBI
         IVALS(IDX+1) = IOBU
         RVALS(IDX+2) = OBUV
         IVALS(IDX+3) = IOBO
         RVALS(IDX+4) = OBOV
         IVALS(IDX+5) = IOHI
         IVALS(IDX+6) = IOHU
         RVALS(IDX+7) = OHUV
         IVALS(IDX+8) = IOHO
         RVALS(IDX+9) = OHOV
         IDX = IDX + 10
         DO I = 1, IGBUFF(16) * 2
            RVALS(IDX+I-1) = OFFSET(I)
         END DO
      END IF

      !------------------!
      ! write the record !
      !------------------!
      ISTAT = 0
      CALL zwritex(IFLTAB, CPATH, NPATH, IGBUFF, 16, ICHEAD, 0, IUHEAD,
     * NUHEAD, RVALS, NVALS, 560, IPLAN, ISTAT, LFOUND)
      GO TO 9999

      !----------------!
      ! error handlers !
      !----------------!
 9730 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9731) NVALS, MXVAL,
     * CPATH(1:NPATH)

 9731 FORMAT (/, ' -----DSS---zsrst:  ERROR;  The Number of Values',
     * ' Required to Store the Record is Out of Range', /,
     * ' Number Required:', I6, /, ' Maximum:', I6, /,
     * ' Pathname: ', A, /)
      ISTAT = -9
      GO TO 9999

 9740 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9741) I, ISHO(I), CPATH(1:NPATH)
 9741 FORMAT (/, ' -----DSS---zsrst:  ERROR;  The Overflow Handling',
     * ' Method Supplied for the Shift Heights is Invalid.', /,
     * 'Shift: ', I2, /, ' Method: ', I2, /, ' Pathname: ', A, /)
      ISTAT = -27
      GO TO 9999

 9750 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9751) I, ISHU(I), CPATH(1:NPATH)
 9751 FORMAT (/, ' -----DSS---zsrst:  ERROR;  The Underflow Handling',
     * ' Method Supplied for the Shift Heights is Invalid.', /,
     * 'Shift: ', I2, /, ' Method: ', I2, /, ' Pathname: ', A, /)
      ISTAT = -26
      GO TO 9999

 9760 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9761) I, ISHI(I), CPATH(1:NPATH)
 9761 FORMAT (/, ' -----DSS---zsrst:  ERROR;  The Interpolation Method',
     * ' Supplied for the Shift Heights is Invalid.', /,
     * 'Shift: ', I2, /, ' Method: ', I2, /, ' Pathname: ', A, /)
      ISTAT = -25
      GO TO 9999

 9770 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9771) I, ISBO(I), CPATH(1:NPATH)
 9771 FORMAT (/, ' -----DSS---zsrst:  ERROR;  The Overflow Handling',
     * ' Method Supplied for the Shift Breakpoints is Invalid.', /,
     * 'Shift: ', I2, /, ' Method: ', I2, /, ' Pathname: ', A, /)
      ISTAT = -24
      GO TO 9999

 9780 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9781) I, ISBU(I), CPATH(1:NPATH)
 9781 FORMAT (/, ' -----DSS---zsrst:  ERROR;  The Underflow Handling',
     * ' Method Supplied for the Shift Breakpoints is Invalid.', /,
     * 'Shift: ', I2, /, ' Method: ', I2, /, ' Pathname: ', A, /)
      ISTAT = -23
      GO TO 9999

 9790 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9791) I, ISBI(I), CPATH(1:NPATH)
 9791 FORMAT (/, ' -----DSS---zsrst:  ERROR;  The Interpolation Method',
     * ' Supplied for the Shift Breakpoints is Invalid.', /,
     * 'Shift: ', I2, /, ' Method: ', I2, /, ' Pathname: ', A, /)
      ISTAT = -22
      GO TO 9999

 9800 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9801) ISTO, CPATH(1:NPATH)
 9801 FORMAT (/, ' -----DSS---zsrst:  ERROR;  The Overflow Handling',
     * ' Method Supplied for the Shift Effective Times is Invalid.', /,
     * ' Method Supplied: ', I2, /, ' Pathname: ', A, /)
      ISTAT = -21
      GO TO 9999

 9810 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9811) ISTU, CPATH(1:NPATH)
 9811 FORMAT (/, ' -----DSS---zsrst:  ERROR;  The Underflow Handling',
     * ' Method Supplied for the Shift Effective Times is Invalid.',
     *  /, ' Method Supplied: ', I2, /, ' Pathname: ', A, /)
      ISTAT = -20
      GO TO 9999

 9820 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9821) ISTI, CPATH(1:NPATH)
 9821 FORMAT (/, ' -----DSS---zsrst:  ERROR;  The Interpolation Method',
     * ' Supplied for the Shift Effective Times is Invalid.', /,
     * ' Method Supplied: ', I2, /, ' Pathname: ', A, /)
      ISTAT = -19
      GO TO 9999

C 9830 CONTINUE
C      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9831) ICOUNT, UBOUND(SHIFTS),
C     * CPATH(1:NPATH)
C 9831 FORMAT (/, ' -----DSS---zsrst:  ERROR;  The Length of the Array',
C     * ' Containing the Shifts is Less than',
C     * ' Indicated by the Array Containing the Number of Shift ', /,
C     * ' Points per Shift. *** ',
C     * ' Indicated Array Length:', I6, /, ' Array Length:', I6, /,
C     * ' Pathname: ', A, /)
C      ISTAT = -18
C      GO TO 9999

 9840 CONTINUE
      !WRITE(*, *) TMPTIM
      !WRITE(*, *) EFFTIM
      IF (MLEVEL .GT. 1) THEN
         IMIN = MOD(TMPTIM, 1440)
         IHR = IMIN / 60
         IMIN = MOD(IMIN, 60)
         CALL JULDAT(TMPTIM / 1440 + 1, 7, CTEMP1, NTEMP1)
         WRITE (CTEMP1(NTEMP1+1:), '(A,I2.2,A,I2.2)') ', ',IHR, ':',
     *    IMIN
         IMIN = MOD(EFFTIM, 1440)
         IHR = IMIN / 60
         IMIN = MOD(IMIN, 60)
         CALL JULDAT(EFFTIM / 1440 + 1, 7, CTEMP2, NTEMP2)
         WRITE (CTEMP2(NTEMP2+1:), '(A,I2.2,A,I2.2)') ', ',IHR, ':',
     *    IMIN
         WRITE (MUNIT, 9841) I, CTEMP1, CTEMP2, CPATH(1:NPATH)
      END IF
 9841 FORMAT (/, ' -----DSS---zsrst:  ERROR;  Effective Time for Shift',
     * ' preceeds Effective Time for Base Curve.',
     * ' Shift Number:', I6, /,
     * ' Shift Effective Time:', A, /,
     * ' Base Curve Effective Time:', A, /,
     * ' Pathname: ', A, /)
      ISTAT = -17
      GO TO 9999

C 9850 CONTINUE
C      IF (MLEVEL .GT. 1) THEN
C         IMAX = I
C         DO I = I, NSHIFT
C            IF (LATMSK(I)) IMAX = I
C         END DO
C         WRITE (MUNIT, 9851) IMAX, UBOUND(IATIME), CPATH(1:NPATH)
C      END IF
C 9851 FORMAT (/, ' -----DSS---zsrst:  ERROR;  The Length of the Array',
C     * ' Containing the Activated Times is less than required by',
C     * ' the Activated Time Mask Array.', /,
C     * ' Required by Mask Array:', I6, /, ' Array Length:', I6, /,
C     * ' Pathname: ', A, /)
C      ISTAT = -16
C      GO TO 9999

 9860 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9861) I, NSHFTP(I),
     * CPATH(1:NPATH)
 9861 FORMAT (/, ' -----DSS---zsrst:  ERROR;  Number of Shift Points',
     * ' Indicated is Less than One.', /, ' Shift Number:', I6, /,
     * ' Number of Points Indicated:', I6, /, ' Pathname: ', A, /)
      ISTAT = -15
      GO TO 9999

C 9870 CONTINUE
C      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9871) NSHIFT, UBOUND(IETIME),
C     * CPATH(1:NPATH)
C 9871 FORMAT (/, ' -----DSS---zsrst:  ERROR;  The Length of the Array',
C     * ' Containing the Effective Times is less than',
C     * ' the Number of Shifts Indicated.', /,
C     * ' Number of Shifts Indicated:', I6, /, ' Array Length:', I6, /,
C     * ' Pathname: ', A, /)
C      ISTAT = -13
C      GO TO 9999

 9880 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9881) CPATH(1:NPATH)
 9881 FORMAT (/, ' -----DSS---zsrst:  ERROR;  Cannot Determine the',
     * ' Effective Time of the Rating from the Pathname.', /,
     * ' Pathname: ', A, /)
      ISTAT = -12
      GO TO 9999

C 9890 CONTINUE
C      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9891) NSHIFT, UBOUND(NSHFTP),
C     * CPATH(1:NPATH)
C 9891 FORMAT (/, ' -----DSS---zsrst:  ERROR;  The Length of the Array',
C     * ' Containing the Activated Time Flags is less than',
C     * ' the Number of Shifts Indicated.', /,
C     * ' Number of Shifts Indicated:', I6, /, ' Array Length:', I6, /,
C     * ' Pathname: ', A, /)
C      ISTAT = -11
C      GO TO 9999

C 9900 CONTINUE
C      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9901) NSHIFT, UBOUND(NSHFTP),
C     * CPATH(1:NPATH)
C 9901 FORMAT (/, ' -----DSS---zsrst:  ERROR;  The Length of the Array',
C     * ' Containing the Number of Shift Points per Shift is less than',
C     * ' the Number of Shifts Indicated.', /,
C     * ' Number of Shifts Indicated:', I6, /, ' Array Length:', I6, /,
C     * ' Pathname: ', A, /)
C      ISTAT = -10
C      GO TO 9999

 9910 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9911) NSHIFT, MXSHF,
     * CPATH(1:NPATH)
 9911 FORMAT (/, ' -----DSS---zsrst:  ERROR;  The Number of Shifts',
     * ' Supplied is Out of Range', /,
     * ' Number Supplied:', I6, /, ' Maximum:', I6, /,
     * ' Pathname: ', A, /)
      ISTAT = -9
      GO TO 9999

 9920 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9921) IBFO, CPATH(1:NPATH)
 9921 FORMAT (/, ' -----DSS---zsrst:  ERROR;  The Overflow Handling',
     * ' Method Supplied for the Base Curve Flows is Invalid.', /,
     * ' Method Supplied: ', I2, /, ' Pathname: ', A, /)
      ISTAT = -8
      GO TO 9999

 9930 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9931) IBFU, CPATH(1:NPATH)
 9931 FORMAT (/, ' -----DSS---zsrst:  ERROR;  The Underflow Handling',
     * ' Method Supplied for the Base Curve Flows is Invalid.', /,
     * ' Method Supplied: ', I2, /, ' Pathname: ', A, /)
      ISTAT = -7
      GO TO 9999

 9940 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9941) IBHO, CPATH(1:NPATH)
 9941 FORMAT (/, ' -----DSS---zsrst:  ERROR;  The Overflow Handling',
     * ' Method Supplied for the Base Curve Heights is Invalid.', /,
     * ' Method Supplied: ', I2, /, ' Pathname: ', A, /)
      ISTAT = -6
      GO TO 9999

 9950 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9951) IBHU, CPATH(1:NPATH)
 9951 FORMAT (/, ' -----DSS---zsrst:  ERROR;  The Underflow Handling',
     * ' Method Supplied for the Base Curve Heights is Invalid.', /,
     * ' Method Supplied: ', I2, /, ' Pathname: ', A, /)
      ISTAT = -5
      GO TO 9999

 9960 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9961) IBHI, IBFI, CPATH(1:NPATH)
 9961 FORMAT (/, ' -----DSS---zsrst:  ERROR;  The Interpolation Method',
     * ' Supplied for the Base Curve is Invalid.', /,
     * ' Method Supplied: ', I2, ', ',I2 /, ' Pathname: ', A, /)
      ISTAT = -4
      GO TO 9999

C9970 CONTINUE
C     IF (MLEVEL .GT. 1) WRITE (MUNIT, 9971) NBASE, UBOUND(FLOWS),
C    * CPATH(1:NPATH)
C9971 FORMAT (/, ' -----DSS---zsrst:  ERROR;  The Number of Flows',
C    * ' Supplied for the Base Curve is Less than the Coordinate',
C    * ' Count.', / 'Number Supplied:', I6, /, ' Coordinate Count:', I6,
C    *  /, ' Pathname: ', A, /)
C     ISTAT = -3
C     GO TO 9999

C9980 CONTINUE
C     IF (MLEVEL .GT. 1) WRITE (MUNIT, 9981) NBASE, UBOUND(HEIGHTS),
C    * CPATH(1:NPATH)
C9981 FORMAT (/, ' -----DSS---zsrst:  ERROR;  The Number of Heights',
C    * ' Supplied for the Base Curve is Less than the Coordinate',
C    * ' Count.', /, ' Number Supplied:', I6, /, ' Coordinate Count:',
C    * I6, /, ' Pathname: ', A, /)
C     ISTAT = -2
C     GO TO 9999

 9990 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9991) NBASE, CPATH(1:NPATH)
 9991 FORMAT (/, ' -----DSS---zsrst:  ERROR;  The Number of',//
     * ' Coordinates Supplied for the Base Curve is Less than One',  /,
     * ' Number Supplied:', I6, /, ' Pathname: ', A, /)
      ISTAT = -1
      GO TO 9999

 9998 FORMAT (T5, '----- Exit zsrst, Number data values stored:', I5,
     * ', Number shifts stored:', I4, ', Status:', I4, /)

      !-------------------!
      ! normal exit point !
      !-------------------!
 9999 CONTINUE
      !--------------------!
      ! debug exit message !
      !--------------------!
      IF (MLEVEL .GT. 7) WRITE (MUNIT, 9998) NBASE, NSHIFT, ISTAT
      RETURN
      END

C=======================================================================
      SUBROUTINE STUFFUSERHEADER(LABEL, ITEM, RUHEAD, KUHEAD, NUHEAD,
     * ISTAT)

      CHARACTER LABEL*(*), ITEM*(*), LOCALLABEL*60, LOCALITEM*512,
     * CCHUNK*60
      REAL*4 RUHEAD(*)
      INTEGER*4 KUHEAD, NUHEAD, ISTAT

      CALL CHRLNB(ITEM, NITEM)
      LOCALITEM = ITEM(1:NITEM)
      DO I = 1, NITEM
         IF (LOCALITEM(I:I) .EQ. ':') LOCALITEM(I:I) = '~'
         IF (LOCALITEM(I:I) .EQ. ';') LOCALITEM(I:I) = '`'
      END DO

      NCHUNKS = (NITEM - 1) / 60 + 1
      IF (NCHUNKS .EQ. 1) THEN
         CALL zstfh(LABEL, LOCALITEM, 1, RUHEAD, KUHEAD, NUHEAD, ISTAT)
      ELSE
         DO I = 1, NCHUNKS
            CCHUNK = LOCALITEM((I-1)*60+1:I*60)
            IF (I .EQ. 1) THEN
               LOCALLABEL = LABEL
            ELSE
               WRITE(LOCALLABEL(1:), *)LABEL, I
            END IF
            CALL zstfh(LOCALLABEL, CCHUNK, 1, RUHEAD, KUHEAD, NUHEAD,
     *       ISTAT)
         END DO
      END IF
      RETURN
      END


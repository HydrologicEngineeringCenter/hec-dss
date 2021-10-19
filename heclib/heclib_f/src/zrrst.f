      SUBROUTINE zrrst(
       !------------------!
       ! file/record info !
       !------------------!
     * IFLTAB, ! file table
     * CPATH,  ! pathname
       !--------------!
       ! general info !
       !--------------!
     * IETIM,  ! effective time (minutes since 31Dec1899 00:00)
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
     * KBASE,  ! dimenson of height & flow arrays (must be at least
               ! nbase)
     * NBASE,  ! number of points in base curve
     * HEIGHT, ! stages or elevations
     * FLOW,   ! flows
       !------------!
       ! shift info !
       !------------!
     * KSHIFT, ! dimension of arrays related to shifts (must be at
               ! least nshift)
     * NSHIFT, ! number of constant or variable shifts
     * ISTI,   ! interpolation type for shift effective times
     * ISTU,   ! underflow handling type for shift effective times
     * ISTO,   ! overflow handling type for shift effective times
     * ISBI,   ! interpolation type for shift breakpoints for each shift
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
     * KSHVAL, ! dimension of shifts array (must be at least nshval)
     * NSHVAL, ! total number of number of shift point values
     * SHIFTS, ! shift points (for each shift, constant shift or
               ! break(1), height(1),...break(n),height(n))
       !-------------!
       ! offset info !
       !-------------!
     * LCOFF,  ! flag specifying whether a constant offset is used
     * COFVAL, ! constant offset value
     * KOFF,   ! dimension of offset array (must be at least noff * 2)
     * NOFF,   ! number of variable offset breakpoint, height pairs
     * IOBI,   ! interpolation type for variable offset breakpoints
     * IOBU,   ! underflow handling type for variable offset breakpoints
     * OBUV,   ! offset breakpoint underflow value (may or may not be
               ! used)
     * IOBO,   ! overflow handling type for variable offset breakpoints
     * OBOV,   ! offset breakpoint oferflow value (may or may not be
               ! used)
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
     * ISTAT   ! success/failure status
     * )
c
c     Retrieve STREAM RATING
c
c     Mike Perryman
c     Nov, 2003
c
      PARAMETER (KUHEAD=400)
      PARAMETER (KCHEAD=1)

      INTEGER*4 IFLTAB(*), IETIM, ISNUM, IBHI, IBHO, IBHU, IBFI, IBFO,
     * IBFU, NBASE, NSHIFT, ISTI, ISTU, ISTO, ISBI(*), ISBU(*), ISBO(*),
     * ISHI(*), ISHU(*), ISHO(*), JBDATE, IETIME(*), IATIME(*),
     * NSHFTP(*), NOFF, IOBI, IOBU, IOBO, IOHI, IOHU, IOHO, IHORIZ,
     * ISTAT, NPATH, NVALS, JTYPE, NGBFU, NCHEAD, ICHEAD(KCHEAD),
     * NUHEAD, IUHEAD(KUHEAD), NATIME, NA, NB, NC, ND, NE, NF

      CHARACTER CPATH*(*), CLOC*(*), CATIM*(*), CHPARM*(*), CHUNIT*(*),
     * CFPARM*(*), CFUNIT*(*), CCMT*(*), COPT*(*), CA*80, CB*80, CC*80,
     * CD*80, CE*80, CF*80, CTEMP1*80

      LOGICAL*4 LSNUM, LDATUM, LSTAGE, LATMSK(*), LCOFF, LFOUND

      REAL*4 DATUM, HUV, HOV, FUV, FOV, HEIGHT(*), FLOW(*), SBUV(*),
     * SBOV(*), SHUV(*), SHOV(*), SHIFTS(*), COFVAL, OBUV, OBOV, OHUV,
     * OHOV, OFFSET(*), RCHEAD(KCHEAD), RUHEAD(KUHEAD)

      EQUIVALENCE (IUHEAD, RUHEAD)
      EQUIVALENCE (ICHEAD, RCHEAD)

      INCLUDE 'zdsskz.h'
      INCLUDE 'zdssiz.h'
      INCLUDE 'zdssmz.h'
      INCLUDE 'zdssbf.h'
      INCLUDE 'zdssrst.h'

      CALL CHRLNB(CPATH, NPATH)

      !---------------------!
      ! debug entry message !
      !---------------------!
      IF (MLEVEL .GE. 7) THEN
         WRITE(MUNIT, 10) KBASE, KSHIFT, KOFF, CPATH(1:NPATH)
      END IF
   10 FORMAT (T5, '----- Enter zrrst -----', /,
     * T11, 'KBASE:', I6, ', KSHIFT:', I4, ', KOFF:', I4, /,
     * T11, 'Pathname: ', A)
      !-------------------------------------------------------------!
      ! determine whether the record exists and is of expected type !
      !-------------------------------------------------------------!
      CALL zdtype(IFLTAB, CPATH, NVALS, LFOUND, CTEMP1, JTYPE)
      IF (.NOT. LFOUND) GO TO 9990
      IF (JTYPE .NE. 560) GO TO 9980
      IF (NVALS .GT. MXVAL) GO TO 9970
      !--------------------------------------------------------!
      ! get the effective time from the d part of the pathname !
      !--------------------------------------------------------!
      CALL zufpn(CA, NA, CB, NB, CC, NC, CD, ND, CE, NE, CF, NF,
     * CPATH, NPATH, ISTAT)
      IF (ISTAT .NE. 0 .OR. ND .LT. 7) GO TO 9960
      CALL RATTIM(CD, ND, IETIM, ISTAT)
      IF (ISTAT .NE. 0) GO TO 9960
      !-----------------!
      ! read the record !
      !-----------------!
      CALL zreadx(IFLTAB, CPATH(1:NPATH), IGBUFF, NGBUFF, NGBFU,
     * RCHEAD, KCHEAD, NCHEAD, RUHEAD, KUHEAD, NUHEAD, RVALS, MXVAL,
     * NVALS, 0, LFOUND)
      IF (.NOT. LFOUND) GO TO 9990
      !-------------------------------------!
      ! un-stuff the record internal header !
      !-------------------------------------!
      IF (NGBFU .LT. 16) GO TO 9950
      NBASE  = IGBUFF(1)
      NSHIFT = IGBUFF(2)
      NATIME = IGBUFF(3)
      IHORIZ = IGBUFF(4)
      LSTAGE = IGBUFF(5) .EQ. 1
      IBHI   = IGBUFF(6)
      IBHO   = IGBUFF(7)
      IBHU   = IGBUFF(8)
      IBFI   = IGBUFF(9)
      IBFO   = IGBUFF(10)
      IBFU   = IGBUFF(11)
      ISTI   = IGBUFF(12)
      ISTU   = IGBUFF(13)
      ISTO   = IGBUFF(14)
      JBDATE = IGBUFF(15)
      NOFF   = IGBUFF(16)
      IF (NBASE  .GT. KBASE ) GO TO 9940
      IF (NSHIFT .GT. KSHIFT) GO TO 9930
      IF (NOFF   .GT. KOFF * 2) GO TO 9920
      !---------------------------------!
      ! un-stuff the record user header !
      !---------------------------------!
      CALL UNSTUFFUSERHEADER('LOC', CLOC, IPOS, RUHEAD, NUHEAD,
     * ISTAT)
      CALL UNSTUFFUSERHEADER('ATIM', CATIM, IPOS, RUHEAD, NUHEAD,
     * ISTAT)
      CALL UNSTUFFUSERHEADER('HPARM', CHPARM, IPOS, RUHEAD, NUHEAD,
     * ISTAT)
      CALL UNSTUFFUSERHEADER('HUNIT', CHUNIT, IPOS, RUHEAD, NUHEAD,
     * ISTAT)
      CALL UNSTUFFUSERHEADER('FPARM', CFPARM, IPOS, RUHEAD, NUHEAD,
     * ISTAT)
      CALL UNSTUFFUSERHEADER('FUNIT', CFUNIT, IPOS, RUHEAD, NUHEAD,
     * ISTAT)
      CALL UNSTUFFUSERHEADER('COMMENT', CCMT, IPOS, RUHEAD, NUHEAD,
     * ISTAT)
      CALL UNSTUFFUSERHEADER('OPTION', COPT, IPOS, RUHEAD, NUHEAD,
     * ISTAT)
      CALL UNSTUFFUSERHEADER('SERIAL', CTEMP1, IPOS, RUHEAD, NUHEAD,
     * ISTAT)
      CALL CHRLNB(CTEMP1, NTEMP1)
      IF (NTEMP1 .GT. 0) THEN
         LSNUM = .TRUE.
      ELSE
         LSNUM = .FALSE.
      END IF
      CALL UNSTUFFUSERHEADER('DATUM', CTEMP1, IPOS, RUHEAD, NUHEAD,
     * ISTAT)
      CALL CHRLNB(CTEMP1, NTEMP1)
      IF (NTEMP1 .GT. 0) THEN
         LDATUM = .TRUE.
      ELSE
         LDATUM = .FALSE.
      END IF
      CALL UNSTUFFUSERHEADER('OFFSET', CTEMP1, IPOS, RUHEAD, NUHEAD,
     * ISTAT)
      CALL CHRLNB(CTEMP1, NTEMP1)
      IF (NTEMP1 .GT. 0) THEN
         LCOFF = .TRUE.
      ELSE
         LCOFF = .FALSE.
      END IF
      !---------------------------------!
      ! un-stuff the record data values !
      !---------------------------------!
      HUV    = RVALS(1)
      HOV    = RVALS(2)
      FUV    = RVALS(3)
      FOV    = RVALS(4)
      ISNUM  = IVALS(5)
      DATUM  = RVALS(6)
      COFVAL = RVALS(7)
      IDX = 8

      DO I = 1, NBASE
         HEIGHT(I) = RVALS(IDX+2*(I-1)+0)
         FLOW(I)   = RVALS(IDX+2*(I-1)+1)
      END DO
      IDX = IDX + NBASE * 2

      NATMSK = (NSHIFT - 1) / 32 + 1
      DO I = 1, NATMSK
         IATMSK(I) = IVALS(IDX+I-1)
      END DO
      IDX = IDX + NATMSK

      IOFFSET = 0
      DO I = 1, NSHIFT
         IBIT = I - 1
         IWORD = IBIT / 32
         IBIT = IBIT - (IWORD * 32)
         IWORD = IWORD + 1
         IF (IAND(IATMSK(IWORD), 2 ** IBIT) .NE. 0) THEN
            LATMSK(I) = .TRUE.
            IATIME(I) = IVALS(IDX + IOFFSET)
            IOFFSET = IOFFSET + 1
         ELSE
            LATMSK(I) = .FALSE.
         END IF
      END DO
      IF (IOFFSET .NE. NATIME) GO TO 9880
      IDX = IDX + NATIME

      NSHVAL = 0
      DO I = 1, NSHIFT
         IETIME(I) = IVALS(IDX+12*(I-1)+0)
         NSHFTP(I) = IVALS(IDX+12*(I-1)+1)
         ISBI(I)   = IVALS(IDX+12*(I-1)+2)
         ISBU(I)   = IVALS(IDX+12*(I-1)+3)
         SBUV(I)   = RVALS(IDX+12*(I-1)+4)
         ISBO(I)   = IVALS(IDX+12*(I-1)+5)
         SBOV(I)   = RVALS(IDX+12*(I-1)+6)
         ISHI(I)   = IVALS(IDX+12*(I-1)+7)
         ISHU(I)   = IVALS(IDX+12*(I-1)+8)
         SHUV(I)   = RVALS(IDX+12*(I-1)+9)
         ISHO(I)   = IVALS(IDX+12*(I-1)+10)
         SHOV(I)   = RVALS(IDX+12*(I-1)+11)
         IF (NSHFTP(I) .EQ. 0) THEN
            NSHVAL = NSHVAL + 1
         ELSE
            NSHVAL = NSHVAL + 2 * NSHFTP(I)
         END IF
      END DO
      IDX = IDX + NSHIFT * 12
      IF (KSHVAL .LT. NSHVAL) GO TO 9870

      DO I = 1, NSHVAL
         SHIFTS(I) = RVALS(IDX+I-1)
      END DO
      IDX = IDX + NSHVAL

      IF (NOFF .GT. 0) THEN
         IOBI = IVALS(IDX+0)
         IOBU = IVALS(IDX+1)
         OBUV = RVALS(IDX+2)
         IOBO = IVALS(IDX+3)
         OBOV = RVALS(IDX+4)
         IOHI = IVALS(IDX+5)
         IOHU = IVALS(IDX+6)
         OHUV = RVALS(IDX+7)
         IOHO = IVALS(IDX+8)
         OHOV = RVALS(IDX+9)
         IDX = IDX + 10
         DO I = 1, NOFF * 2
             OFFSET(I) = RVALS(IDX+I-1)
         END DO
      END IF

      ISTAT = 0
      GO TO 9999
      !----------------!
      ! error handlers !
      !----------------!
 9870 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9871) NSHVAL, KSHVAL,
     * CPATH(1:NPATH)
 9871 FORMAT (/, ' -----DSS---zrrst:  ERROR;  Record has more shift',
     * ' values than dimensioned with KSHVAL.',/,
     * T11, ' Number of shift values: ', I4, /,
     * T11, ' KSHVAL:                 ', I4, /,
     * T11, ' Pathname: ', a, /)
      ISTAT = -11
      GO TO 9999

 9880 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9881) NATIME, IOFFSET,
     * CPATH(1:NPATH)
 9881 FORMAT (/, ' -----DSS---zrrst:  ERROR;  Number of shift',
     * ' activated times in record does not agree with the',/
     * ' number of shift activated times specified in the header.', /
     * T11, ' Number specified in header: ', I4, /,
     * T11, ' Number in record:           ', I4, /,
     * T11, ' Pathname: ', a, /)
      ISTAT = -10
      GO TO 9999

 9890 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9891) CTEMP1(1:NTEMP1),
     * CPATH(1:NPATH)
 9891 FORMAT (/, ' -----DSS---zrrst:  ERROR;  Cannot read real',
     * ' value for constant offset from user header item.' ,/,
     * T11, ' User header label: OFFSET', /,
     * T11, ' User header item:  ', A, /,
     * T11, ' Pathname: ', A, /)
      ISTAT = -9
      GO TO 9999

 9900 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9901) CTEMP1(1:NTEMP1),
     * CPATH(1:NPATH)
 9901 FORMAT (/, ' -----DSS---zrrst:  ERROR;  Cannot read real',
     * ' value for datum elevation from user header item.' ,/,
     * T11, ' User header label: DATUM', /,
     * T11, ' User header item:  ', A, /,
     * T11, ' Pathname: ', A, /)
      ISTAT = -9
      GO TO 9999

 9910 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9911) CTEMP1(1:NTEMP1),
     * CPATH(1:NPATH)
 9911 FORMAT (/, ' -----DSS---zrrst:  ERROR;  Cannot read integer',
     * ' value for serial number from user header item.' ,/,
     * T11, ' User header label: SERIAL', /,
     * T11, ' User header item:  ', A, /,
     * T11, ' Pathname: ', A, /)
      ISTAT = -9
      GO TO 9999

 9920 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9921) NOFF * 2, KOFF,
     * CPATH(1:NPATH)
 9921 FORMAT (/, ' -----DSS---zrrst:  ERROR;  Record has more values',
     * ' for variable offsets than dimensioned with KOFF.',/,
     * T11, ' Number of values: ', I4, /,
     * T11, ' KOFF:             ', I4, /,
     * T11, ' Pathname: ', A, /)
      ISTAT = -8
      GO TO 9999

 9930 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9931) NSHIFT, KSHIFT,
     * CPATH(1:NPATH)
 9931 FORMAT (/, ' -----DSS---zrrst:  ERROR;  Record has more shifts',
     * ' than dimensioned with KSHIFT.',/,
     * T11, ' Number of shifts: ', I4, /,
     * T11, ' KSHIFT:           ', I4, /,
     * T11, ' Pathname: ', A, /)
      ISTAT = -7
      GO TO 9999

 9940 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9941) NBASE, KBASE,
     * CPATH(1:NPATH)
 9941 FORMAT (/, ' -----DSS---zrrst:  ERROR;  Record has more values',
     * ' in base curve than dimensioned with KBASE.',/,
     * T11, ' Number of values: ', I4, /,
     * T11, ' KBASE:            ', I4, /,
     * T11, ' Pathname: ', A, /)
      ISTAT = -6
      GO TO 9999

 9950 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9951) NGBFU, 16, CPATH(1:NPATH)
 9951 FORMAT (/, ' -----DSS---zrrst:  ERROR;  Record has fewer values',
     * ' in then interal header than expected.',/,
     * T11, ' Number of values: ', I3, /,
     * T11, ' Expected:         ', I3, /,
     * T11, ' Pathname: ', A, /)
      ISTAT = -5
      GO TO 9999

 9960 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9961) CPATH(1:NPATH)
 9961 FORMAT (/, ' -----DSS---zrrst:  ERROR;  Cannot determine',
     * ' effective time of rating from pathname.',/,
     * T11, ' Pathname: ', A, /)
      ISTAT = -4
      GO TO 9999

 9970 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9971) NVALS, MXVAL,
     * CPATH(1:NPATH)
 9971 FORMAT (/, ' -----DSS---zrrst:  ERROR;  Record has more values',
     * ' than current limit.',/,
     * T11, ' Number of values: ', I6, /,
     * T11, ' Current limit:    ', I6, /,
     * T11, ' Pathname: ', A, /)
      ISTAT = -3
      GO TO 9999

 9980 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9981) JTYPE, 560, CPATH(1:NPATH),
     * CPATH(1:NPATH)
 9981 FORMAT (/, ' -----DSS---zrrst:  ERROR;  The Record type does',
     * ' not match the expected record type.', /,
     * T11, ' Expected type:', I4, /,
     * T11, ' Record type:  ', I4, /,
     * T11, ' Pathname: ', A, /)
      ISTAT = -2
      GO TO 9999

 9990 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9991) CPATH(1:NPATH)
 9991 FORMAT (/, ' -----DSS---zrrst:  ERROR;  Record does not exist.', /
     * T11, ' Pathname: ', A, /)
      ISTAT = 1
      GO TO 9999

 9998 FORMAT (t5, '----- Exit zrrst, Number data values retrieved:', I5,
     * ', Number shifts retrieved:', I4, ', Status:', I4, /)

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

      SUBROUTINE UNSTUFFUSERHEADER(LABEL, ITEM, IPOS, RUHEAD, NUHEAD,
     * ISTAT)

      CHARACTER LABEL*(*), ITEM*(*), LOCALLABEL*60, CCHUNK*60
      REAL*4 RUHEAD(*)
      LOGICAL LEXITLOOP
      INTEGER*4 NUHEAD, ISTAT

      CALL zustfh(LABEL, CCHUNK, 1, IPOS, RUHEAD, NUHEAD, ISTAT)
      ITEM = CCHUNK(1:60)
      I = 2
      LEXITLOOP = .FALSE.
      DO WHILE (.NOT. LEXITLOOP)
         WRITE(LOCALLABEL(1:), *)LABEL, I
         CALL zustfh(LOCALLABEL, CCHUNK,1, IPOS, RUHEAD, NUHEAD, ISTAT)
         CALL CHRLNB(CCHUNK, NCHUNK)
         IF (NCHUNK .GE. 1) THEN
            ITEM((I-1)*60+1:I*60) = CCHUNK(1:60)
            I = I + 1
         ELSE
            LEXITLOOP = .TRUE.
         END IF
      END DO

      CALL CHRLNB(ITEM, NITEM)
      DO I = 1, NITEM
         IF (ITEM(I:I) .EQ. '~') ITEM(I:I) = ':'
         IF (ITEM(I:I) .EQ. '`') ITEM(I:I) = ';'
      END DO
      RETURN
      END


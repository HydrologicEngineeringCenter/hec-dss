      SUBROUTINE ZSRSTI(
       !------------------!
       ! file/record info !
       !------------------!
     * IFLTAB, ! file table
     * CPATH,  ! pathname
       !---------------------------!
       ! stream rating series info !
       !---------------------------!
     * IINTRP, ! interpolation behavior
     * IUFLOW, ! underflow behavoir
     * IOFLOW, ! overflow behavior
       !-----------!
       ! misc info !
       !-----------!
     * IPLAN,  ! OVERWRITE PLAN
     * ISTAT   ! success/failure status
     *)
c
c     Store STREAM RATING SERIES INFOMRATION
c
c     Mike Perryman
c     Nov, 2003
c
      CHARACTER CPATH*(*)
      CHARACTER*64  CA, CB, CC, CD, CE, CF
      CHARACTER*391 CPATH1
      INTEGER*4 IFLTAB(*), IINTRP, IUFLOW, IOFLOW, NPATH, NA, NB, NC,
     * ND, NE, NF
      LOGICAL   LFOUND
      REAL*4 DUMMY

      INCLUDE 'zdsskz.h'
      INCLUDE 'zdssiz.h'
      INCLUDE 'zdssmz.h'
      INCLUDE 'zdssbf.h'

      CALL CHRLNB(CPATH, NPATH)

      !---------------------!
      ! debug entry message !
      !---------------------!
      IF (MLEVEL .GE. 7) THEN
         WRITE(MUNIT, 10) IINTRP, IUFLOW, IOFLOW, CPATH(1:NPATH)
      END IF
   10 FORMAT (T5, '----- Enter ZSRSTI -----', /,
     * T11, 'IINTRP:', I2, ', IUFLOW:', I2, ', IOFLOW:', I2, /,
     * T11, 'Pathname: ', A)

      !----------------------------!
      ! check for error conditions !
      !----------------------------!
      LFOUND = .FALSE.
      I = 1
      DO WHILE (.NOT. LFOUND .AND. (I .LE. NINTYP))
         IF (IINTRP .EQ. IINTYP(I)) LFOUND = .TRUE.
         I = I + 1
      END DO
      IF (.NOT. LFOUND) GO TO 9990

      LFOUND = .FALSE.
      I = 1
      DO WHILE (.NOT. LFOUND .AND. (I .LE. NEXTYP))
         IF (IUFLOW .EQ. IEXTYP(I)) LFOUND = .TRUE.
         I = I + 1
      END DO
      IF (.NOT. LFOUND) GO TO 9980

      LFOUND = .FALSE.
      I = 1
      DO WHILE (.NOT. LFOUND .AND. (I .LE. NEXTYP))
         IF (IOFLOW .EQ. IEXTYP(I)) LFOUND = .TRUE.
         I = I + 1
      END DO
      IF (.NOT. LFOUND) GO TO 9970

      !----------------------------------!
      ! stuff the record internal header !
      !----------------------------------!
      IGBUFF(1) = IINTRP
      IGBUFF(2) = IUFLOW
      IGBUFF(3) = IOFLOW

      !-------------------------------------!
      ! transform the pathname if necessary !
      !-------------------------------------!
      CALL ZUFPN(CA, NA, CB, NB, CC, NC, CD, ND, CE, NE, CF, NF,
     * CPATH, NPATH, ISTAT)
      IF (ISTAT .NE. 0 .OR. ND .LT. 7) GO TO 9960
      CALL ZPATH(CA, CB, CC, 'SERIESINFO', CE, CF, CPATH1, LEN(CPATH1))
      CALL CHRLNB(CPATH1, NPATH)

      !------------------!
      ! write the record !
      !------------------!
      ISTAT = 0
      DUMMY = 0
      CALL ZWRITX(IFLTAB, CPATH1, NPATH, IGBUFF, 3, ICHEAD, 0, IUHEAD,
     * 0, DUMMY, 1, 561, IPLAN, ISTAT, LFOUND)
      GO TO 9999

      !----------------!
      ! error handlers !
      !----------------!
 9960 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9961) CPATH(1:NPATH)
 9961 FORMAT (/, ' -----DSS---ZSRST:  ERROR;  The Pathname',
     * ' Supplied is Invalid.', /,
     * ' Pathname: ', A, /)
      ISTAT = -4
      GO TO 9999

 9970 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9971) IOFLOW, CPATH(1:NPATH)
 9971 FORMAT (/, ' -----DSS---ZSRST:  ERROR;  The Overflow Handling',
     * ' Method Supplied is Invalid.', /,
     * ' Method Supplied: ', I2, /, ' Pathname: ', A, /)
      ISTAT = -3
      GO TO 9999

 9980 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9981) IUFLOW, CPATH(1:NPATH)
 9981 FORMAT (/, ' -----DSS---ZSRST:  ERROR;  The Underflow Handling',
     * ' Method Supplied is Invalid.',
     *  /, ' Method Supplied: ', I2, /, ' Pathname: ', A, /)
      ISTAT = -2
      GO TO 9999

 9990 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9991) IINTRP, CPATH(1:NPATH)
 9991 FORMAT (/, ' -----DSS---ZSRST:  ERROR;  The Interpolation Method',
     * ' Supplied is Ivalid.', /,
     * ' Method Supplied: ', I2, /, ' Pathname: ', A, /)
      ISTAT = -1
      GO TO 9999
 9998 FORMAT (T5, '----- Exit ZSRSTI, Status:', i4, /)

      !-------------------!
      ! normal exit point !
      !-------------------!
 9999 CONTINUE
      !--------------------!
      ! debug exit message !
      !--------------------!
      IF (MLEVEL .GT. 7) WRITE (MUNIT, 9998) ISTAT
      RETURN
      END


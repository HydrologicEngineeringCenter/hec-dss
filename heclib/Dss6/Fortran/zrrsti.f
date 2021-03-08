      SUBROUTINE zrrsti(
       !------------------!
       ! file/record info !
       !------------------!
     * IFLTAB, ! (in)     file table
     * CPATH,  ! (in)     pathname
     * IFCAT,  ! (in)     force a new catalog? 0=no, 1=if needed, >1=yes
       !---------------------------!
       ! stream rating series info !
       !---------------------------!
     * IINTRP, ! (out)    interpolation behavior
     * IUFLOW, ! (out)    underflow behavoir
     * IOFLOW, ! (out)    overflow behavior
     * LFOUND, ! (out)    series info record found?
      !---------------------------------------!
      ! effective times of individual ratings !
      !---------------------------------------!
     * KTIMES, ! (in)     dimension of itimes
     * NTIMES, ! (out)    number of itimes set
     * ITIMES, ! (in/out) array of effective times
       !-----------!
       ! misc info !
       !-----------!
     * ISTAT   ! (out)    success/failure status
     *)
c
c     Retrieve STREAM RATING SERIES INFOMRATION
c
c     Mike Perryman
c     Nov, 2003
c
      CHARACTER CPATH*(*), CPATH1*391, CFNAME*256
      CHARACTER CCATPN*391, CCATTAG*8, CTYPE*20
      CHARACTER*64  CA, CB, CC, CD, CE, CF, CA1, CB1, CC1, CD1, CE1, CF1
      INTEGER*4 IFLTAB(*), IFCAT, IINTRP, IUFLOW, IOFLOW, KTIMES,
     * NTIMES, ITIMES(*), ISTAT, NPATH, NA, NB, NC, ND, NE, NF, NFRECS,
     * NCRECS, NPATHS, NFOUND, NA1, NB1, NC1, ND1, NE1, NF1, ITIME
      LOGICAL*4   LFOUND, LCATO, LCATV, LCCATO, LCCATV, LFOUND1

      INCLUDE 'zdsskz.h'
      INCLUDE 'zdssiz.h'
      INCLUDE 'zdssmz.h'
      INCLUDE 'zdssbf.h'

      CALL CHRLNB(CPATH, NPATH)

      !---------------------!
      ! debug entry message !
      !---------------------!
      IF (MLEVEL .GE. 7) THEN
         WRITE(MUNIT, 10) CPATH(1:NPATH)
      END IF
   10 FORMAT (T5, '----- Enter zrrsti -----', /,
     * T11, 'Pathname: ', A)

      !-------------------------------------!
      ! transform the pathname if necessary !
      !-------------------------------------!
      CALL zufpn(CA, NA, CB, NB, CC, NC, CD, ND, CE, NE, CF, NF,
     * CPATH, NPATH, ISTAT)
      IF (ISTAT .NE. 0) GO TO 9990
      CALL zpath(CA, CB, CC, 'SERIESINFO', CE, CF, CPATH1, LEN(CPATH1))
      CALL CHRLNB(CPATH1, NPATHS)
      CALL UPCASE(CPATH1)
      CALL zufpn(CA, NA, CB, NB, CC, NC, CD, ND, CE, NE, CF, NF,
     * CPATH1, NPATHS, ISTAT)

      !-------------------------------------------------------------!
      ! determine whether the record exists and is of expected type !
      !-------------------------------------------------------------!
      CALL zdtype(IFLTAB, CPATH1, NVALS, LFOUND, CTYPE, ITYPE)
      IF (LFOUND) THEN
            IF (ITYPE .NE. 561) GO TO 9980
            !-----------------!
            ! read the record !
            !-----------------!
            CALL zreadx(IFLTAB, CPATH1, IGBUFF, NGBUFF, NGBFU, RCHEAD,
     *         0, NCHEAD, RUHEAD, 0, NUHEAD, RVALS, 0, NVALS, 0, LFOUND)
            IINTRP = IGBUFF(1)
            IUFLOW = IGBUFF(2)
            IOFLOW = IGBUFF(3)
      ELSE
            !-----------------------!
            ! set to default values !
            !-----------------------!
            IINTRP =  1 ! LIN_INTERP
            IUFLOW = 11 ! NOT_ALLOWED
            IOFLOW = 13 ! USE_NEAREST
      END IF

      !----------------------------------------------!
      ! open the catalog file, creating if specified !
      !----------------------------------------------!
      NTIMES = 0
      CALL zinqir(IFLTAB, 'NAME', CFNAME, ND)
      IF (IFCAT .GE. 1) THEN
            CALL zopnca(CFNAME, 12, .TRUE., LCATO, LCATV, 13, .FALSE.,
     *               LCCATO, LCCATV, NCRECS)
            IF (IFCAT .EQ. 1) THEN
               CALL zinqir(IFLTAB, 'NREC', CD, NFRECS)
               IF (NFRECS .NE. NCRECS) THEN
                  CALL zcat(IFLTAB, 12, 0, 0, ' ', .FALSE., .FALSE.,
     *                   LCCATO, NCRECS)
                  CALL zopnca(CFNAME, 12, .FALSE., LCATO, LCATV, 13,
     *                     .FALSE., LCCATO, LCCATV, NCRECS)
               END IF
            END IF
      ELSE
            CALL zopnca(CFNAME, 12, .FALSE., LCATO, LCATV, 13, .FALSE.,
     *               LCCATO, LCCATV, NCRECS)
      END IF
      IF (.NOT. (LCATO .AND. LCATV)) GO TO 9970

      !--------------------------------------------------!
      ! read the catalog, finding all matching pathnames !
      !--------------------------------------------------!
      CALL CHRLNB(CPATH1, NPATHS)
      NFOUND = 1
      DO WHILE (NFOUND .EQ. 1)
            CALL zrdcat(12, .TRUE., 0, CCATTAG, 0, CCATPN, NPATHS,
     *                  NFOUND)
            IF (NFOUND .GT. 0) THEN
               CALL CHRLNB(CCATPN, NPATHS)
               CALL zufpn(CA1, NA1, CB1, NB1, CC1, NC1, CD1, ND1, CE1,
     *             NE1, CF1, NF1, CCATPN, NPATHS, ISTAT)
               IF (ISTAT .EQ. 0) THEN
                  IF ( ((CA1(1:NA1) .EQ. CA(1:NA)) .OR.
     *                 (NA .EQ. 0 .AND. NA1 .EQ. 0)) .AND.
     *                 ((CB1(1:NB1) .EQ. CB(1:NB)) .OR.
     *                 (NB .EQ. 0 .AND. NB1 .EQ. 0)) .AND.
     *                 ((CC1(1:NC1) .EQ. CC(1:NC)) .OR.
     *                 (NC .EQ. 0 .AND. NC1 .EQ. 0)) .AND.
     *                 ((CE1(1:NE1) .EQ. CE(1:NE)) .OR.
     *                 (NE .EQ. 0 .AND. NE1 .EQ. 0)) .AND.
     *                 ((CF1(1:NF1) .EQ. CF(1:NF)) .OR.
     *                 (NF .EQ. 0 .AND. NF1 .EQ. 0)) ) THEN
                     ITYPE = 0
                     CALL zdtype(IFLTAB, CCATPN, NVALS, LFOUND1,
     *                     CTYPE, ITYPE)
                     IF (ITYPE .EQ. 560) THEN
                        CALL RATTIM(CD1, ND1, ITIME, ISTAT)
                        IF (ISTAT .EQ. 0) THEN
                           NTIMES = NTIMES + 1
                           IF (NTIMES .LE. KTIMES) THEN
                              ITIMES(NTIMES) = ITIME
                           END IF
                        END IF
                     END IF
                  END IF
               END IF
            END IF
      END DO

      IF (NTIMES .EQ. KTIMES) GO TO 9960
      ISTAT = 0
      GO TO 9999
      !----------------!
      ! error handlers !
      !----------------!
 9960 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9961) NTIMES, KTIMES,
     * CPATH(1:NPATH)
 9961 FORMAT (/, ' -----DSS---zrrsti:  ERROR;  Catalog has more',
     * ' matching ratings than dimensioned with KTIMES.',/,
     * T11, ' Number of records: ', I4, /,
     * T11, ' KTIMES:            ', I4, /)
      ISTAT = -4
      GO TO 9999

 9970 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9971) CPATH1,
     * CPATH(1:NPATH)
 9971 FORMAT (/, ' -----DSS---zrrsti:  ERROR;  The Catalog file',
     * ' could not be opened.', /,
     * T11, ' Pathname: ', A, /)
      ISTAT = -3
      GO TO 9999

 9980 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9981) ITYPE, 561, CPATH1,
     * CPATH(1:NPATH)
 9981 FORMAT (/, ' -----DSS---zrrsti:  ERROR;  The Record type does',
     * ' not match the expected record type.', /,
     * T11, ' Expected type:', I4, /,
     * T11, ' Record type:  ', I4, /,
     * T11, ' Pathname: ', A, /)
      ISTAT = -2
      GO TO 9999

 9990 CONTINUE
      IF (MLEVEL .GT. 1) WRITE (MUNIT, 9991) CPATH(1:NPATH)
 9991 FORMAT (/, ' -----DSS---zrrsti:  ERROR;  The Pathname',
     * ' Supplied is Invalid.', /,
     * T11, 'Pathname: ', A, /)
      ISTAT = -1
      GO TO 9999

 9998 FORMAT (T5, '----- Exit zsrsti, Status:', I4, /)

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


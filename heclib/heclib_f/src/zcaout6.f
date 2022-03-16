      SUBROUTINE zcaout6 (IFLTAB, ICUNIT, ICDUNT, LCDCAT, NORECS)
C

C     Write out the sorted file to the catalog file, and the condensed
C     catalog file, if set.
C
C     Written by Troy Nicolini at HEC, 1989.
C
      COMMON /UNDEF_TIME/ ITIME_UNDEF
      INTEGER ITIME_UNDEF
C
      include 'dss_parameters.h'
      CHARACTER(len=dss_maxpart) CPART(6)
      CHARACTER CDASH*64, CFORMT*10
      CHARACTER(len=dss_maxpath) CLINE, CLINEL, CPATH
      CHARACTER CSTR*400, CTAG*8
      INTEGER  IBPART(6), IEPART(6), ILPART(6), ILPRTL(6)
      LOGICAL LWRITE, LCDCAT, LBLANK(6), LMISS, LCURMIS
      INTEGER IFLTAB(*)
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdssca.h'
C
      INCLUDE 'zdssmz.h'
C
      INCLUDE 'zdsscm.h'
C
C
      DATA CDASH /'- - - - - - - - - - - - - - - - '/
C
C
      IF (MLEVEL.GE.11) WRITE (MUNIT,20)
 20   FORMAT (T6, '-----DSS---Debug:  Enter zcaout6')
C
C     Initializations.
      DO 40 I=1,6
      ILPRTL(I) = 1
      CPART(I) = ' '
 40   CONTINUE
      LWRITE = .FALSE.
      LMISS = .FALSE.
      LCURMIS = .FALSE.
      JULS  = ITIME_UNDEF
      JULSL = ITIME_UNDEF
      JULEL = ITIME_UNDEF
      CSTR = ' '
C
      MTOTAL = MTOTAL + 1
      WRITE (CFORMT, 60) MTOTAL
 60   FORMAT ('(T',I3.3,',I7)')
      REWIND (ISUNIT(2))
C
C     Read each record from the sorted file
      DO 300 ILOOP=1,NORECS
C
C     Read the record number of the sorted file
      READ (ISUNIT(2), CFORMT, END=800, ERR=300) INUMB
C     Read the catalog line corresponging to that number
      READ (ISUNIT(3), REC=INUMB, ERR=300) CSTR(1:400)
C
C     Read the sorted catalog
C
      IF (.NOT.LEXTND) THEN
C
C     Short version of catalog
      CALL CHRLNB (CSTR, NSTR)
      IF (ILOOP.GE.1000000) THEN
      WRITE (ICUNIT,101,ERR=900) ILOOP, CSTR(1:NSTR)
 101  FORMAT (I7,1X,A)
      ELSE
      WRITE (ICUNIT,100,ERR=900) ILOOP, CSTR(1:NSTR)
 100  FORMAT (I6,2X,A)
      ENDIF
      IF (LMAP) WRITE (MAPUNT,120,ERR=900) CSTR(11:NSTR)
 120  FORMAT (A)
C
      ELSE
C
C     Extended Version of Catalog
      CALL CHRLNB (CSTR, NSTR)
      IF (ILOOP.GE.1000000) THEN
      WRITE (ICUNIT,101,ERR=900) ILOOP, CSTR(1:NSTR)
      ELSE
      WRITE (ICUNIT,100,ERR=900) ILOOP, CSTR(1:NSTR)
      ENDIF
      IF (LMAP) WRITE (MAPUNT,120,ERR=900) CSTR(49:NSTR)
      ENDIF
C
C
C     Is a compressed catalog being produced?
      IF (.NOT.LCDCAT) GO TO 300
C
      DO 200 N=1,6
      LBLANK(N) = .TRUE.
 200  CONTINUE
C
      CLINE = ' '
      CTAG = CSTR (1:8)
C
      IF (.NOT.LEXTND) THEN
      CPATH = CSTR (11:)
      ELSE
      CPATH = CSTR (49:)
      ENDIF
C
C     Insert the tag at the start of the line.
      IEND = MAXPRT(7)
      CLINE(1:IEND) = CTAG
C
      CALL zupath (CPATH, IBPART, IEPART, ILPART, ISTAT)
C
C
C     Search the pathname to see if there is a null part
C     If a null part is found, rebuild the pathname, replacing
C     null parts with the part "(null)"
      DO 210 N=1,6
         IF (ILPART(N).EQ.0) THEN
            CSTR = '/ '
            IPOS = 2
            DO 205 J=1,6
               IF (ILPART(J).GT.0) THEN
                  CSTR(IPOS:) = CPATH(IBPART(J):IEPART(J)) // '/'
                  IPOS = IPOS + ILPART(J) + 1
               ELSE
                  CSTR(IPOS:) = '(null)/'
                  IPOS = IPOS + 7
               ENDIF
 205        CONTINUE
            CPATH = CSTR(1:392)
            CALL zupath (CPATH, IBPART, IEPART, ILPART, ISTAT)
            GO TO 215
         ENDIF
 210  CONTINUE
 215  CONTINUE
C
C
      DO 220 N=1,5
      MPART = IORDER(N)
C
      IBEGIN = IEND + 3
      IEND   = IBEGIN + MAXPRT(MPART) - 1
C
C     Test for equality of parts.
      IF  ((CPATH(IBPART(MPART):IEPART(MPART)).EQ.
     * CPART(MPART)(1:ILPRTL(MPART))).AND.(LBLANK(MPART))) THEN
      CLINE(IBEGIN:IEND-1) = CDASH
C
C     Otherwise write part to CLINE and update the lagging variables.
      ELSE
      CLINE(IBEGIN:IEND) = CPATH(IBPART(MPART):IEPART(MPART))
      ILPRTL(MPART) = ILPART(MPART)
      CPART(MPART) = CPATH(IBPART(MPART):IEPART(MPART))
      J = IORDER(N+1)
      LBLANK(J) = .FALSE.
      ENDIF
C
 220  CONTINUE
C
C
C     Set up to test whether the date part of the current line is
C     in sequence with date part of the lagging line. This is only
C     necessary if parts ABCFE  of the current line are the same as
C     parts ABCFE  of the lagging line.  (I.e. LBLANK  is true.)
C
      IF (LBLANK(4)) THEN
C
      IF ((ILPART(4).LT.7).OR.(ILPART(5).LT.4)) THEN
      JULS = ITIME_UNDEF
      GO TO 240
      ENDIF
C
C     Get the julian date of this pathname
      CALL DATJUL (CPATH(IBPART(4):IEPART(4)),JULS,IERR)
      IF (IERR.NE.0) GO TO 240
      JULEL = JULSL
      IST = 1
C
C     Get the julian date for the next block
      CALL zgintl6 (INTL, CPATH(IBPART(5):IEPART(5)), NVALS, IST)
      IF (IST.EQ.0) THEN
C     Regular interval
      CALL zbegdt6(JULEL, INTL,IYR, IMON, IDAY, IBLOCK, IFLTAB(KVERNO))
      ELSE IF (IST.EQ.1) THEN
C     Irregular interval
      CALL zirbeg6 (IFLTAB, JULEL, CPATH(IBPART(5):IEPART(5)), IYR,
     * IMON, IDAY, IBLOCK, IDUM1, IDUM2)
      ELSE
C     Not time series
      JULEL = ITIME_UNDEF
      GO TO 240
      ENDIF
C     Increment the date to the next block
      CALL zincbk6 (IBLOCK, JULEL, IYR, IMON, IDAY)
      ENDIF
C
C     End of date set up block.
C
 240  CONTINUE
      IBEGIN = IEND + 3
      IEND  = IBEGIN + MAXPRT(4) - 1
C
C
C     Test for equality of previous parts and whether dates are in
C     sequence.  Hyphenate current date onto lagging line.
C     Update lagging variable JULSL.
C
      IF (LBLANK(4).AND.(JULS.EQ.JULEL).AND.(JULEL.NE.ITIME_UNDEF))THEN
      CLINEL(IENDL + 1:IENDL + 3) = ' - '
      CLINEL(IENDL+ 4:IENDL+ 4+ MAXPRT(4)) = CPATH(IBPART(4):IEPART(4))
      CALL DATJUL (CPATH(IBPART(4):IEPART(4)),JULSL,IERR)
C
      ELSE IF (LBLANK(4) .AND. (JULS.NE.JULEL) .AND. (LCCDAT) .AND.
     * (JULEL.NE.ITIME_UNDEF)) THEN
      CLINEL(IENDL + 1:IENDL + 3) = ' - '
      CLINEL(IENDL+ 4:IENDL+ 6+ MAXPRT(4)) = CPATH(IBPART(4):IEPART(4))
      CALL DATJUL (CPATH(IBPART(4):IEPART(4)),JULSL,IERR)
      LCURMIS = .TRUE.
      LMISS = .TRUE.
C
C     Otherwise, write the current date to CLINE and
C     update the lagging variables.
      ELSE
      CLINE(IBEGIN:IEND) = CPATH(IBPART(4):IEPART(4))
      CALL DATJUL (CPATH(IBPART(4):IEPART(4)),JULSL,IERR)
      IF (IERR.NE.0) JULSL = ITIME_UNDEF
      ILPRTL(4) = ILPART(4)
      CPART(4) = CPATH(IBPART(4):IEPART(4))
      LWRITE = .TRUE.
      ENDIF
C
C
      IF (LWRITE) THEN
      IF (ILOOP.GT.1) THEN
      CALL CHRLNB (CLINEL, N)
      IF (LCURMIS) THEN
      CLINEL(N+1:N+2) = ' *'
      N = N + 2
      LCURMIS = .FALSE.
      ENDIF
      WRITE (ICDUNT,260,ERR=900) CLINEL(1:N)
 260  FORMAT (1X,A)
      ENDIF
      CLINEL = CLINE
      IENDL = IBEGIN + ILPART(4) - 1
      LWRITE = .FALSE.
      ENDIF
C
C
 300  CONTINUE
C
      IF (LCDCAT) THEN
      IF (LWRITE) THEN
      CALL CHRLNB (CLINE, N)
      WRITE (ICDUNT,260,ERR=900) CLINE(1:N)
      ELSE
      CALL CHRLNB (CLINEL, N)
      WRITE (ICDUNT,260,ERR=900) CLINEL(1:N)
      ENDIF
      IF (LMISS) WRITE (ICDUNT, 310, ERR=900)
 310  FORMAT ('   *  Record time span has missing periods.')
      ENDIF
C
C
 800  CONTINUE
      LCCDAT = .FALSE.
      !ENDFILE ICDUNT
C
      IF (MLEVEL.GE.11) WRITE (MUNIT,820)
 820  FORMAT (T6,'-----DSS---Debug:  EXIT zcaout6')
      RETURN
C
C
C     Error during write.
 900  CONTINUE
      WRITE (MUNIT, 901)
 901  FORMAT (/' **** ERROR - zcat6:   Error during write to',
     * ' the catalog file ****',/,' Unable to complete catalog ')
      GO TO 800
C
C
      END


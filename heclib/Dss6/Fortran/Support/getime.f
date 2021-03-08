      SUBROUTINE GETIME (CIN, IBLINE, LENLIN, JULS, ISTIME,
     * JULE, IETIME, ISTAT)
C
C     GETS STARTING AND ENDING DATES AND TIMES FROM A TIME
C     COMMAND LINE
C
C     ISTAT =  0      NO ERROR
C     ISTAT = -1      ERROR OCCURED
C
      COMMON /UNDEF_TIME/ ITIME_UNDEF
      INTEGER ITIME_UNDEF
C
      CHARACTER CIN*(*), CLINE*80
      CHARACTER CMON(12)*3
      CHARACTER*16 CVAR, CTMP
      PARAMETER (MAXF=10)
      INTEGER   IBF(10), IEF(10), ILF(10)
      INTEGER*4 JULE, JULS, JUL                                         ML
      INTEGER*4 INTL(4)                                                 ML
C     INTEGER INTL(4)                                                   Hu
      LOGICAL   LSDATE, LCUR, LSTIME, LEDATE, LSTART, LENDFL
C
      DATA CMON/'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN',
     *          'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'/
      DATA INTL /525600, 42300, 1440, 60/
C
C
C
      ISTAT = 0
C
      IF (LENLIN.EQ.0) GO TO 900
      CLINE = CIN(IBLINE:IBLINE+LENLIN-1)
      CALL UPCASE (CLINE)
C
      IF (ISTIME.EQ.-1) ISTIME = 1
      IF (IETIME.EQ.-1) IETIME = 1440
C
C     SEPERATE LINE INTO FIELDS
      CALL PARSLI ( CLINE, MAXF, NFIELD, IBF, IEF, ILF)
      IF (NFIELD.EQ.0) GO TO 900
C
C     INITIALIZE LOCAL VARIABLES
 20   CONTINUE
      LSDATE = .FALSE.
      LSTIME = .FALSE.
      LEDATE = .FALSE.
      LCUR   = .FALSE.
      LENDFL = .FALSE.
C
C
      DO 100 N=1,NFIELD
C
C     MOVE THIS FIELD INTO IVAR
      IF (ILF(N).EQ.0) GO TO 100
      IF (ILF(N).GT.16) GO TO 900
      CVAR = CLINE(IBF(N):IEF(N))
C
      IF ((N.EQ.1).OR.((N.EQ.2).AND.(NFIELD.GT.2))) THEN
      LSTART = .TRUE.
      ELSE
      LSTART = .FALSE.
      ENDIF
C
C     Is this a reference to the current time, or another time (e.g. E)
      IF (INDEX('DTBSE', CVAR(1:1)).GT.0) THEN
C     LOOK FOR A CURRENT DATE AND TIME FLAG (I.E. "T")
      IF (CVAR(1:1) .EQ. 'T') THEN
      LCUR = .TRUE.
C     STARTING DATE AND TIME, OR ENDING?
      IF (N.EQ.1) THEN
      CALL CURTIM ( JULS, ISTIME)
      LSDATE = .TRUE.
      LSTIME = .TRUE.
      ELSE
      CALL CURTIM ( JULE, IETIME)
      LEDATE = .TRUE.
      ENDIF
C
C     ELSE CHECK DATE FLAG ("D")
      ELSE IF (CVAR(1:1) .EQ. 'D') THEN
      LCUR = .TRUE.
C     BEGINNING DATE, OR ENDING DATE?
      IF (.NOT.LSDATE) THEN
      CALL CURTIM ( JULS, IDUM)
      LSDATE = .TRUE.
      ELSE
      CALL CURTIM ( JULE, IDUM)
      LEDATE = .TRUE.
      ENDIF
C
C     Is this an ending date with a reference to the starting date
C     (e.g., "S+5H")
      ELSE IF ((CVAR(1:1).EQ.'B').OR.(CVAR(1:1).EQ.'S')) THEN
      JULE = JULS
      IETIME = ISTIME
      LEDATE = .TRUE.
      LSTART = .FALSE.
C
C     Is this a starting date with a reference to the ending date
C     (e.g., "E-1M")
      ELSE IF (CVAR(1:1).EQ.'E') THEN
C     This reference must always be in the first field
      IF (N.GT.1) GO TO 900
C     Since we may not know the ending date, compute the
C     ending date first, then come back ad compute this
C     (fool the code by setting the numb of fields to 1
C     after computing the ending date)
      IF (NFIELD.EQ.1) THEN
      IF (JULE.LE.0) GO TO 900
      JULS = JULE
      ISTIME = IETIME
      LSDATE = .TRUE.
      LSTIME = .TRUE.
      ELSE
      LENDFL = .TRUE.
      GO TO 100
      ENDIF
      ENDIF
C
C     Remove "T" (or D, S, etc.)
      CTMP = CVAR(2:)
      CVAR = CTMP
      CALL CHRLNB (CVAR, ILAST)
      IF (ILAST.EQ.0) GO TO 100
      ENDIF
C
C     SCAN FOR A DATE VALUE (IE. A MONTH)
      DO 40 I=1,12
      J = INDEX (CVAR, CMON(I))
      IF (J.GT.0) GO TO 60
 40   CONTINUE
      J = INDEX (CVAR, '/')
C
 60   CONTINUE
      IF (J.NE.0) THEN
C     IT IS A DATE
      CALL DATJUL (CVAR,JUL,IERROR)
      IF (IERROR.EQ.-1) GO TO 900
C     BEGINNING DATE OR ENDING DATE?
      IF (.NOT.LSDATE) THEN
      JULS = JUL
      LSDATE = .TRUE.
      ELSE IF (.NOT.LEDATE) THEN
      JULE = JUL
      LEDATE = .TRUE.
      ELSE
      GO TO 900
      ENDIF
      GO TO 100
      ENDIF
C
C     NO DATE FOUND
C     EITHER A TIME, OR A TIME OFFSET (EG. T-36H)
C     SCAN FOR A TIME VALUE
      J = NSCAN ( CVAR, 1, 5, '1234567890: ', 1, 12)
      IF (J.NE.0) GO TO 80
C
C     A TIME VALUE (EG. 0900)
      ITIME = IHM2M (CVAR)
      IF (ITIME.LT.0) GO TO 900
C     BEGINNING OR ENDING TIME?
      IF (.NOT.LSTIME) THEN
      ISTIME = ITIME
      LSTIME = .TRUE.
      ELSE
      IF ((N.EQ.2).AND.(LCUR)) THEN
      ISTIME = ITIME
      LSTIME = .TRUE.
      ELSE
      IETIME = ITIME
      ENDIF
      ENDIF
C
      GO TO 100
C
C
C     MUST BE A TIME OFFSET (EG. -36H)
 80   CONTINUE
      CALL CHRLNB (CVAR, NVAR)
      J = INDEX ('YMDH', CVAR(NVAR:NVAR))
      IF (J.EQ.0) GO TO 900
      NPER = INTGR (CVAR,1,NVAR-1,IERR)
      IF (IERR.NE.0) GO TO 900
C
      IF (LSTART) THEN
      J = INCTIM (INTL(J), 0, NPER, JULS, ISTIME, JULS, ISTIME)
      ELSE
      J = INCTIM (INTL(J), 0, NPER, JULE, IETIME, JULE, IETIME)
      ENDIF
C
 100  CONTINUE
C
      IF (LENDFL) THEN
      NFIELD = 1
      GO TO 20
      ENDIF
C
C
 800  CONTINUE
      RETURN
C
C
 900  CONTINUE
      ISTAT = -1
      JULS = ITIME_UNDEF
      JULE = ITIME_UNDEF
      ISTIME = -1
      IETIME = -1
      RETURN
C
      END


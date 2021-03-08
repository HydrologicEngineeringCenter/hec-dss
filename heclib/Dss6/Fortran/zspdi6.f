      SUBROUTINE zspdi6 (IFLTAB, CPATH, NORD, NCURVE, IHORIZ,
     * C1UNIT, C1TYPE, C2UNIT, C2TYPE, SVALUES, DVALUES,
     * LDOUBLE, CLABEL, LABEL, IUHEAD, NUHEAD, IPLAN, ISTAT)
C
C
C     Internal Store Paired DATA
C
C     Written by Bill Charley
C
C
C     IPLAN:  10 - Alloocated space for a series of curves,
C     where each of the curves is written individually at separate times.
C     The SVALUES or DVALUES array must contain (only) ordinates.
C     NORD, NCURVE, IHORIZ, C1UNIT, C1TYPE, C2UNIT, C2TYPE, LDOUBLE,
C     LABEL, IUHEAD, NUHEAD all must be set at this time.
C     The amount of space to allocate is based on NORD and NCURVE.
C     No curve data is set for this call.
C
C     IPLAN 11 - Write the coordiantes (not ordinates) for an individual
C     cruve that has been allocated with plan 10 above.
C     NCURVE must be the curve number (starting with 1).
C     The SVALUES or DVALUES array must contain (only) coordiantes for
C     that one curve.  If LABEL, then CLABEL must contain the label
C     for that curve.  You must store the same size (float) as allocated.
C     NORD, IHORIZ, C1UNIT, C1TYPE, C2UNIT, C2TYPE, LDOUBLE,
C     IUHEAD, NUHEAD are ignored.
C
C
      INTEGER IFLTAB(*), IUHEAD(*)
      CHARACTER CPATH*(*), CLABEL(*)*(*)
      CHARACTER C1UNIT*(*), C1TYPE*(*), C2UNIT*(*), C2TYPE*(*)
      CHARACTER CTEMP*100
      REAL SVALUES(*)
      DOUBLE PRECISION DVALUES(*)
      LOGICAL LABEL, LFOUND, LDOUBLE
	INTEGER MAXLABEL
C
      INCLUDE 'zdssmz.h'
C
      INCLUDE 'zdssbf.h'
C
      INCLUDE 'zdssts.h'
C
      INCLUDE 'zdsscc.h'
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdsslz.h'
C
C
      CALL CHRLNB (CPATH, NPATH)
C
C     If a debug level is on, print out information
      IF (MLEVEL.GE.7) THEN
      WRITE (MUNIT,20) NORD, NCURVE, IHORIZ, LABEL, IPLAN, NUHEAD,
     * CPATH(1:NPATH)
 20   FORMAT (T5,'----- Enter zspd6 -----',/,
     * T11,'NORD:',I6,',  NCURVE:',I4,',  IHORIZ:',I4,/,
     * T11,'LABEL: ',L1,',  IPLAN:',I3,',  NUHEAD:',I5,/,
     * T11,'Pathname: ',A)
      WRITE (MUNIT,21) C1UNIT, C1TYPE, C2UNIT, C2TYPE
 21   FORMAT(T5,'X Units: ',A,'  X Type: ',A,/,
     *       T5,'Y Units: ',A,'  Y Type: ',A)
      IF (LABEL) WRITE (MUNIT, 22) CLABEL(1)
 22   FORMAT (5X,'First label: ',A)
      ENDIF
C
C
C     Check that IFLTAB is valid (e.g., the DSS file is open)
      IF (IFLTAB(1).NE.6) CALL zerror6(IFLTAB, 5, 'ZSPD  ',
     * 0, IFLTAB, ' ', 0, ' ',0)
C
C
      IF (IPLAN.EQ.11) THEN
C        Store only a single coordinate array (one curve) and label
         LWRITE = .TRUE.
         CALL ZMULTU6 ( IFLTAB, .TRUE., .TRUE.)
         CALL ZRDINF6 (IFLTAB, CPATH, NH, ND, ISTAT)
         IF (ISTAT.NE.0) THEN
            CALL ZMULTU6 ( IFLTAB, .FALSE., .TRUE.)
            LWRITE = .FALSE.
            GO TO 900
         ENDIF

         IF (LDOUBLE) THEN
            N = NORD  *2
            IADD = INFO(NPPWRD+KIADAT) + (NCURVE * NORD * 2)
            IF (IFLTAB(KDSWAP).NE.0) CALL ZDSWAP6(DVALUES, N)
            CALL ZPTREC6(IFLTAB, DVALUES, N, IADD, .FALSE.)
            IF (IFLTAB(KDSWAP).NE.0) CALL ZDSWAP6(DVALUES, N)
         ELSE
            IADD = INFO(NPPWRD+KIADAT) + (NCURVE * NORD)
            CALL ZPTREC6(IFLTAB, SVALUES, NORD, IADD, .FALSE.)
         ENDIF
         NVALS = NORD

C       Now store the label
        IF (LABEL) THEN
            CTEMP = CLABEL(1)
            IADD = INFO(NPPWRD+KIAIHE)
            NIHEAD = INFO(NPPWRD+KINIHE)
            CALL zgtrec6(IFLTAB, IGBUFF, NIHEAD, IADD, .FALSE.)
            CALL CHRHOL (CTEMP, 1, 12, IGBUFF((NCURVE*3)+9), 1)

C           Do we have extended labels?
            NTOTALCURV = IGBUFF(2)
            NH = (NTOTALCURV*3) + 11
	      IF (NIHEAD.GT.NH) THEN
	         NH = NH + 1
C              NWDS is the number of words for each extended label
	         NWDS = IGBUFF(NH)
	         N = NH + NWDS
	         IF ((NIHEAD.GE.N).AND.(NWDS.LT.26)) THEN
	             NH = NH + 1
                   NH = NH + (NCURVE - 1) * NWDS
	             CALL CH2HOL (CTEMP, IGBUFF(NH), NWDS)
	          ENDIF
             ENDIF
          CALL ZPTREC6(IFLTAB, IGBUFF, NIHEAD, IADD, .FALSE.)
        ENDIF

         CALL ZMULTU6( IFLTAB, .FALSE., .TRUE.)
         LWRITE = .FALSE.
         GO TO 800
      ENDIF
C
C
C     Error checking
      IF (NORD.LT.1) GO TO 905
      IF (NCURVE.LT.1) GO TO 910
      IF (NCURVE.GT.100) GO TO 920
C
      IGBUFF(1) = NORD
      IGBUFF(2) = NCURVE
      IGBUFF(3) = 1
      IF (IHORIZ.EQ.2) IGBUFF(3) = 2
C
      CTEMP = C1UNIT
      CALL CHRHOL (CTEMP, 1, 8, IGBUFF(4), 1)
      CTEMP = C1TYPE
      CALL CHRHOL (CTEMP, 1, 8, IGBUFF(6), 1)
      CTEMP = C2UNIT
      CALL CHRHOL (CTEMP, 1, 8, IGBUFF(8), 1)
      CTEMP = C2TYPE
      CALL CHRHOL (CTEMP, 1, 8, IGBUFF(10), 1)
C
      NVALS = (NORD * (NCURVE + 1))
      NIHEAD = 11
C
C
      IF (IPLAN.EQ.10) THEN
C       Use space from the time series buffer array and store zeros
        IF (LDOUBLE) THEN
            JTYPE = 205
             NTOT = NVALS * 2
         ELSE
             JTYPE = 200
             NTOT = NVALS
         ENDIF

         IF (MLEVEL.GE.7) THEN
            WRITE (MUNIT,70) NVALS, NTOT, KLBUFF
 70         FORMAT('    Number values to store:',I7,
     *      ' Size to store:',I7,' Size Limit:',I7)
         ENDIF
         IF (NTOT.GT.KLBUFF) GO TO 930

C        Store a zeroed array
         DO 90 I=1,NTOT
            BUFF(I) = 0.0
 90      CONTINUE

         IF (LABEL) THEN
C           Store blank labels
            CTEMP = ' '
            NIHEAD = (NCURVE*3) + 11
            DO 91 I=1,NCURVE
              CALL CHRHOL (CTEMP, 1, 12, IGBUFF((I*3)+9), 1)
 91        CONTINUE
C
C       Do we have extended labels?  If so, store their size in words
C       and the full labels following the standard labels (for
C       compatibility)
          CALL CHRLNB(CLABEL(1), MAXLABEL)
          IF (MAXLABEL.GT.12) THEN
	      NWDS = ((MAXLABEL - 1) / 4) + 1
	      NIHEAD = NIHEAD + 1
	      IGBUFF(NIHEAD) = NWDS
            IF ((NIHEAD + (NCURVE * NWDS)).GT.NGBUFF) GO TO 900
	      DO 92 I=1,NCURVE
              NIHEAD = NIHEAD + 1
	        CALL CH2HOL (CTEMP, IGBUFF(NIHEAD), NWDS)
	        NIHEAD = NIHEAD + NWDS - 1
 92         CONTINUE
	    ENDIF
        ENDIF

         CALL zwritex6(IFLTAB, CPATH, NPATH, IGBUFF, NIHEAD,
     *   ICHEAD, 0, IUHEAD, NUHEAD, BUFF, NTOT, JTYPE,
     *   0, ISTAT, LFOUND)
         IF (ISTAT.NE.0) GO TO 900

C        Now store the ordinates
         LWRITE = .TRUE.
         CALL ZMULTU6( IFLTAB, .TRUE., .TRUE.)
         CALL ZRDINF6(IFLTAB, CPATH, NH, ND, ISTAT)
         IF (ISTAT.NE.0) THEN
            CALL ZMULTU6( IFLTAB, .FALSE., .TRUE.)
            LWRITE = .FALSE.
            GO TO 900
         ENDIF

         IF (LDOUBLE) THEN
            N = NORD  *2
            IF (IFLTAB(KDSWAP).NE.0) CALL ZDSWAP6(DVALUES, N)
            CALL ZPTREC6(IFLTAB,DVALUES, N, INFO(NPPWRD+KIADAT),.FALSE.)
            IF (IFLTAB(KDSWAP).NE.0) CALL ZDSWAP6(DVALUES, N)
         ELSE
            CALL ZPTREC6(IFLTAB, SVALUES, NORD, INFO(NPPWRD+KIADAT),
     *                   .FALSE.)
         ENDIF

         CALL ZMULTU6( IFLTAB, .FALSE., .TRUE.)
         LWRITE = .FALSE.
         GO TO 800

      ENDIF
C
C     Normal write
C
      CTEMP = ' '
	MAXLABEL = 0
      IF (LABEL) THEN
        NIHEAD = (NCURVE*3) + 11
        DO 100 I=1,NCURVE
          CTEMP = CLABEL(I)
	    CALL CHRLNB(CLABEL(I), NLAB)
	    IF (NLAB.GT.MAXLABEL) MAXLABEL = NLAB
          CALL CHRHOL (CTEMP, 1, 12, IGBUFF((I*3)+9), 1)
 100    CONTINUE
      ENDIF

C
C
C     Do we have extended labels?  If so, store their size in words
C     and the full labels following the standard labels (for
C     compatibility)
C
      IF (MAXLABEL.GT.12) THEN
	   NWDS = ((MAXLABEL - 1) / 4) + 1
	   NIHEAD = NIHEAD + 1
	   IGBUFF(NIHEAD) = NWDS
	   DO 120 I=1,NCURVE
	      CTEMP = CLABEL(I)
            NIHEAD = NIHEAD + 1
	      CALL CH2HOL (CTEMP, IGBUFF(NIHEAD), NWDS)
	      NIHEAD = NIHEAD + NWDS - 1
 120  CONTINUE
	ENDIF
C
      IF (LDOUBLE) THEN
         JTYPE = 205
         N = NVALS * 2
C        Swap words on unix to keep compatitable with PC
         IF (IFLTAB(KDSWAP).NE.0) CALL zdswap6(DVALUES, N)
         CALL zwritex6 (IFLTAB, CPATH, NPATH, IGBUFF, NIHEAD,
     *   ICHEAD, 0, IUHEAD, NUHEAD, DVALUES, N, JTYPE,
     *   IPLAN, ISTAT, LFOUND)
C        Swap back so we don't mess up the user's data
         IF (IFLTAB(KDSWAP).NE.0) CALL zdswap6(DVALUES, N)
      ELSE
         JTYPE = 200
         CALL zwritex6 (IFLTAB, CPATH, NPATH, IGBUFF, NIHEAD,
     *   ICHEAD, 0, IUHEAD, NUHEAD, SVALUES, NVALS, JTYPE,
     *   IPLAN, ISTAT, LFOUND)
      ENDIF
C

C
C
 800  CONTINUE
      IF (MLEVEL.GE.7) WRITE (MUNIT,820) NVALS, ISTAT
 820  FORMAT(T5,'----- Exit zspd6, Number of data values ',
     * 'stored:',I7,',  Status:',I4,/)
C
      RETURN
C
C
 900  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT, 901) CPATH(1:NPATH)
 901  FORMAT (/,' *** ERROR:  zspd6;  Error writting record',
     * /,' Pathname: ',A,/)
      GO TO 800
C
 905  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT, 906) NORD, CPATH(1:NPATH)
 906  FORMAT (/,' *** ERROR:  zspd6;  The Number of Ordinates is Less',
     * ' than One ***',/,' Number Supplied:',I6,/,' Pathname: ',A,/)
      ISTAT = -4
      GO TO 800
C
 910  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT, 911) NCURVE, CPATH(1:NPATH)
 911  FORMAT (/,' *** ERROR:  ZSPD6;  The Number of Curves is Less',
     * ' than One ***',/,' Number Supplied:',I6,/,' Pathname: ',A,/)
      ISTAT = -5
      GO TO 800
C
 920  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT, 921) NCURVE, CPATH(1:NPATH)
 921  FORMAT (/,' *** ERROR:  zspd6;  The Number of Curves is Greater',
     * ' than 100 ***',/,' Number Supplied:',I6,'(Up to 100 curves may',
     * ' be stored in one record)',/,' Pathname: ',A)
      ISTAT = -5
      GO TO 800
C
C
 930  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT, 931)  CPATH(1:NPATH)
 931  FORMAT (/,' *** ERROR:  zspd6;  Exceeded size limit for number',
     * ' of data ***',/,' Pathname: ',A)
      WRITE (MUNIT,932) NVALS, NTOT, KLBUFF
 932  FORMAT('    Number values to store:',I7,
     *      ' Size to store:',I7,' Size Limit:',I7)
      ISTAT = -5
      GO TO 800
C
      END


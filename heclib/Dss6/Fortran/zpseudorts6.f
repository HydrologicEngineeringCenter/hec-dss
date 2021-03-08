      SUBROUTINE ZPseudoRTS6(CFROMPATH, CTOPATH, INTL,IACTION,ISTATUS)
      IMPLICIT NONE
C
C
C     Pseudo Regular (interval) Time Series
C     Irregular interval time series data that is almost regular
C     We display the almost regular interval with a tilde before
C     to indicate that it is pseudo.
C     For example, if one has daily readings, but occassionally
C     there is an intermittent reading, the ePart would be "~1DAY"
C     but the real ePart stored in the file would be "IR-YEAR",
C     with a pseudo time interval (stored with the path) of 1440 (mins).
C
C     CFROMPATH: Pathname from to be converted (input)
C     CTOPATH:  Pathname to convert to (output)
C     INTL:   The time interval to use when converting to pseudo
C     IACTION:  What to do:
C           1:  Convert pseudo to irregular
C               and return the number of minutes for the pseudo (INTL)
C           2:  Convert irregular to pseudo based on
C               the input INTLPS.  Note - does not check if this is
C               reasonable.
C       other:  undefined - no action.
C     ISTATUS:   0 - all okay
C               -1 - Not a pseudo pathname
C
C
      
C
      CHARACTER CFROMPATH*(*)
      CHARACTER CTOPATH*(*)
      INTEGER INTL, IACTION, ISTATUS
C
      INTEGER NPATH, IDX, JDX, NODATA, N
      CHARACTER CEPART*12
      CHARACTER CINTLS(5)*10
      INTEGER IBPART(6), IEPART(6), ILPART(6)
C
C
      DATA CINTLS / 'IR-DAY    ', 'IR-MONTH  ',
     * 'IR-YEAR   ', 'IR-DECADE ', 'IR-CENTURY'/
C
C
      CALL CHRLNB(CFROMPATH, NPATH)
      CALL ZUPATH (CFROMPATH(1:NPATH), IBPART, IEPART, ILPART, ISTATUS)
      IF (ISTATUS.NE.0) RETURN
C
      IF (IACTION.EQ.1) THEN
C
C       Look for the tilde to identify a pseudo Reg TS path
        IF (CFROMPATH(IBPART(5):IBPART(5)).NE.'~') THEN
           ISTATUS = -1
           RETURN
        ENDIF
        ISTATUS = 1
      CALL ZGINTL6(INTL,CFROMPATH(IBPART(5)+1:IEPART(5)),NODATA,ISTATUS)
        IF (ISTATUS.NE.0) THEN
            RETURN
        ENDIF
        IF (INTL.LT.15) THEN
           CEPART = CINTLS(1)
        ELSE IF (INTL.LT.1440) THEN
           CEPART = CINTLS(2)
        ELSE IF (INTL.LT.10080) THEN
           CEPART = CINTLS(3)
        ELSE IF (INTL.LT.50000) THEN
           CEPART = CINTLS(4)
        ELSE
           CEPART = CINTLS(5)
        ENDIF
        CALL CHRLNB(CEPART, N)
        call strcpy(CTOPATH, CFROMPATH(1:IBPART(5)-1)//CEPART(1:N)//
     *            CFROMPATH(IBPART(6)-1:NPATH))
        ISTATUS = 0
C
      ELSE IF (IACTION.EQ.2) THEN
C
        ISTATUS = 2
        CALL ZGINTL6(INTL, CEPART, NODATA, ISTATUS)
        IF (ISTATUS.NE.0) THEN
            RETURN
        ENDIF
        CALL CHRLNB(CEPART, N)
        call strcpy(CTOPATH,CFROMPATH(1:IBPART(5)-1)//'~'//CEPART(1:N)//
     *            CFROMPATH(IBPART(6)-1:NPATH))
        ISTATUS = 0

      ENDIF
C
      RETURN
      END


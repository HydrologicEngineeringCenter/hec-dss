      SUBROUTINE ZCOLPATH6 (CPATH, NPATH, CHPATH, LCOLL)
C
      implicit none
C
C     Determine if a pathname is a collections pathname
C     If it is, return the the logical LCOLL as true
C     and the base 'hash path' in CHPATH (must be same or
C     larger length as CPATH).
C     If not a collection pathname, return LCOLL false
C
      CHARACTER CPATH*(*), CHPATH*(*)
      INTEGER NPATH
      LOGICAL LCOLL
C
      INTEGER IBAR, ICOLL, JLEN
C
C
C     Is this a collections record?
C     Collections are identified by an F part of /C:000000|REST OF FPART/
C     Where 000000 is generally a sequence number, for example
C     /YUBA/SMARTSVILLE/FLOW/01JAN1997/1HOUR/C:000042|OPERATION A/
C     We'll replace 000000 with XXXXXX so that all records within a collection
C     have the same hash code, and then we only have to search that one hash

      LCOLL = .FALSE.
      IBAR = INDEX(CPATH(1:NPATH), '|')
	  IF (IBAR.GT.0) THEN
	   ICOLL = INDEX(CPATH(1:NPATH), '/C:')
	   IF (ICOLL.GT.0) THEN
	   JLEN = ICOLL + 9
	      IF (IBAR.EQ.JLEN) THEN
	         call strcpy(CHPATH, CPATH(1:ICOLL+2))
	         call strcpy(CHPATH(ICOLL+3:IBAR), 'XXXXXX')
	         call strcpy(CHPATH(IBAR:NPATH), CPATH(IBAR:NPATH))
	         LCOLL = .TRUE.
	         CALL UPCASE(CHPATH)
	      ENDIF
	   ENDIF
	  ENDIF
C
      RETURN
      END


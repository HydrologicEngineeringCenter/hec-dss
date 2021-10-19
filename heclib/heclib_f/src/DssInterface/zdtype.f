      SUBROUTINE zdtype7 (IFLTAB, CPATH, NDATA, LFOUND, CDTYPE, IDTYPE)
C
      implicit none
C
C     Determine the data type of a record and whether it exists
*
*     ******************   FIX ME  (NOT URGENT) ****************
*     ** THIS CALL is in FORTRAN SO THAT THE CHARACTER ID CAN BE RETURNED.
C
C     Written by Bill Charley at HEC, 1990.
C
       INCLUDE 'zdssShared.h'
C
      INTEGER IFLTAB(*)
      CHARACTER CPATH*(*), CDTYPE*(*)
      LOGICAL LFOUND
      INTEGER STATUS,NPATH,NDATA,IDTYPE,ISTAT,IST,INTL,N,I
      INTEGER IBPART(6), IEPART(6), ILPART(6)
C
C
C     Check if this record exists
C
      CALL CHRLNB (CPATH, NPATH)
      CALL zdataType7 (IFLTAB, CPATH(1:NPATH), NDATA,
     * STATUS, IDTYPE)
C
      IF (STATUS.EQ.0) THEN
        LFOUND = .TRUE.
C     Get data type
C
      ELSE
        LFOUND = .FALSE.
C
      NDATA = 0
      IDTYPE = 0
C     See if we can determine if this is a time series record
      CALL ZUPATH (CPATH, IBPART, IEPART, ILPART, ISTAT)
      IF ((ISTAT.EQ.0).AND.(ILPART(5).GE.4)) THEN
      IST = 1
      CALL zgetInterval (INTL, CPATH(IBPART(5):IEPART(5)), N, IST)
      IF (IST.EQ.0) THEN
      IDTYPE = 100
      ELSE IF (IST.EQ.1) THEN
      IDTYPE = 110
      ENDIF
C     CHECK FOR TIME SERIES PATTERN TYPE
      IF (CPATH(IBPART(4):IBPART(4)+2).EQ.'TS-') THEN
         IDTYPE = IDTYPE + 1
      ENDIF
      ENDIF
C
      ENDIF
C
C
C     Get the character record data type
      DO 40 I=1,NRTYPE
         IF (IDTYPE.EQ.IRTYPE(I)) THEN
            call strcpy(CDTYPE, CRTYPE(I))
            GO TO 50
         ENDIF
 40   CONTINUE
      call strcpy(CDTYPE, 'UND')
C
 50   CONTINUE
C
C
C
      RETURN
C
      END


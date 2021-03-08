      SUBROUTINE ZDEBUG7 (IVAL, IVAL8, IVAL4A, IVAL4B, CDVAL, CRVALA,
     *                    CRVALB, CSTRNG, IBYTES)
C
      implicit none
C
C     Return an IFLTAB word in different possible formats.
C     CRVAL is a character string with the REAL representation
C     CHARACTER CRVAL*12
C     CHARACTER CSTRNG*4
C     INTEGER IBYTES(4)
C
C     Written by Bill Charley at HEC
C     
      integer j,n
      INTEGER(4) IVAL4A, IVAL4B
      INTEGER(8) IVAL, IVAL8
      CHARACTER CDVAL*(*), CRVALA*(*), CRVALB*(*)
      CHARACTER CSTRNG*(*)
      INTEGER IBYTES(*)
C
C
      DOUBLE PRECISION DV
      INTEGER(8) I8
      REAL RV(2)
      INTEGER(4) IV(2)
      EQUIVALENCE (I8, IV(1), RV(1), DV)
C
C
C
      IVAL8 = IVAL
      I8 = IVAL
      IVAL4A = IV(1)
      IVAL4B = IV(2)
C
      call strcpy(CSTRNG, ' ')
C
      CALL HOL2CH (IVAL, CSTRNG, 2)
      DO 30 J=1,8
         N = ICHAR(CSTRNG(J:J))
         IF ((N.LT.32).OR.(N.GT.126)) CSTRNG(J:J) = '~'
 30   CONTINUE
C
      DO 40 N=1,8
         CALL GETHOL (IVAL, N, IBYTES(N))
 40   CONTINUE

#ifdef _MSC_VER
      IF (ISNAN(RV(1))) THEN
         call strcpy(CRVALA, '============')
      ELSE
        CALL XREALC (RV(1), CRVALA, 1, LEN(CRVALA), 2)
      ENDIF

      IF (ISNAN(RV(2))) THEN
         call strcpy(CRVALB, '============')
      ELSE
         CALL XREALC (RV(2), CRVALB, 1, LEN(CRVALB), 2)
      ENDIF

      IF (ISNAN(DV)) THEN
         call strcpy(CDVAL, '============')
      ELSE
        CALL XDOUBC (DV, CDVAL, 1, LEN(CDVAL), 2)
      ENDIF

#else
      CALL XREALC (RV(1), CRVALA, 1, LEN(CRVALA), 2)
      CALL XREALC (RV(2), CRVALB, 1, LEN(CRVALB), 2)
      CALL XDOUBC (DV, CDVAL, 1, LEN(CDVAL), 2)
#endif

C
      RETURN
C
      END


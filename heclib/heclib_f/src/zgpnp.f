      SUBROUTINE zgpnp (CLINE, CA, CB, CC, CD, CE, CF, NPARTS)
      include 'dss_parameters.h'
C     Searches character string CLINE for pathname parts
C     The parts are specified by their part letter (A, B, C, D, E,
C     or F), followed by an equal sign then the corresponding part.
C     Information other than the parts may be in CLINE, as long as
C     it can be distinguished from the parts.
C     A typlical line might be:
C        ZR=IN  A=ALLEGHENY, B=SOUTH FORK F=,
C
C     Written by Bill Charley at HEC, September 1989
C
C
      CHARACTER CA*(*), CB*(*), CC*(*), CD*(*), CE*(*), CF*(*)
      CHARACTER CLINE*(*)
      INTEGER NPARTS(6), IST(6)
      CHARACTER CLETTR(12)*1
      CHARACTER(len=dss_maxpart) CPART
      INTEGER IBF(12), IEF(12), ILF(12)
      LOGICAL LSET
C
C
      DO 20 I=1,6
      IST(I) = NPARTS(I)
      NPARTS(I) = -1
 20   CONTINUE
C
      CALL PARSEQ (CLINE, 12, NFIELD, CLETTR, IBF, IEF, ILF)
C
      IF (NFIELD.EQ.0) THEN
      NPARTS(1) = -10
      GO TO 800
      ENDIF
C
C
      LSET = .FALSE.
      DO 40 I=1,NFIELD
C
      IF (ILF(I).GT.0) THEN
      CPART = CLINE(IBF(I):IEF(I))
      CALL CHRLNB (CPART, NLEN)
      ELSE
      CPART = ' '
      NLEN = 0
      ENDIF
C
      IF (CLETTR(I).EQ.'A') THEN
      IF (IST(1).NE.-2) THEN
      call strcpy(CA, CPART)
      NPARTS(1) = NLEN
      LSET = .TRUE.
      ENDIF
C
      ELSE IF (CLETTR(I).EQ.'B') THEN
      IF (IST(2).NE.-2) THEN
      call strcpy(CB, CPART)
      NPARTS(2) = NLEN
      LSET = .TRUE.
      ENDIF
C
      ELSE IF (CLETTR(I).EQ.'C') THEN
      IF (IST(3).NE.-2) THEN
      call strcpy(CC, CPART)
      NPARTS(3) = NLEN
      LSET = .TRUE.
      ENDIF
C
      ELSE IF (CLETTR(I).EQ.'D') THEN
      IF (IST(4).NE.-2) THEN
      call strcpy(CD, CPART)
      NPARTS(4) = NLEN
      LSET = .TRUE.
      ENDIF
C
      ELSE IF (CLETTR(I).EQ.'E') THEN
      IF (IST(5).NE.-2) THEN
      call strcpy(CE, CPART)
      NPARTS(5) = NLEN
      LSET = .TRUE.
      ENDIF
C
      ELSE IF (CLETTR(I).EQ.'F') THEN
      IF (IST(6).NE.-2) THEN
      call strcpy(CF, CPART)
      NPARTS(6) = NLEN
      LSET = .TRUE.
      ENDIF
C
      ENDIF
C
 40   CONTINUE
      IF (.NOT.LSET) NPARTS(1) = -10
C
 800  CONTINUE
      RETURN
      END


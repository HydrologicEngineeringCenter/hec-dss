      INTEGER FUNCTION IHM2S (CTIME)
C
C     CHANGE FROM CODED MILITARY TIME TO SECONDS SINCE MIDNIGHT
C
      CHARACTER CTIME*(*)
      CHARACTER CTEMP*8
C
C
C     Look for a semi colon to indicate a more complex date
      IF (INDEX(CTIME, ':').LT.0) THEN
        J = IHM2M(CTIME)
        IHM2S = J * 60
        RETURN
      ENDIF
C
C     So there is a semi-colon.... crack it.


      CTEMP = ' '
      CALL CHRLNB (CTIME, N)
      IF (N.EQ.0) GO TO 900
      if (N.gt.5) then
        IPOS = 6
      ELSE
        IPOS = 4
      ENDIF
C     Remove everything except numbers, working backwards
C     to take care of a leading 0, if missing.
      DO 10 I=N, 1, -1
         K = ICHAR(CTIME(I:I))
         IF ((K.GE.48).AND.(K.LE.57)) THEN
            IF (IPOS.GE.1) CTEMP(IPOS:IPOS) = CTIME(I:I)
            IPOS = IPOS - 1
         ENDIF
         IF (K.GT.59) GO TO 900
 10   CONTINUE
C
C
C     Now we either have a 4 digit number or 6 digit one...
      if (N.GT.5) then
        READ ( CTEMP, 20, ERR=900) IHR, IMIN
        ISEC = 0
      else
        READ ( CTEMP, 20, ERR=900) IHR, IMIN, ISEC
      endif
 20   FORMAT (3I2)
C
      IHM2S = (IHR*3600) + (IMIN*60) + ISEC
      RETURN
C
C     ERROR
  900 IHM2S = -1
      RETURN
      END


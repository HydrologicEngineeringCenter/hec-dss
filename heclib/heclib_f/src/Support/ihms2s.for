      INTEGER FUNCTION IHMS2S (CTIME)
C
C     CHANGE FROM CODED MILITARY TIME TO seconds SINCE MIDNIGHT
C     Valid forms include 0930, 09:30, 09:30:18
C
      CHARACTER CTIME*(*)
      CHARACTER CTEMP*6
C
C
      CTEMP = ' '
      CALL CHRLNB (CTIME, N)
      IF (N.EQ.0) GO TO 900
      IF (N.LE.5) THEN
         MIN = IHM2M(CTIME)
         IF (MIN.LT.0) GO TO 900
         IHMS2S = MIN * 60
         GO TO 800
      ENDIF
C
      IPOS = 6
      DO 10 I=N, 1, -1
         K = ICHAR(CTIME(I:I))
         IF (((K.GE.48).AND.(K.LE.57)).OR.(K.EQ.32)) THEN
            IF (IPOS.LT.1) GO TO 15
            CTEMP(IPOS:IPOS) = CTIME(I:I)
            IF (K.EQ.32) CTEMP(IPOS:IPOS) = '0'	
            IPOS = IPOS - 1
         ENDIF
         IF (K.GT.59) GO TO 900
 10   CONTINUE
C
C
 15   CONTINUE
      READ ( CTEMP, 20, ERR=900) IHR, IMIN, ISEC
 20   FORMAT (3I2)
C
      IHMS2S = (IHR*3600) + (IMIN*60) + ISEC
C
 800  CONTINUE
      RETURN
C
C     ERROR
 900  IHMS2S = -1
      RETURN
      END

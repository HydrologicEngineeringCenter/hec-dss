      SUBROUTINE ATTMAT (CKYARY,CKYWRD,IEN,JKS,KS,JSUB)
C
C-----------------------------------------------------------------------
C        FIND MATCHING KEYWORD, CKYARY IS ARRAY HOLDING KEYWORDS
C        FROM THE EXECUTE LINE.  CKYWRD IS THE KEYWORD IN THE CALL
C        TO ATTACH.
C-----------------------------------------------------------------------
C
      CHARACTER CKYARY(JKS)*(*), CKYWRD*(*)
C
      KLN = LEN(CKYWRD)
      JLN = INDEX(CKYWRD,' ') -1
      IF (JLN.LE.0) JLN = KLN
      JSUB = 0
C
      DO 200 K=1,KS
        ILN = INDEX(CKYARY(K),' ') -1
        IF (ILN.LE.0) GO TO 200
C
        IF (INDEX(CKYWRD(1:JLN),CKYARY(K)(1:ILN)).EQ.1) THEN
C-----------------------------------------------------------------------
C        ABBREVIATION IS CORRECT BUT DOES THE LENGTH MATCH?
C-----------------------------------------------------------------------
          IF (ILN.LT.IEN) GO TO 200
          JSUB = K
          IF (ILN.EQ.JLN) GO TO 999
C
          DO 100 J=K+1,KS
            ILN = INDEX(CKYARY(J),' ') -1
            IF (ILN.EQ.JLN) THEN
              IF (CKYARY(J)(1:ILN).EQ.CKYWRD(1:JLN)) THEN
                JSUB = J
                GO TO 999
              ENDIF
            ENDIF
 100      CONTINUE
C
        ENDIF
 200  CONTINUE
C
 999  RETURN
      END


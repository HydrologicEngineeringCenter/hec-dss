      LOGICAL FUNCTION LEQNER(X,Y,TOL)
C
C     ******************************************************************
C
C                LOGICAL FUNCTION LEQNER (EQUAL NEAR)
C     THIS FUNCTION RETURNS TRUE OR FALSE (LOGICAL) ANSWERS
C     IF X IS NEAR Y (WITHIN TOLERANCE TOL) ANSWER IS TRUE
C
C     ******************************************************************
C
      LEQNER=.TRUE.
      IF(ABS(X-Y).GT.TOL) LEQNER=.FALSE.
      RETURN
      END


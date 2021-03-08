      LOGICAL FUNCTION LEQNERD (X,Y,TOL)
C
C     ******************************************************************
C
C                LOGICAL FUNCTION LEQNER (EQUAL NEAR)
C     THIS FUNCTION RETURNS TRUE OR FALSE (LOGICAL) ANSWERS
C     IF X IS NEAR Y (WITHIN TOLERANCE TOL) ANSWER IS TRUE
C
C     ******************************************************************
      DOUBLE PRECISION X, Y
C
      LEQNERD = .TRUE.
      IF(DABS(X-Y).GT.TOL) LEQNERD = .FALSE.
      RETURN
      END


      SUBROUTINE WAIT ( I1, I2, I3)                                     MLu
C
C     TEMPORARY SUBROUTINE FOR NON-HARRIS SITES
C
      
      REAL TIM                                                          MLu
     
      I3 = 1                                                            MLu
      IF (I2.EQ.0) THEN                                                 MLu
         TIM = I1/120.0                                                 MLu
      ELSE IF (I2.EQ.1) THEN                                            MLu
         TIM = .001*I1                                                  MLu
      ELSE IF (I2.EQ.2) THEN                                            MLu
         TIM = I1*1.0                                                   MLu
      ELSE IF (I2.EQ.3) THEN                                            MLu
         TIM = I1*60.0                                                  MLu
      ELSE                                                              MLu
         I3 = 2                                                         MLu
C     RETURN                                                            u
      ENDIF                                                             MLu
      CALL WAITS (TIM)                                                  MLu
      RETURN                                                            MLu
      END                                                               MLu


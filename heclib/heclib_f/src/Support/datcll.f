      SUBROUTINE DATCLL (JULIN,INTIME,JULOUT,IOUTIM)
C
C     DATE CLEAN LARGE
C
C     THIS ROUTINE CLEANS UP THE DATE AND OFFSETS BY INCREMENTING
C     JULIN AND INTIME BY MINS MINUTES SO THAT IOUTIM IS NEVER
C     GREATER THAN 1440 (THE NUMBER OF MINUTES PER DAY)
C     The input time (INTIME) is given as an integer*6 or *4 number
C     The output time is an Integer*3 or *2 number
C
C     INTEGER*6 INTIME, I1440                                           H
      INTEGER*4 JULIN, INTIME, JULOUT, I1440, ITEMP                     MLu
C
C
      I1440 = 1440
C
      IF (INTIME.GT.I1440) THEN
      JULOUT = JULIN + INTIME/I1440
C     IOUTIM = MOD2 (INTIME,I1440)                                      H
      ITEMP = MOD (INTIME,I1440)                                        MLu
      IOUTIM = ITEMP                                                    MLu
      ELSE IF (INTIME.LT.0) THEN
      JULOUT = JULIN + INTIME/I1440 -1
C     IOUTIM = MOD2 (INTIME,I1440) + I1440                              H
      ITEMP = MOD (INTIME,I1440) + I1440                                MLu
      IOUTIM = ITEMP                                                    MLu
      ELSE
      JULOUT = JULIN
      IOUTIM = INTIME
      ENDIF
C
      IF (IOUTIM.EQ.0) THEN
      JULOUT = JULOUT - 1
      IOUTIM = 1440
      ENDIF
C
      RETURN
      END


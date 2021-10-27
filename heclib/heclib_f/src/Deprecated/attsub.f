      SUBROUTINE ATTSUB (CDFALT,CDFSUB,NDFALT)
C
C-----------------------------------------------------------------------
C  SUBSTITUTE RECOGNIZED NAMES TO SYSTEM DEPENDENT NAMES.
C-----------------------------------------------------------------------
C
      CHARACTER CDFALT*(*), CDFSUB*(*)
      CHARACTER FMT*4
C
C     CHARACTER CDFARY(50)*4                                            H
C
C     DATA CDFARY/'W1','W2','W3','W4','W5','W6','W7','W8','W9','W0',    H
C    +            'T1','T2','T3','T4','T5','T6','T7','T8','T9','T0',    H
C    +            'S1','S2','S3','S4','S5','S6','S7','S8','S9','S0',    H
C    +            'U1','U2','U3','U4','U5','U6','U7','U8','U9','U0',    H
C    +            'H1','H2','H3','H4','H5','H6','H7','H8','H9','H0'/    H
C
      DATA FMT/'(IX)'/
C
      CDFSUB = CDFALT
C
C-----------------------------------------------------------------------
C  CHANGE 'STDIN' TO APPROPRIATE STANDARD INPUT NAME.
C-----------------------------------------------------------------------
C
      IF (CDFALT(1:NDFALT) .EQ. 'STDIN') THEN
C
        CDFSUB = 'CON'                                                  ML
        NDFALT = 3                                                      ML
C
C       CDFSUB = 'stdin'
C       NDFALT = 5
C
C       CDFSUB = '/dev/tty'                                             u
C       NDFALT = 8                                                      u
C
C       CDFSUB='*0'                                                     H
C       NDFALT=2                                                        H
C
        GO TO 999
C
C-----------------------------------------------------------------------
C  CHANGE 'STDOUT' TO APPROPRIATE STANDARD OUTPUT NAME.
C-----------------------------------------------------------------------
C
      ELSE IF (CDFALT(1:NDFALT) .EQ. 'STDOUT') THEN
C
        CDFSUB = 'CON'                                                  ML
        NDFALT = 3                                                      ML
C
C       CDFSUB = 'stdout'
C       NDFALT = 6
C
C       CDFSUB = '/dev/tty'                                             u
C       NDFALT = 8                                                      u
C
C       CDFSUB='*3'                                                     H
C       NDFALT=2                                                        H
C
        GO TO 999
C
C-----------------------------------------------------------------------
C  CREATE SCRATCH FILE USING PASSED NAME
C-----------------------------------------------------------------------
C
      ELSE IF (NDFALT .GT. 7) THEN
        IF (CDFALT(1:7) .EQ. 'SCRATCH') THEN
         K = INDEX (CDFALT(1:NDFALT), '.')                              MLu
         IF (K.EQ.0) THEN                                               MLu
C
C-----------------------------------------------------------------------
C  CHANGE 'SCRATCHN' -- TO SCRATCH.001 TO .999 FOR THE PC'S
C                       TO WORK FILES W1 TO H0 ON THE HARRIS
C-----------------------------------------------------------------------
C
          J = NDFALT - 7
C
          WRITE (FMT(3:3),'(I1)') J
          READ (CDFALT(8:NDFALT),FMT) NUM
C
          WRITE (CDFSUB,100) NUM                                        MLu
 100      FORMAT ('SCRATCH.',I3.3)                                      MLu
          NDFALT = 11                                                   MLu
C
C         CDFSUB = CDFARY(NUM)                                          H
C         NDFALT=NINDXR(CDFSUB,' ')                                     H
C
        ENDIF                                                           MLu
        ENDIF
      ENDIF
C
 999  RETURN
      END


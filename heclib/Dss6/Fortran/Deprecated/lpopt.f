      LOGICAL FUNCTION LPOPT (COPT)
C
C     PROGRAM OPTION IS A LOGICAL FUNCTION TO TELL IF
C     A OPTION CHARACTER IS USED IN THE EXECUTION.  FOR
C     EXAMPLE IF PROGRAM IS EXCUTED BY:
C         or MYPROG -D
C     THEN THE CALLS TO LPOPT YIELD:
C         IF (LPOPT('D')) THEN .....   (LPOPT RETURNS TRUE)
C         IF (LPOPT('F')) THEN .....   (LPOPT RETURNS FALSE)
C     VALID OPTION CHARACTERS ARE THE LETTERS A-X.
C     USER MUST DEFINE LPOPT AS A EXTERNAL LOGICAL VARIABLE:
C     EXTERNAL LOGICAL LPOPT
C
      CHARACTER COPT*(*)
      CHARACTER CPARM*150, COPTN*1
      SAVE LFIRST, CPARM, NPARM, NFIELD, IBF, IEF
      INTEGER IBF(20), IEF(20), ILF(20)
      LOGICAL LFIRST
C
C
      DATA LFIRST /.TRUE./
C
C
      IF (LFIRST) THEN
         LFIRST = .FALSE.
         CALL CPARMS (CPARM, NPARM)                                     Mu
C        CPARM = ' '                                                    L
C        CALL GETCL (CPARM)                                             L
C        CALL CHRLNB (CPARM, NPARM)                                     L
         IF ((NPARM.LT.0).OR.(NPARM.GT.150)) THEN
            WRITE (*,*)'Dimension error in LPOPT!'
            GO TO 100
         ENDIF
         IF (NPARM.EQ.0) GO TO 100
         CALL UPCASE (CPARM(1:NPARM))
         CALL PARSLI (CPARM(1:NPARM), 20, NFIELD, IBF, IEF, ILF)
      ENDIF
C
      IF (NPARM.GT.0) THEN
         COPTN = COPT
         CALL UPCASE (COPTN)
         DO 80 I=1,NFIELD
C           IF  (CPARM(IBF(I):IBF(I)).EQ.'-') THEN                      u
            IF ((CPARM(IBF(I):IBF(I)).EQ.'-') .OR.                      ML
     *          (CPARM(IBF(I):IBF(I)).EQ.'/')) THEN                     ML
               IF (INDEX(CPARM(IBF(I):IEF(I)),COPTN).GT.0) THEN
                  LPOPT = .TRUE.
                  RETURN
               ENDIF
            ENDIF
 80      CONTINUE
      ENDIF
C
C     OPTION NOT PRESENT
 100  LPOPT = .FALSE.
      RETURN
      END


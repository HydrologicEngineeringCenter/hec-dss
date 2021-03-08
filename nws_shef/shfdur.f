      SUBROUTINE SHFDUR ( INTL,CTYPE,PC,DV )
C
      CHARACTER*(*) CTYPE,PC,DV
C
      DIMENSION ZINTL(14)
      INTEGER ZINTL, INTL                                             M
      CHARACTER CDV*14
C
      DATA ZINTL /1,15,30,60,120,180,360,480,720,1080,1440,10080,
     1            43200,525600 /
      DATA CDV /'UCJHBTQAKLDWMY'/
C
      DV = ' '
      IF ( INDEX(CTYPE,'PER').GT.0 ) THEN
         IF ( INTL.GT.0 ) THEN
         N = 1
   5     CONTINUE
            IF ( INTL.EQ.ZINTL(N) ) THEN
               PC(3:3) = CDV(N:N)
            ELSE IF ( N.LT.14 ) THEN
               N = N + 1
               GO TO 5
            ELSE
               PC(3:3) = 'V'
               CALL SHFDI ( INTL,DV )
               DV(1:2) = 'DV'
            ENDIF
         ELSE IF ( INTL.EQ.0 ) THEN
            PC(3:3) = 'X'
         ENDIF
      ENDIF
      RETURN
      END

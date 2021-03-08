      SUBROUTINE CKMISS ( DATA,CDATA,NC,NCD )
C
      CHARACTER CDATA(*)*(*)
      DIMENSION DATA(*),NC(*)
      logical lpopt
C
      DO 100 N=1,NCD
         IF ( DATA(N).LE.-900.95.AND.DATA(N).GE.-910.05 ) THEN
           if ( lpopt('J') ) then
            CDATA(N) = '(M)'
            NC(N) = 3
           else
            CDATA(N) = 'M'
            NC(N) = 1
           endif
         ENDIF
 100  CONTINUE
      RETURN
      END

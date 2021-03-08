      SUBROUTINE GETDES ( TS,DES )
      CHARACTER*(*) TS,DES
C
      IF ( TS.EQ.'RZ' ) THEN
         DES = 'OBS'
      ELSE IF ( TS.EQ.'RG' ) THEN
         DES = 'GOES'
      ELSE IF ( TS.EQ.'RP' ) THEN
         DES = 'DARDC'
      ELSE IF ( TS.EQ.'RT' ) THEN
         DES = 'TELEMARK'
      ELSE
         DES = TS
      ENDIF
      RETURN
      END

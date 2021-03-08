      SUBROUTINE GETEXT ( CODEX,PARAM,CPART,INTL,TSTYPE,ERRMSG )
C
C     SHEF EXTREMA CODE AFFECTS PARAM (CPART),TSTYPE,AND INTL
C
      CHARACTER*(*) CODEX,PARAM,TSTYPE,ERRMSG,CPART
C
      CHARACTER EXCODE*12
C     INTEGER EXINTL(6)                                                 H
      INTEGER*4 EXINTL(6)                                               M
      INTEGER*4 INTL                                                    M
      DATA EXCODE / 'JKLMNPTUVWXY' /
      DATA EXINTL /0,518400,43200,10080,1440,720 /
C
      ERRMSG=' '
      ICODE = INDEX ( EXCODE,CODEX )
      IF ( ICODE.GE.1 ) THEN
         CALL CHRFLB ( PARAM,IFNB,IEND)
         IF ( ICODE.LE.6 ) THEN
            CPART = PARAM (IFNB:IEND)//' MIN'
         ELSE
            CPART = PARAM (IFNB:IEND)//' MAX'
         ENDIF
         JINTL = MOD ( ICODE,6 )
         IF ( JINTL.EQ.1 ) THEN
            TSTYPE = 'IR'
            INTL = 0
         ELSE
            TSTYPE = 'RG'
            INTL = EXINTL( JINTL )
         ENDIF
      ELSE
         ERRMSG = 'ERROR --- EXTREMA CODE "'//CODEX//'"  NOT RECOGNIZED'
      ENDIF
      RETURN
      END

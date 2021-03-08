      SUBROUTINE zinqir (IFLTAB, CFLG, CALPHA, INUMB)
C
      implicit none
C
C     Inquire about the file or a system setting
C
      INTEGER IFLTAB(*)
      CHARACTER CFLG*(*), CALPHA*(*)
      INTEGER INUMB, iversion, ilen
C
      call zGetVersion(IFLTAB, iversion)
      IF (iversion.EQ.6) THEN
          CALL zinqir6(IFLTAB, CFLG, CALPHA, INUMB)
      ELSE IF (iversion.EQ.7) THEN
          ilen = len(CALPHA)
          CALL zinquireChar(IFLTAB, CFLG, CALPHA, ilen, INUMB)
      ELSE
C       Uh oh... how'd we get here?
        IF ((CFLG(1:3).EQ.'ERR').OR.(CFLG(1:3).EQ.'err')) THEN
            CALL zquery(CFLG, CALPHA, INUMB)
        ELSE IF (CFLG(1:3).EQ.'VER') THEN
            INUMB = 7
        ENDIF
      ENDIF
C
      RETURN
      END
c
      SUBROUTINE zinqir7(IFLTAB, CFLG, CALPHA, INUMB)
C
      implicit none
C
C     Inquire about the file or a system setting
C
      INTEGER IFLTAB(*)
      CHARACTER CFLG*(*), CALPHA*(*)
      INTEGER INUMB, ilen
C
      ilen = len(CALPHA)
      CALL zinquireChar(IFLTAB, CFLG, CALPHA, ilen, INUMB)
      return
      end


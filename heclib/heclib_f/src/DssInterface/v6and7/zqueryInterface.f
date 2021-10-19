      SUBROUTINE zquery(CPARAM, CALPHA, INUMB)
C
      implicit none
      INCLUDE 'zopenDefault.h'
C
C     Query  a global parameter
C     Returns information from DSS 7 first;
C     If not in DSS 7, then returns DSS 6 info
C
      CHARACTER CPARAM*(*), CALPHA*(*)
      INTEGER INUMB
      CHARACTER CP*4
C
C
C
      CP = CPARAM
      CALL UPCASE(CP)
      IF (CP.EQ.'DSSV') THEN
        if (versionNext.NE.0) then
            INUMB = versionNext
          else if (versionAll.NE.0) then
            INUMB = versionAll
          else
            INUMB = 0
        endif
        return
      ELSE IF (CP.EQ.'ALLV') THEN
        !  AllVersions
        INUMB = versionAll
        return
      ENDIF
C
      CALL zquery7(CPARAM, CALPHA, INUMB)
      if (INUMB.LE.0) then
        CALL zquery6(CPARAM, CALPHA, INUMB)
      endif
C
      RETURN
      END


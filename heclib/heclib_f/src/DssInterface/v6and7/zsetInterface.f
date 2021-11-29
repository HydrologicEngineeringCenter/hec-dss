      SUBROUTINE zset(CPARAM, CALPHA, INUMB)
C
      implicit none
      INCLUDE 'zopenDefault.h'
C
C     Set a global parameter
C     Note:  Portions of this have been deprecated;
C     call zsetfi to set any parameters specific to a file
C
      CHARACTER CPARAM*(*), CALPHA*(*)
      CHARACTER CP*4
      INTEGER INUMB
C
C
C     If the set parameter is "VERS", then the next file to
C     open will be the version number in INUMB
C
      CP = CPARAM
      CALL UPCASE(CP)
      IF (CP.EQ.'DSSV') THEN
        versionNext = INUMB
        return
      ELSE IF (CP.EQ.'ALLV') THEN
        !  AllVersions
        versionAll = INUMB
        return
      ENDIF
C
      write(*,*) 'In zset: setting ',cparam,' to ',calpha
      write(*,*) 'Calling zset6'
      CALL zset6(CPARAM, CALPHA, INUMB)
      write(*,*) 'Calling zset7'
      CALL zset7(CPARAM, CALPHA, INUMB)
C
      RETURN
      END


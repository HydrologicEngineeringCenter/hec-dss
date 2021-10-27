      INTEGER FUNCTION fortranwritelc (IUNIT, STRING, LEND_REC)
C
C     Interface routine for C/C++ functions to write to a file
C     under FORTRAN with Line Control (CR/LF)
!      use ifcore
C
C     Not multi-thread safe, but most calls are made one right after
C     the other, so should be okay for most circumstances
C     If an (uncommon) fail, then the message will be messed up
C     (No biggie)
C
C     Rewrite at a later time with thread safety
C
C
      COMMON /FORWRITE/ NCHARS, CSTR
      INTEGER NCHARS
      CHARACTER CSTR*1000
C
      INTEGER IUNIT
      CHARACTER STRING*(*)
      Logical LEND_REC
C
      INTEGER ISTAT

      DATA NCHARS /0/
C
C
C
      IF ((.NOT.LEND_REC).OR.(NCHARS.GT.0)) THEN
        DO 10 I=1,LEN(STRING)
            NCHARS = NCHARS + 1
            IF (NCHARS.GT.500) GO TO 15
            CSTR(NCHARS:NCHARS) = STRING(I:I)
 10     CONTINUE
 15     CONTINUE
      ENDIF
C
      IF (LEND_REC) THEN
        IF (NCHARS.EQ.0) THEN
            WRITE (IUNIT, 20, IOSTAT=ISTAT) STRING
 20         FORMAT(A)
        ELSE
            IF (NCHARS.GT.500) NCHARS = 500
            IF (NCHARS.LT.1) NCHARS = 1
            WRITE (IUNIT, 20, IOSTAT=fortranwritelc) CSTR(1:NCHARS)
            NCHARS = 0
        ENDIF
      ELSE
        fortranwritelc = 0
      ENDIF
      !  Make sure line is written to disk
      !  good for debugging, but slows process down
      !ISTAT = COMMITQQ(IUNIT)
C
      RETURN
      END


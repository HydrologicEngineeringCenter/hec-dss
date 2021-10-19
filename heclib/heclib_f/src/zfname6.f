      SUBROUTINE zfname (CIN, COUT, NNAME, LEXIST)
      CHARACTER CIN*(*), COUT*(*)
      INTEGER NNAME
      LOGICAL LEXIST
      CALL zfname6 (CIN, COUT, NNAME, LEXIST)
      return
      end
      
      
      SUBROUTINE zfname6 (CIN, COUT, NNAME, LEXIST)
C
C
C     Add machine dependent extensions to form DSS file name
C     Determine if the file exists in the current directory
C
C     Written by Bill Charley, HEC, May 1990
C
C     Modified Sept 1998 to call fullpathname
C     Handles filenames up to 256 characters
C
      CHARACTER CIN*(*), COUT*(*)
      INTEGER NNAME
      LOGICAL LEXIST, LEXTEN
C
C
      CALL CHRFLB (CIN, IBEG, IEND)
      IF (IEND.LE.0) THEN
         COUT = ' '
         LEXIST = .FALSE.
         GO TO 800
      ENDIF
C
C
C     See if there is already an extension on this name
      LEXTEN = .FALSE.
      IF (IEND.GT.4) THEN
         IF (CIN(IEND-3:IEND-3).EQ.'.') LEXTEN = .TRUE.
      ENDIF
C
C     Default extension of '.dss'
      IF (.NOT.LEXTEN) THEN
         COUT = CIN(IBEG:IEND) // '.dss'
      ELSE
         COUT = CIN(IBEG:IEND)
      ENDIF
C
      CALL CHRLNB (COUT, NNAME)
      INQUIRE (FILE=COUT(1:NNAME), EXIST=LEXIST, ERR=800)
C
 800  CONTINUE
      RETURN
      END


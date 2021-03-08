      SUBROUTINE zchkpn6 (CPATH, NPATH, ISTAT)
C
C
C     Check to see that the pathname provided meets certain specs
C
C     Written by Bob Carl
C
C     Input Arguments:
C
C       CPATH  = The DSS pathname (/A/B/C/D/E/F/).  Must be of type
C                character.
C       NPATH  = The number of characters in the pathname CPATH.
C
C     Output Arguments
C
C       ISTAT  = The status parameter returned to indicate the validity
C                of the pathname.  Possible status codes are:
C
C                  ISTAT      Description
C
C                   +6        Null character(s) set to blank.
C
C                    0        Pathname is valid.
C
C                   -1        First character in pathname is not a slash
C                             ("/").
C
C                   -2        Last character in pathname is not a slash
C                             ("/").
C
C                   -3        The number of slashes ("/") is not seven -
C                             there must be exactly seven slashes.
C
C                   -4        There are fewer than seven characters in
C                             the pathname.
C
C                   -5        There are more than eighty characters in
C                             the pathname.
C
C                   -6        Bad character(s) in pathname.
C
C
C     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      CHARACTER *(*) CPATH
C
      INCLUDE 'zdssmz.h'
C
      INCLUDE 'zdssnz.h'
C
C
C
      ISTAT=0
      NULL=0
C
C     CORRECT RANGE FOR NUMBER OF CHARACTERS?
      IF (NPATH.GT.6 .AND. NPATH.LE.392) THEN
C
C     FIRST CHARACTER MUST BE SLASH
      IF (CPATH(1:1).NE.'/') THEN
      ISTAT = -1
      GO TO 900
C
C     LAST CHARACTER MUST BE SLASH
      ELSE IF (CPATH(NPATH:NPATH).NE.'/') THEN
      ISTAT = -2
      GO TO 900
C
C     CHECK EACH CHARACTER IN PATHNAME
      ELSE
      NSL=0
      DO 10 I=1,NPATH
      II = I
C
      MASCII = ICHAR(CPATH(I:I))
C
C     COUNT THE NUMBER OF SLASHES
      IF (MASCII.EQ.47) THEN
      NSL = NSL + 1
C
C     CHANGE NULL (0) TO BLANK (32)
      ELSE IF (MASCII.EQ.0) THEN
      ISTAT = 6
      CPATH(I:I) = ' '
      NULL = NULL + 1
      ELSE IF (MASCII.LT.32) THEN
      ISTAT = -6
      GO TO 900
      ELSE IF (MASCII.GT.126) THEN
      ISTAT = -6
      GO TO 900
      ENDIF
C
   10 CONTINUE
C
      IF (NSL.NE.7) THEN
      ISTAT = -3
      GO TO 900
      ENDIF
      ENDIF
C
C     TOO FEW CHARACTERS IN PATHNAME
      ELSE IF (NPATH.LT.7) THEN
      ISTAT = -4
      GO TO 900
C
C     TOO MANY CHARACTERS IN PATHNAME
C     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ELSE IF (NPATH.GT.392) THEN
      ISTAT = -5
      GO TO 900
      ENDIF
C
 800  CONTINUE
      if (istat.ne.0) write(*,*)'Invalild path: ',CPATH
      RETURN
C
 900  CONTINUE
C
       write(*,*)'Invalild path: ',CPATH
      IF (MLEVEL.GT.2) THEN
C
      WRITE (MUNIT,910) ISTAT
 910  FORMAT (' Invalid Pathname Given;  Status:',I4)
C
      IF (ISTAT.EQ.-1) THEN
      WRITE (MUNIT,920) CPATH(1:NPATH)
 920  FORMAT (' Pathname does not begin with a slash:',/,1X,A)
C
      ELSE IF (ISTAT.EQ.-2) THEN
      WRITE (MUNIT,930) CPATH(1:NPATH)
 930  FORMAT (' Pathname does not end with a slash:',/,1X,A)
C
      ELSE IF (ISTAT.EQ.-3) THEN
      WRITE (MUNIT,940) CPATH(1:NPATH)
 940  FORMAT (' The pathname does not contain 7 slashes:',/,1X,A)
C
      ELSE IF (ISTAT.EQ.-4) THEN
      WRITE (MUNIT,950) NPATH
 950  FORMAT (' The pathname contains less than 7 characters;  Length:',
     * I5)
      IF (NPATH.GT.0) WRITE (MUNIT,951) CPATH(1:NPATH)
 951  FORMAT (1X,A)
C
      ELSE IF (ISTAT.EQ.-5) THEN
      WRITE (MUNIT,960) MXPATH, NPATH
 960  FORMAT (' The pathname contains more than',I4,' characters;  ',
     * 'Length:',I7)
      WRITE (MUNIT,951) CPATH(1:MIN(392,NPATH))
C
      ELSE IF (ISTAT.EQ.-6) THEN
      I = ICHAR(CPATH(II:II))
      WRITE (MUNIT,970) II
 970  FORMAT (' The pathname contains an invalid character:',
     * /,' Decimal Value:',I4,',  position:',I3)
      WRITE (MUNIT,951) CPATH
C
      ELSE IF (ISTAT.EQ.+6 .AND. MLEVEL.GT.3) THEN
      WRITE (MUNIT,980) NULL
 980  FORMAT (' Caution:',I4,' Characters in the pathname were ',
     * 'converted from null to blank characters.')
C
      ENDIF
      ENDIF
      GO TO 800
C
      END
      SUBROUTINE zchkpn (CPATH, NPATH, ISTAT)
      CHARACTER *(*) CPATH
      call zchkpn6 (CPATH, NPATH, ISTAT)
      return
      end


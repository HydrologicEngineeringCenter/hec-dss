      SUBROUTINE zpath (CA, CB, CC, CD, CE, CF, CPATH, NPATH)
C
      implicit none
C
C     zpath forms a pathname from the input pathname parts.

C
C     CA, CB, CC, CD, CE, CF - Input character strings
C     CPATH -     Pathname character string returned
C     NPATH -     Length of pathname string, also used as an indexer
C
C     Written by John Miwa at HEC, 1988.
C
C
       include 'dss_parameters.h'
C     Declare variables
         CHARACTER CA*(*),CB*(*),CC*(*),CD*(*),CE*(*),CF*(*)
         CHARACTER(len=dss_maxpart) CPART(6)
         CHARACTER CPATH*(*)   
         INTEGER IBEG, IEND, KEND
C
      integer npath, maxlen, i, j, ilen, jchar, jlen
C
C
C     Fill pathname string with blanks
C      call strcpy(CPATH, ' ')
C
C     Fill dummy character array
      call strcpy(CPART(1), CA)
      call strcpy(CPART(2), CB)
      call strcpy(CPART(3), CC)
      call strcpy(CPART(4), CD)
      call strcpy(CPART(5), CE)
      call strcpy(CPART(6), CF)
C
C
      MAXLEN = MIN0(LEN(CPATH),dss_maxpath)
C
      NPATH = 1
      DO 40 I=1,6
C
C     Get the beginning and ending positions of each pathname part
      CALL CHRFLB (CPART(I), IBEG, IEND)
C     Calculate string lengths
      IF (IEND.EQ.0) THEN
      ILEN = 0
      ELSE
      ILEN = IEND - IBEG + 1
C     Check for illegal characters
      DO 10 J=IBEG,IEND
         JCHAR = ICHAR (CPART(I)(J:J))
         IF (JCHAR.LT.32) CPART(I)(J:J) = '?'
         IF (CPART(I)(J:J).EQ.'/') CPART(I)(J:J) = '?'
 10   CONTINUE
      ENDIF
C
C     Place Slash in path
      CPATH (NPATH:NPATH) = '/'
C
      IF ((NPATH+ILEN).LT.MAXLEN) THEN
C
C     Add pathname part to pathname
      IF (ILEN.NE.0) then
        kend = npath + 1 + iend - ibeg + 1
        call strcpy(CPATH(NPATH+1:kend), CPART(I)(IBEG:IEND))
      endif
C
C     Compute new pathname length
      NPATH = NPATH + ILEN + 1
C
      ELSE
C
C     Extreemly rare to execute the following code.
C     If we have reached maximum length, finish slashes
      JLEN = LEN(CPATH)
      IF (JLEN.GT.MAXLEN) JLEN = MAXLEN
      CPATH(NPATH+1:JLEN) = CPART(I)(IBEG:)
C     Add ending slashmarks
      DO 20 J=1,7-I
      CPATH(JLEN:JLEN) = '/'
      JLEN = JLEN - 1
 20   CONTINUE
      NPATH = MAXLEN
      GO TO 60
      END IF
C
 40   CONTINUE
C
C     Add final slashmark
      CPATH(NPATH:NPATH)= '/'
C
 60   CONTINUE
      RETURN
C
      END


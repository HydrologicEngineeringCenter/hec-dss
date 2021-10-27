      SUBROUTINE chrblk(cstring)
C
C     A simple function to deal with 64 bit mixed language
C     compiler issues (C and Fortran)
C
C     Occasionally, a string passed in would have its
C     explicit len, but not implicit.  The statement
C     cstring = ' '
C     would crash the program.  However, the
C     correct LEN is available.
C
C
      implicit none
      CHARACTER cstring*(*)
      integer nstring, i
C
      nstring = LEN(cstring)
C
      if (nstring.gt.0) then
        do 10 i=1,nstring
           cstring(i:i) = ' '
 10     continue
      endif
C
      RETURN
      END


      SUBROUTINE strcpy(cout, cin)
C
C     A simple function to deal with 64 bit mixed language
C     compiler issues (C and Fortran) when calling from jni
C
C     Occasionally, a string passed in would have its
C     explicit len, but not implicit.  The statement
C     cout = cin
C     would crash the program.  However, the
C     correct LEN is available.
C
C
      implicit none
      CHARACTER cout*(*), cin*(*)
      integer nout, nin
C
      nout = LEN(cout)
      nin = LEN(cin)
C
      if ((nout.gt.0).and.(nin.gt.0)) then
         cout(1:nout) = cin(1:nin)
      else
        if (nout.gt.0) then
           cout(1:nout) = ' '
        endif
      endif
C
      RETURN
      END


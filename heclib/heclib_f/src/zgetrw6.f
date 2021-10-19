      SUBROUTINE zgetrw6 (IADD, IREC, IWRD)
C
C
C     Get record IREC, word IWRD from the word address IADD
C
C     Written by Bill Charley at HEC, 1988.
C
      INCLUDE 'zdsskz.h'
C
      IREC = ((IADD - 1) / NBSIZE) + 1
      IWRD = IADD - ((IREC - 1) * NBSIZE)
C
      RETURN
      END


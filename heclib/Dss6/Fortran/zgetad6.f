      SUBROUTINE zgetad6 (IADD, IREC, IWRD)
C
C
C     Get address IADD from record IREC, word IWRD
C
C     Written by Bill Charley at HEC, 1989.
C
      INCLUDE 'zdsskz.h'
C
C
      IADD = ((IREC - 1) * NBSIZE) + IWRD
C
C
      RETURN
      END


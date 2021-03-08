      SUBROUTINE zdebugout7 (IFLTAB, IARRAY, IADD, ILEN)
C
      implicit none
C
C     Prints coded information in IARRAY (usually IFLTAB)
C     IADD is the beginning address
C     ILEN is the length of the array to print.
C
C     Written by Bill Charley at HEC, 2010
C
      integer(8) ifltab(*), iarray(*)
      integer(8) iadd, jadd
      integer ilen
      character cbuff*120
      integer i
C
      INTEGER(4) IVAL4A, IVAL4B
      INTEGER(8) IVAL8
      CHARACTER CDVAL*12, CRVALA*12, CRVALB*12
      CHARACTER CSTRNG*8
      INTEGER IBYTES(8)
C
C
      cbuff = ' Ordinate    Address           Integer-8 '
     * // '  Integer-4a   Integer-4b    '
     * // '   Double       Real-1       Real-2    Character'
      CALL zmessage(ifltab, cbuff)
      cbuff = ' '
C
      do 100 i=1,ilen
         jadd = iadd + i - 1
         call ZDEBUG7 (IARRAY(i), IVAL8, IVAL4A, IVAL4B, CDVAL, CRVALA,
     *                    CRVALB, CSTRNG, IBYTES)
        write (cbuff, 20) i, jadd, IVAL8, IVAL4A, IVAL4B,
     *                    CDVAL, CRVALA, CRVALB, CSTRNG
 20     FORMAT (I6, I14, I20,  2I13, 4A13)
        CALL zmessage(ifltab, cbuff)
 100  continue
C
      RETURN
      END


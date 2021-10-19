      SUBROUTINE zread(IFLTAB, CPATH, NPATH, IHEAD, NHEAD,
     * IDATA, NDATA, IPLAN, LFOUND)
C
      implicit none
C
C
C
      INTEGER IFLTAB(*), NPATH, iversion
      INTEGER IHEAD(*), NHEAD, IDATA(*), NDATA, IPLAN
      CHARACTER CPATH*(*)
      LOGICAL LFOUND
C
      call zGetVersion(IFLTAB, iversion)
      IF (iversion.EQ.6) THEN
          call zread6(IFLTAB, CPATH, NPATH, IHEAD, NHEAD,
     *                IDATA, NDATA, IPLAN, LFOUND)
      else
          call zreada(IFLTAB, CPATH, NPATH, IHEAD, NHEAD,
     *                IDATA, NDATA, IPLAN, LFOUND)
c       void zreada_(long long *ifltab, const char *path, int *npath,
c		int *userHeader, int *userHeaderNumber,
c		int *values, int *valuesNumber,
c		int *readFlag, int *recordFound, int pathLen)
      endif
      return
      end


      SUBROUTINE zrdpn (IUNIT, IPOS, INUMB, CPATH, NPATH)
C
C     Obsolete subroutine.  Use zrdpat6 instead.
C
C
      CHARACTER CPATH*(*), CTAG*8
      LOGICAL LEND
C
C
      CALL zrdpat (IUNIT, IPOS, INUMB, CTAG, CPATH, NPATH, LEND)
      IF (LEND) IPOS = 1000000
      RETURN
C
      END


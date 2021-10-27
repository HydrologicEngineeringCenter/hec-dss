
C     definitions / variables for reading & writing
C     stream ratings

      PARAMETER (MXSHF=320)
      PARAMETER (MXSMSK=10) ! MXSHF/32 (1 BIT/SHIFT)
      PARAMETER (MXVAL=4000)

      INTEGER*4 IATIMS(MXSHF), IATMSK(MXSMSK), IVALS(MXVAL)
      REAL*4 RVALS(MXVAL)

      EQUIVALENCE (IVALS, RVALS)



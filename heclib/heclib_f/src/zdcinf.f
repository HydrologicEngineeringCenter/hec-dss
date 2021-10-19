      SUBROUTINE zdcinf (JCOMP, BASE, LBASE, ISIZE, IDCPRE, ISTAT)
C
C
C     Gets data compression information from last call to zrrtsx6
C
      LOGICAL LBASE, LRPEAT, LDELTA, LSIGDT
C
      INCLUDE 'zdssdc.h'
C
C
      IF (NDCH.GT.0) THEN
      CALL DHINFO (IDCH, NDCH, JCOMP, NREPT, BASE, LBASE, NELMS, ISIZE,
     * IDCPRE, LRPEAT, LDELTA, LSIGDT, ISTAT )
      ELSE
      ISTAT = -1
      ENDIF
C
      RETURN
      END


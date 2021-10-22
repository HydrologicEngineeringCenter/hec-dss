C     ---------------------------------------
C
C     Internal record buffers
      integer mxbuff
      PARAMETER (MXBUFF=6)
      INTEGER IBUFF(NBSIZE,MXBUFF)
	  integer JCREC(MXBUFF), JBUNIT(MXBUFF)
      integer JWRITE(MXBUFF), JBUFF, JMXREC(MXBUFF)
      COMMON /ZDSSBZ/ IBUFF, JCREC, JBUNIT,
     * JWRITE, LSBUFF, LOCKBF, JBUFF,
     * JMXREC
      LOGICAL LSBUFF(MXBUFF), LOCKBF(MXBUFF)
C
C     ---------------------------------------


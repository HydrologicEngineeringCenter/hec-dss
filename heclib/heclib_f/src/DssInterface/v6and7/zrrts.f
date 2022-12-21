      SUBROUTINE zrrts ( IFLTAB, CPATH, CDATE, CTIME, NVALS, SVALUES,
     * CUNITS, CTYPE, IOFSET, ISTAT)
C
C     Short version for retrieving regular interval time series data
C
      implicit none
C
C
      INTEGER IFLTAB(*),zdssVersion
      CHARACTER CPATH*(*), CDATE*(*), CTIME*(*), CUNITS*(*), CTYPE*(*)
      REAL SVALUES(*)
      DOUBLE PRECISION DVALUES(1)
      INTEGER NVALS, NUHEAD, ISTAT, IOFSET, I
      INTEGER JQUAL(1), IUHEAD(1)
      LOGICAL LQUAL
C
      INTEGER KVALS, JCOMP, KUHEAD
      LOGICAL LFILDOB, LQREAD
C
      real COORDS(1)
      LOGICAL LCOORDS
      INTEGER ICDESC(1)
C
C
      LQUAL = .FALSE.
      KUHEAD = 0
      KVALS = NVALS
      LCOORDS = .FALSE.
C
      IF (zdssVersion(IFLTAB).EQ.6) THEN
         CALL zrrtsi6(IFLTAB, CPATH, CDATE, CTIME, KVALS, NVALS,
     *      .FALSE., LFILDOB, SVALUES, DVALUES, JQUAL, LQUAL, LQREAD,
     *      CUNITS, CTYPE, IUHEAD, KUHEAD, NUHEAD, IOFSET, JCOMP,
     *      COORDS, ICDESC, LCOORDS, ISTAT)
      ELSE
	   write(0,*) 'ifltab  = ', ifltab(1)
	   write(0,*) 'cpath   = ', cpath
	   write(0,*) 'cdate   = ', cdate
	   write(0,*) 'ctime   = ', ctime
	   write(0,*) 'kvals   = ', kvals
	   write(0,*) 'lqual   = ', lqual
	   write(0,*) 'kuhead  = ', kuhead
           call zrrtsi7 (IFLTAB, CPATH, CDATE, CTIME, KVALS, NVALS,
     *      .FALSE., LFILDOB, SVALUES, DVALUES, JQUAL, LQUAL, LQREAD,
     *      CUNITS, CTYPE, IUHEAD, KUHEAD, NUHEAD, IOFSET,
     *      JCOMP, COORDS, ICDESC, LCOORDS, ISTAT)
	   write(0,*) 'nvals   = ', nvals
	   write(0,*) 'svalues = ', (svalues(i), i=1,nvals)
	   write(0,*) 'lqread  = ', lqread
	   write(0,*) 'cunits  = ', cunits
	   write(0,*) 'ctype   = ', ctype
	   write(0,*) 'nuhead  = ', nuhead
	   write(0,*) 'lcoords = ', lcoords
	   write(0,*) 'iofset  = ', iofset 
	   write(0,*) 'istat   = ', istat
      ENDIF
      write(0,*) '==> CP2'
C
C
      RETURN
      END


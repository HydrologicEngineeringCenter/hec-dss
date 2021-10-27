      SUBROUTINE znwbin6 (IFLTAB)
C
C
C     Set up a new pathname Bin.  If a new Bin Block is needed,
C     add it to the end of the file
C
C     Written by Bill Charley at HEC, 1988.
C
C
      INTEGER IFLTAB(*)
C
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdsslz.h'
C
      INCLUDE 'zdssbz.h'
C
      INCLUDE 'zdssmz.h'
C
C
C
      IF (MLEVEL.GE.14) WRITE (MUNIT,20) IFLTAB(KBNREM)
 20   FORMAT (T6,'-----DSS---Debug:  Enter znwbin6',/,
     * T12,'Bins Remaining:',I4)
C
C     Record that we are using a new bin
      IFLTAB(KBINS) = IFLTAB(KBINS) + 1
C
C     If we will be adding a new pathname block area, then
C     keep the path bins on record boundaries
C     This will waste a little space, but improve speed significantly
C     E.g., starting at words 1 or 225 or 449 or 673 on the Harris,
C     1 on the PC or other computer where the bin size is the same as
C     the record length.
      IF (IFLTAB(KBNREM).LE.0) THEN
      CALL zptrec6 (IFLTAB, IZERO, 1, IFLTAB(KFSIZE), .FALSE.)
      CALL zgetrw6 (IFLTAB(KFSIZE), IREC, IWRD)
C     I = (IWRD-2) / INT(IFLTAB(KBNSIZ))
C     JWRD = ((I + 1) * INT(IFLTAB(KBNSIZ))) + 1
C     ISIZE = JWRD - IWRD
C     IFLTAB(KFSIZE) = IFLTAB(KFSIZE) + ISIZE
      IF (IWRD.NE.1) THEN
      NREC = IREC + 1
      CALL zgetad6 (IFLTAB(KFSIZE), NREC, 1)
      ENDIF
      ENDIF
C
      IF (LINTAB) THEN
C     Are there any more pathname bins in this block?
      N = IFLTAB(KBNSIZ) - 1
      IF (IFLTAB(KBNREM).LE.0) THEN
C     Write a new pathname block area at the end of the file
C     First, update pointer to next path block
      IPNBIN(N) = IFLTAB(KFSIZE)
      ELSE
      IPNBIN(N) = IFLTAB(KANBIN)
      ENDIF
C
      I = IFLTAB(KBNSIZ)
      CALL zptrec6 (IFLTAB, IPNBIN, I, IPBADD, .TRUE.)
      IFLTAB(KBOVER) = IFLTAB(KBOVER) + 1
      ENDIF
C
      IF (IFLTAB(KBNREM).LE.0) THEN
C     Second, update last bin to point to new block for catalog
      N = IFLTAB(KBNSIZ)
      CALL zgtrec6 (IFLTAB, IPNBIN, N, IFLTAB(KANBIN), .TRUE.)
      IPNBIN(N) = IFLTAB(KFSIZE)
      CALL zptrec6 (IFLTAB, IPNBIN, N, IFLTAB(KANBIN), .TRUE.)
C
C     Set location of pathname block at end of file
      IFLTAB(KBNREM) = IFLTAB (KBNBLK) - 1
      IFLTAB(KANBIN) = IFLTAB(KFSIZE)
C     Store a new pathname block area (zero filled)
      ISIZE = -(IFLTAB(KBNBLK) * IFLTAB(KBNSIZ))
C     (This call keeps this block in memory - for speed only)
      CALL zptrec6 (IFLTAB, IZERO, 1, IFLTAB(KANBIN), .TRUE.)
      CALL zptrec6 (IFLTAB, IZERO, ISIZE, IFLTAB(KANBIN), .FALSE.)
      LSBUFF(JBUFF) = .TRUE.
C     Now update the file size
      IFLTAB(KFSIZE) = IFLTAB(KFSIZE) + IABS(ISIZE)
      IFLTAB(KBSADD) = IFLTAB(KFSIZE)
C
      ELSE
      IFLTAB(KBNREM) = IFLTAB(KBNREM) - 1
      ENDIF
      JPNBIN = 1
      DO 40 I=1,IFLTAB(KBNSIZ)
      IPNBIN(I) = 0
 40   CONTINUE
      IPBADD = IFLTAB(KANBIN)
C     UPDATE THE LOCATION OF THE NEXT PATHNAME BIN
      IF (IFLTAB(KBNREM).GT.0) THEN
      IFLTAB(KANBIN) = IFLTAB(KANBIN) + IFLTAB(KBNSIZ)
      ENDIF
C
      IF (MLEVEL.GE.14) WRITE (MUNIT,100)
 100  FORMAT (T6,'-----DSS---Debug:  Exit  znwbin6')
C
      RETURN
      END


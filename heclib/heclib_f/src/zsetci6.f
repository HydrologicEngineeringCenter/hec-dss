      SUBROUTINE zsetci6 (IFLTAB, CPART, LPART, JCOMP, DBASE,
     * LDBASE, LDHIGH, NPRE, ISTAT)
C
C
C     Set default compression information in a DSS file.
C     This will cause data, based on their pathname parts,
C     to be compressed according to this info, if ICOMP
C     in zsrtsx6 is zero, or zsrts6 is called.  (If it is greater
C     than zero, that scheme is used, less than zero, no
C     compression will take place.
C     To remove a set of compression info, set JCOMP to zero.
C
C     Definition of settings
C        JCOMP:  Compression scheme:
C                   0:  Clear (remove) scheme now set
C                   1:  Repeat scheme
C                   2:  Delta (Difference)
C                   3:  Repeat combined with Delta
C                   4:  Significant Digits
C                   5:  Repeat combined with Sig. Digits
C
C        DBASE, LDBASE, LDHIGH, and NPRE are used only with
C        the Delta shceme (2 or 3).
C        The number of significant digits is hardwired to 3.
C
C        Repeat:  Optimum for storing data that is often the
C                 same value (e.g., precip; not flows)
C        Delta:   Optimum for storing data that has a small
C                 range (e.g., precip or reservoir elevations -
C                 834.000 to 867.000)
C        Significant Digits:  For data like flow values.
C
C        Delta Variables:
C         DBASE, LDBASE:  Allows a minimum base value to be set,
C           for example, if real-time reservoir information is being
C           entered where the absolute minimum is 834.000, set
C         DBASE = 834.000, and LDBASE = .TRUE., otherwise the
C           software would select a base which would be the minimum
C           in the data set.  Additional data may be less than this
C           minimum, and the entire data set would need to be
C           recompressed.  For precip-inc, set DBASE = 0.0.
C         LDHIGH:  If true, forces 2 bytes to be used, otherwise
C           the software would select the lowest (may be 1 byte) of
C           space needed.  If low precip values were being entered,
C           (1 byte) then some high values came in (requiring 2 bytes)
C           the set would need to be recompressed.
C         NPRE:  The exponent of the precision.  Range 6 to -5.
C           For precip to the hundredth, this would be -2.
C           For reservoir elevations to the nearest .001, it is -3.
C
C
C
C     Written by Bill Charley, HEC, August, 1989.
C
      INTEGER IFLTAB(*)
      CHARACTER CPART(6)*(*)
      INTEGER NPART(6), JCOMP, NPRE, ISTAT
      REAL DBASE
      LOGICAL LPART(6), LDBASE, LDHIGH
C
      LOGICAL L
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdssts.h'
C
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdssmz.h'
C
C
      IF (MLEVEL.GE.10) WRITE (MUNIT, 20) JCOMP, IFLTAB(KCOMPN)
 20   FORMAT (T5,'----DSS--- Entering zsetci6;  JCOMP:',I4,
     * ',   Block Length:',I4)
      IOSIZE = IFLTAB(KCOMPN)
      ISTAT = 0
C
C     Lock the file
      CALL zmultu6 (IFLTAB, .TRUE., .TRUE.)
C
      IF (JCOMP.GT.5) GO TO 900
      IF ((JCOMP.EQ.2).OR.(JCOMP.EQ.3)) THEN
      IF ((NPRE.LT.-6).OR.(NPRE.GT.6)) GO TO 910
      ENDIF
C
C     Has the compression information been read yet?
      IF (IFLTAB(KCOMPN).GT.0) THEN
      JSIZE = IFLTAB(KCOMPN)
      CALL zgtrec6(IFLTAB, ILBUFF(1), JSIZE, IFLTAB(KCOMPI), .FALSE.)
C     Compute the actual length of the compression information
      JNEXT = 1
 30   CONTINUE
      NEXT = JNEXT
      JNEXT = ILBUFF(JNEXT)
      IF ((JNEXT.GT.0).AND.(JNEXT.LE.IFLTAB(KCOMPN))) GO TO 30
      IF (NEXT.LT.1000) IFLTAB(KCOMPN) = NEXT - 1
      ELSE
      DO 35 I=1,NCOMP
      ILBUFF(I) = 0
 35   CONTINUE
      ENDIF
C
C     Count the length of the info to store, and that at least one
C     part is set
      JLEN = 0
      NPARTS = 0
C
      DO 40 I=1,6
      IF (LPART(I)) THEN
      CALL CHRLNB (CPART(I), NPART(I))
      JLEN = JLEN + NPART(I) + 2
      NPARTS = NPARTS + 1
      ENDIF
 40   CONTINUE
C
C     Is at least one part set?
      IF (NPARTS.LE.0) THEN
      ISTAT = -1
      GO TO 800
      ENDIF
C
C     See if this call is to remove or modify one that
C     has already been set
      N = -99
      CALL zgetci6 (IFLTAB, CPART, J, B, L, L, N)
C
      IF ((JCOMP.LE.0).AND.(J.EQ.0)) GO TO 920
C
C     If a compression value is postivie, then this set was found
C     Delete this set, then if ICOMP is GT 0, added it to the end
      IF (J.GT.0) THEN
C     IBPOS is the location of the start of this set
      IBPOS = N
C     JNEXT is the position of the next set of info
      JNEXT = ILBUFF(IBPOS)
      IDIFF = JNEXT - IBPOS
C
C     Loop through the rest of the information, copying it over
C     the old info.
C     LOOP
 50   CONTINUE
C     EXIT LOOP IF (JNEXT.GT.IFLTAB(KCOMPN))
C     Are we at the end of the compression info?
      IF (JNEXT.GT.IFLTAB(KCOMPN)) GO TO 80
      JPOS = ILBUFF(JNEXT)
C     EXIT LOOP IF (ILBUFF(JNEXT).EQ.0)
      IF (ILBUFF(JNEXT).EQ.0) GO TO 80
C     IL is the length of this block (to copy)
      IL = JPOS - JNEXT - 1
C     Copy the block
      DO 60 I=1,IL
      ILBUFF(IBPOS+I) = ILBUFF(JNEXT+I)
 60   CONTINUE
C     Compute pointers for next block
      ILBUFF(IBPOS) = IBPOS + IL + 1
      IBPOS        = IBPOS + IL + 1
      JNEXT = JPOS
C     ENDLOOP
      GO TO 50
C
C     Compute new length for compression info
 80   CONTINUE
      DO 90 I=IBPOS,IFLTAB(KCOMPN)
      ILBUFF(I) = 0
 90   CONTINUE
      IFLTAB(KCOMPN) = IFLTAB(KCOMPN) - IDIFF
      ENDIF
C
C     Was this a call just to remove the compression information?
      IF (JCOMP.LE.0) GO TO 700
C
C
C     Compute the length of this set
      JLEN = JLEN + 5
      JLEN = ((JLEN-1)/NCPW) + 1
      NLEN = IFLTAB(KCOMPN) + 2 + JLEN
C
C     Find new starting position in IFLTAB
      JNEXT = ILBUFF(1)
      IF (JNEXT.EQ.0) JNEXT = 1
C     LOOP
 100  CONTINUE
      NEXT = JNEXT
      IF (JNEXT.GT.IFLTAB(KCOMPN)) GO TO 120
      IF (ILBUFF(NEXT).EQ.0) GO TO 120
      JNEXT = ILBUFF(NEXT)
C     ENDLOOP
      GO TO 100
C
 120  CONTINUE
C     Now store related information
      ILBUFF(NEXT) = NLEN + 1
      ILBUFF(NLEN+1) = 0
      IFLTAB(KCOMPN) = ILBUFF(NEXT) - 1
      CALL CHGTYP (DBASE, ILBUFF(NEXT+1))
      CALL PUTHOL (ILBUFF(NEXT+2), 1, JCOMP)
      IBASE = 0
      IF (LDBASE) IBASE = 1
      CALL PUTHOL (ILBUFF(NEXT+2), 2, IBASE)
      IF (LDHIGH) THEN
      NBYTES = 2
      ELSE
      NBYTES = 0
      ENDIF
      CALL PUTHOL (ILBUFF(NEXT+2), 3, NBYTES)
C     (NPRE is offset by 50 so that negative
C     numbers can be stored in one byte)
      CALL PUTHOL (ILBUFF(NEXT+2), 4, NPRE+50)
      CALL PUTHOL (ILBUFF(NEXT+2), 5, NPARTS)
C
      IPOS = 6
C
C     Store the pathname parts
      DO 140 I=1,6
      IF (LPART(I)) THEN
      CALL PUTHOL (ILBUFF(NEXT+2), IPOS, I)
      IPOS = IPOS + 1
      CALL PUTHOL (ILBUFF(NEXT+2), IPOS, NPART(I))
      IPOS = IPOS + 1
      IF (NPART(I).GT.0) CALL CHRHOL (CPART(I), 1, NPART(I),
     * ILBUFF(NEXT+2), IPOS)
      IPOS = IPOS + NPART(I)
      ENDIF
 140  CONTINUE
C
C
 700  CONTINUE
C     Now store this information in the file
      IF (IFLTAB(KCOMPN).GT.0) THEN
      I = ((IFLTAB(KCOMPN) - 1) / NCOMP) + 1
      JSIZE = I * NCOMP
      IF (JSIZE.EQ.IFLTAB(KCOMPN)) JSIZE = (I + 1) * NCOMP
      IFLTAB(KCOMPN) = JSIZE
      IF (JSIZE.GT.IOSIZE) THEN
      IFLTAB(KCOMPI) = IFLTAB(KFSIZE)
      IFLTAB(KBSADD) = IFLTAB(KFSIZE)
      CALL zptrec6(IFLTAB, ILBUFF(1), JSIZE, IFLTAB(KCOMPI), .FALSE.)
      IFLTAB(KFSIZE) = IFLTAB(KFSIZE) + IFLTAB(KCOMPN)
      IFLTAB(KDEAD) = IFLTAB(KDEAD) + IOSIZE
      ELSE
      CALL zptrec6(IFLTAB, ILBUFF(1), JSIZE, IFLTAB(KCOMPI), .FALSE.)
      ENDIF
      ELSE
      IFLTAB(KCOMPI) = 0
      IFLTAB(KCOMPN) = 0
      ENDIF
C
      NADD = 1
      CALL zptrec6 (IFLTAB, IFLTAB(KPERM), NPERM, NADD, .FALSE.)
C
 800  CONTINUE
C     Dump the buffers to disk, and unlock the file
      CALL zmultu6 (IFLTAB, .FALSE., .TRUE.)
C     Force the compression info to be re-read
      IFLTAB(KCOMP) = 0
C
      IF (MLEVEL.GE.10) WRITE (MUNIT, 820) ISTAT, IFLTAB(KCOMPN)
 820  FORMAT (T5,'----DSS--- Exiting zsetci6;  Status:',I4,
     * ',   New length:',I4)
C
      RETURN
C
C
 900  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT, 901) JCOMP
 901  FORMAT (/,' *** ERROR:  zsetci6;  Illegal compression scheme ***',
     * /,' Setting:',I6,';  Min Allowed: 0,  Max: 5')
      ISTAT = 1
      GO TO 800
C
 910  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT, 911) NPRE
 911  FORMAT (/,' *** ERROR:  zsetci6;  Illegal Precision Value ***',
     * /,' Value:',I6,';  Min Allowed: -6,  Max: 6')
      ISTAT = 2
      GO TO 800
C
 920  CONTINUE
      IF (MLEVEL.GE.4) WRITE (MUNIT, 921)
 921  FORMAT (/,' *** WARNING:  zsetci6;  Pathname part secifiers',
     * ' not found ***')
      ISTAT = -1
      GO TO 800
C
      END
      SUBROUTINE zsetci(IFLTAB, CPART, LPART, JCOMP, DBASE,
     * LDBASE, LDHIGH, NPRE, ISTAT)
      INTEGER IFLTAB(*)
      CHARACTER CPART(6)*(*)
      INTEGER NPART(6), JCOMP, NPRE, ISTAT
      REAL DBASE
      LOGICAL LPART(6), LDBASE, LDHIGH
      call zGetVersion(IFLTAB, iversion)
      IF (iversion.EQ.6) THEN
      call zsetci6 (IFLTAB, CPART, LPART, JCOMP, DBASE,
     * LDBASE, LDHIGH, NPRE, ISTAT)
      endif
      return
      end


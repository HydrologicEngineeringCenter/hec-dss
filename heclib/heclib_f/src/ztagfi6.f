      SUBROUTINE ztagfi6 (IFLTAB, IFUN, NRECS, ITAG, JHASH)
C
C
C     When a new catalog is generated, this subroutine writes
C     to the DSS file records of pathname tags and their
C     hash codes.  This allows one to quickly find a
C     pathname given its tag.
C
C     IFUN is the function to preform:
C        -1:  Initialize
C         0:  Insert a tag
C         1:  Completion (tag given)
C         2:  Completion (no tag given)
C
C     Written by Bill Charley at HEC, January 1990.
C
      INTEGER IFLTAB(*), ITAG(*)
      INTEGER IFUN, NRECS, JHASH
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdssts.h'
C
      INCLUDE 'zdssmz.h'
C
C
      SAVE IBPOS, ITGADD, IENDAD, IADD
C
C
C
      IF (MLEVEL.GE.11) WRITE (MUNIT,20) IFUN, JHASH
 20   FORMAT (T6,'-----DSS---Debug:  Enter ztagfi6;  Function:',I3,
     * ',  Hash:',I5)
C
C     Bypass this routine if we are in a read-only state
      IF (IFLTAB(KREADO).EQ.1) GO TO 800
C
C
      IF (IFUN.LT.0) THEN
C
C     For a function of -1, the file is locked by zcatfi6
C     ITGADD is the address of the first block
C     IADD is the address of the current block,
C     JADD points to the next block
C     If an error occurs, set ITGADD to zero, so that a
C     complete new tag-hash table will be built NEXT time
C
      ITGADD = IFLTAB(KTAGBK)
      IADD = ITGADD
      NPABLK = (NTAGBK - 3) / 3
      NBLCKS = ((NRECS - 1) / NPABLK) + 1
C     If a block aready exists, get it and the number of records it has
      IF (IADD.GT.0) THEN
      CALL zgtrec6 (IFLTAB, ILBUFF, NTAGBK, IADD, .FALSE.)
      JBLCKS = ((ILBUFF(2) - 1) / NPABLK) + 1
      JADD = ILBUFF(NTAGBK)
      ELSE
      JBLCKS = 0
      JADD = 0
      ENDIF
C
C     Reserve space at the end of the file for any additional blocks
      IF (NBLCKS.GT.JBLCKS) THEN
      ISIZE = (NBLCKS - JBLCKS) * NTAGBK
      CALL zgetrw6 (IFLTAB(KFSIZE), IREC, IWRD)
C     Adjust the file size so we start on a record boundary.
      IF (IWRD.NE.1) THEN
C     Clear the end of file flag
      CALL zptrec6 (IFLTAB, IZERO, 1, IFLTAB(KFSIZE), .FALSE.)
      IREC = IREC + 1
      IWRD = 1
      CALL zgetad6 (IFLTAB(KFSIZE), IREC, IWRD)
      ENDIF
      IF (MLEVEL.GE.12) WRITE (MUNIT, 30) ISIZE, IFLTAB(KFSIZE)
 30   FORMAT (T12,'ztagfi6:  Allocating hash-tag table size:',I10,
     * '  at location:',I16)
C
      NSIZE = ISIZE
C
      JNXADD = IFLTAB(KFSIZE)
C     New tag-hash table?  If so, store at end of file.
      IF (IADD.EQ.0) THEN
      ITGADD = IFLTAB(KFSIZE)
      IADD = ITGADD
      JNXADD = JNXADD + NTAGBK
      ENDIF
      IFLTAB(KFSIZE) = IFLTAB(KFSIZE) + ISIZE
      ENDIF
C
C     Set the address of the first block to zero, in case the tag-
C     hash table is not completed (i.e., an abort)
      IFLTAB(KTAGBK) = 0
      NADD = 1
      CALL zptrec6 (IFLTAB, IFLTAB(KPERM), NPERM, NADD, .TRUE.)
C
C     Clear the buffer
      DO 40 I=1,NTAGBK
      ILBUFF(I) = 0
 40   CONTINUE
C
C     Store the tag-hash code flag, and the number of records in the
C     file when this catalog was generated
      ILBUFF(1) = JTAGFL
      ILBUFF(2) = NRECS
      IBPOS = 2
C     Save the current file size to be sure we don't write beyond it.
      IENDAD = IFLTAB(KFSIZE) - NTAGBK
C
C
      ELSE IF ((IFUN.EQ.0).OR.(IFUN.EQ.1)) THEN
C
C     Yes.  Save the tag and hash code
      IBPOS = IBPOS + 1
      ILBUFF(IBPOS) = ITAG(1)
      IBPOS = IBPOS + 1
      ILBUFF(IBPOS) = ITAG(2)
      IBPOS = IBPOS + 1
      ILBUFF(IBPOS) = JHASH
C
C
C     Do we need to dump this buffer (and we are not at EOF)?
      IF ((IBPOS.GE.NTAGBK-3).AND.(IFUN.EQ.0)) THEN
C
      IF (MLEVEL.GE.12) WRITE (MUNIT, 50) IADD
 50   FORMAT (T11,'ztagfi6:  Storing tag-hash block at address',I12)
      IF (JADD.GT.0) THEN
      ILBUFF(NTAGBK) = JADD
      ELSE
      ILBUFF(NTAGBK) = JNXADD
      JNXADD = JNXADD + NTAGBK
      ENDIF
C
      CALL zmultu6 (IFLTAB, .TRUE., .TRUE.)
C     Be sure we don't write in space added to the file
      IF (IADD.GT.IENDAD) ITGADD = 0
C     Store the full block
      IF ((IADD.GT.0).AND.(ITGADD.GT.0)) THEN
      CALL zptrec6 (IFLTAB, ILBUFF, NTAGBK, IADD, .FALSE.)
      IADD = ILBUFF(NTAGBK)
      ELSE
      ITGADD = 0
      ENDIF
C
C     If the next block already exists, pick up the
C     address of the subsequent block
      IF (JADD.GT.0) THEN
      IF ((IADD.GT.0).AND.(ITGADD.GT.0)) THEN
      NADD = IADD + NTAGBK - 1
      CALL zgtrec6 (IFLTAB, JADD, 1, NADD, .FALSE.)
      ELSE
      ITGADD = 0
      ENDIF
      ENDIF
C
      CALL zmultu6 (IFLTAB, .FALSE., .TRUE.)
C
      DO 60 I=1,NTAGBK
      ILBUFF(I) = 0
 60   CONTINUE
      ILBUFF(1) = JTAGFL
      IBPOS = 2
C
      ENDIF
      ENDIF
C
C
      IF ((IFUN.EQ.1).OR.(IFUN.EQ.2)) THEN
C     Lock the file, and read the root record
      CALL zmultu6 (IFLTAB, .TRUE., .TRUE.)
C     Is there a partial block that we need to store?
      IF (IBPOS.GT.2) THEN
      IF (IADD.GT.IENDAD) ITGADD = 0
      ILBUFF(NTAGBK) = 0
      IF ((IADD.GT.0).AND.(ITGADD.GT.0)) THEN
      CALL zptrec6 (IFLTAB, ILBUFF, NTAGBK, IADD, .FALSE.)
      ELSE
      ITGADD = 0
      ENDIF
      ENDIF
C     Store the address of the first block
      IFLTAB(KTAGBK) = ITGADD
      NADD = 1
      CALL zptrec6 (IFLTAB, IFLTAB(KPERM), NPERM, NADD, .TRUE.)
      CALL zmultu6 (IFLTAB, .FALSE., .TRUE.)
      ENDIF
C
C
 800  CONTINUE
      IF (MLEVEL.GE.11) WRITE (MUNIT,820)
 820  FORMAT (T6,'-----DSS---Debug:  Exit ztagfi6')
      RETURN
C
      END
      SUBROUTINE ztagfi (IFLTAB, IFUN, NRECS, ITAG, JHASH)
      INTEGER IFLTAB(*), ITAG(*)
      INTEGER IFUN, NRECS, JHASH
      call zGetVersion(IFLTAB, iversion)
      IF (iversion.EQ.6) THEN
      call ztagfi6(IFLTAB, IFUN, NRECS, ITAG, JHASH)
      endif
      return
      end


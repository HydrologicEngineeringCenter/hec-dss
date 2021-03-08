      SUBROUTINE zdbmod6 (IFLTAB, IADD, IVALUE, CVALUE, LUSECH)
C
C
C     Modifies a single word in an HEC-DSS file
C     This is an internal routine to be used for debug
C     purposes only!
C
C     IADD is the address in the file
C     IVALUE is the value to put at that address
C     CVALUE is a character string to store instead, if
C     LUSECH is set to .TRUE.
C
C     Written by Bill Charley at HEC
C
      INTEGER IFLTAB(*), IADD, IVALUE
      CHARACTER CVALUE*(*)
      LOGICAL LUSECH
C
      INCLUDE 'zdssmz.h'
C
      INCLUDE 'zdssts.h'
C
C
C    Clear all buffers to ensure we get what is on disk
      CALL zbdump6 (IFLTAB, 1)
C     Lock the file for multiple user access
      CALL zmultu6 ( IFLTAB, .TRUE., .TRUE.)
      IF (LUSECH) THEN
	   NLEN = LEN(CVALUE)
         NWRDS = ((NLEN - 1) / 4) + 1
         IF ((NLEN.GT.0).AND.(NWRDS.LE.KLBUFF)) THEN
C           Be sure the last word is blank filled
            CALL CH2HOL('    ', ILBUFF(NWRDS), 1)
            CALL CHRHOL(CVALUE, 1, NLEN, ILBUFF, 1)
            CALL zptrec6 (IFLTAB, ILBUFF, NWRDS, IADD, .FALSE.)
	   ENDIF
      ELSE
         CALL zptrec6 (IFLTAB, IVALUE, 1, IADD, .FALSE.)
      ENDIF
      CALL zbdump6 (IFLTAB, 1)
      CALL zmultu6 ( IFLTAB, .FALSE., .TRUE.)
C
      RETURN
C
      END


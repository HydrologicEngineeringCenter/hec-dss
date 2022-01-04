      SUBROUTINE zfpn (CA, NA, CB, NB, CC, NC, CD, ND, CE, NE, CF, NF,
     *                 CPATH, NPATH)
C
      include 'dss_parameters.h'

C     FORM PATH NAME (Obsolete)
C
C     Subroutine zpath should be called instead of this routine
C
C
C         This subroutine takes as input the six pathname parts and
C     their respective lengths and gives as output the formed pathname
C     and its length.  The pathname length gives the maximum length
C     of the part (i.e., dimension).  Leading and trailing blanks
C     are automatically removed.
C
C       Input: CA, CB, CC, CD, CE, CF (--- Pathname parts)
C              NA, NB, NC, ND, NE, NF (--- Pathname part lengths)
C
C       Output: CPATH (--- Pathname)
C               NPATH (--- Pathname length)
C
C
C
C
      CHARACTER CA*(*), CB*(*), CC*(*), CD*(*), CE*(*), CF*(*)
      CHARACTER CPATH*(*)
      CHARACTER(len=dss_maxpart) CPART(6)
      INTEGER ILEN(6)
C
C
      call strcpy(CPART(1), CA)
      call strcpy(CPART(2), CB)
      call strcpy(CPART(3), CC)
      call strcpy(CPART(4), CD)
      call strcpy(CPART(5), CE)
      call strcpy(CPART(6), CF)
C
      ILEN(1) = NA
      ILEN(2) = NB
      ILEN(3) = NC
      ILEN(4) = ND
      ILEN(5) = NE
      ILEN(6) = NF
C
      DO 40 I=1,6
      IF (ILEN(I).GT.dss_maxpath) THEN
      ILEN(I) = dss_maxpart
      ELSE IF (ILEN(I).LE.0) THEN
      ILEN(I) = 1
      CPART(I) = ' '
      ENDIF
 40   CONTINUE
C
      CALL zpath (CPART(1)(1:ILEN(1)), CPART(2)(1:ILEN(2)),
     *            CPART(3)(1:ILEN(3)), CPART(4)(1:ILEN(4)),
     *            CPART(5)(1:ILEN(5)), CPART(6)(1:ILEN(6)),
     *            CPATH, NPATH)
C
      RETURN
      END


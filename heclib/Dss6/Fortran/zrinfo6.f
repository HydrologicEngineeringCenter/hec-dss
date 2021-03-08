      SUBROUTINE zrinfo6 (IFLTAB, CPATH, LFOUND, IDTYPE, CDTYPE, LDOUB,
     *                   LQUAL, IPRECIS, CRTAG, CLWDATE, CLWTIME,
     *                   CPNAME, IVERS, NDATA, NSPACE, ICOMPRES, LPASS)
C
C
C     Get record information
C     Written by Bill Charley at HEC, 2006.
C
C     Input:   IFLTAB  - DSS file table
C              CPATH   - Pathname
C     Output:  LFOUND  - Logical, true if exists
C              IDTYPE  - Integer, record type
C              CDTYPE  - Character, record type description (50 chr)
C              LDOUB   - Logical, double precision
C              LQUAL   - Logical, quality flags stored
C              IPRECIS - Integer, decimal precision of data
C              CRTAG   - Character, pathname tag (8 chr)
C              CLWDATE - Character, last written date (8 chr)
C              CLWTIME - Character, last written time (10 chr)
C              CPNAME  - Character, program name that wrote data (8)
C              IVERS   - Integer, version of data (number time written)
C              NDATA   - Integer, number of data
C              NSPACE  - Integer, space allocated for data
C              ICOMPRES- Integer, compress method (TS only, 0 for none)
C              LPASS   - Logical, password applied to this record
C
C  Record Found:
C  /MUSKINGUM/ZANF5/PRECIP-INC/01JAN1970/12HOUR/OBS/
C  Regular-interval time series; Tag: T14; Precision: 2; Password Applie
C  Last Written on 02JAN85,  at 11:19  by Program:  NONE
C  Version:   8;  Number of Data:   60;  Space Allocated:  20
C  Compressed to 43%
C  Compression Method:  3;  Delta + Repeat
C  Precision:  -2;  Base:  0.000;  Size: 2
C
C  Record Found:
C  /MUSKINGUM/ZANF5/FLOW/01JAN1990/1HOUR/OBS/
C  Regular-interval time series; Tag: T14; Precision: 2; Password Applie
C  Last Written on 02JAN85,  at 11:19  by Program:  NONE
C  Version:   6;  Number of Data:  744;  Space Allocated: 1440
C  Data qualilty flags set
C
C
      INTEGER IFLTAB(*), IDTYPE, IPRECIS, IVERS, NDATA, NSPACE, ICOMPRES
      CHARACTER CPATH*(*)
      CHARACTER CDTYPE*(*), CRTAG*(*), CLWDATE*(*), CLWTIME*(*)
      CHARACTER CPNAME*(*)
      LOGICAL LFOUND, LDOUB, LQUAL, LPASS
C
      CHARACTER CSCRAT*50
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdsslz.h'
C
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdsscz.h'
C
      INCLUDE 'zdssmz.h'
C
      INCLUDE 'zdssShared.h'
C
C
C
      IF (MLEVEL.GE.11) WRITE ( MUNIT, 20) IFLTAB(KUNIT), CPATH
 20   FORMAT (T6,'-----DSS---Debug: Enter zrinfo6',/,T10,
     * 'UNIT =',I5,'  PATH: ',A)
C
C     Initialize variables (mainly if record not found)
      IDTYPE = 0
      call strcpy(CDTYPE, ' ')
      LDOUB = .FALSE.
      IPRECIS = 0
      LQUAL = .FALSE.
      call strcpy(CRTAG, ' ')
      call strcpy(CLWDATE, ' ')
      call strcpy(CLWTIME, ' ')
      call strcpy(CPNAME, ' ')
      IVERS = 0
      NDATA = 0
      NSPACE = 0
      ICOMPRES = 0
      LPASS = .FALSE.
      CSCRAT = ''
C
      CALL zrdinf6 (IFLTAB, CPATH, NUHEAD, NDATA, ISTAT)
      IF (ISTAT.EQ.0) THEN
         LFOUND = .TRUE.
      ELSE
         LFOUND = .FALSE.
         GO TO 800
      ENDIF
C
C
      NIHEAD = INFO(NPPWRD+KINIHE)
      NCHEAD = INFO(NPPWRD+KINCHE)
      NUHEAD = INFO(NPPWRD+KINUHE)
      NDATA = INFO(NPPWRD+KILNDA)
      NSPACE = INFO(NPPWRD+KINDAT)
      IVERS = INFO(NPPWRD+KIVER)
      IPRECIS = INFO(NPPWRD+KIPREC)
      IF (INFO(NPPWRD+KIPASS).GT.0) LPASS = .TRUE.
      IF (INFO(NPPWRD+KIQUAL).GT.0) LQUAL = .TRUE.
      ICOMPRES = INFO(NPPWRD+KICOMP)
      IDTYPE = INFO(NPPWRD+KITYPE)
      IF (IDTYPE.EQ.105) LDOUB = .TRUE.
      IF (IDTYPE.EQ.115) LDOUB = .TRUE.
      IF (IDTYPE.EQ.205) LDOUB = .TRUE.
C
C     Get type description
      DO 110 I=1,NRTYPE
         IF (IDTYPE.EQ.IRTYPE(I)) THEN
            NT = I
            GO TO 120
         ENDIF
 110  CONTINUE
      NT = NRTYPE
 120  CONTINUE
      call strcpy(CDTYPE, CRDESC(NT))
C
      CSCRAT = ' '
      CALL HOLCHR (INFO(NPPWRD+KITAG), 1, NTAGC, CSCRAT, 1)
      call strcpy(CRTAG, CSCRAT)
      CSCRAT = ' '
      CALL HOLCHR (INFO(NPPWRD+KIDATE), 1, NDATEC, CSCRAT, 1)
      call strcpy(CLWDATE, CSCRAT)
      CSCRAT = ' '
      CALL HOLCHR (INFO(NPPWRD+KITIME), 1, NTIMEC, CSCRAT, 1)
      call strcpy(CLWTIME, CSCRAT)
      CSCRAT = ' '
      CALL HOLCHR (INFO(NPPWRD+KIPROG), 1, NPROGC, CSCRAT, 1)
      call strcpy(CPNAME, CSCRAT)
C
C
C
 800  CONTINUE
      IF (MLEVEL.GE.11) WRITE ( MUNIT,820)
 820  FORMAT (T6,'-----DSS---Debug: Exit  zrinfo6')
      RETURN
C
      END


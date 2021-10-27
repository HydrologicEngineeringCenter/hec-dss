      SUBROUTINE zrinfo7 (IFLTAB, CPATH, LFOUND, IDTYPE, CDTYPE, LDOUB,
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
      implicit none
C
      INCLUDE 'zdssShared.h'
C
      INTEGER IFLTAB(*), IDTYPE, IPRECIS, IVERS, NDATA, NSPACE, ICOMPRES
      CHARACTER CPATH*(*)
      CHARACTER CDTYPE*(*), CRTAG*(*), CLWDATE*(*), CLWTIME*(*)
      CHARACTER CPNAME*(*)
      LOGICAL LFOUND, LDOUB, LQUAL, LPASS
C
      CHARACTER CSCRAT*50
      INTEGER ISTAT, I, NT, JUL, N, IHR, ISEC, ISECS, IMIN
      integer NPATH, NHEAD
C
      INTEGER INFOSTUFF(14)
C      logical zmessageLevel
C
C
C
      !IF (zmessageLevel(ifltab, 11)) THEN
      !  CALL zmessage2D (ifltab, 'Enter zrinfo7;  Pathname: ', CPATH)
      !ENDIF
C
C     Initialize variables (mainly if record not found)
      IDTYPE = 0
      call strcpy(CDTYPE, '')
      LDOUB = .FALSE.
      IPRECIS = 0
      LQUAL = .FALSE.
      call strcpy(CRTAG, '')
      call strcpy(CLWDATE, '')
      call strcpy(CLWTIME, '')
      call strcpy(CPNAME, '')
      IVERS = 0
      NDATA = 0
      NSPACE = 0
      ICOMPRES = 0
      LPASS = .FALSE.
      CSCRAT = ''
C
      CALL zgetInfo7 (IFLTAB, CPATH, INFOSTUFF, ISTAT)
      IF (ISTAT.EQ.0) THEN
         LFOUND = .TRUE.
      ELSE
         LFOUND = .FALSE.
         GO TO 800
      ENDIF
C
C
C	ibuff[1] = data type
C	ibuff[2] = version number (number of writes)
C	ibuff[3] = expansion number (number of time expanded)
C	ibuff[4] = expansion flag
C	ibuff[5] = compression
C	ibuff[6] = precision
C	ibuff[7] = last written date in julian (since 1900)
C	ibuff[8] = last written time in seconds past midnight
C	ibuff[9,10,11,12] = last write program (16 characters long)
C	ibuff[13,14] = record password (8 characters long)
C
C
      CALL CHRLNB(CPATH, NPATH)
      call zcheck7 ( IFLTAB, CPATH, NPATH, NHEAD, NDATA, LFOUND)
      NSPACE = NDATA

      IVERS = INFOSTUFF(2)
      IPRECIS = INFOSTUFF(6)

      IDTYPE = INFOSTUFF(1)
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
      IF (INFOSTUFF(13).GT.0) LPASS = .TRUE.
C
      call strcpy(CLWDATE, ' ')
      JUL = INFOSTUFF(7)
      CALL JULDAT(JUL, 114, CLWDATE, N)

      CSCRAT = ' '
      ISECS = INFOSTUFF(8)
      IHR = ISECS/3600
      IMIN = (ISECS - (IHR * 3600)) / 60
      ISEC = ISECS - (IHR * 3600) - (IMIN * 60)
      WRITE (CSCRAT,30) IHR,IMIN,ISEC
 30   FORMAT (I2.2,':',I2.2,':',I2.2)
      call strcpy(CLWTIME, CSCRAT)

      CSCRAT = ' '
      CALL HOLCHR (INFOSTUFF(9), 1, 16, CSCRAT, 1)
      call strcpy(CPNAME, CSCRAT)
C
C
C
 800  CONTINUE
      !IF (zmessageLevel(ifltab, 11)) THEN
      !  CALL zmessageD (ifltab, 'Exit zrinfo7')
      !ENDIF
      RETURN
C
      END


#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
//
//
//     Get record information
//     Written by Bill Charley at HEC, 2006.
//
//     Input:   IFLTAB  - DSS file table
//              CPATH   - Pathname
//     Output:  LFOUND  - Logical, true if exists
//              IDTYPE  - Integer, record type
//              CDTYPE  - Character, record type description (50 chr)
//              LDOUB   - Logical, double precision
//              LQUAL   - Logical, quality flags stored
//              IPRECIS - Integer, decimal precision of data
//              CRTAG   - Character, pathname tag (8 chr)
//              CLWDATE - Character, last written date (8 chr)
//              CLWTIME - Character, last written time (10 chr)
//              CPNAME  - Character, program name that wrote data (8)
//              IVERS   - Integer, version of data (number time written)
//              NDATA   - Integer, number of data
//              NSPACE  - Integer, space allocated for data
//              ICOMPRES- Integer, compress method (TS only, 0 for none)
//              LPASS   - Logical, password applied to this record
//
//  Record Found:
//  /MUSKINGUM/ZANF5/PRECIP-INC/01JAN1970/12HOUR/OBS/
//  Regular-interval time series; Tag: T14; Precision: 2; Password Applie
//  Last Written on 02JAN85,  at 11:19  by Program:  NONE
//  Version:   8;  Number of Data:   60;  Space Allocated:  20
//  Compressed to 43%
//  Compression Method:  3;  Delta + Repeat
//  Precision:  -2;  Base:  0.000;  Size: 2
//
//  Record Found:
//  /MUSKINGUM/ZANF5/FLOW/01JAN1990/1HOUR/OBS/
//  Regular-interval time series; Tag: T14; Precision: 2; Password Applie
//  Last Written on 02JAN85,  at 11:19  by Program:  NONE
//  Version:   6;  Number of Data:  744;  Space Allocated: 1440
//  Data qualilty flags set
//
      
//
void zrinfo7 (long long* IFLTAB, const char* CPATH, char* LFOUND, int64_t* IDTYPE, char* CDTYPE, bool *LDOUB,
                       bool* LQUAL, int64_t* IPRECIS, char* CRTAG, char* CLWDATE, char* CLWTIME,
                       char* CPNAME, int64_t* IVERS, int64_t* NDATA, int64_t* NSPACE, int64_t* ICOMPRES, bool *LPASS,
                       size_t crtag_len, size_t clwdate_len, size_t clwtime_len, size_t cpname_len) {
      
        
    char CSCRAT[51];
      
    int64_t ISTAT, I, NT, JUL, N, IHR, ISEC, ISECS, IMIN;
    int64_t NPATH, NHEAD;

    int64_t INFOSTUFF[14];

    //     Initialize variables (mainly if record not found)
    *IDTYPE = 0;
    strcpy(CDTYPE, "");
    *LDOUB = false;
    *IPRECIS = 0;
    *LQUAL = false;
    strncpy(CRTAG, "", crtag_len);
    strncpy(CLWDATE, "", clwdate_len);
    strncpy(CLWTIME, "", clwtime_len);
    strncpy(CPNAME, "", cpname_len);
    IVERS = 0;
    *NDATA = 0;
    *NSPACE = 0;
    *ICOMPRES = 0;
    *LPASS = false;
    memcpy(CSCRAT, 0, 51);

    int status = zgetInfo7(IFLTAB, CPATH, INFOSTUFF);
    if (status == 0 ) {
         *LFOUND = true;
    } else {
        LFOUND = false;
        return;
    }      
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
      zcheck7 ( IFLTAB, CPATH, NPATH, NHEAD, NDATA, LFOUND)
      *NSPACE = *NDATA

      IVERS = INFOSTUFF(2)
      IPRECIS = INFOSTUFF(6)

      IDTYPE = INFOSTUFF(1)
      switch (IDTYPE) {
        case 105:
        case 115:
        case 205: {
            *LDOUB = true;
            break;
        }
        default: {
            *LDBOU = false;
        }
      }
      

     //Get type description
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
}

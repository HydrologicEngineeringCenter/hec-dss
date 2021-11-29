      SUBROUTINE zset6 ( CFLG, CSTR, NUMB)
C
      INCLUDE 'zopenDefault.h'
C
C     SET VARIABLES FOR DSS COMMON BLOCKS
C
C     Written by Bill Charley at HEC, 1982.
C
      CHARACTER CFLG*(*), CSTR*(*), CFLAG*4, CSTRIN*16, UCSTRIN*16
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdsslz.h'
C
      INCLUDE 'zdsscz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdssmz.h'
C
      INCLUDE 'zdsscm.h'
C
      INCLUDE 'zdssts.h'
C
      INCLUDE 'verticalDatumFortran.h'
C
      COMMON /ZSTATUS/ TOTAL_NUMB,  CURRENT_NUMB,
     *                 INTERRUPT, NERROR, MAXERROR
      INTEGER TOTAL_NUMB,  CURRENT_NUMB,  INTERRUPT
      INTEGER NERROR, MAXERROR
C
      COMMON /ZDSSFZ/ LFIRST
      LOGICAL LFIRST
C
      COMMON /DCDBUG/ LDCDBG, MDCUNT
      LOGICAL LDCDBG
C
C
C
      IF (LFIRST) THEN
      !return
      CALL zinit6
      LFIRST = .FALSE.
      ENDIF
C
C
      CFLAG = CFLG
      CALL UPCASE(CFLAG)
      CSTRIN = CSTR
C
      IF (MLEVEL.GE.15) WRITE (MUNIT,20) CFLAG, NUMB, CSTR
 20   FORMAT (T2,'-----DSS---Debug:  Enter zset6;  Flag: ',A,
     * ',  Numb:',I9,',  String: ',A)
C
C
      IF (CFLAG.EQ.'DSSV') THEN
        versionNext = INUMB
        return
      ELSE IF (CFLAG.EQ.'ALLV') THEN
        !  AllVersions
        versionAll = INUMB
        return
      ENDIF
C
C
C     Set Program Name
      IF ((CFLAG .EQ. 'PRGN').OR.(CFLAG.EQ.'PROG')) THEN
      CPROG = CSTR
C
C     Set trace level (10 and above is an internal trace)
      ELSE IF ((CFLAG .EQ. 'MLEV').OR.(CFLAG.EQ.'MLVL')) THEN
      MLEVEL = NUMB
      IF (MLEVEL.GT.15) MLEVEL = 15
      IF (MLEVEL.LT.0) MLEVEL = 0
C
C     Set the message unit
      ELSE IF ((CFLAG .EQ. 'MUNI').OR.(CFLAG.EQ.'MLFN')) THEN
      IF ((NUMB.LE.0).OR.(NUMB.GE.200)) GO TO 900
      MUNIT = NUMB
C
      ELSE IF (CFLAG.EQ.'TOTA') THEN
      TOTAL_NUMB = NUMB
C
      ELSE IF (CFLAG.EQ.'CURR') THEN
      CURRENT_NUMB = NUMB
C
      ELSE IF (CFLAG.EQ.'NERR') THEN
      NERROR = NUMB
C
      ELSE IF (CFLAG.EQ.'MAXE') THEN
      MAXERROR = NUMB
C
      ELSE IF (CFLAG.EQ.'INTE') THEN
      INTERRUPT = NUMB
C
C     Set the data's precision
      ELSE IF (CFLAG.EQ. 'PREC') THEN
      IF (CSTRIN(1:3).EQ.'OFF') THEN
      IPREC = 0
      ELSE
      IPREC = NUMB
      ENDIF
C
C     Set a tolerance for replacing data
      ELSE IF (CFLAG(1:3).EQ. 'TOL') THEN
      IF (CSTRIN(1:3).EQ.'OFF') THEN
      LTOL = .FALSE.
      ELSE
      IF ((NUMB.LT.0).OR.(NUMB.GT.8)) GO TO 900
      TOL = 0.6 / (10.0**NUMB)
      LTOL = .TRUE.
      ENDIF
C
C     Set the unit number to use for the next file to open
      ELSE IF ((CFLAG .EQ. 'UNIT').OR.(CFLAG.EQ.'FILN')) THEN
      IF ((NUMB.LE.0).OR.(NUMB.GE.200)) GO TO 900
      IDUNIT = NUMB
      LDUNIT = .TRUE.
C
C     Turn the catalog map option on
      ELSE IF (CFLAG.EQ.'MAP ') THEN
      IF (CSTRIN.EQ.'ON') THEN
      LMAP = .TRUE.
      IF (NUMB.GT.0) MAPUNT = NUMB
      ELSE
      LMAP = .FALSE.
      ENDIF
C
C     Should we exclude missing periods from the condensed catalog?
      ELSE IF (CFLAG.EQ.'CCDA') THEN
      IF (CSTRIN.EQ.'ON') THEN
      LCCDAT = .TRUE.
      ELSE
      LCCDAT = .FALSE.
      ENDIF
C
C     Set the unit number to write the catalog map to
      ELSE IF (CFLAG.EQ.'MAPU') THEN
      MAPUNT = NUMB
C
C     Set the data type
      ELSE IF (CFLAG.EQ.'TYPE') THEN
      ITYPE = NUMB
C
C     Set the output line length to 80 columns
      ELSE IF (CFLAG.EQ.'80CO') THEN
      IF (CSTRIN.EQ.'ON') THEN
      L80COL = .TRUE.
      ELSE
      L80COL = .FALSE.
      ENDIF
C
C     Set this file in an exclusive assign mode (Harris only)
      ELSE IF (CFLAG.EQ.'EXCL') THEN
      LEXCL = .TRUE.
C
C     Force this file to a multi-user mode (don't go to advisory mode!)
      ELSE IF (CFLAG.EQ.'MULT') THEN
      LMULTU = .TRUE.
C
C     Set this file in an (exclusive) write lock mode
      ELSE IF (CFLAG.EQ.'WLOC') THEN
C ***** TEMP DISABLE OF WRITE LOCK!!! *****
C   (NEED TO USE CRTN ON AN ABORT)
C     LWLOCK = .TRUE.
      LEXCL = .TRUE.
C
C     Set the file creation date (for squeeze only)
      ELSE IF (CFLAG.EQ.'FDAT') THEN
      CFDATE = CSTR
      LFDATE = .TRUE.
C
C     Set the estimated size of the file (e.g., LARGE or 40000)
C     to use for next call to zopen6
      ELSE IF (CFLAG.EQ.'SIZE') THEN
      CSIZE = ' '
      CSIZE = CSTR
      NSIZE = NUMB
      IHSIZE = 0
      LSZSET = .TRUE.
      IF ((CSIZE(1:1).EQ.' ').AND.(NSIZE.LT.0)) LSZSET = .FALSE.
C
C     Set the hash table size code, if squeezing the file
      ELSE IF (CFLAG.EQ.'HSIZ') THEN
      IF ((NUMB.GT.0).AND.(NUMB.LT.9)) THEN
      IHSIZE = NUMB
      LSZSET = .FALSE.
      ENDIF
C
C     Set the file to use a Dynamic or Stable Hash table
      ELSE IF (CFLAG.EQ.'TABL') THEN
      IF (CSTRIN(1:1).EQ.'S') THEN
      LSTABL = .TRUE.
      ELSE
      LSTABL = .FALSE.
      ENDIF
C
C     Set the tag for the next record to be written
      ELSE IF (CFLAG.EQ.'TAG ') THEN
      CTAG = CSTR
C
C     Set the version number for the next record to be written
      ELSE IF (CFLAG.EQ.'RVER') THEN
      IBVER = NUMB
C
C     Set the date for the next record to be written
      ELSE IF (CFLAG.EQ.'RDAT') THEN
      CDATE = CSTR
C
C     Set the time for the next record to be written
      ELSE IF (CFLAG.EQ.'RTIM') THEN
      CTIME = CSTR
C
C     Set the time zone offset & name for the next record to be written
      ELSE IF (CFLAG.EQ.'ZONE') THEN
      IWTZONE = NUMB
C     The time zone identifier can contain up to 12 characters
C     Examples include 'PST', 'GMT+0800'
      CWTZONE = CSTR
C
C     Is the quality flag set on?
      ELSE IF ((CFLAG.EQ.'QUAL').OR.(CFLAG.EQ.'FLAG')) THEN
      IQUAL = NUMB
C
C     Set the time window flag for the catalog on or off
      ELSE IF (CFLAG.EQ.'TWCA') THEN
      IF (CSTRIN.EQ.'ON') THEN
      LTWCAT = .TRUE.
      ELSE
      LTWCAT = .FALSE.
      ENDIF
C
C     Set the Protect mode on (don't write over existing records)
      ELSE IF (CFLAG.EQ.'PROT') THEN
      IF (CSTRIN.EQ.'ON') THEN
      LPROTC = .TRUE.
      ELSE
      LPROTC = .FALSE.
      ENDIF
C
C     Set the Read Access Only mode on
      ELSE IF (CFLAG.EQ.'READ') THEN
      IF (CSTRIN.EQ.'ON') THEN
      LREADO = .TRUE.
      ELSE
      LREADO = .FALSE.
      ENDIF
C
C     Compress time-series data when copying records, or
C     Squeezing the file?
      ELSE IF (CFLAG.EQ.'COMP') THEN
      IF (CSTRIN.EQ.'ON') THEN
      LTSCMP = .TRUE.
      ELSE
      LTSCMP = .FALSE.
      ENDIF
C
C     Should status be shown when squeezing the file?
      ELSE IF (CFLAG.EQ.'CAST') THEN
      IF (CSTRIN.EQ.'ON') THEN
      LCATST = .TRUE.
      ELSE
      LCATST = .FALSE.
      ENDIF
C
C     Should status be shown when squeezing the file?
      ELSE IF (CFLAG.EQ.'SQST') THEN
      IF (CSTRIN.EQ.'ON') THEN
      LSQSTA = .TRUE.
      ELSE
      LSQSTA = .FALSE.
      ENDIF
C
C     Should data compression debug be set on?
      ELSE IF (CFLAG.EQ.'DCDB') THEN
      IF (CSTRIN.EQ.'ON') THEN
      LDCDBG = .TRUE.
      IF ((NUMB.GT.0).AND.(NUMB.LT.100)) MDCUNT = NUMB
      ELSE
      LDCDBG = .FALSE.
      ENDIF
C
C
C     Should we abort on a fatal error?
C     (Better know what we are doing - can ruin a recoverable file!)
      ELSE IF (CFLAG.EQ.'ABOR') THEN
      IF (CSTRIN(1:3).EQ.'OFF') THEN
      LNOABT = .TRUE.
      ELSE
      LNOABT = .FALSE.
      ENDIF
C
C
C     Allow the writing of Long Pathnames
C     Max 392 characters and 64 character per part
C     (default of 80 char pathnames and 32 char parts)
C     6 x 64 + 7 slashes = 391 + 1 for "c" null string ender = 392
      ELSE IF (CFLAG.EQ.'LONG') THEN
         CALL UPCASE (CSTRIN)
         IF ((CSTRIN(1:3).EQ.'ON').OR.(CSTRIN(1:3).EQ.'YES').OR.
     *       (CSTRIN(1:3).EQ.'TRU')) THEN
            MXPATH = 392
            MAXPART = 64
         ELSE
            MXPATH = 80
            MAXPART = 32
         ENDIF
C
C
C     Should the record time be updated at write time instead of the
C     time that the file was opened (only for programs that run a
C     very long time - e.g., days!)
      ELSE IF (CFLAG.EQ.'UPRT') THEN
      IF (CSTRIN.EQ.'ON') THEN
         LUPRTIME = .TRUE.
      ELSE
         LUPRTIME = .FALSE.
      ENDIF
C
C
C     Should we observe the Quality Protect bit in flags
C     when storing time series data
      ELSE IF (CFLAG.EQ.'QPRO') THEN
      IF (CSTRIN.EQ.'ON') THEN
         LQPBIT = .TRUE.
      ELSE
         LQPBIT = .FALSE.
      ENDIF
C
C     Should we show the handle instead of the unit number
      ELSE IF (CFLAG.EQ.'HAND') THEN
      IF (CSTRIN.EQ.'ON') THEN
         LSHOWHANDL = .TRUE.
      ELSE
         LSHOWHANDL = .FALSE.
      ENDIF
C
C     Disallow large file size (8 GB) limit (for compatibilty with
C     programs that have not been relinked with DSS 6-QA and later?
      ELSE IF (CFLAG.EQ.'8GB ') THEN
        IF (CSTRIN.EQ.'OFF') THEN
           L8GB = .FALSE.
        ELSE
           L8GB = .TRUE.
        ENDIF
C
C     Set a pseudo regular interval time series interval
C     This is where we have irregular interval data, but view
C     it as if it were almost regular
      ELSE IF (CFLAG.EQ.'PSEU') THEN
        IF (CSTRIN.EQ.'OFF') THEN
           LPSEUDO = .FALSE.
        ELSE IF (CSTRIN.EQ.'ON') THEN
           LPSEUDO = .TRUE.
        ELSE
           INTL_PSEUDO = NUMB
        ENDIF
C
C     Copy empty (all missing) records during a squeeze or copy file?
      ELSE IF (CFLAG.EQ.'EMPT') THEN
        IF (CSTRIN.EQ.'OFF') THEN
           LCPEMPTY = .FALSE.
        ELSE
           LCPEMPTY = .TRUE.
        ENDIF
C
C
C     Force a squeeze when called, even though might not be needed?
      ELSE IF (CFLAG.EQ.'SQUE') THEN
        IF (CSTRIN.EQ.'ON') THEN
           LFSQUEEZE = .TRUE.
        ELSE
           LFSQUEEZE = .FALSE.
        ENDIF
C
C     Set default vertical datum
      ELSE IF (CFLAG.EQ.'VDTM') THEN
        UCSTRIN = CSTRIN
        CALL UPCASE(UCSTRIN)
        write(*,*) 'In zset6: setting ',cflag,' to ',ucstrin
        IF (UCSTRIN.EQ.'') THEN
           IF (NUMB.EQ.IVD_UNSET) THEN
              CVDATUM = CVD_UNSET
              IVDATUM = IVD_UNSET
           ELSE IF (NUMB.EQ.IVD_NAVD88) THEN
              CVDATUM = CVD_NAVD88
              IVDATUM = IVD_NAVD88
           ELSE IF (NUMB.EQ.IVD_NGVD29) THEN
              CVDATUM = CVD_NGVD29
              IVDATUM = IVD_NGVD29
           ELSE IF (NUMB.EQ.IVD_OTHER) THEN
              CVDATUM = CVD_OTHER
              IVDATUM = IVD_OTHER
           ELSE
              ! NOP - BAD INPUT NUMBER
              CVDATUM = CVDATUM
              IVDATUM = IVDATUM
           ENDIF   
        ELSE IF (UCSTRIN.EQ.CVD_UNSET) THEN
           CVDATUM = CVD_UNSET
           IVDATUM = IVD_UNSET
        ELSE IF (UCSTRIN.EQ.CVD_NAVD88) THEN
           CVDATUM = CVD_NAVD88
           IVDATUM = IVD_NAVD88
        ELSE IF (UCSTRIN.EQ.CVD_NGVD29) THEN
           CVDATUM = CVD_NGVD29
           IVDATUM = IVD_NGVD29
        ELSE IF (UCSTRIN.EQ.CVD_OTHER) THEN
           CVDATUM = CVD_OTHER
           IVDATUM = IVD_OTHER
        ELSE
           CVDATUM = CSTRIN
           IVDATUM = IVD_OTHER
        ENDIF
C
C
      ELSE
      GO TO 900
      ENDIF
C
      RETURN
C
C
 900  CONTINUE
C     TEMP REMOVAL OF ERROR WRITE FOR USE WITH ZSET4.  BILL CHARLEY
c      IF (MLEVEL.GE.2) WRITE (MUNIT,910) CFLAG, CSTR, NUMB
c 910  FORMAT (T5,'-----DSS *** zset6 ERROR: Illegal Flag; ',
c     * 'Flag: ',A,'  String: ',A,'  Integer:',I8)
      RETURN
C
      END


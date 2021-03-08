      SUBROUTINE zsetca6 (CINSTR, LSELCA)
C
C
C     Set Selective Catalog Capabilities from CINSTR
C     This routine set parameters needed to match pathnames
C     when subroutine ZMATCA is called
C
C     Written by Bill Charley at HEC, 1990
C
C
      CHARACTER CTEMP*20, CINSTR*(*)
      INTEGER ISTATS(6)
      INTEGER FUNCTION INTGR
      LOGICAL LSELCA
      INTEGER MAXF, i, j, ierr, nfield, ipos, ilast, n
      PARAMETER (MAXF=20)
      INTEGER IBF(MAXF), IEF(MAXF), ILF(MAXF)
C
C
      INCLUDE 'zdssca.h'
C
      INCLUDE 'zdsscc.h'
C
      INCLUDE 'zdssmz.h'
C
C
C
      IF (MLEVEL.GE.12) WRITE (MUNIT,20) CINSTR
 20   FORMAT (T8,'-----DSS---Debug:  Enter zsetca6, Instructions: ',A)
C
      LSELCA = .FALSE.
      LSPROG = .FALSE.
      ILWFLG = 0
C     Clear the part Array, and limit sort up to 20 characters/part
      DO 40 I=1,6
      CPART(I) = ' '
      JPART(1,I) = 1
      ISTATS(I) = 0
 40   CONTINUE
C
C     Look for Parts Specified
      CALL CHRLNB (CINSTR, ILAST)
      IF (ILAST.EQ.0) GO TO 800
      CALL zgpnp (CINSTR(1:ILAST), CPART(1), CPART(2), CPART(3),
     * CPART(4), CPART(5), CPART(6), ISTATS)
C
C     If ISTATS(1) = -10, no reference to a selective catalog was made
      IF (ISTATS(1).EQ.-10) GO TO 200
      LSELCA = .TRUE.
C
C     Check for a D part with a date reference (e.g., D=M-2M)
C     This is used exclusive for time-series data
      IF ((ISTATS(4).GT.0).AND.(ISTATS(4).LE.6)) THEN
          CALL zcatdr6 (CPART(4), N)
          IF (N.GT.0) THEN
              ISTATS(4) = N
              IF (MLEVEL.GE.3) WRITE (MUNIT,60) CPART(4)(1:N)
 60           FORMAT(' -----DSS--- zcatlg6: Date reference set to D=',A)
          ENDIF
        ENDIF

      IF ((ISTATS(5).GT.0).AND.(ISTATS(5).LE.10)) THEN
        IF (INDEX(CPART(5), 'MIN').GT.-1) THEN
            NUMB = INTGR ( CPART(5), 1, 1, IERR)
            IF (IERR.EQ.0) THEN
C              Looks like a time interval
                CPART(5) = CPART(5)(1:4) // '@'
            ENDIF
        ENDIF
      ENDIF
C
C
C
C     Store information on part comparison and part length
C     JPART(1,I) = The type of part comparison:
C     JPART(1,I)= 1 - Do not compare this part
C               = 2 - NAME - Parts must be identical
C               = 3 - NAME@ - Part must start with these characters
C               = 4 - @NAME - Part must end with these characters
C               = 5 - @NAME@ - Part must contain this line segment
C               = 6 - #NAME - Part must not be idnetical
C               = 7 - #NAME@ - Part must not start with these characters
C               = 8 - #@NAME - Part must not end with these characters
C               = 9 - #@NAME@ - Part must not contain this line segment
C     JPART(2,I) = The lenght of the part to compare
C
      DO 100 I=1,6
      IF (ISTATS(I).LT.0) THEN
C     Do NOT compare this part
      JPART(1,I) = 1
      ELSE IF (ISTATS(I).EQ.0) THEN
      JPART(1,I) = 2
      JPART(2,I) = 0
C
      ELSE
C
      IPOS = ISTATS(I)
      CTEMP = ' '
C
      IF (CPART(I)(1:1).NE.'#') THEN
C
C         @NAME@
      IF (CPART(I)(IPOS:IPOS).EQ.'@') THEN
      IF (CPART(I)(1:1).EQ.'@') THEN
      JPART(1,I) = 5
      JPART(2,I) = ISTATS(I) - 2
      IF (JPART(2,I).LE.0) THEN
      JPART(1,I) = 1
      GO TO 100
      ENDIF
      CTEMP = CPART(I)(2:JPART(2,I)+1)
      CPART(I) = CTEMP
      GO TO 100
C
C          NAME@
      ELSE
      JPART(1,I) = 3
      JPART(2,I) = ISTATS(I) - 1
      GO TO 100
      ENDIF
C
C         @NAME
      ELSE
      IF (CPART(I)(1:1).EQ.'@') THEN
      JPART(1,I) = 4
      JPART(2,I) = ISTATS(I) - 1
      CTEMP = CPART(I)(2:JPART(2,I)+1)
      CPART(I) = CTEMP
      GO TO 100
C
C          NAME
      ELSE
      JPART(1,I) = 2
      JPART(2,I) = ISTATS(I)
      GO TO 100
      ENDIF
      ENDIF
C
C     Negtation of the above Parameters
C
C         #@NAME@
      ELSE
C
      IF (CPART(I)(IPOS:IPOS).EQ.'@') THEN
      IF (CPART(I)(2:2).EQ.'@') THEN
      JPART(1,I) = 9
      JPART(2,I) = ISTATS(I) - 3
      IF (JPART(2,I).LE.0) THEN
      JPART(1,I) = 1
      GO TO 100
      ENDIF
      CTEMP = CPART(I)(3:JPART(2,I)+2)
      CPART(I) = CTEMP
      GO TO 100
C
C         #NAME@
      ELSE
      JPART(1,I) = 7
      JPART(2,I) = ISTATS(I) - 2
      CTEMP = CPART(I)(2:JPART(2,I)+1)
      CPART(I) = CTEMP
      GO TO 100
      ENDIF
C
C         #@NAME
      ELSE
      IF (CPART(I)(2:2).EQ.'@') THEN
      JPART(1,I) = 8
      JPART(2,I) = ISTATS(I) - 2
      CTEMP = CPART(I)(3:JPART(2,I)+2)
      CPART(I) = CTEMP
      GO TO 100
C
C         #NAME
      ELSE
      JPART(1,I) = 6
      JPART(2,I) = ISTATS(I) - 1
      CTEMP = CPART(I)(2:JPART(2,I)+1)
      CPART(I) = CTEMP
      ENDIF
      ENDIF
      ENDIF
      ENDIF
C
 100  CONTINUE
C
 200  CONTINUE
C     Is this a reference to the last written date of the record,
C     or the progrm that wrote it?
      CALL PARSLI (CINSTR(1:ILAST), MAXF, NFIELD, IBF, IEF, ILF)
C
      DO 220 I=1,NFIELD
C
      IF (ILF(I).LE.1) GO TO 220
      IF (CINSTR(IBF(I):IBF(I)+1).EQ.'LW') THEN
C     Found a LW= type reference
      J = IBF(I) + 3
      IF (J.GT.IEF(I)) J = IEF(I)
C     Get the date corresponding to that
      CALL DATJUL (CINSTR(J:IEF(I)), JULLW, IERR)
      IF (IERR.NE.0) go to 900
      J = IBF(I) + 2
C     Is the reference less than, equal to, or greater than?
      IF (CINSTR(J:J).EQ.'<') THEN
      ILWFLG = 1
      ELSE IF (CINSTR(J:J).EQ.'=') THEN
      ILWFLG = 2
      ELSE IF (CINSTR(J:J).EQ.'>') THEN
      ILWFLG = 3
      ELSE
      GO TO 900
      ENDIF
      LSELCA = .TRUE.
C
C     The reference was to the program that last wrote the record.
      ELSE IF (CINSTR(IBF(I):IBF(I)+1).EQ.'PR') THEN
      J = INDEX (CINSTR(IBF(I):IEF(I)),'=')
      IF (J.GT.0) THEN
      LSPROG = .TRUE.
      LSELCA = .TRUE.
      J = IBF(I) + J
      IF (J.LE.IEF(I)) THEN
      CSPROG = CINSTR(J:IEF(I))
      ELSE
      GO TO 910
      ENDIF
      ENDIF
      ENDIF
C
 220  CONTINUE
C
C     Check for a scan of a string in the path (index)
      IPOS = INDEX (CINSTR, 'S=')
      IF (IPOS.NE.0) THEN
         CSINDEX = CINSTR(IPOS+2:)
         CALL CHRLNB (CSINDEX, NSINDEX)
      ENDIF
C
C
 800  CONTINUE
      IF (MLEVEL.GE.12) WRITE (MUNIT,820) LSELCA
 820  FORMAT (T8,'-----DSS---Debug:  Exit zsetca6, LSELCA:',L2)
      RETURN
C
 900  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,901) CINSTR(1:ILAST)
 901  FORMAT (' -----DSS*** zcat6:  Unrecognized Last Write Date',
     * /,' Catalog Instructions: ',A)
      ILWFLG = -1
      GO TO 800
C
 910  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,911) CINSTR(1:ILAST)
 911  FORMAT (' -----DSS*** zcat6:  Unrecognized Program Reference',
     * /,' Catalog Instructions: ',A)
      ILWFLG = -1
      GO TO 800
C
      END
      SUBROUTINE zsetca (CINSTR, LSELCA)
      CHARACTER CINSTR*(*)
      LOGICAL LSELCA
      call zsetca6(CINSTR, LSELCA)
      return
      end


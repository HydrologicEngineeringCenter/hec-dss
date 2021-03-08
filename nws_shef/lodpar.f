      SUBROUTINE LODPAR ( CPFILE,LFNOUT )
C
C     <LODPAR> READS INFORMATION LINKING SHEF PHYSICAL ELEMENTS <PE> TO
C     APPROPRIATE DSS PARAMTER INFORMATION <PARAM>,
C     AND <TYPE>.
C
      CHARACTER*(*) CPFILE
C
CADD C.PSTBLS                                                           H
      INCLUDE 'pstbls.h'                                                MLlg
C
C     LOCAL VARIABLES
      CHARACTER LINE*80
      LOGICAL LPOPT
      DATA LFNIN/40/
C
      CALL TASG( LFNOUT,LFNIN,CPFILE )
C
      NPAR = 0
   10 CONTINUE
      READ ( LFNIN,'(A)',IOSTAT=IOS ) LINE
C     WRITE ( LFNOUT,* ) LINE
      IF ( IOS.EQ.0 ) THEN
         IF ( LINE(1:1).EQ.'*') THEN
            GO TO 10
         ELSE IF ( LINE(1:2).EQ.'  ' ) THEN
            GO TO 10
         ELSE
	      CALL CHRLNB(LINE, NLINE)
            NPAR = NPAR + 1
            PE(NPAR)=LINE(1:2)
            PARAM(NPAR) = LINE(4:28)
            IF ( PARAM(NPAR).EQ.' ' ) THEN
               WRITE ( LFNOUT,* ) 'PARAMETER BLANK -- ENTRY IGNORED'
               NPAR = NPAR - 1
               GO TO 10
            ENDIF
            UNITS(NPAR) = LINE(30:37)
            IF ( UNITS(NPAR).EQ.' ') THEN
               UNITS(NPAR) = 'UNK'
               WRITE ( LFNOUT,* ) 'BLANK UNITS : ',LINE
            ENDIF
            TYPE(NPAR) = LINE(39:46)
            IF ( TYPE(NPAR).NE.'INST-VAL'.AND.
     1           TYPE(NPAR).NE.'INST-CUM'.AND.
     2           TYPE(NPAR)(1:7).NE.'PER-AVE' .AND.
     3           TYPE(NPAR).NE.'PER-CUM'       ) THEN
                   TYPE(NPAR) = 'UNK'
                   WRITE ( LFNOUT,* ) 'TYPE NOT RECOGNIZED : ',LINE
            ENDIF
	      IF (NLINE.GE.48) THEN
               FACTOR(NPAR) = XREAL ( LINE,48,10,ISTAT )
               IF ( ISTAT.NE.0 ) THEN
                  FACTOR(NPAR) = 1.
                  WRITE ( LFNOUT,* ) 'INVALID FACTOR : ',LINE
                  WRITE ( LFNOUT,* ) 'FACTOR ASSUMED = 1'
               ENDIF
	      ELSE
	         FACTOR(NPAR) = 1.0
	      ENDIF
            IF ( FACTOR(NPAR).LT.1.0E-6 ) FACTOR(NPAR)=1.
            IF ( LINE(62:62).EQ.' ' ) THEN
               NSIG(NPAR) = -1
            ELSE
               NSIG(NPAR) = INTGR ( LINE,62,1,ISTAT )
               IF ( ISTAT.NE.0 ) THEN
                  NSIG(NPAR) = 8
                  WRITE ( LFNOUT,* ) 'INVALID NSIG : ',LINE
                  WRITE ( LFNOUT,* ) 'NSIG ASSUMED = 8'
               ENDIF
            ENDIF
            IF ( LINE(66:67).EQ.'  ') THEN
               IRND(NPAR) = -999
            ELSE
               IRND(NPAR) = INTGR ( LINE,66,2,ISTAT )
               IF ( ISTAT.NE.0 ) THEN
                  IRND(NPAR) = -8
                  WRITE ( LFNOUT,* ) 'INVALID IRND : ',LINE
                  WRITE ( LFNOUT,* ) 'IRND ASSUMED = -8'
               ENDIF
           ENDIF
            IF ( NPAR.LT.MAXPAR ) THEN
               GO TO 10
            ELSE
               WRITE ( LFNOUT,* ) 'MAX PARAMS LOADED'
            ENDIF
         ENDIF
      ELSE IF ( IOS.GT.0 ) THEN
         WRITE ( LFNOUT,* )'ERROR ',IOS,' IN READING SHFDSSP '
      ELSE
      ENDIF
      CLOSE (UNIT=LFNIN)
C      IF ( LPOPT('E')) THEN
C         DO 20 I=1,NPAR
C            WRITE(LFNOUT,30) I,PE(I),PARAM(I),UNITS(I),TYPE(I),
C     .                       FACTOR(I),NSIG(I),IRND(I)
C   30       FORMAT ( I3,4(1X,A),F12.2,2I6 )
C   20    CONTINUE
C      ENDIF
C
      RETURN
C
      END

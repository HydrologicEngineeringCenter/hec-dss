      SUBROUTINE LODSEN( CSFILE,ISTAT )
C
C      <LODSEN>  READS INFORMATION ABOUT FIELD SENSORS OF INTEREST WHICH
C      IS NECESSARY FOR FORMATING AND LABELING DATA FROM THOSE SENSORS
C      INTO DSS.  <SENSOR> IS A LIST OF SENSOR LABELS FORMED BY A
C      CONCATENATION OF THE SHEF STATION IDENTIFIER AND PHYSICAL
C      ELEMENT CODE.  EACH ENTRY IN SENSOR HAS, OPTIONALLY, A
C      CORRESPONDING REGULAR TIME INTERVAL OF REPORTING AND DSS
C      PATHNAME LABEL PARTS <GROUP> AND <LOCATION> FOR PARTS
C      A AND B,RESPECTIVELY.
C                   DENNIS HUFF  1 MARCH 85
C
C        ADDED OPTIONAL PATHNAME F PART (CFPRT) AND PASSWORD (UID)
C        FOR EACH SENSOR.  ADDED CODE TO CHECK AND CORRECT IMPROPER
C        ORDER.  THE NEW CODE IS IN SUBROUTINE ADDSEN
C                     DENNIS HUFF 11 FEB 88
C
C
      CHARACTER CSFILE*(*)
C
CADD C.SDCNTL                                                           H
      INCLUDE 'sdcntl.h'                                                MLlg
CADD C.SENSRS                                                           H
      INCLUDE 'sensrs.h'                                                MLlg
C
C     LOCAL VARIABLES
      CHARACTER LINE*80,CRLF*2
      LOGICAL LPOPT
      DATA LFNIN/40/
C
      ISTAT = 0
      OPEN ( UNIT=LFNIN,FILE=CSFILE,IOSTAT=IOS )
         IF ( IOS.NE.0 ) THEN
            WRITE ( LFNOUT,* ) 'ERROR IN ASSIGNING STATION FILE '
            IF ( IOS.EQ.53 ) THEN
               WRITE ( LFNOUT,* ) 'FILE ACCESS DENIED'
            ELSE IF ( IOS.EQ.77 ) THEN
               WRITE ( LFNOUT,* ) 'OTHER USER WRITING TO FILE'
            ELSE
               WRITE ( LFNOUT,* ) 'SEE ERROR MESSAGE NO ',IOS+4500
            ENDIF
            ISTAT = -1
            GO TO 900
         ENDIF
      CRLF = CHAR(13)//CHAR(10)
      NSEN = 0
      MSG = 0
   10 CONTINUE
      READ ( LFNIN,'(A)',IOSTAT=IOS ) LINE
C     WRITE ( LFNOUT,* ) LINE
      IF ( IOS.EQ.0 ) THEN
         IF ( LINE(1:1).EQ.'*') THEN
            GO TO 10
         ELSE IF ( LINE(1:10).EQ.' ' ) THEN
            GO TO 10
         ELSE
            IF ( LINE(12:15).EQ.'    ' ) THEN
               TINTL = 0
            ELSE
               TINTL = INTGR ( LINE,12,3,ISTAT )
               IF ( ISTAT.NE.0 ) THEN
                  WRITE ( LFNOUT,* ) 'THE FOLLOWING LINE CONTAINED AN '
     1            ,'ERROR IN THE INTERVAL SPECIFICATION',CRLF,LINE,CRLF,
     2            'TABLE ENTRY NOT MADE'
                  GO TO 10
               ENDIF
c
c     write (21,*) 'lodsen1 - tintl: ', tintl
c
               IF ( LINE(15:15).EQ.'M' ) THEN
               ELSE IF ( LINE(15:15).EQ.'H' ) THEN
                        TINTL = 60 * TINTL
               ELSE IF ( LINE(15:15).EQ.'D' ) THEN
                  TINTL = 1440 * TINTL
               ELSE IF ( LINE(15:15).EQ.'L' ) THEN
                  TINTL = 43200 * TINTL
               ELSE IF ( LINE(15:15).EQ.'Y' ) THEN
                  TINTL = 525600 * TINTL
               ELSE
                     WRITE ( LFNOUT,* )'INTERVAL NOT RECOGNIZED IN THE '
     1                ,'FOLLOWING LINE ',CRLF,LINE,CRLF,' TABLE ENTRY',
     2                      ' NOT MADE'
                     GO TO 10
               ENDIF
            ENDIF
c
c     write (21,*) 'lodsen2 - tintl: ', tintl
c
            CALL ADDSEN ( LINE,TINTL,ISTAT)
            IF ( ISTAT.LT.0 ) THEN
C               WRITE ( LFNOUT,* ) ' ** WARNING : SENSOR OUT OF ORDER'
C               WRITE ( LFNOUT,* ) LINE
            ENDIF
            IF ( NSEN.LT.MAXSEN ) THEN
               GO TO 10
            ELSE
               WRITE ( LFNOUT,* ) 'MAX SENSORS LOADED'
            ENDIF
         ENDIF
      ELSE IF ( IOS.GT.0 ) THEN
         WRITE ( LFNOUT,* )'ERROR ',IOS,' IN READING SHFDSSS '
         ISTAT = -1
      ELSE
      ENDIF
      CLOSE (UNIT=LFNIN)
C      IF ( LPOPT('E') ) THEN
C         DO 20 I=1,NSEN
C            WRITE(LFNOUT,30) I,SENSOR(I),SINTL(I),GROUP(I),LOCAT(I),
C     1                      CFPRT(I),UID(I)
C   30       FORMAT ( I6,1X,A10,1X,I8,1X,A,1X,A,1X,A,1X,A )
C   20    CONTINUE
C      ENDIF
C
  900 CONTINUE
      RETURN
C
      END

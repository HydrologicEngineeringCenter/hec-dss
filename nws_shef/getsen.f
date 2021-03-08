      SUBROUTINE GETSEN ( KEY,ISEN,idura)
C
C     <GETSEN> FINDS THE POINTER <ISEN> TO A SENSOR LABEL <SENSOR>
C     WHICH MATCHES THE STRING <KEY>.
C     THE METHOD IS BINARY SEARCH ( ALSO CALLED BRACKETING )---
C     THE LIST OF SENSORS IS ASSUMED TO BE IN ALPHABETICAL ORDER.
C     THE NUMBER OF SENSORS IN THE LIST IS <NSEN>.
C     THE POINTER <ISEN> IS RETURNED AS 0 (ZERO) IF
C     <KEY> DOES NOT MATCH ANY ENTRY IN THE SENSOR LIST.
C                   DENNIS HUFF    MARCH 1 1085
C
      CHARACTER KEY*(*)
C
CADD C.SENSRS                                                           H
      INCLUDE 'sensrs.h'                                                MLlg
c     open(unit=95,file='debug.out')
c     write(95,*)'key,idura'
c     write(95,'(A,1x,i5)')key,idura
C
C     WRITE ( 21,* ) 'SEARCHING FOR MATCH FOR ***',KEY,'***'
c        write (*,*) 'key:     ', key
c        write (*,*) 'sensor1: ', sensor(1)
c        write (*,*) 'sensor2: ', sensor(nsen)
      IF (LLT(KEY,SENSOR(1)) .OR. LGT(KEY,SENSOR(NSEN)) ) THEN
         ISEN = 0
         GO TO 90
      ENDIF
      ILO=1
      IHI=NSEN
      ILAST = 0
 10   CONTINUE
         ISEN=(IHI+ILO)/2
c        write (*,*) 'sensor: ', sensor(isen)
C        WRITE(21,* )CHAR(10),CHAR(11),'ILO =',ILO,
C    1    ' ISEN = ', ISEN,' IHI = ',IHI
            IF ( LLT(KEY,SENSOR(ISEN))) THEN
               IHI = ISEN
            ELSE IF ( LGT(KEY,SENSOR(ISEN))) THEN
               ILO = ISEN
            ELSE
c     check to see if the sensor has a duration specified
              if(idurac(isen).ge.0.) then
c              write(95,*)'isen,idurac(isen)',isen,idurac(isen)
               ilo=isen-5
               if(ilo.lt.1)ilo=1
               do 15 i=ilo,nsen
c               write(95,*)'i,sensor(i),key'
c               write(95,'(i3,1x,a,1x,a)')i,sensor(i),key
                if(sensor(i).ne.key) go to 15
c                write(95,*)'i,idurac(i),idura',i,idurac(i),idura
                if(idurac(i).lt.0) then
c                write(6,*)' Sensor File Error Encountered'
c                write(6,*)' Sensor ',sensor(i)
c                write(6,*)' Does not have a duration specified '
c                write(6,*)' Fatal Error - Aborting '
                 RETURN
                else
                 if(idurac(i).eq.idura) then
                   isen=i
                   go to 90
                 endif
                endif
 15            continue
               isen=0
              endif
              GO TO 90
            ENDIF
            IF ( ILAST.NE.ISEN ) THEN
               ILAST = ISEN
               GO TO 10
            ELSE IF ( ILO.LT.IHI.AND.IHI.EQ.NSEN ) THEN
               ILO = IHI
               ILAST = ISEN
               GO TO 10
            ELSE IF ( IHI.GT.ILO.AND.ILO.EQ.1 ) THEN
               IHI = ILO
               ILAST = ISEN
               GO TO 10
            ELSE
               ISEN = 0
            ENDIF
  90  CONTINUE
C
      RETURN
C
      END

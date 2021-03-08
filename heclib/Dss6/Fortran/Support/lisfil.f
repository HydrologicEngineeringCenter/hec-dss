      FUNCTION LISFIL (STR)
C
C     Determine if a string is valid filename.
C     Uses HARRIS filename syntax.
C
      CHARACTER STR*(*)
      LOGICAL LISFIL
C
C
      INQUIRE (FILE=STR,EXIST=LISFIL,ERR=1800)                          MLu
      LISFIL = .TRUE.                                                   MLu
      GO TO 1900                                                        MLu
C
C     CHARACTER C                                                       H
C     CHARACTER CHLIST*79                                               H
C
C     DATA CHLIST /'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ!$%&()+-./:;<=>?H
C    1@[\]_~ '/                                                         H
C
C ======================================================================
C
C     --- Check string length
C
C     CALL CHRLNB(STR,NC)                                               H
C     IF (NC .GT. 17 .OR. NC.LE.0) THEN                                 H
C        GO TO 1800                                                     H
C     ENDIF                                                             H
C
C     --- Look for '*'
C     IQ = INDEX (STR(1:NC), '*')                                       H
C     IF ( IQ .GT. 9 ) THEN                                             H
C        GO TO 1800                                                     H
C     ENDIF                                                             H
C     IF ( IQ .GT. 1 ) THEN                                             H
C        --- Check Qualifier
C        --- Account may have up to 4 numerals.
C        DO 1100 I = 1, 4                                               H
C           C = STR(I:I)                                                H
C           IF ( C .LT. '0' .OR. C .GT. '9' ) THEN                      H
C              J = I                                                    H
C              GO TO 1120                                               H
C           ENDIF                                                       H
C1100    CONTINUE                                                       H
C        J = 5                                                          H
C
C1120    CONTINUE                                                       H
C        --- First character of qualifier is a letter.
C        --- Remaining characters may be letters or numerals.
C        C = STR(J:J)                                                   H
C        IF ( C .GE. 'A' .AND. C .LE. 'Z' ) THEN                        H
C           DO 1140 I = J+1, IQ-1                                       H
C              C = STR(I:I)                                             H
C              IF ( (C .LT. '0' .OR. C .GT. '9') .AND.                  H
C    +              (C .LT. 'A' .OR. C .GT. 'Z') ) THEN                 H
C                 GO TO 1800                                            H
C              ENDIF                                                    H
C1140       CONTINUE                                                    H
C        ELSE                                                           H
C           GO TO 1800                                                  H
C        ENDIF                                                          H
C     ENDIF                                                             H
C     --- Check area name.
C     --- First character is a letter.
C     J = IQ + 1                                                        H
C     C = STR(J:J)                                                      H
C     IF ( C .GE. 'A' .AND. C .LE. 'Z' ) THEN                           H
C        DO 1200 I = J, NC                                              H
C           C = STR(I:I)                                                H
C           IF ( INDEX (CHLIST, C) .LE. 0 ) THEN                        H
C              GO TO 1800                                               H
C           ENDIF                                                       H
C1200    CONTINUE                                                       H
C        LISFIL = .TRUE.                                                H
C        GO TO 1900                                                     H
C     ENDIF                                                             H
C
 1800 LISFIL = .FALSE.
C
 1900 RETURN
      END


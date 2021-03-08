C *****************************************************************************
C     DATA BUFFERS AND BUFFER POINTERS
      PARAMETER (MAXDAT=5000,MAXHD=10 )
      COMMON / SDDATA / JDATE(MAXDAT),MTIME(MAXDAT),DATA(MAXDAT),
     1   NXTDAT(MAXDAT),NDATA,IFIRST(MAXHD),ILAST(MAXHD),NHEAD,
     1   INTLE(MAXHD),HEAD,DQ
      CHARACTER HEAD(MAXHD)*18,DQ(MAXDAT)*1
C     INTEGER   JDATE, INTLE                                            H
      INTEGER*4 JDATE, INTLE                                            M
C
C        HEAD    ---  THE CHARACTER ARRAY WHICH UNIQUELY IDENTIFIES
C                     DESCRIBES THE ASSOCIATED DATA
C                     VALUES WITH SHEF OR SHEF-DERIVED PARAMETERS.
C
C                     HEAD CHAR.       CONTENTS
C                     =========        ========
C                     1-8              STATION ID.
C                     9-10             SHEF PE CODE
C                     11               SHEF EXTREMA CODE
C                     12-15            SHEF DURATION CODE
C                     16               SHEF TYPE CODE
C                     17               SHEF SOURCE CODE
C                     18               REVISION FLAG
C
C        NHEAD   ---   THE NUMBER OF UNIQUE DATA SEQUENCES
C                      RETURNED IN ONE PASS FROM GETSHF
C        JDATE   ---  OBSERVATION DATES
C        MTIME   ---  OBSERVATION TIMES
C        DATA    ---  OBSERVATION VALUES
C        DQ      ---  DATA QUALITY AND QUALIFIER CODES
C        NXTDAT  ---  ARRAY CONTAINING POINTERS TO NEXT DATA INDEX
C                     IN ASCENDING TEMPORAL ORDER
C        NDATA   ---  NUMBER OF DATA VALUES RETURNED
C        IFIRST  ---  POINTS TO FIRST DATA ELEMENT IN DATA
C                     SEQUENCE
C        ILAST   ---  POINTS TO LAST  DATA ELEMENT IN DATA
C                     SEQUENCE
C
C

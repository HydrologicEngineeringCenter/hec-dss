      SUBROUTINE CPARMS ( C , M )
C
C------ RETURN ARGUMENTS ON COMMAND LINE AND ITS LENGTH
C
C     USE DFLIB

      CHARACTER C*(*), CT*132
      integer :: length, status
      K = LEN ( C )
      M = 1
#ifdef __sun__      
      N = IARGC (  )
#else
      N = command_argument_count ()                               
#endif
      IF ( N .GE. 1 ) THEN
      DO 10 I = 1, N
      CT = ' '

#ifdef __sun__
      CALL GETARG ( I, CT )     
#else
      call GET_COMMAND_ARGUMENT(I,CT,length,status)
C      CALL GETARG ( I, CT,KK)                                          
      if (status .ne. 0 ) GO TO 50         
      if( length == 0 ) goto 50
#endif
      CALL CHRLNB ( CT, J )
      IF ( M + J + 1 .LE. K ) THEN
      C(M:) = CT
      M = M + J + 1
      ELSE
      GO TO 50
      ENDIF
   10 CONTINUE
      ENDIF
C------
   50 CONTINUE
      M = M - 2
      M = MAX0 ( M, 0 )
      RETURN
      END


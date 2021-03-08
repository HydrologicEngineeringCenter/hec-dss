      SUBROUTINE LOADPARAM (LFNOUT, CPFILE, CSFILE)
C   
      CHARACTER CPFILE*(*), CSFILE*(*)
C
C
C ******************************************************************************
C LOAD PARAMETER AND STATION TABLES
C ******************************************************************************
         CALL LODPAR ( CPFILE,LFNOUT )
         CALL LODSTA ( CSFILE,LFNOUT )
	RETURN
	END
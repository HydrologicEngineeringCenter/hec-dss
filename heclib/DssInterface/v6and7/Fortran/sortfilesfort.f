      SUBROUTINE sortfilesfort (fileIn, fileOut, status)
      CHARACTER fileIn*(*)
	CHARACTER fileOut*(*)
	INTEGER status
      CALL sortfilesinterface (fileIn, fileOut, status)
      RETURN
      END


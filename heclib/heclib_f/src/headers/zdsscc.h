      include 'dss_parameters.h'
C
C     Catalog Character information
      COMMON /ZDSSCC/ CPART, CPPATH, CSPROG, CSINDEX
      CHARACTER(len=dss_maxpart) CPART(6)
      CHARACTER(len=dss_maxpath) CPPATH
      CHARACTER CSPROG*6, CSINDEX*100
C
C     ---------------------------------------


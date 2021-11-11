#include "stdio.h"
#include "string.h"
#include "math.h"
#include "heclib.h"

int test_stringCat()
{
    char mess[500];
    int status;
    char *proj ="PROJCS[\"USA_Contiguous_Albers_Equal_Area_Conic_USGS_version\",GEOGCS[\"GCS_North_American_1983\",DATUM[\"D_North_American_1983\",SPHEROID[\"GRS_1980\",6378137.0,298.257222101]],PRIMEM[\"Greenwich\",0.0],UNIT[\"Degree\",0.0174532925199433]],PROJECTION[\"Albers\"],PARAMETER[\"False_Easting\",0.0],PARAMETER[\"False_Northing\",0.0],PARAMETER[\"Central_Meridian\",-96.0],PARAMETER[\"Standard_Parallel_1\",29.5],PARAMETER[\"Standard_Parallel_2\",45.5],PARAMETER[\"Latitude_Of_Origin\",23.0],UNIT[\"Meter\",1.0]]";
    int proj_len = (int)strlen(proj);

    getCurrentTimeString(mess, sizeof(mess));
    printf("\n'%s'",mess);
    status = stringCat(mess,sizeof(mess),"",0);

    status = stringCat(mess,sizeof(mess),proj, proj_len);
    printf("\nstatus=%d len=%d",status,(int)strlen(mess));
    // attempt to overflow
    char* forty_numbers = "0123456789012345678901234567890123456789";
    status = stringCat(mess,sizeof(mess),forty_numbers,40 );
    printf("\nstatus=%d len=%d",status,(int)strlen(mess));
    if( strlen(mess)> 499)
    {
        printf("\nError: string overflow in %s",__FILE__);
        return -1;
    }
    status = stringCat(mess,sizeof(mess),NULL,0);
    printf("\ntry null. status=%d",status);
    if( status == 0)
    {
     printf("\nError null string not detected in %s",__FILE__);
    return -1;
    }
    


return 0;
}



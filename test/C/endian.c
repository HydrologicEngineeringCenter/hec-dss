#include <stdio.h> 
#include "heclib.h"
int main()  
{ 
    int bigEndian=0;

	printf("\nyour system is: ");
    bigEndian  =bigEndian();
   if (bigEndian)     
       printf("Big endian\n"); 
   else
       printf("Little endian\n"); 
  
   int *ip;
   int i;
   float *fp;
   float f;
   long l;
   long long ll;
   long long *llp;
   void *vp;
   char c;
   char *cp;

   printf("\nData types and sizes");
   printf("\nint        %zd", sizeof(i));
   printf("\nint*       %zd", sizeof(ip));
   printf("\nfloat      %zd", sizeof(f));
   printf("\nfloat*     %zd", sizeof(fp));
   printf("\nlong       %zd", sizeof(l));
   printf("\nlong long  %zd", sizeof(ll));
   printf("\nlong long* %zd", sizeof(llp));
   printf("\nvoid*      %zd", sizeof(vp));
   printf("\nchar       %zd", sizeof(c));
   printf("\nchar*      %zd", sizeof(cp));
   printf("\n");

   return 0; 
} 



#include <stdio.h>



 

int interop_char_(const char *c ,size_t len)
{
      int i;
    printf("\n length of char %ld\n", len);

    for( i=0; i<len; i++)
      printf("%c",c[i]);
    printf("\n");


} 


int interop_char_array_(const char *c ,size_t len)
{
    int i;
        printf("\n length of char  array %ld\n", len);

    for( i=0; i<len; i++)
      printf("%c",c[i]);
    printf("\n\n");


} 

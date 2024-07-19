#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "TestDssC.h"

int checknumbers(int numberOrig, int numberRead, const char *mess) {
      if (numberOrig != numberRead) {

      
        printf("\n");
        printf("\n");
        printf("*****  Number read does not match that written *****\n");
        
        printf("Number Written: %d, Read: %d",numberOrig, numberRead);
        printf("%s", mess);
        printf("\n");
        printf("\n");
        return -1;
    }
    return 0;
}

void checknumbers_(int *numberOrig, int *numberRead, const char* mess, int *status, size_t mess_len) {
    *status = checknumbers(*numberOrig,*numberRead,mess);
}

int checkstring(const char* orig, const char* read, const char* mess) {
    int status = 0;
    size_t orig_len = strlen(orig);
    size_t read_len = strlen(orig);
    if (orig_len != read_len) {
        status = -1;
    }
    else if (strcmp(orig,read) == 0) {
        status = 0;
    }

    // case insensitive compare;
    for (int i = 0; i < orig_len; i++) {
        if (toupper(orig[i]) != toupper(read[i])) {
            status = -1;
            break;
        }
    }
    if (status != 0) {
        printf("\n");
        printf("\n");
        printf("***  String read does not match that written *****");
        printf("String Written: ==>%s<== Read: ==>%s<==", orig, read);
        printf("\n");
        printf("\n");
    }
    return status;
}
     
void checkstring_(const char* orig, const char* read, const char* msg, int * status, size_t orig_len, size_t read_length, size_t mess_len) {
    *status = checkstring(orig, read, msg);
}   
     
int checkdoubles(double* orig, double* read, int numberRows, const char* mess) {
    
    for(int i = 0; i < numberRows; i++) {
        if (orig[i] != read[i]) {
            printf("\n");
            printf("\n");
            printf("*****  Data read does not match those written *****");
            printf("At ordinate: %d Written: %f Read: %f", i, orig[i], read[i]);
            printf("\n");
            printf("\n");
            return -1;
        }
    }    
}

void checkdoubles_(double *orig, double* read, int* numberRows, const char* mess, int *status, size_t mess_length) {
    *status = checkdoubles(orig, read, *numberRows, mess);
}

int checkfloats(float* orig, float* read, int numberRows, const char* mess) {
    
    for(int i = 0; i < numberRows; i++) {
        if (orig[i] != read[i]) {
            printf("\n");
            printf("\n");
            printf("*****  Data read does not match those written *****");
            printf("At ordinate: %d Written: %f Read: %f", i, orig[i], read[i]);
            printf("\n");
            printf("\n");
            return -1;
        }
    }    
}

void checkfloats_(float *orig, float* read, int* numberRows, const char* mess, int *status, size_t mess_length) {
    *status = checkfloats(orig, read, *numberRows, mess);
}


int checkints(int* orig, int* read, int numberRows, const char* mess) {
    
    for(int i = 0; i < numberRows; i++) {
        if (orig[i] != read[i]) {
            printf("\n");
            printf("\n");
            printf("*****  Data read does not match those written *****");
            printf("At ordinate: %d Written: %d Read: %d", i, orig[i], read[i]);
            printf("\n");
            printf("\n");
            return -1;
        }
    }    
}

void checkints_(int *orig, int* read, int* len, int* numberRows, const char* mess, int *status, size_t mess_length) {
    *status = checkints(orig, read, *numberRows, mess);
}

   
//       subroutine checkTimes(dataOrig, dataRead, baseDate,
//      * timeGranularityFlag, number, mess, status)
   
//       implicit none
   
//       integer number, status
//       integer dataOrig(number), dataRead(number)
//       character mess*(*)
   
//       integer i, minutesInBaseDate, baseDate
//       integer timeRead, timeGranularityFlag
   
//       minutesInBaseDate = baseDate * 1440
//       do 100 i=1, number
//         if (timeGranularityFlag.eq.1) then
//           timeRead = (dataRead(i) / 60) + minutesInBaseDate
//         else
//           timeRead = dataRead(i) + minutesInBaseDate
//         endif
//         if (dataOrig(i).ne.timeRead) then
//         write (*,*)' '
//         write (*,*)' '
//         write (*,*)'*****  Data read does not match those written *****'
//         write (*, 20) i, dataOrig(i), timeRead
//  20     Format('At ordinate: ', I8, ' Written: ', I12, '  Read: ',I12)
//          write (*,*)mess
//          write (*,*)' '
//          write (*,*)' '
//         status = -1
//         return
//         endif
//  100  continue

//       status = 0
//       return
//       end


//       subroutine checkNotes(dataOrig, dataRead, number, mess, status)
   
//       implicit none
   
//       integer number, status
//       character dataOrig(number)*(*), dataRead(number)*(*)
//       character mess*(*)
   
//       integer i, n2, n3

//       do 100 i=1, number
//         call chrlnb(dataOrig(i), n3)
//         call chrlnb(dataRead(i), n2)
//         if ((n3.le.0).and.(n2.le.0)) go to 100
//         if (n3.le.0) n3 = 1
//         if (n2.le.0) n2 = 1
//         call checkString(dataOrig(i)(1:n3), dataRead(i)(1:n2), mess,
//      * status)
//         if (status.ne.0) then
//             write(*,*)'At ordinate number ',i
//             status = -1
//             return
//         endif
//  100  continue

//       status = 0
//       return
//       end
   
//       subroutine checkBlanks(dataRead, number, mess, status)
   
//       implicit none
   
//       integer number, status
//       character dataRead(number)*(*)
//       character mess*(*)
   
//       integer i, n2

//       do 100 i=1, number
//       call chrlnb(dataRead(i), n2)
//       if (n2.ne.0) then
//       write (*,*)' '
//       write (*,*)' '
//       write (*,*)'***** Strings read does not match those written *****'
//        write (*,*)mess
//        write (*,*)' '
//        write (*,*)' '
//       write(*,*)'At ordinate number ',i
//       status = -1
//       return
//       endif
//  100  continue

//       status = 0
//       return
//       end

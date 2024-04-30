#include <stdio.h>
#include <string.h>

#include "TestDssC.h"

int current_time_testing() {

  long long t;
  long long prev_t = getCurrentTimeMillis();

  for (size_t i = 0; i < 500; i++)
  {
#ifdef _MSC_VER
    Sleep(10);
#else
    usleep(10 * 1000);
#endif
    t = getCurrentTimeMillis(); 
    //printf("\n %lld", t);
    if (t < prev_t) {
      printf("\n... Oops... time going backward... in %s",__FILE__);
      return -1;
    }

    prev_t = t;
    

  }
  return 0;
}
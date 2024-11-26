#include "heclib.h"

/// <summary>
/// determine if the D part of a dss path is blank
/// </summary>
/// <param name="pathname"></param>
/// <returns>returns 1 if D part is blank/empty, otherwise return 0 </returns>
int isDpartEmpty(const char* pathname) {
  char dPart[MAX_PART_SIZE];
  int partNumber = 4; //A=1, B=2, C=3, D=4
  int length = zpathnameGetPart(pathname, partNumber, dPart, sizeof(dPart));
  return length == 0;
}
#include "heclib.h"

/// <summary>
/// determine if the D part contains 'TS-Pattern'
/// 
/// </summary>
/// <param name="pathname"></param>
/// <returns>returns 1 if D part is blank/empty, otherwise return 0 </returns>
int isTsPattern(const char* pathname) {
  char dPart[MAX_PART_SIZE];
  int partNumber = 4; //A=1, B=2, C=3, D=4
  int len = zpathnameGetPart(pathname, partNumber, dPart, sizeof(dPart));
  if (len == STATUS_NOT_OKAY) {
    return 0;
  }
  
  if (zstringCompare(dPart, "TS-Pattern", strlen(dPart))) {
    return 1;
  }
    
  return 0;
}
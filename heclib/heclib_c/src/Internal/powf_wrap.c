/* only under gcc (Linux)*/
#if defined(__GNUC__) && (__linux__)

/* for glibc version */
#include <features.h>
#include <math.h> /* powf call */
 /* glibc redefined powf in 2.27, breaks compatibility with prior versions */
#if (defined(__GLIBC__) && (__GLIBC__ == 2) && (__GLIBC_MINOR__ >= 27)) || (defined(__GNUC__) && (__GNUC__ <= 10))

/* 64 bit glibc powf was defined in 2.2.5 */
#if __LP64__
# define SYMVER "GLIBC_2.2.5"

/* 32-bit glibc powf was defined in 2.0 */
#else 
# define SYMVER "GLIBC_2.0"

/* endif Architecture check */
#endif

/* to replace the function with the old call */
#define USE_OLD_SYM(F,V) __asm__(".symver " #F ", " #F "@" V)

/* force powf to be from earlier compatible glibc */
USE_OLD_SYM(powf,SYMVER);

/* endif GLIBC >= 2.27 */
#endif

/* wrap the function requires gcc -Wl,--wrap=powf when linking */
float __wrap_powf( float base, float exponent ) {
  /* call into the supported version of powf */
  return powf(base, exponent);
}

/* so gets reloaded if necessary */
#undef _FEATURES_H

/* endif gcc & linux */
#endif

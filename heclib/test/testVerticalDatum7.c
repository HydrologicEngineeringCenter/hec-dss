#include <assert.h>
#include <string.h>
#include <heclib.h>
#include <verticalDatum.h>

void main (int argc, char *argv[]) {
    long long ifltab[250];
    int   status;
    int   intVal;
    int  *userHeader;
    char  charVal[17];
    char *userHeaderStringIn;
    char *userHeaderStringOut;

    zquery("VERS", charVal, sizeof(charVal), &intVal);
    assert(intVal == 7);
    
    zquery("VDTM", charVal, sizeof(charVal), &intVal);
    assert(intVal == IVERTICAL_DATUM_UNSET);
    assert(!strcmp(charVal, CVERTICAL_DATUM_UNSET));

    zset("VDTM", "", 1);
    zquery("VDTM", charVal, sizeof(charVal), &intVal);
    assert(intVal == IVERTICAL_DATUM_NAVD88);
    assert(!strcmp(charVal, CVERTICAL_DATUM_NAVD88));

    zset("VDTM", "", 2);
    zquery("VDTM", charVal, sizeof(charVal), &intVal);
    assert(intVal == IVERTICAL_DATUM_NGVD29);
    assert(!strcmp(charVal, CVERTICAL_DATUM_NGVD29));

    intVal = zset("VDTM", "", 4);
    // assert(intVal == STATUS_NOT_OKAY); bug, always returns STATUS_OK - Jira issue DSS-122
    zquery("VDTM", charVal, sizeof(charVal), &intVal);
    assert(intVal == IVERTICAL_DATUM_NGVD29);
    assert(!strcmp(charVal, CVERTICAL_DATUM_NGVD29));

    zset("VDTM", "", 3);
    zquery("VDTM", charVal, sizeof(charVal), &intVal);
    assert(intVal == IVERTICAL_DATUM_OTHER);
    assert(intVal == IVERTICAL_DATUM_LOCAL);
    assert(!strcmp(charVal, CVERTICAL_DATUM_OTHER));
    assert(!strcmp(charVal, CVERTICAL_DATUM_LOCAL));

    zset("VDTM", "", 0);
    zquery("VDTM", charVal, sizeof(charVal), &intVal);
    assert(intVal == IVERTICAL_DATUM_UNSET);
    assert(!strcmp(charVal, CVERTICAL_DATUM_UNSET));

    zset("VDTM", CVERTICAL_DATUM_NAVD88, 0);
    zquery("VDTM", charVal, sizeof(charVal), &intVal);
    assert(intVal == IVERTICAL_DATUM_NAVD88);
    assert(!strcmp(charVal, CVERTICAL_DATUM_NAVD88));

    zset("VDTM", CVERTICAL_DATUM_NGVD29, 0);
    zquery("VDTM", charVal, sizeof(charVal), &intVal);
    assert(intVal == IVERTICAL_DATUM_NGVD29);
    assert(!strcmp(charVal, CVERTICAL_DATUM_NGVD29));

    zset("VDTM", CVERTICAL_DATUM_OTHER, 0);
    zquery("VDTM", charVal, sizeof(charVal), &intVal);
    assert(intVal == IVERTICAL_DATUM_OTHER);
    assert(intVal == IVERTICAL_DATUM_LOCAL);
    assert(!strcmp(charVal, CVERTICAL_DATUM_OTHER));
    assert(!strcmp(charVal, CVERTICAL_DATUM_LOCAL));

    zset("VDTM", CVERTICAL_DATUM_LOCAL, 0);
    zquery("VDTM", charVal, sizeof(charVal), &intVal);
    assert(intVal == IVERTICAL_DATUM_OTHER);
    assert(intVal == IVERTICAL_DATUM_LOCAL);
    assert(!strcmp(charVal, CVERTICAL_DATUM_OTHER));
    assert(!strcmp(charVal, CVERTICAL_DATUM_LOCAL));

    zset("VDTM", "Pensacola", 0);
    zquery("VDTM", charVal, sizeof(charVal), &intVal);
    assert(intVal == IVERTICAL_DATUM_OTHER);
    assert(intVal == IVERTICAL_DATUM_LOCAL);
    assert(!strcmp(charVal, "Pensacola"));

    zset("VDTM", CVERTICAL_DATUM_UNSET, 0);
    zquery("VDTM", charVal, sizeof(charVal), &intVal);
    assert(intVal == IVERTICAL_DATUM_UNSET);
    assert(!strcmp(charVal, CVERTICAL_DATUM_UNSET));

    intVal = 0;
    userHeaderStringIn = "This is a test string for the user header ";
    userHeader = string_to_user_header(userHeaderStringIn, &intVal);
    assert(intVal == (strlen(userHeaderStringIn)-1)/4+1);
    userHeaderStringOut = string_from_user_header(userHeader, intVal);
    assert(!strcmp(userHeaderStringIn, userHeaderStringOut));

    free(userHeader);
    free(userHeaderStringOut);

    exit(0);
}
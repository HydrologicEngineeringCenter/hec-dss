#include <jni.h>
#include <string.h>
#include <stdlib.h>
#include "heclib.h"

//#define WHERE_AM_I() {printf("At line %d in file %s.\n", __LINE__, __FILE__); fflush(stdout);}
#define WHERE_AM_I()

JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zrrst(
	JNIEnv       *env, 
	jobject       obj, 
	jintArray     j_ifltab, // file table                                                
	jbyteArray    j_cpath,  // pathname                                                  
	jintArray     j_ietim,  // effective time (minutes since 31Dec1899 00:00)
	jbyteArray    j_cloc,   // location string
	jbyteArray    j_catim,  // activated time string
	jbyteArray    j_chparm, // height parameter string
	jbyteArray    j_chunit, // height units string
	jbyteArray    j_cfparm, // flow parameter string
	jbyteArray    j_cfunit, // flow units string
	jbyteArray    j_ccmt,   // comment string
	jbyteArray    j_copt,   // option string
	jbooleanArray j_lsnum,  // flag for specifying if rating has a serial number
	jintArray     j_isnum,  // rating serial number
	jbooleanArray j_ldatum, // flag for specifying if rating has datum elevation
	jfloatArray   j_datum,  // rating datum elevation
	jbooleanArray j_lstage, // flag for stage heights (vs elevation heights)                  
	jintArray     j_ibhi,   // height interpolation type for base curve                         
	jintArray     j_ibhu,   // height underflow handling type for base curve             
	jfloatArray   j_huv,    // height underflow value (may not be used)
	jintArray     j_ibho,   // height overflow handling type for base curve              
	jfloatArray   j_hov,    // height overflow value (may not be used)
	jintArray     j_ibfi,   // flow interpolation type for base curve                         
	jintArray     j_ibfu,   // flow underflow handling type for base curve               
	jfloatArray   j_fuv,    // flow underflow value (may not be used)
	jintArray     j_ibfo,   // flow overflow handling type for base curve                
	jfloatArray   j_fov,    // flow overflow value (may not be used)
	jintArray     j_nbase,  // number of points in base curve                            
	jfloatArray   j_height, // stages or elevations                                      
	jfloatArray   j_flow,   // flows                                                     
	jintArray     j_nshift, // number of constant or variable shifts                     
	jintArray     j_isti,   // interpolation type for shift effective times              
	jintArray     j_istu,   // underflow handling type for shift effective times         
	jintArray     j_isto,   // overflow handling type for shift effective times          
	jintArray     j_isbi,   // interpolation type for shift breakpoints for each shift
	jintArray     j_isbu,   // underflow handling type for shift breakpoints for each shift
	jfloatArray   j_sbuv,   // shift breakpoints underflow values for each shift
	jintArray     j_isbo,   // overflow handling type for shift breakpoints for each shift
	jfloatArray   j_sbov,   // shift breakpoints overflow values for each shift
	jintArray     j_ishi,   // interpolation type for shift heights for each shift
	jintArray     j_ishu,   // underflow handling type for shift heights for each shift
	jfloatArray   j_shuv,   // shift heights underflow values for each shift
	jintArray     j_isho,   // overflow handling type for shift heights for each shift                  
	jfloatArray   j_shov,   // shift heights overflow values for each shift
	jbooleanArray j_latmsk, // masks specifying whether each shift has activated time    
	jintArray     j_jbdate, // julian base date for shift times                          
	jintArray     j_ietime, // effective times for shifts (minutes relative to jbdate)   
	jintArray     j_iatime, // activated times for shifts (minutes relative to jbdate)   
	jintArray     j_nshftp, // number of shift points for each shift (0 = constant shift)
	jintArray     j_nshval, // total number of number of shift point values       
	jfloatArray   j_shifts, // shift points                                              
	jbooleanArray j_lcoff,  // flag specifying whether a constant offset is used
	jfloatArray   j_cofval, // constant offset value
	jintArray     j_noff,   // number of variable offset breakpoint, height pairs
	jintArray     j_iobi,   // interpolation type for variable offset breakpoints
	jintArray     j_iobu,   // underflow handling type for variable offset breakpoints
	jfloatArray   j_obuv,   // offset breakpoint underflow value (may or may not be used)
	jintArray     j_iobo,   // overflow handling type for variable offset breakpoints
	jfloatArray   j_obov,   // offset breakpoint overflow value (may or may not be used)
	jintArray     j_iohi,   // interpolation type for variable offset heights
	jintArray     j_iohu,   // underflow handling type for variable offset heights
	jfloatArray   j_ohuv,   // offset height underflow value (may or may not be used)
	jintArray     j_ioho,   // overflow handling type for variable offset heights
	jfloatArray   j_ohov,   // offset height overflow value (may or may not be used)
	jfloatArray   j_offset, // variable offset values (break(1),height(1),...break(n),height(n))
	jintArray     j_ihoriz, // horizontal axis                                           
	jintArray     j_istat)  // success/failure status                                    
{

	const char *cpath;
	char *cloc, *catim, *chparm, *chunit, *cfparm, *cfunit, *ccmt, *copt;
	unsigned char *lsnum, *ldatum, *lstage, *lcoff, *latmsk;

	int *ifltab, *ietim, npath, nloc, natim, nhparm, nhunit, nfparm, nfunit, ncmt, nopt, 
		*isnum, *ibhi, *ibhu, *ibho, *ibfi, *ibfu, *ibfo, kbase, *nbase, kshift, *nshift, 
		*isti, *istu, *isto, *isbi, *isbu, *isbo, *ishi, *ishu, *isho, *jbdate, *ietime, 
		*iatime, *nshftp, kshval, *nshval, koff, *noff, *iobi, *iobu, *iobo, *iohi, *iohu, 
		*ioho, *ihoriz, *istat, _lsnum, _ldatum, _lstage, _lcoff, *_latmsk, i;

	float *datum, *huv, *hov, *fuv, *fov, *height, *flow, *sbuv, *sbov, *shuv, *shov, *shifts, 
    *cofval, *obuv, *obov, *ohuv, *ohov, *offset;
  
	//------------------//
	// attach variables //
	//------------------//
	jint capacity=80;
	WHERE_AM_I()
	(*env)->EnsureLocalCapacity(env, capacity);
fprintf(stderr, "\n\nFunction zrrst not implemented\n\n");
	WHERE_AM_I()
	ifltab = (*env)->GetIntArrayElements(env, j_ifltab, 0);
	cpath  = (*env)->GetStringUTFChars(env, j_cpath,  0);
	ietim  = (*env)->GetIntArrayElements(env, j_ietim, 0);
	cloc   = (*env)->GetByteArrayElements(env, j_cloc,   0);
	catim  = (*env)->GetByteArrayElements(env, j_catim,  0);
	chparm = (*env)->GetByteArrayElements(env, j_chparm, 0);
	chunit = (*env)->GetByteArrayElements(env, j_chunit, 0);
	cfparm = (*env)->GetByteArrayElements(env, j_cfparm, 0);
	cfunit = (*env)->GetByteArrayElements(env, j_cfunit, 0);
	ccmt   = (*env)->GetByteArrayElements(env, j_ccmt,   0);
	copt   = (*env)->GetByteArrayElements(env, j_copt,   0);
	lsnum  = (*env)->GetBooleanArrayElements(env, j_lsnum, 0);
	isnum  = (*env)->GetIntArrayElements(env, j_isnum, 0);
	ldatum = (*env)->GetBooleanArrayElements(env, j_ldatum, 0);
	datum  = (*env)->GetFloatArrayElements(env, j_datum, 0);
	lstage = (*env)->GetBooleanArrayElements(env, j_lstage, 0);
	ibhi   = (*env)->GetIntArrayElements(env, j_ibhi, 0);
	ibhu   = (*env)->GetIntArrayElements(env, j_ibhu, 0);
	huv    = (*env)->GetFloatArrayElements(env, j_huv, 0);
	ibho   = (*env)->GetIntArrayElements(env, j_ibho, 0);
	hov    = (*env)->GetFloatArrayElements(env, j_hov, 0);
	ibfi   = (*env)->GetIntArrayElements(env, j_ibfi, 0);
	ibfu   = (*env)->GetIntArrayElements(env, j_ibfu, 0);
	fuv    = (*env)->GetFloatArrayElements(env, j_fuv, 0);
	ibfo   = (*env)->GetIntArrayElements(env, j_ibfo, 0);
	fov    = (*env)->GetFloatArrayElements(env, j_fov, 0);
	nbase  = (*env)->GetIntArrayElements(env, j_nbase, 0);
	height = (*env)->GetFloatArrayElements(env, j_height, 0);
	flow   = (*env)->GetFloatArrayElements(env, j_flow, 0);
	nshift = (*env)->GetIntArrayElements(env, j_nshift, 0);
	isti   = (*env)->GetIntArrayElements(env, j_isti, 0);
	istu   = (*env)->GetIntArrayElements(env, j_istu, 0);
	isto   = (*env)->GetIntArrayElements(env, j_isto, 0);
	isbi   = (*env)->GetIntArrayElements(env, j_isbi, 0);
	isbu   = (*env)->GetIntArrayElements(env, j_isbu, 0);
	sbuv   = (*env)->GetFloatArrayElements(env, j_sbuv, 0);
	isbo   = (*env)->GetIntArrayElements(env, j_isbo, 0);
	sbov   = (*env)->GetFloatArrayElements(env, j_sbov, 0);
	ishi   = (*env)->GetIntArrayElements(env, j_ishi, 0);
	ishu   = (*env)->GetIntArrayElements(env, j_ishu, 0);
	shuv   = (*env)->GetFloatArrayElements(env, j_shuv, 0);
	isho   = (*env)->GetIntArrayElements(env, j_isho, 0);
	shov   = (*env)->GetFloatArrayElements(env, j_shov, 0);
	latmsk = (*env)->GetBooleanArrayElements(env, j_latmsk, 0);
	jbdate = (*env)->GetIntArrayElements(env, j_jbdate, 0);
	ietime = (*env)->GetIntArrayElements(env, j_ietime, 0);
	iatime = (*env)->GetIntArrayElements(env, j_iatime, 0);
	nshftp = (*env)->GetIntArrayElements(env, j_nshftp, 0);
	nshval = (*env)->GetIntArrayElements(env, j_nshval, 0);
	shifts = (*env)->GetFloatArrayElements(env, j_shifts, 0);
	lcoff  = (*env)->GetBooleanArrayElements(env, j_lcoff, 0);
	cofval = (*env)->GetFloatArrayElements(env, j_cofval, 0);
	noff   = (*env)->GetIntArrayElements(env, j_noff, 0);
	iobi   = (*env)->GetIntArrayElements(env, j_iobi, 0);
	iobu   = (*env)->GetIntArrayElements(env, j_iobu, 0);
	obuv   = (*env)->GetFloatArrayElements(env, j_obuv, 0);
	iobo   = (*env)->GetIntArrayElements(env, j_iobo, 0);
	obov   = (*env)->GetFloatArrayElements(env, j_obov, 0);
	iohi   = (*env)->GetIntArrayElements(env, j_iohi, 0);
	iohu   = (*env)->GetIntArrayElements(env, j_iohu, 0);
	ohuv   = (*env)->GetFloatArrayElements(env, j_ohuv, 0);
	ioho   = (*env)->GetIntArrayElements(env, j_ioho, 0);
	ohov   = (*env)->GetFloatArrayElements(env, j_ohov, 0);
	offset = (*env)->GetFloatArrayElements(env, j_offset, 0);
	ihoriz = (*env)->GetIntArrayElements(env, j_ihoriz, 0);
	istat  = (*env)->GetIntArrayElements(env, j_istat, 0);

	WHERE_AM_I()
	npath  = (int)strlen(cpath);
	nloc   = (int)(*env)->GetArrayLength(env, j_cloc);
	natim  = (int)(*env)->GetArrayLength(env, j_catim);
	nhparm = (int)(*env)->GetArrayLength(env, j_chparm);
	nhunit = (int)(*env)->GetArrayLength(env, j_chunit);
	nfparm = (int)(*env)->GetArrayLength(env, j_cfparm);
	nfunit = (int)(*env)->GetArrayLength(env, j_cfunit);
	ncmt   = (int)(*env)->GetArrayLength(env, j_ccmt);
	nopt   = (int)(*env)->GetArrayLength(env, j_copt);
	kbase  = (int)(*env)->GetArrayLength(env, j_height);
	kshift = (int)(*env)->GetArrayLength(env, j_ietime);
	kshval = (int)(*env)->GetArrayLength(env, j_shifts);
	koff   = (int)(*env)->GetArrayLength(env, j_offset);

	WHERE_AM_I()
	_latmsk = (int *)malloc(kshift * sizeof(int));
	memset(_latmsk, 0, kshift * sizeof(int));

	//---------------//
	// make the call //
	//---------------//
	WHERE_AM_I()

	WHERE_AM_I()
	zrrst_ ((long long *)ifltab, cpath, ietim, cloc, catim, chparm, chunit, cfparm, cfunit, 
	  ccmt, copt, &_lsnum, isnum, &_ldatum, datum, &_lstage, ibhi, ibhu, huv, ibho, 
	  hov, ibfi, ibfu, fuv, ibfo, fov, &kbase, nbase, height, flow, &kshift, nshift, 
	  isti, istu, isto, isbi, isbu, sbuv, isbo, sbov, ishi, ishu, shuv, isho, 
	  shov, _latmsk, jbdate, ietime, iatime, nshftp, &kshval, nshval, shifts, 
	  &_lcoff, cofval, &koff, noff, iobi, iobu, obuv, iobo, obov, iohi, iohu, ohuv, 
	  ioho, ohov, offset, ihoriz, istat, npath, nloc, natim, nhparm, nhunit, nfparm, 
	  nfunit, ncmt, nopt);
  
	WHERE_AM_I()
	lsnum[0]  = _lsnum  ? JNI_TRUE : JNI_FALSE;
	ldatum[0] = _ldatum ? JNI_TRUE : JNI_FALSE;
	lstage[0] = _lstage ? JNI_TRUE : JNI_FALSE;
	lcoff[0]  = _lcoff  ? JNI_TRUE : JNI_FALSE;

	WHERE_AM_I()
	for (i = 0; i < nshift[0]; ++i) latmsk[i] = _latmsk[i] ? JNI_TRUE : JNI_FALSE;
	free(_latmsk);

	//-------------------//
	// release variables //
	//-------------------//
	WHERE_AM_I()
	(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
	(*env)->ReleaseStringUTFChars(env, j_cpath, cpath);
	(*env)->ReleaseIntArrayElements(env, j_ietim, ietim, 0);
	(*env)->ReleaseByteArrayElements(env, j_cloc, cloc,   0);
	(*env)->ReleaseByteArrayElements(env, j_catim, catim,  0);
	(*env)->ReleaseByteArrayElements(env, j_chparm, chparm, 0);
	(*env)->ReleaseByteArrayElements(env, j_chunit, chunit, 0);
	(*env)->ReleaseByteArrayElements(env, j_cfparm, cfparm, 0);
	(*env)->ReleaseByteArrayElements(env, j_cfunit, cfunit, 0);
	(*env)->ReleaseByteArrayElements(env, j_ccmt, ccmt,   0);
	(*env)->ReleaseByteArrayElements(env, j_copt, copt,   0);
	(*env)->ReleaseBooleanArrayElements(env, j_lsnum, lsnum, 0);
	(*env)->ReleaseIntArrayElements(env, j_isnum, isnum, 0);
	(*env)->ReleaseBooleanArrayElements(env, j_ldatum, ldatum, 0);
	(*env)->ReleaseFloatArrayElements(env, j_datum, datum, 0);
	(*env)->ReleaseBooleanArrayElements(env, j_lstage, lstage, 0);
	(*env)->ReleaseIntArrayElements(env, j_ibhi, ibhi, 0);
	(*env)->ReleaseIntArrayElements(env, j_ibhu, ibhu, 0);
	(*env)->ReleaseFloatArrayElements(env, j_huv, huv, 0);
	(*env)->ReleaseIntArrayElements(env, j_ibho, ibho, 0);
	(*env)->ReleaseFloatArrayElements(env, j_hov, hov, 0);
	(*env)->ReleaseIntArrayElements(env, j_ibfi, ibfi, 0);
	(*env)->ReleaseIntArrayElements(env, j_ibfu, ibfu, 0);
	(*env)->ReleaseFloatArrayElements(env, j_fuv, fuv, 0);
	(*env)->ReleaseIntArrayElements(env, j_ibfo, ibfo, 0);
	(*env)->ReleaseFloatArrayElements(env, j_fov, fov, 0);
	(*env)->ReleaseIntArrayElements(env, j_nbase, nbase, 0);
	(*env)->ReleaseFloatArrayElements(env, j_height, height, 0);
	(*env)->ReleaseFloatArrayElements(env, j_flow, flow, 0);
	(*env)->ReleaseIntArrayElements(env, j_nshift, nshift, 0);
	(*env)->ReleaseIntArrayElements(env, j_isti, isti, 0);
	(*env)->ReleaseIntArrayElements(env, j_istu, istu, 0);
	(*env)->ReleaseIntArrayElements(env, j_isto, isto, 0);
	(*env)->ReleaseIntArrayElements(env, j_isbi, isbi, 0);
	(*env)->ReleaseIntArrayElements(env, j_isbu, isbu, 0);
	(*env)->ReleaseFloatArrayElements(env, j_sbuv, sbuv, 0);
	(*env)->ReleaseIntArrayElements(env, j_isbo, isbo, 0);
	(*env)->ReleaseFloatArrayElements(env, j_sbov, sbov, 0);
	(*env)->ReleaseIntArrayElements(env, j_ishi, ishi, 0);
	(*env)->ReleaseIntArrayElements(env, j_ishu, ishu, 0);
	(*env)->ReleaseFloatArrayElements(env, j_shuv, shuv, 0);
	(*env)->ReleaseIntArrayElements(env, j_isho, isho, 0);
	(*env)->ReleaseFloatArrayElements(env, j_shov, shov, 0);
	(*env)->ReleaseBooleanArrayElements(env, j_latmsk, latmsk, 0);
	(*env)->ReleaseIntArrayElements(env, j_jbdate, jbdate, 0);
	(*env)->ReleaseIntArrayElements(env, j_ietime, ietime, 0);
	(*env)->ReleaseIntArrayElements(env, j_iatime, iatime, 0);
	(*env)->ReleaseIntArrayElements(env, j_nshftp, nshftp, 0);
	(*env)->ReleaseIntArrayElements(env, j_nshval, nshval, 0);
	(*env)->ReleaseFloatArrayElements(env, j_shifts, shifts, 0);
	(*env)->ReleaseBooleanArrayElements(env, j_lcoff, lcoff, 0);
	(*env)->ReleaseFloatArrayElements(env, j_cofval, cofval, 0);
	(*env)->ReleaseIntArrayElements(env, j_noff, noff, 0);
	(*env)->ReleaseIntArrayElements(env, j_iobi, iobi, 0);
	(*env)->ReleaseIntArrayElements(env, j_iobu, iobu, 0);
	(*env)->ReleaseFloatArrayElements(env, j_obuv, obuv, 0);
	(*env)->ReleaseIntArrayElements(env, j_iobo, iobo, 0);
	(*env)->ReleaseFloatArrayElements(env, j_obov, obov, 0);
	(*env)->ReleaseIntArrayElements(env, j_iohi, iohi, 0);
	(*env)->ReleaseIntArrayElements(env, j_iohu, iohu, 0);
	(*env)->ReleaseFloatArrayElements(env, j_ohuv, ohuv, 0);
	(*env)->ReleaseIntArrayElements(env, j_ioho, ioho, 0);
	(*env)->ReleaseFloatArrayElements(env, j_ohov, ohov, 0);
	(*env)->ReleaseFloatArrayElements(env, j_offset, offset, 0);
	(*env)->ReleaseIntArrayElements(env, j_ihoriz, ihoriz, 0);
	(*env)->ReleaseIntArrayElements(env, j_istat, istat, 0);
	WHERE_AM_I()
}

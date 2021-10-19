#include <jni.h>
#include <string.h>
#include <stdlib.h>
#include "heclib.h"

// #define WHERE_AM_I() {printf("At line %d in file %s\n", __LINE__, __FILE__); fflush(stdout);}
#define WHERE_AM_I()

JNIEXPORT void JNICALL Java_hec_heclib_util_Heclib_Hec_1zsrst(
	JNIEnv       *env, 
	jobject       obj, 
	jintArray     j_ifltab, // file table                                                
	jstring       j_cpath,  // pathname                                                  
	jstring       j_cloc,   // location string
	jstring       j_catim,  // activated time string
	jstring       j_chparm, // height parameter string
	jstring       j_chunit, // height units string
	jstring       j_cfparm, // flow parameter string
	jstring       j_cfunit, // flow units string
	jstring       j_ccmt,   // comment string
	jstring       j_copt,   // option string
	jboolean      j_lsnum,  // flag for specifying if rating has a serial number
	jint          j_isnum,  // rating serial number
	jboolean      j_ldatum, // flag for specifying if rating has datum elevation
	jfloat        j_datum,  // rating datum elevation
	jboolean      j_lstage, // flag for stage heights (vs elevation heights)                  
	jint          j_ibhi,   // height interpolation type for base curve                         
	jint          j_ibhu,   // height underflow handling type for base curve             
	jfloat        j_huv,    // height underflow value (may not be used)
	jint          j_ibho,   // height overflow handling type for base curve              
	jfloat        j_hov,    // height overflow value (may not be used)
	jint          j_ibfi,   // flow interpolation type for base curve                         
	jint          j_ibfu,   // flow underflow handling type for base curve               
	jfloat        j_fuv,    // flow underflow value (may not be used)
	jint          j_ibfo,   // flow overflow handling type for base curve                
	jfloat        j_fov,    // flow overflow value (may not be used)
	jint          j_nbase,  // number of points in base curve                            
	jfloatArray   j_height, // stages or elevations                                      
	jfloatArray   j_flow,   // flows                                                     
	jint          j_nshift, // number of constant or variable shifts                     
	jint          j_isti,   // interpolation type for shift effective times              
	jint          j_istu,   // underflow handling type for shift effective times         
	jint          j_isto,   // overflow handling type for shift effective times          
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
	jint          j_jbdate, // julian base date for shift times                          
	jintArray     j_ietime, // effective times for shifts (minutes relative to jbdate)   
	jintArray     j_iatime, // activated times for shifts (minutes relative to jbdate)   
	jintArray     j_nshftp, // number of shift points for each shift (0 = constant shift)
	jfloatArray   j_shifts, // shift points                                              
	jboolean      j_lcoff,  // flag specifying whether a constant offset is used
	jfloat        j_cofval, // constant offset value
	jint          j_noff,   // number of variable offset breakpoint, height pairs
	jint          j_iobi,   // interpolation type for variable offset breakpoints
	jint          j_iobu,   // underflow handling type for variable offset breakpoints
	jfloat        j_obuv,   // offset breakpoint underflow value (may or may not be used)
	jint          j_iobo,   // overflow handling type for variable offset breakpoints
	jfloat        j_obov,   // offset breakpoint overflow value (may or may not be used)
	jint          j_iohi,   // interpolation type for variable offset heights
	jint          j_iohu,   // underflow handling type for variable offset heights
	jfloat        j_ohuv,   // offset height underflow value (may or may not be used)
	jint          j_ioho,   // overflow handling type for variable offset heights
	jfloat        j_ohov,   // offset height overflow value (may or may not be used)
	jfloatArray   j_offset, // variable offset values (break(1),height(1),...break(n),height(n))
	jint          j_ihoriz, // horizontal axis                                           
	jint          j_iplan,  // overwrite plan                                            
	jintArray     j_istat)  // success/failure status                                    
{

	int   i[1] = {0};
	float f[1] = {0.f};

	const char *cpath, *cloc, *catim, *chparm, *chunit, *cfparm, *cfunit, *ccmt, *copt;

	unsigned char *latmsk;

	int *ifltab;
	int npath, nloc, natim, nhparm, nhunit, nfparm, nfunit, ncmt, nopt, lsnum, 
		isnum, ldatum, lstage, ibhi, ibhu, ibho, ibfi, ibfu, ibfo, nbase, nshift, isti, 
		istu, isto, *isbi = i, *isbu = i, *isbo = i, *ishi = i, *ishu = i, *isho = i, 
		jbdate, *ietime = i, *iatime = i, *nshftp = i, lcoff, noff, iobi, iobu, iobo, 
		iohi, iohu, ioho, ihoriz, iplan, *istat, *_latmsk;

	float datum, huv, hov, fuv, fov, *height = f, *flow = f, *sbuv = f, *sbov = f, *shuv = f, 
		*shov = f, *shifts = f, cofval, obuv, obov, ohuv, ohov, *offset = f;

	//------------------//
	// attach variables //
	//------------------//
	jint capacity=40;
  WHERE_AM_I();
	(*env)->EnsureLocalCapacity(env, capacity);

  WHERE_AM_I();
	ifltab = (*env)->GetIntArrayElements(env, j_ifltab, 0);
	cpath  = (*env)->GetStringUTFChars(env, j_cpath, 0);
	npath  = (int)strlen(cpath);
	cloc   = (*env)->GetStringUTFChars(env, j_cloc, 0);
	nloc   = (int)strlen(cloc); 
	catim  = (*env)->GetStringUTFChars(env, j_catim, 0);
	natim  = (int)strlen(catim);
	chparm = (*env)->GetStringUTFChars(env, j_chparm, 0);
	nhparm = (int)strlen(chparm);
	chunit = (*env)->GetStringUTFChars(env, j_chunit, 0);
	nhunit = (int)strlen(chunit);
	cfparm = (*env)->GetStringUTFChars(env, j_cfparm, 0);
	nfparm = (int)strlen(cfparm);
	cfunit = (*env)->GetStringUTFChars(env, j_cfunit, 0);
	nfunit = (int)strlen(cfunit);
	ccmt   = (*env)->GetStringUTFChars(env, j_ccmt, 0);
	ncmt   = (int)strlen(ccmt);
	copt   = (*env)->GetStringUTFChars(env, j_copt, 0);
	nopt   = (int)strlen(copt);
	lsnum  = (int)j_lsnum;
	if (lsnum)
	{
		isnum  = (int)j_isnum;
	}
	ldatum = (int)j_ldatum;
	if (ldatum)
	{
		datum  = (float)j_datum;
	}
	lstage = (int)j_lstage;
	ibhi   = (int)j_ibhi;
	ibhu   = (int)j_ibhu;
	huv    = (float)j_huv;
	ibho   = (int)j_ibho;
	hov    = (float)j_hov;
	ibfi   = (int)j_ibfi;
	ibfu   = (int)j_ibfu;
	fuv    = (float)j_fuv;
	ibfo   = (int)j_ibfo;
	fov    = (float)j_fov;
	nbase  = (int)j_nbase;
	if (nbase)
	{
		height = (*env)->GetFloatArrayElements(env, j_height, 0);
		flow   = (*env)->GetFloatArrayElements(env, j_flow, 0);
	}
	nshift = (int)j_nshift;
	if (nshift > 0)
	{
		int i;
		isti   = (int)j_isti;
		istu   = (int)j_istu;
		isto   = (int)j_isto;
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
		jbdate = (int)j_jbdate;
		ietime = (*env)->GetIntArrayElements(env, j_ietime, 0);
		iatime = (*env)->GetIntArrayElements(env, j_iatime, 0);
		nshftp = (*env)->GetIntArrayElements(env, j_nshftp, 0);
		shifts = (*env)->GetFloatArrayElements(env, j_shifts, 0);

		_latmsk = (int *)malloc(nshift * sizeof(int));
		for (i = 0; i < nshift; ++i) _latmsk[i] = (int)latmsk[i];
	}
	lcoff  = (int)j_lcoff;
	if(lcoff)
	{
		cofval = (float)j_cofval;
	}
	else
	{
		noff   = (int)j_noff;
		if(noff > 0)
		{
			iobi   = (int)j_iobi;
			iobu   = (int)j_iobu;
			obuv   = (float)j_obuv;
			iobo   = (int)j_iobo;
			obov   = (float)j_obov;
			iohi   = (int)j_iohi;
			iohu   = (int)j_iohu;
			ohuv   = (float)j_ohuv;
			ioho   = (int)j_ioho;
			ohov   = (float)j_ohov;
			offset = (*env)->GetFloatArrayElements(env, j_offset, 0);
		}
	}
  
	ihoriz = (int)j_ihoriz;
	iplan  = (int)j_iplan;
	istat  = (*env)->GetIntArrayElements(env, j_istat, 0);


	//---------------//
	// make the call //
	//---------------//
  WHERE_AM_I();
	zsrst_ ((long long*)ifltab, cpath, cloc, catim, chparm, chunit, cfparm, cfunit, ccmt, 
	  copt,  &lsnum, &isnum, &ldatum, &datum, &lstage, &ibhi, &ibhu, &huv, &ibho, 
      &hov, &ibfi, &ibfu, &fuv, &ibfo, &fov, &nbase, height, flow,  &nshift, 
	  &isti, &istu, &isto, isbi, isbu, sbuv, isbo, sbov, ishi, ishu, shuv, isho, 
	  shov, _latmsk, &jbdate, ietime, iatime, nshftp, shifts, &lcoff, &cofval, 
	  &noff, &iobi, &iobu, &obuv, &iobo,  &obov, &iohi, &iohu, &ohuv, &ioho, 
	  &ohov, offset,&ihoriz, &iplan, istat, npath, nloc, natim, nhparm, nhunit, 
	  nfparm, nfunit, ncmt, nopt);
	
	//-------------------//
	// release variables //
	//-------------------//
  WHERE_AM_I();
	(*env)->ReleaseIntArrayElements(env, j_ifltab, ifltab, 0);
	(*env)->ReleaseStringUTFChars(env, j_cpath, cpath);
	(*env)->ReleaseStringUTFChars(env, j_cloc, cloc);
	(*env)->ReleaseStringUTFChars(env, j_catim, catim);
	(*env)->ReleaseStringUTFChars(env, j_chparm, chparm);
	(*env)->ReleaseStringUTFChars(env, j_chunit, chunit);
	(*env)->ReleaseStringUTFChars(env, j_cfparm, cfparm);
	(*env)->ReleaseStringUTFChars(env, j_cfunit, cfunit);
	(*env)->ReleaseStringUTFChars(env, j_ccmt, ccmt);
	(*env)->ReleaseStringUTFChars(env, j_copt, copt);
	if (nbase)
	{
		(*env)->ReleaseFloatArrayElements(env, j_height, height, 0);
		(*env)->ReleaseFloatArrayElements(env, j_flow, flow, 0);
	}
	if (nshift > 0)
	{
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
		(*env)->ReleaseIntArrayElements(env, j_ietime, ietime, 0);
		(*env)->ReleaseIntArrayElements(env, j_iatime, iatime, 0);
		(*env)->ReleaseIntArrayElements(env, j_nshftp, nshftp, 0);
		(*env)->ReleaseFloatArrayElements(env, j_shifts, shifts, 0);
		free(_latmsk);
	}
	if(!lcoff && noff > 0)
	{
		(*env)->ReleaseFloatArrayElements(env, j_offset, offset, 0);
	}
	(*env)->ReleaseIntArrayElements(env, j_istat, istat, 0);
  WHERE_AM_I();
}

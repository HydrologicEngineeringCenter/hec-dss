#include "stdio.h"
#include "string.h"

#include "heclib.h"
#include "hecdss7.h"
#include "hecdssInternal.h"
#include "TestDssC.h"



int testMisc()
{
	int DEBUG=0;

	long long ifltab[600];
	char cpath1[100];
	char cpath2[100];
	char apart[65];
	char fpart[65];
	char bpart[65];
	char cpart[65];
	char dpart[65];
	char epart[65];
	int partAction[6];
	int lengths[6];

	int status;
	int result;




	//  First test misc functions used by catalog

	stringCopy(cpath1, sizeof(cpath1), "abcdefghijklmnop", _TRUNCATE);
	stringCopy(cpath2, sizeof(cpath2), "F", _TRUNCATE);
	result = zfindString(cpath1, (int)strlen(cpath1), cpath2, (int)strlen(cpath2)); 
	status = zcompareInts(ifltab, result, 5, 1,  "testMisc, zfindString failure loc A ");
	if (status != STATUS_OKAY) return status;
	
	stringCopy(cpath1, sizeof(cpath1), "abcdefghijklmnop", _TRUNCATE);
	stringCopy(cpath2, sizeof(cpath2), "FgHi", _TRUNCATE);
	result = zfindString(cpath1, (int)strlen(cpath1), cpath2, (int)strlen(cpath2)); 
	status = zcompareInts(ifltab, result, 5, 1,  "testMisc, zfindString failure loc B ");
	if (status != STATUS_OKAY) return status;
	
	stringCopy(cpath1, sizeof(cpath1), "abcdefghijklmnop", _TRUNCATE);
	stringCopy(cpath2, sizeof(cpath2), "Fh", _TRUNCATE);
	result = zfindString(cpath1, (int)strlen(cpath1), cpath2, (int)strlen(cpath2)); 
	status = zcompareInts(ifltab, result, -1, 1,  "testMisc, zfindString failure loc C ");
	if (status != STATUS_OKAY) return status;
	
	stringCopy(cpath1, sizeof(cpath1), "abcdefghijklmnop", _TRUNCATE);
	stringCopy(cpath2, sizeof(cpath2), "abc", _TRUNCATE);
	result = zfindString(cpath1, (int)strlen(cpath1), cpath2, (int)strlen(cpath2)); 
	status = zcompareInts(ifltab, result, 0, 1,  "testMisc, zfindString failure loc D ");
	if (status != STATUS_OKAY) return status;
	
	stringCopy(cpath1, sizeof(cpath1), "abcdefghijklmnop", _TRUNCATE);
	stringCopy(cpath2, sizeof(cpath2), "nOp", _TRUNCATE);
	result = zfindString(cpath1, (int)strlen(cpath1), cpath2, (int)strlen(cpath2));
	status = zcompareInts(ifltab, result, 13, 1,  "testMisc, zfindString failure loc E ");
	if (status != STATUS_OKAY) return status;
	
	stringCopy(cpath1, sizeof(cpath1), "abcdefghijklmnop", _TRUNCATE);
	stringCopy(cpath2, sizeof(cpath2), "P", _TRUNCATE);
	result = zfindString(cpath1, (int)strlen(cpath1), cpath2, (int)strlen(cpath2));
	status = zcompareInts(ifltab, result, 15, 1,  "testMisc, zfindString failure loc F ");
	if (status != STATUS_OKAY) return status;
		
	stringCopy(cpath1, sizeof(cpath1), "abcdefghijklmnop", _TRUNCATE);
	stringCopy(cpath2, sizeof(cpath2), "abcdefghijklmnop", _TRUNCATE);
	result = zfindString(cpath1, (int)strlen(cpath1), cpath2, (int)strlen(cpath2));
	status = zcompareInts(ifltab, result, 0, 1,  "testMisc, zfindString failure loc G ");
	if (status != STATUS_OKAY) return status;

	stringCopy(cpath1, sizeof(cpath1), "abcdefghijklmnop", _TRUNCATE);
	stringCopy(cpath2, sizeof(cpath2), "abcdefghijklmnopQ", _TRUNCATE);
	result = zfindString(cpath1, (int)strlen(cpath1), cpath2, (int)strlen(cpath2));
	status = zcompareInts(ifltab, result, -1, 1,  "testMisc, zfindString failure loc H ");
	if (status != STATUS_OKAY) return status;


//	Input:		cpath:  Must be either '\0' (match all) or a valid pathname
//						/Sacramento/Natomas/Flow*/*/1Day/Run*/
//						Note '\0' == /*/*/*/*/*/*/,  returns 0 (match all)
//
//	 Number		Action				Examples
//		0		Match all (ignore)	/*/						
//		1		Match Exactly		/Flow/    
//		2		Starts with			/Flow*/
//		3		Ends with			/*Loc/
//		4		Contains			/*Flow*/
//
//	Retruns		0:	Match all (don't do compare)
//				1:	Do compare

	stringCopy(cpath1, sizeof(cpath1), "/Full Char Notes Irregular/*North/Flow*/*Jan*/*/F/", _TRUNCATE);
	result = zcatParsePath(cpath1, partAction, lengths, 
			apart, sizeof(apart),
			bpart, sizeof(bpart),
			cpart, sizeof(cpart),
			dpart, sizeof(dpart),
			epart, sizeof(epart),
			fpart, sizeof(fpart));

	//  Pathname compare
	stringCopy(cpath1, sizeof(cpath1), "/Full Char Notes Irregular/Red-North/Flow-out/01Jan1956/~1Day/F/", _TRUNCATE);
	result = zcatComparePath(cpath1, partAction, lengths, apart, bpart, cpart, dpart, epart, fpart,0);
	status = zcompareInts(ifltab, result, 1, 1,  "testMisc, zcatComparePath failure loc A ");
	if (status != STATUS_OKAY) return status;

	
	//  Pathname compare
	stringCopy(cpath1, sizeof(cpath1), "/Different/mERGED/Flow/01Jan1958/~1Day/F/", _TRUNCATE);
	result = zcatComparePath(cpath1, partAction, lengths, apart, bpart, cpart, dpart, epart, fpart,0);
	status = zcompareInts(ifltab, result, 0, 1,  "testMisc, zcatComparePath failure loc A ");
	if (status != STATUS_OKAY) return status;


		//  Pathname compare
	stringCopy(cpath1, sizeof(cpath1), "/Full Char Notes Irregular/Red-North/Out Flow/01Jan1956/~1Day/F/", _TRUNCATE);
	result = zcatComparePath(cpath1, partAction, lengths, apart, bpart, cpart, dpart, epart, fpart,0);
	status = zcompareInts(ifltab, result, 0, 1,  "testMisc, zcatComparePath failure loc A ");
	if (status != STATUS_OKAY) return status;

	
	return 0;
}
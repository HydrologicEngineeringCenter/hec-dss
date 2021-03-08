#include <stdio.h>
#include <stdlib.h>

#include "zdssKeys.h"

#include "hecdssInternal.h"


/**
*  Function:	zprintKeys
*
*  Use:			Private;  For debug
*
*  Description:	Print internal keys for debug use (only
*
*  Declaration: void zprintKeys(long long *ifltab);
*
*  Parameters:	long long ifltab
*					The file table array, similar to a handle number..
*
*
*	Remarks:	Prints to the message unit most of the keys (positions in ifltab)
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
*
*/

void zprintKeys(long long *ifltab)
{
	 char buff[50];

      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "knumVersion          %d", zdssKeys.knumVersion);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kmultiUserAccess        %d", zdssKeys.kmultiUserAccess);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kfileHeader        %d", zdssKeys.kfileHeader);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kdss         %d", zdssFileKeys.kdss);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "knumberRecords       %d", zdssFileKeys.knumberRecords);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kfileSize       %d", zdssFileKeys.kfileSize);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "klocked        %d", zdssKeys.klocked);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kdead        %d", zdssFileKeys.kdead);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kversion        %d", zdssFileKeys.kversion);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kcreateDate       %d", zdssFileKeys.kcreateDate);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "klastWriteTime       %d", zdssFileKeys.klastWriteTime);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kopenStatus       %d", zdssKeys.kopenStatus);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kmaxHash        %d", zdssFileKeys.kmaxHash);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kaddHashTableStart       %d", zdssFileKeys.kaddHashTableStart);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kbinsPerBlock       %d", zdssFileKeys.kbinsPerBlock);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kbinsRemainInBlock       %d", zdssFileKeys.kbinsRemainInBlock);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kbinSize       %d", zdssFileKeys.kbinSize);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kaddFirstBin       %d", zdssFileKeys.kaddFirstBin);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kaddNextEmptyBin       %d", zdssFileKeys.kaddNextEmptyBin);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kfilewkfileWritten %d", zdssKeys.kfileWritten);
      zmessage (ifltab, buff);
      //_snprintf_s(buff, sizeof(buff), _TRUNCATE, "kinfoQuality       %d", zdssInfoKeys.kinfoQuality);
      //zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kaddInfoLastPath       %d", zdssKeys.kaddInfoLastPath);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kinfoFlag       %d", zdssInfoKeys.kinfoFlag);
      zmessage (ifltab, buff);
     // _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kinfoCompression       %d", zdssInfoKeys.kinfoCompression);
      //zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kinfoStatus       %d", zdssInfoKeys.kinfoStatus);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kinfoPathnameLength       %d", zdssInfoKeys.kinfoPathnameLength);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kinfoPathname       %d", zdssInfoKeys.kinfoPathname);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kinfoReserved       %d", zdssInfoKeys.kinfoReserved);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kinfoValues1Address       %d", zdssInfoKeys.kinfoValues1Address);
      zmessage (ifltab, buff);
  //    _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kinfoNumberAndLogical       %d", zdssInfoKeys.kinfoNumberAndLogical);
  //    zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kinfoValues1Number       %d", zdssInfoKeys.kinfoValues1Number);
      zmessage (ifltab, buff);
//      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kinfoType       %d", zdssInfoKeys.kinfoType);
 //     zmessage (ifltab, buff);
 //     _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kinfoVersion        %d", zdssInfoKeys.kinfoVersion);
   //   zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kinfoProgram       %d", zdssInfoKeys.kinfoProgram);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kinfoLastWriteTime       %d", zdssInfoKeys.kinfoLastWriteTime);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kmessLevel        %d", zdssKeys.kmessLevel);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kmessHandle       %d", zdssKeys.kmessHandle);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kinfoInternalHeadAddress       %d", zdssInfoKeys.kinfoInternalHeadAddress);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kinfoInternalHeadNumber       %d", zdssInfoKeys.kinfoInternalHeadNumber);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kinfoHeader2Address       %d", zdssInfoKeys.kinfoHeader2Address);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kinfoHeader2Number       %d", zdssInfoKeys.kinfoHeader2Number);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kinfoUserHeadAddress       %d", zdssInfoKeys.kinfoUserHeadAddress);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kinfoUserHeadNumber       %d", zdssInfoKeys.kinfoUserHeadNumber);
      zmessage (ifltab, buff);
     // _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kinfoPrecision       %d", zdssInfoKeys.kinfoPrecision);
     // zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kbinAddCurrent        %d", zdssKeys.kbinAddCurrent);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kintegrityKey1         %d", zdssKeys.kintegrityKey1);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kintegrityKey2         %d", zdssKeys.kintegrityKey2);
	  zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kintegrityKey3         %d", zdssKeys.kintegrityKey3);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "pathnameHash       %d", zdssKeys.kpathnameHash);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "ktotalBins        %d", zdssFileKeys.ktotalBins);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "khashsUsed       %d", zdssFileKeys.khashsUsed);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kbinsOverflow       %d", zdssFileKeys.kbinsOverflow);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kmaxPathsOneHash       %d", zdssFileKeys.kmaxPathsOneHash);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kmaxPathsHashCode       %d", zdssFileKeys.kmaxPathsHashCode);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "khandle       %d", zdssKeys.khandle);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kexclusive   %d", zdssKeys.kexclusive);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "ksuser       %d", zdssKeys.ksuser);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kfilePassword       %d", zdssFileKeys.kfilePassword);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "klockAddressWord       %d", zdssFileKeys.klockAddressWord);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kswap        %d", zdssKeys.kswap);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "klocksDenied       %d", zdssKeys.klocksDenied);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kremote      %d", zdssKeys.kremote);
      zmessage (ifltab, buff);
      _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kdswap       %d", zdssKeys.kdswap);
      zmessage (ifltab, buff);
    //  _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kinfoJulianFirst       %d", zdssInfoKeys.kinfoJulianFirst);
     // zmessage (ifltab, buff);
    //  _snprintf_s(buff, sizeof(buff), _TRUNCATE, "kinfoJulianLast       %d", zdssInfoKeys.kinfoJulianLast);
    //  zmessage (ifltab, buff);
}


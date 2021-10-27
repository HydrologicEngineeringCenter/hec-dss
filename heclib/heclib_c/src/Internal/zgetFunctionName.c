#include "zdssMessages.h"
#include "hecdssInternal.h"

/**
*  Function:	zgetFunctionName
*
*  Use:			Semi-Private
*
*  Description:	Returns the character string name of a DSS function, given its ID number from zdssMessages.h
*
*  Declaration: char* zgetFunctionName(int functionID)
*
*  Parameters:	int functionID
*					The numeric ID given in file zdssMessages.h
*
*
*	Returns:	char *functionName
*					The #define name from zdssMessages.h
*
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

char* zgetFunctionName(int functionID)
{
	switch (functionID)	{
		case DSS_FUNCTION_zopen_ID:
			return DSS_FUNCTION_zopen;
		case DSS_FUNCTION_zclose_ID:
			return DSS_FUNCTION_zclose;
		case DSS_FUNCTION_zset_ID:
			return DSS_FUNCTION_zset;
		case DSS_FUNCTION_zget_ID:
			return DSS_FUNCTION_zget;
		case DSS_FUNCTION_zput_ID:
			return DSS_FUNCTION_zput;
		case DSS_FUNCTION_zgetBuff_ID:
			return DSS_FUNCTION_zgetBuff;
		case DSS_FUNCTION_zputBuff_ID:
			return DSS_FUNCTION_zputBuff;
		case DSS_FUNCTION_zmemoryGet_ID:
			return DSS_FUNCTION_zmemoryGet;
		case DSS_FUNCTION_zmemoryFree_ID:
			return DSS_FUNCTION_zmemoryFree;
		case DSS_FUNCTION_zreadDisk_ID:
			return DSS_FUNCTION_zreadDisk;
		case DSS_FUNCTION_zwriteDisk_ID:
			return DSS_FUNCTION_zwriteDisk;
		case DSS_FUNCTION_zpermRead_ID:
			return DSS_FUNCTION_zpermRead;
		case DSS_FUNCTION_zpermCreate_ID:
			return DSS_FUNCTION_zpermCreate;
		case DSS_FUNCTION_zpermWrite_ID:
			return DSS_FUNCTION_zpermWrite;
		case DSS_FUNCTION_zwriteEOF_ID:
			return DSS_FUNCTION_zwriteEOF;
		case DSS_FUNCTION_zinit_ID:
			return DSS_FUNCTION_zinit;
		case DSS_FUNCTION_zinitIfltab_ID:
			return DSS_FUNCTION_zinitIfltab;
		case DSS_FUNCTION_zlocking_ID:
			return DSS_FUNCTION_zlocking;
		case DSS_FUNCTION_znewFileSize_ID:
			return DSS_FUNCTION_znewFileSize;
		case DSS_FUNCTION_zgetFileSpace_ID:
			return DSS_FUNCTION_zgetFileSpace;
		case DSS_FUNCTION_zcheckKeys_ID:
			return DSS_FUNCTION_zcheckKeys;
		case DSS_FUNCTION_zflushToDisk_ID:
			return DSS_FUNCTION_zflushToDisk;
		case DSS_FUNCTION_zhash_ID:
			return DSS_FUNCTION_zhash;
		case DSS_FUNCTION_zcheck_ID:
			return DSS_FUNCTION_zcheck;
		case DSS_FUNCTION_zreadInfo_ID:
			return DSS_FUNCTION_zreadInfo;
		case DSS_FUNCTION_zcheckMultiUser_ID:
			return DSS_FUNCTION_zcheckMultiUser;
		case DSS_FUNCTION_zreadInternal_ID:
			return DSS_FUNCTION_zreadInternal;
		case DSS_FUNCTION_zwriteInternal_ID:
			return DSS_FUNCTION_zwriteInternal;
		case DSS_FUNCTION_zwriteNew_ID:
			return DSS_FUNCTION_zwriteNew;
		case DSS_FUNCTION_zwriteOld_ID:
			return DSS_FUNCTION_zwriteOld;
		case DSS_FUNCTION_zbinNew_ID:
			return DSS_FUNCTION_zbinNew;
		case DSS_FUNCTION_zbinUpdate_ID:
			return DSS_FUNCTION_zbinUpdate;
		case DSS_FUNCTION_zsetFile_ID:
			return DSS_FUNCTION_zsetFile;
		case DSS_FUNCTION_zquery_ID:
			return DSS_FUNCTION_zquery;
		case DSS_FUNCTION_zdelete_ID:
			return DSS_FUNCTION_zdelete;
		case DSS_FUNCTION_zundelete_ID:
			return DSS_FUNCTION_zundelete;
		case DSS_FUNCTION_zcopyFile_ID:
			return DSS_FUNCTION_zcopyFile;
		case DSS_FUNCTION_ztsStore_ID:
			return DSS_FUNCTION_ztsStore;
		case DSS_FUNCTION_ztsRetrieve_ID:
			return DSS_FUNCTION_ztsRetrieve;
		case DSS_FUNCTION_ztsStoreReg_ID:
			return DSS_FUNCTION_ztsStoreReg;
		case DSS_FUNCTION_ztsRegStoreBlock_ID:
			return DSS_FUNCTION_ztsRegStoreBlock;
		case DSS_FUNCTION_ztsRegReadBlock_ID:
			return DSS_FUNCTION_ztsRegReadBlock;
		case DSS_FUNCTION_ztsWriteBlock_ID:
			return DSS_FUNCTION_ztsWriteBlock;
		case DSS_FUNCTION_ztsRetrieveReg_ID:
			return DSS_FUNCTION_ztsRetrieveReg;
		case DSS_FUNCTION_ztsTrim_ID:
			return DSS_FUNCTION_ztsTrim;
		case DSS_FUNCTION_ztsStoreIrreg_ID:
			return DSS_FUNCTION_ztsStoreIrreg;
		case DSS_FUNCTION_ztsIrregReadBlock_ID:
			return DSS_FUNCTION_ztsIrregReadBlock;
		case DSS_FUNCTION_ztsIrregMergeBlocks_ID:
			return DSS_FUNCTION_ztsIrregMergeBlocks;
		case DSS_FUNCTION_ztsRetrieveIrreg_ID:
			return DSS_FUNCTION_ztsRetrieveIrreg;
		case DSS_FUNCTION_ztsIrregStoreBlock_ID:
			return DSS_FUNCTION_ztsIrregStoreBlock;
		case DSS_FUNCTION_ztsProcessTimes_ID:
			return DSS_FUNCTION_ztsProcessTimes;
		case DSS_FUNCTION_zcatalog_ID:
			return DSS_FUNCTION_zcatalog;
		case DSS_FUNCTION_zpdStore_ID:
			return DSS_FUNCTION_zpdStore;
		case DSS_FUNCTION_zpdRetrieve_ID:
			return DSS_FUNCTION_zpdRetrieve;
		case DSS_FUNCTION_zinquire_ID:
			return DSS_FUNCTION_zinquire;
		case DSS_FUNCTION_zsqueeze_ID:
			return DSS_FUNCTION_zsqueeze;
		case DSS_FUNCTION_ztextStore_ID:
			return DSS_FUNCTION_ztextStore;
		case DSS_FUNCTION_ztextRetrieve_ID:
			return DSS_FUNCTION_ztextRetrieve;
		case DSS_FUNCTION_zrename_ID:
			return DSS_FUNCTION_zrename;
		case DSS_FUNCTION_zcopyRecord_ID:
			return DSS_FUNCTION_zcopyRecord;
		case DSS_FUNCTION_zarrayStore_ID:
			return DSS_FUNCTION_zarrayStore;
		case DSS_FUNCTION_zarrayRetrieve_ID:
			return DSS_FUNCTION_zarrayRetrieve;
		case DSS_FUNCTION_zbinaryStore_ID:
			return DSS_FUNCTION_zbinaryStore;
			case DSS_FUNCTION_zbinaryRetrieve_ID:
			return DSS_FUNCTION_zbinaryRetrieve;
		case DSS_FUNCTION_zduplicateRecord_ID:
			return DSS_FUNCTION_zduplicateRecord;
		case DSS_FUNCTION_zcheckHashTable_ID:
			return DSS_FUNCTION_zcheckHashTable;
		case DSS_FUNCTION_zcheckInfo_ID:
			return DSS_FUNCTION_zcheckInfo;
		case DSS_FUNCTION_zcheckLinks_ID:
			return DSS_FUNCTION_zcheckLinks;
		case DSS_FUNCTION_zcheckPathnameBins_ID:
			return DSS_FUNCTION_zcheckPathnameBins;
		case DSS_FUNCTION_zcheckPathnames_ID:
			return DSS_FUNCTION_zcheckPathnames;
		case DSS_FUNCTION_zspatialTinStore_ID:
			return DSS_FUNCTION_zspatialTinStore;
		case DSS_FUNCTION_zspatialTinRetrieve_ID:
			return DSS_FUNCTION_zspatialTinRetrieve;
		case DSS_FUNCTION_zaliasAdd_ID:
			return DSS_FUNCTION_zaliasAdd;
		case DSS_FUNCTION_zaliasRemove_ID:
			return DSS_FUNCTION_zaliasRemove;
		case DSS_FUNCTION_zaliasUtil_ID:
			return DSS_FUNCTION_zaliasUtil;
		case DSS_FUNCTION_zlocationStore_ID:
			return DSS_FUNCTION_zlocationStore;
		case DSS_FUNCTION_zlocationRetrieve_ID:
			return DSS_FUNCTION_zlocationRetrieve;
		case DSS_FUNCTION_zread_ID:
			return DSS_FUNCTION_zread;
		case DSS_FUNCTION_zwrite_ID:
			return DSS_FUNCTION_zwrite;
		case DSS_FUNCTION_internalUtility_ID:
			return DSS_FUNCTION_internalUtility;
		case DSS_FUNCTION_ztsAggregate_ID:
			return DSS_FUNCTION_ztsAggregate;
		case DSS_FUNCTION_ztsDisaggregate_ID:
			return DSS_FUNCTION_ztsDisaggregate;
		case DSS_FUNCTION_zspatialGridStore_ID:
			return DSS_FUNCTION_zspatialGridStore;
		case DSS_FUNCTION_zspatialGridRetrieve_ID:
			return DSS_FUNCTION_zspatialGridRetrieve;

		case DSS_FUNCTION_javaNativeInterface_ID:
			return DSS_FUNCTION_javaNativeInterface;


		case DSS_FUNCTION_other_ID:
			return DSS_FUNCTION_other;


		default:
			return " ";
		}


}


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "heclib7.h"
#include "zdssKeys.h"
#include "zdssVals.h"
#include "zdssMessages.h"
#include "heclib.h"
#include "zerrorCodes.h"

/**
*  Function:	ztsRegStoreBlock
*
*  Use:			Private
*
*  Description:	Prepare a single record of regular-interval time series data to be written to disk
*					This includes merging with the existing record, if present, and computing
*					positions within arrays.  Data is written with ztsWriteBlock.
*
*  Declaration: 
*				int ztsRegStoreBlock(long long *ifltab, zStructTimeSeries *tss, const char *pathname, 					
*									int numberToStore,												
*									int *values, int valueSize, int valueElementSize, 
*									int *quality, int qualityElementSize,
*									int *notes, int inoteElementSize,
*									char *cnotes, int cnotesSize, int *lengthCNotes,
*									int *profileDepths, int profileDepthsSize, int profileDepthsNumber, 
									int *internalHeader, int internalHeaderNumber, 
*									int *userHeader, int userHeaderNumber, 
*									int positionInValues, int numberInBlock,
*									int startJulian, int startSeconds, int intervalSeconds, 
*									int julianBlockDate, int blockStartPosition, 
*									int boolReadBlock, int dataType, int storageFlag);
*
*  Parameters:	long long *ifltab
*					The file table array, similar to a handle number
*
*				const char* pathname 
*					The pathname of the record to store.  The pathname must be exact, but is case insensitive
*					(i.e., "Apart" is the same as "APART")
*
*				int numberToStore
*					The number of values to store for this block.  If quality or notes are
*					to be stored also, they must have the same number to store.
*
*				int *values
*					The values array.  Generally, values are floats or doubles.
*					This array must contain at least numberToStore number.
*
*				int valueSize
*					The number of words each value takes; generally 1 for floats, 2 for doubles.
*					However, if profiles are being stored, this is the number of words for the 
*					individual profile, e.g., for 10 double values in the profile, this would be 10 * 2 = 20.
*
*				int valueElementSize
*					The number of words each value element takes; generally 1 for floats, 2 for doubles.
*					Usually valueElementSize == valueSize.  However, if profiles are being stored, 
*					this is the value size for each profile element, e.g., 2 for doubles.
*
*				int *quality (Optional)
*					The quality array, if quality is to be stored.  
*
*				int qualityElementSize
*					The number of words for each quality element, either 0 (zero) if no quality is 
*					to be stored, or greater than 0.  Typically 1 or 2, but should handle any reasonable number.
*
*				int *notes (Optional)
*					If integer notes array, if integer notes are to be stored.  If character notes are to be stored,
*					this must be a dummy array, as only character or integer notes (or neither) can be stored, not both.
*
*				int inoteElementSize
*					The number of words for each integer note element, either 0 (zero) if integer notes are not  
*					to be stored, or greater than 0.  Typically 1 or 2, but should handle any reasonable number.
*					Must be zero if character notes are stored.
*
*				const char *cnotes (Optional)
*					A character array containing a null terminated string for each value, if character notes
*					are to be stored.  Each character note element is identified by a null termination, one per value
*					May not be used in combination with integer notes.
*
*				int cnotesSize
*					The size (dimension) of the cnote array.  This prevents over-running this array.
*					If character notes are not to be stored, this should be set to zero.
*
*				int *lengthCNotes (output)
*					The length of the cnote array used.  This provides the position for the next block write.
*
*				int *profileDepths (Optional)
*					If time series profiles are being stored, this is the "depths array".  Each full record has
*					only one depths array, which must be the same for all values.  For example, this might 
*					be 0., 5., 10., 15., 20., 25.  Use missing flags for periods where a value is not used.    
*					If no measurement is made at 25 for a time period, set that value to the missing data flag.
*					Depths are either floats or doubles, as defined by lengthEachValue.  Depths and values
*					must be the same type (float or double).
*
*				int profileDepthsSize
*					The number of words in the profileDepths array to store.  For the example above, if stored as doubles, this would be 12.
*					If profiles are not stored, set this to zero.
*
*				int profileDepthsNumber
*					The number of values in the profileDepths array.  For the example above, if stored as doubles, this would be 6. 
*					For non-profile stores, this is set to zero.
*
*				int *internalHeader
*					The internal header.
*
*				int internalHeaderNumber
*					The number of int 4 words to write from the internal header array.
*
*				int *userHeader
*					The user header.
*
*				int userHeaderNumber
*					The number of int 4 words to store in the user header array.
*
*				int positionInValues
*					The position in the values (and other) array for this block.  This is used to computer proper start time.
*
*				int numberInBlock
*					The number (size) of the block.  This number is independent on the number being stored, but is the 
*					number to store
*
*				int startJulian
*					The julian date (days since 1900) of the first value to be stored
*
*				int startSeconds
*					The start time, in seconds after midnight from startJulian, of the first value to be stored.
*
*				int intervalSeconds
*					The time interval, in seconds, for the regular interval data set.
*
*				int julianBlockDate
*					The julian date of the start of the block.  For example, if the data was monthly, this would be 
*					day 1 (e.g., March 1) and not the date associated with the first value (not March 31).
*
*				int blockStartPosition
*					The position of the first value in the block, compared to the start of the block
*
*				int boolReadBlock
*					A boolean flag indicating if the (exiting) block needs to be read (merged) first.
*					For example, if you are re-writing the complete block, you do not need to read the
*					block first.  Set to 1, if the block needs to be read first.
*
*				int dataType
*					The data type associated with this record.  See header file zdssVals.h for a list of data types.
*
*				int storageFlag
*					A flag indicating how to handle existing data on disk.  For regular interval data:
*						storageFlag = 0  Always replace data.
*						storageFlag = 1  Only replace missing data.
*						storageFlag = 2  Write regardless, even if all missing data (write a missing record)
*						storageFlag = 3  If a record is all missing, do not write it
*							and delete it from disk if it exists.
*						storageFlag = 4  Do not allow a missing input data to
*							replace a valid data piece.
*
*				
*				
*	Returns:	int status 
*					STATUS_OKAY		
*					error code - value contains a description of the error and where it occurred.
*					See zerrorDecode for descriptions.
*	
*	Remarks:	If only values are stored (most frequent case), then the code will save the position 
*					of the first value and last value (and not store leading or trailing missing)
*					If quality or notes (either inotes or cnotes) are stored, then the full block
*					will be stored with leading and trailing missing, although it will be compressed.
*
*					Example for daily data
*					First case:
*					May 2, 2000:    1234.5
*
*					Second case: Time	value		cnotes
*					Jan 1, 2000:	-901.0		'\0'
*					....
*					May 1, 2000:	-901.0		'\0'
*					May 2, 2000:	1234.5		'Gage was one foot below flood.'
*					May 3, 2000:	-901.0		'\0'
*					....
*					Dec 31, 2000:	-901.0		'\0'
*
*
*
*	Author:			Bill Charley
*	Date:			2014
*   Organization:	US Army Corps of Engineers, Hydrologic Engineering Center (USACE HEC)
*					All Rights Reserved
*
**/

int ztsRegStoreBlock(long long *ifltab, zStructTimeSeries *tss, const char *pathname,
					int numberToStore,										
					int *values, int valueSize, int valueElementSize,
					int *quality, int qualityElementSize,
					int *notes, int inoteElementSize,
					char *cnotes, int cnotesSize, int *lengthCNotes,
					int *profileDepths, int profileDepthsSize,  int profileDepthsNumber, 
					int *internalHeader,  
					int *userHeader, int userHeaderNumber, 
					int positionInValues, int numberInBlock,
					int startJulian, int startSeconds, int intervalSeconds, 
					int julianBlockDate, int blockStartPosition, 
					int boolReadBlock, int dataType, int storageFlag)
{	
	int firstValid;
	int lastValid;
	int julianFirstValue;
	int julianLastValue;
	int secondsFirstValue;
	int secondsLastValue;
	int posRelativeToStart;
	int boolAllMissing;
	int boolReplace;
	int status;
	int ipos;
	int jpos;
	int len;
	int start;
	int end;
	int ival;
	int i;
	long long *info;
	
	char messageString[80];
	int lenData;
	int lenQuality;
	int lenNotes;

	int internalHeaderRead[INT_HEAD_SIZE];
	int sizeInternalHeaderRead = INT_HEAD_SIZE;
	int lengthInternalHeaderRead;
	int boolFound;

	int internalHeaderReadNumber;
	int *userHeaderRead;
	int userHeaderNumberRead;
	int userHeaderSize;

	int *valuesRead;
	int valueReadSize;
	int *qualityRead;
	int qualityReadSize;
	int *notesRead;
	int notesReadSize;
	char *cnotesRead;
	int cnotesReadLen;
	int cnotesReadSize;	
	char *cnotesToStore;
	int cnotesToStoreLen;
	char *cnotesBuff;	
	int cnotesBuffSize;
	int valuesReadLength;
	int qualityReadLength=0;
	int notesReadLength=0;

	int iposValues;
	int jposValues;
	int iposQuality;
	int jposQuality;
	int iposiNotes;
	int jposiNotes;
	int iposcNotes;
	int jposcNotes;

	int *headerRead3;
	int sizeHeaderRead3 = 0;
	int numberHeaderRead3;
	

	//////////////  FIX ME - do I need buff?????????
	int buffer[500];
	long long bufferControl[4];
	for (i=0; i<4; i++) bufferControl[i] = 0;
	bufferControl[BUFF_SIZE] = 500;


	//  CHECK ME - NEED to be sure data types (RTS vs profile) match!!!

	if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_USER_DIAG)) {
		zmessageDebug(ifltab, DSS_FUNCTION_ztsRegStoreBlock_ID, "Enter, Pathname: ", pathname);
		zmessageDebugInt(ifltab, DSS_FUNCTION_ztsRegStoreBlock_ID, "Number to store: ", numberToStore);
		lenData = numberToStore * valueSize;
		lenQuality = numberToStore * qualityElementSize;
		lenNotes = numberToStore * inoteElementSize;
		_snprintf_s(messageString, sizeof(messageString), _TRUNCATE, 
			"%d, Length quality flags: %d, length notes: %d,  length C notes: %d", 
			lenData, lenQuality, lenNotes, cnotesSize);
		zmessageDebug(ifltab, DSS_FUNCTION_ztsRegStoreBlock_ID, "Length data array: ", messageString);
	}
	


	status = STATUS_OKAY;
	julianFirstValue = 0;
	julianLastValue = 0;

	//  Set new space memory pointers to 0
	valuesRead = 0;
	qualityRead = 0;
	notesRead = 0;
	cnotesRead = 0;
	cnotesReadSize = 0;
	cnotesToStore = 0;	
	cnotesBuff = 0;
	headerRead3 = 0;
	userHeaderRead = 0;
	userHeaderNumberRead = 0;
	

	//  Check what happens when we store 1 or 2 values as missing to replace data on disk
	//  Determine any leading missing values
	boolAllMissing = ztsFirstLastValidValues(values, valueElementSize, 
										profileDepthsNumber, numberToStore, 
										quality, qualityElementSize,
										notes, inoteElementSize,
										cnotes, cnotesSize,
										&firstValid, &lastValid);

	//  Compute the start date/time (the time of the first value we are storing in this block)
	posRelativeToStart = positionInValues + firstValid;
	incrementTime(intervalSeconds, posRelativeToStart, startJulian, startSeconds,
				 &julianFirstValue, &secondsFirstValue);
	//  And the date/time of the last value we're storing	
	posRelativeToStart = positionInValues + lastValid;
	incrementTime(intervalSeconds, posRelativeToStart, startJulian, startSeconds,
				  &julianLastValue, &secondsLastValue);

	if (cnotesSize > 0) {
		//  Figure out how much we will be writing for C notes. 
		cnotesToStoreLen = copyLines(cnotesToStore, (size_t)0, cnotes, (size_t)cnotesSize, numberToStore);
		cnotesToStore = cnotes;
	}
	else {
		cnotesToStoreLen = 0;
	}
	*lengthCNotes  = cnotesToStoreLen;

	if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
		ztsDateMessage(ifltab, DSS_FUNCTION_ztsRegStoreBlock_ID, "Time for first value:  ", julianFirstValue, secondsFirstValue);
		ztsDateMessage(ifltab, DSS_FUNCTION_ztsRegStoreBlock_ID, "Time for last  value:  ", julianLastValue, secondsLastValue);
	}
	
	
	//  Do we need to see if this block already exists, so that we have to merge the data sets?
	boolFound = 0;
	if (boolReadBlock) {
		//  Because we might read the internal header right after the info block,
		//  read info manually, using a buffer area to read the internal header at the same time
		status = zreadInfo (ifltab, pathname, 0);		
		if (zisError(status)) {
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_ztsRegStoreBlock_ID);
		} 
		if (status == STATUS_RECORD_FOUND) {			
			info = (long long *)ifltab[zdssKeys.kinfo];	
			boolFound = 1;
			//  Keep location of info
			ifltab[zdssKeys.kinfoSize] = zdssVals.infoSize + numberLongsInBytes((int)ifltab[zdssKeys.klenLastPath]);;
			ifltab[zdssKeys.kiftPathHash] = ifltab[zdssKeys.kpathnameHash];
			ifltab[zdssKeys.kinfoAddress] = ifltab[zdssKeys.kaddInfoLastPath];						

			//  Now read internal header array 
			lengthInternalHeaderRead = (int)info[zdssInfoKeys.kinfoInternalHeadNumber];
			if (lengthInternalHeaderRead > sizeInternalHeaderRead) lengthInternalHeaderRead = sizeInternalHeaderRead;
			status = zget(ifltab, info[zdssInfoKeys.kinfoInternalHeadAddress], internalHeaderRead, lengthInternalHeaderRead, 1);
			if (zisError(status)) {
				return zerrorUpdate(ifltab, status, DSS_FUNCTION_ztsRegStoreBlock_ID);
			} 
			if (bigEndian()) {
				zswitchInts(internalHeaderRead, INT_HEAD_cnotesLength + 1);
			}
			if (info[zdssInfoKeys.kinfoLogicalNumber] < 1) {
				//  Very rare (not storing data, but quality or notes)
				boolReadBlock = 0;
			}
		}
	}
	
		
///////   FIX ME - move the description header to AFTER the internal header (so that we can read internal with info block)
	if ((boolReadBlock && boolFound) && (storageFlag != 1) && (storageFlag !=4)) {
		//  Also, don't read or expand block if we are replacing the same (lengths)
		//  Need to compare missing values that are in data set and block, and quality and note lengths
	
		//  Are all the lengths the same?
		if ((valueSize == internalHeaderRead[INT_HEAD_valueSize]) && (qualityElementSize == internalHeaderRead[INT_HEAD_qualityElementSize]) 
			 && (inoteElementSize == internalHeaderRead[INT_HEAD_inotesElementSize]) && (cnotesSize == internalHeaderRead[INT_HEAD_cnotesLength])) {
			//  Lengths the same... compare the first and last missing to see if we are 
			//  writing the same number of values (in the same spot)
			if ((qualityElementSize == 0) && (inoteElementSize == 0) && (cnotesSize == 0)) {
				//  Use the position of the first non missing and last non missing vlaues			
				start = blockStartPosition + firstValid;
				end = start + lastValid - firstValid;;												
			}
			else {
				//  Use the position of the first non missing and last non missing vlaues						
				start = blockStartPosition;
				end = blockStartPosition + numberToStore - 1;					
			}					
			if ((start == internalHeaderRead[INT_HEAD_blockStartPosition]) && (end == internalHeaderRead[INT_HEAD_blockEndPosition])) {
				//  We are replacing the data (same size, lengths).
				//  No need to read in block on disk and merge.
				if ((storageFlag == 0) || (storageFlag == 2)) {
					boolReadBlock = 0;
				}
			}
			else {
				boolAllMissing = 0;
			}
		}
	}
	//	If we are storing quality or notes, always store a full block
	//  (and let compression shrink)
	if (!boolReadBlock && ((qualityElementSize > 0) || (inoteElementSize > 0) || (cnotesSize > 0))) {
		boolReadBlock = 1;
	}
	
	if (boolReadBlock) {
		//  Read in the existing block and merge the data

		//  First, get space for arrays 					
		if (valueSize > 0) {
			valuesReadLength = numberInBlock * valueSize;
			valuesRead = (int *)calloc((size_t)valuesReadLength, WORD_SIZE);

			if (!valuesRead) {
				return zerrorProcessing(ifltab, DSS_FUNCTION_ztsRegStoreBlock_ID, 
										zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, valuesReadLength, 0, 
										zdssErrorSeverity.MEMORY_ERROR, pathname, "values array");
			}
		}
		else {
			valuesRead = 0;
		}
	
		if (qualityElementSize > 0) {
			qualityReadLength = numberInBlock * qualityElementSize;
			qualityRead = (int *)calloc((size_t)qualityReadLength, WORD_SIZE);
			if (!qualityRead) {
				return zerrorProcessing(ifltab, DSS_FUNCTION_ztsRegStoreBlock_ID, 
										zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, qualityReadLength, 0, 
										zdssErrorSeverity.MEMORY_ERROR, pathname, "quality array");
			}
		}
		else {
			qualityRead = 0;
		}
	
		if (inoteElementSize > 0) {
			notesReadLength = numberInBlock * inoteElementSize;
			notesRead = (int *)calloc((size_t)notesReadLength, WORD_SIZE);
			if (!notesRead) {
				return zerrorProcessing(ifltab, DSS_FUNCTION_ztsRegStoreBlock_ID, 
										zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, notesReadLength, 0, 
										zdssErrorSeverity.MEMORY_ERROR, pathname, "notes array");
			}
		}
		else {
			notesRead = 0;
		}
		//  Get the amount of space used on disk				
		if (boolFound && (internalHeaderRead[INT_HEAD_cnotesLength] > 0)) {
			cnotesReadSize = internalHeaderRead[INT_HEAD_cnotesLength] + internalHeaderRead[INT_HEAD_blockStartPosition] + 
				(numberInBlock - internalHeaderRead[INT_HEAD_blockEndPosition]); 										
			cnotesRead = (char *)calloc((size_t)cnotesReadSize, CHAR_SIZE);
		}
		else if (cnotesSize > 0) {
			cnotesReadSize = numberInBlock;
			cnotesRead = (char *)calloc((size_t)cnotesReadSize, CHAR_SIZE);
		}
		else {
			cnotesReadSize = 0;
			cnotesRead = 0;
		}
		if ((cnotesReadSize > 0) && !cnotesRead) {
			return zerrorProcessing(ifltab, DSS_FUNCTION_ztsRegStoreBlock_ID, 
									zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, cnotesReadSize, 0, 
									zdssErrorSeverity.MEMORY_ERROR, pathname, "cnotes array");
		}

		if (profileDepthsNumber > 0) {
			//  FIX me - do I need to read this????
			//headerRead3, sizeHeaderRead3
			sizeHeaderRead3 = 0;
		}

		//  If the user header is not being stored, and there is one on disk,
		//  read it and store (i.e., keep an existing user header)
		if (userHeaderNumber > 0) {
			//  No, we are over writing any exisiting user header
			userHeaderSize = 0;
		}
		else {
			//  Check for existing and keep that
			userHeaderSize = 1;
		}

		status = ztsRegReadBlock(ifltab, pathname, boolFound,  
			buffer, bufferControl,
			valuesRead, valuesReadLength, valueElementSize, &valueReadSize,
			qualityRead, qualityReadLength, qualityElementSize, &qualityReadSize,
			notesRead, notesReadLength, inoteElementSize, &notesReadSize,
			cnotesRead, cnotesReadSize, &cnotesReadLen,			
			headerRead3, profileDepthsNumber, 
			sizeHeaderRead3, &numberHeaderRead3,
			internalHeaderRead, sizeInternalHeaderRead, &internalHeaderReadNumber,
			userHeaderRead, userHeaderSize, &userHeaderNumberRead,
			numberInBlock, 0); 

		
		if (zisError(status)) {		
			//  Free any space malloced
			if (valuesRead)  free(valuesRead);
			if (qualityRead) free(qualityRead);
			if (notesRead)   free(notesRead);
			if (cnotesRead)  free(cnotesRead);
			return zerrorUpdate(ifltab, status, DSS_FUNCTION_ztsRegStoreBlock_ID);
		}
		//  Now that we've read the data, merge the two sets together

		//  Is a storage flag set so that we need to compare values to do the merge?
		if ((storageFlag == 1) || (storageFlag == 4)) {
			boolReplace = 0;

			if ((valueSize > 0) && (valuesRead))  {
				//  storageFlag = 1  Only replace missing data.
				if (storageFlag == 1) {
					ipos = blockStartPosition * valueSize;
					assert ((ipos + (numberToStore * valueSize)) <= valuesReadLength);
					for (i=0; i<numberToStore; i++) {					
						if (zisMissing(&valuesRead[ipos], valueSize)) {
							boolReplace = 1;
							break;
						}												
						ipos += valueSize;
					}
				}
				else {  //  if (storageFlag == 4) {
					//  storageFlag = 4  Do not allow a missing input data to replace a valid data
					jpos = 0;
					for (i=0; i<numberToStore; i++) {					
						if (!zisMissing(&values[jpos], valueSize)) { 
							boolReplace = 1;
							break;
						}	
						jpos += valueSize;
					}
				}
				if (!boolReplace) {
					//  No replacement.
					if (valuesRead)  free(valuesRead);
					if (qualityRead) free(qualityRead);
					if (notesRead)   free(notesRead);
					if (cnotesRead)  free(cnotesRead);
					if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
						if (storageFlag == 1) {
							zmessageDebug(ifltab, DSS_FUNCTION_ztsRegStoreBlock_ID, "Storage flag 1, no missing values read", "");
						}
						else {
							zmessageDebug(ifltab, DSS_FUNCTION_ztsRegStoreBlock_ID, "Storage flag 4, all missing values to be stored", "");
						}
						zmessageDebug(ifltab, DSS_FUNCTION_ztsRegStoreBlock_ID, "No replacement for pathname: ", pathname);
					}
					return STATUS_OKAY;
				}
			}
		}
		
		
		if ((storageFlag == 0) || (storageFlag == 2) || (storageFlag == 3)) {
			//  Usual case

			if ((valueSize > 0) && (valuesRead))  {

				ipos = blockStartPosition * valueSize;
				assert ((ipos + (numberToStore * valueSize)) <= valuesReadLength);
				convertDataArray(values, &valuesRead[ipos], numberToStore, valueSize, valueSize);
				//  Set the values pointer to the values array read
				values = valuesRead; 
			}
			
			if ((qualityElementSize > 0) && (qualityRead))  {
				ipos = blockStartPosition * qualityElementSize;
				assert ((ipos + (numberToStore * qualityElementSize)) <= qualityReadLength);
				convertIntArray(quality, &qualityRead[ipos], numberToStore, qualityElementSize, qualityElementSize);
				quality = qualityRead;
			}

			if ((inoteElementSize > 0) && (notesRead))  {
				ipos = blockStartPosition * inoteElementSize;
				assert ((ipos + (numberToStore * inoteElementSize)) <= notesReadLength);
				convertIntArray(notes, &notesRead[ipos], numberToStore, inoteElementSize, inoteElementSize);	
				notes = notesRead;
			}
			cnotesToStoreLen = 0;
			if ((cnotesReadLen > 0) || (cnotesSize > 0)) {						
				//  In this area of the code, we are working with a full block (can be missing at beginning or end)
				cnotesBuffSize = cnotesSize + cnotesReadLen;
				cnotesBuff = (char *)calloc((size_t)cnotesBuffSize, CHAR_SIZE);
				if (!cnotesBuff) {
					return zerrorProcessing(ifltab, DSS_FUNCTION_ztsRegStoreBlock_ID, 
											zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, cnotesBuffSize, 0, 
											zdssErrorSeverity.MEMORY_ERROR, pathname, "cnotes array");
				}
				cnotesToStore = cnotesBuff;						
				if ((cnotesReadLen > 0) && (cnotesSize > 0)) {
					//  Both notes read and notes to store.
					//  Get the starting position in cnotesRead relative to cnotes passed in.
					if (blockStartPosition > 0) {
						cnotesToStoreLen = copyLines(cnotesToStore, (size_t)cnotesBuffSize, cnotesRead, (size_t)cnotesReadLen, blockStartPosition);
					}
					//  Now copy in our notes (passed in)
					cnotesToStoreLen += copyLines(&cnotesToStore[cnotesToStoreLen], (size_t)(cnotesBuffSize - cnotesToStoreLen), cnotes, (size_t)cnotesSize, numberToStore);
					//  And copy any remaining notes read in							
					len = blockStartPosition + numberToStore;							
					if (len < numberInBlock) {		
						//  We need to find out where in cnotesRead we are (remember, cnotesRead is for the entire block)
						ipos = copyLines(cnotesToStore, (size_t)0, cnotesRead, (size_t)cnotesReadLen, len);
						cnotesToStoreLen += copyLines(&cnotesToStore[cnotesToStoreLen], (size_t)(cnotesBuffSize - cnotesToStoreLen),
							&cnotesRead[ipos], (size_t)(cnotesReadLen - ipos), (numberInBlock - len));
					}
				}
				else if (cnotesReadLen > 0)  {
					//  Notes read, but none to store.
					//  If the user does not pass in cnotes, and they are already there, assume an update.
					//  To delete notes, an array of null chars has to be passed in; 
					//  or the record may be deleted and writing without notes
					//  The notes array read should be good with the correct length.
					cnotesToStore = cnotesRead;
					cnotesToStoreLen = cnotesReadLen;
				}
				else {
					//  No notes read, but notes to store
					//  Array is already null filled, so we only need to copy in the notes to store.
					cnotesToStoreLen += copyLines(&cnotesToStore[blockStartPosition], (size_t)(cnotesBuffSize - blockStartPosition), cnotes, (size_t)cnotesSize, numberToStore);
					//  And copy any remaining notes read in							
					len = blockStartPosition + numberToStore;
					cnotesToStoreLen += numberInBlock - len;							
				}
			}
		}
		else {
			
			//  Special storage flag
			//  We need to compare each value
			//  storageFlag = 1  Only replace missing data.
			//  storageFlag = 4  Do not allow a missing input data to replace a valid data
			if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
				zmessageDebug(ifltab, DSS_FUNCTION_ztsRegStoreBlock_ID, "Storage flag condition found.  Processing data for pathname: ", pathname);
			}
			iposValues = blockStartPosition * valueSize;
			jposValues = 0;
			iposQuality = blockStartPosition * qualityElementSize;
			jposQuality = 0;
			iposiNotes = blockStartPosition * inoteElementSize;
			jposiNotes = 0;
			cnotesToStoreLen = 0;
			iposcNotes = 0;
			jposcNotes = 0;
			if ((cnotesReadLen > 0) || (cnotesSize > 0)) {						
				//  In this area of the code, we are working with a full block (can be missing at beginning or end)
				cnotesBuffSize = cnotesSize + cnotesReadLen;
				cnotesBuff = (char *)calloc((size_t)cnotesBuffSize, CHAR_SIZE);
				if (!cnotesBuff) {
					return zerrorProcessing(ifltab, DSS_FUNCTION_ztsRegStoreBlock_ID, 
											zdssErrorCodes.CANNOT_ALLOCATE_MEMORY, cnotesBuffSize, 0, 
											zdssErrorSeverity.MEMORY_ERROR, pathname, "cnotes array");
				}
				cnotesToStore = cnotesBuff;	
				if ((cnotesReadLen > 0) && (cnotesSize > 0)) {
					//  Both notes read and notes to store.
					//  Get the starting position in cnotesRead relative to cnotes passed in.
					if (blockStartPosition > 0) {
						cnotesToStoreLen = copyLines(cnotesToStore, (size_t)cnotesBuffSize, cnotesRead, (size_t)cnotesReadLen, blockStartPosition);
						jposcNotes = cnotesToStoreLen;
					}
				}
				else if ((cnotesReadLen > 0) && (cnotesSize == 0)) {
					//  Notes read, but none to store.
					//  If the user does not pass in cnotes, and they are already there, assume an update.
					//  To delete notes, an array of null chars has to be passed in; 
					//  or the record may be deleted and writing without notes
					//  The notes array read should be good with the correct length.
					cnotesToStore = cnotesRead;
					cnotesToStoreLen = cnotesReadLen;
				}
				else if ((cnotesReadLen == 0) && (cnotesSize> 0)) { 
					//  No notes read, but notes to store
					//  Array is already null filled, so we only need to copy in the notes to store.
					cnotesToStoreLen += copyLines(&cnotesToStore[blockStartPosition], (size_t)(cnotesBuffSize - blockStartPosition), cnotes, (size_t)cnotesSize, numberToStore);
					//  And copy any remaining notes read in							
					len = blockStartPosition + numberToStore;
					cnotesToStoreLen += numberInBlock - len;							
				}
			}
			
			for (i=0; i<numberToStore; i++) {
				boolReplace = 0;
				if ((valueSize > 0) && (valuesRead))  {
					assert ((iposValues + ((numberToStore - i) * valueSize)) <= valuesReadLength);
					if (storageFlag == 1) {
						if (zisMissing(&valuesRead[iposValues], valueSize)) {
							boolReplace = 1;
						}						
					}
					else if (storageFlag == 4) {
						if (!zisMissing(&values[jposValues], valueSize)) {
							boolReplace = 1;
						}
					}
					if (boolReplace) {	
						convertDataArray(&values[jposValues], &valuesRead[iposValues], 1, valueSize, valueSize);
					}
					iposValues += valueSize;
					jposValues += valueSize;
				}
				if ((qualityElementSize > 0) && (qualityRead))  {
					if (boolReplace) {
						assert ((iposQuality +  ((numberToStore - i) * qualityElementSize)) <= qualityReadLength);
						convertIntArray(&quality[jposQuality], &qualityRead[iposQuality], 1, qualityElementSize, qualityElementSize);
					}
					iposQuality += qualityElementSize;
					jposQuality += qualityElementSize;
				}
				if ((inoteElementSize > 0) && (notesRead))  {
					if (boolReplace) {
						assert ((iposiNotes +  ((numberToStore - i) * inoteElementSize)) <= notesReadLength);
						convertIntArray(&notes[jposiNotes], &notesRead[iposiNotes], 1, inoteElementSize, inoteElementSize);
					}
					iposiNotes += inoteElementSize;
					jposiNotes += inoteElementSize;					
				}
				if ((cnotesReadLen > 0) && (cnotesSize > 0)) {
					//  Copy in our notes (passed in)
					if (boolReplace) {
						len = copyLines(&cnotesToStore[cnotesToStoreLen], (size_t)(cnotesBuffSize - cnotesToStoreLen), 
							&cnotes[iposcNotes], (size_t)(cnotesSize - iposcNotes), 1);
						cnotesToStoreLen += len;
						iposcNotes += len;
						jposcNotes += (int)strlen(&cnotesRead[jposcNotes]) + 1;  //  + 1 is to include the null char also
					}
					else {
						len = copyLines(&cnotesToStore[cnotesToStoreLen], (size_t)(cnotesBuffSize - cnotesToStoreLen), 
							&cnotesRead[jposcNotes], (size_t)(cnotesReadLen - jposcNotes), 1);
						cnotesToStoreLen += len;
						jposcNotes += len;
						iposcNotes += (int)strlen(&cnotes[iposcNotes]) + 1;
					}					
				}
			}
			
			//  Set the values pointer to the values array read
			if ((valueSize > 0) && (valuesRead))  {
				values = valuesRead;
			}
			if ((qualityElementSize > 0) && (qualityRead))  {
				quality = qualityRead;
			}
			if ((inoteElementSize > 0) && (notesRead))  {
				notes = notesRead;
			}
			if ((cnotesReadLen > 0) && (cnotesSize > 0)) {
				//  copy any remaining notes read in							
				len = blockStartPosition + numberToStore;							
				if (len < numberInBlock) {
					//  We need to find out where in cnotesRead we are (remember, cnotesRead is for the entire block)
					ipos = copyLines(cnotesToStore, (size_t)0, cnotesRead, (size_t)cnotesReadLen, len);
					cnotesToStoreLen += copyLines(&cnotesToStore[cnotesToStoreLen], (size_t)(cnotesBuffSize - cnotesToStoreLen),
						&cnotesRead[ipos], (size_t)(cnotesReadLen - ipos), (numberInBlock - len));
				}
			}
		}
		

		//  Now we need to recompute missing positions, since we're working with the full, modified, block
		//  Determine any leading missing values
		boolAllMissing = ztsFirstLastValidValues(values, valueElementSize, 
											profileDepthsNumber, numberInBlock, 
											quality, qualityElementSize,
											notes, inoteElementSize,
											cnotesToStore, cnotesToStoreLen,
											&firstValid, &lastValid);
		///////////////////  FIX ME - use time offset!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		incrementTime(intervalSeconds, firstValid, julianBlockDate, intervalSeconds,
						&julianFirstValue, &secondsFirstValue);
		//  And the date/time of the last value we're storing	
		incrementTime(intervalSeconds, lastValid, julianBlockDate, intervalSeconds,
						&julianLastValue, &secondsLastValue);
		
		blockStartPosition = 0;
		numberToStore = numberInBlock;
	}

	//  If the record is all missing, should we save or delete it?
	if (boolAllMissing) {
		if (storageFlag == 3) {			
		}
		else {
			zquery("empty", messageString, sizeof(messageString), &ival);
			if (ival) boolAllMissing = 0;
			if (storageFlag == 2) boolAllMissing = 0;
		}
	}

	if (!boolAllMissing) {
		//  Do we need to write this block (is it all missing), or
		//  more so, do we need to delete the block on disk?
		//  Note - If quality flags or notes are used, we will always
		//  write the record, regardless if the data itself is all missing.

		//  Now store the block in the DSS file
		// --------------   Main Write ------------------
		ifltab[zdssKeys.kdataFirstDate] = i4toi8(julianFirstValue, secondsFirstValue);
		ifltab[zdssKeys.kdataLastDate] = i4toi8(julianLastValue, secondsLastValue);

		if ((qualityElementSize > 0) || (inoteElementSize > 0) || (cnotesSize > 0)) {
			//  If quality or notes, store the full block
			numberToStore = numberInBlock;
			blockStartPosition = 0;
			firstValid = 0;
		}
		else {
			//  Otherwise, only store valid values
			numberToStore = lastValid - firstValid + 1;
			blockStartPosition += firstValid;
		}
		//  Are we writing a user header?
		//  If not, and one exists, store what we just read (i.e., keep an existing user header)
		if (userHeaderNumberRead > 0) {
			userHeader = userHeaderRead;
			userHeaderNumber = userHeaderNumberRead;
		}

		internalHeader[INT_HEAD_blockStartPosition] = blockStartPosition;
		internalHeader[INT_HEAD_blockEndPosition] = blockStartPosition + numberToStore - 1;
		ipos = firstValid * valueSize;

		zStructRecordBasics* rb = zstructRecordBasicsNew(pathname);
		status = zgetRecordBasics(ifltab, rb);

		int recordType = rb->recordType;
		zstructFree(rb);

		if (status == STATUS_RECORD_FOUND && recordType == DATA_TYPE_RTD
			&& dataType == DATA_TYPE_RTS
			&& tss->floatValues
			&& tss->doubleValues == NULL) {
			// Support writing floats into a double record (calling ztsStore recursively) 

			zStructTimeSeries* tsClone = zstructTsClone(tss, pathname);

			tsClone->startJulianDate = julianBlockDate + (blockStartPosition / (86400 / tsClone->timeIntervalSeconds));
			tsClone->startTimeSeconds = (blockStartPosition + 1) * tsClone->timeIntervalSeconds % 86400;

			free(tsClone->floatValues);
			tsClone->floatValues = NULL;

			tsClone->doubleValues = calloc(numberToStore, sizeof(double));
			tsClone->allocated[zSTRUCT_TS_doubleValues] = 1;
			tsClone->dataType = DATA_TYPE_RTD;
			tsClone->numberValues = numberToStore;
			if (tsClone->doubleValues) {
				convertDataArray((void*)&values[ipos], (void*)tsClone->doubleValues, numberToStore, 1, 2);

				status = ztsStore(ifltab, tsClone, storageFlag);
				zstructFree(tsClone);
			}
			else
			{
				if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
					zmessageDebug(ifltab, DSS_FUNCTION_ztsRegStoreBlock_ID, "Memory Error storing ", pathname);
				}
			}
		}
		else {
			status = ztsWriteBlock(ifltab, tss, pathname,
				&ival, 0, numberToStore,
				&values[ipos], valueSize,
				quality, qualityElementSize,
				notes, inoteElementSize,
				cnotesToStore, cnotesToStoreLen,
				profileDepths, profileDepthsSize,
				internalHeader,
				userHeader, userHeaderNumber,
				0, numberInBlock,
				dataType);
		}
	}
	else {
		//  All missing
		// Delete record?
		if (boolFound) {
			if (zmessageLevel(ifltab, MESS_METHOD_TS_WRITE_ID, MESS_LEVEL_INTERNAL_DIAG_1)) {
				zmessageDebug(ifltab, DSS_FUNCTION_ztsRegStoreBlock_ID, "All missing values for storage flag condition found.  Deleting record: ", pathname);
			}
			status = zdelete(ifltab, pathname);			
			if (zisError(status)) {						
				status = zerrorUpdate(ifltab, status, DSS_FUNCTION_ztsRegStoreBlock_ID);
			}
		}
	}


	//  Free any space malloced
	if (valuesRead)  free(valuesRead);
	if (qualityRead) free(qualityRead);
	if (notesRead)   free(notesRead);
	if (cnotesRead)  free(cnotesRead);
	if (cnotesBuff)  free(cnotesBuff);
	if (headerRead3) free(headerRead3);  
	if (userHeaderRead) free(userHeaderRead);

	return status;
}

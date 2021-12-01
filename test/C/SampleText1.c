#include <stdio.h>
#include <string.h>
#include "heclib.h"

// If MacOS, use hec_zopen instead of stdio::zopen
#ifdef __APPLE__
#define zopen hec_zopen
#endif

//  An example of storing and retrieving a single text string

int main()
{
	long long ifltab[250];
	char textStuff[] = "This is a text message that is being written to HEC-DSS";
	zStructText *textStruct;
	int status;

	//  Open the DSS file.  
	status = zopen(ifltab, "SampleText1.dss");
	if (status != STATUS_OKAY) return status;

	//  Store the text string
	textStruct = zstructTextStringNew("/Group/Location/Message/d/e/f/", textStuff);
	status = ztextStore(ifltab, textStruct);
	zstructFree(textStruct);
	if (status != STATUS_OKAY) return status;

	//  Retrieve
	textStruct = zstructTextNew("/Group/Location/Message/d/e/f/");
	status = ztextRetrieve(ifltab, textStruct);

	printf("text ==>%s<==\n", textStruct->textString);
	if (strcmp(textStuff, textStruct->textString) != 0)
	{
		printf("\nstrings are different...");
		return -1;
	}
	zstructFree(textStruct);
	if (status != STATUS_OKAY) return status;

	//  When all done (near end of program), close the file
	zclose(ifltab);

	return 0;
}

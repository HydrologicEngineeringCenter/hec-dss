#include "pch.h"
#include "NativeLocationWrapper.h"

double GetXOrdinate(zStructLocation* loc)
{
	return loc->xOrdinate;
}

double GetYOrdinate(zStructLocation* loc)
{
	return loc->yOrdinate;
}

double GetZOrdinate(zStructLocation* loc)
{
	return loc->zOrdinate;
}

int GetCoordinateSystem(zStructLocation* loc)
{
	return loc->coordinateSystem;
}

int GetCoordinateID(zStructLocation* loc)
{
	return loc->coordinateID;
}

int GetHorizontalUnits(zStructLocation* loc)
{
	return loc->horizontalUnits;
}

int GetHorizontalDatum(zStructLocation* loc)
{
	return loc->horizontalDatum;
}

int GetVerticalUnits(zStructLocation* loc)
{
	return loc->verticalUnits;
}

int GetVerticalDatum(zStructLocation* loc)
{
	return loc->verticalDatum;
}

BSTR GetTimeZoneName(zStructLocation* loc)
{
	return ANSItoBSTR(loc->timeZoneName);
}

BSTR GetSupplemental(zStructLocation* loc)
{
	return ANSItoBSTR(loc->supplemental);
}

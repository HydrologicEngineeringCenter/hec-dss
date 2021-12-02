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

void SetXOrdinate(zStructLocation* loc, double value)
{
	loc->xOrdinate = value;
}

void SetYOrdinate(zStructLocation* loc, double value)
{
	loc->yOrdinate = value;
}

void SetZOrdinate(zStructLocation* loc, double value)
{
	loc->zOrdinate = value;
}

void SetCoordinateSystem(zStructLocation* loc, int value)
{
	loc->coordinateSystem = value;
}

void SetCoordinateID(zStructLocation* loc, int value)
{
	loc->coordinateID = value;
}

void SetHorizontalUnits(zStructLocation* loc, int value)
{
	loc->horizontalUnits = value;
}

void SetHorizontalDatum(zStructLocation* loc, int value)
{
	loc->horizontalDatum = value;
}

void SetVerticalUnits(zStructLocation* loc, int value)
{
	loc->verticalUnits = value;
}

void SetVerticalDatum(zStructLocation* loc, int value)
{
	loc->verticalDatum = value;
}

void SetTimeZoneName(zStructLocation* loc, const char* value)
{
	if (loc->timeZoneName)
		free(loc->timeZoneName);

	loc->timeZoneName = _strdup(value);
}

void SetSupplemental(zStructLocation* loc, const char* value)
{
	if (loc->supplemental)
		free(loc->supplemental);

	loc->supplemental = _strdup(value);
}

void SetPathName(zStructLocation* loc, const char* value)
{
	if (loc->pathname)
		free(loc->pathname);

	loc->pathname = _strdup(value);
}

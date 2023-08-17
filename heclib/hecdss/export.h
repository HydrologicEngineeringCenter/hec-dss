#pragma once

#include "hecdss.h"

int exportTimeSeries(dss_file* dss,
  const char* path,
  const char* outputFile,
  const char* startDate, const char* startTime,
  const char* endDate, const char* endTime);
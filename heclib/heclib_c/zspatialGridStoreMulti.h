#pragma once
#ifndef ZSPATIAL_GRID_STORE_MULTI_H
#define ZSPATIAL_GRID_STORE_MULTI_H

/*
 *  Author :  USACE – HEC
 *  Date   :  2024
 *
 *  Store a collection of spatial grids in one call.  Compression is
 *  done in parallel (CPU bound) while the final write to the DSS file
 *  is performed serially (I/O bound – DSS can only handle one write at
 *  a time).
 *
 *  See zspatialGridStoreMulti.c for full documentation.
 */

#include "zStructSpatialGrid.h"

 /*
  *  Prototype
  *  ---------
  *  ifltab     – Normal DSS file table (same as zspatialGridStore)
  *  grids      – Pointer to an array of zStructSpatialGrid* (NOT the
  *               structure itself – identical to what you already pass
  *               to zspatialGridStore)
  *  gridCount  – Number of entries in the grids array
  *  maxThreads – Upper bound on the number of worker threads.
  *               0  -> use number of cores detected
  *               1  -> execute completely serially
  *
  *  Returns  STATUS_OKAY           when every grid is written.
  *           First negative error  when any grid fails – the remaining
  *           grids are skipped, and that error is returned.
  */
int zspatialGridStoreMulti(long long* ifltab,
    zStructSpatialGrid** grids,
    int                  gridCount,
    int                  maxThreads);

#endif /* ZSPATIAL_GRID_STORE_MULTI_H */
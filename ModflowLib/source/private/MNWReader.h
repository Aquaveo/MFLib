//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#pragma once

#include <private\util\util.h>


namespace mnw
{

  //----------------------------------------------------------------------------
  // Format in H5 file:
  //
  // "00. Number of BCs" - MXMNW
  // "01. Use Last"      - ITMP
  // "02. Cell IDs"      - Layer Row Column
  // "03. Name"          - MNWsite (well name)
  // "07. Property"      - Active, Qdes, WellId, QWval, Rw, Skin, Hlim, Href, 
  //                       DD, Iwgrp, Cp:C, QCUT, Qfrcmn, Qfrcmx, MNWsite
  //    Active: 0.0 or 1.0 for active or not active this stress period
  //    WellId: Unique ID to group multi-node wells.  Zero for single node 
  //            cells.
  //    QCUT: 0.0 = None, 1.0 = Rate, 2.0 = Percent
  //    DD: 0.0 = No Hlim, 1.0 = Elevation, 2.0 = Drawdown or Buildup
  //    MNWsite: 0.0 = Don't Print, 1.0 = Print Site File
  //    QWVal < 0.0 then not calculated and Iwgrp ignored
  //    Iwgrp <= -1.0 then uses MODFLOW default group (counts up each node)
  //
  // "21. Stress P Ref"  - kspref
  // "22. Loss Type"     - PLoss < 0.99 SKIN.  PLoss > 1.001 for NONLINEAR.  
  //                       Otherwise LINEAR.
  // "23. Well IO"       - IOWELL2(3) Zero for unset.  Negative for ALLTIME
  //    IOWELL2[0] == iunw1
  //    IOWELL2[1] == iunby
  //    IOWELL2[2] == iunqs
  //----------------------------------------------------------------------------

  // h5 array indeces 
  enum MnwH5 { H5_ACTIVE, H5_QDES, H5_WELLID, H5_QWVAL, H5_RW, H5_SKIN, H5_HLIM,
               H5_HREF, H5_DD, H5_IWGRP, H5_C, H5_QCUT, H5_QFRCMN, H5_QFRCMX,
               H5_SITE, H5_SIZE };

  // MODFLOW MNW well2 array indeces
  enum MnwMF { W2_NODE = 1, W2_QDES = 2, W2_QWVAL = 4,
         W2_RW = 5, W2_SKIN = 6, W2_HLIM = 7,
         W2_HREF = 8, W2_IWGRP = 9, W2_C = 16,
         W2_QFRCMN = 13, W2_QFRCMX = 14, W2_ID = 18,
         W2_SIZE = 18 };

  // For the MNW package the values in the well2 array are changed as the line
  // is being read.  The MNWFLGS array is used to store items read from either
  // an H5 or text file as they were read while allowing the MNW package to
  // change the values along the way.  The array also contains the ierr value
  // used when reading H5 used to specify how many numeric items would have been
  // missing if the well data had been specified as a text line.
  enum { MNWFLGS_QDES=1, MNWFLGS_HLIM, MNWFLGS_HREF, MNWFLGS_DD, MNWFLGS_QCUT,
         MNWFLGS_IERR, MNWFLGS_DEFAULT, MNWFLGS_SIZE=7 };

  enum { NOT_ACTIVE, ACTIVE };
  enum { QCUT_NONE, QCUT_RATE, QCUT_PCT };
  enum { DD_NONE, DD_ELEVATION, DD_RELATIVE };
  enum { SITE_DONT_PRINT, SITE_PRINT };

  bool ReadMfH5Data(int a_itmp,
                    double* a_well2,
                    char* a_mnwsite,
                    double* a_mnwflgs,
                    const char* a_line);

  bool ReadH5Data(std::vector<int>& a_cellids,
                  CAR_DBL2D& a_well2,
                  std::vector<CStr>& a_names,
                  const CStr& a_line);
  bool ReadMnw2H5Data(double* MNW2,
                      const int* MNWMAX,
                      const int* NMNWVL,
                      const int* NAUX,
                      const CStr& a_line);
}


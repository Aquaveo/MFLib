//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef SFR2READERH5_H
#define SFR2READERH5_H

#include <private\util\util.h>


namespace sfr2
{
  void GetReachData(int ii,
                    int nrow,
                    int ncol,
                    int& krch,
                    int& irch,
                    int& jrch,
                    int& jseg,
                    int& ireach,
                    Real* Strm,
                    int NStrmD,
                    CStr& line);
  void GetSegData(int Nlst,
                  int Kper,
                  int* Iseg,
                  int* Iotsg,
                  int* Idivar,
                  Real* Seg,
                  Real* Xsec,
                  Real* Qstage,
                  CStr& line);
};
#endif

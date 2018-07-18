//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef H5UTIL_H
#define H5UTIL_H
#include <private/util/util.h>

namespace MfData
{
  namespace Export
  {
    bool H5Util_CreateDefaultMfH5File (const char *a_,
                                       int a_modelType/*=1*/,
                                       bool a_compress/*=false*/);
    void H5Util_CreateWelClnGroup (const char* a_,
                                   bool a_compress);
  } // namespace Export
} // namespace MfData
int xfpWriteDatasetString(hid_t a_Loc,
                          const char *a_Name,
                          const char *a_Str);
int xfpWriteAttributeInt(hid_t a_Loc,
                          const char *a_Name,
                          int a_Number,
                          int *a_val);
int xfpWriteAttributeString(hid_t a_Loc,
                            const char * a_Name,
                            const char * a_Str);
int xfpWriteAttributeDouble(hid_t a_Loc,
                            const char *a_Name,
                            int a_Number,
                            double *a_val);
#endif

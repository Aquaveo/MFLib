//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef H5DATAREADERUTIL_H
#define H5DATAREADERUTIL_H

#include <private\util\util.h>

namespace H5DataReader
{
  inline H5T_class_t Get_H5T_class_t (const int*) { return H5T_INTEGER; }
  inline H5T_class_t Get_H5T_class_t (const float*) { return H5T_FLOAT; }
  inline H5T_class_t Get_H5T_class_t (const double*) { return H5T_FLOAT; }
  inline H5T_class_t Get_H5T_class_t (const char*) { return H5T_INTEGER; }
  inline H5T_class_t Get_H5T_class_t (hid_t a_)
  {
    if (a_ == H5T_NATIVE_INT || a_ == H5T_NATIVE_CHAR)
      return H5T_INTEGER;
    else if (a_ == H5T_NATIVE_FLOAT || a_ == H5T_NATIVE_DOUBLE)
      return H5T_FLOAT;
    else
      return H5T_NO_CLASS;//error
  }

  inline hid_t Get_H5_nativetype (const int*) { return H5T_NATIVE_INT; }
  inline hid_t Get_H5_nativetype (const float*) { return H5T_NATIVE_FLOAT; }
  inline hid_t Get_H5_nativetype (const double*) { return H5T_NATIVE_DOUBLE; }
  inline hid_t Get_H5_nativetype (const char*) { return H5T_NATIVE_CHAR; }

  bool GetDataSetDimensions(hid_t a_dataSpaceId,
                            std::vector<hsize_t> &a_dims,
                            std::vector<hsize_t> &a_maxDims);
} // namespace H5DataReader

#endif


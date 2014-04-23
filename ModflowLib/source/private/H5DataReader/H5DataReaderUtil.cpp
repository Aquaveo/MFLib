//------------------------------------------------------------------------------
// FILE      H5DataReaderUtil.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------

#include <private\H5DataReader\H5DataReaderUtil.h>

//------------------------------------------------------------------------------
/// \brief Gets the data set dimensions given a data space
//------------------------------------------------------------------------------
bool H5DataReader::GetDataSetDimensions (hid_t a_dataSpaceId,
                                         std::vector<hsize_t> &a_dims,
                                         std::vector<hsize_t> &a_maxDims)
{
  if (a_dataSpaceId < 0)
    return false;

  const int nDims(H5Sget_simple_extent_ndims(a_dataSpaceId));

  a_dims.assign(nDims, 0);
  a_maxDims.assign(nDims, 0);
  if (H5Sget_simple_extent_dims(a_dataSpaceId, &a_dims[0], &a_maxDims[0]) < 0)
  {
    a_dims.clear();
    a_maxDims.clear();
    return false;
  }
  return true;
} // H5DataReader::GetDataSetDimensions




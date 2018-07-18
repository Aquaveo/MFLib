//------------------------------------------------------------------------------
// FILE      H5Initialize.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/util/util.h>

#include <private/util/H5Initialize.h>

#include <hdf5.h>
#if _DEBUG
#include <crtdbg.h>
#endif
//------------------------------------------------------------------------------
/// \breif Initialzes the HDF5 library
//------------------------------------------------------------------------------
void H5Initialize::Init ()
{
  static bool firstTime(true); // ok to leave static
  if (firstTime)
  {
    firstTime = false;
    H5open();
    H5Eset_auto(NULL, NULL);
  }
} // H5Initialize::Init



//------------------------------------------------------------------------------
// FILE      NativeExpArr1d.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpArr1d.h>

#include <fstream>
#include <sstream>

#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackFields.h>

namespace { // unnamed namespace

} // unnamed namespace

using namespace MfData::Export;

//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpArr1d::NativeExpArr1d (bool a_h5) :
  m_h5(a_h5)
{
} // MfNativeExpArr1d::MfNativeExpArr1d
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpArr1d::~NativeExpArr1d ()
{
} // MfNativeExpArr1d::~MfNativeExpArr1d
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpArr1d::Export ()
{
  const int  *JJ(0), *IPRN(0);
  const Real *ARR(0), *MULT(0);
  const double *ARRdbl(0);
  const int *ARRint(0);
  const int *K(0);
  int k = 0;

  MfPackage* p = GetPackage();
  if ((!p->GetField("ARR", &ARR) || !ARR) &&
      (!p->GetField("ARR", &ARRdbl) || !ARRdbl) &&
      (!p->GetField("ARR", &ARRint) || !ARRint)) {
    ASSERT(0);
    return false;
  }

  if (!p->GetField("MULT", &MULT) || !MULT ||
      !p->GetField("JJ", &JJ) || !JJ ||
      !p->GetField("IPRN", &IPRN) || !IPRN)
  {
    ASSERT(0);
    return false;
  }

  if (!GetPackage()->GetField("K", &K) || !K) {
    K = &k;
  }

  // Because ARR can be Real, double or int, we do everything in templates
  bool rv;
  if (ARR) {
    rv = ExportT(JJ, IPRN, ARR, MULT, K);
  }
  else if (ARRdbl) {
    rv = ExportT(JJ, IPRN, ARRdbl, MULT, K);
  }
  else {
    rv = ExportT(JJ, IPRN, ARRint, MULT, K);
  }
  return rv;

} // MfNativeExpArr1d::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpArr1d::GetFname (const int* K)
{
  CStr name(GetPackage()->PackageName());
  // write the array to a file
  // the name will be "modelname"_array_DELR ...
  CStr path, folderStr, fname, fname1 = GetNative()->FileName();
  util::StripExtensionFromFilename(fname1, fname1);
  util::StripFileFromFilename(fname1, path);
  util::StripPathFromFilename(fname1, fname1);
  name.Replace(" ", "_");
  if (GetNative()->GetArraysInFolder())
  {
    folderStr = "arrays\\";
    CStr dir = path + "arrays";
    ::CreateDirectory(dir, NULL);
  }
  if (*K == 0)
    fname.Format("%s%s%s_array_%s.txt", path, folderStr, fname1, name);
  else
    fname.Format("%s%s%s_array_%s_%d.txt", path, folderStr, fname1, name, *K);
  return fname;
} // NativeExpArr1d::GetFname

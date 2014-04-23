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
using namespace MfData::Export;
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpArr1d::NativeExpArr1d ()
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
  CStr name(GetPackage()->PackageName());
  const int  *JJ(0), *IPRN(0);
  const Real *ARR(0), *MULT(0);
  if (!GetPackage()->GetField("ARR", &ARR) || !ARR ||
      !GetPackage()->GetField("MULT", &MULT) || !MULT ||
      !GetPackage()->GetField("JJ", &JJ) || !JJ ||
      !GetPackage()->GetField("IPRN", &IPRN) || !IPRN)
  {
    ASSERT(0);
    return false;
  }

  // see if we can do constant
  if (1.0 == *MULT)
  {
    bool constant(true);
    for (int i=1; i<*JJ && constant; ++i)
    {
      if (ARR[0] != ARR[i]) constant = false;
    }
    if (constant)
    {
      CStr str;
      str.Format("CONSTANT %s", STR(ARR[0]));
      AddToStoredLinesDesc(str, "");
      return true;
    }
  }

  if (!GetNative()->GetArraysInternal())
    WriteExternal(JJ, IPRN, ARR, MULT);
  else
    WriteInternal(JJ, IPRN, ARR, MULT);

  return true;
} // MfNativeExpArr1d::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpArr1d::WriteExternal (const int* JJ, const int* IPRN,
                                    const Real* ARR, const Real* MULT)
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
  fname.Format("%s%s%s_array_%s.txt", path, folderStr, fname1, name);

  std::fstream os;
  os.open((LPCTSTR)fname, std::ios_base::out);
  if (os.bad()) return;
  WriteToStream(os, JJ, ARR);
  os.close();

  CStr strIprn, strMult;
  strIprn.Format("%5d", *IPRN);
  //if (*IPRN < 0) strIprn = "-1";
  //else if (*IPRN == 0) strIprn = "10G12.5";
  //else strIprn = "5G12.5";
  strMult = STR(*MULT);
  util::StripPathFromFilename(fname, fname);
  if (GetNative()->GetArraysInFolder())
  {
    fname = ".\\arrays\\" + fname;
  }
  CStr str;
  str.Format("OPEN/CLOSE %s %s (FREE) %s", fname, strMult, strIprn);
  AddToStoredLinesDesc(str, "");
} // NativeExpArr1d::WriteExternal
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpArr1d::WriteInternal  (const int* JJ, const int* IPRN,
                                     const Real* ARR, const Real* MULT)
{
  CStr str, strIprn, strMult;
  strIprn.Format("%5d", *IPRN);
  strMult = STR(*MULT);
  str.Format("INTERNAL %s (FREE) %s", strMult, strIprn);
  AddToStoredLinesDesc(str, "");
  std::stringstream os;
  WriteToStream(os, JJ, ARR);
  str = os.str();
  while (str.at(str.GetLength()-1) == '\n')
  {
    str.pop_back();
  }
  AddToStoredLinesDesc(str, "");
} // NativeExpArr1d::WriteInternal
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpArr1d::WriteToStream (std::ostream& a_os, const int* JJ,
                                    const Real* ARR)
{
  for (int i=0; i<*JJ; ++i)
  {
    a_os << STR(ARR[i]) << " ";
    if (i > 0 && i%10 == 0) a_os << "\n";
  }
} // NativeExpArr1d::WriteToStream


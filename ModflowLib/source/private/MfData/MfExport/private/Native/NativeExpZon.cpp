//------------------------------------------------------------------------------
// FILE      NativeExpZon.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpZon.h>

#include <sstream>

#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\MfExportUtil.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackStrings.h>

using namespace MfData::Export;
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpZon::NativeExpZon ()
{
} // MfNativeExpZon::MfNativeExpZon
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpZon::~NativeExpZon ()
{
} // MfNativeExpZon::~MfNativeExpZon
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpZon::Export ()
{
  std::vector<CStr>& lines(GetPackage()->StringsToWrite());
  std::vector<CStr>& desc(GetPackage()->StringDescriptions());
  std::vector<CStr> lCopy(lines);
  lines.clear();
  desc.clear();
  bool internalArrays = GetNative()->GetArraysInternal();
  // see how many zone arrays
  int nzones = (int)lCopy.size() / 2;
  if (internalArrays)
  {
    nzones = 0;
    for (size_t i=0; i<lCopy.size(); i+=2)
    {
      nzones++;
      if (MfExportUtil::ArrayWriteNextLineInternal(GetNative(), lCopy[i+1])) i++;
    }
  }
  CStr str;
  str.Format("%d", nzones);
  AddToStoredLinesDesc(str, Desc(1));
  for (size_t i=0; i<lCopy.size(); i+=2)
  {
    AddToStoredLinesDesc(lCopy[i], Desc(2));
    AddToStoredLinesDesc(lCopy[i+1], Desc(3));
    if (MfExportUtil::ArrayWriteNextLineInternal(GetNative(), lCopy[i+1]))
    {
      i++;
      AddToStoredLinesDesc(lCopy[i+1], "");
    }
  }

  WriteComments();
  WriteStoredLines();
  return true;
} // MfNativeExpZon::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpZon::Desc (int a_line)
{
  CStr desc[3] = {" 1. NZN",
                  " 2. ZONNAM",
                  " 3. IZON(NCOL,NROW)"
                 };
  return desc[a_line-1];
} // MfNativeExpZon::Export


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData\MfExport\private\Native\NativeExpZon.t.h>

#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackStrings.h>
#include <private/MfLibAsserts.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpZonT::setUp ()
{
  m_p = NULL;
  Mf2kNative* n = new Mf2kNative;
  MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  MfPackage* dis = new MfPackage(Packages::ZON);
  NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  m_p = dynamic_cast<NativeExpZon*>(p);
} // NativeExpZonT::setUp
//------------------------------------------------------------------------------
void NativeExpZonT::tearDown ()
{
  m_p->UnitTestingDeletePointers();
  delete(m_p);
} // NativeExpZonT::tearDown
//------------------------------------------------------------------------------
void NativeExpZonT::testCreateClass ()
{
  TS_ASSERT(m_p);
} // NativeExpZonT::testCreateClass
//------------------------------------------------------------------------------
void NativeExpZonT::testDesc ()
{
  CStr base = " 1. NZN";
  CStr str = m_p->Desc(1);
  TS_ASSERT_EQUALS2(base, str);
  base = " 2. ZONNAM";
  str = m_p->Desc(2);
  TS_ASSERT_EQUALS2(base, str);
  base = " 3. IZON(NCOL,NROW)";
  str = m_p->Desc(3);
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpZonT::testDesc

#endif
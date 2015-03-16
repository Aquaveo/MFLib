//------------------------------------------------------------------------------
// FILE      NativeExpMlt.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpMlt.h>

#include <sstream>

#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\MfExportUtil.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackStrings.h>

using namespace MfData::Export;
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMlt::NativeExpMlt ()
{
} // MfNativeExpMlt::MfNativeExpMlt
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMlt::~NativeExpMlt ()
{
} // MfNativeExpMlt::~MfNativeExpMlt
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpMlt::Export ()
{
  if ("FNC" == GetPackage()->PackageName())
  {
    const char *name(0), *func(0);
    if (!GetPackage()->GetField("NAME", &name) || !name ||
        !GetPackage()->GetField("FUNC", &func) || !func)
      return false;

    MfPackage* p = GetGlobal()->GetPackage(Packages::MLT);
    if (!p)
    {
      MfPackage p1(Packages::MLT);
      GetGlobal()->AddPackage(&p1);
      p = GetGlobal()->GetPackage(Packages::MLT);
    }
    p->StringsToWrite().push_back(name);
    p->StringsToWrite().push_back(func);
    return false;
  }

  bool internalArrays = GetNative()->GetArraysInternal();
  std::vector<CStr>& lines(GetPackage()->StringsToWrite());
  std::vector<CStr>& desc(GetPackage()->StringDescriptions());
  std::vector<CStr> lCopy(lines);
  lines.clear();
  desc.clear();
  // see how many zone arrays
  int nMlt = (int)lCopy.size() / 2;
  if (internalArrays)
  {
    nMlt = 0;
    for (size_t i=0; i<lCopy.size(); i+=2)
    {
      nMlt++;
      CStr l = lCopy[i+1];
      l.ToLower();
      if (l.Find("function") == -1 && l.Find("constant") == -1) i++;
    }
  }
  CStr str;
  str.Format("%d", nMlt);
  AddToStoredLinesDesc(str, Desc(1));
  for (size_t i=0; i<lCopy.size(); i+=2)
  {
    AddToStoredLinesDesc(lCopy[i], Desc(2));
    CStr line = lCopy[i+1];
    line.ToLower();
    CStr desc = Desc(3);
    bool func(false);
    if (line.Find("function") != -1)
    {
      desc = Desc(4);
      func = true;
    }
    AddToStoredLinesDesc(lCopy[i+1], desc);
    if (MfExportUtil::ArrayWriteNextLineInternal(GetNative(), lCopy[i+1]) &&
        !func)
    {
      i++;
      AddToStoredLinesDesc(lCopy[i+1], "");
    }
  }

  WriteComments();
  WriteStoredLines();
  return true;
} // MfNativeExpMlt::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpMlt::Desc (int a_line)
{
  CStr desc[4] = {" 1. NML",
                  " 2. MLTNAM [FUNCTION]",
                  " 3. RMLT(NCOL,NROW)",
                  " 4. [MLTNAM1 [op1 MLTNAM2] [op2 MLTNAM3] [op3 MLTNAM4] ... ] [IPRN]"
                 };
  if (GetGlobal()->ModelType() == MfData::USG)
  {
    desc[2].Replace(" 3", "3b");
    if (GetGlobal()->Unstructured()) desc[2] = "3a. RMLT(NDSLAY)";
  }
  return desc[a_line-1];
} // NativeExpMlt::Desc


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData\MfExport\private\Native\NativeExpMlt.t.h>

#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackStrings.h>
#include <private/MfLibAsserts.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpMltT::setUp ()
{
  m_p = NULL;
  Mf2kNative* n = new Mf2kNative;
  MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  MfPackage* dis = new MfPackage(Packages::MLT);
  NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  m_p = dynamic_cast<NativeExpMlt*>(p);
} // NativeExpMltT::setUp
//------------------------------------------------------------------------------
void NativeExpMltT::tearDown ()
{
  m_p->UnitTestingDeletePointers();
  delete(m_p);
} // NativeExpMltT::tearDown
//------------------------------------------------------------------------------
void NativeExpMltT::testCreateClass ()
{
  TS_ASSERT(m_p);
} // NativeExpMltT::testCreateClass
//------------------------------------------------------------------------------
void NativeExpMltT::testDesc ()
{
  CStr base = " 1. NML";
  CStr str = m_p->Desc(1);
  TS_ASSERT_EQUALS2(base, str);
  base = " 2. MLTNAM [FUNCTION]";
  str = m_p->Desc(2);
  TS_ASSERT_EQUALS2(base, str);
  base = " 3. RMLT(NCOL,NROW)";
  str = m_p->Desc(3);
  TS_ASSERT_EQUALS2(base, str);
  base = " 4. [MLTNAM1 [op1 MLTNAM2] [op2 MLTNAM3] [op3 MLTNAM4] ... ] [IPRN]";
  str = m_p->Desc(4);
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpMltT::testDesc

#endif
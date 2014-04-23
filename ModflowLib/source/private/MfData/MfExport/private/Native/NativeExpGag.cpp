//------------------------------------------------------------------------------
// FILE      NativeExpGag.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpGag.h>

#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackFields.h>

using namespace MfData::Export;
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpGag::NativeExpGag ()
{
} // MfNativeExpGag::MfNativeExpGag
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpGag::~NativeExpGag ()
{
} // MfNativeExpGag::~MfNativeExpGag
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpGag::Export ()
{
  AddToStoredLinesDesc(Line1(), Desc(1));
  Line2();
  WriteComments();
  WriteStoredLines();
  return true;
} // MfNativeExpGag::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpGag::Desc (int a_line)
{
  CStr rval[3] = {" 1. NUMGAGE",
                  "2a. LAKE UNIT [OUTTYPE]",
                  "2b. GAGESEG GAGERCH UNIT OUTTYPE",
                  };
  return rval[a_line-1];
} // MfNativeExpGag::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpGag::Line1 ()
{
  using namespace MfData::Packages;
  CStr rval;
  const int *numgage(0);
  if (GetPackage()->GetField(GAGpack::NUMGAGE, &numgage) && numgage)
  {
    rval.Format("%d", *numgage);
  }
  return rval;
} // NativeExpGag::Line1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpGag::Line2 ()
{
  using namespace MfData::Packages;
  const int *igglst(0), *numgage(0);
  if (GetPackage()->GetField(GAGpack::IGGLST, &igglst) && igglst &&
      GetPackage()->GetField(GAGpack::NUMGAGE, &numgage) && numgage)
  {
    CStr s;
    for (int i=0; i<*numgage; ++i)
    {
      int type = igglst[i*4 + 0];
      if (type >= 0)
      {
        // line 2b: GAGESEG GAGERCH UNIT OUTTYPE
        int gageseg = type;
        int gagerch = igglst[i*4 + 1];
        int unit    = igglst[i*4 + 2];
        int outtype = igglst[i*4 + 3];
        s.Format("%3d %4d %4d %3d", gageseg, gagerch, unit, outtype);
        AddToStoredLinesDesc(s, Desc(3));
      }
      else
      {
        // line 2a: LAKE UNIT {OUTTYPE}
        int lake    = type;
        int unit    = igglst[i*4 + 2];
        int outtype = igglst[i*4 + 3];
        s.Format("%3d %4d %4d", lake, unit, outtype);
        AddToStoredLinesDesc(s, Desc(2));
      }
    }
  }
} // NativeExpGag::Line2

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData\MfExport\private\Native\NativeExpGag.t.h>

#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackStrings.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpGagT::setUp ()
{
  m_p = NULL;
  Mf2kNative* n = new Mf2kNative;
  MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  MfPackage* dis = new MfPackage(Packages::GAGE);
  NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  m_p = dynamic_cast<NativeExpGag*>(p);
} // NativeExpGagT::setUp
//------------------------------------------------------------------------------
void NativeExpGagT::tearDown ()
{
  m_p->UnitTestingDeletePointers();
  delete(m_p);
} // NativeExpGagT::tearDown
//------------------------------------------------------------------------------
void NativeExpGagT::testCreateClass ()
{
  TS_ASSERT(m_p);
} // NativeExpGagT::testCreateClass

#endif
//------------------------------------------------------------------------------
// FILE      NativeExpDdf.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/MfData/MfExport/private/Native/NativeExpDdf.h>

#include <sstream>

#include <private/MfData/MfExport/private/Mf2kNative.h>
#include <private/MfData/MfExport/private/MfExportUtil.h>
#include <private/MfData/MfExport/private/Native/NativeUtil.h>
#include <private/MfData/MfExport/private/TxtExporter.h>
#include <private/MfData/MfGlobal.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackFields.h>
#include <private/MfData/Packages/MfPackStrings.h>

using namespace MfData::Export;
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpDdf::NativeExpDdf ()
{
} // MfNativeExpDdf::MfNativeExpDdf
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpDdf::~NativeExpDdf ()
{
} // MfNativeExpDdf::~MfNativeExpDdf
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpDdf::Export ()
{
  if (GetPackage()->PackageName() != Packages::DDF)
  {
    return false;
  }

  MfPackage* a_p = GetPackage();
  const double *rhofresh(0), *rhostd(0), *cstd(0);
  if (a_p->GetField(Packages::Ddf::RHOFRESH, &rhofresh) && rhofresh &&
      a_p->GetField(Packages::Ddf::RHOSTD, &rhostd) && rhostd &&
      a_p->GetField(Packages::Ddf::CSTD, &cstd) && cstd)
  {

    CStr desc = "1. RHOFRESH, RHOSTD, CSTD";
    CStr var;
    CStr line;
    int width = util::RealWidth();

    var.Format("%s ", STR(*rhofresh, -1, width, STR_FULLWIDTH));
    line += var;
    var.Format("%s ", STR(*rhostd, -1, width, STR_FULLWIDTH));
    line += var;
    var.Format("%s", STR(*cstd, -1, width, STR_FULLWIDTH));
    line += var;
    AddToStoredLinesDesc(line, desc);
  }

  WriteComments();
  WriteStoredLines();
  return true;
} // MfNativeExpDdf::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpDdf::AddToStoredLinesDesc (const char* a_line,
                                         const char* a_desc)
{
  MfPackage* p = GetGlobal()->GetPackage(Packages::DDF);
  if (!p || GetPackage() == p)
    NativePackExp::AddToStoredLinesDesc(a_line, a_desc);
  else
  {
    MfPackage* p1 = GetPackage();
    SetData(GetNative(), GetGlobal(), p);
    NativePackExp::AddToStoredLinesDesc(a_line, a_desc);
    SetData(GetNative(), GetGlobal(), p1);
  }
} // NativeExpDdf::AddToStoredLinesDesc

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private/MfData/MfExport/private/Native/NativeExpDdf.t.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpDdfT::setUp ()
{
  //m_p = NULL;
  //Mf2kNative* n = new Mf2kNative;
  //MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  //MfPackage* dis = new MfPackage(Packages::LAK);
  //NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  //m_p = dynamic_cast<NativeExpDdf*>(p);
} // NativeExpDdfT::setUp
//------------------------------------------------------------------------------
void NativeExpDdfT::tearDown ()
{
  //m_p->UnitTestingDeletePointers();
  //delete(m_p);
} // NativeExpDdfT::tearDown
//------------------------------------------------------------------------------
void NativeExpDdfT::testCreateClass ()
{
  //TS_ASSERT(m_p);
} // NativeExpDdfT::testCreateClass

#endif
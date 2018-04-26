//------------------------------------------------------------------------------
// FILE      Mf2kNative.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Mf2kNative.h>

#include <RunTest.h>

#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\MfExport\private\CellNumbering.h>
#include <private\MfData\MfExport\private\H5\H5Util.h>
#include <private\MfData\MfExport\private\Native\NativePackExp.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfExport\private\TxtExporter.h>

#include <private\util\util.h>

using namespace MfData::Export;

//------------------------------------------------------------------------------
/// \brief constructor
//------------------------------------------------------------------------------
Mf2kNative::Mf2kNative () :
  MfExporterImpl("mf2knative")
, m_arraysInFolder(false)
, m_arraysInternal(true)
, m_h5(false)
, m_sqlite(false)
, m_exportMf6(false)
, m_cn(nullptr)
{
} // Mf2kNative::Mf2kNative
//------------------------------------------------------------------------------
/// \brief constructor
//------------------------------------------------------------------------------
Mf2kNative::~Mf2kNative ()
{
  if (m_cn) delete(m_cn);
  m_cn = nullptr;
} // Mf2kNative::~Mf2kNative
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void Mf2kNative::SetFileName (const char *a_)
{
  MfExporterImpl::SetFileName(a_);
  if (m_h5)
  {
    CStr base = GetExp()->GetBaseFileName();
    H5Util_CreateDefaultMfH5File(base, GetModelType(), CompressH5());
  }
} // Mf2kNative::SetFileName
//------------------------------------------------------------------------------
/// \brief Exports the package data.
//------------------------------------------------------------------------------
bool Mf2kNative::ExportPackage (MfData::MfGlobal* a_global,
                                MfData::MfPackage* a_package)
{
  if (!a_global ||
      !a_package)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }

  CStr packName = a_package->PackageName();
  bool rval(true);
  // do something with the data
  NativePackExp* p = NativeUtil::CreatePackExp(this, a_global, a_package);
  if (p)
  {
    rval = p->Export();
    delete(p);
  }
  bool testsRunning(0);
#ifdef CXX_TEST
  if (testCxx::TestsRunning())
  {
    testsRunning = true;
  }
#endif
  if (rval && !testsRunning)
  {
    printf("Writing data for package: %s\n", packName.c_str());
    fflush(stdout);
  }
  return rval;
} // MfData::Export::Mf2kNative::ExportPackage
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void Mf2kNative::SetArraysInFolder (bool a_)
{
  m_arraysInFolder = a_;
} // MfData::Export::Mf2kNative::SetArraysInFolder
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void Mf2kNative::SetArraysInternal (bool a_)
{
  m_arraysInternal = a_;
} // MfData::Export::Mf2kNative::SetArraysInternal
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void Mf2kNative::SetUseH5 (bool a_, bool a_compress)
{
  m_h5 = a_;
  SetCompressH5(a_compress);
} // MfData::Export::Mf2kNative::SetCompressH5

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData\MfExport\private\Mf2kNative.t.h>

//------------------------------------------------------------------------------
void Mf2kNativeT::testExportPackage ()
{
  MfData::MfGlobal g(2,5,3,1,4,6,0);
  MfData::MfPackage p("dis");
  MfData::MfGlobal *gp(0);
  MfData::MfPackage *pp(0);

  Mf2kNative m;
  TS_ASSERT(!m.ExportPackage(gp, pp));
  TS_ASSERT(!m.ExportPackage(&g, pp));
  TS_ASSERT(!m.ExportPackage(gp, &p));
  TS_ASSERT(m.ExportPackage(&g, &p));
}
#endif

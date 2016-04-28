//------------------------------------------------------------------------------
// FILE      MfExporter.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\MfExporter.h>

#include <private\MfData\MfGlobal.h>
#include <private\MfData\MfExport\private\MfExporterImpl.h>
#include <private\MfData\MfExport\private\MfExportUtil.h>

using namespace MfData::Export;

//------------------------------------------------------------------------------
/// \brief Constructor.
//------------------------------------------------------------------------------
MfExporter::MfExporter (const char *a_) :
m_p(MfExportUtil::CreateExporter(a_))
{
} // MfExporter::MfExporter
//------------------------------------------------------------------------------
/// \brief Destructor.
//------------------------------------------------------------------------------
MfExporter::~MfExporter ()
{
  if (m_p)
    delete(m_p);
} // MfExporter::~MfExporter
//------------------------------------------------------------------------------
/// \brief Returns the type name for the exporter
//------------------------------------------------------------------------------
const char* MfExporter::GetTypeName ()
{
  return(m_p->GetTypeName());
} // MfExporter::GetTypeName
//------------------------------------------------------------------------------
/// \brief Sets the filename where the data will be exported to
//------------------------------------------------------------------------------
void MfExporter::SetFileName (const char *a_)
{
  m_p->SetFileName(a_);
} // MfExporter::SetFileName
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void MfExporter::SetTablesStr (const char *a_)
{
  m_p->SetTablesStr(a_);
} // MfExporter::SetPackagesStr
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
int MfExporter::GetModelType ()
{
  return (m_p->GetModelType());
} // MfExporter::GetModelType
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void MfExporter::SetModelType (int a_)
{
  m_p->SetModelType(a_);
} // MfExporter::SetModelType
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
const char* MfExporter::GetTablesStr ()
{
  return (m_p->GetTablesStr());
} // MfExporter::GetTablesStr
//------------------------------------------------------------------------------
/// \brief Exports a package to some format based on the type of exporter
//------------------------------------------------------------------------------
bool MfExporter::ExportPackage (MfData::MfGlobal *a_global,
                                MfData::MfPackage *a_package)
{
  return(m_p->ExportPackage(a_global, a_package));
} // MfExporter::ExportPackage
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool MfExporter::CanExportTable (const char *a_)
{
  return (m_p->CanExportTable(a_));
} // MfExporter::CanExportPackage

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData\MfExport\MfExporter.t.h>

//------------------------------------------------------------------------------
#endif


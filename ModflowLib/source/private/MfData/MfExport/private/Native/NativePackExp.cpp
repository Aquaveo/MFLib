//------------------------------------------------------------------------------
// FILE      NativePackExp.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/MfData/MfExport/private/Native/NativePackExp.h>

#include <sstream>

#include <private/MfData/MfExport/private/Mf2kNative.h>
#include <private/MfData/MfExport/private/MfExportUtil.h>
#include <private/MfData/MfExport/private/Native/NativeUtil.h>
#include <private/MfData/MfExport/private/TxtExporter.h>
#include <private/MfData/MfGlobal.h>
#include <private/MfData/MfPackageUtil.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackStrings.h>
#include <private/Parameters.h>
#include <private/Parameters/Param.h>
#include <private/Parameters/ParamList.h>

using namespace MfData;
using namespace MfData::Export;

class NativePackExp::impl
{
public:
  impl()
       : m_native(NULL)
       , m_global(NULL)
       , m_package(NULL)
       , m_h5(false)
  {

  }

  Mf2kNative* m_native;
  MfGlobal*   m_global;
  MfPackage*  m_package;
  bool        m_h5;
};
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativePackExp::NativePackExp ()
  : m_p(new NativePackExp::impl())
{
} // NativePackExp::NativePackExp
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativePackExp::~NativePackExp ()
{
  if (m_p) delete(m_p);
} // NativePackExp::~NativePackExp
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativePackExp::SetData (Mf2kNative* a_native,
                             MfGlobal* a_global,
                             MfPackage* a_package)
{
  m_p->m_native = a_native;
  m_p->m_global = a_global;
  m_p->m_package = a_package;
  OnSetData();
} // NativePackExp::SetData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativePackExp::SetH5Flag (bool a_h5)
{
  m_p->m_h5 = a_h5;
} // NativePackExp::SetH5Flag
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
Mf2kNative* NativePackExp::GetNative ()
{
  return m_p->m_native;
} // NativePackExp::GetNative
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
MfGlobal* NativePackExp::GetGlobal ()
{
  return m_p->m_global;
} // NativePackExp::GetGlobal
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
const MfGlobal* NativePackExp::GetGlobal () const
{
  return m_p->m_global;
} // NativePackExp::GetGlobal
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
MfPackage* NativePackExp::GetPackage ()
{
  return m_p->m_package;
} // NativePackExp::GetPackage
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativePackExp::GetH5Flag ()
{
  return m_p->m_h5;
} // NativePackExp::GetH5Flag
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativePackExp::AddToStoredLinesDesc (const char* a_line,
                                          const char* a_desc)
{
  std::vector<CStr>& lines(m_p->m_package->StringsToWrite());
  std::vector<CStr>& desc(m_p->m_package->StringDescriptions());
  lines.push_back(a_line);
  desc.push_back(a_desc);
} // NativePackExp::AddToStoredLinesDesc
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativePackExp::AddToStoredLinesDesc (const std::vector<CStr>& a_line,
                                          const std::vector<CStr>& a_desc)
{
  ASSERT(a_line.size() == a_desc.size());
  std::vector<CStr>& lines(m_p->m_package->StringsToWrite());
  std::vector<CStr>& desc(m_p->m_package->StringDescriptions());
  lines.insert(lines.end(), a_line.begin(), a_line.end());
  desc.insert(desc.end(), a_desc.begin(), a_desc.end());
} // NativePackExp::AddToStoredLinesDesc
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativePackExp::WriteComments ()
{
  using namespace MfData::Packages;
  CStr comments, pName = m_p->m_package->PackageName();
  CStr pNameTmp = pName;
  if (pNameTmp == HUF) pNameTmp = HUF2;
  // write any comments
  GetComments(pNameTmp, comments);
  if (comments.find("GMS") != -1)
  {
    comments.Replace("#GMS_HDF5_01", "#");
    comments.Replace("#GMS_PARAM_CLUSTERS", "#");
    comments.Replace("# GMS_HGU_MAT_NUMUNITS", "#");
    comments.Replace("# GMS_HGU_MAT", "#");
    comments.Replace("# Generated by GMS ", "#");
  }
  if (!comments.empty())
  {
    m_p->m_native->GetExp()->WriteLineToFile(pName, comments);
  }
  return true;
} // NativePackExp::WriteComments
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativePackExp::WriteStoredLines ()
{
  CStr pName = m_p->m_package->PackageName();
  std::vector<CStr>& lines(m_p->m_package->StringsToWrite());
  std::vector<CStr>& desc(m_p->m_package->StringDescriptions());
  m_p->m_native->GetExp()->WriteLinesAndDescriptionsToFile(pName, lines, desc);
  lines.clear();
  desc.clear();
  return true;
} // NativePackExp::WriteStoredLines
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativePackExp::SkipPar_Pval_Sen (const CStr& a_name)
{
  ParamList* list(0);
  Parameters::GetParameterList(&list);
  if (!list) return false;

  std::set<CStr> setSkipPar;
  CStr parsToSkip;
  if (GetGlobal()->GetStrVar("Pars2Skip", parsToSkip))
  {
    std::stringstream ss;
    ss << parsToSkip;
    while (ss.good())
    {
      ss >> parsToSkip;
      parsToSkip.ToLower();
      setSkipPar.insert(parsToSkip);
    }
  }

  CStr nm = a_name;
  nm.ToLower();
  if (setSkipPar.find(nm) != setSkipPar.end()) return true;

  Param p;
  int scatIdx;
  bool checkType(0);
  if (list->FindByName(a_name, &p))
  {
    checkType = true;
  }
  // ignore the HK here, it is not really used
  else if (list->IsPilotParName(a_name, "HK", &scatIdx))
  {
    for (size_t i=0; !checkType && i<list->Size(); ++i)
    {
      list->At(i, &p);
      if (p.m_scatIndex == scatIdx) checkType = true;
    }
  }

  if (checkType)
  {
    std::set<CStr> lpfTypes = MfExportUtil::LpfParamTypes();
    CStr type = p.m_type;
    type.ToLower();
    if (lpfTypes.find(type) != lpfTypes.end()) checkType = true;
    else checkType = false;
  }

  if (!checkType) return false;

  int val(0);
  if (GetGlobal()->GetIntVar("Uses BCF", val) && val) return true;
  else if (p.m_clust.empty()) return true;

  return false;
} // NativePackExp::SkipPar_Pval_Sen
//------------------------------------------------------------------------------
/// \brief virtual
//------------------------------------------------------------------------------
void NativePackExp::LastChanceBeforeWriting ()
{
} // NativePackExp::LastChanceBeforeWriting
//------------------------------------------------------------------------------
/// \brief virtual
//------------------------------------------------------------------------------
bool NativePackExp::ClearFile ()
{
  return m_p->m_native->GetExp()->ClearFile(m_p->m_package->PackageName().c_str());
} // NativePackExp::ClearFile
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativePackExp::UnitTestingDeletePointers ()
{
  if (m_p->m_global) delete(m_p->m_global);
  if (m_p->m_native) delete(m_p->m_native);
  if (m_p->m_package) delete(m_p->m_package);
} // NativePackExp::UnitTestingDeletePointers

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST
#endif

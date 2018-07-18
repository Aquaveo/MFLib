//------------------------------------------------------------------------------
// FILE      MfNativeExpMf6Ic.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/MfData/MfExport/private/Native/mf6/NativeExpMf6Ic.h>

#include <sstream>

#include <private/MfData/MfGlobal.h>
#include <private/MfData/MfExport/private/Mf2kNative.h>
#include <private/MfData/MfExport/private/MfExporterImpl.h>
#include <private/MfData/MfExport/private/MfExportUtil.h>
#include <private/MfData/MfExport/private/Native/NativePackExp.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackFields.h>
#include <private/MfData/Packages/MfPackStrings.h>

using namespace MfData::Export;

//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMf6Ic::NativeExpMf6Ic (NativePackExp* a_) :
m_pack(a_)
{
} // MfNativeExpMf6Ic::MfNativeExpMf6Ic
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMf6Ic::~NativeExpMf6Ic ()
{
} // MfNativeExpMf6Ic::~MfNativeExpMf6Ic
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpMf6Ic::Export ()
{
  if (!m_pack) return false;

  std::vector<CStr> lines, comments;
  // get the time units
  MfGlobal *g = m_pack->GetGlobal();
  if (!g) return false;
  Mf2kNative* nat = m_pack->GetNative();
  if (!nat) return false;

  // comments
  lines.push_back(MfExportUtil::GetMf6CommentHeader());

  int layered(true);
  g->GetIntVar("ARRAYS_LAYERED", layered);

  std::string str;

  lines.push_back("BEGIN GRIDDATA");
  str = "  STRT"; 
  if (layered) str += " LAYERED";
  lines.push_back(str);
  lines.push_back(MfExportUtil::GetMf6ArrayString(g, nat, ARR_BAS_SHEAD));
  lines.push_back("END GRIDDATA"); 

  comments.assign(lines.size(), "");
  TmpPackageNameChanger tmp(m_pack->GetPackage(), "ic");
  m_pack->AddToStoredLinesDesc(lines, comments);
  m_pack->WriteStoredLines();
  return true;
} // NativeExpMf6Ic::ExportMf6Ic
///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST


#endif
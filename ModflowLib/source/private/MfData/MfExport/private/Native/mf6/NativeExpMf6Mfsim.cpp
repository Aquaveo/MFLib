//------------------------------------------------------------------------------
// FILE      MfNativeExpMf6Mfsim.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\mf6\NativeExpMf6Mfsim.h>

#include <sstream>

#include <private\MfData\MfGlobal.h>
#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\TxtExporter.h>
#include <private\MfData\MfExport\private\MfExporterImpl.h>
#include <private\MfData\MfExport\private\MfExportUtil.h>
#include <private\MfData\MfExport\private\Native\NativePackExp.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackFields.h>
#include <private\MfData\Packages\MfPackStrings.h>

using namespace MfData::Export;

//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMf6Mfsim::NativeExpMf6Mfsim (NativePackExp* a_) :
m_pack(a_)
{
} // MfNativeExpMf6Mfsim::MfNativeExpMf6Mfsim
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMf6Mfsim::~NativeExpMf6Mfsim ()
{
} // MfNativeExpMf6Mfsim::~MfNativeExpMf6Mfsim
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpMf6Mfsim::Export ()
{
  if (!m_pack) return false;

  std::vector<CStr> lines, comments;  
  MfGlobal *g = m_pack->GetGlobal();
  if (!g) return false;
  Mf2kNative* n = m_pack->GetNative();
  if (!n) return false;
   
  // comments
  lines.push_back(MfExportUtil::GetMf6CommentHeader());

  lines.push_back("BEGIN OPTIONS"); 
  lines.push_back("END OPTIONS"); 
  lines.push_back("");

  CStr baseName;
  util::StripPathFromFilename(n->FileName(), baseName);
  util::StripExtensionFromFilename(baseName, baseName);
  CStr fname =  baseName + ".tdis";
  lines.push_back("BEGIN TIMING");
  lines.push_back("  TDIS6 " + fname);
  lines.push_back("END TIMING");
  lines.push_back("");

  fname = baseName + ".mfn";
  lines.push_back("BEGIN MODELS");
  lines.push_back("  GWF6 " + fname + " GWF_Model"); 
  lines.push_back("END MODELS");
  lines.push_back("");

  lines.push_back("BEGIN EXCHANGES");
  lines.push_back("END EXCHANGES");
  lines.push_back("");

  fname = baseName + ".ims";
  lines.push_back("BEGIN SOLUTIONGROUP 1");
  lines.push_back("  MXITER 1");
  lines.push_back("  IMS6 "+ fname + " GWF_Model");
  lines.push_back("END SOLUTIONGROUP");
  lines.push_back("");

  comments.assign(lines.size(), "");
  TmpPackageNameChanger tmp(m_pack->GetPackage(), "nam");
  CStr old = m_pack->GetNative()->GetExp()->GetBaseFileName();
  CStr tmpName;
  util::StripFileFromFilename(old, tmpName);
  tmpName += "mfsim";
  m_pack->GetNative()->GetExp()->SetBaseFileName(tmpName);
  m_pack->AddToStoredLinesDesc(lines, comments);
  m_pack->WriteStoredLines();
  m_pack->GetNative()->GetExp()->SetBaseFileName(old);
  return true;
} // NativeExpMf6Mfsim::ExportMf6Sto

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST


#endif
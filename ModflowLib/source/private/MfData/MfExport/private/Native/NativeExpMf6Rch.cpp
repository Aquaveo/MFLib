//------------------------------------------------------------------------------
// FILE      MfNativeExpMf6Rch.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpMf6Rch.h>

#include <sstream>

#include <private\MfData\MfGlobal.h>
#include <private\MfData\MfExport\private\Mf2kNative.h>
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
NativeExpMf6Rch::NativeExpMf6Rch (NativePackExp* a_) :
m_pack(a_)
{
} // MfNativeExpMf6Rch::MfNativeExpMf6Rch
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMf6Rch::~NativeExpMf6Rch ()
{
} // MfNativeExpMf6Rch::~MfNativeExpMf6Rch
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpMf6Rch::Export ()
{
  if (!m_pack) return false;

  std::vector<CStr> lines, comments; 
  MfGlobal *g = m_pack->GetGlobal();
  if (!g) return false;
  Mf2kNative* nat = m_pack->GetNative();
  if (!nat) return false;

  // INRECH and INIRCH
  const int* INRECH(0),* INIRCH(0),* NRCHOP(0),* IRCHCB(0);
  MfPackage* p = m_pack->GetPackage();
  if (!p ||
      !p->GetField(Packages::RCHpack::NRCHOP, &NRCHOP) || !NRCHOP ||
      !p->GetField(Packages::RCHpack::INIRCH, &IRCHCB) || !IRCHCB ||
      !p->GetField(Packages::RCHpack::INIRCH, &INIRCH) || !INIRCH ||
      !p->GetField(Packages::RCHpack::INRECH, &INRECH) || !INRECH) return false;

  if (1 == g->GetCurrentPeriod())
  {
    // comments
    lines.push_back(MfExportUtil::GetMf6CommentHeader());

    lines.push_back("BEGIN OPTIONS");
    lines.push_back("  READASARRAYS");
    if (3 != *NRCHOP) lines.push_back("  FIXED_CELL");
    if (*IRCHCB > 0)
    {
      g->SetIntVar("MF6_SAVE_FLOWS", 1);
      lines.push_back("  SAVE_FLOWS");
    }
    lines.push_back("END OPTIONS");
    lines.push_back("");
  }

  bool writeLayer(false);
  if (2 == *NRCHOP && *INIRCH > -1) writeLayer = true;

  // if both are < 0 then do nothing
  if (*INRECH < 0 && !writeLayer) return false;

  std::stringstream ss; 
  ss << "BEGIN PERIOD " << g->GetCurrentPeriod();
  lines.push_back(ss.str());
  // print array for stress period
  if (writeLayer)
  {
    lines.push_back("  IRCH LAYERED");
    lines.push_back(MfExportUtil::GetMf6ArrayString(g, nat, ARR_RCH_LAY));
  }
  if (*INRECH > -1)
  {
    lines.push_back("  RECHARGE LAYERED");
    lines.push_back(MfExportUtil::GetMf6ArrayString(g, nat, ARR_RCH_RCH));
  }

  lines.push_back("END PERIOD");
  lines.push_back("");
  comments.assign(lines.size(), "");
  //TmpPackageNameChanger tmp(m_pack->GetPackage(), "rch");
  m_pack->AddToStoredLinesDesc(lines, comments);
  m_pack->WriteStoredLines();
  return true;
} // NativeExpMf6Rch::ExportMf6Rch

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST


#endif
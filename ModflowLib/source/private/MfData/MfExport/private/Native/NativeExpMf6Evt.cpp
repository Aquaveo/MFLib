//------------------------------------------------------------------------------
// FILE      MfNativeExpMf6Evt.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpMf6Evt.h>

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
NativeExpMf6Evt::NativeExpMf6Evt (NativePackExp* a_) :
m_pack(a_)
{
} // MfNativeExpMf6Evt::MfNativeExpMf6Evt
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMf6Evt::~NativeExpMf6Evt ()
{
} // MfNativeExpMf6Evt::~MfNativeExpMf6Evt
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpMf6Evt::Export ()
{
  if (!m_pack) return false;
  std::vector<CStr> lines, comments; 
  MfGlobal *g = m_pack->GetGlobal();
  if (!g) return false;
  Mf2kNative* nat = m_pack->GetNative();
  if (!nat) return false;
  const int *NEVTOP(0), *INSURF(0), *INEVTR(0), *INEXDP(0), *INIEVT(0),* IEVTCB(0);
  MfPackage* p = m_pack->GetPackage();
  if (!p->GetField(Packages::EVTpack::NEVTOP, &NEVTOP) || !NEVTOP ||
      !p->GetField(Packages::EVTpack::INSURF, &INSURF) || !INSURF ||
      !p->GetField(Packages::EVTpack::INEVTR, &INEVTR) || !INEVTR ||
      !p->GetField(Packages::EVTpack::INEXDP, &INEXDP) || !INEXDP ||
      !p->GetField(Packages::EVTpack::INIEVT, &INIEVT) || !INIEVT ||
      !p->GetField("IEVTCB", &IEVTCB) || !IEVTCB)
    return false;

  if (1 == g->GetCurrentPeriod())
  {
    // comments
    lines.push_back(MfExportUtil::GetMf6CommentHeader());

    lines.push_back("BEGIN OPTIONS");
    lines.push_back("  READASARRAYS");
    if (3 != *NEVTOP) lines.push_back("  FIXED_CELL");
    if (*IEVTCB > 0)
    {
      g->SetIntVar("MF6_SAVE_FLOWS", 1);
      lines.push_back("  SAVE_FLOWS");
    }
    lines.push_back("END OPTIONS");
    lines.push_back("");

    lines.push_back("");
  }

  bool writeLayer(false);
  if (3 == *NEVTOP && *INIEVT > -1) writeLayer = true;

  if (!writeLayer && *INSURF < 0 && *INEVTR < 0 && *INEXDP < 0) return false;

  std::stringstream ss; 
  ss << "BEGIN PERIOD " << g->GetCurrentPeriod();
  lines.push_back(ss.str());
  if (writeLayer)
  {
    lines.push_back("  IEVT LAYERED");
    lines.push_back(MfExportUtil::GetMf6ArrayString(g, nat, ARR_EVT_LAY));
  }
  if (*INSURF > -1)
  {
    lines.push_back("  SURFACE LAYERED");
    lines.push_back(MfExportUtil::GetMf6ArrayString(g, nat, ARR_EVT_SURF));
  }
  if (*INEVTR > -1)
  {
    lines.push_back("  RATE LAYERED");
    lines.push_back(MfExportUtil::GetMf6ArrayString(g, nat, ARR_EVT_RATE));
  }
  if (*INEXDP > -1)
  {
    lines.push_back("  DEPTH LAYERED");
    lines.push_back(MfExportUtil::GetMf6ArrayString(g, nat, ARR_EVT_EXT));
  }
  lines.push_back("END PERIOD");
  lines.push_back("");
  comments.assign(lines.size(), ""); 
  m_pack->AddToStoredLinesDesc(lines, comments);
  m_pack->WriteStoredLines();
  return true;
} // NativeExpMf6Evt::Export

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST


#endif
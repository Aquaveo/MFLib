//------------------------------------------------------------------------------
// FILE      MfNativeExpMf6Evt.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\mf6\NativeExpMf6Evt.h>

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
  const int *NEVTOP(0), *INSURF(0), *INEVTR(0), *INEXDP(0), *INIEVT(0),
    * IEVTCB(0),* MXNDEVT(0);
  MfPackage* p = m_pack->GetPackage();
  if (!p->GetField(Packages::EVTpack::NEVTOP, &NEVTOP) || !NEVTOP ||
      !p->GetField(Packages::EVTpack::INSURF, &INSURF) || !INSURF ||
      !p->GetField(Packages::EVTpack::INEVTR, &INEVTR) || !INEVTR ||
      !p->GetField(Packages::EVTpack::INEXDP, &INEXDP) || !INEXDP ||
      !p->GetField(Packages::EVTpack::INIEVT, &INIEVT) || !INIEVT ||
      !p->GetField("IEVTCB", &IEVTCB) || !IEVTCB)
    return false;
  p->GetField(Packages::EVTpack::MXNDEVT, &MXNDEVT);

  // need number of cells in layer 1
  MfPackage* p1 = g->GetPackage(Packages::DISU);
  const int* NODLAY(0);
  p1->GetField(Packages::Disu::NODLAY, &NODLAY);

  int MAXBOUND = g->NumCol() * g->NumRow();
  if (NODLAY) MAXBOUND = NODLAY[0];
  if (MXNDEVT) MAXBOUND = *MXNDEVT; 

  int layers(1);
  g->GetIntVar("ARRAYS_LAYERED", layers);
  // unstructured grid with this option means the cell id is specified
  if (2 == *NEVTOP && g->Unstructured()) layers = 0;

  if (1 == g->GetCurrentPeriod())
  {
    // comments
    lines.push_back(MfExportUtil::GetMf6CommentHeader());

    lines.push_back("BEGIN OPTIONS");
    if (layers)
    {
      lines.push_back("  READASARRAYS");
      if (3 != *NEVTOP) lines.push_back("  FIXED_CELL");
    }
    if (*IEVTCB > 0)
    {
      g->SetIntVar("MF6_SAVE_FLOWS", 1);
      lines.push_back("  SAVE_FLOWS");
    }
    lines.push_back("END OPTIONS");
    lines.push_back("");

    if (!layers)
    {
      lines.push_back("BEGIN DIMENSIONS");
      std::stringstream ss;
      ss << "  MAXBOUND " << MAXBOUND;
      lines.push_back(ss.str());
      lines.push_back("END DIMENSIONS");
      lines.push_back("");
    }
  }

  bool writeLayer(false);
  if (2 == *NEVTOP && *INIEVT > -1) writeLayer = true;

  if (!writeLayer && *INSURF < 0 && *INEVTR < 0 && *INEXDP < 0) return false;

  std::stringstream ss; 
  ss << "BEGIN PERIOD " << g->GetCurrentPeriod();
  lines.push_back(ss.str());

  CStr layStr, surfStr, rateStr, exdpStr;
  if (writeLayer)
  {
    layStr = MfExportUtil::GetMf6ArrayString(g, nat, ARR_EVT_LAY);
    g->SetStrVar(ARR_EVT_LAY, layStr);
    if (layers)
    {
      lines.push_back("  IEVT LAYERED");
      lines.push_back(layStr);
    }
  }
  if (*INSURF > -1)
  {
    surfStr = MfExportUtil::GetMf6ArrayString(g, nat, ARR_EVT_SURF);
    g->SetStrVar(ARR_EVT_SURF, surfStr);
    if (layers)
    {
      lines.push_back("  SURFACE LAYERED");
      lines.push_back(surfStr);
    }
  }
  if (*INEVTR > -1)
  {
    rateStr = MfExportUtil::GetMf6ArrayString(g, nat, ARR_EVT_RATE);
    g->SetStrVar(ARR_EVT_RATE, rateStr);
    if (layers)
    {
      lines.push_back("  RATE LAYERED");
      lines.push_back(rateStr);
    }
  }
  if (*INEXDP > -1)
  {
    exdpStr = MfExportUtil::GetMf6ArrayString(g, nat, ARR_EVT_EXT);
    g->SetStrVar(ARR_EVT_EXT, exdpStr);
    if (layers)
    {
      lines.push_back("  DEPTH LAYERED");
      lines.push_back(exdpStr);
    }
  }

  if (!layers)
  {
    CStr disPackType;
    g->GetStrVar("DIS_PACKAGE_TYPE", disPackType);
    const int* NODLAY(0);
    MfPackage* p = g->GetPackage(Packages::DISU);
    p->GetField(Packages::Disu::NODLAY, &NODLAY);

    std::vector<int> cellids;
    std::vector<Real> surf, rate, exdp;

    g->GetStrVar(ARR_EVT_LAY, layStr);
    g->GetStrVar(ARR_EVT_SURF, surfStr);
    g->GetStrVar(ARR_EVT_RATE, rateStr);
    g->GetStrVar(ARR_EVT_EXT, exdpStr);

    if (!layStr.empty())
      MfExportUtil::Mf6StringToArray(layStr, cellids, MAXBOUND);
    MfExportUtil::Mf6StringToArray(surfStr, surf, MAXBOUND);
    MfExportUtil::Mf6StringToArray(rateStr, rate, MAXBOUND);
    MfExportUtil::Mf6StringToArray(exdpStr, exdp, MAXBOUND);

    if (cellids.empty())
    {
      cellids.reserve(surf.size());
      for (size_t i=0; i<surf.size(); ++i)
        cellids.push_back((int)i+1);
    }

    int w = util::RealWidth();
    CStr str;
    std::stringstream ss;
    for (size_t i=0; i<cellids.size(); ++i)
    {
      str.Format("%10d", cellids[i]);
      if ("DISV" == disPackType && NODLAY)
      {
        int begId(0), endId(0), idInLay, layId;
        for (int k=0; k<g->NumLay(); ++k)
        {
          endId += NODLAY[k];
          if (cellids[i] <= endId)
          {
            idInLay = cellids[i] - begId;
            layId = k + 1;
            break;
          }
          begId += NODLAY[k];
        }
        str.Format("%5d %10d", layId, idInLay);
      }
      ss << "  " << str << " "
          << STR(surf[i], -1, w, STR_FULLWIDTH) << " "
          << STR(rate[i], -1, w, STR_FULLWIDTH) << " "
          << STR(exdp[i], -1, w, STR_FULLWIDTH);
      if (i+1 < cellids.size()) ss << "\n";
    }
    lines.push_back(ss.str());
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
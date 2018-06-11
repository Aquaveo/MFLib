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
#include <private\MfData\MfExport\private\CellNumbering.h>
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
  if (!p) return false;

  bool etsPack = p->PackageName() == "ETS";

  if (!p->GetField(Packages::EVTpack::NEVTOP, &NEVTOP) || !NEVTOP ||
      !p->GetField(Packages::EVTpack::INSURF, &INSURF) || !INSURF ||
      !p->GetField(Packages::EVTpack::INEVTR, &INEVTR) || !INEVTR ||
      !p->GetField(Packages::EVTpack::INEXDP, &INEXDP) || !INEXDP ||
      !p->GetField(Packages::EVTpack::INIEVT, &INIEVT) || !INIEVT)
    return false;
  if (etsPack) p->GetField("IETSCB", &IEVTCB);
  else         p->GetField("IEVTCB", &IEVTCB);
  if (!IEVTCB) return false;
  const int* NETSEG(0),* INSGDF;
  p->GetField(Packages::EVTpack::NETSEG, &NETSEG);
  p->GetField(Packages::EVTpack::INSGDF, &INSGDF);
  p->GetField(Packages::EVTpack::MXNDEVT, &MXNDEVT);

  // need number of cells in layer 1
  MfPackage* p1 = g->GetPackage(Packages::DISU);
  const int* NODLAY(0);
  if (p1) p1->GetField(Packages::Disu::NODLAY, &NODLAY);

  int MAXBOUND = g->NumCol() * g->NumRow();
  if (NODLAY) MAXBOUND = NODLAY[0];
  if (MXNDEVT) MAXBOUND = *MXNDEVT; 

  int layers(1);
  g->GetIntVar("ARRAYS_LAYERED", layers);
  // unstructured grid with this option means the cell id is specified
  if (2 == *NEVTOP && g->Unstructured()) layers = 0;
  if (etsPack) layers = 0;

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
      if (NETSEG && *NETSEG > 1)
      {
        std::stringstream ss1;
        ss1 << "  NSEG " << *NETSEG;
        lines.push_back(ss1.str());
      }
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

  CStr arrayLay(ARR_EVT_LAY), arraySurf(ARR_EVT_SURF),
       arrayRate(ARR_EVT_RATE), arrayExdp(ARR_EVT_EXT);
  if (etsPack)
  {
    arrayLay = ARR_ETS_LAY;
    arraySurf = ARR_ETS_SURF;
    arrayRate = ARR_ETS_RATE;
    arrayExdp = ARR_ETS_EXT;
  }

  CStr layStr, surfStr, rateStr, exdpStr, pxdpStr, petmStr;
  if (writeLayer)
  {
    layStr = MfExportUtil::GetMf6ArrayString(g, nat, arrayLay);
    g->SetStrVar(ARR_EVT_LAY, layStr);
    if (layers)
    {
      lines.push_back("  IEVT LAYERED");
      lines.push_back(layStr);
    }
  }
  if (*INSURF > -1)
  {
    surfStr = MfExportUtil::GetMf6ArrayString(g, nat, arraySurf);
    g->SetStrVar(ARR_EVT_SURF, surfStr);
    if (layers)
    {
      lines.push_back("  SURFACE LAYERED");
      lines.push_back(surfStr);
    }
  }
  if (*INEVTR > -1)
  {
    rateStr = MfExportUtil::GetMf6ArrayString(g, nat, arrayRate);
    g->SetStrVar(ARR_EVT_RATE, rateStr);
    if (layers)
    {
      lines.push_back("  RATE LAYERED");
      lines.push_back(rateStr);
    }
  }
  if (*INEXDP > -1)
  {
    exdpStr = MfExportUtil::GetMf6ArrayString(g, nat, arrayExdp);
    g->SetStrVar(ARR_EVT_EXT, exdpStr);
    if (layers)
    {
      lines.push_back("  DEPTH LAYERED");
      lines.push_back(exdpStr);
    }
  }
  if (NETSEG && *NETSEG > 1 && INSGDF && *INSGDF > -1)
  {
    pxdpStr = MfExportUtil::GetMf6ArrayString(g, nat, ARR_ETS_PXDP);
    g->SetStrVar(ARR_ETS_PXDP, pxdpStr);
    petmStr = MfExportUtil::GetMf6ArrayString(g, nat, ARR_ETS_PETM);
    g->SetStrVar(ARR_ETS_PETM, petmStr);
  }

  if (!layers)
  {
    CellNumbering* cn = nat->GetCellNumbering();
    ASSERT(cn);
    CStr disPackType("DIS");
    g->GetStrVar("DIS_PACKAGE_TYPE", disPackType);
    const int* NODLAY(0);
    MfPackage* p = g->GetPackage(Packages::DISU);
    if (p) p->GetField(Packages::Disu::NODLAY, &NODLAY);

    std::vector<int> cellids;
    std::vector<Real> surf, rate, exdp, pxdp, petm;

    g->GetStrVar(ARR_EVT_LAY, layStr);
    g->GetStrVar(ARR_EVT_SURF, surfStr);
    g->GetStrVar(ARR_EVT_RATE, rateStr);
    g->GetStrVar(ARR_EVT_EXT, exdpStr);
    g->GetStrVar(ARR_ETS_PXDP, pxdpStr);
    g->GetStrVar(ARR_ETS_PETM, petmStr);

    if (!layStr.empty())
      MfExportUtil::Mf6StringToArray(layStr, cellids, MAXBOUND);
    MfExportUtil::Mf6StringToArray(surfStr, surf, MAXBOUND);
    MfExportUtil::Mf6StringToArray(rateStr, rate, MAXBOUND);
    MfExportUtil::Mf6StringToArray(exdpStr, exdp, MAXBOUND);
    if (!pxdpStr.empty())
    {
      MfExportUtil::Mf6MultiLayerStringToArray(pxdpStr, pxdp, *NETSEG-1, MAXBOUND);
      MfExportUtil::Mf6MultiLayerStringToArray(petmStr, petm, *NETSEG-1, MAXBOUND);
    }

    if (cellids.empty())
    { // this will only happen with a DIS package, ETS was not supported with USG
      cellids.reserve(surf.size());
      for (size_t i=0; i<surf.size(); ++i)
        cellids.push_back((int)i+1);
      if (3 == *NEVTOP)
      {
        std::vector<std::vector<int>>& ibound(nat->Ibound());
        for (size_t i=0; i<cellids.size(); ++i)
        {
          if (ibound[0][i] != 0) continue;
          bool done(false);
          for (int k=1; done && k<g->NumLay(); ++k)
          {
            if (ibound[k][i] != 0)
            {
              done = true;
              int ci, cj, ck;
              cn->IjkFromId(ci, cj, ck, cellids[i]);
              cellids[i] = cn->IdFromIjk(ci, cj, k+1);
            }
          }
        }
      }
    }
    else if ("DIS" == disPackType)
    { // If the flow package is dis then we are here because this is the ETS
      // package. ETS is only supported by writing the list format of the evt
      // file.
      int cnt(0);
      // convert from IJK to id
      for (int i=0; i<g->NumRow(); ++i)
      {
        for (int j=0; j<g->NumCol(); ++j)
        {
          cellids[cnt] = cn->IdFromIjk(i+1, j+1, cellids[cnt]);
          cnt++;
        }
      }
    }

    int w = util::RealWidth();
    CStr str;
    std::stringstream ss;
    for (size_t i=0; i<cellids.size(); ++i)
    {
      str = cn->CellIdStringFromId(cellids[i]);
      ss << "  " << str
          << STR(surf[i], -1, w, STR_FULLWIDTH) << " "
          << STR(rate[i], -1, w, STR_FULLWIDTH) << " "
          << STR(exdp[i], -1, w, STR_FULLWIDTH);
      // if ETS write additional data
      if (!petm.empty())
      {
        for (int n=0; n<*NETSEG-1; ++n)
        {
          int idx = (int)i + (n * MAXBOUND);
          ss << " " << STR(pxdp[idx], -1, w, STR_FULLWIDTH);
        }
        for (int n=0; n<*NETSEG-1; ++n)
        {
          int idx = (int)i + (n * MAXBOUND);
          ss << " " << STR(petm[idx], -1, w, STR_FULLWIDTH);
        }
      }
      if (i+1 < cellids.size()) ss << "\n";
    }
    lines.push_back(ss.str());
  }

  lines.push_back("END PERIOD");
  lines.push_back("");
  comments.assign(lines.size(), ""); 
  TmpPackageNameChanger tmp(p, "EVT");
  m_pack->AddToStoredLinesDesc(lines, comments);
  m_pack->WriteStoredLines();
  return true;
} // NativeExpMf6Evt::Export

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST


#endif
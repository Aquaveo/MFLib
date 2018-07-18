//------------------------------------------------------------------------------
// FILE      MfNativeExpMf6Rch.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/MfData/MfExport/private/Native/mf6/NativeExpMf6Rch.h>

#include <sstream>

#include <private/MfData/MfGlobal.h>
#include <private/MfData/MfExport/private/CellNumbering.h>
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
  const int* INRECH(0),* INIRCH(0),* NRCHOP(0),* IRCHCB(0),* MXNDRCH(0);
  MfPackage* p = m_pack->GetPackage();
  if (!p ||
      !p->GetField(Packages::RCHpack::NRCHOP, &NRCHOP) || !NRCHOP ||
      !p->GetField(Packages::RCHpack::INIRCH, &IRCHCB) || !IRCHCB ||
      !p->GetField(Packages::RCHpack::INIRCH, &INIRCH) || !INIRCH ||
      !p->GetField(Packages::RCHpack::INRECH, &INRECH) || !INRECH) return false;
  p->GetField(Packages::RCHpack::MXNDRCH, &MXNDRCH);

  // need number of cells in layer 1
  MfPackage* p1 = g->GetPackage(Packages::DISU);
  const int* NODLAY(0);
  if (p1) p1->GetField(Packages::Disu::NODLAY, &NODLAY);

  int MAXBOUND = g->NumCol() * g->NumRow();
  if (NODLAY) MAXBOUND = NODLAY[0];
  if (MXNDRCH) MAXBOUND = *MXNDRCH; 

  int layers(1);
  g->GetIntVar("ARRAYS_LAYERED", layers);
  // unstructured grid with this option means the cell id is specified
  if (2 == *NRCHOP && g->Unstructured()) layers = 0;

  if (1 == g->GetCurrentPeriod())
  {
    // comments
    lines.push_back(MfExportUtil::GetMf6CommentHeader());

    lines.push_back("BEGIN OPTIONS");
    if (layers)
    {
      lines.push_back("  READASARRAYS");
      if (3 != *NRCHOP) lines.push_back("  FIXED_CELL");
    }
    if (*IRCHCB > 0)
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
  if (2 == *NRCHOP && *INIRCH > -1) writeLayer = true;

  // if both are < 0 then do nothing
  if (*INRECH < 0 && !writeLayer) return false;

  std::stringstream ss; 
  ss << "BEGIN PERIOD " << g->GetCurrentPeriod();
  lines.push_back(ss.str());

  // print array for stress period
  CStr layStr, rateStr;
  if (writeLayer)
  {
    layStr = MfExportUtil::GetMf6ArrayString(g, nat, ARR_RCH_LAY);
    g->SetStrVar(ARR_RCH_LAY, layStr);
    if (layers)
    {
      lines.push_back("  IRCH LAYERED");
      lines.push_back(layStr);
    }
  }

  if (*INRECH > -1)
  {
    rateStr = MfExportUtil::GetMf6ArrayString(g, nat, ARR_RCH_RCH);
    g->SetStrVar(ARR_RCH_RCH, rateStr);
    if (layers)
    {
      lines.push_back("  RECHARGE LAYERED");
      lines.push_back(rateStr);
    }
  }

  if (!layers)
  {
    CellNumbering* cn = nat->GetCellNumbering();
    ASSERT(cn);
    CStr disPackType;
    g->GetStrVar("DIS_PACKAGE_TYPE", disPackType);
    const int* NODLAY(0);
    MfPackage* p = g->GetPackage(Packages::DISU);
    p->GetField(Packages::Disu::NODLAY, &NODLAY);

    std::vector<int> cellids;
    std::vector<Real> rate;

    g->GetStrVar(ARR_RCH_LAY, layStr);
    g->GetStrVar(ARR_RCH_RCH, rateStr);
    if (!layStr.empty())
      MfExportUtil::Mf6StringToArray(layStr, cellids, MAXBOUND);
    MfExportUtil::Mf6StringToArray(rateStr, rate, MAXBOUND);

    if (cellids.empty())
    {
      cellids.reserve(rate.size());
      for (size_t i=0; i<rate.size(); ++i)
        cellids.push_back((int)i+1);
    }

    int w = util::RealWidth();
    CStr str;
    std::stringstream ss;
    for (size_t i=0; i<cellids.size(); ++i)
    {
      str = cn->CellIdStringFromId(cellids[i]);
      ss << "  " << str
        << STR(rate[i], -1, w, STR_FULLWIDTH);
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
} // NativeExpMf6Rch::ExportMf6Rch



///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST


#endif
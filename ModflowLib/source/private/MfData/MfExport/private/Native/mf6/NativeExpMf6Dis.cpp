//------------------------------------------------------------------------------
// FILE      MfNativeExpMf6Dis.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\mf6\NativeExpMf6Dis.h>

#include <sstream>

#include <private\MfData\MfGlobal.h>
#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\MfExporterImpl.h>
#include <private\MfData\MfExport\private\MfExportUtil.h>
#include <private\MfData\MfExport\private\Native\NativePackExp.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackFields.h>
#include <private\MfData\Packages\MfPackStrings.h>


using namespace MfData;
using namespace MfData::Export;

//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMf6Dis::NativeExpMf6Dis (NativePackExp* a_) :
m_pack(a_)
{
} // MfNativeExpMf6Dis::MfNativeExpMf6Dis
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMf6Dis::~NativeExpMf6Dis ()
{
} // MfNativeExpMf6Dis::~MfNativeExpMf6Dis
//------------------------------------------------------------------------------
/// \brief
///BEGIN OPTIONS
///  [LENGTH_UNITS <length_units>]
///  [NOGRB]
///  [XORIGIN <xorigin>]
///  [YORIGIN <yorigin>]
///  [ANGROT <angrot>]
///END OPTIONS
///BEGIN DIMENSIONS
///  NLAY <nlay>
///  NROW <nrow>
///  NCOL <ncol>
///END DIMENSIONS
///BEGIN GRIDDATA
///  DELR [LAYERED]
///    <delr(ncol)> -- READARRAY
///  DELC [LAYERED]
///    <delc(nrow)> -- READARRAY
///  TOP [LAYERED]
///    <top(ncol, nrow)> -- READARRAY
///  BOTM [LAYERED]
///    <botm(ncol, nrow, nlay)> -- READARRAY
///  [IDOMAIN [LAYERED]
///    <idomain(ncol, nrow, nlay)> -- READARRAY]
///END GRIDDATA
//------------------------------------------------------------------------------
bool NativeExpMf6Dis::Export ()
{
  if (!m_pack) return false;

  std::vector<CStr> lines, desc;
  // get the time units
  MfGlobal *g = m_pack->GetGlobal();
  ASSERT(g);
  if (!g) return false;
  Mf2kNative *nat = m_pack->GetNative();
  ASSERT(nat);
  if (!nat) return false;
  // comments
  lines.push_back(MfExportUtil::GetMf6CommentHeader());

  lines.push_back("BEGIN OPTIONS");
  CStr lengthString("");
  int lengthUnits = g->LengthUnit();
  if (1 == lengthUnits) lengthString = "FEET";
  else if (2 == lengthUnits) lengthString = "METERS";
  else if (3 == lengthUnits) lengthString = "CENTIMETERS";
  if (!lengthString.empty())
  {
    std::stringstream ss;
    ss << " LENGTH_UNITS " << lengthString;
    lines.push_back(ss.str());
  }
  lines.push_back("  NOGRB");
  lines.push_back("END OPTIONS");
  lines.push_back("");

  lines.push_back("BEGIN DIMENSIONS");
  {
    std::stringstream ss;
    ss << " NLAY " << g->NumLay();
    lines.push_back(ss.str());
  }
  {
    std::stringstream ss;
    ss << " NROW " << g->NumRow();
    lines.push_back(ss.str());
  }
  {
    std::stringstream ss;
    ss << " NCOL " << g->NumCol();
    lines.push_back(ss.str());
  }
  lines.push_back("END DIMENSIONS");
  lines.push_back("");

  lines.push_back("BEGIN GRIDDATA");
  lines.push_back("  DELR");
  CStr delrStr = MfExportUtil::GetMf6ArrayString(g, nat, "DELR");
  g->SetStrVar("DELR", delrStr);
  lines.push_back(delrStr);
  lines.push_back("  DELC");
  CStr delcStr = MfExportUtil::GetMf6ArrayString(g, nat, "DELC");
  g->SetStrVar("DELC", delcStr);
  lines.push_back(delcStr);
  lines.push_back("  TOP LAYERED");
  lines.push_back(MfExportUtil::GetMf6ArrayString(g, nat, "TOP ELEVATION OF LAYER 1"));
  lines.push_back("  BOTM LAYERED");
  lines.push_back(MfExportUtil::GetMf6ArrayString(g, nat, "MODEL LAYER BOTTOM EL."));
  MfExportUtil::Mf6IboundToIdomain(g, nat);
  lines.push_back("  IDOMAIN LAYERED");
  lines.push_back(MfExportUtil::GetMf6ArrayString(g, nat, ARR_BAS_IBND));
  lines.push_back("END GRIDDATA");

  desc.assign(lines.size(), "");
  m_pack->AddToStoredLinesDesc(lines, desc);
  m_pack->WriteStoredLines();
  return true;
} // NativeExpMf6Dis::ExportMf6Dis

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST


#endif
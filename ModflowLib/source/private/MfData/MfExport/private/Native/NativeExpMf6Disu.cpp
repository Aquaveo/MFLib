//------------------------------------------------------------------------------
// FILE      MfNativeExpMf6Disuu.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpMf6Disu.h>

#include <sstream>

#include <private\MfData\MfGlobal.h>
#include <private\MfData\MfExport\private\MfExportUtil.h>
#include <private\MfData\MfExport\private\Native\NativePackExp.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackFields.h>


using namespace MfData;
using namespace MfData::Export;


//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMf6Disu::NativeExpMf6Disu (NativePackExp* a_) :
m_pack(a_)
{
} // MfNativeExpMf6Disu::MfNativeExpMf6Disu
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMf6Disu::~NativeExpMf6Disu ()
{
} // MfNativeExpMf6Disu::~MfNativeExpMf6Disu
//------------------------------------------------------------------------------
/// \brief
/// BEGIN OPTIONS
///     [LENGTH_UNITS <length_units>]
///     [NOGRB]
///     [XORIGIN <xorigin>]
///     [YORIGIN <yorigin>]
///     [ANGROT <angrot>]
/// END OPTIONS
/// 
/// BEGIN DIMENSIONS
///     NODES <nodes>
///     NJA <nja>
///     [NVERT <nvert>]
/// END DIMENSIONS
/// 
/// BEGIN GRIDDATA
///     TOP [LAYERED]
///     <top(nodes)> -- READARRAY
///     BOT [LAYERED]
///     <bot(nodes)> -- READARRAY
///     AREA [LAYERED]
///     <area(nodes)> -- READARRAY
/// END GRIDDATA
/// 
/// BEGIN CONNECTIONDATA
///     IAC [LAYERED]
///     <iac(nodes)> -- READARRAY
///     JA [LAYERED]
///     <ja(nja)> -- READARRAY
///     IHC [LAYERED]
///     <ihc(nja)> -- READARRAY
///     CL12 [LAYERED]
///     <cl12(nja)> -- READARRAY
///     HWVA [LAYERED]
///     <hwva(nja)> -- READARRAY
///     [ANGLDEGX [LAYERED]
///     <angldegx(nja)> -- READARRAY]
/// END CONNECTIONDATA
/// 
/// BEGIN VERTICES
///     <iv> <xv> <yv>
///     <iv> <xv> <yv>
///     ...
/// END VERTICES
/// 
/// BEGIN CELL2D
///     <icell2d> <xc> <yc> <ncvert> <icvert(ncvert)>
///     <icell2d> <xc> <yc> <ncvert> <icvert(ncvert)>
///     ...
/// END CELL2D
//------------------------------------------------------------------------------
bool NativeExpMf6Disu::Export ()
{
  if (!m_pack) return false;

  std::vector<CStr> lines, desc;
  // comments
  lines.push_back(MfExportUtil::GetMf6CommentHeader());

  WriteOptions(lines);
  WriteDimensions(lines);
  WriteGridData(lines);
  WriteConnections(lines);

  desc.assign(lines.size(), "");
  m_pack->AddToStoredLinesDesc(lines, desc);
  m_pack->WriteStoredLines();
  return true;
} // NativeExpMf6Disu::ExportMf6Dis
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Disu::WriteOptions (std::vector<CStr>& a_lines)
{
  // get the time units
  MfGlobal *g = m_pack->GetGlobal();
  if (!g) return;

  a_lines.push_back("BEGIN OPTIONS");
  CStr lengthString("");
  int lengthUnits = g->LengthUnit();
  if (1 == lengthUnits) lengthString = "FEET";
  else if (2 == lengthUnits) lengthString = "METERS";
  else if (3 == lengthUnits) lengthString = "CENTIMETERS";
  if (!lengthString.empty())
  {
    std::stringstream ss;
    ss << " LENGTH_UNITS " << lengthString;
    a_lines.push_back(ss.str());
  }
  a_lines.push_back("END OPTIONS");
  a_lines.push_back("");
} // NativeExpMf6Disu::WriteOptions
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Disu::WriteDimensions (std::vector<CStr>& a_lines)
{
  MfPackage* p = m_pack->GetPackage();
  a_lines.push_back("BEGIN DIMENSIONS");
  {
    const int* NODES(0), *NJAG(0);
    p->GetField(MfData::Packages::Disu::NODES, &NODES);
    p->GetField(MfData::Packages::Disu::NJAG, &NJAG);
    if (!NODES || !NJAG) return;

    {
      std::stringstream ss;
      ss << "  NODES " << *NODES;
      a_lines.push_back(ss.str());
    }
    {
      std::stringstream ss;
      ss << "  NJA " << *NJAG;
      a_lines.push_back(ss.str());
    }
  }
  a_lines.push_back("END DIMENSIONS");
  a_lines.push_back("");
} // NativeExpMf6Disu::WriteDimensions
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Disu::WriteGridData (std::vector<CStr>& a_lines)
{
  MfGlobal *g = m_pack->GetGlobal();
  if (!g) return;
  Mf2kNative* nat = m_pack->GetNative();
  if (!nat) return;

  a_lines.push_back("BEGIN GRIDDATA");

  using namespace MfData::Packages::Disu;
  a_lines.push_back("  TOP");
  a_lines.push_back(MfExportUtil::GetMf6ArrayString(g, nat, TOP));
  a_lines.push_back("  BOT");
  a_lines.push_back(MfExportUtil::GetMf6ArrayString(g, nat, BOT));
  a_lines.push_back("  AREA");
  a_lines.push_back(MfExportUtil::GetMf6ArrayString(g, nat, AREA));
  a_lines.push_back("END GRIDDATA");
  a_lines.push_back("");
} // NativeExpMf6Disu::WriteGridData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Disu::WriteConnections (std::vector<CStr>& a_lines)
{
  MfGlobal *g = m_pack->GetGlobal();
  if (!g) return;
  Mf2kNative* nat = m_pack->GetNative();
  if (!nat) return;
  a_lines.push_back("BEGIN CONNECTIONDATA");
  using namespace MfData::Packages::Disu;
  a_lines.push_back("  IAC");
  a_lines.push_back(MfExportUtil::GetMf6ArrayString(g, nat, IA));
  a_lines.push_back("  JA");
  a_lines.push_back(MfExportUtil::GetMf6ArrayString(g, nat, JA));
  if (g->GetPackage(IVC))
  {
    a_lines.push_back("  IHC");
    a_lines.push_back(MfExportUtil::GetMf6ArrayString(g, nat, IVC));
  }
  a_lines.push_back("  CL12");
  a_lines.push_back(MfExportUtil::GetMf6ArrayString(g, nat, CL12));
  a_lines.push_back("  HWVA");
  a_lines.push_back(MfExportUtil::GetMf6ArrayString(g, nat, FAHL));
  if (g->GetPackage("FACE ANGLE"))
  {
    a_lines.push_back("  ANGLEDEGX LAYERED");
    a_lines.push_back(MfExportUtil::GetMf6ArrayString(g, nat, "FACE ANGLE"));
  }
  a_lines.push_back("END CONNECTIONDATA");
  a_lines.push_back("");
} // NativeExpMf6Disu::WriteConnections

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST


#endif
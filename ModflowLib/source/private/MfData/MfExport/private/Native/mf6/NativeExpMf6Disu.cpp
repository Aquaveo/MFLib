//------------------------------------------------------------------------------
// FILE      MfNativeExpMf6Disuu.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\mf6\NativeExpMf6Disu.h>

#include <fstream>
#include <sstream>

#include <private\MfData\MfGlobal.h>
#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\MfExportUtil.h>
#include <private\MfData\MfExport\private\Native\NativePackExp.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackFields.h>


using namespace MfData;
using namespace MfData::Export;

struct point
{
  double x, y, z;
};

class NativeExpMf6Disu::impl
{
public:
  impl(NativePackExp* a_) : m_pack(a_) {}
  bool ReadGsf();

  NativePackExp* m_pack;
  std::vector<point> m_pts;
  std::vector<point> m_cellCenters;
  std::vector<std::vector<int>> m_cellVerts;
};

//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMf6Disu::NativeExpMf6Disu (NativePackExp* a_) :
m_pack(a_)
, m_p(new impl(a_))
{
} // MfNativeExpMf6Disu::MfNativeExpMf6Disu
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMf6Disu::~NativeExpMf6Disu ()
{
  if (m_p)
  {
    delete(m_p);
    m_p = nullptr;
  }
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
  MfGlobal *g = m_pack->GetGlobal();
  if (!g) return false;

  std::vector<CStr> lines, desc;
  // comments
  lines.push_back(MfExportUtil::GetMf6CommentHeader());
  DisuWriteOptions(lines);

  bool disv = WriteDisv();
  if (!disv)
  {
    g->SetIntVar("ARRAYS_LAYERED", false);
    DisuWriteDimensions(lines);
    DisuWriteGridData(lines);
    DisuWriteConnections(lines);
  }
  else
  {
    DisvWriteDimensions(lines);
    DisvWriteGridData(lines);
    DisvWriteVerts(lines);
    DisvWriteCell2d(lines);
  }

  desc.assign(lines.size(), "");
  CStr pname("DISU");
  if (disv) pname = "DISV";
  TmpPackageNameChanger tmp(m_pack->GetPackage(), pname.c_str());
  m_pack->AddToStoredLinesDesc(lines, desc);
  m_pack->WriteStoredLines();
  return true;
} // NativeExpMf6Disu::ExportMf6Dis
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpMf6Disu::WriteDisv ()
{
  const int* IVSD(0);
  m_pack->GetPackage()->GetField(MfData::Packages::Disu::IVSD, &IVSD);
  if (IVSD && *IVSD < 0)
  {
    if (!m_p->ReadGsf()) return false;
    return true;
  }

  return false;
} // NativeExpMf6Disu::WriteDisv
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Disu::DisuWriteOptions (std::vector<CStr>& a_lines)
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
  a_lines.push_back("  NOGRB");
  a_lines.push_back("END OPTIONS");
  a_lines.push_back("");
} // NativeExpMf6Disu::DisuWriteOptions
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Disu::DisuWriteDimensions (std::vector<CStr>& a_lines)
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
} // NativeExpMf6Disu::DisuWriteDimensions
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Disu::DisuWriteGridData (std::vector<CStr>& a_lines)
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
} // NativeExpMf6Disu::DisuWriteGridData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Disu::DisuWriteConnections (std::vector<CStr>& a_lines)
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
} // NativeExpMf6Disu::DisuWriteConnections
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Disu::DisvWriteDimensions (std::vector<CStr>& a_lines)
{
  MfGlobal *g = m_pack->GetGlobal();
  a_lines.push_back("BEGIN DIMENSIONS");
  {
    std::stringstream ss;
    ss << "  NLAY " << g->NumLay();
    a_lines.push_back(ss.str());
  }
  {
    std::stringstream ss;
    ss << "  NCPL " << m_p->m_cellCenters.size();
    a_lines.push_back(ss.str());
  }
  {
    std::stringstream ss;
    ss << "  NVERT " << m_p->m_pts.size();
    a_lines.push_back(ss.str());
  }
  a_lines.push_back("END DIMENSIONS");
  a_lines.push_back("");
} // NativeExpMf6Disu::DisuWriteDimensions
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Disu::DisvWriteGridData (std::vector<CStr>& a_lines)
{
  MfGlobal *g = m_pack->GetGlobal();
  Mf2kNative *nat = m_pack->GetNative();
  using namespace MfData::Packages;
  a_lines.push_back("BEGIN GRIDDATA");
  a_lines.push_back("  TOP");
  a_lines.push_back(MfExportUtil::GetMf6ArrayString(g, nat, Disu::TOP));
  a_lines.push_back("  BOTM LAYERED");
  a_lines.push_back(MfExportUtil::GetMf6ArrayString(g, nat, Disu::BOT));
  a_lines.push_back("  IDOMAIN LAYERED");
  a_lines.push_back(MfExportUtil::GetMf6ArrayString(g, nat, ARR_BAS_IBND));
  a_lines.push_back("END GRIDDATA");
  a_lines.push_back("");
} // NativeExpMf6Disu::DisvWriteGridData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Disu::DisvWriteVerts (std::vector<CStr>& a_lines)
{
  std::vector<point>& pts(m_p->m_pts);
  a_lines.push_back("BEGIN VERTICES");
  for (size_t i=0; i<pts.size(); ++i)
  {
    std::stringstream ss;
    ss << "  " << i+1 << " " << STR(pts[i].x) << " " << STR(pts[i].y);
    a_lines.push_back(ss.str());
  }
  a_lines.push_back("END VERTICES");
  a_lines.push_back("");
} // NativeExpMf6Disu::DisvWriteVerts
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Disu::DisvWriteCell2d (std::vector<CStr>& a_lines)
{
  std::vector<point>& cc(m_p->m_cellCenters);
  std::vector<std::vector<int>>& cells(m_p->m_cellVerts);
  a_lines.push_back("BEGIN CELL2D");
  for (size_t i=0; i<cc.size(); ++i)
  {
    std::stringstream ss;
    ss << "  " << i+1 << " " << STR(cc[i].x) << " " << STR(cc[i].y) << " "
       << cells[i].size();
    for (size_t j=0; j<cells[i].size(); ++j)
    {
      ss << " " << cells[i][j];
    }
    a_lines.push_back(ss.str());
  }
  a_lines.push_back("END CELL2D");
  a_lines.push_back("");
} // NativeExpMf6Disu::DisvWriteCell2d


//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpMf6Disu::impl::ReadGsf ()
{
  // the grid is stacked we now need to read the gsf file so we can
  // write the disv information
  Mf2kNative* n = m_pack->GetNative();
  if (!n) return false;

  CStr baseName = n->FileName();
  util::StripExtensionFromFilename(baseName, baseName);
  CStr fname =  baseName + ".gsf";

  std::string line, myStr;
  std::fstream is(fname, std::fstream::in);
  std::getline(is, line);
  if (line != "# Created by GMS") return false;
  // read line 1 - this line should say STRUCTURED or UNSTRUCTURED
  std::getline(is, line);

  // line 2
  int nnode, nlay, iz, ic;
  {
    std::getline(is, line);
    std::stringstream ss; 
    ss << line;
    ss >> nnode >> nlay >> iz >> ic;
    if (iz != 1 || ic != 1) return false;
  }
  // line 3
  int nvert;
  {
    std::getline(is, line);
    std::stringstream ss;
    ss << line;
    ss >> nvert;
    if (nvert < 1) return false;
  }
  // read the vert locations
  for (int i=0; i<nvert; ++i)
  {
    std::getline(is, line);
    std::stringstream ss;
    ss << line;
    point pt;
    ss >> pt.x >> pt.y >> pt.z;
    m_pts.push_back(pt);
  }
  // read NNODE
  for (int i=0; i<nnode; ++i)
  {
    if (!std::getline(is, line)) return false;
    std::stringstream ss;
    ss << line;
    point pt;
    int id, layer, numVert;
    ss >> id >> pt.x >> pt.y >> pt.z >> layer >> numVert;
    std::vector<int> verts(numVert, 0);
    for (int j = 0; j<numVert; ++j)
      ss >> verts[j];
    // only save layer 1 and we only want the top face
    if (layer == 1)
    {
      m_cellCenters.push_back(pt);
      // the top face are the verts in the last half of the array and we
      // want to reverse the order for MODFLOW
      std::vector<int> verts2;
      for (int j=numVert; j>numVert/2; --j)
      {
        verts2.push_back(verts[j-1]);
      }
      m_cellVerts.push_back(verts2);
    }
  }
  return true;
} // NativeExpMf6Disu::impl::ReadGsf


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST


#endif
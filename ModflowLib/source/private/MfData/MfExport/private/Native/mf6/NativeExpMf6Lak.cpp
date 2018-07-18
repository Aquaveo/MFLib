//------------------------------------------------------------------------------
// FILE      MfNativeExpMf6Lak.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/MfData/MfExport/private/Native/mf6/NativeExpMf6Lak.h>

#include <sstream>

#include <private/MfData/MfGlobal.h>
#include <private/MfData/MfExport/private/CellNumbering.h>
#include <private/MfData/MfExport/private/Mf2kNative.h>
#include <private/MfData/MfExport/private/MfExporterImpl.h>
#include <private/MfData/MfExport/private/MfExportUtil.h>
#include <private/MfData/MfExport/private/Mf2kNative.h>
#include <private/MfData/MfExport/private/Native/NativeExpLak.h>
#include <private/MfData/MfExport/private/Native/NativePackExp.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackFields.h>
#include <private/MfData/Packages/MfPackStrings.h>


using namespace MfData::Export;

class LakConnection
{
public:
  int m_cellId;
  CStr m_claktype;
  Real m_bedleak;
  Real m_connlen;
  Real m_connwidth;
};

class NativeExpMf6Lak::impl
{
public:
  impl(NativePackExp* a_) : m_pack(a_) {}

  void WriteOptions();
  void WriteDimensions();
  void WritePackageData();
  void WriteConnectionData();
  void WriteStressPeriodData();

  void CreateConnections();
  void AddConnectionsForLakeCell(int a_lakId, int a_cellId, Real a_bedleak,
    const std::vector<int>& a_ibound);
  void GetAdjacentActiveCells (int a_cellId,
    std::vector<int>& a_adjCellIds, std::vector<CStr>& a_claktyp,
    std::vector<Real>& a_width, std::vector<Real>& a_length,
    const std::vector<int>& a_ibound);

  NativePackExp* m_pack;
  std::vector<CStr> m_lines;
  std::vector<std::vector<LakConnection>> m_connections;
};
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMf6Lak::NativeExpMf6Lak (NativePackExp* a_) :
m_p(new impl(a_))

{
} // MfNativeExpMf6Lak::MfNativeExpMf6Lak
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMf6Lak::~NativeExpMf6Lak ()
{
  if (m_p) delete(m_p);
  m_p = nullptr;
} // MfNativeExpMf6Lak::~MfNativeExpMf6Lak
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpMf6Lak::Export ()
{
  if (!m_p) return false;

  if (m_p->m_pack->GetPackage()->PackageName() == Packages::LAK)
  {
    m_p->WriteOptions();
    m_p->WriteDimensions();

    std::vector<CStr> desc(m_p->m_lines.size(), "");
    m_p->m_pack->AddToStoredLinesDesc(m_p->m_lines, desc);
  }
  else
  {
    if (m_p->m_pack->GetGlobal()->GetCurrentPeriod() == 1)
    {
      m_p->CreateConnections();

      m_p->WritePackageData();
      m_p->WriteConnectionData();
    }
    m_p->WriteStressPeriodData();

    NativeExpLak* p = dynamic_cast<NativeExpLak*>(m_p->m_pack);
    if (p)
    {
      for (size_t i=0; i<m_p->m_lines.size(); ++i)
        p->AddToStoredLinesDesc(m_p->m_lines[i], "");
    }
  }

  return true;
} // NativeExpMf6Lak::ExportMf6Rch
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Lak::impl::WriteOptions ()
{
  MfPackage* p = m_pack->GetPackage();
  const int* ilkcb(0);
  const Real* surfdep(0);
  p->GetField(Packages::LAKpack::ILKCB, &ilkcb);
  p->GetField(Packages::LAKpack::SURFDEP, &surfdep);
  m_lines.push_back("BEGIN OPTIONS");
  if (ilkcb && *ilkcb > 0)
  {
    m_lines.push_back("  SAVE_FLOWS");
  }
  if (surfdep && *surfdep > 0)
  {
    m_lines.push_back("  SURFDEP " + STR(*surfdep));
  }
  m_lines.push_back("END OPTIONS");
  m_lines.push_back("");
} // NativeExpMf6Lak::impl::WriteOptions
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Lak::impl::WriteDimensions ()
{
  MfPackage* p = m_pack->GetPackage();
  const int*nlakes(0);
  p->GetField(Packages::LAKpack::NLAKES, &nlakes);
  if (!nlakes) return;
  m_lines.push_back("BEGIN DIMENSIONS");
  std::stringstream ss; 
  ss << "  NLAKES " << *nlakes;
  m_lines.push_back(ss.str());
  m_lines.push_back("END DIMENSIONS");
  m_lines.push_back("");
} // NativeExpMf6Lak::impl::WriteDimensions
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Lak::impl::WritePackageData ()
{
  MfPackage* p = m_pack->GetPackage();
  const Real* stages(0); 
  p->GetField(Packages::LAKSPpack::STAGES, &stages);
  if (!stages) return;

  int w = util::RealWidth();
  m_lines.push_back("BEGIN PACKAGEDATA");
  for (size_t i=0; i<m_connections.size(); ++i)
  {
    std::stringstream ss;
    CStr s1, s2;
    s1.Format("%5d", i+1);
    s2.Format("%5d", m_connections[i].size());
    ss << "  " << s1 << " " << STR(stages[i],-1,w,STR_FULLWIDTH) << " " << s2;
    m_lines.push_back(ss.str());
  }
  m_lines.push_back("END PACKAGEDATA");
  m_lines.push_back("");
} // NativeExpMf6Lak::impl::WritePackageData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Lak::impl::WriteConnectionData ()
{
  CellNumbering* cn = m_pack->GetNative()->GetCellNumbering();
  int w = util::RealWidth();
  m_lines.push_back("BEGIN CONNECTIONDATA");
  for (size_t i=0; i<m_connections.size(); ++i)
  {
    int lakId = (int)(i+1);
    std::vector<LakConnection>& cons(m_connections[i]);
    for (size_t j=0; j<cons.size(); ++j)
    {
      std::stringstream ss;
      CStr s1;
      s1.Format("  %5d %5d ", lakId, j+1);
      ss << s1 << cn->CellIdStringFromId(cons[j].m_cellId) << " " 
         << cons[j].m_claktype << " "
         << STR(cons[j].m_bedleak,-1,w,STR_FULLWIDTH)
         << " 0.0 0.0 "
         << STR(cons[j].m_connlen,-1,w,STR_FULLWIDTH) << " "
         << STR(cons[j].m_connwidth,-1,w,STR_FULLWIDTH);
      m_lines.push_back(ss.str());
    }
  }
  m_lines.push_back("END CONNECTIONDATA");
  m_lines.push_back("");
} // NativeExpMf6Lak::impl::WriteConnectionData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Lak::impl::WriteStressPeriodData ()
{
  int w = util::RealWidth();
  MfPackage* p = m_pack->GetPackage();
  const int *nlakes(0), *itmp1(0);
  const Real *rnf(0), *wthdrw(0);
  const double *prcplk(0), *evaplk(0);
  MfPackage* pLak = m_pack->GetGlobal()->GetPackage(Packages::LAK);
  if (pLak->GetField(Packages::LAKpack::NLAKES, &nlakes) && nlakes &&
      p->GetField(Packages::LAKSPpack::ITMP1, &itmp1) && itmp1 &&
      p->GetField(Packages::LAKSPpack::PRCPLK, &prcplk) && prcplk &&
      p->GetField(Packages::LAKSPpack::EVAPLK, &evaplk) && evaplk &&
      p->GetField(Packages::LAKSPpack::RNF, &rnf) && rnf &&
      p->GetField(Packages::LAKSPpack::WTHDRW, &wthdrw) && wthdrw )
  {
    std::stringstream ss;
    ss << "BEGIN PERIOD " << m_pack->GetGlobal()->GetCurrentPeriod();
    m_lines.push_back(ss.str());
    for (int i=0; i<*nlakes; ++i)
    {
      CStr lakId;
      lakId.Format("  %5d ", i+1);
      std::stringstream ss1;
      ss1 << lakId << "rainfall    " << STR(prcplk[i],-1,w,STR_FULLWIDTH) << "\n"
          << lakId << "evaporation " << STR(evaplk[i],-1,w,STR_FULLWIDTH) << "\n"
          << lakId << "runoff      " << STR(rnf[i],-1,w,STR_FULLWIDTH) << "\n"
          << lakId << "withdrawal  " << STR(wthdrw[i],-1,w,STR_FULLWIDTH);
      m_lines.push_back(ss1.str());
    }
    m_lines.push_back("END PERIOD");
    m_lines.push_back("");
  }

} // NativeExpMf6Lak::impl::WriteStressPeriodData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Lak::impl::CreateConnections ()
{
  MfGlobal* g = m_pack->GetGlobal();
  Mf2kNative* nat = m_pack->GetNative();
  int nLay = m_pack->GetGlobal()->NumLay();
  CellNumbering* cn = nat->GetCellNumbering();
  int ncellsPerLayer = cn->GetStackedNumberOfCellsPerLayer();

  std::vector<int> lakId;
  CStr lakIdStr = MfExportUtil::GetMf6ArrayString(g, nat, ARR_LAK_ID);
  MfExportUtil::Mf6MultiLayerStringToArray(lakIdStr, lakId, nLay, ncellsPerLayer);

  std::vector<Real> lakLeak;
  CStr lakLeakStr = MfExportUtil::GetMf6ArrayString(g, nat, ARR_LAK_LEAK);
  MfExportUtil::Mf6MultiLayerStringToArray(lakLeakStr, lakLeak, nLay, ncellsPerLayer);

  std::vector<int> ibound;
  {
    std::vector<std::vector<int>> ibound2d = nat->Ibound();
    ibound.reserve(ibound2d.size()*ibound2d.front().size());
    for (size_t i=0; i<ibound2d.size(); ++i)
    {
      for (size_t j=0; j<ibound2d[i].size(); ++j)
      {
        ibound.push_back(ibound2d[i][j]);
      }
    }
  }

  MfPackage* p = g->GetPackage(Packages::LAK);
  const int*nlakes(0);
  p->GetField(Packages::LAKpack::NLAKES, &nlakes);

  m_connections.assign(*nlakes, std::vector<LakConnection>());
  // loop through number of lakes
  for (int q=0; q<*nlakes; ++q)
  {
    int id = q+1;
    // loop through cells
    for (size_t i=0; i<lakId.size(); ++i)
    {
      if (lakId[i] == id && ibound[i] == 0)
      { // get adjacent active cells
        int cellId = (int)i+1;
        AddConnectionsForLakeCell(id, cellId, lakLeak[i], ibound);
      }
    }
  }
} // NativeExpMf6Lak::impl::CreateConnections
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Lak::impl::AddConnectionsForLakeCell (int a_lakId,
  int a_cellId, Real a_bedleak, const std::vector<int>& a_ibound)
{
  std::vector<int> adjCellIds;
  std::vector<Real> width, length;
  std::vector<CStr> claktyp;
  // get adjacent active cell ids with the face width and connection length
  GetAdjacentActiveCells(a_cellId, adjCellIds, claktyp, width, length, a_ibound);
  
  std::vector<LakConnection>& cons(m_connections[a_lakId-1]);
  // add connections
  for (size_t i=0; i<adjCellIds.size(); ++i)
  {
    LakConnection c;
    c.m_cellId = adjCellIds[i];
    c.m_bedleak = a_bedleak;
    c.m_claktype = claktyp[i];
    c.m_connwidth = 0.0;
    c.m_connlen = 0.0;
    if (claktyp[i] == "HORIZONTAL")
    {
      c.m_connwidth = width[i];
      c.m_connlen = length[i];
    }
    cons.push_back(c);
  }
} // NativeExpMf6Lak::impl::AddConnectionsForLakeCell
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Lak::impl::GetAdjacentActiveCells (int a_cellId,
  std::vector<int>& a_adjCellIds, std::vector<CStr>& a_claktyp,
  std::vector<Real>& a_width, std::vector<Real>& a_length,
  const std::vector<int>& a_ibound)
{
  CellNumbering* cn = m_pack->GetNative()->GetCellNumbering();
  std::vector<int> adjCellLayer;
  cn->AdjCellInfoFromId(a_cellId, a_adjCellIds,
    adjCellLayer, a_width, a_length);

  int idInLay, lay;
  cn->IdInLayerFromId(idInLay, lay, a_cellId);
  for (size_t i=0; i<adjCellLayer.size(); ++i)
  {
    if (adjCellLayer[i] == lay) a_claktyp.push_back("HORIZONTAL");
    else                        a_claktyp.push_back("VERTICAL  ");
  }

  // remove any cell in layer above lay
  // remove any cell with ibound = 0
  for (size_t i=a_adjCellIds.size(); i > 0; --i)
  {
    int id = a_adjCellIds[i-1];
    if (a_ibound[id-1] == 0 || adjCellLayer[i-1] < lay)
    {
      int idx = (int)i-1;
      a_adjCellIds.erase(a_adjCellIds.begin()+idx);
      a_claktyp.erase(a_claktyp.begin()+idx);
      a_width.erase(a_width.begin()+idx);
      a_length.erase(a_length.begin()+idx);
    }
  }
} // NativeExpMf6Lak::impl::GetAdjacentActiveCells

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST


#endif
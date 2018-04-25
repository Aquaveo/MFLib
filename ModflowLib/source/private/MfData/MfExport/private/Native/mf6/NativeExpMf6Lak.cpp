//------------------------------------------------------------------------------
// FILE      MfNativeExpMf6Lak.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\mf6\NativeExpMf6Lak.h>

#include <sstream>

#include <private\MfData\MfGlobal.h>
#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\MfExporterImpl.h>
#include <private\MfData\MfExport\private\MfExportUtil.h>
#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\Native\NativeExpLak.h>
#include <private\MfData\MfExport\private\Native\NativePackExp.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackFields.h>
#include <private\MfData\Packages\MfPackStrings.h>


using namespace MfData::Export;

class CellNumbering
{
public:
  int m_nI, m_nJ, m_nK, m_NODES, m_NJAG;
  int m_flowPackage; // 0 - DIS, 1 - DISV, 2 - DISU
  std::vector<int> m_NODLAY;
  std::vector<std::vector<int>> m_JA;
  std::vector<Real> m_DELR, m_DELC;
  std::vector<std::vector<Real>> m_CL12, m_HWVA;

  MfData::MfGlobal* m_g;


  CellNumbering(MfData::MfGlobal* a_g) : m_g(a_g)
  {
    m_nK = a_g->NumLay();
    m_NODES = -1;
    m_flowPackage = 0; // DIS
    if (m_g->Unstructured())
    {
      CStr packName;
      m_g->GetStrVar("DIS_PACKAGE_TYPE", packName);
      if ("DISV" == packName)      m_flowPackage = 1;
      else if ("DISU" == packName) m_flowPackage = 2;
      else ASSERT(0);

      MfData::MfPackage* p = m_g->GetPackage(MfData::Packages::DISU);
      if (p)
      {
        const int* NODLAY(0),* NODES(0),* NJAG(0);
        p->GetField(MfData::Packages::Disu::NODLAY, &NODLAY);
        p->GetField(MfData::Packages::Disu::NODES, &NODES);
        p->GetField(MfData::Packages::Disu::NJAG, &NJAG);
        m_NODES = *NODES;
        m_NJAG = *NJAG;
        for (int i=0; i<m_nK; ++i) m_NODLAY.push_back(NODLAY[i]);
      }
      CStr str;
      m_g->GetStrVar("IAC", str);
      std::vector<int> iac;
      MfData::Export::MfExportUtil::Mf6StringToArray(str, iac, m_NODES);
      m_g->GetStrVar("JA", str);
      std::vector<int> ja;
      MfData::Export::MfExportUtil::Mf6StringToArray(str, ja, m_NJAG);
      m_g->GetStrVar("CL12", str);
      std::vector<Real> cl12;
      MfData::Export::MfExportUtil::Mf6StringToArray(str, cl12, m_NJAG);
      m_g->GetStrVar("HWVA", str);
      std::vector<Real> hwva;
      MfData::Export::MfExportUtil::Mf6StringToArray(str, hwva, m_NJAG);

      size_t q=0;
      for (size_t i=0; i<iac.size(); ++i)
      {
        int nCon = iac[i];
        m_JA.push_back(std::vector<int>());
        m_CL12.push_back(std::vector<Real>());
        m_HWVA.push_back(std::vector<Real>());
        q++; // increment to move past the cell id
        for (int j=1; j<nCon; ++j)
        {
          m_JA.back().push_back(ja[q]);
          m_CL12.back().push_back(cl12[q]);
          m_HWVA.back().push_back(hwva[q]);
          q++;
        }
      }
    }
    else
    {
      m_nI = a_g->NumRow();
      m_nJ = a_g->NumCol();
      CStr str;
      m_g->GetStrVar("DELR", str);
      MfExportUtil::Mf6StringToArray(str, m_DELR, m_nJ);
      m_g->GetStrVar("DELC", str);
      MfExportUtil::Mf6StringToArray(str, m_DELC, m_nI);
    }
  }

  CStr CellIdStringFromId (int id)
  {
    CStr ln;
    if (0 == m_flowPackage)
    {
      int i,j,k;
      IjkFromId(i, j, k, id);
      ln.Format("%5d %5d %5d ", k, i, j);
    }
    else if (1 == m_flowPackage)
    {
      int idInLay, lay;
      IdInLayerFromId(idInLay, lay, id);
      ln.Format("%5d %5d ", lay, idInLay);
    }
    else
    {
      ln.Format("%5d ", id);
    }
    return ln;
  } // CellIdStringFromId

  int IdFromIjk(int i, int j, int k)
  {
    if (0 != m_flowPackage) { ASSERT(0); return -1; }
    int id = j + ( (i-1) * m_nJ ) + ( (k-1) * m_nI * m_nJ );
    return id;
  } // IdFromIjk

  int IdInLayerFromIjk (int i, int j, int /*k*/)
  {
    if (0 != m_flowPackage) { ASSERT(0); return -1; }
    int idInLay = i * j;
    return idInLay;
  } // IdInLayerFromIjk

  void IjkFromId(int& i, int& j, int& k, int id)
  {
    if (0 == m_flowPackage)
    {
      k  = (id-1) / (m_nI*m_nJ) + 1;
      i  = ( (id-1)/m_nJ ) % m_nI + 1;
      j  = (id-1) % m_nJ + 1;
    }
    else
    {
      ASSERT(0);
      i = j = k = -1;
    }
  } // IjkFromId

  void IdInLayerFromId (int& idInLay, int& lay, int id)
  {
    if (0 == m_flowPackage)
    {
      int i,j,k;
      IjkFromId(i, j, k, id);
      lay = k;
      idInLay = IdInLayerFromIjk(i, j, k);
    }
    else
    {
      int beginId(0), endId(0);
      for (size_t q=0; q<m_NODLAY.size(); ++q)
      {
        endId += m_NODLAY[q];
        if (id < endId)
        {
          lay = (int)(q + 1);
          idInLay = id - beginId;
          break;
        }
        beginId += m_NODLAY[q];
      }
    }
  }

  int LayerFromId (int id)
  {
    int idInLay, layer;
    IdInLayerFromId(idInLay, layer, id);
    return layer;
  }

  void AdjCellInfoFromId (int id, std::vector<int>& a_adjCellIds,
    std::vector<int>& a_adjCellLayer, std::vector<Real>& a_adjCellFaceWidth,
    std::vector<Real>& a_adjCellLength)
  {
    Real half = (Real)0.5;
    a_adjCellIds.clear();
    a_adjCellLayer.clear();
    a_adjCellFaceWidth.clear();
    a_adjCellLength.clear();
    if (0 == m_flowPackage)
    {
      int i, j, k;
      IjkFromId(i, j, k, id);
      //check adjacent in I
      if (i > 1)
      {
        a_adjCellIds.push_back(IdFromIjk(i-1, j, k));
        a_adjCellLayer.push_back(k);
        a_adjCellLength.push_back(half * m_DELC[i-2]);
        a_adjCellFaceWidth.push_back(m_DELR[j-1]);
      }
      if (i+1 < m_nI)
      {
        a_adjCellIds.push_back(IdFromIjk(i+1, j, k));
        a_adjCellLayer.push_back(k);
        a_adjCellLength.push_back(half * m_DELC[i]);
        a_adjCellFaceWidth.push_back(m_DELR[j-1]);
      }
      if (j > 1)
      {
        a_adjCellIds.push_back(IdFromIjk(i, j-1, k));
        a_adjCellLayer.push_back(k);
        a_adjCellLength.push_back(half * m_DELR[j-2]);
        a_adjCellFaceWidth.push_back(m_DELC[i-1]);
      }
      if (j+1 < m_nJ)
      {
        a_adjCellIds.push_back(IdFromIjk(i, j+1, k));
        a_adjCellLayer.push_back(k);
        a_adjCellLength.push_back(half * m_DELR[j]);
        a_adjCellFaceWidth.push_back(m_DELC[i-1]);
      }
      if (k > 1)
      {
        a_adjCellIds.push_back(IdFromIjk(i, j, k-1));
        a_adjCellLayer.push_back(k-1);
        a_adjCellLength.push_back(0.0);
        a_adjCellFaceWidth.push_back(0.0);
      }
      if (k+1 < m_nK)
      {
        a_adjCellIds.push_back(IdFromIjk(i, j, k+1));
        a_adjCellLayer.push_back(k+1);
        a_adjCellLength.push_back(0.0);
        a_adjCellFaceWidth.push_back(0.0);
      }
    }
    else
    { 
      a_adjCellIds = m_JA[id-1];
      a_adjCellLength = m_CL12[id-1];
      a_adjCellFaceWidth = m_HWVA[id-1];
      for (size_t i=0; i<a_adjCellIds.size(); ++i)
      {
        a_adjCellLayer.push_back(LayerFromId(a_adjCellIds[i]));
      }
    }
  }

  int GetStackedNumberOfCellsPerLayer()
  {
    if (2 == m_flowPackage) { ASSERT(0); return -1; }

    if (0 == m_flowPackage)
      return (m_nI * m_nJ);
    else
      return m_NODLAY[0];
  } // GetStackedNumberOfCellsPerLayer

};

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
  impl(NativePackExp* a_) : m_pack(a_), m_cellNumbering(m_pack->GetGlobal()) {}

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
  CellNumbering m_cellNumbering;
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
      ss << s1 << m_cellNumbering.CellIdStringFromId(cons[j].m_cellId) << " " 
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
  int ncellsPerLayer = m_cellNumbering.GetStackedNumberOfCellsPerLayer();

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
  std::vector<int> adjCellLayer;
  m_cellNumbering.AdjCellInfoFromId(a_cellId, a_adjCellIds,
    adjCellLayer, a_width, a_length);

  int idInLay, lay;
  m_cellNumbering.IdInLayerFromId(idInLay, lay, a_cellId);
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
      int idx = i-1;
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
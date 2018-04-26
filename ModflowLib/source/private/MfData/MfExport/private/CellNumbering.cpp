//------------------------------------------------------------------------------
// FILE      CellNumbering.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\CellNumbering.h>

#include <private\MfData\MfGlobal.h>
#include <private\MfData\MfExport\private\MfExportUtil.h>
#include <private\MfData\MfExport\private\Native\NativePackExp.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackFields.h>
#include <private\MfData\Packages\MfPackStrings.h>

using namespace MfData::Export;

class CellNumberingImpl : public CellNumbering
{
public:
  int m_nI, m_nJ, m_nK, m_NODES, m_NJAG;
  int m_flowPackage; // 0 - DIS, 1 - DISV, 2 - DISU
  std::vector<int> m_NODLAY;
  std::vector<std::vector<int>> m_JA;
  std::vector<Real> m_DELR, m_DELC;
  std::vector<std::vector<Real>> m_CL12, m_HWVA;

  MfData::MfGlobal* m_g;

  CellNumberingImpl() {}

  CellNumberingImpl (MfData::MfGlobal* a_g) : m_g(a_g)
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
      ln.Format("%5d %10d ", lay, idInLay);
    }
    else
    {
      ln.Format("%10d ", id);
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
        if (id <= endId)
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

//------------------------------------------------------------------------------
/// \brief constructor
//------------------------------------------------------------------------------
CellNumbering* CellNumbering::New (MfData::MfGlobal* a_g)
{
  CellNumbering* rval = new CellNumberingImpl(a_g);
  return rval;
} // Mf2kNative::Mf2kNative
//------------------------------------------------------------------------------
/// \brief destructor
//------------------------------------------------------------------------------
CellNumbering::~CellNumbering ()
{
} // CellNumbering::~CellNumbering


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#endif

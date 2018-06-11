//------------------------------------------------------------------------------
// FILE      MfNativeExpMf6Uzf.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\mf6\NativeExpMf6Uzf.h>

#include <sstream>

#include <private\MfData\MfGlobal.h>
#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\CellNumbering.h>
#include <private\MfData\MfExport\private\MfExporterImpl.h>
#include <private\MfData\MfExport\private\MfExportUtil.h>
#include <private\MfData\MfExport\private\Native\NativePackExp.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackFields.h>
#include <private\MfData\Packages\MfPackStrings.h>



using namespace MfData::Export;

class NativeExpMf6Uzf::impl
{
public:
  impl(NativePackExp* a_) : m_pack(a_) {}

  void WriteOptions();
  void WriteDimensions();
  void WritePackageData ();
  void WriteStressPeriod();

  void GetIuzfbnd(std::vector<int>& a_iuzfbnd,
    std::vector<int>& a_cellids, std::vector<int>& a_ibound);

  NativePackExp* m_pack;
  std::vector<CStr> m_lines;
};
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMf6Uzf::NativeExpMf6Uzf (NativePackExp* a_) :
m_p(new impl(a_))

{
} // MfNativeExpMf6Uzf::MfNativeExpMf6Uzf
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMf6Uzf::~NativeExpMf6Uzf ()
{
  if (m_p) delete(m_p);
  m_p = nullptr;
} // MfNativeExpMf6Uzf::~MfNativeExpMf6Uzf
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpMf6Uzf::Export ()
{
  if (!m_p || !m_p->m_pack) return false;

  MfPackage* p = m_p->m_pack->GetPackage();
  if (!p) return false;

  CStr nm = p->PackageName();
  if (nm == Packages::UZFLine1)
  {
  }
  else if (nm == Packages::UZFLine8)
  {
    m_p->m_lines.push_back(MfExportUtil::GetMf6CommentHeader());
    m_p->WriteOptions();
    m_p->WriteDimensions();
    m_p->WritePackageData();
  }
  else if (nm == Packages::UZFStressPeriod)
  {
    m_p->WriteStressPeriod();
  }

  std::vector<CStr> desc(m_p->m_lines.size(), "");
  TmpPackageNameChanger tmp(p, "UZF");
  m_p->m_pack->AddToStoredLinesDesc(m_p->m_lines, desc);
  m_p->m_pack->WriteStoredLines();
  return true;
} // NativeExpMf6Uzf::ExportMf6Rch
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Uzf::impl::WriteOptions ()
{
  using namespace MfData::Packages;

   // UNSAT ETWC, UNSAT ETAE needed?

  const int *ietflg, *iuzfcb1 , *iuzfcb2, *nosurfleak;
  MfPackage* p= m_pack->GetGlobal()->GetPackage(UZFLine1);
  if (p->GetField(UZFpack::IETFLG, &ietflg) && ietflg &&
      p->GetField(UZFpack::IUZFCB1, &iuzfcb1) && iuzfcb1 &&
      p->GetField(UZFpack::IUZFCB2, &iuzfcb2) && iuzfcb2 &&
      p->GetField(UZFpack::NOSURFLEAK, &nosurfleak) && nosurfleak)
  {
    m_lines.push_back("BEGIN OPTIONS");
    std::stringstream ss; 
    if (*iuzfcb2 != 0) m_lines.push_back(" SAVE_FLOWS");
    if (*iuzfcb1 != 0)
    {
      CStr baseName;
      util::StripPathFromFilename(m_pack->GetNative()->FileName(), baseName);
      util::StripExtensionFromFilename(baseName, baseName);
      baseName += "_uzf.ccf";
      std::stringstream ss;
      ss << "  BUDGET FILEOUT " << baseName;
      m_lines.push_back(ss.str());
    }
    if(*ietflg !=0)
    {
      m_lines.push_back(" SIMULATE_ET");
      m_lines.push_back(" LINEAR_GWET");
    }
    if (*nosurfleak != 0) m_lines.push_back(" SIMULATE_GWSEEP");

    m_lines.push_back("END OPTIONS");
    m_lines.push_back("");
  }
} // NativeExpMf6Uzf::impl::WriteOptions
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Uzf::impl::WriteDimensions ()
{
  using namespace MfData::Packages; 
  std::vector<int> iuzfbnd, cellids, ibound;
  GetIuzfbnd(iuzfbnd, cellids, ibound);
  int cnt(0);
  for (size_t i=0; i<iuzfbnd.size(); ++i)
  {
    if (iuzfbnd[i] != 0 && ibound[cellids[i]-1] != 0) cnt++;
  }

  const int* ntrail2(0),* nsets2(0);
  MfPackage* p= m_pack->GetGlobal()->GetPackage(UZFLine1);
  if (p->GetField(UZFpack::NTRAIL2, &ntrail2) && ntrail2 &&
      p->GetField(UZFpack::NSETS2, &nsets2) && nsets2)
  {
    m_lines.push_back("BEGIN DIMENSIONS");
    std::stringstream ss;
    ss << "  NUZFCELLS   " << STR(cnt,-1,5,STR_FULLWIDTH) << "\n"
       << "  NTRAILWAVES " << STR(*ntrail2,-1,5,STR_FULLWIDTH) << "\n"
       << "  NWAVESETS   " << STR(*nsets2,-1,5,STR_FULLWIDTH);
    m_lines.push_back(ss.str());
    m_lines.push_back("END DIMENSIONS");
    m_lines.push_back("");
  } 
} // NativeExpMf6Uzf::impl::WriteDimensions
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Uzf::impl::WritePackageData ()
{
  CellNumbering* cn = m_pack->GetNative()->GetCellNumbering();
  ASSERT(cn);
  MfGlobal* g = m_pack->GetGlobal();
  Mf2kNative* nat = m_pack->GetNative();

  int nCells = g->NumRow() * g->NumCol();

  using namespace MfData::Packages;
  MfPackage* p = g->GetPackage(UZFLine1);
  const Real* surfdep;
  p->GetField(UZFpack::SURFDEP, &surfdep);
  if (!surfdep) return;

  std::vector<int> iuzfbnd, cellids, ibound;
  GetIuzfbnd(iuzfbnd, cellids, ibound);

  // get array data VKS, THTR, THTS, THTI, EPS
  std::vector<Real> vks(nCells, 0), thtr(nCells, 0), thts(nCells, 0),
    thti(nCells, 0), eps(nCells,0);
  if (g->GetPackage(ARR_UZF_VKS))
  {
    CStr vksStr = MfExportUtil::GetMf6ArrayString(g, nat, ARR_UZF_VKS);
    MfExportUtil::Mf6StringToArray(vksStr, vks, nCells);
  }
  if (g->GetPackage(ARR_UZF_EPS))
  {
    CStr epsStr = MfExportUtil::GetMf6ArrayString(g, nat, ARR_UZF_EPS);
    MfExportUtil::Mf6StringToArray(epsStr, eps, nCells);
  }
  if (g->GetPackage(ARR_UZF_THTS))
  {
    CStr thtsStr = MfExportUtil::GetMf6ArrayString(g, nat, ARR_UZF_THTS);
    MfExportUtil::Mf6StringToArray(thtsStr, thts, nCells);
  }
  if (g->GetPackage(ARR_UZF_THTR))
  {
    CStr thtrStr = MfExportUtil::GetMf6ArrayString(g, nat, ARR_UZF_THTR);
    MfExportUtil::Mf6StringToArray(thtrStr, thtr, nCells);
  }
  if (g->GetPackage(ARR_UZF_THTI))
  {
    CStr thtiStr = MfExportUtil::GetMf6ArrayString(g, nat, ARR_UZF_THTI);
    MfExportUtil::Mf6StringToArray(thtiStr, thti, nCells);
  }

  int w = util::RealWidth();
  CStr surfdepStr = STR(*surfdep,-1,w,STR_FULLWIDTH);
  CStr ivertconStr("0");
  CStr landFlagStr("0"); // TODO Ask USGS
  m_lines.push_back("BEGIN PACKAGEDATA");
  int cnt(1);
  for (size_t i=0; i<cellids.size(); ++i)
  {
    // skip cells where ibound is zero
    if (0 == ibound[cellids[i]-1]) continue;

    std::stringstream ss;
    CStr iuzno, landFlagStr;
    iuzno.Format("  %10d", cnt);
    cnt++;
    landFlagStr.Format("%5d", iuzfbnd[i] ? 1 : 0);
    ss << iuzno
       << cn->CellIdStringFromId(cellids[i]) << " "
       << landFlagStr << " "
       << ivertconStr << " "
       << surfdepStr << " "
       << STR(vks[i],-1,w,STR_FULLWIDTH) << " "
       << STR(thtr[i],-1,w,STR_FULLWIDTH) << " "
       << STR(thts[i],-1,w,STR_FULLWIDTH) << " "
       << STR(thti[i],-1,w,STR_FULLWIDTH) << " "
       << STR(eps[i],-1,w,STR_FULLWIDTH);
    m_lines.push_back(ss.str());
  }

  m_lines.push_back("END PACKAGEDATA");
  m_lines.push_back("");
}//NativeExpMf6Uzf::impl::WritePackageData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Uzf::impl::WriteStressPeriod ()
{
  MfGlobal* g = m_pack->GetGlobal();
  Mf2kNative* nat = m_pack->GetNative();
  int nCells = g->NumRow() * g->NumCol();

  using namespace MfData::Packages;
  MfPackage* p = m_pack->GetPackage();
  const int *nuzf1(0), *nuzf2(0), *nuzf3(0), *nuzf4(0);
  if (p->GetField(UZFpack::NUZF1, &nuzf1) && nuzf1 &&
      p->GetField(UZFpack::NUZF2, &nuzf2) && nuzf2 &&
      p->GetField(UZFpack::NUZF3, &nuzf3) && nuzf3 &&
      p->GetField(UZFpack::NUZF4, &nuzf4) && nuzf4)
  {
    {
      std::stringstream ss; 
      ss << "BEGIN PERIOD " << g->GetCurrentPeriod();
      m_lines.push_back(ss.str());
    }

    std::vector<int> iuzfbnd, cellids, ibound;
    GetIuzfbnd(iuzfbnd, cellids, ibound);
    CStr iuzfbndStr;
    g->GetStrVar(ARR_UZF_UBND, iuzfbndStr);
    MfExportUtil::Mf6StringToArray(iuzfbndStr, iuzfbnd, nCells);

    int w = util::RealWidth();

    if (g->GetPackage(ARR_UZF_RCH) && *nuzf1 > -1)
    {
      CStr str = MfExportUtil::GetMf6ArrayString(g, nat, ARR_UZF_RCH);
      g->SetStrVar(ARR_UZF_RCH, str);
    }
    if (g->GetPackage(ARR_UZF_ET) && *nuzf2 > -1)
    {
      CStr str = MfExportUtil::GetMf6ArrayString(g, nat, ARR_UZF_ET);
      g->SetStrVar(ARR_UZF_ET, str);
    }
    if (g->GetPackage(ARR_UZF_EXT) && *nuzf3 > -1)
    {
      CStr str = MfExportUtil::GetMf6ArrayString(g, nat, ARR_UZF_EXT);
      g->SetStrVar(ARR_UZF_EXT, str);
    }
    if (g->GetPackage(ARR_UZF_EXTWC) && *nuzf4 > -1)
    {
      CStr str = MfExportUtil::GetMf6ArrayString(g, nat, ARR_UZF_EXTWC);
      g->SetStrVar(ARR_UZF_EXTWC, str);
    }

    CStr finfStr, petStr, extdpStr, extwcStr;
    g->GetStrVar(ARR_UZF_RCH, finfStr);
    g->GetStrVar(ARR_UZF_ET, petStr);
    g->GetStrVar(ARR_UZF_EXT, extdpStr);
    g->GetStrVar(ARR_UZF_EXTWC, extwcStr);

    std::vector<Real> finf(nCells, 0), pet(nCells, 0), extdp(nCells, 0),
      extwc(nCells, 0);
    if (!finfStr.empty()) MfExportUtil::Mf6StringToArray(finfStr, finf, nCells);
    if (!petStr.empty()) MfExportUtil::Mf6StringToArray(petStr, pet, nCells);
    if (!extdpStr.empty()) MfExportUtil::Mf6StringToArray(extdpStr, extdp, nCells);
    if (!extwcStr.empty()) MfExportUtil::Mf6StringToArray(extwcStr, extwc, nCells);

    int cnt(1);
    for (int i=0; i<nCells; ++i)
    {
      if (iuzfbnd[i] == 0) continue;
      if (0 == ibound[cellids[i]-1]) continue;

      std::stringstream ss;
      CStr iuzno;
      iuzno.Format("  %10d", cnt);
      cnt++;
      ss << iuzno << " "
         << STR(finf[i],-1,w,STR_FULLWIDTH) << " "
         << STR(pet[i],-1,w,STR_FULLWIDTH) << " "
         << STR(extdp[i],-1,w,STR_FULLWIDTH) << " "
         << STR(extwc[i],-1,w,STR_FULLWIDTH) << " "
         << " 0.0 0.0 0.0";
      m_lines.push_back(ss.str());
    }

    m_lines.push_back("END PERIOD");
    m_lines.push_back("");
  }
}//NativeExpMf6Uzf::impl::WriteStressPeriod () 
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMf6Uzf::impl::GetIuzfbnd (std::vector<int>& a_iuzfbnd,
  std::vector<int>& a_cellids, std::vector<int>& a_ibound)
{
  MfGlobal* g = m_pack->GetGlobal();
  Mf2kNative* nat = m_pack->GetNative();
  CellNumbering* cn = nat->GetCellNumbering();

  int nCells = g->NumRow() * g->NumCol();
  a_cellids.assign(nCells, -1);
  for (size_t i=0; i<a_cellids.size(); ++i)
    a_cellids[i] = (int)(i+1);

  {
    std::vector<std::vector<int>>& ibound2d = nat->Ibound();
    a_ibound = ibound2d[0];
    for (size_t i=1; i<ibound2d.size(); ++i)
      a_ibound.insert(a_ibound.end(), ibound2d[i].begin(), ibound2d[i].end());
  }

  using namespace MfData::Packages;
  MfPackage* p = g->GetPackage(UZFLine1);
  const int* nuztop(0);
  const Real* surfdep;
  p->GetField(UZFpack::NUZTOP, &nuztop);
  p->GetField(UZFpack::SURFDEP, &surfdep);
  if (!nuztop || !surfdep) return;
  if (g->GetPackage(ARR_UZF_UBND))
  {
    CStr iuzfbndStr = MfExportUtil::GetMf6ArrayString(g, nat, ARR_UZF_UBND);
    MfExportUtil::Mf6StringToArray(iuzfbndStr, a_iuzfbnd, nCells);
    g->SetStrVar(ARR_UZF_UBND, iuzfbndStr);
  }

  if (*nuztop == 2 && g->GetPackage(ARR_UZF_UBND))
  { // specified layer
    int cnt(0);
    for (int i =0; i<g->NumRow(); ++i)
    {
      for (int j=0; j<g->NumCol(); ++j)
      {
        int lay = a_iuzfbnd[cnt];
        a_cellids[cnt] = cn->IdFromIjk(i+1, j+1, lay);
        cnt++;
      }
    }
  }
  else if (*nuztop == 3)
  { // highest active cell
    std::vector<std::vector<int>>& ibound = nat->Ibound();
    int cnt(0);
    for (int i=0; i<g->NumRow(); ++i)
    {
      for (int j=0; j<g->NumCol(); ++j)
      {
        bool done(0);
        for (int k=0; !done && k<g->NumLay(); ++k)
        {
          int id = cn->IdFromIjk(i+1, j+1, k+1);
          if (ibound[k][cnt] != 0)
          {
            a_cellids[cnt] = id;
            done = true;
          }
        }
        cnt++;
      }
    }
  }

} // NativeExpMf6Uzf::impl::GetIuzfbnd


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST


#endif
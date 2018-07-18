//------------------------------------------------------------------------------
// FILE      NativeExpLak.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/MfData/MfExport/private/Native/NativeExpLak.h>

#include <sstream>

#include <private/MfData/MfExport/private/Mf2kNative.h>
#include <private/MfData/MfExport/private/MfExportUtil.h>
#include <private/MfData/MfExport/private/Native/mf6/NativeExpMf6Lak.h>
#include <private/MfData/MfExport/private/Native/NativeUtil.h>
#include <private/MfData/MfExport/private/TxtExporter.h>
#include <private/MfData/MfGlobal.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackFields.h>
#include <private/MfData/Packages/MfPackStrings.h>

using namespace MfData::Export;
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpLak::NativeExpLak ()
{
} // MfNativeExpLak::MfNativeExpLak
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpLak::~NativeExpLak ()
{
} // MfNativeExpLak::~MfNativeExpLak
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpLak::Export ()
{
  if (GetNative()->GetExportMf6())
  {
    NativeExpMf6Lak lak(this);
    lak.Export();
    return true;
  }

  if (GetPackage()->PackageName() == Packages::LAK)
  {
    Line1to2();
  }
  else
  {
    if (1 == GetGlobal()->GetCurrentPeriod()) Line3();
    Line4();
    Line5();
    Line6();
    Line7to8();
    Line9();
  }
  // LAK doesn't support comments at the top of the file
  //WriteComments();
  return true;
} // MfNativeExpLak::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLak::Line1to2 ()
{
  MfPackage* a_p = GetPackage();
  const int *nlakes(0), *ilkcb(0), *nssitr(0), *itrss(0);
  const Real *theta(0), *sscncr(0);
  if (a_p->GetField(Packages::LAKpack::NLAKES, &nlakes) && nlakes &&
      a_p->GetField(Packages::LAKpack::ILKCB, &ilkcb) && ilkcb &&
      a_p->GetField(Packages::LAKpack::THETA, &theta) && theta &&
      a_p->GetField(Packages::LAKpack::NSSITR, &nssitr) && nssitr &&
      a_p->GetField(Packages::LAKpack::SSCNCR, &sscncr) && sscncr &&
      a_p->GetField(Packages::LAKpack::ITRSS, &itrss) && itrss)
  {
    CStr desc = " 1. NLAKES ILKCB";
    CStr ln, ln1;
    ln.Format("%5d %5d", *nlakes, *ilkcb);
    AddToStoredLinesDesc(ln, desc);

    std::stringstream os;
    int width = util::RealWidth();
    desc = " 2. THETA [NSSITR SSCNCR] [SURFDEPTH]";
    ln1 = STR(*theta, -1, width, STR_FULLWIDTH);
    os << ln1 << " ";
    if (*itrss <= 0 || *theta < 0.0)
    {
      ln1.Format("%5d %s",  *nssitr, STR(*sscncr, -1, width, STR_FULLWIDTH));
      os << ln1;
    }
    AddToStoredLinesDesc(os.str().c_str(), desc);
  }
} // NativeExpLak::Line1to2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLak::Line3 ()
{
  const int *nlakes(0), *dummy(0); 
  const Real *stages(0), *ssmn(0), *ssmx(0), *clake(0);
  MfPackage* a_p = GetPackage();
  MfPackage* a_pLak = GetGlobal()->GetPackage(Packages::LAK);
  if (a_pLak->GetField(Packages::LAKpack::NLAKES, &nlakes) && nlakes &&
      a_p->GetField(Packages::LAKSPpack::NSOL, &dummy) && dummy &&
      a_p->GetField(Packages::LAKSPpack::STAGES, &stages) && stages &&
      a_p->GetField(Packages::LAKSPpack::SSMN, &ssmn) && ssmn &&
      a_p->GetField(Packages::LAKSPpack::SSMX, &ssmx) && ssmx &&
      a_p->GetField(Packages::LAKSPpack::CLAKE, &clake) && clake)
  {
    int width = util::RealWidth();

    // Coded to include NSOL but it seems that mf2k gives NSOL=1
    // even though it wasn't included in export file.  Removing
    // for now.
    //int nsolutes(0), *nsol=&nsolutes;

    CStr desc = " 3. STAGES [SSMN SSMX] [CLAKE(1)..........CLAKE(NSOL)]";
    // export STAGES {SSMN SSMX} {CLAKE(1)... CLAKE(NSOL)}
    for (int lake = 0; lake < *nlakes; ++lake)
    {
      std::stringstream os;
      os << STR(stages[lake], -1, width, STR_FULLWIDTH) << " ";
      if (GetNative()->GetExp()->FirstStressIsSteadyState())
      {
        os << STR(ssmn[lake], -1, width, STR_FULLWIDTH) << " "
           << STR(ssmx[lake], -1, width, STR_FULLWIDTH);
      }
      AddToStoredLinesDesc(os.str().c_str(), desc);
    }
  }
} // NativeExpLak::Line3
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLak::Line4 ()
{
  const int *itmp(0), *itmp1(0), *lwrt(0);
  MfPackage* a_p = GetPackage();
  if (a_p->GetField(Packages::LAKSPpack::ITMP, &itmp) && itmp &&
      a_p->GetField(Packages::LAKSPpack::ITMP1, &itmp1) && itmp1 &&
      a_p->GetField(Packages::LAKSPpack::LWRT, &lwrt) && lwrt)
  {
    CStr desc = " 4. ITMP ITMP1 LWRT";
    CStr ln;
    ln.Format("%5d %5d %5d", *itmp, *itmp1, *lwrt);
    AddToStoredLinesDesc(ln, desc);
  }
} // NativeExpLak::Line4
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLak::Line5 ()
{
  const int *itmp(0);
  MfPackage* a_p = GetPackage();
  if (a_p->GetField(Packages::LAKSPpack::ITMP, &itmp) && itmp &&
      *itmp > 0)
  {
    MfPackage* p = GetGlobal()->GetPackage(ARR_LAK_ID);
    if (!p) return;

    CStr d, desc = " 5. LKARR(NCOL,NROW)    LAY ";
    std::vector<CStr>& lines(p->StringsToWrite());
    for (size_t lay=1, i=0; i<lines.size(); ++i, ++lay)
    {
      d.Format("%s%d", desc, lay);
      AddToStoredLinesDesc(lines[i], d);
      if (MfExportUtil::ArrayWriteNextLineInternal(GetNative(), lines[i]))
      {
        AddToStoredLinesDesc(lines[++i], "");
      }
    }
    lines.clear();
  }
} // NativeExpLak::Line5
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLak::Line6 ()
{
  const int *itmp(0);
  MfPackage* a_p = GetPackage();
  if (a_p->GetField(Packages::LAKSPpack::ITMP, &itmp) && itmp &&
    *itmp > 0)
  {
    MfPackage* p = GetGlobal()->GetPackage(ARR_LAK_LEAK);
    if (!p) return;

    CStr d, desc = " 6. BDLKNC(NCOL,NROW)   LAY ";
    std::vector<CStr>& lines(p->StringsToWrite());
    for (size_t lay=1, i=0; i<lines.size(); ++i, ++lay)
    {
      d.Format("%s%d", desc, lay);
      AddToStoredLinesDesc(lines[i], d);
      if (MfExportUtil::ArrayWriteNextLineInternal(GetNative(), lines[i]))
      {
        AddToStoredLinesDesc(lines[++i], "");
      }
    }
    lines.clear();
  }
} // NativeExpLak::Line6
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLak::Line7to8 ()
{
  const int *nlakes(0), *itmp(0), *nslms(0), *ic(0), *isub(0);
  const Real *sillvt(0);
  MfPackage* a_p = GetPackage();
  MfPackage* a_pLak = GetGlobal()->GetPackage(Packages::LAK);
  if (a_pLak->GetField(Packages::LAKpack::NLAKES, &nlakes) && nlakes &&
      a_p->GetField(Packages::LAKSPpack::ITMP, &itmp) && itmp &&
      a_p->GetField(Packages::LAKSPpack::NSLMS, &nslms) && nslms &&
      a_p->GetField(Packages::LAKSPpack::IC, &ic) && ic &&
      a_p->GetField(Packages::LAKSPpack::ISUB, &isub) && isub &&
      a_p->GetField(Packages::LAKSPpack::SILLVT, &sillvt) && sillvt)
  {
    if (*itmp < 1) return;

    CStr desc = " 7. NSLMS";
    CStr ln;
    ln.Format("%5d", *nslms);
    AddToStoredLinesDesc(ln, desc);

    if (*nslms < 1) return;

    for (int s=0; s<*nslms && ic[s] > 0; ++s)
    {
      desc = "8a. IC ISUB(1) ISUB(2) ............ ISUB(IC)";
      ln.Format("%5d ", ic[s]);
      for (int i=0; i<ic[s]; ++i)
      {
        CStr tmp;
        tmp.Format("%5d ", isub[i*(*nlakes)+s]);
        ln += tmp;
      }
      AddToStoredLinesDesc(ln, desc);

      int width = util::RealWidth();
      ln = "";
      desc = "8b. SILLVT(2) ............. SILLVT(IC)";
      for (int i=0; i<ic[s]-1; ++i)
      {
        CStr tmp;
        tmp.Format("%s ", STR(sillvt[i*(*nlakes)+s], -1, width, STR_FULLWIDTH));
        ln += tmp;
      }
      AddToStoredLinesDesc(ln, desc);
    }
  }
} // NativeExpLak::Line7to8
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLak::Line9 ()
{
  const int *nlakes(0), *itmp1(0);
  const Real *rnf(0), *wthdrw(0), *ssmn(0), *ssmx(0);
  const double *prcplk(0), *evaplk(0);
  MfPackage* a_p = GetPackage();
  MfPackage* a_pLak = GetGlobal()->GetPackage(Packages::LAK);
  if (a_pLak->GetField(Packages::LAKpack::NLAKES, &nlakes) && nlakes &&
      a_p->GetField(Packages::LAKSPpack::ITMP1, &itmp1) && itmp1 &&
      a_p->GetField(Packages::LAKSPpack::PRCPLK, &prcplk) && prcplk &&
      a_p->GetField(Packages::LAKSPpack::EVAPLK, &evaplk) && evaplk &&
      a_p->GetField(Packages::LAKSPpack::RNF, &rnf) && rnf &&
      a_p->GetField(Packages::LAKSPpack::WTHDRW, &wthdrw) && wthdrw &&
      a_p->GetField(Packages::LAKSPpack::SSMN, &ssmn) && ssmn &&
      a_p->GetField(Packages::LAKSPpack::SSMX, &ssmx) && ssmx)
  {
    if (*itmp1 < 0 || *nlakes < 1) return;

    int width = util::RealWidth();
    CStr desc = "9a. PRCPLK EVAPLK RNF WTHDRW [SSMN] [SSMX]";
    CStr ln;
    std::set<int>& ssPer(GetNative()->GetExp()->SetOfSteadyStateStressPeriods());
    for (int i=0; i<*nlakes; ++i)
    {
      ln.Format("%s %s %s %s",
                STR(prcplk[i], -1, width, STR_FULLWIDTH),
                STR(evaplk[i], -1, width, STR_FULLWIDTH),
                STR(rnf[i], -1, width, STR_FULLWIDTH),
                STR(wthdrw[i], -1, width, STR_FULLWIDTH));
      if (GetGlobal()->GetCurrentPeriod() != 1 &&
          ssPer.find(GetGlobal()->GetCurrentPeriod()) != ssPer.end())
      {
        CStr tmp;
        tmp.Format(" %s %s",
                   STR(ssmn[i], -1, width, STR_FULLWIDTH),
                   STR(ssmx[i], -1, width, STR_FULLWIDTH));
        ln += tmp;
      }
      AddToStoredLinesDesc(ln, desc);
    }
  }
} // NativeExpLak::Line9
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLak::AddToStoredLinesDesc (const char* a_line,
                                         const char* a_desc)
{
  MfPackage* p = GetGlobal()->GetPackage(Packages::LAK);
  if (!p || GetPackage() == p)
    NativePackExp::AddToStoredLinesDesc(a_line, a_desc);
  else
  {
    MfPackage* p1 = GetPackage();
    SetData(GetNative(), GetGlobal(), p);
    NativePackExp::AddToStoredLinesDesc(a_line, a_desc);
    SetData(GetNative(), GetGlobal(), p1);
  }
} // NativeExpLak::AddToStoredLinesDesc

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private/MfData/MfExport/private/Native/NativeExpLak.t.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpLakT::setUp ()
{
  m_p = NULL;
  Mf2kNative* n = new Mf2kNative;
  MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  MfPackage* dis = new MfPackage(Packages::LAK);
  NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  m_p = dynamic_cast<NativeExpLak*>(p);
} // NativeExpLakT::setUp
//------------------------------------------------------------------------------
void NativeExpLakT::tearDown ()
{
  m_p->UnitTestingDeletePointers();
  delete(m_p);
} // NativeExpLakT::tearDown
//------------------------------------------------------------------------------
void NativeExpLakT::testCreateClass ()
{
  TS_ASSERT(m_p);
} // NativeExpLakT::testCreateClass

#endif
//------------------------------------------------------------------------------
// FILE      NativeExpSfr.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpSfr.h>

#include <sstream>

#include <private\MfData\MfExport\private\H5\H5BcList.h>
#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\MfExportUtil.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfExport\private\Native\mf6\NativeExpMf6Sfr.h>
#include <private\MfData\MfExport\private\TxtExporter.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackFields.h>
#include <private\MfData\Packages\MfPackStrings.h>
#include <private\Parameters.h>
#include <private\Parameters\ParamList.h>
#include <private\Parameters\Param.h>
#include <private\util\EReadAsciiFile.h>

using namespace MfData::Export;

namespace
{
  const CStr SFR_PARAM_SP_NSEG = "SFR_PARAM_SP_NSEG";
  const CStr SFR_PAR_DEF_SP    = "SFR_PAR_DEF_SP";
  const CStr SFR_PARAM_SP      = "SFR_PARAM_SP_";
  const CStr SFR_NPARSEG       = "SFR_NPARSEG";
  const CStr SFR_NO_PARAM      = "SFR_00";
  const CStr SFR_LINE5         = "SFR_LINE5";
  const CStr SFR_H5_LINE_5     = "SFR_H5_LINE_5";

//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void AddToGlobalVar (MfData::MfGlobal* a_g,
                            CStr a_var,
                            CStr a_pname,
                            int a_val)
{
  CStr strVal;
  a_var += a_pname;
  a_g->GetStrVar(a_var, strVal);
  std::stringstream ss;
  ss << strVal << a_val << " ";
  a_g->SetStrVar(a_var, ss.str());
} // AddToGlobalVar
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr Desc5 ()
{
  return " 5. ITMP IRDFLG IPTFLG [NP]";
} // Desc5
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
std::vector<Real>& SfrCondFact (int a_idx)
{
  static std::map< int, std::vector<Real> > m_map;
  return m_map[a_idx];
} // SfrCondFact
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
std::vector<Real>& SfrCondFact2 (int a_idx)
{
  static std::map< int, std::vector<Real> > m_map;
  return m_map[a_idx];
} // SfrCondFact2
} // unnamed namespace
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpSfr::NativeExpSfr ()
: m_iseg(0)
, m_iotsg(0)
, m_idivar(0)
, m_segOrig(0)
, m_xsec(0)
, m_qstage(0)
, m_writingPar(0)
, m_seg(0)
, m_segVec()
, m_sz(26)
, m_usg(false)
, m_unstructured(false)
{
  m_usg = MfExportUtil::MfIsUsgModelType();
  if (m_usg)
  {
    m_unstructured = MfData::MfGlobal::Get().Unstructured() ? 1 : 0;
  }
} // MfNativeExpSfr::MfNativeExpSfr
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpSfr::~NativeExpSfr ()
{
} // MfNativeExpSfr::~MfNativeExpSfr
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpSfr::Export ()
{
  using namespace MfData::Packages;
  MfGlobal* g = GetGlobal();
  g->GetIntVar("SFR_SEG_SIZE", m_sz);
  MfPackage* p = g->GetPackage(Packages::SFR);
  if (!p)
  {
    MfPackage p1(Packages::SFR);
    g->AddPackage(&p1);
  }

  CStr nm = GetPackage()->PackageName();

  if (GetNative()->GetExportMf6())
  {
    if (Packages::SFRLine2 == nm)
    {
      MfPackage* p1 = g->GetPackage(SFRLine1);
      MfPackage* p2 = g->GetPackage(SFRLine2);
      // We may need these values for export of MF6 so we save them here
      // because they become corrupted by the time we try to write MF6
      if (p1)
      {
        const int* nstrm;
        p1->GetField(SFRpack::NSTRM, &nstrm);
        if (nstrm) g->SetIntVar(SFRpack::NSTRM, *nstrm);
      }
      if (p2)
      {
        const int* nistrmd(0),* nstrmd(0);
        p2->GetField(SFRpack::NISTRMD, &nistrmd);
        p2->GetField(SFRpack::NSTRMD, &nstrmd);
        if (nistrmd) g->SetIntVar(SFRpack::NISTRMD, *nistrmd);
        if (nstrmd) g->SetIntVar(SFRpack::NSTRMD, *nstrmd);
      }
    }
    if (Packages::SFR_MF6 == nm)
    {
      NativeExpMf6Sfr sfr(this);
      sfr.Export();
    }
    return true;
  }


  if ("SFR_CONDFACT" == nm)
  {
    SaveCondFact();
  }
  else if (Packages::SFRLine1 == nm)
  {
    if (GetH5Flag()) AddToStoredLinesDesc("#GMS_HDF5_01", "");
    Line1();
    WriteCommentsSfr();
  }
  else if (Packages::SFRLine2 == nm)
  {
    Line2();
  }
  else if (Packages::SFRLine5 == nm)
  {
    Line5();
  }
  else if (Packages::SFRLine6 == nm)
  {
    Line6();
    WriteStoredLinesSfr();
    if (!GetH5Flag() && GetGlobal()->GetCurrentPeriod() == GetGlobal()->NumPeriods())
    {
      LastChanceBeforeWriting();
    }
  }

  return true;
} // MfNativeExpSfr::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSfr::Line1 ()
{
  using namespace MfData::Packages;
  const int *nstrm(0), *nss(0), *istcb1(0), *istcb2(0), *isfropt(0), 
            *nstrail(0), *isuzn(0), *nsfrsets(0);
  const Real *constv(0), *dleak(0);
  MfPackage* a_pSFRLine1 = GetPackage();
  if (a_pSFRLine1->GetField(SFRpack::NSTRM, &nstrm) && nstrm &&
      a_pSFRLine1->GetField(SFRpack::NSS, &nss) && nss &&
      a_pSFRLine1->GetField(SFRpack::CONSTV, &constv) && constv &&
      a_pSFRLine1->GetField(SFRpack::DLEAK, &dleak) && dleak &&
      a_pSFRLine1->GetField(SFRpack::ISTCB1, &istcb1) && istcb1 &&
      a_pSFRLine1->GetField(SFRpack::ISTCB2, &istcb2) && istcb2 &&
      a_pSFRLine1->GetField(SFRpack::ISFROPT, &isfropt) && isfropt &&
      a_pSFRLine1->GetField(SFRpack::NSTRAIL, &nstrail) && nstrail &&
      a_pSFRLine1->GetField(SFRpack::ISUZN, &isuzn) && isuzn &&
      a_pSFRLine1->GetField(SFRpack::NSFRSETS, &nsfrsets) && nsfrsets)
  {
    int flg = STR_FULLWIDTH;
    int w = util::RealWidth();
    CStr ln, desc = " 1. NSTRM NSS NSFRPAR NPARSEG CONST DLEAK ISTCB1 ISTCB2 ";
    if (m_usg) desc.Replace(" 1", "1c");
    ln.Format("%5d %5d %5d %5d %s %s %5d %5d", *nstrm, *nss, 0, 0,
              STR(*constv,-1,w,flg), STR(*dleak,-1,w,flg), *istcb1, *istcb2);

    if (*nstrm < 0)
    {
      CStr nstrmPart;
      nstrmPart.Format(" %5d", *isfropt);
      desc += "[ISFROPT] ";
      ln += nstrmPart;
      if (*isfropt > 1)
      {
        CStr isfrPart;
        isfrPart.Format(" %5d %5d %5d", *nstrail, *isuzn, *nsfrsets);
        ln += isfrPart;
        desc += "[NSTRAIL] [ISUZN] [NSFRSETS]";
      }
    }
    AddToStoredLinesDesc(ln, desc);
  }
} // NativeExpSfr::Line1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSfr::Line2 ()
{
  CStr desc = " 2. KRCH IRCH JRCH ISEG IREACH RCHLEN ";
  if (m_usg) desc.Replace(" 2", "2a");
  if (GetH5Flag())
  {
    H5BcList h(this);
    CStr line = h.SfrLn2();
    if (!line.empty()) AddToStoredLinesDesc(line, desc);
    return;
  }
  using namespace MfData::Packages;
  const int *nstrm(0), *nss(0), *isfropt(0), *istrm(0), *nistrmd(0), *nstrmd(0);
  const Real *strm(0), *uhc(0);
  const double *thts(0), *thti(0), *eps(0);
  MfPackage* a_pSFRLine1 = GetGlobal()->GetPackage(Packages::SFRLine1);
  MfPackage* a_pSFRLine2 = GetPackage();
  if (a_pSFRLine1->GetField(SFRpack::NSTRM, &nstrm) && nstrm &&
      a_pSFRLine1->GetField(SFRpack::NSS, &nss) && nss &&
      a_pSFRLine1->GetField(SFRpack::ISFROPT, &isfropt) && isfropt &&

      a_pSFRLine2->GetField(SFRpack::ISTRM, &istrm) && istrm &&
      a_pSFRLine2->GetField(SFRpack::NISTRMD, &nistrmd) && nistrmd &&
      a_pSFRLine2->GetField(SFRpack::STRM, &strm) && strm &&
      a_pSFRLine2->GetField(SFRpack::NSTRMD, &nstrmd) && nstrmd &&
      a_pSFRLine2->GetField(SFRpack::THTS, &thts) && thts &&
      a_pSFRLine2->GetField(SFRpack::THTI, &thti) && thti &&
      a_pSFRLine2->GetField(SFRpack::EPS, &eps) && eps &&
      a_pSFRLine2->GetField(SFRpack::UHC, &uhc) && uhc)
  {
    int numReaches(*nstrm);
    if (numReaches < 0)
      numReaches = -numReaches;

    int w = util::RealWidth();
    int flg = STR_FULLWIDTH;
    CStr ln;
    for (int i = 0; i < numReaches; i++)
    {
      int nrch;
      int ck = istrm[i*(*nistrmd)+0];
      int ci = istrm[i*(*nistrmd)+1];
      int cj = istrm[i*(*nistrmd)+2];
      int iseg = istrm[i*(*nistrmd)+3];;
      int ireach = istrm[i*(*nistrmd)+4];
      Real rchlen = strm[i*(*nstrmd)+0];
      ln.Format("%5d %5d %5d %5d %5d %s ", ck, ci, cj, iseg, ireach,
                STR(rchlen,-1,w,flg));
      if (m_unstructured)
      {
        desc = "2b. NRCH ISEG IREACH RCHLEN ";
        nrch = istrm[i*(*nistrmd)+5];
        ln.Format("%5d %5d %5d %s ", nrch, iseg, ireach,
                  STR(rchlen,-1,w,flg));
      }

      // STRTOP, SLOPE, STRTHICK, and STRHC1 are read when NSTRM is less
      // than 0 and ISFROPT is 1, 2, or 3.
      if (*nstrm < 0 && *isfropt > 0 && *isfropt < 4)
      { // STRTOP-3 SLOPE-2 STRTHICK-8 STRHC1-6
        Real strtop    = strm[i*(*nstrmd)+2];
        Real slope     = strm[i*(*nstrmd)+1];
        Real strthick  = strm[i*(*nstrmd)+7];
        Real strhc1    = strm[i*(*nstrmd)+5];
        CStr tmp;
        tmp.Format("%s %s %s %s ",
                   STR(strtop,-1,w,flg), STR(slope,-1,w,flg),
                   STR(strthick,-1,w,flg), STR(strhc1,-1,w,flg));
        desc += "[STRTOP] [SLOPE] [STRTHICK] [STRHC1] ";
        // THTS, THTI, and EPS are read when NSTRM is less than 0
        // and ISFROPT is 2 or 3
        if (*isfropt > 1)
        {
          tmp.Format("%s %s %s ", STR(thts[i],-1,15,flg),
                                  STR(thti[i],-1,15,flg),
                                  STR(eps[i],-1,15,flg));
          ln += tmp;
          desc += "[THTS] [THTI] [EPS] ";
        }
        // UHC is read when NSTRM is less than 0 and ISFROPT is 3.
        if (*isfropt == 3)
        {
          tmp.Format("%s ", STR(uhc[i],-1,w,flg));
          ln += tmp;
          desc += "[UHC]";
        }
      }
      AddToStoredLinesDesc(ln, desc);
    }
  }
} // NativeExpSfr::Line2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSfr::Line5 ()
{
  using namespace MfData::Packages;
  const int *itmp(0), *irdflg(0), *iptflg(0);
  MfPackage* a_pSFRLine5 = GetPackage();
  if (a_pSFRLine5->GetField(SFRpack::ITMP, &itmp) && itmp &&
      a_pSFRLine5->GetField(SFRpack::IRDFLG, &irdflg) && irdflg &&
      a_pSFRLine5->GetField(SFRpack::IPTFLG, &iptflg) && iptflg)
  {
    CStr ln;
    ln.Format("%5d %5d %5d", *itmp, *irdflg, *iptflg);
    if (!GetH5Flag()) AddToStoredLinesDesc(ln, Desc5());
    GetGlobal()->SetStrVar(SFR_H5_LINE_5, ln);
    CStr tmp;
    GetGlobal()->GetStrVar(SFR_LINE5, tmp);
    if (tmp.IsEmpty()) tmp += "\n";
    tmp += ln;
    GetGlobal()->SetStrVar(SFR_LINE5, tmp);
  }
} // NativeExpSfr::Line5
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSfr::Line6 ()
{
  if (GetH5Flag())
  {
    CStr line6, line5, desc = "6a. NSEG ICALC OUTSEG IUPSEG";
    GetGlobal()->GetStrVar(SFR_H5_LINE_5, line5);
    H5BcList h(this);
    int irdflg, iptflg, itmp, tmpItmp;
    std::stringstream ss;
    ss << line5;
    ss >> itmp >> irdflg >> iptflg;
    tmpItmp = itmp;
    line6 = h.SfrLn6(tmpItmp);
    if (itmp != tmpItmp) line5.Format("%5d %5d %5d", tmpItmp, irdflg, iptflg);
    AddToStoredLinesDesc(line5, Desc5());
    if (!line6.empty()) AddToStoredLinesDesc(line6, desc);
    return;
  }
  using namespace MfData::Packages;
  using util::ForElement;
  const int *nstrm(0), *nss(0), *isfropt(0);
  const int *iseg(0), *iotsg(0), *idivar(0);
  const Real *seg(0), *xsec(0), *qstage(0);
  MfPackage* a_pSFRLine1 = GetGlobal()->GetPackage(Packages::SFRLine1);
  MfPackage* a_pSFRLine6 = GetPackage();
  if (a_pSFRLine1->GetField(SFRpack::NSTRM, &nstrm) && nstrm &&
      a_pSFRLine1->GetField(SFRpack::NSS, &nss) && nss &&
      a_pSFRLine1->GetField(SFRpack::ISFROPT, &isfropt) && isfropt &&

      a_pSFRLine6->GetField(SFRpack::ISEG, &iseg) && iseg &&
      a_pSFRLine6->GetField(SFRpack::IOTSG, &iotsg) && iotsg &&
      a_pSFRLine6->GetField(SFRpack::IDIVAR, &idivar) && idivar &&
      a_pSFRLine6->GetField(SFRpack::SEG, &seg) && seg &&
      a_pSFRLine6->GetField(SFRpack::XSEC, &xsec) && xsec &&
      a_pSFRLine6->GetField(SFRpack::QSTAGE, &qstage) && qstage)
  {
    m_iseg = iseg;
    m_iotsg = iotsg;
    m_idivar = idivar;
    m_segOrig = seg;
    m_xsec = xsec;
    m_qstage = qstage;

    m_segVec.assign(*nss*m_sz, 0);
    for (size_t i=0; i<m_segVec.size(); ++i) m_segVec[i] = m_segOrig[i];
    m_seg = &m_segVec[0];

    int curSp = GetGlobal()->GetCurrentPeriod();
    CStr ln, tmp, desc;

    std::map<CStr, std::vector<int> > pMap;
    CheckSegForPar(pMap, *nss);
    if (!pMap.empty())
    {
      ParToTmp(pMap, *isfropt, curSp);
    }
    if (GetITMP() < 1 && pMap.size() < 2) return;

    for (int i=0; i<*nss; ++i)
    {
      int nseg        = i + 1;
      Line6a(nseg);
      Line6b(nseg, *isfropt, curSp);
      Line6c(nseg, *isfropt, curSp);
      Line6d(nseg, *isfropt, curSp);
      Line6e(nseg);
      // not supported Line6f();
    }
  }
} // NativeExpSfr::Line6
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSfr::Line6a (int nseg)
{
  using util::ForElement;
  int icalc       = ForElement(m_iseg, 1, nseg, 4);
  int iupseg      = ForElement(m_idivar, 1, nseg, 2);

  CStr line, desc;
  desc = "6a. NSEG ICALC OUTSEG IUPSEG ";
  // "[IPRIOR] [NSTRPTS] FLOW RUNOFF "
  // "ETSW PPTSW [ROUGHCH] [ROUGHBK] [CDPTH] [FDPTH] [AWDTH] [BWDTH]";
  // READ DATA SET 4B OR 6A FOR SEGMENTS THAT ARE NOT DIVERSIONS.
  if (iupseg <= 0)
  {
    if (icalc <= 0)
    {
      line = StrVal(nseg) + Iseg(1, nseg) + Iotsg(nseg) + Idivar(1, nseg) +
              Seg(nseg, 2, 5);
      desc += "FLOW RUNOFF ETSW PPTSW";
    }
    else if (icalc == 1)
    {
      line = StrVal(nseg) + Iseg(1, nseg) + Iotsg(nseg) + Idivar(1, nseg) +
              Seg(nseg, 2, 5) + Seg(16, nseg);
      desc += "FLOW RUNOFF ETSW PPTSW [ROUGHCH]";
    }
    else if (icalc == 2)
    {
      line = StrVal(nseg) + Iseg(1, nseg) + Iotsg(nseg) +
              Idivar(1, nseg) + Seg(nseg, 2, 5) +
              Seg(nseg, 16, 17);
      desc += "FLOW RUNOFF ETSW PPTSW [ROUGHCH] [ROUGHBK]";
    }
    else if (icalc == 3)
    {
      line = StrVal(nseg) + Iseg(1, nseg) + Iotsg(nseg) +
              Idivar(1, nseg) + Seg(nseg, 2, 5) +
              Seg(9, nseg) + Seg(10, nseg) + Seg(14, nseg) +
              Seg(15, nseg);
      desc += "FLOW RUNOFF ETSW PPTSW [CDPTH] [FDPTH] [AWDTH] [BWDTH]";
    }
    else if (icalc == 4)
    {
      line = StrVal(nseg) + Iseg(1, nseg) + Iotsg(nseg) +
              Idivar(1, nseg) + Iseg(2, nseg) +
              Seg(nseg, 2, 5);
      desc += "[NSTRPTS] FLOW RUNOFF ETSW PPTSW";
    }
  }
  else if (icalc <= 0)
  {
    line = StrVal(nseg) + Iseg(1, nseg) + Iotsg(nseg) +
            Idivar(nseg, 1, 2) +
            Seg(nseg, 2, 5);
    desc += "[IPRIOR] FLOW RUNOFF ETSW PPTSW";
  }
  else if (icalc == 1)
  {
    line = StrVal(nseg) + Iseg(1, nseg) + Iotsg(nseg) +
            Idivar(nseg, 1, 2) +
            Seg(nseg, 2, 5) + Seg(16, nseg);
    desc += "[IPRIOR] FLOW RUNOFF ETSW PPTSW [ROUGHCH]";
  }
  else if (icalc == 2)
  {
    line = StrVal(nseg) + Iseg(1, nseg) + Iotsg(nseg) +
            Idivar(nseg, 1, 2) +
            Seg(nseg, 2, 5) +
            Seg(nseg, 16, 17);
    desc += "[IPRIOR] FLOW RUNOFF ETSW PPTSW [ROUGHCH] [ROUGHBK]";
  }
  else if (icalc == 3)
  {
    line = StrVal(nseg) + Iseg(1, nseg) + Iotsg(nseg) +
            Idivar(nseg, 1, 2) +
            Seg(nseg, 2, 5) + Seg(9, nseg) +
            Seg(10, nseg) + Seg(14, nseg) + Seg(15, nseg);
    desc += "[IPRIOR] FLOW RUNOFF ETSW PPTSW [CDPTH] [FDPTH] [AWDTH] [BWDTH]";
  }
  else if (icalc == 4)
  {
    line = StrVal(nseg) + Iseg(1, nseg) + Iotsg(nseg) +
            Idivar(nseg, 1, 2) + Iseg(2, nseg) +
            Seg(nseg, 2, 5);
    desc += "[IPRIOR] [NSTRPTS] FLOW RUNOFF ETSW PPTSW";
  }
  AddToStoredLinesDesc(line, desc);
} // NativeExpSfr::Line6a
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSfr::Line6b (int nseg, int isfropt, int kper)
{
  int icalc       = util::ForElement(m_iseg, 1, nseg, 4);
  // READ DATA SET 4C OR 6B.
  CStr line = "", desc;
  desc = "6b. ";
  if (isfropt == 0)
  {
    if (icalc <= 0)
    {
      line = Seg(nseg, 6, 10);
      desc += "[HCOND1] [THICKM1] [ELEVUP] [WIDTH1] [DEPTH1]";
    }
    else if (icalc == 1)
    {
      line = Seg(nseg, 6, 9);
      desc += "[HCOND1] [THICKM1] [ELEVUP] [WIDTH1]";
    }
    else if (icalc >= 2 && icalc <= 4)
    {
      line = Seg(nseg, 6, 8);
      desc += "[HCOND1] [THICKM1] [ELEVUP]";
    }
  }
  else if (isfropt == 1)
  {
    if (icalc <= 0)
    {
      line = Seg(9, nseg) + Seg(10, nseg);
      desc += "[WIDTH1] [DEPTH1]";
    }
    else if (icalc == 1)
    {
      line = Seg(9, nseg);
      desc += "[WIDTH1]";
    }
  }
  else if (isfropt == 2 || isfropt == 3)
  {
    if (icalc <= 0)
    {
      line = Seg(9, nseg) + Seg(10, nseg);
      desc += "[WIDTH1] [DEPTH1]";
    }
    else if (icalc == 1 && kper == 1)
    {
      line = Seg(9, nseg);
      desc += "[WIDTH1]";
    }
  }
  else if (isfropt == 4)
  {
    if (icalc <= 0)
    {
      line = Seg(nseg, 6, 10);
      desc += "[HCOND1] [THICKM1] [ELEVUP] [WIDTH1] [DEPTH1]";
    }
    else if (icalc == 1)
    {
      if (kper == 1)
      {
        line = Seg(nseg, 6, 9) +
               Seg(nseg, 18, 20);
        desc += "[HCOND1] [THICKM1] [ELEVUP] [WIDTH1] [THTS1] [THTI1] [EPS1]";
      }
      else
      {
        line = Seg(6, nseg);
        desc += "[HCOND1]";
      }
    }
    else if (icalc == 2)
    {
      if (kper == 1)
      {
        line = Seg(nseg, 6, 8) +
               Seg(nseg, 18, 20);
        desc += "[HCOND1] [THICKM1] [ELEVUP] [THTS1] [THTI1] [EPS1]";
      }
      else
      {
        line = Seg(6, nseg);
        desc += "[HCOND1]";
      }
    }
    else if (icalc >= 3 && icalc <= 4)
    {
      line = Seg(nseg, 6, 8);
      desc += "[HCOND1] [THICKM1] [ELEVUP]";
    }
  }
  else if (isfropt == 5)
  {
    if (icalc <= 0)
    {
      line = Seg(nseg, 6, 10);
      desc += "[HCOND1] [THICKM1] [ELEVUP] [WIDTH1] [DEPTH1]";
    }
    else if (icalc == 1)
    {
      if (kper == 1)
      {
        line = Seg(nseg, 6, 9) +
               Seg(nseg, 18, 21);
        desc += "[HCOND1] [THICKM1] [ELEVUP] [WIDTH1] [THTS1] [THTI1] [EPS1] [UHC1]";
      }
      else
      {
        line = Seg(6, nseg);
        desc += "[HCOND1]";
      }
    }
    else if (icalc == 2)
    {
      if (kper == 1)
      {
        line = Seg(nseg, 6, 8) +
               Seg(nseg, 18, 21);
        desc += "[HCOND1] [THICKM1] [ELEVUP] [THTS1] [THTI1] [EPS1] [UHC1]";
      }
      else
      {
        line = Seg(6, nseg);
        desc += "[HCOND1]";
      }
    }
    else if (icalc >= 3 && icalc <= 4)
    {
      line = Seg(nseg, 6, 8);
      desc += "[HCOND1] [THICKM1] [ELEVUP]";
    }
  }
  if (!line.IsEmpty()) AddToStoredLinesDesc(line, desc);
} // NativeExpSfr::Line6b
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSfr::Line6c (int nseg, int isfropt, int kper)
{
  int icalc       = util::ForElement(m_iseg, 1, nseg, 4);
  CStr line = "", desc;
  desc = "6c. ";
  //"[HCOND2] [THICKM2] [ELEVDN] [WIDTH2] [DEPTH2] [THTS2] [THTI2] [EPS2] [UHC2]";
  // READ DATA SET 4D OR 6C.
  if (isfropt == 0)
  {
    if (icalc <= 0)
    {
      line = Seg(nseg, 11, 15);
      desc += "[HCOND2] [THICKM2] [ELEVDN] [WIDTH2] [DEPTH2]";
    }
    else if (icalc == 1)
    {
      line = Seg(nseg, 11, 14);
      desc += "[HCOND2] [THICKM2] [ELEVDN] [WIDTH2]";
    }
    else if (icalc >= 2 && icalc <= 4)
    {
      line = Seg(nseg, 11, 13);
      desc += "[HCOND2] [THICKM2] [ELEVDN]";
    }
  }
  else if (isfropt == 1)
  {
    if (icalc <= 0)
    {
      line = Seg(14, nseg) + Seg(15, nseg);
      desc += "[WIDTH2] [DEPTH2]";
    }
    else if (icalc == 1)
    {
      line = Seg(14, nseg);
      desc += "[WIDTH2]";
    }
  }
  else if (isfropt == 2)
  {
    if (icalc <= 0)
    {
      line = Seg(14, nseg) + Seg(15, nseg);
      desc += "[WIDTH2] [DEPTH2]";
    }
    else if (icalc == 1 && kper == 1)
    {
      line = Seg(14, nseg);
      desc += "[WIDTH2]";
    }
  }
  else if (isfropt == 3)
  {
    if (icalc <= 0)
    {
      line = Seg(14, nseg) + Seg(15, nseg);
      desc += "[WIDTH2] [DEPTH2]";
    }
    else if (icalc == 1 && kper == 1)
    {
      line = Seg(14, nseg);
      desc += "[WIDTH2]";
    }
  }
  else if (isfropt == 4)
  {
    if (icalc <= 0)
    {
      line = Seg(nseg, 11, 15);
      desc += "[HCOND2] [THICKM2] [ELEVDN] [WIDTH2] [DEPTH2]";
    }
    else if (icalc == 1)
    {
      if (kper == 1)
      {
        line = Seg(nseg, 11, 14) +
               Seg(nseg, 22, 24);
        desc += "[HCOND2] [THICKM2] [ELEVDN] [WIDTH2] [THTS2] [THTI2] [EPS2]";
      }
      else
      {
        line = Seg(11, nseg);
        desc += "[HCOND2]";
      }
    }
    else if (icalc == 2)
    {
      if (kper == 1)
      {
        line = Seg(nseg, 11, 13) +
               Seg(nseg, 22, 24);
        desc += "[HCOND2] [THICKM2] [ELEVDN] [THTS2] [THTI2] [EPS2]";
      }
      else
      {
        line = Seg(11, nseg);
        desc += "[HCOND2]";
      }
    }
    else if (icalc >= 3 && icalc <= 4)
    {
      line = Seg(nseg, 11, 13);
      desc += "[HCOND2] [THICKM2] [ELEVDN]";
    }
  }
  else if (isfropt == 5)
  {
    if (icalc <= 0)
    {
      line = Seg(nseg, 11, 15);
      desc += "[HCOND2] [THICKM2] [ELEVDN] [WIDTH2] [DEPTH2]";
    }
    else if (icalc == 1)
    {
      if (kper == 1)
      {
        line = Seg(nseg, 11, 14) +
               Seg(nseg, 22, 25);
        desc += "[HCOND2] [THICKM2] [ELEVDN] [WIDTH2] [THTS2] [THTI2] [EPS2] [UHC2]";
      }
      else
      {
        line = Seg(11, nseg);
        desc += "[HCOND2]";
      }
    }
    else if (icalc == 2)
    {
      if (kper == 1)
      {
        line = Seg(nseg, 11, 13) +
               Seg(nseg, 22, 25);
        desc += "[HCOND2] [THICKM2] [ELEVDN] [THTS2] [THTI2] [EPS2] [UHC2]";
      }
      else
      {
        line = Seg(11, nseg);
        desc += "[HCOND2]";
      }
    }
    else if (icalc >= 3 && icalc <= 4)
    {
      line = Seg(nseg, 11, 13);
      desc += "[HCOND2] [THICKM2] [ELEVDN]";
    }
  }
  if (!line.IsEmpty()) AddToStoredLinesDesc(line, desc);
} // NativeExpSfr::Line6c
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSfr::Line6d (int nseg, int isfropt, int kper)
{
  int icalc       = util::ForElement(m_iseg, 1, nseg, 4);
  // READ DATA SET 4E OR 6D FOR SEGMENT WHEN ICALC IS 2.
  CStr line = "", desc1, desc2;
  desc1 = "6d. XCPT1 XCPT2 ... XCPT8";
  desc2 = "6d. ZCPT1 ZCPT2 ... ZCPT8";
  if (icalc == 2)
  {
    if (kper == 1 || isfropt <= 1)
    {
      line = Xsec(nseg, 1, 8);
      if (!line.IsEmpty()) AddToStoredLinesDesc(line, desc1);
      line = Xsec(nseg, 9, 16);
      if (!line.IsEmpty()) AddToStoredLinesDesc(line, desc2);
    }
  }
} // NativeExpSfr::Line6d
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSfr::Line6e (int nseg)
{
  int icalc       = util::ForElement(m_iseg, 1, nseg, 4);
  // READ DATA SET 4F OR 6E FOR SEGMENT WHEN ICALC IS 4.
  CStr line = "", d1, d2, d3;
  d1 = "6e. FLOWTAB(1) FLOWTAB(2) ... FLOWTAB(NSTRPTS)";
  d2 = "6e. DPTHTAB(1) DPTHTAB(2) ... DPTHTAB(NSTRPTS)";
  d3 = "6e. WDTHTAB(1) WDTHTAB(2) ... WDTHTAB(NSTRPTS)";
  if (icalc == 4)
  {
    int nstrpts = util::ForElement(m_iseg, 2, nseg, 4);
    line = Qstage(nseg, 1, nstrpts);
    if (!line.IsEmpty()) AddToStoredLinesDesc(line, d1);
    line = Qstage(nseg, nstrpts+1, 2*nstrpts);
    if (!line.IsEmpty()) AddToStoredLinesDesc(line, d2);
    line = Qstage(nseg, 2*nstrpts+1, 3*nstrpts);
    if (!line.IsEmpty()) AddToStoredLinesDesc(line, d3);
  }
} // NativeExpSfr::Line6e
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSfr::StrVal (int a_)
{
  CStr s;
  s.Format("%5d ", a_);
  return s;
} // NativeExpSfr::StrVal
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSfr::StrVal (Real a_)
{
  CStr s;
  s.Format("%s ", STR(a_, -1, util::RealWidth(), STR_FULLWIDTH));
  return s;
} // NativeExpSfr::StrVal
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSfr::Iseg (int a_i, int a_j)
{
  using namespace util;
  CStr s;
  s.Format("%5d ", ForElement(m_iseg, a_i, a_j, 4));
  return s;
} // NativeExpSfr::Iseg
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSfr::Iotsg (int a_i)
{
  CStr s;
  s.Format("%5d ", m_iotsg[a_i-1]);
  return s;
} // NativeExpSfr::Iotsg
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSfr::Idivar (int a_i, int a_j)
{
  using namespace util;
  CStr s;
  s.Format("%5d ", ForElement(m_idivar, a_i, a_j, 2));
  return s;
} // NativeExpSfr::Idivar
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSfr::Idivar (int a_indexTwo,
                           int a_indexOneFrom,
                           int a_indexOneTo)
{
  using namespace util;
  CStr line;
  for (int i = a_indexOneFrom; i <= a_indexOneTo; ++i)
  {
    line += Idivar(i, a_indexTwo);
  }
  return line;
} // NativeExpSfr::Idivar
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSfr::Seg (int a_i, int a_j)
{
  using namespace util;
  CStr s;
  s.Format("%s ", STR(ForElement(m_seg, a_i, a_j, m_sz), -1,
                      util::RealWidth(), STR_FULLWIDTH));
  return s;
} // NativeExpSfr::Seg
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSfr::Seg (int a_indexTwo,
                        int a_indexOneFrom,
                        int a_indexOneTo)
{
  using namespace util;
  CStr line;
  for (int i = a_indexOneFrom; i <= a_indexOneTo; ++i)
  {
    line += StrVal(ForElement(m_seg, i, a_indexTwo, m_sz));
  }
  return line;
} // NativeExpSfr::Seg
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSfr::Xsec (int a_indexTwo,
                         int a_indexOneFrom,
                         int a_indexOneTo)
{
  using namespace util;
  CStr line;
  for (int i = a_indexOneFrom; i <= a_indexOneTo; ++i)
  {
    line += StrVal(ForElement(m_xsec, i, a_indexTwo, 16));
  }
  return line;
} // NativeExpSfr::Xsec
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSfr::Qstage (int a_indexTwo,
                           int a_indexOneFrom,
                           int a_indexOneTo)
{
  using namespace util;
  CStr line;
  for (int i = a_indexOneFrom; i <= a_indexOneTo; ++i)
  {
    line += StrVal(ForElement(m_qstage, i, a_indexTwo, 16));
  }
  return line;
} // NativeExpSfr::Qstage
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSfr::AddToStoredLinesDesc (const char* a_line,
                                         const char* a_desc)
{
  MfPackage* p = GetGlobal()->GetPackage(Packages::SFR);
  MfPackage* curP = GetPackage();

  if (!m_writingPar) SetData(GetNative(), GetGlobal(), p);
  NativePackExp::AddToStoredLinesDesc(a_line, a_desc);
  if (!m_writingPar) SetData(GetNative(), GetGlobal(), curP);
} // NativeExpSfr::AddToStoredLinesDesc
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSfr::LastChanceBeforeWriting ()
{
  std::map<int, CStr>& m(GetParMap());
  if (m.empty())
  {
    CStr fname = TmpFileName(SFR_NO_PARAM);
    remove(fname);
    return;
  }

  CStr tmpF = CopySfrFile();
  GetNative()->GetExp()->ClearFile(Packages::SFR);
  RewriteLines1and2(tmpF, NparToWrite());
  std::vector< std::vector<CStr> > vecParSp;
  WriteLines3and4(vecParSp);
  WriteLines5to7(vecParSp);

} // NativeExpSfr::LastChanceBeforeWriting
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSfr::CopySfrFile ()
{
  CStr fname = TmpFileName("stuff");
  fname.Replace(".stuff.txt", "");
  CStr tmpFname = fname;
  tmpFname += ".tmp";
  // copy the sfr file
  ::CopyFile(fname, tmpFname, false);

  TmpPackageNameChanger(GetPackage(), "SFR");
  ClearFile();

  return tmpFname;
} // NativeExpSfr::CopySfrFile
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSfr::RewriteLines1and2 (CStr& a_tmpF, int a_npar)
{
  EReadAsciiFile f(a_tmpF);
  f.OpenFile();
  bool done = false;
  CStr ln, ln1, desc;
  while (!done && f.GetLine(&ln))
  {
    if (ln.Find("#") == 0) continue;
    if (ln.Find("#  1. NSTRM") != -1 ||
        ln.Find("# 1c. NSTRM") != -1 || 
        ln.Find("#  2. KRCH") != -1 ||
        ln.Find("# 2a. KRCH") != -1 ||
        ln.Find("# 2b. NRCH") != -1)
    {
      desc = ln;
      ln1 = ln.Left(ln.Find("#"));
      desc.Replace(ln1, "");
      desc.Replace("# ", "");
      ln1.TrimRight();

      if (ln.Find("#  1. NSTRM") != -1 ||
          ln.Find("# 1c. NSTRM") != -1)
      {
        int nstrm, nss, tmp, nparseg;
        GetGlobal()->GetIntVar(SFR_NPARSEG, nparseg);
        std::stringstream ss;
        ss << ln1;
        ss >> nstrm >> nss >> tmp >> tmp;
        CStr tmpStr;
        std::getline(ss, tmpStr);
        ln1.Format("%5d %5d %5d %5d%s", nstrm, nss, a_npar, nparseg, tmpStr);
      }
      AddToStoredLinesDesc(ln1, desc);
    }
    else done = true;
  }
  WriteCommentsSfr();
  WriteStoredLinesSfr();
  f.CloseFile();
  remove(a_tmpF);
} // NativeExpSfr::RewriteLines1and2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSfr::WriteLines3and4 (std::vector< std::vector<CStr> >& a_vec)
{
  int nStress = GetGlobal()->NumPeriods();
  a_vec.assign(nStress, std::vector<CStr>());
  std::map<int, CStr>& pMap = GetParMap();
  std::map<int, CStr>::iterator it = pMap.begin();
  for (; it != pMap.end(); ++it)
  {
    CStr parStr;
    int nSeg, nInst;
    std::vector<int> vStress, vNseg;
    GetParameterStessPeriods(it->second, vStress, vNseg);
    std::map<int, int> mapNseg_NInst;
    GetParameterNSegNInst(vStress, vNseg, mapNseg_NInst);
    std::map<int, int>::iterator it2(mapNseg_NInst.begin());
    for (int cnt = 0; it2 != mapNseg_NInst.end(); ++it2, ++cnt)
    {
      nSeg = it2->first;
      nInst = it2->second;
      parStr = Line3(it->second, nSeg, nInst, cnt);

      CStr fname = TmpFileName(it->second);
      EReadAsciiFile f(fname);
      f.OpenFile();
      CStr parLine;
      for (size_t i=0; i<vStress.size(); ++i)
      {
        if (vStress[i]  < 0)
        {
          a_vec[abs(vStress[i])-1].push_back(parLine);
          continue;
        }
        // get the lines from the temp file
        int nLine, sp;
        f.GetLine();
        f.ReadData(nLine);
        f.ReadData(sp);
        std::vector<CStr> lines;
        for (int j=0; j<nLine; ++j)
        {
          lines.push_back(CStr());
          f.GetLine(&lines.back());
        }
        // see if we use these lines for this parameter
        if (nSeg == vNseg[i])
        {
          parLine = parStr;
          if (nInst > 1) parLine += Line4a(vStress[i]);
          Line4bto4g(lines);
          WriteStoredLinesSfr();
          a_vec[vStress[i]-1].push_back(parLine);
        }
      }
      f.CloseFile();
    }
  }
  // remove temp files
  for (it = pMap.begin(); it != pMap.end(); ++it)
  {
    CStr fname = TmpFileName(it->second);
    remove(fname);
  }
} // NativeExpSfr::WriteLines3and4
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSfr::WriteLines5to7 (std::vector< std::vector<CStr> >& a_vec)
{
  CStr fname = TmpFileName(SFR_NO_PARAM);
  EReadAsciiFile f(fname);
  f.OpenFile();
  int nStress = GetGlobal()->NumPeriods();
  CStr line, line5;
  GetGlobal()->GetStrVar(SFR_LINE5, line5);
  std::stringstream ss;
  ss << line5;
  for (int i=0; i<nStress; ++i)
  {
    std::getline(ss, line5);
    int vals[3];
    ss >> vals[0] >> vals[1] >> vals[2];
    line5.Format("%5d %5d %5d %5d", 0, vals[1], vals[2], a_vec[i].size());
    AddToStoredLinesDesc(line5, Desc5());
    for (size_t j=0; j<a_vec[i].size(); ++j)
    {
      AddToStoredLinesDesc(a_vec[i][j], " 7. Pname [Iname]");
    }
  }
  WriteStoredLinesSfr();
} // NativeExpSfr::WriteLines5to7
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSfr::Line4a (int a_sp)
{
  CStr line, desc = "4a. [INSTNAM]";
  line.Format("SP_%d", a_sp);
  AddToStoredLinesDesc(line, desc);
  return line;
} // NativeExpSfr::Line4a
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSfr::Line4bto4g (std::vector<CStr>& a_lines)
{
  CStr desc, line;
  for (size_t i=0; i<a_lines.size(); ++i)
  {
    desc = a_lines[i];
    line = a_lines[i].Left(a_lines[i].Find("#"));
    desc.Replace(line, "");
    desc.Replace("# 6a", "4b");
    desc.Replace("# 6b. [HCOND1] [THICKM1] [ELEVUP]",
                   "4c. Hc1fact THICKM1 ELEVUP");
    desc.Replace("# 6c. [HCOND2] [THICKM2] [ELEVDN]",
                   "4d. Hc2fact THICKM2 ELEVDN");
    desc.Replace("# 6d", "4e");
    desc.Replace("# 6e", "4f");
    line.TrimRight();
    AddToStoredLinesDesc(line, desc);
  }
} // NativeExpSfr::Line4bto4g
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSfr::Line3 (CStr& a_pname, int a_nSeg, int a_nInst, int a_cnt)
{
  CStr line, desc = " 3. PARNAM PARTYP Parval NLST [INSTANCES  NUMINST]";
  CStr name = a_pname;
  if (a_cnt > 0) name.Format("%s_%d", a_pname, a_cnt);
  while (name.GetLength() < 12) name += " ";
  ParamList *pList;
  Parameters::GetParameterList(&pList);
  Param par;
  if (!pList->FindByName(a_pname, &par))
  {
    par.m_name = a_pname;
    par.m_value = 1.0;
    pList->PushBack(&par);
  }
  double val = par.m_value;
  int w = util::RealWidth();
  int flg = STR_FULLWIDTH;
  line.Format("%s SFR %s %5d ", name, STR(val,-1,w,flg), a_nSeg);
  if (a_nInst > 1)
  {
    CStr l1; l1.Format("INSTANCES %5d", a_nInst);
    line += l1;
  }
  AddToStoredLinesDesc(line, desc);
  if (!pList->FindByName(name, &par))
  {
    AddToPval(name, val);
  }
  return name;
} // NativeExpSfr::Line3
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSfr::GetParameterNSegNInst (std::vector<int>& a_vStress,
                                          std::vector<int>& a_vNseg,
                                          std::map<int, int>& a_mapNseg_NInst)
{
  std::set<int> setNseg(a_vNseg.begin(), a_vNseg.end());
  std::set<int>::iterator it(setNseg.begin());
  a_mapNseg_NInst.clear();
  for (; it != setNseg.end(); ++it)
  {
    a_mapNseg_NInst[*it] = 0;
  }
  for (it = setNseg.begin(); it != setNseg.end(); ++it)
  {
    for (size_t i=0; i<a_vNseg.size(); ++i)
    {
      if (a_vNseg[i] == *it && a_vStress[i] > 0)
        a_mapNseg_NInst[*it] += 1;
    }
  }
} // NativeExpSfr::GetParameterNSegNInst
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSfr::GetParameterStessPeriods (CStr& a_pname,
                                             std::vector<int>& a_vStress,
                                             std::vector<int>& a_vNseg)
{
  CStr strVal, varName = SFR_PARAM_SP;
  varName += a_pname;
  {
    a_vStress.resize(0);
    GetGlobal()->GetStrVar(varName, strVal);
    std::stringstream ss;
    ss << strVal;
    int iVal;
    while (ss.good())
    {
      ss >> iVal;
      if (ss.good()) a_vStress.push_back(iVal);
    }
  }
  varName = SFR_PARAM_SP_NSEG;
  varName += a_pname;
  {
    a_vNseg.resize(0);
    GetGlobal()->GetStrVar(varName, strVal);
    std::stringstream ss;
    ss << strVal;
    int iVal;
    while (ss.good())
    {
      ss >> iVal;
      if (ss.good()) a_vNseg.push_back(iVal);
    }
  }
} // NativeExpSfr::GetParameterStessPeriods
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
int NativeExpSfr::GetITMP ()
{
  int ival(0);
  using namespace MfData::Packages;
  const int *itmp(0);
  MfPackage* a_pSFRLine5 = GetGlobal()->GetPackage(SFRLine5);
  if (a_pSFRLine5->GetField(SFRpack::ITMP, &itmp) && itmp) ival = *itmp;
  return ival;
} // NativeExpSfr::GetITMP
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSfr::WriteCommentsSfr ()
{
  MfPackage *p = GetGlobal()->GetPackage(Packages::SFR);
  MfPackage *orig = GetPackage();
  SetData(GetNative(), GetGlobal(), p);
  WriteComments();
  SetData(GetNative(), GetGlobal(), orig);
} // NativeExpSfr::WriteCommentsSfr
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSfr::WriteStoredLinesSfr ()
{
  MfPackage *p = GetGlobal()->GetPackage(Packages::SFR);
  MfPackage *orig = GetPackage();
  SetData(GetNative(), GetGlobal(), p);
  WriteStoredLines();
  SetData(GetNative(), GetGlobal(), orig);
} // NativeExpSfr::WriteStoredLinesSfr
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
std::map<int, CStr>& NativeExpSfr::GetParMap ()
{
  if (m_mapKeyName.empty())
  {
    std::vector<Param> vecP = MfExportUtil::GetParamsOfType("SFR");
    for (size_t i=0; i<vecP.size(); ++i)
    {
      m_mapKeyName[(int)vecP[i].m_key] = vecP[i].m_name;
    }
  }
  return m_mapKeyName;
} // NativeExpSfr::GetParMap
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
std::map<int, Real>& NativeExpSfr::GetParMapVal ()
{
  if (m_mapKeyVal.empty())
  {
    std::vector<Param> vecP = MfExportUtil::GetParamsOfType("SFR");
    for (size_t i=0; i<vecP.size(); ++i)
    {
      m_mapKeyVal[(int)vecP[i].m_key] = (Real)vecP[i].m_value;
    }
  }
  return m_mapKeyVal;
} // NativeExpSfr::GetParMap
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSfr::CheckSegForPar (std::map<CStr, std::vector<int> >& a_map,
                                   int a_nss)
{
  using util::ForElement;
  std::vector<Real>& condfact1 = SfrCondFact((int)GetGlobal()->CurModIdx());
  if (condfact1.size() < (size_t)a_nss) condfact1.assign(a_nss, 1.0);
  std::vector<Real>& condfact2 = SfrCondFact2((int)GetGlobal()->CurModIdx());
  if (condfact2.size() < (size_t)a_nss) condfact2.assign(a_nss, 1.0);
  std::map<int, CStr>& pMap = GetParMap();
  std::map<int, Real>& pMapVal = GetParMapVal();
  Real* seg = const_cast<Real*>(m_seg);
  for (int i=0; i<a_nss; ++i)
  {
    Real hcond1Fact = condfact1[i];
    Real hcond2Fact = condfact2[i];
    Real& hcond1 = ForElement(seg,  6, i+1, m_sz);
    Real& hcond2 = ForElement(seg, 11, i+1, m_sz);
    if (pMap.find((int)hcond1) != pMap.end())
    {
      a_map[pMap[(int)hcond1]].push_back(i);
      if (hcond2 == hcond1)
      {
        hcond2 = hcond2Fact;
      }
      else if (pMap.find((int)hcond2) != pMap.end())
      {
        hcond2 = pMapVal[(int)hcond2] * hcond2Fact;
        hcond2 = hcond2 / pMapVal[(int)hcond1];
      }
      else
      {
        hcond2 = hcond2 / pMapVal[(int)hcond1];
      }
      hcond1 = hcond1Fact;
    }
    else if (pMap.find((int)hcond2) != pMap.end())
    {
      a_map[pMap[(int)hcond2]].push_back(i);
      hcond1 = hcond1 / pMapVal[(int)hcond2];
      hcond2 = hcond2Fact;
    }
    else
    {
      a_map[SFR_NO_PARAM].push_back(i);
    }
  }
} // NativeExpSfr::CheckSegForPar
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSfr::ParToTmp (std::map<CStr, std::vector<int> >& a_map,
                             int isfropt, int curSp)
{
  m_writingPar = true;
  std::map<CStr, std::vector<int> >::iterator it(a_map.begin());
  int nParSeg(0);
  GetGlobal()->GetIntVar(SFR_NPARSEG, nParSeg);
  for (; it != a_map.end(); ++it)
  {
    for (size_t i=0; i<it->second.size(); ++i)
    {
      int nseg = (int)it->second[i] + 1;
      Line6a(nseg);
      Line6b(nseg, isfropt, curSp);
      Line6c(nseg, isfropt, curSp);
      Line6d(nseg, isfropt, curSp);
      Line6e(nseg);
    }
    int mult(1);
    if (!WriteStoredLinesToTmp(it->first)) mult = -1;

    AddToGlobalVar(GetGlobal(), SFR_PARAM_SP, it->first, curSp*mult);
    AddToGlobalVar(GetGlobal(), SFR_PARAM_SP_NSEG, it->first,
                   (int)it->second.size());

    if (mult > 0)
    {
      nParSeg += (int)it->second.size();
    }
  }

  GetGlobal()->SetIntVar(SFR_NPARSEG, nParSeg);

  m_writingPar = false;
} // NativeExpSfr::ParToTmp
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpSfr::WriteStoredLinesToTmp (const CStr& a_pname)
{
  CStr tmpFname = TmpFileName(a_pname);
  std::string wFlg("a");
  int firstTime(true);
  CStr var = "SFRVAR_";
  var += a_pname;
  GetGlobal()->GetIntVar(var, firstTime);
  if (firstTime) wFlg = "w";
  firstTime = false;
  GetGlobal()->SetIntVar(var, firstTime);

  CStr line;
  int len = GetNative()->GetExp()->GetMaxLineLengthFromType(Packages::SFR);
  MfPackage* p = GetPackage();
  std::vector<CStr>& lines(p->StringsToWrite());
  std::vector<CStr>& descs(p->StringDescriptions());

  std::stringstream ss;
  for (size_t i=0; i<lines.size(); ++i)
  {
    line = lines[i];
    int size = len-line.GetLength();
    if (size < 0) size = 0;
    CStr buff(size, ' ');
    line += buff;
    line += " # ";
    line += descs[i];
    ss << line << "\n";
  }

  bool rval(false);
  CStr tmpStr;
  GetGlobal()->GetStrVar(SFR_PAR_DEF_SP, tmpStr);
  if (tmpStr != ss.str())
  {
    FILE* fp = fopen(tmpFname, wFlg.c_str());
    if (!fp) return rval;
    fprintf(fp, "%d %d\n", lines.size(), GetGlobal()->GetCurrentPeriod());
    fprintf(fp, "%s", ss.str().c_str());
    fclose(fp);
    GetGlobal()->SetStrVar(SFR_PAR_DEF_SP, ss.str());
    rval = true;
  }

  lines.clear();
  descs.clear();
  return rval;
} // NativeExpSfr::WriteStoredLinesToTmp
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSfr::TmpFileName (const CStr& a_name)
{
  CStr base = GetNative()->FileName();
  util::StripExtensionFromFilename(base, base);
  CStr fname;
  fname.Format("%s.sfr.%s.txt", base, a_name);
  return fname;
} // NativeExpSfr::TmpFileName
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpSfr::NoParamExists ()
{
  CStr fname = TmpFileName(SFR_NO_PARAM);
  FILE *fp = fopen(fname, "r");
  if (!fp) return false;
  fclose(fp);
  return true;
} // NativeExpSfr::NoParamExists
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSfr::AddToPval (const CStr& a_name, double a_val)
{
  MfPackage* p = GetGlobal()->GetPackage(Packages::PVAL);
  if (p)
  {
    CStr desc = p->StringDescriptions().back();
    CStr line, name = a_name;
    while (name.GetLength() < 12) name += " ";
    line.Format("%s %s", name, STR(a_val));
    p->StringsToWrite().push_back(line);
    p->StringDescriptions().push_back(desc);
  }
} // NativeExpSfr::AddToPval
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
int NativeExpSfr::NparToWrite ()
{
  std::map<int, CStr>& pMap = GetParMap();
  if (NoParamExists())
  {
    ParamList *pList; Parameters::GetParameterList(&pList);
    int key = (int)pList->UnusedParamKey();
    pMap[key] = SFR_NO_PARAM;
  }
  std::map<int, CStr>::iterator it = pMap.begin();
  int npar(0);
  for (; it != pMap.end(); ++it)
  {
    std::vector<int> vStress, vNseg;
    GetParameterStessPeriods(it->second, vStress, vNseg);
    std::map<int, int> mapNseg_NInst;
    GetParameterNSegNInst(vStress, vNseg, mapNseg_NInst);
    npar += (int)mapNseg_NInst.size();
  }
  return npar;
} // NativeExpSfr::NparToWrite
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSfr::SaveCondFact ()
{
  const Real *condfact(0), *condfact2(0);
  const int  *nval(0), *nval2(0);
  MfPackage* a_p = GetPackage();
  if (a_p->GetField("CONDFACT", &condfact) && condfact &&
      a_p->GetField("NVAL", &nval) && nval &&
      a_p->GetField("CONDFACT2", &condfact2) && condfact2 &&
      a_p->GetField("NVAL2", &nval2) && nval2)
  {
    int idx = (int)GetGlobal()->CurModIdx();
    std::vector<Real>& vec = SfrCondFact(idx);
    vec.resize(*nval);
    for (size_t i=0; i<vec.size(); ++i) vec[i] = condfact[i];
    std::vector<Real>& vec2 = SfrCondFact2(idx);
    vec2.resize(*nval2);
    for (size_t i=0; i<vec2.size(); ++i) vec2[i] = condfact2[i];
  }
} // NativeExpSfr::SaveCondFact


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData\MfExport\private\Native\NativeExpSfr.t.h>


using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpSfrT::setUp ()
{
  m_p = NULL;
  Mf2kNative* n = new Mf2kNative;
  MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  MfPackage* dis = new MfPackage(Packages::SFRLine1);
  NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  m_p = dynamic_cast<NativeExpSfr*>(p);
} // NativeExpSfrT::setUp
//------------------------------------------------------------------------------
void NativeExpSfrT::tearDown ()
{
  m_p->UnitTestingDeletePointers();
  delete(m_p);
} // NativeExpSfrT::tearDown
//------------------------------------------------------------------------------
void NativeExpSfrT::testCreateClass ()
{
  TS_ASSERT(m_p);
} // NativeExpSfrT::testCreateClass

#endif
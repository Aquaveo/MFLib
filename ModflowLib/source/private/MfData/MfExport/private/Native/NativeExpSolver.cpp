//------------------------------------------------------------------------------
// FILE      NativeExpSolver.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/MfData/MfExport/private/Native/NativeExpSolver.h>

#include <sstream>

#include <private/MfData/MfGlobal.h>
#include <private/MfData/MfExport/private/Mf2kNative.h>
#include <private/MfData/MfExport/private/Native/NativePackExp.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackFields.h>
#include <private/MfData/Packages/MfPackStrings.h>

using namespace MfData::Export;
using namespace MfData::Packages;
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpSolver::NativeExpSolver ()
{
} // MfNativeExpSolver::MfNativeExpSolver
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpSolver::~NativeExpSolver ()
{
} // MfNativeExpSolver::~MfNativeExpSolver
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpSolver::Export ()
{
  CStr name = GetPackage()->PackageName();
  bool mf6 = GetNative() && GetNative()->GetExportMf6();

  TmpPackageNameChanger* tmp(nullptr);
  if (mf6)
  {
    Export_IMS();
    tmp = new TmpPackageNameChanger(GetPackage(), "IMS");
  }
  else if (Packages::SIP == name)      Export_SIP();
  else if (Packages::DE4Line2 == name) Export_DE4();
  else if (Packages::SOR == name)      Export_SOR();
  else if (Packages::PCG == name)      Export_PCG();
  else if (Packages::PCGN == name)     Export_PCGN();
  else if (Packages::LMG == name)      Export_LMG();
  else if (Packages::GMG == name)      Export_GMG();
  else if (Packages::NWT == name)      Export_NWT();
  else if (Packages::SMS == name)      Export_SMS();


  WriteComments();
  WriteStoredLines();
  if (tmp) delete(tmp);
  return true;
} // MfNativeExpSolver::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSolver::Desc (CStr a_name,
                            int a_line)
{
  std::vector<CStr> desc;
  if (SIP == a_name)
  {
    desc.push_back(" 1. MXITER NPARM");
    desc.push_back(" 2. ACCL HCLOSE IPCALC WSEED IPRSIP");
  }
  else if (DE4 == a_name)
  {
    desc.push_back(" 1. ITMX MXUP MXLOW MXBW");
    desc.push_back(" 2. IFREQ MUTD4 ACCL HCLOSE IPRD4");
  }
  else if (SOR == a_name)
  {
    desc.push_back(" 1. MXITER");
    desc.push_back(" 2. ACCL HCLOSE IPRSOR");
  }
  else if (PCG == a_name)
  {
    desc.push_back(" 1. MXITER ITER1 NPCOND [IHCOFADD]");
    desc.push_back(" 2. HCLOSE RCLOSE RELAX NBPOL IPRPCG MUTPCG DAMPPCG [DAMPPCGT]");
  }
  else if (PCGN == a_name)
  {
    desc.push_back(" 1. ITER_MO, ITER_MI, CLOSE_R, CLOSE_H");
    desc.push_back(" 2. RELAX, IFILL, UNIT_PC, UNIT_TS");
    desc.push_back(" 3. ADAMP, DAMP, DAMP_LB, RATE_D, CHGLIMIT");
    desc.push_back(" 4. ACNVG, CNVG_LB, MCNVG, RATE_C, IPUNIT");
  }
  else if (LMG == a_name)
  {
    desc.push_back(" 1. STOR1 STOR2 STOR3 ICG");
    desc.push_back(" 2. MXITER MXCYC B/RCLOSE DAMP IOUTAMG");
    desc.push_back(" 3. DUP DLOW");
    desc.push_back(" 4. HCLOSE CONTROL");
  }
  else if (GMG == a_name)
  {
    desc.push_back(" 1. RCLOSE IITER HCLOSE MXITER");
    desc.push_back(" 2. DAMP IADAMP IOUTGMG [IUNITMHC]");
    desc.push_back(" 3. ISM ISC [DUP DLOW CHGLIMIT]");
    desc.push_back(" 4. RELAX");
  }
  else if (NWT == a_name)
  {
    desc.push_back(" 1. HEADTOL FLUXTOL MAXITEROUT THICKFACT LINMETH IPRNWT "
                   "IBOTAV OPTIONS [DBDTHETA] [DBDKAPPA] [DBDGAMMA] [MOMFACT] "
                   "[BACKFLAG] [MAXBACKITER] [BACKTOL] [BACKREDUCE]");
    desc.push_back("2a. [MAXITINNER] [ILUMETHOD][LEVFILL][STOPTOL][MSDR]");
    desc.push_back("2b. [IACL][NORDER][LEVEL][NORTH][IREDSYS][RRCTOLS]"
                   "[IDROPTOL][EPSRN] [HCLOSEXMD][MXITERXMD]");
  }
  else if (SMS == a_name) {
    desc.push_back("1a. OPTIONS");
    desc.push_back("1b. HCLOSE HICLOSE MXITER ITER1 IPRSMS NONLINMETH LINMETH");
    desc.push_back(" 2. THETA AKAPPA GAMMA AMOMENTUM NUMTRACK BTOL BREDUC RESLIM");
    desc.push_back(" 3. IACL NORDER LEVEL NORTH IREDSYS RRCTOL IDROPTOL EPSRN");
    desc.push_back(" 4. IPC ISCL IORD RCLOSEPCGU");
  }

  try
  {
    return desc.at(a_line-1);
  }
  catch (std::out_of_range&) {}
  return CStr();
} // NativeExpSolver::Desc
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSolver::Export_SIP ()
{
  AddToStoredLinesDesc(Line1_SIP(), Desc(SIP, 1));
  AddToStoredLinesDesc(Line2_SIP(), Desc(SIP, 2));
} // NativeExpSolver::Export_SIP
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSolver::Line1_SIP ()
{
  CStr rval;
  const int *mxiter, *nparm;

  if (GetPackage()->GetField(SipPack::MXITER, &mxiter) && mxiter &&
      GetPackage()->GetField(SipPack::NPARM, &nparm) && nparm)
  {
    rval.Format("%d %d ", *mxiter, *nparm);
  }

  return rval;
} // NativeExpSolver::Line1_SIP
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSolver::Line2_SIP ()
{
  CStr rval;
  const int *ipcalc, *iprsip;
  const Real *accl, *hclose, *wseed;

  if (GetPackage()->GetField(SipPack::IPCALC, &ipcalc) && ipcalc &&
      GetPackage()->GetField(SipPack::IPRSIP, &iprsip) && iprsip &&
      GetPackage()->GetField(SipPack::ACCL, &accl) && accl &&
      GetPackage()->GetField(SipPack::HCLOSE, &hclose) && hclose &&
      GetPackage()->GetField(SipPack::WSEED, &wseed) && wseed)
  {
    rval.Format("%s %s %d %s %d ", STR(*accl), STR(*hclose), *ipcalc,
                                   STR(*wseed), *iprsip);
  }
  return rval;
} // NativeExpSolver::Line2_SIP
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSolver::Export_DE4 ()
{
  TmpPackageNameChanger tmp(GetPackage(), Packages::DE4);
  AddToStoredLinesDesc(Line1_DE4(), Desc(DE4, 1));
  AddToStoredLinesDesc(Line2_DE4(), Desc(DE4, 2));
} // NativeExpSolver::Export_DE4
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSolver::Line1_DE4 ()
{
  CStr rval;
  const int *itmx, *mxup, *mxlow, *mxbw;

  MfPackage* p = GetGlobal()->GetPackage(Packages::DE4Line1);
  if (!p) return rval;

  if (p->GetField(De4Pack::ITMX, &itmx) && itmx &&
      p->GetField(De4Pack::MXUP, &mxup) && mxup &&
      p->GetField(De4Pack::MXLOW, &mxlow) && mxlow &&
      p->GetField(De4Pack::MXBW, &mxbw) && mxbw)
  {
    rval.Format("%d %d %d %d ", *itmx, *mxup, *mxlow, *mxbw);
  }
  return rval;
} // NativeExpSolver::Line1_DE4
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSolver::Line2_DE4 ()
{
  CStr rval;
  const int *ifreq, *mutd4, *iprd4;
  const Real *accl, *hclose;

  if (GetPackage()->GetField(De4Pack::IFREQ, &ifreq) && ifreq &&
      GetPackage()->GetField(De4Pack::MUTD4, &mutd4) && mutd4 &&
      GetPackage()->GetField(De4Pack::ACCL, &accl) && accl &&
      GetPackage()->GetField(De4Pack::HCLOSE, &hclose) && hclose &&
      GetPackage()->GetField(De4Pack::IPRD4, &iprd4) && iprd4)
  {
    rval.Format("%d %d %s %s %d ", *ifreq, *mutd4, STR(*accl), STR(*hclose),
                                   *iprd4);
  }
  return rval;
} // NativeExpSolver::Line2_DE4
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSolver::Export_SOR ()
{
  AddToStoredLinesDesc(Line1_SOR(), Desc(SOR, 1));
  AddToStoredLinesDesc(Line2_SOR(), Desc(SOR, 2));
} // NativeExpSolver::Export_SOR
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSolver::Line1_SOR ()
{
  CStr rval;
  const int *mxiter;
  if (GetPackage()->GetField(SorPack::MXITER, &mxiter) && mxiter)
  {
    rval.Format("%d ", *mxiter);
  }
  return rval;
} // NativeExpSolver::Line1_SOR
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSolver::Line2_SOR ()
{
  CStr rval;
  const int *iprsor;
  const Real *accl, *hclose;
  if (GetPackage()->GetField(SorPack::ACCL, &accl) && accl &&
      GetPackage()->GetField(SorPack::HCLOSE, &hclose) && hclose &&
      GetPackage()->GetField(SorPack::IPRSOR, &iprsor) && iprsor)
  {
    rval.Format("%s %s %d ", STR(*accl), STR(*hclose), *iprsor);
  }
  return rval;
} // NativeExpSolver::Line2_SOR
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSolver::Export_PCG ()
{
  AddToStoredLinesDesc(Line1_PCG(), Desc(PCG, 1));
  AddToStoredLinesDesc(Line2_PCG(), Desc(PCG, 2));
} // NativeExpSolver::Export_PCG
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSolver::Line1_PCG ()
{
  CStr rval;
  const int *mxiter(0), *iter1(0), *npcond(0), *ihcofadd(0);
  // For damp MODFLOW bumps 0.0 up to 1.0.  Only damping factors > 0.0 or < 1.0
  // cause damping to be applied.
  if (GetPackage()->GetField(PcgPack::MXITER, &mxiter) && mxiter &&
      GetPackage()->GetField(PcgPack::ITER1, &iter1) && iter1 &&
      GetPackage()->GetField(PcgPack::NPCOND, &npcond) && npcond)
  {
    rval.Format("%d %d %d ", *mxiter, *iter1, *npcond);
    if (GetPackage()->GetField("IHCOFADD", &ihcofadd) && ihcofadd)
    {
      std::stringstream ss;
      ss << *ihcofadd << " ";
      rval += ss.str();
    }
  }
  return rval;
} // NativeExpSolver::Line1_PCG
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSolver::Line2_PCG ()
{
  CStr rval;
  const int *nbpol, *iprpcg, *mutpcg;
  const Real *hclose, *rclose, *relax, *damp, *damppcgt;
  // For damp MODFLOW bumps 0.0 up to 1.0.  Only damping factors > 0.0 or < 1.0
  // cause damping to be applied.
  if (GetPackage()->GetField(PcgPack::NBPOL, &nbpol) && nbpol &&
      GetPackage()->GetField(PcgPack::IPRPCG, &iprpcg) && iprpcg &&
      GetPackage()->GetField(PcgPack::MUTPCG, &mutpcg) && mutpcg &&
      GetPackage()->GetField(PcgPack::HCLOSE, &hclose) && hclose &&
      GetPackage()->GetField(PcgPack::RCLOSE, &rclose) && rclose &&
      GetPackage()->GetField(PcgPack::RELAX, &relax) && relax &&
      GetPackage()->GetField(PcgPack::DAMP, &damp) && damp)
  {
    Real tmpDamp = *damp;
    CStr strDamppcgt;
    if (GetPackage()->GetField("DAMPPCGT", &damppcgt) && damppcgt)
    {
      std::stringstream ss;
      ss << STR(*damppcgt) << " ";
      strDamppcgt = ss.str();
      tmpDamp *= -1;
    }
    rval.Format("%s %s %s %d %d %d %s ", STR(*hclose), STR(*rclose), 
      STR(*relax), *nbpol, *iprpcg, *mutpcg, STR(tmpDamp));
    rval += strDamppcgt;
  }
  return rval;
} // NativeExpSolver::Line2_PCG
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSolver::Export_PCGN ()
{
  AddToStoredLinesDesc(Line1_PCGN(), Desc(PCGN, 1));
  AddToStoredLinesDesc(Line2_PCGN(), Desc(PCGN, 2));
  AddToStoredLinesDesc(Line3_PCGN(), Desc(PCGN, 3));
  AddToStoredLinesDesc(Line4_PCGN(), Desc(PCGN, 4));
} // NativeExpSolver::Export_PCGN
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSolver::Line1_PCGN ()
{
  CStr rval;
  const int *iter_mo(NULL), *iter_mi(NULL);
  const Real *close_r(NULL), *close_h(NULL);
  if (GetPackage()->GetField(PcgnPack::ITER_MO, &iter_mo) && iter_mo &&
      GetPackage()->GetField(PcgnPack::ITER_MI, &iter_mi) && iter_mi &&
      GetPackage()->GetField(PcgnPack::CLOSE_R, &close_r) && close_r &&
      GetPackage()->GetField(PcgnPack::CLOSE_H, &close_h) && close_h)
  {
    // Line 1: ITER_MO, ITER_MI, CLOSE_R, CLOSE_H
    rval.Format("%d %d %s %s ", *iter_mo, *iter_mi, STR(*close_r), STR(*close_h));
  }
  return rval;
} // NativeExpSolver::Line1_PCGN
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSolver::Line2_PCGN ()
{
  CStr rval;
  const int *ifill(NULL), *unit_pc(NULL), *unit_ts(NULL);
  const Real *relax(NULL);
  if (GetPackage()->GetField(PcgnPack::RELAX, &relax) && relax &&
      GetPackage()->GetField(PcgnPack::IFILL, &ifill) && ifill &&
      GetPackage()->GetField(PcgnPack::UNIT_PC, &unit_pc) && unit_pc &&
      GetPackage()->GetField(PcgnPack::UNIT_TS, &unit_ts) && unit_ts)
  {
    // Line 2: RELAX, IFILL, UNIT_PC, UNIT_TS
    rval.Format("%s %d %d %d ", STR(*relax), *ifill, *unit_pc, *unit_ts);
  }
  return rval;
} // NativeExpSolver::Line2_PCGN
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSolver::Line3_PCGN ()
{
  CStr rval;
  const int *iter_mo(NULL), *adamp(NULL);
  const Real *damp(NULL), *damp_lb(NULL), *rate_d(NULL), *chglimit(NULL);
  if (GetPackage()->GetField(PcgnPack::ITER_MO, &iter_mo) && iter_mo &&
      GetPackage()->GetField(PcgnPack::ADAMP, &adamp) && adamp &&
      GetPackage()->GetField(PcgnPack::DAMP, &damp) && damp &&
      GetPackage()->GetField(PcgnPack::DAMP_LB, &damp_lb) && damp_lb &&
      GetPackage()->GetField(PcgnPack::RATE_D, &rate_d) && rate_d &&
      GetPackage()->GetField(PcgnPack::CHGLIMIT, &chglimit) && chglimit)
  {
    if (*iter_mo > 1)
    {
      // Line 3: ADAMP, DAMP, DAMP_LB, RATE_D, CHGLIMIT
      rval.Format("%d %s %s %s %s ", *adamp, STR(*damp), STR(*damp_lb),
                                    STR(*rate_d), STR(*chglimit));
    }
  }
  return rval;
} // NativeExpSolver::Line3_PCGN
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSolver::Line4_PCGN ()
{
  CStr rval;
  const int *iter_mo(NULL),*acnvg(NULL), *mcnvg(NULL), *ipunit(NULL);
  const Real *cnvg_lb(NULL), *rate_c(NULL);
  if (GetPackage()->GetField(PcgnPack::ITER_MO, &iter_mo) && iter_mo &&
      GetPackage()->GetField(PcgnPack::ACNVG, &acnvg) && acnvg &&
      GetPackage()->GetField(PcgnPack::CNVG_LB, &cnvg_lb) && cnvg_lb &&
      GetPackage()->GetField(PcgnPack::MCNVG, &mcnvg) && mcnvg &&
      GetPackage()->GetField(PcgnPack::RATE_C, &rate_c) && rate_c &&
      GetPackage()->GetField(PcgnPack::IPUNIT, &ipunit) && ipunit)
  {
    if (*iter_mo > 1)
    {
      // Line 4: ACNVG, CNVG_LB, MCNVG, RATE_C, IPUNIT
      rval.Format("%d %s %d %s %d ", *acnvg, STR(*cnvg_lb), *mcnvg, STR(*rate_c),
                                    *ipunit);
    }
  }
  return rval;
} // NativeExpSolver::Line4_PCGN
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSolver::Export_LMG ()
{
  AddToStoredLinesDesc(Line1_LMG(), Desc(LMG, 1));
  AddToStoredLinesDesc(Line2_LMG(), Desc(LMG, 2));
  if (WriteLine3_LMG()) AddToStoredLinesDesc(Line3_LMG(), Desc(LMG, 3));
  AddToStoredLinesDesc(Line4_LMG(), Desc(LMG, 4));
} // NativeExpSolver::Export_LMG
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpSolver::WriteLine3_LMG ()
{
  const Real *damp(0);
  if (GetPackage()->GetField(LmgPack::DAMP, &damp) && damp)
  {
    if (-2.0 == *damp) return true;
  }
  return false;
} // NativeExpSolver::WriteLine3_LMG
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSolver::Line1_LMG ()
{
  CStr rval;
  const int *icg;
  const Real *stor1, *stor2, *stor3;
  if (GetPackage()->GetField(LmgPack::ICG, &icg) && icg &&
      GetPackage()->GetField(LmgPack::STOR1, &stor1) && stor1 &&
      GetPackage()->GetField(LmgPack::STOR2, &stor2) && stor2 &&
      GetPackage()->GetField(LmgPack::STOR3, &stor3) && stor3)
  {
    rval.Format("%s %s %s %d ", STR(*stor1), STR(*stor2), STR(*stor3), *icg);
  }
  return rval;
} // NativeExpSolver::Line1_LMG
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSolver::Line2_LMG ()
{
  CStr rval;
  const int  *mxiter, *mxcyc, *ioutamg;
  const Real *bclose, *damp;
  if (GetPackage()->GetField(LmgPack::MXITER, &mxiter) && mxiter &&
      GetPackage()->GetField(LmgPack::MXCYC, &mxcyc) && mxcyc &&
      GetPackage()->GetField(LmgPack::IOUTAMG, &ioutamg) && ioutamg &&
      GetPackage()->GetField(LmgPack::BCLOSE, &bclose) && bclose &&
      GetPackage()->GetField(LmgPack::DAMP, &damp) && damp)
  {
    rval.Format("%d %d %s %s %d ", *mxiter, *mxcyc, STR(*bclose), STR(*damp),
                                   *ioutamg);
  }
  return rval;
} // NativeExpSolver::Line2_LMG
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSolver::Line3_LMG ()
{
  CStr rval;
  const Real *dup, *dlow;
  if (GetPackage()->GetField(LmgPack::DUP, &dup) && dup &&
      GetPackage()->GetField(LmgPack::DLOW, &dlow) && dlow)
  {
    rval.Format("%s %s ", STR(*dup), STR(*dlow));
  }
  return rval;
} // NativeExpSolver::Line3_LMG
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSolver::Line4_LMG ()
{
  CStr rval;
  const int *control;
  const Real *hclose;
  if (GetPackage()->GetField(LmgPack::HCLOSE, &hclose) && hclose &&
      GetPackage()->GetField(LmgPack::CONTROL, &control) && control)
  {
    rval.Format("%s %d ", STR(*hclose), *control);
  }
  return rval;
} // NativeExpSolver::Line4_LMG
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSolver::Export_GMG ()
{
  AddToStoredLinesDesc(Line1_GMG(), Desc(GMG, 1));
  AddToStoredLinesDesc(Line2_GMG(), Desc(GMG, 2));
  AddToStoredLinesDesc(Line3_GMG(), Desc(GMG, 3));
  AddToStoredLinesDesc(Line4_GMG(), Desc(GMG, 4));
} // NativeExpSolver::Export_GMG
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSolver::Line1_GMG ()
{
  CStr rval;
  const int *iiter, *mxiter;
  const Real *rclose, *hclose;
  if (GetPackage()->GetField(GmgPack::RCLOSE, &rclose) && rclose &&
      GetPackage()->GetField(GmgPack::IITER, &iiter) && iiter &&
      GetPackage()->GetField(GmgPack::HCLOSE, &hclose) && hclose &&
      GetPackage()->GetField(GmgPack::MXITER, &mxiter) && mxiter)
  {
    rval.Format("%s %d %s %d ", STR(*rclose), *iiter, STR(*hclose), *mxiter);
  }
  return rval;
} // NativeExpSolver::Line1_GMG
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSolver::Line2_GMG ()
{
  CStr rval;
  const int *iadamp, *ioutgmg;
  const Real *damp;
  if (GetPackage()->GetField(GmgPack::DAMP, &damp) && damp &&
      GetPackage()->GetField(GmgPack::IADAMP, &iadamp) && iadamp &&
      GetPackage()->GetField(GmgPack::IOUTGMG, &ioutgmg) && ioutgmg)
  {
    rval.Format("%s %d %d ", STR(*damp), *iadamp, *ioutgmg);
  }
  return rval;
} // NativeExpSolver::Line2_GMG
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSolver::Line3_GMG ()
{
  CStr rval;
  const int *ism, *isc;
  if (GetPackage()->GetField(GmgPack::ISM, &ism) && ism &&
      GetPackage()->GetField(GmgPack::ISC, &isc) && isc)
  {
    rval.Format("%d %d ", *ism, *isc);
  }
  return rval;
} // NativeExpSolver::Line3_GMG
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSolver::Line4_GMG ()
{
  CStr rval;
  const double *relax;
  if (GetPackage()->GetField(GmgPack::RELAX, &relax) && relax)
  {
    rval.Format("%s ", STR(*relax));
  }
  return rval;
} // NativeExpSolver::Line4_GMG
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSolver::Export_NWT ()
{
  AddToStoredLinesDesc(Line1_NWT(), Desc(NWT, 1));
  int whichLine2 = WhichLine2_NWT();
  if (2 == whichLine2) AddToStoredLinesDesc(Line2a_NWT(), Desc(NWT, 2));
  if (3 == whichLine2) AddToStoredLinesDesc(Line2b_NWT(), Desc(NWT, 3));
} // NativeExpSolver::Export_NWT
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSolver::Line1_NWT ()
{
  CStr rval;
  const Real* toldum(0),* ftoldum(0),* Thickdum(0),* thetadum(0),
    * akappadum(0),* gammadum(0),* amomentdum(0),* Btoldum(0),
    * Breducdum(0);
  const int* Mxiter(0),* Linmeth(0),* IPRNWT(0),* IBOTAV(0),* IFDPARAM(0),
    * Btrack(0),* Numtrack(0), * ICNVGFLG(0);

  MfPackage* p = GetPackage();
  if (p->GetField(NWTpack::toldum, &toldum) && toldum &&
      p->GetField(NWTpack::ftoldum, &ftoldum) && ftoldum &&
      p->GetField(NWTpack::Mxiter, &Mxiter) && Mxiter &&
      p->GetField(NWTpack::Thickdum, &Thickdum) && Thickdum &&
      p->GetField(NWTpack::Linmeth, &Linmeth) && Linmeth &&
      p->GetField(NWTpack::IPRNWT, &IPRNWT) && IPRNWT &&
      p->GetField(NWTpack::IBOTAV, &IBOTAV) && IBOTAV &&
      p->GetField(NWTpack::IFDPARAM, &IFDPARAM) && IFDPARAM &&
      p->GetField(NWTpack::ICNVGFLG, &ICNVGFLG) && ICNVGFLG)
  {
    CStr opt[4] = {"SIMPLE", "MODERATE", "COMPLEX", "SPECIFIED"};

    rval.Format("%s %s %d %s %d %d %d ",
                STR(*toldum), STR(*ftoldum), *Mxiter, STR(*Thickdum), *Linmeth,
                *IPRNWT, *IBOTAV);

    int var(*IFDPARAM - 1);
    if (var < 0 || var > 3)
    {
      ASSERT(0);
      var = 0;
    }
    rval += opt[var];
    if (1 == *ICNVGFLG) rval += " CONTINUE";
    rval += " ";
    if (var == 3)
    {
      if (p->GetField(NWTpack::thetadum, &thetadum) && thetadum &&
          p->GetField(NWTpack::akappadum, &akappadum) && akappadum &&
          p->GetField(NWTpack::gammadum, &gammadum) && gammadum &&
          p->GetField(NWTpack::amomentdum, &amomentdum) && amomentdum &&
          p->GetField(NWTpack::Btrack, &Btrack) && Btrack &&
          p->GetField(NWTpack::Numtrack, &Numtrack) && Numtrack &&
          p->GetField(NWTpack::Btoldum, &Btoldum) && Btoldum &&
          p->GetField(NWTpack::Breducdum, &Breducdum) && Breducdum)
      {
        CStr line2;
        line2.Format("%s %s %s %s %d %d %s %s ",
        STR(*thetadum), STR(*akappadum), STR(*gammadum),
        STR(*amomentdum), *Btrack, *Numtrack,
        STR(*Btoldum), STR(*Breducdum));
        rval += line2;
      }
    }
  }
  return rval;
} // NativeExpSolver::Line1_NWT
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
int NativeExpSolver::WhichLine2_NWT ()
{
  int rval(0);
  const int* Linmeth(0),* IFDPARAM(0);

  MfPackage* p = GetPackage();
  if (p->GetField(NWTpack::IFDPARAM, &IFDPARAM) && IFDPARAM &&
      p->GetField(NWTpack::Linmeth, &Linmeth) && Linmeth)
  {
    int var(*IFDPARAM - 1);
    if (var < 0 || var > 3) var = 0;
    if (3 == var)
    {
      if (1 == *Linmeth) return 2;
      else if (2 == *Linmeth) return 3;
    }
  }
  return rval;
} // NativeExpSolver::WhichLine2_NWT
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSolver::Line2a_NWT ()
{
  CStr rval;
  const Real* Stop_toldum(0);
  const int* Maxitr_gmres(0),* Ilu_method(0),* Lev_fill(0),* Msdr(0);
  MfPackage* p = GetPackage();
  if (p->GetField(NWTpack::Maxitr_gmres, &Maxitr_gmres) && Maxitr_gmres &&
      p->GetField(NWTpack::Ilu_method, &Ilu_method) && Ilu_method &&
      p->GetField(NWTpack::Lev_fill, &Lev_fill) && Lev_fill &&
      p->GetField(NWTpack::Stop_toldum, &Stop_toldum) && Stop_toldum &&
      p->GetField(NWTpack::Msdr, &Msdr) && Msdr)
  {
    rval.Format("%d %d %d %s %d ",
                *Maxitr_gmres, *Ilu_method, *Lev_fill, STR(*Stop_toldum),
                *Msdr);
  }
  return rval;
} // NativeExpSolver::Line2a_NWT
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSolver::Line2b_NWT ()
{
  CStr rval;
  const Real* RRCTOLS(0),* EPSRNS(0),* HCLOSEXMDDUM(0);
  const int* IACL(0),* NORDER(0),* LEVEL(0),
    * NORTH(0),* IREDSYS(0),* IDROPTOL(0),* MXITERXMD(0);

  MfPackage* p = GetPackage();
  if (p->GetField(NWTpack::IACL, &IACL) && IACL &&
      p->GetField(NWTpack::NORDER, &NORDER) && NORDER &&
      p->GetField(NWTpack::LEVEL, &LEVEL) && LEVEL &&
      p->GetField(NWTpack::NORTH, &NORTH) && NORTH &&
      p->GetField(NWTpack::IREDSYS, &IREDSYS) && IREDSYS &&
      p->GetField(NWTpack::RRCTOLS, &RRCTOLS) && RRCTOLS &&
      p->GetField(NWTpack::IDROPTOL, &IDROPTOL) && IDROPTOL &&
      p->GetField(NWTpack::EPSRNS, &EPSRNS) && EPSRNS &&
      p->GetField(NWTpack::HCLOSEXMDDUM, &HCLOSEXMDDUM) && HCLOSEXMDDUM &&
      p->GetField(NWTpack::MXITERXMD, &MXITERXMD) && MXITERXMD)
  {
    rval.Format("%d %d %d %d %d %s %d %s %s %d ",
                *IACL, *NORDER, *LEVEL, *NORTH, *IREDSYS,
                STR(*RRCTOLS), *IDROPTOL, STR(*EPSRNS),
                STR(*HCLOSEXMDDUM), *MXITERXMD);
  }
  return rval;
} // NativeExpSolver::Line2b_NWT
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSolver::Export_SMS ()
{
  if (WriteLine1a_SMS()) {
    AddToStoredLinesDesc(Line1a_SMS(), Desc(SMS, 1));
  }
  AddToStoredLinesDesc(Line1b_SMS(), Desc(SMS, 2));
  if (WriteLine2_SMS()) {
    AddToStoredLinesDesc(Line2_SMS(), Desc(SMS, 3));
  }
  if (WriteLine3_SMS()) {
    AddToStoredLinesDesc(Line3_SMS(), Desc(SMS, 4));
  }
  if (WriteLine4_SMS()) {
    AddToStoredLinesDesc(Line4_SMS(), Desc(SMS, 5));
  }
} // NativeExpSolver::Export_SMS
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpSolver::WriteLine1a_SMS ()
{
  const int* IFDPARAM(0);

  MfPackage* p = GetPackage();
  if (p->GetField(SmsPack::IFDPARAM, &IFDPARAM) && IFDPARAM)
  {
    return (*IFDPARAM >= 1 && *IFDPARAM <= 3);
  }
  return true;
} // NativeExpSolver::WriteLine1a_SMS
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpSolver::WriteLine2_SMS ()
{
  bool rval = true;
  const int* NONLINMETH(0),* IFDPARAM(0);

  MfPackage* p = GetPackage();
  if (p->GetField(SmsPack::IFDPARAM, &IFDPARAM) && IFDPARAM &&
      p->GetField(SmsPack::NONLINMETH, &NONLINMETH) && NONLINMETH)
  {
    rval = (*NONLINMETH != 0 && (*IFDPARAM < 1 || *IFDPARAM > 3));
  }
  return rval;
} // NativeExpSolver::WriteLine2_SMS
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpSolver::WriteLine3_SMS ()
{
  bool rval = true;
  const int* LINMETH(0),* IFDPARAM(0);

  MfPackage* p = GetPackage();
  if (p->GetField(SmsPack::IFDPARAM, &IFDPARAM) && IFDPARAM &&
      p->GetField(SmsPack::LINMETH, &LINMETH) && LINMETH)
  {
    rval = (*LINMETH == 1 && (*IFDPARAM < 1 || *IFDPARAM > 3));
  }
  return rval;
} // NativeExpSolver::WriteLine3_SMS
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpSolver::WriteLine4_SMS ()
{
  bool rval = true;
  const int* LINMETH(0)/*,* IFDPARAM(0)*/;

  MfPackage* p = GetPackage();
  if (//p->GetField(SmsPack::IFDPARAM, &IFDPARAM) && IFDPARAM &&
      p->GetField(SmsPack::LINMETH, &LINMETH) && LINMETH)
  {
    // this line follows the documentation but the documentation is not
    // followed by USG. If LINMETH=2 then we should write this line.
    //rval = (*LINMETH == 2 && (*IFDPARAM < 1 || *IFDPARAM > 3));
    rval = (*LINMETH == 2);
  }
  return rval;
} // NativeExpSolver::WriteLine4_SMS
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSolver::Line1a_SMS ()
{
  CStr rval;
  const int* IFDPARAM(0);

  MfPackage* p = GetPackage();
  if (p->GetField(SmsPack::IFDPARAM, &IFDPARAM) && IFDPARAM)
  {
    CStr OPTIONS;
    switch (*IFDPARAM) {
    case 1:
      OPTIONS = "SIMPLE";
      break;
    case 2:
      OPTIONS = "MODERATE";
      break;
    case 3:
      OPTIONS = "COMPLEX";
      break;
    default:
      OPTIONS = "";
      break;
    }
    if (!OPTIONS.empty()) {
      rval.Format("%s ", OPTIONS);
    }
  }
  return rval;
} // NativeExpSolver::Line1a_SMS
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSolver::Line1b_SMS ()
{
  CStr rval;
  const double* HCLOSE(0),* HICLOSE(0);
  const int* MXITER(0),* ITER1(0),* IPRSMS(0),
           * NONLINMETH(0),* LINMETH(0);

  MfPackage* p = GetPackage();
  if (p->GetField(SmsPack::HCLOSE, &HCLOSE) && HCLOSE &&
      p->GetField(SmsPack::HICLOSE, &HICLOSE) && HICLOSE &&
      p->GetField(SmsPack::MXITER, &MXITER) && MXITER &&
      p->GetField(SmsPack::ITER1, &ITER1) && ITER1 &&
      p->GetField(SmsPack::IPRSMS, &IPRSMS) && IPRSMS &&
      p->GetField(SmsPack::NONLINMETH, &NONLINMETH) && NONLINMETH &&
      p->GetField(SmsPack::LINMETH, &LINMETH) && LINMETH)
  {
    rval.Format("%s %s %d %d %d %d %d ",
                STR((Real)*HCLOSE), STR((Real)*HICLOSE), *MXITER, *ITER1, *IPRSMS,
                *NONLINMETH, *LINMETH);
  }
  return rval;
} // NativeExpSolver::Line1b_SMS
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSolver::Line2_SMS ()
{
  CStr rval;
  const double* THETA(0),* AKAPPA(0),* GAMMA(0),* AMOMENTUM(0),* BTOL(0),
              * BREDUC(0), * RESLIM(0);
  const int* NUMTRACK(0);

  MfPackage* p = GetPackage();
  if (p->GetField(SmsPack::THETA, &THETA) && THETA &&
      p->GetField(SmsPack::AKAPPA, &AKAPPA) && AKAPPA &&
      p->GetField(SmsPack::GAMMA, &GAMMA) && GAMMA &&
      p->GetField(SmsPack::AMOMENTUM, &AMOMENTUM) && AMOMENTUM &&
      p->GetField(SmsPack::NUMTRACK, &NUMTRACK) && NUMTRACK &&
      p->GetField(SmsPack::BTOL, &BTOL) && BTOL &&
      p->GetField(SmsPack::BREDUC, &BREDUC) && BREDUC &&
      p->GetField(SmsPack::RESLIM, &RESLIM) && RESLIM)
  {
    rval.Format("%s %s %s %s %d %s %s %s ",
                STR(*THETA), STR(*AKAPPA), STR(*GAMMA), STR(*AMOMENTUM),
                *NUMTRACK, STR(*BTOL), STR(*BREDUC), STR(*RESLIM));
  }
  return rval;
} // NativeExpSolver::Line2_SMS
//------------------------------------------------------------------------------
/// \brief xMD stuff.
//------------------------------------------------------------------------------
CStr NativeExpSolver::Line3_SMS ()
{
  CStr rval;
  const double* RRCTOL(0),* EPSRN(0);
  const int* IACL(0),* NORDER(0),* LEVEL(0),* NORTH(0),* IREDSYS(0),* IDROPTOL(0);

  MfPackage* p = GetPackage();
  if (p->GetField(SmsPack::IACL, &IACL) && IACL &&
      p->GetField(SmsPack::NORDER, &NORDER) && NORDER &&
      p->GetField(SmsPack::LEVEL, &LEVEL) && LEVEL &&
      p->GetField(SmsPack::NORTH, &NORTH) && NORTH &&
      p->GetField(SmsPack::IREDSYS, &IREDSYS) && IREDSYS &&
      p->GetField(SmsPack::RRCTOL, &RRCTOL) && RRCTOL &&
      p->GetField(SmsPack::IDROPTOL, &IDROPTOL) && IDROPTOL &&
      p->GetField(SmsPack::EPSRN, &EPSRN) && EPSRN)
  {
    rval.Format("%d %d %d %d %d %s %d %s ",
                *IACL, *NORDER, *LEVEL, *NORTH, *IREDSYS, STR(*RRCTOL),
                *IDROPTOL, STR(*EPSRN));
  }
  return rval;
} // NativeExpSolver::Line3_SMS
//------------------------------------------------------------------------------
/// \brief PCGU stuff.
//------------------------------------------------------------------------------
CStr NativeExpSolver::Line4_SMS ()
{
  CStr rval, flag, strRELAXPCGU;
  const Real* RCLOSEPCGU(0),* RELAXPCGU(0);
  const int* IPC(0),* ISCL(0),* IORD(0), * IFLAG(0);

  MfPackage* p = GetPackage();
  p->GetField(SmsPack::RELAXPCGU, &RELAXPCGU);
  if (p->GetField(SmsPack::IPC, &IPC) && IPC &&
      p->GetField(SmsPack::ISCL, &ISCL) && ISCL &&
      p->GetField(SmsPack::IORD, &IORD) && IORD &&
      p->GetField(SmsPack::IFLAG, &IFLAG) && IFLAG &&
      p->GetField(SmsPack::RCLOSEPCGU, &RCLOSEPCGU) && RCLOSEPCGU)
  {
    if (1 == *IFLAG) flag = "CG ";
    else if (2 == *IFLAG)
    {
      flag = "BCGS ";
      if (RELAXPCGU) strRELAXPCGU = STR(*RELAXPCGU);
    }
    rval.Format("%s%d %d %d %s %s",
                flag, *IPC, *ISCL, *IORD, STR(*RCLOSEPCGU), strRELAXPCGU);
  }
  return rval;
} // NativeExpSolver::Line4_SMS
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSolver::Export_IMS ()
{
 if (GetPackage()->PackageName() == "SMS")
 {
   AddToStoredLinesDesc("BEGIN OPTIONS","");
   AddToStoredLinesDesc(GetPrintOption(),"");
   CStr complexity = Line1a_SMS();
   if (!complexity.empty()) AddToStoredLinesDesc("  COMPLEXITY " + complexity,"");
   AddToStoredLinesDesc("END OPTIONS","");
   AddToStoredLinesDesc("","");

   AddToStoredLinesDesc("BEGIN NONLINEAR","");
   AddToStoredLinesDesc(GetImsNonlinearLine(),"");
   AddToStoredLinesDesc("END NONLINEAR","");
   AddToStoredLinesDesc("","");

   AddToStoredLinesDesc("BEGIN LINEAR","");
   AddToStoredLinesDesc(GetImsLinearLine(),"");
   AddToStoredLinesDesc("END LINEAR","");
 }
 else
 {
   AddToStoredLinesDesc("BEGIN OPTIONS","");
   AddToStoredLinesDesc(" PRINT_OPTION SUMMARY","");
   AddToStoredLinesDesc(" COMPLEXITY MODERATE","");
   AddToStoredLinesDesc("END OPTIONS","");
 }  
 
} // NativeExpSolver::Export_IMS
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSolver::GetPrintOption()
{ 
  std::stringstream ss;
  const int* IPRSMS(0);
  MfPackage* p = GetPackage();
  if (p->GetField(SmsPack::IPRSMS, &IPRSMS) && IPRSMS)
  {
    ss << "  PRINT_OPTION";
    if(*IPRSMS==0)      ss << " NONE";
    else if(*IPRSMS==1) ss << " SUMMARY";
    else                ss << " ALL";
  }
  return ss.str();
} //NativeExpSolver::GetPrintOption
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSolver::GetImsNonlinearLine ()
{
  std::stringstream ss;
  const double* HCLOSE(0);
  const int* MXITER(0),* NONLINMETH(0);

  MfPackage* p = GetPackage();
  if (p->GetField(SmsPack::HCLOSE, &HCLOSE) && HCLOSE &&    
      p->GetField(SmsPack::MXITER, &MXITER) && MXITER &&     
      p->GetField(SmsPack::NONLINMETH, &NONLINMETH) && NONLINMETH)
  {
    ss << "  OUTER_HCLOSE " << STR(*HCLOSE) << "\n";
    ss << "  OUTER_MAXIMUM " << *MXITER << "\n";
    ss << "  UNDER_RELAXATION ";
    if(*NONLINMETH == 1) ss << " DBD\n";
    else                 ss << " Cooley\n";
  }

  const double*  RESLIM(0);
  const int* NUMTRACK(0);
  const Real* thetadum(0),* akappadum(0),* gammadum(0),* amomentdum(0),* Btoldum(0),
            * Breducdum(0);
  if (p->GetField(NWTpack::thetadum, &thetadum) && thetadum)
    ss << "  UNDER_RELAXATION_THETA " << STR(*thetadum) <<  "\n";
  if (p->GetField(NWTpack::akappadum, &akappadum) && akappadum)
    ss << "  UNDER_RELAXATION_KAPPA " << STR(*akappadum) <<  "\n";
  if (p->GetField(NWTpack::gammadum, &gammadum) && gammadum)
    ss << "  UNDER_RELAXATION_GAMMA " << STR(*gammadum) <<  "\n";
  if (p->GetField(NWTpack::amomentdum, &amomentdum) && amomentdum)
    ss << "  UNDER_RELAXATION_MOMENTUM " << STR(*amomentdum) << "\n";
  if (p->GetField(SmsPack::NUMTRACK, &NUMTRACK) && NUMTRACK)
    ss << "  BACKTRACKING_NUMBER " << *NUMTRACK << "\n";
  if (p->GetField(NWTpack::Btoldum, &Btoldum) && Btoldum)
    ss << "  BACKTRACKING_TOLERANCE " << STR(*Btoldum) << "\n";
  if (p->GetField(NWTpack::Breducdum, &Breducdum) && Breducdum)
    ss << "  BACKTRACKING_REDUCTION_FACTOR " << STR(*Breducdum) << "\n";
  if (p->GetField(SmsPack::RESLIM, &RESLIM) && RESLIM)
    ss << "  BACKTRACKING_RESIDUAL_LIMIT " << STR(*RESLIM) << "\n";
  return ss.str();
}//NativeExpSolver::GetImsNonlinearLine
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpSolver::GetImsLinearLine ()
{
  std::stringstream ss;
  const double* HICLOSE(0);
  const int* ITER1(0),* ICLIN(0);
  const int* ISCL(0),* IORD(0),* LEVEL(0),* NORTH(0);
  const Real* RCLOSEPCGU(0),* CLIN(0),* RELAXPCGU(0);
  const double* EPSRN(0);

  MfPackage* p = GetPackage();
  if (p->GetField(SmsPack::ITER1, &ITER1) && ITER1)
    ss << "  INNER_MAXIMUM " << *ITER1 << "\n";
  if (p->GetField(SmsPack::HICLOSE, &HICLOSE) && HICLOSE)
    ss << "  INNER_HCLOSE " << STR(*HICLOSE) << "\n";
  if (p->GetField(SmsPack::RCLOSEPCGU, &RCLOSEPCGU) && RCLOSEPCGU)
    ss << "  INNER_RCLOSE " << STR(*RCLOSEPCGU) << "\n";

  CStr strCLIN("CG");
  if (p->GetField(SmsPack::IFLAG, &ICLIN) && CLIN)
  {
    ss << "  LINEAR_ACCELERATION ";
    if (1 == *CLIN) strCLIN = "CG";
    else if (2 == *CLIN) strCLIN = "BICGSTAB";
  }
  ss << "  LINEAR_ACCELERATION " << strCLIN << "\n";

  if (p->GetField(SmsPack::RELAXPCGU, &RELAXPCGU) && RELAXPCGU)
    ss << "  RELAXATION_FACTOR " << STR(*RELAXPCGU) << "\n";
  if (p->GetField(SmsPack::LEVEL, &LEVEL) && LEVEL)
    ss << "  PRECONDITIONER_LEVELS " << *LEVEL << "\n";
  if (p->GetField(SmsPack::EPSRN, &EPSRN) && EPSRN)
    ss << "  PRECONDITIONER_DROP_TOLERANCE " << STR(*EPSRN) << "\n";
  if (p->GetField(SmsPack::NORTH, &NORTH) && NORTH)
    ss << "  NUMBER_ORTHOGONALIZATIONS " << *NORTH << "\n";
  if (p->GetField(SmsPack::ISCL, &ISCL) && ISCL && *ISCL > -1 && *ISCL < 3)
  {
    CStr strs[3] = {"NONE", "DIAGONAL", "L2NORM"};
    ss << "  SCALING_METHOD " << strs[*ISCL] << "\n";
  }
  if (p->GetField(SmsPack::IORD, &IORD) && IORD && *IORD > -1 && *IORD < 3)
  {
    CStr strs[3] = {"NONE", "RCM", "MD"};
    ss << "  REORDERING_METHOD " << strs[*IORD] << "\n";
  }
  return ss.str();
} // NativeExpSolver::GetImsLinearLine


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private/MfData/MfExport/private/Native/NativeExpSolver.t.h>

#include <private/MfData/MfExport/private/Mf2kNative.h>
#include <private/MfData/MfExport/private/Native/NativeUtil.h>
#include <private/MfData/MfGlobal.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackStrings.h>
#include <private/MfLibAsserts.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpSolverT::setUp ()
{
  m_p = NULL;
  Mf2kNative* n = new Mf2kNative;
  MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  MfPackage* dis = new MfPackage(SIP);
  NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  m_p = dynamic_cast<NativeExpSolver*>(p);
} // NativeExpSolverT::setUp
//------------------------------------------------------------------------------
void NativeExpSolverT::tearDown ()
{
  m_p->UnitTestingDeletePointers();
  delete(m_p);
} // NativeExpSolverT::tearDown
//------------------------------------------------------------------------------
void NativeExpSolverT::testCreateClass ()
{
  TS_ASSERT(m_p);
} // NativeExpSolverT::testCreateClass
//------------------------------------------------------------------------------
void NativeExpSolverT::testDesc ()
{
  CStr base = " 1. MXITER NPARM";
  CStr str = m_p->Desc(SIP, 1);
  TS_ASSERT_EQUALS2(base, str);
  base = " 2. ACCL HCLOSE IPCALC WSEED IPRSIP";
  str = m_p->Desc(SIP, 2);
  TS_ASSERT_EQUALS2(base, str);

  base = " 1. ITMX MXUP MXLOW MXBW";
  str = m_p->Desc(DE4, 1);
  TS_ASSERT_EQUALS2(base, str);
  base = " 2. IFREQ MUTD4 ACCL HCLOSE IPRD4";
  str = m_p->Desc(DE4, 2);
  TS_ASSERT_EQUALS2(base, str);

  base = " 1. MXITER";
  str = m_p->Desc(SOR, 1);
  TS_ASSERT_EQUALS2(base, str);
  base = " 2. ACCL HCLOSE IPRSOR";
  str = m_p->Desc(SOR, 2);
  TS_ASSERT_EQUALS2(base, str);

  base = " 1. MXITER ITER1 NPCOND [IHCOFADD]";
  str = m_p->Desc(PCG, 1);
  TS_ASSERT_EQUALS2(base, str);
  base = " 2. HCLOSE RCLOSE RELAX NBPOL IPRPCG MUTPCG DAMPPCG [DAMPPCGT]";
  str = m_p->Desc(PCG, 2);
  TS_ASSERT_EQUALS2(base, str);

  base = " 1. ITER_MO, ITER_MI, CLOSE_R, CLOSE_H";
  str = m_p->Desc(PCGN, 1);
  TS_ASSERT_EQUALS2(base, str);
  base = " 2. RELAX, IFILL, UNIT_PC, UNIT_TS";
  str = m_p->Desc(PCGN, 2);
  TS_ASSERT_EQUALS2(base, str);
  base = " 3. ADAMP, DAMP, DAMP_LB, RATE_D, CHGLIMIT";
  str = m_p->Desc(PCGN, 3);
  TS_ASSERT_EQUALS2(base, str);
  base = " 4. ACNVG, CNVG_LB, MCNVG, RATE_C, IPUNIT";
  str = m_p->Desc(PCGN, 4);
  TS_ASSERT_EQUALS2(base, str);

  base = " 1. STOR1 STOR2 STOR3 ICG";
  str = m_p->Desc(LMG, 1);
  TS_ASSERT_EQUALS2(base, str);
  base = " 2. MXITER MXCYC B/RCLOSE DAMP IOUTAMG";
  str = m_p->Desc(LMG, 2);
  TS_ASSERT_EQUALS2(base, str);
  base = " 3. DUP DLOW";
  str = m_p->Desc(LMG, 3);
  TS_ASSERT_EQUALS2(base, str);
  base = " 4. HCLOSE CONTROL";
  str = m_p->Desc(LMG, 4);
  TS_ASSERT_EQUALS2(base, str);

  base = " 1. RCLOSE IITER HCLOSE MXITER";
  str = m_p->Desc(GMG, 1);
  TS_ASSERT_EQUALS2(base, str);
  base = " 2. DAMP IADAMP IOUTGMG [IUNITMHC]";
  str = m_p->Desc(GMG, 2);
  TS_ASSERT_EQUALS2(base, str);
  base = " 3. ISM ISC [DUP DLOW CHGLIMIT]";
  str = m_p->Desc(GMG, 3);
  TS_ASSERT_EQUALS2(base, str);
  base = " 4. RELAX";
  str = m_p->Desc(GMG, 4);
  TS_ASSERT_EQUALS2(base, str);

  base = " 1. HEADTOL FLUXTOL MAXITEROUT THICKFACT LINMETH IPRNWT "
         "IBOTAV OPTIONS [DBDTHETA] [DBDKAPPA] [DBDGAMMA] [MOMFACT] "
         "[BACKFLAG] [MAXBACKITER] [BACKTOL] [BACKREDUCE]";
  str = m_p->Desc(NWT, 1);
  TS_ASSERT_EQUALS2(base, str);
  base = "2a. [MAXITINNER] [ILUMETHOD][LEVFILL][STOPTOL][MSDR]";
  str = m_p->Desc(NWT, 2);
  TS_ASSERT_EQUALS2(base, str);
  base = "2b. [IACL][NORDER][LEVEL][NORTH][IREDSYS][RRCTOLS]"
         "[IDROPTOL][EPSRN] [HCLOSEXMD][MXITERXMD]";
  str = m_p->Desc(NWT, 3);
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpSolverT::testDesc
//------------------------------------------------------------------------------
void NativeExpSolverT::testLine1_SIP ()
{
  int mxiter(5), nparm(3);
  m_p->GetPackage()->SetField(SipPack::MXITER, &mxiter);
  m_p->GetPackage()->SetField(SipPack::NPARM, &nparm);
  CStr base = "5 3 ";
  CStr str = m_p->Line1_SIP();
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpSolverT::testLine1_SIP
//------------------------------------------------------------------------------
void NativeExpSolverT::testLine2_SIP ()
{
  int ipcalc(11), iprsip(13);
  Real accl((Real).9), hclose((Real).01), wseed((Real)21.3);
  m_p->GetPackage()->SetField(SipPack::IPCALC, &ipcalc);
  m_p->GetPackage()->SetField(SipPack::IPRSIP, &iprsip);
  m_p->GetPackage()->SetField(SipPack::ACCL, &accl);
  m_p->GetPackage()->SetField(SipPack::HCLOSE, &hclose);
  m_p->GetPackage()->SetField(SipPack::WSEED, &wseed);
  CStr base = "0.9 0.01 11 21.3 13 ";
  CStr str = m_p->Line2_SIP();
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpSolverT::testLine2_SIP
//------------------------------------------------------------------------------
void NativeExpSolverT::testLine1_DE4 ()
{
  int itmx(11), mxup(12), mxlow(13), mxbw(14);
  MfPackage p(MfData::Packages::DE4Line1);
  p.SetField(De4Pack::ITMX, &itmx);
  p.SetField(De4Pack::MXUP, &mxup);
  p.SetField(De4Pack::MXLOW, &mxlow);
  p.SetField(De4Pack::MXBW, &mxbw);
  m_p->GetGlobal()->AddPackage(&p);
  CStr base = "11 12 13 14 ";
  CStr str = m_p->Line1_DE4();
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpSolverT::testLine1_DE4
//------------------------------------------------------------------------------
void NativeExpSolverT::testLine2_DE4 ()
{
  int  ifreq(21), mutd4(22), iprd4(23);
  Real accl((Real).8), hclose((Real).5);
  m_p->GetPackage()->SetField(De4Pack::IFREQ, &ifreq);
  m_p->GetPackage()->SetField(De4Pack::MUTD4, &mutd4);
  m_p->GetPackage()->SetField(De4Pack::ACCL, &accl);
  m_p->GetPackage()->SetField(De4Pack::HCLOSE, &hclose);
  m_p->GetPackage()->SetField(De4Pack::IPRD4, &iprd4);
  CStr base = "21 22 0.8 0.5 23 ";
  CStr str = m_p->Line2_DE4();
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpSolverT::testLine2_DE4
//------------------------------------------------------------------------------
void NativeExpSolverT::testLine1_SOR ()
{
  int mxiter(7);
  m_p->GetPackage()->SetField(SorPack::MXITER, &mxiter);
  CStr base = "7 ";
  CStr str = m_p->Line1_SOR();
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpSolverT::testLine1_SOR
//------------------------------------------------------------------------------
void NativeExpSolverT::testLine2_SOR ()
{
  int iprsor(5);
  Real accl((Real).75), hclose((Real)3.6);
  m_p->GetPackage()->SetField(SorPack::ACCL, &accl);
  m_p->GetPackage()->SetField(SorPack::HCLOSE, &hclose);
  m_p->GetPackage()->SetField(SorPack::IPRSOR, &iprsor);
  CStr base = "0.75 3.6 5 ";
  CStr str = m_p->Line2_SOR();
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpSolverT::testLine2_SOR
//------------------------------------------------------------------------------
void NativeExpSolverT::testLine1_PCG ()
{
  int mxiter(11), iter1(12), npcond(13), ihcofadd(14);
  m_p->GetPackage()->SetField(PcgPack::MXITER, &mxiter);
  m_p->GetPackage()->SetField(PcgPack::ITER1, &iter1);
  m_p->GetPackage()->SetField(PcgPack::NPCOND, &npcond);
  CStr base = "11 12 13 ";
  CStr str = m_p->Line1_PCG();
  TS_ASSERT_EQUALS2(base, str);
  m_p->GetPackage()->SetField("IHCOFADD", &ihcofadd);
  base += "14 ";
  str = m_p->Line1_PCG();
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpSolverT::testLine1_PCG
//------------------------------------------------------------------------------
void NativeExpSolverT::testLine2_PCG ()
{
  int nbpol(21), iprpcg(22), mutpcg(23);
  Real hclose((Real).75), rclose((Real)1.1), relax((Real).86),
       damp((Real).99), damppcgt((Real).82);
  m_p->GetPackage()->SetField(PcgPack::NBPOL, &nbpol);
  m_p->GetPackage()->SetField(PcgPack::IPRPCG, &iprpcg);
  m_p->GetPackage()->SetField(PcgPack::MUTPCG, &mutpcg);
  m_p->GetPackage()->SetField(PcgPack::HCLOSE, &hclose);
  m_p->GetPackage()->SetField(PcgPack::RCLOSE, &rclose);
  m_p->GetPackage()->SetField(PcgPack::RELAX, &relax);
  m_p->GetPackage()->SetField(PcgPack::DAMP, &damp);
  CStr base = "0.75 1.1 0.86 21 22 23 0.99 ";
  CStr str = m_p->Line2_PCG();
  TS_ASSERT_EQUALS2(base, str);
  m_p->GetPackage()->SetField("DAMPPCGT", &damppcgt);
  base = "0.75 1.1 0.86 21 22 23 -0.99 0.82 ";
  str = m_p->Line2_PCG();
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpSolverT::testLine2_PCG
//------------------------------------------------------------------------------
void NativeExpSolverT::testLine1_PCGN ()
{
  int  iter_mo(5), iter_mi(4);
  Real close_r((Real).634), close_h((Real)1.23);
  m_p->GetPackage()->SetField(PcgnPack::ITER_MO, &iter_mo);
  m_p->GetPackage()->SetField(PcgnPack::ITER_MI, &iter_mi);
  m_p->GetPackage()->SetField(PcgnPack::CLOSE_R, &close_r);
  m_p->GetPackage()->SetField(PcgnPack::CLOSE_H, &close_h);
  CStr base = "5 4 0.634 1.23 ";
  CStr str = m_p->Line1_PCGN();
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpSolverT::testLine1_PCGN
//------------------------------------------------------------------------------
void NativeExpSolverT::testLine2_PCGN ()
{
  int  ifill(6), unit_pc(7), unit_ts(8);
  Real relax((Real)1.23);
  m_p->GetPackage()->SetField(PcgnPack::RELAX, &relax);
  m_p->GetPackage()->SetField(PcgnPack::IFILL, &ifill);
  m_p->GetPackage()->SetField(PcgnPack::UNIT_PC, &unit_pc);
  m_p->GetPackage()->SetField(PcgnPack::UNIT_TS, &unit_ts);
  CStr base = "1.23 6 7 8 ";
  CStr str = m_p->Line2_PCGN();
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpSolverT::testLine2_PCGN
//------------------------------------------------------------------------------
void NativeExpSolverT::testLine3_PCGN ()
{
  int  iter_mo(1), adamp(2);
  Real damp((Real).064), damp_lb((Real)1.054), rate_d((Real)5.678),
       chglimit((Real)1e-7);
  m_p->GetPackage()->SetField(PcgnPack::ITER_MO, &iter_mo);
  m_p->GetPackage()->SetField(PcgnPack::ADAMP, &adamp);
  m_p->GetPackage()->SetField(PcgnPack::DAMP, &damp);
  m_p->GetPackage()->SetField(PcgnPack::DAMP_LB, &damp_lb);
  m_p->GetPackage()->SetField(PcgnPack::RATE_D, &rate_d);
  m_p->GetPackage()->SetField(PcgnPack::CHGLIMIT, &chglimit);
  CStr base = "";
  CStr str = m_p->Line3_PCGN();
  TS_ASSERT_EQUALS2(base, str);
  iter_mo = 2;
  base = "2 0.064 1.054 5.678 1.0e-007 ";
  str = m_p->Line3_PCGN();
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpSolverT::testLine3_PCGN
//------------------------------------------------------------------------------
void NativeExpSolverT::testLine4_PCGN ()
{
  int  iter_mo(1), acnvg(11), mcnvg(12), ipunit(13);
  Real cnvg_lb((Real)1.98), rate_c((Real)5.678);
  m_p->GetPackage()->SetField(PcgnPack::ITER_MO, &iter_mo);
  m_p->GetPackage()->SetField(PcgnPack::ACNVG, &acnvg);
  m_p->GetPackage()->SetField(PcgnPack::CNVG_LB, &cnvg_lb);
  m_p->GetPackage()->SetField(PcgnPack::MCNVG, &mcnvg);
  m_p->GetPackage()->SetField(PcgnPack::RATE_C, &rate_c);
  m_p->GetPackage()->SetField(PcgnPack::IPUNIT, &ipunit);
  CStr base = "";
  CStr str = m_p->Line4_PCGN();
  TS_ASSERT_EQUALS2(base, str);
  iter_mo = 2;
  base = "11 1.98 12 5.678 13 ";
  str = m_p->Line4_PCGN();
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpSolverT::testLine4_PCGN
//------------------------------------------------------------------------------
void NativeExpSolverT::testWriteLine3_LMG ()
{
  Real damp((Real)1);
  m_p->GetPackage()->SetField(LmgPack::DAMP, &damp);
  TS_ASSERT(!m_p->WriteLine3_LMG());
  damp = -2;
  TS_ASSERT(m_p->WriteLine3_LMG());
} // NativeExpSolverT::testWriteLine3_LMG
//------------------------------------------------------------------------------
void NativeExpSolverT::testLine1_LMG ()
{
  int  icg(15);
  Real stor1((Real)2.3), stor2((Real)1.2), stor3((Real)3.6);
  m_p->GetPackage()->SetField(LmgPack::ICG, &icg);
  m_p->GetPackage()->SetField(LmgPack::STOR1, &stor1);
  m_p->GetPackage()->SetField(LmgPack::STOR2, &stor2);
  m_p->GetPackage()->SetField(LmgPack::STOR3, &stor3);
  CStr base = "2.3 1.2 3.6 15 ";
  CStr str = m_p->Line1_LMG();
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpSolverT::testLine1_LMG
//------------------------------------------------------------------------------
void NativeExpSolverT::testLine2_LMG ()
{
  int  mxiter(13), mxcyc(14), ioutamg(15);
  Real bclose((Real)3.4), damp((Real)5.6);
  m_p->GetPackage()->SetField(LmgPack::MXITER, &mxiter);
  m_p->GetPackage()->SetField(LmgPack::MXCYC, &mxcyc);
  m_p->GetPackage()->SetField(LmgPack::IOUTAMG, &ioutamg);
  m_p->GetPackage()->SetField(LmgPack::BCLOSE, &bclose);
  m_p->GetPackage()->SetField(LmgPack::DAMP, &damp);
  CStr base = "13 14 3.4 5.6 15 ";
  CStr str = m_p->Line2_LMG();
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpSolverT::testLine2_LMG
//------------------------------------------------------------------------------
void NativeExpSolverT::testLine3_LMG ()
{
  Real dup((Real)9.8), dlow((Real)7.6);
  m_p->GetPackage()->SetField(LmgPack::DUP, &dup);
  m_p->GetPackage()->SetField(LmgPack::DLOW, &dlow);
  CStr base = "9.8 7.6 ";
  CStr str = m_p->Line3_LMG();
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpSolverT::testLine3_LMG
//------------------------------------------------------------------------------
void NativeExpSolverT::testLine4_LMG ()
{
  int  control(3);
  Real hclose((Real).67);
  m_p->GetPackage()->SetField(LmgPack::HCLOSE, &hclose);
  m_p->GetPackage()->SetField(LmgPack::CONTROL, &control);
  CStr base = "0.67 3 ";
  CStr str = m_p->Line4_LMG();
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpSolverT::testLine4_LMG
//------------------------------------------------------------------------------
void NativeExpSolverT::testLine1_GMG ()
{
  int  iiter(21), mxiter(22);
  Real rclose((Real)1.2), hclose((Real)3.4);
  m_p->GetPackage()->SetField(GmgPack::RCLOSE, &rclose);
  m_p->GetPackage()->SetField(GmgPack::IITER, &iiter);
  m_p->GetPackage()->SetField(GmgPack::HCLOSE, &hclose);
  m_p->GetPackage()->SetField(GmgPack::MXITER, &mxiter);
  CStr base = "1.2 21 3.4 22 ";
  CStr str = m_p->Line1_GMG();
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpSolverT::testLine1_GMG
//------------------------------------------------------------------------------
void NativeExpSolverT::testLine2_GMG ()
{
  int  iadamp(6), ioutgmg(7);
  Real damp((Real)5.6);
  m_p->GetPackage()->SetField(GmgPack::DAMP, &damp);
  m_p->GetPackage()->SetField(GmgPack::IADAMP, &iadamp);
  m_p->GetPackage()->SetField(GmgPack::IOUTGMG, &ioutgmg);
  CStr base = "5.6 6 7 ";
  CStr str = m_p->Line2_GMG();
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpSolverT::testLine2_GMG
//------------------------------------------------------------------------------
void NativeExpSolverT::testLine3_GMG ()
{
  int ism(8), isc(9);
  m_p->GetPackage()->SetField(GmgPack::ISM, &ism);
  m_p->GetPackage()->SetField(GmgPack::ISC, &isc);
  CStr base = "8 9 ";
  CStr str = m_p->Line3_GMG();
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpSolverT::testLine3_GMG
//------------------------------------------------------------------------------
void NativeExpSolverT::testLine4_GMG ()
{
  double relax(1.3);
  m_p->GetPackage()->SetField(GmgPack::RELAX, &relax);
  CStr base = "1.3 ";
  CStr str = m_p->Line4_GMG();
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpSolverT::testLine4_GMG
//------------------------------------------------------------------------------
void NativeExpSolverT::testLine1_NWT ()
{
  Real toldum((Real)1.2), ftoldum((Real)3.4), Thickdum((Real)5.6),
       thetadum((Real)7.8), akappadum((Real)9.1), gammadum((Real)2.3),
       amomentdum((Real)4.5), Btoldum((Real)6.7), Breducdum((Real)8.9);
  int  Mxiter(11), Linmeth(1), IPRNWT(12), IBOTAV(13), IFDPARAM(1),
       Btrack(14), Numtrack(15), ICNVGFLG(0);

  MfPackage* p = m_p->GetPackage();
  p->SetField(NWTpack::toldum, &toldum);
  p->SetField(NWTpack::ftoldum, &ftoldum);
  p->SetField(NWTpack::Mxiter, &Mxiter);
  p->SetField(NWTpack::Thickdum, &Thickdum);
  p->SetField(NWTpack::Linmeth, &Linmeth);
  p->SetField(NWTpack::IPRNWT, &IPRNWT);
  p->SetField(NWTpack::IBOTAV, &IBOTAV);
  p->SetField(NWTpack::IFDPARAM, &IFDPARAM);
  p->SetField(NWTpack::ICNVGFLG, &ICNVGFLG);
  CStr base = "1.2 3.4 11 5.6 1 12 13 SIMPLE ";
  CStr str = m_p->Line1_NWT();
  TS_ASSERT_EQUALS2(base, str);
  IFDPARAM = 2;
  base = "1.2 3.4 11 5.6 1 12 13 MODERATE ";
  str = m_p->Line1_NWT();
  TS_ASSERT_EQUALS2(base, str);
  IFDPARAM = 3;
  base = "1.2 3.4 11 5.6 1 12 13 COMPLEX ";
  str = m_p->Line1_NWT();
  TS_ASSERT_EQUALS2(base, str);

  p->SetField(NWTpack::thetadum, &thetadum);
  p->SetField(NWTpack::akappadum, &akappadum);
  p->SetField(NWTpack::gammadum, &gammadum);
  p->SetField(NWTpack::amomentdum, &amomentdum);
  p->SetField(NWTpack::Btrack, &Btrack);
  p->SetField(NWTpack::Numtrack, &Numtrack);
  p->SetField(NWTpack::Btoldum, &Btoldum);
  p->SetField(NWTpack::Breducdum, &Breducdum);
  IFDPARAM = 4;
  base = "1.2 3.4 11 5.6 1 12 13 SPECIFIED 7.8 9.1 2.3 4.5 14 15 6.7 8.9 ";
  str = m_p->Line1_NWT();
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpSolverT::testLine1_NWT
//------------------------------------------------------------------------------
void NativeExpSolverT::testWhichLine2_NWT ()
{
  int  Linmeth(1), IFDPARAM(1);

  MfPackage* p = m_p->GetPackage();
  p->SetField(NWTpack::Linmeth, &Linmeth);
  p->SetField(NWTpack::IFDPARAM, &IFDPARAM);
  TS_ASSERT_EQUALS(0, m_p->WhichLine2_NWT());
  IFDPARAM = 4;
  TS_ASSERT_EQUALS(2, m_p->WhichLine2_NWT());
  Linmeth = 2;
  TS_ASSERT_EQUALS(3, m_p->WhichLine2_NWT());
} // NativeExpSolverT::testWhichLine2_NWT
//------------------------------------------------------------------------------
void NativeExpSolverT::testLine2a_NWT ()
{
  Real Stop_toldum((Real)1.2);
  int  Maxitr_gmres(21), Ilu_method(22), Lev_fill(23), Msdr(24);
  MfPackage* p = m_p->GetPackage();
  p->SetField(NWTpack::Maxitr_gmres, &Maxitr_gmres);
  p->SetField(NWTpack::Ilu_method, &Ilu_method);
  p->SetField(NWTpack::Lev_fill, &Lev_fill);
  p->SetField(NWTpack::Stop_toldum, &Stop_toldum);
  p->SetField(NWTpack::Msdr, &Msdr);
  CStr base = "21 22 23 1.2 24 ";
  CStr str = m_p->Line2a_NWT();
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpSolverT::testLine2a_NWT
//------------------------------------------------------------------------------
void NativeExpSolverT::testLine2b_NWT ()
{
  Real RRCTOLS((Real)9.8), EPSRNS((Real)7.6), HCLOSEXMDDUM((Real)5.4);
  int IACL(31), NORDER(32), LEVEL(33), NORTH(34), IREDSYS(35), IDROPTOL(36),
      MXITERXMD(37);
  MfPackage* p = m_p->GetPackage();
  p->SetField(NWTpack::IACL, &IACL);
  p->SetField(NWTpack::NORDER, &NORDER);
  p->SetField(NWTpack::LEVEL, &LEVEL);
  p->SetField(NWTpack::NORTH, &NORTH);
  p->SetField(NWTpack::IREDSYS, &IREDSYS);
  p->SetField(NWTpack::RRCTOLS, &RRCTOLS);
  p->SetField(NWTpack::IDROPTOL, &IDROPTOL);
  p->SetField(NWTpack::EPSRNS, &EPSRNS);
  p->SetField(NWTpack::HCLOSEXMDDUM, &HCLOSEXMDDUM);
  p->SetField(NWTpack::MXITERXMD, &MXITERXMD);
  CStr base = "31 32 33 34 35 9.8 36 7.6 5.4 37 ";
  CStr str = m_p->Line2b_NWT();
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpSolverT::testLine2b_NWT
//------------------------------------------------------------------------------
void NativeExpSolverT::testLine1a_SMS ()
{
  int IFDPARAM(0);
  MfPackage* p = m_p->GetPackage();
  p->SetField(SmsPack::IFDPARAM, &IFDPARAM);
  CStr base = "";
  CStr str = m_p->Line1a_SMS();
  TS_ASSERT_EQUALS2(base, str);

  IFDPARAM = 1;
  p->SetField(SmsPack::IFDPARAM, &IFDPARAM);
  base = "SIMPLE ";
  str = m_p->Line1a_SMS();
  TS_ASSERT_EQUALS2(base, str);

  IFDPARAM = 2;
  p->SetField(SmsPack::IFDPARAM, &IFDPARAM);
  base = "MODERATE ";
  str = m_p->Line1a_SMS();
  TS_ASSERT_EQUALS2(base, str);

  IFDPARAM = 3;
  p->SetField(SmsPack::IFDPARAM, &IFDPARAM);
  base = "COMPLEX ";
  str = m_p->Line1a_SMS();
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpSolverT::testLine1a_SMS
//------------------------------------------------------------------------------
void NativeExpSolverT::testLine1b_SMS ()
{
  int MXITER(100), ITER1(100), IPRSMS(1), NONLINMETH(0), LINMETH(1);
  double HCLOSE(1e-8), HICLOSE(1e-8);
  MfPackage* p = m_p->GetPackage();
  p->SetField(SmsPack::HCLOSE, &HCLOSE);
  p->SetField(SmsPack::HICLOSE, &HICLOSE);
  p->SetField(SmsPack::MXITER, &MXITER);
  p->SetField(SmsPack::ITER1, &ITER1);
  p->SetField(SmsPack::IPRSMS, &IPRSMS);
  p->SetField(SmsPack::NONLINMETH, &NONLINMETH);
  p->SetField(SmsPack::LINMETH, &LINMETH);
  CStr base;
  if (sizeof(Real) == 8) {
    base = "1.0e-008 1.0e-008 100 100 1 0 1 ";
  }
  else if (sizeof(Real) == 4) {
    base = "1.0e-008 1.0e-008 100 100 1 0 1 ";
    //base = "9.99999994e-009 9.99999994e-009 100 100 1 0 1 ";
      // I get this in production code.
  }
  else {
    TS_FAIL("NativeExpSolverT::testLine1b_SMS");
  }
  CStr str = m_p->Line1b_SMS();
  TS_ASSERT_EQUALS2(base, str);

} // NativeExpSolverT::testLine1b_SMS
//------------------------------------------------------------------------------
void NativeExpSolverT::testLine2_SMS ()
{
  int NUMTRACK(200);
  double THETA(.9), AKAPPA(.07), GAMMA(.1), AMOMENTUM(0), BTOL(1.1),
         BREDUC(.2), RESLIM(1);
  MfPackage* p = m_p->GetPackage();
  p->SetField(SmsPack::THETA, &THETA);
  p->SetField(SmsPack::AKAPPA, &AKAPPA);
  p->SetField(SmsPack::GAMMA, &GAMMA);
  p->SetField(SmsPack::AMOMENTUM, &AMOMENTUM);
  p->SetField(SmsPack::NUMTRACK, &NUMTRACK);
  p->SetField(SmsPack::BTOL, &BTOL);
  p->SetField(SmsPack::BREDUC, &BREDUC);
  p->SetField(SmsPack::RESLIM, &RESLIM);
  CStr base = "0.9 0.07 0.1 0.0 200 1.1 0.2 1.0 ";
  CStr str = m_p->Line2_SMS();
  TS_ASSERT_EQUALS2(base, str);

} // NativeExpSolverT::testLine2_SMS
//------------------------------------------------------------------------------
void NativeExpSolverT::testLine3_SMS ()
{
  int IACL(2), NORDER(1), LEVEL(3), NORTH(14), IREDSYS(0), IDROPTOL(0);
  double RRCTOL(0.0), EPSRN(1e-3);
  MfPackage* p = m_p->GetPackage();
  p->SetField(SmsPack::IACL, &IACL);
  p->SetField(SmsPack::NORDER, &NORDER);
  p->SetField(SmsPack::LEVEL, &LEVEL);
  p->SetField(SmsPack::NORTH, &NORTH);
  p->SetField(SmsPack::IREDSYS, &IREDSYS);
  p->SetField(SmsPack::RRCTOL, &RRCTOL);
  p->SetField(SmsPack::IDROPTOL, &IDROPTOL);
  p->SetField(SmsPack::EPSRN, &EPSRN);
  CStr base = "2 1 3 14 0 0.0 0 0.001 ";
  CStr str = m_p->Line3_SMS();
  TS_ASSERT_EQUALS2(base, str);
}
//------------------------------------------------------------------------------
void NativeExpSolverT::testLine4_SMS ()
{
  int IPC(1), ISCL(2), IORD(3), IFLAG(0);
  Real RCLOSEPCGU((Real)0.1);
  MfPackage* p = m_p->GetPackage();
  p->SetField(SmsPack::IPC, &IPC);
  p->SetField(SmsPack::ISCL, &ISCL);
  p->SetField(SmsPack::IORD, &IORD);
  p->SetField(SmsPack::IFLAG, &IFLAG);
  p->SetField(SmsPack::RCLOSEPCGU, &RCLOSEPCGU);
  CStr base = "1 2 3 0.1 ";
  CStr str = m_p->Line4_SMS();
  TS_ASSERT_EQUALS2(base, str);
  IFLAG = 1;
  base = "CG 1 2 3 0.1 ";
  str = m_p->Line4_SMS();
  TS_ASSERT_EQUALS2(base, str);
  IFLAG = 2;
  base = "BCGS 1 2 3 0.1 ";
  str = m_p->Line4_SMS();
  TS_ASSERT_EQUALS2(base, str);
}
#endif

//------------------------------------------------------------------------------
// FILE      NativeExpSwi.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpSwi.h>

#include <iomanip>
#include <sstream>

#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\MfExportUtil.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackFields.h>
#include <private\MfData\Packages\MfPackStrings.h>
#include <private\Parameters.h>
#include <private\Parameters\Param.h>
#include <private\Parameters\ParamList.h>

using namespace MfData::Export;
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpSwi::NativeExpSwi ()
: NativePackExp()
, m_nsrf(0)
, m_nobs(0)
, m_nsolver(0)
, m_adaptive(0)
{
} // MfNativeExpSwi::MfNativeExpSwi
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpSwi::~NativeExpSwi ()
{
} // MfNativeExpSwi::~MfNativeExpSwi
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpSwi::Export ()
{
  Line1();
  Line2a();
  if (*m_nsolver && *m_nsolver == 2) {
    Line2b();
  }
  Line3a();
  if (*m_adaptive && *m_adaptive == 1) {
    Line3b();
  }
  Line4();
  Line5();
  Line6();
  Line7();
  Line8();

  WriteComments();
  WriteStoredLines();

  return true;
} // MfNativeExpSwi::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSwi::Line1 ()
{
  CStr desc = " 1. NSRF ISTRAT NOBS ISWIZT ISWIBD ISWIOBS [OPTIONS]";

  const int *istrat(0), *iswizt(0), *iswibd(0), *iswiobs(0);
  MfPackage* a_p=GetPackage();
  if (!a_p->GetField(Packages::Swi::NSRF, &m_nsrf) || !m_nsrf ||
      !a_p->GetField(Packages::Swi::ISTRAT, &istrat) || !istrat ||
      !a_p->GetField(Packages::Swi::NOBS, &m_nobs) || !m_nobs ||
      !a_p->GetField(Packages::Swi::ISWIZT, &iswizt) || !iswizt ||
      !a_p->GetField(Packages::Swi::ISWIBD, &iswibd) || !iswibd ||
      !a_p->GetField(Packages::Swi::ISWIOBS, &iswiobs) || !iswiobs ||
      !a_p->GetField(Packages::Swi::iadptflg, &m_adaptive) || !m_adaptive)
    return;

  CStr ln;
  ln.Format("%5d %5d %5d %5d %5d %5d %s", *m_nsrf, *istrat, *m_nobs, *iswizt,
            *iswibd, *iswiobs, (*m_adaptive ? "ADAPTIVE" : ""));
  AddToStoredLinesDesc(ln, desc);
} // NativeExpSwi::Line1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSwi::Line2a ()
{
  CStr desc = "2a. NSOLVER IPRSOL MUTSOL";

  const int *iprsol(0), *mutsol(0);
  MfPackage* a_p=GetPackage();
  if (!a_p->GetField(Packages::Swi::NSOLVER, &m_nsolver) || !m_nsolver ||
      !a_p->GetField(Packages::Swi::IPRSOL, &iprsol) || !iprsol ||
      !a_p->GetField(Packages::Swi::MUTSOL, &mutsol) || !mutsol)
    return;

  CStr ln;
  ln.Format("%5d %5d %5d", *m_nsolver, *iprsol, *mutsol);
  AddToStoredLinesDesc(ln, desc);
} // NativeExpSwi::Line2a
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSwi::Line2b ()
{
  CStr desc = "2b. MXITER ITER1 NPCOND ZCLOSE RCLOSE RELAX NBPOL DAMP [DAMPT]";

  const int *mxiter(0), *iter1(0), *npcond(0), *nbpol(0);
  const Real *zclose(0), *rclose(0), *damp(0), *dampt(0);
  MfPackage* a_p=GetPackage();
  if (!a_p->GetField(Packages::Swi::MXITER, &mxiter) || !mxiter ||
      !a_p->GetField(Packages::Swi::ITER1, &iter1) || !iter1 ||
      !a_p->GetField(Packages::Swi::NPCOND, &npcond) || !npcond ||
      !a_p->GetField(Packages::Swi::ZCLOSE, &zclose) || !zclose ||
      !a_p->GetField(Packages::Swi::RCLOSE, &rclose) || !rclose ||
      !a_p->GetField(Packages::Swi::RELAX, &nbpol) || !nbpol ||
      !a_p->GetField(Packages::Swi::NBPOL, &damp) || !damp ||
      !a_p->GetField(Packages::Swi::DAMP, &dampt) || !dampt)
    return;

  CStr ln;
  ln.Format("%5d %5d %5d %s %s %5d %5d %s %s", *mxiter, *iter1, *npcond,
            STR(*zclose), STR(*rclose), *nbpol, STR(*damp), STR(*dampt));
  AddToStoredLinesDesc(ln, desc);
} // NativeExpSwi::Line2b
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSwi::Line3a ()
{
  CStr desc = "3a. TOESLOPE TIPSLOPE [ALPHA] [BETA]";

  const Real *toeslope(0), *tipslope(0), *alpha(0), *beta(0);
  MfPackage* a_p=GetPackage();
  if (!a_p->GetField(Packages::Swi::TOESLOPE, &toeslope) || !toeslope ||
      !a_p->GetField(Packages::Swi::TIPSLOPE, &tipslope) || !tipslope ||
      !a_p->GetField(Packages::Swi::ALPHA, &alpha) || !alpha ||
      !a_p->GetField(Packages::Swi::BETA, &beta) || !beta)
    return;

  CStr ln;
  ln.Format("%s %s %s %s", STR(*toeslope), STR(*tipslope), STR(*alpha),
                           STR(*beta));
  AddToStoredLinesDesc(ln, desc);
} // NativeExpSwi::Line3a
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSwi::Line3b ()
{
  CStr desc = "3b. NADPTMX NADPTMN ADPTFCT";

  const int *nadptmx(0), *nadptmn(0);
  const Real *adptfct(0);
  MfPackage* a_p=GetPackage();
  if (!a_p->GetField(Packages::Swi::NADPTMX, &nadptmx) || !nadptmx ||
      !a_p->GetField(Packages::Swi::NADPTMN, &nadptmn) || !nadptmn ||
      !a_p->GetField(Packages::Swi::ADPTFCT, &adptfct) || !adptfct)
    return;

  CStr ln;
  ln.Format("%s %s %s", STR(*nadptmx), STR(*nadptmn), STR(*adptfct));
  AddToStoredLinesDesc(ln, desc);
} // NativeExpSwi::Line3b
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSwi::Line4 ()
{
  MfPackage* p = GetGlobal()->GetPackage(MfData::Packages::Swi::NUZONE);
  if (!p)
  {
    p = GetGlobal()->GetPackage(MfData::Packages::Swi::NUSURF);
  }
  std::vector<CStr>& lines(p->StringsToWrite());
  std::vector<CStr>& desc(p->StringDescriptions());
  desc[0] = " 4. NU(ISTRAT=0: NSRF+2, ISTRAT=1: NSRF+1)";
  AddToStoredLinesDesc(lines, desc);
} // NativeExpSwi::Line4
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSwi::Line5 ()
{
  for (int i = 0; i < *m_nsrf; ++i) {
    std::stringstream ss;
    ss << "ZETA SURFACE" << std::setw(3) << (i + 1);
    std::string s = ss.str().c_str();
    MfPackage* p = GetGlobal()->GetPackage(s.c_str());
    if (p) {
      std::vector<CStr>& lines(p->StringsToWrite());
      std::vector<CStr>& desc(p->StringDescriptions());
      desc[0] = " 5. ZETA(NCOL,NROW)";
      AddToStoredLinesDesc(lines, desc);
    }
  }
} // NativeExpSwi::Line5
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSwi::Line6 ()
{
  int nlay = GetGlobal()->NumLay();
  for (int i = 0; i < nlay; ++i) {
    MfPackage* p = GetGlobal()->GetPackage(ARR_SWI_SSZ);
    if (p) {
      std::vector<CStr>& lines(p->StringsToWrite());
      std::vector<CStr>& desc(p->StringDescriptions());
      desc[0] = " 6. SSZ(NCOL,NROW)";
      AddToStoredLinesDesc(lines, desc);
    }
  }
} // NativeExpSwi::Line6
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSwi::Line7 ()
{
  int nlay = GetGlobal()->NumLay();
  for (int i = 0; i < nlay; ++i) {
    MfPackage* p = GetGlobal()->GetPackage(ARR_SWI_ISOURCE);
    if (p) {
      std::vector<CStr>& lines(p->StringsToWrite());
      std::vector<CStr>& desc(p->StringDescriptions());
      desc[0] = " 7. ISOURCE(NCOL,NROW)";
      AddToStoredLinesDesc(lines, desc);
    }
  }
} // NativeExpSwi::Line7
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSwi::Line8 ()
{
  //using util::ForElement;
  using util::ForIndex;

  CStr desc = " 8. OBSNAM LAYER ROW COLUMN";

  const char *obsname;
  const int *obsk(0), *obsi(0), *obsj(0);
  MfPackage* a_p=GetPackage();
  if (!a_p->GetField(Packages::Swi::OBSNAME, &obsname) || !obsname ||
      !a_p->GetField(Packages::Swi::OBSK, &obsk) || !obsk ||
      !a_p->GetField(Packages::Swi::OBSI, &obsi) || !obsi ||
      !a_p->GetField(Packages::Swi::OBSJ, &obsj) || !obsj)
    return;

  for (int i = 0; i < *m_nobs; ++i) {
    int nameIdx = ForIndex(1, i+1, 12);
    char name[12];
    memcpy(&name[0], &obsname[nameIdx], 12);
    name[11] = '\0';
    CStr cname(name);
    cname.Trim();
    CStr ln;
    ln.Format("%-12s %6d %6d %6d", cname.c_str(), obsk[i], obsi[i], obsj[i]);
    AddToStoredLinesDesc(ln, (i == 0 ? desc : ""));
  }
} // NativeExpSwi::Line8


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData\MfExport\private\Native\NativeExpSwi.t.h>

#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackStrings.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpSwiT::setUp ()
{
  m_p = NULL;
  Mf2kNative* n = new Mf2kNative;
  MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  MfPackage* swi = new MfPackage(Packages::SWI);
  NativePackExp* p = NativeUtil::CreatePackExp(n, g, swi);
  m_p = dynamic_cast<NativeExpSwi*>(p);
} // NativeExpSwiT::setUp
//------------------------------------------------------------------------------
void NativeExpSwiT::tearDown ()
{
  m_p->UnitTestingDeletePointers();
  delete(m_p);
} // NativeExpSwiT::tearDown
//------------------------------------------------------------------------------
void NativeExpSwiT::testCreateClass ()
{
  TS_ASSERT(m_p);
} // NativeExpSwiT::testCreateClass

#endif

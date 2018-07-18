//------------------------------------------------------------------------------
// FILE      NativeExpPes.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/MfData/MfExport/private/Native/NativeExpPes.h>

#include <private/MfData/MfExport/private/Native/NativeExpNam.h>
#include <private/MfData/MfExport/private/Native/NativeUtil.h>
#include <private/MfData/MfGlobal.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackFields.h>
#include <private/MfData/Packages/MfPackStrings.h>

using namespace MfData::Export;
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpPes::NativeExpPes ()
{
} // MfNativeExpPes::MfNativeExpPes
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpPes::~NativeExpPes ()
{
} // MfNativeExpPes::~MfNativeExpPes
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpPes::Export ()
{
  using namespace MfData::Packages;
  const int *maxiter, *ibeflg, *iycflg, *iostar, *nopt,
            *nfit, *iap, *iprcov, *iprint, *lprint, *lastx;
  const Real *tol, *sosc, *sosr, *maxchange, *rmar, *rmarm, *csa, *fconv;

  MfPackage* a_p = GetPackage();
  if (a_p->GetField(PESpack::ITMXP, &maxiter) && maxiter &&
      a_p->GetField(PESpack::DMAX, &maxchange) && maxchange &&
      a_p->GetField(PESpack::RTOL, &tol) && tol &&
      a_p->GetField(PESpack::SOSC, &sosc) && sosc &&
      a_p->GetField(PESpack::IBEFLG, &ibeflg) && ibeflg &&
      a_p->GetField(PESpack::IYCFLG, &iycflg) && iycflg &&
      a_p->GetField(PESpack::IOSTAR, &iostar) && iostar &&
      a_p->GetField(PESpack::NOPT, &nopt) && nopt &&
      a_p->GetField(PESpack::NFIT, &nfit) && nfit &&
      a_p->GetField(PESpack::SOSR, &sosr) && sosr &&
      a_p->GetField(PESpack::RMAR, &rmar) && rmar &&
      a_p->GetField(PESpack::RMARM, &rmarm) && rmarm &&
      a_p->GetField(PESpack::IAP, &iap) && iap &&
      a_p->GetField(PESpack::IPRC, &iprcov) && iprcov &&
      a_p->GetField(PESpack::IPRINT, &iprint) && iprint &&
      a_p->GetField(PESpack::LPRINT, &lprint) && lprint &&
      a_p->GetField(PESpack::CSA, &csa) && csa &&
      a_p->GetField(PESpack::FCONV, &fconv) && fconv &&
      a_p->GetField(PESpack::LASTX, &lastx) && lastx)
  {
    CStr str;
    str.Format("%d %s %s %s ", *maxiter, STR(*maxchange), STR(*tol), STR(*sosc));
    AddToStoredLinesDesc(str, Desc(1));

    str.Format("%d %d %d %d %d %s %s %s %d ", *ibeflg, *iycflg, *iostar, *nopt,
             *nfit, STR(*sosr), STR(*rmar), STR(*rmarm), *iap);
    AddToStoredLinesDesc(str, Desc(2));
    
    str.Format("%d %d %d ", *iprcov, *iprint, *lprint);
    AddToStoredLinesDesc(str, Desc(3));
    
    str.Format("%s %s %d ", STR(*csa), STR(*fconv), *lastx);
    AddToStoredLinesDesc(str, Desc(4));
    
    AddToStoredLinesDesc("0 0 0 ", Desc(5));
  }

  WriteComments();
  WriteStoredLines();
  return true;
} // MfNativeExpPes::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpPes::Desc (int a_line)
{
  CStr rval[] = {" 1. MAX-ITER MAX-CHANGE TOL SOSC",
                 " 2. IBEFLG IYCFLG IOSTAR NOPT NFIT SOSR RMAR RMARM IAP",
                 " 3. PRCOV IPRINT LPRINT",
                 " 4. CSA FCONV LASTX",
                 " 5. NPNG IPR MPR",
                 " 6. PARNEG(1), PARNEG(2), . . . , PARNEG(NPNG)",
                 " 7. NIPRNAM BPRI PLOT-SYMBOL",
                 " 8. IWTP",
                 " 9. WTP(1,1), WTP(1,2), ..., WTP(1,IPR)",
                 "10. EQNAM PRM \"=\" [SIGN] [COEF \"*\"] PNAM [SIGN [COEF \"*\"] PNAM [SIGN…]] \"STAT\" STATP STAT-FLAG PLOT-SYMBOL"
                 };

  return rval[a_line-1];
} // NativeExpPes::Desc


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private/MfData/MfExport/private/Native/NativeExpPes.t.h>

#include <private/MfData/MfExport/private/Mf2kNative.h>
#include <private/MfData/MfExport/private/Native/NativeUtil.h>
#include <private/MfData/MfGlobal.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackStrings.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpPesT::setUp ()
{
  m_p = NULL;
  Mf2kNative* n = new Mf2kNative;
  MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  MfPackage* dis = new MfPackage(Packages::PES);
  NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  m_p = dynamic_cast<NativeExpPes*>(p);
} // NativeExpPesT::setUp
//------------------------------------------------------------------------------
void NativeExpPesT::tearDown ()
{
  m_p->UnitTestingDeletePointers();
  delete(m_p);
} // NativeExpPesT::tearDown
//------------------------------------------------------------------------------
void NativeExpPesT::testCreateClass ()
{
  TS_ASSERT(m_p);
} // NativeExpPesT::testCreateClass

#endif
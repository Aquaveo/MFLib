//------------------------------------------------------------------------------
// FILE      NativeExpOc.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpOc.h>

#include <private\MfData\MfGlobal.h>
#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfExport\private\Native\NativeExpMf6Oc.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackFields.h>
#include <private\MfData\Packages\MfPackStrings.h>

using namespace MfData::Export;
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpOc::NativeExpOc ()
{
} // MfNativeExpOc::MfNativeExpOc
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpOc::~NativeExpOc ()
{
} // MfNativeExpOc::~MfNativeExpOc
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpOc::Export ()
{
  Mf2kNative* n1 = GetNative();
  if (n1 && n1->GetExportMf6())
  {    
    NativeExpMf6Oc evt(this);
    evt.Export();   
    return true;
  }

  if (GetPackage()->PackageName() == Packages::OC)
  {
    AddToStoredLinesDesc(Line1(), Desc1());
  }
  if (GetPackage()->PackageName() == Packages::OCT)
  {
    CStr l2 = Line2(), d2 = Desc2();
    std::vector<CStr> l3 = Line3(), d3 = Desc3();
    MfPackage* p = GetGlobal()->GetPackage(Packages::OC);
    this->SetData(GetNative(), GetGlobal(), p);
    AddToStoredLinesDesc(l2, d2);
    AddToStoredLinesDesc(l3, d3);
  }
  return true;
} // MfNativeExpOc::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
std::vector<CStr> NativeExpOc::Line1 ()
{
  using namespace MfData::Packages;
  MfPackage* a_p = GetPackage();
  std::vector<CStr> rval;
  const int *ihedfm, *iddnfm, *ihedun, *iddnun, *ibdopt, *iauxsv, *ibouun(0),
            *lbbosv;
  const char* chedfm, *cddnfm(0), *cboufm(0);
  if (a_p->GetField(OCpack::IHEDFM, &ihedfm) && ihedfm &&
      a_p->GetField(OCpack::IDDNFM, &iddnfm) && iddnfm &&
      a_p->GetField(OCpack::CHEDFM, &chedfm) && chedfm &&
      a_p->GetField(OCpack::IHEDUN, &ihedun) && ihedun &&
      a_p->GetField(OCpack::IDDNUN, &iddnun) && iddnun &&
      a_p->GetField(OCpack::IBDOPT, &ibdopt) && ibdopt &&
      a_p->GetField(OCpack::IAUXSV, &iauxsv) && iauxsv &&
      a_p->GetField(OCpack::LBBOSV, &lbbosv) && lbbosv)
  {
    CStr aStr;

    // Heads
    if (*ihedfm != 0)
    {
      aStr.Format("HEAD PRINT FORMAT %d", *ihedfm);
      rval.push_back(aStr);
    }
    if (chedfm && strlen(chedfm) != 0)
    {
      aStr.Format("HEAD SAVE FORMAT %s", chedfm);
      rval.push_back(aStr);
    }
    if (*ihedun > 0)
    {
      aStr.Format("HEAD SAVE UNIT %d", *ihedun);
      rval.push_back(aStr);
    }
    // Drawdown
    if (*iddnfm != 0)
    {
      aStr.Format("DRAWDOWN PRINT FORMAT %d", *iddnfm);
      rval.push_back(aStr);
    }
    if (a_p->GetField(OCpack::CDDNFM, &cddnfm) && cddnfm &&
        strlen(cddnfm) != 0)
    {
      aStr.Format("DRAWDOWN SAVE FORMAT %s", cddnfm);
      rval.push_back(aStr);
    }
    if (*iddnun > 0)
    {
      aStr.Format("DRAWDOWN SAVE UNIT %d", *iddnun);
      rval.push_back(aStr);
    }
    // Ibound
    if (*lbbosv > 0 && a_p->GetField(OCpack::CBOUFM, &cboufm) && cboufm &&
        strlen(cboufm) != 0)
    {
      aStr.Format("IBOUND SAVE FORMAT %s", cboufm);
      rval.push_back(aStr);
    }
    if (a_p->GetField(OCpack::IBOUUN, &ibouun) && ibouun && *ibouun > 0)
    {
      aStr.Format("IBOUND SAVE UNIT %d", *ibouun);
      rval.push_back(aStr);
    }
    // Budget
    if (*ibdopt == 2 && *iauxsv == 0)
    {
      rval.push_back("COMPACT BUDGET");
    }
    else if (*ibdopt == 2 && *iauxsv == 1)
    {
      rval.push_back("COMPACT BUDGET AUXILIARY");
    }
  }

  return rval;
} // NativeExpOc::Line1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
std::vector<CStr> NativeExpOc::Desc1 ()
{
  std::vector<CStr> l = Line1();
  CStr s = " 1. HEAD/DRAWDOWN/IBOUND FORMAT/UNIT/SAVE, COMPACT BUDGET "
           "[ AUX or AUXILIARY]";
  std::vector<CStr> rval(l.size(), s);
  return rval;
} // NativeExpOc::Desc1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpOc::Line2 ()
{
  CStr rval;
  MfPackage* a_p = GetPackage();
  using namespace Packages;
  const int *spid, *tsnum;
  if (a_p->GetField(OCTpack::SPID, &spid) && spid &&
      a_p->GetField(OCTpack::TSNum, &tsnum) && tsnum)
  {
    rval.Format("PERIOD %d STEP %d", *spid, *tsnum);
  }
  return rval;
} // NativeExpOc::Line2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpOc::Desc2 ()
{
  CStr rval = " 2. PERIOD IPEROC STEP ITSOC [DDREFERENCE]";
  return rval;
} // NativeExpOc::Desc2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
std::vector<CStr> NativeExpOc::Line3 ()
{
  std::vector<CStr> rval;
  MfPackage* a_p = GetPackage();
  using namespace Packages;
  const int *ihddfl, *ibudfl, *icbcfl, *hdpr, *ddpr, *hdsv, *ddsv, *ibsv;
  if (a_p->GetField(OCTpack::IHDDFL, &ihddfl) && ihddfl &&
      a_p->GetField(OCTpack::IBUDFL, &ibudfl) && ibudfl &&
      a_p->GetField(OCTpack::ICBCFL, &icbcfl) && icbcfl &&
      a_p->GetField(OCTpack::Hdpr, &hdpr) && hdpr &&
      a_p->GetField(OCTpack::Ddpr, &ddpr) && ddpr &&
      a_p->GetField(OCTpack::Hdsv, &hdsv) && hdsv &&
      a_p->GetField(OCTpack::Ddsv, &ddsv) && ddsv &&
      a_p->GetField(OCTpack::Ibsv, &ibsv) && ibsv)
  {
    if (*hdpr != 0)
      rval.push_back("   PRINT HEAD");
    if (*ddpr != 0)
      rval.push_back("   PRINT DRAWDOWN");
    if (*ibudfl != 0)
      rval.push_back("   PRINT BUDGET");
    if (*hdsv != 0)
      rval.push_back("   SAVE HEAD");
    if (*ddsv != 0)
      rval.push_back("   SAVE DRAWDOWN");
    if (*ibsv != 0)
      rval.push_back("   SAVE IBOUND");
    if (*icbcfl != 0)
      rval.push_back("   SAVE BUDGET");
  }
  return rval;
} // NativeExpOc::Line3
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
std::vector<CStr> NativeExpOc::Desc3 ()
{
  std::vector<CStr> l = Line3();
  CStr s = " 3. PRINT HEAD/DRAWDOWN/BUDGET, SAVE HEAD/DRAWDOWN/BUDGET/IBOUND";
  std::vector<CStr> rval(l.size(), s);
  return rval;
} // NativeExpOc::Desc3
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpOc::LastChanceBeforeWriting ()
{
  Mf2kNative* n1 = GetNative();
  if (n1 && n1->GetExportMf6())
  {    
    NativeExpMf6Oc oc(this);
    oc.WriteFinal();
  }
} // NativeExpOc::LastChanceBeforeWriting


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData\MfExport\private\Native\NativeExpOc.t.h>

#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackStrings.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpOcT::setUp ()
{
  m_p = NULL;
  Mf2kNative* n = new Mf2kNative;
  MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  MfPackage* dis = new MfPackage("OC");
  NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  m_p = dynamic_cast<NativeExpOc*>(p);
} // NativeExpOcT::setUp
//------------------------------------------------------------------------------
void NativeExpOcT::tearDown ()
{
  m_p->UnitTestingDeletePointers();
  delete(m_p);
} // NativeExpOcT::tearDown
//------------------------------------------------------------------------------
void NativeExpOcT::testCreateClass ()
{
  TS_ASSERT(m_p);
} // NativeExpOcT::testCreateClass

#endif
//------------------------------------------------------------------------------
// FILE      NativeExpOc.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpOc.h>

#include <sstream>

#include <private\MfData\MfGlobal.h>
#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\Native\mf6\NativeExpMf6Oc.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackFields.h>
#include <private\MfData\Packages\MfPackStrings.h>

using namespace MfData::Export;
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpOc::NativeExpOc () :
  m_usgTransport(false)
, m_usgTransportAts(false)
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

  MfGlobal *g = GetGlobal();
  if (g->ModelType() == MfData::USG_TRANSPORT)
  {
    m_usgTransport = true;
    const int* iats(0);
    using namespace MfData::Packages;
    GetPackage()->GetField(OCpack::IATS, &iats);
    if (iats && *iats)
      m_usgTransportAts = true;
  }


  if (GetPackage()->PackageName() == Packages::OC)
  {
    CStr options = Options();
    if (!options.empty()) AddToStoredLinesDesc(options, " 1. [OPTIONS]");
    AddToStoredLinesDesc(Line1(), Desc1());
    if (!m_usgTransportLine2.empty())
    {
      AddToStoredLinesDesc(m_usgTransportLine2, " 2. TIMOT [nptimes]");
    }
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
/// \brief Gets a formatted line to write to the file for the "OPTIONS" line
//------------------------------------------------------------------------------
CStr NativeExpOc::Options ()
{
  using namespace MfData::Packages;
  MfPackage* p = GetPackage();
  const int* iats(0),* nptimes(0),* npstps(0),* ifast(0),* ispfast(0),
    * itsfast(0),* iugfast(0);
  const double* timot(0);
  p->GetField(OCpack::IATS, &iats);
  p->GetField(OCpack::NPTIMES, &nptimes);
  p->GetField(OCpack::NPSTPS, &npstps);
  p->GetField(OCpack::IFAST, &ifast);
  p->GetField(OCpack::ISPFAST, &ispfast);
  p->GetField(OCpack::ITSFAST, &itsfast);
  p->GetField(OCpack::IUGFAST, &iugfast);
  p->GetField(OCpack::TIMOT, &timot);

  CStr rval;
  if (iats && *iats != 0)
  {
    rval = "ATSA ";
  }
  else
    return rval;

  if (nptimes && *nptimes > 0)
  {
    std::stringstream ss;
    ss << "NPTIMES " << *nptimes << " ";
    rval += ss.str();

  }
  if (npstps && *npstps > 0)
  {
    std::stringstream ss;
    ss << "NPSTPS " << *npstps << " ";
    rval += ss.str();
    if (timot)
    {
      std::stringstream ss;
      ss << "TIMOT ";
      for (int i=0; i<*npstps; ++i) ss << STR(timot[i]) << " ";
      m_usgTransportLine2 = ss.str();
    }
  }
  if (ifast && *ifast != 0 && ispfast && itsfast && iugfast)
  {
    std::stringstream ss;
    ss << "FASTFORWARD " << *ispfast << " " << *itsfast << " " << *iugfast;
    rval += ss.str();
  }

  return rval;
} // NativeExpOc::Options
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
std::vector<CStr> NativeExpOc::Line1 ()
{
  using namespace MfData::Packages;
  MfPackage* a_p = GetPackage();
  std::vector<CStr> rval;
  const int *ihedfm(0), *iddnfm(0), *ihedun(0), *iddnun(0), *ibdopt(0),
    *iauxsv(0), *ibouun(0), *lbbosv(0);
  const char* chedfm(0), *cddnfm(0), *cboufm(0);
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

    // This only applies to USG-Transport
    const int* ispcfm(0),* ispcun(0);
    const char* cspcfm(0);
    a_p->GetField(OCpack::ISPCFM, &ispcfm);
    a_p->GetField(OCpack::CSPCFM, &cspcfm);
    a_p->GetField(OCpack::ISPCUN, &ispcun);
    // concentration
    if (ispcfm && *ispcfm != 0)
    {
      aStr.Format("CONC PRINT FORMAT %d", *ispcfm);
      rval.push_back(aStr);
    }
    if (cspcfm && strlen(cspcfm) != 0)
    {
      aStr.Format("CONC SAVE FORMAT %s", cspcfm);
      rval.push_back(aStr);
    }
    if (ispcun && *ispcun > 0)
    {
      aStr.Format("CONC SAVE UNIT %d", *ispcun);
      rval.push_back(aStr);
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
  if (m_usgTransport)
    s =  " 1. HEAD/DRAWDOWN/IBOUND/CONC FORMAT/UNIT/SAVE, COMPACT BUDGET "
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
  if (m_usgTransport)
  {
    if (m_usgTransportAts)
      rval = "3b. PERIOD IPEROC [DDREFERENCE]";
    else
      rval = "3a. PERIOD IPEROC STEP ITSOC [DDREFERENCE]";
  }
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
  const int *ibudfl(0), *icbcfl(0), *hdpr(0), *ddpr(0), *hdsv(0), *ddsv(0),
    *ibsv(0);
  if (a_p->GetField(OCTpack::IBUDFL, &ibudfl) && ibudfl &&
      a_p->GetField(OCTpack::ICBCFL, &icbcfl) && icbcfl &&
      a_p->GetField(OCTpack::Hdpr, &hdpr) && hdpr &&
      a_p->GetField(OCTpack::Ddpr, &ddpr) && ddpr &&
      a_p->GetField(OCTpack::Hdsv, &hdsv) && hdsv &&
      a_p->GetField(OCTpack::Ddsv, &ddsv) && ddsv &&
      a_p->GetField(OCTpack::Ibsv, &ibsv) && ibsv)
  {
    // only exist in USG-TRANSPORT
    const int *conc_save(0), *conc_print(0);
    const int *read_deltat(0), *read_tminat(0), *read_tmaxat(0), *read_tadjat(0),
      *read_tcutat(0), *read_hclose(0), *read_btol(0), *read_mxiter(0),
      *mxiter(0);
    const double *deltat(0), *tminat(0), *tmaxat(0), *tadjat(0),
      *tcutat(0), *hclose(0), *btol(0);
    a_p->GetField(OCTpack::CONC_PRINT, &conc_print);
    a_p->GetField(OCTpack::CONC_SAVE, &conc_save);
    a_p->GetField(OCTpack::DELTAT, &deltat);
    a_p->GetField(OCTpack::r_DELTAT, &read_deltat);
    a_p->GetField(OCTpack::TMINAT, &tminat);
    a_p->GetField(OCTpack::r_TMINAT, &read_tminat);
    a_p->GetField(OCTpack::TMAXAT, &tmaxat);
    a_p->GetField(OCTpack::r_TMAXAT, &read_tmaxat);
    a_p->GetField(OCTpack::TADJAT, &tadjat);
    a_p->GetField(OCTpack::r_TADJAT, &read_tadjat);
    a_p->GetField(OCTpack::TCUTAT, &tcutat);
    a_p->GetField(OCTpack::r_TCUTAT, &read_tcutat);
    a_p->GetField(OCTpack::HCLOSE, &hclose);
    a_p->GetField(OCTpack::r_HCLOSE, &read_hclose);
    a_p->GetField(OCTpack::BTOL, &btol);
    a_p->GetField(OCTpack::r_BTOL, &read_btol);
    a_p->GetField(OCTpack::MXITER, &mxiter);
    a_p->GetField(OCTpack::r_MXITER, &read_mxiter);

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
    // for some reason the variable for IBOUND SAVE was used to store 
    // CONC_PRINT in USG-TRANSPORT
    if (*ibsv != 0 && !conc_print)
      rval.push_back("   SAVE IBOUND");
    if (*icbcfl != 0)
      rval.push_back("   SAVE BUDGET");

    if (conc_print && *conc_print)
      rval.push_back("   PRINT CONCENTRATION");
    if (conc_save && *conc_save)
      rval.push_back("   SAVE CONCENTRATION");

    // these variables are read as REALs in FORTRASH so write that way
    if (read_deltat && *read_deltat && deltat)
      rval.push_back("   DELTAT " + STR((Real)*deltat));
    if (read_tminat && *read_tminat && tminat)
      rval.push_back("   TMINAT " + STR((Real)*tminat));
    if (read_tmaxat && *read_tmaxat && tmaxat)
      rval.push_back("   TMAXAT " + STR((Real)*tmaxat));
    if (read_tadjat && *read_tadjat && tadjat)
      rval.push_back("   TADJAT " + STR((Real)*tadjat));
    if (read_tcutat && *read_tcutat && tcutat)
      rval.push_back("   TCUTAT " + STR((Real)*tcutat));
    if (read_hclose && *read_hclose && hclose)
      rval.push_back("   HCLOSE " + STR((Real)*hclose));
    if (read_btol && *read_btol && btol)
      rval.push_back("   BTOL " + STR((Real)*btol));
    if (read_mxiter && *read_mxiter && mxiter)
    {
      std::stringstream ss;
      ss << "   MXITER " << *mxiter;
      rval.push_back(ss.str());
    }
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
  if (m_usgTransport)
    s = " 4. PRINT HEAD/DRAWDOWN/BUDGET/CONC, SAVE HEAD/DRAWDOWN/BUDGET/CONC";
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
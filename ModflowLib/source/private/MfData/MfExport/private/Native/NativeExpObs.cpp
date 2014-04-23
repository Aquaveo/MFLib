//------------------------------------------------------------------------------
// FILE      NativeExpObs.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpObs.h>

#include <sstream>
//#include <private\MfData\MfExport\private\Native\NativeExpNam.h>
//#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackStrings.h>
#include <private\MfData\Packages\ObsHd.h>

using namespace MfData::Export;
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpObs::NativeExpObs () :
m_isMf2k(true)
{
} // MfNativeExpObs::MfNativeExpObs
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpObs::~NativeExpObs ()
{
} // MfNativeExpObs::~MfNativeExpObs
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpObs::Export ()
{
  if (GetGlobal()->ModelType() != MfData::MF2K)
  {
    m_isMf2k = false;
  }

  if ("OB1" == GetPackage()->PackageName())
    ExportObsPack();
  else if (Packages::HOB == GetPackage()->PackageName())
    ExportHob();
  return true;
} // MfNativeExpObs::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpObs::ExportObsPack ()
{
  const char *c(0);
  const int *i(0);
  MfPackage* a_package = GetPackage();
  if (!a_package->GetField("OUTNAM", &c) || !c ||
      !a_package->GetField("ISCALS", &i) || !i)
    return;

  CStr ln;
  ln.Format("%s %d", c, *i);
  AddToStoredLinesDesc(ln, " 1. OUTNAM ISCALS [ALLFILLES]");

  TmpPackageNameChanger tmp(a_package, "OBS");

  WriteComments();
  WriteStoredLines();
} // NativeExpObs::ExportObsPack
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpObs::ExportHob ()
{
  HobLine1();
  HobLine2();
  HobLines3to6();

  TmpPackageNameChanger tmp(GetPackage(), "HOB");
  WriteComments();
  WriteStoredLines();
} // NativeExpObs::ExportHob
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpObs::HobLine1 ()
{
  std::vector<HdObs>& d(GetHOB());
  int NH(0), MOBS(0), MAXM(0);
  for (size_t i=0; i<d.size(); ++i)
  {
    if (d[i].m_vLay.empty() || d[i].m_vTimes.empty()) continue;

    NH++;
    int nLay = (int)d[i].m_vLay.size();
    if (nLay > 1)
    {
      MOBS++;
      if (MAXM < nLay) MAXM = nLay;
    }
  }

  std::stringstream os;
  os << NH << " " << MOBS << " " << MAXM;
  CStr desc = " 1. NH MOBS MAXM";
  if (!m_isMf2k)
  {
    MfPackage* p = GetGlobal()->GetPackage("OV2");
    const int *IUHOBSV(0), *NOPRINT(0);
    const Real *HOBDRY(0);
    if (p && p->GetField("IUHOBSV", &IUHOBSV) && IUHOBSV &&
        p->GetField("NOPRINT", &NOPRINT) && NOPRINT &&
        p->GetField("HOBDRY", &HOBDRY) && HOBDRY)
    {
      os << IUHOBSV << " " << STR(*HOBDRY);
      if (*NOPRINT) os << " NOPRINT";
      desc += " IUHOBSV HOBDRY [NOPRINT]";
    }
  }

  AddToStoredLinesDesc(os.str().c_str(), desc);
} // NativeExpObs::HobLine1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpObs::HobLine2 ()
{
  std::stringstream os;
  CStr desc = " 2. TOMULTH";
  if (m_isMf2k)
  {
    MfPackage* p = GetGlobal()->GetPackage("OB2");
    const Real *TOMULTH(0), *EVH(0);
    if (p && p->GetField("TOMULTH", &TOMULTH) && TOMULTH &&
        p->GetField("EVH", &EVH) && EVH)
    {
      os << STR(*TOMULTH) << " " << STR(*EVH);
      desc += " EVH";
    }
  }
  else
  {
    MfPackage* p = GetGlobal()->GetPackage("OV2");
    const Real *TOMULTH(0);
    if (p && p->GetField("TOMULTH", &TOMULTH) && TOMULTH)
    {
      os << STR(*TOMULTH);
    }
  }

  AddToStoredLinesDesc(os.str().c_str(), desc);
} // NativeExpObs::HobLine2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpObs::HobLines3to6 ()
{
  std::vector<HdObs>& d(GetHOB());

  CStr desc = " 3. OBSNAM LAYER ROW COLUMN IREFSP TOFFSET ROFF COFF HOBS";
  CStr d4 = " 4. MLAY(1), PR(1), MLAY(2), PR(2), ..., MLAY(|LAYER|), PR(|LAYER|)";
  CStr d6 = " 6. OBSNAM IREFSP TOFFSET HOBS";
  if (m_isMf2k)
  {
    desc += " STAT-FLAG PLOT-SYMBOL";
    d6 += " STATh STATdd  STAT-FLAG PLOT-SYMBOL";
  }
  if (m_isMf2k)

  for (size_t i=0; i<d.size(); ++i)
  {
    std::stringstream os;
    HdObs& hd = d[i];
    if (hd.m_vLay.empty() || hd.m_vTimes.empty()) continue;

    os << hd.m_name << " ";

    int nlay = (int)hd.m_vLay.size();
    if (nlay > 1) os << -nlay << " ";
    else os << hd.m_vLay[0].m_lay << " ";

    os << hd.m_row << " " << hd.m_col << " ";

    int nTime = (int)hd.m_vTimes.size();
    if (nTime > 1) os << -nTime << " ";
    else os << hd.m_vTimes[0].m_iRefSp << " "
            << STR(hd.m_vTimes[0].m_tOff) << " ";

    os << STR(hd.m_rOff) << " " << STR(hd.m_cOff) << " "
       << STR(hd.m_vTimes[0].m_hob) << " ";

    if (m_isMf2k)
    {
      os << STR(hd.m_vTimes[0].m_statH) << " " << hd.m_vTimes[0].m_statFlag
         << " " << hd.m_vTimes[0].m_plot;
    }

    AddToStoredLinesDesc(os.str().c_str(), desc);

    if (nlay > 1)
    {
      std::stringstream os1;
      for (size_t j=0; j<hd.m_vLay.size(); ++j)
      {
        os1 << hd.m_vLay[j].m_lay << " " << STR(hd.m_vLay[j].m_factor) << " ";
      }
      AddToStoredLinesDesc(os1.str().c_str(), d4);
    }

    if (nTime > 1)
    {
      CStr ln;
      ln.Format("%d", hd.m_ITT);
      AddToStoredLinesDesc(ln, " 5. ITT");
    }

    for (size_t j=0; nTime > 1 && j<hd.m_vTimes.size(); ++j)
    {
      std::stringstream os1;
      os1 << hd.m_vTimes[j].m_name << " " << hd.m_vTimes[j].m_iRefSp << " "
          << STR(hd.m_vTimes[j].m_tOff) << " " << STR(hd.m_vTimes[j].m_hob);
      if (m_isMf2k)
      {
        os1 << STR(hd.m_vTimes[j].m_statH) << " "
            << STR(hd.m_vTimes[j].m_statdd) << " "
            << hd.m_vTimes[j].m_statFlag << " " << hd.m_vTimes[j].m_plot;
      }
      AddToStoredLinesDesc(os1.str().c_str(), d6);
    }
  }
} // NativeExpObs::HobLines3to6


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData\MfExport\private\Native\NativeExpObs.t.h>

#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackStrings.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpObsT::setUp ()
{
  m_p = NULL;
  Mf2kNative* n = new Mf2kNative;
  MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  MfPackage* dis = new MfPackage("OB1");
  NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  m_p = dynamic_cast<NativeExpObs*>(p);
} // NativeExpObsT::setUp
//------------------------------------------------------------------------------
void NativeExpObsT::tearDown ()
{
  m_p->UnitTestingDeletePointers();
  delete(m_p);
} // NativeExpObsT::tearDown
//------------------------------------------------------------------------------
void NativeExpObsT::testCreateClass ()
{
  TS_ASSERT(m_p);
} // NativeExpObsT::testCreateClass

#endif
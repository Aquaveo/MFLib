//------------------------------------------------------------------------------
// FILE      NativeExpVsc.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpVsc.h>

#include <private\MfData\MfExport\private\H5\H5ArrayWriter.h>
#include <private\MfData\MfExport\private\H5\H5UseLastWriter.h>
#include <private\MfData\MfExport\private\MfExporterImpl.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackStrings.h>
#include <private\MfData\Packages\MfPackFields.h>

using namespace MfData::Export;
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpVsc::NativeExpVsc ()
{
} // MfNativeExpVsc::MfNativeExpVsc
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpVsc::~NativeExpVsc ()
{
} // MfNativeExpVsc::~MfNativeExpVsc
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpVsc::Export ()
{
  if (Packages::VSCLine3 == GetPackage()->PackageName())
  {
    Lines1to3();
  }
  else if (Packages::VSCStressPeriod == GetPackage()->PackageName())
  {
    Lines4to5();
    TmpPackageNameChanger tmp(GetPackage(), Packages::VSC);
    WriteComments();
    WriteStoredLines();
  }
  return true;
} // MfNativeExpVsc::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpVsc::Lines1to3 ()
{
  using namespace MfData::Packages;
  MfData::MfPackage* a_p = GetPackage();
  const int *mt3dmuflg, *nsmueos, *mutempopt, *mtmuspec, *mtmutempspec;
  const Real *viscmin, *viscmax, *viscref, *dmudc, *cmuref, *amucoeff;
  if (a_p->GetField(VSCpack::MT3DMUFLG, &mt3dmuflg) && mt3dmuflg &&
      a_p->GetField(VSCpack::NSMUEOS, &nsmueos) && nsmueos &&
      a_p->GetField(VSCpack::MUTEMPOPT, &mutempopt) && mutempopt &&
      a_p->GetField(VSCpack::MTMUSPEC, &mtmuspec) && mtmuspec &&
      a_p->GetField(VSCpack::MTMUTEMPSPEC, &mtmutempspec) && mtmutempspec &&
      a_p->GetField(VSCpack::VISCMIN, &viscmin) && viscmin &&
      a_p->GetField(VSCpack::VISCMAX, &viscmax) && viscmax &&
      a_p->GetField(VSCpack::VISCREF, &viscref) && viscref &&
      a_p->GetField(VSCpack::DMUDC, &dmudc) && dmudc &&
      a_p->GetField(VSCpack::CMUREF, &cmuref) && cmuref &&
      a_p->GetField(VSCpack::AMUCOEFF, &amucoeff) && amucoeff)
  {
    int w = util::RealWidth();
    int f = STR_FULLWIDTH;
    CStr line;

    // Line 1: MT3DMUFLG
    line.Format("%d", *mt3dmuflg);
    AddToStoredLinesDesc(line, Desc("1"));

    // Line 2: VISCMIN VISCMAX
    line.Format("%s %s", STR(*viscmin), STR(*viscmax));
    AddToStoredLinesDesc(line, Desc("2"));

    if (*mt3dmuflg >= 0)
    {
      // Line 3: VISCREF DMUDC(1) CMUREF(1)
      line.Format("%s %s %s",
        STR(*viscref,-1,w,f), STR(*dmudc,-1,w,f), STR(*cmuref,-1,w,f));
      AddToStoredLinesDesc(line, Desc("3"));
    }
    else if (*mt3dmuflg == -1)
    {
      // Line 3a: VISCREF
      line.Format("%s", STR(*viscref,-1,w,f));
      AddToStoredLinesDesc(line, Desc("3a"));

      // Line 3b: NSMUEOS MUTEMPOPT
      line.Format("%d %d", *nsmueos, *mutempopt);
      AddToStoredLinesDesc(line, Desc("3b"));

      for (int i = 0; i < *nsmueos; ++i)
      {
        // Line 3c: MTMUSPEC(NSMUEOS) DMUDC(NSMUEOS) CMUREF(NSMUEOS)
        line.Format("%d %s %s",
          mtmuspec[i], STR(dmudc[i],-1,w,f), STR(cmuref[i],-1,w,f));
        AddToStoredLinesDesc(line, Desc("3c"));
      }

      if (*mutempopt > 0)
      {
        int muncoeff(4);
        if (*mutempopt == 2)
          muncoeff = 5;
        else if (*mutempopt == 3)
          muncoeff = 2;

        // Line 3d: MTMUTEMPSPEC AMUCOEFF(MUNCOEFF)
        line.Format("%d", *mtmutempspec);
        for (int i = 0; i < muncoeff; ++i)
        {
          CStr amucoeffStr;
          amucoeffStr.Format(" %s", STR(amucoeff[i],-1,w,f));
          line += amucoeffStr;
        }
        AddToStoredLinesDesc(line, Desc("3d"));
      }
    }
  }
} // NativeExpVsc::Lines1to3
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpVsc::Lines4to5 ()
{
  using namespace MfData::Packages;
  const int *invisc, *mt3dmuflg;
  MfData::MfPackage* a_pLine3 = GetGlobal()->GetPackage(VSCLine3);
  MfData::MfPackage* a_pSP = GetPackage();
  if (a_pLine3 && a_pSP && 
      a_pLine3->GetField(VSCpack::MT3DMUFLG, &mt3dmuflg) && mt3dmuflg &&
      a_pSP->GetField(VSCpack::INVISC, &invisc) && invisc)
  {
    std::vector<CStr>& l(a_pLine3->StringsToWrite());
    std::vector<CStr>& d(a_pLine3->StringDescriptions());
    for (size_t i=0; i<l.size(); ++i) AddToStoredLinesDesc(l[i], d[i]);
    l.clear();
    d.clear();
    bool arrayWritten = false;
    if (*mt3dmuflg == 0)
    {
      CStr line;

      // Line 4: INVISC
      line.Format("%d", *invisc);
      AddToStoredLinesDesc(line, Desc("4"));

      // Use Last
      std::vector<int> vDat(1, *invisc);
      H5UseLastWriter uh(this);
      uh.WriteData(vDat);

      // Line 5: [VISC(NCOL,NROW)] – U2DREL
      if (*invisc > 0)
      {
        MfPackage* p1 = GetGlobal()->GetPackage(ARR_VSC_VSC);
        if (!p1) p1 = GetGlobal()->GetPackage(ARR_VSC_CONC);
        if (p1 && !p1->StringsToWrite().empty())
        {
          arrayWritten = true;
          std::vector<CStr>& s(p1->StringsToWrite());
          for (size_t i=0; i<s.size(); ++i)
          {
            AddToStoredLinesDesc(s[i], Desc("5"));
          }
          s.clear();
        }
      }
    }

    if (!arrayWritten)
    {
      int sp = GetGlobal()->GetCurrentPeriod();
      int rows = GetGlobal()->NumRow();
      int cols = GetGlobal()->NumCol();
      int lays = GetGlobal()->NumLay();
      H5ArrayWriter ah(this);
      ah.Extend3dDblArray("VSC/07. Property", sp, rows*cols*lays);
      ah.Extend2dDblArray("VSC/08. Property Multiplier", sp, lays);
    }
  }
} // NativeExpVsc::Lines4to5
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpVsc::Desc (const char *a_line)
{
  std::map<CStr, CStr> desc;
  desc["1"]  = " 1. MT3DMUFLG";
  desc["2"]  = " 2. VISCMIN VISCMAX";
  desc["3"]  = " 3. VISCREF DMUDC(1) CMUREF(1)";
  desc["3a"] = "3a. VISCREF";
  desc["3b"] = "3b. NSMUEOS MUTEMPOPT";
  desc["3c"] = "3c. MTMUSPEC(NSMUEOS) DMUDC(NSMUEOS) CMUREF(NSMUEOS)";
  desc["3d"] = "3d. MTMUTEMPSPEC AMUCOEFF(MUNCOEFF)";
  desc["4"]  = " 5. FIRSTDT";
  desc["5"]  = " 6. INDENSE";
  return desc[a_line];
} // NativeExpVsc::Desc


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData\MfExport\private\Native\NativeExpVsc.t.h>

#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackStrings.h>
#include <private/MfLibAsserts.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpVscT::setUp ()
{
} // NativeExpVscT::setUp
//------------------------------------------------------------------------------
void NativeExpVscT::tearDown ()
{
} // NativeExpVscT::tearDown
//------------------------------------------------------------------------------
void NativeExpVscT::testCreateClass ()
{
  NativeExpVsc* p = new NativeExpVsc();
  TS_ASSERT(p);
  if (p) delete(p);
} // NativeExpVscT::testCreateClass

#endif
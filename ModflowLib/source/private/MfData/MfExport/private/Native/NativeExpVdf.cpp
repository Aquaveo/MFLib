//------------------------------------------------------------------------------
// FILE      NativeExpVdf.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpVdf.h>

#include <private\MfData\MfExport\private\MfExporterImpl.h>
#include <private\MfData\MfExport\private\Native\H5ArrayWriter.h>
#include <private\MfData\MfExport\private\Native\H5UseLastWriter.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackStrings.h>
#include <private\MfData\Packages\MfPackFields.h>

using namespace MfData::Export;
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpVdf::NativeExpVdf (bool a_h5) :
m_h5(a_h5)
{
} // MfNativeExpVdf::MfNativeExpVdf
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpVdf::~NativeExpVdf ()
{
} // MfNativeExpVdf::~MfNativeExpVdf
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpVdf::Export ()
{
  if (Packages::VDFLine5 == GetPackage()->PackageName())
  {
    Lines1to5();
  }
  else if (Packages::VDFStressPeriod == GetPackage()->PackageName())
  {
    Lines6to7();
    TmpPackageNameChanger tmp(GetPackage(), Packages::VDF);
    WriteComments();
    WriteStoredLines();
  }
  return true;
} // MfNativeExpVdf::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpVdf::Lines1to5 ()
{
  using namespace MfData::Packages;
  const int *mt3drhoflg, *mfnadvfd, *nswtcpl, *iwtable, *nsrhoeos, *mtrhospec;
  const Real *densemin, *densemax, *dnscrit, *denseref, *drhodc, *drhodprhd,
             *prhdref, *crhoref, *firstdt;
  MfPackage* a_p = GetPackage();
  if (a_p->GetField(VDFpack::MT3DRHOFLG, &mt3drhoflg) && mt3drhoflg &&
      a_p->GetField(VDFpack::MFNADVFD, &mfnadvfd) && mfnadvfd &&
      a_p->GetField(VDFpack::NSWTCPL, &nswtcpl) && nswtcpl &&
      a_p->GetField(VDFpack::IWTABLE, &iwtable) && iwtable &&
      a_p->GetField(VDFpack::DENSEMIN, &densemin) && densemin &&
      a_p->GetField(VDFpack::DENSEMAX, &densemax) && densemax &&
      a_p->GetField(VDFpack::DNSCRIT, &dnscrit) && dnscrit &&
      a_p->GetField(VDFpack::DENSEREF, &denseref) && denseref &&
      a_p->GetField(VDFpack::DRHODC, &drhodc) && drhodc &&
      a_p->GetField(VDFpack::DRHODPRHD, &drhodprhd) && drhodprhd &&
      a_p->GetField(VDFpack::PRHDREF, &prhdref) && prhdref &&
      a_p->GetField(VDFpack::NSRHOEOS, &nsrhoeos) && nsrhoeos &&
      a_p->GetField(VDFpack::MTRHOSPEC, &mtrhospec) && mtrhospec &&
      a_p->GetField(VDFpack::CRHOREF, &crhoref) && crhoref &&
      a_p->GetField(VDFpack::FIRSTDT, &firstdt) && firstdt)
  {
    int w = util::RealWidth();
    int flg = STR_FULLWIDTH;
    CStr line;

    // Line 1: MT3DRHOFLG MFNADVFD NSWTCPL IWTABLE
    line.Format("%5d %5d %5d %5d", *mt3drhoflg, *mfnadvfd, *nswtcpl, *iwtable);
    AddToStoredLinesDesc(line, Desc("1"));

    // Line 2: DENSEMIN DENSEMAX
    line.Format("%s %s", STR(*densemin,-1,w,flg), STR(*densemax,-1,w,flg));
    AddToStoredLinesDesc(line, Desc("2"));

    // Line 3: DNSCRIT 
    if (*nswtcpl > 1 || *nswtcpl == -1)
    {
      line.Format("%s", STR(*dnscrit,-1,w,flg));
      AddToStoredLinesDesc(line, Desc("3"));
    }

    if (*mt3drhoflg >= 0)
    {
      // Line 4: DENSEREF DRHODC(1)
      line.Format("%s %s", STR(*denseref,-1,w,flg), STR(*drhodc,-1,w,flg));
      AddToStoredLinesDesc(line, Desc("4"));
    }
    else if (*mt3drhoflg == -1)
    {
      // Line 4a: DENSEREF DRHODPRHD PRHDREF
      line.Format("%s %s %s", STR(*denseref,-1,w,flg),
                  STR(*drhodprhd,-1,w,flg), STR(*prhdref,-1,w,flg));
      AddToStoredLinesDesc(line, Desc("4a"));

      // Line 4b: NSRHOEOS
      line.Format("%d", *nsrhoeos);
      AddToStoredLinesDesc(line, Desc("4b"));

      for (int i = 0; i < *nsrhoeos; ++i)
      {
        // Line 4c: MTRHOSPEC(NSRHOEOS) DRHODC(NSRHOEOS) CRHOREF(NSRHOEOS)
        line.Format("%d %s %s", mtrhospec[i],
                    STR(drhodc[i],-1,w,flg), STR(crhoref[i],-1,w,flg));
        AddToStoredLinesDesc(line, Desc("4c"));
      }
    }

    // Line 5: FIRSTDT
    line.Format("%s", STR(*firstdt,-1,w,flg));
    AddToStoredLinesDesc(line, Desc("5"));
  }
} // NativeExpVdf::Lines1to5
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpVdf::Lines6to7 ()
{
  using namespace MfData::Packages;
  const int *indense, *mt3drhoflg;
  MfPackage* a_pLine5 = GetGlobal()->GetPackage(VDFLine5);
  MfPackage* a_pSP = GetPackage();
  if (a_pLine5 && a_pSP &&
      a_pLine5->GetField(VDFpack::MT3DRHOFLG, &mt3drhoflg) && mt3drhoflg &&
      a_pSP->GetField(VDFpack::INDENSE, &indense) && indense)
  {
    std::vector<CStr>& l(a_pLine5->StringsToWrite());
    std::vector<CStr>& d(a_pLine5->StringDescriptions());
    for (size_t i=0; i<l.size(); ++i) AddToStoredLinesDesc(l[i], d[i]);
    bool arrayWritten = false;
    if (*mt3drhoflg == 0)
    {
      CStr line;

      // Line 6: INDENSE
      line.Format("%d", *indense);
      AddToStoredLinesDesc(line, Desc("6"));

      // Use Last
      std::vector<int> vDat(1,*indense);
      H5UseLastWriter uh(this);
      uh.WriteData(vDat);

      // Line 7: [DENSE(NCOL,NROW)] – U2DREL
      if (*indense > 0)
      {
        MfPackage* p1 = GetGlobal()->GetPackage(ARR_VDF_DENS);
        if (!p1) p1 = GetGlobal()->GetPackage(ARR_VDF_CONC);
        if (p1 && !p1->StringsToWrite().empty())
        {
          arrayWritten = true;
          std::vector<CStr>& s(p1->StringsToWrite());
          for (size_t i=0; i<s.size(); ++i)
          {
            AddToStoredLinesDesc(s[i], Desc("7"));
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
      ah.Extend3dDblArray("VDF/07. Property", sp, rows*cols*lays);
      ah.Extend2dDblArray("VDF/08. Property Multiplier", sp, lays);
    }
  }
} // NativeExpVdf::Lines6to7
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpVdf::Desc (const char *a_line)
{
  std::map<CStr, CStr> desc;
  desc["1"]  = " 1. MT3DRHOFLG MFNADVFD NSWTCPL IWTABLE";
  desc["2"]  = " 2. DENSEMIN DENSEMAX";
  desc["3"]  = " 3. DNSCRIT";
  desc["4"]  = " 4. DENSEREF DRHODC(1)";
  desc["4a"] = "4a. DENSEREF DRHODPRHD PRHDREF";
  desc["4b"] = "4b. NSRHOEOS";
  desc["4c"] = "4c. MTRHOSPEC(NSRHOEOS) DRHODC(NSRHOEOS) CRHOREF(NSRHOEOS)";
  desc["5"]  = " 5. FIRSTDT";
  desc["6"]  = " 6. INDENSE";
  desc["7"]  = " 7. [DENSE(NCOL,NROW)]";
  return desc[a_line];
} // NativeExpVdf::Desc


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData\MfExport\private\Native\NativeExpVdf.t.h>

#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackStrings.h>
#include <private/MfLibAsserts.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpVdfT::setUp ()
{
} // NativeExpVdfT::setUp
//------------------------------------------------------------------------------
void NativeExpVdfT::tearDown ()
{
} // NativeExpVdfT::tearDown
//------------------------------------------------------------------------------
void NativeExpVdfT::testCreateClass ()
{
  NativeExpVdf* p = new NativeExpVdf(true);
  TS_ASSERT(p);
  if (p) delete(p);
} // NativeExpVdfT::testCreateClass

#endif
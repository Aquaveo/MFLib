//------------------------------------------------------------------------------
// FILE      NativeExpMnw1.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpMnw1.h>

#include <private\ListReader\CellIdToIJK.h>
#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackFields.h>
#include <private\MfData\Packages\MfPackStrings.h>
#include <private\MNWReader.h>

using namespace MfData::Export;
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMnw1::NativeExpMnw1 ()
{
} // MfNativeExpMnw1::MfNativeExpMnw1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpMnw1::~NativeExpMnw1 ()
{
} // MfNativeExpMnw1::~MfNativeExpMnw1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpMnw1::Export ()
{
  if (Packages::MNWSetup == GetPackage()->PackageName())
  {
    Line1to3();
  }
  else
  {
    Line4();
    Line5();
  }
  return true;
} // MfNativeExpMnw1::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMnw1::Line1to3 ()
{
  using namespace MfData::Packages;
  using util::ForElement;
  const int *mxwel2(0), *iwl2cb(0), *iwelpt(0), *kspref(0), *iowell2(0),
            *nomoiter(0);
  const double *ploss(0);
  const char *ftag(0);
  MfPackage* a_p = GetPackage();
  if (a_p->GetField(MNWpack::MXWEL2, &mxwel2) && mxwel2 &&
      a_p->GetField(MNWpack::IWL2CB, &iwl2cb) && iwl2cb &&
      a_p->GetField(MNWpack::IWELPT, &iwelpt) && iwelpt &&
      a_p->GetField(MNWpack::KSPREF, &kspref) && kspref &&
      a_p->GetField(MNWpack::PLoss, &ploss) && ploss &&
      a_p->GetField(MNWpack::IOWELL2, &iowell2) && iowell2 &&
      a_p->GetField(MNWpack::NOMOITER, &nomoiter) && nomoiter &&
      a_p->GetField(MNWpack::FTAG, &ftag) && ftag)
  {
    Real realPloss = (Real)*ploss;
    GetGlobal()->SetRealVar(MNWpack::PLoss, realPloss);
    CStr desc = " 1. MXMNW IWL2CB IWELPT [NOMOITER] REFerence SP: kspref";
    CStr ln;
    ln.Format("%5d %5d %5d ", *mxwel2, *iwl2cb, *iwelpt);
    if (*nomoiter < 9999)
    {
      CStr tmp; tmp.Format("%5d ", *nomoiter);
      ln += tmp;
    }
    if (*kspref > 1)
    {
      CStr tmp; tmp.Format(" REFERENCE SP:%d", *kspref);
      ln += tmp;
    }
    AddToStoredLinesDesc(ln, desc);

    int w = util::RealWidth();
    desc = " 2. LOSSTYPE (PLossMNW)";
    if (*ploss < 0.99) ln = "SKIN";
    else if (*ploss > 1.001)
    {
      ln.Format("NONLINEAR %s", STR(*ploss, -1, w, STR_FULLWIDTH));
    }
    else ln = "LINEAR";
    AddToStoredLinesDesc(ln, desc);

    // line 3a:FILE:filename WEL1:iunw1
    // line 3b:FILE:filename BYNODE:iunby (ALLTIME)
    // line 3c:FILE:filename QSUM:iunqs (ALLTIME)
    std::map<CStr, int> auxiliaryUnits;
    CStr tag1(ftag, 6);
    CStr tag2(ftag+6, 6);
    CStr tag3(ftag+12, 6);
    auxiliaryUnits[tag1.Trim().ToUpper()] = iowell2[0];
    auxiliaryUnits[tag2.Trim().ToUpper()] = iowell2[1];
    auxiliaryUnits[tag3.Trim().ToUpper()] = iowell2[2];
    std::vector<int> iowell2Sorted(3, 0);
    if (auxiliaryUnits["WEL1"] != 0)
    {
      desc = "3a. FILE:filename WEL1:iunw1";
      ln.Format("FILE:MNW-WEL1.out WEL1:%d", abs(auxiliaryUnits["WEL1"]));
      AddToStoredLinesDesc(ln, desc);
    }

    if (auxiliaryUnits["BYNODE"] != 0)
    {
      desc = "3b. FILE:filename BYNODE:iunby ALLTIME";
      ln.Format("FILE:MNW-BYNODE.out BYNODE:%d", abs(auxiliaryUnits["BYNODE"]));
      if (auxiliaryUnits["BYNODE"] < 0)
        ln += " ALLTIME";
      AddToStoredLinesDesc(ln, desc);
    }

    if (auxiliaryUnits["QSUM"] != 0)
    {
      desc = "3c. FILE:filename QSUM:iunqs ALLTIME";
      ln.Format("FILE:MNW-QSUM.out QSUM:%d", abs(auxiliaryUnits["QSUM"]));
      if (auxiliaryUnits["QSUM"] < 0)
        ln += " ALLTIME";
      AddToStoredLinesDesc(ln, desc);
    }
  }

} // NativeExpMnw1::Line1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMnw1::Line4 ()
{
  using namespace MfData::Packages;
  const int *itmp(0), *nwell2(0);
  MfPackage* a_p = GetPackage();
  if (a_p->GetField(MNWpack::ITMP, &itmp) && itmp &&
      a_p->GetField(MNWpack::NWELL2, &nwell2) && nwell2)
  {
    CStr desc = " 4. ITMP ADD";
    CStr ln;
    if (*itmp < 0) ln.Format("%5d", *itmp);
    else           ln.Format("%5d", *nwell2);
    AddToStoredLinesDesc(ln, desc);
  }
} // NativeExpMnw1::Line4
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMnw1::Line5 ()
{
  using namespace MfData::Packages;
  const int *itmp(0), *nwell2(0);
  const double *well2(0), *mnwflgs(0);
  const char *mnwsite(0);
  const int W2_SZ = 18;
  Real ploss;
  using util::ForElement;
  GetGlobal()->GetRealVar(MNWpack::PLoss, ploss);
  MfPackage* a_p = GetPackage();
  if (a_p->GetField(MNWpack::ITMP, &itmp) && itmp &&
      a_p->GetField(MNWpack::NWELL2, &nwell2) && nwell2 &&
      a_p->GetField(MNWpack::WELL2, &well2) && well2 &&
      a_p->GetField(MNWpack::MNWSITE, &mnwsite) && mnwsite &&
      a_p->GetField(MNWpack::MNWFLGS, &mnwflgs) && mnwflgs)
  {
    if (*itmp < 0) return;

//    CStr desc=" 5. Layer Row Column Qdes ";
//              "[MN] QWval Rw Skin Hlim "
//              "Href [DD] Iwgrp Cp: C [QCUT or Q-%CUT: Qfrcmn Qfrcmx] DEFAULT"
//              "   SITE: MNWsite";
    int w = 15;
    int wellid(-1);
    CStr ln, defaultQcut;
    CellIdToIJK cIjk(GetGlobal()->NumRow(), GetGlobal()->NumCol());
    bool writeDefault;
    for (int i=1; i<=*nwell2; ++i)
    {
      writeDefault = false;
      CStr desc=" 5. Layer Row Column Qdes [MN] QWval Rw Skin ";
      bool isMn = false;
      int cell = static_cast<int>(ForElement(well2, 1, i, W2_SZ));
      int wellNum = static_cast<int>(ForElement(well2, 18, i, W2_SZ));
      double q = ForElement(mnwflgs, mnw::MNWFLGS_QDES, i, mnw::MNWFLGS_SIZE);
      ln.Format("%5d %5d %5d %s ", cIjk.KFromId(cell), cIjk.IFromId(cell),
                cIjk.JFromId(cell), STR(q, -1, w, STR_FULLWIDTH));
      // see if node is multinode
      if (wellid == wellNum)
      {
        isMn = true;
        ln += "MN ";
      }
      else ln += "   ";
      // get QWval
      double qwval = ForElement(well2, 4, i, W2_SZ);
      ln += STR(qwval, -1, w, STR_FULLWIDTH);
      ln += " ";
      // get Rw
      double rw = ForElement(well2, 5, i, W2_SZ);
      ln += STR(rw, -1, w, STR_FULLWIDTH);
      ln += " ";
      // get Skin
      double skin = ForElement(well2, 6, i, W2_SZ);
      ln += STR(skin, -1, w, STR_FULLWIDTH);
      ln += " ";
      // Hlim, Href, DD
      double dd = ForElement(mnwflgs, mnw::MNWFLGS_DD, i, mnw::MNWFLGS_SIZE);
      if (dd != mnw::DD_NONE)
      {
        desc += "Hlim Href [DD] ";
        double hlim, href;
        hlim = ForElement(mnwflgs, mnw::MNWFLGS_HLIM, i, mnw::MNWFLGS_SIZE);
        href = ForElement(mnwflgs, mnw::MNWFLGS_HREF, i, mnw::MNWFLGS_SIZE);
        CStr tmp;
        tmp.Format("%s %s ", STR(hlim, -1, w, STR_FULLWIDTH),
                             STR(href, -1, w, STR_FULLWIDTH));
        if (dd == mnw::DD_RELATIVE) tmp += "DD ";
        else tmp += "   ";
        ln += tmp;
      }
      else ln += "                                   ";
      // Iwgrp
      double iqwgrp = ForElement(mnwflgs, mnw::MNWFLGS_IERR, i,
                                 mnw::MNWFLGS_SIZE);
      if (iqwgrp < 1)
      {
        int grp = static_cast<int>(ForElement(well2, 9, i, W2_SZ));
        CStr tmp; tmp.Format("%5d ", grp);
        ln += tmp;
        desc += "Iwgrp ";
      }
      else ln += "      ";
      // Cp:C
      if (ploss > 1.001)
      {
        double c = ForElement(well2, 16, i, W2_SZ);
        CStr tmp; tmp.Format("CP:%s ", STR(c));
        ln += tmp;
        desc += "CP:C ";
      }
      // [QCUT or Q-%CUT: Qfrcmn Qfrcmx] DEFAULT
      int qcut = util::lrint(ForElement(mnwflgs, mnw::MNWFLGS_QCUT, i,
                                        mnw::MNWFLGS_SIZE));
      if (qcut == 1 || qcut == 2)
      {
        CStr str = "Qcut:  ";
        if (qcut == 2)
        {
          q = 100;
          str = "Q-%cut:";
        }
        double qfrcmn = ForElement(well2, 13, i, W2_SZ)*q;
        double qfrcmx = ForElement(well2, 14, i, W2_SZ)*q;
        CStr tmp;
        tmp.Format("%s %s %s ", str,
                                STR(qfrcmn, -1, w, STR_FULLWIDTH),
                                STR(qfrcmx, -1, w, STR_FULLWIDTH));
        if (tmp != defaultQcut)
        {
          ln += tmp;
          desc += "[QCUT or Q-%CUT: Qfrcmn Qfrcmx] ";
        }
        else ln += "                                        ";
        double val = ForElement(mnwflgs, mnw::MNWFLGS_DEFAULT, i,
                                mnw::MNWFLGS_SIZE);
        if (val != 0)
        {
          defaultQcut = tmp;
          writeDefault = true;
        }
      }

      // SITE
      CStr siteName(mnwsite + 32*(i-1), 32);
      siteName.Trim();
      if (!isMn && "NO-PRINT" != siteName)
      {
        ln += "SITE:";
        ln += siteName;
        desc += "SITE: MNWsite";
      }
      if (writeDefault) ln += " DEFAULT";

      AddToStoredLinesDesc(ln, desc);
      wellid = wellNum;
    }

  }
} // NativeExpMnw1::Line5
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpMnw1::AddToStoredLinesDesc (const char* a_line,
                                          const char* a_desc)
{
  MfPackage* p = GetGlobal()->GetPackage(Packages::MNW);
  if (!p)
  {
    MfPackage p1(Packages::MNW);
    GetGlobal()->AddPackage(&p1);
    p = GetGlobal()->GetPackage(Packages::MNW);
  }

  MfPackage* curP = GetPackage();
  SetData(GetNative(), GetGlobal(), p);
  NativePackExp::AddToStoredLinesDesc(a_line, a_desc);
  SetData(GetNative(), GetGlobal(), curP);
} // NativeExpMnw1::AddToStoredLinesDesc


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData\MfExport\private\Native\NativeExpMnw1.t.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpMnw1T::setUp ()
{
  m_p = NULL;
  Mf2kNative* n = new Mf2kNative;
  MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  MfPackage* dis = new MfPackage(Packages::MNWSetup);
  NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  m_p = dynamic_cast<NativeExpMnw1*>(p);
} // NativeExpMnw1T::setUp
//------------------------------------------------------------------------------
void NativeExpMnw1T::tearDown ()
{
  m_p->UnitTestingDeletePointers();
  delete(m_p);
} // NativeExpMnw1T::tearDown
//------------------------------------------------------------------------------
void NativeExpMnw1T::testCreateClass ()
{
  TS_ASSERT(m_p);
} // NativeExpMnw1T::testCreateClass

#endif
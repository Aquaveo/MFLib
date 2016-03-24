//------------------------------------------------------------------------------
// FILE      NativeExpUzf.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpUzf.h>

#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\MfExportUtil.h>
#include <private\MfData\MfExport\private\Native\H5UseLastWriter.h>
#include <private\MfData\MfExport\private\Native\NativeExpNam.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackFields.h>
#include <private\MfData\Packages\MfPackStrings.h>

using namespace MfData::Export;
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpUzf::NativeExpUzf ()
{
} // MfNativeExpUzf::MfNativeExpUzf
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpUzf::~NativeExpUzf ()
{
} // MfNativeExpUzf::~MfNativeExpUzf
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpUzf::Export ()
{
  CStr name = GetPackage()->PackageName();
  if (Packages::UZFLine1 == name) Line1();
  else if (Packages::UZFLine8 == name) Lines2to8();
  else if (Packages::UZFStressPeriod == name) Lines9to16();

  TmpPackageNameChanger tmp(GetPackage(), Packages::UZF);
  if (Packages::UZFLine1 == name) WriteComments();
  WriteStoredLines();
  return true;
} // MfNativeExpUzf::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpUzf::Line1 ()
{
  using namespace MfData::Packages;
  const int *nuztop, *iuzfopt, *irunflg, *ietflg, *iuzfcb1, *iuzfcb2, *ntrail2,
            *nsets2, *nuzgag;
  const Real *surfdep;
  MfPackage* a_p=GetPackage();
  if (a_p->GetField(UZFpack::NUZTOP, &nuztop) && nuztop &&
      a_p->GetField(UZFpack::IUZFOPT, &iuzfopt) && iuzfopt &&
      a_p->GetField(UZFpack::IRUNFLG, &irunflg) && irunflg &&
      a_p->GetField(UZFpack::IETFLG, &ietflg) && ietflg &&
      a_p->GetField(UZFpack::IUZFCB1, &iuzfcb1) && iuzfcb1 &&
      a_p->GetField(UZFpack::IUZFCB2, &iuzfcb2) && iuzfcb2 &&
      a_p->GetField(UZFpack::NTRAIL2, &ntrail2) && ntrail2 &&
      a_p->GetField(UZFpack::NSETS2, &nsets2) && nsets2 &&
      a_p->GetField(UZFpack::NUZGAG, &nuzgag) && nuzgag &&
      a_p->GetField(UZFpack::SURFDEP, &surfdep) && surfdep)
  {
    GetGlobal()->SetIntVar(UZFpack::IUZFOPT, *iuzfopt);
    GetGlobal()->SetIntVar(UZFpack::IRUNFLG, *irunflg);
    GetGlobal()->SetIntVar(UZFpack::IETFLG, *ietflg);
    GetGlobal()->SetIntVar(UZFpack::NUZGAG, *nuzgag);
    // NUZTOP IUZFOPT IRUNFLG IETFLG IUZFCB1 IUZFCB2 [NTRAIL2 NSETS2] NUZGAG SURFDEP
    CStr desc = "1b. NUZTOP IUZFOPT IRUNFLG IETFLG IUZFCB1 IUZFCB2 "
                "[NTRAIL2 NSETS2] NUZGAG SURFDEP";
    CStr line;
    line.Format("%5d %5d %5d %5d %5d %5d", *nuztop, *iuzfopt, *irunflg, *ietflg,
                                           *iuzfcb1, *iuzfcb2);
    if (*iuzfopt > 0)
    {
      CStr part2;
      part2.Format(" %5d %5d", *ntrail2, *nsets2);
      line += part2;
    }

    int w=util::RealWidth();
    int flg = STR_FULLWIDTH;
    CStr part3;
    part3.Format(" %5d %s", *nuzgag, STR(*surfdep,-1,w,flg));
    line += part3;

    AddToStoredLinesDesc(line, desc);
  }
} // NativeExpUzf::Line1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpUzf::Lines2to8 ()
{
  using namespace MfData::Packages;
  using util::ForElement;
  const int *iuzlist;
  int iuzfopt, irunflg, nuzgag;
  MfPackage* a_pLine8=GetPackage();
  if (a_pLine8->GetField(UZFpack::IUZLIST, &iuzlist) && iuzlist)
  {
    GetGlobal()->GetIntVar(UZFpack::IUZFOPT, iuzfopt);
    GetGlobal()->GetIntVar(UZFpack::IRUNFLG, irunflg);
    GetGlobal()->GetIntVar(UZFpack::NUZGAG, nuzgag);
    CStr desc = " 2. IUZFBND(NCOL, NROW)";
    CStr line;

    MfPackage* p = GetGlobal()->GetPackage(ARR_UZF_UBND);
    if (p)
    {
      line = p->StringsToWrite().front();
    }
    else
    {
      line = "0";
    }
    AddToStoredLinesDesc(line, desc);
    if (MfExportUtil::ArrayWriteNextLineInternal(GetNative(), line))
    {
      line = p->StringsToWrite()[1];
      AddToStoredLinesDesc(line, "");
    }

    desc = " 3. IRUNBND(NCOL, NROW)";
    p = GetGlobal()->GetPackage(ARR_UZF_RBND);
    if (irunflg > 0 && p)
    {
      line = p->StringsToWrite().front();
      AddToStoredLinesDesc(line, desc);
      if (MfExportUtil::ArrayWriteNextLineInternal(GetNative(), line))
      {
        line = p->StringsToWrite()[1];
        AddToStoredLinesDesc(line, "");
      }
    }

    desc = " 4. VKS(NCOL, NROW)";
    p = GetGlobal()->GetPackage(ARR_UZF_VKS);
    if (p && (abs(iuzfopt) == 0 || abs(iuzfopt) == 1))
    {
      line = p->StringsToWrite().front();
      AddToStoredLinesDesc(line, desc);
      if (MfExportUtil::ArrayWriteNextLineInternal(GetNative(), line))
      {
        line = p->StringsToWrite()[1];
        AddToStoredLinesDesc(line, "");
      }
    }

    desc = " 5. EPS(NCOL, NROW)";
    p = GetGlobal()->GetPackage(ARR_UZF_EPS);
    if (p && iuzfopt > 0)
    {
      line = p->StringsToWrite().front();
      AddToStoredLinesDesc(line, desc);
      if (MfExportUtil::ArrayWriteNextLineInternal(GetNative(), line))
      {
        line = p->StringsToWrite()[1];
        AddToStoredLinesDesc(line, "");
      }
    }

    desc = "6a. THTS(NCOL, NROW)";
    p = GetGlobal()->GetPackage(ARR_UZF_THTS);
    if (p && iuzfopt > 0)
    {
      line = p->StringsToWrite().front();
      AddToStoredLinesDesc(line, desc);
      if (MfExportUtil::ArrayWriteNextLineInternal(GetNative(), line))
      {
        line = p->StringsToWrite()[1];
        AddToStoredLinesDesc(line, "");
      }
    }

    desc = "6b. THTR(NCOL, NROW)";
    //p = GetGlobal()->GetPackage(ARR_UZF_THTR);
    //if (p)
    //{
    //  line = p->StringsToWrite().front();
    //  AddToStoredLinesDesc(line, desc);
    //}

    desc = " 7. THTI(NCOL, NROW)";
    p = GetGlobal()->GetPackage(ARR_UZF_THTI);
    if (p)
    {
      line = p->StringsToWrite().front();
      AddToStoredLinesDesc(line, desc);
      if (MfExportUtil::ArrayWriteNextLineInternal(GetNative(), line))
      {
        line = p->StringsToWrite()[1];
        AddToStoredLinesDesc(line, "");
      }
    }

    desc = " 8. [IUZROW] [IUZCOL] IFTUNIT [IUZOPT]";
    for (int i = 1; i <= nuzgag; ++i)
    {
      const int IUZLIST_SIZE = 4;
      int iuzrow, iuzcol, iftunit, iuzopt;
      iuzrow  = ForElement(iuzlist, 1, i, IUZLIST_SIZE);
      iuzcol  = ForElement(iuzlist, 2, i, IUZLIST_SIZE);
      iftunit = ForElement(iuzlist, 3, i, IUZLIST_SIZE);
      iuzopt  = ForElement(iuzlist, 4, i, IUZLIST_SIZE);
      if (iuzrow != 0)
        line.Format("%5d %5d %5d %5d", iuzrow, iuzcol, iftunit, iuzopt);
      else
        line.Format("%5d", -iftunit);
      AddToStoredLinesDesc(line, desc);
    }
  }
} // NativeExpUzf::Lines2to8
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpUzf::Lines9to16 ()
{
  using namespace MfData::Packages;
  const int *nuzf1, *nuzf2, *nuzf3, *nuzf4;
  int iuzfopt, ietflg;
  MfPackage* a_pSP=GetPackage();
  if (a_pSP->GetField(UZFpack::NUZF1, &nuzf1) && nuzf1 &&
      a_pSP->GetField(UZFpack::NUZF2, &nuzf2) && nuzf2 &&
      a_pSP->GetField(UZFpack::NUZF3, &nuzf3) && nuzf3 &&
      a_pSP->GetField(UZFpack::NUZF4, &nuzf4) && nuzf4)
  {
    GetGlobal()->GetIntVar(UZFpack::IUZFOPT, iuzfopt);
    GetGlobal()->GetIntVar(UZFpack::IETFLG, ietflg);
    CStr line;
    CStr spStr;
    spStr.Format("SP %d", GetGlobal()->GetCurrentPeriod());
    std::map<int, CStr> mapDesc;
    mapDesc[9]  = " 9. NUZF1              " + spStr;
    mapDesc[10] = "10. FINF(NCOL, NROW)   " + spStr;
    mapDesc[11] = "11. NUZF2              " + spStr;
    mapDesc[12] = "12. PET(NCOL, NROW)    " + spStr;
    mapDesc[13] = "13. NUZF3              " + spStr;
    mapDesc[14] = "14. EXTDP(NCOL, NROW)  " + spStr;
    mapDesc[15] = "15. NUZF4              " + spStr;
    mapDesc[16] = "16. EXTWC(NCOL, NROW)  " + spStr;

    line.Format("%5d", *nuzf1);
    AddToStoredLinesDesc(line, mapDesc[9]);

    MfPackage* p = GetGlobal()->GetPackage(ARR_UZF_RCH);
    if (p && *nuzf1 >= 0)
    {
      line = p->StringsToWrite().front();
      AddToStoredLinesDesc(line, mapDesc[10]);
      if (MfExportUtil::ArrayWriteNextLineInternal(GetNative(), line))
      {
        line = p->StringsToWrite()[1];
        AddToStoredLinesDesc(line, "");
      }
      p->StringsToWrite().clear();
    }

    if (ietflg > 0)
    {
      line.Format("%5d", *nuzf2);
      AddToStoredLinesDesc(line, mapDesc[11]);

      p = GetGlobal()->GetPackage(ARR_UZF_ET);
      if (p && *nuzf2 >= 0)
      {
        line = p->StringsToWrite().front();
        AddToStoredLinesDesc(line, mapDesc[12]);
        if (MfExportUtil::ArrayWriteNextLineInternal(GetNative(), line))
        {
          line = p->StringsToWrite()[1];
          AddToStoredLinesDesc(line, "");
        }
        p->StringsToWrite().clear();
      }

      line.Format("%5d", *nuzf3);
      AddToStoredLinesDesc(line, mapDesc[13]);

      p = GetGlobal()->GetPackage(ARR_UZF_EXT);
      if (p && *nuzf3 >= 0)
      {
        line = p->StringsToWrite().front();
        AddToStoredLinesDesc(line, mapDesc[14]);
        if (MfExportUtil::ArrayWriteNextLineInternal(GetNative(), line))
        {
          line = p->StringsToWrite()[1];
          AddToStoredLinesDesc(line, "");
        }
        p->StringsToWrite().clear();
      }

      if (iuzfopt > 0)
      {
        line.Format("%5d", *nuzf4);
        AddToStoredLinesDesc(line, mapDesc[15]);

        p = GetGlobal()->GetPackage(ARR_UZF_EXTWC);
        if (p && *nuzf4 >= 0)
        {
          line = p->StringsToWrite().front();
          AddToStoredLinesDesc(line, mapDesc[16]);
          if (MfExportUtil::ArrayWriteNextLineInternal(GetNative(), line))
          {
            line = p->StringsToWrite()[1];
            AddToStoredLinesDesc(line, "");
          }
          p->StringsToWrite().clear();
        }
      }
    }

    if (GetH5Flag())
    {
      std::vector<int> vDat(4, 1);
      vDat[0] = *nuzf1 < 0 ? 1 : 0;
      if (ietflg)
      {
        vDat[1] = *nuzf2 < 0 ? 1 : 0;
        vDat[2] = *nuzf3 < 0 ? 1 : 0;
        vDat[3] = *nuzf4 < 0 ? 1 : 0;
      }
      H5UseLastWriter h(this);
      h.WriteData(vDat);
    }
  }
} // NativeExpUzf::Lines9to16


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData\MfExport\private\Native\NativeExpUzf.t.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpUzfT::setUp ()
{
  m_p = NULL;
  Mf2kNative* n = new Mf2kNative;
  MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  MfPackage* dis = new MfPackage(Packages::UZF);
  NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  m_p = dynamic_cast<NativeExpUzf*>(p);
} // NativeExpUzfT::setUp
//------------------------------------------------------------------------------
void NativeExpUzfT::tearDown ()
{
  m_p->UnitTestingDeletePointers();
  delete(m_p);
} // NativeExpUzfT::tearDown
//------------------------------------------------------------------------------
void NativeExpUzfT::testCreateClass ()
{
  TS_ASSERT(m_p);
} // NativeExpUzfT::testCreateClass

#endif
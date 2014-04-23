//------------------------------------------------------------------------------
// FILE      NativeExpSub.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpSub.h>

#include <private\MfData\MfExport\private\Mf2kNative.h>
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
NativeExpSub::NativeExpSub ()
{
} // MfNativeExpSub::MfNativeExpSub
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpSub::~NativeExpSub ()
{
} // MfNativeExpSub::~MfNativeExpSub
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpSub::Export ()
{
  if (Packages::SUB == GetPackage()->PackageName())
  {
    Lines1to3();
    Lines4to14();

    WriteComments();
    WriteStoredLines();
  }
  else
  {
    if (Packages::SUBLine15 == GetPackage()->PackageName()) Line15();
    if (Packages::SUBLine16 == GetPackage()->PackageName()) Line16();

    TmpPackageNameChanger tmp(GetPackage(), Packages::SUB);
    WriteStoredLines();
  }

  return true;
} // MfNativeExpSub::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSub::Lines1to3 ()
{
  using namespace MfData::Packages;
  using util::ForElement;
  const int *isubcb(NULL), *isuboc(NULL), *nndb(NULL), *ndb(NULL), *nmz(NULL),
            *nn(NULL), *itmin(NULL), *idsave(NULL), *idrest(NULL),
            *ln(NULL), *ldn(NULL);
  const Real *ac1(NULL), *ac2(NULL), *dp(NULL);
  MfPackage* a_p = GetPackage();
  if (a_p->GetField(SUBpack::ISUBCB, &isubcb) && isubcb &&
      a_p->GetField(SUBpack::ISUBOC, &isuboc) && isuboc &&
      a_p->GetField(SUBpack::NNDB, &nndb) && nndb &&
      a_p->GetField(SUBpack::NDB, &ndb) && ndb &&
      a_p->GetField(SUBpack::NMZ, &nmz) && nmz &&
      a_p->GetField(SUBpack::NN, &nn) && nn &&
      a_p->GetField(SUBpack::AC1, &ac1) && ac1 &&
      a_p->GetField(SUBpack::AC2, &ac2) && ac2 &&
      a_p->GetField(SUBpack::ITMIN, &itmin) && itmin &&
      a_p->GetField(SUBpack::IDSAVE, &idsave) && idsave &&
      a_p->GetField(SUBpack::IDREST, &idrest) && idrest &&
      ((*nndb == 0) || (a_p->GetField(SUBpack::LN, &ln) && ln)) &&
      ((*ndb == 0) || (a_p->GetField(SUBpack::LDN, &ldn) && ldn &&
       a_p->GetField(SUBpack::DP, &dp) && dp)))
  {
    int w = util::RealWidth();
    int flg = STR_FULLWIDTH;
    CStr lin;
    lin.Format("%5d %5d %5d %5d %5d %5d %s %s %5d %5d %5d",
              *isubcb, *isuboc, *nndb, *ndb, *nmz, *nn,
              STR(*ac1,-1,w,flg), STR(*ac2,-1,w,flg),
              *itmin, *idsave, *idrest);
    CStr desc = " 1. ISUBCB ISUBOC NNDB NDB NMZ NN AC1 AC2 ITMIN IDSAVE IDREST";
    AddToStoredLinesDesc(lin, desc);

    lin = "";
    desc = " 2. LN(NNDB)";
    for (int i=0; i<*nndb; ++i)
    {
      CStr tmp; tmp.Format("%5d ", ln[i]);
      lin += tmp;
    }
    AddToStoredLinesDesc(lin, desc);

    lin = "";
    desc = " 3. LDN(NDB)";
    for (int i=0; i<*ndb; ++i)
    {
      CStr tmp; tmp.Format("%5d ", ldn[i]);
      lin += tmp;
    }
    AddToStoredLinesDesc(lin, desc);
  }
} // NativeExpSub::Line1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSub::Lines4to14 ()
{
  using namespace MfData::Packages;
  using util::ForElement;
  const int *nndb(NULL), *ndb(NULL), *nmz(NULL), *ln(NULL), *ldn(NULL);
  const Real *dp(NULL);
  MfPackage* a_p = GetPackage();
  if (a_p->GetField(SUBpack::NNDB, &nndb) && nndb &&
      a_p->GetField(SUBpack::NDB, &ndb) && ndb &&
      a_p->GetField(SUBpack::NMZ, &nmz) && nmz &&
      ((*nndb == 0) || (a_p->GetField(SUBpack::LN, &ln) && ln)) &&
      ((*ndb == 0) || (a_p->GetField(SUBpack::LDN, &ldn) && ldn &&
      a_p->GetField(SUBpack::DP, &dp) && dp)))
  {
    int w = util::RealWidth();
    int flg = STR_FULLWIDTH;
    CStr desc[15] = {"","","",""
                    ," 4. RNB(NCOL,NROW)    LAY"
                    ," 5. HC(NCOL,NROW)     LAY"
                    ," 6. Sfe(NCOL,NROW)    LAY"
                    ," 7. Sfv(NCOL,NROW)    LAY"
                    ," 8. COM(NCOL,NROW)    LAY"
                    ," 9. DP(NMZ,3) - Kv Sske Sskv"
                    ,"10. Dstart(NCOL,NROW) LAY"
                    ,"11. DHC(NCOL,NROW)    LAY"
                    ,"12. DCOM(NCOL,NROW)   LAY"
                    ,"13. DZ(NCOL,NROW)     LAY"
                    ,"14. NZ(NCOL,NROW)     LAY"
                    };
    WriteArrays(ARR_SUB_RNB, *ndb, ldn, desc[4], -1);
    for (int i=0; i<*nndb; ++i)
    {
      WriteArrays(ARR_SUB_HC, *nndb, ln, desc[5], i);
      WriteArrays(ARR_SUB_SFE, *nndb, ln, desc[6], i);
      WriteArrays(ARR_SUB_SFV, *nndb, ln, desc[7], i);
      WriteArrays(ARR_SUB_COM, *nndb, ln, desc[8], i);
    }
    for (int i=1; *ndb > 0 && i <= *nmz; ++i)
    {
      CStr tmp;
      tmp.Format("%s %s %s", STR(ForElement(dp,i,1,*nmz),-1,w,flg),
                             STR(ForElement(dp,i,2,*nmz),-1,w,flg),
                             STR(ForElement(dp,i,3,*nmz),-1,w,flg));
      AddToStoredLinesDesc(tmp, desc[9]);
    }
    for (int i=0; i<*ndb; ++i)
    {
      WriteArrays(ARR_SUB_DSTRT, *ndb, ldn, desc[10], i);
      WriteArrays(ARR_SUB_DHC, *ndb, ldn, desc[11], i);
      WriteArrays(ARR_SUB_DCOM, *ndb, ldn, desc[12], i);
      WriteArrays(ARR_SUB_DZ, *ndb, ldn, desc[13], i);
      WriteArrays(ARR_SUB_NZ, *ndb, ldn, desc[14], i);
    }
  }
} // NativeExpSub::Lines4to14
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSub::WriteArrays (const char* const a_name,
                                int a_num,
                                const int* a_lay,
                                const CStr& a_desc,
                                int a_idx)
{
  CStr desc;
  MfPackage* p = GetGlobal()->GetPackage(a_name);
  bool internalArrays = GetNative()->GetArraysInternal();
  if (p)
  {
    std::vector<CStr>& lines = p->StringsToWrite();
    if ((int)lines.size() < a_num) ASSERT(0);
    if (a_idx == -1)
    {
      for (size_t i=0; i<lines.size(); ++i)
      {
        desc.Format("%s %d", a_desc, a_lay[i]);
        AddToStoredLinesDesc(lines[i], desc);
        if (internalArrays && lines[i].Find("CONSTANT") == -1)
        {
          AddToStoredLinesDesc(lines[i], "");
          lines.erase(lines.begin()+i+1);
        }
      }
    }
    else
    {
      desc.Format("%s %d", a_desc, a_lay[a_idx]);
      AddToStoredLinesDesc(lines[a_idx], desc);
      if (internalArrays && lines[a_idx].Find("CONSTANT") == -1)
      {
        AddToStoredLinesDesc(lines[a_idx+1], "");
        lines.erase(lines.begin()+a_idx+1);
      }
    }
  }
} // NativeExpSub::WriteArrays
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSub::Line15 ()
{
  using namespace MfData::Packages;
  using util::ForElement;
  const int *isbocf, *isbocu;
  MfPackage* a_p=GetPackage();
  if (a_p->GetField(SUBpack::ISBOCF, &isbocf) && isbocf &&
      a_p->GetField(SUBpack::ISBOCU, &isbocu) && isbocu)
  {
    CStr line;
    CStr desc = "15. [Ifm1 Iun1 Ifm2 Iun2 Ifm3 Iun3 Ifm4 Iun4 Ifm5 Iun5 Ifm6 Iun6]";
    line.Format("%3d %3d %3d %3d %3d %3d %3d %3d %3d %3d %3d %3d",
        isbocf[0], isbocu[0],
        isbocf[1], isbocu[1],
        isbocf[2], isbocu[2],
        isbocf[3], isbocu[3],
        isbocf[4], isbocu[4],
        isbocf[5], isbocu[5]);
    AddToStoredLinesDesc(line, desc);
  }
} // NativeExpSub::Line15
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSub::Line16 ()
{
  using namespace MfData::Packages;
  using util::ForElement;
  const int *isp1, *isp2, *jts1, *jts2, *ifl;
  MfPackage* a_p=GetPackage();
  if (a_p->GetField(SUBpack::ISP1, &isp1) && isp1 &&
      a_p->GetField(SUBpack::ISP2, &isp2) && isp2 &&
      a_p->GetField(SUBpack::JTS1, &jts1) && jts1 &&
      a_p->GetField(SUBpack::JTS2, &jts2) && jts2 &&
      a_p->GetField(SUBpack::IFL, &ifl) && ifl)
  {
    CStr line;
    CStr desc = "16. [ISP1 ISP2 ITS1 ITS2 Ifl1 Ifl2 Ifl3 Ifl4 Ifl5 Ifl6 Ifl7 "
                 "Ifl8 Ifl9 Ifl10 Ifl11 Ifl12 Ifl13]";
    line.Format("%3d %3d %3d %3d %3d %3d %3d %3d %3d %3d %3d %3d %3d %3d %3d %3d %3d",
        *isp1, *isp2, *jts1, *jts2, ifl[0], ifl[1], ifl[2], ifl[3], ifl[4],
        ifl[5], ifl[6], ifl[7], ifl[8], ifl[9], ifl[10], ifl[11], ifl[12]);
    AddToStoredLinesDesc(line, desc);
  }
} // NativeExpSub::Line16


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData\MfExport\private\Native\NativeExpSub.t.h>

#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackStrings.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpSubT::setUp ()
{
  m_p = NULL;
  Mf2kNative* n = new Mf2kNative;
  MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  MfPackage* dis = new MfPackage(Packages::SUB);
  NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  m_p = dynamic_cast<NativeExpSub*>(p);
} // NativeExpSubT::setUp
//------------------------------------------------------------------------------
void NativeExpSubT::tearDown ()
{
  m_p->UnitTestingDeletePointers();
  delete(m_p);
} // NativeExpSubT::tearDown
//------------------------------------------------------------------------------
void NativeExpSubT::testCreateClass ()
{
  TS_ASSERT(m_p);
} // NativeExpSubT::testCreateClass

#endif
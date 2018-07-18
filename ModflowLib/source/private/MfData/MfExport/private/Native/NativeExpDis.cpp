//------------------------------------------------------------------------------
// FILE      MfNativeExpDis.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/MfData/MfExport/private/Native/NativeExpDis.h>

#include <sstream>

#include <private/MfData/MfGlobal.h>
#include <private/MfData/MfExport/private/CellNumbering.h>
#include <private/MfData/MfExport/private/Mf2kNative.h>
#include <private/MfData/MfExport/private/MfExportUtil.h>
#include <private/MfData/MfExport/private/Native/mf6/NativeExpMf6Dis.h>
#include <private/MfData/MfExport/private/Native/mf6/NativeExpMf6Tdis.h>
#include <private/MfData/MfExport/private/TxtExporter.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackFields.h>

using namespace MfData::Export;

//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpDis::NativeExpDis ()
{
} // MfNativeExpDis::MfNativeExpDis
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpDis::~NativeExpDis ()
{
} // MfNativeExpDis::~MfNativeExpDis
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpDis::OnSetData ()
{
  if (!GetGlobal()) return;
  m_nLay = GetGlobal()->NumLay();
  m_nSp = GetGlobal()->NumPeriods();
} // NativeExpDis::OnSetData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpDis::Export ()
{
  Mf2kNative* n = GetNative();
  if (n && n->GetExportMf6())
  {
    // have to export when we have the IBOUND from BAS package
    //NativeExpMf6Dis dis(this);
    //dis.Export();
    NativeExpMf6Tdis tdis(this);
    tdis.Export();  
    return true;
  }

  AddToStoredLinesDesc(Line1(), Desc(1));
  AddToStoredLinesDesc(Line2(), Desc(2));
  Line3();
  Line4();
  Line5();
  Line6();
  AddToStoredLinesDesc(Line7(), Desc6and7(7));

  WriteComments();
  WriteStoredLines();

  // create cell numbering class
  if (GetNative()->GetExportMf6())
  {
    CellNumbering* cn = CellNumbering::New(GetGlobal());
    GetNative()->SetCellNumbering(cn);
  }

  return true;
} // MfNativeExpDis::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpDis::Line1 ()
{
  MfGlobal *g = GetGlobal();
  CStr aStr;
  aStr.Format("%6d %6d %6d %6d %6d %6d ", g->NumLay(), g->NumRow(), g->NumCol(),
                                          g->NumPeriods(), g->TimeUnit(), 
                                          g->LengthUnit());
  return aStr;
} // NativeExpDis::Line1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpDis::Desc (int a_line, int a_lay/*-1*/)
{
  if (a_line < 1 || a_line > 8) return CStr();

  CStr strs[8] = {" 1. NLAY NROW NCOL NPER ITMUNI LENUNI",
                  " 2. LAYCBD(NLAY)",
                  " 3. DELR(NCOL)",
                  " 4. DELC(NROW)",
                  " 5. Top(NCOL,NROW)     LAY 1",
                  " 6. BOTM(NCOL,NROW)    LAY ",
                  " 7. PERLEN NSTP TSMULT Ss/tr    PERIOD ",
                  " 6. BOTM(NCOL,NROW)    LAY "
                 };
  std::stringstream ss;
  ss << strs[a_line-1];
  if (-1 != a_lay)
  {
    ss << a_lay;
    if (8 == a_line) ss << " Confining Bed";
  }
  return ss.str();
} // NativeExpDis::Desc
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpDis::Line2 ()
{
  using namespace MfData::Packages;
  const int* laycbd(0);
  std::stringstream ss;
  CStr aStr;
  if (GetPackage()->GetField(DisPack::LAYCBD, &laycbd) && laycbd)
  {
    for (int k=0; k<m_nLay; ++k)
    {
      m_layCbd.push_back(laycbd[k]);
      ss << laycbd[k] << " ";
    }
    aStr = ss.str();
    aStr.Trim();
  }
  else ASSERT(0);
  return aStr;
} // NativeExpDis::Line2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpDis::Line3 ()
{
  CStr str;
  MfPackage* p = GetGlobal()->GetPackage("DELR");
  if (!p)
  {
    ASSERT(0);
  }
  std::vector<CStr>& lines(p->StringsToWrite());
  ASSERT(!lines.empty());
  if (!lines.empty()) str = lines.front();
  AddToStoredLinesDesc(str, Desc(3));
  if (lines.size() > 1) AddToStoredLinesDesc(lines[1], "");
} // NativeExpDis::Line3
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpDis::Line4 ()
{
  CStr str;
  MfPackage* p = GetGlobal()->GetPackage("DELC");
  if (!p)
  {
    ASSERT(0);
  }
  std::vector<CStr>& lines(p->StringsToWrite());
  ASSERT(!lines.empty());
  if (!lines.empty()) str = lines.front();
  AddToStoredLinesDesc(str, Desc(4));
  if (lines.size() > 1) AddToStoredLinesDesc(lines[1], "");
} // NativeExpDis::Line4
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpDis::Line5 ()
{
  CStr rval;
  MfPackage* p = GetGlobal()->GetPackage(ARR_DIS_TOP);
  if (!p)
  {
    ASSERT(0);
    return;
  }
  std::vector<CStr>& lines(p->StringsToWrite());
  ASSERT(!lines.empty());
  if (!lines.empty()) rval = lines.front();
  AddToStoredLinesDesc(rval, Desc(5));
  if (lines.size() > 1) AddToStoredLinesDesc(lines[1], "");
} // NativeExpDis::Line5
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpDis::Line6 ()
{
  std::vector<CStr> rval, desc;
  MfPackage* p = GetGlobal()->GetPackage(ARR_DIS_BOT);
  if (!p)
  {
    ASSERT(0);
    return;
  }
  MfPackage* pCbd = GetGlobal()->GetPackage(ARR_DIS_VCB);

  try
  {
    int cnt(0);
    std::vector<CStr>& lines(p->StringsToWrite());
    for (int c=0, i=0; i<m_nLay; ++i)
    {
      rval.push_back(lines.at(c++));
      desc.push_back(Desc(6, i+1));
      if (MfExportUtil::ArrayWriteNextLineInternal(GetNative(), rval.back()))
      {
        rval.push_back(lines.at(c++));
        desc.push_back("");
      }

      if (pCbd && m_layCbd[i])
      {
        rval.push_back(pCbd->StringsToWrite()[cnt]);
        desc.push_back(Desc(8, i+1));
        ++cnt;
        if (MfExportUtil::ArrayWriteNextLineInternal(GetNative(), rval.back()))
        {
          rval.push_back(pCbd->StringsToWrite()[cnt]);
          desc.push_back("");
          ++cnt;
        }
      }
    }
  }
  catch (std::out_of_range&)
  {
    ASSERT(0);
  }

  AddToStoredLinesDesc(rval, desc);
} // NativeExpDis::Line6
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
std::vector<CStr> NativeExpDis::Desc6and7 (int a_)
{
  std::vector<CStr> rval;
  int count = m_nLay;
  if (7 == a_) count = m_nSp;
  for (int i=1; i<=count; ++i)
  {
    rval.push_back(Desc(a_, i));
    if (6 == a_ && m_layCbd[i-1])
    {
      rval.push_back(Desc(8, i));
    }
  }
  return rval;
} // NativeExpDis::Desc6
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
std::vector<CStr> NativeExpDis::Line7 ()
{
  using namespace MfData::Packages;
  std::vector<CStr> rval;
  const Real *perLen(0), *tsMult(0);
  const int *nstps(0), *issFlag(0);
  if (GetPackage()->GetField(DisPack::PERLEN, &perLen) && perLen &&
      GetPackage()->GetField(DisPack::NSTP, &nstps) && nstps &&
      GetPackage()->GetField(DisPack::TSMULT, &tsMult) && tsMult &&
      GetPackage()->GetField(DisPack::ISSFLG, &issFlag) && issFlag)
  {
    CStr str;
    for (int i=0; i<m_nSp; ++i)
    {
      str.Format("%s %d %s %s", STR(perLen[i]), nstps[i], STR(tsMult[i]),
                 issFlag[i] ? "Ss":"Tr");
      if (!issFlag[i])
        GetNative()->GetExp()->AtLeastOneTransientSPExists() = true;
      else
        GetNative()->GetExp()->SetOfSteadyStateStressPeriods().insert(i+1);
      rval.push_back(str);
    }
  }
  return rval;;
} // NativeExpDis::Line7

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private/MfData/MfExport/private/Native/NativeExpDis.t.h>

#include <private/MfData/MfExport/private/Mf2kNative.h>
#include <private/MfData/MfExport/private/Native/NativeUtil.h>
#include <private/MfData/MfGlobal.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackStrings.h>
#include <private/MfLibAsserts.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpDisT::setUp ()
{
  m_p = NULL;
  Mf2kNative* n = new Mf2kNative;
  MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  MfPackage* dis = new MfPackage(Packages::DIS);
  NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  m_p = dynamic_cast<NativeExpDis*>(p);
} // NativeExpDisT::setUp
//------------------------------------------------------------------------------
void NativeExpDisT::tearDown ()
{
  m_p->UnitTestingDeletePointers();
  delete(m_p);
} // NativeExpDisT::tearDown
//------------------------------------------------------------------------------
void NativeExpDisT::testCreateClass ()
{
  TS_ASSERT(m_p);
} // NativeExpDisT::testCreateClass
//------------------------------------------------------------------------------
void NativeExpDisT::testLine1 ()
{
  CStr base = "     3     10     15      2      3      2 ";
  CStr str = m_p->Line1();
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpDisT::testLine1
//------------------------------------------------------------------------------
void NativeExpDisT::testDesc ()
{
  CStr base = " 1. NLAY NROW NCOL NPER ITMUNI LENUNI";
  CStr str = m_p->Desc(1);
  TS_ASSERT_EQUALS2(base, str);
  base = " 2. LAYCBD(NLAY)";
  str = m_p->Desc(2);
  TS_ASSERT_EQUALS2(base, str);
  base = " 3. DELR(NCOL)";
  str = m_p->Desc(3);
  TS_ASSERT_EQUALS2(base, str);
  base = " 4. DELC(NROW)";
  str = m_p->Desc(4);
  TS_ASSERT_EQUALS2(base, str);
  base = " 5. Top(NCOL,NROW)     LAY 1";
  str = m_p->Desc(5);
  TS_ASSERT_EQUALS2(base, str);
  base = " 6. BOTM(NCOL,NROW)    LAY 1";
  str = m_p->Desc(6, 1);
  TS_ASSERT_EQUALS2(base, str);
  base = " 6. BOTM(NCOL,NROW)    LAY 3";
  str = m_p->Desc(6, 3);
  TS_ASSERT_EQUALS2(base, str);
  base = " 7. PERLEN NSTP TSMULT Ss/tr    PERIOD 1";
  str = m_p->Desc(7, 1);
  TS_ASSERT_EQUALS2(base, str);
  base = " 7. PERLEN NSTP TSMULT Ss/tr    PERIOD 5";
  str = m_p->Desc(7, 5);
  TS_ASSERT_EQUALS2(base, str);
  base = " 6. BOTM(NCOL,NROW)    LAY 3 Confining Bed";
  str = m_p->Desc(8, 3);
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpDisT::testDesc
//------------------------------------------------------------------------------
void NativeExpDisT::testLine2 ()
{
  const int laycbd[3] = {0,1,2};
  m_p->GetPackage()->SetField(Packages::DisPack::LAYCBD, laycbd);
  CStr base = "0 1 2";
  CStr str = m_p->Line2();
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpDisT::testLine2
//------------------------------------------------------------------------------
void NativeExpDisT::testLine3 ()
{
} // NativeExpDisT::testLine3
//------------------------------------------------------------------------------
void NativeExpDisT::testLine4 ()
{
} // NativeExpDisT::testLine4
//------------------------------------------------------------------------------
void NativeExpDisT::testLine5 ()
{
} // NativeExpDisT::testLine5
//------------------------------------------------------------------------------
void NativeExpDisT::testLine6 ()
{
} // NativeExpDisT::testLine6
//------------------------------------------------------------------------------
void NativeExpDisT::testLine7 ()
{
  using namespace MfData::Packages;
  int nstps[2]={50,60},
      issFlag[2]={1,0};
  Real perLen[2]={5,6},
       tsMult[2]={7,8};
  MfPackage& p(*m_p->GetPackage());
  p.SetField(DisPack::PERLEN, perLen);
  p.SetField(DisPack::TSMULT, tsMult);
  p.SetField(DisPack::NSTP, nstps);
  p.SetField(DisPack::ISSFLG, issFlag);
  std::vector<CStr> strs = m_p->Line7();
  std::vector<CStr> base;
  base.push_back("5.0 50 7.0 Ss");
  base.push_back("6.0 60 8.0 Tr");
}

#endif
//------------------------------------------------------------------------------
// FILE      NativeExpBcf.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpBcf.h>

#include <sstream>

#include <private\MfData\MfGlobal.h>
#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\TxtExporter.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackFields.h>
#include <private\MfData\Packages\MfPackStrings.h>

using namespace MfData;
using namespace MfData::Export;

namespace
{
const CStr ARR_TRPY   = "COLUMN TO ROW ANISOTROPY";
} // unnamed namespace
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpBcf::NativeExpBcf () :
  m_nLay(0)
, m_lineCnt(10, 0)
, m_internalArrays(0)
{
} // MfNativeExpBcf::MfNativeExpBcf
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpBcf::~NativeExpBcf ()
{
} // MfNativeExpBcf::~MfNativeExpBcf
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpBcf::Export ()
{
  AddToStoredLinesDesc(Line1(), Desc(1));
  AddToStoredLinesDesc(Line2(), Desc(2));
  AddToStoredLinesDesc(Line3(), Desc(3));
  for (int i=1; i<=m_nLay; ++i)
  {
    if (CanWriteLine(4, i)) Line4to9(4,i);
    if (CanWriteLine(5, i)) Line4to9(5,i);
    if (CanWriteLine(6, i)) Line4to9(6,i);
    if (CanWriteLine(7, i)) Line4to9(7,i);
    if (CanWriteLine(8, i)) Line4to9(8,i);
    if (CanWriteLine(9, i)) Line4to9(9,i);
  }

  WriteComments();
  WriteStoredLines();
  return true;
} // MfNativeExpBcf::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpBcf::OnSetData ()
{
  m_nLay = GetGlobal()->NumLay();
  m_internalArrays = GetNative()->GetArraysInternal();
} // NativeExpBcf::OnSetData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpBcf::Desc (int a_line, int a_lay)
{
  CStr desc[9] = {" 1. IBCFCB HDRY IWDFLG WETFCT IWETIT IHDWET",
                  " 2. Ltype(NLAY)",
                  " 3. TRPY(NLAY)",
                  " 4. Sf1(NCOL,NROW)     LAY ",
                  " 5. Tran(NCOL,NROW)    LAY ",
                  " 6. HY(NCOL,NROW)      LAY ",
                  " 7. Vcont(NCOL,NROW)   LAY ",
                  " 8. Sf2(NCOL,NROW)     LAY ",
                  " 9. WETDRY(NCOL,NROW)  LAY "
                 };
  std::stringstream ss;
  ss << desc[a_line-1];
  if (-1 != a_lay)
  {
    ss << a_lay;
  }
  return ss.str();
} // MfNativeExpBcf::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpBcf::Line1 ()
{
  CStr rval;
  const Real *hdry(0), *wetfct(0);
  const int  *ibcfcb(0), *iwdflg(0), *iwetit(0), *ihdwet(0);
  MfPackage* p = GetPackage();
  if (!p->GetField(Packages::BCFpack::IBCFCB, &ibcfcb) || !ibcfcb ||
      !p->GetField(Packages::BCFpack::HDRY, &hdry) || !hdry ||
      !p->GetField(Packages::BCFpack::WETFCT, &wetfct) || !wetfct ||
      !p->GetField(Packages::BCFpack::IWDFLG, &iwdflg) || !iwdflg ||
      !p->GetField(Packages::BCFpack::IWETIT, &iwetit) || !iwetit ||
      !p->GetField(Packages::BCFpack::IHDWET, &ihdwet) || !ihdwet)
    return rval;

  // IBCFCB HDRY IWDFLG WETFCT IWETIT IHDWET
  rval.Format("%d %s %d %s %d %d ", *ibcfcb, STR(*hdry), *iwdflg, STR(*wetfct),
                                    *iwetit, *ihdwet);
  return rval;
} // NativeExpBcf::Line1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpBcf::Line2 ()
{
  CStr rval;
  const int  *layavg(0), *laycon(0);
  MfPackage* p = GetPackage();
  if (!p->GetField(Packages::BCFpack::LAYAVG, &layavg) || !layavg ||
      !p->GetField(Packages::BCFpack::LAYCON, &laycon) || !laycon)
    return rval;

  CStr str, str1;
  for (int i=0; i<m_nLay; ++i)
  {
    if (str1.length() >= 78)
    {
      rval += str1;
      rval += "\n";
      str1 = "";
    }
    str.Format("%d%d ", layavg[i], laycon[i]);
    str1 += str;
  }
  rval += str1;
  return rval;
} // NativeExpBcf::Line2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpBcf::Line3 ()
{
  CStr rval;
  MfPackage* p = GetGlobal()->GetPackage(ARR_TRPY);
  if (!p || p->StringsToWrite().empty())
  {
    ASSERT(0);
    return rval;
  }
  rval = p->StringsToWrite().front();
  return rval;
} // NativeExpBcf::Line3
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpBcf::Line4to9 (int a_line, int a_lay)
{
  std::vector<CStr> rval;
  CStr arrName = GetArrayName(a_line);
  MfPackage* p = GetGlobal()->GetPackage(arrName);
  if (!p || p->StringsToWrite().empty())
  {
    ASSERT(0);
  }
  std::vector<CStr>& strs(p->StringsToWrite());
  CStr s = strs[m_lineCnt[a_line]];
  rval.push_back(s);
  m_lineCnt[a_line]++;
  if (m_internalArrays && s.Find("CONSTANT") == -1)
  {
    rval.push_back(strs[m_lineCnt[a_line]]);
    m_lineCnt[a_line]++;
  }
  std::vector<CStr> desc;
  desc.push_back(Desc(a_line, a_lay));
  if (rval.size() > 1) desc.push_back("");
  AddToStoredLinesDesc(rval, desc);
} // NativeExpBcf::Line4
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpBcf::CanWriteLine (int a_line, int a_lay)
{
  int i(a_lay-1);
  const int  *laycon(0), *iwdflg(0);
  MfPackage* p = GetPackage();
  if (!p->GetField(Packages::BCFpack::LAYCON, &laycon) || !laycon ||
      !p->GetField(Packages::BCFpack::IWDFLG, &iwdflg) || !iwdflg)
    return false;
  if (4 == a_line) // Sf1
  {
    if (GetNative()->GetExp()->AtLeastOneTransientSPExists()) return true;
  }
  else if (5 == a_line) // Tran
  {
    if (0 == laycon[i] || 2 == laycon[i]) return true;
  }
  else if (6 == a_line) // HY
  {
    if (1 == laycon[i] || 3 == laycon[i]) return true;
  }
  else if (7 == a_line) // VCONT
  {
    if (a_lay < m_nLay) return true;
  }
  else if (8 == a_line) // SF2
  {
    if (GetNative()->GetExp()->AtLeastOneTransientSPExists() &&
        (2 == laycon[i] || 3 == laycon[i]))
      return true;
  }
  else if (9 == a_line)
  {
    if (0 != *iwdflg && (1 == laycon[i] || 3 == laycon[i])) return true;
  }
  return false;
} // NativeExpBcf::CanWriteLine
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpBcf::GetArrayName (int a_line)
{
  if (4 == a_line) return ARR_BCF_SF1;
  else if (5 == a_line) return ARR_BCF_TRAN;
  else if (6 == a_line) return ARR_LPF_HK;  // have to use LPF to find package
  else if (7 == a_line) return ARR_BCF_VCONT;
  else if (8 == a_line) return ARR_BCF_SF2;
  else if (9 == a_line) return ARR_LPF_WET; // have to use LPF to find package
  return CStr();
} // NativeExpBcf::GetArrayName


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData\MfExport\private\Native\NativeExpBcf.t.h>

#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackStrings.h>
#include <private/MfLibAsserts.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpBcfT::setUp ()
{
  m_p = NULL;
  Mf2kNative* n = new Mf2kNative;
  MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  MfPackage* dis = new MfPackage(Packages::BCF);
  NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  m_p = dynamic_cast<NativeExpBcf*>(p);
} // NativeExpBcfT::setUp
//------------------------------------------------------------------------------
void NativeExpBcfT::tearDown ()
{
  m_p->UnitTestingDeletePointers();
  delete(m_p);
} // NativeExpBcfT::tearDown
//------------------------------------------------------------------------------
void NativeExpBcfT::testCreateClass ()
{
  TS_ASSERT(m_p);
} // NativeExpBcfT::testCreateClass
//------------------------------------------------------------------------------
void NativeExpBcfT::testDesc ()
{
  CStr base = " 1. IBCFCB HDRY IWDFLG WETFCT IWETIT IHDWET";
  CStr str = m_p->Desc(1);
  TS_ASSERT_EQUALS2(base, str);
  base = " 2. Ltype(NLAY)";
  str = m_p->Desc(2);
  TS_ASSERT_EQUALS2(base, str);
  base = " 3. TRPY(NLAY)";
  str = m_p->Desc(3);
  TS_ASSERT_EQUALS2(base, str);
  base = " 4. Sf1(NCOL,NROW)     LAY 2";
  str = m_p->Desc(4, 2);
  TS_ASSERT_EQUALS2(base, str);
  base = " 5. Tran(NCOL,NROW)    LAY 2";
  str = m_p->Desc(5, 2);
  TS_ASSERT_EQUALS2(base, str);
  base = " 6. HY(NCOL,NROW)      LAY 2";
  str = m_p->Desc(6, 2);
  TS_ASSERT_EQUALS2(base, str);
  base = " 7. Vcont(NCOL,NROW)   LAY 2";
  str = m_p->Desc(7, 2);
  TS_ASSERT_EQUALS2(base, str);
  base = " 8. Sf2(NCOL,NROW)     LAY 2";
  str = m_p->Desc(8, 2);
  TS_ASSERT_EQUALS2(base, str);
  base = " 9. WETDRY(NCOL,NROW)  LAY 2";
  str = m_p->Desc(9, 2);
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpBcfT::testDesc
//------------------------------------------------------------------------------
void NativeExpBcfT::testLine1 ()
{
  Real hdry((Real)-888), wetfct((Real)1.23);
  int  ibcfcb(1), iwdflg(2), iwetit(3), ihdwet(4);
  MfPackage* p = m_p->GetPackage();
  p->SetField(Packages::BCFpack::IBCFCB, &ibcfcb);
  p->SetField(Packages::BCFpack::HDRY, &hdry);
  p->SetField(Packages::BCFpack::WETFCT, &wetfct);
  p->SetField(Packages::BCFpack::IWDFLG, &iwdflg);
  p->SetField(Packages::BCFpack::IWETIT, &iwetit);
  p->SetField(Packages::BCFpack::IHDWET, &ihdwet);
  CStr base = "1 -888.0 2 1.23 3 4 ";
  CStr str = m_p->Line1();
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpBcfT::testLine1
//------------------------------------------------------------------------------
void NativeExpBcfT::testLine2 ()
{
  int nlay(31),
      layavg[31]={0,1,2,3,0,1,2,3,0,1,2,3,0,1,2,3,0,1,2,3,0,1,2,3,0,1,2,3,0,1,2},
      laycon[31]={3,2,1,0,3,2,1,0,3,2,1,0,3,2,1,0,3,2,1,0,3,2,1,0,3,2,1,0,3,2,1};
  MfPackage* p = m_p->GetPackage();
  p->SetField(Packages::BCFpack::LAYAVG, layavg);
  p->SetField(Packages::BCFpack::LAYCON, laycon);
  m_p->m_nLay = nlay;
  CStr base = "03 12 21 30 03 12 21 30 03 12 21 30 03 12 21 30 03 12 21 30 03 12 21 30 03 12 \n21 30 03 12 21 ";
  CStr str = m_p->Line2();
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpBcfT::testLine2
//------------------------------------------------------------------------------
void NativeExpBcfT::testLine3 ()
{
  MfPackage p1(ARR_TRPY);
  m_p->GetGlobal()->AddPackage(&p1);
  MfPackage* p = m_p->GetGlobal()->GetPackage(ARR_TRPY);
  p->StringsToWrite().push_back("CONSTANT 1.0");
  CStr base = "CONSTANT 1.0";
  CStr str = m_p->Line3();
  TS_ASSERT_EQUALS2(base, str);

  p->StringsToWrite().clear();
  p->StringsToWrite().push_back("OPEN/CLOSE model_array_COLUMN_TO_ROW_ANISOTROPY.txt 1.0 10G12.5");
  str = m_p->Line3();
  base = "OPEN/CLOSE model_array_COLUMN_TO_ROW_ANISOTROPY.txt 1.0 10G12.5";
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpBcfT::testLine3
//------------------------------------------------------------------------------
void NativeExpBcfT::testCanWriteLine ()
{
  int  laycon[3]={3,2,1}, iwdflg(0);
  MfPackage* p = m_p->GetPackage();
  p->SetField(Packages::BCFpack::LAYCON, laycon);
  p->SetField(Packages::BCFpack::IWDFLG, &iwdflg);
  m_p->OnSetData();
  // Sf1
  TS_ASSERT(!m_p->CanWriteLine(4, 1));
  m_p->GetNative()->GetExp()->AtLeastOneTransientSPExists() = true;
  TS_ASSERT(m_p->CanWriteLine(4, 1));
  m_p->GetNative()->GetExp()->AtLeastOneTransientSPExists() = false;

  // TRAN
  TS_ASSERT(!m_p->CanWriteLine(5, 1));
  TS_ASSERT(m_p->CanWriteLine(5, 2));
  laycon[0] = 0;
  TS_ASSERT(m_p->CanWriteLine(5, 1));
  laycon[0] = 3;

  // HY
  TS_ASSERT(m_p->CanWriteLine(6, 1));
  TS_ASSERT(!m_p->CanWriteLine(6, 2));
  TS_ASSERT(m_p->CanWriteLine(6, 3));

  // VCONT
  TS_ASSERT(m_p->CanWriteLine(7, 1));
  TS_ASSERT(m_p->CanWriteLine(7, 2));
  TS_ASSERT(!m_p->CanWriteLine(7, 3));

  // Sf2
  TS_ASSERT(!m_p->CanWriteLine(8, 1));
  m_p->GetNative()->GetExp()->AtLeastOneTransientSPExists() = true;
  TS_ASSERT(m_p->CanWriteLine(8, 1));
  TS_ASSERT(m_p->CanWriteLine(8, 2));
  TS_ASSERT(!m_p->CanWriteLine(8, 3));
  m_p->GetNative()->GetExp()->AtLeastOneTransientSPExists() = true;

  // WETDRY
  TS_ASSERT(!m_p->CanWriteLine(9, 1));
  iwdflg = 1;
  TS_ASSERT(m_p->CanWriteLine(9, 1));
  TS_ASSERT(!m_p->CanWriteLine(9, 2));
  TS_ASSERT(m_p->CanWriteLine(9, 3));
} // NativeExpBcfT::testCanWriteLine
//------------------------------------------------------------------------------
void NativeExpBcfT::testGetArrayName ()
{
  CStr str;
  TS_ASSERT_EQUALS2(ARR_BCF_SF1, m_p->GetArrayName(4));
  TS_ASSERT_EQUALS2(ARR_BCF_TRAN, m_p->GetArrayName(5));
  TS_ASSERT_EQUALS2(ARR_LPF_HK, m_p->GetArrayName(6));
  TS_ASSERT_EQUALS2(ARR_BCF_VCONT, m_p->GetArrayName(7));
  TS_ASSERT_EQUALS2(ARR_BCF_SF2, m_p->GetArrayName(8));
  TS_ASSERT_EQUALS2(ARR_LPF_WET, m_p->GetArrayName(9));
  TS_ASSERT_EQUALS2(str, m_p->GetArrayName(3));
  TS_ASSERT_EQUALS2(str, m_p->GetArrayName(10));
} // NativeExpBcfT::testLine4
//------------------------------------------------------------------------------
void NativeExpBcfT::testLine4 ()
{
} // NativeExpBcfT::testLine4
//------------------------------------------------------------------------------
void NativeExpBcfT::testLine5 ()
{
} // NativeExpBcfT::testLine5
//------------------------------------------------------------------------------
void NativeExpBcfT::testLine6 ()
{
} // NativeExpBcfT::testLine6
//------------------------------------------------------------------------------
void NativeExpBcfT::testLine7 ()
{
} // NativeExpBcfT::testLine7
//------------------------------------------------------------------------------
void NativeExpBcfT::testLine8 ()
{
} // NativeExpBcfT::testLine8
//------------------------------------------------------------------------------
void NativeExpBcfT::testLine9 ()
{
} // NativeExpBcfT::testLine9

#endif
//------------------------------------------------------------------------------
// FILE      NativeExpBas.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/MfData/MfExport/private/Native/NativeExpBas.h>

#include <sstream>

#include <private/MfData/MfExport/private/Mf2kNative.h>
#include <private/MfData/MfExport/private/MfExportUtil.h>
#include <private/MfData/MfGlobal.h>
#include <private/MfData/MfExport/private/MfExporterImpl.h>
#include <private/MfData/MfExport/private/Native/mf6/NativeExpMf6Dis.h>
#include <private/MfData/MfExport/private/Native/mf6/NativeExpMf6Disu.h>
#include <private/MfData/MfExport/private/Native/mf6/NativeExpMf6Ic.h>
#include <private/MfData/MfExport/private/Native/NativeExpDis.h>
#include <private/MfData/MfExport/private/Native/NativeExpDisu.h>
#include <private/MfData/MfPackageUtil.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackFields.h>
#include <private/MfData/Packages/MfPackStrings.h>
using namespace MfData::Export;

const CStr ARRSTR_STRT=ARR_BAS_SHEAD;

//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpBas::NativeExpBas ()
{
} // MfNativeExpBas::MfNativeExpBas
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpBas::~NativeExpBas ()
{
} // MfNativeExpBas::~MfNativeExpBas
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpBas::OnSetData ()
{
  if (!GetGlobal()) return;
  m_nLay = GetGlobal()->NumLay();
} // NativeExpBas::OnSetData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpBas::Export ()
{
  Mf2kNative* n = GetNative();
  if (n && n->GetExportMf6())
  {
    MfGlobal* g = GetGlobal();
    if (!g->Unstructured())
    {
      MfPackage* p = GetGlobal()->GetPackage(MfData::Packages::DIS);
      if (p)
      {
        NativeExpDis di;
        di.SetData(GetNative(), GetGlobal(), p);
        NativeExpMf6Dis dis(&di);
        dis.Export();
      }
    }
    else
    {
      MfPackage* p = GetGlobal()->GetPackage(MfData::Packages::DISU);
      if (p)
      {
        NativeExpDisu du;
        du.SetData(GetNative(), GetGlobal(), p);
        NativeExpMf6Disu disu(&du);
        disu.Export();
      }
    }

    NativeExpMf6Ic ic(this);
    ic.Export();
    return true;
  }
  AddToStoredLinesDesc(Line1(), Desc(1));
  Line2();
  AddToStoredLinesDesc(Line3(), Desc(3));
  Line4();

  MfPackage *pOld = GetPackage(), p(MfData::Packages::BAS6);
  const char *heading1(nullptr), *heading2(nullptr);
//  pOld->GetField("HEADNG1", &heading1);
//  pOld->GetField("HEADNG2", &heading2);
  CStr comment;
  MfData::Packages::GetComments(MfData::Packages::BAS, comment);
  if (heading1)
  {
    if (!comment.IsEmpty()) comment += "\n";
    comment += heading1;
  }
  if (heading2)
  {
    if (!comment.IsEmpty()) comment += "\n";
    comment += heading2;
  }
  if (!comment.IsEmpty())
  {
    MfData::Packages::Comment(MfData::Packages::BAS6, comment);
  }
  SetData(GetNative(), GetGlobal(), &p);
  AddToStoredLinesDesc(pOld->StringsToWrite(), pOld->StringDescriptions());
  WriteComments();
  WriteStoredLines();
  SetData(GetNative(), GetGlobal(), pOld);
  return true;
} // MfNativeExpBas::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpBas::Line1 ()
{
  CStr opt;
  const char *cptr;
  if (!GetPackage()->GetField(Packages::BASpack::OPT, &cptr) || !cptr)
    return opt;

  opt = cptr;
  opt.ToUpper();
  opt.Replace("NO OPTIONS", "");
  opt.Trim();
  if (opt.Find("FREE") == -1)
  {
    if (!opt.IsEmpty()) opt += " ";
    opt += "FREE";
  }
  return opt;
} // NativeExpBas::Line1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpBas::Desc (int a_line, int a_lay/*-1*/)
{
  if (a_line < 1 || a_line > 4) return CStr();

  CStr strs[4] = {" 1. Options",
                  " 2. IBOUND(NCOL,NROW) LAY ",
                  " 3. HNOFLO",
                  " 4. STRT(NCOL,NROW)   LAY "
                  };
  std::stringstream ss;
  ss << strs[a_line-1];
  if (-1 != a_lay)
  {
    ss << a_lay;
  }
  return ss.str();
} // NativeExpBas::Desc
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpBas::Line2 ()
{
  std::vector<CStr> rval;
  MfPackage* p = GetGlobal()->GetPackage(ARR_BAS_IBND);
  if (!p)
  {
    ASSERT(0);
    return;
  }
  ArrayToFile(p, 2);
} // NativeExpBas::Line2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpBas::Line3 ()
{
  CStr rval;
  const Real* hNoflo;
  if (!GetPackage()->GetField(Packages::BASpack::HNOFLO, &hNoflo) || !hNoflo)
    return rval;
  rval = STR(*hNoflo);
  return rval;
} // NativeExpBas::Line3
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpBas::Line4 ()
{
  MfPackage* p = GetGlobal()->GetPackage(ARRSTR_STRT);
  if (!p)
  {
    ASSERT(0);
    return;
  }
  ArrayToFile(p, 4);
} // NativeExpBas::Line4
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpBas::ArrayToFile (MfData::MfPackage* p, int a_line)
{
  std::vector<CStr>& lines(p->StringsToWrite());
  int lay = 1;
  for (size_t i=0; i<lines.size(); ++i)
  {
    AddToStoredLinesDesc(lines.at(i), Desc(a_line, lay));
    lay++;
    if (MfExportUtil::ArrayWriteNextLineInternal(GetNative(), lines[i]))
    {
      i++;
      AddToStoredLinesDesc(lines[i], "");
    }
  }
} // NativeExpBas::ArrayToFile
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
int NativeExpBas::NumLines (int a_line)
{
  int rval = 0;
  MfPackage* p(0);
  if (2 == a_line)
  {
    p = GetGlobal()->GetPackage(ARR_BAS_IBND);
  }
  else if (4 == a_line)
  {
    p = GetGlobal()->GetPackage(ARRSTR_STRT);
  }
  else return rval;

  if (!p)
  {
    ASSERT(0);
    return rval;
  }
  rval = (int)p->StringsToWrite().size();
  return rval;
} // NativeExpBas::NumLines

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private/MfData/MfExport/private/Native/NativeExpBas.t.h>

#include <private/MfData/MfExport/private/Mf2kNative.h>
#include <private/MfData/MfExport/private/Native/NativeUtil.h>
#include <private/MfData/MfGlobal.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackStrings.h>
#include <private/MfLibAsserts.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpBasT::setUp ()
{
  m_p = NULL;
  Mf2kNative* n = new Mf2kNative;
  MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  MfPackage* dis = new MfPackage(Packages::BAS);
  NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  m_p = dynamic_cast<NativeExpBas*>(p);
} // NativeExpBasT::setUp
//------------------------------------------------------------------------------
void NativeExpBasT::tearDown ()
{
  m_p->UnitTestingDeletePointers();
  delete(m_p);
} // NativeExpBasT::tearDown
//------------------------------------------------------------------------------
void NativeExpBasT::testCreateClass ()
{
  TS_ASSERT(m_p);
} // NativeExpBasT::testCreateClass
//------------------------------------------------------------------------------
void NativeExpBasT::testLine1 ()
{
  char c[199];
  strcpy_s(c, "NO options");
  m_p->GetPackage()->SetField(Packages::BASpack::OPT, c);
  CStr base = "FREE";
  CStr str = m_p->Line1();
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpBasT::testCreateClass
//------------------------------------------------------------------------------
void NativeExpBasT::testDesc ()
{
  CStr base = " 1. Options";
  CStr str = m_p->Desc(1);
  TS_ASSERT_EQUALS2(base, str);
  base = " 2. IBOUND(NCOL,NROW) LAY 1";
  str = m_p->Desc(2, 1);
  TS_ASSERT_EQUALS2(base, str);
  base = " 2. IBOUND(NCOL,NROW) LAY 2";
  str = m_p->Desc(2, 2);
  TS_ASSERT_EQUALS2(base, str);
  base = " 3. HNOFLO";
  str = m_p->Desc(3);
  TS_ASSERT_EQUALS2(base, str);
  base = " 4. STRT(NCOL,NROW)   LAY 3";
  str = m_p->Desc(4, 3);
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpBasT::testCreateClass
//------------------------------------------------------------------------------
void NativeExpBasT::testLine2 ()
{
  MfPackage p1(ARR_BAS_IBND);
  m_p->GetGlobal()->AddPackage(&p1);
  MfPackage* p = m_p->GetGlobal()->GetPackage(ARR_BAS_IBND);
  std::vector<CStr>& l(p->StringsToWrite());
  l.push_back("CONSTANT 1");
  l.push_back("CONSTANT 2");
  l.push_back("CONSTANT 3");
  std::vector<CStr> base = l;
}
//------------------------------------------------------------------------------
void NativeExpBasT::testLine3 ()
{
  Real hNoflow = -1234;
  m_p->GetPackage()->SetField(Packages::BASpack::HNOFLO, &hNoflow);
  CStr base = "-1234.0";
  CStr str = m_p->Line3();
  TS_ASSERT_EQUALS2(base, str);
}
//------------------------------------------------------------------------------
void NativeExpBasT::testLine4 ()
{
}
#endif
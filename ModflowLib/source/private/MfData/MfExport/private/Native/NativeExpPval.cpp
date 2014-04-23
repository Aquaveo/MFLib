//------------------------------------------------------------------------------
// FILE      NativeExpPval.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpPval.h>

#include <sstream>

#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfExport\private\MfExportUtil.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackFields.h>
#include <private\MfData\Packages\MfPackStrings.h>
#include <private\Parameters.h>
#include <private\Parameters\Param.h>
#include <private\Parameters\ParamList.h>

using namespace MfData::Export;
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpPval::NativeExpPval ()
{
} // MfNativeExpPval::MfNativeExpPval
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpPval::~NativeExpPval ()
{
} // MfNativeExpPval::~MfNativeExpPval
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpPval::Export ()
{
  if (Line2().empty()) return true;

  if (GetPackage()->StringsToWrite().empty())
  {
    AddToStoredLinesDesc(Line1(), Desc1());
    AddToStoredLinesDesc(Line2(), Desc2());
  }

  int var(0);
  if (GetGlobal()->GetIntVar("Write PVAL", var) && var)
  {
    CheckParameters();

    if (!GetPackage()->StringsToWrite().empty())
    {
      WriteComments();
      WriteStoredLines();
      GetGlobal()->SetIntVar("PVAL_Exported", 1);
    }
  }
  return true;
} // MfNativeExpPval::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpPval::Line1 ()
{
  using namespace MfData::Packages;
  CStr rval;
  std::vector<CStr> lines = Line2();
  rval.Format("%d", lines.size());
  return rval;
} // MfNativeExpPval::Line1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpPval::Desc1 ()
{
  CStr rval = " 1. NP";
  return rval;
} // MfNativeExpPval::Desc1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
std::vector<CStr> NativeExpPval::Line2()
{
  std::vector<CStr> rval;
  using namespace MfData::Packages;
  const int *nplist;
  const char *parnam;
  const Real *b;
  CStr aCStr;
  char myChar[11];
  myChar[10] = '\0';


  MfPackage* a_p = GetPackage();
  if (a_p->GetField(PVALpack::NPVAL, &nplist) && nplist &&
      a_p->GetField(PVALpack::PARNAM, &parnam) && parnam &&
      a_p->GetField(PVALpack::B, &b) && b)
  {
    for (int i = 0; i < *nplist; ++i)
    {
      for (int j=0; j<10; ++j) myChar[j] = parnam[(i*10)+j];
      CStr parnamStr = myChar;
      parnamStr.Trim();
      ConvertNameIfPilotPoint(parnamStr);
      MfExportUtil::InsertSingleQuotesInName(parnamStr);
      while (parnamStr.GetLength() < 12) parnamStr += " ";
      aCStr.Format("%s %s", parnamStr, STR(b[i]));
      rval.push_back(aCStr);
    }
  }
  return rval;
} // MfNativeExpPval::Line2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpPval::ConvertNameIfPilotPoint (CStr& a_name)
{
  if (ParamList::IsPilotParName(a_name, "HK", nullptr))
  {
    a_name.Replace("sc", "pp");
    a_name.Replace("v", "_");
  }
} // NativeExpPval::ConvertNameIfPilotPoint
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
std::vector<CStr> NativeExpPval::Desc2 ()
{
  std::vector<CStr> rval(Line2().size(), " 2. PARNAM Parval");
  return rval;
} // MfNativeExpPval::Desc2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpPval::CheckParameters ()
{
  std::vector<CStr>& line(GetPackage()->StringsToWrite()), line1(line);
  std::vector<CStr>& desc(GetPackage()->StringDescriptions()), desc1(desc);

  line.resize(0);
  desc.resize(0);
  CStr pname;
  for (size_t i=1; i<line1.size(); ++i)
  {
    std::stringstream ss;
    ss << line1[i];
    ss >> pname;
    if (SkipPar_Pval_Sen(pname)) continue;

    line.push_back(line1[i]);
    desc.push_back(desc1[i]);
  }
  if (line.empty()) return;
  pname.Format("%d", line.size());
  line.insert(line.begin(), pname);
  desc.insert(desc.begin(), desc1.front());
} // NativeExpPval::CheckParameters

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData\MfExport\private\Native\NativeExpPval.t.h>

#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackStrings.h>
#include <private/MfLibAsserts.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpPvalT::setUp ()
{
  m_p = NULL;
  Mf2kNative* n = new Mf2kNative;
  MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  MfPackage* dis = new MfPackage(Packages::PVAL);
  NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  m_p = dynamic_cast<NativeExpPval*>(p);
} // NativeExpPvalT::setUp
//------------------------------------------------------------------------------
void NativeExpPvalT::tearDown ()
{
  m_p->UnitTestingDeletePointers();
  delete(m_p);
} // NativeExpPvalT::tearDown
//------------------------------------------------------------------------------
void NativeExpPvalT::testCreateClass ()
{
  TS_ASSERT(m_p);
} // NativeExpPvalT::testCreateClass
//------------------------------------------------------------------------------
void NativeExpPvalT::testLine1 ()
{
  int np = 3;
  char name[30];
  Real b[3] = {1, 2, 3};
  strcpy(&name[0], "par1");
  strcpy(&name[10], "par2");
  strcpy(&name[20], "par3");
  m_p->GetPackage()->SetField(MfData::Packages::PVALpack::NPVAL, &np);
  m_p->GetPackage()->SetField(MfData::Packages::PVALpack::PARNAM, &name[0]);
  m_p->GetPackage()->SetField(MfData::Packages::PVALpack::B, &b[0]);
  CStr base = "3";
  CStr str = m_p->Line1();
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpPvalT::testLine1
//------------------------------------------------------------------------------
void NativeExpPvalT::testDesc1 ()
{
  CStr base = " 1. NP";
  CStr str = m_p->Desc1();
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpPvalT::testDesc1
//------------------------------------------------------------------------------
void NativeExpPvalT::testLine2 ()
{
  int np = 3;
  char name[30];
  Real b[3] = {1, 2, 3};
  strcpy(&name[0], "par1");
  strcpy(&name[10], "par2");
  strcpy(&name[20], "par3");
  m_p->GetPackage()->SetField(MfData::Packages::PVALpack::NPVAL, &np);
  m_p->GetPackage()->SetField(MfData::Packages::PVALpack::PARNAM, &name[0]);
  m_p->GetPackage()->SetField(MfData::Packages::PVALpack::B, &b[0]);
  CStr base[3] = {"par1         1.0",
                  "par2         2.0", 
                  "par3         3.0"};
  std::vector<CStr> str = m_p->Line2();
  for (int i=0; i<3; ++i) TS_ASSERT_EQUALS2(base[i], str[i]);
} // NativeExpPvalT::testLine2
//------------------------------------------------------------------------------
void NativeExpPvalT::testDesc2 ()
{
  int np = 3;
  char name[30];
  Real b[3] = {1, 2, 3};
  strcpy(&name[0], "par1");
  strcpy(&name[10], "par2");
  strcpy(&name[20], "par3");
  m_p->GetPackage()->SetField(MfData::Packages::PVALpack::NPVAL, &np);
  m_p->GetPackage()->SetField(MfData::Packages::PVALpack::PARNAM, &name[0]);
  m_p->GetPackage()->SetField(MfData::Packages::PVALpack::B, &b[0]);
  CStr base = " 2. PARNAM Parval";
  std::vector<CStr> str = m_p->Desc2();
  for (int i=0; i<3; ++i) TS_ASSERT_EQUALS2(base, str[i]);
} // NativeExpPvalT::testDesc2

#endif
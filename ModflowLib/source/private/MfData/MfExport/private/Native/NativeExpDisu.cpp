//------------------------------------------------------------------------------
// FILE      MfNativeExpDisu.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpDisu.h>

#include <sstream>
#include <map>

#include <private\MfData\MfGlobal.h>
#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\TxtExporter.h>
#include <private\MfData\MfExport\private\MfExporterImpl.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackFields.h>

using namespace MfData::Export;

//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpDisu::NativeExpDisu ()
: NativePackExp()
, m_nLay()
, m_nSp()
, m_layCbd()
, m_mapDesc()
{
  InitDescriptionMap();
} // MfNativeExpDisu::MfNativeExpDisu
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpDisu::~NativeExpDisu ()
{
} // MfNativeExpDisu::~MfNativeExpDisu
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpDisu::OnSetData ()
{
  if (!GetGlobal()) return;
  m_nLay = GetGlobal()->NumLay();
  m_nSp = GetGlobal()->NumPeriods();
} // NativeExpDisu::OnSetData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpDisu::Export ()
{
  AddToStoredLinesDesc(Line1(), Desc("1"));
  AddToStoredLinesDesc(Line2(), Desc("2"));
  Line3();
  Line4();
  Line5();
  Line6();
  Line7();
  Line8();
  Line9();
  Line10a();
  Line10b();
  Line11();
  Line12();
  
  AddToStoredLinesDesc(Line13(), std::vector<CStr>(1, Desc("13")));

  WriteComments();
  WriteStoredLines();
  return true;
} // MfNativeExpDisu::Export
//------------------------------------------------------------------------------
/// \brief 1. NODES NLAY NJAG IVSD NPER ITMUNI LENUNI IDSYMRD
//------------------------------------------------------------------------------
CStr NativeExpDisu::Line1 ()
{
  MfGlobal *g = GetGlobal();
  CStr aStr;

  MfPackage* p = GetPackage();

  using namespace MfData::Packages;
  const int *nodes(0), *njag(0), *ivsd(0), *idsymrd(0);
  if (p->GetField(Disu::NODES, &nodes) && nodes &&
      p->GetField(Disu::NJAG, &njag) && njag &&
      p->GetField(Disu::IVSD, &ivsd) && ivsd &&
      p->GetField(Disu::IDSYMRD, &idsymrd) && idsymrd) {

    aStr.Format("%6d %6d %6d %6d %6d %6d %6d %6d ", *nodes, g->NumLay(),
                *njag, *ivsd, g->NumPeriods(), g->TimeUnit(), g->LengthUnit(),
                *idsymrd);
  }

  return aStr;
} // NativeExpDisu::Line1
//------------------------------------------------------------------------------
/// \brief 2. LAYCBD(NLAY)
//------------------------------------------------------------------------------
CStr NativeExpDisu::Line2 ()
{
  using namespace MfData::Packages;
  const int* laycbd(0);
  std::stringstream ss;
  CStr aStr;
  if (GetPackage()->GetField(Disu::LAYCBD, &laycbd) && laycbd)
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
} // NativeExpDisu::Line2
//------------------------------------------------------------------------------
/// \brief 3. NODELAY(NLAY)
//------------------------------------------------------------------------------
void NativeExpDisu::Line3 ()
{
  using namespace MfData::Packages;
  const int* nodlay(0);
  std::stringstream ss;
  CStr aStr;
  MfPackage* p = GetPackage();
  if (p->GetField(Disu::NODLAY, &nodlay) && nodlay)
  {
    for (int k=0; k<m_nLay; ++k)
    {
      ss << nodlay[k] << " ";
    }
    aStr = ss.str();
    aStr.Trim();
  }
  else ASSERT(0);
  AddToStoredLinesDesc(aStr, Desc("3"));
} // NativeExpDisu::Line3
//------------------------------------------------------------------------------
/// \brief 4. Top(NDSLAY) - U1DREL
//------------------------------------------------------------------------------
void NativeExpDisu::Line4 ()
{
  AddArrayLines(Packages::Disu::TOP, "4");
} // NativeExpDisu::Line4
//------------------------------------------------------------------------------
/// \brief 5. Bot(NDSLAY) - U1DREL
//------------------------------------------------------------------------------
void NativeExpDisu::Line5 ()
{
  AddArrayLines(Packages::Disu::BOT, "5");
} // NativeExpDisu::Line5
//------------------------------------------------------------------------------
/// \brief 6. Area(NDSLAY) - U1DREL
//------------------------------------------------------------------------------
void NativeExpDisu::Line6 ()
{
  AddArrayLines(Packages::Disu::AREA, "6");
} // NativeExpDisu::Line6
//------------------------------------------------------------------------------
/// \brief 7. IAC(NODES) - U1DINT
//------------------------------------------------------------------------------
void NativeExpDisu::Line7 ()
{
  AddArrayLines(Packages::Disu::IA, "7");
} // NativeExpDisu::Line7
//------------------------------------------------------------------------------
/// \brief 8. JA(NJAG) - U1DINT
//------------------------------------------------------------------------------
void NativeExpDisu::Line8 ()
{
  AddArrayLines(Packages::Disu::JA, "8");
} // NativeExpDisu::Line8
//------------------------------------------------------------------------------
/// \brief 9. IVC(NJAG) - U1DINT
//------------------------------------------------------------------------------
void NativeExpDisu::Line9 ()
{
  AddArrayLines(Packages::Disu::IVC, "9"); // "VERT CONNECT INDEX ARRAY"
} // NativeExpDisu::Line9
//------------------------------------------------------------------------------
/// \brief 10a. CL1(NJAGS) - U1DREL
//------------------------------------------------------------------------------
void NativeExpDisu::Line10a ()
{
  AddArrayLines(Packages::Disu::CL1, "10"); // "CONNECTION LENGTH 1"
} // NativeExpDisu::Line10
//------------------------------------------------------------------------------
/// \brief 10b. CL2(NJAGS) - U1DREL
//------------------------------------------------------------------------------
void NativeExpDisu::Line10b ()
{
  AddArrayLines(Packages::Disu::CL2, "10b"); // "CONNECTION LENGTH 2"
} // NativeExpDisu::Line10b
//------------------------------------------------------------------------------
/// \brief 11. CL12(NJAG) - U1DREL
//------------------------------------------------------------------------------
void NativeExpDisu::Line11 ()
{
  AddArrayLines(Packages::Disu::CL12, "11"); // "CONNECTION LENGTH 12"
} // NativeExpDisu::Line11
//------------------------------------------------------------------------------
/// \brief 12. FAHL(NJAG/NJAGS) - U1DREL
//------------------------------------------------------------------------------
void NativeExpDisu::Line12 ()
{
  AddArrayLines(Packages::Disu::FAHL, "12"); // "PERPENDICULAR AREA"
} // NativeExpDisu::Line12
//------------------------------------------------------------------------------
/// \brief 13. PERLEN NSTP TSMULT Ss/Tr
//------------------------------------------------------------------------------
std::vector<CStr> NativeExpDisu::Line13 ()
{
  using namespace MfData::Packages;
  std::vector<CStr> rval;
  MfPackage* p = GetPackage();
  const Real *perlen(0), *tsmult(0);
  const int *nstp(0), *issflg(0);
  if (p->GetField(Disu::PERLEN, &perlen) && perlen &&
      p->GetField(Disu::NSTP, &nstp) && nstp &&
      p->GetField(Disu::TSMULT, &tsmult) && tsmult &&
      p->GetField(Disu::ISSFLG, &issflg) && issflg) {
    CStr str;
    for (int i = 0; i < m_nSp; ++i) {
      str.Format("%s %6d %s %s", STR(perlen[i]), nstp[i], STR(tsmult[i]),
                                  issflg[i] ? "Ss":"Tr");
      //AddToStoredLinesDesc(str, (i == 0 ? Desc("13") : ""));
      if (!issflg[i])
        GetNative()->GetExp()->AtLeastOneTransientSPExists() = true;
      else
        GetNative()->GetExp()->SetOfSteadyStateStressPeriods().insert(i+1);
      rval.push_back(str);
    }
  }
  return rval;
} // NativeExpDisu::Line13
//------------------------------------------------------------------------------
/// \brief 1. NODES NLAY NJAG IVSD NPER ITMUNI LENUNI IDSYMRD
//------------------------------------------------------------------------------
void NativeExpDisu::InitDescriptionMap ()
{
  m_mapDesc.clear();
  m_mapDesc.insert(std::make_pair("1", " 1. NODES NLAY NJAG IVSD NPER ITMUNI LENUNI IDSYMRD"));
  m_mapDesc.insert(std::make_pair("2", " 2. LAYCBD(NLAY)"));
  m_mapDesc.insert(std::make_pair("3", " 3. NODELAY(NLAY)"));
  m_mapDesc.insert(std::make_pair("4", " 4. Top(NDSLAY)"));
  m_mapDesc.insert(std::make_pair("5", " 5. Bot(NDSLAY)"));
  m_mapDesc.insert(std::make_pair("6", " 6. Area(NDSLAY)"));
  m_mapDesc.insert(std::make_pair("7", " 7. IAC(NODES)"));
  m_mapDesc.insert(std::make_pair("8", " 8. JA(NJAG)"));
  m_mapDesc.insert(std::make_pair("9", " 9. IVC(NJAG)"));
  m_mapDesc.insert(std::make_pair("10a", "10a. CL1(NJAGS)"));
  m_mapDesc.insert(std::make_pair("10b", "10b. CL2(NJAGS)"));
  m_mapDesc.insert(std::make_pair("11", "11. CL12(NJAG)"));
  m_mapDesc.insert(std::make_pair("12", "12. FAHL(NJAG/NJAGS)"));
  m_mapDesc.insert(std::make_pair("13", "13. PERLEN NSTP TSMULT Ss/Tr"));
} // NativeExpDisu::Desc
//------------------------------------------------------------------------------
/// \brief Return the description string for the given line.
//------------------------------------------------------------------------------
CStr NativeExpDisu::Desc (const CStr& a_line)
{
  std::map<CStr, CStr>::iterator it = m_mapDesc.find(a_line);
  if (it == m_mapDesc.end()) return "";
  std::stringstream ss;
  ss << it->second;
  return ss.str();
} // NativeExpDisu::Desc
//------------------------------------------------------------------------------
/// \brief
/// \param a_name: Name of the package.
/// \param a_desc: Line number for the description.
//------------------------------------------------------------------------------
void NativeExpDisu::AddArrayLines (const CStr& a_name, const CStr& a_desc)
{
  CStr str;
  MfPackage* p = GetGlobal()->GetPackage(a_name);
  if (!p) {
    return;
  }
  std::vector<CStr>& lines(p->StringsToWrite());
  ASSERT(!lines.empty());
  for (size_t i = 0; i < lines.size(); ++i) {
    AddToStoredLinesDesc(lines[i], (i == 0 ? Desc(a_desc) : ""));
  }
} // NativeExpDisu::AddArrayLines


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData\MfExport\private\Native\NativeExpDisu.t.h>

#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackStrings.h>
#include <private/MfLibAsserts.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpDisuT::setUp ()
{
  m_p = NULL;
  Mf2kNative* n = new Mf2kNative;
  MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  MfPackage* dis = new MfPackage(Packages::DISU);
  NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  m_p = dynamic_cast<NativeExpDisu*>(p);
} // NativeExpDisuT::setUp
//------------------------------------------------------------------------------
void NativeExpDisuT::tearDown ()
{
  m_p->UnitTestingDeletePointers();
  delete(m_p);
} // NativeExpDisuT::tearDown
//------------------------------------------------------------------------------
void NativeExpDisuT::testCreateClass ()
{
  TS_ASSERT(m_p);
} // NativeExpDisuT::testCreateClass
//------------------------------------------------------------------------------
void NativeExpDisuT::testLine1 ()
{
  int nodes(10), njag(8), ivsd(0), idsymrd(0);
  m_p->GetPackage()->SetField(Packages::Disu::NODES, &nodes);
  m_p->GetPackage()->SetField(Packages::Disu::NJAG, &njag);
  m_p->GetPackage()->SetField(Packages::Disu::IVSD, &ivsd);
  m_p->GetPackage()->SetField(Packages::Disu::IDSYMRD, &idsymrd);
  CStr base = "    10      3      8      0      2      3      2      0 ";
  CStr str = m_p->Line1();
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpDisuT::testLine1
//------------------------------------------------------------------------------
void NativeExpDisuT::testLine2 ()
{
  const int laycbd[3] = {0,1,2};
  m_p->GetPackage()->SetField(Packages::Disu::LAYCBD, laycbd);
  CStr base = "0 1 2";
  CStr str = m_p->Line2();
  TS_ASSERT_EQUALS2(base, str);
} // NativeExpDisuT::testLine2
//------------------------------------------------------------------------------
void NativeExpDisuT::testLine3 ()
{
} // NativeExpDisuT::testLine3
//------------------------------------------------------------------------------
void NativeExpDisuT::testLine4 ()
{
} // NativeExpDisuT::testLine4
//------------------------------------------------------------------------------
void NativeExpDisuT::testLine5 ()
{
} // NativeExpDisuT::testLine5
//------------------------------------------------------------------------------
void NativeExpDisuT::testLine6 ()
{
} // NativeExpDisuT::testLine6
//------------------------------------------------------------------------------
void NativeExpDisuT::testLine13 ()
{
  using namespace MfData::Packages;
  int nstps[2]={50,60},
      issFlag[2]={1,0};
  Real perLen[2]={5,6},
       tsMult[2]={7,8};
  MfPackage& p(*m_p->GetPackage());
  p.SetField(Disu::PERLEN, perLen);
  p.SetField(Disu::TSMULT, tsMult);
  p.SetField(Disu::NSTP, nstps);
  p.SetField(Disu::ISSFLG, issFlag);
  std::vector<CStr> strs = m_p->Line13();
  std::vector<CStr> base;
  base.push_back("5.0 50 7.0 Ss");
  base.push_back("6.0 60 8.0 Tr");
}

#endif
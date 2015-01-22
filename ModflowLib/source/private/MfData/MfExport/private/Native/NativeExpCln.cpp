//------------------------------------------------------------------------------
// FILE      MfNativeExpCln.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpCln.h>

#include <sstream>
#include <map>

#include <private\MfData\MfGlobal.h>
#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\TxtExporter.h>
#include <private\MfData\MfExport\private\MfExporterImpl.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackFields.h>
#include <private\MfData\Packages\MfPackStrings.h>
#include <private/util/util.h>

using namespace MfData::Export;

//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpCln::NativeExpCln ()
: NativePackExp()
, m_mapDesc()
{
  InitDescriptionMap();
} // MfNativeExpCln::MfNativeExpCln
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpCln::~NativeExpCln ()
{
} // MfNativeExpCln::~MfNativeExpCln
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpCln::Export ()
{
  MfPackage* p = GetGlobal()->GetPackage(Packages::CLN);
  if (!p)
  {
    MfPackage p1(Packages::CLN);
    GetGlobal()->AddPackage(&p1);
  }

  CStr nm = GetPackage()->PackageName();

  if (nm == MfData::Packages::CLNLine1) {
    AddToStoredLinesDesc(Line1(), Desc("1"));
    WriteCommentsCln();
  }
  else {
    Line2();
    Line3();
    Line4();
    if (GetGlobal()->Unstructured()) {
      Line5();
    }
    else {
      Line6();
    }
    Line7();
    Line8();
    Line9();

    WriteStoredLinesCln();
  }

  return true;
} // NativeExpCln::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpCln::WriteCommentsCln ()
{
  MfPackage *p = GetGlobal()->GetPackage(Packages::CLN);
  MfPackage *orig = GetPackage();
  SetData(GetNative(), GetGlobal(), p);
  WriteComments();
  SetData(GetNative(), GetGlobal(), orig);
} // NativeExpCln::WriteCommentsCln
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpCln::WriteStoredLinesCln ()
{
  MfPackage *p = GetGlobal()->GetPackage(Packages::CLN);
  MfPackage *orig = GetPackage();
  SetData(GetNative(), GetGlobal(), p);
  WriteStoredLines();
  SetData(GetNative(), GetGlobal(), orig);
} // NativeExpCln::WriteStoredLinesCln
//------------------------------------------------------------------------------
/// \brief 1. NCLN ICLNNDS ICLNCB ICLNHD ICLNDD ICLNIB NCLNGWC NCONDUITYP
//------------------------------------------------------------------------------
CStr NativeExpCln::Line1 ()
{
  CStr aStr;
  MfPackage* p = GetPackage();

  using namespace MfData::Packages;
  const int *ncln(0), *iclnnds(0), *iclncb(0), *iclnhd(0),
            *iclndd(0), *iclnib(0), *nclngwc(0), *nconduityp(0);
  if (p->GetField(Cln::NCLN, &ncln) && ncln &&
      p->GetField(Cln::ICLNNDS, &iclnnds) && iclnnds &&
      p->GetField(Cln::ICLNCB, &iclncb) && iclncb &&
      p->GetField(Cln::ICLNHD, &iclnhd) && iclnhd &&
      p->GetField(Cln::ICLNDD, &iclndd) && iclndd &&
      p->GetField(Cln::ICLNIB, &iclnib) && iclnib &&
      p->GetField(Cln::NCLNGWC, &nclngwc) && nclngwc &&
      p->GetField(Cln::NCONDUITYP, &nconduityp) && nconduityp) {

    aStr.Format("%6d %6d %6d %6d %6d %6d %6d %6d ", *ncln, *iclnnds, *iclncb,
                *iclnhd, *iclndd, *iclnib, *nclngwc, *nconduityp);
  }

  return aStr;
} // NativeExpCln::Line1
//------------------------------------------------------------------------------
/// \brief NNDCLN(NCLN) – U1DREL
//------------------------------------------------------------------------------
void NativeExpCln::Line2 ()
{
  AddArrayLines(Packages::Cln::NNDCLN, "2");
} // NativeExpCln::Line2
//------------------------------------------------------------------------------
/// \brief 3. CLNCON[NNDCLN(NCLN)]
//------------------------------------------------------------------------------
void NativeExpCln::Line3 ()
{
  //AddArrayLines(Packages::Cln::NODLAY, "3");
} // NativeExpCln::Line3
//------------------------------------------------------------------------------
/// \brief 4. IFNO IFTYP IFDIR FLENG FELEV FANGLE IFLIN ICCWADI
//------------------------------------------------------------------------------
void NativeExpCln::Line4 ()
{
  using namespace MfData::Packages;
  using util::ForElement;

  MfPackage* p = GetPackage();
  const Real *aclnndsaq(0);
  const int *nclnnds(0);

  if (p->GetField(Cln::ACLNNDSAQ, &aclnndsaq) && aclnndsaq &&
      p->GetField(Cln::NCLNNDS, &nclnnds) && nclnnds) {
    int n = *nclnnds;
    CStr aStr;
    CStr desc = Desc("4");
    for (int i = 1; i <= n; ++i) {
      aStr.Format("%s %s %s %s %s %s %s %s ",
                  STR(ForElement(aclnndsaq, i, 1, n), 0),
                  STR(ForElement(aclnndsaq, i, 2, n), 0),
                  STR(ForElement(aclnndsaq, i, 3, n), 0),
                  STR(ForElement(aclnndsaq, i, 4, n)),
                  STR(ForElement(aclnndsaq, i, 5, n)),
                  STR(ForElement(aclnndsaq, i, 6, n)),
                  STR(ForElement(aclnndsaq, i, 7, n), 0),
                  STR(ForElement(aclnndsaq, i, 8, n), 0));
      AddToStoredLinesDesc(aStr, (i == 1 ? desc : ""));
    }
  }
} // NativeExpCln::Line4
//------------------------------------------------------------------------------
/// \brief 5. IFNOD IGWNOD IFCON FSKIN FLENGW FANISO ICGWADI
//------------------------------------------------------------------------------
void NativeExpCln::Line5 ()
{
  using namespace MfData::Packages;
  using util::ForElement;

  MfPackage* pLine5 = GetPackage();
  MfPackage *pLine1 = GetGlobal()->GetPackage(MfData::Packages::CLNLine1);
  const Real *aclngwcaq(0);
  const int *nclngwc(0);

  if (pLine5->GetField(Cln::ACLNGWCAQ, &aclngwcaq) && aclngwcaq &&
      pLine1->GetField(Cln::NCLNGWC, &nclngwc) && nclngwc) {
    int n = *nclngwc;
    CStr aStr;
    CStr desc = Desc("5");
    for (int i = 1; i <= n; ++i) {
      aStr.Format("%s %s %s %s %s %s %s ",
                  STR(ForElement(aclngwcaq, i, 1, n), 0),
                  STR(ForElement(aclngwcaq, i, 2, n), 0),
                  STR(ForElement(aclngwcaq, i, 3, n), 0),
                  STR(ForElement(aclngwcaq, i, 4, n)),
                  STR(ForElement(aclngwcaq, i, 5, n)),
                  STR(ForElement(aclngwcaq, i, 6, n)),
                  STR(ForElement(aclngwcaq, i, 7, n), 0));
      AddToStoredLinesDesc(aStr, (i == 1 ? desc : ""));
    }
  }
} // NativeExpCln::Line5
//------------------------------------------------------------------------------
/// \brief 6. IFNOD IGWLAY IGWROW IGWFCOL IFCON FSKIN FLENGW FANISO ICGWADI
//------------------------------------------------------------------------------
void NativeExpCln::Line6 ()
{
  using namespace MfData::Packages;
  using util::ForElement;

  MfPackage* pLine6 = GetPackage();
  MfPackage *pLine1 = GetGlobal()->GetPackage(MfData::Packages::CLNLine1);
  const Real *aclngwcaq(0);
  const int *nclngwc(0);

  if (pLine6->GetField(Cln::ACLNGWCAQ, &aclngwcaq) && aclngwcaq &&
      pLine1->GetField(Cln::NCLNGWC, &nclngwc) && nclngwc) {
    int n = *nclngwc;
    CStr aStr;
    CStr desc = Desc("6");
    for (int i = 1; i <= n; ++i) {
      aStr.Format("%s %s %s %s %s %s %s %s %s ",
                  STR(ForElement(aclngwcaq, i, 1, n), 0),
                  STR(ForElement(aclngwcaq, i, 2, n), 0),
                  STR(ForElement(aclngwcaq, i, 3, n), 0),
                  STR(ForElement(aclngwcaq, i, 4, n), 0),
                  STR(ForElement(aclngwcaq, i, 5, n), 0),
                  STR(ForElement(aclngwcaq, i, 6, n)),
                  STR(ForElement(aclngwcaq, i, 7, n)),
                  STR(ForElement(aclngwcaq, i, 8, n)),
                  STR(ForElement(aclngwcaq, i, 9, n), 0));
      AddToStoredLinesDesc(aStr, (i == 1 ? desc : ""));
    }
  }
} // NativeExpCln::Line6
//------------------------------------------------------------------------------
/// \brief 7. ICONDUITYP FRAD CONDUITK
//------------------------------------------------------------------------------
void NativeExpCln::Line7 ()
{
  using namespace MfData::Packages;
  using util::ForElement;

  MfPackage* pLine7 = GetPackage();
  MfPackage *pLine1 = GetGlobal()->GetPackage(MfData::Packages::CLNLine1);
  const Real *aclncond(0);
  const int *nconduityp(0);

  if (pLine7->GetField(Cln::ACLNCOND, &aclncond) && aclncond &&
      pLine1->GetField(Cln::NCONDUITYP, &nconduityp) && nconduityp) {
    int n = *nconduityp;
    CStr aStr;
    CStr desc = Desc("7");
    for (int i = 1; i <= n; ++i) {
      aStr.Format("%s %s %s",
                  STR(ForElement(aclncond, i, 1, n), 0),
                  STR(ForElement(aclncond, i, 2, n)),
                  STR(ForElement(aclncond, i, 3, n)));
      AddToStoredLinesDesc(aStr, (i == 1 ? desc : ""));
    }
  }
} // NativeExpCln::Line7
//------------------------------------------------------------------------------
/// \brief 8. IBOUND(NCLNNDS) – U1DINT
//------------------------------------------------------------------------------
void NativeExpCln::Line8 ()
{
  AddArrayLines(Packages::Cln::IBOUND, "8");
} // NativeExpCln::Line8
//------------------------------------------------------------------------------
/// \brief 9. STRT(NCLNNDS) – U1DREL
//------------------------------------------------------------------------------
void NativeExpCln::Line9 ()
{
  AddArrayLines(Packages::Cln::STRT, "9");
} // NativeExpCln::Line9
//------------------------------------------------------------------------------
/// \brief 1. NODES NLAY NJAG IVSD NPER ITMUNI LENUNI IDSYMRD
//------------------------------------------------------------------------------
void NativeExpCln::InitDescriptionMap ()
{
  m_mapDesc.clear();
  m_mapDesc.insert(std::make_pair("1", " 1. NCLN ICLNNDS ICLNCB ICLNHD ICLNDD ICLNIB NCLNGWC NCONDUITYP"));
  m_mapDesc.insert(std::make_pair("2", " 2. NNDCLN(NCLN)"));
  m_mapDesc.insert(std::make_pair("3", " 3. CLNCON[NNDCLN(NCLN)]"));
  m_mapDesc.insert(std::make_pair("4", " 4. IFNO IFTYP IFDIR FLENG FELEV FANGLE IFLIN ICCWADI"));
  m_mapDesc.insert(std::make_pair("5", " 5. IFNOD IGWNOD IFCON FSKIN FLENGW FANISO ICGWADI"));
  m_mapDesc.insert(std::make_pair("6", " 6. IFNOD IGWLAY IGWROW IGWFCOL IFCON FSKIN FLENGW FANISO ICGWADI"));
  m_mapDesc.insert(std::make_pair("7", " 7. ICONDUITYP FRAD CONDUITK"));
  m_mapDesc.insert(std::make_pair("8", " 8. IBOUND(NCLNNDS)"));
  m_mapDesc.insert(std::make_pair("9", " 9. STRT(NCLNNDS)"));
} // NativeExpCln::Desc
//------------------------------------------------------------------------------
/// \brief Return the description string for the given line.
//------------------------------------------------------------------------------
CStr NativeExpCln::Desc (const CStr& a_line)
{
  std::map<CStr, CStr>::iterator it = m_mapDesc.find(a_line);
  if (it == m_mapDesc.end()) return "";
  std::stringstream ss;
  ss << it->second;
  return ss.str();
} // NativeExpCln::Desc
//------------------------------------------------------------------------------
/// \brief
/// \param a_name: Name of the package.
/// \param a_desc: Line number for the description.
//------------------------------------------------------------------------------
void NativeExpCln::AddArrayLines (const CStr& a_name, const CStr& a_desc)
{
  CStr str;
  MfPackage* p = GetGlobal()->GetPackage(a_name);
  if (!p) {
    return;
  }
  std::vector<CStr>& lines(p->StringsToWrite());
  ASSERT(!lines.empty());
  for (size_t i = 0; i < lines.size(); ++i) {
    // Show the description on just the first line of the array
    CStr desc = "";
    if (i == 0 ||
        lines[i].find("CONSTANT") != std::string::npos ||
        lines[i].find("OPEN/CLOSE") != std::string::npos ||
        lines[i].find("INTERNAL") != std::string::npos) {
      desc = Desc(a_desc);
    }
    AddToStoredLinesDesc(lines[i], desc);
  }
} // NativeExpCln::AddArrayLines
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpCln::AddToStoredLinesDesc (const char* a_line,
                                         const char* a_desc)
{
  MfPackage* p = GetGlobal()->GetPackage(Packages::CLN);
  MfPackage* curP = GetPackage();

  SetData(GetNative(), GetGlobal(), p);
  NativePackExp::AddToStoredLinesDesc(a_line, a_desc);
  SetData(GetNative(), GetGlobal(), curP);
} // NativeExpCln::AddToStoredLinesDesc


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData\MfExport\private\Native\NativeExpCln.t.h>

#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackStrings.h>
#include <private/MfLibAsserts.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpClnT::setUp ()
{
  m_p = NULL;
  Mf2kNative* n = new Mf2kNative;
  MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  MfPackage* dis = new MfPackage(Packages::CLN);
  NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  m_p = dynamic_cast<NativeExpCln*>(p);
} // NativeExpClnT::setUp
//------------------------------------------------------------------------------
void NativeExpClnT::tearDown ()
{
  m_p->UnitTestingDeletePointers();
  delete(m_p);
} // NativeExpClnT::tearDown
//------------------------------------------------------------------------------
void NativeExpClnT::testCreateClass ()
{
  TS_ASSERT(m_p);
} // NativeExpClnT::testCreateClass

#endif

//------------------------------------------------------------------------------
// FILE      MfNativeExpCln.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/MfData/MfExport/private/Native/NativeExpCln.h>

#include <sstream>
#include <map>

#include <private/MfData/MfGlobal.h>
#include <private/MfData/MfExport/private/Mf2kNative.h>
#include <private/MfData/MfExport/private/TxtExporter.h>
#include <private/MfData/MfExport/private/MfExporterImpl.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackFields.h>
#include <private/MfData/Packages/MfPackStrings.h>
#include <private/util/util.h>

using namespace MfData::Export;

namespace {
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
std::vector<int>& NNDCLN ()
{
  static std::vector<int> fg_nndcln;
  return fg_nndcln;
} // NNDCLN
}
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

  if (nm == MfData::Packages::CLNLines0And1) {
    CStr line0 = Line0();
    if (!line0.empty()) {
      AddToStoredLinesDesc(line0, Desc("0"));
    }
    // comment on line 1 is causing problems with modflow
    //AddToStoredLinesDesc(Line1(), Desc("1"));
    AddToStoredLinesDesc(Line1(), "");
    WriteCommentsCln();
  }
  else if (nm == MfData::Packages::CLNLine2) {
    Line2();
  }
  else if (nm == MfData::Packages::CLNLine3) {
    Line3();
  }
  else if (nm == MfData::Packages::CLNLine4To6) {
    Line4();
    Line5();
    Line6();
  }
  else if (nm == MfData::Packages::CLNLine13) {
    Line13();
    WriteStoredLinesCln();
  }
  else if (nm == MfData::Packages::CLNLine14) {
    Line14();
    WriteStoredLinesCln();
  }
  else if (nm == MfData::Packages::CLNLine15) {
    Line15();
    WriteStoredLinesCln();
  }
  else if (nm == MfData::Packages::CLNLine16) {
    Line16();
    WriteStoredLinesCln();
  }
  else {
    Line7();
    if (GetGlobal()->Unstructured()) {
      Line8();
    }
    else {
      Line9();
    }
    Line10();
    Line11();
    Line12();

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
/// \brief 0. [OPTIONS opt, ...]
//------------------------------------------------------------------------------
CStr NativeExpCln::Line0 ()
{
  CStr aStr;
  MfPackage* p = GetPackage();

  using namespace MfData::Packages;
  const int *iclntib(0), *iclnpcb(0), *iclngwcb(0);
  if (p->GetField(Cln::ICLNTIB, &iclntib) && iclntib &&
      p->GetField(Cln::ICLNPCB, &iclnpcb) && iclnpcb &&
      p->GetField(Cln::ICLNGWCB, &iclngwcb)) {
    if (*iclntib || *iclnpcb) {
      aStr = "OPTIONS";
      if (*iclntib) {
        aStr += " TRANSIENT";
      }
      // newest version of MODFLOW doesn't like this
      //if (*iclnpcb) {
      //  aStr += " PROCESSCCF ";
      //  CStr aUnitStr;
      //  aUnitStr.Format("%d", *iclngwcb);
      //  aStr += aUnitStr;
      //}
    }
  }

  return aStr;
} // NativeExpCln::Line0
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

  MfPackage* pNNDCLN = GetGlobal()->GetPackage(Packages::Cln::NNDCLN);
  if (!pNNDCLN) return;

  const int *nndclnArray(0), *nndclnSize(0);
  if (pNNDCLN->GetField("ARRAY", &nndclnArray) && nndclnArray &&
      pNNDCLN->GetField("JJ", &nndclnSize) && nndclnSize) {
    std::vector<int>& nndcln = NNDCLN();
    nndcln.assign(*nndclnSize, 0);
    for (int i = 0; i < *nndclnSize; ++i) {
      nndcln[i] = nndclnArray[i];
    }
  }
} // NativeExpCln::Line2
//------------------------------------------------------------------------------
/// \brief 3. CLNCON[NNDCLN(NCLN)]
//------------------------------------------------------------------------------
void NativeExpCln::Line3 ()
{
  MfPackage* p = GetPackage();
  if (!p) return;

  using namespace MfData::Packages;
  const int *nclncons(0), *clncon(0);
  if (p->GetField("NCLNCONS", &nclncons) && nclncons &&
      p->GetField("CLNCON", &clncon) && clncon) {
    int count = 0;
    std::vector<int>& nndcln = NNDCLN();
    for (size_t i = 0; i < nndcln.size(); ++i) {
      CStr aStr;
      int nodeCount = nndcln[i];
      for (int j = 0; j < nodeCount; ++j) {
        CStr aStr2;
        aStr2.Format("%6d ", clncon[count++]);
        aStr += aStr2;
      }
      AddToStoredLinesDesc(aStr, Desc("3"));
    }
  }
} // NativeExpCln::Line3
//------------------------------------------------------------------------------
/// \brief 4. NJA_CLN
//------------------------------------------------------------------------------
void NativeExpCln::Line4 ()
{
  MfPackage* p = GetPackage();
  if (!p) return;

  using namespace MfData::Packages;
  const int *nja_cln(0);
  if (p->GetField("NJA_CLN", &nja_cln) && nja_cln) {
    CStr aStr;
    aStr.Format("%d", *nja_cln);
    AddToStoredLinesDesc(aStr, Desc("4"));
  }
} // NativeExpCln::Line4
//------------------------------------------------------------------------------
/// \brief 5. IAC_CLN(ICLNNDS)
//------------------------------------------------------------------------------
void NativeExpCln::Line5 ()
{
  AddArrayLines(Packages::Cln::IAC_CLN, "5");
} // NativeExpCln::Line5
//------------------------------------------------------------------------------
/// \brief 6. JA_CLN(NJA_CLN)
//------------------------------------------------------------------------------
void NativeExpCln::Line6 ()
{
  AddArrayLines(Packages::Cln::JA_CLN, "6");
} // NativeExpCln::Line6
//------------------------------------------------------------------------------
/// \brief 7. IFNO IFTYP IFDIR FLENG FELEV FANGLE IFLIN ICCWADI
//------------------------------------------------------------------------------
void NativeExpCln::Line7 ()
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
    CStr desc = Desc("7");
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
} // NativeExpCln::Line7
//------------------------------------------------------------------------------
/// \brief 8. IFNOD IGWNOD IFCON FSKIN FLENGW FANISO ICGWADI
//------------------------------------------------------------------------------
void NativeExpCln::Line8 ()
{
  using namespace MfData::Packages;
  using util::ForElement;

  MfPackage* pLine8 = GetPackage();
  MfPackage *pLine1 = GetGlobal()->GetPackage(MfData::Packages::CLNLines0And1);
  const Real *aclngwcaq(0);
  const int *nclngwc(0);

  if (pLine8->GetField(Cln::ACLNGWCAQ, &aclngwcaq) && aclngwcaq &&
      pLine1->GetField(Cln::NCLNGWC, &nclngwc) && nclngwc) {
    int n = *nclngwc;
    CStr aStr;
    CStr desc = Desc("8");
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
} // NativeExpCln::Line8
//------------------------------------------------------------------------------
/// \brief 9. IFNOD IGWLAY IGWROW IGWFCOL IFCON FSKIN FLENGW FANISO ICGWADI
//------------------------------------------------------------------------------
void NativeExpCln::Line9 ()
{
  using namespace MfData::Packages;
  using util::ForElement;

  MfPackage* pLine9 = GetPackage();
  MfPackage *pLine1 = GetGlobal()->GetPackage(MfData::Packages::CLNLines0And1);
  const Real *aclngwcaq(0);
  const int *nclngwc(0);

  if (pLine9->GetField(Cln::ACLNGWCAQ, &aclngwcaq) && aclngwcaq &&
      pLine1->GetField(Cln::NCLNGWC, &nclngwc) && nclngwc) {
    int n = *nclngwc;
    CStr aStr;
    CStr desc = Desc("9");
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
} // NativeExpCln::Line9
//------------------------------------------------------------------------------
/// \brief 10. ICONDUITYP FRAD CONDUITK
//------------------------------------------------------------------------------
void NativeExpCln::Line10 ()
{
  using namespace MfData::Packages;
  using util::ForElement;

  MfPackage* pLine10 = GetPackage();
  MfPackage *pLine1 = GetGlobal()->GetPackage(MfData::Packages::CLNLines0And1);
  const Real *aclncond(0);
  const double *aclncondDbl(0);
  const int *nconduityp(0);

  pLine10->GetField(Cln::ACLNCOND, &aclncond);
  pLine10->GetField(Cln::ACLNCOND, &aclncondDbl);
  if ( (aclncond || aclncondDbl) &&
      pLine1->GetField(Cln::NCONDUITYP, &nconduityp) && nconduityp) {
    int n = *nconduityp;
    CStr aStr;
    CStr desc = Desc("10");
    for (int i = 1; i <= n; ++i) {
      if (aclncond)
        aStr.Format("%s %s %s",
                    STR(ForElement(aclncond, i, 1, n), 0),
                    STR(ForElement(aclncond, i, 2, n)),
                    STR(ForElement(aclncond, i, 3, n)));
      else
        aStr.Format("%s %s %s",
                    STR(ForElement(aclncondDbl, i, 1, n), 0),
                    STR(ForElement(aclncondDbl, i, 2, n)),
                    STR(ForElement(aclncondDbl, i, 3, n)));
      AddToStoredLinesDesc(aStr, (i == 1 ? desc : ""));
    }
  }
} // NativeExpCln::Line10
//------------------------------------------------------------------------------
/// \brief 11. IBOUND(NCLNNDS) – U1DINT
//------------------------------------------------------------------------------
void NativeExpCln::Line11 ()
{
  AddArrayLines(Packages::Cln::IBOUND, "11");
} // NativeExpCln::Line11
//------------------------------------------------------------------------------
/// \brief 12. STRT(NCLNNDS) – U1DREL
//------------------------------------------------------------------------------
void NativeExpCln::Line12 ()
{
  AddArrayLines(Packages::Cln::STRT, "12");
} // NativeExpCln::Line12
//------------------------------------------------------------------------------
/// \brief 13. NIB0 NIB1 NIBM1
//------------------------------------------------------------------------------
void NativeExpCln::Line13 ()
{
  MfPackage* p = GetPackage();
  if (!p) return;

  using namespace MfData::Packages;
  const int *nib0(0), *nib1(0), *nibm1(0);
  if (p->GetField("NIB0", &nib0) && nib0 &&
      p->GetField("NIB1", &nib1) && nib1 &&
      p->GetField("NIBM1", &nibm1) && nibm1) {
    CStr aStr;
    aStr.Format("%d %d %d", *nib0, *nib1, *nibm1);
    AddToStoredLinesDesc(aStr, Desc("13"));
  }
} // NativeExpCln::Line13
//------------------------------------------------------------------------------
/// \brief 14. IB0 – U1DREL
//------------------------------------------------------------------------------
void NativeExpCln::Line14 ()
{
  AddArrayLines(Packages::Cln::IB0, "14");
} // NativeExpCln::Line14
//------------------------------------------------------------------------------
/// \brief 15. IB1 [HEADOPT]
//------------------------------------------------------------------------------
void NativeExpCln::Line15 ()
{
  MfPackage* p = GetPackage();
  if (!p) return;

  using namespace MfData::Packages;
  const int *ib1(0), *iheadopt(0);
  const Real *hvalue(0);
  if (p->GetField("IB1", &ib1) && ib1 &&
      p->GetField("IHEADOPT", &iheadopt) && iheadopt &&
      p->GetField("HVALUE", &hvalue) && hvalue) {
    CStr aStr;
    if (*iheadopt == 0)
      aStr.Format("%d", *ib1);
    else if (*iheadopt == 1)
      aStr.Format("%d HEAD %s", *ib1, STR(*hvalue));
    else
      aStr.Format("%d AVHEAD", *ib1);
    AddToStoredLinesDesc(aStr, Desc("15"));
  }
} // NativeExpCln::Line15
//------------------------------------------------------------------------------
/// \brief 16. IBM1 [HEADOPT]
//------------------------------------------------------------------------------
void NativeExpCln::Line16 ()
{
  MfPackage* p = GetPackage();
  if (!p) return;

  using namespace MfData::Packages;
  const int *ibm1(0), *iheadopt(0);
  const Real *hvalue(0);
  if (p->GetField("IBM1", &ibm1) && ibm1 &&
      p->GetField("IHEADOPT", &iheadopt) && iheadopt &&
      p->GetField("HVALUE", &hvalue) && hvalue) {
    CStr aStr;
    if (*iheadopt == 0)
      aStr.Format("%d", *ibm1);
    else if (*iheadopt == 1)
      aStr.Format("%d HEAD %s", *ibm1, STR(*hvalue));
    else
      aStr.Format("%d AVHEAD", *ibm1);
    AddToStoredLinesDesc(aStr, Desc("16"));
  }
} // NativeExpCln::Line16
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpCln::InitDescriptionMap ()
{
  m_mapDesc.clear();
  m_mapDesc.insert(std::make_pair( "0", " 0. [OPTIONS opt, ...]"));
  m_mapDesc.insert(std::make_pair( "1", " 1. NCLN ICLNNDS ICLNCB ICLNHD ICLNDD ICLNIB NCLNGWC NCONDUITYP"));
  m_mapDesc.insert(std::make_pair( "2", " 2. NNDCLN(NCLN)"));
  m_mapDesc.insert(std::make_pair( "3", " 3. CLNCON[NNDCLN(NCLN)]"));
  m_mapDesc.insert(std::make_pair( "4", " 4. NJA_CLN"));
  m_mapDesc.insert(std::make_pair( "5", " 5. IAC_CLN(ICLNNDS)"));
  m_mapDesc.insert(std::make_pair( "6", " 6. JA_CLN(NJA_CLN)"));
  m_mapDesc.insert(std::make_pair( "7", " 7. IFNO IFTYP IFDIR FLENG FELEV FANGLE IFLIN ICCWADI"));
  m_mapDesc.insert(std::make_pair( "8", " 8. IFNOD IGWNOD IFCON FSKIN FLENGW FANISO ICGWADI"));
  m_mapDesc.insert(std::make_pair( "9", " 9. IFNOD IGWLAY IGWROW IGWFCOL IFCON FSKIN FLENGW FANISO ICGWADI"));
  m_mapDesc.insert(std::make_pair("10", "10. ICONDUITYP FRAD CONDUITK"));
  m_mapDesc.insert(std::make_pair("11", "11. IBOUND(NCLNNDS)"));
  m_mapDesc.insert(std::make_pair("12", "12. STRT(NCLNNDS)"));
  m_mapDesc.insert(std::make_pair("13", "13. NIB0 NIB1 NIBM1"));
  m_mapDesc.insert(std::make_pair("14", "14. IB0(NIBO)"));
  m_mapDesc.insert(std::make_pair("15", "15. IB1 [HEADOPT]"));
  m_mapDesc.insert(std::make_pair("16", "16. IBM1 [HEADOPT]"));
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

#include <private/MfData/MfExport/private/Native/NativeExpCln.t.h>

#include <private/MfData/MfExport/private/Mf2kNative.h>
#include <private/MfData/MfExport/private/Native/NativeUtil.h>
#include <private/MfData/MfGlobal.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackStrings.h>
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

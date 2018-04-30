//------------------------------------------------------------------------------
// FILE      NativeExpBct.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpBct.h>

#include <sstream>

#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\MfExportUtil.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfExport\private\TxtExporter.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackFields.h>
#include <private\MfData\Packages\MfPackStrings.h>

using namespace MfData::Export;
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpBct::NativeExpBct ()
  : m_descriptions()
{
  SetUpDescriptions();
} // MfNativeExpBct::MfNativeExpBct
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpBct::~NativeExpBct ()
{
} // MfNativeExpBct::~MfNativeExpBct
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpBct::SetUpDescriptions()
{
  m_descriptions.insert(std::make_pair(ARR_BCT_ICBUND, BctDescription(" 2. ICBUND(NDSLAY)"," 21. ICBUND(NCOL,NROW)")));
  m_descriptions.insert(std::make_pair(ARR_BCT_PRSITY, BctDescription(" 3. PRSITY(NDSLAY)"," 22. PRSITY(NCOL,NROW)")));
  m_descriptions.insert(std::make_pair(ARR_BCT_BULKD,  BctDescription(" 4. BULKD(NDSLAY)", " 23. BULKD(NCOL,NROW)")));
  m_descriptions.insert(std::make_pair(ARR_BCT_ANGLEX, BctDescription(" 5. ANGLEX(NJA)",   "")));
  m_descriptions.insert(std::make_pair(ARR_BCT_DL,     BctDescription(" 6. DL(NDSLAY)",    " 24. DL(NCOL,NROW)")));
  m_descriptions.insert(std::make_pair(ARR_BCT_DT,     BctDescription(" 7. DT(NDSLAY)",    " 25. DT(NCOL,NROW)")));
  m_descriptions.insert(std::make_pair(ARR_BCT_DLX,    BctDescription(" 8. DLX(NDSLAY)",   " 26. DLX(NCOL,NROW)")));
  m_descriptions.insert(std::make_pair(ARR_BCT_DLY,    BctDescription(" 9. DLY(NDSLAY)",   " 27. DLY(NCOL,NROW)")));
  m_descriptions.insert(std::make_pair(ARR_BCT_DLZ,    BctDescription("10. DLZ(NDSLAY)",   " 28. DLZ(NCOL,NROW)")));
  m_descriptions.insert(std::make_pair(ARR_BCT_DTXY,   BctDescription("11. DTXY(NDSLAY)",  " 29. DTXY(NCOL,NROW)")));
  m_descriptions.insert(std::make_pair(ARR_BCT_DTYZ,   BctDescription("12. DTYZ(NDSLAY)",  " 30. DTYZ(NCOL,NROW)")));
  m_descriptions.insert(std::make_pair(ARR_BCT_DTXZ,   BctDescription("13. DTXZ(NDSLAY)",  " 31. DTXZ(NCOL,NROW)")));
  m_descriptions.insert(std::make_pair(ARR_BCT_ADSORB, BctDescription("14. ADSORB(NDSLAY,ICOMP)"," 32. ADSORB(NCOL,NROW)")));
  m_descriptions.insert(std::make_pair(ARR_BCT_FLICH,  BctDescription("15. FLICH(NDSLAY,ICOMP)", " 33. FLICH(NCOL,NROW)")));
  m_descriptions.insert(std::make_pair(ARR_BCT_ZODRW,  BctDescription("16. ZODRW(NDSLAY,ICOMP)", " 34. ZODRW(NCOL,NROW)")));
  m_descriptions.insert(std::make_pair(ARR_BCT_ZODRS,  BctDescription("17. ZODRS(NDSLAY,ICOMP)", " 35. ZODRS(NCOL,NROW)")));
  m_descriptions.insert(std::make_pair(ARR_BCT_FODRW,  BctDescription("18. FODRW(NDSLAY,ICOMP)", " 36. FODRW(NCOL,NROW)")));
  m_descriptions.insert(std::make_pair(ARR_BCT_FODRS,  BctDescription("19. FODRS(NDSLAY,ICOMP)", " 37. FODRS(NCOL,NROW)")));
  m_descriptions.insert(std::make_pair(ARR_BCT_CONC,   BctDescription("20. CONC(NDSLAY,ICOMP)",  " 38. CONC(NCOL,NROW)")));
} // NativeExpBct::GetDescription
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpBct::GetDescription (const CStr& a_key)
{
  CStr desc;
  std::map<CStr, BctDescription>::iterator it = m_descriptions.find(a_key);
  if (it == m_descriptions.end())
  {
    ASSERT(0);
  }
  else
  {
    if (GetGlobal()->Unstructured())
    {
      desc = it->second.m_unstructured;
    }
    else
    {
      desc = it->second.m_structured;
    }
  }

  return desc;
} // NativeExpBct::GetDescription
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpBct::ExportArray (const CStr& a_array, int a_speciesId)
{
  MfPackage* p = GetGlobal()->GetPackage(a_array);
  if (!p) return;

  CStr description = GetDescription(a_array);
  CStr d;

  int startIdx(0);
  std::vector<CStr>& lines(p->StringsToWrite());

  if (ARR_BCT_ANGLEX != a_array)
  {
    if (a_speciesId > -1)
    {
      startIdx = (a_speciesId - 1) * GetGlobal()->NumLay();
    }
    for (int lay=1, i=0; i<GetGlobal()->NumLay(); ++i, ++lay)
    {
      d.Format("%s Layer %d", description, lay);
      AddToStoredLinesDesc(lines[startIdx + i], d);
      if (MfExportUtil::ArrayWriteNextLineInternal(GetNative(), lines[i + startIdx]))
      {
        AddToStoredLinesDesc(lines[++i + startIdx], "");
      }
    }
  }
  else
  {
    AddToStoredLinesDesc(lines[0], description);
    if (MfExportUtil::ArrayWriteNextLineInternal(GetNative(), lines[0]))
    {
      AddToStoredLinesDesc(lines[1], "");
    }
  }
  lines.clear();
} // NativeExpBct::ExportArray
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpBct::Line1aAnd1b()
{
  const int *itrnsp(0), *ibctcb(0), *mcomp(0), *icbndflg(0), *itvd(0);
  const int *iadsorb(0), *ict(0), *idisp(0), *ixdisp(0), *izod(0), *ifod(0);
  const int *ifmbc(0);
  const Real *cinact(0), *diffnc(0);
  const double *ciclose(0);
  MfPackage* a_p = GetPackage();
  if (a_p->GetField(Packages::Bct::ITRNSP, &itrnsp) && itrnsp &&
      a_p->GetField(Packages::Bct::IBCTCB, &ibctcb) && ibctcb &&
      a_p->GetField(Packages::Bct::MCOMP, &mcomp) && mcomp &&
      a_p->GetField(Packages::Bct::ICBNDFLG, &icbndflg) && icbndflg &&
      a_p->GetField(Packages::Bct::ITVD, &itvd) && itvd &&
      a_p->GetField(Packages::Bct::IADSORB, &iadsorb) && iadsorb &&
      a_p->GetField(Packages::Bct::ICT, &ict) && ict &&
      a_p->GetField(Packages::Bct::CINACT, &cinact) && cinact &&
      a_p->GetField(Packages::Bct::CICLOSE, &ciclose) && ciclose &&
      a_p->GetField(Packages::Bct::IDISP, &idisp) && idisp &&
      a_p->GetField(Packages::Bct::IXDISP, &ixdisp) && ixdisp &&
      a_p->GetField(Packages::Bct::DIFFNC, &diffnc) && diffnc &&
      a_p->GetField(Packages::Bct::IZOD, &izod) && izod &&
      a_p->GetField(Packages::Bct::IFOD, &ifod) && ifod &&
      a_p->GetField(Packages::Bct::IFMBC, &ifmbc) && ifmbc)
  {
    CStr desc = "# 1a. ITRNSP IBCTCB MCOMP ICBNDFLG ITVD IADSORB ICT CINACT CICLOSE IDISP IXDISP DIFFNC IZOD IFOD IFMBC";
    CStr var;
    CStr line;
    int width = util::RealWidth();

    var.Format("%5d ", *itrnsp);
    line += var;
    var.Format("%5d ", *ibctcb);
    line += var;
    var.Format("%5d ", *mcomp);
    line += var;
    var.Format("%5d ", *icbndflg);
    line += var;
    var.Format("%5d ", *itvd);
    line += var;
    var.Format("%5d ", *iadsorb);
    line += var;
    var.Format("%5d ", *ict);
    line += var;
    var.Format("%s ", STR(*cinact, -1, width, STR_FULLWIDTH));
    line += var;
    var.Format("%s ", STR(*ciclose, -1, width, STR_FULLWIDTH));
    line += var;
    var.Format("%5d ", *idisp);
    line += var;
    var.Format("%5d ", *ixdisp);
    line += var;
    var.Format("%s ", STR(*diffnc, -1, width, STR_FULLWIDTH));
    line += var;
    var.Format("%5d ", *izod);
    line += var;
    var.Format("%5d ", *ifod);
    line += var;
    var.Format("%5d", *ifmbc);
    line += var;
    AddToStoredLinesDesc(desc, "");
    AddToStoredLinesDesc(line, "");

    if (*ifmbc != 0)
    {
      const int *mbegwunf(0), *mbegwunt(0), *mbeclnunf(0), *mbeclnunt(0);
      if (a_p->GetField(Packages::Bct::MBEGWUNF, &mbegwunf) && mbegwunf &&
          a_p->GetField(Packages::Bct::MBEGWUNT, &mbegwunt) && mbegwunt &&
          a_p->GetField(Packages::Bct::MBECLNUNF, &mbeclnunf) && mbeclnunf &&
          a_p->GetField(Packages::Bct::MBECLNUNT, &mbeclnunt) && mbeclnunt)
      {
        CStr desc = " 1b. MBEGWUNF MBEGWUNT MBECLNUNF MBECLNUNT";
        CStr var;
        CStr line;

        var.Format("%5d ", *mbegwunf);
        line += var;
        var.Format("%5d ", *mbegwunt);
        line += var;
        var.Format("%5d ", *mbeclnunf);
        line += var;
        var.Format("%5d ", *mbeclnunt);
        line += var;
      }
    }
  }
} // NativeExpBct::Line1aAnd1b
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpBct::Export ()
{
  if (GetPackage()->PackageName() != Packages::BCT)
  {
    return false;
  }

  Line1aAnd1b();

  MfPackage* a_p = GetPackage();
  const int *mcomp(0), *icbndflg(0), *iadsorb(0), *idisp(0), *izod(0), *ifod(0);
  if (a_p->GetField(Packages::Bct::MCOMP, &mcomp) && mcomp &&
      a_p->GetField(Packages::Bct::ICBNDFLG, &icbndflg) && icbndflg &&
      a_p->GetField(Packages::Bct::IADSORB, &iadsorb) && iadsorb &&
      a_p->GetField(Packages::Bct::IDISP, &idisp) && idisp &&
      a_p->GetField(Packages::Bct::IZOD, &izod) && izod &&
      a_p->GetField(Packages::Bct::IFOD, &ifod) && ifod)
  {

    if (*icbndflg == 0) // ICBUND doc says the opposite in some places but is wrong.
    {
      ExportArray(ARR_BCT_ICBUND, -1);
    }
    ExportArray(ARR_BCT_PRSITY, -1);
    if (*iadsorb != 0)
    {
      ExportArray(ARR_BCT_BULKD, -1);
    }
    if (GetGlobal()->Unstructured() && *idisp != 0)
    {
      ExportArray(ARR_BCT_ANGLEX, -1);
    }
    if (*idisp == 1)
    {
      ExportArray(ARR_BCT_DL, -1);
      ExportArray(ARR_BCT_DT, -1);
    }
    else if (*idisp == 2)
    {
      ExportArray(ARR_BCT_DLX, -1);
      ExportArray(ARR_BCT_DLY, -1);
      ExportArray(ARR_BCT_DLZ, -1);
      ExportArray(ARR_BCT_DTXY, -1);
      ExportArray(ARR_BCT_DTYZ, -1);
      ExportArray(ARR_BCT_DTXZ, -1);
    }

    // Species-dependent variables
    int nSpecies = *mcomp;
    for (int speciesId = 1; speciesId <= nSpecies; ++speciesId)
    {
      if (*iadsorb != 0)
      {
        ExportArray(ARR_BCT_ADSORB, speciesId);
      }
      if (*iadsorb == 2)
      {
        ExportArray(ARR_BCT_FLICH, speciesId);
      }
      if (*izod == 1 || *izod == 3)
      {
        ExportArray(ARR_BCT_ZODRW, speciesId);
      }
      if (*iadsorb != 0 && (*izod == 2 || *izod == 3))
      {
        ExportArray(ARR_BCT_ZODRS, speciesId);
      }
      if (*ifod == 1 || *ifod == 3)
      {
        ExportArray(ARR_BCT_FODRW, speciesId);
      }
      if (*iadsorb != 0 && (*ifod == 2 || *ifod == 3))
      {
        ExportArray(ARR_BCT_FODRS, speciesId);
      }
      ExportArray(ARR_BCT_CONC, speciesId);
    }
  }

  WriteComments();
  WriteStoredLines();
  return true;
} // MfNativeExpBct::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpBct::AddToStoredLinesDesc (const char* a_line,
                                         const char* a_desc)
{
  MfPackage* p = GetGlobal()->GetPackage(Packages::BCT);
  if (!p || GetPackage() == p)
    NativePackExp::AddToStoredLinesDesc(a_line, a_desc);
  else
  {
    MfPackage* p1 = GetPackage();
    SetData(GetNative(), GetGlobal(), p);
    NativePackExp::AddToStoredLinesDesc(a_line, a_desc);
    SetData(GetNative(), GetGlobal(), p1);
  }
} // NativeExpBct::AddToStoredLinesDesc

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData\MfExport\private\Native\NativeExpBct.t.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpBctT::setUp ()
{
  //m_p = NULL;
  //Mf2kNative* n = new Mf2kNative;
  //MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  //MfPackage* dis = new MfPackage(Packages::LAK);
  //NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  //m_p = dynamic_cast<NativeExpBct*>(p);
} // NativeExpBctT::setUp
//------------------------------------------------------------------------------
void NativeExpBctT::tearDown ()
{
  //m_p->UnitTestingDeletePointers();
  //delete(m_p);
} // NativeExpBctT::tearDown
//------------------------------------------------------------------------------
void NativeExpBctT::testCreateClass ()
{
  //TS_ASSERT(m_p);
} // NativeExpBctT::testCreateClass

#endif
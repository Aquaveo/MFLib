//------------------------------------------------------------------------------
// FILE      NativeExpDpt.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/MfData/MfExport/private/Native/NativeExpDpt.h>

#include <sstream>

#include <private/MfData/MfExport/private/Mf2kNative.h>
#include <private/MfData/MfExport/private/MfExportUtil.h>
#include <private/MfData/MfExport/private/Native/NativeUtil.h>
#include <private/MfData/MfExport/private/TxtExporter.h>
#include <private/MfData/MfGlobal.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackFields.h>
#include <private/MfData/Packages/MfPackStrings.h>

using namespace MfData::Export;
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpDpt::NativeExpDpt ()
  : m_descriptions()
{
  SetUpDescriptions();
} // MfNativeExpDpt::MfNativeExpDpt
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpDpt::~NativeExpDpt ()
{
} // MfNativeExpDpt::~MfNativeExpDpt
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpDpt::SetUpDescriptions()
{
  m_descriptions.insert(std::make_pair(ARR_DPT_ICBUNDIM, " 2. ICBUNDIM(NDSLAY)"));
  m_descriptions.insert(std::make_pair(ARR_DPT_PHIF,     " 3. PHIF(NDSLAY)"));
  m_descriptions.insert(std::make_pair(ARR_DPT_PRSITYIM, " 4. PRSITYIM(NDSLAY)"));
  m_descriptions.insert(std::make_pair(ARR_DPT_BULKDIM,  " 5. BULKDIM(NDSLAY)"));
  m_descriptions.insert(std::make_pair(ARR_DPT_DLIM,     " 6. DLIM(NDSLAY)"));
  m_descriptions.insert(std::make_pair(ARR_DPT_DDTTR,    " 7. DDTTR(NDSLAY)"));
  m_descriptions.insert(std::make_pair(ARR_DPT_SIM,      " 8. SIM(NDSLAY)"));
  m_descriptions.insert(std::make_pair(ARR_DPT_ADSORBIM, " 9. ADSORBIM(NDSLAY,ICOMP)"));
  m_descriptions.insert(std::make_pair(ARR_DPT_FLICHIM,  "10. FLICHIM(NDSLAY,ICOMP)"));
  m_descriptions.insert(std::make_pair(ARR_DPT_ZODRWIM,  "11. ZODRWIM(NDSLAY,ICOMP)"));
  m_descriptions.insert(std::make_pair(ARR_DPT_ZODRSIM,  "12. ZODRSIM(NDSLAY,ICOMP)"));
  m_descriptions.insert(std::make_pair(ARR_DPT_FODRWIM,  "13. FODRWIM(NDSLAY,ICOMP)"));
  m_descriptions.insert(std::make_pair(ARR_DPT_FODRS,    "14. FODRS(NDSLAY,ICOMP)"));
  m_descriptions.insert(std::make_pair(ARR_DPT_CONC,     "15. CONC(NDSLAY,ICOMP)"));
} // NativeExpDpt::GetDescription
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr NativeExpDpt::GetDescription (const CStr& a_key)
{
  CStr desc;
  std::map<CStr, CStr>::iterator it = m_descriptions.find(a_key);
  if (it == m_descriptions.end())
  {
    ASSERT(0);
  }
  else
  {
    desc = it->second;
  }

  return desc;
} // NativeExpDpt::GetDescription
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpDpt::ExportArray (const CStr& a_array, int a_speciesId)
{
  MfPackage* p = GetGlobal()->GetPackage(a_array);
  if (!p) return;

  CStr description = GetDescription(a_array);
  CStr d;

  int startIdx(0);
  std::vector<CStr>& lines(p->StringsToWrite());

  if (a_speciesId > -1)
  {
    startIdx = (a_speciesId - 1) * GetGlobal()->NumLay();
  }
  for (int lay=1, i=0; i<GetGlobal()->NumLay(); ++i, ++lay)
  {
    d.Format("%s Layer %d", description, lay);
    if (a_speciesId > 0)
    {
      d.Format("%s Layer %d Species %d", description, lay, a_speciesId);
    }
    AddToStoredLinesDesc(lines[startIdx + i], d);
    if (MfExportUtil::ArrayWriteNextLineInternal(GetNative(), lines[i + startIdx]))
    {
      AddToStoredLinesDesc(lines[++i + startIdx], "");
    }
  }
  //lines.clear();
} // NativeExpDpt::ExportArray
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpDpt::Line1()
{
  const int *idptcb(0), *idptcon(0), *icbndimflg(0), *iadsorbim(0), *idispim(0);
  const int *izodim(0), *ifodim(0), *imsat(0), *ifrahk(0);
  MfPackage* a_p = GetPackage();
  if (a_p->GetField(Packages::Dpt::IDPTCB, &idptcb) && idptcb &&
      a_p->GetField(Packages::Dpt::IDPTCON, &idptcon) && idptcon &&
      a_p->GetField(Packages::Dpt::ICBNDIMFLG, &icbndimflg) && icbndimflg &&
      a_p->GetField(Packages::Dpt::IADSORBIM, &iadsorbim) && iadsorbim &&
      a_p->GetField(Packages::Dpt::IDISPIM, &idispim) && idispim &&
      a_p->GetField(Packages::Dpt::IZODIM, &izodim) && izodim &&
      a_p->GetField(Packages::Dpt::IFODIM, &ifodim) && ifodim &&
      a_p->GetField(Packages::Dpt::IMSAT, &imsat) && imsat &&
      a_p->GetField(Packages::Dpt::IFRAHK, &ifrahk) && ifrahk)
  {
    CStr desc = "1. IDPTCB IDPTCON ICBNDIMFLG IADSORBIM IDISPIM IZODIM IFODIM [OPTIONS]";
    CStr var;
    CStr line;

    var.Format("%5d ", *idptcb);
    line += var;
    var.Format("%5d ", *idptcon);
    line += var;
    var.Format("%5d ", *icbndimflg);
    line += var;
    var.Format("%5d ", *iadsorbim);
    line += var;
    var.Format("%5d ", *idispim);
    line += var;
    var.Format("%5d ", *izodim);
    line += var;
    var.Format("%5d", *ifodim);
    line += var;

    // Optional keywords
    if (*ifrahk == 1)
      line += " FRAHK";
    if (*imsat == 2)
      line += " MOBILESAT";
    else if (*imsat == 3)
      line += " INPUTSAT";

    AddToStoredLinesDesc(line, desc);
  }
} // NativeExpDpt::Line1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpDpt::Export ()
{
  if (GetPackage()->PackageName() != Packages::DPT)
  {
    return false;
  }

  Line1();

  MfPackage* a_p = GetPackage();
  const int *idptcb(0), *idptcon(0), *icbndimflg(0), *iadsorbim(0), *idispim(0);
  const int *izodim(0), *ifodim(0), *imsat(0);
  if (a_p->GetField(Packages::Dpt::IDPTCB, &idptcb) && idptcb &&
      a_p->GetField(Packages::Dpt::IDPTCON, &idptcon) && idptcon &&
      a_p->GetField(Packages::Dpt::ICBNDIMFLG, &icbndimflg) && icbndimflg &&
      a_p->GetField(Packages::Dpt::IADSORBIM, &iadsorbim) && iadsorbim &&
      a_p->GetField(Packages::Dpt::IDISPIM, &idispim) && idispim &&
      a_p->GetField(Packages::Dpt::IZODIM, &izodim) && izodim &&
      a_p->GetField(Packages::Dpt::IFODIM, &ifodim) && ifodim &&
      a_p->GetField(Packages::Dpt::IMSAT, &imsat) && imsat)
  {

    int idpf = 0; // IDPF = 1 if the DPF (Dual Porosity Flow) package is used (gwf2dpf1u1.f)

    if (*icbndimflg == 0) // The doc says the opposite in one place but is wrong.
    {
      ExportArray(ARR_DPT_ICBUNDIM, -1);
    }
    if (idpf == 0)
    {
      ExportArray(ARR_DPT_PHIF, -1);
    }
    ExportArray(ARR_DPT_PRSITYIM, -1);
    if (*iadsorbim != 0)
    {
      ExportArray(ARR_DPT_BULKDIM, -1);
    }
    if (idpf != 0)
    {
      ExportArray(ARR_DPT_DLIM, -1);
    }
    ExportArray(ARR_DPT_DDTTR, -1);
    if (idpf == 0 && *imsat == 3) // imsat == 3 means INPUTSAT option used
    {
      ExportArray(ARR_DPT_SIM, -1);
    }

    // Species-dependent variables
    MfPackage* pbct = GetGlobal()->GetPackage(Packages::BCT);
    ASSERT(pbct);
    if (!pbct)
      return false;

    const int *mcomp(0);
    pbct->GetField(Packages::Bct::MCOMP, &mcomp);
    ASSERT(mcomp);
    if (!mcomp)
      return false;

    int nSpecies = *mcomp;
    for (int speciesId = 1; speciesId <= nSpecies; ++speciesId)
    {
      if (*iadsorbim != 0)
      {
        ExportArray(ARR_DPT_ADSORBIM, speciesId);
      }
      if (*iadsorbim == 2)
      {
        ExportArray(ARR_DPT_FLICHIM, speciesId);
      }
      if (*izodim == 1 || *izodim == 3)
      {
        ExportArray(ARR_DPT_ZODRWIM, speciesId);
      }
      if (*iadsorbim != 0 && (*izodim == 2 || *izodim == 3))
      {
        ExportArray(ARR_DPT_ZODRSIM, speciesId);
      }
      if (*ifodim == 1 || *ifodim == 3)
      {
        ExportArray(ARR_DPT_FODRWIM, speciesId);
      }
      if (*iadsorbim != 0 && (*ifodim == 2 || *ifodim == 3))
      {
        ExportArray(ARR_DPT_FODRS, speciesId);
      }
      ExportArray(ARR_DPT_CONC, speciesId);
    }
  }

  WriteComments();
  WriteStoredLines();
  return true;
} // NativeExpDpt::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpDpt::AddToStoredLinesDesc (const char* a_line,
                                         const char* a_desc)
{
  MfPackage* p = GetGlobal()->GetPackage(Packages::DPT);
  if (!p || GetPackage() == p)
    NativePackExp::AddToStoredLinesDesc(a_line, a_desc);
  else
  {
    MfPackage* p1 = GetPackage();
    SetData(GetNative(), GetGlobal(), p);
    NativePackExp::AddToStoredLinesDesc(a_line, a_desc);
    SetData(GetNative(), GetGlobal(), p1);
  }
} // NativeExpDpt::AddToStoredLinesDesc

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private/MfData/MfExport/private/Native/NativeExpDpt.t.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpDptT::setUp ()
{
  //m_p = NULL;
  //Mf2kNative* n = new Mf2kNative;
  //MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  //MfPackage* dis = new MfPackage(Packages::LAK);
  //NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  //m_p = dynamic_cast<NativeExpDpt*>(p);
} // NativeExpDptT::setUp
//------------------------------------------------------------------------------
void NativeExpDptT::tearDown ()
{
  //m_p->UnitTestingDeletePointers();
  //delete(m_p);
} // NativeExpDptT::tearDown
//------------------------------------------------------------------------------
void NativeExpDptT::testCreateClass ()
{
  //TS_ASSERT(m_p);
} // NativeExpDptT::testCreateClass

#endif
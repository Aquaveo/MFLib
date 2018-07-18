//------------------------------------------------------------------------------
// FILE      NativeExpLgr.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/MfData/MfExport/private/Native/NativeExpLgr.h>

#include <private/MfData/MfExport/private/Mf2kNative.h>
#include <private/MfData/MfExport/private/TxtExporter.h>
#include <private/MfData/MfGlobal.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackStrings.h>

using namespace MfData::Export;
namespace
{
static std::vector<CStr>& Lines ()
{
  static std::vector<CStr> m_lines;
  return m_lines;
}
static std::vector<CStr>& Desc ()
{
  static std::vector<CStr> m_lines;
  return m_lines;
}
} // unnamed namespace

//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpLgr::NativeExpLgr () :
  m_lgr(0)
{
} // MfNativeExpLgr::MfNativeExpLgr
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpLgr::~NativeExpLgr ()
{
} // MfNativeExpLgr::~MfNativeExpLgr
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpLgr::Export ()
{
  CStr nm = GetPackage()->PackageName();
  if (nm != Packages::LGR_1 && nm != Packages::LGR_2 &&
      nm != Packages::LGR) return true;

  if (nm == Packages::LGR_1) Lgr_1();
  else if (nm == Packages::LGR_2) Lgr_2();
  else WriteFile();
  return true;
} // MfNativeExpLgr::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLgr::Lgr_1 ()
{
  const int *NGRIDS,*IUPBHSV,*IUPBFSV;
  MfPackage* a_p = GetPackage();
  if (a_p->GetField("NGRIDS", &NGRIDS) && NGRIDS &&
      a_p->GetField("IUPBHSV", &IUPBHSV) && IUPBHSV &&
      a_p->GetField("IUPBFSV", &IUPBFSV) && IUPBFSV)
  {
    CStr ln;
    AddToStoredLinesDesc("LGR", " 1.   LGR");
    ln.Format("%d", *NGRIDS);
    AddToStoredLinesDesc(ln.c_str(), " 2.   NGRIDS");
    ln = GetNative()->FileName();
    util::StripPathFromFilename(ln.c_str(), ln);
    util::StripExtensionFromFilename(ln.c_str(), ln);
    ln += ".mfn";
    AddToStoredLinesDesc(ln.c_str(), " 3.   NAME FILE");
    AddToStoredLinesDesc("PARENTONLY", " 4.   GRIDSTATUS");
    ln.Format("%d %d", *IUPBHSV, *IUPBFSV);
    AddToStoredLinesDesc(ln.c_str(), " 5.   IUPBHSV IUPBFSV");
  }
} // NativeExpLgr::Lgr_1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLgr::Lgr_2 ()
{
  MfPackage* a_p = GetPackage();
  const int *ISHFLG,*IBFLG,*IUCBHSV,*IUCBFSV,*MXLGRITER,*IOUTLGR,
            *NPLBEG,*NPRBEG,*NPCBEG,*NPLEND,*NPREND,*NPCEND,*NCPP,*NCPPL;
  const Real *RELAXH,*RELAXF,*HCLOSELGR,*FCLOSELGR;
  if (a_p->GetField("ISHFLG", &ISHFLG) && ISHFLG &&
      a_p->GetField("IBFLG", &IBFLG) && IBFLG &&
      a_p->GetField("IUCBHSV", &IUCBHSV) && IUCBHSV &&
      a_p->GetField("IUCBFSV", &IUCBFSV) && IUCBFSV &&
      a_p->GetField("MXLGRITER", &MXLGRITER) && MXLGRITER &&
      a_p->GetField("IOUTLGR", &IOUTLGR) && IOUTLGR &&
      a_p->GetField("NPLBEG", &NPLBEG) && NPLBEG &&
      a_p->GetField("NPRBEG", &NPRBEG) && NPRBEG &&
      a_p->GetField("NPCBEG", &NPCBEG) && NPCBEG &&
      a_p->GetField("NPLEND", &NPLEND) && NPLEND &&
      a_p->GetField("NPREND", &NPREND) && NPREND &&
      a_p->GetField("NPCEND", &NPCEND) && NPCEND &&
      a_p->GetField("NCPP", &NCPP) && NCPP &&
      a_p->GetField("NCPPL", &NCPPL) && NCPPL &&
      a_p->GetField("RELAXH", &RELAXH) && RELAXH &&
      a_p->GetField("RELAXF", &RELAXF) && RELAXF &&
      a_p->GetField("HCLOSELGR", &HCLOSELGR) && HCLOSELGR &&
      a_p->GetField("FCLOSELGR", &FCLOSELGR) && FCLOSELGR)
  {
    CStr ln = GetNative()->FileName();
    util::StripPathFromFilename(ln.c_str(), ln);
    util::StripExtensionFromFilename(ln.c_str(), ln);
    ln += ".mfn";
    AddToStoredLinesDesc(ln, " 6.   NAME FILE");
    AddToStoredLinesDesc("CHILDONLY", " 7.   GRIDSTATUS");
    ln.Format("%d %d %d %d", *ISHFLG, *IBFLG, *IUCBHSV, *IUCBFSV);
    AddToStoredLinesDesc(ln, " 8.   ISHFLG IBFLG IUCBHSV IUCBFSV");
    ln.Format("%d %d", *MXLGRITER, *IOUTLGR);
    AddToStoredLinesDesc(ln, " 9.   MXLGRITER IOUTLGR");
    ln.Format("%s %s", STR(*RELAXH), STR(*RELAXF));
    AddToStoredLinesDesc(ln, "10.   RELAXH RELAXF");
    ln.Format("%s %s", STR(*HCLOSELGR), STR(*FCLOSELGR));
    AddToStoredLinesDesc(ln, "11.   HCLOSELGR FCLOSELGR");
    ln.Format("%d %d %d", *NPLBEG, *NPRBEG, *NPCBEG);
    AddToStoredLinesDesc(ln, "12.   NPLBEG NPRBEG NPCBEG");
    ln.Format("%d %d %d", *NPLEND, *NPREND, *NPCEND);
    AddToStoredLinesDesc(ln, "13.   NPLEND NPREND NPCEND");
    ln.Format("%d", *NCPP);
    AddToStoredLinesDesc(ln, "14.   NCPP");
    ln = "";
    int nval = *NPLEND + 1 - *NPLBEG;
    for (int i=0; i<nval; i++)
    {
      CStr ln1;
      ln1.Format("%d ", NCPPL[i]);
      ln += ln1;
    }
    AddToStoredLinesDesc(ln, "15.   NCPPL");
  }
} // NativeExpLgr::Lgr_2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLgr::WriteFile ()
{
  if (Lines().empty()) return;
  CStr fname = GetGlobal()->LgrName();
  util::StripExtensionFromFilename(fname, fname);
  fname += ".lgr";
  TxtExporter txt(fname);
  txt.WriteLinesAndDescriptionsToFile("", Lines(), Desc());
  Lines().clear();
  Desc().clear();
} // NativeExpLgr::WriteFile
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLgr::OnSetData ()
{
  m_lgr = GetGlobal()->GetPackage(Packages::LGR);
  if (!m_lgr)
  {
    MfPackage p(Packages::LGR);
    GetGlobal()->AddPackage(&p);
    m_lgr = GetGlobal()->GetPackage(Packages::LGR);
  }
} // NativeExpLgr::OnSetData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpLgr::AddToStoredLinesDesc (const char* a_line,
                                         const char* a_desc)
{
  Lines().push_back(a_line);
  Desc().push_back(a_desc);
} // NativeExpLgr::AddToStoredLinesDesc


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private/MfData/MfExport/private/Native/NativeExpLgr.t.h>

#include <private/MfData/MfExport/private/Mf2kNative.h>
#include <private/MfData/MfExport/private/Native/NativeUtil.h>
#include <private/MfData/MfGlobal.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackStrings.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpLgrT::setUp ()
{
  m_p = NULL;
  Mf2kNative* n = new Mf2kNative;
  MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  MfPackage* dis = new MfPackage(Packages::LGR_1);
  NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  m_p = dynamic_cast<NativeExpLgr*>(p);
} // NativeExpLgrT::setUp
//------------------------------------------------------------------------------
void NativeExpLgrT::tearDown ()
{
  m_p->UnitTestingDeletePointers();
  delete(m_p);
} // NativeExpLgrT::tearDown
//------------------------------------------------------------------------------
void NativeExpLgrT::testCreateClass ()
{
  TS_ASSERT(m_p);
} // NativeExpLgrT::testCreateClass

#endif
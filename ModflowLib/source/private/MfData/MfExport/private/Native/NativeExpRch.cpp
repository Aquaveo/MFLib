//------------------------------------------------------------------------------
// FILE      NativeExpRch.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpRch.h>

#include <sstream>

#include <private\MfData\MfExport\private\H5\H5UseLastWriter.h>
#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\MfExportUtil.h>
#include <private\MfData\MfExport\private\Native\mf6\NativeExpMf6Rch.h>
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
NativeExpRch::NativeExpRch () :
  m_par()
, m_usg(false)
, m_unstructured(false)
{
  m_usg = MfExportUtil::MfIsUsgModelType();
  if (m_usg) m_unstructured = MfData::MfGlobal::Get().Unstructured() ? 1 : 0;
} // MfNativeExpRch::MfNativeExpRch
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpRch::~NativeExpRch ()
{
} // MfNativeExpRch::~MfNativeExpRch
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpRch::Export ()
{
  Mf2kNative* n1 = GetNative();
  if (n1 && n1->GetExportMf6())
  {    
    NativeExpMf6Rch rch(this);
    rch.Export();   
    return true;
  }
  
  if (GetGlobal()->GetCurrentPeriod() == 1)
  {
    m_par.Line1();
    Line2();
    m_par.Lines3to4(true);
    WriteComments();
  }

  Line5();
  Line6();
  Line8();
  return true;
} // MfNativeExpRch::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpRch::Line2 ()
{
  CStr desc = " 2. NRCHOP IRCHCB";
  if (m_usg) desc.Replace(" 2", "2a");

  const int *nrchop(0), *irchcb(0);
  MfPackage* a_p=GetPackage();
  if (!a_p->GetField(Packages::RCHpack::NRCHOP, &nrchop) || !nrchop ||
      !a_p->GetField("IRCHCB", &irchcb) || !irchcb)
    return;

  GetGlobal()->SetIntVar("RCH Spec Layer", *nrchop == 2 ? 1 : 0);
  CStr ln;
  ln.Format("%5d %5d", *nrchop, *irchcb);
  AddToStoredLinesDesc(ln, desc);
  if (m_unstructured && 2 == *nrchop)
  {
    desc = "2b. MXNDRCH";
    const int *mxndrch(0);
    if (a_p->GetField(Packages::RCHpack::MXNDRCH, &mxndrch) && mxndrch)
    {
      ln.Format("%5d", *mxndrch);
      AddToStoredLinesDesc(ln, desc);
    }
  }
} // NativeExpRch::Line2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpRch::Line5 ()
{
  CStr desc = " 5. INRECH INIRCH";
  const int *nrchop(0), *inirch(0), *inrech(0);
  MfPackage* a_p=GetPackage();
  if (!a_p->GetField(Packages::RCHpack::NRCHOP, &nrchop) || !nrchop ||
      !a_p->GetField(Packages::RCHpack::INIRCH, &inirch) || !inirch ||
      !a_p->GetField(Packages::RCHpack::INRECH, &inrech) || !inrech)
    return;

  int irch = 0;
  if (2 == *nrchop) irch = *inirch;
  CStr ln;
  ln.Format("%5d %5d", *inrech, irch);
  AddToStoredLinesDesc(ln, desc);
  if (GetNative()->GetUseH5())
  {
    std::vector<int> vDat(2,0);
    vDat[0] = *inrech < 0 ? 1 : 0;
    if (*nrchop == 2)
      vDat[1] = *inirch < 0 ? 1 : 0;
    H5UseLastWriter w(this);
    w.WriteData(vDat);
  }
} // NativeExpRch::Line5
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpRch::Line6 ()
{
  const int *inrech(0);
  MfPackage* a_p=GetPackage();
  if (!a_p->GetField(Packages::RCHpack::INRECH, &inrech) || !inrech) return;
  if (*inrech < 0) return;

  // get parameters with RCH type
  if (m_par.WriteStressPar(GetGlobal()->GetCurrentPeriod())) return;

  CStr desc = " 6. RECH(NCOL,NROW)";
  MfPackage* p = GetGlobal()->GetPackage(ARR_RCH_RCH);
  //if (!p) return;
  // it is possible that we have rch params defined by GMS (no "real" clusters)
  // and so we need a line here for stuff to work when this file gets written
  // with parameters
  if (!p)
  {
    if (m_par.NumPar() > 0) AddToStoredLinesDesc("CONSTANT 1.0", desc);
    return;
  }

  //desc.Format("%s%d", " 6. RECH(NCOL,NROW)   SP ",
  //            GetGlobal()->GetCurrentPeriod());
  std::vector<CStr>& l(p->StringsToWrite());
  AddToStoredLinesDesc(l[0], desc);
  if (MfExportUtil::ArrayWriteNextLineInternal(GetNative(), l[0]))
  {
    AddToStoredLinesDesc(l[1], "");
  }
  p->StringsToWrite().clear();
} // NativeExpRch::Line6
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpRch::Line8 ()
{
  MfPackage* p = GetGlobal()->GetPackage(ARR_RCH_LAY);
  if (!p) return;
  const int *inirch(0), *nrchop(0);
  MfPackage* a_p=GetPackage();
  if (!a_p->GetField(Packages::RCHpack::NRCHOP, &nrchop) || !nrchop ||
      !a_p->GetField(Packages::RCHpack::INIRCH, &inirch) || !inirch) return;
  if (*nrchop != 2 || *inirch < 1) return;

  CStr desc = " 8. IRCH(NCOL,NROW)";
  //desc.Format("%s%d", " 8. IRCH(NCOL,NROW)   SP ",
  //            GetGlobal()->GetCurrentPeriod());
  std::vector<CStr>& l(p->StringsToWrite());
  AddToStoredLinesDesc(l[0], desc);
  if (MfExportUtil::ArrayWriteNextLineInternal(GetNative(), l[0]))
  {
    AddToStoredLinesDesc(l[1], "");
  }
  p->StringsToWrite().clear();
} // NativeExpRch::Line8
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpRch::LastChanceBeforeWriting ()
{
  if (m_par.NumPar() < 1) return;
  m_par.RewriteFileWithParameters();
} // NativeExpRch::LastChanceBeforeWriting
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpRch::OnSetData ()
{
  m_par.SetPackage(this);
} // NativeExpRch::OnSetData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpRch::RewriteFileWithParameters ()
{
  // check if parameters already written
  if (m_par.ClustersDefinedFromStart()) return;

  std::vector<CStr> oldLine = GetPackage()->StringsToWrite(),
                    oldDesc = GetPackage()->StringDescriptions();
  std::vector<CStr> &lines(GetPackage()->StringsToWrite()),
                    &descs(GetPackage()->StringDescriptions());
  lines.clear();
  descs.clear();
  CStr ln;
  ln.Format("PARAMETER %5d", m_par.NumPar());
  AddToStoredLinesDesc(ln, " 1. [PARAMETER NPRCH]");
  // add back line 2
  AddToStoredLinesDesc(oldLine[0], oldDesc[0]);
  m_par.Lines3to4(false);
  int curLine = 1;
  for (int i=0; i<GetGlobal()->NumPeriods(); ++i)
  {
    // get the values of inrech and inirch
    int inrech, inirch;
    std::stringstream ss;
    ss << oldLine[curLine];
    ss >> inrech >> inirch;
    AddToStoredLinesDesc(oldLine[curLine], oldDesc[curLine]);
    if (inrech > -1) // write parameters unless inrech < 1
    {
      m_par.LineWithPar(i+1, lines.back());
      curLine++; // skip old line 5
      CStr ln;
      std::stringstream ss;
      ss << oldLine[curLine];
      ss >> ln >> ln;
      // we don't need this array file so remove it
      CStr fname = GetNative()->FileName();
      util::StripFileFromFilename(fname, fname);
      fname += ln;
      remove(fname);
    }

    if (inirch >= 0)
    {
      curLine++;
      // add back line 8
      AddToStoredLinesDesc(oldLine[curLine], oldDesc[curLine]);
    }
    curLine++;
  }

} // NativeExpRch::RewriteFileWithParameters


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData\MfExport\private\Native\NativeExpRch.t.h>

#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackStrings.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpRchT::setUp ()
{
  m_p = NULL;
  Mf2kNative* n = new Mf2kNative;
  MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  MfPackage* dis = new MfPackage(Packages::RCH);
  NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  m_p = dynamic_cast<NativeExpRch*>(p);
} // NativeExpRchT::setUp
//------------------------------------------------------------------------------
void NativeExpRchT::tearDown ()
{
  m_p->UnitTestingDeletePointers();
  delete(m_p);
} // NativeExpRchT::tearDown
//------------------------------------------------------------------------------
void NativeExpRchT::testCreateClass ()
{
  TS_ASSERT(m_p);
} // NativeExpRchT::testCreateClass

#endif
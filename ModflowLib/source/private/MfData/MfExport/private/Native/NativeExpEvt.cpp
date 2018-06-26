//------------------------------------------------------------------------------
// FILE      NativeExpEvt.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpEvt.h>

#include <private\MfData\MfExport\private\H5\H5UseLastWriter.h>
#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\MfExportUtil.h>
#include <private\MfData\MfExport\private\Native\mf6\NativeExpMf6Evt.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackFields.h>
#include <private\MfData\Packages\MfPackStrings.h>

using namespace MfData::Export;
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpEvt::NativeExpEvt () :
  m_par()
, m_usg(false)
, m_unstructured(false)
{
  m_usg = MfExportUtil::MfIsUsgModelType();
  if (m_usg) m_unstructured = MfData::MfGlobal::Get().Unstructured() ? 1 : 0;
} // MfNativeExpEvt::MfNativeExpEvt
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpEvt::~NativeExpEvt ()
{
} // MfNativeExpEvt::~MfNativeExpEvt
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpEvt::Export ()
{
  Mf2kNative* n1 = GetNative();
  if (n1 && n1->GetExportMf6())
  {    
    NativeExpMf6Evt evt(this);
    evt.Export();   
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
  Line7();
  Line9();
  Line10();
  return true;
} // MfNativeExpEvt::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpEvt::Line2 ()
{
  const int *nevtop(0), *ievtcb(0);
  MfPackage* a_p = GetPackage();
  if (!a_p->GetField(Packages::EVTpack::NEVTOP, &nevtop) || !nevtop ||
      !a_p->GetField("IEVTCB", &ievtcb) || !ievtcb)
    return;

  CStr ln, desc = " 2. NEVTOP IEVTCB";
  if (m_usg) desc.Replace(" 2", "2a");
  ln.Format("%5d %5d", *nevtop, *ievtcb);
  AddToStoredLinesDesc(ln, desc);
  if (m_usg && 2 == *nevtop && m_unstructured)
  {
    desc = "2b. MXNDEVT";
    const int *mxndevt(0);
    if (a_p->GetField(Packages::EVTpack::MXNDEVT, &mxndevt) && mxndevt)
    {
      ln.Format("%5d", *mxndevt);
      AddToStoredLinesDesc(ln, desc);
    }
  }
} // MfNativeExpEvt::Line2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpEvt::Line5 ()
{
  const int *nevtop(0), *insurf(0), *inevtr(0), *inexdp(0), *inievt(0);
  MfPackage* a_p = GetPackage();
  if (!a_p->GetField(Packages::EVTpack::NEVTOP, &nevtop) || !nevtop ||
      !a_p->GetField(Packages::EVTpack::INSURF, &insurf) || !insurf ||
      !a_p->GetField(Packages::EVTpack::INEVTR, &inevtr) || !inevtr ||
      !a_p->GetField(Packages::EVTpack::INEXDP, &inexdp) || !inexdp ||
      !a_p->GetField(Packages::EVTpack::INIEVT, &inievt) || !inievt)
    return;

  GetGlobal()->SetIntVar("EVT Spec Layer", *nevtop == 2 ? 1 : 0);
  int ievt = 0;
  if (2 == *nevtop) ievt = *inievt;
  CStr ln;
  ln.Format("%5d %5d %5d %5d", *insurf, *inevtr, *inexdp, ievt);
  AddToStoredLinesDesc(ln, " 5. INSURF INEVTR INEXDP INIEVT");
  if (GetNative()->GetUseH5())
  {
    std::vector<int> vDat(4,0);
    vDat[0] = *insurf < 0 ? 1 : 0;
    vDat[1] = *inevtr < 0 ? 1 : 0;
    vDat[2] = *inexdp < 0 ? 1 : 0;
    if (*nevtop == 2)
      vDat[3] = *inievt < 0 ? 1: 0;
    H5UseLastWriter w(this);
    w.WriteData(vDat);
  }
} // NativeExpEvt::Line5
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpEvt::Line6 ()
{
  const int *insurf(0);
  MfPackage* a_p = GetPackage();
  if (!a_p->GetField(Packages::EVTpack::INSURF, &insurf) || !insurf ||
      *insurf < 0)
    return;

  MfPackage* p = GetGlobal()->GetPackage(ARR_EVT_SURF);
  if (!p) return;

  CStr desc = " 6. SURF(NCOL,NROW)";
  ArrayToFile(p, desc);
} // NativeExpEvt::Line6
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpEvt::Line7 ()
{
  const int *inevtr(0);
  MfPackage* a_p = GetPackage();
  if (!a_p->GetField(Packages::EVTpack::INEVTR, &inevtr) || !inevtr ||
      *inevtr < 0)
    return;

  // get parameters with EVT type
  if (m_par.WriteStressPar(GetGlobal()->GetCurrentPeriod())) return;

  MfPackage* p = GetGlobal()->GetPackage(ARR_EVT_RATE);
  if (!p) return;

  CStr desc = " 7. EVTR(NCOL,NROW)";
  ArrayToFile(p, desc);
} // NativeExpEvt::Line7
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpEvt::Line9 ()
{
  const int *inexdp(0);
  MfPackage* a_p = GetPackage();
  if (!a_p->GetField(Packages::EVTpack::INEXDP, &inexdp) || !inexdp ||
      *inexdp < 0)
    return;

  MfPackage* p = GetGlobal()->GetPackage(ARR_EVT_EXT);
  if (!p) return;

  CStr desc = " 9. EXDP(NCOL,NROW)";
  ArrayToFile(p, desc);
} // NativeExpEvt::Line9
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpEvt::Line10 ()
{
  const int *nevtop(0), *inievt(0);
  MfPackage* a_p = GetPackage();
  if (!a_p->GetField(Packages::EVTpack::NEVTOP, &nevtop) || !nevtop ||
      !a_p->GetField(Packages::EVTpack::INIEVT, &inievt) || !inievt)
    return;
  if (2 != *nevtop || *inievt < 0) return;

  MfPackage* p = GetGlobal()->GetPackage(ARR_EVT_LAY);
  if (!p) return;

  CStr desc = "10. IEVT(NCOL,NROW)";
  ArrayToFile(p, desc);
} // NativeExpEvt::Line10
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpEvt::ArrayToFile (MfData::MfPackage* p, const CStr& a_desc)
{
  CStr l = p->StringsToWrite()[0];
  AddToStoredLinesDesc(l, a_desc);
  if (MfExportUtil::ArrayWriteNextLineInternal(GetNative(), l))
  {
    AddToStoredLinesDesc(p->StringsToWrite()[1], "");
  }
  p->StringsToWrite().clear();
} // NativeExpEvt::ArrayToFile
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpEvt::LastChanceBeforeWriting ()
{
  if (m_par.NumPar() < 1) return;
  m_par.RewriteFileWithParameters();
} // NativeExpEvt::LastChanceBeforeWriting
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpEvt::OnSetData ()
{
  m_par.SetPackage(this);
} // NativeExpEvt::OnSetData

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData\MfExport\private\Native\NativeExpEvt.t.h>

#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackStrings.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpEvtT::setUp ()
{
  m_p = NULL;
  Mf2kNative* n = new Mf2kNative;
  MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  MfPackage* dis = new MfPackage(Packages::EVT);
  NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  m_p = dynamic_cast<NativeExpEvt*>(p);
} // NativeExpEvtT::setUp
//------------------------------------------------------------------------------
void NativeExpEvtT::tearDown ()
{
  m_p->UnitTestingDeletePointers();
  delete(m_p);
} // NativeExpEvtT::tearDown
//------------------------------------------------------------------------------
void NativeExpEvtT::testCreateClass ()
{
  TS_ASSERT(m_p);
} // NativeExpEvtT::testCreateClass

#endif
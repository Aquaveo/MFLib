//------------------------------------------------------------------------------
// FILE      NativeExpEts.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpEts.h>

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
NativeExpEts::NativeExpEts ()
{
} // NativeExpEts::MfNativeExpEts
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpEts::~NativeExpEts ()
{
} // NativeExpEts::~MfNativeExpEts
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpEts::Export ()
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
    WriteComments();
    Line1();
    m_par.Lines3to4(true);
  }
  Line4();
  Line5();
  Line6();
  Line8();
  Line9();
  Line10and11();
  return true;
} // NativeExpEts::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpEts::Line1 ()
{
  const int *nevtop(0), *ietscb(0), *netseg(0);
  MfPackage* a_p = GetPackage();
  if (!a_p->GetField(Packages::EVTpack::NEVTOP, &nevtop) || !nevtop ||
      !a_p->GetField("IETSCB", &ietscb) || !ietscb ||
      !a_p->GetField(Packages::EVTpack::NETSEG, &netseg) || !netseg)
    return;

  GetGlobal()->SetIntVar("ETS Spec Layer", *nevtop == 2 ? 1 : 0);
  GetGlobal()->SetIntVar("ETS NETSEG", *netseg);
  CStr ln;
  int npar=0;
  if (m_par.ClustersDefinedFromStart()) npar = m_par.NumPar();
  ln.Format("%5d %5d %5d %5d", *nevtop, *ietscb, npar, *netseg);
  CStr desc = " 1. NETSOP IETSCB NPETS NETSEG";
  AddToStoredLinesDesc(ln, desc);
  if (GetNative()->GetUseH5())
  {
    H5UseLastWriter w(this);
    w.WriteEtsNetSeg(*netseg);
  }
} // NativeExpEts::Line1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpEts::Line4 ()
{
  const int *nevtop(0), *insurf(0), *inevtr(0), *inexdp(0), *inievt(0),
            *netseg(0), *insgdf(0);
  MfPackage* a_p = GetPackage();
  if (!a_p->GetField(Packages::EVTpack::NEVTOP, &nevtop) || !nevtop ||
      !a_p->GetField(Packages::EVTpack::INSURF, &insurf) || !insurf ||
      !a_p->GetField(Packages::EVTpack::INEVTR, &inevtr) || !inevtr ||
      !a_p->GetField(Packages::EVTpack::INEXDP, &inexdp) || !inexdp ||
      !a_p->GetField(Packages::EVTpack::INIEVT, &inievt) || !inievt ||
      !a_p->GetField(Packages::EVTpack::NETSEG, &netseg) || !netseg ||
      !a_p->GetField(Packages::EVTpack::INSGDF, &insgdf) || !insgdf)
    return;

  CStr ln;
  ln.Format("%5d %5d %5d ", *insurf, *inevtr, *inexdp);
  if (2 == *nevtop || *netseg > 1)
  {
    CStr tmp;
    tmp.Format("%5d ", *inievt);
    ln += tmp;
  }
  if (*netseg > 1)
  {
    CStr tmp;
    tmp.Format("%5d ", *insgdf);
    ln += tmp;
  }

  CStr desc = " 4. INETSS INETSR INETSX [INIETS [INSGDF]]";
  AddToStoredLinesDesc(ln, desc);
  if (GetNative()->GetUseH5())
  {
    std::vector<int> vDat(5,0);
    vDat[0] = *insurf < 0 ? 1 : 0;
    vDat[1] = *inevtr < 0 ? 1 : 0;
    vDat[2] = *inexdp < 0 ? 1 : 0;
    if (*nevtop == 2)
      vDat[3] = *inievt < 0 ? 1: 0;
    int insgdfOut = 0;
    if (*netseg > 1)
      insgdfOut = *insgdf;
    vDat[4] = insgdfOut < 0 ? 1 : 0;
    H5UseLastWriter w(this);
    w.WriteData(vDat);
  }
} // NativeExpEts::Line4
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpEts::Line5 ()
{
  const int *insurf(0);
  MfPackage* a_p = GetPackage();
  if (!a_p->GetField(Packages::EVTpack::INSURF, &insurf) || !insurf ||
      *insurf < 0)
    return;

  MfPackage* p = GetGlobal()->GetPackage(ARR_ETS_SURF);
  if (!p) return;

  CStr desc = " 5. ETSS(NCOL,NROW)";
  ArrayToFile(p, desc);
} // NativeExpEts::Line5
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpEts::Line6 ()
{
  const int *inevtr(0);
  MfPackage* a_p = GetPackage();
  if (!a_p->GetField(Packages::EVTpack::INEVTR, &inevtr) || !inevtr ||
      *inevtr < 0)
    return;

  // get parameters with EVT type
  if (m_par.WriteStressPar(GetGlobal()->GetCurrentPeriod())) return;

  MfPackage* p = GetGlobal()->GetPackage(ARR_ETS_RATE);
  if (!p) return;

  CStr desc = " 6. ETSR(NCOL,NROW)";
  ArrayToFile(p, desc);
} // NativeExpEts::Line6
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpEts::Line8 ()
{
  const int *inexdp(0);
  MfPackage* a_p = GetPackage();
  if (!a_p->GetField(Packages::EVTpack::INEXDP, &inexdp) || !inexdp ||
      *inexdp < 0)
    return;

  MfPackage* p = GetGlobal()->GetPackage(ARR_ETS_EXT);
  if (!p) return;

  CStr desc = " 8. ETSX(NCOL,NROW)";
  ArrayToFile(p, desc);
} // NativeExpEts::Line8
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpEts::Line9 ()
{
  const int *nevtop(0), *inievt(0);
  MfPackage* a_p = GetPackage();
  if (!a_p->GetField(Packages::EVTpack::NEVTOP, &nevtop) || !nevtop ||
      !a_p->GetField(Packages::EVTpack::INIEVT, &inievt) || !inievt)
    return;
  if (2 != *nevtop || *inievt < 0) return;

  MfPackage* p = GetGlobal()->GetPackage(ARR_ETS_LAY);
  if (!p) return;

  CStr desc = " 9. IETS(NCOL,NROW)";
  ArrayToFile(p, desc);
} // NativeExpEts::Line9
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpEts::Line10and11 ()
{
  const int *netseg(0), *insgdf(0);
  MfPackage* a_p = GetPackage();
  if (!a_p->GetField(Packages::EVTpack::NETSEG, &netseg) || !netseg ||
      !a_p->GetField(Packages::EVTpack::INSGDF, &insgdf) || !insgdf)
    return;
  if (*insgdf < 0 || *netseg < 2) return;

  MfPackage* p = GetGlobal()->GetPackage(ARR_ETS_PXDP);
  if (!p) return;
  MfPackage* p2 = GetGlobal()->GetPackage(ARR_ETS_PETM);
  if (!p2) return;

  CStr desc =  "10. PXDP(NCOL,NROW)";
  CStr desc2 = "11. PETM(NCOL,NROW)";
  std::vector<CStr> &s(p->StringsToWrite()), &s2(p2->StringsToWrite());
  int cnt(0), cnt2(0);
  for (int i=0; i<(*netseg-1); ++i)
  {
    AddToStoredLinesDesc(s[cnt], desc);
    if (MfExportUtil::ArrayWriteNextLineInternal(GetNative(), s[cnt]))
    {
      cnt++;
      AddToStoredLinesDesc(s[cnt], "");
    }
    AddToStoredLinesDesc(s2[cnt2], desc2);
    if (MfExportUtil::ArrayWriteNextLineInternal(GetNative(), s2[cnt2]))
    {
      cnt2++;
      AddToStoredLinesDesc(s2[cnt2], "");
    }
    cnt++;
    cnt2++;
  }
  s.clear();
  s2.clear();
} // NativeExpEts::Line10
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpEts::ArrayToFile (MfData::MfPackage* p, const CStr& a_desc)
{
  CStr l = p->StringsToWrite()[0];
  AddToStoredLinesDesc(l, a_desc);
  if (MfExportUtil::ArrayWriteNextLineInternal(GetNative(), l))
  {
    AddToStoredLinesDesc(p->StringsToWrite()[1], "");
  }
  p->StringsToWrite().clear();
} // NativeExpEts::ArrayToFile
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpEts::LastChanceBeforeWriting ()
{
  if (m_par.NumPar() < 1) return;
  m_par.RewriteFileWithParameters();
} // NativeExpEts::LastChanceBeforeWriting
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpEts::OnSetData ()
{
  m_par.SetPackage(this);
} // NativeExpEts::OnSetData


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData\MfExport\private\Native\NativeExpEts.t.h>

#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackStrings.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpEtsT::setUp ()
{
  m_p = NULL;
  Mf2kNative* n = new Mf2kNative;
  MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  MfPackage* dis = new MfPackage(Packages::ETS);
  NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  m_p = dynamic_cast<NativeExpEts*>(p);
} // NativeExpEtsT::setUp
//------------------------------------------------------------------------------
void NativeExpEtsT::tearDown ()
{
  m_p->UnitTestingDeletePointers();
  delete(m_p);
} // NativeExpEtsT::tearDown
//------------------------------------------------------------------------------
void NativeExpEtsT::testCreateClass ()
{
  TS_ASSERT(m_p);
} // NativeExpEtsT::testCreateClass

#endif
//------------------------------------------------------------------------------
// FILE      NativeExpSTP.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeExpSTP.h>

#include <Export.h>
#include <private\H5DataReader\H5DataSetWriter.h>
#include <private\MfData\MfExport\private\H5\H5BcList.h>
#include <private\MfData\MfExport\private\H5\H5UseLastWriter.h>
#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\Native\NativeExpNam.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfExport\private\TxtExporter.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackStrings.h>

using namespace MfData::Export;
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpSTP::NativeExpSTP ()
{
} // MfNativeExpSTP::MfNativeExpSTP
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
NativeExpSTP::~NativeExpSTP ()
{
} // MfNativeExpSTP::~MfNativeExpSTP
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool NativeExpSTP::Export ()
{
  // write zone file
  MfPackage* zn = GetGlobal()->GetPackage(MfData::Packages::ZON);
  NativePackExp* exZn = NativeUtil::CreatePackExp(GetNative(), GetGlobal(), zn);
  if (exZn) exZn->Export(); delete(exZn);
  // write mult file
  MfPackage* mlt = GetGlobal()->GetPackage(MfData::Packages::MLT);
  NativePackExp* exMlt = NativeUtil::CreatePackExp(GetNative(), GetGlobal(), mlt);
  if (exMlt) exMlt->Export(); delete(exMlt);
  // write the pval file
  GetGlobal()->SetIntVar("Write PVAL", 1);
  MfPackage* pvl = GetGlobal()->GetPackage(MfData::Packages::PVAL);
  NativePackExp* exPvl = NativeUtil::CreatePackExp(GetNative(), GetGlobal(), pvl);
  if (exPvl) exPvl->Export(); delete(exPvl);
  // write MNW2
  MfPackage* mnw2 = GetGlobal()->GetPackage(MfData::Packages::MNW2);
  if (mnw2)
  {
    mnw2->SetLineNumber("Export Final");
    NativePackExp* exMnw2 = NativeUtil::CreatePackExp(GetNative(), GetGlobal(), mnw2);
    if (exMnw2) exMnw2->Export(); delete(exMnw2);
  }

  ForcePackageWrite(MfData::Packages::SEN1); // must be before the name file
//  if (!GetNative()->StpFlag())
  {
    // write the name file
    MfPackage* nm = GetGlobal()->GetPackage(MfData::Packages::NAM);
    if (GetH5Flag()) NativeUtil::ExportNextToH5();
    NativePackExp* ex = NativeUtil::CreatePackExp(GetNative(), GetGlobal(), nm);
    NativeExpNam* exNm = dynamic_cast<NativeExpNam*>(ex);
    if (exNm) exNm->WriteFileStp(); delete(exNm);
  }

  ForcePackageWrite(MfData::Packages::RCH);
  ForcePackageWrite(MfData::Packages::EVT);
  ForcePackageWrite(MfData::Packages::ETS);
  ForcePackageWrite(MfData::Packages::LAK);
  ForcePackageWrite(MfData::Packages::MNW);
  ForcePackageWrite(MfData::Packages::LGR);
  // write the oc file
  MfPackage* oc = GetGlobal()->GetPackage(MfData::Packages::OC);
  if (oc)
  {
    NativePackExp* exOc = NativeUtil::CreatePackExp(GetNative(), GetGlobal(), oc);
    if (exOc) exOc->WriteComments(); exOc->WriteStoredLines(); delete(exOc);
  }

  if (GetH5Flag())
  {
    H5BcList h(this);
    h.WriteMapIdsForListBcs();
    H5UseLastWriter ul(this);
    ul.CheckArealFromUseLast();
    H5DataReader::CloseAllH5FilesOpenForWriting();
  }

  // copy world file and prj file if they exist
  CopyAdditionalFiles();
  return true;
} // MfNativeExpSTP::Export
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSTP::ForcePackageWrite (const char* const a_)
{
  MfPackage* p = GetGlobal()->GetPackage(a_);
  if (p)
  {
    NativePackExp* exP = NativeUtil::CreatePackExp(GetNative(), GetGlobal(), p);
    if (exP)
    {
      exP->LastChanceBeforeWriting();
      exP->WriteStoredLines();
      delete(exP);
    }
  }
} // NativeExpSTP::ForcePackageWrite
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void NativeExpSTP::CopyAdditionalFiles ()
{
  if (!mfLibExp_Exporting()) return;

  CStr nf, base, outBase, mwf, prj, outMwf, outPrj, vtu, gsf, outVtu, outGsf;
  outBase = GetNative()->GetExp()->GetBaseFileName();
  GetGlobal()->GetStrVar("NAME_FILE_STR", nf);
  util::StripExtensionFromFilename(nf.c_str(), base);
  mwf = base + ".mfw";
  prj = base + ".prj";
  vtu = base + ".vtu";
  gsf = base + ".gsf";
  outMwf = outBase + ".mfw";
  outPrj = outBase + ".prj";
  outVtu = outBase + ".vtu";
  outGsf = outBase + ".gsf";
  FILE* fp = fopen(mwf.c_str(), "r");
  if (fp)
  {
    fclose(fp);
    fp = NULL;
    util::FileCopy(mwf, outMwf);
  }
  fp = fopen(prj.c_str(), "r");
  if (fp)
  {
    fclose(fp);
    fp = NULL;
    util::FileCopy(prj, outPrj);
  }
  fp = fopen(vtu.c_str(), "r");
  if (fp)
  {
    fclose(fp);
    fp = NULL;
    util::FileCopy(vtu, outVtu);
  }
  fp = fopen(gsf.c_str(), "r");
  if (fp)
  {
    fclose(fp);
    fp = NULL;
    util::FileCopy(gsf, outGsf);
  }
} // NativeExpSTP::CopyAdditionalFiles


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData\MfExport\private\Native\NativeExpSTP.t.h>

#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackStrings.h>

using namespace MfData;
//------------------------------------------------------------------------------
void NativeExpSTPT::setUp ()
{
  m_p = NULL;
  Mf2kNative* n = new Mf2kNative;
  MfGlobal* g= new MfGlobal(10, 15, 3, 2, 3, 2, 0);
  MfPackage* dis = new MfPackage("STP");
  NativePackExp* p = NativeUtil::CreatePackExp(n, g, dis);
  m_p = dynamic_cast<NativeExpSTP*>(p);
} // NativeExpSTPT::setUp
//------------------------------------------------------------------------------
void NativeExpSTPT::tearDown ()
{
  m_p->UnitTestingDeletePointers();
  delete(m_p);
} // NativeExpSTPT::tearDown
//------------------------------------------------------------------------------
void NativeExpSTPT::testCreateClass ()
{
  TS_ASSERT(m_p);
} // NativeExpSTPT::testCreateClass

#endif
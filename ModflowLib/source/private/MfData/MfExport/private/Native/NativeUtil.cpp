//------------------------------------------------------------------------------
// FILE      MfNativeUtil.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\Native\NativeUtil.h>

#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\MfExportUtil.h>
#include <private\MfData\MfExport\private\Native\NativeExpArr1d.h>
#include <private\MfData\MfExport\private\Native\NativeExpArr2d.h>
#include <private\MfData\MfExport\private\Native\NativeExpBas.h>
#include <private\MfData\MfExport\private\Native\NativeExpBcf.h>
#include <private\MfData\MfExport\private\Native\NativeExpBct.h>
#include <private\MfData\MfExport\private\Native\NativeExpCln.h>
#include <private\MfData\MfExport\private\Native\NativeExpDis.h>
#include <private\MfData\MfExport\private\Native\NativeExpDisu.h>
#include <private\MfData\MfExport\private\Native\NativeExpEts.h>
#include <private\MfData\MfExport\private\Native\NativeExpEvt.h>
#include <private\MfData\MfExport\private\Native\NativeExpGag.h>
#include <private\MfData\MfExport\private\Native\NativeExpGnc.h>
#include <private\MfData\MfExport\private\Native\NativeExpHfb.h>
#include <private\MfData\MfExport\private\Native\NativeExpHuf.h>
#include <private\MfData\MfExport\private\Native\NativeExpLak.h>
#include <private\MfData\MfExport\private\Native\NativeExpLgr.h>
#include <private\MfData\MfExport\private\Native\NativeExpLpf.h>
#include <private\MfData\MfExport\private\Native\NativeExpLstobs.h>
#include <private\MfData\MfExport\private\Native\NativeExpLstPack.h>
#include <private\MfData\MfExport\private\Native\NativeExpLstPar.h>
#include <private\MfData\MfExport\private\Native\NativeExpMlt.h>
#include <private\MfData\MfExport\private\Native\NativeExpMnw1.h>
#include <private\MfData\MfExport\private\Native\NativeExpMnw2.h>
#include <private\MfData\MfExport\private\Native\NativeExpMnwi.h>
#include <private\MfData\MfExport\private\Native\NativeExpNam.h>
#include <private\MfData\MfExport\private\Native\NativeExpObs.h>
#include <private\MfData\MfExport\private\Native\NativeExpOc.h>
#include <private\MfData\MfExport\private\Native\NativeExpPes.h>
#include <private\MfData\MfExport\private\Native\NativeExpPval.h>
#include <private\MfData\MfExport\private\Native\NativeExpRch.h>
#include <private\MfData\MfExport\private\Native\NativeExpSen.h>
#include <private\MfData\MfExport\private\Native\NativeExpSfr.h>
#include <private\MfData\MfExport\private\Native\NativeExpSolver.h>
#include <private\MfData\MfExport\private\Native\NativeExpSTP.h>
#include <private\MfData\MfExport\private\Native\NativeExpStr.h>
#include <private\MfData\MfExport\private\Native\NativeExpSub.h>
#include <private\MfData\MfExport\private\Native\NativeExpSwi.h>
#include <private\MfData\MfExport\private\Native\NativeExpUzf.h>
#include <private\MfData\MfExport\private\Native\NativeExpVdf.h>
#include <private\MfData\MfExport\private\Native\NativeExpVsc.h>
#include <private\MfData\MfExport\private\Native\NativeExpZon.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackStrings.h>
#include <private\util\util.h>

using namespace MfData;
using namespace MfData::Export;

namespace
{
//------------------------------------------------------------------------------
bool ClnPack (const CStr& type)
{
  if (Packages::CLNLines0And1 == type ||
      Packages::CLN == type ||
      Packages::CLNLine2 == type ||
      Packages::CLNLine3 == type ||
      Packages::CLNLine4To6 == type ||
      Packages::CLNLine13 == type ||
      Packages::CLNLine14 == type ||
      Packages::CLNLine15 == type ||
      Packages::CLNLine16 == type)
    return true;
  return false;
} // ClnPack
//------------------------------------------------------------------------------
bool HobPack (const CStr& type)
{
  if ("OB1" == type || Packages::HOB == type) return true;
  return false;
} // HobPack
//------------------------------------------------------------------------------
bool IsFlowObsPackage (const CStr& type)
{
  if (Packages::FOB == type ||
      "OB2" == type || "OV2" == type ||
      "OB3" == type || "OV3" == type ||
      "OB4" == type || "OV4" == type ||
      "OB5" == type || "OV5" == type ||
      "OB6" == type || "OV6" == type ||
      "OB7" == type || "OV7" == type ||
      "OB8" == type || "OV8" == type) return true;
  return false;
} // IsFlowObsPackage
//------------------------------------------------------------------------------
bool Array2d (const CStr& a_type,
              Mf2kNative* a_native)
{
  if (MfExportUtil::IsDataArray(a_type, a_native->GetMapArrays())) return true;
  else if ("K22" == a_type || "K33" == a_type) return true;
  return false;
} // Array2d
//------------------------------------------------------------------------------
bool LstPack (const CStr& type)
{
  if (Packages::DRN == type || Packages::RIV == type ||
      Packages::WEL == type || Packages::GHB == type ||
      Packages::CHD == type || Packages::DRT == type) return true;
  return false;
} // LstPack
//------------------------------------------------------------------------------
bool LakPack (const CStr& type)
{
  if (Packages::LAK == type || Packages::LAKSP == type) return true;
  return false;
} // LakPack
//------------------------------------------------------------------------------
bool Mnw1Pack (const CStr& type)
{
  if (Packages::MNW == type || Packages::MNWSetup == type ||
      Packages::MNWStressPeriod == type) return true;
  return false;
} // Mnw1Pack
//------------------------------------------------------------------------------
bool SfrPack (const CStr& type)
{
  if (Packages::SFRLine1 == type || Packages::SFRLine2 == type ||
      Packages::SFRLine5 == type || Packages::SFRLine6 == type ||
      "SFR_CONDFACT" == type) return true;
  return false;
} // SfrPack
//------------------------------------------------------------------------------
bool SubPack (const CStr& type)
{
  if (Packages::SUB == type || Packages::SUBLine15 == type ||
      Packages::SUBLine16 == type) return true;
  return false;
} // SubPack
//------------------------------------------------------------------------------
bool UzfPack (const CStr& type)
{
  if (Packages::UZF == type || Packages::UZFLine1 == type ||
      Packages::UZFLine8 == type ||
      Packages::UZFStressPeriod == type) return true;
  return false;
} // UzfPack
//------------------------------------------------------------------------------
bool StrPack (const CStr& type)
{
  if (Packages::STRSP == type || "STR_CONDFACT" == type) return true;
  return false;
} // StrPack
//------------------------------------------------------------------------------
bool LgrPack (const CStr& type)
{
  if (Packages::LGR_1 == type || Packages::LGR_2 == type ||
      Packages::LGR == type) return true;
  return false;
} // LgrPack
//------------------------------------------------------------------------------
bool VdfPack (const CStr& type)
{
  if (Packages::VDFLine5 == type || Packages::VDFStressPeriod == type)
    return true;
  return false;
} // VdfPack
//------------------------------------------------------------------------------
bool VscPack (const CStr& type)
{
  if (Packages::VSCLine3 == type || Packages::VSCStressPeriod == type)
    return true;
  return false;
} // VscPack
} // unnamed namespace
//------------------------------------------------------------------------------
/// \brief returns the right type of MfExporter.
//------------------------------------------------------------------------------
NativePackExp* NativeUtil::CreatePackExp (Mf2kNative* a_native,
                                          MfGlobal* a_global,
                                          MfPackage* a_package)
{
  using namespace MfData;
  NativePackExp *ret(NULL);
  if (!a_package) return ret;

  bool h5(a_native->GetUseH5());
  CStr type(a_package->PackageName());
  if (Array2d(type, a_native))                  ret = new NativeExpArr2d();
  else if (MfExportUtil::Is1dArray(type))       ret = new NativeExpArr1d();
  else if (MfExportUtil::IsSolver(type))        ret = new NativeExpSolver();
  else if (Packages::NAM == type)               ret = new NativeExpNam();
  else if (ClnPack(type))                       ret = new NativeExpCln();
  else if (Packages::DIS == type)               ret = new NativeExpDis();
  else if (Packages::DISU == type)              ret = new NativeExpDisu();
  else if (Packages::BAS == type)               ret = new NativeExpBas();
  else if (Packages::BCF == type)               ret = new NativeExpBcf();
  else if ("L98" == type)                       ret = new NativeExpLpf();
  else if (Packages::UPW == type)               ret = new NativeExpLpf();
  else if (Packages::HUF == type)               ret = new NativeExpHuf();
  else if (Packages::ZON == type)               ret = new NativeExpZon();
  else if (Packages::MLT == type)               ret = new NativeExpMlt();
  else if (Packages::OC == type)                ret = new NativeExpOc();
  else if (Packages::OCT == type)               ret = new NativeExpOc();
  else if (Packages::PVAL == type)              ret = new NativeExpPval();
  else if (Packages::SEN1 == type)              ret = new NativeExpSen();
  else if ("FNC" == type)                       ret = new NativeExpMlt();
  else if (Packages::HFB == type)               ret = new NativeExpHfb();
  else if (Packages::PES == type)               ret = new NativeExpPes();
  else if (LstPack(type))                       ret = new NativeExpLstPack();
  else if (Packages::LPRM == type)              ret = new NativeExpLstPar();
  else if (Packages::RCH == type)               ret = new NativeExpRch();
  else if (Packages::ETS == type)               ret = new NativeExpEts();
  else if (Packages::EVT == type)               ret = new NativeExpEvt();
  else if (LakPack(type))                       ret = new NativeExpLak();
  else if (Packages::GAGE == type)              ret = new NativeExpGag();
  else if (Mnw1Pack(type))                      ret = new NativeExpMnw1();
  else if (Packages::MNW2 == type)              ret = new NativeExpMnw2();
  else if (Packages::MNWI == type)              ret = new NativeExpMnwi();
  else if (SfrPack(type))                       ret = new NativeExpSfr();
  else if (HobPack(type))                       ret = new NativeExpObs();
  else if (IsFlowObsPackage(type))              ret = new NativeExpLstObs();
  else if (SubPack(type))                       ret = new NativeExpSub();
  else if (UzfPack(type))                       ret = new NativeExpUzf();
  else if (StrPack(type))                       ret = new NativeExpStr();
  else if (LgrPack(type))                       ret = new NativeExpLgr();
  else if (Packages::GNC == type)               ret = new NativeExpGnc();
  else if (Packages::SWI == type)               ret = new NativeExpSwi();
  else if (VdfPack(type))                       ret = new NativeExpVdf();
  else if (VscPack(type))                       ret = new NativeExpVsc();
  else if (Packages::BCT == type)               ret = new NativeExpBct();
  // leave at end. This is the last "package" processed
  else if ("STP" == type)                       ret = new NativeExpSTP();
  else if (Packages::BIN == type)
  {
    int hasBinary = 1;
    a_global->SetIntVar("BINARY_EXPORT", hasBinary);
  }

  if (ret)
  {
    ret->SetData(a_native, a_global, a_package);
    ret->SetH5Flag(h5);
  }
  return(ret);
} // MfExportUtil::CreatePackExp



#if 0
///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData\MfExport\private\MfExportUtil.t.h>

//------------------------------------------------------------------------------
void MfExportUtilT::testGetExporter_WrongString ()
{
  MfData::Export::MfExporterImpl *ex;
  ex = MfData::Export::MfExportUtil::CreateExporter("stuff");
  TS_ASSERT(ex);
  if (ex)
    delete(ex);
  // TODO
  //TS_FAIL("implement other exporters");
}
#endif
#endif

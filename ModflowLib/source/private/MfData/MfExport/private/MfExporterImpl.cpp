//------------------------------------------------------------------------------
// FILE      MfExporterImpl.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------

#include <private\MfData\MfExport\private\MfExporterImpl.h>

#include <private\MfData\MfExport\private\TxtExporter.h>
#include <private\MfData\Packages\MfPackStrings.h>
using namespace MfData::Export;
namespace
{
struct ArrayInfo
{
  ArrayInfo() {}
  ArrayInfo(CStr a_pack, CStr a_ds, CStr a_par, CStr a_mf)
    : m_package(a_pack), m_H5dataset(a_ds), m_partype(a_par), m_mfVarName(a_mf)
  {}

  CStr m_package;
  CStr m_H5dataset;
  CStr m_partype;
  CStr m_mfVarName;
};
void ArrayMap (std::map<CStr, ArrayInfo>& a_map)
{
  namespace PA = MfData::Packages;
  a_map.clear();
  // BAS
  a_map[ARR_BAS_IBND] = ArrayInfo(PA::BAS, "ibound", "", "IBOUND");
  a_map[ARR_BAS_SHEAD] = ArrayInfo(PA::BAS, "StartHead", "", "STRT");
  // DIS
  a_map[ARR_DIS_TOP] = ArrayInfo(PA::DIS, "top", "", "Top");
  a_map[ARR_DIS_BOT] = ArrayInfo(PA::DIS, "bot", "", "BOTM");
  a_map[ARR_DIS_VCB] = ArrayInfo(PA::DIS, "vcb", "", "BOTM");
  // BCF
  a_map[ARR_BCF_HY] = ArrayInfo(PA::BCF, "HY_", "HK", "HY");
  a_map[ARR_BCF_HK_U] = ArrayInfo(PA::BCF, "HY_", "HK", "HY");
  a_map[ARR_BCF_TRAN] = ArrayInfo(PA::BCF, "TRAN_", "", "TRAN");
  a_map[ARR_BCF_VCONT] = ArrayInfo(PA::BCF, "LEAK_", "", "Vcont");
  a_map[ARR_BCF_SF1] = ArrayInfo(PA::BCF, "SF1_", "", "Sf1");
  a_map[ARR_BCF_SF2] = ArrayInfo(PA::BCF, "SF2_", "", "Sf2");
  a_map[ARR_BCF_WET] = ArrayInfo(PA::BCF, "WET_", "", "WETDRY");
  // HUF
  a_map[ARR_HUF_TOP] = ArrayInfo(PA::HUF, "top", "", "TOP");
  a_map[ARR_HUF_THCK] = ArrayInfo(PA::HUF, "thick", "", "THCK");
  a_map[ARR_HUF_WET] = ArrayInfo(PA::HUF, "WET_", "", "WETDRY");
  // LPF
  a_map[ARR_LPF_HK] = ArrayInfo(PA::LPF, "HK", "HK", "HK");
  a_map[ARR_LPF_HANI] = ArrayInfo(PA::LPF, "HANI", "HANI", "HANI");
  a_map[ARR_LPF_VK] = ArrayInfo(PA::LPF, "VK", "VK", "VK");
  a_map[ARR_LPF_VANI] = ArrayInfo(PA::LPF, "VANI", "VANI", "VANI");
  a_map[ARR_LPF_SS] = ArrayInfo(PA::LPF, "SS", "SS", "SS");
  a_map[ARR_LPF_SY] = ArrayInfo(PA::LPF, "SY", "SY", "SY");
  a_map[ARR_LPF_WET] = ArrayInfo(PA::LPF, "WET", "", "WETDRY");
  a_map[ARR_LPF_VKCBD] = ArrayInfo(PA::LPF, "QUASIVK", "VKCB", "VKCB");
  a_map[ARR_LPF_ANGX] = ArrayInfo(PA::LPF, "ANGLEX", "", "ANGLEX");
  // RCH
  a_map[ARR_RCH_RCH] = ArrayInfo(PA::RCH, "Recharge/07. Property", "RCH", "RECH");
  a_map[ARR_RCH_LAY] = ArrayInfo(PA::RCH, "Recharge/09. Layer", "", "IRCH");
  // EVT
  a_map[ARR_EVT_SURF] = ArrayInfo(PA::EVT, "ET/07. Property", "", "SURF");
  a_map[ARR_EVT_RATE] = ArrayInfo(PA::EVT, "ET/07. Property", "EVT", "EVTR");
  a_map[ARR_EVT_EXT] = ArrayInfo(PA::EVT, "ET/07. Property", "", "EXDP");
  a_map[ARR_EVT_LAY] = ArrayInfo(PA::EVT, "ET/09. Layer", "", "IEVT");
  // ETS
  a_map[ARR_ETS_SURF] = ArrayInfo(PA::ETS, "ETS/07. Property", "", "ETSS");
  a_map[ARR_ETS_RATE] = ArrayInfo(PA::ETS, "ETS/07. Property", "ETS", "ETSR");
  a_map[ARR_ETS_EXT] = ArrayInfo(PA::ETS, "ETS/07. Property", "", "ETSX");
  a_map[ARR_ETS_LAY] = ArrayInfo(PA::ETS, "ETS/09. Layer", "", "IETS");
  a_map[ARR_ETS_PXDP] = ArrayInfo(PA::ETS, "ETS/16. Ext Depth", "", "PXDP");
  a_map[ARR_ETS_PETM] = ArrayInfo(PA::ETS, "ETS/18. Evap Rate", "", "PETM");
  // uzf
  a_map[ARR_UZF_UBND] = ArrayInfo(PA::UZF, "UZF/12. IUZFBND", "", "IUZFBND");
  a_map[ARR_UZF_RBND] = ArrayInfo(PA::UZF, "UZF/14. IRUNBND", "", "IRUNBND");
  a_map[ARR_UZF_VKS] = ArrayInfo(PA::UZF, "UZF/16. VKS", "", "VKS");
  a_map[ARR_UZF_EPS] = ArrayInfo(PA::UZF, "UZF/18. EPS", "", "EPS");
  a_map[ARR_UZF_THTS] = ArrayInfo(PA::UZF, "UZF/20. THTS", "", "THTS");
  a_map[ARR_UZF_THTI] = ArrayInfo(PA::UZF, "UZF/22. THTI", "", "THTI");
  a_map[ARR_UZF_RCH] = ArrayInfo(PA::UZF, "UZF/07. Property", "", "FINF");
  a_map[ARR_UZF_ET] = ArrayInfo(PA::UZF, "UZF/07. Property", "", "PET");
  a_map[ARR_UZF_EXT] = ArrayInfo(PA::UZF, "UZF/07. Property", "", "EXTDP");
  a_map[ARR_UZF_EXTWC] = ArrayInfo(PA::UZF, "UZF/07. Property", "", "EXTWC");
  // sub
  a_map[ARR_SUB_RNB] = ArrayInfo(PA::SUB, "SUB/12. RNB", "", "RNB");
  a_map[ARR_SUB_HC] = ArrayInfo(PA::SUB, "SUB/14. HC", "", "HC");
  a_map[ARR_SUB_SFE] = ArrayInfo(PA::SUB, "SUB/16. Sfe", "", "Sfe");
  a_map[ARR_SUB_SFV] = ArrayInfo(PA::SUB, "SUB/18. Sfv", "", "Sfv");
  a_map[ARR_SUB_COM] = ArrayInfo(PA::SUB, "SUB/20. Com", "", "Com");
  a_map[ARR_SUB_DSTRT] = ArrayInfo(PA::SUB, "SUB/22. Dstart", "", "Dstart");
  a_map[ARR_SUB_DHC] = ArrayInfo(PA::SUB, "SUB/24. DHC", "", "DHC");
  a_map[ARR_SUB_DCOM] = ArrayInfo(PA::SUB, "SUB/26. DCOM", "", "DCOM");
  a_map[ARR_SUB_DZ] = ArrayInfo(PA::SUB, "SUB/28. DZ", "", "DZ");
  a_map[ARR_SUB_NZ] = ArrayInfo(PA::SUB, "SUB/30. NZ", "", "NZ");
  // SWI
  a_map[ARR_SWI_ZETA] = ArrayInfo(PA::SWI, "ZETA", "", "ZETA");
  a_map[ARR_SWI_SSZ] = ArrayInfo(PA::SWI, "SSZ", "", "SSZ");
  a_map[ARR_SWI_ISOURCE] = ArrayInfo(PA::SWI, "ISOURCE", "", "ISOURCE");
  // LAK
  a_map[ARR_LAK_ID] = ArrayInfo(PA::LAK, "Lak_", "", "ID");
  a_map[ARR_LAK_LEAK] = ArrayInfo(PA::LAK, "LakLeak_", "", "BDLKNC");
  // VDF
  a_map[ARR_VDF_DENS] = ArrayInfo(PA::VDF, "VDF/07. Property", "", "DENSE");
  a_map[ARR_VDF_CONC] = ArrayInfo(PA::VDF, "VDF/07. Property", "", "DCONC");
  // VSC
  a_map[ARR_VSC_VSC] = ArrayInfo(PA::VSC, "VSC/07. Property", "", "VISC");
  a_map[ARR_VSC_CONC] = ArrayInfo(PA::VSC, "VSC/07. Property", "", "VCONC");
} // ArrayMap
void InitArrayMap (std::map<CStr, CStr>& m_map)
{
  m_map.clear();
  std::map<CStr, ArrayInfo> theMap;
  ArrayMap(theMap);
  std::map<CStr, ArrayInfo>::iterator it = theMap.begin();
  for (; it != theMap.end(); ++it)
    m_map.insert(std::make_pair(it->first, it->second.m_H5dataset));
} // InitArrayMap
} // unnamed namespace
//------------------------------------------------------------------------------
/// \brief Constructor
//------------------------------------------------------------------------------
MfExporterImpl::MfExporterImpl (const char *a_,
                                bool a_compress/*true*/) :
    m_typeName(a_)
  , m_fileName()
  , m_tables()
  , m_modelType(0)
  , m_compress(a_compress)
  , m_exp(new TxtExporter(""))
  , m_types()
  , m_map()
{
  InitExtensions();
  InitArrayMap(m_map);
} // MfExporterImpl::MfExporterImpl
//------------------------------------------------------------------------------
/// \brief Constructor
//------------------------------------------------------------------------------
MfExporterImpl::~MfExporterImpl ()
{
  if (m_exp) delete(m_exp);
} // MfExporterImpl::~MfExporterImpl
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
const char* MfExporterImpl::GetTypeName ()
{
   return m_typeName.c_str();
} // MfExporterImpl::GetTypeName
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfExporterImpl::SetFileName (const char *a_)
{
  // The file name needs some sort of path, even if the user didn't
  // give one.  If no path is given, add ".\" to the front of the filename
  CStr temp1, temp2;
  temp1.Format("%s", a_);
  util::StripPathFromFilename(temp1, temp2);
  if (temp1 == temp2) // No path was given
    m_fileName.Format(".\\%s", a_); // Add a path of ".\"
  else
    m_fileName.Format("%s", a_); // The filename already has a path
  CStr base;
  util::StripExtensionFromFilename(a_, base);
  m_exp->SetBaseFileName(base);
} // MfExporterImpl::SetFileName
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfExporterImpl::SetTablesStr (const char *a_)
{
  m_tables = a_;
} // MfExporterImpl::SetTablesStr
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
const char* MfExporterImpl::GetTablesStr ()
{
  return (m_tables.c_str());
} // MfExporterImpl::GetTablesStr
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfExporterImpl::SetModelType (int a_modelType)
{
  m_modelType = a_modelType;
} // MfExporterImpl::SetModelType
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
int MfExporterImpl::GetModelType ()
{
  return m_modelType;
} // MfExporterImpl::GetModelType
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool MfExporterImpl::CanExportTable (const char *a_)
{
  if (m_tables.length() < 1)
    return true;

  if (m_tables.Find(a_) != -1)
    return true;
  return false;
} // MfExporterImpl::CanExportPackage
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfExporterImpl::InitExtensions ()
{
  using namespace MfData::Packages;
  m_types.insert(std::make_pair(BAS6, "ba6"));
  m_types.insert(std::make_pair(BCF, "bc6"));
  m_types.insert(std::make_pair(BCF6, "bc6"));
  m_types.insert(std::make_pair(CHD, CHD));
  m_types.insert(std::make_pair(CLN, CLN));
  m_types.insert(std::make_pair(DE4, DE4));
  m_types.insert(std::make_pair(DIS, DIS));
  m_types.insert(std::make_pair(DISU, DISU));
  m_types.insert(std::make_pair(DRN, DRN));
  m_types.insert(std::make_pair(DRT, DRT));
  m_types.insert(std::make_pair(EVT, EVT));
  m_types.insert(std::make_pair(ETS, ETS));
  m_types.insert(std::make_pair(GAGE, "gag"));
  m_types.insert(std::make_pair(GHB, GHB));
  m_types.insert(std::make_pair(GLOBAL, "glo"));
  m_types.insert(std::make_pair(GMG, GMG));
  m_types.insert(std::make_pair(GNC, GNC));
  m_types.insert(std::make_pair(HFB, "hfb"));
  m_types.insert(std::make_pair(HUF2, HUF));
  m_types.insert(std::make_pair(HUF, HUF));
  m_types.insert(std::make_pair(LAK, LAK));
  m_types.insert(std::make_pair(LIST, "out"));
  m_types.insert(std::make_pair(LMG, LMG));
  //m_types.insert(std::make_pair(LMT6, "lmt"));
  m_types.insert(std::make_pair(LPF, LPF));
  m_types.insert(std::make_pair(MLT, "mlt"));
  m_types.insert(std::make_pair(MNW, "mnw"));
  m_types.insert(std::make_pair(MNW2,MNW2));
  m_types.insert(std::make_pair(MNWI,MNWI));
  m_types.insert(std::make_pair(NAM, "mfn"));
  m_types.insert(std::make_pair(NWT,NWT));
  m_types.insert(std::make_pair(OC, OC));
  m_types.insert(std::make_pair(PCG, PCG));
  m_types.insert(std::make_pair(PVAL, "pval"));
  m_types.insert(std::make_pair(PES, PES));
  m_types.insert(std::make_pair(RCH, RCH));
  m_types.insert(std::make_pair(RIV, RIV));
  m_types.insert(std::make_pair(SEN, SEN));
  m_types.insert(std::make_pair(SFR, SFR));
  m_types.insert(std::make_pair(SIP, SIP));
  m_types.insert(std::make_pair(SMS, SMS));
  m_types.insert(std::make_pair(SOR, SOR));
  m_types.insert(std::make_pair(STRSP, STRSP));
  m_types.insert(std::make_pair(SUB, SUB));
  // SWI is only supported for native text. See NativeExpNam::WriteFileStp
  //m_types.insert(std::make_pair(SWI, SWI));
  m_types.insert(std::make_pair(UPW, UPW));
  m_types.insert(std::make_pair(UZF, UZF));
  m_types.insert(std::make_pair(WEL, WEL));
  m_types.insert(std::make_pair(ZON, "zon"));

  m_exp->SetTypesToExtensions(m_types);
} // MfExporterImpl::InitExtensions
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
CStr MfExporterImpl::PackageFromArrayName (CStr a_name)
{
  CStr rval;
  std::map<CStr, ArrayInfo> theMap;
  ArrayMap(theMap);
  if (theMap.find(a_name) != theMap.end())
  {
    rval = theMap[a_name].m_package;
  }
  return rval;
} // MfExporterImpl::PackageFromArrayName
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
CStr MfExporterImpl::ParamTypeFromArrayName (CStr a_name)
{
  CStr rval;
  std::map<CStr, ArrayInfo> theMap;
  ArrayMap(theMap);
  if (theMap.find(a_name) != theMap.end())
  {
    rval = theMap[a_name].m_partype;
  }
  return rval;
} // MfExporterImpl::PackageFromArrayName
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
CStr MfExporterImpl::VarNameFromArrayName (CStr a_name)
{
  CStr rval;
  std::map<CStr, ArrayInfo> theMap;
  ArrayMap(theMap);
  std::map<CStr, ArrayInfo>::const_iterator it = theMap.find(a_name);
  if (it != theMap.end())
  {
    const ArrayInfo& a = it->second;
    rval = a.m_mfVarName;
  }
  return rval;
} // MfExporterImpl::VarNameFromArrayName
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void MfExporterImpl::BuildUniqueName (const CStr& a_baseName,
                                      const CStr& a_extension,
                                      int a_unitNumber,
                                      std::set<CStr>& a_uniqueNames,
                                      CStr& a_fileName)
{
  a_fileName = a_baseName;
  a_fileName += ".";
  a_fileName += a_extension;
  CStr fileLower(a_fileName);
  fileLower.ToLower();
  if (a_uniqueNames.find(fileLower) != a_uniqueNames.end())
  {
    CStr append;
    append.Format("_UNIT_%d", a_unitNumber); // unit numbers are unique

    a_fileName = a_baseName;
    a_fileName += ".";
    a_fileName += a_extension;
    a_fileName += append;

    fileLower = a_fileName;
    fileLower.ToLower();
  }
  a_uniqueNames.insert(fileLower);
} // MfExporterImpl::BuildUniqueName



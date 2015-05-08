//------------------------------------------------------------------------------
// FILE      ExpGmsH5.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private\MfData\MfExport\private\ExpGmsH5.h>

#include <hash_map>
#include <map>
#include <math.h>
#include <set>
#include <sstream>

#include <RunTest.h>

#include <private\H5DataReader\H5DataReaderUtil.h>
#include <private\H5DataReader\H5DataSetReader.h>
#include <private\H5DataReader\H5DataSetWriter.h>
#include <private\H5DataReader\H5DataSetWriterSetup.h>
#include <private\MfData\MfExport\private\MfExportUtil.h>
#include <private\ListReader\CellIdToIJK.h>
#include <private\MfData\MfExport\private\MfExportUtil.h>
#include <private\MfData\MfExport\private\Mf2kNative.h>
#include <private\MfData\MfExport\private\Native\H5BcList.h>
#include <private\MfData\MfExport\private\Native\H5Strings.h>
#include <private\MfData\MfExport\private\Native\Mnw1PropList.h>
#include <private\MfData\MfExport\private\Native\NativeUtil.h>
#include <private\MfData\MfGlobal.h>
#include <private\MfData\MfPackageUtil.h>
#include <private\MfData\Packages\MfPackage.h>
#include <private\MfData\Packages\MfPackFields.h>
#include <private\MfData\Packages\MfPackStrings.h>
#include <private\MfData\Packages\ObsHd.h>
#include <private\MfData\MfExport\private\TxtExporter.h>
#include <private\MNWReader.h>
#include <private\Parameters.h>
#include <private\Parameters\Param.h>
#include <private\Parameters\ParamList.h>
#include <private\util\EReadAsciiFile.h>

using namespace MfData;
using namespace Export;
using std::vector;
using std::set;

//-----CONSTANTS----------------------------------------------------------------
#define PARAM_FILE_TYPES "DRN DRT GHB RIV CHD Q   STR SFR"

enum enumNameFileType { NF_MODFLOW, NF_MT3D, NF_SEAWAT };

////////////////////////////////////////////////////////////////////////////////
/// \class ExpGmsH5::impl
////////////////////////////////////////////////////////////////////////////////
class ExpGmsH5::impl
{
public:
  impl() : m_nativeExp(0)
  {}
  ~impl()
  {
    if (m_nativeExp) delete(m_nativeExp);
  }

  bool ExportNative (MfGlobal* a_global,
                     MfPackage* a_package,
                     TxtExporter* a_exp);
  void SetFileName (const char * const a_) { m_fname = a_; }
  Mf2kNative* m_nativeExp;
  CStr m_fname;
};
////////////////////////////////////////////////////////////////////////////////
/// \struct SensitivityItem
////////////////////////////////////////////////////////////////////////////////
//------------------------------------------------------------------------------
/// \brief Used to store SEN item info between reading the sensitivity file and
///        writing it.
//------------------------------------------------------------------------------
struct SensitivityItem
{
  CStr name;
  int isens;
  int ln;
  Real b;
  Real bl;
  Real bu;
  Real bscal;
};

////////////////////////////////////////////////////////////////////////////////
/// \class SensitivityHeader
////////////////////////////////////////////////////////////////////////////////
//------------------------------------------------------------------------------
/// \brief Used to store SEN header info between reading the sensitivity file
///        and writing.
//------------------------------------------------------------------------------
struct SensitivityHeader
{
  int isenall;
  int iuhead;
  int iprints;
  int isensu;
  int isenpu;
  int isenfm;
  int nplist;
};

class ExpGmsH5Public
{
public:
  ExpGmsH5Public() :
    m_paramFileWasExported(0)
  , m_MultArrayMap()
  , m_ArrayOrderVector()
  , m_ZoneArrayMap()
  , m_ExportedPackages()
  , m_SubPackageArrayCounts()
  , m_needsSENFile(0)
  , m_SensitivityHeader()
  , m_SensitivityItems()
  , m_NAM_ftype()
  , m_NAM_fname()
  , m_NAM_niu()
  , m_NAM_maxStrLen(0)
  , m_HasBinaryExport(0)
  {}

  virtual ~ExpGmsH5Public()
  {}

  bool m_paramFileWasExported;
  std::map<CStr, CStr> m_MultArrayMap;
  std::vector<CStr> m_ArrayOrderVector;
  std::map<CStr, CStr> m_ZoneArrayMap;
  std::set<CStr> m_ExportedPackages;
  std::map<CStr, int> m_SubPackageArrayCounts;
  bool m_needsSENFile;
  SensitivityHeader m_SensitivityHeader;
  std::vector<SensitivityItem> m_SensitivityItems;
  std::vector<CStr> m_NAM_ftype;
  std::vector<CStr> m_NAM_fname;
  std::vector<int> m_NAM_niu;
  int m_NAM_maxStrLen;
  bool m_HasBinaryExport;

};

static void CreateH5Dataset(const CStr &a_file,
                            const CStr &a_group,
                            const CStr &a_path,
                            const hid_t a_type,
                            const int a_dim,
                            const bool a_compress);
static bool CreateDefaultMfH5File(const char *a_,
                                  int a_modelType=1,
                                  bool a_compress=false);
static std::map<CStr,std::vector<int> > &GetChunkMap();
static void expNameFile(MfPackage *a_package,
                        TxtExporter *a_exp,
                        int a_write,
                        int a_modelType,
                        ExpGmsH5* a_h5);
static void expSuperFile(int a_model_type,
                         MfPackage* a_package,
                         TxtExporter *a_exp);
static void expParamFile(MfPackage* a_package,
                         TxtExporter *a_exp);
static void expSEN(TxtExporter *a_exp);
static void saveSEN(MfPackage *a_pSen1,
                    MfPackage *a_pSen,
                    TxtExporter* a_exp);
static void expFinalize(MfGlobal* a_global,
                        TxtExporter *a_exp);
static void showWarnings(TxtExporter* a_exp);
static void expCheckArealFromUseLast(int a_nCells,
                                     const char * a_baseName);
static bool& iNeedsSENFile();
static bool& iHasBinaryExport(TxtExporter* a_exp);
static void WriteDataSetWithZeros (CStr& f, CStr& path, hsize_t dim[3],
                                   hsize_t start[3]);

//------------------------------------------------------------------------------
/// \brief Creates new class and passes ownership to the receiving class
//------------------------------------------------------------------------------
ExpGmsH5Public* New_ExpGmsH5Public ()
{
  ExpGmsH5Public* p = new ExpGmsH5Public();
  return p;
} // New_ExpGmsH5Public
void Delete_ExpGmsH5Public (ExpGmsH5Public* a_)
{
  delete(a_);
} // Delete_ExpGmsH5Public
//------------------------------------------------------------------------------
/// \brief holds a flag
//------------------------------------------------------------------------------
static bool& iWasParamFileExported (TxtExporter* a_exp)
{
  return a_exp->m_public->m_paramFileWasExported;
} // iWasParamFileExported
//------------------------------------------------------------------------------
/// \brief Set that holds the packages that have been exported
//------------------------------------------------------------------------------
static std::set<CStr> &ExportedPackages (TxtExporter* a_exp)
{
  return a_exp->m_public->m_ExportedPackages;
} // expExportedPackages
//------------------------------------------------------------------------------
/// \brief Constructor
//------------------------------------------------------------------------------
ExpGmsH5::ExpGmsH5 (bool a_compress/*=true*/) :
  MfExporterImpl("GmsH5", a_compress)
, m_p(new ExpGmsH5::impl())
{
} // ExpGmsH5::ExpGmsH5
//------------------------------------------------------------------------------
/// \brief Constructor
//------------------------------------------------------------------------------
ExpGmsH5::ExpGmsH5 (const char *a_fileName) : 
  MfExporterImpl("GmsH5", true)
, m_p(new ExpGmsH5::impl())
{
  SetFileName(a_fileName);
} // ExpGmsH5::ExpGmsH5
//------------------------------------------------------------------------------
/// \brief Destructor
//------------------------------------------------------------------------------
ExpGmsH5::~ExpGmsH5 ()
{
  if (m_p) delete(m_p);
} // ExpGmsH5::~ExpGmsH5
//------------------------------------------------------------------------------
/// \brief Sets the filename
//------------------------------------------------------------------------------
void ExpGmsH5::SetFileName (const char *a_)
{
  MfExporterImpl::SetFileName(a_);
  m_p->SetFileName(a_);
  CStr base;
  util::StripExtensionFromFilename(a_, base);
  GetExp()->SetBaseFileName(base);
  CreateDefaultMfH5File(base, GetModelType(), Compress());
} // ExpGmsH5::SetFileName
//------------------------------------------------------------------------------
static bool iUzfPack (const CStr& a_)
{
  bool rval(0);
  //return rval;
  if (   Packages::UZFLine1 == a_
      || Packages::UZFLine8 == a_
      || Packages::UZFStressPeriod == a_
      || ARR_UZF_UBND == a_
      || ARR_UZF_RBND == a_
      || ARR_UZF_VKS == a_
      || ARR_UZF_EPS == a_
      || ARR_UZF_THTS == a_
      || ARR_UZF_THTI == a_
      || ARR_UZF_RCH == a_
      || ARR_UZF_ET == a_
      || ARR_UZF_EXT == a_
      || ARR_UZF_EXTWC == a_
     )
  {
    rval = true;
  }
  return rval;
} // iUzfPack
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
static bool iArrayToNative (const CStr& a_, MfGlobal* a_global)
{
  bool rval(0);
  if (!a_global) return rval;
  if (a_global->ModelType() == USG && 
        (   Packages::Disu::NODLAY == a_
         || Packages::Disu::TOP == a_
         || Packages::Disu::BOT == a_
         || Packages::Disu::AREA == a_
         || Packages::Disu::IA == a_
         || Packages::Disu::JA == a_
         || Packages::Disu::IVC == a_
         || Packages::Disu::CL1 == a_
         || Packages::Disu::CL2 == a_
         || Packages::Disu::CL12 == a_
         || Packages::Disu::FAHL == a_
        )
      ) rval = true;
  else if (   ARR_BAS_IBND == a_
           || ARR_BAS_SHEAD == a_
           || ARR_DIS_TOP == a_
           || ARR_DIS_BOT == a_
           || ARR_DIS_VCB == a_
           || Packages::DisPack::DELR == a_
           || Packages::DisPack::DELC == a_
           || ARR_BCF_HY == a_
           || ARR_BCF_TRAN == a_
           || ARR_BCF_TRAN_U == a_
           || ARR_BCF_HK_U == a_
           || ARR_BCF_VCONT == a_
           || ARR_BCF_SF1 == a_
           || ARR_BCF_SF2 == a_
           || ARR_BCF_TRPY == a_
           || ARR_BCF_WET == a_
           || ARR_LPF_HK == a_
           || ARR_LPF_HANI == a_
           || ARR_LPF_VK == a_
           || ARR_LPF_VANI == a_
           || ARR_LPF_SS == a_
           || ARR_LPF_SY == a_
           || ARR_LPF_VKCBD == a_
           || ARR_LPF_ANGX == a_
           || ARR_LPF_WET == a_
           || ARR_HUF_TOP == a_
           || ARR_HUF_THCK == a_
           || ARR_HUF_WET == a_
           || ARR_LAK_ID == a_
           || ARR_LAK_LEAK == a_
           || ARR_SUB_RNB == a_
           || ARR_SUB_HC == a_
           || ARR_SUB_SFE == a_
           || ARR_SUB_SFV == a_
           || ARR_SUB_COM == a_
           || ARR_SUB_DSTRT == a_
           || ARR_SUB_DHC == a_
           || ARR_SUB_DCOM == a_
           || ARR_SUB_DZ == a_
           || ARR_SUB_NZ == a_
           || ARR_RCH_RCH == a_
           || ARR_RCH_LAY == a_
           || ARR_EVT_SURF == a_
           || ARR_EVT_RATE == a_
           || ARR_EVT_EXT == a_
           || ARR_EVT_LAY == a_
           || ARR_ETS_SURF == a_
           || ARR_ETS_RATE == a_
           || ARR_ETS_EXT == a_
           || ARR_ETS_LAY == a_
           || ARR_ETS_PXDP == a_
           || ARR_ETS_PETM == a_
           || ARR_SWI_SSZ == a_
           || ARR_SWI_ISOURCE == a_
           || ARR_VDF_DENS == a_
           || ARR_VDF_CONC == a_
           || ARR_VSC_VSC == a_
           || ARR_VSC_CONC == a_
           )
  {
    NativeUtil::ExportNextToH5();
    rval = true;
  }
  else if (   a_.find(ARR_HUF_TOP) != -1
           || a_.find(ARR_HUF_THCK) != -1
           || a_.find("ZONE ARRAY:") != -1
           || a_.find("MULT. ARRAY:") != -1
           || a_.find("ZETA SURFACE") != -1
           )
  {
    NativeUtil::ExportNextToH5();
    rval = true;
  }
  return rval;
} // iArrayToNative
//------------------------------------------------------------------------------
static bool iSfrPack (const CStr& a_)
{
  if (   Packages::SFRLine1 == a_
      || Packages::SFRLine2 == a_
      || Packages::SFRLine5 == a_
      || Packages::SFRLine6 == a_
     ) return true;
  return false;
} // iSfrPack
//------------------------------------------------------------------------------
static bool iLstPack (const CStr& a_)
{
  if (   Packages::CHD == a_
      || Packages::DRN == a_
      || Packages::DRT == a_
      || Packages::RIV == a_
      || Packages::GHB == a_
      || Packages::WEL == a_
      || Packages::LPRM == a_
     ) return true;
  return false;
} // iLstPack
//------------------------------------------------------------------------------
/// \brief Uses the native exporter to write the files
//------------------------------------------------------------------------------
static bool iPackageToNativeExport (
  const CStr& a_,
  MfGlobal* a_global,
  MfPackage* a_pack)
{
  bool rval = false;
  CStr packName(a_);

  if (Packages::LPRM == packName)
  {
    Packages::GetPackNameFromParameter(a_pack, packName);
  }

  if (   MfExportUtil::IsSolver(packName)
      //|| Packages::NAM == packName
      || Packages::PVAL == packName
      //|| Packages::SEN1 == packName
      || Packages::DISU == packName
      || Packages::DIS == packName
      || Packages::BCF == packName
      || Packages::BAS == packName
      || "L98" == packName
      || Packages::HUF == packName
      || Packages::UPW == packName
      || Packages::OC == packName
      || Packages::OCT == packName
      || "STP" == packName
      || "PES" == packName
      || Packages::GAGE == packName
      || Packages::HFB == packName
      || Packages::LAK == packName
      || Packages::LAKSP == packName
      || Packages::MNWI == packName
      || Packages::LGR == packName
      || Packages::LGR_1 == packName
      || Packages::LGR_2 == packName
      || Packages::SUB == packName
      || Packages::SUBLine15 == packName
      || Packages::SUBLine16 == packName
      || "FNC" == packName
      || Packages::RCH == packName
      || Packages::EVT == packName
      || Packages::ETS == packName
      || Packages::MNW2 == packName
      || Packages::MNW == packName
      || Packages::MNWSetup == packName
      || Packages::MNWStressPeriod == packName
      || Packages::STRSP == packName
      || Packages::SWI == packName
      || Packages::Swi::NUZONE == packName
      || Packages::Swi::NUSURF == packName
      || iLstPack(packName)
      || iSfrPack(packName)
      || iUzfPack(packName)
      || iArrayToNative(packName, a_global)
      || Packages::VDFLine5 == packName
      || Packages::VDFStressPeriod == packName
      || Packages::VSCLine3 == packName
      || Packages::VSCStressPeriod == packName
     )
  {
     rval = true;
  }

  if (   Packages::MNW2 == packName
      || Packages::MNW == packName
      || Packages::MNWSetup == packName
      || Packages::MNWStressPeriod == packName
      || Packages::STRSP == packName
      || iSfrPack(packName)
      || iUzfPack(packName)
      || iLstPack(packName)
     )  NativeUtil::ExportNextToH5();

  return rval;
} // iPackageToNativeExport
//------------------------------------------------------------------------------
/// \brief Uses the native exporter to write the files
//------------------------------------------------------------------------------
bool ExpGmsH5::impl::ExportNative (MfGlobal* a_global,
                                   MfPackage* a_package,
                                   TxtExporter* a_exp)
{
    bool rval=false;
    CStr packName(a_package->PackageName());
    if (iPackageToNativeExport(packName, a_global, a_package))
    {
      if (!m_nativeExp)
      {
        m_nativeExp = new Mf2kNative;
        m_nativeExp->SetFileName(m_fname.c_str());
        m_nativeExp->SetArraysInternal(true);
        m_nativeExp->ArealUseLastToh5(true);
      }
      if (m_nativeExp)
      {
        // TODO REMOVE
        if ("STP" == packName) m_nativeExp->StpFlag() = true;
        a_exp->AtLeastOneTransientSPExists() =
          m_nativeExp->GetExp()->AtLeastOneTransientSPExists();
        a_exp->SetOfSteadyStateStressPeriods() =
          m_nativeExp->GetExp()->SetOfSteadyStateStressPeriods();

        rval = m_nativeExp->ExportPackage(a_global, a_package);

        // TODO REMOVE
        if ("STP" == packName)
        {
          m_nativeExp->StpFlag() = false;
          rval = false;
        }
      }
    }
    return rval;
} // ExpGmsH5::impl::ExportNative
//------------------------------------------------------------------------------
/// \brief Exports the package data.
//------------------------------------------------------------------------------
bool ExpGmsH5::ExportPackage (MfGlobal* a_global,
                              MfPackage* a_package)
{
  // export package as native text
  bool rval(true);
  if (m_p->ExportNative(a_global, a_package, GetExp())) return rval;

  CStr packName(a_package->PackageName());
  ExportedPackages(GetExp()).insert(packName);
  if (Packages::NAM == packName)
  {
    expNameFile(a_package, GetExp(), false, GetModelType(), this);
  }
  else if (Packages::SEN1 == packName)
  {
    saveSEN(a_global->GetPackage(Packages::SEN),
            a_package,
            GetExp());
  }
  else if (Packages::BIN == packName)
  {
    iHasBinaryExport(GetExp()) = true;
  }
  else if ("STP" == packName) // this comes at the very end
  {
    ASSERT(_CrtCheckMemory());
    expParamFile(a_global->GetPackage("NAM1"), GetExp());
    expSEN(GetExp());
    expNameFile(0, GetExp(), true, GetModelType(), this);
    expSuperFile(a_global->ModelType(), a_global->GetPackage("NAM1"), GetExp());
    expFinalize(a_global, GetExp());
  }
  else
  {
    rval = false;
  }

  static int stressPeriod(0); // ok to leave
  if (a_global->GetCurrentPeriod() != stressPeriod)
  {
    stressPeriod = a_global->GetCurrentPeriod();
    printf("Writing data for Stress Period %d\n", stressPeriod);
  }

  bool testsRunning(0);
#ifdef CXX_TEST
  if (testCxx::TestsRunning())
  {
    testsRunning = true;
  }
#endif
  if (rval && !testsRunning)
  {
    printf("Writing data for package: %s\n", packName.c_str());
    if ("STP" == packName)
      showWarnings(GetExp());
    fflush(stdout);
  }
  return rval;
} // ExpGmsH5::ExportPackage


///////////////////////////////////////////////////////////////////////////////
// INTERNAL FUNCTIONS
///////////////////////////////////////////////////////////////////////////////

//------------------------------------------------------------------------------
/// \brief Creates a dataset in an h5 file
//------------------------------------------------------------------------------
static void CreateH5Dataset (const CStr &a_file,
                             const CStr &a_group,
                             const CStr &a_path,
                             const hid_t a_type,
                             const int a_dim,
                             const bool a_compress)
{
  int    iData(0);
  double dData(0);
  char   cData(0);
  CStr   path;

  if (a_group != "")
    path = a_group + "/" + a_path;
  else
    path = a_path;
  H5DataSetWriterSetup s1(a_file, path, a_type, a_dim, a_compress);
  std::map<CStr,std::vector<int> >& chunkMap = GetChunkMap();
  std::vector<int> chunkSize;
  if (chunkMap.find(a_group) != chunkMap.end())
    chunkSize = chunkMap[a_group];
  else if (chunkMap.find(path) != chunkMap.end())
    chunkSize = chunkMap[path];
  else
    chunkSize = chunkMap[a_path];
  s1.SetChunkSize(chunkSize);
  
  std::vector<hsize_t> start, n2write;
  start.assign(a_dim, 0);
  n2write.assign(a_dim,0);
  H5DSWriterDimInfo dim(start, n2write);
  H5DataSetWriter t1(&s1);
  t1.SetDimInfoForWriting(&dim);
  if (a_type == H5T_NATIVE_INT)
    t1.WriteData(&iData, 0);
  else if (a_type == H5T_NATIVE_CHAR)
    t1.WriteData(&cData, 0);
  else
    t1.WriteData(&dData, 0);
} // CreateH5Dataset
//------------------------------------------------------------------------------
/// \brief Writes a string dataset
//------------------------------------------------------------------------------
int xfpWriteDatasetString (hid_t a_Loc,
                           const char *a_Name,
                           const char *a_Str)
{
  hid_t   DsetId, SpaceId;
  hid_t   StringType;
  hsize_t Dims;
  herr_t  status;

  /* Create the string type */
  StringType = H5Tcopy(H5T_C_S1);
  if (StringType < 0) {
    return StringType;
  }
  H5Tset_strpad(StringType, H5T_STR_NULLTERM);

  /* Set the length of the string datatype */
  status = H5Tset_size(StringType, strlen(a_Str) + 1);

  /* Create the dataspace; */
  Dims = 1;
  SpaceId = H5Screate_simple(1, &Dims, &Dims);
    
  DsetId = H5Dcreate(a_Loc, a_Name, StringType, SpaceId, H5P_DEFAULT);
  if (DsetId < 0) {
    H5Tclose(StringType);
    H5Sclose(SpaceId);
    return DsetId;
  }

  status = H5Dwrite(DsetId, StringType, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                                                                      a_Str);

  /* close resources */
  H5Sclose(SpaceId);
  H5Dclose(DsetId);
  H5Tclose(StringType);

  return status;
} // xfpWriteDatasetString
//------------------------------------------------------------------------------
/// \brief writes an int attribute to a dataset
//------------------------------------------------------------------------------
int xfpWriteAttributeInt (hid_t a_Loc,
                          const char *a_Name,
                          int a_Number,
                          int *a_val)
{
  hid_t   AttId, SpaceId;
  hsize_t Dims;
  herr_t  status;

  /* Create the dataspace; */
  Dims = a_Number;
  SpaceId = H5Screate_simple(1, &Dims, &Dims);

  AttId = H5Aopen_name(a_Loc, a_Name);
  if (AttId < 0) {
    AttId = H5Acreate(a_Loc, a_Name, H5T_NATIVE_INT, SpaceId, H5P_DEFAULT);
    if (AttId < 0) {
      H5Sclose(SpaceId);
      return AttId;
    }
  }

  status = H5Awrite(AttId, H5T_NATIVE_INT, a_val);

  /* close resources */
  H5Sclose(SpaceId);
  H5Aclose(AttId);

  return status;
} // xfpWriteAttributeInt
//------------------------------------------------------------------------------
/// \brief writes a string attribute to a dataset
//------------------------------------------------------------------------------
int xfpWriteAttributeString (hid_t a_Loc,
                             const char * a_Name, 
                             const char * a_Str)
{
  hid_t   AttId, SpaceId;
  hid_t   StringType;
  hsize_t Dims;
  herr_t  status;
  int     length;

  // Create the string type
  StringType = H5Tcopy(H5T_C_S1);
  if (StringType < 0) {
    return StringType;
  }
  H5Tset_strpad(StringType, H5T_STR_NULLTERM);

  // Set the length of the string datatype
  length = (int)strlen(a_Str) + 1;
  status = H5Tset_size(StringType, length);

  // Create the dataspace
  Dims = 1;
  SpaceId = H5Screate_simple(1, &Dims, &Dims);
    
  AttId = H5Acreate1(a_Loc, a_Name, StringType, SpaceId, H5P_DEFAULT);
  if (AttId < 0) {
    H5Sclose(SpaceId);
    return AttId;
  }

  status = H5Awrite(AttId, StringType, a_Str);

  // close resources
  H5Sclose(SpaceId);
  H5Aclose(AttId);
  H5Tclose(StringType);

  return status;
} // xfpWriteAttributeString
//------------------------------------------------------------------------------
/// \brief writes a string attribute to a dataset
//------------------------------------------------------------------------------
int xfpWriteAttributeDouble (hid_t a_Loc,
                             const char *a_Name, 
                             int a_Number,
                             double *a_val)
{
  hid_t   AttId, SpaceId;
  hsize_t Dims;
  herr_t  status;

  // Create the dataspace
  Dims = a_Number;
  SpaceId = H5Screate_simple(1, &Dims, &Dims);

  AttId = H5Acreate1(a_Loc, a_Name, H5T_IEEE_F64LE, SpaceId, H5P_DEFAULT);
  if (AttId < 0) {
    H5Sclose(SpaceId);
    return AttId;
  }

  status = H5Awrite(AttId, H5T_NATIVE_DOUBLE, a_val);

  // close resources
  H5Sclose(SpaceId);
  H5Aclose(AttId);

  return status;
} // xfpWriteAttributeDouble
//------------------------------------------------------------------------------
/// \brief Goes through the bcs and writes the int attribute for the char
/// datasets i.e. Name, Map ID
//------------------------------------------------------------------------------
static void iWriteIntAttForDataSets (hid_t fid,
                                     CStr *bcGrp,
                                     int a_nBc,
                                     std::vector<CStr> &bcPaths,
                                     const char *attrName,
                                     int value)
{
  int i;
  hid_t dataId(-1);
  CStr path;
  for (i=0; i<a_nBc; i++)
  {
    std::vector<CStr>::iterator currPath = bcPaths.begin(); 
    for ( ; currPath != bcPaths.end(); ++currPath)
    {
      path.Format("%s/%s", bcGrp[i], *currPath);
      dataId = H5Dopen(fid, path);
      if (dataId > -1)
      {
        xfpWriteAttributeInt(dataId, attrName, 1, &value);
        H5Dclose(dataId);
        dataId = -1;
      }
    }
  }
} // iWriteIntAttForCharDataSets
//------------------------------------------------------------------------------
/// \brief For each H5 dataset (string) give chunks
//------------------------------------------------------------------------------
static std::map<CStr, std::vector<int> >& GetChunkMap ()
{
  static std::map<CStr, std::vector<int> > fg_chunkMap; // ok to leave
  if (fg_chunkMap.empty())
  {
    using std::vector;
    typedef std::vector<int> vec_int;
    vec_int v_1(1, 1);
    vec_int v_3(1, 3);
    vec_int v_5000(1, 5000);

    vec_int v_3_3(2, 3);
    vec_int v_3_5000;
    v_3_5000.push_back(3);
    v_3_5000.push_back(5000);
    vec_int v_5000_3;
    v_5000_3.push_back(5000);
    v_5000_3.push_back(3);
    
    vec_int v_3_5000_1;
    v_3_5000_1.push_back(3);
    v_3_5000_1.push_back(5000);
    v_3_5000_1.push_back(1);

    fg_chunkMap[FILE_VERSION] = v_1;
    fg_chunkMap[MFBC_VERSION] = v_1;
    fg_chunkMap["Arrays"] = v_5000;
    fg_chunkMap[MFBC_NUMBC] = v_1;
    fg_chunkMap[MFBC_USELAST] = v_3;
    fg_chunkMap["ET/01. Use Last"] = v_3_3;
    fg_chunkMap["ETS/01. Use Last"] = v_3_3;
    fg_chunkMap["Recharge/01. Use Last"] = v_3_3;
    fg_chunkMap["UZF/01. Use Last"] = v_3_3;
    fg_chunkMap[MFBC_CELLIDS] = v_5000;
    fg_chunkMap[MFBC_NAME] = v_5000;
    fg_chunkMap[MFBC_MAPIDSTR] = v_5000;
    fg_chunkMap[MFBC_FACTOR] = v_3;
    fg_chunkMap[MFBC_IFACE] = v_5000;
    fg_chunkMap[MFBC_DATA] = v_3_5000_1;
    fg_chunkMap[MFBC_DATAMULT] = v_3_3;
    fg_chunkMap[MFBC_LAY] = v_5000_3;
    fg_chunkMap[MFBC_LAYMULT] = v_3;
    // stream
    fg_chunkMap[MFBC_STRSEGID] = v_5000;
    fg_chunkMap[MFBC_SEGID] = v_5000;
    fg_chunkMap[MFBC_SEGFLW] = v_5000_3;
    fg_chunkMap[MFBC_ITRIB] = v_5000_3;
    fg_chunkMap[MFBC_UPID] = v_5000;
    fg_chunkMap[MFBC_NSEG] = v_1;
    fg_chunkMap[MFBC_SEGP] = v_3_5000_1;
    fg_chunkMap[MFBC_SEGFLWT] = v_3_5000;
    // ets
    fg_chunkMap[MFBC_PXDP] = v_3_5000_1;
    fg_chunkMap[MFBC_PXDPMULT] = v_3_3;
    fg_chunkMap[MFBC_PETM] = v_3_5000_1;
    fg_chunkMap[MFBC_PETMMULT] = v_3_3;
    fg_chunkMap[MFBC_NETSEG] = v_1;
    // mnw
    fg_chunkMap[MFBC_KSPREF] = v_1;
    fg_chunkMap[MFBC_LOSSTYPE] = v_1;
    fg_chunkMap[MFBC_IOWELL2] = v_3;
    // uzf
    fg_chunkMap[MFBC_IUZFBND] = v_5000;
    fg_chunkMap[MFBC_IUZFBNDMULT] = v_5000;
    fg_chunkMap[MFBC_IRUNBND] = v_5000;
    fg_chunkMap[MFBC_IRUNBNDMULT] = v_5000;
    fg_chunkMap[MFBC_VKS] = v_5000;
    fg_chunkMap[MFBC_VKSMULT] = v_3;
    fg_chunkMap[MFBC_EPS] = v_5000;
    fg_chunkMap[MFBC_EPSMULT] = v_3;
    fg_chunkMap[MFBC_THTS] = v_5000;
    fg_chunkMap[MFBC_THTSMULT] = v_3;
    fg_chunkMap[MFBC_THTI] = v_5000;
    fg_chunkMap[MFBC_THTIMULT] = v_3;
    // sub
    fg_chunkMap[MFBC_RNB] = v_5000;
    fg_chunkMap[MFBC_RNBMULT] = v_3;
    fg_chunkMap[MFBC_HC] = v_5000;
    fg_chunkMap[MFBC_HCMULT] = v_3;
    fg_chunkMap[MFBC_SFE] = v_5000;
    fg_chunkMap[MFBC_SFEMULT] = v_3;
    fg_chunkMap[MFBC_SFV] = v_5000;
    fg_chunkMap[MFBC_SFVMULT] = v_3;
    fg_chunkMap[MFBC_COM] = v_5000;
    fg_chunkMap[MFBC_COMMULT] = v_3;
    fg_chunkMap[MFBC_DSTART] = v_5000;
    fg_chunkMap[MFBC_DSTARTMULT] = v_3;
    fg_chunkMap[MFBC_DHC] = v_5000;
    fg_chunkMap[MFBC_DHCMULT] = v_3;
    fg_chunkMap[MFBC_DCOM] = v_5000;
    fg_chunkMap[MFBC_DCOMMULT] = v_3;
    fg_chunkMap[MFBC_DZ] = v_5000;
    fg_chunkMap[MFBC_DZMULT] = v_3;
    fg_chunkMap[MFBC_NZ] = v_5000;
    fg_chunkMap[MFBC_NZMULT] = v_3;
  }
  return fg_chunkMap;
} // GetChunkMap
//------------------------------------------------------------------------------
/// \brief Creates a default modflow h5 file
//------------------------------------------------------------------------------
static bool CreateDefaultMfH5File (const char *a_,
                                   int a_modelType/*=1*/,
                                   bool a_compress/*=false*/)
{
  CStr file(a_);
  file += ".h5";

  H5DataSetWriterSetup s(file);
  H5DataSetWriter t(&s);
  if (!t.CreateGroup("Arrays"))
  {
    return false;
  }

  int    i, j;
  const int GROUPS = 10;
  CStr bcGrp[GROUPS] = { "Drain", "General Head", "River", "Specified Head",
                         "Well", "Stream", "Stream (SFR2)", "Drain Return",
                         "Multi-Node Well", "MNW2" };

  const int SW_GROUPS = 2;
  CStr swBcGrp[SW_GROUPS] = { "VDF", "VSC" };

  const int NUM_BC_PATHS = 7;
  CStr bcPaths[NUM_BC_PATHS] = { MFBC_NUMBC, MFBC_USELAST, MFBC_CELLIDS,
                                 MFBC_NAME, MFBC_MAPIDSTR, MFBC_IFACE,
                                 MFBC_DATA };
  const int NUM_ST_PATHS = 6;
  CStr stPaths[NUM_ST_PATHS] = { MFBC_STRSEGID, MFBC_SEGID, MFBC_SEGFLW,
                                 MFBC_ITRIB, MFBC_UPID, MFBC_NSEG };
  const int NUM_SF_PATHS = 4;
  CStr sfPaths[NUM_SF_PATHS] = { MFBC_STRSEGID, MFBC_NSEG, MFBC_SEGP,
                                 MFBC_SEGFLWT };
  hid_t bcType[NUM_BC_PATHS] = { H5T_NATIVE_INT, H5T_NATIVE_INT, H5T_NATIVE_INT,
                                 H5T_NATIVE_CHAR, H5T_NATIVE_CHAR,
                                 H5T_NATIVE_INT, H5T_NATIVE_DOUBLE };
  hid_t stType[NUM_ST_PATHS] = { H5T_NATIVE_INT, H5T_NATIVE_INT,
                                 H5T_NATIVE_DOUBLE, H5T_NATIVE_INT,
                                 H5T_NATIVE_INT, H5T_NATIVE_INT };
  hid_t sfType[NUM_SF_PATHS] = { H5T_NATIVE_INT, H5T_NATIVE_INT,
                                 H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE};
  int   bcDim[NUM_BC_PATHS] = { 1, 1, 1, 1, 1, 1, 3 };
  int   stDim[NUM_ST_PATHS] = { 1, 1, 2, 2, 1, 1 };
  int   sfDim[NUM_SF_PATHS] = { 1, 1, 3, 2 };

  for (i=0; i<GROUPS; i++)
  {
    t.CreateGroup(bcGrp[i]);
    for (j=0; j<NUM_BC_PATHS; j++)
    {
      CreateH5Dataset(file, bcGrp[i], bcPaths[j], bcType[j], bcDim[j],
                      a_compress);
    }
  }

  // do the extra stuff on streams (STR)
  for (j=0; j<NUM_ST_PATHS; j++)
  {
    CreateH5Dataset(file, bcGrp[5], stPaths[j], stType[j], stDim[j],
                    a_compress);
  }

  // do the extra stuff for streams (SFR2)
  for (j=0; j<NUM_SF_PATHS; j++)
  {
    CreateH5Dataset(file, bcGrp[6], sfPaths[j], sfType[j], sfDim[j],
                    a_compress);
  }

  // create SEAWAT groups
  if (a_modelType == MfData::SEAWAT)
  {
    for (i=0; i<SW_GROUPS; i++)
    {
      t.CreateGroup(swBcGrp[i]);
      for (j=0; j<NUM_BC_PATHS; j++)
      {
        CreateH5Dataset(file, swBcGrp[i], bcPaths[j], bcType[j], bcDim[j],
                        a_compress);
      }
    }
  }

  CStr arGrp[3] = {"ET", "Recharge", "ETS"};
  CStr arPaths[5] = { MFBC_USELAST, MFBC_DATA, MFBC_DATAMULT, MFBC_LAY,
                      MFBC_LAYMULT };
  hid_t arType[5] = { H5T_NATIVE_INT, H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE,
                      H5T_NATIVE_INT, H5T_NATIVE_INT };
  int   arDim[5] = { 2, 3, 2, 2, 1 };

  for (i=0; i<3; i++)
  {
    t.CreateGroup(arGrp[i]);
    for (j=0; j<5; j++)
    {
      CreateH5Dataset(file, arGrp[i], arPaths[j], arType[j], arDim[j],
                      a_compress);
    }
  }
  
  int etStuffSize = 5;
  CStr etPaths[] = { MFBC_PXDP, MFBC_PXDPMULT, MFBC_PETM, MFBC_PETMMULT,
                     MFBC_NETSEG };
  hid_t etType[] = { H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE,
                     H5T_NATIVE_DOUBLE, H5T_NATIVE_INT};
  int   etDim[] = { 3, 2, 3, 2, 1 };

  // do the extra stuff for evapotranspiration (ETS)
  for (j=0; j<etStuffSize; j++)
  {
    CreateH5Dataset(file, arGrp[2], etPaths[j], etType[j], etDim[j],
                    a_compress);
  }

  // UZF
  const int NUM_UZ_PATHS = 15;
  CStr uzPaths[NUM_UZ_PATHS] = { MFBC_USELAST, MFBC_DATA, MFBC_DATAMULT,
                                 MFBC_IUZFBND, MFBC_IUZFBNDMULT,
                                 MFBC_IRUNBND, MFBC_IRUNBNDMULT,
                                 MFBC_VKS, MFBC_VKSMULT,
                                 MFBC_EPS, MFBC_EPSMULT,
                                 MFBC_THTS, MFBC_THTSMULT,
                                 MFBC_THTI, MFBC_THTIMULT };
  hid_t uzType[NUM_UZ_PATHS] = { H5T_NATIVE_INT, H5T_NATIVE_DOUBLE, 
                                 H5T_NATIVE_DOUBLE,
                                 H5T_NATIVE_INT, H5T_NATIVE_INT,
                                 H5T_NATIVE_INT, H5T_NATIVE_INT,
                                 H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE,
                                 H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE,
                                 H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE,
                                 H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE };
  int   uzDim[NUM_UZ_PATHS] = { 2, 3, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };

  t.CreateGroup("UZF");
  for (j=0; j<NUM_UZ_PATHS; j++)
  {
    CreateH5Dataset(file, "UZF", uzPaths[j], uzType[j], uzDim[j],
                    a_compress);
  }

  int mnwStuffSize = 3;
  CStr mnwPaths[] = { MFBC_KSPREF, MFBC_LOSSTYPE, MFBC_IOWELL2 };
  hid_t mnwType[] = { H5T_NATIVE_INT, H5T_NATIVE_DOUBLE, H5T_NATIVE_INT };
  int   mnwDim[]  = { 1, 1, 1 };

  // do the extra stuff for multi-node well (MNW)
  for (j=0; j<mnwStuffSize; ++j)
  {
    CreateH5Dataset(file, bcGrp[8], mnwPaths[j], mnwType[j], mnwDim[j],
                    a_compress);
  }

  // sub
  const int NUM_SUB_PATHS = 23;
  CStr subPaths[NUM_SUB_PATHS] = { MFBC_USELAST, MFBC_DATA, MFBC_DATAMULT,
                                   MFBC_RNB, MFBC_RNBMULT,
                                   MFBC_HC, MFBC_HCMULT,
                                   MFBC_SFE, MFBC_SFEMULT,
                                   MFBC_SFV, MFBC_SFVMULT,
                                   MFBC_COM, MFBC_COMMULT,
                                   MFBC_DSTART, MFBC_DSTARTMULT,
                                   MFBC_DHC, MFBC_DHCMULT,
                                   MFBC_DCOM, MFBC_DCOMMULT,
                                   MFBC_DZ, MFBC_DZMULT,
                                   MFBC_NZ, MFBC_NZMULT };
  hid_t subType[NUM_SUB_PATHS] = { H5T_NATIVE_INT, H5T_NATIVE_DOUBLE, 
                                   H5T_NATIVE_DOUBLE,
                                   H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE, 
                                   H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE,
                                   H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE,
                                   H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE,
                                   H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE,
                                   H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE,
                                   H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE,
                                   H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE,
                                   H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE,
                                   H5T_NATIVE_INT, H5T_NATIVE_INT };
  int   subDim[NUM_SUB_PATHS] = { 2, 3, 2, 
                                  2, 1, 2, 1, 2, 1, 2, 1, 2, 1,
                                  2, 1, 2, 1, 2, 1, 2, 1, 2, 1 };

  t.CreateGroup("SUB");
  for (j=0; j<NUM_SUB_PATHS; j++)
  {
    CreateH5Dataset(file, "SUB", subPaths[j], subType[j], subDim[j],
                    a_compress);
  }

  // bc version
  CreateH5Dataset(file, "", MFBC_VERSION, H5T_NATIVE_DOUBLE, 1, a_compress);
  {
    H5DataSetWriterSetup s1(file, MFBC_VERSION, H5T_NATIVE_DOUBLE, 1);
    H5DataSetWriter t1(&s1);
    double tmpd(3.0);
    t1.WriteData(&tmpd, 1);
  }

  // file version
  CreateH5Dataset(file, "", FILE_VERSION, H5T_NATIVE_DOUBLE, 1, a_compress);
  {
    H5DataSetWriterSetup s1(file, FILE_VERSION, H5T_NATIVE_DOUBLE, 1);
    H5DataSetWriter t1(&s1);
    double tmpd(1.0);
    t1.WriteData(&tmpd, 1);
  }

  std::vector<CStr> bcAttrPaths;
  bcAttrPaths.push_back(MFBC_NAME);
  bcAttrPaths.push_back(MFBC_MAPIDSTR);
  {
    hid_t fid(H5DataReader::GetFileId(file));
    xfpWriteDatasetString(fid, "File Type", "Xmdf");
    // write the int attribute for the mapid and name
    iWriteIntAttForDataSets(fid, bcGrp, GROUPS, bcAttrPaths,
                            MFBC_MAX_STR_LEN, 1);

    // write the int attribute for segment flow table
    bcAttrPaths.clear();
    bcAttrPaths.push_back(MFBC_SEGFLWT);
    iWriteIntAttForDataSets(fid, bcGrp+6, 1, bcAttrPaths, "NumRows", 0);
  }
  return true;
} // CreateDefaultMfH5File
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static bool& iNeedsSENFile (TxtExporter* a_exp)
{
  return a_exp->m_public->m_needsSENFile;
} // iNeedsSENFile
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool iIsSeawatType (const CStr& a_type)
{
  return a_type == "VDF" || a_type == "VSC";
} // iIsSeawatType
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool iIsMt3dType (const CStr& a_type)
{
  std::set<CStr> mt3dTypes;
  mt3dTypes.insert("BTN");
  mt3dTypes.insert("ADV");
  mt3dTypes.insert("DSP");
  mt3dTypes.insert("SSM");
  mt3dTypes.insert("RCT");
  mt3dTypes.insert("TOB");
  mt3dTypes.insert("GCG");
  return mt3dTypes.find(a_type) != mt3dTypes.end();
} // iIsMt3dType
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static bool iIsTypeFiltered (const CStr& a_type,
                             int a_namefileType,
                             int a_modelType,
                             TxtExporter* a_exp)
{
  if (a_type == "sen" && !iNeedsSENFile(a_exp))
    return true;
  else if (a_modelType == MfData::SEAWAT)
  {
    // for SEAWAT
    if (a_namefileType == NF_MODFLOW &&
        (iIsMt3dType(a_type) || iIsSeawatType(a_type)))
    {
      // only include MODFLOW packages in mfn
      return true;
    }
    else if (a_namefileType == NF_MT3D && !iIsMt3dType(a_type))
    {
      // only include MT3D packages in mts
      return true;
    }
  }
  return false;
} // iIsTypeFiltered
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static bool iIsTypeSupported (const CStr& a_type,
                              TxtExporter *a_exp,
                              int a_modelType)
{
  if (a_modelType != MfData::SEAWAT)
    return a_exp->IsTypeSupported(a_type);
  else
  {
    return a_exp->IsTypeSupported(a_type) || iIsSeawatType(a_type);
  }
} // iIsTypeSupported
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static CStr iGetTypeExtension (const CStr& a_type,
                               TxtExporter *a_exp)
{
  if (a_exp->IsTypeSupported(a_type))
    return a_exp->GetExtension(a_type);
  else
  {
    CStr lowerType(a_type);
    lowerType.ToLower();
    return lowerType;
  }
} // iGetTypeExtension
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool& iHasBinaryExport (TxtExporter* a_exp)
{
  return a_exp->m_public->m_HasBinaryExport;
} // iHasBinaryExport
//------------------------------------------------------------------------------
/// \brief Exports the name file
//------------------------------------------------------------------------------
static void expNameFile (MfPackage *a_package,
                         TxtExporter *a_exp,
                         int a_write,
                         int a_modelType,
                         ExpGmsH5* a_h5)
{
  using namespace Packages;
  std::vector<CStr>& ftype = a_exp->m_public->m_NAM_ftype;
  std::vector<CStr>& fname = a_exp->m_public->m_NAM_fname;
  std::vector<int>& niu = a_exp->m_public->m_NAM_niu;
  int& maxStrLen = a_exp->m_public->m_NAM_maxStrLen;

  if (a_write == 2)
  {
    // for testing
    ftype.clear();
    fname.clear();
    niu.clear();
    maxStrLen = 0;
  }
  else if (!a_write)
  {
    const char *t(0), *n(0);
    const int *iu;
    if (!a_package->GetField(NameFile::FNAME, &n) || !n ||
        !a_package->GetField(NameFile::FTYPE, &t) || !t ||
        !a_package->GetField(NameFile::NIU, &iu) || !iu)
       return;
    ftype.push_back(t);
    fname.push_back(n);
    niu.push_back(*iu);
    if (fname.back().GetLength() > maxStrLen)
      maxStrLen = fname.back().GetLength();
  }
  else
  {
    int lastType;
    if (a_modelType == MfData::SEAWAT)
      lastType = NF_SEAWAT;
    else
      lastType = NF_MODFLOW;

    for (int nfType = NF_MODFLOW; nfType <= lastType; ++nfType)
    {
      std::set<CStr> uniqueNames;
      CStr file, type, baseName(a_exp->GetBaseFileName());
      util::StripPathFromFilename(baseName, baseName);
      std::ostringstream oStream;
      bool printedHeading(false);
      for (size_t i=0; i<ftype.size(); i++)
      {
        type = ftype.at(i);
        type.ToLower();
        if (!iIsTypeFiltered(ftype.at(i), nfType, a_modelType, a_exp))
        {
          if (type.find("data") != -1 && nfType != NF_MT3D)
          {
            CStr extension(fname.at(i));
            util::StripAllButExtension(extension, extension);
            a_h5->BuildUniqueName(baseName, extension, niu.at(i),
                                  uniqueNames, file);
          }
          else
          {
            if (iIsTypeSupported(ftype.at(i), a_exp, a_modelType))
            {
              a_h5->BuildUniqueName(baseName,
                                    iGetTypeExtension(ftype.at(i), a_exp),
                                    niu.at(i),
                                    uniqueNames,
                                    file);
            }
            else if (!iIsTypeFiltered(ftype.at(i), nfType, a_modelType, a_exp))
            {
              a_h5->BuildUniqueName(baseName, type, niu.at(i),
                                    uniqueNames, file);

              // copy unsupported file
              CStr sourceFile(fname.at(i));
              CStr destPath;
              util::StripFileFromFilename(a_exp->GetBaseFileName(), destPath);
              CStr destFile(destPath + file);
              util::FileCopy(sourceFile, destFile);
            }
          }

          if (nfType == NF_MODFLOW || nfType == NF_SEAWAT)
          {
            CStr str;
            str.Format("%s %d %s", ftype.at(i), niu.at(i), file);
            if (nfType == NF_MODFLOW)
              a_exp->WriteLineToFile(NAM, str);
            else
              a_exp->WriteLineToFile(SWN, str);
            if (a_modelType != MfData::SEAWAT || nfType == NF_SEAWAT)
            {
              oStream << "FILE UNIT: " << niu.at(i);
              if (niu.at(i) < 100)
                oStream << " ";
              if (niu.at(i) < 10)
                oStream << " ";
              oStream << "  FILE NAME: " << fname.at(i);
              int diff(maxStrLen - fname.at(i).GetLength());
              for (int q=0; q<diff; q++)
                oStream << " ";
              oStream << "  FILE TYPE: " << ftype.at(i) << "\n";
            }
          }
          else
          {
            if (!printedHeading)
            {
              a_exp->WriteLineToFile(MTS, "MT3DSUP");
              printedHeading = true;
            }
            CStr str;
            str.Format("%s \"%s\"", ftype.at(i), file);
            a_exp->WriteLineToFile(MTS, str);
          }
        }
      }
  #ifdef CXX_TEST
      if (!testCxx::TestsRunning())
  #endif
        printf("%s", oStream.str().c_str());
    }
  }
} // expNameFile
//------------------------------------------------------------------------------
/// \brief Writes out the super file so we can read in the parameter file
//------------------------------------------------------------------------------
static void expSuperFile (int a_model_type,
                          MfPackage* a_package,
                          TxtExporter *a_exp)
{
  using namespace MfData::Packages;

  if (!a_exp)
    return;

  const char *fname(NULL);
  if (a_package->GetField(NameFile::FNAME, &fname) && fname)
  {
    CStr sourceFile;
    util::StripExtensionFromFilename(fname, sourceFile);
    sourceFile += ".mfs";
    FILE *fp(fopen(sourceFile, "r"));
    if (fp)
    {
      fclose(fp);
      CStr destFile(a_exp->GetBaseFileName());
      destFile += ".mfs";
      util::FileCopy(sourceFile, destFile);
      return;
    }
  }


  CStr line, base(a_exp->GetBaseFileName());
  //util::StripExtensionFromFilename(base, base);
  util::StripPathFromFilename(base, base);

  switch (a_model_type)
  {
  case 0: // MODFLOW-2000
    a_exp->WriteLineToFile("mfs", "MF2KSUP");
    break;
  case 1: // MODFLOW-2005
    a_exp->WriteLineToFile("mfs", "MF2K5SUP");
    break;
  case 2: // MODFLOW-NWT
    a_exp->WriteLineToFile("mfs", "MFNWTSUP");
    break;
  case 3: // SEAWAT
    a_exp->WriteLineToFile("mfs", "MF2KSUP");
    break;
  case 4: // MODFLOW-LGR
    a_exp->WriteLineToFile("mfs", "MFLGRSUP");
    break;
  case 5: // MODFLOW-USG
    a_exp->WriteLineToFile("mfs", "MFUSGSUP");
    break;
  default:
    ASSERT(0);
    break;
  }

  if (iWasParamFileExported(a_exp))
  {
    line.Format("MPARAM \"%s.param\"", base);
    a_exp->WriteLineToFile("mfs", line);
  }
  line.Format("NAME 99 \"%s.mfn\"", base);
  a_exp->WriteLineToFile("mfs", line);
} // expSuperFile
//------------------------------------------------------------------------------
/// \brief Writes out the parameter file
//------------------------------------------------------------------------------
static void expParamFile (MfPackage* a_package,
                          TxtExporter *a_exp)
{
  using namespace MfData::Packages;
  const char *fname(NULL);

  iWasParamFileExported(a_exp) = false;
  if (a_package->GetField(NameFile::FNAME, &fname) && fname)
  {
    CStr sourceFile;
    util::StripExtensionFromFilename(fname, sourceFile);
    sourceFile += ".param";
    FILE* sourceParamFile = fopen(sourceFile.c_str(), "r");
    if (sourceParamFile != NULL)
    {
      const size_t maxLineSize = 256;
      char line[maxLineSize];
      while (fgets(line, maxLineSize, sourceParamFile))
      {
        a_exp->WriteStringToFile("param", line);
      }
      iWasParamFileExported(a_exp) = true;
    }
  }
  
  if (!iWasParamFileExported(a_exp))
  {

    Param p;
    ParamList *list(0);
    Parameters::GetParameterList(&list);

    // write out parameters for the DRN, DRT, GHB, RIV, CHD, WEL (Q)
    CStr parTypes(PARAM_FILE_TYPES);

    // We decided that we didn't want to duplicate code in mfLib for reading
    // and interpreting array based parameters. So we decided that we would
    // not write them to the param file and we would just have the code in
    // GMS to read the array based parameters and figure out if they are
    // using clusters or if they can be represented with key values. Once you
    // write out the files from GMS then all of the parameters will be written
    // to the param file.

    for (size_t i=0; i<list->Size(); i++)
    {
      CStr line, type;
      list->At(i, &p);
      type = p.m_type;
      line.ToUpper();
      if (parTypes.find(type) != std::string::npos)
      {
        a_exp->WriteLineToFile("param", "BEGPAR");

        line.Format("NAME \"%s\"", p.m_name);
        a_exp->WriteLineToFile("param", line);

        if (type == "Q")
          type = "WELL";
        line.Format("TYPE %s", type);
        a_exp->WriteLineToFile("param", line);

        line.Format("KEY %s", STR(p.m_key));
        a_exp->WriteLineToFile("param", line);

        line.Format("VALUE %s %s %s",
                    STR(p.m_value),
                    STR(p.m_min == 0 ? 1e-10 : p.m_min),
                    STR(p.m_max == 0 ? p.m_start*100 : p.m_max));
        a_exp->WriteLineToFile("param", line);

        line.Format("SOLVE %d", p.m_isens ? 1 : 0);
        a_exp->WriteLineToFile("param", line);

        if (p.m_logTrans)
        {
          line = "LOGXFORM";
          a_exp->WriteLineToFile("param", line);
        }

        line.Format("BSCAL %s", STR(p.m_bscal));
        a_exp->WriteLineToFile("param", line);

        a_exp->WriteLineToFile("param", "ENDPAR");
        iWasParamFileExported(a_exp) = true;
      }
    }
  }
} // expParamFile
//------------------------------------------------------------------------------
/// \brief Writes the array multiplier for the areal data
//------------------------------------------------------------------------------
template <class T>
static void expArealArrayMultiplier (const char *a_file,
                                     const char *a_path,
                                     const int a_spIdx,
                                     const hid_t a_datatype,
                                     const int a_idx,
                                     const int a_nDim,
                                     const T a_val)
{
  CStr path(a_path);
  path += " Multiplier";
  path.Replace("07.", "08.");
  path.Replace("09.", "10.");

  path.Replace("12.", "13.");
  path.Replace("14.", "15.");
  path.Replace("16.", "17.");
  path.Replace("18.", "19.");
  path.Replace("20.", "21.");
  path.Replace("22.", "23.");
  path.Replace("24.", "25.");
  path.Replace("26.", "27.");
  path.Replace("28.", "29.");
  path.Replace("30.", "31.");

  H5DataSetWriterSetup s(a_file, path, a_datatype, a_nDim);
  H5DataSetWriter h(&s);
  h.AllowTypeConversions(true);
  std::vector<hsize_t> start(a_nDim,0), n2write(a_nDim,1);
  if (a_nDim == 1)
    start[0] = a_spIdx;
  else if (a_nDim == 2)
  {
    start[0] = a_idx;
    start[1] = a_spIdx;
  }
  H5DSWriterDimInfo dim(start, n2write);
  h.SetDimInfoForWriting(&dim);
  h.WriteData(&a_val, 1);
} // expArealArrayMultiplier
//------------------------------------------------------------------------------
/// \brief Exports the use last information for the areal packages
//------------------------------------------------------------------------------
static void expUseLastAreal (const char * a_file,
                             const char * a_path,
                             int a_sp,
                             std::vector<int> &a_data)
{
  CStr f(a_file), path(a_path);
  f += ".h5";
  path += MFBC_USELAST;
  H5DataSetWriterSetup setup(f, path, H5T_NATIVE_INT, 2);
  H5DataSetWriter h(&setup);
  h.AllowTypeConversions(true);
  // do multidim stuff if needed
  std::vector<hsize_t> start(2,0), n2write(2,1);
  start[1] = a_sp - 1;
  n2write[0] = a_data.size();
  H5DSWriterDimInfo dim(start, n2write);
  h.SetDimInfoForWriting(&dim);
  h.WriteData(&a_data[0], a_data.size());
} // expUseLastAreal
//------------------------------------------------------------------------------
/// \brief Saves sensitivity process input file values until the type is known
//         so they can be exported
//------------------------------------------------------------------------------
static void saveSEN (MfPackage *a_pSen,
                     MfPackage *a_pSen1,
                     TxtExporter* a_exp)
{
  using namespace MfData::Packages;
  const int *isenall, *iuhead, *iprints, *isensu, *isenpu, *isenfm,
            *nplist, *isens, *ln;
  const char *parnam;
  const Real *b, *bl, *bu, *bscal;
  ParamList *list;
  Parameters::GetParameterList(&list);

  if (a_pSen->GetField(SENpack::ISENALL, &isenall) && isenall &&
      a_pSen->GetField(SENpack::IUHEAD, &iuhead) && iuhead &&
      a_pSen->GetField(SENpack::IPRINTS, &iprints) && iprints &&
      a_pSen->GetField(SENpack::ISENSU, &isensu) && isensu &&
      a_pSen->GetField(SENpack::ISENPU, &isenpu) && isenpu &&
      a_pSen->GetField(SENpack::ISENFM, &isenfm) && isenfm &&
      a_pSen1->GetField(SEN1pack::NPLIST, &nplist) && nplist &&
      a_pSen1->GetField(SEN1pack::PARNAM, &parnam) && parnam &&
      a_pSen1->GetField(SEN1pack::ISENS, &isens) && isens &&
      a_pSen1->GetField(SEN1pack::LN, &ln) && ln &&
      a_pSen1->GetField(SEN1pack::B, &b) && b &&
      a_pSen1->GetField(SEN1pack::BL, &bl) && bl &&
      a_pSen1->GetField(SEN1pack::BU, &bu) && bu &&
      a_pSen1->GetField(SEN1pack::BSCAL, &bscal) && bscal)
  {
    SensitivityHeader& sh = a_exp->m_public->m_SensitivityHeader;
    sh.isenall = *isenall;
    sh.iuhead  = *iuhead;
    sh.iprints = *iprints;
    sh.isensu  = *isensu;
    sh.isenpu  = *isenpu;
    sh.isenfm  = *isenfm;
    sh.nplist  = *nplist;

    vector<SensitivityItem>& si = a_exp->m_public->m_SensitivityItems;
    for (int i = 0; i < *nplist; ++i)
    {
      SensitivityItem senItem;
      CStr parnamStr(parnam + i*10, 10);
      senItem.name = parnamStr;
      senItem.isens = isens[i];
      senItem.ln    = ln[i];
      senItem.b     = b[i];
      senItem.bl    = bl[i];
      senItem.bu    = bu[i];
      senItem.bscal = bscal[i];
      si.push_back(senItem);

      Param p;
      if (list->FindByName(parnamStr.c_str(), &p))
      {
        p.m_bscal = bscal[i];
        list->UpdateParameter(&p);
      }
    }
  }
} // saveSEN
//------------------------------------------------------------------------------
/// \brief Exports sensitivity process input file
//------------------------------------------------------------------------------
static void expSEN (TxtExporter *a_exp)
{
  using namespace MfData::Packages;
  vector<SensitivityItem>& si = a_exp->m_public->m_SensitivityItems;
  SensitivityHeader& sh = a_exp->m_public->m_SensitivityHeader;

  ParamList *list;
  Parameters::GetParameterList(&list);
  Param p;
  CStr paramFileTypes(PARAM_FILE_TYPES);
  int nonKeyedCount = 0;
  for (int i = 0; i < sh.nplist; ++i)
  {
    SensitivityItem& s = si[i];
    if (list->FindByName(s.name, &p))
    {
      CStr type(p.m_type);
      type.ToUpper();
      if (paramFileTypes.find(p.m_type) == std::string::npos)
        nonKeyedCount++;
    }
  }

  iNeedsSENFile(a_exp) = nonKeyedCount != 0;
  if (!si.empty() && nonKeyedCount)
  {
    if (sh.nplist > 0)
    {
      CStr aCStr;
      aCStr.Format("%d %d %d %d \n"
                   "%d %d %d %d ",
                   nonKeyedCount, sh.isenall, sh.iuhead, sh.nplist,
                   sh.iprints, sh.isensu, sh.isenpu, sh.isenfm);
      a_exp->WriteLineToFile(SEN, aCStr);
      for (int i = 0; i < sh.nplist; ++i)
      {
        SensitivityItem& s = si[i];
        if (list->FindByName(s.name, &p) &&
            paramFileTypes.find(p.m_type) == std::string::npos)
        {
          CStr parnamStr(s.name);
          CStr parnamStrCopy(parnamStr);
          parnamStrCopy.Trim();
          aCStr.Format("%s %d %d %s %s %s %s ", s.name, s.isens, s.ln,
                       STR(s.b), STR(s.bl), STR(s.bu), STR(s.bscal));
          a_exp->WriteLineToFile(SEN, aCStr);
        }
      }
    }
  }
} // expSEN
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void iGetSeawatBcIdx (const CStr& a_type,
                      int& a_seawatBcIdx0,
                      int& a_seawatBcIdx1)
{
  a_seawatBcIdx0 = a_seawatBcIdx1 = -1;
  if (a_type == "River")
  {
    // stage, cond, elev, factor, RBDTHK, RIVDEN
    a_seawatBcIdx0 = 4;
    a_seawatBcIdx1 = 5;
  }
  else if (a_type == "Specified Head")
  {
    // startHead, endHead, factor1, factor2, CHDDENSOPT, CHDDEN
    a_seawatBcIdx0 = 4;
    a_seawatBcIdx1 = 5;
  }
  else if (a_type == "Drain")
  {
    // elev, cond, factor, DRNBELEV
    a_seawatBcIdx0 = 3;
  }
  else if (a_type == "General Head")
  {
    // head, cond, factor, GHBELEV, GHBDENS
    a_seawatBcIdx0 = 3;
    a_seawatBcIdx1 = 4;
  }
  else if (a_type == "Drain Return")
  {
    // elev, cond, layR, rowR, colR, Rfprop, factor
  }
  else if (a_type == "Well")
  {
    // Q, factor, WELDENS
    a_seawatBcIdx0 = 2;
  }
  else if (a_type == "Stream")
  {
    // stage, cond, bot. elev., top elev., width, slope, rough, factor
  }
  else if (a_type == "Stream (SFR2)")
  {
    // RCHLEN
  }
} // iGetSeawatBcIdx
//------------------------------------------------------------------------------
/// \brief This function does any final things that need to be done before
/// shutting down the program.
//------------------------------------------------------------------------------
static void expFinalize (MfGlobal* a_global,
                         TxtExporter *a_exp)
{
  expCheckArealFromUseLast(a_global->NumRow()*a_global->NumCol(),
                           a_exp->GetBaseFileName());
  H5DataReader::CloseAllH5FilesOpenForWriting();
} // expFinalize
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void showWarnings (TxtExporter* a_exp)
{
  if (iHasBinaryExport(a_exp))
  {
    printf("\nGMS Binary Array Warning:: This model conatins binary arrays. "
      "The binary format could possibly differ from GMS MODFLOW. The array "
      "values need to be manually verified in GMS.\n\n");
  }
} // showWarnings
//------------------------------------------------------------------------------
/// \brief This function updates the areal BC data from the use last flags
//------------------------------------------------------------------------------
static void expGetAllArealUseLast (CAR_INT2D& a_flags,
                                   const char *a_path,
                                   const char *a_baseName)
{
  CStr f(a_baseName), path(a_path);
  f += ".h5";
  path += MFBC_USELAST;
  std::vector<hsize_t> dims;

  {
    VEC_INT_PAIR indices;
    H5DataSetReader r(f, path, indices);
    r.GetDataSetDimensions(dims);
    //printf("crash");
    //printf("dims size %d", dims.size());
    //if (!dims.empty())
      a_flags.SetSize(static_cast<int>(dims[0]), static_cast<int>(dims[1]), 0);
    //else
    //  return;
  }
  {
    std::pair<int, int> p(0,0);
    VEC_INT_PAIR indices(2, p);
    indices[0].second = a_flags.GetSize1();
    indices[1].second = a_flags.GetSize2();
    H5DataSetReader r(f, path, indices);
    r.GetData(&a_flags.at(0,0), a_flags.GetSize1()*a_flags.GetSize2());
  }
} // expGetAllArealUseLast
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void WriteDataSetWithZeros (CStr& f, CStr& path, hsize_t dim[3],
                                   hsize_t start[3])
{
  // we don't want num to be bigger than ~75MB when num is about 10 million
  int nSp = 10000000 / (int)(dim[0] * dim[1]);
  int dim2 = nSp;
  if (dim[2] < nSp) dim2 = (int)dim[2];
  CAR_DBL3D dat;
  dat.SetSize(static_cast<int>(dim[0]),
              static_cast<int>(dim[1]),
              dim2, 0);

  for (int start2 = 0; start2 < (int)dim[2]; start2 += nSp)
  {
    H5DataSetWriterSetup s(f, path, H5T_NATIVE_DOUBLE, 3);
    std::vector<hsize_t> start1(&start[0], &start[3]),
                          n2write(&dim[0], &dim[3]);
    start1[2] += start2;
    if (start2 + dim2 > (int)dim[2]) dim2 = (int)dim[2] - start2;
    n2write[2] = (hsize_t)dim2;
    H5DSWriterDimInfo dim1(start1, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim1);
    size_t num1(static_cast<size_t>(dim[0] * dim[1] * dim2));
    w.WriteData(&dat.at(0,0,0), num1);
  }
} // WriteDataSetWithZeros
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void expArealPropFromUseLast (CAR_INT2D& a_flags,
                                     int a_nCells,
                                     const char *a_path,
                                     const char *a_baseName)
{
  CStr f(a_baseName), path(a_path);
  f += ".h5";
  path += MFBC_DATA;
  std::vector<hsize_t> dims;
  // get the current dimensions
  {
    VEC_INT_PAIR indices;
    H5DataSetReader r(f, path, indices);
    r.GetDataSetDimensions(dims);
    if (dims.size() < 3)
    {
      ASSERT(0);
      return;
    }
  }

  hsize_t start[3] = {0,0,0};
  hsize_t dim[3] = {a_flags.GetSize1() - 1, a_nCells, a_flags.GetSize2()};
  if (dims[0] != 1 ||
      dims[1] != 1 ||
      dims[2] != 1)
  {
    if (CStr(a_path) == "ETS/")
      dim[0] = 3;
    dim[2] = a_flags.GetSize2() - dims[2];
    start[2] = dims[2];
  }

  if (dim[0] < 1 ||
      dim[1] < 1 ||
      dim[2] < 1)
    return;

  // write the data set
  {
    WriteDataSetWithZeros(f, path, dim, start);
  }
  {
    CAR_DBL2D mult;
    mult.SetSize(static_cast<int>(dim[0]),
                 static_cast<int>(dim[2]), 1);
    path = a_path;
    path += MFBC_DATAMULT;
    H5DataSetWriterSetup s(f, path, H5T_NATIVE_DOUBLE, 2);
    std::vector<hsize_t> start1(2, 0),
                         n2write(2, 0);
    start1[1] = start[2];
    n2write[0] = dim[0];
    n2write[1] = dim[2];
    H5DSWriterDimInfo dim1(start1, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim1);
    w.WriteData(&mult.at(0,0), static_cast<size_t>(n2write[0]*n2write[1]));
  }
} // expArealPropFromUseLast
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void expEtSegFromUseLast (CAR_INT2D& a_flags,
                                 int a_nCells,
                                 const char *a_baseName,
                                 const char *a_dataPath,
                                 const char *a_multPath)
{
  if (a_flags.GetSize1() <= 1)
    return;

  CStr f(a_baseName), path("ETS/");
  f += ".h5";
  path += a_dataPath;
  std::vector<hsize_t> dims;
  // get the current dimensions
  {
    VEC_INT_PAIR indices;
    H5DataSetReader r(f, path, indices);
    r.GetDataSetDimensions(dims);
    if (dims.size() < 3)
    {
      ASSERT(0);
      return;
    }
  }

  hsize_t start[3] = {0,0,0};
  hsize_t dim[3] = {dims[0], a_nCells, a_flags.GetSize2()};
  if (dims[0] != 1 ||
      dims[1] != 1 ||
      dims[2] != 1)
  {
    dim[2] = a_flags.GetSize2() - dims[2];
    start[2] = dims[2];
  }

  if (dim[0] < 1 ||
      dim[1] < 1 ||
      dim[2] < 1)
    return;

  // write the data set
  {
    WriteDataSetWithZeros(f, path, dim, start);
  }
  {
    CAR_DBL2D mult;
    mult.SetSize(static_cast<int>(dim[0]),
                 static_cast<int>(dim[2]), 1);
    path = "ETS/";
    path += a_multPath;
    H5DataSetWriterSetup s(f, path, H5T_NATIVE_DOUBLE, 2);
    std::vector<hsize_t> start1(2, 0),
                         n2write(2, 0);
    start1[1] = start[2];
    n2write[0] = dim[0];
    n2write[1] = dim[2];
    H5DSWriterDimInfo dim1(start1, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim1);
    w.WriteData(&mult.at(0,0), static_cast<size_t>(n2write[0]*n2write[1]));
  }
} // expEtSegFromUseLast
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void expArealLayFromUseLast (CAR_INT2D& a_flags,
                                    int a_nCells,
                                    const char *a_path,
                                    const char *a_baseName)
{
  if (a_flags.GetSize1() <= 1)
    return;

  CStr f(a_baseName), path(a_path);
  f += ".h5";
  path += MFBC_LAY;
  std::vector<hsize_t> dims;
  // get the current dimensions
  {
    VEC_INT_PAIR indices;
    H5DataSetReader r(f, path, indices);
    r.GetDataSetDimensions(dims);
    if (dims.size() < 2)
    {
      ASSERT(0);
      return;
    }
  }

  hsize_t start[2] = {0,0};
  hsize_t dim[2] = {a_nCells, a_flags.GetSize2()};
  if (dims[0] != 1 ||
      dims[1] != 1)
  {
    dim[1] = a_flags.GetSize2() - dims[1];
    start[1] = dims[1];
  }

  CAR_INT2D dat;
  dat.SetSize(static_cast<int>(dim[0]),
              static_cast<int>(dim[1]), 1);
  // write the data set
  if (dim[0] > 0 && dim[1] > 0)
  {
    H5DataSetWriterSetup s(f, path, H5T_NATIVE_INT, 2);
    std::vector<hsize_t> start1(&start[0], &start[2]),
                         n2write(&dim[0], &dim[2]);
    H5DSWriterDimInfo dim1(start1, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim1);
    size_t num(static_cast<size_t>(dim[0] * dim[1]));
    w.WriteData(&dat.at(0,0), num);
  }
  if (dim[0] > 0 && dim[1] > 0)
  {
    std::vector<int> mult(static_cast<size_t>(dim[1]), 1);
    path = a_path;
    path += MFBC_LAYMULT;
    H5DataSetWriterSetup s(f, path, H5T_NATIVE_INT, 1);
    std::vector<hsize_t> start1(1, start[1]),
                         n2write(1, dim[1]);
    H5DSWriterDimInfo dim1(start1, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim1);
    w.WriteData(&mult.at(0), static_cast<size_t>(dim[1]));
  }
} // expArealLayFromUseLast
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void expCheckArealFromUseLast (int a_nCells,
                                      const char * a_baseName)
{
  // read all of the use last flags on the data
  CAR_INT2D flags;
  expGetAllArealUseLast(flags, "Recharge/", a_baseName);
  // update the areal data
  expArealPropFromUseLast(flags, a_nCells, "Recharge/", a_baseName);
  // update the areal layer
  expArealLayFromUseLast(flags, a_nCells, "Recharge/", a_baseName);

  expGetAllArealUseLast(flags, "ET/", a_baseName);
  // update the areal data
  expArealPropFromUseLast(flags, a_nCells, "ET/", a_baseName);
  // update the areal layer
  expArealLayFromUseLast(flags, a_nCells, "ET/", a_baseName);

  expGetAllArealUseLast(flags, "ETS/", a_baseName);
  // update the areal data
  expArealPropFromUseLast(flags, a_nCells, "ETS/", a_baseName);
  // update the areal layer
  expArealLayFromUseLast(flags, a_nCells, "ETS/", a_baseName);
  // update the extinction depth proportion
  expEtSegFromUseLast(flags, a_nCells, a_baseName, MFBC_PXDP, MFBC_PXDPMULT);
  // update the extinction rate proportion
  expEtSegFromUseLast(flags, a_nCells, a_baseName, MFBC_PETM, MFBC_PETMMULT);

  expGetAllArealUseLast(flags, "UZF/", a_baseName);
  // update the areal data
  expArealPropFromUseLast(flags, a_nCells, "UZF/", a_baseName);
} // expCheckArealFromUseLast

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\MfData\MfExport\private\ExpGmsH5.t.h>
#include <private/MfLibAsserts.h>

#define TESTBASE "4814dfa0-51de-11dd-ae16-0800200c9a66"

#define _TS_ASSERT_H5_INT(f, l, file, path, expected)                          \
  Check1DH5IntValue(f, l, file, path, expected)
#define TS_ASSERT_H5_INT(file, path, expected)                                 \
  _TS_ASSERT_H5_INT(__FILE__, __LINE__, file, path, expected)

#define _TS_ASSERT_H5_DBL(f, l, file, path, expected)                          \
  Check1DH5DoubleValue(f, l, file, path, expected)
#define TS_ASSERT_H5_DBL(file, path, expected)                                 \
  _TS_ASSERT_H5_DBL(__FILE__, __LINE__, file, path, expected)

#define _TS_ASSERT_H5_ARRAY(f, l, file, path, expected, length)                \
  CheckH5ArrayValue(f, l, file, path, expected, length)
#define TS_ASSERT_H5_ARRAY(file, path, expected, length)                       \
  _TS_ASSERT_H5_ARRAY(__FILE__, __LINE__, file, path, expected, length)

#define _TS_ASSERT_H5_INT_ATT(f, l, file, path, att, expected)                 \
  CheckH5IntAtt(f, l, file, path, att, expected)
#define TS_ASSERT_H5_INT_ATT(file, path, att, expected)                        \
  _TS_ASSERT_H5_INT_ATT(__FILE__, __LINE__, file, path, att, expected)

#define EQ_TOL(A, B, tolerance) (fabs((A) - (B)) <= (tolerance))

//------------------------------------------------------------------------------
template <class T>
static void CheckH5ArrayValue (const char *a_file,
                               int a_line,
                               const CStr& a_filePath,
                               const CStr& a_h5path,
                               const T* a_expected,
                               size_t a_expectedLength)
{
  std::vector<T> actual;
  std::vector<T> expected(a_expected, a_expected+a_expectedLength);
  H5DataSetReader n(a_filePath, a_h5path);
  n.AllowTypeConversions(true); // allow conversion from double to float
  n.GetAllData(actual);
  _TS_ASSERT_EQUALS_VEC(a_file, a_line, expected, actual);
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testCreateClass ()
{
  ExpGmsH5 *p = new ExpGmsH5;
  TS_ASSERT(p);
  if (p)
  {
    delete(p);
    p = NULL;
  }
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testSupportedPackage ()
{
  using namespace MfData::Packages;
  ExpGmsH5 e;
  TxtExporter* t(e.GetExp());
  TS_ASSERT(t->IsTypeSupported(BAS6));
  TS_ASSERT(t->IsTypeSupported(BCF));
  TS_ASSERT(t->IsTypeSupported(BCF6));
  TS_ASSERT(t->IsTypeSupported(CHD));
  TS_ASSERT(t->IsTypeSupported(CLN));
  TS_ASSERT(t->IsTypeSupported(DE4));
  TS_ASSERT(t->IsTypeSupported(DIS));
  TS_ASSERT(t->IsTypeSupported(DRN));
  TS_ASSERT(t->IsTypeSupported(DRT));
  TS_ASSERT(t->IsTypeSupported(EVT));
  TS_ASSERT(t->IsTypeSupported(ETS));
  TS_ASSERT(t->IsTypeSupported(GAGE));
  TS_ASSERT(t->IsTypeSupported(GHB));
  TS_ASSERT(t->IsTypeSupported(GLOBAL));
  TS_ASSERT(t->IsTypeSupported(GMG));
  TS_ASSERT(t->IsTypeSupported(GNC));
  TS_ASSERT(t->IsTypeSupported(HFB));
  TS_ASSERT(t->IsTypeSupported(HUF2));
  TS_ASSERT(t->IsTypeSupported(HUF));
  TS_ASSERT(t->IsTypeSupported(LAK));
  TS_ASSERT(t->IsTypeSupported(LIST));
  TS_ASSERT(t->IsTypeSupported(LMG));
  //TS_ASSERT(t->IsTypeSupported(LMT6));
  TS_ASSERT(t->IsTypeSupported(LPF));
  TS_ASSERT(t->IsTypeSupported(MLT));
  TS_ASSERT(t->IsTypeSupported(NWT));
  TS_ASSERT(t->IsTypeSupported(NAM));
  TS_ASSERT(t->IsTypeSupported(OC));
  TS_ASSERT(t->IsTypeSupported(PCG));
  TS_ASSERT(t->IsTypeSupported(PES));
  TS_ASSERT(t->IsTypeSupported(RCH));
  TS_ASSERT(t->IsTypeSupported(RIV));
  TS_ASSERT(t->IsTypeSupported(SIP));
  TS_ASSERT(t->IsTypeSupported(SEN));
  TS_ASSERT(t->IsTypeSupported(SFR));
  TS_ASSERT(t->IsTypeSupported(SOR));
  TS_ASSERT(t->IsTypeSupported(STRSP));
  TS_ASSERT(t->IsTypeSupported(SUB));
  TS_ASSERT(t->IsTypeSupported(SWI));
  TS_ASSERT(t->IsTypeSupported(WEL));
  TS_ASSERT(t->IsTypeSupported(MNW));
  TS_ASSERT(t->IsTypeSupported(MNW2));
  TS_ASSERT(t->IsTypeSupported(MNWI));
  TS_ASSERT(t->IsTypeSupported(UZF));
  TS_ASSERT(t->IsTypeSupported(UPW));
  TS_ASSERT(!t->IsTypeSupported(VDF));
  TS_ASSERT(!t->IsTypeSupported(VSC));
  TS_ASSERT(t->IsTypeSupported(ZON));

  TS_ASSERT_EQUALS(e.m_types.size(), 48);
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpNameFile ()
{
  MfPackage p(Packages::NAM);
  const int MF_FILE_NUMBER(9);
  const int SW_FILE_NUMBER(13);
  const char *nm[SW_FILE_NUMBER] =
    {"f.global", "f1.basic", "f2.lay", "h.hed",
     "h.l", "h.gage", "h.g", "i.ab", "i.abc",
     "j.vdf", "j.vsc",
     "k.btn", "l.gcg"};
  const char *type[SW_FILE_NUMBER] =
    {"GLOBAL", "BAS6", "LPF", "DATA(BINARY)",
     "LMT6", "DATA", "GAGE", "ABC", "ABC",
     "VDF", "VSC",
     "BTN", "GCG"};
  int units[SW_FILE_NUMBER] = {1, 3, 4, 30, 18, 201, 23, 301, 302,
                               401, 402, 501, 502};

  CStr outPath;
  util::GetTempDirectory(outPath);
  CStr nameFile(outPath + "\\testname.mfn");
  CStr abcFile(outPath + "\\testname.abc");

  CStr basePath;
  util::GetTestFilesDirectory(basePath);
  basePath += "\\HDF5_InputFiles";
  TS_ASSERT(SetCurrentDirectory(basePath));
  {
    ExpGmsH5 exp(outPath+"\\testname");
    TxtExporter* t = exp.GetExp();

    for (int i=0; i<MF_FILE_NUMBER; i++)
    {
      p.SetField(Packages::NameFile::FNAME, nm[i]);
      p.SetField(Packages::NameFile::FTYPE, type[i]);
      p.SetField(Packages::NameFile::NIU, &units[i]);
      expNameFile(&p, t, false, MfData::MF2K, &exp);
    }

    expNameFile(0, t, true, MfData::MF2K, &exp);

    CStr expected;
    expected = "GLOBAL 1 testname.glo\n"
               "BAS6 3 testname.ba6\n"
               "LPF 4 testname.lpf\n"
               "DATA(BINARY) 30 testname.hed\n"
               "LMT6 18 testname.lmt6\n"
               "DATA 201 testname.gage\n"
               "GAGE 23 testname.gag\n"
               "ABC 301 testname.abc\n"
               "ABC 302 testname.abc_UNIT_302\n";
    CStr output;
    t->GetFileContents(Packages::NAM, output);
    TS_ASSERT_EQUALS2(expected, output);
    TS_ASSERT_TXT_FILES_EQUAL(basePath+"\\i.ab", abcFile);
  }
  TS_ASSERT(!remove(nameFile));
  TS_ASSERT(!remove(abcFile));

  // test SEAWAT name file and MT3D
  CStr btnFile(outPath + "\\testname.btn");
  CStr gcgFile(outPath + "\\testname.gcg");
  {
    ExpGmsH5 exp(outPath+"\\testname");
    TxtExporter* t = exp.GetExp();

    // clear out the static variables in expNameFile
    expNameFile(&p, t, 2, 0, &exp);

    for (int i=0; i<SW_FILE_NUMBER; i++)
    {
      p.SetField(Packages::NameFile::FNAME, nm[i]);
      p.SetField(Packages::NameFile::FTYPE, type[i]);
      p.SetField(Packages::NameFile::NIU, &units[i]);
      expNameFile(&p, t, false, MfData::MF2K, &exp);
    }

    expNameFile(0, t, true, MfData::SEAWAT, &exp);

    // MODFLOW name file shouldn't have SEAWAT and MT3D packages
    CStr expected;
    expected = "GLOBAL 1 testname.glo\n"
               "BAS6 3 testname.ba6\n"
               "LPF 4 testname.lpf\n"
               "DATA(BINARY) 30 testname.hed\n"
               "LMT6 18 testname.lmt6\n"
               "DATA 201 testname.gage\n"
               "GAGE 23 testname.gag\n"
               "ABC 301 testname.abc\n"
               "ABC 302 testname.abc_UNIT_302\n";
    CStr output;
    t->GetFileContents(Packages::NAM, output);
    TS_ASSERT_EQUALS2(expected, output);
    TS_ASSERT_TXT_FILES_EQUAL(basePath+"\\i.ab", abcFile);

    // SEAWAT name file should have SEAWAT and MT3D packages
    expected = "GLOBAL 1 testname.glo\n"
               "BAS6 3 testname.ba6\n"
               "LPF 4 testname.lpf\n"
               "DATA(BINARY) 30 testname.hed\n"
               "LMT6 18 testname.lmt6\n"
               "DATA 201 testname.gage\n"
               "GAGE 23 testname.gag\n"
               "ABC 301 testname.abc\n"
               "ABC 302 testname.abc_UNIT_302\n"
               "VDF 401 testname.vdf\n"
               "VSC 402 testname.vsc\n"
               "BTN 501 testname.btn\n"
               "GCG 502 testname.gcg\n";
    t->GetFileContents(Packages::SWN, output);
    TS_ASSERT_EQUALS2(expected, output);

    // MT3D super file should have MT3D packages
    expected = "MT3DSUP\n"
               "BTN \"testname.btn\"\n"
               "GCG \"testname.gcg\"\n";
    t->GetFileContents(Packages::MTS, output);
    TS_ASSERT_EQUALS2(expected, output);
    TS_ASSERT_TXT_FILES_EQUAL(basePath+"\\k.btn", btnFile);
    TS_ASSERT_TXT_FILES_EQUAL(basePath+"\\l.gcg", gcgFile);

  }
  TS_ASSERT(!remove(nameFile));
  TS_ASSERT(!remove(abcFile));
  TS_ASSERT(!remove(btnFile));
  TS_ASSERT(!remove(gcgFile));
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testGetArrayMap ()
{
  ExpGmsH5 ex;
  std::map<CStr, CStr> &m(ex.GetMapArrays());
  TS_ASSERT_EQUALS(65, m.size());
  TS_ASSERT(m.find("crap") == m.end());

  TS_ASSERT(m[ARR_DIS_TOP] == "top");
  TS_ASSERT(m[ARR_DIS_BOT] == "bot");
  TS_ASSERT(m[ARR_DIS_VCB] == "vcb");
  TS_ASSERT(m[ARR_BAS_IBND] == "ibound");
  TS_ASSERT(m[ARR_BAS_SHEAD] == "StartHead");

  TS_ASSERT(m[ARR_RCH_RCH] == "Recharge/07. Property");
  TS_ASSERT(m[ARR_RCH_LAY] == "Recharge/09. Layer");

  TS_ASSERT(m[ARR_EVT_SURF] == "ET/07. Property");
  TS_ASSERT(m[ARR_EVT_RATE] == "ET/07. Property");
  TS_ASSERT(m[ARR_EVT_EXT] == "ET/07. Property");
  TS_ASSERT(m[ARR_EVT_LAY] == "ET/09. Layer");

  TS_ASSERT(m[ARR_ETS_SURF] == "ETS/07. Property");
  TS_ASSERT(m[ARR_ETS_RATE] == "ETS/07. Property");
  TS_ASSERT(m[ARR_ETS_EXT] == "ETS/07. Property");
  TS_ASSERT(m[ARR_ETS_LAY] == "ETS/09. Layer");
  TS_ASSERT(m[ARR_ETS_PXDP] == "ETS/16. Ext Depth");
  TS_ASSERT(m[ARR_ETS_PETM] == "ETS/18. Evap Rate");

  TS_ASSERT(m[ARR_BCF_HY] == "HY_");
  TS_ASSERT(m[ARR_BCF_HK_U] == "HY_");
  TS_ASSERT(m[ARR_BCF_TRAN] == "TRAN_");
  TS_ASSERT(m[ARR_BCF_VCONT] == "LEAK_");
  TS_ASSERT(m[ARR_BCF_SF1] == "SF1_");
  TS_ASSERT(m[ARR_BCF_SF2] == "SF2_");
  TS_ASSERT(m[ARR_BCF_WET] == "WET_");

  TS_ASSERT(m[ARR_LPF_HK] == "HK");
  TS_ASSERT(m[ARR_LPF_HANI] == "HANI");
  TS_ASSERT(m[ARR_LPF_VK] == "VK");
  TS_ASSERT(m[ARR_LPF_VANI] == "VANI");
  TS_ASSERT(m[ARR_LPF_SS] == "SS");
  TS_ASSERT(m[ARR_LPF_SY] == "SY");
  TS_ASSERT(m[ARR_LPF_WET] == "WET");
  TS_ASSERT(m[ARR_LPF_VKCBD] == "QUASIVK");
  TS_ASSERT(m[ARR_LPF_ANGX] == "ANGLEX");

  TS_ASSERT(m[ARR_UZF_UBND] == "UZF/12. IUZFBND");
  TS_ASSERT(m[ARR_UZF_RBND] == "UZF/14. IRUNBND");
  TS_ASSERT(m[ARR_UZF_VKS] == "UZF/16. VKS");
  TS_ASSERT(m[ARR_UZF_EPS] == "UZF/18. EPS");
  TS_ASSERT(m[ARR_UZF_THTS] == "UZF/20. THTS");
  TS_ASSERT(m[ARR_UZF_THTI] == "UZF/22. THTI");
  TS_ASSERT(m[ARR_UZF_RCH] == "UZF/07. Property");
  TS_ASSERT(m[ARR_UZF_ET] == "UZF/07. Property");
  TS_ASSERT(m[ARR_UZF_EXT] == "UZF/07. Property");
  TS_ASSERT(m[ARR_UZF_EXTWC] == "UZF/07. Property");

  TS_ASSERT(m[ARR_VDF_DENS] == "VDF/07. Property");
  TS_ASSERT(m[ARR_VDF_CONC] == "VDF/07. Property");

  TS_ASSERT(m[ARR_VSC_VSC] == "VSC/07. Property");
  TS_ASSERT(m[ARR_VSC_CONC] == "VSC/07. Property");

  TS_ASSERT(m[ARR_SUB_RNB] == "SUB/12. RNB");
  TS_ASSERT(m[ARR_SUB_HC] == "SUB/14. HC");
  TS_ASSERT(m[ARR_SUB_SFE] == "SUB/16. Sfe");
  TS_ASSERT(m[ARR_SUB_SFV] == "SUB/18. Sfv");
  TS_ASSERT(m[ARR_SUB_COM] == "SUB/20. Com");
  TS_ASSERT(m[ARR_SUB_DSTRT] == "SUB/22. Dstart");
  TS_ASSERT(m[ARR_SUB_DHC] == "SUB/24. DHC");
  TS_ASSERT(m[ARR_SUB_DCOM] == "SUB/26. DCOM");
  TS_ASSERT(m[ARR_SUB_DZ] == "SUB/28. DZ");
  TS_ASSERT(m[ARR_SUB_NZ] == "SUB/30. NZ");

  TS_ASSERT(m[ARR_SWI_ZETA] == "ZETA");
  TS_ASSERT(m[ARR_SWI_SSZ] == "SSZ");
  TS_ASSERT(m[ARR_SWI_ISOURCE] == "ISOURCE");

  TS_ASSERT(m[ARR_LAK_ID] == "Lak_");
  TS_ASSERT(m[ARR_LAK_LEAK] == "LakLeak_");
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testIsDataArray ()
{
  ExpGmsH5 h5;
  TS_ASSERT(!MfExportUtil::IsDataArray(CStr("crap"), h5.GetMapArrays()));
  TS_ASSERT(MfExportUtil::IsDataArray(CStr(ARR_DIS_TOP),
                                      h5.GetMapArrays()));
  TS_ASSERT(MfExportUtil::IsDataArray(CStr(ARR_DIS_BOT),
                                      h5.GetMapArrays())); // good enough
  TS_ASSERT(MfExportUtil::IsDataArray(CStr("ZONE ARRAY: stuff"),
                                      h5.GetMapArrays()));
  TS_ASSERT(MfExportUtil::IsDataArray(CStr("MULT. ARRAY: stuff"),
                                      h5.GetMapArrays()));
}
//------------------------------------------------------------------------------
static bool testCheckGroupExists (hid_t fid,
                                  const char *a_)
{
  bool rval(true);
  hid_t g(H5Gopen(fid, a_));
  rval = g > -1;
  H5Gclose(g);
  return rval;
}
static bool testCheckDatasetExists (hid_t fid,
                                    const char *a_)
{
  bool rval(true);
  hid_t d(H5Dopen(fid, a_));
  rval = d > -1;
  H5Dclose(d);
  return rval;
}
void ExpGmsH5T::testCreateDefaultMfH5File ()
{
  CStr basePath;
  util::GetTempDirectory(basePath);
  basePath += "\\array";
  CreateDefaultMfH5File(basePath);
  H5DataReader::CloseAllH5FilesOpenForWriting();

  CStr fullPath(basePath + ".h5");
  hid_t fid;
  fid = H5Fopen(fullPath, H5F_ACC_RDONLY, H5P_DEFAULT);
  TS_ASSERT(fid > 0);
  if (fid < 0)
    return;

  TS_ASSERT(testCheckGroupExists(fid, "Arrays"));
  TS_ASSERT(testCheckGroupExists(fid, "Drain"));
  TS_ASSERT(testCheckGroupExists(fid, "Drain Return"));
  TS_ASSERT(testCheckGroupExists(fid, "ET"));
  TS_ASSERT(testCheckGroupExists(fid, "General Head"));
  TS_ASSERT(testCheckGroupExists(fid, "Recharge"));
  TS_ASSERT(testCheckGroupExists(fid, "River"));
  TS_ASSERT(testCheckGroupExists(fid, "Specified Head"));
  TS_ASSERT(testCheckGroupExists(fid, "Stream"));
  TS_ASSERT(testCheckGroupExists(fid, "Well"));
  TS_ASSERT(testCheckGroupExists(fid, "Multi-Node Well"));
  TS_ASSERT(testCheckGroupExists(fid, "UZF"));
  TS_ASSERT(!testCheckGroupExists(fid, "VDF"));
  TS_ASSERT(!testCheckGroupExists(fid, "VSC"));

  TS_ASSERT(testCheckDatasetExists(fid, "Well/00. Number of BCs"));
  TS_ASSERT(testCheckDatasetExists(fid, "Well/01. Use Last"));
  TS_ASSERT(testCheckDatasetExists(fid, "Well/02. Cell IDs"));
  TS_ASSERT(testCheckDatasetExists(fid, "Well/03. Name"));
  TS_ASSERT(testCheckDatasetExists(fid, "Well/04. Map ID"));
  TS_ASSERT(testCheckDatasetExists(fid, "Well/06. IFACE"));
  TS_ASSERT(testCheckDatasetExists(fid, "Well/07. Property"));

  TS_ASSERT(testCheckDatasetExists(fid, "Stream/08. Str reach segment ID"));
  TS_ASSERT(testCheckDatasetExists(fid, "Stream/09. Segment ID"));
  TS_ASSERT(testCheckDatasetExists(fid, "Stream/10. Segment Flow"));
  TS_ASSERT(testCheckDatasetExists(fid, "Stream/11. ITRIB"));
  TS_ASSERT(testCheckDatasetExists(fid, "Stream/12. Upstream ID"));
  TS_ASSERT(testCheckDatasetExists(fid, "Stream/13. Number of Segments"));
  
  TS_ASSERT(testCheckDatasetExists(fid, "ET/08. Property Multiplier"));

  TS_ASSERT(testCheckDatasetExists(fid, "Recharge/01. Use Last"));
  TS_ASSERT(testCheckDatasetExists(fid, "Recharge/07. Property"));
  TS_ASSERT(testCheckDatasetExists(fid, "Recharge/09. Layer"));
  TS_ASSERT(testCheckDatasetExists(fid, "Recharge/10. Layer Multiplier"));

  TS_ASSERT(testCheckDatasetExists(fid, "ETS/16. Ext Depth"));
  TS_ASSERT(testCheckDatasetExists(fid, "ETS/17. Ext Depth Multiplier"));
  TS_ASSERT(testCheckDatasetExists(fid, "ETS/18. Evap Rate"));
  TS_ASSERT(testCheckDatasetExists(fid, "ETS/19. Evap Rate Multiplier"));
  TS_ASSERT(testCheckDatasetExists(fid, "ETS/20. Number of Segments"));

  TS_ASSERT(testCheckDatasetExists(fid, "Multi-Node Well/21. Stress Period Ref"));
  TS_ASSERT(testCheckDatasetExists(fid, "Multi-Node Well/22. Loss Type"));
  TS_ASSERT(testCheckDatasetExists(fid, "Multi-Node Well/23. Well IO"));

  TS_ASSERT(testCheckDatasetExists(fid, "UZF/12. IUZFBND"));
  TS_ASSERT(testCheckDatasetExists(fid, "UZF/13. IUZFBND Multiplier"));
  TS_ASSERT(testCheckDatasetExists(fid, "UZF/14. IRUNBND"));
  TS_ASSERT(testCheckDatasetExists(fid, "UZF/15. IRUNBND Multiplier"));
  TS_ASSERT(testCheckDatasetExists(fid, "UZF/16. VKS"));
  TS_ASSERT(testCheckDatasetExists(fid, "UZF/17. VKS Multiplier"));
  TS_ASSERT(testCheckDatasetExists(fid, "UZF/18. EPS"));
  TS_ASSERT(testCheckDatasetExists(fid, "UZF/19. EPS Multiplier"));
  TS_ASSERT(testCheckDatasetExists(fid, "UZF/20. THTS"));
  TS_ASSERT(testCheckDatasetExists(fid, "UZF/21. THTS Multiplier"));
  TS_ASSERT(testCheckDatasetExists(fid, "UZF/22. THTI"));
  TS_ASSERT(testCheckDatasetExists(fid, "UZF/23. THTI Multiplier"));

  TS_ASSERT(testCheckDatasetExists(fid, MFBC_VERSION));
  H5DataSetReader r(fullPath, MFBC_VERSION);
  std::vector<double> vD;
  r.GetAllData(vD);
  TS_ASSERT(vD.size() == 1);
  TS_ASSERT_EQUALS(vD.at(0), 3.0);

  H5Fclose(fid);
  H5Reader::CloseAllH5Files();
  TS_ASSERT(!remove(fullPath));

  // test creation of SEAWAT specific items
  CreateDefaultMfH5File(basePath, 3);
  H5DataReader::CloseAllH5FilesOpenForWriting();

  fid = H5Fopen(fullPath, H5F_ACC_RDONLY, H5P_DEFAULT);
  TS_ASSERT(fid > 0);
  if (fid < 0)
    return;

  TS_ASSERT(testCheckGroupExists(fid, "VDF"));
  TS_ASSERT(testCheckGroupExists(fid, "VSC"));

  TS_ASSERT(testCheckDatasetExists(fid, "VDF/07. Property"));
  TS_ASSERT(testCheckDatasetExists(fid, "VSC/07. Property"));

  H5Fclose(fid);
  H5Reader::CloseAllH5Files();
  TS_ASSERT(!remove(fullPath));
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpSEN ()
{
  using namespace MfData::Packages;
  MfData::MfPackage pSen(Packages::SEN);
  MfData::MfPackage pSen1(Packages::SEN1);
  int i[6]={1,2,3,4,5,6};
  int nplist=4;
  char *parnam = "RCH_ZONE_1RCH_ZONE_2RCH_ZONE_3Q         ";
  int isens[4]={1,2,3,4};
  int ln[4]={4,5,6,7};
  Real b[4]={7,8,9,0};
  Real bl[4]={0,1,2,3};
  Real bu[4]={3,4,5,6};
  Real bscal[4]={6,7,8,9};
  ParamList *list;
  Parameters::GetParameterList(&list);
  list->Clear();

  pSen.SetField("ISENALL", &i[0]);
  pSen.SetField("IUHEAD", &i[1]);
  pSen.SetField("IPRINTS", &i[2]);
  pSen.SetField("ISENSU", &i[3]);
  pSen.SetField("ISENPU", &i[4]);
  pSen.SetField("ISENFM", &i[5]);

  pSen1.SetField("NPLIST", &nplist);
  pSen1.SetField("PARNAM", parnam);
  pSen1.SetField("ISENS", isens);
  pSen1.SetField("LN", ln);
  pSen1.SetField("B", b);
  pSen1.SetField("BL", bl);
  pSen1.SetField("BU", bu);

  TxtExporter t(TESTBASE);
  CStr expected, output;

  saveSEN(&pSen, &pSen1, &t);
  expSEN(&t);
  t.GetFileContents(Packages::OCT, output);
  TS_ASSERT(output.empty());
  t.m_public->m_SensitivityItems.clear();

  pSen1.SetField("BSCAL", bscal);

  saveSEN(&pSen, &pSen1, &t);
  expSEN(&t);
  t.GetFileContents(Packages::OCT, output);
  TS_ASSERT(output.empty());
  t.m_public->m_SensitivityItems.clear();

  Param pRCH1("RCH_ZONE_1", -2, "RCH");
  Param pRCH2("RCH_ZONE_2", -3, "RCH");
  Param pRCH3("RCH_ZONE_3", -4, "RCH");
  Param pQ("QPARAM", -5, "Q  ");
  list->PushBack(&pRCH1);
  list->PushBack(&pRCH2);
  list->PushBack(&pRCH3);
  list->PushBack(&pQ);
  saveSEN(&pSen, &pSen1, &t);
  expSEN(&t);
  list->Clear();
  t.m_public->m_SensitivityItems.clear();

  expected = "3 1 2 4 \n"
             "3 4 5 6 \n"
             "RCH_ZONE_1 1 4 7.0 0.0 3.0 6.0 \n"
             "RCH_ZONE_2 2 5 8.0 1.0 4.0 7.0 \n"
             "RCH_ZONE_3 3 6 9.0 2.0 5.0 8.0 \n";
  t.GetFileContents(Packages::SEN, output);
  TS_ASSERT_EQUALS2(expected, output);
  t.m_public->m_SensitivityItems.clear();
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testWellPropertyList()
{
  Mnw1PropList wpl;

  int cells[]   = { 0, 1, 0, 3, 4, 5, 6, 7, 8, 9, 10 };
  int expectedCellIds[] = { 0, 1, 0, 3, 0, 1, 0, 1, 1, 0, 1 };
  int expectedProps[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
  vector<int> expectedPropsVec;
  const int NUM_NAMES = 11;
  char *names[NUM_NAMES] = { "Site-1", "Site-1", "Site-2", "Site-2", "Well-3",
                             "Well-3", "Well-4", "Well-4", "Well-5", "Well-5",
                             "Well-6" };
  vector<CStr> namesVec(names, names+NUM_NAMES);
  CStr *expectedNames = &namesVec[0];
  int expectedWellIds[] = { 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 0 };
  char expectedIsSite[] = { true, true, true, true, false, false, false, false,
                            false, false, false };
  vector<int> cellIds;
  vector<int> properties;

  cellIds.insert(cellIds.begin(), cells, cells+2);
  properties = wpl.GetPropIndicees("Site-1", cellIds);
  TS_ASSERT_EQUALS_AVEC(expectedProps, 2, properties);
  TS_ASSERT_EQUALS_AVEC(expectedCellIds, 2, wpl.GetCellIds());
  TS_ASSERT_EQUALS_AVEC(expectedNames, 2, wpl.GetWellNames());
  TS_ASSERT_EQUALS_AVEC(expectedWellIds, 2, wpl.GetWellIds());
  TS_ASSERT_EQUALS_AVEC(expectedIsSite, 2, wpl.GetIsSiteName());

  // second site entry with different name moves on to new properties position
  cellIds.clear();
  cellIds.insert(cellIds.begin(), cells+2, cells+4);
  properties = wpl.GetPropIndicees("Site-2", cellIds);
  TS_ASSERT_EQUALS_AVEC(expectedProps+2, 2, properties);
  TS_ASSERT_EQUALS_AVEC(expectedCellIds, 4, wpl.GetCellIds());
  TS_ASSERT_EQUALS_AVEC(expectedNames, 4, wpl.GetWellNames());
  TS_ASSERT_EQUALS_AVEC(expectedWellIds, 4, wpl.GetWellIds());
  TS_ASSERT_EQUALS_AVEC(expectedIsSite, 4, wpl.GetIsSiteName());

  // second site entry with same name and cells moves on to new position
  cellIds.clear();
  cellIds.insert(cellIds.begin(), cells, cells+2);
  properties = wpl.GetPropIndicees("Site-1", cellIds);
  TS_ASSERT_EQUALS_AVEC(expectedProps+4, 2, properties);
  TS_ASSERT_EQUALS_AVEC(expectedCellIds, 6, wpl.GetCellIds());
  TS_ASSERT_EQUALS_AVEC(expectedNames, 6, wpl.GetWellNames());
  TS_ASSERT_EQUALS_AVEC(expectedWellIds, 6, wpl.GetWellIds());
  TS_ASSERT_EQUALS_AVEC(expectedIsSite, 6, wpl.GetIsSiteName());

  // new well entry moves on to new properties position
  properties = wpl.GetPropIndicees("", cellIds);
  TS_ASSERT_EQUALS_AVEC(expectedProps+6, 2, properties);
  TS_ASSERT_EQUALS_AVEC(expectedCellIds, 8, wpl.GetCellIds());
  TS_ASSERT_EQUALS_AVEC(expectedNames, 8, wpl.GetWellNames());
  TS_ASSERT_EQUALS_AVEC(expectedWellIds, 8, wpl.GetWellIds());
  TS_ASSERT_EQUALS_AVEC(expectedIsSite, 8, wpl.GetIsSiteName());

  // new well entry with same cellid moves on to new properties position
  cellIds[0] = 1;
  cellIds[1] = 0;
  properties = wpl.GetPropIndicees("", cellIds);
  TS_ASSERT_EQUALS_AVEC(expectedProps+8, 2, properties);
  TS_ASSERT_EQUALS_AVEC(expectedCellIds, 10, wpl.GetCellIds());
  TS_ASSERT_EQUALS_AVEC(expectedNames, 10, wpl.GetWellNames());
  TS_ASSERT_EQUALS_AVEC(expectedWellIds, 10, wpl.GetWellIds());
  TS_ASSERT_EQUALS_AVEC(expectedIsSite, 10, wpl.GetIsSiteName());

  // new single cell well entry uses zero for wellid
  cellIds.clear();
  cellIds.push_back(1);
  properties = wpl.GetPropIndicees("", cellIds);
  TS_ASSERT_EQUALS_AVEC(expectedProps+10, 1, properties);
  TS_ASSERT_EQUALS_AVEC(expectedCellIds, 11, wpl.GetCellIds());
  TS_ASSERT_EQUALS_AVEC(expectedNames, 11, wpl.GetWellNames());
  TS_ASSERT_EQUALS_AVEC(expectedWellIds, 11, wpl.GetWellIds());
  TS_ASSERT_EQUALS_AVEC(expectedIsSite, 11, wpl.GetIsSiteName());

  // new stess period should find the same places
  // also reversing cellIds above should reverse properties returned
  wpl.NewStressPeriod();
  cellIds.resize(2);
  cellIds[0] = 1;
  cellIds[1] = 0;
  properties = wpl.GetPropIndicees("Site-1", cellIds);
  expectedPropsVec.clear();
  expectedPropsVec.insert(expectedPropsVec.begin(), expectedProps, expectedProps+2);
  std::reverse(expectedPropsVec.begin(), expectedPropsVec.end());
  TS_ASSERT_EQUALS_VEC(expectedPropsVec, properties);
  TS_ASSERT_EQUALS_AVEC(expectedCellIds, 11, wpl.GetCellIds());
  TS_ASSERT_EQUALS_AVEC(expectedNames, 11, wpl.GetWellNames());
  TS_ASSERT_EQUALS_AVEC(expectedWellIds, 11, wpl.GetWellIds());
  TS_ASSERT_EQUALS_AVEC(expectedIsSite, 11, wpl.GetIsSiteName());

  // second site entry with different name moves on to new properties position
  cellIds[0] = 3;
  cellIds[1] = 0;
  properties = wpl.GetPropIndicees("Site-2", cellIds);
  expectedPropsVec.clear();
  expectedPropsVec.insert(expectedPropsVec.begin(), expectedProps+2, expectedProps+4);
  std::reverse(expectedPropsVec.begin(), expectedPropsVec.end());
  TS_ASSERT_EQUALS_VEC(expectedPropsVec, properties);
  TS_ASSERT_EQUALS_AVEC(expectedCellIds, 11, wpl.GetCellIds());
  TS_ASSERT_EQUALS_AVEC(expectedNames, 11, wpl.GetWellNames());
  TS_ASSERT_EQUALS_AVEC(expectedWellIds, 11, wpl.GetWellIds());
  TS_ASSERT_EQUALS_AVEC(expectedIsSite, 11, wpl.GetIsSiteName());

  // second site entry with same name moves on to new properties position
  cellIds[0] = 1;
  cellIds[1] = 0;
  properties = wpl.GetPropIndicees("Site-1", cellIds);
  expectedPropsVec.clear();
  expectedPropsVec.insert(expectedPropsVec.begin(), expectedProps+4, expectedProps+6);
  std::reverse(expectedPropsVec.begin(), expectedPropsVec.end());
  TS_ASSERT_EQUALS_VEC(expectedPropsVec, properties);
  TS_ASSERT_EQUALS_AVEC(expectedCellIds, 11, wpl.GetCellIds());
  TS_ASSERT_EQUALS_AVEC(expectedNames, 11, wpl.GetWellNames());
  TS_ASSERT_EQUALS_AVEC(expectedWellIds, 11, wpl.GetWellIds());
  TS_ASSERT_EQUALS_AVEC(expectedIsSite, 11, wpl.GetIsSiteName());

  // new well entry moves on to new properties position
  properties = wpl.GetPropIndicees("", cellIds);
  expectedPropsVec.clear();
  expectedPropsVec.insert(expectedPropsVec.begin(), expectedProps+6, expectedProps+8);
  std::reverse(expectedPropsVec.begin(), expectedPropsVec.end());
  TS_ASSERT_EQUALS_VEC(expectedPropsVec, properties);
  TS_ASSERT_EQUALS_AVEC(expectedCellIds, 11, wpl.GetCellIds());
  TS_ASSERT_EQUALS_AVEC(expectedNames, 11, wpl.GetWellNames());
  TS_ASSERT_EQUALS_AVEC(expectedWellIds, 11, wpl.GetWellIds());
  TS_ASSERT_EQUALS_AVEC(expectedIsSite, 11, wpl.GetIsSiteName());

  // new well entry with same cellid moves on to new properties position
  cellIds[0] = 0;
  cellIds[1] = 1;
  properties = wpl.GetPropIndicees("", cellIds);
  expectedPropsVec.clear();
  expectedPropsVec.insert(expectedPropsVec.begin(), expectedProps+8, expectedProps+10);
  std::reverse(expectedPropsVec.begin(), expectedPropsVec.end());
  TS_ASSERT_EQUALS_VEC(expectedPropsVec, properties);
  TS_ASSERT_EQUALS_AVEC(expectedCellIds, 11, wpl.GetCellIds());
  TS_ASSERT_EQUALS_AVEC(expectedNames, 11, wpl.GetWellNames());
  TS_ASSERT_EQUALS_AVEC(expectedWellIds, 11, wpl.GetWellIds());
  TS_ASSERT_EQUALS_AVEC(expectedIsSite, 11, wpl.GetIsSiteName());
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpGetAllArealUseLast ()
{
  CStr f, f1, path, path1;
  util::GetTempDirectory(f);
  f += "\\tmp";
  f1 = f;
  f1 += ".h5";

  CreateDefaultMfH5File(f);

  path = "Recharge/";
  path1 = path;
  path1 += MFBC_USELAST;
  std::vector<int> dat(2, 0);
  expUseLastAreal(f, path, 1, dat);
  dat[0] = dat[1] = 1;
  expUseLastAreal(f, path, 2, dat);

  CAR_INT2D flags;
  expGetAllArealUseLast(flags, "Recharge/", f);
  TS_ASSERT_EQUALS(flags.GetSize1(), 2);
  TS_ASSERT_EQUALS(flags.GetSize2(), 2);
  TS_ASSERT_EQUALS(flags.at(0,0), 0);
  TS_ASSERT_EQUALS(flags.at(0,1), 1);
  TS_ASSERT_EQUALS(flags.at(1,0), 0);
  TS_ASSERT_EQUALS(flags.at(1,1), 1);
  H5DataReader::CloseAllH5FilesOpenForWriting();

  TS_ASSERT(!remove(f1));
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpArealPropFromUseLast ()
{
  CStr f, f1, path, path1, path2;
  util::GetTempDirectory(f);
  f += "\\tmp";
  f1 = f;
  f1 += ".h5";

  CreateDefaultMfH5File(f);

  path = "Recharge/";
  path1 = path;
  path1 += MFBC_DATA;
  path2 = path;
  path2 += MFBC_DATAMULT;
  std::vector<int> dat(2, 0);
  expUseLastAreal(f, path, 1, dat);
  dat[0] = dat[1] = 1;
  expUseLastAreal(f, path, 2, dat);

  CAR_INT2D flags;
  expGetAllArealUseLast(flags, "Recharge/", f);
  expArealPropFromUseLast(flags, 10, "Recharge/", f);
  {
    CAR_DBL3D dat;
    dat.SetSize(1, 10, 2, 1);
    std::pair<int, int> p(0,1);
    VEC_INT_PAIR indices(3,p);
    indices[1].second = 10;
    indices[2].second = 2;
    H5DataSetReader r(f1, path1, indices);
    r.GetData(&dat.at(0,0,0), 20);
    TS_ASSERT_EQUALS(dat.at(0,0,0), 0);
    TS_ASSERT_EQUALS(dat.at(0,9,1), 0);
  }
  {
    CAR_DBL2D dat;
    dat.SetSize(1, 2, 0);
    std::pair<int, int> p(0,1);
    VEC_INT_PAIR indices(2,p);
    indices[1].second = 2;
    H5DataSetReader r(f1, path2, indices);
    r.GetData(&dat.at(0,0), 2);
    TS_ASSERT_EQUALS(dat.at(0,0), 1);
    TS_ASSERT_EQUALS(dat.at(0,1), 1);
  }
  H5DataReader::CloseAllH5FilesOpenForWriting();
  TS_ASSERT(!remove(f1));

  CreateDefaultMfH5File(f);

  path = "Recharge/";
  path1 = path;
  path1 += MFBC_DATA;
  dat[0] = dat[1] = 0;
  expUseLastAreal(f, path, 1, dat);
  dat[0] = dat[1] = 1;
  expUseLastAreal(f, path, 2, dat);
  expGetAllArealUseLast(flags, "Recharge/", f);
  // write some data and then call the function
  {
    std::vector<double> vd(10, 2);
    H5DataSetWriterSetup s(f1, path1, H5T_NATIVE_DOUBLE, 3);
    std::vector<hsize_t> start1(3, 0), n2write(3, 1);
    n2write[1] = 10;
    H5DSWriterDimInfo dim1(start1, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim1);
    w.WriteData(&vd[0], 10);
  }
  {
    CAR_DBL2D mult;
    mult.SetSize(1,1,2);
    H5DataSetWriterSetup s(f1, path2, H5T_NATIVE_DOUBLE, 2);
    std::vector<hsize_t> start1(2, 0),
                         n2write(2, 1);
    H5DSWriterDimInfo dim1(start1, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim1);
    w.WriteData(&mult.at(0,0), 1);
  }

  expArealPropFromUseLast(flags, 10, "Recharge/", f);
  {
    CAR_DBL3D dat;
    dat.SetSize(1, 10, 2, 1);
    std::pair<int, int> p(0,1);
    VEC_INT_PAIR indices(3,p);
    indices[1].second = 10;
    indices[2].second = 2;
    H5DataSetReader r(f1, path1, indices);
    r.GetData(&dat.at(0,0,0), 20);
    TS_ASSERT_EQUALS(dat.at(0,0,0), 2);
    TS_ASSERT_EQUALS(dat.at(0,9,0), 2);
    TS_ASSERT_EQUALS(dat.at(0,9,1), 0);
    TS_ASSERT_EQUALS(dat.at(0,9,1), 0);
  }
  {
    CAR_DBL2D dat;
    dat.SetSize(1, 2, 0);
    std::pair<int, int> p(0,1);
    VEC_INT_PAIR indices(2,p);
    indices[1].second = 2;
    H5DataSetReader r(f1, path2, indices);
    r.GetData(&dat.at(0,0), 2);
    TS_ASSERT_EQUALS(dat.at(0,0), 2);
    TS_ASSERT_EQUALS(dat.at(0,1), 1);
  }
  H5DataReader::CloseAllH5FilesOpenForWriting();
  TS_ASSERT(!remove(f1));
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpEtSegFromUseLast ()
{
  CStr f, f1, path, path1, path2;
  util::GetTempDirectory(f);
  f += "\\tmp";
  f1 = f;
  f1 += ".h5";

  CreateDefaultMfH5File(f);

  path = "ETS/";
  path1 = path;
  path1 += MFBC_PXDP;
  path2 = path;
  path2 += MFBC_PXDPMULT;

  // shouldn't add items from Use Last when ETS package not enabled
  CAR_INT2D flags;
  expGetAllArealUseLast(flags, "ETS/", f);
  // update the areal data
  expArealPropFromUseLast(flags, 10, "ETS/", f);
  // update the areal layer
  expArealLayFromUseLast(flags, 10, "ETS/", f);
  // update the extinction depth proportion
  expEtSegFromUseLast(flags, 10, f, MFBC_PXDP, MFBC_PXDPMULT);
  {
    vector<hsize_t> size;

    // property
    H5DataSetReader propertyReader(f1, path + MFBC_DATA);
    propertyReader.GetDataSetDimensions(size);
    TS_ASSERT(size.size() == 3);
    TS_ASSERT_EQUALS2(1, size.at(0));
    TS_ASSERT_EQUALS2(1, size.at(1));
    TS_ASSERT_EQUALS2(1, size.at(2));

    // layer
    H5DataSetReader layerReader(f1, path + MFBC_LAY);
    layerReader.GetDataSetDimensions(size);
    TS_ASSERT(size.size() == 2);
    TS_ASSERT_EQUALS2(1, size.at(0));
    TS_ASSERT_EQUALS2(1, size.at(1));

    // extinction depth
    H5DataSetReader extDepthReader(f1, path + MFBC_PXDP);
    extDepthReader.GetDataSetDimensions(size);
    TS_ASSERT(size.size() == 3);
    TS_ASSERT_EQUALS2(1, size.at(0));
    TS_ASSERT_EQUALS2(1, size.at(1));
    TS_ASSERT_EQUALS2(1, size.at(2));
  }

  std::vector<int> dat(2, 0);
  expUseLastAreal(f, path, 1, dat);
  dat[0] = dat[1] = 1;
  expUseLastAreal(f, path, 2, dat);

  expGetAllArealUseLast(flags, "ETS/", f);
  expEtSegFromUseLast(flags, 10, f, MFBC_PXDP, MFBC_PXDPMULT);
  {
    CAR_DBL3D dat;
    dat.SetSize(1, 10, 2, 1);
    std::pair<int, int> p(0,1);
    VEC_INT_PAIR indices(3,p);
    indices[1].second = 10;
    indices[2].second = 2;
    H5DataSetReader r(f1, path1, indices);
    r.GetData(&dat.at(0,0,0), 20);
    TS_ASSERT_EQUALS(dat.at(0,0,0), 0);
    TS_ASSERT_EQUALS(dat.at(0,9,1), 0);
  }
  {
    CAR_DBL2D dat;
    dat.SetSize(1, 2, 0);
    std::pair<int, int> p(0,1);
    VEC_INT_PAIR indices(2,p);
    indices[1].second = 2;
    H5DataSetReader r(f1, path2, indices);
    r.GetData(&dat.at(0,0), 2);
    TS_ASSERT_EQUALS(dat.at(0,0), 1);
    TS_ASSERT_EQUALS(dat.at(0,1), 1);
  }
  H5DataReader::CloseAllH5FilesOpenForWriting();
  TS_ASSERT(!remove(f1));
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpArealLayFromUseLast ()
{
  CStr f, f1, path, path1, path2;
  util::GetTempDirectory(f);
  f += "\\tmp";
  f1 = f;
  f1 += ".h5";

  CreateDefaultMfH5File(f);

  path = "Recharge/";
  path1 = path;
  path1 += MFBC_LAY;
  path2 = path;
  path2 += MFBC_LAYMULT;
  std::vector<int> dat(2, 0);
  expUseLastAreal(f, path, 1, dat);
  dat[0] = dat[1] = 1;
  expUseLastAreal(f, path, 2, dat);

  CAR_INT2D flags;
  expGetAllArealUseLast(flags, "Recharge/", f);
  expArealLayFromUseLast(flags, 10, "Recharge/", f);
  {
    CAR_INT2D dat;
    dat.SetSize(10, 2, 2);
    std::pair<int, int> p(0,10);
    VEC_INT_PAIR indices(2,p);
    indices[1].second = 2;
    H5DataSetReader r(f1, path1, indices);
    r.GetData(&dat.at(0,0), 20);
    TS_ASSERT_EQUALS(dat.at(0,0), 1);
    TS_ASSERT_EQUALS(dat.at(9,1), 1);
  }
  {
    std::vector<int> dat(2, 0);
    std::pair<int, int> p(0,2);
    VEC_INT_PAIR indices(1,p);
    H5DataSetReader r(f1, path2, indices);
    r.GetData(&dat.at(0), 2);
    TS_ASSERT_EQUALS(dat.at(0), 1);
    TS_ASSERT_EQUALS(dat.at(1), 1);
  }
  H5DataReader::CloseAllH5FilesOpenForWriting();
  TS_ASSERT(!remove(f1));


  CreateDefaultMfH5File(f);

  path = "Recharge/";
  path1 = path;
  path1 += MFBC_LAY;
  dat[0] = dat[1] = 0;
  expUseLastAreal(f, path, 1, dat);
  dat[0] = dat[1] = 1;
  expUseLastAreal(f, path, 2, dat);
  expGetAllArealUseLast(flags, "Recharge/", f);
  // write some data and then call the function
  {
    std::vector<int> vd(10, 2);
    H5DataSetWriterSetup s(f1, path1, H5T_NATIVE_INT, 2);
    std::vector<hsize_t> start1(2, 0), n2write(2, 1);
    n2write[0] = 10;
    H5DSWriterDimInfo dim1(start1, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim1);
    w.WriteData(&vd[0], 10);
  }
  {
    int mult(2);
    H5DataSetWriterSetup s(f1, path2, H5T_NATIVE_INT, 1);
    std::vector<hsize_t> start1(1, 0),
                         n2write(1, 1);
    H5DSWriterDimInfo dim1(start1, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim1);
    w.WriteData(&mult, 1);
  }

  expArealLayFromUseLast(flags, 10, "Recharge/", f);
  {
    CAR_INT2D dat;
    dat.SetSize(10, 2, 2);
    std::pair<int, int> p(0,10);
    VEC_INT_PAIR indices(2,p);
    indices[1].second = 2;
    H5DataSetReader r(f1, path1, indices);
    r.GetData(&dat.at(0,0), 20);
    TS_ASSERT_EQUALS(dat.at(0,0), 2);
    TS_ASSERT_EQUALS(dat.at(9,0), 2);
    TS_ASSERT_EQUALS(dat.at(0,1), 1);
    TS_ASSERT_EQUALS(dat.at(9,1), 1);
  }
  {
    std::vector<int> dat(2, 0);
    std::pair<int, int> p(0,2);
    VEC_INT_PAIR indices(1,p);
    H5DataSetReader r(f1, path2, indices);
    r.GetData(&dat.at(0), 2);
    TS_ASSERT_EQUALS(dat.at(0), 2);
    TS_ASSERT_EQUALS(dat.at(1), 1);
  }
  H5DataReader::CloseAllH5FilesOpenForWriting();
  TS_ASSERT(!remove(f1));
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpSuperFile ()
{
  MfData::MfPackage pack("NAM1");
  CStr f, output, line;
  util::GetTempDirectory(f);
  f += "\\fmsuper";

  {
    TxtExporter t(f);
    iWasParamFileExported(&t) = false;
    expSuperFile(0, &pack, &t);
    t.GetFileContents("mfs", output);
    line = "MF2KSUP\n"
           "NAME 99 \"fmsuper.mfn\"\n";
    TS_ASSERT_EQUALS2(output, line);
  }
  {
    TxtExporter t(f);
    iWasParamFileExported(&t) = true;
    expSuperFile(0, &pack, &t);
    t.GetFileContents("mfs", output);
    line = "MF2KSUP\n"
           "MPARAM \"fmsuper.param\"\n"
           "NAME 99 \"fmsuper.mfn\"\n";
    TS_ASSERT_EQUALS2(output, line);
  }
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpParamFile ()
{
  MfData::MfPackage pack("NAM1");
  CStr f, output, line;
  util::GetTempDirectory(f);
  f += "\\parameters.mfs";
  {
    //pack.SetField(MfData::Packages::ListPack::ITMP, &itmp);
    TxtExporter t(f);
    expParamFile(&pack, &t);
    t.GetFileContents("param", output);
    line = "";
    TS_ASSERT_EQUALS2(output, line);
    TS_ASSERT_EQUALS(iWasParamFileExported(&t), false);
  }

  ParamList *list(0);
  Parameters::GetParameterList(&list);
  list->Clear();

  {
    Param p("w1", -1, "Q", 5, .1, 25);
    p.m_isens = 1;
    list->PushBack(&p);
  }
  {
    Param p("d1", -2, "DRN", 5, .1, 25);
    list->PushBack(&p);
  }

  {
    TxtExporter t(f);
    expParamFile(&pack, &t);
    t.GetFileContents("param", output);
    line = "BEGPAR\n"
           "NAME \"w1\"\n"
           "TYPE WELL\n"
           "KEY -1.0\n"
           "VALUE 5.0 0.1 25.0\n"
           "SOLVE 1\n"
           "BSCAL 1.0\n"
           "ENDPAR\n"
           "BEGPAR\n"
           "NAME \"d1\"\n"
           "TYPE DRN\n"
           "KEY -2.0\n"
           "VALUE 5.0 0.1 25.0\n"
           "SOLVE 0\n"
           "BSCAL 1.0\n"
           "ENDPAR\n";
    TS_ASSERT_EQUALS2(output, line);
    TS_ASSERT_EQUALS(iWasParamFileExported(&t), true);
  }
  list->Clear();
}

#endif

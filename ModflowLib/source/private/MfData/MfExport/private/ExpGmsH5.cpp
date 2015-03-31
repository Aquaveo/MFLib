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
#define FILE_VERSION   "File Version"
#define MFBC_VERSION   "MFH5 Version"
#define MFBC_NUMBC     "00. Number of BCs"
#define MFBC_USELAST   "01. Use Last"
#define MFBC_CELLIDS   "02. Cell IDs"
#define MFBC_NAME      "03. Name"
#define MFBC_MAPIDSTR  "04. Map ID"
#define MFBC_FACTOR    "05. Factor"
#define MFBC_IFACE     "06. IFACE"
#define MFBC_DATA      "07. Property"
#define MFBC_DATAMULT  "08. Property Multiplier"
#define MFBC_LAY       "09. Layer"
#define MFBC_LAYMULT   "10. Layer Multiplier"
// stream
#define MFBC_STRSEGID  "08. Str reach segment ID"
#define MFBC_SEGID     "09. Segment ID"
#define MFBC_SEGFLW    "10. Segment Flow"
#define MFBC_ITRIB     "11. ITRIB"
#define MFBC_UPID      "12. Upstream ID"
#define MFBC_NSEG      "13. Number of Segments"
#define MFBC_SEGP      "14. Segment Property"
#define MFBC_SEGFLWT   "15. Segment Flow Table"
// ets
#define MFBC_PXDP      "16. Ext Depth"
#define MFBC_PXDPMULT  "17. Ext Depth Multiplier"
#define MFBC_PETM      "18. Evap Rate"
#define MFBC_PETMMULT  "19. Evap Rate Multiplier"
#define MFBC_NETSEG    "20. Number of Segments"
// mnw
#define MFBC_KSPREF    "21. Stress Period Ref"
#define MFBC_LOSSTYPE  "22. Loss Type"
#define MFBC_IOWELL2   "23. Well IO"
// uzf
#define MFBC_IUZFBND     "12. IUZFBND"
#define MFBC_IUZFBNDMULT "13. IUZFBND Multiplier"
#define MFBC_IRUNBND     "14. IRUNBND"
#define MFBC_IRUNBNDMULT "15. IRUNBND Multiplier"
#define MFBC_VKS         "16. VKS"
#define MFBC_VKSMULT     "17. VKS Multiplier"
#define MFBC_EPS         "18. EPS"
#define MFBC_EPSMULT     "19. EPS Multiplier"
#define MFBC_THTS        "20. THTS"
#define MFBC_THTSMULT    "21. THTS Multiplier"
#define MFBC_THTI        "22. THTI"
#define MFBC_THTIMULT    "23. THTI Multiplier"
// sub
#define MFBC_RNB         "12. RNB"
#define MFBC_RNBMULT     "13. RNB Multiplier"
#define MFBC_HC          "14. HC"
#define MFBC_HCMULT      "15. HC Multiplier"
#define MFBC_SFE         "16. Sfe"
#define MFBC_SFEMULT     "17. Sfe Multiplier"
#define MFBC_SFV         "18. Sfv"
#define MFBC_SFVMULT     "19. Sfv Multiplier"
#define MFBC_COM         "20. Com"
#define MFBC_COMMULT     "21. Com Multiplier"
#define MFBC_DSTART      "22. Dstart"
#define MFBC_DSTARTMULT  "23. Dstart Multiplier"
#define MFBC_DHC         "24. DHC"
#define MFBC_DHCMULT     "25. DHC Multiplier"
#define MFBC_DCOM        "26. DCOM"
#define MFBC_DCOMMULT    "27. DCOM Multiplier"
#define MFBC_DZ          "28. DZ"
#define MFBC_DZMULT      "29. DZ Multiplier"
#define MFBC_NZ          "30. NZ"
#define MFBC_NZMULT      "31. NZ Multiplier"

#define MFBC_MAX_STR_LEN "Max. String Length"

#define PARAM_FILE_TYPES "DRN DRT GHB RIV CHD Q   STR SFR"



enum sfr2SegFds_enum { SFR2S_ICALC=0, SFR2S_OUTSEG,SFR2S_IUPSEG,
                       SFR2S_IPRIOR, SFR2S_FLOW,
                       SFR2S_RUNOFF, SFR2S_ETSW, SFR2S_PPTSW, SFR2S_ROUGHCH,
                       SFR2S_ROUGHBK, SFR2S_CDPTH, SFR2S_FDPTH, SFR2S_AWDPTH,
                       SFR2S_BWDTH, SFR2S_HCOND1, SFR2S_THICKM1, SFR2S_ELEVUP,
                       SFR2S_WIDTH1, SFR2S_DEPTH1, SFR2S_HCOND2,SFR2S_THICKM2,
                       SFR2S_ELEVDN, SFR2S_WIDTH2, SFR2S_DEPTH2,
                       SFR2S_XSECT,
                       SFR2S_XS02,SFR2S_XS03,SFR2S_XS04,SFR25_XS05,SFR2S_XS06,
                       SFR2S_XS07,SFR2S_XS08,SFR2S_XS09,SFR25_XS10,SFR2S_XS11,
                       SFR2S_XS12,SFR2S_XS13,SFR2S_XS14,SFR25_XS15,
                       SFR2S_XSECT_END,
                       SFR2S_COND1FACT, SFR2S_COND2FACT,
                       SFR2S_NPROP };


enum enumNameFileType { NF_MODFLOW, NF_MT3D, NF_SEAWAT };

////////////////////////////////////////////////////////////////////////////////
/// \class WellPropertyList
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
/// \class WellPropertyList
////////////////////////////////////////////////////////////////////////////////
//------------------------------------------------------------------------------
/// \brief Class to manage list of cells and names and how they map into the
///        properties array
//------------------------------------------------------------------------------
class WellPropertyList
{
public:
  WellPropertyList();

  void NewStressPeriod();
  vector<int> GetPropIndicees(const CStr& a_siteName,
                                   const vector<int>& a_cellids);
  const vector<int>& GetCellIds();
  const vector<int>& GetWellIds();
  const vector<CStr>& GetWellNames();
  const vector<char>& GetIsSiteName();

protected:
  bool MissingSiteName(const CStr& a_siteName);
  bool MatchingSiteCells(const CStr& a_siteName, 
                         const vector<int>& a_cellids,
                         size_t& a_startIndex);
  std::vector<int> BuildPropIndicees(size_t a_startIndex,
                                     const vector<int>& a_cellids);
  void AppendSiteWell(const CStr& a_siteName,
                      const vector<int>& a_cellids,
                      size_t& a_startIndex);
  bool FoundCellSet(const vector<int>& a_cellids,
                    size_t& a_startIndex);
  void AppendNewWell(const vector<int>& a_cellids, size_t& a_startIndex);
  void AppendItem(int a_cellId,
                  const CStr& a_name,
                  bool a_isSite,
                  int a_wellNumber);

  vector<int> m_cellIds;
  vector<int> m_wellIds;
  vector<CStr> m_names;
  vector<char> m_isSiteName;
  vector<char> m_used;
  int m_nextWellNumber;

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
  , m_TxtLineArrayMap()
  , m_SubPackageArrayCounts()
  , m_needsSENFile(0)
  , m_mnwFlgsCopy()
  , m_maxBcSoFar()
  , m_BcCellIds()
  , m_BcIface()
  , m_BcCellGrp()
  , m_BcIdxMap()
  , m_HFBParameterData()
  , m_SensitivityHeader()
  , m_SensitivityItems()
  , m_ArrayCountMap()
  , m_NAM_ftype()
  , m_NAM_fname()
  , m_NAM_niu()
  , m_NAM_maxStrLen(0)
  , m_HasBinaryExport(0)
  , m_WellPropertyList()
  {}

  virtual ~ExpGmsH5Public()
  {}

  bool m_paramFileWasExported;
  std::map<CStr, CStr> m_MultArrayMap;
  std::vector<CStr> m_ArrayOrderVector;
  std::map<CStr, CStr> m_ZoneArrayMap;
  std::set<CStr> m_ExportedPackages;
  std::map<CStr,CStr> m_TxtLineArrayMap;
  std::map<CStr, int> m_SubPackageArrayCounts;
  bool m_needsSENFile;
  std::vector<double> m_mnwFlgsCopy;
  std::map<CStr, int> m_maxBcSoFar;
  std::map<CStr, std::vector<int> > m_BcCellIds;
  std::map<CStr, std::vector<int> > m_BcIface;
  std::map<CStr, std::vector<int> > m_BcCellGrp;
  std::map<CStr, stdext::hash_map<int, VEC_INT_PAIR> > m_BcIdxMap;
  std::map<CStr, vector<Real> > m_HFBParameterData;
  SensitivityHeader m_SensitivityHeader;
  std::vector<SensitivityItem> m_SensitivityItems;
  std::map<CStr, int> m_ArrayCountMap;
  std::vector<CStr> m_NAM_ftype;
  std::vector<CStr> m_NAM_fname;
  std::vector<int> m_NAM_niu;
  int m_NAM_maxStrLen;
  bool m_HasBinaryExport;
  WellPropertyList m_WellPropertyList;

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
static std::map<CStr,CStr> &GetTxtLineArrayMap(TxtExporter* a_exp);
static void WriteArrayLineToFile(TxtExporter* a_exp, const char* a_packName,
                                 const char* a_arrayName, int a_index);
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
static void expMultArrayFunc(MfPackage* a_package,
                             TxtExporter* a_exp);
static void expMultFile(TxtExporter *a_exp);
static void expZoneFile(TxtExporter *a_exp);
static void expDataArray(MfPackage *a_package,
                         MfGlobal *a_global,
                         TxtExporter *a_exp,
                         const char *a_fname,
                         std::map<CStr, CStr>& a_map,
                         const bool a_compress=true);
static int  GetMultiDimArrayIndex(const CStr &a_name,
                                  TxtExporter* a_exp,
                                  int a_sp = 0);
static void expRecharge(MfPackage *a_p,
                        const int a_sp,
                        TxtExporter *a_exp);
static void expET(MfPackage *a_p,
                  const int a_sp,
                  TxtExporter *a_exp,
                  const char *a_baseName);
static void expOC(MfPackage *a_p,
                  TxtExporter *a_exp);
static void expOCT(MfPackage *a_p,
                   TxtExporter *a_exp);
static void expPVAL(MfPackage *a_p,
                    TxtExporter *a_exp);
static void expSEN(TxtExporter *a_exp);
static void saveSEN(MfPackage *a_pSen1,
                    MfPackage *a_pSen,
                    TxtExporter* a_exp);
static void expPES(MfPackage *a_p,
                   TxtExporter *a_exp);
static void expHUF(MfPackage *a_p,
                   int a_lay,
                   TxtExporter *a_exp);
static void expSTR(MfPackage *a_p,
                   const int a_sp,
                   const int a_nRow,
                   const int a_nCol,
                   TxtExporter *a_exp);
static void iGetListPackTypeFromPtr(const char *a_p,
                                    CStr &a_type);
static void expListPack(MfPackage *a_p,
                        const int a_sp,
                        const int a_nRow,
                        const int a_nCol,
                        TxtExporter *a_exp);
static void expListParameterData(MfPackage *a_p,
                                 const int a_sp,
                                 const int a_nRow,
                                 const int a_nCol,
                                 TxtExporter *a_exp);
static void iSizeBcDataArray(const CStr &a_type,
                             const int a_maxIdx,
                             CAR_DBL2D &a_data);
static std::map<CStr, std::vector<int> > &iGetBcIface(TxtExporter* a_exp);
static std::map<CStr, std::vector<int> > &iGetBcCellGrp(TxtExporter* a_exp);
static std::map<CStr, std::vector<int> > &iGetBcCellIds(TxtExporter* a_exp);
static std::map<CStr, stdext::hash_map<int, VEC_INT_PAIR> > &iGetBcIdxMap(TxtExporter* a_exp);
static int iGetBcIndex(const CStr &a_type,
                       int a_cellid,
                       int a_sp,
                       std::vector<int> &a_cellIds,
                       std::vector<int> &a_iface,
                       std::vector<int> &a_cellgrp,
                       TxtExporter* a_exp);
static int& iGetMaxBcSoFar(const CStr& a_type,
                           TxtExporter* a_exp);
static void expHFB(MfPackage *a_p,
                   TxtExporter *a_exp);
static void expLAK(MfPackage *a_p,
                   TxtExporter *a_exp);
static void expLAKSP(MfGlobal *a_g,
                     MfPackage *a_p,
                     MfPackage *a_LAK,
                     TxtExporter *a_exp);
static void expSFRLine1(MfPackage *a_pSFRLine1,
                        TxtExporter *a_exp);
static void expSFRLine2(MfPackage *a_pSFRLine1,
                        MfPackage *a_pSFRLine2,
                        int a_nRow,
                        int a_nCol,
                        int a_nStressPeriods,
                        TxtExporter *a_exp);
static void expSFRLine5(MfPackage *a_pSFRLine5,
                        int a_sp,
                        TxtExporter *a_exp);
static void expSFRLine6(MfPackage *a_pSFRLine1,
                        MfPackage *a_pSFRLine6,
                        int a_sp,
                        TxtExporter *a_exp);
static void expMNWSetup(MfPackage *a_p,
                        TxtExporter *a_exp);
static void expMNWStressPeriod(MfPackage *a_p,
                               int a_sp,
                               TxtExporter *a_exp);
static void expMNW2(MfGlobal *a_g,
                    MfPackage *a_p,
                    TxtExporter *a_exp);
static void expMNWI(MfGlobal *a_g,
                    MfPackage *a_p,
                    TxtExporter *a_exp);
static void expSUB(MfPackage *a_p,
                   TxtExporter *a_exp);
static void expSUBLine15(MfPackage *a_p,
                         TxtExporter *a_exp);
static void expSUBLine16(MfPackage *a_p,
                         TxtExporter *a_exp);
static void expUZFLine1(MfPackage *a_p,
                        TxtExporter *a_exp);
static void expUZFLine8(MfPackage *a_pLine1,
                        MfPackage *a_pLine8,
                        int a_numRow,
                        int a_numCol,
                        TxtExporter *a_exp);
static void expUZFStressPeriod(MfPackage *a_pLine1,
                               MfPackage *a_pSP,
                               int a_sp,
                               TxtExporter *a_exp);
static void expVDFLine5(MfPackage *a_p,
                        TxtExporter *a_exp);
static void expVDFStressPeriod(MfPackage *a_pLine5,
                               MfPackage *a_pSP,
                               int a_sp,
                               int a_rows,
                               int a_cols,
                               int a_layer,
                               TxtExporter *a_exp);
static void expVSCLine3(MfPackage *a_p,
                        TxtExporter *a_exp);
static void expVSCStressPeriod(MfPackage *a_pLine3,
                               MfPackage *a_pSP,
                               int a_sp,
                               int a_rows,
                               int a_cols,
                               int a_layer,
                               TxtExporter *a_exp);
static void expGAG(MfPackage *a_p,
                   TxtExporter *a_exp);
static void expLGR_1(MfPackage *a_p,
                     TxtExporter *a_exp);
static void expLGR_2(MfPackage *a_p,
                     TxtExporter *a_exp);
static void expLGR(MfGlobal* a_global);
static void expFinalize(MfGlobal* a_global,
                        TxtExporter *a_exp);
static void showWarnings(TxtExporter* a_exp);
static void expCheckArealFromUseLast(int a_nCells,
                                     const char * a_baseName);
static void expWriteMapIdsForListBcs(MfGlobal* a_global,
                                     TxtExporter *a_exp);
static std::vector<CStr> &GetArrayOrderVector();
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
/// \brief Create a sep separated string from an iterator range
//------------------------------------------------------------------------------
template <class Str, class It>
Str join(It begin, const It end, const Str &sep)
{
  typedef typename Str::value_type     char_type;
  typedef typename Str::traits_type    traits_type;
  typedef typename Str::allocator_type allocator_type;
  typedef std::basic_ostringstream<char_type,traits_type,allocator_type>
                                       ostringstream_type;
  ostringstream_type result;

  if(begin!=end)
    result << *begin++;
  while(begin!=end) {
    result << sep;
    result << *begin++;
  }
  return result.str();
}
//------------------------------------------------------------------------------
/// \brief holds a flag
//------------------------------------------------------------------------------
static bool& iWasParamFileExported (TxtExporter* a_exp)
{
  return a_exp->m_public->m_paramFileWasExported;
} // iWasParamFileExported
//------------------------------------------------------------------------------
/// \brief Map that holds the multiplier arrays with the path to the h5 file
//------------------------------------------------------------------------------
static std::map<CStr, CStr>& MultArrayMap (TxtExporter* a_exp)
{
  return a_exp->m_public->m_MultArrayMap;
} // MultArrayMap
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
static std::vector<CStr>& GetArrayOrderVector (TxtExporter* a_exp)
{
  return a_exp->m_public->m_ArrayOrderVector;
}
//------------------------------------------------------------------------------
/// \brief Map that holds the zone arrays with the path to the h5 file
//------------------------------------------------------------------------------
static std::map<CStr, CStr>& ZoneArrayMap (TxtExporter* a_exp)
{
  return a_exp->m_public->m_ZoneArrayMap;
} // ZoneArrayMap
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
/// \brief
//------------------------------------------------------------------------------
static void WriteSingleH5IntValue (const char *filePath,
                                   const char *h5Path,
                                   int value)
{
  H5DataSetWriterSetup s(filePath, h5Path, H5T_NATIVE_INT, 1);
  std::vector<hsize_t> start(1, 0), n2write(1, 1);
  H5DSWriterDimInfo dim(start, n2write);
  H5DataSetWriter w(&s);
  w.SetDimInfoForWriting(&dim);
  w.WriteData(&value, 1);
} // WriteSingleH5IntValue
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void WriteSingleH5DoubleValue (const char *filePath,
                                      const char *h5Path,
                                      double value)
{
  H5DataSetWriterSetup s(filePath, h5Path, H5T_NATIVE_DOUBLE, 1);
  std::vector<hsize_t> start(1, 0), n2write(1, 1);
  H5DSWriterDimInfo dim(start, n2write);
  H5DataSetWriter w(&s);
  w.SetDimInfoForWriting(&dim);
  w.WriteData(&value, 1);
} // WriteSingleH5DoubleValue
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void Write1DH5Value (const char *filePath,
                            const char *h5Path,
                            hsize_t position,
                            int value)
{
  H5DataSetWriterSetup s(filePath, h5Path, H5T_NATIVE_INT, 1);
  std::vector<hsize_t> start(1, position), n2write(1, 1);
  H5DSWriterDimInfo dim(start, n2write);
  H5DataSetWriter w(&s);
  w.SetDimInfoForWriting(&dim);
  w.WriteData(&value, 1);
} // Write1DH5Value
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void Write1DIntArray (const char *a_filePath,
                             const char *a_h5Path,
                             const int *a_array,
                             size_t a_length,
                             size_t a_start = 0)
{
  H5DataSetWriterSetup s(a_filePath, a_h5Path, H5T_NATIVE_INT, 1);
  std::vector<hsize_t> start(1, a_start), n2write(1, a_length);
  H5DSWriterDimInfo dim(start, n2write);
  H5DataSetWriter w(&s);
  w.SetDimInfoForWriting(&dim);
  w.WriteData(a_array, a_length);
} // Write1DIntArray
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void Write1DDblArray (const char *a_filePath,
                             const char *a_h5Path,
                             const double *a_array,
                             size_t a_length,
                             size_t a_start = 0)
{
  H5DataSetWriterSetup s(a_filePath, a_h5Path, H5T_NATIVE_DOUBLE, 1);
  std::vector<hsize_t> start(1, a_start), n2write(1, a_length);
  H5DSWriterDimInfo dim(start, n2write);
  H5DataSetWriter w(&s);
  w.SetDimInfoForWriting(&dim);
  w.WriteData(a_array, a_length);
} // Write1DDblArray
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void WriteH5StringArray (const char *a_filePath,
                                const char *a_h5Path,
                                const vector<CStr>& a_array)
{
  int maxLen(0);

  for (vector<CStr>::const_iterator s = a_array.begin();
       s != a_array.end(); ++s)
  {
    int len = (int)s->length()+1;
    if (len > maxLen)
      maxLen = len;
  }

  vector<char> strings(a_array.size()*maxLen, ' ');
  for (size_t i = 0; i < a_array.size(); ++i)
  {
    strcpy(&strings.at(i*maxLen), a_array.at(i).c_str());
  }

  H5DataSetWriterSetup ws(a_filePath, a_h5Path, H5T_NATIVE_CHAR, 1);
  std::vector<hsize_t> start(1, 0), n2write(1, strings.size());
  H5DSWriterDimInfo dim(start, n2write);
  H5DataSetWriter w(&ws);
  w.SetDimInfoForWriting(&dim);
  w.WriteData(&strings[0], strings.size());
  w.WriteAtt(MFBC_MAX_STR_LEN, maxLen);
} // WriteH5StringArray
//------------------------------------------------------------------------------
/// \brief checks if package name is a DISU array
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
           || ARR_LPF_HK == a_
           || ARR_LPF_HANI == a_
           || ARR_LPF_VK == a_
           || ARR_LPF_VANI == a_
           || ARR_LPF_SS == a_
           || ARR_LPF_SY == a_
           || ARR_LPF_VKCBD == a_
           || ARR_LPF_ANGX == a_
           )
  {
    NativeUtil::ExportNextToH5();
    rval = true;
  }
  else if (   ARR_BCF_WET == a_
           || ARR_LPF_WET == a_
           )
  {
    if (a_global->GetPackage(Packages::LPF) || 
        a_global->GetPackage(Packages::BCF) )
    {
      NativeUtil::ExportNextToH5();
      rval = true;
    }
  }
  return rval;
} // iArrayToNative
//------------------------------------------------------------------------------
/// \brief Uses the native exporter to write the files
//------------------------------------------------------------------------------
static bool iPackageToNativeExport (
  const CStr& a_,
  MfGlobal* a_global)
{
  bool rval = false;
  if (   MfExportUtil::IsSolver(a_)
      || Packages::DISU == a_
      || Packages::DIS == a_
      || Packages::BCF == a_
      || Packages::BAS == a_
      || "L98" == a_
      || Packages::UPW == a_
      //|| Packages::OC == a_
      //|| Packages::OCT == a_
      || iArrayToNative(a_, a_global))
  {
     rval = true;
  }

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
    if (iPackageToNativeExport(packName, a_global))
    {
      if (!m_nativeExp)
      {
        m_nativeExp = new Mf2kNative;
        m_nativeExp->SetFileName(m_fname.c_str());
        m_nativeExp->SetArraysInternal(true);
      }
      if (m_nativeExp)
      {
        a_exp->AtLeastOneTransientSPExists() =
          m_nativeExp->GetExp()->AtLeastOneTransientSPExists();
        a_exp->SetOfSteadyStateStressPeriods() =
          m_nativeExp->GetExp()->SetOfSteadyStateStressPeriods();
        rval = m_nativeExp->ExportPackage(a_global, a_package);
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
  bool rval(true), unstructured(a_global->Unstructured() ? 1 : 0);
  if (m_p->ExportNative(a_global, a_package, GetExp())) return rval;

  CStr packName(a_package->PackageName());
  ExportedPackages(GetExp()).insert(packName);
  if (Packages::NAM == packName)
  {
    expNameFile(a_package, GetExp(), false, GetModelType(), this);
  }
  else if (Packages::RCH == packName)
  {
    expRecharge(a_package,
                a_global->GetCurrentPeriod(),
                GetExp());
  }
  else if (Packages::EVT == packName || Packages::ETS == packName)
  {
    expET(a_package, a_global->GetCurrentPeriod(), GetExp(),
          GetExp()->GetBaseFileName());
  }
  else if (Packages::OC == packName)
  {
    expOC(a_package, GetExp());
  }
  else if (Packages::OCT == packName)
  {
    expOCT(a_package, GetExp());
  }
  else if (Packages::HUF == packName)
  {
    expHUF(a_global->GetPackage(Packages::HUF),
           a_global->NumLay(),
           GetExp());
  }
  else if (Packages::SEN1 == packName)
  {
    saveSEN(a_global->GetPackage(Packages::SEN),
            a_package,
            GetExp());
  }
  else if (Packages::PVAL == packName)
  {
    expPVAL(a_package, GetExp());
  }
  else if (Packages::PES == packName)
  {
    expPES(a_package, GetExp());
  }
  else if (Packages::STRSP == packName)
  {
    expSTR(a_package,
           a_global->GetCurrentPeriod(),
           a_global->NumRow(),
           a_global->NumCol(),
           GetExp());
  }
  else if (Packages::DRN == packName ||
           Packages::RIV == packName ||
           Packages::WEL == packName ||
           Packages::GHB == packName ||
           Packages::CHD == packName ||
           Packages::DRT == packName)
  {
    expListPack(a_package,
                a_global->GetCurrentPeriod(),
                a_global->NumRow(),
                a_global->NumCol(),
                GetExp());
  }
  else if (MfData::Packages::LPRM == packName)
  {
    expListParameterData(a_package,
                         a_global->GetCurrentPeriod(),
                         a_global->NumRow(),
                         a_global->NumCol(),
                         GetExp());
  }
  else if (Packages::HFB == packName)
  {
    expHFB(a_package, GetExp());
  }
  else if (Packages::LAK == packName)
  {
    expLAK(a_package, GetExp());
  }
  else if (Packages::LAKSP == packName)
  {
    expLAKSP(a_global,
             a_package,
             a_global->GetPackage(Packages::LAK),
             GetExp());
  }
  else if (Packages::SFRLine1 == packName)
  {
    expSFRLine1(a_global->GetPackage(Packages::SFRLine1),
                GetExp());
  }
  else if (Packages::SFRLine2 == packName)
  {
    expSFRLine2(a_global->GetPackage(Packages::SFRLine1),
                a_global->GetPackage(Packages::SFRLine2),
                a_global->NumRow(),
                a_global->NumCol(),
                a_global->NumPeriods(),
                GetExp());
  }
  else if (Packages::SFRLine5 == packName)
  {
    expSFRLine5(a_global->GetPackage(Packages::SFRLine5),
                a_global->GetCurrentPeriod(),
                GetExp());
  }
  else if (Packages::SFRLine6 == packName)
  {
    expSFRLine6(a_global->GetPackage(Packages::SFRLine1),
                a_global->GetPackage(Packages::SFRLine6),
                a_global->GetCurrentPeriod(),
                GetExp());
  }
  else if (Packages::MNWSetup == packName)
  {
    expMNWSetup(a_global->GetPackage(Packages::MNWSetup),
                GetExp());
  }
  else if (Packages::MNWStressPeriod == packName)
  {
    expMNWStressPeriod(a_global->GetPackage(Packages::MNWStressPeriod),
                       a_global->GetCurrentPeriod(),
                       GetExp());
  }
  else if (Packages::MNW2 == packName)
  {
    expMNW2(a_global,
            a_package,
            GetExp());
  }
  else if (Packages::MNWI == packName)
  {
    expMNWI(a_global,
            a_package,
            GetExp());
  }
  else if (Packages::SUB == packName)
  {
    expSUB(a_package, GetExp());
  }
  else if (Packages::SUBLine15 == packName)
  {
    expSUBLine15(a_package, GetExp());
  }
  else if (Packages::SUBLine16 == packName)
  {
    expSUBLine16(a_package, GetExp());
  }
  else if (Packages::UZFLine1 == packName)
  {
    expUZFLine1(a_global->GetPackage(Packages::UZFLine1), GetExp());
  }
  else if (Packages::UZFLine8 == packName)
  {
    expUZFLine8(a_global->GetPackage(Packages::UZFLine1),
                a_global->GetPackage(Packages::UZFLine8),
                a_global->NumRow(),
                a_global->NumCol(),
                GetExp());
  }
  else if (Packages::UZFStressPeriod == packName)
  {
    expUZFStressPeriod(a_global->GetPackage(Packages::UZFLine1),
                       a_global->GetPackage(Packages::UZFStressPeriod),
                       a_global->GetCurrentPeriod(),
                       GetExp());
  }
  else if (Packages::VDFLine5 == packName)
  {
    expVDFLine5(a_global->GetPackage(Packages::VDFLine5), GetExp());
  }
  else if (Packages::VDFStressPeriod == packName)
  {
    expVDFStressPeriod(a_global->GetPackage(Packages::VDFLine5),
                       a_global->GetPackage(Packages::VDFStressPeriod),
                       a_global->GetCurrentPeriod(),
                       a_global->NumRow(),
                       a_global->NumCol(),
                       a_global->NumLay(),
                       GetExp());
  }
  else if (Packages::VSCLine3 == packName)
  {
    expVSCLine3(a_global->GetPackage(Packages::VSCLine3), GetExp());
  }
  else if (Packages::VSCStressPeriod == packName)
  {
    expVSCStressPeriod(a_global->GetPackage(Packages::VSCLine3),
                       a_global->GetPackage(Packages::VSCStressPeriod),
                       a_global->GetCurrentPeriod(),
                       a_global->NumRow(),
                       a_global->NumCol(),
                       a_global->NumLay(),
                       GetExp());
  }
  else if (Packages::GAGE == packName)
  {
    expGAG(a_package, GetExp());
  }
  else if (Packages::LGR_1 == packName)
  {
    expLGR_1(a_package, GetExp());
  }
  else if (Packages::LGR_2 == packName)
  {
    expLGR_2(a_package, GetExp());
  }
  else if (Packages::LGR == packName)
  {
    expLGR(a_global);
  }
  else if ("FNC" == packName) // multiplier function
  {
    expMultArrayFunc(a_package,
                     GetExp());
  }
  else if (MfExportUtil::IsDataArray(packName, GetMapArrays()))
  {
    expDataArray(a_package, a_global, 
                 GetExp(),
                 GetExp()->GetBaseFileName(),
                 GetMapArrays(),
                 Compress());
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
    expMultFile(GetExp());
    expZoneFile(GetExp());
    expSuperFile(a_global->ModelType(), a_global->GetPackage("NAM1"), GetExp());
    expFinalize(a_global,
                GetExp());
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
/// \brief Holds the file global
//------------------------------------------------------------------------------
static std::map<CStr,CStr> &GetTxtLineArrayMap (TxtExporter* a_exp)
{
  return a_exp->m_public->m_TxtLineArrayMap;
} // GetTxtLineArrayMap
//------------------------------------------------------------------------------
/// \brief Output line for array to file
//------------------------------------------------------------------------------
static void WriteArrayLineToFile(TxtExporter* a_exp, const char* a_packName,
                                 const char* a_arrayName, int a_index)
{
  std::map<CStr,CStr> &m(GetTxtLineArrayMap(a_exp));
  CStr tag;
  tag.Format("%s/%s%d", a_packName, a_arrayName, a_index);
  if (m.find(tag) != m.end())
    a_exp->WriteLineToFile(a_packName, m[tag]);
} // WriteArrayLineToFile
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static std::map<CStr, int>& GetSubPackageArrayCounts (TxtExporter* a_exp)
{
  return a_exp->m_public->m_SubPackageArrayCounts;
} // GetSubPackageArrayCounts
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static int GetSubPackageArrayIndex (const CStr &a_name,
                                    TxtExporter* a_exp)
{
  std::map<CStr, int>& arrayCount = GetSubPackageArrayCounts(a_exp);
  arrayCount[a_name] += 1;
  return arrayCount[a_name];
} // GetSubPackageArrayIndex
//------------------------------------------------------------------------------
/// \brief Gets the type of list bc package
//------------------------------------------------------------------------------
static void iGetListPackTypeFromPtr (const char *a_p,
                                     CStr &a_type)
{
  CStr packName(a_p);
  if (Packages::DRN == packName)
    a_type = "Drain";
  else if (Packages::DRT == packName)
    a_type = "Drain Return";
  else if (Packages::RIV == packName)
    a_type = "River";
  else if (Packages::WEL == packName)
    a_type = "Well";
  else if (Packages::GHB == packName)
    a_type = "General Head";
  else if (Packages::CHD == packName)
    a_type = "Specified Head";
  else
    a_type = "";

} // iGetListPackTypeFromPtr
//------------------------------------------------------------------------------
/// \brief Gets the type of list bc package
//------------------------------------------------------------------------------
static void iGetListParamTypeFromPtr (const char *a_p,
                                      CStr &a_type)
{
  CStr packName(a_p);
  if (Packages::DRN == packName)
    a_type = "DRN";
  else if (Packages::DRT == packName)
    a_type = "DRT";
  else if (Packages::RIV == packName)
    a_type = "RIV";
  else if (Packages::WEL == packName)
    a_type = "Q";
  else if (Packages::GHB == packName)
    a_type = "GHB";
  else if (Packages::CHD == packName)
    a_type = "CHD";
  else
    a_type = "";

} // iGetListParamTypeFromPtr
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
/// \brief stores the multiplier array function
//------------------------------------------------------------------------------
static void expMultArrayFunc (MfPackage* a_package,
                              TxtExporter* a_exp)
{
  const char *name(0), *func(0);

  if (!a_package->GetField("NAME", &name) || !name ||
      !a_package->GetField("FUNC", &func) || !func)
    return;

  // prefix the name with "m" - see expDataArray for the explanation
  CStr cName("m");
  cName += name;

  std::vector<CStr>& v = GetArrayOrderVector(a_exp);
  std::map<CStr, CStr>& m = MultArrayMap(a_exp);
  if (m.find(cName) == m.end())
  {
    m.insert(std::make_pair(cName, func));
    v.push_back(cName);
  }
  else
  {
    // ignore and don't export any duplicates
    // MODFLOW takes first MULT item found
  }
} // expMultArrayFunc
//------------------------------------------------------------------------------
/// \brief Exports the multiplier file
//------------------------------------------------------------------------------
static void expMultFile (TxtExporter *a_exp)
{
  using namespace Packages;

  CStr line, tag;
  std::map<CStr, CStr> &m(MultArrayMap(a_exp));
  std::vector<CStr> &v(GetArrayOrderVector(a_exp));

  // get the number of multipliers
  line.Format("%d", m.size());
  a_exp->WriteLineToFile(MLT, line);

  // write out the multipliers
  std::vector<CStr>::iterator it(v.begin());
  std::map<CStr, CStr>::iterator it_m(m.begin());
  for (; it != v.end(); it++)
  {
    tag.Format("%s1", (*it));
    it_m = m.find((*it));
    if (GetTxtLineArrayMap(a_exp).find(tag) != GetTxtLineArrayMap(a_exp).end())
    {
      line = it_m->first;
      line.Delete(0, 1);
      a_exp->WriteLineToFile(MLT, line);
      a_exp->WriteLineToFile(MLT, GetTxtLineArrayMap(a_exp)[tag]);
    }
    else if (!it_m->second.IsEmpty())
    {
      // this is a multiplier function
      line.Format("%s FUNCTION", it_m->first);
      line.Delete(0, 1);
      a_exp->WriteLineToFile(MLT, line);
      a_exp->WriteLineToFile(MLT, it_m->second);
    }
  }
  //std::map<CStr, CStr>::iterator it(m.begin());
  //for (; it != m.end(); it++)
  //{
  //  tag.Format("%s1", it->first);
  //  if (GetTxtLineArrayMap().find(tag) != GetTxtLineArrayMap().end())
  //  {
  //    line = it->first;
  //    line.Delete(0, 1);
  //    a_exp->WriteLineToFile(MLT, line);
  //    a_exp->WriteLineToFile(MLT, GetTxtLineArrayMap()[tag]);
  //  }
  //  else if (!it->second.IsEmpty())
  //  {
  //    // this is a multiplier function
  //    line.Format("%s FUNCTION", it->first);
  //    line.Delete(0, 1);
  //    a_exp->WriteLineToFile(MLT, line);
  //    a_exp->WriteLineToFile(MLT, it->second);
  //  }
  //}
} // expMultFile
//------------------------------------------------------------------------------
/// \brief Exports the zone file
//------------------------------------------------------------------------------
static void expZoneFile (TxtExporter *a_exp)
{
  using namespace Packages;

  CStr line, tag;
  std::map<CStr, CStr> &m(ZoneArrayMap(a_exp));

  // get the number of multipliers
  line.Format("%d", m.size());
  a_exp->WriteLineToFile(ZON, line);
  // write out the zones
  std::map<CStr, CStr>::iterator it(m.begin());
  for (; it != m.end(); it++)
  {
    tag.Format("%s1", it->first);
    if (GetTxtLineArrayMap(a_exp).find(tag) != GetTxtLineArrayMap(a_exp).end())
    {
      line = it->first;
      line.Delete(0, 1);
      a_exp->WriteLineToFile(ZON, line);
      a_exp->WriteLineToFile(ZON, GetTxtLineArrayMap(a_exp)[tag]);
    }
  }
} // expZoneFile
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
/// \brief Exports 2D data array
/// \param a_package Package setup with the unique name of the array from
///                  MODFLOW. Each package parameter read as an array has a
///                  unique array name.
/// \param a_global
/// \param a_fname   H5 file name
//------------------------------------------------------------------------------
static void expDataArray (MfPackage *a_package,
                          MfGlobal *a_global,
                          TxtExporter *a_exp,
                          const char *a_fname,
                          std::map<CStr, CStr>& a_map,
                          const bool a_compressed)
{
  CStr name(a_package->PackageName());

  if (a_global->GetPackage(Packages::BCF))
  {
    if (name.find(ARR_LPF_HK) == 0)
      name = ARR_BCF_HY;
    else if (name.find(ARR_LPF_WET) == 0)
      name = ARR_BCF_WET;
  }

  CStr path;
  std::vector<Real> zoneVals;
  Real zoneMult;
  int nCells(a_global->NumCol()*a_global->NumRow());
  bool zoneArray(false);
  if (name.find("MULT. ARRAY:") != -1 ||
      name.find("ZONE ARRAY:") != -1)
  {
    // read the name of the array
    CStr aName(name);
    // we replace these strings with "m" and "z" because you can
    // have mult and zone arrays with the same name and that will
    // cause problems for our h5 file in the code below
    aName.Replace("MULT. ARRAY: ", "m");
    aName.Replace("ZONE ARRAY: ", "z");
    a_map.insert(std::make_pair(aName, aName));
    path.Format("Arrays/%s", aName);
    if (name.find("MULT. ARRAY:") != -1)
    {
      MultArrayMap(a_exp).insert(std::make_pair(aName, ""));
      GetArrayOrderVector(a_exp).push_back(aName);
    }
    else
    {
      zoneArray = true;
      ZoneArrayMap(a_exp).insert(std::make_pair(aName, ""));
      zoneVals.assign(nCells, 0);
      const int *iVal, *iValMult;
      if (a_package->GetField(Packages::Array::ARRAY, &iVal) &&
          a_package->GetField(Packages::Array::MULT, &iValMult))
      {
        for (int i=0; i<nCells; i++)
          zoneVals[i] = static_cast<Real>(iVal[i]);
        a_package->SetField(Packages::Array::ARRAY, &zoneVals[0]);
        zoneMult = static_cast<Real>(*iValMult);
        a_package->SetField(Packages::Array::MULT, &zoneMult);
      }
    }
    name = aName;
  }

  int tmpiMult;
  const int* lay(0), *iData(0), *iMult(0), *iPRN(0);
  const Real* data(0), *mult(0);
  hid_t datatype(-1);
  if (!a_package->GetField(Packages::Array::LAYER, &lay) || !lay)
    return;
  a_package->GetField(Packages::Array::IPRN, &iPRN);
  // the array is either "Reals" or "Ints"
  a_package->GetField(Packages::Array::MULT, &mult);
  if (!a_package->GetField(Packages::Array::ARRAY, &data) || !data ||
      !mult)
  {
    a_package->GetField(Packages::Array::MULT, &iMult);
    if (!iMult && mult)
    {
      tmpiMult = static_cast<int>(*mult);
      iMult = &tmpiMult;
      mult = nullptr;
    }

    if (!a_package->GetField(Packages::Array::ARRAY, &iData) || !iData ||
        !iMult)
      return;
    else
      datatype = H5T_NATIVE_INT;
  }
  else
    datatype = H5T_NATIVE_DOUBLE;

  int theLay(*lay);
  if (theLay < 1) theLay = 1;

  bool useSP(false), arealSpLay(false), arealInt(false), arealReal(false),
       allCells(false), arealCountedLayer(false);
  CStr f(a_fname);
  f += ".h5";
  if (path.empty())
    path.Format("Arrays/%s%d", a_map.find(name)->second, theLay);

  int spIdx(0);
  // if this is ET, RCH, etc then it is slightly different
  if (path.find("Recharge/") != -1 ||
      path.find("ET/") != -1 || path.find("ETS/") != -1)
  {
    path = a_map.find(name)->second;
    if (path.find("/09. Layer") != -1)
      arealSpLay = true;
    else
      useSP = true;
  }
  else if (path.find("UZF/") != -1)
  {
    path = a_map.find(name)->second;
    if (path.find("07. Property") != -1)
    {
      useSP = true;
      // we need to create the first stress period will all of the dimensions
      int flag(1);
      if (!a_global->GetIntVar("UZF_ARRAY_FIRST_TIME", flag))
      {
        flag = 0;
        a_global->SetIntVar("UZF_ARRAY_FIRST_TIME", flag);
        std::vector<hsize_t> tmpDim(3, 1), tmpStart(3, 0);
        tmpDim[0] = 4;
        tmpDim[1] = static_cast<hsize_t>(nCells);
        WriteDataSetWithZeros(f, path, &tmpDim[0], &tmpStart[0]);
      }
    }
    else if (path.find(MFBC_IUZFBND) != -1 || path.find(MFBC_IRUNBND) != -1)
      arealInt = true;
    else
      arealReal = true;
  }
  else if (path.find("VDF/07. Property") != -1)
  {
    path = a_map.find(name)->second;
    allCells = true;
  }
  else if (path.find("VSC/07. Property") != -1)
  {
    path = a_map.find(name)->second;
    allCells = true;
  }
  else if (path.find("SUB/") != -1)
  {
    path = a_map.find(name)->second;
    theLay = GetSubPackageArrayIndex(name, a_exp);
    arealCountedLayer = true;
  }

  int nDim(1);
  if (useSP)
    nDim = 3;
  else if (arealSpLay || arealCountedLayer)
    nDim = 2;
  else if (allCells)
    nDim = 3;

  CStr multStr;
  int multiIdx(-1);
  {
    H5DataSetWriterSetup setup(f, path, datatype, nDim, a_compressed);
    H5DataSetWriter h(&setup);
    h.AllowTypeConversions(true);
    // do multidim stuff if needed
    spIdx = a_global->GetCurrentPeriod() - 1;
    std::vector<hsize_t> start(2,0), n2write(2,1);
    if (useSP)
    {
      start.push_back(0);
      n2write.push_back(1);
      multiIdx = GetMultiDimArrayIndex(name, a_exp, spIdx);
      start[0] = static_cast<hsize_t>(multiIdx);
      start[2] = static_cast<hsize_t>(spIdx);
      n2write[1] = static_cast<hsize_t>(nCells);
      H5DSWriterDimInfo dim(start, n2write);
      h.SetDimInfoForWriting(&dim);
    }
    else if (arealSpLay)
    {
      n2write[0] = static_cast<hsize_t>(nCells);
      start[1] = static_cast<hsize_t>(spIdx);
      H5DSWriterDimInfo dim(start, n2write);
      h.SetDimInfoForWriting(&dim);
    }
    else if (arealCountedLayer)
    {
      n2write[0] = static_cast<hsize_t>(nCells);
      start[1] = static_cast<hsize_t>(theLay-1);
      H5DSWriterDimInfo dim(start, n2write);
      h.SetDimInfoForWriting(&dim);
    }
    else if (allCells)
    {
      start.push_back(0);
      n2write.push_back(1);
      n2write[1] = static_cast<hsize_t>(nCells);
      multiIdx = GetMultiDimArrayIndex(name, a_exp, spIdx);
      start[1] = static_cast<hsize_t>(nCells*multiIdx);
      start[2] = static_cast<hsize_t>(spIdx);
      H5DSWriterDimInfo dim(start, n2write);
      h.SetDimInfoForWriting(&dim);
    }

    if (data)
    {
      h.WriteData(data, static_cast<size_t>(nCells));
      multStr = STR(*mult);
    }
    else
    {
      h.WriteData(iData, static_cast<size_t>(nCells));
      multStr = STR((float)*iMult);
    }
  }

  // store the path to the data with the multiplier
  CStr f1;
  util::StripPathFromFilename(f, f1);
  CStr line, tag;
  int prn(-1);
  if (iPRN)
    prn = *iPRN;
  line.Format("HDF5 %s %d \"%s\" \"%s\" ", multStr, prn, f1, path);
  if (useSP)
  {
    tag.Format("3 %d 1 0 %d %d 1", multiIdx, nCells, spIdx);
    line += tag;
    theLay = spIdx+1;
    // write the multiplier
    expArealArrayMultiplier(f, path, spIdx, datatype, multiIdx, 2, *mult);
  }
  else if (arealSpLay)
  {
    tag.Format("2 0 %d %d 1", nCells, spIdx);
    line += tag;
    theLay = spIdx+1;
    // write the multiplier
    expArealArrayMultiplier(f, path, spIdx, datatype, multiIdx, 1, *iMult);
  }
  else if (arealCountedLayer)
  {
    tag.Format("2 0 %d %d 1", nCells, theLay-1);
    line += tag;
    // write the multiplier
    if (mult)
      expArealArrayMultiplier(f, path, theLay-1, datatype, multiIdx, 1, *mult);
    else
      expArealArrayMultiplier(f, path, theLay-1, datatype, multiIdx, 1, *iMult);
  }
  else if (allCells)
  {
    tag.Format("3 0 1 %d %d %d 1", nCells*multiIdx, nCells, spIdx);
    line += tag;
    theLay = spIdx+1;
    // write the multiplier
    expArealArrayMultiplier(f, path, spIdx, datatype, multiIdx, 2, *mult);
  }
  else if (arealInt)
  {
    tag.Format("1 0 %d", nCells);
    line += tag;
    expArealArrayMultiplier(f, path, spIdx, datatype, multiIdx, 1, *iMult);
  }
  else if (arealReal)
  {
    tag.Format("1 0 %d", nCells);
    line += tag;
    expArealArrayMultiplier(f, path, spIdx, datatype, multiIdx, 1, *mult);
  }
  else
  {
    tag.Format("1 0 %d", nCells);
    line += tag;
  }

  tag.Format("%s%d", a_map.find(name)->second, theLay);
  if (multiIdx > -1)
  {
    CStr tmp;
    tmp.Format("_%d", multiIdx);
    tag += tmp;
  }

  if (data && mult && *mult == 1 && nCells > 0)
  {
    bool constArray(true);
    Real firstVal = data[0];
    for (int i=1; i<nCells && constArray; i++)
    {
      if (firstVal != data[i])
        constArray = false;
    }
    if (constArray)
    {
      if (zoneArray)
      {
        // zone array was changed from int to real above
        line.Format("CONSTANT %s", STR(firstVal, 0));
      }
      else
      {
        line.Format("CONSTANT %s", STR(firstVal));
      }
    }
  }
  else if (iData && iMult && *iMult == 1 && nCells > 0)
  {
    bool constArray(true);
    int firstVal = iData[0];
    for (int i=1; i<nCells && constArray; i++)
    {
      if (firstVal != iData[i])
        constArray = false;
    }
    if (constArray)
    {
      line.Format("CONSTANT %d", firstVal);
    }
  }
  GetTxtLineArrayMap(a_exp).insert(std::make_pair(tag, line));
} // expDataArray
//------------------------------------------------------------------------------
/// \brief internal to store the number of times an array with a given name
///        and stress period has been added to an h5 array
//------------------------------------------------------------------------------
static std::map<CStr, int>& iArrayCountMap (TxtExporter* a_exp)
{
  return a_exp->m_public->m_ArrayCountMap;
} // iArrayCountMap
//------------------------------------------------------------------------------
/// \brief get incremented count for the named array and stress period
//------------------------------------------------------------------------------
static int iGetIncrementedArrayIndex (const CStr &a_name,
                                      int a_sp,
                                      TxtExporter* a_exp)
{
  CStr nameAndSp;
  nameAndSp.Format("%s-%d", a_name.c_str(), a_sp);
  iArrayCountMap(a_exp)[nameAndSp] += 1;
  return iArrayCountMap(a_exp)[nameAndSp] - 1;
} // iGetIncrementedArrayCount
//------------------------------------------------------------------------------
/// \brief Gets the index of a multidimensional array like RCH or ET so we know
/// where to write the data
//------------------------------------------------------------------------------
static int GetMultiDimArrayIndex (const CStr &a_name,
                                  TxtExporter* a_exp,
                                  int a_sp /* = 0*/)
{
  int rval(0);
  if (a_name.CompareNoCase(ARR_EVT_RATE) == 0 ||
      a_name.CompareNoCase(ARR_ETS_RATE) == 0 ||
      a_name.CompareNoCase(ARR_UZF_ET) == 0)
    rval = 1;
  else if (a_name.CompareNoCase(ARR_EVT_EXT) == 0 ||
           a_name.CompareNoCase(ARR_ETS_EXT) == 0 ||
           a_name.CompareNoCase(ARR_UZF_EXT) == 0)
    rval = 2;
  else if (a_name.CompareNoCase(ARR_UZF_EXTWC) == 0)
    rval = 3;
  else if (a_name.CompareNoCase(ARR_ETS_PXDP) == 0 ||
           a_name.CompareNoCase(ARR_ETS_PETM) == 0 ||
           a_name.CompareNoCase(ARR_VDF_DENS) == 0 ||
           a_name.CompareNoCase(ARR_VDF_CONC) == 0 ||
           a_name.CompareNoCase(ARR_VSC_VSC) == 0 ||
           a_name.CompareNoCase(ARR_VSC_CONC) == 0)
    rval = iGetIncrementedArrayIndex(a_name, a_sp, a_exp);
  return rval;
} // GetMultiDimArrayIndex
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
/// \brief
//------------------------------------------------------------------------------
static void ExportArealParamList (std::vector<Param>& rchList,
                                  const char *packName,
                                  TxtExporter *a_exp)
{
  using std::vector;
  using std::ostringstream;
  //using std::ostream_iterator;
  CStr line;

  vector<Param>::iterator p = rchList.begin();
  for ( ; p != rchList.end(); ++p)
  {
    // we have to write out the parameter even if this info is not here
    // because a parameter may be used in another stress period and this
    // stuff gets written at the end of the first stress period
    //if (p->m_instStress.empty())
    //  continue;

    int nInst=1, nClst;
    if (!p->m_instNames.empty())
      nInst = static_cast<int>(p->m_instNames.size());
    nClst = static_cast<int>(p->m_clust.size());
    if (nClst % nInst)
      ASSERT(0);
    nClst = nClst / nInst;

    // [PARNAM PARTYP Parval NCLU]
    CStr pname(p->m_name);
    MfExportUtil::InsertSingleQuotesInName(pname);
    // use m_value in case there is a SEN file with a different value
    // than the parameter value specified in the package file
    line.Format("%s %s %s %d", pname, p->m_type, STR(p->m_value), 
                               nClst);
    if (p->m_instNames.size())
    {
      CStr instances;
      instances.Format(" INSTANCES %d", nInst);
      line += instances;
    }
    a_exp->WriteLineToFile(packName, line);

    int j, k;
    for (j=0; j<nInst; j++)
    {
      if (nInst > 1)
      {
        CStr iName = p->m_instNames[j];
        MfExportUtil::InsertSingleQuotesInName(iName);
        a_exp->WriteLineToFile(packName, iName);
      }
      for (k=0; k<nClst; k++)
      {
        int q(j*nClst); // offset for instances
        ostringstream os;
        for (size_t c=0; c<p->m_clust[q+k].m_iz.size(); c++)
        {
          os << p->m_clust[q+k].m_iz[c] << " ";
        }
        //copy(p->m_clust[q+k].m_iz.begin(), p->m_clust[q+k].m_iz.end(),
        //     ostream_iterator<int>(os, " "));
        // Mltarr Zonarr IZ
        line.Format("%s %s %s", p->m_clust[q+k].m_mlt,
                                p->m_clust[q+k].m_zon,
                                os.str().c_str());
        a_exp->WriteLineToFile(packName, line);
      }
    }
  }
} // ExportArealParamList
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void ExportArealStressParam (std::vector<Param>& rchList,
                                    int a_sp,
                                    const char *packName,
                                    TxtExporter *a_exp)
{
  using std::vector;
  vector<Param>::iterator p = rchList.begin();
  for ( ; p != rchList.end(); ++p)
  {
    std::map<CStr, vector<int> >::iterator i = p->m_instStress.begin();
    for ( ; i != p->m_instStress.end(); ++i)
    {
      CStr name(i->first), nameToWrite;
      // find the correct instance name with the correct case that was
      // read when we read the parameter definition
      for (size_t j=0; nameToWrite.IsEmpty() && j<p->m_instNames.size(); j++)
      {
        if (name.CompareNoCase(p->m_instNames.at(j)) == 0)
          nameToWrite = p->m_instNames.at(j);
      }
      if (nameToWrite.IsEmpty())
      {
        if (!p->m_instNames.empty())
          ASSERT(0);
        nameToWrite = name;
      }
      vector<int>& spList = i->second;
      if (std::find(spList.begin(), spList.end(), a_sp) != spList.end())
      {
        CStr pname(p->m_name);
        MfExportUtil::InsertSingleQuotesInName(pname);
        MfExportUtil::InsertSingleQuotesInName(nameToWrite);
        if (p->m_instNames.size() > 0)
          name.Format("%s %s", pname, nameToWrite);
        else
          name = p->m_name;
        a_exp->WriteLineToFile(packName, name);
      }
    }
  }
} // ExportArealStressParam
//------------------------------------------------------------------------------
/// \brief Counts the number of parameters that are used in stress periods
//------------------------------------------------------------------------------
//static int CountArealParWithInstances (std::vector<Param>& a_parList)
//{
//  int cnt(0);
//  for (size_t i=0; i<a_parList.size(); i++)
//  {
//    if (!a_parList[i].m_instStress.empty())
//      cnt++;
//  }
//  return cnt;
//} // CountArealParWithInstances
//------------------------------------------------------------------------------
/// \brief Exports the recharge package
//------------------------------------------------------------------------------
static void expRecharge (MfPackage *a_p,
                         const int a_sp,
                         TxtExporter *a_exp)
{
  using std::vector;
  using std::copy;
  using std::find;
  using std::ostringstream;
  //using std::ostream_iterator;
  const int *nrchop(0), *inirch(0), *inrech(0);

  if (!a_p->GetField(Packages::RCHpack::NRCHOP, &nrchop) || !nrchop ||
      !a_p->GetField(Packages::RCHpack::INIRCH, &inirch) || !inirch ||
      !a_p->GetField(Packages::RCHpack::INRECH, &inrech) || !inrech)
    return;

  // get parameters with RCH type
  std::vector<Param> rchList = MfExportUtil::GetParamsOfType("RCH");
  // can't just count those with instances because the parameter may be
  // used in a later stress period and we write this at the end of the
  // first stress period
  //int nprch(CountArealParWithInstances(rchList));
  int nprch = (int)(rchList.size());

  CStr line;
  if (a_sp == 1)
  {
    // PARAMETER NPRCH
    if (nprch > 0)
    {
      line.Format("PARAMETER %d", nprch);
      a_exp->WriteLineToFile(Packages::RCH, line);
    }

    // NRCHOP IRCHCB
    line.Format("%d 40", *nrchop);
    a_exp->WriteLineToFile(Packages::RCH, line);

    ExportArealParamList(rchList, Packages::RCH, a_exp);
  }

  // INRCH INIRCH
  CStr inRchStr;
  if (nprch > 0)
  {
    int numParams = *inrech;
    if (numParams > nprch)
      numParams = nprch;
    inRchStr.Format("%d", numParams);
  }
  else
    inRchStr.Format("%d", *inrech);
  if (*nrchop == 2)
    line.Format("%s %d", inRchStr.c_str(), *inirch);
  else
    line = inRchStr;
  a_exp->WriteLineToFile(Packages::RCH, line);
  // write the use last data to the file
  {
    std::vector<int> vDat(2,0);
    vDat[0] = *inrech < 0 ? 1 : 0;
    if (*nrchop == 2)
      vDat[1] = *inirch < 0 ? 1 : 0;
    expUseLastAreal(a_exp->GetBaseFileName(), "Recharge/", a_sp, vDat);
  }

  CStr tag;
  tag.Format("Recharge/07. Property%d_0", a_sp);

  if (*inrech > -1 && nprch == 0)
  {
    // RECH
    a_exp->WriteLineToFile(Packages::RCH, GetTxtLineArrayMap(a_exp)[tag]);
  }
  else if (nprch > 0)
  {
    // Pname [IRCHPF]
    ExportArealStressParam(rchList, a_sp, Packages::RCH, a_exp);
  }

  // IRCH
  if (*nrchop  == 2)
  {
    tag.Format("Recharge/09. Layer%d", a_sp);
    if (*inirch > -1)
      a_exp->WriteLineToFile(Packages::RCH, GetTxtLineArrayMap(a_exp)[tag]);
  }
} // expRecharge
//------------------------------------------------------------------------------
/// \brief Exports the EVT and ETS package
//------------------------------------------------------------------------------
static void expET (MfPackage *a_p,
                   const int a_sp,
                   TxtExporter *a_exp,
                   const char *a_baseName)
{
  using std::vector;
  using std::copy;
  using std::find;
  using std::ostringstream;
  //using std::ostream_iterator;
  CStr packName(a_p->PackageName());
  const int *nevtop(0), *insurf(0), *inevtr(0), *inexdp(0), *inievt(0),
            *netseg(0), *insgdf(0);
  if (!a_p->GetField(Packages::EVTpack::NEVTOP, &nevtop) || !nevtop ||
      !a_p->GetField(Packages::EVTpack::INSURF, &insurf) || !insurf ||
      !a_p->GetField(Packages::EVTpack::INEVTR, &inevtr) || !inevtr ||
      !a_p->GetField(Packages::EVTpack::INEXDP, &inexdp) || !inexdp ||
      !a_p->GetField(Packages::EVTpack::INIEVT, &inievt) || !inievt ||
      !a_p->GetField(Packages::EVTpack::NETSEG, &netseg) || !netseg ||
      !a_p->GetField(Packages::EVTpack::INSGDF, &insgdf) || !insgdf)
    return;

  // get parameters with EVT type
  std::vector<Param> evtList = MfExportUtil::GetParamsOfType(packName);
  // can't just count those with instances because the parameter may be
  // used in a later stress period and we write this at the end of the
  // first stress period
  int npevt = (int)(evtList.size());
  const char *h5Prefix;

  if (packName == "EVT")
    h5Prefix = "ET/";
  else
    h5Prefix = "ETS/";

  CStr line;
  if (a_sp == 1)
  {
    // PARAMETER NPEVT (EVT Only)
    if (npevt > 0 && packName == "EVT")
    {
      line.Format("PARAMETER %d", npevt);
      a_exp->WriteLineToFile(packName, line);
    }

    // NEVTOP IEVTCB / NETSOP IETSCB NPETS NETSEG
    line.Format("%d 40", *nevtop);

    if (packName == "ETS")
    {
      CStr etsExtra;
      etsExtra.Format(" %d %d", npevt, *netseg);
      line += etsExtra;
    }
    a_exp->WriteLineToFile(packName, line);

    ExportArealParamList(evtList, packName, a_exp);

    // writing the NETSEG
    if (packName == "ETS")
    {
      CStr f(a_exp->GetBaseFileName()), path;
      f += ".h5";
      path.Format("%s%s", h5Prefix, MFBC_NETSEG);
      H5DataSetWriterSetup s(f, path, H5T_NATIVE_INT, 1);
      std::vector<hsize_t> start(1, 0), n2write(1, 1);
      H5DSWriterDimInfo dim(start, n2write);
      H5DataSetWriter w(&s);
      w.SetDimInfoForWriting(&dim);
      w.WriteData(netseg, 1);
    }
  }

  // INSURF INEVTR INEXDP INIEVT / INETSS INETSR INETSX [INIETS [INSGDF]]
  line.Format("%d %d %d", *insurf, *inevtr, *inexdp);
  if (*nevtop == 2 || *netseg > 1)
  {
    CStr tmp;
    tmp.Format(" %d", *inievt);
    line += tmp;
  }

  if (*netseg > 1)
  {
    CStr tmp;
    tmp.Format(" %d", *insgdf);
    line += tmp;
  }
  a_exp->WriteLineToFile(packName, line);
  // write the use last data to the file
  {
    std::vector<int> vDat(4,0);
    vDat[0] = *insurf < 0 ? 1 : 0;
    vDat[1] = *inevtr < 0 ? 1 : 0;
    vDat[2] = *inexdp < 0 ? 1 : 0;
    if (*nevtop == 2)
      vDat[3] = *inievt < 0 ? 1: 0;
    if (packName == "ETS")
    {
      int insgdfOut = 0;
      if (*netseg > 1)
        insgdfOut = *insgdf;
      vDat.push_back(insgdfOut < 0 ? 1 : 0);
    }
    expUseLastAreal(a_baseName, h5Prefix, a_sp, vDat);
  }

  CStr tag;
  tag.Format("%s07. Property%d_0", h5Prefix, a_sp);
  if (*insurf > -1)
  {
    // SURF / ETSS
    a_exp->WriteLineToFile(packName, GetTxtLineArrayMap(a_exp)[tag]);
  }

  tag.Format("%s07. Property%d_1", h5Prefix, a_sp);
  if (*inevtr > -1 && npevt == 0)
  {
    // EVTR / ETSR
    a_exp->WriteLineToFile(packName, GetTxtLineArrayMap(a_exp)[tag]);
  }
  else if (npevt > 0)
  {
    // Pname [IEVTPF]
    ExportArealStressParam(evtList, a_sp, packName, a_exp);
  }

  // EXDP / ETSX
  tag.Format("%s07. Property%d_2", h5Prefix, a_sp);
  if (*inexdp > -1)
    a_exp->WriteLineToFile(packName, GetTxtLineArrayMap(a_exp)[tag]);
  
  // IEVT / IETS
  if (*nevtop  == 2)
  {
    tag.Format("%s09. Layer%d", h5Prefix, a_sp);
    if (*inievt > -1)
      a_exp->WriteLineToFile(packName, GetTxtLineArrayMap(a_exp)[tag]);
  }

  if (*insgdf >= 0)
  {
    for (int i = 0; i < *netseg - 1; ++i)
    {
      // PXDP (ETS Only)
      tag.Format("ETS/16. Ext Depth%d_%d", a_sp, i);
      line = GetTxtLineArrayMap(a_exp)[tag];
      a_exp->WriteLineToFile(packName, line);

      // PETM (ETS Only)
      tag.Format("ETS/18. Evap Rate%d_%d", a_sp, i);
      line = GetTxtLineArrayMap(a_exp)[tag];
      a_exp->WriteLineToFile(packName, line);
    }
  }

} // expET
//------------------------------------------------------------------------------
/// \brief Exports part of the output control
//------------------------------------------------------------------------------
static void expOC (MfPackage *a_p,
                   TxtExporter *a_exp)
{
  using namespace Packages;
  const int *ihedfm, *iddnfm, *ihedun, *iddnun, *ibdopt, *iauxsv;
  const char* chedfm;
  if (a_p->GetField(OCpack::IHEDFM, &ihedfm) && ihedfm &&
      a_p->GetField(OCpack::IDDNFM, &iddnfm) && iddnfm &&
      a_p->GetField(OCpack::CHEDFM, &chedfm) && chedfm &&
      a_p->GetField(OCpack::IHEDUN, &ihedun) && ihedun &&
      a_p->GetField(OCpack::IDDNUN, &iddnun) && iddnun &&
      a_p->GetField(OCpack::IBDOPT, &ibdopt) && ibdopt &&
      a_p->GetField(OCpack::IAUXSV, &iauxsv) && iauxsv)
  {
    CStr aStr;

    if (*ihedfm != 0)
    {
      aStr.Format("HEAD PRINT FORMAT %d", *ihedfm);
      a_exp->WriteLineToFile(OC, aStr);
    }

    if (chedfm && strlen(chedfm) != 0)
    {
      aStr.Format("HEAD SAVE FORMAT %s", chedfm);
      a_exp->WriteLineToFile(OC, aStr);
    }

    if (*ihedun > 0)
    {
      aStr.Format("HEAD SAVE UNIT %d", *ihedun);
      a_exp->WriteLineToFile(OC, aStr);
    }

    if (*iddnfm != 0)
    {
      aStr.Format("DRAWDOWN PRINT FORMAT %d", *iddnfm);
      a_exp->WriteLineToFile(OC, aStr);
    }

    if (*iddnun > 0)
    {
      aStr.Format("DRAWDOWN SAVE UNIT %d", *iddnun);
      a_exp->WriteLineToFile(OC, aStr);
    }

    if (*ibdopt == 2 && *iauxsv == 0)
    {
      a_exp->WriteLineToFile(OC, "COMPACT BUDGET");
    }
    else if (*ibdopt == 2 && *iauxsv == 1)
    {
      a_exp->WriteLineToFile(OC, "COMPACT BUDGET AUXILIARY");
    }
  }
} // expOC
//------------------------------------------------------------------------------
/// \brief Exports part of the output control
//------------------------------------------------------------------------------
static void expOCT (MfPackage *a_p,
                    TxtExporter *a_exp)
{
  using namespace Packages;
  const int *spid, *tsnum, *ihddfl, *ibudfl, *icbcfl, *hdpr, *ddpr, *hdsv, 
            *ddsv;
  if (a_p->GetField(OCTpack::SPID, &spid) && spid &&
      a_p->GetField(OCTpack::TSNum, &tsnum) && tsnum &&
      a_p->GetField(OCTpack::IHDDFL, &ihddfl) && ihddfl &&
      a_p->GetField(OCTpack::IBUDFL, &ibudfl) && ibudfl &&
      a_p->GetField(OCTpack::ICBCFL, &icbcfl) && icbcfl &&
      a_p->GetField(OCTpack::Hdpr, &hdpr) && hdpr &&
      a_p->GetField(OCTpack::Ddpr, &ddpr) && ddpr &&
      a_p->GetField(OCTpack::Hdsv, &hdsv) && hdsv &&
      a_p->GetField(OCTpack::Ddsv, &ddsv) && ddsv)
  {
    CStr aStr;
    aStr.Format("PERIOD %d STEP %d", *spid, *tsnum);
    a_exp->WriteLineToFile(OC, aStr);
    if (*ibudfl != 0)
      a_exp->WriteLineToFile(OC, "   PRINT BUDGET");
    if (*icbcfl != 0)
      a_exp->WriteLineToFile(OC, "   SAVE BUDGET");
    if (*hdpr != 0)
      a_exp->WriteLineToFile(OC, "   PRINT HEAD");
    if (*ddpr != 0)
      a_exp->WriteLineToFile(OC, "   PRINT DRAWDOWN");
    if (*hdsv != 0)
      a_exp->WriteLineToFile(OC, "   SAVE HEAD");
    if (*ddsv != 0)
      a_exp->WriteLineToFile(OC, "   SAVE DRAWDOWN");
  }
} // expOCT
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
/// \brief Exports for parameter value file
//------------------------------------------------------------------------------
static void expPVAL (MfPackage *a_p,
                     TxtExporter *a_exp)
{
  using namespace MfData::Packages;
  const int *nplist;
  const char *parnam;
  const Real *b;
  CStr aCStr;

  if (a_p->GetField(PVALpack::NPVAL, &nplist) && nplist &&
      a_p->GetField(PVALpack::PARNAM, &parnam) && parnam &&
      a_p->GetField(PVALpack::B, &b) && b)
  {
    aCStr.Format("%d", *nplist);
    a_exp->WriteLineToFile(PVAL, aCStr);
    for (int i = 0; i < *nplist; ++i)
    {
      CStr parnamStr(parnam + i*10, 10);
      parnamStr.Trim();
      MfExportUtil::InsertSingleQuotesInName(parnamStr);
      aCStr.Format("%s %s",
                   parnamStr.c_str(), STR(b[i]));
      a_exp->WriteLineToFile(PVAL, aCStr);
    }
  }
}
//------------------------------------------------------------------------------
/// \brief Exports for parameter estimation process file
//------------------------------------------------------------------------------
static void expPES (MfPackage *a_p,
                    TxtExporter *a_exp)
{
  using namespace MfData::Packages;
  const int *maxiter, *ibeflg, *iycflg, *iostar, *nopt,
            *nfit, *iap, *iprcov, *iprint, *lprint, *lastx;
  const Real *tol, *sosc, *sosr, *maxchange, *rmar, *rmarm, *csa, *fconv;

  if (a_p->GetField(PESpack::ITMXP, &maxiter) && maxiter &&
      a_p->GetField(PESpack::DMAX, &maxchange) && maxchange &&
      a_p->GetField(PESpack::RTOL, &tol) && tol &&
      a_p->GetField(PESpack::SOSC, &sosc) && sosc &&
      a_p->GetField(PESpack::IBEFLG, &ibeflg) && ibeflg &&
      a_p->GetField(PESpack::IYCFLG, &iycflg) && iycflg &&
      a_p->GetField(PESpack::IOSTAR, &iostar) && iostar &&
      a_p->GetField(PESpack::NOPT, &nopt) && nopt &&
      a_p->GetField(PESpack::NFIT, &nfit) && nfit &&
      a_p->GetField(PESpack::SOSR, &sosr) && sosr &&
      a_p->GetField(PESpack::RMAR, &rmar) && rmar &&
      a_p->GetField(PESpack::RMARM, &rmarm) && rmarm &&
      a_p->GetField(PESpack::IAP, &iap) && iap &&
      a_p->GetField(PESpack::IPRC, &iprcov) && iprcov &&
      a_p->GetField(PESpack::IPRINT, &iprint) && iprint &&
      a_p->GetField(PESpack::LPRINT, &lprint) && lprint &&
      a_p->GetField(PESpack::CSA, &csa) && csa &&
      a_p->GetField(PESpack::FCONV, &fconv) && fconv &&
      a_p->GetField(PESpack::LASTX, &lastx) && lastx)
  {
    CStr str;
    str.Format("%d %s %s %s ", *maxiter, STR(*maxchange), STR(*tol), STR(*sosc));
    a_exp->WriteLineToFile(PES, str);

    str.Format("%d %d %d %d %d %s %s %s %d ", *ibeflg, *iycflg, *iostar, *nopt,
             *nfit, STR(*sosr), STR(*rmar), STR(*rmarm), *iap);
    a_exp->WriteLineToFile(PES, str);
    
    str.Format("%d %d %d ", *iprcov, *iprint, *lprint);
    a_exp->WriteLineToFile(PES, str);
    
    str.Format("%s %s %d ", STR(*csa), STR(*fconv), *lastx);
    a_exp->WriteLineToFile(PES, str);
    
    a_exp->WriteLineToFile(PES, "0 0 0 ");
  }
} // expPES
//------------------------------------------------------------------------------
/// \brief Exports the HUF package
//------------------------------------------------------------------------------
static bool iWriteHufLine1to5 (MfPackage *a_p,
                               int a_nlay,
                               TxtExporter *a_exp)
{
  const int *IHUFCB(0), *NHUF(0), *NPHUF(0), *IOHUFHEADS(0), *IOHUFFLOWS(0),
            *LTHUF(0), *LAYWT(0);
  const Real *HDRY(0);

  if (!a_p->GetField(Packages::HUFPack::HDRY, &HDRY) || !HDRY ||
      !a_p->GetField(Packages::HUFPack::IHUFCB, &IHUFCB) || !IHUFCB ||
      !a_p->GetField(Packages::HUFPack::NHUF, &NHUF) || !NHUF ||
      !a_p->GetField(Packages::HUFPack::NPHUF, &NPHUF) || !NPHUF ||
      !a_p->GetField(Packages::HUFPack::IOHUFHEADS, &IOHUFHEADS) || !IOHUFHEADS ||
      !a_p->GetField(Packages::HUFPack::IOHUFFLOWS, &IOHUFFLOWS) || !IOHUFFLOWS ||
      !a_p->GetField(Packages::HUFPack::LTHUF, &LTHUF) || !LTHUF ||
      !a_p->GetField(Packages::HUFPack::LAYWT, &LAYWT) || !LAYWT
     )
   return false;

  // line 1 IHUFCB HDRY NHUF NPHUF IOHUFHEADS IOHUFFLOWS
  CStr line;
  line.Format("%d %s %d %d %d %d",
              *IHUFCB,
              STR(*HDRY),
              *NHUF,
              *NPHUF,
              *IOHUFHEADS,
              *IOHUFFLOWS);
  a_exp->WriteLineToFile(Packages::HUF, line);

  // line 2 LTHUF
  line = "";
  CStr tmp;
  int i;
  for (i=0; i<a_nlay; i++)
  {
    tmp.Format("%d ", LTHUF[i]);
    line += tmp;
  }
  line.Delete(line.GetLength()-1);
  a_exp->WriteLineToFile(Packages::HUF, line);

  // line LAYWT
  bool wet(false);
  line = "";
  for (i=0; i<a_nlay; i++)
  {
    tmp.Format("%d ", LAYWT[i]);
    if (LAYWT[i] == 1)
      wet = true;
    line += tmp;
  }
  line.Delete(line.GetLength()-1);
  a_exp->WriteLineToFile(Packages::HUF, line);

  if (wet)
  {
    const int *IWETIT(0), *IHDWET(0);
    const Real *WETFCT(0);
    if (!a_p->GetField(Packages::HUFPack::WETFCT, &WETFCT) || !WETFCT ||
        !a_p->GetField(Packages::HUFPack::IWETIT, &IWETIT) || !IWETIT ||
        !a_p->GetField(Packages::HUFPack::IHDWET, &IHDWET) || !IHDWET
       )
     return false;

    // line 4 WETFCT IWETIT IHDWET
    line.Format("%s %d %d", STR(*WETFCT), *IWETIT, *IHDWET);
    a_exp->WriteLineToFile(Packages::HUF, line);

    // write the arrays
    std::map<CStr,CStr> &m(GetTxtLineArrayMap(a_exp));
    CStr tag;
    // line 5 WETDRY(NCOL,NROW) - U2DREL
    for (i=0; i<a_nlay; i++)
    {
      if (LAYWT[i] == 0)
        continue;
      tag.Format("WET%d", i+1);
      if (m.find(tag) == m.end())
        return false;
      a_exp->WriteLineToFile(Packages::HUF, m[tag]);
    }
  }
  return true;
} // iWriteHufLine1to5
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static bool iWriteHGUs (MfPackage *a_p,
                        TxtExporter *a_exp)
{
  const int *NHUF(0);
  const char *HGUNAM(0);
  const Real *HGUHANI(0), *HGUVANI(0);

  if (!a_p->GetField(Packages::HUFPack::NHUF, &NHUF) || !NHUF
      || !a_p->GetField(Packages::HUFPack::HGUNAM, &HGUNAM) || !HGUNAM
      || !a_p->GetField(Packages::HUFPack::HGUHANI, &HGUHANI) || !HGUHANI
      || !a_p->GetField(Packages::HUFPack::HGUVANI, &HGUVANI) || !HGUVANI
     )
   return false;

  // get the HGU names
  std::vector<CStr> names;
  char c[11] = {0};
  int i, j;
  for (i=0; i<*NHUF; i++)
  {
    for (j=0; j<10; j++)
      c[j] = HGUNAM[(i*10)+j];
    names.push_back(c);
    names.back().Trim();
  }

  // write the arrays
  CStr tag;
  std::map<CStr,CStr> &m(GetTxtLineArrayMap(a_exp));
  for (i=0; i<*NHUF; i++)
  {
    a_exp->WriteLineToFile(Packages::HUF, names.at(i));
    tag.Format("top_%s1", names.at(i));
    if (m.find(tag) == m.end())
      return false;
    a_exp->WriteLineToFile(Packages::HUF, m[tag]);
    tag.Format("thick_%s1", names.at(i));
    if (m.find(tag) == m.end())
      return false;
    a_exp->WriteLineToFile(Packages::HUF, m[tag]);
  }
  // write the HGUHANI, VANI terms
  // check for "all" case
  bool all(true);
  for (i=1; i<*NHUF && all; i++)
  {
    if (HGUHANI[0] != HGUHANI[i])
      all = false;
    if (HGUVANI[0] != HGUVANI[i])
      all = false;
  }

  for (i=0; i<*NHUF; i++)
  {
    if (all)
    {
      tag.Format("ALL %s %s", STR(HGUHANI[i]), STR(HGUVANI[i]));
      i = *NHUF;
    }
    else
      tag.Format("%s %s %s", names.at(i), STR(HGUHANI[i]), STR(HGUVANI[i]));
    a_exp->WriteLineToFile(Packages::HUF, tag);
  }
  return true;
} // iWriteHGUs
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void iWriteHGUPar (TxtExporter *a_exp)
{
  Param      p;
  ParamList *list;
  Parameters::GetParameterList(&list);

  std::set<CStr> types = MfExportUtil::HufParamTypes();
  CStr pType, line, str;

  for (size_t i=0; i<list->Size(); i++)
  {
    list->At(i, &p);
    pType = p.m_type;
    pType.ToLower();
    if (types.find(pType) != types.end())
    {
      CStr pname(p.m_name);
      MfExportUtil::InsertSingleQuotesInName(pname);
      line.Format("%s %s %s %d",
                  pname,
                  p.m_type,
                  STR(p.m_value), // use value in case the model had a sen file
                                  // that specified a different value than the huf
                  p.m_clust.size());
      a_exp->WriteLineToFile(Packages::HUF, line);
      for (size_t j=0; j<p.m_clust.size(); j++)
      {
        line.Format("%s %s %s ",
                    p.m_clust[j].m_hgu,
                    p.m_clust[j].m_mlt,
                    p.m_clust[j].m_zon);
        for (size_t k=0; k<p.m_clust[j].m_iz.size(); k++)
        {
          str.Format("%d ", p.m_clust[j].m_iz[k]);
          line += str;
        }
        a_exp->WriteLineToFile(Packages::HUF, line);
      }
    }
  }
} // iWriteHGUPar
//------------------------------------------------------------------------------
/// \brief Exports the HUF package
//------------------------------------------------------------------------------
static void expHUF (MfPackage *a_p,
                    int a_nlay,
                    TxtExporter *a_exp)
{
  if (!a_p)
  {
    printf("NULL package pointer: HUF. Aborting.\n");
    return;
  }
  iWriteHufLine1to5(a_p, a_nlay, a_exp);
  //printf("%s: %d\n", __FILE__, __LINE__);
  iWriteHGUs(a_p, a_exp);
  //printf("%s: %d\n", __FILE__, __LINE__);
  iWriteHGUPar(a_exp);
  //printf("%s: %d\n", __FILE__, __LINE__);
} // expHUF
//------------------------------------------------------------------------------
/// \brief Exports the stream package
//------------------------------------------------------------------------------
static void expSTR (MfPackage *a_p,
                    const int a_sp,
                    const int a_nRow,
                    const int a_nCol,
                    TxtExporter *a_exp)
{
  const int *istrpb(0), *nss(0), *ntrib(0), *ndiv(0), *icalc(0), *istcb1(0),
            *istcb2(0), *itmp(0), *irdflg(0), *iptflg(0), *istrm(0),
            *nstrem(0), *mxstrm(0), *itrbar(0), *idivar(0);
  const Real *constv, *strm(0);
  if (a_p->GetField(Packages::STRpack::MXACTS, &istrpb) && istrpb &&
      a_p->GetField(Packages::STRpack::NSS, &nss) && nss &&
      a_p->GetField(Packages::STRpack::NTRIB, &ntrib) && ntrib &&
      a_p->GetField(Packages::STRpack::NDIV, &ndiv) && ndiv &&
      a_p->GetField(Packages::STRpack::ICALC, &icalc) && icalc &&
      a_p->GetField(Packages::STRpack::CONSTV, &constv) && constv &&
      a_p->GetField(Packages::STRpack::ISTCB1, &istcb1) && istcb1 &&
      a_p->GetField(Packages::STRpack::ISTCB2, &istcb2) && istcb2 &&
      a_p->GetField(Packages::STRpack::ITMP, &itmp) && itmp &&
      a_p->GetField(Packages::STRpack::IRDFLG, &irdflg) && irdflg &&
      a_p->GetField(Packages::STRpack::IPTFLG, &iptflg) && iptflg &&
      a_p->GetField(Packages::STRpack::STRM, &strm) && strm &&
      a_p->GetField(Packages::STRpack::ISTRM, &istrm) && istrm &&
      a_p->GetField(Packages::STRpack::NSTREM, &nstrem) && nstrem &&
      a_p->GetField(Packages::STRpack::MXSTRM, &mxstrm) && mxstrm &&
      a_p->GetField(Packages::STRpack::ITRBAR, &itrbar) && itrbar &&
      a_p->GetField(Packages::STRpack::IDIVAR, &idivar) && idivar)
  {
    const char         *a_fileType(Packages::STRSP);
    CStr               type("Stream"), line;
    CellIdToIJK        grid(a_nRow, a_nCol);
    std::vector<int>   &cellids(iGetBcCellIds(a_exp)[type]),
                       &vIface(iGetBcIface(a_exp)[type]),
                       &vCellgrp(iGetBcCellGrp(a_exp)[type]);
    const int          *nBcs(0), *nFields(0), *maxBc(0);
    const Real         *data(0);
    int                i, j, ifaceIdx(-1), nBcFields, ci, cj, ck, cellId;

    // Some of the following code was grabbed from expListPack but has been
    // left to be merged back into expListPack until later when expListPack is
    // tested better when other boundary condition package exports are added.
    // The changes and additions are noted below.
    int streamFields(11);
    maxBc = mxstrm;
    data = strm;
    nBcs = nstrem;
    nFields = &streamFields;
    
    CStr f(a_exp->GetBaseFileName()), path;
    f += ".h5";

    if (a_sp == 1)
    {
      // changed from expListPack
      line.Format("%10d%10d%10d%10d%10d%10s%10d%10d", *istrpb - 1, *nss, *ntrib, 
                  *ndiv, *icalc, STR(*constv), *istcb1, *istcb2);
      a_exp->WriteLineToFile(a_fileType, "#GMS_HDF5_01");
      a_exp->WriteLineToFile(a_fileType, line);
      // end change

      {
        path.Format("%s/%s", type, MFBC_NSEG);
        H5DataSetWriterSetup s(f, path, H5T_NATIVE_INT, 1);
        std::vector<hsize_t> start(1, 0), n2write(1,1);
        H5DSWriterDimInfo dim(start, n2write);
        H5DataSetWriter w(&s);
        w.SetDimInfoForWriting(&dim);
        w.WriteData(nss, 1);
      }
    }

    {
      path.Format("%s/%s", type, MFBC_USELAST);
      H5DataSetWriterSetup s(f, path, H5T_NATIVE_INT, 1);
      std::vector<hsize_t> start(1, a_sp-1), n2write(1,1);
      H5DSWriterDimInfo dim(start, n2write);
      H5DataSetWriter w(&s);
      w.SetDimInfoForWriting(&dim);
      int itmpToWrite = *itmp >= 0 ? 0 : -1;
      w.WriteData(&itmpToWrite, 1);
    }

    line.Format("%10d%10d%10d", *itmp > 0 ? *nstrem : *itmp, 0, 0);
    a_exp->WriteLineToFile(a_fileType, line);
    // we can't return without updating the rest of the data
    //if (*itmp < 0)
    //  return;

    // first create a vector of indices so we can size the data array
    int maxIdx(-1);
    std::vector<int> idxs;
    idxs.reserve(*nBcs);
    for (i=0; i<*nBcs; i++)
    {
      // changed from expListPack
      ck = istrm[i*(5)+0];
      ci = istrm[i*(5)+1];
      cj = istrm[i*(5)+2];
      // end change

      cellId = grid.IdFromIJK(ci, cj, ck);
      idxs.push_back(iGetBcIndex(type, cellId, a_sp, cellids, vIface, vCellgrp, a_exp));
      if (idxs.back() > maxIdx)
        maxIdx = idxs.back();
      // get the iface values if they exist
      // TODO
      if (ifaceIdx != -1)
      {
        vIface.at(idxs.back()) = static_cast<int>(data[i*(*nFields)+ifaceIdx]);
      }
    }

    // size the BC data vector
    CAR_DBL2D bcData;
    iSizeBcDataArray(type, maxIdx, bcData);
    // fill in the bcData

    // changed from expListPack
    nBcFields = 7;
    // end change

    for (i=0; i<*nBcs; i++)
    {
      for (j=0; j<nBcFields; j++)
      {
        // changed from expListPack
        bcData.at(j, idxs.at(i)) = static_cast<double>(data[i*(*nFields)+1+j]);
        // end change
      }
    }

    // added to expListPack code
    for (i=0; i<*nBcs; i++)
    { // CONDFACT
      bcData.at(7, i) = 1.0;
    }
    // end added

    // handle any stream parameters
    {
      ParamList *list(0);
      Parameters::GetParameterList(&list);
      Param p;
      for (size_t ii=0; ii<list->Size(); ii++)
      {
        list->At(ii, &p);
        if (p.m_type == "STR")
        { // move the current conductance value to the condfact
          // and set conductance to the key value
          int stop = p.m_str_start + p.m_str_nbc;
          for (i=p.m_str_start; i<stop; i++)
          {
            bcData.at(7, i) = bcData.at(1, i);
            bcData.at(1, i) = p.m_key;
          }
          p.m_str_start = -1;
          p.m_str_nbc = -1;
          list->UpdateParameter(&p);
        }
      }
    }

    // writing the bc "Property"
    {
      path.Format("%s/%s", type, MFBC_DATA);
      H5DataSetWriterSetup s(f, path, H5T_NATIVE_DOUBLE, 3);
      std::vector<hsize_t> start(3, 0), n2write(3,1);
      n2write[0] = bcData.GetSize1();
      n2write[1] = bcData.GetSize2();
      start[2] = a_sp - 1;
      H5DSWriterDimInfo dim(start, n2write);
      H5DataSetWriter w(&s);
      w.SetDimInfoForWriting(&dim);
      w.WriteData(&bcData.at(0,0),
                  static_cast<size_t>(bcData.GetSize1()*bcData.GetSize2()));
    }
    // writing the "numbcs"
    path.Format("%s/%s", type, MFBC_NUMBC);
    WriteSingleH5IntValue(f, path, (int)cellids.size());

    // writing the bc "cellids"
    {
      path.Format("%s/%s", type, MFBC_CELLIDS);
      H5DataSetWriterSetup s(f, path, H5T_NATIVE_INT, 1);
      std::vector<hsize_t> start(1, 0), n2write(1, cellids.size());
      H5DSWriterDimInfo dim(start, n2write);
      H5DataSetWriter w(&s);
      w.SetDimInfoForWriting(&dim);
      w.WriteData(&cellids.at(0), cellids.size());
    }
    // writing the bc "iface"
    {
      path.Format("%s/%s", type, MFBC_IFACE);
      H5DataSetWriterSetup s(f, path, H5T_NATIVE_INT, 1);
      std::vector<hsize_t> start(1, 0), n2write(1, vIface.size());
      H5DSWriterDimInfo dim(start, n2write);
      H5DataSetWriter w(&s);
      w.SetDimInfoForWriting(&dim);
      w.WriteData(&vIface.at(0), vIface.size());
    }
    // write name
    std::vector<char> vC(cellids.size(), 0);
    {
      path.Format("%s/%s", type, MFBC_NAME);
      H5DataSetWriterSetup s(f, path, H5T_NATIVE_CHAR, 1);
      std::vector<hsize_t> start(1, 0), n2write(1, vC.size());
      H5DSWriterDimInfo dim(start, n2write);
      H5DataSetWriter w(&s);
      w.SetDimInfoForWriting(&dim);
      w.WriteData(&vC.at(0), vC.size());
    }
    // write map id
    {
      path.Format("%s/%s", type, MFBC_MAPIDSTR);
      H5DataSetWriterSetup s(f, path, H5T_NATIVE_CHAR, 1);
      std::vector<hsize_t> start(1, 0), n2write(1, vC.size());
      H5DSWriterDimInfo dim(start, n2write);
      H5DataSetWriter w(&s);
      w.SetDimInfoForWriting(&dim);
      w.WriteData(&vC.at(0), vC.size());
    }

    // added to expListPack
    // write the str reach segment ids
    if (a_sp == 1)
    {
      std::vector<int> reachIds;
      idxs.reserve(*nBcs);
      for (i=0; i<*nBcs; i++)
      {
        reachIds.push_back(istrm[i*5+3]);
      }
      path.Format("%s/%s", type, MFBC_STRSEGID);
      H5DataSetWriterSetup s(f, path, H5T_NATIVE_INT, 1);
      std::vector<hsize_t> start(1, 0), n2write(1, reachIds.size());
      H5DSWriterDimInfo dim(start, n2write);
      H5DataSetWriter w(&s);
      w.SetDimInfoForWriting(&dim);
      w.WriteData(&reachIds.at(0), reachIds.size());
    }
    // write the segment ids and flow
    {
      std::vector<int> segmentIds;
      std::vector<Real> flow;
      int lastSegmentId = istrm[3] - 1;
      segmentIds.assign(*nss, 0);
      flow.assign(*nss, 0);
      for (i=0; i<*nss; ++i) segmentIds[i] = i+1;
      for (i = 0; i < *nBcs; ++i)
      {
        int segmentId = istrm[i*5+3];
        if (segmentId != lastSegmentId)
        {
          flow[segmentId-1] = strm[i*(*nFields)+0];
          lastSegmentId = segmentId;
        }
      }

      if (a_sp == 1)
      {
        path.Format("%s/%s", type, MFBC_SEGID);
        H5DataSetWriterSetup s(f, path, H5T_NATIVE_INT, 1);
        std::vector<hsize_t> start(1, 0), n2write(1, segmentIds.size());
        H5DSWriterDimInfo dim(start, n2write);
        H5DataSetWriter w(&s);
        w.SetDimInfoForWriting(&dim);
        w.WriteData(&segmentIds.at(0), segmentIds.size());
      }
      {
        path.Format("%s/%s", type, MFBC_SEGFLW);
        H5DataSetWriterSetup s(f, path, H5T_NATIVE_DOUBLE, 2);
        std::vector<hsize_t> start(2, 0), n2write(2, flow.size());
        start[1] = a_sp - 1;
        n2write[0] = flow.size();
        n2write[1] = 1;
        H5DSWriterDimInfo dim(start, n2write);
        H5DataSetWriter w(&s);
        w.SetDimInfoForWriting(&dim);
        w.AllowTypeConversions(true);
        w.WriteData(&flow.at(0), flow.size());
      }
    }
    // write the tributaries
    if (a_sp == 1)
    {
      CAR_INT2D itrib;
      itrib.SetSize(*nss, 10, 0);
      for (int segment = 0; segment < *nss; ++segment)
      {
        for (int trib = 0; trib < *ntrib; ++trib)
        {
          itrib.at(segment, trib) = itrbar[trib*(*nss) + segment];
        }
      }

      path.Format("%s/%s", type, MFBC_ITRIB);
      H5DataSetWriterSetup s(f, path, H5T_NATIVE_INT, 2);
      std::vector<hsize_t> start(2, 0), n2write(2, 0);
      n2write[0] = *nss;
      n2write[1] = 10;
      H5DSWriterDimInfo dim(start, n2write);
      H5DataSetWriter w(&s);
      w.SetDimInfoForWriting(&dim);
      w.WriteData(&itrib.at(0,0), *nss*10);
    }
    // write the upstream segment ids
    //if (a_sp == 1 && *ndiv > 0)
    // we want to write this even if ndiv < 1
    if (a_sp == 1)
    {
      path.Format("%s/%s", type, MFBC_UPID);
      H5DataSetWriterSetup s(f, path, H5T_NATIVE_INT, 1);
      std::vector<hsize_t> start(1, 0), n2write(1, *nss);
      H5DSWriterDimInfo dim(start, n2write);
      H5DataSetWriter w(&s);
      w.SetDimInfoForWriting(&dim);
      w.WriteData(idivar, *nss);
    }
    // end added

    CStr file1;
    util::StripPathFromFilename(f, file1);
    line.Format("GMS_HDF5_01 \"%s\" \"%s\" %d", file1, type, a_sp);
    a_exp->WriteLineToFile(a_fileType, line);
  }
} // expSTR
//------------------------------------------------------------------------------
/// \brief Exports list package data
//------------------------------------------------------------------------------
static void iWriteBcListData (const int a_sp,
                              const int a_start,
                              CStr &type,
                              CStr &f,
                              std::vector<int>& cellids,
                              CAR_DBL2D& bcData,
                              std::vector<int>& vIface)
{
  CStr path;
  // writing the bc "Property"
  {
    path.Format("%s/%s", type, MFBC_DATA);
    H5DataSetWriterSetup s(f, path, H5T_NATIVE_DOUBLE, 3);
    std::vector<hsize_t> start(3, 0), n2write(3,1);
    n2write[0] = bcData.GetSize1();
    n2write[1] = bcData.GetSize2();
    start[1] = a_start;
    start[2] = a_sp - 1;
    H5DSWriterDimInfo dim(start, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim);
    w.WriteData(&bcData.at(0,0),
                static_cast<size_t>(bcData.GetSize1()*bcData.GetSize2()));
  }
  // writing the "numbcs"
  {
    path.Format("%s/%s", type, MFBC_NUMBC);
    H5DataSetWriterSetup s(f, path, H5T_NATIVE_INT, 1);
    std::vector<hsize_t> start(1, 0), n2write(1, 1);
    H5DSWriterDimInfo dim(start, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim);
    int nCellids(static_cast<int>(cellids.size()));
    w.WriteData(&nCellids, 1);
  }
  // writing the bc "cellids"
  if (!cellids.empty())
  {
    path.Format("%s/%s", type, MFBC_CELLIDS);
    H5DataSetWriterSetup s(f, path, H5T_NATIVE_INT, 1);
    std::vector<hsize_t> start(1, 0), n2write(1, cellids.size());
    H5DSWriterDimInfo dim(start, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim);
    w.WriteData(&cellids.at(0), cellids.size());
  }
  // writing the bc "iface"
  if (!vIface.empty())
  {
    path.Format("%s/%s", type, MFBC_IFACE);
    H5DataSetWriterSetup s(f, path, H5T_NATIVE_INT, 1);
    std::vector<hsize_t> start(1, 0), n2write(1, vIface.size());
    H5DSWriterDimInfo dim(start, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim);
    w.WriteData(&vIface.at(0), vIface.size());
  }
  // write name and map id
  std::vector<char> vC(cellids.size(), 0);
  if (!cellids.empty())
  {
    path.Format("%s/%s", type, MFBC_NAME);
    H5DataSetWriterSetup s(f, path, H5T_NATIVE_CHAR, 1);
    std::vector<hsize_t> start(1, 0), n2write(1, vC.size());
    H5DSWriterDimInfo dim(start, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim);
    w.WriteData(&vC.at(0), vC.size());
  }
  if (!cellids.empty())
  {
    path.Format("%s/%s", type, MFBC_MAPIDSTR);
    H5DataSetWriterSetup s(f, path, H5T_NATIVE_CHAR, 1);
    std::vector<hsize_t> start(1, 0), n2write(1, vC.size());
    H5DSWriterDimInfo dim(start, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim);
    w.WriteData(&vC.at(0), vC.size());
  }
} // iWriteBcListData
//------------------------------------------------------------------------------
/// \brief fills in an array of flags to signify if an index in the bc array
/// for a type of BC has already been used.
//------------------------------------------------------------------------------
static void iGetArrayOfUsedBcIndices (const CStr &a_type,
                                      int a_sp,
                                      std::vector<char> &a_vAlreadyUsed,
                                      TxtExporter* a_exp)
{
  // there is a hash_map for each type of BC (RIV, DRN...)
  stdext::hash_map<int, VEC_INT_PAIR> &hMap(iGetBcIdxMap(a_exp)[a_type]);
  stdext::hash_map<int, VEC_INT_PAIR>::iterator it(hMap.begin());
  std::pair<int, int> *p;
  int arrayIndex, lastUsedInSp;

  for ( ; it != hMap.end(); it++)
  {
    for (size_t i=0; i<it->second.size(); i++)
    {
      p = &it->second.at(i);
      arrayIndex = p->first;
      lastUsedInSp = p->second;

      // see if the BC has been used yet
      if (lastUsedInSp > a_sp)
      {
        a_vAlreadyUsed.at(arrayIndex) = true;
      }
    }
  }
} // iGetArrayOfUsedBcIndices
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
/// \brief
//------------------------------------------------------------------------------
static void expListPack (MfPackage *a_p,
                         const int a_sp,
                         const int a_nRow,
                         const int a_nCol,
                         TxtExporter *a_exp)
{
  CStr                type, ptype, fileType(a_p->PackageName());
  iGetListPackTypeFromPtr(fileType, type);
  iGetListParamTypeFromPtr(fileType, ptype);

  CStr                line;
  CellIdToIJK         grid(a_nRow, a_nCol);
  std::vector<int>   &cellids(iGetBcCellIds(a_exp)[type]),
                     &vIface(iGetBcIface(a_exp)[type]),
                     &vCellgrp(iGetBcCellGrp(a_exp)[type]);
  const int          *itmp(0), *maxBc(0), *np(0);
  int                 i, j, ifaceIdx(-1), nBcFields, ci, cj, ck, cellId,
                      prevSpNumBc, cellgrpIdx(-1), seawatIdx0(-1),
                      seawatIdx1(-1);

  int                &maxBcSoFar = iGetMaxBcSoFar(type, a_exp);
  if (a_sp == 1)
    maxBcSoFar = 0;

  std::vector<Param> pList = MfExportUtil::GetParamsOfType(ptype);

  if (!a_p->GetField(Packages::ListPack::ITMP, &itmp) || !itmp ||
      !a_p->GetField(Packages::ListPack::MAXBC, &maxBc) || !maxBc ||
      !a_p->GetField(Packages::ListPack::NP , &np) || !np)
    return;

  // get the list BC data
  const int          *nBcs(0), *nAux(0), *nDataFields(0);
  int                 nFields;
  const Real*         data(0),* PHIRAMP(0);
  std::vector<CStr>   fieldStrings;
  if (!MfData::Packages::GetBcData(a_p, fileType, &nBcs, &nFields,
                                   &nAux, &data, &nDataFields, fieldStrings))
    return;

  prevSpNumBc = (int)cellids.size();

  if (maxBcSoFar < *nBcs)
    maxBcSoFar = *nBcs;

  int condfactIdx(-1), qfactIdx(-1);
  {
    for (size_t q=0; q<fieldStrings.size(); q++)
    {
      // iface can come from AUX fields
      if (fieldStrings[q].CompareNoCase("IFACE") == 0)
        ifaceIdx = static_cast<int>(q);
      if (fieldStrings[q].CompareNoCase("CELLGRP") == 0)
        cellgrpIdx = static_cast<int>(q);
      if (fieldStrings[q].CompareNoCase("CONDFACT") == 0)
        condfactIdx = static_cast<int>(q);
      if (fieldStrings[q].CompareNoCase("QFACT") == 0)
        qfactIdx = static_cast<int>(q);

      // seawat stuff
      if (   fieldStrings[q].CompareNoCase("RBDTHK") == 0
          || fieldStrings[q].CompareNoCase("DRNBELEV") == 0
          || fieldStrings[q].CompareNoCase("WELDENS") == 0
          || fieldStrings[q].CompareNoCase("GHBELEV") == 0
          || fieldStrings[q].CompareNoCase("CHDDENSOPT") == 0
          )
        seawatIdx0 = static_cast<int>(q);
      if (   fieldStrings[q].CompareNoCase("RIVDEN") == 0
          || fieldStrings[q].CompareNoCase("GHBDENS") == 0
          || fieldStrings[q].CompareNoCase("CHDDEN") == 0
          )
        seawatIdx1 = static_cast<int>(q);
    }
  }

  if (a_sp == 1)
  {
    line.Format("%d ", *maxBc);
    if (MfData::Packages::CHD != fileType)
      line += "40";
    if (fileType == MfData::Packages::DRT && *nDataFields - *nAux >= 9)
    {
      line += " 0 0 returnflow";
    }
    else if (fileType == MfData::Packages::DRT)
    {
      line += " 0 0";
    }
    if (MfData::Packages::CHD != fileType)
    {
      if (ifaceIdx != -1)
        line += " AUX IFACE";
      if (condfactIdx != -1)
        line += " AUX CONDFACT";
      if (qfactIdx != -1)
        line += " AUX QFACT";
      if (cellgrpIdx != -1)
        line += " AUX CELLGRP";
    }
    if (seawatIdx0 != -1)
    {
      line += " AUX ";
      line += fieldStrings[seawatIdx0];
    }
    if (seawatIdx1 != -1)
    {
      line += " AUX ";
      line += fieldStrings[seawatIdx1];
    }
    a_exp->WriteLineToFile(fileType, "#GMS_HDF5_01");
    a_exp->WriteLineToFile(fileType, line);
    if (MfData::Packages::WEL == fileType &&
        a_p->GetField("PHIRAMP", &PHIRAMP) && PHIRAMP)
    {
      CStr tmpLine;
      tmpLine.Format("SPECIFY %s", STR(*PHIRAMP));
      a_exp->WriteLineToFile(fileType, tmpLine);
    }
  }

  // Go through the data that was passed in. It is possible that there are
  // already BCs active in this stress period that have come from parameters.
  int maxIdx((int)cellids.size()-1);
  std::vector<int> idxs;
  idxs.reserve(*nBcs);
  for (i=0; i<*nBcs && *itmp > -1; i++)
  {
    ck = static_cast<int>(data[i*(*nDataFields)+0]);
    ci = static_cast<int>(data[i*(*nDataFields)+1]);
    cj = static_cast<int>(data[i*(*nDataFields)+2]);
    cellId = grid.IdFromIJK(ci, cj, ck);
    idxs.push_back(iGetBcIndex(type, cellId, a_sp, cellids, vIface, vCellgrp, a_exp));
    if (idxs.back() > maxIdx)
      maxIdx = idxs.back();
    // get the iface values if they exist
    if (ifaceIdx != -1)
    {
      vIface.at(idxs.back()) = 
                              static_cast<int>(data[i*(*nDataFields)+ifaceIdx]);
    }
    // get the cellgrp values if they exist
    if (cellgrpIdx != -1)
    {
      vCellgrp.at(idxs.back()) = 
                            static_cast<int>(data[i*(*nDataFields)+cellgrpIdx]);
    }
  }

  // Write the line containing itmp. If there are parameters then we don't let
  // itmp = -1 because in GMS there is only 1 use last flag that applies to all
  // bcs of a given package. If itmp = -1 there can still be parameters that
  // change from the previous stress period to the current stress period.
  if (!pList.empty())
    line.Format("%10d%10d%10d", maxIdx+1, 0, 0);
  else
    line.Format("%10d%10d%10d", *itmp, 0, 0);
  a_exp->WriteLineToFile(fileType, line);

  // Write the use last flag. If there are any parameters then we always set
  // the use last flag to false because we don't know what parameters may
  // have changed since the last stress period.
  CStr f(a_exp->GetBaseFileName()), path;
  f += ".h5";
  {
    path.Format("%s/%s", type, MFBC_USELAST);
    H5DataSetWriterSetup s(f, path, H5T_NATIVE_INT, 1);
    std::vector<hsize_t> start(1, a_sp-1), n2write(1,1);
    H5DSWriterDimInfo dim(start, n2write);
    H5DataSetWriter w(&s);
    w.SetDimInfoForWriting(&dim);
    int tmpItmp(*itmp < 0 ? 1 : 0);
    if (!pList.empty()) // see comment above
      tmpItmp = 0;
    w.WriteData(&tmpItmp, 1);
  }

  // size the data array for this stress period
  CAR_DBL2D bcData;
  int dSize(maxIdx);
  if (*nBcs < 1)
  {
    dSize = static_cast<int>(cellids.size());
    --dSize;
  }
  iSizeBcDataArray(type, dSize, bcData);

  // get the bc data for the current stress period if there are parameters
  if (*np > 0)
  {// if we are using last then get the data from the last stress period
    CAR_DBL2D tmpData;
    iSizeBcDataArray(type, prevSpNumBc-1, tmpData);
    path.Format("%s/%s", type, MFBC_DATA);
    std::pair<int, int> p(0,1);
    VEC_INT_PAIR indices(3,p);
    indices[0].second = tmpData.GetSize1();
    indices[1].second = prevSpNumBc;
    indices[2].first = a_sp-1;
    H5DataSetReader r(f, path, indices);
    r.GetData(&tmpData.at(0,0),
              static_cast<size_t>(tmpData.GetSize1()*prevSpNumBc));
    for (i=0; i<tmpData.GetSize1(); i++)
    {
      for (j=0; j<tmpData.GetSize2(); j++)
      {
        bcData.at(i,j) = tmpData.at(i,j);
      }
    }
  }
  if (*itmp < 0)
  { // get the previous stress period
    CAR_DBL2D prevData;
    iSizeBcDataArray(type, dSize, prevData);
    path.Format("%s/%s", type, MFBC_DATA);
    std::pair<int, int> p(0,1);
    VEC_INT_PAIR indices(3,p);
    indices[0].second = prevData.GetSize1();
    indices[1].second = prevData.GetSize2();
    indices[2].first = a_sp-2;
    H5DataSetReader r(f, path, indices);
    r.GetData(&prevData.at(0,0),
              static_cast<size_t>(prevData.GetSize1()*prevSpNumBc));
    // If there are parameters then we can't overwrite the current sp with
    // the previous. So we have to check to see which bcs have been used by
    // the parameters and skip those ones.
    std::vector<char> vAlreadyUsed(prevSpNumBc, 0);
    iGetArrayOfUsedBcIndices(type, a_sp, vAlreadyUsed, a_exp);
    for (i=0; i<bcData.GetSize1(); i++)
    {
      for (j=0; j<bcData.GetSize2(); j++)
      {
        if (!vAlreadyUsed.at(j))
          bcData.at(i,j) = prevData.at(i,j);
      }
    }
    // If there are no parameters then get any key values for this type of
    // parameter and set those to zero (0.0) in the bcData array so that they
    // don't affect the model.
    if (*np < 1)
    {
      std::set<Real> keys;
      for (i=0; i<(int)pList.size(); i++)
      {
        keys.insert(static_cast<Real>(pList.at(i).m_key));
      }
      for (i=0; !keys.empty() && i<bcData.GetSize1(); i++)
      {
        for (j=0; j<bcData.GetSize2(); j++)
        {
          if (keys.find(static_cast<Real>(bcData.at(i,j))) != keys.end())
          {
            bcData.at(i,j) = 0.0;
          }
        }
      }
    }
  }

  // fill in the bcData. Use the indices that we calculated above to figure
  // out where to put the data associated with a particular BC.
  nBcFields = nFields - *nAux - 3; // k,i,j
  int seawatBcIdx0(-1), seawatBcIdx1(-1);
  iGetSeawatBcIdx(type, seawatBcIdx0, seawatBcIdx1);
  for (i=0; i<*nBcs && *itmp > -1; i++)
  {
    for (j=0; j<nBcFields; j++)
    {
      bcData.at(j, idxs.at(i)) =
                               static_cast<double>(data[i*(*nDataFields)+3+j]);
    }
    if (condfactIdx != -1)
    {
      bcData.at(nBcFields, idxs.at(i)) =
                        static_cast<double>(data[i*(*nDataFields)+condfactIdx]);
    }
    if (qfactIdx != -1)
    {
      bcData.at(nBcFields, idxs.at(i)) =
                        static_cast<double>(data[i*(*nDataFields)+qfactIdx]);
    }
    if (seawatIdx0 != -1 && seawatBcIdx0 != -1)
    {
      bcData.at(seawatBcIdx0, idxs.at(i)) =
                        static_cast<double>(data[i*(*nDataFields)+seawatIdx0]);
    }
    if (seawatIdx1 != -1 && seawatBcIdx1 != -1)
    {
      bcData.at(seawatBcIdx1, idxs.at(i)) =
                        static_cast<double>(data[i*(*nDataFields)+seawatIdx1]);
    }
  }

  if (bcData.GetSize1() * bcData.GetSize2() > 0)
    iWriteBcListData(a_sp, 0, type, f, cellids, bcData, vIface);

  CStr file1;
  util::StripPathFromFilename(f, file1);
  line.Format("GMS_HDF5_01 \"%s\" \"%s\" %d", file1, type, a_sp);
  if ((!pList.empty() && maxIdx+1 > 0) ||
      *itmp > 0)
  {
    a_exp->WriteLineToFile(fileType, line);
  }
} // expListPack
//------------------------------------------------------------------------------
/// \brief Exports list package parameter data
//------------------------------------------------------------------------------
static void expListParameterData (MfPackage *a_p,
                                  const int a_sp,
                                  const int a_nRow,
                                  const int a_nCol,
                                  TxtExporter *a_exp)
{
  CStr                type, listPack, fileType(a_p->PackageName());
  if (!MfData::Packages::GetPackNameFromParameter(a_p, listPack))
    return;
  iGetListPackTypeFromPtr(listPack, type);

  if (listPack.CompareNoCase(MfData::Packages::HFB) == 0)
  {
    MfData::Packages::SaveHfbParameterData(a_p, a_exp->HfbParData());
    return;
  }

  CellIdToIJK         grid(a_nRow, a_nCol);
  std::vector<int>   &cellids(iGetBcCellIds(a_exp)[type]),
                     &vIface(iGetBcIface(a_exp)[type]),
                     &vCellgrp(iGetBcCellGrp(a_exp)[type]);


  // get the list BC data
  const int          *nBcs, *nAux, *nDataFields;
  int                 nFields;
  const Real*         data;
  std::vector<CStr>   fieldStrings;
  MfData::Packages::GetBcData(a_p, listPack, &nBcs, &nFields,
                              &nAux, &data, &nDataFields, fieldStrings);

  // get some info about the parameter
  int                 start;
  Real                key;
  if (!MfData::Packages::GetParamKeyAndDataStart(a_p, key, start))
    return;

  // first create a vector of indices so we can size the data array
  int i, maxIdx(-1), ifaceIdx(-1), cellgrpIdx(-1), cellId, ci, cj, ck;
  int minIdx(static_cast<int>(cellids.size()+1));
  std::vector<int> idxs;
  idxs.reserve(*nBcs);

  // see if iface is one of the aux fields
  {
    size_t j;
    for (j=0; j<fieldStrings.size(); j++)
    {
      if (fieldStrings[j].CompareNoCase("iface") == 0)
        ifaceIdx = static_cast<int>(j);
      if (fieldStrings[j].CompareNoCase("cellgrp") == 0)
        cellgrpIdx = static_cast<int>(j);
    }
  }

  for (i=start; i<(*nBcs+start); i++)
  {
    ck = static_cast<int>(data[i*(*nDataFields)+0]);
    ci = static_cast<int>(data[i*(*nDataFields)+1]);
    cj = static_cast<int>(data[i*(*nDataFields)+2]);
    cellId = grid.IdFromIJK(ci, cj, ck);
    idxs.push_back(iGetBcIndex(type, cellId, a_sp, cellids, vIface, vCellgrp, a_exp));
    if (idxs.back() > maxIdx)
      maxIdx = idxs.back();
    if (idxs.back() < minIdx)
      minIdx = idxs.back();
    // get the iface values if they exist
    if (ifaceIdx != -1)
    {
      vIface.at(idxs.back()) =
                              static_cast<int>(data[i*(*nDataFields)+ifaceIdx]);
    }
    // get the cellgrp values if they exist
    if (cellgrpIdx != -1)
    {
      vCellgrp.at(idxs.back()) =
                              static_cast<int>(data[i*(*nDataFields)+cellgrpIdx]);
    }
  }

  // size the BC data vector
  CAR_DBL2D bcData;
  iSizeBcDataArray(type, maxIdx-minIdx, bcData);

  int bcIdx, fieldIdx, dataIdx;
  std::map<int, int> srcDestIdxs;
  MfData::Packages::GetParamSrcDestFields(listPack, fieldStrings, srcDestIdxs);
  // fill in the bcData
  int j, nBcFields = nFields - *nAux - 3; // k,i,j
  for (i=start; i<(*nBcs+start); i++)
  {
    for (j=0; j<nBcFields; j++)
    {
      bcIdx = idxs.at(i-start) - minIdx;
      dataIdx = i*(*nDataFields)+3+j;
      fieldIdx = j;
      if (srcDestIdxs.find(j+3) != srcDestIdxs.end())
        fieldIdx = srcDestIdxs[j+3];
      bcData.at(fieldIdx, bcIdx) = static_cast<double>(data[dataIdx]);
      if (j != fieldIdx)
        bcData.at(j, bcIdx) = key;
    }
  }

  CStr f(a_exp->GetBaseFileName()), path;
  f += ".h5";
  iWriteBcListData(a_sp, minIdx, type, f, cellids, bcData, vIface);
} // expListParameterData
//------------------------------------------------------------------------------
/// \brief Gets the file global
//------------------------------------------------------------------------------
static void iSizeBcDataArray (const CStr &a_type,
                              const int a_maxIdx,
                              CAR_DBL2D &a_data)
{
  a_data.Clear();
  int start(0), num(1);
  if (a_type == "River")
  {
    // stage, cond, elev, factor, RBDTHK, RIVDEN
    a_data.SetSize(6, a_maxIdx+1, 0);
    start = 3;
  }
  else if (a_type == "Specified Head")
  {
    // startHead, endHead, factor1, factor2, CHDDENSOPT, CHDDEN
    a_data.SetSize(6, a_maxIdx+1, 0);
    start = 2;
    num = 2;
  }
  else if (a_type == "Drain")
  {
    // elev, cond, factor, DRNBELEV
    a_data.SetSize(4, a_maxIdx+1, 0);
    start = 2;
  }
  else if (a_type == "General Head")
  {
    // head, cond, factor, GHBELEV, GHBDENS
    a_data.SetSize(5, a_maxIdx+1, 0);
    start = 2;
  }
  else if (a_type == "Drain Return")
  {
    // elev, cond, layR, rowR, colR, Rfprop, factor
    a_data.SetSize(7, a_maxIdx+1, 0);
    start = 6;
  }
  else if (a_type == "Well")
  {
    // Q, factor, WELDENS
    a_data.SetSize(3, a_maxIdx+1, 0);
    start = 1;
  }
  else if (a_type == "Stream")
  {
    // stage, cond, bot. elev., top elev., width, slope, rough, factor
    a_data.SetSize(8, a_maxIdx+1, 0);
    start = 7;
  }
  else if (a_type == "Stream (SFR2)")
  {
    // RCHLEN
    a_data.SetSize(1, a_maxIdx+1, 0);
  }

  for (int i=start; (i-start)<num; i++)
  {
    // this initializes the condfact to 1.0 or for CHD the headfact
    for (int j=0; j<a_data.GetSize2(); j++)
      a_data.at(i, j) = 1.0;
  }
} // iSizeBcDataArray
//------------------------------------------------------------------------------
/// \brief Gets the file global
//------------------------------------------------------------------------------
static std::map<CStr, std::vector<int> > &iGetBcIface (TxtExporter* a_exp)
{
  return a_exp->m_public->m_BcIface;
} // iGetBcIface
//------------------------------------------------------------------------------
/// \brief Gets the file global
//------------------------------------------------------------------------------
static std::map<CStr, std::vector<int> > &iGetBcCellGrp (TxtExporter* a_exp)
{
  return a_exp->m_public->m_BcCellGrp;
} // iGetBcCellGrp
//------------------------------------------------------------------------------
/// \brief Gets the file global
//------------------------------------------------------------------------------
static std::map<CStr, std::vector<int> > &iGetBcCellIds (TxtExporter* a_exp)
{
  return a_exp->m_public->m_BcCellIds;
} // iGetBcCellIds
//------------------------------------------------------------------------------
/// \brief Gets the file global
//------------------------------------------------------------------------------
static std::map<CStr, stdext::hash_map<int, VEC_INT_PAIR> > &iGetBcIdxMap (TxtExporter* a_exp)
{
  return a_exp->m_public->m_BcIdxMap;
} // iGetBcIdxMap
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static int& iGetMaxBcSoFar(const CStr& a_type, TxtExporter* a_exp)
{
  std::map<CStr, int>& maxBcSoFar = a_exp->m_public->m_maxBcSoFar;
  if (maxBcSoFar.find(a_type) == maxBcSoFar.end())
    maxBcSoFar[a_type] = 0;
  return maxBcSoFar[a_type];
}
//------------------------------------------------------------------------------
/// \brief Gets the index of the bc for use in the arrays of data
//------------------------------------------------------------------------------
static int iGetBcIndex (const CStr &a_type,
                        int a_cellid,
                        int a_sp,
                        std::vector<int> &a_cellids,
                        std::vector<int> &a_iface,
                        std::vector<int> &a_cellgrp,
                        TxtExporter* a_exp)
{
  // there is a hash_map for each type of BC (RIV, DRN...)
  stdext::hash_map<int, VEC_INT_PAIR> &hMap(iGetBcIdxMap(a_exp)[a_type]);
  stdext::hash_map<int, VEC_INT_PAIR>::iterator it;

  size_t i;
  int    idx(-1);
  bool   found(false);
  // see if this cellid is in the hash_map
  it = hMap.find(a_cellid);
  if (it != hMap.end())
  {
    // inside the hash_map we have a vector of int pairs
    // the map has cellid as the key and associated with cellid is a vector
    // of pairs. In the pair the 'first' is the array index (the spot in the
    // data array [this is where we store stage, cond, elev...] associated with
    // the cellid). The 'second' in the pair is the stress period number of the
    // last time this index was used.
    int     arrayIndex, lastUsedInSp;
    std::pair<int, int> *p;
    // loop through the vector and find the first index that hasn't been used
    // in this stress period
    for (i=0; i<it->second.size() && !found; i++)
    {
      p = &it->second.at(i);
      arrayIndex = p->first;
      lastUsedInSp = p->second;

      // see if the BC has been used yet
      if (lastUsedInSp > a_sp)
        continue;

      idx = p->first;
      // update the lastUsedInSp field
      p->second = a_sp + 1;
      found = true;
    }
  }

  // no available spot for this BC so make a new one
  if (!found)
  {
    // this means that this is the first time a BC is in this cell OR
    // in this stress period we now have more BCs in this cell than in any
    // previous stress period
    
    if (it == hMap.end())
    {
      hMap.insert(std::make_pair(a_cellid, VEC_INT_PAIR()));
      it = hMap.find(a_cellid);
    }

    idx = static_cast<int>(a_cellids.size());
    a_cellids.push_back(a_cellid);
    std::pair<int, int> p1(idx, a_sp+1);
    it->second.push_back(p1);

    a_iface.push_back(6);
    if (a_type.CompareNoCase("well") == 0)
      a_iface.back() = 0;
    a_cellgrp.push_back(-1);
  }

  return idx;
} // iGetBcIndex
//------------------------------------------------------------------------------
/// \brief Exports the horizontal flow barrier package
//------------------------------------------------------------------------------
static void expHFB (MfPackage *a_p,
                    TxtExporter *a_exp)
{
  using std::map;
  const int HFB_ROW_SIZE = 7;
  const int VAL_ROW_SIZE = 6;
  const int *nhfbnp;
  const Real *hfb;
  bool gotNhfbnp = a_p->GetField(Packages::HFBpack::NHFBNP, &nhfbnp);
  bool gotHfb = a_p->GetField(Packages::HFBpack::HFB, &hfb);
  if (gotNhfbnp && nhfbnp && gotHfb && hfb)
  {
    CStr s;
    map<CStr, vector<Real> >& usedParams = a_exp->HfbParData();
    std::vector<Param> paramList = MfExportUtil::GetParamsOfType("HFB");

    int nphfb = (int)usedParams.size();
    size_t mxfb = 0;
    map<CStr, vector<Real> >::iterator currParam;
    for (currParam = usedParams.begin();
         currParam != usedParams.end();
         ++currParam)
    {
      mxfb += currParam->second.size()/VAL_ROW_SIZE;
    }

    // Line 1: NPHFB MXFB NHFBNP
    s.Format("%ld %ld %d ", nphfb, mxfb, *nhfbnp);
    a_exp->WriteLineToFile(Packages::HFB, s);

    for (currParam = usedParams.begin();
         currParam != usedParams.end();
         ++currParam)
    {
      CStr pname = currParam->first;
      vector<Real>& values = currParam->second;
      size_t nlst = values.size()/VAL_ROW_SIZE;

      ParamList *list(0);
      Parameters::GetParameterList(&list);
      Param p;
      if (list->FindByName(pname.c_str(), &p))
      {
        // Line 2: PARNAM PARTYP Parval NLST
        MfExportUtil::InsertSingleQuotesInName(pname);
        // use m_value in case there is a SEN file with a different value
        // than the parameter value specified in the package file
        s.Format("%s %s %s %d", pname, p.m_type, STR(p.m_value), 
                 nlst);
        a_exp->WriteLineToFile(Packages::HFB, s);

        for (size_t i = 0; i < nlst; ++i)
        {
          int lay = static_cast<int>(values[(i*VAL_ROW_SIZE)+0]);
          int row1 = static_cast<int>(values[(i*VAL_ROW_SIZE)+1]);
          int col1 = static_cast<int>(values[(i*VAL_ROW_SIZE)+2]);
          int row2 = static_cast<int>(values[(i*VAL_ROW_SIZE)+3]);
          int col2 = static_cast<int>(values[(i *VAL_ROW_SIZE)+4]);
          Real factor = values[(i*VAL_ROW_SIZE)+5];

          // Line 3: Layer IROW1 ICOL1 IROW2  ICOL2 Factor
          s.Format("%d %d %d %d %d %s ", lay, row1, col1, row2, col2,
                                         STR(factor));
          a_exp->WriteLineToFile(Packages::HFB, s);
        }
      }
    }
    for (int i = 0; i < *nhfbnp; ++i)
    {
      int lay = static_cast<int>(hfb[(i*HFB_ROW_SIZE)+0]);
      int row1 = static_cast<int>(hfb[(i*HFB_ROW_SIZE)+1]);
      int col1 = static_cast<int>(hfb[(i*HFB_ROW_SIZE)+2]);
      int row2 = static_cast<int>(hfb[(i*HFB_ROW_SIZE)+3]);
      int col2 = static_cast<int>(hfb[(i *HFB_ROW_SIZE)+4]);
      Real hydc = hfb[(i*HFB_ROW_SIZE)+5];

      // Line 4: Layer IROW1 ICOL1 IROW2 ICOL2 Hydchr
      s.Format("%d %d %d %d %d %s ", lay, row1, col1, row2, col2, STR(hydc));
      a_exp->WriteLineToFile(Packages::HFB, s);
    }

    if (nphfb > 0)
    {
      // Line 5: NACTHFB
      s.Format("%ld", usedParams.size());
      a_exp->WriteLineToFile(Packages::HFB, s);

      for (currParam = usedParams.begin();
           currParam != usedParams.end();
           ++currParam)
      {
        CStr pname(currParam->first);
        MfExportUtil::InsertSingleQuotesInName(pname);

        // Line 6: Pname
        a_exp->WriteLineToFile(Packages::HFB, pname);
      }
    }
  }
} // expHFB
//------------------------------------------------------------------------------
/// \brief Exports the lake package without stress periods
//------------------------------------------------------------------------------
static void expLAK (MfPackage *a_p,
                    TxtExporter *a_exp)
{
  const int *nlakes(0), *ilkcb(0), *nssitr(0), *itrss(0);
  const Real *theta(0), *sscncr(0);
  if (a_p->GetField(Packages::LAKpack::NLAKES, &nlakes) && nlakes &&
      a_p->GetField(Packages::LAKpack::ILKCB, &ilkcb) && ilkcb &&
      a_p->GetField(Packages::LAKpack::THETA, &theta) && theta &&
      a_p->GetField(Packages::LAKpack::NSSITR, &nssitr) && nssitr &&
      a_p->GetField(Packages::LAKpack::SSCNCR, &sscncr) && sscncr &&
      a_p->GetField(Packages::LAKpack::ITRSS, &itrss) && itrss)
  {
    CStr s;

    // LAK doesn't support comments at the top of the file
    //a_exp->WriteLineToFile(Packages::LAK, "#GMS_HDF5_01");

    // export NLAKES ILKCB
    s.Format("%d %d", *nlakes, *ilkcb);
    a_exp->WriteLineToFile(Packages::LAK, s);

    // export THETA NSSITR SSCNCR
    s.Format("%s", STR(*theta));
    if (*itrss <= 0 || *theta < 0.0)
    {
      CStr ssiStr;
      ssiStr.Format(" %d %s",  *nssitr, STR(*sscncr));
      s += ssiStr;
    }
    a_exp->WriteLineToFile(Packages::LAK, s);
  }
} // expLAK
//------------------------------------------------------------------------------
/// \brief Exports the lake package stress periods
//------------------------------------------------------------------------------
static void expLAKSP (MfGlobal *a_g,
                      MfPackage *a_p,
                      MfPackage *a_pLAK,
                      TxtExporter *a_exp)
{
  const int *nlakes(0), *dummy(0), *itmp(0), *itmp1(0), *lwrt(0),
            *lkarr(0), *nslms(0), *ic(0), *isub(0), 
            currentPeriod(a_g->GetCurrentPeriod());
  const Real *stages(0), *ssmn(0), *ssmx(0), *clake(0), *bdlknc(0), *sillvt(0),
             *rnf(0), *wthdrw(0), *cppt(0), *crnf(0), *caug(0);
  const double *prcplk(0), *evaplk(0);

  if (a_pLAK->GetField(Packages::LAKpack::NLAKES, &nlakes) && nlakes &&
      a_p->GetField(Packages::LAKSPpack::NSOL, &dummy) && dummy &&
      a_p->GetField(Packages::LAKSPpack::STAGES, &stages) && stages &&
      a_p->GetField(Packages::LAKSPpack::SSMN, &ssmn) && ssmn &&
      a_p->GetField(Packages::LAKSPpack::SSMX, &ssmx) && ssmx &&
      a_p->GetField(Packages::LAKSPpack::CLAKE, &clake) && clake &&
      a_p->GetField(Packages::LAKSPpack::ITMP, &itmp) && itmp &&
      a_p->GetField(Packages::LAKSPpack::ITMP1, &itmp1) && itmp1 &&
      a_p->GetField(Packages::LAKSPpack::LWRT, &lwrt) && lwrt &&
      a_p->GetField(Packages::LAKSPpack::LKARR, &lkarr) && lkarr &&
      a_p->GetField(Packages::LAKSPpack::BDLKNC, &bdlknc) && bdlknc &&
      a_p->GetField(Packages::LAKSPpack::NSLMS, &nslms) && nslms &&
      a_p->GetField(Packages::LAKSPpack::IC, &ic) && ic &&
      a_p->GetField(Packages::LAKSPpack::ISUB, &isub) && isub &&
      a_p->GetField(Packages::LAKSPpack::SILLVT, &sillvt) && sillvt &&
      a_p->GetField(Packages::LAKSPpack::PRCPLK, &prcplk) && prcplk &&
      a_p->GetField(Packages::LAKSPpack::EVAPLK, &evaplk) && evaplk &&
      a_p->GetField(Packages::LAKSPpack::RNF, &rnf) && rnf &&
      a_p->GetField(Packages::LAKSPpack::WTHDRW, &wthdrw) && wthdrw &&
      a_p->GetField(Packages::LAKSPpack::CPPT, &cppt) && cppt &&
      a_p->GetField(Packages::LAKSPpack::CRNF, &crnf) && crnf &&
      a_p->GetField(Packages::LAKSPpack::CAUG, &caug) && caug)
  {
    CStr s;

    // Coded to include NSOL but it seems that mf2k gives NSOL=1
    // even though it wasn't included in export file.  Removing
    // for now.
    int nsolutes(0), *nsol=&nsolutes;
    
    if (currentPeriod == 1)
    {
      // export STAGES {SSMN SSMX} {CLAKE(1)... CLAKE(NSOL)}
      for (int lake = 0; lake < *nlakes; ++lake)
      {
        s.Format("%s %s %s", STR(stages[lake]), STR(ssmn[lake]), STR(ssmx[lake]));
        for (int sol = 0; sol < *nsol; ++sol)
        {
          CStr clakeStr;
          clakeStr.Format(" %s", STR(clake[sol*(*nlakes) + lake]));
          s += clakeStr;
        }
        a_exp->WriteLineToFile(Packages::LAK, s);
      }
    }

    // ITMP ITMP1 LWRT
    s.Format("%d %d %d", *itmp, *itmp1, *lwrt);
    a_exp->WriteLineToFile(Packages::LAK, s);

    if (*itmp > 0)
    {
      CStr path, fBase, fName(CStr(a_exp->GetBaseFileName()) + ".h5");
      util::StripPathFromFilename(fName, fBase);

      // LKARR
      s = "";
      //s.clear();
      int nCol(a_g->NumCol()), nRow(a_g->NumRow()), nLay(a_g->NumLay());
      int inLay(nCol * nRow);
      for (int k = 0; k < nLay; ++k)
      {
        s.Format("HDF5 1.0 -1 \"%s\" \"Arrays/Lak_%d\" 1 0 %d", 
                 fBase, k+1, nCol*nRow);
        a_exp->WriteLineToFile(Packages::LAK, s);
        {
          path.Format("Arrays/Lak_%d", k+1);
          H5DataSetWriterSetup wSetup(fName, path, H5T_NATIVE_INT, 1);
          std::vector<hsize_t> start(1, (currentPeriod-1)*inLay), 
                               n2write(1, inLay);
          H5DSWriterDimInfo dim(start, n2write);
          H5DataSetWriter w(&wSetup);
          w.SetDimInfoForWriting(&dim);
          w.WriteData(lkarr + k*inLay, inLay);
        }
      }

      // BDLKNC
      for (int k = 0; k < nLay; ++k)
      {
        s.Format("HDF5 1.0 -1 \"%s\" \"Arrays/LakLeak_%d\" 1 0 %d", 
                 fBase, k+1, nCol*nRow);
        a_exp->WriteLineToFile(Packages::LAK, s);
        {
          path.Format("Arrays/LakLeak_%d", k+1);
          H5DataSetWriterSetup wSetup(fName, path, H5T_NATIVE_DOUBLE, 1);
          std::vector<hsize_t> start(1, (currentPeriod-1)*inLay), 
                               n2write(1, inLay);
          H5DSWriterDimInfo dim(start, n2write);
          H5DataSetWriter w(&wSetup);
          w.SetDimInfoForWriting(&dim);
          w.AllowTypeConversions(true);
          w.WriteData(bdlknc + k*inLay, inLay);
        }
      }

      s.Format("%d", *nslms);
      a_exp->WriteLineToFile(Packages::LAK, s);

      if (*nslms > 0)
      {
        for (int system = 0; system < *nslms && ic[system] > 0; ++system)
        {
          // IC ISUB(IC)
          s.Format("%d ", ic[system]);
          for (int subLake = 0; subLake < ic[system]; ++subLake)
          {
            CStr itemStr;
            itemStr.Format("%d ", isub[subLake*(*nlakes) + system]);
            s += itemStr;
          }
          a_exp->WriteLineToFile(Packages::LAK, s);

          // SILLVT(IC)
          s = "";
          //s.clear();
          for (int subLake = 0; subLake < ic[system] - 1; ++subLake)
          {
            CStr itemStr;
            itemStr.Format("%s ", STR(sillvt[subLake*(*nlakes) + system]));
            s += itemStr;
          }
          a_exp->WriteLineToFile(Packages::LAK, s);
        }
      }
    }
    if (*itmp1 >= 0)
    {
      // PRCPLK EVAPLK RNF WTHDRW
      for (int lake = 0; lake < *nlakes; ++lake)
      {
        s.Format("%s %s %s %s", STR(prcplk[lake]), STR(evaplk[lake]), 
          STR(rnf[lake]), STR(wthdrw[lake]));
        a_exp->WriteLineToFile(Packages::LAK, s);

        // CPPT CRNF {CAUG}
        for (int sol = 0; sol < *nsol; ++sol)
        {
          int i = sol*(*nlakes) + lake;
          s.Format("%s %s", STR(cppt[i]), STR(crnf[i]));
          CStr caugStr;
          if (wthdrw[lake] < 0.0)
          {
            caugStr.Format(" %s", STR(caug[i]));
          }
          s += caugStr;
          a_exp->WriteLineToFile(Packages::LAK, s);
        }
      }
    }
  }
} // expLAKSP
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void expSFRLine1 (MfPackage *a_pSFRLine1,
                         TxtExporter *a_exp)
{
  using namespace MfData::Packages;
  const int *nstrm(0), *nss(0), *istcb1(0), *istcb2(0), *isfropt(0), 
            *nstrail(0), *isuzn(0), *nsfrsets(0);
  const Real *constv(0), *dleak(0);
  if (a_pSFRLine1->GetField(SFRpack::NSTRM, &nstrm) && nstrm &&
      a_pSFRLine1->GetField(SFRpack::NSS, &nss) && nss &&
      a_pSFRLine1->GetField(SFRpack::CONSTV, &constv) && constv &&
      a_pSFRLine1->GetField(SFRpack::DLEAK, &dleak) && dleak &&
      a_pSFRLine1->GetField(SFRpack::ISTCB1, &istcb1) && istcb1 &&
      a_pSFRLine1->GetField(SFRpack::ISTCB2, &istcb2) && istcb2 &&
      a_pSFRLine1->GetField(SFRpack::ISFROPT, &isfropt) && isfropt &&
      a_pSFRLine1->GetField(SFRpack::NSTRAIL, &nstrail) && nstrail &&
      a_pSFRLine1->GetField(SFRpack::ISUZN, &isuzn) && isuzn &&
      a_pSFRLine1->GetField(SFRpack::NSFRSETS, &nsfrsets) && nsfrsets)
  {
    a_exp->WriteLineToFile(SFR, "#GMS_HDF5_01");
    
    CStr line;
    line.Format("%d %d 0 0 %s %s %d %d", *nstrm, *nss, STR(*constv),
               STR(*dleak), *istcb1, *istcb2);
    if (*nstrm < 0)
    {
      CStr nstrmPart;
      nstrmPart.Format(" %d", *isfropt);
      line += nstrmPart;
      if (*isfropt > 1)
      {
        CStr isfrPart;
        isfrPart.Format(" %d %d %d", *nstrail, *isuzn, *nsfrsets);
        line += isfrPart;
      }
    }
    a_exp->WriteLineToFile(SFR, line);
  }
} // expSFRLine1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void expSFRLine2 (MfPackage *a_pSFRLine1,
                         MfPackage *a_pSFRLine2,
                         int a_nRow,
                         int a_nCol,
                         int a_nStressPeriods,
                         TxtExporter *a_exp)
{
  using namespace MfData::Packages;
  const int *nstrm(0), *nss(0), *isfropt(0), *istrm(0), *nistrmd(0), *nstrmd(0);
  const Real *strm(0);
  if (a_pSFRLine1->GetField(SFRpack::NSTRM, &nstrm) && nstrm &&
      a_pSFRLine1->GetField(SFRpack::NSS, &nss) && nss &&
      a_pSFRLine1->GetField(SFRpack::ISFROPT, &isfropt) && isfropt &&

      a_pSFRLine2->GetField(SFRpack::ISTRM, &istrm) && istrm &&
      a_pSFRLine2->GetField(SFRpack::NISTRMD, &nistrmd) && nistrmd &&
      a_pSFRLine2->GetField(SFRpack::STRM, &strm) && strm &&
      a_pSFRLine2->GetField(SFRpack::NSTRMD, &nstrmd) && nstrmd)
  {
    // much of the following code was grabbed from expSTR and modified slightly
    CStr f(a_exp->GetBaseFileName()), path, fileName, line;
    const char* type = "Stream (SFR2)";
    std::vector<int>   &cellids(iGetBcCellIds(a_exp)[type]),
                       &vIface(iGetBcIface(a_exp)[type]),
                       &vCellgrp(iGetBcCellGrp(a_exp)[type]);
    CellIdToIJK        grid(a_nRow, a_nCol);
    int numReaches(*nstrm);
    if (numReaches < 0)
      numReaches = -numReaches;

    f += ".h5";
    util::StripPathFromFilename(f, fileName);
    line.Format("GMS_HDF5_SFR2_REACH \"%s\" \"SFR2\"", fileName);
    a_exp->WriteLineToFile(SFR, line);

    // first create a vector of indices so we can size the data array
    int maxIdx(-1);
    std::vector<int> idxs;
    idxs.reserve(numReaches);
    for (int i = 0; i < numReaches; i++)
    {
      int ck = istrm[i*(*nistrmd)+0];
      int ci = istrm[i*(*nistrmd)+1];
      int cj = istrm[i*(*nistrmd)+2];

      int cellId = grid.IdFromIJK(ci, cj, ck);
      idxs.push_back(iGetBcIndex(type, cellId, 1, cellids, vIface, vCellgrp, a_exp));
      if (idxs.back() > maxIdx)
        maxIdx = idxs.back();
    }

    // size the BC data vector
    CAR_DBL2D bcData;
    iSizeBcDataArray(type, maxIdx, bcData);

    // fill in the bcData
    for (int i = 0; i < numReaches; ++i)
    {
      bcData.at(0, idxs.at(i)) = static_cast<double>(strm[i*(*nstrmd)]);
    }

    // 00. Number of BCs (value: NSTRM)
    {
      path.Format("%s/%s", type, MFBC_NUMBC);
      H5DataSetWriterSetup s(f, path, H5T_NATIVE_INT, 1);
      std::vector<hsize_t> start(1, 0), n2write(1, 1);
      H5DSWriterDimInfo dim(start, n2write);
      H5DataSetWriter w(&s);
      w.SetDimInfoForWriting(&dim);
      w.WriteData(&numReaches, 1);
    }

    // 02. Cell IDs (size: NSTRM)
    {
      path.Format("%s/%s", type, MFBC_CELLIDS);
      H5DataSetWriterSetup s(f, path, H5T_NATIVE_INT, 1);
      std::vector<hsize_t> start(1, 0), n2write(1, cellids.size());
      H5DSWriterDimInfo dim(start, n2write);
      H5DataSetWriter w(&s);
      w.SetDimInfoForWriting(&dim);
      w.WriteData(&cellids.at(0), cellids.size());
    }

    // 03. Name (size: NSTRM)
    std::vector<char> vC(cellids.size(), 0);
    {
      path.Format("%s/%s", type, MFBC_NAME);
      H5DataSetWriterSetup s(f, path, H5T_NATIVE_CHAR, 1);
      std::vector<hsize_t> start(1, 0), n2write(1, vC.size());
      H5DSWriterDimInfo dim(start, n2write);
      H5DataSetWriter w(&s);
      w.SetDimInfoForWriting(&dim);
      w.WriteData(&vC.at(0), vC.size());
    }

    // 04. Map ID
    {
      path.Format("%s/%s", type, MFBC_MAPIDSTR);
      H5DataSetWriterSetup s(f, path, H5T_NATIVE_CHAR, 1);
      std::vector<hsize_t> start(1, 0), n2write(1, vC.size());
      H5DSWriterDimInfo dim(start, n2write);
      H5DataSetWriter w(&s);
      w.SetDimInfoForWriting(&dim);
      w.WriteData(&vC.at(0), vC.size());
    }

    // 06. IFACE
    {
      path.Format("%s/%s", type, MFBC_IFACE);
      H5DataSetWriterSetup s(f, path, H5T_NATIVE_INT, 1);
      std::vector<hsize_t> start(1, 0), n2write(1, vIface.size());
      H5DSWriterDimInfo dim(start, n2write);
      H5DataSetWriter w(&s);
      w.SetDimInfoForWriting(&dim);
      w.WriteData(&vIface.at(0), vIface.size());
    }

    // 07. Property (size: NSTRM values: RCHLEN dup for each stress period)
    for (int sp = 0; sp < a_nStressPeriods; ++sp)
    {
      path.Format("%s/%s", type, MFBC_DATA);
      H5DataSetWriterSetup s(f, path, H5T_NATIVE_DOUBLE, 3);
      std::vector<hsize_t> start(3, 0), n2write(3,1);
      n2write[0] = bcData.GetSize1();
      n2write[1] = bcData.GetSize2();
      start[2] = sp;
      H5DSWriterDimInfo dim(start, n2write);
      H5DataSetWriter w(&s);
      w.SetDimInfoForWriting(&dim);
      w.WriteData(&bcData.at(0,0),
                  static_cast<size_t>(bcData.GetSize1()*bcData.GetSize2()));
    }

    // 08. Str reach segment ID (size: NSTRM)
    {
      std::vector<int> reachIds;
      idxs.reserve(numReaches);
      for (int i = 0; i < numReaches; ++i)
      {
        reachIds.push_back(istrm[i*(*nistrmd)+3]);
      }
      path.Format("%s/%s", type, MFBC_STRSEGID);
      H5DataSetWriterSetup s(f, path, H5T_NATIVE_INT, 1);
      std::vector<hsize_t> start(1, 0), n2write(1, reachIds.size());
      H5DSWriterDimInfo dim(start, n2write);
      H5DataSetWriter w(&s);
      w.SetDimInfoForWriting(&dim);
      w.WriteData(&reachIds.at(0), reachIds.size());
    }

    // 13. Number of Segments (NSS)
    {
      path.Format("%s/%s", type, MFBC_NSEG);
      H5DataSetWriterSetup s(f, path, H5T_NATIVE_INT, 1);
      std::vector<hsize_t> start(1, 0), n2write(1,1);
      H5DSWriterDimInfo dim(start, n2write);
      H5DataSetWriter w(&s);
      w.SetDimInfoForWriting(&dim);
      w.WriteData(nss, 1);
    }
  }
} // expSFRLine2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void expSFRLine5 (MfPackage *a_pSFRLine5,
                         int a_sp,
                         TxtExporter *a_exp)
{
  using namespace MfData::Packages;
  const int *itmp(0), *irdflg(0), *iptflg(0);
  if (a_pSFRLine5->GetField(SFRpack::ITMP, &itmp) && itmp &&
      a_pSFRLine5->GetField(SFRpack::IRDFLG, &irdflg) && irdflg &&
      a_pSFRLine5->GetField(SFRpack::IPTFLG, &iptflg) && iptflg)
  {
    CStr line;
    line.Format("%d %d %d", *itmp, *irdflg, *iptflg);
    a_exp->WriteLineToFile(SFR, line);

    CStr f(a_exp->GetBaseFileName()), path, fileName;
    f += ".h5";
    util::StripPathFromFilename(f, fileName);
    if (*itmp > 0)
    {
      line.Format("GMS_HDF5_01 \"%s\" \"SFR2\" %d", fileName, a_sp);
      a_exp->WriteLineToFile(SFR, line);
    }

    {
      const char* type = "Stream (SFR2)";
      path.Format("%s/%s", type, MFBC_USELAST);
      H5DataSetWriterSetup s(f, path, H5T_NATIVE_INT, 1);
      std::vector<hsize_t> start(1, a_sp-1), n2write(1,1);
      H5DSWriterDimInfo dim(start, n2write);
      H5DataSetWriter w(&s);
      w.SetDimInfoForWriting(&dim);
      int itmpToWrite = *itmp > 0 ? 0 : 1;
      w.WriteData(&itmpToWrite, 1);
    }
  }
} // expSFRLine5
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void expSFRLine6 (MfPackage *a_pSFRLine1,
                         MfPackage *a_pSFRLine6,
                         int a_sp,
                         TxtExporter *a_exp)
{
  using namespace MfData::Packages;
  using util::ForElement;
  const int *nstrm(0), *nss(0), *isfropt(0);
  const int *iseg(0), *iotsg(0), *idivar(0);
  const Real *seg(0), *xsec(0), *qstage(0);
  if (a_pSFRLine1->GetField(SFRpack::NSTRM, &nstrm) && nstrm &&
      a_pSFRLine1->GetField(SFRpack::NSS, &nss) && nss &&
      a_pSFRLine1->GetField(SFRpack::ISFROPT, &isfropt) && isfropt &&

      a_pSFRLine6->GetField(SFRpack::ISEG, &iseg) && iseg &&
      a_pSFRLine6->GetField(SFRpack::IOTSG, &iotsg) && iotsg &&
      a_pSFRLine6->GetField(SFRpack::IDIVAR, &idivar) && idivar &&
      a_pSFRLine6->GetField(SFRpack::SEG, &seg) && seg &&
      a_pSFRLine6->GetField(SFRpack::XSEC, &xsec) && xsec &&
      a_pSFRLine6->GetField(SFRpack::QSTAGE, &qstage) && qstage)
  {
    CStr f(a_exp->GetBaseFileName()), fileName, path, line;
    const char* type = "Stream (SFR2)";
    f += ".h5";
    util::StripPathFromFilename(f, fileName);

    int sz=26;
    MfData::Get().GetIntVar("SFR_SEG_SIZE", sz);
    // 14. Segment Property
    CAR_DBL2D v;
    v.SetSize(SFR2S_NPROP, *nss, 0.0);

    for (int i = 0; i < *nss; ++i)
    {
      int segnum = i + 1;
      v.at(SFR2S_ICALC, i)   = ForElement(iseg, 1, segnum, 4);
      v.at(SFR2S_OUTSEG, i)  = iotsg[i];
      v.at(SFR2S_IUPSEG, i)  = ForElement(idivar, 1, segnum, 2);
      v.at(SFR2S_IPRIOR, i)  = ForElement(idivar, 2, segnum, 2);
      v.at(SFR2S_FLOW, i)    = ForElement(seg, 2, segnum, sz);
      v.at(SFR2S_RUNOFF, i)  = ForElement(seg, 3, segnum, sz);
      v.at(SFR2S_ETSW, i)    = ForElement(seg, 4, segnum, sz);
      v.at(SFR2S_PPTSW, i)   = ForElement(seg, 5, segnum, sz);
      v.at(SFR2S_ROUGHCH, i) = ForElement(seg, 16, segnum, sz);
      v.at(SFR2S_ROUGHBK, i) = ForElement(seg, 17, segnum, sz);
      v.at(SFR2S_CDPTH, i)   = ForElement(seg, 9, segnum, sz);
      v.at(SFR2S_FDPTH, i)   = ForElement(seg, 10, segnum, sz);
      v.at(SFR2S_AWDPTH, i)  = ForElement(seg, 14, segnum, sz);
      v.at(SFR2S_BWDTH, i)   = ForElement(seg, 15, segnum, sz);
      v.at(SFR2S_HCOND1, i)  = ForElement(seg, 6, segnum, sz);
      v.at(SFR2S_THICKM1, i) = ForElement(seg, 7, segnum, sz);
      v.at(SFR2S_ELEVUP, i)  = ForElement(seg, 8, segnum, sz);
      v.at(SFR2S_WIDTH1, i)  = ForElement(seg, 9, segnum, sz);
      v.at(SFR2S_DEPTH1, i)  = ForElement(seg, 10, segnum, sz);
      v.at(SFR2S_HCOND2, i)  = ForElement(seg, 11, segnum, sz);
      v.at(SFR2S_THICKM2, i) = ForElement(seg, 12, segnum, sz);
      v.at(SFR2S_ELEVDN, i)  = ForElement(seg, 13, segnum, sz);
      v.at(SFR2S_WIDTH2, i)  = ForElement(seg, 14, segnum, sz);
      v.at(SFR2S_DEPTH2, i)  = ForElement(seg, 15, segnum, sz);
      
      for (int j = 0; j < 16; ++j)
      {
        int jj = j + 1;
        v.at(SFR2S_XSECT+j, i) = ForElement(xsec, jj, segnum, 16);
      }
      
      v.at(SFR2S_COND1FACT, i) = 1.0;
      v.at(SFR2S_COND2FACT, i) = 1.0;
    }

    // handle any sfr parameters
    {
      ParamList *list(0);
      Parameters::GetParameterList(&list);
      Param p;
      for (size_t ii=0; ii<list->Size(); ii++)
      {
        list->At(ii, &p);
        if (p.m_type == "SFR")
        { // Move the current conductance value to the condfact
          // and set conductance to the key value.
          // Must be done for both Hc1fact and Hc2fact.

          int stop = p.m_str_start + p.m_str_nbc;
          for (int i = p.m_str_start; i < stop; i++)
          {
            int nseg = ForElement(iseg, 3, i, 4);
            v.at(SFR2S_COND1FACT, nseg-1) = ForElement(seg, 6, i, sz);
            v.at(SFR2S_HCOND1, nseg-1) = p.m_key;

            v.at(SFR2S_COND2FACT, nseg-1) = ForElement(seg, 11, i, sz);
            v.at(SFR2S_HCOND2, nseg-1) = p.m_key;
          }
          p.m_str_start = -1;
          p.m_str_nbc = -1;
          list->UpdateParameter(&p);
        }
      }
    }

    // write data for 14. Segment Property
    {
      path.Format("%s/%s", type, MFBC_SEGP);
      H5DataSetWriterSetup s(f, path, H5T_NATIVE_DOUBLE, 3);
      std::vector<hsize_t> start(3, 0), n2write(3,1);
      n2write[0] = v.GetSize1();
      n2write[1] = v.GetSize2();
      start[2] = a_sp - 1;
      H5DSWriterDimInfo dim(start, n2write);
      H5DataSetWriter w(&s);
      w.SetDimInfoForWriting(&dim);
      w.WriteData(&v.at(0,0),
                  static_cast<size_t>(v.GetSize1()*v.GetSize2()));
    }

    // 15. Segment Flow Table
    // get the number of QSTAGE items for this stress period to size array
    int numQstage(0);
    for (int i = 0; i < *nss; ++i)
    {
      int segnum = i + 1;
      int icalc = ForElement(iseg, 1, segnum, 4);

      // when ICALC == 4 add NSTRPTS items
      if (icalc == 4)
        numQstage += ForElement(iseg, 2, segnum, 4);
    }
    if (numQstage > 0)
    {
      v.SetSize(5, numQstage, 0.0);

      int entryNum = 0;
      for (int i = 0; i < *nss; ++i)
      {
        int segnum = i + 1;
        if (ForElement(iseg, 1, segnum, 4) == 4)
        {
          int nstrpts = ForElement(iseg, 2, segnum, 4);
          for (int j = 0; j < nstrpts; ++j)
          {
            int jj = j + 1;
            v.at(0, entryNum+j) = segnum;
            v.at(1, entryNum+j) = a_sp;
            v.at(2, entryNum+j) = ForElement(qstage, jj, segnum, 150);

            jj += nstrpts;
            v.at(3, entryNum+j) = ForElement(qstage, jj, segnum, 150);

            jj += nstrpts;
            v.at(4, entryNum+j) = ForElement(qstage, jj, segnum, 150);
          }
          entryNum++;
        }
      }
      {
        path.Format("%s/%s", type, MFBC_SEGFLWT);

        // get size of existing data
        H5DataSetReader r(f, path);
        int tmpNrow(0);
        r.GetAtt("NumRows", tmpNrow);

        // write to end of existing data
        H5DataSetWriterSetup s(f, path, H5T_NATIVE_DOUBLE, 2);
        std::vector<hsize_t> start(2, 0), n2write(2, 1);
        n2write[0] = v.GetSize1();
        n2write[1] = v.GetSize2();
        start[1] = tmpNrow;
        H5DSWriterDimInfo dim(start, n2write);
        H5DataSetWriter w(&s);
        w.SetDimInfoForWriting(&dim);
        w.WriteData(&v.at(0,0),
                    static_cast<size_t>(v.GetSize1()*v.GetSize2()));
        int numRows = tmpNrow + numQstage;
        w.WriteAtt("NumRows", numRows);
      }
    }
  }
} // expSFRLine6
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void expMNWSetup (MfPackage *a_p,
                         TxtExporter *a_exp)
{
  using namespace MfData::Packages;
  using util::ForElement;
  const int *mxwel2(0), *iwl2cb(0), *iwelpt(0), *kspref(0), *iowell2(0);
  const double *ploss(0);
  const char *ftag(0);
  if (a_p->GetField(MNWpack::MXWEL2, &mxwel2) && mxwel2 &&
      a_p->GetField(MNWpack::IWL2CB, &iwl2cb) && iwl2cb &&
      a_p->GetField(MNWpack::IWELPT, &iwelpt) && iwelpt &&
      a_p->GetField(MNWpack::KSPREF, &kspref) && kspref &&
      a_p->GetField(MNWpack::PLoss, &ploss) && ploss &&
      a_p->GetField(MNWpack::IOWELL2, &iowell2) && iowell2 &&
      a_p->GetField(MNWpack::FTAG, &ftag) && ftag)
  {
    CStr line, f(a_exp->GetBaseFileName()), fileName;
    const char *type = "Multi-Node Well";
    CStr path;
    f += ".h5";
    util::StripPathFromFilename(f, fileName);

    a_exp->WriteLineToFile(MNW, "#GMS_HDF5_01");

    // line 1: MXMNW IWL2CB IWELPT REFerence SP:kspref
    line.Format("%d %d %d REFERENCE SP:%d", *mxwel2, *iwl2cb, *iwelpt, *kspref);
    a_exp->WriteLineToFile(MNW, line);

    // line 2: LOSSTYPE (PLossMNW)
    if (*ploss < 0.99)
      a_exp->WriteLineToFile(MNW, "SKIN");
    else if (*ploss > 1.001)
    {
      line.Format("NONLINEAR %s", STR(*ploss));
      a_exp->WriteLineToFile(MNW, line);
    }
    else
      a_exp->WriteLineToFile(MNW, "LINEAR");

    // line 3a:FILE:filename WEL1:iunw1
    // line 3b:FILE:filename BYNODE:iunby (ALLTIME)
    // line 3c:FILE:filename QSUM:iunqs (ALLTIME)
    std::map<CStr, int> auxiliaryUnits;
    CStr tag1(ftag, 6);
    CStr tag2(ftag+6, 6);
    CStr tag3(ftag+12, 6);
    auxiliaryUnits[tag1.Trim().ToUpper()] = iowell2[0];
    auxiliaryUnits[tag2.Trim().ToUpper()] = iowell2[1];
    auxiliaryUnits[tag3.Trim().ToUpper()] = iowell2[2];
    vector<int> iowell2Sorted(3, 0);
    if (auxiliaryUnits["WEL1"] != 0)
    {
      line.Format("FILE:MNW-WEL1.out WEL1:%d", abs(auxiliaryUnits["WEL1"]));
      a_exp->WriteLineToFile(MNW, line);
      iowell2Sorted[0] = auxiliaryUnits["WEL1"];
    }

    if (auxiliaryUnits["BYNODE"] != 0)
    {
      line.Format("FILE:MNW-BYNODE.out BYNODE:%d",
                  abs(auxiliaryUnits["BYNODE"]));
      if (auxiliaryUnits["BYNODE"] < 0)
        line += " ALLTIME";
      a_exp->WriteLineToFile(MNW, line);
      iowell2Sorted[1] = auxiliaryUnits["BYNODE"];
    }

    if (auxiliaryUnits["QSUM"] != 0)
    {
      line.Format("FILE:MNW-QSUM.out QSUM:%d", abs(auxiliaryUnits["QSUM"]));
      if (auxiliaryUnits["QSUM"] < 0)
        line += " ALLTIME";
      a_exp->WriteLineToFile(MNW, line);
      iowell2Sorted[2] = auxiliaryUnits["QSUM"];
    }

    // update "21. Stress Period Ref"
    path.Format("%s/%s", type, MFBC_KSPREF);
    WriteSingleH5IntValue(f, path, *kspref);

    // update "22. Loss Type"
    path.Format("%s/%s", type, MFBC_LOSSTYPE);
    WriteSingleH5DoubleValue(f, path, *ploss);

    // update "23. Well IO"
    path.Format("%s/%s", type, MFBC_IOWELL2);
    Write1DIntArray(f, path, &iowell2Sorted[0], 3);
  }
} // expMNWSetup
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
WellPropertyList::WellPropertyList()
: m_nextWellNumber(0)
{
} // WellPropertyList::WellPropertyList
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void WellPropertyList::NewStressPeriod()
{
  m_used.clear();
  m_used.resize(m_cellIds.size(), false);
} // WellPropertyList::NewStressPeriod
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
std::vector<int> WellPropertyList::GetPropIndicees(const CStr& a_siteName,
                                                   const vector<int>& a_cellids)
{
  size_t startIndex;

  if (!a_siteName.empty() && MissingSiteName(a_siteName))
  {
    AppendSiteWell(a_siteName, a_cellids, startIndex);
    return BuildPropIndicees(startIndex, a_cellids);
  }
  else if (!a_siteName.empty() && 
           MatchingSiteCells(a_siteName, a_cellids, startIndex))
  {
    return BuildPropIndicees(startIndex, a_cellids);
  }
  else if (FoundCellSet(a_cellids, startIndex))
  {
    return BuildPropIndicees(startIndex, a_cellids);
  }
  else
  {
    AppendNewWell(a_cellids, startIndex);
    return BuildPropIndicees(startIndex, a_cellids);
  }
} // WellPropertyList::GetPropIndicees
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
const vector<int>& WellPropertyList::GetCellIds ()
{
  return m_cellIds;
} // WellPropertyList::GetCellIds
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
const vector<int>& WellPropertyList::GetWellIds ()
{
  return m_wellIds;
} // WellPropertyList::GetWellIds
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
const vector<CStr>& WellPropertyList::GetWellNames ()
{
  return m_names;
} // WellPropertyList::GetWellNames
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
const vector<char>& WellPropertyList::GetIsSiteName ()
{
  return m_isSiteName;
} // WellPropertyList::GetWellNames
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool WellPropertyList::MissingSiteName(const CStr& a_siteName)
{
  std::vector<CStr>::const_iterator nameIter = 
    std::find_if(m_names.begin(), m_names.end(),
                 util::CaseInsensitiveEqual(a_siteName));
  return nameIter == m_names.end();
} // WellPropertyList::MissingSiteName
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool WellPropertyList::MatchingSiteCells(const CStr& a_siteName,
                                         const vector<int>& a_cellids,
                                         size_t& a_startIndex)
{
  bool found = false;

  // find first item with site name
  std::vector<CStr>::const_iterator nameIter = 
    std::find_if(m_names.begin(), m_names.end(),
                 util::CaseInsensitiveEqual(a_siteName));
  if (nameIter != m_names.end())
  {
    a_startIndex = nameIter - m_names.begin();
    if (!m_used[a_startIndex])
    {
      // build a set of existing cellids with the site name
      std::vector<CStr>::const_iterator nameEndIter = nameIter;
      util::CaseInsensitiveEqual equalToSiteName(a_siteName);
      for (; nameEndIter != m_names.end() && equalToSiteName(*nameEndIter);
           ++nameEndIter)
      {
      }
      std::set<int> existingCellIds(m_cellIds.begin() + a_startIndex, 
                                    m_cellIds.begin() + a_startIndex + 
                                    (nameEndIter - nameIter));

      // build a set of the checkedCellIds
      std::set<int> checkedCellIds(a_cellids.begin(), a_cellids.end());

      found = checkedCellIds == existingCellIds;
    }
  }
  return found;
} // WellPropertyList::MatchingSiteCells
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
vector<int> WellPropertyList::BuildPropIndicees(size_t a_startIndex,
                                                const vector<int>& a_cellids)
{
  vector<int> properties;
  std::map<int, int> cellIdsToProp;

  for (size_t i = 0; i < a_cellids.size(); ++i)
  {
    size_t propIndex = a_startIndex + i;
    cellIdsToProp[m_cellIds[propIndex]] = (int)propIndex;
    m_used[a_startIndex + i] = true;
  }

  for (std::vector<int>::const_iterator cell = a_cellids.begin();
       cell != a_cellids.end(); ++cell)
  {
    properties.push_back(cellIdsToProp[*cell]);
  }

  return properties;
} // WellPropertyList::BuildPropIndicees
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void WellPropertyList::AppendSiteWell(const CStr& a_siteName,
                                                  const vector<int>& a_cellids,
                                                  size_t& a_startIndex)
{
  std::vector<int> properties;
  a_startIndex = m_cellIds.size();
  m_nextWellNumber++;
  for (std::vector<int>::const_iterator cellId = a_cellids.begin();
       cellId != a_cellids.end(); ++cellId)
  {
    int wellId;
    if (a_cellids.size() == 1)
      wellId = 0;
    else
      wellId = m_nextWellNumber;
    AppendItem(*cellId, a_siteName, true, wellId);
  }
} // WellPropertyList::AppendSiteWell
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool WellPropertyList::FoundCellSet(const vector<int>& a_cellids,
                                    size_t& a_startIndex)
{
  bool found = false;

  // build cell set to find
  set<int> cellsToFind(a_cellids.begin(), a_cellids.end());

  CStr currSetName;
  set<int> cellsToCheck;
  for (size_t i = 0; i < m_names.size(); ++i)
  {
    if (!m_isSiteName[i] && !m_used[i])
    {
      if (m_names[i] != currSetName)
      {
        if (cellsToCheck == cellsToFind)
        {
          found = true;
          break;
        }
        a_startIndex = i;
        currSetName = m_names[i];
        cellsToCheck.clear();
      }
      cellsToCheck.insert(m_cellIds[i]);
    }
  }

  if (cellsToCheck == cellsToFind)
    found = true;

  return found;
} // WellPropertyList::FoundCellSet
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void WellPropertyList::AppendNewWell(const vector<int>& a_cellids,
                                     size_t& a_startIndex)
{
  // find an unused name
  CStr wellName;
  wellName.Format("Well-%d", ++m_nextWellNumber);
  vector<CStr>::iterator nameIter = std::find_if(m_names.begin(), m_names.end(),
                                          util::CaseInsensitiveEqual(wellName));
  while (nameIter != m_names.end())
  {
    wellName.Format("Well-%d", ++m_nextWellNumber);
    nameIter = std::find_if(m_names.begin(), m_names.end(),
                            util::CaseInsensitiveEqual(wellName));
  }

  // get index where new names will start
  a_startIndex = m_names.size();

  // add name and cellids
  for (vector<int>::const_iterator cellId = a_cellids.begin();
       cellId != a_cellids.end(); ++cellId)
  {
    int wellId;
    if (a_cellids.size() == 1)
      wellId = 0;
    else
      wellId = m_nextWellNumber;
    AppendItem(*cellId, wellName, false, wellId);
  }
} // WellPropertyList::AppendNewWell
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void WellPropertyList::AppendItem(int a_cellId,
                                  const CStr& a_name,
                                  bool a_isSite,
                                  int  a_wellNumber)
{
    m_cellIds.push_back(a_cellId);
    m_wellIds.push_back(a_wellNumber);
    m_names.push_back(a_name);
    m_isSiteName.push_back(a_isSite);
    m_used.push_back(false);
}
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void expMNWStressPeriod (MfPackage *a_p,
                               int a_sp,
                               TxtExporter *a_exp)
{
  using namespace MfData::Packages;
  using std::map;
  using std::set;
  using std::vector;
  using util::ForElement;
  const int *itmp(0), *nwell2(0);
  const double *well2(0), *mnwflgs(0);
  const char *mnwsite(0);
  const int WELL2_SIZE = 18;
  if (a_p->GetField(MNWpack::ITMP, &itmp) && itmp &&
      a_p->GetField(MNWpack::NWELL2, &nwell2) && nwell2 &&
      a_p->GetField(MNWpack::WELL2, &well2) && well2 &&
      a_p->GetField(MNWpack::MNWSITE, &mnwsite) && mnwsite &&
      a_p->GetField(MNWpack::MNWFLGS, &mnwflgs) && mnwflgs)
  {
    CStr line, f(a_exp->GetBaseFileName()), fileName;
    const char *type = "Multi-Node Well";
    CStr path;
    f += ".h5";
    util::StripPathFromFilename(f, fileName);

    // line 4. ITMP (ADD)
    // not handling ADD so use nwell2 if itmp > 0
    int useLast(*itmp);
    if (useLast > 0)
      useLast = *nwell2;
    line.Format("%d", useLast);
    a_exp->WriteLineToFile(MNW, line);

    // line 5. as HDF5
    if (*itmp > 0)
    {
      line.Format("GMS_HDF5_MNW \"%s\" \"MNW1\" %d", fileName, a_sp);
      a_exp->WriteLineToFile(MNW, line);
    }

    // update use last
    path.Format("%s/%s", type, MFBC_USELAST);
    Write1DH5Value(f, path, a_sp-1, useLast < 0 ? 1 : 0);

    WellPropertyList& wpl = a_exp->m_public->m_WellPropertyList;
    wpl.NewStressPeriod();
    vector<int> properties;

    if (*nwell2 != 0)
    {

      // get a list of indicees into the properties array for each line 5
      // row from well2 and append to properties array
      vector<int> wellCellIds;
      vector<int> wellProperties;
      int currWellNum = static_cast<int>(ForElement(well2, 18, 1, 18));
      int firstWell2Index = 1;
      for (int i = 1; i <= *nwell2; ++i)
      {
        int cell = static_cast<int>(ForElement(well2, 1, i, 18));
        int wellNum = static_cast<int>(ForElement(well2, 18, i, 18));
        if (wellNum != currWellNum)
        {
          // write properties for well
          CStr siteName(mnwsite + 32*(firstWell2Index-1), 32);
          siteName.Trim();
          if (siteName == "NO-PRINT")
            siteName = "";
          wellProperties = wpl.GetPropIndicees(siteName, wellCellIds);
          properties.insert(properties.end(), wellProperties.begin(),
                            wellProperties.end());

          wellCellIds.clear();
          wellProperties.clear();
          currWellNum = wellNum;
          firstWell2Index = i;
        }
        wellCellIds.push_back(cell);
      }

      // get properties for last well
      CStr siteName(mnwsite + 32*(firstWell2Index-1), 32);
      siteName.Trim();
      if (siteName == "NO-PRINT")
        siteName = "";
      wellProperties = wpl.GetPropIndicees(siteName, wellCellIds);
      properties.insert(properties.end(), wellProperties.begin(),
                        wellProperties.end());
    }

    if (wpl.GetCellIds().size() != 0)
    {
      // save mnwflgs in case using last next stress period
      std::vector<double>& mnwFlgsCopy = a_exp->m_public->m_mnwFlgsCopy;
      if (a_sp == 1 || *itmp > 0)
      {
        mnwFlgsCopy.clear();
        mnwFlgsCopy.insert(mnwFlgsCopy.begin(), mnwflgs, 
                           mnwflgs+mnw::MNWFLGS_SIZE*(*nwell2));
      }

      // set property values
      CAR_DBL2D v;
      v.SetSize(mnw::H5_SIZE, (int)wpl.GetCellIds().size(), 0.0);
      for (int i = 1; i <= *nwell2; ++i)
      {
        int j = properties.at(i-1);
        double *flgs = &mnwFlgsCopy.at(0);
        v.at(mnw::H5_ACTIVE, j) = mnw::ACTIVE;
        double q = ForElement(flgs, mnw::MNWFLGS_QDES, i, mnw::MNWFLGS_SIZE);
        v.at(mnw::H5_QDES, j)   = q;
        v.at(mnw::H5_WELLID, j) = wpl.GetWellIds().at(j);
        v.at(mnw::H5_QWVAL, j)  = ForElement(well2, 4, i, WELL2_SIZE);
        v.at(mnw::H5_RW, j)     = ForElement(well2, 5, i, WELL2_SIZE);
        v.at(mnw::H5_SKIN, j)   = ForElement(well2, 6, i, WELL2_SIZE);
        if (ForElement(flgs, mnw::MNWFLGS_DD, i, mnw::MNWFLGS_SIZE) ==
          mnw::DD_NONE)
        {
          v.at(mnw::H5_HLIM, j) = 0.0;
          v.at(mnw::H5_HREF, j) = 0.0;
        }
        else
        {
          v.at(mnw::H5_HLIM, j)   = ForElement(flgs, mnw::MNWFLGS_HLIM, i, 
                                               mnw::MNWFLGS_SIZE);
          v.at(mnw::H5_HREF, j)   = ForElement(flgs, mnw::MNWFLGS_HREF, i,
                                               mnw::MNWFLGS_SIZE);
        }
        v.at(mnw::H5_DD, j)     = ForElement(flgs, mnw::MNWFLGS_DD, i,
                                             mnw::MNWFLGS_SIZE);
        if (util::lrint(ForElement(flgs, mnw::MNWFLGS_IERR, i,
                                   mnw::MNWFLGS_SIZE)) >= 1)
          v.at(mnw::H5_IWGRP, j) = -1;
        else
          v.at(mnw::H5_IWGRP, j) = ForElement(well2, 9, i, WELL2_SIZE);
        v.at(mnw::H5_C, j)      = ForElement(well2, 16, i, WELL2_SIZE);
        int qcut = util::lrint(ForElement(flgs, mnw::MNWFLGS_QCUT, i,
                                          mnw::MNWFLGS_SIZE));
        v.at(mnw::H5_QCUT, j)   = qcut;
        //if (v.at(mnw::H5_QDES, j) == 0)
        //{ // if the Q is zero then the MNW packages doesn't read these values
        //  // the values may have been set if the user specifies "DEFAULT" but
        //  // the package code ignores the values
        //  v.at(mnw::H5_QFRCMN, j) = 0;
        //  v.at(mnw::H5_QFRCMX, j) = 0;
        //}
        //else if (qcut == 1)
        if (qcut == 1)
        {
          v.at(mnw::H5_QFRCMN, j) = ForElement(well2, 13, i, WELL2_SIZE)*q;
          v.at(mnw::H5_QFRCMX, j) = ForElement(well2, 14, i, WELL2_SIZE)*q;
        }
        else if (qcut == 2)
        {
          v.at(mnw::H5_QFRCMN, j) = ForElement(well2, 13, i, WELL2_SIZE)*100;
          v.at(mnw::H5_QFRCMX, j) = ForElement(well2, 14, i, WELL2_SIZE)*100;
        }
        v.at(mnw::H5_SITE, j)   = wpl.GetIsSiteName().at(j) ? mnw::SITE_PRINT : 
                                                          mnw::SITE_DONT_PRINT;
      }

      // write properties
      {
        path.Format("%s/%s", type, MFBC_DATA);
        H5DataSetWriterSetup s(f, path, H5T_NATIVE_DOUBLE, 3);
        std::vector<hsize_t> start(3, 0), n2write(3,1);
        n2write[0] = v.GetSize1();
        n2write[1] = v.GetSize2();
        start[2] = a_sp - 1;
        H5DSWriterDimInfo dim(start, n2write);
        H5DataSetWriter w(&s);
        w.SetDimInfoForWriting(&dim);
        w.WriteData(&v.at(0,0),
                    static_cast<size_t>(v.GetSize1()*v.GetSize2()));
      }

      // update number of boundary conditions
      path.Format("%s/%s", type, MFBC_NUMBC);
      WriteSingleH5IntValue(f, path, (int)wpl.GetCellIds().size());

      // update cellids
      path.Format("%s/%s", type, MFBC_CELLIDS);
      Write1DIntArray(f, path, &wpl.GetCellIds()[0], wpl.GetCellIds().size());

      // update names
      path.Format("%s/%s", type, MFBC_NAME);
      WriteH5StringArray(f, path, wpl.GetWellNames());

      // update iface
      std::vector<int> iface(wpl.GetCellIds().size(), 0);
      path.Format("%s/%s", type, MFBC_IFACE);
      Write1DIntArray(f, path, &iface[0], iface.size());

      // update mapids
      std::vector<CStr> mapids(wpl.GetCellIds().size(), CStr());
      path.Format("%s/%s", type, MFBC_MAPIDSTR);
      WriteH5StringArray(f, path, mapids);

    }
  }
} // expMNWStressPeriod
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void iMnw2Line1 (MfPackage *a_p)
{
  using namespace MfData::Packages;
  const int *MNWMAX(0),*IWL2CB(0),*MNWPRNT(0),*NAUX(0);
  const char *MNWAUX(0);

  if (!a_p->GetField(MNW2pack::MNWMAX, &MNWMAX) || !MNWMAX ||
      !a_p->GetField(MNW2pack::IWL2CB, &IWL2CB) || !IWL2CB ||
      !a_p->GetField(MNW2pack::MNWPRNT, &MNWPRNT) || !MNWPRNT ||
      !a_p->GetField(MNW2pack::NAUX, &NAUX) || !NAUX ||
      !a_p->GetField(MNW2pack::MNWAUX, &MNWAUX) || !MNWAUX)
  {
    ASSERT(0);
    return;
  }

  std::vector<CStr>& lines(a_p->StringsToWrite());
  std::vector<CStr>& desc(a_p->StringDescriptions());

  desc.push_back("1.    MNWMAX,IWL2CB,MNWPRNT,{OPTION}");
  CStr line;
  line.Format("%d %d %d", *MNWMAX, *IWL2CB, *MNWPRNT);

  // copy the aux variable names
  std::vector<CStr> auxNames;
  char              tmpAux[17];
  int               i, j, cnt(0);
  tmpAux[16] = '\0';
  for (i=0; i<*NAUX; i++)
  {
    for (j=0; j<16; j++)
    {
      tmpAux[j] = MNWAUX[cnt++];
    }
    auxNames.push_back(tmpAux);
    auxNames.back().Trim();
  }

  for (i=0; i<*NAUX; i++)
  {
    line += " AUX ";
    line += auxNames[i];
  }
  lines.push_back(line);
} // iMnw2Line1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void iMnw2Line2ab (MfPackage *a_p)
{
  using namespace MfData::Packages;
  const int * NNODES(0),* PUMPLOC(0),* Qlimit(0),* PPFLAG(0),* PUMPCAP(0);
  const char* WELLID(0),*LOSSTYPE(0);
  if (!a_p->GetField(MNW2pack::WELLID, &WELLID) || !WELLID ||
      !a_p->GetField(MNW2pack::NNODES, &NNODES) || !NNODES ||
      !a_p->GetField(MNW2pack::LOSSTYPE, &LOSSTYPE) || !LOSSTYPE ||
      !a_p->GetField(MNW2pack::PUMPLOC, &PUMPLOC) || !PUMPLOC ||
      !a_p->GetField(MNW2pack::Qlimit, &Qlimit) || !Qlimit ||
      !a_p->GetField(MNW2pack::PPFLAG, &PPFLAG) || !PPFLAG ||
      !a_p->GetField(MNW2pack::PUMPCAP, &PUMPCAP) || !PUMPCAP)
  {
    ASSERT(0);
    return;
  }

  std::vector<CStr>& lines(a_p->StringsToWrite());
  std::vector<CStr>& desc(a_p->StringDescriptions());
  CStr line, wellid(WELLID);
  if (wellid.find(" ") != -1)
  {
    wellid.Format("'%s'", WELLID);
  }
  line.Format("%s %d", wellid, *NNODES);
  lines.push_back(line);
  desc.push_back("2a.   WELLID,NNODES");

  line.Format("%s %d %d %d %d", LOSSTYPE,*PUMPLOC,*Qlimit,*PPFLAG,*PUMPCAP);
  lines.push_back(line);
  desc.push_back("2b.   LOSSTYPE,PUMPLOC,Qlimit,PPFFLAG,PUMPCAP");
} // iMnw2Line2ab
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
static void iMnw2LineToSet (const char *LnDesc,
                            std::set<CStr>& a_set)
{
  a_set.clear();
  std::stringstream os;
  std::string str;
  os << LnDesc;

  while (std::getline(os, str, ','))
  {
    if (!str.empty())
    {
      a_set.insert(str.c_str());
    }
  }
} // iMnw2LineToSet
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void iMnw2Line2c (MfPackage *a_p)
{
  using namespace MfData::Packages;
  const double* Rw(0),* Rskin(0),* Kskin(0),* B(0),* C(0),* P(0),* CWC(0);
  const char* LnDesc1(0);

  if (!a_p->GetField(MNW2pack::Rw, &Rw) || !Rw ||
      !a_p->GetField(MNW2pack::Rskin, &Rskin) || !Rskin ||
      !a_p->GetField(MNW2pack::Kskin, &Kskin) || !Kskin ||
      !a_p->GetField(MNW2pack::B, &B) || !B ||
      !a_p->GetField(MNW2pack::C, &C) || !C ||
      !a_p->GetField(MNW2pack::P, &P) || !P ||
      !a_p->GetField(MNW2pack::CWC, &CWC) || !CWC ||
      !a_p->GetField(MNW2pack::LnDesc, &LnDesc1) || !LnDesc1)
  {
    ASSERT(0);
    return;
  }

  std::set<CStr> aSet;
  CStr LnDesc(util::GetStr(LnDesc1, 200));
  // parse line description into a set of strings
  iMnw2LineToSet(LnDesc, aSet);

  std::vector<CStr>& lines(a_p->StringsToWrite());
  std::vector<CStr>& desc(a_p->StringDescriptions());
  std::stringstream os;
  if (aSet.find("Rw") != aSet.end())
    os << STR(*Rw) << " ";
  if (aSet.find("Rskin") != aSet.end())
    os << STR(*Rskin) << " ";
  if (aSet.find("Kskin") != aSet.end())
    os << STR(*Kskin) << " ";
  if (aSet.find("B") != aSet.end())
    os << STR(*B) << " ";
  if (aSet.find("C") != aSet.end())
    os << STR(*C) << " ";
  if (aSet.find("P") != aSet.end())
    os << STR(*P) << " ";
  if (aSet.find("CWC") != aSet.end())
    os << STR(*CWC) << " ";

  CStr line = os.str().c_str();
  if (!line.empty())
  {
    lines.push_back(line);
    line.Format("2c.   %s", LnDesc);
    desc.push_back(line);
  }
} // iMnw2Line2c
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void iMnw2Line2d (MfPackage *a_p)
{
  using namespace MfData::Packages;
  const int* IL(0),* IR(0),* IC(0);
  const double* RwNode(0),* RskinNode(0),* KskinNode(0),* BNode(0),
              * CNode(0),* PNode(0),
              * CWCNode(0),* PP(0),* Ztop(0),* Zbotm(0);
  const char* LnDesc1(0);

  if (!a_p->GetField(MNW2pack::IR, &IR) || !IR ||
      !a_p->GetField(MNW2pack::IC, &IC) || !IC ||
      !a_p->GetField(MNW2pack::RwNode, &RwNode) || !RwNode ||
      !a_p->GetField(MNW2pack::RskinNode, &RskinNode) || !RskinNode ||
      !a_p->GetField(MNW2pack::KskinNode, &KskinNode) || !KskinNode ||
      !a_p->GetField(MNW2pack::BNode, &BNode) || !BNode ||
      !a_p->GetField(MNW2pack::CNode, &CNode) || !CNode ||
      !a_p->GetField(MNW2pack::PNode, &PNode) || !PNode ||
      !a_p->GetField(MNW2pack::CWCNode, &CWCNode) || !CWCNode ||
      !a_p->GetField(MNW2pack::PP, &PP) || !PP ||
      !a_p->GetField(MNW2pack::LnDesc, &LnDesc1) || !LnDesc1)
  {
    ASSERT(0);
    return;
  }
  if (!a_p->GetField(MNW2pack::IL, &IL) || !IL)
  {
    if (!a_p->GetField(MNW2pack::Ztop, &Ztop) || !Ztop ||
        !a_p->GetField(MNW2pack::Zbotm, &Zbotm) || !Zbotm)
    {
      ASSERT(0);
      return;
    }
  }

  std::set<CStr> aSet;
  CStr LnDesc(util::GetStr(LnDesc1, 200));
  // parse line description into a set of strings
  iMnw2LineToSet(LnDesc, aSet);

  std::vector<CStr>& lines(a_p->StringsToWrite());
  std::vector<CStr>& desc(a_p->StringDescriptions());
  std::stringstream os;
  CStr label("2d-1.");
  if (IL)
  {
    os << *IL << " ";
  }
  else
  {
    label = "2d-2.";
    os << STR(*Ztop) << " " << STR(*Zbotm) << " ";
  }
  os  << *IR << " " << *IC << " ";

  if (aSet.find("RwNode") != aSet.end())
    os << STR(*RwNode) << " ";
  if (aSet.find("RskinNode") != aSet.end())
    os << STR(*RskinNode) << " ";
  if (aSet.find("KskinNode") != aSet.end())
    os << STR(*KskinNode) << " ";
  if (aSet.find("BNode") != aSet.end())
    os << STR(*BNode) << " ";
  if (aSet.find("CNode") != aSet.end())
    os << STR(*CNode) << " ";
  if (aSet.find("PNode") != aSet.end())
    os << STR(*PNode) << " ";
  if (aSet.find("CWCNode") != aSet.end())
    os << STR(*CWCNode) << " ";
  if (aSet.find("PP") != aSet.end())
    os << STR(*PP) << " ";

  CStr line = os.str().c_str();
  if (!line.empty())
  {
    lines.push_back(line);
    line.Format("%s %s", label, LnDesc);
    desc.push_back(line);
  }
} // iMnw2Line2d
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void iMnw2Line2e (MfPackage *a_p)
{
  using namespace MfData::Packages;
  const int* PUMPLAY(0), * PUMPROW(0),* PUMPCOL(0);
  const double* Zpump(0);
  const char* LnDesc1(0);

  if (!a_p->GetField(MNW2pack::PUMPLAY, &PUMPLAY) || !PUMPLAY ||
      !a_p->GetField(MNW2pack::PUMPROW, &PUMPROW) || !PUMPROW ||
      !a_p->GetField(MNW2pack::PUMPCOL, &PUMPCOL) || !PUMPCOL ||
      !a_p->GetField(MNW2pack::Zpump, &Zpump) || !Zpump ||
      !a_p->GetField(MNW2pack::LnDesc, &LnDesc1) || !LnDesc1)
  {
    ASSERT(0);
    return;
  }

  std::set<CStr> aSet;
  CStr LnDesc(util::GetStr(LnDesc1, 200));
  // parse line description into a set of strings
  iMnw2LineToSet(LnDesc, aSet);

  std::vector<CStr>& lines(a_p->StringsToWrite());
  std::vector<CStr>& desc(a_p->StringDescriptions());
  std::stringstream os;
  if (aSet.find("PUMPLAY") != aSet.end())
    os << *PUMPLAY << " ";
  if (aSet.find("PUMPROW") != aSet.end())
    os << *PUMPROW << " ";
  if (aSet.find("PUMPCOL") != aSet.end())
    os << *PUMPCOL << " ";
  if (aSet.find("Zpump") != aSet.end())
    os << STR(*Zpump) << " ";

  CStr line = os.str().c_str();
  if (!line.empty())
  {
    lines.push_back(line);
    line.Format("2e.   %s", LnDesc);
    desc.push_back(line);
  }
} // iMnw2Line2e
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void iMnw2Line2f (MfPackage *a_p)
{
  using namespace MfData::Packages;
  const double* Hlim(0),* Qfrcmn(0),* Qfrcmx(0);
  const int* QCUT(0);
  const char* LnDesc1(0);
  if (!a_p->GetField(MNW2pack::Hlim, &Hlim) || !Hlim ||
      !a_p->GetField(MNW2pack::QCUT, &QCUT) || !QCUT ||
      !a_p->GetField(MNW2pack::Qfrcmn, &Qfrcmn) || !Qfrcmn ||
      !a_p->GetField(MNW2pack::Qfrcmx, &Qfrcmx) || !Qfrcmx ||
      !a_p->GetField(MNW2pack::LnDesc, &LnDesc1) || !LnDesc1)
  {
    ASSERT(0);
    return;
  }

  std::set<CStr> aSet;
  CStr LnDesc(util::GetStr(LnDesc1, 200));
  // parse line description into a set of strings
  iMnw2LineToSet(LnDesc, aSet);

  std::vector<CStr>& lines(a_p->StringsToWrite());
  std::vector<CStr>& desc(a_p->StringDescriptions());
  std::stringstream os;
  if (aSet.find("Hlim") != aSet.end())
    os << STR(*Hlim) << " ";
  if (aSet.find("QCUT") != aSet.end())
    os << *QCUT << " ";
  if (aSet.find("Qfrcmn") != aSet.end())
    os << STR(*Qfrcmn) << " ";
  if (aSet.find("Qfrcmx") != aSet.end())
    os << STR(*Qfrcmx) << " ";

  CStr line = os.str().c_str();
  if (!line.empty())
  {
    lines.push_back(line);
    line.Format("2f.   %s", LnDesc);
    desc.push_back(line);
  }
} // iMnw2Line2f
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void iMnw2Line2g (MfPackage *a_p)
{
  using namespace MfData::Packages;
  const double* Hlift(0),* LIFTq0(0),* LIFTqdes(0),* HWtol(0);
  const char* LnDesc1(0);
  if (!a_p->GetField(MNW2pack::Hlift, &Hlift) || !Hlift ||
      !a_p->GetField(MNW2pack::LIFTq0, &LIFTq0) || !LIFTq0 ||
      !a_p->GetField(MNW2pack::LIFTqdes, &LIFTqdes) || !LIFTqdes ||
      !a_p->GetField(MNW2pack::HWtol, &HWtol) || !HWtol ||
      !a_p->GetField(MNW2pack::LnDesc, &LnDesc1) || !LnDesc1)
  {
    ASSERT(0);
    return;
  }

  std::set<CStr> aSet;
  CStr LnDesc(util::GetStr(LnDesc1, 200));
  // parse line description into a set of strings
  iMnw2LineToSet(LnDesc, aSet);

  std::vector<CStr>& lines(a_p->StringsToWrite());
  std::vector<CStr>& desc(a_p->StringDescriptions());
  std::stringstream os;
  if (aSet.find("Hlift") != aSet.end())
    os << STR(*Hlift) << " ";
  if (aSet.find("LIFTq0") != aSet.end())
    os << *LIFTq0 << " ";
  if (aSet.find("LIFTqdes") != aSet.end())
    os << STR(*LIFTqdes) << " ";
  if (aSet.find("HWtol") != aSet.end())
    os << STR(*HWtol) << " ";

  CStr line = os.str().c_str();
  if (!line.empty())
  {
    lines.push_back(line);
    line.Format("2g.   %s", LnDesc);
    desc.push_back(line);
  }
} // iMnw2Line2g
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void iMnw2Line2h (MfPackage *a_p)
{
  using namespace MfData::Packages;
  const double* Liftn(0),* Qn(0);
  const char* LnDesc1(0);
  if (!a_p->GetField(MNW2pack::Liftn, &Liftn) || !Liftn ||
      !a_p->GetField(MNW2pack::Qn, &Qn) || !Qn ||
      !a_p->GetField(MNW2pack::LnDesc, &LnDesc1) || !LnDesc1)
  {
    ASSERT(0);
    return;
  }

  std::set<CStr> aSet;
  CStr LnDesc(util::GetStr(LnDesc1, 200));
  // parse line description into a set of strings
  iMnw2LineToSet(LnDesc, aSet);

  std::vector<CStr>& lines(a_p->StringsToWrite());
  std::vector<CStr>& desc(a_p->StringDescriptions());
  std::stringstream os;
  if (aSet.find("Liftn") != aSet.end())
    os << STR(*Liftn) << " ";
  if (aSet.find("Qn") != aSet.end())
    os << STR(*Qn) << " ";

  CStr line = os.str().c_str();
  if (!line.empty())
  {
    lines.push_back(line);
    line.Format("2h.   %s", LnDesc);
    desc.push_back(line);
  }

} // iMnw2Line2h
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void iWriteStoredLines (const char* a_package, MfPackage *a_p,
                               TxtExporter *a_exp)
{
  std::vector<CStr>& lines(a_p->StringsToWrite());
  std::vector<CStr>& desc(a_p->StringDescriptions());
  a_exp->WriteLinesAndDescriptionsToFile(a_package, lines, desc);
#if 0
  size_t len=40, diff;
  std::vector<CStr>& lines(a_p->StringsToWrite());
  std::vector<CStr>& desc(a_p->StringDescriptions());
  if (lines.size() != desc.size())
  {
    ASSERT(0);
    return;
  }
  // see what the max string length is
  for (size_t i=0; i<lines.size(); i++)
  {
    if (len < lines[i].size())
      len = lines[i].size();
  }

  CStr line;
  for (size_t i=0; i<lines.size(); i++)
  {
    // write the line
    line = lines[i];
    diff = len - line.size();
    if (diff > 0)
    {
      // buffer the text out to the max
      CStr buff(diff, ' ');
      line += buff;
    }

    // write the line description
    line += "# ";
    line += desc[i];

    a_exp->WriteLineToFile(a_package, line);
  }
  lines.clear();
  desc.clear();
#endif
} // iWriteStoredLines
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void iWriteMnw2Lines12 (MfPackage *a_p,
                               TxtExporter *a_exp)
{
  using namespace MfData::Packages;
  CStr comments;
  // write any comments
  GetComments(MNW2, comments);
  if (!comments.empty())
  {
    a_exp->WriteLineToFile(MNW2, comments);
  }

  iWriteStoredLines(MNW2, a_p, a_exp);

} // iWriteMnw2Lines12
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void iWriteMnw2Lines34 (MfPackage *a_p,
                               TxtExporter *a_exp,
                               int a_sp)
{
  using namespace MfData::Packages;
  using util::ForElement;

  try
  {
    const int* ITMP(0),* NMNWVL(0),* MNWMAX(0),* NAUX(0);
    const double* MNW2d(0);
    if (!a_p->GetField(MNW2pack::ITMP, &ITMP) || !ITMP ||
        !a_p->GetField(MNW2pack::MNW2, &MNW2d) || !MNW2d ||
        !a_p->GetField(MNW2pack::NMNWVL, &NMNWVL) || !NMNWVL ||
        !a_p->GetField(MNW2pack::MNWMAX, &MNWMAX) || !MNWMAX ||
        !a_p->GetField(MNW2pack::NAUX, &NAUX) || !NAUX)
    {
      ASSERT(0);
      return;
    }
    size_t diff(1);
    CStr line;
    // write the line with ITMP
    line.Format("%d", *ITMP);
    if (line.size() < 80)
    {
      diff = 50 - line.size();
    }
    CStr buff(diff, ' ');
    line += buff;
    buff.Format("# ITMP (SP%d)", a_sp);
    line += buff;
    a_exp->WriteLineToFile(MNW2, line.c_str());

    CStr f(a_exp->GetBaseFileName()), path;
    f += ".h5";
    {// write the use last flag
      path.Format("%s/%s", "MNW2", MFBC_USELAST);
      H5DataSetWriterSetup s(f, path, H5T_NATIVE_INT, 1);
      std::vector<hsize_t> start(1, a_sp-1), n2write(1,1);
      H5DSWriterDimInfo dim(start, n2write);
      H5DataSetWriter w(&s);
      w.SetDimInfoForWriting(&dim);
      int tmpItmp(*ITMP < 0 ? 1 : 0);
      w.WriteData(&tmpItmp, 1);
    }

    CAR_DBL2D bcData;
    bcData.SetSize(13, *MNWMAX, 0);
    if (*ITMP > 0)
    { // get the data from the MNW2 array
      for (int i=0; i<*MNWMAX; i++)
      {
        bcData.at(0,i) = ForElement(MNW2d,  1, i+1, *NMNWVL); // active
        bcData.at(1,i) = ForElement(MNW2d,  5, i+1, *NMNWVL); // Qdes
        bcData.at(2,i) = ForElement(MNW2d, 24, i+1, *NMNWVL); // CapMult
        bcData.at(3,i) = ForElement(MNW2d, 12, i+1, *NMNWVL); // Cprime
        bcData.at(4,i) = ForElement(MNW2d,  7, i+1, *NMNWVL); // Hlim
        bcData.at(5,i) = ForElement(MNW2d,  8, i+1, *NMNWVL); // QCUT
        bcData.at(6,i) = ForElement(MNW2d,  9, i+1, *NMNWVL); // Qfrcmn
        bcData.at(7,i) = ForElement(MNW2d, 10, i+1, *NMNWVL); // Qfrcmx
        for (int j=0; j<*NAUX; j++) // AUX
        {
          bcData.at(8+j,i) = ForElement(MNW2d, 31+j, i+1, *NMNWVL);
        }
      }
    }
    else if (*ITMP < 0 && a_sp > 1)
    { // get the previous stress period
      path.Format("%s/%s", "MNW2", MFBC_DATA);
      std::pair<int, int> p(0,1);
      VEC_INT_PAIR indices(3,p);
      indices[0].second = bcData.GetSize1();
      indices[1].second = bcData.GetSize2();
      indices[2].first = a_sp-2;
      H5DataSetReader r(f, path, indices);
      r.GetData(&bcData.at(0,0),
                static_cast<size_t>(bcData.GetSize1()*bcData.GetSize2()));
    }

    std::vector<int> cellids(*MNWMAX, 0), iface(*MNWMAX, 0);
    for (size_t q=0; q<cellids.size(); q++)
    {
      cellids[q] = static_cast<int>(q+1);
    }
    // write the data for the stress period
    CStr mStr = "MNW2";
    iWriteBcListData(a_sp, 0, mStr, f, cellids, bcData, iface);

    CStr file1;
    util::StripPathFromFilename(f, file1);
    line.Format("GMS_HDF5_01 \"%s\" \"%s\" %d", file1, "MNW2", a_sp);
    if (*ITMP > 0)
    {
      a_exp->WriteLineToFile(MNW2, line);
    }
  }
  catch (std::exception&)
  {
    ASSERT(false);
  }

} // iWriteMnw2Lines34
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void iMnwiLine1 (MfPackage *a_p)
{
  using namespace MfData::Packages;
  const int* Wel1flag(0);
  const int* QSUMflag(0);
  const int* BYNDflag(0);
  if (!a_p->GetField(MNWIpack::Wel1flag, &Wel1flag) || !Wel1flag ||
      !a_p->GetField(MNWIpack::QSUMflag, &QSUMflag) || !QSUMflag ||
      !a_p->GetField(MNWIpack::BYNDflag, &BYNDflag) || !BYNDflag)
  {
    ASSERT(0);
    return;
  }

  std::vector<CStr>& lines(a_p->StringsToWrite());
  std::vector<CStr>& desc(a_p->StringDescriptions());

  desc.push_back("1.    Wel1flag,QSUMflag,BYNDflag");
  CStr line;
  line.Format("%d %d %d", *Wel1flag, *QSUMflag, *BYNDflag);
  lines.push_back(line);

} // iMnwiLine1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void iMnwiLine2 (MfPackage *a_p)
{
  using namespace MfData::Packages;
  const int* MNWOBS(0);
  if (!a_p->GetField(MNWIpack::MNWOBS, &MNWOBS) || !MNWOBS)
  {
    ASSERT(0);
    return;
  }

  std::vector<CStr>& lines(a_p->StringsToWrite());
  std::vector<CStr>& desc(a_p->StringDescriptions());

  desc.push_back("2.    MNWOBS");
  CStr line;
  line.Format("%d", *MNWOBS);
  lines.push_back(line);

} // iMnwiLine2
//------------------------------------------------------------------------------
/// \brief Collect the line and later we'll write them all out.
//------------------------------------------------------------------------------
static void iMnwiLine3 (MfPackage *a_p)
{
  using namespace MfData::Packages;
  const char* WELLID(0);
  const int* UNIT(0);
  const int* QNDflag(0);
  const int* QBHflag(0);
  const int* CONCflag(0);
  if (!a_p->GetField(MNWIpack::WELLID, &WELLID) || !WELLID ||
      !a_p->GetField(MNWIpack::UNIT, &UNIT) || !UNIT ||
      !a_p->GetField(MNWIpack::QNDflag, &QNDflag) || !QNDflag ||
      !a_p->GetField(MNWIpack::QBHflag, &QBHflag) || !QBHflag ||
      !a_p->GetField(MNWIpack::CONCflag, &CONCflag) || !CONCflag)
  {
    ASSERT(0);
    return;
  }

  std::vector<CStr>& lines(a_p->StringsToWrite());
  std::vector<CStr>& desc(a_p->StringDescriptions());
  CStr line, wellid(WELLID);
  if (wellid.find(" ") != -1)
  {
    wellid.Format("'%s'", WELLID);
  }
  line.Format("%s %d %d %d %d", wellid, *UNIT, *QNDflag, *QBHflag, *CONCflag);
  lines.push_back(line);
  desc.push_back("3.   WELLID,UNIT,QNDflag,QBHflag,CONCflag");
} // iMnwiLine3
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void expMNW2 (MfGlobal *a_g,
                     MfPackage *a_p,
                     TxtExporter *a_exp)
{
  CStr ln = a_p->GetLineNumber();
  if ("1" == ln) { iMnw2Line1(a_p); }
  else if ("2ab" == ln) { iMnw2Line2ab(a_p); }
  else if ("2c" == ln) { iMnw2Line2c(a_p); }
  else if ("2d" == ln) { iMnw2Line2d(a_p); }
  else if ("2e" == ln) { iMnw2Line2e(a_p); }
  else if ("2f" == ln) { iMnw2Line2f(a_p); }
  else if ("2g" == ln) { iMnw2Line2g(a_p); }
  else if ("2h" == ln) { iMnw2Line2h(a_p); }
  else if ("34" == ln)
  {
    int sp=a_g->GetCurrentPeriod();
    if (1 == sp)
    { // write lines 1 and 2
      iWriteMnw2Lines12(a_p, a_exp);
    }
    iWriteMnw2Lines34(a_p, a_exp, sp);
  }
} // expMNW2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void expMNWI (MfGlobal*,
                     MfPackage *a_p,
                     TxtExporter* a_exp)
{
  CStr ln = a_p->GetLineNumber();

  if ("1" == ln) { iMnwiLine1(a_p); }
  else if ("2" == ln) { iMnwiLine2(a_p); }
  else if ("3" == ln) { iMnwiLine3(a_p); }
  else if ("end" == ln)
  {
    iWriteStoredLines(MfData::Packages::MNWI, a_p, a_exp);
  }
} // expMNWI
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void expSUB (MfPackage *a_p,
                    TxtExporter *a_exp)
{
  using namespace MfData::Packages;
  using util::ForElement;
  const int *isubcb(NULL), *isuboc(NULL), *nndb(NULL), *ndb(NULL), *nmz(NULL),
    *nn(NULL), *itmin(NULL), *idsave(NULL), *idrest(NULL),
    *ln(NULL), *ldn(NULL);
  const Real *ac1(NULL), *ac2(NULL), *dp(NULL);
  if (a_p->GetField(SUBpack::ISUBCB, &isubcb) && isubcb &&
      a_p->GetField(SUBpack::ISUBOC, &isuboc) && isuboc &&
      a_p->GetField(SUBpack::NNDB, &nndb) && nndb &&
      a_p->GetField(SUBpack::NDB, &ndb) && ndb &&
      a_p->GetField(SUBpack::NMZ, &nmz) && nmz &&
      a_p->GetField(SUBpack::NN, &nn) && nn &&
      a_p->GetField(SUBpack::AC1, &ac1) && ac1 &&
      a_p->GetField(SUBpack::AC2, &ac2) && ac2 &&
      a_p->GetField(SUBpack::ITMIN, &itmin) && itmin &&
      a_p->GetField(SUBpack::IDSAVE, &idsave) && idsave &&
      a_p->GetField(SUBpack::IDREST, &idrest) && idrest &&
      ((*nndb == 0) || (a_p->GetField(SUBpack::LN, &ln) && ln)) &&
      ((*ndb == 0) || (a_p->GetField(SUBpack::LDN, &ldn) && ldn &&
       a_p->GetField(SUBpack::DP, &dp) && dp)))
  {
    CStr line;

    // Line 1: ISUBCB ISUBOC NNDB NDB NMZ NN AC1 AC2 ITMIN IDSAVE IDREST
    line.Format("%d %d %d %d %d %d %s %s %d %d %d",
        *isubcb, *isuboc, *nndb, *ndb, *nmz, *nn, STR(*ac1), STR(*ac2), *itmin,
        *idsave, *idrest);
    a_exp->WriteLineToFile(SUB, line);

    // Line 2: [LN(NNDB)] if NNDB > 0
    if (*nndb > 0)
    {
      std::string output = join(ln, ln + *nndb, std::string(" "));
      a_exp->WriteLineToFile(SUB, output);
    }

    // Line 3: [LDN(NDB)] if NDB > 0
    if (*ndb > 0)
    {
      std::string output = join(ldn, ldn + *ndb, std::string(" "));
      a_exp->WriteLineToFile(SUB, output);
    }

    // Line 4: [RNB(NCOL,NROW)] U2DREL if NDB > 0 and IDREST <= 0
    for (int i = 1; i <= *ndb; ++i)
    {
      WriteArrayLineToFile(a_exp, SUB, MFBC_RNB, i);
    }

    for (int i = 1; i <= *nndb; ++i)
    {
      // Line 5: [HC(NCOL,NROW)] U2DREL if NNDB > 0
      WriteArrayLineToFile(a_exp, SUB, MFBC_HC, i);

      // Line 6: [Sfe(NCOL,NROW)] U2DREL if NNDB > 0
      WriteArrayLineToFile(a_exp, SUB, MFBC_SFE, i);

      // Line 7: [Sfv(NCOL,NROW)] U2DREL if NNDB > 0
      WriteArrayLineToFile(a_exp, SUB, MFBC_SFV, i);

      // Line 8: [Com(NCOL,NROW)] U2DREL if NNDB > 0
      WriteArrayLineToFile(a_exp, SUB, MFBC_COM, i);
    }

    // Line 9: [DP(NMZ,3)] if NDB > 0
    if (*ndb > 0)
    {
      for (int mz = 1; mz <= *nmz; ++mz)
      {
        using util::ForElement;
        line.Format("%s %s %s",
            STR(ForElement(dp, mz, 1, *nmz)),
            STR(ForElement(dp, mz, 2, *nmz)),
            STR(ForElement(dp, mz, 3, *nmz)));
        a_exp->WriteLineToFile(SUB, line);
      }
    }

    for (int i = 1; i <= *ndb; ++i)
    {
      if (*idrest <= 0)
      {
        // Line 10: [Dstart(NCOL,NROW)] U2DREL if NDB > 0 and IDREST <= 0
        WriteArrayLineToFile(a_exp, SUB, MFBC_DSTART, i);

        // Line 11: [DHC(NCOL,NROW)] U2DREL if NDB > 0 and IDREST <= 0
        WriteArrayLineToFile(a_exp, SUB, MFBC_DHC, i);
      }

      // Line 12: [DCOM(NCOL,NROW)] U2DREL if NDB > 0
      WriteArrayLineToFile(a_exp, SUB, MFBC_DCOM, i);

      // Line 13: [DZ(NCOL,NROW)] U2DREL if NDB > 0
      WriteArrayLineToFile(a_exp, SUB, MFBC_DZ, i);

      // Line 14: [NZ(NCOL,NROW)] U2DINT if NDB > 0
      WriteArrayLineToFile(a_exp, SUB, MFBC_NZ, i);
    }

    // Line 15: see function below
    // Line 16: see function below
  }
} // expSUB
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void expSUBLine15 (MfPackage *a_p,
                         TxtExporter *a_exp)
{
  using namespace MfData::Packages;
  using util::ForElement;
  const int *isbocf, *isbocu;
  if (a_p->GetField(SUBpack::ISBOCF, &isbocf) && isbocf &&
      a_p->GetField(SUBpack::ISBOCU, &isbocu) && isbocu)
  {
    CStr line;

    // Line 15: [Ifm1 Iun1 Ifm2 Iun2 Ifm3 Iun3 Ifm4 Iun4 Ifm5 Iun5 Ifm6 Iun6]
    //          if ISUBOC > 0
    line.Format("%d %d %d %d %d %d %d %d %d %d %d %d",
        isbocf[0], isbocu[0],
        isbocf[1], isbocu[1],
        isbocf[2], isbocu[2],
        isbocf[3], isbocu[3],
        isbocf[4], isbocu[4],
        isbocf[5], isbocu[5]);
    a_exp->WriteLineToFile(SUB, line);
  }
} // expSUBLine15
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void expSUBLine16 (MfPackage *a_p,
                         TxtExporter *a_exp)
{
  using namespace MfData::Packages;
  using util::ForElement;
  const int *isp1, *isp2, *jts1, *jts2, *ifl;
  if (a_p->GetField(SUBpack::ISP1, &isp1) && isp1 &&
      a_p->GetField(SUBpack::ISP2, &isp2) && isp2 &&
      a_p->GetField(SUBpack::JTS1, &jts1) && jts1 &&
      a_p->GetField(SUBpack::JTS2, &jts2) && jts2 &&
      a_p->GetField(SUBpack::IFL, &ifl) && ifl)
  {
    CStr line;

    // Line 16: [ISP1 ISP2 ITS1 ITS2 Ifl1 Ifl2 Ifl3 Ifl4 Ifl5 Ifl6 Ifl7 Ifl8
    //           Ifl9 Ifl10 Ifl11 Ifl12 Ifl13] if ISUBOC > 0.
    line.Format("%d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d",
        *isp1, *isp2, *jts1, *jts2, ifl[0], ifl[1], ifl[2], ifl[3], ifl[4],
        ifl[5], ifl[6], ifl[7], ifl[8], ifl[9], ifl[10], ifl[11], ifl[12]);
    a_exp->WriteLineToFile(SUB, line);
  }
} // expSUBLine16
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void expUZFLine1 (MfPackage *a_p,
                         TxtExporter *a_exp)
{
  using namespace MfData::Packages;
  const int *nuztop, *iuzfopt, *irunflg, *ietflg, *iuzfcb1, *iuzfcb2, *ntrail2,
            *nsets2, *nuzgag;
  const Real *surfdep;
  if (a_p->GetField(UZFpack::NUZTOP, &nuztop) && nuztop &&
      a_p->GetField(UZFpack::IUZFOPT, &iuzfopt) && iuzfopt &&
      a_p->GetField(UZFpack::IRUNFLG, &irunflg) && irunflg &&
      a_p->GetField(UZFpack::IETFLG, &ietflg) && ietflg &&
      a_p->GetField(UZFpack::IUZFCB1, &iuzfcb1) && iuzfcb1 &&
      a_p->GetField(UZFpack::IUZFCB2, &iuzfcb2) && iuzfcb2 &&
      a_p->GetField(UZFpack::NTRAIL2, &ntrail2) && ntrail2 &&
      a_p->GetField(UZFpack::NSETS2, &nsets2) && nsets2 &&
      a_p->GetField(UZFpack::NUZGAG, &nuzgag) && nuzgag &&
      a_p->GetField(UZFpack::SURFDEP, &surfdep) && surfdep)
  {
    // NUZTOP IUZFOPT IRUNFLG IETFLG IUZFCB1 IUZFCB2 [NTRAIL2 NSETS2] NUZGAG SURFDEP
    CStr line;
    line.Format("%d %d %d %d %d %d", *nuztop, *iuzfopt, *irunflg, *ietflg,
                                     *iuzfcb1, *iuzfcb2);
    if (*iuzfopt > 0)
    {
      CStr part2;
      part2.Format(" %d %d", *ntrail2, *nsets2);
      line += part2;
    }

    CStr part3;
    part3.Format(" %d %s", *nuzgag, STR(*surfdep));
    line += part3;

    a_exp->WriteLineToFile(UZF, line);
  }
} // expUZFLine1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void Fill1dDblArray(const char* a_file,
                           const char* a_path,
                           int a_nCells)
{
  std::vector<double> data(a_nCells, 0);
  Write1DDblArray(a_file, a_path, &data[0], a_nCells);
} // Fill1dDblArray
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void expUZFLine8 (MfPackage *a_pLine1,
                         MfPackage *a_pLine8,
                         int a_numRow,
                         int a_numCol,
                         TxtExporter *a_exp)
{
  using namespace MfData::Packages;
  using util::ForElement;
  const int *iuzfopt, *irunflg, *nuzgag, *iuzlist;
  if (a_pLine1->GetField(UZFpack::IUZFOPT, &iuzfopt) && iuzfopt &&
      a_pLine1->GetField(UZFpack::IRUNFLG, &irunflg) && irunflg &&
      a_pLine1->GetField(UZFpack::NUZGAG, &nuzgag) && nuzgag &&
      a_pLine8->GetField(UZFpack::IUZLIST, &iuzlist) && iuzlist)
  {
    std::map<CStr,CStr> &m(GetTxtLineArrayMap(a_exp));
    int nCells = a_numRow * a_numCol;
    CStr fileName(a_exp->GetBaseFileName());
    fileName += ".h5";

    // Line 2: IUZFBND (NCOL, NROW) -- U2DINT
    CStr tag = "UZF/12. IUZFBND1";
    if (m.find(tag) != m.end())
      a_exp->WriteLineToFile(UZF, m[tag]);
    else
      Fill1dDblArray(fileName, "UZF/12. IUZFBND", nCells);

    // Line 3: [IRUNBND (NCOL, NROW)] -- U2DINT
    tag = "UZF/14. IRUNBND1";
    if (m.find(tag) != m.end())
      a_exp->WriteLineToFile(UZF, m[tag]);
    else
      Fill1dDblArray(fileName, "UZF/14. IRUNBND", nCells);

    // Line 4: [VKS (NCOL, NROW)] -- U2DREL
    tag = "UZF/16. VKS1";
    if (m.find(tag) != m.end())
      a_exp->WriteLineToFile(UZF, m[tag]);
    else
      Fill1dDblArray(fileName, "UZF/16. VKS", nCells);

    // Line 5: EPS (NCOL, NROW) -- U2DREL
    tag = "UZF/18. EPS1";
    if (m.find(tag) != m.end())
      a_exp->WriteLineToFile(UZF, m[tag]);
    else
      Fill1dDblArray(fileName, "UZF/18. EPS", nCells);

    // Line 6: THTS (NCOL, NROW) -- U2DREL
    tag = "UZF/20. THTS1";
    if (m.find(tag) != m.end())
      a_exp->WriteLineToFile(UZF, m[tag]);
    else
      Fill1dDblArray(fileName, "UZF/20. THTS", nCells);

    // Line 7: [THTI (NCOL, NROW)] -- U2DREL
    tag = "UZF/22. THTI1";
    if (m.find(tag) != m.end())
      a_exp->WriteLineToFile(UZF, m[tag]);
    else
      Fill1dDblArray(fileName, "UZF/22. THTI", nCells);

    // Line 8: [IUZROW] [IUZCOL] IFTUNIT [IUZOPT]
    for (int i = 1; i <= *nuzgag; ++i)
    {
      const int IUZLIST_SIZE = 4;
      int iuzrow, iuzcol, iftunit, iuzopt;
      iuzrow  = ForElement(iuzlist, 1, i, IUZLIST_SIZE);
      iuzcol  = ForElement(iuzlist, 2, i, IUZLIST_SIZE);
      iftunit = ForElement(iuzlist, 3, i, IUZLIST_SIZE);
      iuzopt  = ForElement(iuzlist, 4, i, IUZLIST_SIZE);
      CStr line;
      if (iuzrow != 0)
        line.Format("%d %d %d %d", iuzrow, iuzcol, iftunit, iuzopt);
      else
        line.Format("%d", -iftunit);
      a_exp->WriteLineToFile(UZF, line);
    }
  }
} // expUZFLine8
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void expUZFStressPeriod (MfPackage *a_pLine1,
                                MfPackage *a_pSP,
                                int a_sp,
                                TxtExporter *a_exp)
{
  using namespace MfData::Packages;
  const int *iuzfopt, *ietflg, *nuzf1, *nuzf2, *nuzf3, *nuzf4;
  if (a_pLine1->GetField(UZFpack::IUZFOPT, &iuzfopt) && iuzfopt &&
      a_pLine1->GetField(UZFpack::IETFLG, &ietflg) && ietflg &&
      a_pSP->GetField(UZFpack::NUZF1, &nuzf1) && nuzf1 &&
      a_pSP->GetField(UZFpack::NUZF2, &nuzf2) && nuzf2 &&
      a_pSP->GetField(UZFpack::NUZF3, &nuzf3) && nuzf3 &&
      a_pSP->GetField(UZFpack::NUZF4, &nuzf4) && nuzf4)
  {
    CStr line;
    CStr tag;
    std::map<CStr,CStr> &m(GetTxtLineArrayMap(a_exp));

    // Line 9: NUZF1
    line.Format("%d", *nuzf1);
    a_exp->WriteLineToFile(UZF, line);

    // Line 10: [FINF (NCOL, NROW)] – U2DREL
    tag.Format("UZF/07. Property%d_0", a_sp);
    if (m.find(tag) != m.end())
      a_exp->WriteLineToFile(UZF, m[tag]);

    if (*ietflg > 0)
    {
      // Line 11: NUZF2
      line.Format("%d", *nuzf2);
      a_exp->WriteLineToFile(UZF, line);

      // Line 12: [PET (NCOL, NROW)] – U2DREL
      tag.Format("UZF/07. Property%d_1", a_sp);
      if (m.find(tag) != m.end())
        a_exp->WriteLineToFile(UZF, m[tag]);

      // Line 13: NUZF3
      line.Format("%d", *nuzf3);
      a_exp->WriteLineToFile(UZF, line);

      // Line 14: [EXTDP (NCOL, NROW)] – U2DREL
      tag.Format("UZF/07. Property%d_2", a_sp);
      if (m.find(tag) != m.end())
        a_exp->WriteLineToFile(UZF, m[tag]);

      if (*iuzfopt > 0)
      {
        // Line 15: NUZF4
        line.Format("%d", *nuzf4);
        a_exp->WriteLineToFile(UZF, line);

        // Line 16: [EXTWC (NCOL, NROW)] – U2DREL
        tag.Format("UZF/07. Property%d_3", a_sp);
        if (m.find(tag) != m.end())
          a_exp->WriteLineToFile(UZF, m[tag]);
      }
    }

    // write use last data to the file
    std::vector<int> useLast;
    useLast.push_back(*nuzf1 < 0 ? 1 : 0);
    useLast.push_back(*nuzf2 < 0 ? 1 : 0);
    useLast.push_back(*nuzf3 < 0 ? 1 : 0);
    useLast.push_back(*nuzf4 < 0 ? 1 : 0);
    expUseLastAreal(a_exp->GetBaseFileName(), "UZF/", a_sp, useLast);
  }
} // expUZFStressPeriod
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void Extend2dDblArray(const char* a_file,
                                const char* a_path,
                                int a_sp,
                                int a_nLayers)
{
  H5DataSetWriterSetup setup(a_file, a_path, H5T_NATIVE_DOUBLE , 2);
  H5DataSetWriter h(&setup);
  h.AllowTypeConversions(true);
  std::vector<hsize_t> start(2, 0), n2write(2, 1);
  start[1] = a_sp - 1;
  n2write[0] = a_nLayers;
  H5DSWriterDimInfo dim(start, n2write);
  h.SetDimInfoForWriting(&dim);
  std::vector<double> data(a_nLayers, 0);
  h.WriteData(&data[0], data.size());
} // Extend2dDblArray
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void Extend3dDblArray(const char* a_file,
                                const char* a_path,
                                int a_sp,
                                int a_nCells)
{
  H5DataSetWriterSetup setup(a_file, a_path, H5T_NATIVE_DOUBLE , 3);
  H5DataSetWriter h(&setup);
  h.AllowTypeConversions(true);
  std::vector<hsize_t> start(3, 0), n2write(3, 1);
  start[2] = a_sp - 1;
  n2write[1] = a_nCells;
  H5DSWriterDimInfo dim(start, n2write);
  h.SetDimInfoForWriting(&dim);
  std::vector<double> data(a_nCells, 0);
  h.WriteData(&data[0], data.size());
} // Extend2dDblArray
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void expVDFLine5(MfPackage *a_p,
                        TxtExporter *a_exp)
{
  using namespace MfData::Packages;
  const int *mt3drhoflg, *mfnadvfd, *nswtcpl, *iwtable, *nsrhoeos, *mtrhospec;
  const Real *densemin, *densemax, *dnscrit, *denseref, *drhodc, *drhodprhd,
             *prhdref, *crhoref, *firstdt;
  if (a_p->GetField(VDFpack::MT3DRHOFLG, &mt3drhoflg) && mt3drhoflg &&
      a_p->GetField(VDFpack::MFNADVFD, &mfnadvfd) && mfnadvfd &&
      a_p->GetField(VDFpack::NSWTCPL, &nswtcpl) && nswtcpl &&
      a_p->GetField(VDFpack::IWTABLE, &iwtable) && iwtable &&
      a_p->GetField(VDFpack::DENSEMIN, &densemin) && densemin &&
      a_p->GetField(VDFpack::DENSEMAX, &densemax) && densemax &&
      a_p->GetField(VDFpack::DNSCRIT, &dnscrit) && dnscrit &&
      a_p->GetField(VDFpack::DENSEREF, &denseref) && denseref &&
      a_p->GetField(VDFpack::DRHODC, &drhodc) && drhodc &&
      a_p->GetField(VDFpack::DRHODPRHD, &drhodprhd) && drhodprhd &&
      a_p->GetField(VDFpack::PRHDREF, &prhdref) && prhdref &&
      a_p->GetField(VDFpack::NSRHOEOS, &nsrhoeos) && nsrhoeos &&
      a_p->GetField(VDFpack::MTRHOSPEC, &mtrhospec) && mtrhospec &&
      a_p->GetField(VDFpack::CRHOREF, &crhoref) && crhoref &&
      a_p->GetField(VDFpack::FIRSTDT, &firstdt) && firstdt)
  {
    CStr line;

    // Line 1: MT3DRHOFLG MFNADVFD NSWTCPL IWTABLE
    line.Format("%d %d %d %d", *mt3drhoflg, *mfnadvfd, *nswtcpl, *iwtable);
    a_exp->WriteLineToFile(VDF, line);

    // Line 2: DENSEMIN DENSEMAX
    line.Format("%s %s", STR(*densemin), STR(*densemax));
    a_exp->WriteLineToFile(VDF, line);

    // Line 3: DNSCRIT 
    if (*nswtcpl > 1 || *nswtcpl == -1)
    {
      line.Format("%s", STR(*dnscrit));
      a_exp->WriteLineToFile(VDF, line);
    }

    
    if (*mt3drhoflg >= 0)
    {
      // Line 4: DENSEREF DRHODC(1)
      line.Format("%s %s", STR(*denseref), STR(*drhodc));
      a_exp->WriteLineToFile(VDF, line);
    }
    else if (*mt3drhoflg == -1)
    {
      // Line 4a: DENSEREF DRHODPRHD PRHDREF
      line.Format("%s %s %s", STR(*denseref), STR(*drhodprhd), STR(*prhdref));
      a_exp->WriteLineToFile(VDF, line);

      // Line 4b: NSRHOEOS
      line.Format("%d", *nsrhoeos);
      a_exp->WriteLineToFile(VDF, line);

      for (int i = 0; i < *nsrhoeos; ++i)
      {
        // Line 4c: MTRHOSPEC(NSRHOEOS) DRHODC(NSRHOEOS) CRHOREF(NSRHOEOS)
        line.Format("%d %s %s", mtrhospec[i], STR(drhodc[i]), STR(crhoref[i]));
        a_exp->WriteLineToFile(VDF, line);
      }
    }

    // Line 5: FIRSTDT
    line.Format("%s", STR(*firstdt));
    a_exp->WriteLineToFile(VDF, line);
  }
} // expVDFLine5
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void expVDFStressPeriod(MfPackage *a_pLine5,
                               MfPackage *a_pSP,
                               int a_sp,
                               int a_rows,
                               int a_cols,
                               int a_layers,
                               TxtExporter *a_exp)
{
  using namespace MfData::Packages;
  const int *indense, *mt3drhoflg;
  if (a_pLine5->GetField(VDFpack::MT3DRHOFLG, &mt3drhoflg) && mt3drhoflg &&
      a_pSP->GetField(VDFpack::INDENSE, &indense) && indense)
  {
    bool arrayWritten = false;
    if (*mt3drhoflg == 0)
    {
      CStr line;

      // Line 6: INDENSE
      line.Format("%d", *indense);
      a_exp->WriteLineToFile(VDF, line);

      // Use Last
      {
        CStr f(a_exp->GetBaseFileName()), path, fileName;
        f += ".h5";
        util::StripPathFromFilename(f, fileName);
        const char* type = "VDF";
        path.Format("%s/%s", type, MFBC_USELAST);
        H5DataSetWriterSetup s(f, path, H5T_NATIVE_INT, 1);
        std::vector<hsize_t> start(1, a_sp-1), n2write(1,1);
        H5DSWriterDimInfo dim(start, n2write);
        H5DataSetWriter w(&s);
        w.SetDimInfoForWriting(&dim);
        int itmpToWrite = *indense;
        w.WriteData(&itmpToWrite, 1);
      }

      // Line 7: [DENSE(NCOL,NROW)] – U2DREL
      if (*indense > 0)
      {
        arrayWritten = true;
        std::map<CStr,CStr> &m(GetTxtLineArrayMap(a_exp));
        for (int i = 0; i < a_layers; ++i)
        {
          CStr tag;
          tag.Format("VDF/07. Property%d_%d", a_sp, i);
          if (m.find(tag) != m.end())
            a_exp->WriteLineToFile(VDF, m[tag]);
        }
      }
    }

    if (!arrayWritten)
    {
      CStr fileName(a_exp->GetBaseFileName());
      fileName += ".h5";
      Extend3dDblArray(fileName, "VDF/07. Property", a_sp,
                                                     a_rows*a_cols*a_layers);
      Extend2dDblArray(fileName, "VDF/08. Property Multiplier", a_sp,
                                                                a_layers);
    }
  }
} // expVDFStressPeriod
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void expVSCLine3(MfPackage *a_p,
                        TxtExporter *a_exp)
{
  using namespace MfData::Packages;
  const int *mt3dmuflg, *nsmueos, *mutempopt, *mtmuspec, *mtmutempspec;
  const Real *viscmin, *viscmax, *viscref, *dmudc, *cmuref, *amucoeff;
  if (a_p->GetField(VSCpack::MT3DMUFLG, &mt3dmuflg) && mt3dmuflg &&
      a_p->GetField(VSCpack::NSMUEOS, &nsmueos) && nsmueos &&
      a_p->GetField(VSCpack::MUTEMPOPT, &mutempopt) && mutempopt &&
      a_p->GetField(VSCpack::MTMUSPEC, &mtmuspec) && mtmuspec &&
      a_p->GetField(VSCpack::MTMUTEMPSPEC, &mtmutempspec) && mtmutempspec &&
      a_p->GetField(VSCpack::VISCMIN, &viscmin) && viscmin &&
      a_p->GetField(VSCpack::VISCMAX, &viscmax) && viscmax &&
      a_p->GetField(VSCpack::VISCREF, &viscref) && viscref &&
      a_p->GetField(VSCpack::DMUDC, &dmudc) && dmudc &&
      a_p->GetField(VSCpack::CMUREF, &cmuref) && cmuref &&
      a_p->GetField(VSCpack::AMUCOEFF, &amucoeff) && amucoeff)
  {
    CStr line;

    // Line 1: MT3DMUFLG
    line.Format("%d", *mt3dmuflg);
    a_exp->WriteLineToFile(VSC, line);

    // Line 2: VISCMIN VISCMAX
    line.Format("%s %s", STR(*viscmin), STR(*viscmax));
    a_exp->WriteLineToFile(VSC, line);

    if (*mt3dmuflg >= 0)
    {
      // Line 3: VISCREF DMUDC(1) CMUREF(1)
      line.Format("%s %s %s", STR(*viscref), STR(*dmudc), STR(*cmuref));
      a_exp->WriteLineToFile(VSC, line);
    }
    else if (*mt3dmuflg == -1)
    {
      // Line 3a: VISCREF
      line.Format("%s", STR(*viscref));
      a_exp->WriteLineToFile(VSC, line);

      // Line 3b: NSMUEOS MUTEMPOPT
      line.Format("%d %d", *nsmueos, *mutempopt);
      a_exp->WriteLineToFile(VSC, line);

      for (int i = 0; i < *nsmueos; ++i)
      {
        // Line 3c: MTMUSPEC(NSMUEOS) DMUDC(NSMUEOS) CMUREF(NSMUEOS)
        line.Format("%d %s %s", mtmuspec[i], STR(dmudc[i]), STR(cmuref[i]));
        a_exp->WriteLineToFile(VSC, line);
      }

      if (*mutempopt > 0)
      {
        int muncoeff(4);
        if (*mutempopt == 2)
          muncoeff = 5;
        else if (*mutempopt == 3)
          muncoeff = 2;

        // Line 3d: MTMUTEMPSPEC AMUCOEFF(MUNCOEFF)
        line.Format("%d", *mtmutempspec);
        for (int i = 0; i < muncoeff; ++i)
        {
          CStr amucoeffStr;
          amucoeffStr.Format(" %s", STR(amucoeff[i]));
          line += amucoeffStr;
        }
        a_exp->WriteLineToFile(VSC, line);
      }
    }
  }
} // expVSCLine3
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void expVSCStressPeriod(MfPackage *a_pLine3,
                               MfPackage *a_pSP,
                               int a_sp,
                               int a_rows,
                               int a_cols,
                               int a_layers,
                               TxtExporter *a_exp)
{
  using namespace MfData::Packages;
  const int *invisc, *mt3dmuflg;
  if (a_pLine3->GetField(VSCpack::MT3DMUFLG, &mt3dmuflg) && mt3dmuflg &&
      a_pSP->GetField(VSCpack::INVISC, &invisc) && invisc)
  {
    bool arrayWritten = false;
    if (*mt3dmuflg == 0)
    {
      CStr line;

      // Line 4: INVISC
      line.Format("%d", *invisc);
      a_exp->WriteLineToFile(VSC, line);

      // Use Last
      {
        CStr f(a_exp->GetBaseFileName()), path, fileName;
        f += ".h5";
        util::StripPathFromFilename(f, fileName);
        const char* type = "VSC";
        path.Format("%s/%s", type, MFBC_USELAST);
        H5DataSetWriterSetup s(f, path, H5T_NATIVE_INT, 1);
        std::vector<hsize_t> start(1, a_sp-1), n2write(1,1);
        H5DSWriterDimInfo dim(start, n2write);
        H5DataSetWriter w(&s);
        w.SetDimInfoForWriting(&dim);
        int itmpToWrite = *invisc;
        w.WriteData(&itmpToWrite, 1);
      }

      // Line 5: [VISC(NCOL,NROW)] – U2DREL
      if (*invisc > 0)
      {
        arrayWritten = true;
        std::map<CStr,CStr> &m(GetTxtLineArrayMap(a_exp));
        for (int i = 0; i < a_layers; ++i)
        {
          CStr tag;
          tag.Format("VSC/07. Property%d_%d", a_sp, i);
          if (m.find(tag) != m.end())
            a_exp->WriteLineToFile(VSC, m[tag]);
        }
      }
    }

    if (!arrayWritten)
    {
      CStr fileName(a_exp->GetBaseFileName());
      fileName += ".h5";
      Extend3dDblArray(fileName, "VSC/07. Property", a_sp,
                                                     a_rows*a_cols*a_layers);
      Extend2dDblArray(fileName, "VSC/08. Property Multiplier", a_sp,
                                                                a_layers);
    }
  }
} // expVSCStressPeriod
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void expGAG (MfPackage* a_p,
                    TxtExporter* a_exp)
{
  using namespace MfData::Packages;
  const int *igglst(0), *numgage(0);
  if (a_p->GetField(GAGpack::IGGLST, &igglst) && igglst &&
      a_p->GetField(GAGpack::NUMGAGE, &numgage) && numgage)
  {
      // line 1: NUMGAGE
      CStr line;
      line.Format("%d", *numgage);
      a_exp->WriteLineToFile(GAGE, line);

      for (int i=0; i < *numgage; ++i)
      {
        int type = igglst[i*4 + 0];
        if (type >= 0)
        {
          // line 2a: GAGESEG GAGERCH UNIT OUTTYPE
          int gageseg = type;
          int gagerch = igglst[i*4 + 1];
          int unit    = igglst[i*4 + 2];
          int outtype = igglst[i*4 + 3];
          line.Format("%d %d %d %d", gageseg, gagerch, unit, outtype);
          a_exp->WriteLineToFile(GAGE, line);
        }
        else
        {
          // line 2b: LAKE UNIT {OUTTYPE}
          int lake    = type;
          int unit    = igglst[i*4 + 2];
          int outtype = igglst[i*4 + 3];
          line.Format("%d %d %d", lake, unit, outtype);
          a_exp->WriteLineToFile(GAGE, line);
        }
      }
  }
} // expGAG
static MfPackage& iLgrPack ()
{
  static MfPackage m_pack(Packages::LGR); // ok to leave static
  return m_pack;
} // iLgrPack
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void expLGR_1 (MfPackage* a_p,
                      TxtExporter* a_exp)
{
  const int *NGRIDS,*IUPBHSV,*IUPBFSV;
  if (a_p->GetField("NGRIDS", &NGRIDS) && NGRIDS &&
      a_p->GetField("IUPBHSV", &IUPBHSV) && IUPBHSV &&
      a_p->GetField("IUPBFSV", &IUPBFSV) && IUPBFSV)
  {
    std::vector<CStr>& lines(iLgrPack().StringsToWrite());
    std::vector<CStr>& desc(iLgrPack().StringDescriptions());
    CStr ln;
    lines.push_back("LGR");
    desc.push_back(" 1.   LGR");
    ln.Format("%d", *NGRIDS);
    lines.push_back(ln);
    desc.push_back(" 2.   NGRIDS");
    ln = a_exp->GetBaseFileName();
    util::StripPathFromFilename(ln.c_str(), ln);
    ln += ".mfn";
    lines.push_back(ln);
    desc.push_back(" 3.   NAME FILE");
    lines.push_back("PARENTONLY");
    desc.push_back(" 4.   GRIDSTATUS");
    ln.Format("%d %d", *IUPBHSV, *IUPBFSV);
    lines.push_back(ln);
    desc.push_back(" 5.   IUPBHSV IUPBFSV");
  }
} // expLGR_1
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void expLGR_2 (MfPackage* a_p,
                      TxtExporter* a_exp)
{
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
    std::vector<CStr>& lines(iLgrPack().StringsToWrite());
    std::vector<CStr>& desc(iLgrPack().StringDescriptions());
    CStr ln = a_exp->GetBaseFileName();
    util::StripPathFromFilename(ln.c_str(), ln);
    ln += ".mfn";
    lines.push_back(ln);
    desc.push_back(" 6.   NAME FILE");
    lines.push_back("CHILDONLY");
    desc.push_back(" 7.   GRIDSTATUS");
    ln.Format("%d %d %d %d", *ISHFLG, *IBFLG, *IUCBHSV, *IUCBFSV);
    lines.push_back(ln);
    desc.push_back(" 8.   ISHFLG IBFLG IUCBHSV IUCBFSV");
    ln.Format("%d %d", *MXLGRITER, *IOUTLGR);
    lines.push_back(ln);
    desc.push_back(" 9.   MXLGRITER IOUTLGR");
    ln.Format("%s %s", STR(*RELAXH), STR(*RELAXF));
    lines.push_back(ln);
    desc.push_back("10.   RELAXH RELAXF");
    ln.Format("%s %s", STR(*HCLOSELGR), STR(*FCLOSELGR));
    lines.push_back(ln);
    desc.push_back("11.   HCLOSELGR FCLOSELGR");
    ln.Format("%d %d %d", *NPLBEG, *NPRBEG, *NPCBEG);
    lines.push_back(ln);
    desc.push_back("12.   NPLBEG NPRBEG NPCBEG");
    ln.Format("%d %d %d", *NPLEND, *NPREND, *NPCEND);
    lines.push_back(ln);
    desc.push_back("13.   NPLEND NPREND NPCEND");
    ln.Format("%d", *NCPP);
    lines.push_back(ln);
    desc.push_back("14.   NCPP");
    ln = "";
    int nval = *NPLEND + 1 - *NPLBEG;
    for (int i=0; i<nval; i++)
    {
      CStr ln1;
      ln1.Format("%d ", NCPPL[i]);
      ln += ln1;
    }
    lines.push_back(ln);
    desc.push_back("15.   NCPPL");
  }
} // expLGR_2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static void expLGR (MfGlobal* a_global)
{
  CStr fname = a_global->LgrName();
  util::StripExtensionFromFilename(fname.c_str(), fname);
  TxtExporter txt(fname);

  iWriteStoredLines(Packages::LGR, &iLgrPack(), &txt);
} // expLGR
//------------------------------------------------------------------------------
/// \brief This function does any final things that need to be done before
/// shutting down the program.
//------------------------------------------------------------------------------
static void expFinalize (MfGlobal* a_global,
                         TxtExporter *a_exp)
{
  expCheckArealFromUseLast(a_global->NumRow()*a_global->NumCol(),
                           a_exp->GetBaseFileName());
  expWriteMapIdsForListBcs(a_global, a_exp);
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
/// \brief This function writes the mapids to the h5 file if they exist
//------------------------------------------------------------------------------
static bool iReadListBcMapIdComments (const CStr& a_fname,
                                      const CStr& a_ftype,
                                      std::vector<CStr>& a_mapids)
{
  bool found = false;
  a_mapids.resize(0);
  CStr fname;
  fname.Format("%s.%s", a_fname, a_ftype);

  EReadAsciiFile r(fname.c_str());

  if (!r.OpenFile())
  {
    CStr msg("Error opening file: " + fname + ".");
    ErrorStack::Get().PutError(msg);
    return found;
  }

  r.UseExceptions();
  try
  {
    CStr str, line;
    while (r.GetLine(&line))
    {
      r.ReadData(str);
      if (str == "#GMSCOMMENT")
      {
        line.Replace("#GMSCOMMENT ", "");
        a_mapids.push_back(line);
        found = true;
      }
      else if (found)
      {
        r.CloseFile();
      }
    }
  }
  catch (ioexception &e)
  {
    CStr msg(e.what());
    if (!msg.IsEmpty())
    {
      ErrorStack::Get().PutError(msg);
    }
  }
  return found;
} // iReadListBcMapIdComments
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
static void iFillInListBcMapIds (const std::vector<CStr>& a_mapids,
                                 const std::vector<int>& a_cellGrp,
                                 std::vector<CStr>& a_mapids2)
{
  a_mapids2.resize(0);
  try
  {
    a_mapids2.reserve(a_cellGrp.size());
    for (size_t j=0; j<a_cellGrp.size(); j++)
    {
      a_mapids2.push_back(a_mapids.at(a_cellGrp.at(j)-1));
    }
  }
  catch (std::out_of_range&)
  {
    ASSERT(0);
    a_mapids2.clear();
  }
} // iFillInListBcMapIds
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
static void iWriteListBcMapIdsToH5 (TxtExporter *a_exp,
                                    const std::vector<CStr>& a_mapids,
                                    const CStr& a_type)
{
  CStr f(a_exp->GetBaseFileName()), path;
  f += ".h5";

  // figure out the max string length
  size_t i;
  int maxStrLen(-1);
  for (i=0; i<a_mapids.size(); i++)
  {
    if (a_mapids.at(i).GetLength() > maxStrLen)
      maxStrLen = a_mapids.at(i).GetLength();
  }
  maxStrLen++;
  if (maxStrLen < 2)
    return;
  
  size_t nToWrite(a_mapids.size() * maxStrLen);
  CAR_CHR2D chars;
  chars.SetSize((int)a_mapids.size(), maxStrLen, '\0');
  for (i=0; i<a_mapids.size(); i++)
  {
    strcpy(&chars.at((int)i, 0), a_mapids.at(i).c_str());
  }

  path.Format("%s/%s", a_type, MFBC_MAPIDSTR);
  H5DataSetWriterSetup s(f, path, H5T_NATIVE_CHAR, 1);
  std::vector<hsize_t> start(1, 0), n2write(1, nToWrite);
  H5DSWriterDimInfo dim(start, n2write);
  H5DataSetWriter w(&s);
  w.SetDimInfoForWriting(&dim);
  w.WriteData(&chars.at(0,0), nToWrite);

  // write the string length attribute
  hid_t fid(H5DataReader::GetFileId(f));
  hid_t dataId(H5Dopen(fid, path));
  if (dataId > -1)
  {
    xfpWriteAttributeInt(dataId, MFBC_MAX_STR_LEN, 1, &maxStrLen);
    H5Dclose(dataId);
  }
} // iWriteListBcMapIdsToH5
//------------------------------------------------------------------------------
/// \brief This function writes the mapids to the h5 file if they exist
//------------------------------------------------------------------------------
static void expWriteMapIdsForListBcs (MfGlobal* a_global,
                                      TxtExporter *a_exp)
{
  using namespace MfData::Packages;
  if (!a_global)
    return;

  const char *name(0);
  MfPackage *pack(a_global->GetPackage("NAM1"));
  if (!pack || !pack->GetField(NameFile::FNAME, &name) || !name)
    return;

  CStr fname(name);
  if (fname.IsEmpty())
    return;

  util::StripExtensionFromFilename(fname, fname);

  // loop through the list packages and see if mapids are in there
  // chd, riv, drn, ghb
  int i;
  std::vector<CStr> mapids, mapids2;
  CStr type, packages[4] = {DRN, RIV, GHB, CHD};
  for (i=0; i<4; i++)
  {
    // see if cellgrp exists for this type
    iGetListPackTypeFromPtr(packages[i], type);
    std::vector<int> &vCellgrp(iGetBcCellGrp(a_exp)[type]);
    if (vCellgrp.empty())
      continue;
    // if all values are -1 then don't do anything
    bool flag = false;
    for (size_t j=0; !flag && j<vCellgrp.size(); j++)
    {
      if (vCellgrp.at(j) != -1)
        flag = true;
    }
    if (!flag)
      continue;

    // read the mapids from the file
    if (a_exp->FileTypeExists(packages[i]))
    {
      // read the comments at the top of the file
      if (iReadListBcMapIdComments(fname, packages[i], mapids))
      {
        // fill an array of CStrings with the map ids
        iFillInListBcMapIds(mapids, vCellgrp, mapids2);
        if (!mapids2.empty())
        {
          // write the map ids to the h5 file
          iWriteListBcMapIdsToH5(a_exp, mapids2, type);
        }
      }
    }
  }
} // expWriteMapIdsForListBcs
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
static void Check1DH5IntValue (const char *a_file,
                               int a_line,
                               const CStr& a_filePath,
                               const CStr& a_h5path,
                               int a_expected)
{
  std::vector<int> v;
  H5DataSetReader n(a_filePath, a_h5path);
  n.GetAllData(v);
  _TS_ASSERT(a_file, a_line, v.size() == 1);
  _TS_ASSERT_EQUALS(a_file, a_line, a_expected, v.at(0));
} // Check1DH5IntValue
//------------------------------------------------------------------------------
static void Check1DH5DoubleValue (const char *a_file,
                                  int a_line,
                                  const CStr& a_filePath,
                                  const CStr& a_h5path,
                                  double a_expected)
{
  std::vector<double> v;
  H5DataSetReader n(a_filePath, a_h5path);
  n.GetAllData(v);
  _TS_ASSERT(a_file, a_line, v.size() == 1);
  _TS_ASSERT_EQUALS(a_file, a_line, a_expected, v.at(0));
} // Check1DH5DoubleValue
//------------------------------------------------------------------------------
static void CheckH5IntAtt (const char *a_file,
                           int a_line,
                           const CStr& a_filePath,
                           const CStr& a_h5path,
                           const CStr& a_attName,
                           int a_expected)
{
  int actual;
  H5DataSetReader n(a_filePath, a_h5path);
  n.GetAtt(a_attName, actual);
  _TS_ASSERT_EQUALS2(a_file, a_line, a_expected, actual);
} // CheckH5IntAtt
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
static void CheckH5ArrayValue (const char *a_file,
                               int a_line,
                               const CStr& a_filePath,
                               const CStr& a_h5path,
                               const double* a_expected,
                               size_t a_expectedLength)
{
  std::vector<double> actual;
  std::vector<double> expected(a_expected, a_expected+a_expectedLength);
  H5DataSetReader n(a_filePath, a_h5path);
  n.GetAllData(actual);
  _TS_ASSERT_DELTA_VEC(a_file, a_line, expected, actual, CXXDELTA);
}

//------------------------------------------------------------------------------
static void GetH5Array (const CStr& a_filePath,
                        const CStr& a_h5path,
                        vector<double>& a_array)
{
  H5DataSetReader n(a_filePath, a_h5path);
  n.GetAllData(a_array);
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
  //TS_ASSERT(t->IsTypeSupported(SWI));
  TS_ASSERT(t->IsTypeSupported(WEL));
  TS_ASSERT(t->IsTypeSupported(MNW));
  TS_ASSERT(t->IsTypeSupported(MNW2));
  TS_ASSERT(t->IsTypeSupported(MNWI));
  TS_ASSERT(t->IsTypeSupported(UZF));
  TS_ASSERT(t->IsTypeSupported(UPW));
  TS_ASSERT(!t->IsTypeSupported(VDF));
  TS_ASSERT(!t->IsTypeSupported(VSC));
  TS_ASSERT(t->IsTypeSupported(ZON));

  TS_ASSERT_EQUALS(e.m_types.size(), 47);
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
void ExpGmsH5T::testexpDataArray ()
{
  {
    ExpGmsH5 h5;
    TxtExporter t(TESTBASE);
    CStr f;
    util::GetTempDirectory(f);
    f += "\\array";
    MfPackage p(ARR_DIS_TOP);
    MfGlobal g(3, 2, 3, 2, 3, 2, 0);
    Real r[6] = {1,2,3,4,5,6}, m(1.5);
    int lay(2);
    p.SetField(Packages::Array::LAYER, &lay);
    p.SetField(Packages::Array::ARRAY, r);
    p.SetField(Packages::Array::MULT, &m);

    CreateDefaultMfH5File(f);
    expDataArray(&p, &g, &t, f, h5.GetMapArrays());

    H5DataReader::CloseAllH5FilesOpenForWriting();
    f += ".h5";
    std::pair<int, int> myPair;
    VEC_INT_PAIR idx(1, myPair);
    idx[0].first = 0; 
    idx[0].second = 6;
    H5DataSetReader h(f, "Arrays/top2", idx);
    std::vector<Real> vR;
    h.AllowTypeConversions(true);
    h.GetData(vR);
    TS_ASSERT(vR.size() == 6);
    for (int i=0; i<6; i++)
      TS_ASSERT_EQUALS(vR[i], r[i]);
    H5Reader::CloseAllH5Files();
    TS_ASSERT(!remove(f));
    TS_ASSERT_EQUALS2(GetTxtLineArrayMap(&t)["top2"],
                      "HDF5 1.5 -1 \"array.h5\" \"Arrays/top2\" 1 0 6");
  }
  {
    ExpGmsH5 h5;
    TxtExporter t(TESTBASE);
    // areal array
    CStr f;
    util::GetTempDirectory(f);
    f += "\\array";
    MfPackage p(ARR_EVT_EXT);
    MfGlobal g(3, 2, 3, 2, 3, 2, 0);
    g.PutCurrentPeriod(2);
    Real r[6] = {1,2,3,4,5,6}, m((Real)1.1);
    int lay(2);
    p.SetField(Packages::Array::LAYER, &lay);
    p.SetField(Packages::Array::ARRAY, r);
    p.SetField(Packages::Array::MULT, &m);

    CreateDefaultMfH5File(f);
    expDataArray(&p, &g, &t, f, h5.GetMapArrays());

    H5DataReader::CloseAllH5FilesOpenForWriting();
    f += ".h5";
    std::pair<int, int> myPair;
    VEC_INT_PAIR idx(3, myPair);
    idx[0].first = 2; 
    idx[0].second = 1;
    idx[1].first = 0; 
    idx[1].second = 6;
    idx[2].first = 1; 
    idx[2].second = 1;
    H5DataSetReader h(f, "ET/07. Property", idx);
    std::vector<Real> vR;
    h.AllowTypeConversions(true);
    h.GetData(vR);
    TS_ASSERT(vR.size() == 6);
    for (int i=0; i<6; i++)
      TS_ASSERT_EQUALS(vR[i], r[i]);
    VEC_INT_PAIR idx1(2, myPair);
    idx1[0].first = 2;
    idx1[0].second = 1;
    idx1[1].first = 0;
    idx1[1].second = 2;
    std::vector<Real> mult;
    H5DataSetReader h1(f, "ET/08. Property Multiplier", idx1);
    h1.AllowTypeConversions(true);
    h1.GetData(mult);
    TS_ASSERT(mult.size() == 2);
    TS_ASSERT_EQUALS(mult.at(0), 0);
    TS_ASSERT_EQUALS(mult.at(1), (Real)1.1);

    H5Reader::CloseAllH5Files();
    TS_ASSERT(!remove(f));
    TS_ASSERT_EQUALS2(GetTxtLineArrayMap(&t)["ET/07. Property2_2"],
                      "HDF5 1.1 -1 \"array.h5\" \"ET/07. Property\" 3 2 1 0 6 1 1");
  }
  {
    ExpGmsH5 h5;
    TxtExporter t(TESTBASE);
    // areal layer array
    CStr f;
    util::GetTempDirectory(f);
    f += "\\array";
    MfPackage p(ARR_EVT_LAY);
    MfGlobal g(3, 2, 3, 2, 3, 2, 0);
    g.PutCurrentPeriod(2);
    int r[6] = {1,2,3,4,5,6}, m(2);
    int lay(2);
    p.SetField(Packages::Array::LAYER, &lay);
    p.SetField(Packages::Array::ARRAY, r);
    p.SetField(Packages::Array::MULT, &m);

    CreateDefaultMfH5File(f);
    expDataArray(&p, &g, &t, f, h5.GetMapArrays());

    H5DataReader::CloseAllH5FilesOpenForWriting();
    f += ".h5";
    std::pair<int, int> myPair;
    VEC_INT_PAIR idx(2, myPair);
    idx[0].first = 0; 
    idx[0].second = 6;
    idx[1].first = 1; 
    idx[1].second = 1;
    H5DataSetReader h(f, "ET/09. Layer", idx);
    std::vector<int> vR;
    h.GetData(vR);
    TS_ASSERT(vR.size() == 6);
    for (int i=0; i<6; i++)
      TS_ASSERT_EQUALS(vR[i], r[i]);

    VEC_INT_PAIR idx1(1, myPair);
    idx1[0].first = 0;
    idx1[0].second = 2;
    std::vector<int> mult;
    H5DataSetReader h1(f, "ET/10. Layer Multiplier", idx1);
    h1.GetData(mult);
    TS_ASSERT(mult.size() == 2);
    TS_ASSERT_EQUALS(mult.at(0), 0);
    TS_ASSERT_EQUALS(mult.at(1), 2);

    H5Reader::CloseAllH5Files();
    TS_ASSERT(!remove(f));
    TS_ASSERT_EQUALS2(GetTxtLineArrayMap(&t)["ET/09. Layer2"],
                      "HDF5 2.0 -1 \"array.h5\" \"ET/09. Layer\" 2 0 6 1 1");
  }
  {
    ExpGmsH5 h5;
    TxtExporter t(TESTBASE);
    // extinction depth segment
    CStr f;
    util::GetTempDirectory(f);
    f += "\\array";
    MfPackage p(ARR_ETS_PXDP);
    MfGlobal g(3, 2, 3, 2, 3, 2, 0);
    g.PutCurrentPeriod(2);
    Real r[12] = {1,2,3,4,5,6,11,12,13,14,15,16}, m((Real)1.1);
    int lay(2);
    p.SetField(Packages::Array::LAYER, &lay);
    p.SetField(Packages::Array::ARRAY, r);
    p.SetField(Packages::Array::MULT, &m);

    CreateDefaultMfH5File(f);
    expDataArray(&p, &g, &t, f, h5.GetMapArrays());

    m = REAL(1.2);
    p.SetField(Packages::Array::ARRAY, r+6);
    expDataArray(&p, &g, &t, f, h5.GetMapArrays());

    H5DataReader::CloseAllH5FilesOpenForWriting();
    f += ".h5";
    std::pair<int, int> myPair;
    VEC_INT_PAIR idx(3, myPair);
    idx[0].first = 0; 
    idx[0].second = 2;
    idx[1].first = 0; 
    idx[1].second = 6;
    idx[2].first = 1; 
    idx[2].second = 1;
    H5DataSetReader h(f, "ETS/16. Ext Depth", idx);
    std::vector<Real> vR;
    h.AllowTypeConversions(true);
    h.GetData(vR);
    TS_ASSERT(vR.size() == 12);
    for (int i=0; i<12; i++)
      TS_ASSERT_EQUALS(vR[i], r[i]);
    VEC_INT_PAIR idx1(2, myPair);
    idx1[0].first = 0;
    idx1[0].second = 1;
    idx1[1].first = 0;
    idx1[1].second = 2;
    std::vector<Real> mult;
    H5DataSetReader h1(f, "ETS/17. Ext Depth Multiplier", idx1);
    h1.AllowTypeConversions(true);
    h1.GetData(mult);
    TS_ASSERT(mult.size() == 2);
    TS_ASSERT_EQUALS(mult.at(0), 0);
    TS_ASSERT_EQUALS(mult.at(1), (Real)1.1);

    H5Reader::CloseAllH5Files();
    TS_ASSERT(!remove(f));
    TS_ASSERT_EQUALS2(GetTxtLineArrayMap(&t)["ETS/16. Ext Depth2_0"],
                      "HDF5 1.1 -1 \"array.h5\" \"ETS/16. Ext Depth\" 3 0 1 0 6 1 1");
    TS_ASSERT_EQUALS2(GetTxtLineArrayMap(&t)["ETS/16. Ext Depth2_1"],
                      "HDF5 1.2 -1 \"array.h5\" \"ETS/16. Ext Depth\" 3 1 1 0 6 1 1");
  }
  {
    ExpGmsH5 h5;
    TxtExporter t(TESTBASE);
    // SUB package arrays with same layer
    CStr f;
    util::GetTempDirectory(f);
    f += "\\array";
    MfPackage p(ARR_SUB_HC);
    MfGlobal g(3, 2, 3, 2, 3, 2, 0);
    Real r1[6] = {1, 2, 3, 4, 5, 6};
    Real r2[6] = {11, 12, 13, 14, 15, 16};
    Real m(1.5);
    int lay(2);
    p.SetField(Packages::Array::LAYER, &lay);
    p.SetField(Packages::Array::ARRAY, r1);
    p.SetField(Packages::Array::MULT, &m);

    CreateDefaultMfH5File(f);
    expDataArray(&p, &g, &t, f, h5.GetMapArrays());

    p.SetField(Packages::Array::ARRAY, r2);
    m = 2.0;
    expDataArray(&p, &g, &t, f, h5.GetMapArrays());

    H5DataReader::CloseAllH5FilesOpenForWriting();
    f += ".h5";
    std::pair<int, int> myPair;
    VEC_INT_PAIR idx(2, myPair);
    idx[0].first = 0; 
    idx[0].second = 6;
    idx[1].first = 0;
    idx[1].second = 1;

    // check first array
    H5DataSetReader h1(f, "SUB/14. HC", idx);
    std::vector<Real> vR;
    h1.AllowTypeConversions(true);
    h1.GetData(vR);
    TS_ASSERT_EQUALS2(6, vR.size());
    for (int i=0; i<6; i++)
      TS_ASSERT_EQUALS(vR[i], r1[i]);
    TS_ASSERT_EQUALS2(GetTxtLineArrayMap(&t)["SUB/14. HC1"],
                      "HDF5 1.5 -1 \"array.h5\" \"SUB/14. HC\" 2 0 6 0 1");

    // check second array
    idx[1].first = 1;
    idx[1].second = 1;
    H5DataSetReader h2(f, "SUB/14. HC", idx);
    h2.AllowTypeConversions(true);
    h2.GetData(vR);
    TS_ASSERT_EQUALS2(6, vR.size());
    for (int i=0; i<6; i++)
      TS_ASSERT_EQUALS(vR[i], r2[i]);
    TS_ASSERT_EQUALS2(GetTxtLineArrayMap(&t)["SUB/14. HC2"],
                      "HDF5 2.0 -1 \"array.h5\" \"SUB/14. HC\" 2 0 6 1 1");

    H5Reader::CloseAllH5Files();
    TS_ASSERT(!remove(f));
  }
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
static void iFillMap_RCH_Test (TxtExporter* a_exp)
{
  std::map<CStr,CStr> &m(GetTxtLineArrayMap(a_exp));
    m.insert(std::make_pair("Recharge/07. Property1_0", "l1"));
    m.insert(std::make_pair("Recharge/09. Layer1", "l2"));
    m.insert(std::make_pair("Recharge/07. Property2_0", "l3"));
    m.insert(std::make_pair("Recharge/09. Layer2", "l4"));
    m.insert(std::make_pair("Recharge/07. Property3_0", "l5"));
    m.insert(std::make_pair("Recharge/09. Layer3", "l6"));
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpRecharge ()
{
  using namespace Packages;
  MfGlobal g(10, 15, 3, 2, 3, 2, 0);
  MfPackage pack(Packages::RCH);

  int nrchop(1), inrech(1), inirch(1);
  CStr cStr, str;

  {
    TxtExporter t(TESTBASE);
    iFillMap_RCH_Test(&t);
    expRecharge(&pack, 1, &t);
    t.GetFileContents(Packages::RCH, cStr);
    TS_ASSERT(cStr.empty());

    pack.SetField(Packages::RCHpack::NRCHOP, &nrchop);
    pack.SetField(Packages::RCHpack::INRECH, &inrech);
    pack.SetField(Packages::RCHpack::INIRCH, &inirch);

    expRecharge(&pack, 1, &t);

    str = "1 40\n"
          "1\n"
          "l1\n";
    t.GetFileContents(Packages::RCH, cStr);
    TS_ASSERT_EQUALS2(str, cStr);
  }
  {
    TxtExporter t(TESTBASE);
    iFillMap_RCH_Test(&t);
    nrchop = 2;
    expRecharge(&pack, 1, &t);
    inrech = -1;
    expRecharge(&pack, 2, &t);
    inrech = 1;
    inirch = -1;
    expRecharge(&pack, 3, &t);

    str = "2 40\n"
          "1 1\n"
          "l1\n"
          "l2\n"
          "-1 1\n"
          "l4\n"
          "1 -1\n"
          "l5\n";
    t.GetFileContents(Packages::RCH, cStr);
    TS_ASSERT_EQUALS2(str, cStr);
  }
  {
    TxtExporter t(TESTBASE);
    iFillMap_RCH_Test(&t);
    ParamList *list(0);
    Parameters::GetParameterList(&list);
    list->Clear();

    Param p;
    p.m_name = "RCH_1";
    p.m_start = 1.0;
    p.m_value = 1.0;
    p.m_type = "RCH";
    p.m_key = -2;
    p.m_instStress.insert(std::make_pair("rch_1", std::vector<int>()));
    p.m_instStress["rch_1"].push_back(1);
    p.m_instStress["rch_1"].push_back(2);
    list->PushBack(&p);

    p.m_name = "EVT_1";
    p.m_start = 2.0;
    p.m_value = 2.0;
    p.m_type = "EVT";
    p.m_key = -3;
    list->PushBack(&p);

    p.m_name = "RCH_2";
    p.m_start = 3.0;
    p.m_value = 3.0;
    p.m_type = "RCH";
    p.m_key = -4;
    PClust clust;
    clust.m_mlt = "array-1";
    clust.m_zon = "zone-array-1";
    clust.m_iz.push_back(1);
    clust.m_iz.push_back(2);
    clust.m_iz.push_back(3);
    p.m_clust.push_back(clust);
    p.m_instStress.clear();
    p.m_instStress.insert(std::make_pair("rch_2", std::vector<int>()));
    p.m_instStress["rch_2"].push_back(2);
    p.m_instStress["rch_2"].push_back(3);
    list->PushBack(&p);

    p.m_name = "RCH_NOWRITE";
    p.m_start = 3.0;
    p.m_value = 3.0;
    p.m_type = "RCH";
    p.m_key = -100;
    p.m_instStress.clear();
    list->PushBack(&p);

    expRecharge(&pack, 1, &t);
    expRecharge(&pack, 2, &t);
    list->At(3, &p);
    p.m_instStress["rch_nowrite"].push_back(3);
    list->UpdateParameter(&p);
    expRecharge(&pack, 3, &t);

    str = "PARAMETER 3\n"
          "2 40\n"
          "RCH_1 RCH 1.0 0\n"
          "RCH_2 RCH 3.0 1\n"
          "array-1 zone-array-1 1 2 3 \n"
          "RCH_NOWRITE RCH 3.0 1\n"
          "array-1 zone-array-1 1 2 3 \n"
          "1 -1\n"
          "RCH_1\n"
          "1 -1\n"
          "RCH_1\n"
          "RCH_2\n"
          "1 -1\n"
          "RCH_2\n"
          "RCH_NOWRITE\n";
    t.GetFileContents(Packages::RCH, cStr);
    TS_ASSERT_EQUALS2(str, cStr);
    list->Clear();
  }
}
static void iFillMap_EVT_Test (TxtExporter* a_exp)
{
  std::map<CStr,CStr> &m(GetTxtLineArrayMap(a_exp));
  m.insert(std::make_pair("ET/07. Property1_0", "l1"));
  m.insert(std::make_pair("ET/07. Property1_1", "l2"));
  m.insert(std::make_pair("ET/07. Property1_2", "l3"));
  m.insert(std::make_pair("ET/09. Layer1", "l4"));
  m.insert(std::make_pair("ET/07. Property2_0", "l5"));
  m.insert(std::make_pair("ET/07. Property2_1", "l6"));
  m.insert(std::make_pair("ET/07. Property2_2", "l7"));
  m.insert(std::make_pair("ET/09. Layer2", "l8"));
  m.insert(std::make_pair("ET/07. Property3_0", "l9"));
  m.insert(std::make_pair("ET/07. Property3_1", "l10"));
  m.insert(std::make_pair("ET/07. Property3_2", "l11"));
  m.insert(std::make_pair("ET/09. Layer3", "l12"));
  m.insert(std::make_pair("ET/07. Property4_0", "l13"));
  m.insert(std::make_pair("ET/07. Property4_1", "l14"));
  m.insert(std::make_pair("ET/07. Property4_2", "l15"));
  m.insert(std::make_pair("ET/09. Layer4", "l16"));
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpEVT ()
{
  using namespace Packages;
  MfGlobal g(10, 15, 3, 2, 3, 2, 0);
  MfPackage evtPack(Packages::EVT);


  int nevtop(1), insurf(0), inevtr(0), inexdp(0), inievt(0), netseg(0),
      insgdf(0);
  CStr cStr, str;
  {
    TxtExporter t(TESTBASE);
    iFillMap_EVT_Test(&t);
    expET(&evtPack, 1, &t, TESTBASE);
    t.GetFileContents(Packages::RCH, cStr);
    TS_ASSERT(cStr.empty());

    evtPack.SetField(Packages::EVTpack::NEVTOP, &nevtop);
    evtPack.SetField(Packages::EVTpack::INSURF, &insurf);
    evtPack.SetField(Packages::EVTpack::INEVTR, &inevtr);
    evtPack.SetField(Packages::EVTpack::INEXDP, &inexdp);
    evtPack.SetField(Packages::EVTpack::INIEVT, &inievt);
    evtPack.SetField(Packages::EVTpack::NETSEG, &netseg);
    evtPack.SetField(Packages::EVTpack::INSGDF, &insgdf);

    expET(&evtPack, 1, &t, TESTBASE);

    str = "1 40\n0 0 0\nl1\nl2\nl3\n";
    t.GetFileContents(Packages::EVT, cStr);
    TS_ASSERT_EQUALS2(str, cStr);
  }
  {
    TxtExporter t(TESTBASE);
    iFillMap_EVT_Test(&t);
    nevtop=2;
    insurf=-1;
    expET(&evtPack, 1, &t,TESTBASE);
    insurf=1;
    inevtr=-1;
    expET(&evtPack, 2, &t, TESTBASE);
    inevtr=1;
    expET(&evtPack, 3, &t, TESTBASE);
    inexdp=-1;
    inievt=-1;
    expET(&evtPack, 4, &t, TESTBASE);

    str = "2 40\n-1 0 0 0\nl2\nl3\nl4\n1 -1 0 0\nl5\nl7\nl8\n1 1 0 0\n"
          "l9\nl10\nl11\nl12\n1 1 -1 -1\nl13\nl14\n";
    t.GetFileContents(Packages::EVT, cStr);
    TS_ASSERT_EQUALS2(str, cStr);
  }
  {
    ParamList *list(0);
    Parameters::GetParameterList(&list);
    list->Clear();

    Param p;
    p.m_name = "evt_1";
    p.m_start = 1.0;
    p.m_value = 1.0;
    p.m_type = "EVT";
    p.m_key = -2;
    p.m_instStress.insert(std::make_pair("evt_1", std::vector<int>()));
    p.m_instStress["evt_1"].push_back(3);
    list->PushBack(&p);

    p.m_name = "rch_1";
    p.m_start = 2.0;
    p.m_value = 2.0;
    p.m_type = "RCH";
    p.m_key = -3;
    list->PushBack(&p);

    p.m_name = "evt_2";
    p.m_start = 3.0;
    p.m_value = 3.0;
    p.m_type = "EVT";
    p.m_key = -4;
    PClust clust;
    clust.m_mlt = "array-1";
    clust.m_zon = "zone-array-1";
    clust.m_iz.push_back(1);
    clust.m_iz.push_back(2);
    clust.m_iz.push_back(3);
    p.m_clust.push_back(clust);
    p.m_instStress.clear();
    p.m_instStress.insert(std::make_pair("evt_2", std::vector<int>()));
    p.m_instStress["evt_2"].push_back(3);
    p.m_instStress["evt_2"].push_back(4);
    list->PushBack(&p);

    TxtExporter t(TESTBASE);
    iFillMap_EVT_Test(&t);

    nevtop=2;
    insurf=-1;
    inevtr=0;
    inexdp=0;
    inievt=0;
    expET(&evtPack, 1, &t,TESTBASE);
    insurf=1;
    inevtr=-1;
    expET(&evtPack, 2, &t, TESTBASE);
    inevtr=1;
    expET(&evtPack, 3, &t, TESTBASE);
    inexdp=-1;
    inievt=-1;
    expET(&evtPack, 4, &t, TESTBASE);

    str = "PARAMETER 2\n"
          "2 40\n"
          "evt_1 EVT 1.0 0\n"
          "evt_2 EVT 3.0 1\n"
          "array-1 zone-array-1 1 2 3 \n"
          "-1 0 0 0\n"
          "l3\n"
          "l4\n"
          "1 -1 0 0\n"
          "l5\n"
          "l7\n"
          "l8\n"
          "1 1 0 0\n"
          "l9\n"
          "evt_1\n"
          "evt_2\n"
          "l11\n"
          "l12\n"
          "1 1 -1 -1\n"
          "l13\n"
          "evt_2\n";
    t.GetFileContents(Packages::EVT, cStr);
    TS_ASSERT_EQUALS2(str, cStr);
    list->Clear();
  }
  {
    ParamList *list(0);
    Parameters::GetParameterList(&list);
    list->Clear();

    {
      Param p;
      p.m_name = "evt_1";
      p.m_start = 1.0;
      p.m_value = 1.0;
      p.m_type = "EVT";
      p.m_key = -2;
      p.m_instStress.insert(std::make_pair("evt_1", std::vector<int>()));
      p.m_instStress["evt_1"].push_back(3);
      list->PushBack(&p);
    }

    {
      Param p;
      p.m_name = "rch_1";
      p.m_start = 2.0;
      p.m_value = 2.0;
      p.m_type = "RCH";
      p.m_key = -3;
      list->PushBack(&p);
    }

    {
      Param p;
      p.m_name = "evt_2";
      p.m_start = 5.0;
      p.m_value = 5.0;
      p.m_type = "EVT";
      p.m_key = -4;
      PClust clust;
      clust.m_mlt = "array-1";
      clust.m_zon = "zone-array-1";
      clust.m_iz.push_back(1);
      clust.m_iz.push_back(2);
      clust.m_iz.push_back(3);
      p.m_clust.push_back(clust);
      p.m_clust.push_back(clust);
      p.m_clust.push_back(clust);
      p.m_clust.push_back(clust);
      p.m_instNames.push_back("evt_2_1");
      p.m_instNames.push_back("evt_2_2");
      p.m_instStress["evt_2_1"].push_back(1);
      p.m_instStress["evt_2_1"].push_back(3);
      p.m_instStress["evt_2_2"].push_back(2);
      p.m_instStress["evt_2_2"].push_back(4);
      list->PushBack(&p);
    }

    TxtExporter t(TESTBASE);
    iFillMap_EVT_Test(&t);

    nevtop=2;
    insurf=-1;
    inevtr=0;
    inexdp=0;
    inievt=0;
    expET(&evtPack, 1, &t,TESTBASE);
    insurf=1;
    inevtr=-1;
    expET(&evtPack, 2, &t, TESTBASE);
    inevtr=1;
    expET(&evtPack, 3, &t, TESTBASE);
    inexdp=-1;
    inievt=-1;
    expET(&evtPack, 4, &t, TESTBASE);

    str = "PARAMETER 2\n"
          "2 40\n"
          "evt_1 EVT 1.0 0\n"
          "evt_2 EVT 5.0 2 INSTANCES 2\n"
          "evt_2_1\n"
          "array-1 zone-array-1 1 2 3 \n"
          "array-1 zone-array-1 1 2 3 \n"
          "evt_2_2\n"
          "array-1 zone-array-1 1 2 3 \n"
          "array-1 zone-array-1 1 2 3 \n"
          "-1 0 0 0\n"
          "evt_2 evt_2_1\n"
          "l3\n"
          "l4\n"
          "1 -1 0 0\n"
          "l5\n"
          "evt_2 evt_2_2\n"
          "l7\n"
          "l8\n"
          "1 1 0 0\n"
          "l9\n"
          "evt_1\n"
          "evt_2 evt_2_1\n"
          "l11\n"
          "l12\n"
          "1 1 -1 -1\n"
          "l13\n"
          "evt_2 evt_2_2\n";
    t.GetFileContents(Packages::EVT, cStr);
    TS_ASSERT_EQUALS2(str, cStr);
    list->Clear();
  }
}
static void iFillMap_ETS_Test (TxtExporter* a_exp)
{
  std::map<CStr,CStr> &m(GetTxtLineArrayMap(a_exp));
  m.insert(std::make_pair("ETS/07. Property1_0", "l1"));
  m.insert(std::make_pair("ETS/07. Property1_1", "l2"));
  m.insert(std::make_pair("ETS/07. Property1_2", "l3"));
  m.insert(std::make_pair("ETS/09. Layer1", "l4"));
  m.insert(std::make_pair("ETS/16. Ext Depth1_0", "PXDP1_0"));
  m.insert(std::make_pair("ETS/16. Ext Depth1_1", "PXDP1_1"));
  m.insert(std::make_pair("ETS/18. Evap Rate1_0", "PETM1_0"));
  m.insert(std::make_pair("ETS/18. Evap Rate1_1", "PETM1_1"));
  m.insert(std::make_pair("ETS/07. Property2_0", "l5"));
  m.insert(std::make_pair("ETS/07. Property2_1", "l6"));
  m.insert(std::make_pair("ETS/07. Property2_2", "l7"));
  m.insert(std::make_pair("ETS/09. Layer2", "l8"));
  m.insert(std::make_pair("ETS/16. Ext Depth2_0", "PXDP2_0"));
  m.insert(std::make_pair("ETS/16. Ext Depth2_1", "PXDP2_1"));
  m.insert(std::make_pair("ETS/18. Evap Rate2_0", "PETM2_0"));
  m.insert(std::make_pair("ETS/18. Evap Rate2_1", "PETM2_1"));
  m.insert(std::make_pair("ETS/07. Property3_0", "l9"));
  m.insert(std::make_pair("ETS/07. Property3_1", "l10"));
  m.insert(std::make_pair("ETS/07. Property3_2", "l11"));
  m.insert(std::make_pair("ETS/09. Layer3", "l12"));
  m.insert(std::make_pair("ETS/16. Ext Depth3_0", "PXDP3_0"));
  m.insert(std::make_pair("ETS/16. Ext Depth3_1", "PXDP3_1"));
  m.insert(std::make_pair("ETS/18. Evap Rate3_0", "PETM3_0"));
  m.insert(std::make_pair("ETS/18. Evap Rate3_1", "PETM3_1"));
  m.insert(std::make_pair("ETS/07. Property4_0", "l13"));
  m.insert(std::make_pair("ETS/07. Property4_1", "l14"));
  m.insert(std::make_pair("ETS/07. Property4_2", "l15"));
  m.insert(std::make_pair("ETS/09. Layer4", "l16"));
  m.insert(std::make_pair("ETS/16. Ext Depth4_0", "PXDP4_0"));
  m.insert(std::make_pair("ETS/16. Ext Depth4_1", "PXDP4_1"));
  m.insert(std::make_pair("ETS/18. Evap Rate4_0", "PETM4_0"));
  m.insert(std::make_pair("ETS/18. Evap Rate4_1", "PETM4_1"));
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpETS ()
{
  using namespace Packages;
  MfGlobal g(10, 15, 3, 2, 3, 2, 0);
  MfPackage evtPack(Packages::ETS);

  CStr f;
  util::GetTempDirectory(f);
  f += "\\array";
  CStr f2(f+".h5");


  int nevtop(1), insurf(0), inevtr(0), inexdp(0), inievt(0), netseg(0),
      insgdf(0);
  CStr cStr, str, f1(f+".ets");
  {
    CreateDefaultMfH5File(f);
    TxtExporter t(f);
    iFillMap_ETS_Test(&t);

    expET(&evtPack, 1, &t, t.GetBaseFileName());
    t.GetFileContents(Packages::RCH, cStr);
    TS_ASSERT(cStr.empty());

    evtPack.SetField(Packages::EVTpack::NEVTOP, &nevtop);
    evtPack.SetField(Packages::EVTpack::INSURF, &insurf);
    evtPack.SetField(Packages::EVTpack::INEVTR, &inevtr);
    evtPack.SetField(Packages::EVTpack::INEXDP, &inexdp);
    evtPack.SetField(Packages::EVTpack::INIEVT, &inievt);
    evtPack.SetField(Packages::EVTpack::NETSEG, &netseg);
    evtPack.SetField(Packages::EVTpack::INSGDF, &insgdf);

    expET(&evtPack, 1, &t, t.GetBaseFileName());

    str = "1 40 0 0\n"
          "0 0 0\n"
          "l1\n"
          "l2\n"
          "l3\n";
    t.GetFileContents(Packages::ETS, cStr);
    TS_ASSERT_EQUALS2(str, cStr);

    // Verify NETSEG in h5 file
    TS_ASSERT_H5_INT(f2, "ETS/20. Number of Segments", 0);
    H5DataReader::CloseAllH5FilesOpenForWriting();
    TS_ASSERT(!remove(f2));
  }
  TS_ASSERT(!remove(f1));

  {
    CreateDefaultMfH5File(f);
    TxtExporter t(f);
    iFillMap_ETS_Test(&t);
    nevtop=1;
    netseg=3;
    insurf=-1;
    expET(&evtPack, 1, &t,t.GetBaseFileName());
    insurf=1;
    inevtr=-1;
    expET(&evtPack, 2, &t, t.GetBaseFileName());
    inevtr=1;
    expET(&evtPack, 3, &t, t.GetBaseFileName());
    inexdp=-1;
    inievt=-1;
    insgdf=-1;
    expET(&evtPack, 4, &t, t.GetBaseFileName());

    str = "1 40 0 3\n"
          "-1 0 0 0 0\n"
          "l2\n"
          "l3\n"
          "PXDP1_0\n"
          "PETM1_0\n"
          "PXDP1_1\n"
          "PETM1_1\n"
          "1 -1 0 0 0\n"
          "l5\n"
          "l7\n"
          "PXDP2_0\n"
          "PETM2_0\n"
          "PXDP2_1\n"
          "PETM2_1\n"
          "1 1 0 0 0\n"
          "l9\n"
          "l10\n"
          "l11\n"
          "PXDP3_0\n"
          "PETM3_0\n"
          "PXDP3_1\n"
          "PETM3_1\n"
          "1 1 -1 -1 -1\n"
          "l13\n"
          "l14\n";
    t.GetFileContents(Packages::ETS, cStr);
    TS_ASSERT_EQUALS2(str, cStr);

    // Verify NETSEG in h5 file
    TS_ASSERT_H5_INT(f2, "ETS/20. Number of Segments", 3);
    H5DataReader::CloseAllH5FilesOpenForWriting();
    TS_ASSERT(!remove(f2));
  }
  TS_ASSERT(!remove(f1));

  {
    ParamList *list(0);
    Parameters::GetParameterList(&list);
    list->Clear();

    Param p;
    p.m_name = "evt_1";
    p.m_start = 1.0;
    p.m_value = 1.0;
    p.m_type = "ETS";
    p.m_key = -2;
    p.m_instStress.insert(std::make_pair("evt_1", std::vector<int>()));
    p.m_instStress["evt_1"].push_back(3);
    list->PushBack(&p);

    p.m_name = "rch_1";
    p.m_start = 2.0;
    p.m_value = 2.0;
    p.m_type = "RCH";
    p.m_key = -3;
    list->PushBack(&p);

    p.m_name = "evt_2";
    p.m_start = 3.0;
    p.m_value = 3.0;
    p.m_type = "ETS";
    p.m_key = -4;
    PClust clust;
    clust.m_mlt = "array-1";
    clust.m_zon = "zone-array-1";
    clust.m_iz.push_back(1);
    clust.m_iz.push_back(2);
    clust.m_iz.push_back(3);
    p.m_clust.push_back(clust);
    p.m_instStress.clear();
    p.m_instStress.insert(std::make_pair("evt_2", std::vector<int>()));
    p.m_instStress["evt_2"].push_back(3);
    p.m_instStress["evt_2"].push_back(4);
    list->PushBack(&p);

    CreateDefaultMfH5File(f);
    TxtExporter t(f);
    iFillMap_ETS_Test(&t);

    nevtop=2;
    netseg=0;
    insurf=-1;
    inevtr=0;
    inexdp=0;
    inievt=0;
    expET(&evtPack, 1, &t,t.GetBaseFileName());
    insurf=1;
    inevtr=-1;
    expET(&evtPack, 2, &t, t.GetBaseFileName());
    inevtr=1;
    expET(&evtPack, 3, &t, t.GetBaseFileName());
    inexdp=-1;
    inievt=-1;
    expET(&evtPack, 4, &t, t.GetBaseFileName());

    str = "2 40 2 0\n"
          "evt_1 ETS 1.0 0\n"
          "evt_2 ETS 3.0 1\n"
          "array-1 zone-array-1 1 2 3 \n"
          "-1 0 0 0\n"
          "l3\n"
          "l4\n"
          "1 -1 0 0\n"
          "l5\n"
          "l7\n"
          "l8\n"
          "1 1 0 0\n"
          "l9\n"
          "evt_1\n"
          "evt_2\n"
          "l11\n"
          "l12\n"
          "1 1 -1 -1\n"
          "l13\n"
          "evt_2\n";
    t.GetFileContents(Packages::ETS, cStr);
    TS_ASSERT_EQUALS2(str, cStr);
    list->Clear();

    // Verify NETSEG in h5 file
    TS_ASSERT_H5_INT(f2, "ETS/20. Number of Segments", 0);
    H5DataReader::CloseAllH5FilesOpenForWriting();
    TS_ASSERT(!remove(f2));
  }
  TS_ASSERT(!remove(f1));

  {
    ParamList *list(0);
    Parameters::GetParameterList(&list);
    list->Clear();

    {
      Param p;
      p.m_name = "evt_1";
      p.m_start = 1.0;
      p.m_value = 1.0;
      p.m_type = "ETS";
      p.m_key = -2;
      p.m_instStress["evt_1"].push_back(3);
      list->PushBack(&p);
    }

    {
      Param p;
      p.m_name = "rch_1";
      p.m_start = 2.0;
      p.m_value = 2.0;
      p.m_type = "RCH";
      p.m_key = -3;
      list->PushBack(&p);
    }

    {
      Param p;
      p.m_name = "evt_2";
      p.m_start = 3.0;
      p.m_value = 3.0;
      p.m_type = "ETS";
      p.m_key = -4;
      PClust clust;
      clust.m_mlt = "array-1";
      clust.m_zon = "zone-array-1";
      clust.m_iz.push_back(1);
      clust.m_iz.push_back(2);
      clust.m_iz.push_back(3);
      p.m_clust.push_back(clust);
      p.m_clust.push_back(clust);
      p.m_clust.push_back(clust);
      p.m_clust.push_back(clust);
      p.m_instNames.push_back("evt_2_1");
      p.m_instNames.push_back("evt_2_2");
      p.m_instStress["evt_2_1"].push_back(1);
      p.m_instStress["evt_2_1"].push_back(3);
      p.m_instStress["evt_2_2"].push_back(2);
      p.m_instStress["evt_2_2"].push_back(4);
      list->PushBack(&p);
    }

    CreateDefaultMfH5File(f);
    TxtExporter t(f);
    iFillMap_ETS_Test(&t);

    nevtop=2;
    insurf=-1;
    inevtr=0;
    inexdp=0;
    inievt=0;
    expET(&evtPack, 1, &t,t.GetBaseFileName());
    insurf=1;
    inevtr=-1;
    expET(&evtPack, 2, &t, t.GetBaseFileName());
    inevtr=1;
    expET(&evtPack, 3, &t, t.GetBaseFileName());
    inexdp=-1;
    inievt=-1;
    expET(&evtPack, 4, &t, t.GetBaseFileName());

    str = "2 40 2 0\n"
          "evt_1 ETS 1.0 0\n"
          "evt_2 ETS 3.0 2 INSTANCES 2\n"
          "evt_2_1\n"
          "array-1 zone-array-1 1 2 3 \n"
          "array-1 zone-array-1 1 2 3 \n"
          "evt_2_2\n"
          "array-1 zone-array-1 1 2 3 \n"
          "array-1 zone-array-1 1 2 3 \n"
          "-1 0 0 0\n"
          "evt_2 evt_2_1\n"
          "l3\n"
          "l4\n"
          "1 -1 0 0\n"
          "l5\n"
          "evt_2 evt_2_2\n"
          "l7\n"
          "l8\n"
          "1 1 0 0\n"
          "l9\n"
          "evt_1\n"
          "evt_2 evt_2_1\n"
          "l11\n"
          "l12\n"
          "1 1 -1 -1\n"
          "l13\n"
          "evt_2 evt_2_2\n";
    t.GetFileContents(Packages::ETS, cStr);
    TS_ASSERT_EQUALS2(str, cStr);
    list->Clear();

    // Verify NETSEG in h5 file
    TS_ASSERT_H5_INT(f2, "ETS/20. Number of Segments", 0);
    H5DataReader::CloseAllH5FilesOpenForWriting();
    TS_ASSERT(!remove(f2));
  }
  TS_ASSERT(!remove(f1));
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpOc ()
{
  using namespace Packages;
  MfPackage p(Packages::OC);
  int ihedfm = 0;
  int iddnfm = 0;
  int ihedun = 0;
  int iddnun = 0;
  int ibdopt = 1;
  int iauxsv = 0;
  char* chedfm[2] = {"", "format"};

  p.SetField("IHEDFM", &ihedfm);
  p.SetField("IDDNFM", &iddnfm);
  p.SetField("IHEDUN", &ihedun);
  p.SetField("IDDNUN", &iddnun);
  p.SetField("IBDOPT", &ibdopt);
  p.SetField("CHEDFM", chedfm[0]);

  CStr expected, output;

  {
    // don't export without all fields
    TxtExporter t(TESTBASE);

    expOC(&p, &t);

    t.GetFileContents(Packages::OC, output);
    TS_ASSERT(output.empty());

    // includes unit and print format when unit not zero
    p.SetField("IAUXSV", &iauxsv);
    expOC(&p, &t);

    ihedun = 1;
    expOC(&p, &t);
    t.GetFileContents(Packages::OC, output);
    expected =  "HEAD SAVE UNIT 1\n";
    TS_ASSERT_EQUALS2(expected, output);
    ihedun = 0;
  }

  // test head format only
  {
    // includes unit and print format when unit not zero
    TxtExporter t(TESTBASE);
    ihedfm = 9;
    expOC(&p, &t);
    t.GetFileContents(Packages::OC, output);
    expected =  "HEAD PRINT FORMAT 9\n";
    TS_ASSERT_EQUALS2(expected, output);
    ihedfm = 0;
  }

  // test drawdown unit only
  {
    // includes unit and print format when unit not zero
    TxtExporter t(TESTBASE);
    iddnun = 2;
    expOC(&p, &t);
    t.GetFileContents(Packages::OC, output);
    expected =  "DRAWDOWN SAVE UNIT 2\n";
    TS_ASSERT_EQUALS2(expected, output);
    iddnun = 0;
  }

  // test drawdown format only
  {
    // includes unit and print format when unit not zero
    TxtExporter t(TESTBASE);
    iddnfm = 8;
    expOC(&p, &t);
    t.GetFileContents(Packages::OC, output);
    expected =  "DRAWDOWN PRINT FORMAT 8\n";
    TS_ASSERT_EQUALS2(expected, output);
    iddnfm = 0;
  }

  // test head save format only
  {
    // includes format text when text is non-blank
    TxtExporter t(TESTBASE);
    p.SetField("CHEDFM", chedfm[1]);
    expOC(&p, &t);
    t.GetFileContents(Packages::OC, output);
    expected =  "HEAD SAVE FORMAT format\n";
    TS_ASSERT_EQUALS2(expected, output);
    p.SetField("CHEDFM", chedfm[0]);
  }

  // include "COMPACT BUDGET" when IBDOPT=2 and IAUXSV=0
  {
    TxtExporter t(TESTBASE);
    ibdopt = 2;
    expOC(&p, &t);

    t.GetFileContents(Packages::OC, output);
    expected = "COMPACT BUDGET\n";
    TS_ASSERT_EQUALS2(expected, output);
  }

  // include "COMPACT BUDGET AUXILIARY" when IBDOPT=2 and IAUXSV=1
  {
    TxtExporter t(TESTBASE);
    iauxsv = 1;
    expOC(&p, &t);

    t.GetFileContents(Packages::OC, output);
    expected = "COMPACT BUDGET AUXILIARY\n";
    TS_ASSERT_EQUALS2(expected, output);
  }
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpOct ()
{
  using namespace Packages;
  MfPackage p(Packages::OCT);
  int i[3]={1,2,3};
  int ibudfl = 0;
  int icbcfl = 0;
  int hdpr = 0;
  int ddpr = 0;
  int hdsv = 0;
  int ddsv = 0;
  p.SetField("SPID", &i[0]);
  p.SetField("TSNum", &i[1]);
  p.SetField("IHDDFL", &i[2]);
  p.SetField("IBUDFL", &ibudfl);
  p.SetField("ICBCFL", &icbcfl);
  p.SetField("Hdpr", &hdpr);
  p.SetField("Ddpr", &ddpr);
  p.SetField("Hdsv", &hdsv);

  CStr expected, output;

  {
    // don't export without all fields
    TxtExporter t(TESTBASE);

    expOCT(&p, &t);

    t.GetFileContents(Packages::OCT, output);
    TS_ASSERT(output.empty());

    p.SetField("Ddsv", &ddsv);
    expected =  "PERIOD 1 STEP 2\n";

    expOCT(&p, &t);

    t.GetFileContents(Packages::OC, output);
    TS_ASSERT_EQUALS2(expected, output);
  }
  {
    // when flags are non-zero include following lines
    ibudfl = 1;
    icbcfl = 1;
    hdpr = 1;
    ddpr = 1;
    hdsv = 1;
    ddsv = 1;
    expected += "   PRINT BUDGET\n";
    expected += "   SAVE BUDGET\n";
    expected += "   PRINT HEAD\n";
    expected += "   PRINT DRAWDOWN\n";
    expected += "   SAVE HEAD\n";
    expected += "   SAVE DRAWDOWN\n";
    
    TxtExporter t(TESTBASE);

    expOCT(&p, &t);

    t.GetFileContents(Packages::OC, output);
    TS_ASSERT_EQUALS2(expected, output);
  }
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
void ExpGmsH5T::testexpPVAL ()
{
  using namespace Packages;
  MfPackage p(Packages::PVAL);
  TxtExporter t(TESTBASE);
  int nplist=3;
  char *parnam = "RCH_ZONE_1RCH_ZONE_2RCH_ZONE_3";
  Real b[3]={7,8,9};

  p.SetField("PARNAM", parnam);
  p.SetField("B", b);

  expPVAL(&p, &t);

  CStr cStr;
  t.GetFileContents(Packages::PVAL, cStr);
  TS_ASSERT(cStr.empty());

  p.SetField("NPVAL", &nplist);

  expPVAL(&p, &t);

  t.GetFileContents(Packages::PVAL, cStr);
  CStr expected("3\n"
                "RCH_ZONE_1 7.0\n"
                "RCH_ZONE_2 8.0\n"
                "RCH_ZONE_3 9.0\n");
  TS_ASSERT_EQUALS2(expected, cStr);
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpPES ()
{
  using namespace MfData::Packages;
  MfData::MfPackage p(Packages::PES);
  int i[12]={1,2,3,4,5,6,7,8,9,0,1,2};
  Real r[8]={9,8,7,6,5,4,3,2};

  p.SetField("MAXITER", &i[0]);
  p.SetField("MAXCHANGE", &r[7]);
  p.SetField("TOL", &r[0]);
  p.SetField("SOSC", &r[1]);
  p.SetField("IBEFLG", &i[2]);
  p.SetField("IYCFLG", &i[3]);
  p.SetField("IOSTAR", &i[4]);
  p.SetField("NOPT", &i[5]);
  p.SetField("NFIT", &i[6]);
  p.SetField("SOSR", &r[2]);
  p.SetField("RMAR", &r[3]);
  p.SetField("RMARM", &r[4]);
  p.SetField("IAP", &i[7]);
  p.SetField("IPRCOV", &i[8]);
  p.SetField("IPRINT", &i[9]);
  p.SetField("LPRINT", &i[10]);
  p.SetField("CSA", &r[5]);
  p.SetField("FCONV", &r[6]);

  TxtExporter t(TESTBASE);
  CStr expected, output;

  // don't export without all fields
  expPES(&p, &t);
  
  t.GetFileContents(Packages::PES, output);
  TS_ASSERT(output.empty());

  p.SetField("LASTX", &i[11]);
  expPES(&p, &t);

  t.GetFileContents(Packages::PES, output);
  expected = "1 2.0 9.0 8.0 \n"
             "3 4 5 6 7 7.0 6.0 5.0 8 \n"
             "9 0 1 \n"
             "4.0 3.0 2 \n"
             "0 0 0 \n";
  TS_ASSERT_EQUALS2(expected, output);
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testGetMultiDimArrayIndex ()
{
  TxtExporter t(TESTBASE);

  CStr str(ARR_EVT_LAY);
  TS_ASSERT_EQUALS(0, GetMultiDimArrayIndex(str, &t));
  str = ARR_EVT_SURF;
  TS_ASSERT_EQUALS(0, GetMultiDimArrayIndex(str, &t));
  str = ARR_EVT_RATE;
  TS_ASSERT_EQUALS(1, GetMultiDimArrayIndex(str, &t));
  str = ARR_EVT_EXT;
  TS_ASSERT_EQUALS(2, GetMultiDimArrayIndex(str, &t));

  TS_ASSERT_EQUALS(0, GetMultiDimArrayIndex(ARR_UZF_RCH, &t));
  TS_ASSERT_EQUALS(1, GetMultiDimArrayIndex(ARR_UZF_ET, &t));
  TS_ASSERT_EQUALS(2, GetMultiDimArrayIndex(ARR_UZF_EXT, &t));
  TS_ASSERT_EQUALS(3, GetMultiDimArrayIndex(ARR_UZF_EXTWC, &t));

  str = ARR_RCH_RCH;
  TS_ASSERT_EQUALS(0, GetMultiDimArrayIndex(str, &t));
  str = ARR_RCH_LAY;
  TS_ASSERT_EQUALS(0, GetMultiDimArrayIndex(str, &t));

  str = ARR_ETS_PXDP;
  TS_ASSERT_EQUALS(0, GetMultiDimArrayIndex(str, &t, 0));
  TS_ASSERT_EQUALS(1, GetMultiDimArrayIndex(str, &t, 0));
  TS_ASSERT_EQUALS(0, GetMultiDimArrayIndex(str, &t, 1));
  TS_ASSERT_EQUALS(0, GetMultiDimArrayIndex(str, &t, 2));

  str = ARR_ETS_PETM;
  TS_ASSERT_EQUALS(0, GetMultiDimArrayIndex(str, &t, 0));
  TS_ASSERT_EQUALS(1, GetMultiDimArrayIndex(str, &t, 0));
  TS_ASSERT_EQUALS(0, GetMultiDimArrayIndex(str, &t, 1));
  TS_ASSERT_EQUALS(1, GetMultiDimArrayIndex(str, &t, 1));

  str = ARR_VDF_DENS;
  TS_ASSERT_EQUALS(0, GetMultiDimArrayIndex(str, &t, 0));
  TS_ASSERT_EQUALS(1, GetMultiDimArrayIndex(str, &t, 0));
  TS_ASSERT_EQUALS(0, GetMultiDimArrayIndex(str, &t, 1));
  TS_ASSERT_EQUALS(1, GetMultiDimArrayIndex(str, &t, 1));

  str = ARR_VDF_CONC;
  TS_ASSERT_EQUALS(0, GetMultiDimArrayIndex(str, &t, 0));
  TS_ASSERT_EQUALS(1, GetMultiDimArrayIndex(str, &t, 0));
  TS_ASSERT_EQUALS(0, GetMultiDimArrayIndex(str, &t, 1));
  TS_ASSERT_EQUALS(1, GetMultiDimArrayIndex(str, &t, 1));

  str = ARR_VSC_VSC;
  TS_ASSERT_EQUALS(0, GetMultiDimArrayIndex(str, &t, 0));
  TS_ASSERT_EQUALS(1, GetMultiDimArrayIndex(str, &t, 0));
  TS_ASSERT_EQUALS(0, GetMultiDimArrayIndex(str, &t, 1));
  TS_ASSERT_EQUALS(1, GetMultiDimArrayIndex(str, &t, 1));

  str = ARR_VSC_CONC;
  TS_ASSERT_EQUALS(0, GetMultiDimArrayIndex(str, &t, 0));
  TS_ASSERT_EQUALS(1, GetMultiDimArrayIndex(str, &t, 0));
  TS_ASSERT_EQUALS(0, GetMultiDimArrayIndex(str, &t, 1));
  TS_ASSERT_EQUALS(1, GetMultiDimArrayIndex(str, &t, 1));

  iArrayCountMap(&t).clear();
  TS_ASSERT_EQUALS(0, GetMultiDimArrayIndex(str, &t));
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpHFB ()
{
  using namespace MfData::Packages;
  using std::vector;
  using std::map;
  MfData::MfPackage p(Packages::HFB);
  int nhfbnp=2;
  Real hfb[14]={1,2,3,2,4,(Real)0.001,0,
                2,3,4,3,5,(Real)0.002,0};

  p.SetField("NHFBNP", &nhfbnp);

  TxtExporter t(TESTBASE);
  CStr expected, output;

  // don't export without all fields
  expHFB(&p, &t);
  
  p.SetField("HFB", hfb);

  // test without parameters
  expHFB(&p, &t);

  t.GetFileContents(Packages::HFB, output);
  expected = "0 0 2 \n"
             "1 2 3 2 4 0.001 \n"
             "2 3 4 3 5 0.002 \n";
  TS_ASSERT_EQUALS2(expected, output);

  // test with parameters
  ParamList *list(0);
  Parameters::GetParameterList(&list);
  list->Clear();

  Param param;
  param.m_name = "HFB_1";
  param.m_start = 1.0;
  param.m_value = 1.0;
  param.m_type = "HFB";
  param.m_key = -2;
  list->PushBack(&param);

  param.m_name = "HFB_2";
  param.m_start = 2.0;
  param.m_value = 2.0;
  param.m_type = "HFB";
  param.m_key = -3;
  list->PushBack(&param);

  map<CStr, vector<Real> >& usedParams = t.HfbParData();
  Real listValues[] = { 1, 2, 3, 4, 5, 6,
                        7, 8, 9, 0, 1, 2,
                        3, 4, 5, 6, 7, 8 };
  usedParams["HFB_1"] = vector<Real>(listValues, listValues + 6);
  usedParams["HFB_2"] = vector<Real>(listValues + 6, listValues + 18);
  expHFB(&p, &t);

  t.GetFileContents(Packages::HFB, output);
  expected += "2 3 2 \n"
              "HFB_1 HFB 1.0 1\n"
              "1 2 3 4 5 6.0 \n"
              "HFB_2 HFB 2.0 2\n"
              "7 8 9 0 1 2.0 \n"
              "3 4 5 6 7 8.0 \n"
              "1 2 3 2 4 0.001 \n"
              "2 3 4 3 5 0.002 \n"
              "2\n"
              "HFB_1\n"
              "HFB_2\n";
  TS_ASSERT_EQUALS2(expected, output);
  list->Clear();
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpRiv ()
{
  CStr f;
  util::GetTempDirectory(f);
  f += "\\array";
  CStr f2(f+".h5");

  int nRiv(5), nFields(9), nAux(3), itmp(5), np(0);
  Real d[45] = {1.0f,11.0f,17.0f,305.40f,8.42f,305.25f,6.0f,22.11f,1.0f,
                1.0f,10.0f,17.0f,305.38f,24.15f,305.23f,6.0f,63.39f,1.0f,
                1.0f,10.0f,16.0f,305.34f,61.11f,305.19f,6.0f,160.40f,1.0f,
                1.0f,9.0f,16.0f,305.25f,88.89f,305.10f,6.0f,233.33f,1.0f,
                1.0f,9.0f,15.0f,305.20f,2.60f,305.05f,6.0f,6.83f,1.0f};
  char c[80] = {73,70,65,67,69,32,32,32,32,32,32,32,32,32,32,32,67,79,78,68,
                70,65,67,84,32,32,32,32,32,32,32,32,67,69,76,76,71,82,80,32,
                32,32,32,32,32,32,32,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                0,0,0,0,0,0,0,0,0,0,0,0,0,0};

  MfData::MfPackage pack(MfData::Packages::RIV);
  pack.SetField(MfData::Packages::ListPack::ITMP, &itmp);
  pack.SetField(MfData::Packages::ListPack::MAXBC, &nRiv);
  pack.SetField(MfData::Packages::ListPack::NUMBC, &nRiv);
  pack.SetField(MfData::Packages::ListPack::NUMFIELDS, &nFields);
  pack.SetField(MfData::Packages::ListPack::DATA, &d[0]);
  pack.SetField(MfData::Packages::ListPack::NAUX, &nAux);
  pack.SetField(MfData::Packages::ListPack::NP, &np);
  //pack.SetField(MfData::Packages::ListPack::AUX, c); tested below

  CStr cStr, str, f1(f+".riv");
  CreateDefaultMfH5File(f);
  {
    TxtExporter t(f);
    expListPack(&pack, 1, 15, 20, &t);
    t.GetFileContents(Packages::RIV, cStr);
    TS_ASSERT(cStr.empty());
  }

  pack.SetField(MfData::Packages::ListPack::AUX, &c[0]);
  {
    TxtExporter t(f);
    expListPack(&pack, 1, 15, 20, &t);
    H5DataReader::CloseAllH5FilesOpenForWriting();
    t.GetFileContents(Packages::RIV, str);
    cStr = "#GMS_HDF5_01\n"
           "5 40 AUX IFACE AUX CONDFACT AUX CELLGRP\n"
           "         5         0         0\n"
           "GMS_HDF5_01 \"array.h5\" \"River\" 1\n";
    TS_ASSERT_EQUALS2(cStr, str);
  }
  TS_ASSERT(!remove(f2));
  TS_ASSERT(!remove(f1));

  CreateDefaultMfH5File(f);
  {
    TxtExporter t(f);
    expListPack(&pack, 1, 15, 20, &t);
    itmp = -1;
    expListPack(&pack, 2, 15, 20, &t);
    itmp = 5;
    expListPack(&pack, 3, 15, 20, &t);
    itmp = 0;
    nRiv = 0;
    expListPack(&pack, 4, 15, 20, &t);
    H5DataReader::CloseAllH5FilesOpenForWriting();
    t.GetFileContents(Packages::RIV, str);
    cStr = "#GMS_HDF5_01\n"
           "5 40 AUX IFACE AUX CONDFACT AUX CELLGRP\n"
           "         5         0         0\n"
           "GMS_HDF5_01 \"array.h5\" \"River\" 1\n"
           "        -1         0         0\n"
           "         5         0         0\n"
           "GMS_HDF5_01 \"array.h5\" \"River\" 3\n"
           "         0         0         0\n";
    TS_ASSERT_EQUALS2(cStr, str);
  }
  {
    std::vector<int> vI;
    H5DataSetReader numberBcsReader(f2, "River/00. Number of BCs");
    numberBcsReader.GetAllData(vI);
    TS_ASSERT(vI.size() == 1);
    TS_ASSERT_EQUALS(vI.at(0), 5);
    std::vector<double> vD;

    H5DataSetReader r1(f2, MFBC_VERSION);
    r1.GetAllData(vD);
    TS_ASSERT(vD.size() == 1);
    TS_ASSERT_EQUALS(vD.at(0), 3.0);

    H5DataSetReader useLastReader(f2, "River/01. Use Last");
    useLastReader.GetAllData(vI);
    TS_ASSERT(vI.size() == 4);
    TS_ASSERT_EQUALS(vI.at(0), 0);
    TS_ASSERT_EQUALS(vI.at(1), 1);
    TS_ASSERT_EQUALS(vI.at(2), 0);
    TS_ASSERT_EQUALS(vI.at(3), 0);

    H5DataSetReader propertyReader(f2, "River/07. Property");
    propertyReader.GetAllData(vD);
    Real expectedProperties[120] = {
      305.4f, 305.4f, 305.4f, 0.0f, // stage
      305.38f, 305.38f, 305.38f, 0.0f,
      305.34f, 305.34f, 305.34f, 0.0f,
      305.25f, 305.25f, 305.25f, 0.0f,
      305.2f, 305.2f, 305.2f, 0.0f,
      8.42f, 8.42f, 8.42f, 0.0f, // cond
      24.15f, 24.15f, 24.15f, 0.0f,
      61.11f, 61.11f, 61.11f, 0.0f,
      88.89f, 88.89f, 88.89f, 0.0f,
      2.6f, 2.6f, 2.6f, 0.0f,
      305.25f, 305.25f, 305.25f, 0.0f, //elev
      305.23f, 305.23f, 305.23f, 0.0f,
      305.19f, 305.19f, 305.19f, 0.0f,
      305.1f, 305.1f, 305.1f, 0.0f,
      305.05f, 305.05f, 305.05f, 0.0f,
      22.11000061035150f,22.11000061035150f,22.11000061035150f,1.0f, // condfact
      63.38999938964840f,63.38999938964840f,63.38999938964840f,1.0f,
      160.3999938964840f,160.3999938964840f,160.3999938964840f,1.0f,
      233.3300018310540f,233.3300018310540f,233.3300018310540f,1.0f,
      6.82999992370605f,6.82999992370605f,6.82999992370605f,1.0f,
      0,0,0,0, 0,0,0,0, 0,0, // this is SEAWAT data that is zero
      0,0,0,0, 0,0,0,0, 0,0,
      0,0,0,0, 0,0,0,0, 0,0,
      0,0,0,0, 0,0,0,0, 0,0
    };
    TS_ASSERT_H5_ARRAY(f2, "River/07. Property", expectedProperties, 120);

    H5Reader::CloseAllH5Files();
  }
  TS_ASSERT(!remove(f2));
  TS_ASSERT(!remove(f1));
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpDRT ()
{
  CStr f;
  util::GetTempDirectory(f);
  f += "\\array";
  CStr f2(f+".h5");

  int nDrt(5), nFields(9), nAux(0), itmp(5), np(0);
  Real d[45] = {1.0f,11.0f,17.0f,305.40f,8.42f,305.25f,6.0f,22.11f,1.0f,
                1.0f,10.0f,17.0f,305.38f,24.15f,305.23f,6.0f,63.39f,1.0f,
                1.0f,10.0f,16.0f,305.34f,61.11f,305.19f,6.0f,160.40f,1.0f,
                1.0f,9.0f,16.0f,305.25f,88.89f,305.10f,6.0f,233.33f,1.0f,
                1.0f,9.0f,15.0f,305.20f,2.60f,305.05f,6.0f,6.83f,1.0f};
  char c[80] = {73,70,65,67,69,32,32,32,32,32,32,32,32,32,32,32,67,79,78,68,
                70,65,67,84,32,32,32,32,32,32,32,32,67,69,76,76,71,82,80,32,
                32,32,32,32,32,32,32,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                0,0,0,0,0,0,0,0,0,0,0,0,0,0};

  MfData::MfPackage pack(MfData::Packages::DRT);
  pack.SetField(MfData::Packages::ListPack::ITMP, &itmp);
  pack.SetField(MfData::Packages::ListPack::MAXBC, &nDrt);
  pack.SetField(MfData::Packages::ListPack::NUMBC, &nDrt);
  pack.SetField(MfData::Packages::ListPack::NUMFIELDS, &nFields);
  pack.SetField(MfData::Packages::ListPack::DATA, &d[0]);
  pack.SetField(MfData::Packages::ListPack::NAUX, &nAux);
  pack.SetField(MfData::Packages::ListPack::NP, &np);
  //pack.SetField(MfData::Packages::ListPack::AUX, c); tested below

  CStr cStr, str, f1(f+".drt");
  CreateDefaultMfH5File(f);
  {
    TxtExporter t(f);
    expListPack(&pack, 1, 15, 20, &t);
    t.GetFileContents(Packages::DRT, cStr);
    TS_ASSERT(cStr.empty());
  }

  pack.SetField(MfData::Packages::ListPack::AUX, &c[0]);
  {
    TxtExporter t(f);
    expListPack(&pack, 1, 15, 20, &t);
    H5DataReader::CloseAllH5FilesOpenForWriting();
    t.GetFileContents(Packages::DRT, str);
    cStr = "#GMS_HDF5_01\n"
           "5 40 0 0 returnflow\n"
           "         5         0         0\n"
           "GMS_HDF5_01 \"array.h5\" \"Drain Return\" 1\n";
    TS_ASSERT_EQUALS2(cStr, str);
  }
  TS_ASSERT(!remove(f2));
  TS_ASSERT(!remove(f1));

  CreateDefaultMfH5File(f);
  {
    TxtExporter t(f);
    expListPack(&pack, 1, 15, 20, &t);
    itmp = -1;
    expListPack(&pack, 2, 15, 20, &t);
    itmp = 5;
    expListPack(&pack, 3, 15, 20, &t);
    H5DataReader::CloseAllH5FilesOpenForWriting();
    t.GetFileContents(Packages::DRT, str);
    cStr = "#GMS_HDF5_01\n"
           "5 40 0 0 returnflow\n"
           "         5         0         0\n"
           "GMS_HDF5_01 \"array.h5\" \"Drain Return\" 1\n"
           "        -1         0         0\n"
           "         5         0         0\n"
           "GMS_HDF5_01 \"array.h5\" \"Drain Return\" 3\n";
    TS_ASSERT_EQUALS2(cStr, str);
  }
  {
    std::vector<int> vI;
    H5DataSetReader r(f2, "Drain Return/00. Number of BCs");
    r.GetAllData(vI);
    TS_ASSERT(vI.size() == 1);
    TS_ASSERT_EQUALS(vI.at(0), 5);
    std::vector<double> vD;
    H5DataSetReader r1(f2, MFBC_VERSION);
    r1.GetAllData(vD);
    TS_ASSERT(vD.size() == 1);
    TS_ASSERT_EQUALS(vD.at(0), 3.0);

    H5DataSetReader r2(f2, "Drain Return/01. Use Last");
    r2.GetAllData(vI);
    TS_ASSERT(vI.size() == 3);
    TS_ASSERT_EQUALS(vI.at(0), 0);
    TS_ASSERT_EQUALS(vI.at(1), 1);
    TS_ASSERT_EQUALS(vI.at(2), 0);

    H5Reader::CloseAllH5Files();
  }
  TS_ASSERT(!remove(f2));
  TS_ASSERT(!remove(f1));
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testiGetBcIndex ()
{
  TxtExporter t(TESTBASE);
  {
    const char *type="River";
    int idx;
    std::vector<int> cellIds, iface, cellgrp;

    idx = iGetBcIndex(type, 5, 1, cellIds, iface, cellgrp, &t);
    TS_ASSERT_EQUALS(idx, 0);
    TS_ASSERT(cellIds.size() == 1);
    TS_ASSERT_EQUALS(cellIds.at(0), 5);
    TS_ASSERT(iface.size() == 1);
    TS_ASSERT_EQUALS(iface.at(0), 6);
    idx = iGetBcIndex(type, 5, 2, cellIds, iface, cellgrp, &t);
    TS_ASSERT_EQUALS(idx, 0);
    TS_ASSERT(cellIds.size() == 1);
    TS_ASSERT_EQUALS(cellIds.at(0), 5);
    idx = iGetBcIndex(type, 5, 2, cellIds, iface, cellgrp, &t);
    TS_ASSERT_EQUALS(idx, 1);
    TS_ASSERT(cellIds.size() == 2);
    TS_ASSERT_EQUALS(cellIds.at(0), 5);
    TS_ASSERT_EQUALS(cellIds.at(1), 5);
    idx = iGetBcIndex(type, 5, 3, cellIds, iface, cellgrp, &t);
    TS_ASSERT_EQUALS(idx, 0);
    TS_ASSERT(cellIds.size() == 2);
    idx = iGetBcIndex(type, 5, 3, cellIds, iface, cellgrp, &t);
    TS_ASSERT_EQUALS(idx, 1);
    TS_ASSERT(cellIds.size() == 2);
    idx = iGetBcIndex(type, 2, 3, cellIds, iface, cellgrp, &t);
    TS_ASSERT_EQUALS(idx, 2);
    TS_ASSERT(cellIds.size() == 3);
    TS_ASSERT_EQUALS(cellIds.at(2), 2);
    idx = iGetBcIndex(type, 2, 4, cellIds, iface, cellgrp, &t);
    TS_ASSERT_EQUALS(idx, 2);
    TS_ASSERT(cellIds.size() == 3);
    idx = iGetBcIndex(type, 2, 4, cellIds, iface, cellgrp, &t);
    TS_ASSERT_EQUALS(idx, 3);
    TS_ASSERT(cellIds.size() == 4);
    TS_ASSERT(iface.size() == 4);
    TS_ASSERT_EQUALS(cellIds.at(3), 2);
  }
  {
    const char *type="Well";
    int idx;
    std::vector<int> cellIds, iface, cellgrp;

    idx = iGetBcIndex(type, 5, 1, cellIds, iface, cellgrp, &t);
    TS_ASSERT_EQUALS(idx, 0);
    TS_ASSERT(cellIds.size() == 1);
    TS_ASSERT_EQUALS(cellIds.at(0), 5);
    TS_ASSERT(iface.size() == 1);
    TS_ASSERT_EQUALS(iface.at(0), 0);
    idx = iGetBcIndex(type, 5, 2, cellIds, iface, cellgrp, &t);
    TS_ASSERT_EQUALS(idx, 0);
    TS_ASSERT(cellIds.size() == 1);
    idx = iGetBcIndex(type, 5, 2, cellIds, iface, cellgrp, &t);
    TS_ASSERT_EQUALS(idx, 1);
    TS_ASSERT(cellIds.size() == 2);
    TS_ASSERT(iface.size() == 2);
  }
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testiSizeBcDataArray ()
{
  CAR_DBL2D data;
  iSizeBcDataArray("River", 5, data);
  TS_ASSERT_EQUALS(data.GetSize1(), 6);
  TS_ASSERT_EQUALS(data.GetSize2(), 6);
  TS_ASSERT_EQUALS(data.at(3,0), 1.0);
  TS_ASSERT_EQUALS(data.at(3,5), 1.0);
  iSizeBcDataArray("Specified Head", 3, data);
  TS_ASSERT_EQUALS(data.GetSize1(), 6);
  TS_ASSERT_EQUALS(data.GetSize2(), 4);
  TS_ASSERT_EQUALS(data.at(3,0), 1.0);
  TS_ASSERT_EQUALS(data.at(3,3), 1.0);
  iSizeBcDataArray("Drain", 6, data);
  TS_ASSERT_EQUALS(data.GetSize1(), 4);
  TS_ASSERT_EQUALS(data.GetSize2(), 7);
  TS_ASSERT_EQUALS(data.at(2,0), 1.0);
  TS_ASSERT_EQUALS(data.at(2,6), 1.0);
  iSizeBcDataArray("General Head", 7, data);
  TS_ASSERT_EQUALS(data.GetSize1(), 5);
  TS_ASSERT_EQUALS(data.GetSize2(), 8);
  TS_ASSERT_EQUALS(data.at(2,0), 1.0);
  TS_ASSERT_EQUALS(data.at(2,6), 1.0);
  iSizeBcDataArray("Well", 2, data);
  TS_ASSERT_EQUALS(data.GetSize1(), 3);
  TS_ASSERT_EQUALS(data.GetSize2(), 3);
  TS_ASSERT_EQUALS(data.at(1,0), 1.0);
  TS_ASSERT_EQUALS(data.at(1,2), 1.0);
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpLAK ()
{
  using namespace MfData::Packages;
  int nlakes = 3;
  int itrss = 1;
  int ilkcb = 1;
  int nssitr = 20;
  Real theta = (Real)0.1;
  Real sscncr = (Real)0.008;

  MfData::MfPackage p(Packages::LAK);
  p.SetField("NLAKES", &nlakes);
  p.SetField("ILKCB", &ilkcb);
  p.SetField("THETA", &theta);
  p.SetField("NSSITR", &nssitr);
  p.SetField("SSCNCR", &sscncr);

  CStr expected, output;

  {
    TxtExporter t(TESTBASE);

    // don't export without all fields
    expLAK(&p, &t);

    t.GetFileContents(Packages::LAK, output);
    TS_ASSERT(output.empty());

    // check export for steady state
    p.SetField("ITRSS", &itrss);
    expLAK(&p, &t);

    t.GetFileContents(Packages::LAK, output);
    expected = "3 1\n"
               "0.1\n";

    TS_ASSERT_EQUALS2(expected, output);
  }

  // check export for steady state
  {
    TxtExporter t(TESTBASE);

    itrss = 0;
    expLAK(&p, &t);

    t.GetFileContents(Packages::LAK, output);
    expected = "3 1\n"
               "0.1 20 0.008\n";
    TS_ASSERT_EQUALS2(expected, output);
  }

  // check export for negative theta
  {
    TxtExporter t(TESTBASE);

    itrss = 1;
    theta = (Real)-0.1;
    expLAK(&p, &t);

    t.GetFileContents(Packages::LAK, output);
    expected = "3 1\n"
               "-0.1 20 0.008\n";
    TS_ASSERT_EQUALS2(expected, output);
  }
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpLAKSP ()
{
  int nlakes(3), nsol(0), itmp(0), itmp1(-1), lwrt(1), nslms(0);
  Real stages[3] = {9, 8, 7};
  Real ssmn[3]   = {6, 5, 4};
  Real ssmx[3]   = {3, 2, 1};
  Real clake[6]  = {9, 7, 5,
                    8, 6, 4};
  int lkarr[24] = {1, 2, 3, 4,
                   5, 6, 7, 8,
                   9, 1, 2, 3,

                   4, 5, 6, 7,
                   8, 9, 1, 2,
                   3, 4, 5, 6};

  Real bdlknc[24] = {9, 8, 7, 6,
                     5, 4, 3, 2,
                     1, 9, 8, 7,

                     6, 5, 4, 3,
                     2, 1, 9, 8,
                     7, 6, 5, 4};

  int ic[3] = {3, 2, 0};

  int isub[9] = {1, 2, 3,
                 4, 5, 6,
                 7, 8, 9};

  Real sillvt[9] = {9, 8, 7,
                    6, 5, 4,
                    3, 2, 1};

  double prcplk[3] = {9, 8, 7};
  double evaplk[3] = {6, 5, 4};
  Real rnf[3]    = {3, 2, 1};
  Real wthdrw[3] = {0, -9, 8};

  Real cppt[6] = {9, 8, 7,
                  6, 5, 4};
  Real crnf[6] = {3, 2, 1,
                  0, 9, 8};
  Real caug[6] = {7, 6, 5,
                  4, 3, 2};

  using namespace MfData::Packages;
  MfData::MfPackage pLAK(Packages::LAK);
  pLAK.SetField("NLAKES", &nlakes);

  MfData::MfPackage p(Packages::LAKSP);
  p.SetField("NSOL", &nsol);
  p.SetField("STAGES", stages);
  p.SetField("SSMN", ssmn);
  p.SetField("SSMX", ssmx);
  p.SetField("CLAKE", clake);
  p.SetField("ITMP", &itmp);
  p.SetField("ITMP1", &itmp1);
  p.SetField("LWRT", &lwrt);
  p.SetField("LKARR", lkarr);
  p.SetField("BDLKNC", bdlknc);
  p.SetField("NSLMS", &nslms);
  p.SetField("IC", ic);
  p.SetField("ISUB", isub);
  p.SetField("SILLVT", sillvt);
  p.SetField("PRCPLK", prcplk);
  p.SetField("EVAPLK", evaplk);
  p.SetField("RNF", rnf);
  p.SetField("WTHDRW", wthdrw);
  p.SetField("CPPT", cppt);
  p.SetField("CRNF", crnf);

  CStr expected, output;
  MfGlobal g(3, 4, 2, 2, 3, 2, 0);
  
  CStr f;
  util::GetTempDirectory(f);
  f += "\\array";

  CStr f1(f+".lak"), f2(f+".h5");
  CreateDefaultMfH5File(f);
  {
    TxtExporter t(f);

    // don't export without all fields
    expLAKSP(&g, &p, &pLAK, &t);

    t.GetFileContents(Packages::LAK, output);
    TS_ASSERT(output.empty());
  }

  p.SetField("CAUG", caug);

  // check export with number of solutes zero
  // itmp zero
  {
    TxtExporter t(f);

    g.PutCurrentPeriod(1);
    expLAKSP(&g, &p, &pLAK, &t);

    H5DataReader::CloseAllH5FilesOpenForWriting();
    t.GetFileContents(Packages::LAK, output);
    expected = "9.0 6.0 3.0\n"
               "8.0 5.0 2.0\n"
               "7.0 4.0 1.0\n"
               "0 -1 1\n";

    TS_ASSERT_EQUALS2(expected, output);
  }
  H5Reader::CloseAllH5Files();
  TS_ASSERT(!remove(f1));
  TS_ASSERT(!remove(f2));

  CreateDefaultMfH5File(f);
  {
    TxtExporter t(f);

    // STAGES line should only show for first stress period
    g.PutCurrentPeriod(2);

    expLAKSP(&g, &p, &pLAK, &t);

    H5DataReader::CloseAllH5FilesOpenForWriting();
    t.GetFileContents(Packages::LAK, output);
    expected = "0 -1 1\n";
    TS_ASSERT_EQUALS2(expected, output);
    g.PutCurrentPeriod(1);
  }
  H5Reader::CloseAllH5Files();
  TS_ASSERT(!remove(f1));
  TS_ASSERT(!remove(f2));

  // check export for 
  //   nsol 3
  //   itmp 1
  //   nslms 0
  CreateDefaultMfH5File(f);
  {
    TxtExporter t(f);

    //nsol = 2;  removed NSOL see expLAKSP for details
    itmp = 1;
    expLAKSP(&g, &p, &pLAK, &t);

    H5DataReader::CloseAllH5FilesOpenForWriting();
    t.GetFileContents(Packages::LAK, output);
    expected = "9.0 6.0 3.0\n"  // 9.0 8.0\n" // NSOL removed for now
               "8.0 5.0 2.0\n"  // 7.0 6.0\n"
               "7.0 4.0 1.0\n"  // 5.0 4.0\n"
               "1 -1 1\n"
               "HDF5 1.0 -1 \"array.h5\" \"Arrays/Lak_1\" 1 0 12\n"
               "HDF5 1.0 -1 \"array.h5\" \"Arrays/Lak_2\" 1 0 12\n"
               "HDF5 1.0 -1 \"array.h5\" \"Arrays/LakLeak_1\" 1 0 12\n"
               "HDF5 1.0 -1 \"array.h5\" \"Arrays/LakLeak_2\" 1 0 12\n"
               "0\n";
    TS_ASSERT_EQUALS2(expected, output);
    {
      std::vector<int>vI;
      H5DataSetReader lak1(f2, "Arrays/Lak_1");
      lak1.GetAllData(vI);
      TS_ASSERT(vI.size() == 12);
      TS_ASSERT_EQUALS(vI.at(0), 1);
      TS_ASSERT_EQUALS(vI.at(11), 3);
      vI.clear();

      H5DataSetReader lak2(f2, "Arrays/Lak_2");
      lak2.GetAllData(vI);
      TS_ASSERT(vI.size() == 12);
      TS_ASSERT_EQUALS(vI.at(0), 4);
      TS_ASSERT_EQUALS(vI.at(11), 6);

      std::vector<double>v;
      H5DataSetReader leak1(f2, "Arrays/LakLeak_1");
      leak1.GetAllData(v);
      TS_ASSERT(v.size() == 12);
      TS_ASSERT_EQUALS(v.at(0), 9.0);
      TS_ASSERT_EQUALS(v.at(11), 7.0);

      H5DataSetReader leak2(f2, "Arrays/LakLeak_2");
      leak2.GetAllData(v);
      TS_ASSERT(v.size() == 12);
      TS_ASSERT_EQUALS(v.at(0), 6.0);
      TS_ASSERT_EQUALS(v.at(11), 4.0);
    }
    // nsol = 0; removed NSOL
  }
  H5Reader::CloseAllH5Files();
  TS_ASSERT(!remove(f1));
  TS_ASSERT(!remove(f2));

  // check export for 
  //   number of solutes 3
  //   itmp 1
  //   itmp1 >= 0
  //   nslms 2
  CreateDefaultMfH5File(f);
  {
    TxtExporter t(f);

    // nsol = 2;  removed NSOL
    itmp = 1;
    itmp1 = 0;
    nslms = 2;
    g.PutCurrentPeriod(2);
    expLAKSP(&g, &p, &pLAK, &t);

    H5DataReader::CloseAllH5FilesOpenForWriting();
    t.GetFileContents(Packages::LAK, output);
    expected = "1 0 1\n"                   // ITMP ITMP1 LWRT
               // LKARR(NCOL,NROW,NLAY)
               "HDF5 1.0 -1 \"array.h5\" \"Arrays/Lak_1\" 1 0 12\n"
               "HDF5 1.0 -1 \"array.h5\" \"Arrays/Lak_2\" 1 0 12\n"
               // BDLKNC(NCOL,NROW,NLAY)
               "HDF5 1.0 -1 \"array.h5\" \"Arrays/LakLeak_1\" 1 0 12\n"
               "HDF5 1.0 -1 \"array.h5\" \"Arrays/LakLeak_2\" 1 0 12\n"
               "2\n"                       // NSLMS
               "3 1 4 7 \n"                // IC(NSLMS) ISUB(NSLMS,IC)
               "9.0 6.0 \n"                // SILLVT(NSLMS,IC-1)
               "2 2 5 \n"                  //   above 2 NSLMS times
               "8.0 \n"

               "9.0 6.0 3.0 0.0\n"         // PRCPLK EVAPLK RNF WTHDRW
                                           //   above dim (NLAKES)

               // removed NSOL support
               //"9.0 3.0\n"                 // CPPT, CRNF, CAUG
               //"6.0 0.0\n"                 //   above dim (NLAKES,NSOL)

               "8.0 5.0 2.0 -9.0\n"        //   above NLAKES times
               //"8.0 2.0 6.0\n" // removed NSOL
               //"5.0 9.0 3.0\n" // removed NSOL

               "7.0 4.0 1.0 8.0\n";         //   ...
               //"7.0 1.0\n"   // removed NSOL
               //"4.0 8.0\n";  // removed NSOL
    TS_ASSERT_EQUALS2(expected, output);
    {
      std::vector<int>vI;
      H5DataSetReader lak1(f2, "Arrays/Lak_1");
      lak1.GetAllData(vI);
      TS_ASSERT(vI.size() == 24);
      TS_ASSERT_EQUALS(vI.at(12), 1);
      TS_ASSERT_EQUALS(vI.at(23), 3);
      vI.clear();

      H5DataSetReader lak2(f2, "Arrays/Lak_2");
      lak2.GetAllData(vI);
      TS_ASSERT(vI.size() == 24);
      TS_ASSERT_EQUALS(vI.at(12), 4);
      TS_ASSERT_EQUALS(vI.at(23), 6);

      std::vector<double>v;
      H5DataSetReader leak1(f2, "Arrays/LakLeak_1");
      leak1.GetAllData(v);
      TS_ASSERT(v.size() == 24);
      TS_ASSERT_EQUALS(v.at(12), 9.0);
      TS_ASSERT_EQUALS(v.at(23), 7.0);

      H5DataSetReader leak2(f2, "Arrays/LakLeak_2");
      leak2.GetAllData(v);
      TS_ASSERT(v.size() == 24);
      TS_ASSERT_EQUALS(v.at(12), 6.0);
      TS_ASSERT_EQUALS(v.at(23), 4.0);
    }
    nsol = 0;
  }
  H5Reader::CloseAllH5Files();
  TS_ASSERT(!remove(f1));
  TS_ASSERT(!remove(f2));
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpSTR ()
{
  int mxacts(31), nss(7), ntrib(3), ndiv(1), icalc(1), istcb1(40), istcb2(0);
  Real constv(1.0f);
  int stressPeriod(1), nRow(6), nCol(6), itmp(23), irdflg(0), iptflg(0), 
      nstrem(23), mxstrm(23);
  int istrm[115] = {1, 1, 3, 1, 1,
                    1, 2, 3, 1, 2,
                    1, 2, 3, 2, 1,
                    1, 3, 3, 2, 2,
                    1, 4, 3, 2, 3,
                    1, 5, 3, 2, 4,
                    1, 2, 3, 3, 1,
                    1, 3, 4, 3, 2,
                    1, 4, 4, 3, 3,
                    1, 5, 4, 3, 4,
                    1, 4, 1, 4, 1,
                    1, 4, 2, 4, 2,
                    1, 5, 2, 4, 3,
                    1, 5, 3, 4, 4,
                    1, 5, 3, 5, 1,
                    1, 5, 4, 5, 2,
                    1, 2, 6, 6, 1,
                    1, 3, 6, 6, 2,
                    1, 4, 5, 6, 3,
                    1, 5, 5, 6, 4,
                    1, 5, 4, 6, 5,
                    1, 5, 4, 7, 1,
                    1, 6, 4, 7, 2};
  Real strm[253] = {4.5, 495,  1.2f, 490, 492, 10, 0.007f, 0.03f,  0, 0, 0,
                      0, 490,  0.6f, 485, 487, 10, 0.007f, 0.03f,  0, 0, 0,
                   1.5f, 487,  0.2f, 483, 485,  5, 0.002f, 0.022f, 0, 0, 0,
                      0, 486,  0.4f, 482, 484,  5, 0.002f, 0.022f, 0, 0, 0,
                      0, 484,  0.4f, 480, 482,  5, 0.002f, 0.022f, 0, 0, 0,
                      0, 480,  0.2f, 476, 478,  5, 0.004f, 0.022f, 0, 0, 0,
                     -1, 486,  0.4f, 481, 483, 10, 0.005f, 0.03f,  0, 0, 0,
                      0, 482,  1.2f, 477, 479, 10, 0.005f, 0.03f,  0, 0, 0,
                      0, 478,  1.2f, 473, 475, 10, 0.005f, 0.03f,  0, 0, 0,
                      0, 475,  0.6f, 470, 472, 10, 0.005f, 0.03f,  0, 0, 0,
                   0.8f, 492,  0.4f, 489, 490,  5, 0.004f, 0.022f, 0, 0, 0,
                      0, 488, 0.32f, 485, 486,  5, 0.004f, 0.022f, 0, 0, 0,
                      0, 483, 0.32f, 480, 481,  5, 0.004f, 0.022f, 0, 0, 0,
                      0, 480,  0.2f, 477, 478,  5, 0.004f, 0.022f, 0, 0, 0,
                     -1, 478,  0.2f, 475, 476,  5, 0.005f, 0.022f, 0, 0, 0,
                      0, 474,  0.2f, 471, 472,  5, 0.005f, 0.022f, 0, 0, 0,
                   1.2f, 495,  0.8f, 491, 493,  5, 0.005f, 0.022f, 0, 0, 0,
                      0, 490,  0.8f, 486, 488,  5, 0.008f, 0.022f, 0, 0, 0,
                      0, 480,  0.8f, 476, 478,  5, 0.007f, 0.022f, 0, 0, 0,
                      0, 477,  0.6f, 473, 475,  5, 0.004f, 0.022f, 0, 0, 0,
                      0, 474,  0.2f, 470, 472,  5, 0.003f, 0.022f, 0, 0, 0,
                     -1, 472,  0.6f, 467, 469, 10, 0.004f, 0.03f,  0, 0, 0,
                      0, 469,  1.2f, 464, 466, 10, 0.004f, 0.03f,  0, 0, 0};
  int itrbar[3*7] = {0, 0, 1, 0, 2, 0, 3, 
                     0, 0, 0, 0, 4, 0, 5,
                     0, 0, 0, 0, 0, 0, 6};
  int idivar[7] = {0, 1, 0, 0, 0, 0, 0};

  using namespace MfData::Packages;
  MfData::MfPackage p(Packages::STRSP);

  p.SetField("MXACTS", &mxacts);
  p.SetField("NSS", &nss);
  p.SetField("NTRIB", &ntrib);
  p.SetField("NDIV", &ndiv);
  p.SetField("ICALC", &icalc);
  p.SetField("CONST", &constv);
  p.SetField("ISTCB1", &istcb1);
  p.SetField("ISTCB2", &istcb2);

  p.SetField("ITMP", &itmp);
  p.SetField("IRDFLG", &irdflg);
  p.SetField("IPTFLG", &iptflg);
  p.SetField("STRM", strm);
  p.SetField("ISTRM", istrm);
  p.SetField("NSTREM", &nstrem);
  p.SetField("MXSTRM", &mxstrm);
  p.SetField("ITRBAR", itrbar);

  CStr expected, output;
  
  CStr f;
  util::GetTempDirectory(f);
  f += "\\stream";

  CStr f1(f+".str"), f2(f+".h5");
  CreateDefaultMfH5File(f);
  {
    TxtExporter t(f);

    // don't export without all fields
    expSTR(&p, stressPeriod, nRow, nCol, &t);

    t.GetFileContents(Packages::STRSP, output);
    TS_ASSERT(output.empty());
  }

  p.SetField("IDIVAR", idivar);

  // check for stress period 1
  {
    TxtExporter t(f);

    expSTR(&p, stressPeriod, nRow, nCol, &t);

    H5DataReader::CloseAllH5FilesOpenForWriting();
    t.GetFileContents(Packages::STRSP, output);
    expected = "#GMS_HDF5_01\n"
               "        30         7         3         1         1"
                                   "       1.0        40         0\n"
               "        23         0         0\n"
               "GMS_HDF5_01 \"stream.h5\" \"Stream\" 1\n";

    TS_ASSERT_EQUALS2(expected, output);
    {
      std::vector<int>vI;
      H5DataSetReader bcs(f2, "Stream/00. Number of BCs");
      bcs.GetAllData(vI);
      TS_ASSERT(vI.size() == 1);
      TS_ASSERT_EQUALS(vI.at(0), 23);
      vI.clear();

      H5DataSetReader useLast(f2, "Stream/01. Use Last");
      useLast.GetAllData(vI);
      TS_ASSERT(vI.size() == 1);
      TS_ASSERT_EQUALS(vI.at(0), 0);
      vI.clear();

      H5DataSetReader cellids(f2, "Stream/02. Cell IDs");
      cellids.GetAllData(vI);
      TS_ASSERT(vI.size() == 23);
      TS_ASSERT_EQUALS(vI.at(0), 3);
      TS_ASSERT_EQUALS(vI.at(11), 20);
      TS_ASSERT_EQUALS(vI.at(22), 34);

      std::vector<double>v;
      H5DataSetReader properties(f2, "Stream/07. Property");
      properties.GetAllData(v);
      TS_ASSERT(v.size() == 23*8);
      TS_ASSERT_EQUALS(v.at(0), 495.0);
      TS_ASSERT_EQUALS(v.at(23*7-1), 0.03f);

      H5DataSetReader reachSegs(f2, "Stream/08. Str reach segment ID");
      reachSegs.GetAllData(vI);
      TS_ASSERT(vI.size() == 23);
      TS_ASSERT_EQUALS(vI.at(0), 1);
      TS_ASSERT_EQUALS(vI.at(9), 3);
      TS_ASSERT_EQUALS(vI.at(22), 7);

      H5DataSetReader segId(f2, "Stream/09. Segment ID");
      segId.GetAllData(vI);
      TS_ASSERT(vI.size() == 7);
      TS_ASSERT_EQUALS(vI.at(0), 1);
      TS_ASSERT_EQUALS(vI.at(3), 4);
      TS_ASSERT_EQUALS(vI.at(6), 7);

      H5DataSetReader flow(f2, "Stream/10. Segment Flow");
      flow.GetAllData(v);
      TS_ASSERT(v.size() == 7);
      TS_ASSERT_EQUALS(v.at(0), 4.5f);
      TS_ASSERT_EQUALS(v.at(3), 0.8f);
      TS_ASSERT_EQUALS(v.at(6), -1.0f);

      H5DataSetReader trib(f2, "Stream/11. ITRIB");
      trib.GetAllData(vI);
      TS_ASSERT(vI.size() == 70);
      TS_ASSERT_EQUALS(vI.at(0), 0);
      TS_ASSERT_EQUALS(vI.at(20), 1);
      TS_ASSERT_EQUALS(vI.at(41), 4);
      TS_ASSERT_EQUALS(vI.at(62), 6);

      H5DataSetReader upIds(f2, "Stream/12. Upstream ID");
      upIds.GetAllData(vI);
      TS_ASSERT(vI.size() == 7);
      TS_ASSERT_EQUALS(vI.at(0), 0);
      TS_ASSERT_EQUALS(vI.at(1), 1);
      TS_ASSERT_EQUALS(vI.at(6), 0);

      H5DataSetReader numSegs(f2, "Stream/13. Number of Segments");
      numSegs.GetAllData(vI);
      TS_ASSERT(vI.size() == 1);
      TS_ASSERT_EQUALS(vI.at(0), 7);
    }
  //}
    H5Reader::CloseAllH5Files();
    //TS_ASSERT(!remove(f1));
    TS_ASSERT(!remove(f2));

    // check for stress period 2
    CreateDefaultMfH5File(f);
  //{
  //  TxtExporter t(f);

    stressPeriod = 2;
    expSTR(&p, stressPeriod, nRow, nCol, &t);

    H5DataReader::CloseAllH5FilesOpenForWriting();
    t.GetFileContents(Packages::STRSP, output);
    expected = "#GMS_HDF5_01\n"
               "        30         7         3         1         1"
                                   "       1.0        40         0\n"
               "        23         0         0\n"
               "GMS_HDF5_01 \"stream.h5\" \"Stream\" 1\n"
               "        23         0         0\n"
               "GMS_HDF5_01 \"stream.h5\" \"Stream\" 2\n";

    TS_ASSERT_EQUALS2(expected, output);
    {
      std::vector<int>vI;
      H5DataSetReader useLast(f2, "Stream/01. Use Last");
      useLast.GetAllData(vI);
      TS_ASSERT(vI.size() == 2);
      TS_ASSERT_EQUALS(vI.at(1), 0);

      std::vector<double>v;
      H5DataSetReader properties(f2, "Stream/07. Property");
      properties.GetAllData(v);
      unsigned int inPeriod = 23*8;
      TS_ASSERT(v.size() == 2*inPeriod);
      TS_ASSERT_EQUALS(v.at(1), 495.0);
      TS_ASSERT_EQUALS(v.at(2*inPeriod-23*2-1), 0.03f);

      H5DataSetReader reachSegs(f2, "Stream/08. Str reach segment ID");
      reachSegs.GetAllData(vI);
      TS_ASSERT(vI.size() == 1);

      H5DataSetReader segId(f2, "Stream/09. Segment ID");
      segId.GetAllData(vI);
      TS_ASSERT(vI.size() == 1);

      H5DataSetReader flow(f2, "Stream/10. Segment Flow");
      flow.GetAllData(v);
      TS_ASSERT(v.size() == 14);
      TS_ASSERT_EQUALS(v.at(1), 4.5f);
      TS_ASSERT_EQUALS(v.at(7), 0.8f);
      TS_ASSERT_EQUALS(v.at(13), -1.0f);

      H5DataSetReader trib(f2, "Stream/11. ITRIB");
      trib.GetAllData(vI);
      TS_ASSERT(vI.size() == 1);

      H5DataSetReader upIds(f2, "Stream/12. Upstream ID");
      upIds.GetAllData(vI);
      TS_ASSERT(vI.size() == 1);
    }
  }
  H5Reader::CloseAllH5Files();
  TS_ASSERT(!remove(f1));
  TS_ASSERT(!remove(f2));
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testiGetListPackTypeFromPtr ()
{
  CStr type;

  iGetListPackTypeFromPtr(Packages::DRN, type);
  TS_ASSERT(type == "Drain");

  iGetListPackTypeFromPtr(Packages::DRT, type);
  TS_ASSERT(type == "Drain Return");

  iGetListPackTypeFromPtr(Packages::RIV, type);
  TS_ASSERT(type == "River");

  iGetListPackTypeFromPtr(Packages::WEL, type);
  TS_ASSERT(type == "Well");

  iGetListPackTypeFromPtr(Packages::GHB, type);
  TS_ASSERT(type == "General Head");

  iGetListPackTypeFromPtr(Packages::CHD, type);
  TS_ASSERT(type == "Specified Head");

  iGetListPackTypeFromPtr("stuff", type);
  TS_ASSERT(type == "");
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpSFRLine1 ()
{
  using namespace MfData::Packages;
  MfData::MfPackage pSFRLine1(Packages::SFRLine1);
  int nstrm(10), nss(3), istcb1(4), istcb2(5), isfropt(1), nstrail(7), isuzn(8),
      nsfrsets(9);
  Real constv(1.0), dleak(2.0);

  pSFRLine1.SetField("NSTRM", &nstrm);
  pSFRLine1.SetField("NSS", &nss);
  pSFRLine1.SetField("CONST", &constv);
  pSFRLine1.SetField("DLEAK", &dleak);
  pSFRLine1.SetField("ISTCB1", &istcb1);
  pSFRLine1.SetField("ISTCB2", &istcb2);
  pSFRLine1.SetField("ISFROPT", &isfropt);
  pSFRLine1.SetField("NSTRAIL", &nstrail);
  pSFRLine1.SetField("ISUZN", &isuzn);

  CStr expected, output;
  TxtExporter t(TESTBASE);

  // all fields need to be set
  expSFRLine1(&pSFRLine1, &t);
  t.GetFileContents(Packages::SFR, output);
  TS_ASSERT(output.empty());

  // don't write ISFROPT and after when NSTRM positive
  pSFRLine1.SetField("NSFRSETS", &nsfrsets);
  expSFRLine1(&pSFRLine1, &t);
  expected += "#GMS_HDF5_01\n"
              "10 3 0 0 1.0 2.0 4 5\n";
  t.GetFileContents(Packages::SFR, output);
  TS_ASSERT_EQUALS2(expected, output);

  // don't write NSTRAIL and after when ISFROPT <= 1
  nstrm = -10;
  expSFRLine1(&pSFRLine1, &t);
  expected += "#GMS_HDF5_01\n"
              "-10 3 0 0 1.0 2.0 4 5 1\n";
  t.GetFileContents(Packages::SFR, output);
  TS_ASSERT_EQUALS2(expected, output);

  // include everything when ISFROPT > 1
  isfropt = 2;
  expSFRLine1(&pSFRLine1, &t);
  t.GetFileContents(Packages::SFR, output);
  expected += "#GMS_HDF5_01\n"
              "-10 3 0 0 1.0 2.0 4 5 2 7 8 9\n";
  TS_ASSERT_EQUALS2(expected, output);
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpSFRLine2 ()
{
  using namespace MfData::Packages;
  MfData::MfPackage pSFRLine1(Packages::SFRLine1);
  MfData::MfPackage pSFRLine2(Packages::SFRLine2);
  int nstrm(-23), nss(7), isfropt(0), nrow(6), ncol(6), nSp(3), nstrmd(24);
  int nistrmd(5);

  int istrm[115] = {1, 1, 3, 1, 1,
                    1, 2, 3, 1, 2,
                    1, 2, 3, 2, 1,
                    1, 3, 3, 2, 2,
                    1, 4, 3, 2, 3,
                    1, 5, 3, 2, 4,
                    1, 2, 3, 3, 1,
                    1, 3, 4, 3, 2,
                    1, 4, 4, 3, 3,
                    1, 5, 4, 3, 4,
                    1, 4, 1, 4, 1,
                    1, 4, 2, 4, 2,
                    1, 5, 2, 4, 3,
                    1, 5, 3, 4, 4,
                    1, 5, 3, 5, 1,
                    1, 5, 4, 5, 2,
                    1, 2, 6, 6, 1,
                    1, 3, 6, 6, 2,
                    1, 4, 5, 6, 3,
                    1, 5, 5, 6, 4,
                    1, 5, 4, 6, 5,
                    1, 5, 4, 7, 1,
                    1, 6, 4, 7, 2};
  Real strm_vals[23] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 
                        16, 17, 18, 19, 20, 21, 22};
  std::vector<Real> strm(23*24, 0);

  for (int i = 0; i < 23; ++i)
    strm[i*24] = strm_vals[i];

  pSFRLine1.SetField("NSTRM", &nstrm);
  pSFRLine1.SetField("NSS", &nss);
  pSFRLine1.SetField("ISFROPT", &isfropt);

  pSFRLine2.SetField("ISTRM", &istrm[0]);
  pSFRLine2.SetField("NISTRMD", &nistrmd);
  pSFRLine2.SetField("NSTRMD", &nstrmd);

  CStr expected, output;
  CStr f;
  util::GetTempDirectory(f);
  f += "\\sfrstream";

  CStr f1(f+".sfr"), f2(f+".h5");
  CreateDefaultMfH5File(f);
  {
    TxtExporter t(f);

    // don't export without all fields
    expSFRLine2(&pSFRLine1, &pSFRLine2, nrow, ncol, nSp, &t);

    t.GetFileContents(Packages::SFR, output);
    TS_ASSERT(output.empty());
  }

  pSFRLine2.SetField("STRM", &strm[0]);

  {
    TxtExporter t(f);

    expSFRLine2(&pSFRLine1, &pSFRLine2, nrow, ncol, nSp, &t);

    H5DataReader::CloseAllH5FilesOpenForWriting();
    t.GetFileContents(Packages::SFR, output);
    expected = "GMS_HDF5_SFR2_REACH \"sfrstream.h5\" \"SFR2\"\n";

    TS_ASSERT_EQUALS2(expected, output);
    {
      std::vector<int>vI;
      H5DataSetReader bcs(f2, "Stream (SFR2)/00. Number of BCs");
      bcs.GetAllData(vI);
      TS_ASSERT(vI.size() == 1);
      TS_ASSERT_EQUALS(vI.at(0), 23);
      vI.clear();

      H5DataSetReader cellids(f2, "Stream (SFR2)/02. Cell IDs");
      cellids.GetAllData(vI);
      TS_ASSERT(vI.size() == 23);
      TS_ASSERT_EQUALS(vI.at(0), 3);
      TS_ASSERT_EQUALS(vI.at(11), 20);
      TS_ASSERT_EQUALS(vI.at(22), 34);

      std::vector<double>v;
      H5DataSetReader properties(f2, "Stream (SFR2)/07. Property");
      properties.GetAllData(v);
      TS_ASSERT(v.size() == 23*3);
      TS_ASSERT_EQUALS(v.at(0), 0.0);
      TS_ASSERT_EQUALS(v.at(23*3-1), 22.0);
      TS_ASSERT_EQUALS(v.at(23*3-2), 22.0);
      TS_ASSERT_EQUALS(v.at(23*3-3), 22.0);

      H5DataSetReader reachSegs(f2, "Stream (SFR2)/08. Str reach segment ID");
      reachSegs.GetAllData(vI);
      TS_ASSERT(vI.size() == 23);
      TS_ASSERT_EQUALS(vI.at(0), 1);
      TS_ASSERT_EQUALS(vI.at(9), 3);
      TS_ASSERT_EQUALS(vI.at(22), 7);

      H5DataSetReader numSegs(f2, "Stream (SFR2)/13. Number of Segments");
      numSegs.GetAllData(vI);
      TS_ASSERT(vI.size() == 1);
      TS_ASSERT_EQUALS(vI.at(0), 7);
    }
  }
  H5Reader::CloseAllH5Files();
  TS_ASSERT(!remove(f1));
  TS_ASSERT(!remove(f2));
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpSFRLine5 ()
{
  using namespace MfData::Packages;
  MfData::MfPackage pSFRLine5(Packages::SFRLine5);
  int itmp(1), irdflg(2), iptflg(3), stressPeriod(2);

  pSFRLine5.SetField("ITMP", &itmp);
  pSFRLine5.SetField("IRDFLG", &irdflg);

  CStr expected, output;
  CStr f;
  util::GetTempDirectory(f);
  f += "\\sfrstream";

  CStr f1(f+".sfr"), f2(f+".h5");
  CreateDefaultMfH5File(f);
  {
    TxtExporter t(f);

    // don't export without all fields
    expSFRLine5(&pSFRLine5, stressPeriod, &t);

    t.GetFileContents(Packages::SFR, output);
    TS_ASSERT(output.empty());
  }

  pSFRLine5.SetField("IPTFLG", &iptflg);

  {
    TxtExporter t(f);

    expSFRLine5(&pSFRLine5, stressPeriod, &t);
    itmp = -1;
    irdflg = 0;
    iptflg = 0;
    stressPeriod = 3;
    expSFRLine5(&pSFRLine5, stressPeriod, &t);

    H5DataReader::CloseAllH5FilesOpenForWriting();
    t.GetFileContents(Packages::SFR, output);
    expected = "1 2 3\n"
               "GMS_HDF5_01 \"sfrstream.h5\" \"SFR2\" 2\n"
               "-1 0 0\n";
    TS_ASSERT_EQUALS2(expected, output);

    int expectedUseLast[3] = {0, 0, 1};
    TS_ASSERT_H5_ARRAY(f2, "Stream (SFR2)/01. Use Last", expectedUseLast, 3);
  }
  H5Reader::CloseAllH5Files();
  TS_ASSERT(!remove(f1));
  TS_ASSERT(!remove(f2));
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpSFRLine6 ()
{
  using namespace MfData::Packages;
  MfData::MfPackage pSFRLine1(Packages::SFRLine1);
  MfData::MfPackage pSFRLine6(Packages::SFRLine6);
  int sz(26), nstrm(-10), nss(3), isfropt(0), stressPeriod(2);
  int iseg[3*4] = { 2, 3, 0, 5,
                    3, 4, 2, 6,
                    4, 5, 3, 7 };
  int iotsg[3] = { 21, 22, 23 };
  int idivar[3*2] = { 31, 32, 33, 34, 35, 36 };
  Real seg[3*26];
  Real xsec[3*16];
  Real qstage[3*3*50];

  // fill seg with 101 - 178
  Real base = 101.0;
  for (int i = 0; i < 3*sz; ++i)
    seg[i] = base + i;

  // fill xsec with 201 - 248
  base = 201.0;
  for (int i = 0; i < 3*16; ++i)
    xsec[i] = base + i;

  // fill qstage with 1001 - 1450
  base = 1001.0;
  for (int i = 0; i < 3*3*50; ++i)
    qstage[i] = base + i;

  pSFRLine1.SetField("NSTRM", &nstrm);
  pSFRLine1.SetField("NSS", &nss);
  pSFRLine1.SetField("ISFROPT", &isfropt);

  pSFRLine6.SetField("ISEG", &iseg[0]);
  pSFRLine6.SetField("IOTSG", &iotsg[0]);
  pSFRLine6.SetField("IDIVAR", &idivar[0]);
  pSFRLine6.SetField("SEG", &seg[0]);
  pSFRLine6.SetField("XSEC", &xsec[0]);

  CStr expected, output;
  CStr f;
  util::GetTempDirectory(f);
  f += "\\sfrstream";

  CStr h5FileName(f+".h5");
  CreateDefaultMfH5File(f);
  {
    TxtExporter t(f);

    // don't export without all fields
    expSFRLine6(&pSFRLine1, &pSFRLine6, stressPeriod, &t);

    t.GetFileContents(Packages::SFR, output);
    TS_ASSERT(output.empty());
  }

  pSFRLine6.SetField("QSTAGE", &qstage[0]);
  
  {
    TxtExporter t(f);

    expSFRLine6(&pSFRLine1, &pSFRLine6, stressPeriod, &t);

    H5DataReader::CloseAllH5FilesOpenForWriting();
    t.GetFileContents(Packages::SFR, output);

    TS_ASSERT(output.empty());
    {
      std::vector<double> vD;
      H5DataSetReader numSegs(h5FileName, "Stream (SFR2)/14. Segment Property");
      numSegs.GetAllData(vD);
      TS_ASSERT_EQUALS(vD.size(), 252);
      TS_ASSERT_EQUALS(vD.at(1), 2.0);
      TS_ASSERT_EQUALS(vD.at(87), 132.0);
      TS_ASSERT_EQUALS(vD.at(89), 158.0);
      TS_ASSERT_EQUALS(vD.at(117), 137.0);
      TS_ASSERT_EQUALS(vD.at(119), 163.0);
      TS_ASSERT_EQUALS(vD.at(121), 112.0);
      TS_ASSERT_EQUALS(vD.at(239), 248.0);

      H5DataSetReader sft(h5FileName, "Stream (SFR2)/15. Segment Flow Table");
      sft.GetAllData(vD);
      TS_ASSERT(vD.size() == 25);

      Real vExpected[25] = { 3, 3, 3, 3, 3,
                             2, 2, 2, 2, 2,
                             1301, 1302, 1303, 1304, 1305,
                             1306, 1307, 1308, 1309, 1310,
                             1311, 1312, 1313, 1314, 1315};
      for (int i = 0; i < 25; ++i)
        TS_ASSERT_EQUALS(vExpected[i], vD[i]);
      int numRows(0);
      sft.GetAtt("NumRows", numRows);
      TS_ASSERT_EQUALS(5, numRows);
    }
  }
  H5Reader::CloseAllH5Files();
  TS_ASSERT(!remove(h5FileName));
  
  // test with parameters
  CreateDefaultMfH5File(f);
  {
    TxtExporter t(f);

    Param p("SFR1", -5, "SFR", 6, 1, 20);
    p.m_str_start = 2;
    p.m_str_nbc = 2;
    ParamList *list;
    Parameters::GetParameterList(&list);
    list->Clear();
    list->PushBack(&p);

    expSFRLine6(&pSFRLine1, &pSFRLine6, stressPeriod, &t);

    list->Clear();
    H5DataReader::CloseAllH5FilesOpenForWriting();
    t.GetFileContents(Packages::SFR, output);

    TS_ASSERT(output.empty());
    {
      std::vector<double> vD;
      H5DataSetReader numSegs(h5FileName, "Stream (SFR2)/14. Segment Property");
      numSegs.GetAllData(vD);
      TS_ASSERT_EQUALS(vD.size(), 252);
      TS_ASSERT_EQUALS(vD.at(1), 2.0);
      // parameter items
      TS_ASSERT_EQUALS(vD.at(87), -5.0);
      TS_ASSERT_EQUALS(vD.at(89), -5.0);
      TS_ASSERT_EQUALS(vD.at(117), -5.0);
      TS_ASSERT_EQUALS(vD.at(119), -5.0);

      TS_ASSERT_EQUALS(vD.at(239), 248.0);

      H5DataSetReader sft(h5FileName, "Stream (SFR2)/15. Segment Flow Table");
      sft.GetAllData(vD);
      TS_ASSERT(vD.size() == 25);

      Real vExpected[25] = { 3, 3, 3, 3, 3,
                             2, 2, 2, 2, 2,
                             1301, 1302, 1303, 1304, 1305,
                             1306, 1307, 1308, 1309, 1310,
                             1311, 1312, 1313, 1314, 1315};
      for (int i = 0; i < 25; ++i)
        TS_ASSERT_EQUALS(vExpected[i], vD[i]);
      int numRows(0);
      sft.GetAtt("NumRows", numRows);
      TS_ASSERT_EQUALS(5, numRows);
    }
  }
  H5Reader::CloseAllH5Files();
  TS_ASSERT(!remove(h5FileName));
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testWellPropertyList()
{
  TxtExporter t(TESTBASE);
  WellPropertyList& wpl = t.m_public->m_WellPropertyList;

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
void ExpGmsH5T::testexpMNWSetup ()
{
  using namespace MfData::Packages;
  MfData::MfPackage p(Packages::MNWSetup);
  int mxwel2(3), iwl2cb(4), iwelpt(5), kspref(6);
  int iowell2[3] = { 1, 2, 3 };
  double ploss(0.0);
  char *ftag = "WEL1  QSUM  BYNODE";

  p.SetField("MXMNW", &mxwel2);
  p.SetField("IWL2CB", &iwl2cb);
  p.SetField("IWELPT", &iwelpt);
  p.SetField("KSPREF", &kspref);
  p.SetField("IOWELL2", &iowell2[0]);
  p.SetField("PLossMNW", &ploss);

  CStr expected, output;
  CStr f;
  util::GetTempDirectory(f);
  f += "\\MultiNodeTest";

  CStr fileMnw(f+".mnw1"), fileH5(f+".h5");
  CreateDefaultMfH5File(f);
  {
    TxtExporter t(f);

    // don't export without all fields
    expMNWSetup(&p, &t);

    t.GetFileContents(Packages::MNW, output);
    TS_ASSERT(output.empty());
  }

  p.SetField("FTAG", &ftag[0]);

  // Aux output no ALLTIME, SKIN
  {
    TxtExporter t(f);

    expMNWSetup(&p, &t);

    H5DataReader::CloseAllH5FilesOpenForWriting();
    t.GetFileContents(Packages::MNW, output);
    expected = "#GMS_HDF5_01\n"
               "3 4 5 REFERENCE SP:6\n"
               "SKIN\n"
               "FILE:MNW-WEL1.out WEL1:1\n"
               "FILE:MNW-BYNODE.out BYNODE:3\n"
               "FILE:MNW-QSUM.out QSUM:2\n";
    TS_ASSERT_H5_INT(fileH5, "Multi-Node Well/21. Stress Period Ref", 6);
    TS_ASSERT_H5_DBL(fileH5, "Multi-Node Well/22. Loss Type", 0.0);
    int iowell2Expected[] = { 1, 3, 2 };
    TS_ASSERT_H5_ARRAY(fileH5, "Multi-Node Well/23. Well IO",
                       iowell2Expected, 3);
  }
  H5Reader::CloseAllH5Files();
  TS_ASSERT(!remove(fileMnw));
  TS_ASSERT(!remove(fileH5));

  // aux output with ALLTIME, LINEAR
  kspref = 5;
  iowell2[0] = -2;
  iowell2[1] =  1;
  iowell2[2] = -3;
  ploss = 1.0;
  ftag = "BYNODEWEL1  QSUM  ";
  p.SetField("FTAG", &ftag[0]);
  CreateDefaultMfH5File(f);
  {
    TxtExporter t(f);

    expMNWSetup(&p, &t);

    H5DataReader::CloseAllH5FilesOpenForWriting();
    t.GetFileContents(Packages::MNW, output);
    expected = "#GMS_HDF5_01\n"
               "3 4 5 REFERENCE SP:6\n"
               "LINEAR\n"
               "FILE:MNW-WEL1.out WEL1:1\n"
               "FILE:MNW-BYNODE.out BYNODE:3 ALLTIME\n"
               "FILE:MNW-QSUM.out QSUM:2 ALLTIME\n";
    TS_ASSERT_H5_INT(fileH5, "Multi-Node Well/21. Stress Period Ref", 5);
    TS_ASSERT_H5_DBL(fileH5, "Multi-Node Well/22. Loss Type", 1.0);
    int iowell2Expected[] = { 1, -2, -3 };
    TS_ASSERT_H5_ARRAY(fileH5, "Multi-Node Well/23. Well IO",
                       iowell2Expected, 3);
  }
  H5Reader::CloseAllH5Files();
  TS_ASSERT(!remove(fileMnw));
  TS_ASSERT(!remove(fileH5));

  // no output with ALLTIME, NONLINEAR
  kspref = 4;
  iowell2[0] = 0;
  iowell2[1] = 0;
  iowell2[2] = 0;
  ploss = 4.0;
  CreateDefaultMfH5File(f);
  {
    TxtExporter t(f);

    expMNWSetup(&p, &t);

    H5DataReader::CloseAllH5FilesOpenForWriting();
    t.GetFileContents(Packages::MNW, output);
    expected = "#GMS_HDF5_01\n"
               "3 4 5 REFERENCE SP:6\n"
               "NONLINEAR 4.0\n";
    TS_ASSERT_H5_INT(fileH5, "Multi-Node Well/21. Stress Period Ref", 4);
    TS_ASSERT_H5_DBL(fileH5, "Multi-Node Well/22. Loss Type", 4.0);
    int iowell2Expected[] = { 0, 0, 0 };
    TS_ASSERT_H5_ARRAY(fileH5, "Multi-Node Well/23. Well IO",
                       iowell2Expected, 3);
  }
  H5Reader::CloseAllH5Files();
  TS_ASSERT(!remove(fileMnw));
  TS_ASSERT(!remove(fileH5));
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpMNWStressPeriod ()
{
  using namespace MfData::Packages;
  MfData::MfPackage p(Packages::MNWStressPeriod);
  int itmp(3), nwell2(3);
  //                     CID  Qdes      QW   Rw  SKIN Hlim Href Iwgrp
  //                                    Qmn  Qmx       C         ID
  double well2[18*5] = { 1.0, 6.1, 0.0, 2.0, 0.0, 3.0, 4.1, 5.1, 6.0,
                         0.0, 0.0, 0.0, 0.1, 0.6, 0.0, 1.0, 0.0, 1.0,

                         2.0, 7.1, 0.0, 3.0, 1.0, 4.0, 5.1, 6.1, 7.0,
                         0.0, 0.0, 0.0, 0.2, 0.7, 0.0, 2.0, 0.0, 1.0,

                         3.0, 8.1, 0.0, 4.0,-1.0, 5.0, 6.1, 7.1, 8.0,
                         0.0, 0.0, 0.0, 0.3, 0.9, 0.0, 3.0, 0.0, 2.0,

                         4.0, 9.1, 0.0, 5.0, 2.0, 6.0, 7.1, 8.1, 9.0,
                         0.0, 0.0, 0.0, 0.4, 0.1, 0.0, 4.0, 0.0, 3.0,

                         5.0, 1.1, 0.0, 6.0,-2.0, 7.0, 8.1, 9.1, 1.0,
                         0.0, 0.0, 0.0, 0.5, 0.2, 0.0, 5.0, 0.0, 4.0 };
  double mnwflgs[7*5] = { 6.0, 4.0, 5.0, 0.0, 0.0, 0.0, 0.0,
                          7.0, 5.0, 6.0, 1.0, 1.0, 0.0, 0.0,
                          8.0, 6.0, 7.0, 2.0, 2.0, 1.0, 0.0,
                          9.0, 7.0, 8.0, 0.0, 0.0, 0.0, 0.0,
                          1.0, 8.0, 9.0, 1.0, 1.0, 1.0, 0.0};
  char mnwsite[32*5+1] = "well-1                          "
                         "well-1                          "
                         "NO-PRINT                        "
                         "well-2                          "
                         "well-3                          ";

  p.SetField("ITMP", &itmp);
  p.SetField("NWELL2", &nwell2);
  p.SetField("WELL2", &well2[0]);
  p.SetField("MNWsite", &mnwsite[0]);

  CStr expected, output;
  CStr fileBase;
  util::GetTempDirectory(fileBase);
  fileBase += "\\MultiNodeTest";

  CStr fileMnw(fileBase+".mnw1"), fileH5(fileBase+".h5");
  CreateDefaultMfH5File(fileBase);
  {
    TxtExporter t(fileBase);

    // don't export without all fields
    expMNWStressPeriod(&p, 1, &t);

    t.GetFileContents(Packages::MNW, output);
    TS_ASSERT(output.empty());
  }

  p.SetField("MNWFLGS", &mnwflgs[0]);

  {
    TxtExporter t(fileBase);

    expMNWStressPeriod(&p, 1, &t);

    t.GetFileContents(Packages::MNW, output);
    expected = "3\n"
               "GMS_HDF5_MNW \"MultiNodeTest.h5\" \"MNW1\" 1\n";
    TS_ASSERT_EQUALS2(expected, output);
    TS_ASSERT_H5_INT(fileH5, "Multi-Node Well/00. Number of BCs", 3);

    int expectedUseLast[] = { 0, 0, 0, 1, 0 };
    TS_ASSERT_H5_ARRAY(fileH5, "Multi-Node Well/01. Use Last",
                       expectedUseLast, 1);

    int expectedCellIds[] = { 1, 2, 3, 4, 5 };
    TS_ASSERT_H5_ARRAY(fileH5, "Multi-Node Well/02. Cell IDs",
                       expectedCellIds, 3);

    char expectedName[] = "well-1\x00well-1\x00Well-2\x00Well-3\x00Well-4";
    TS_ASSERT_H5_ARRAY(fileH5, "Multi-Node Well/03. Name", expectedName,
                       21);
    TS_ASSERT_H5_INT_ATT(fileH5, "Multi-Node Well/03. Name", MFBC_MAX_STR_LEN,
                         7);

    vector<double> properties;
    GetH5Array(fileH5, "Multi-Node Well/07. Property", properties);
    double expectedProp[] = { 
      1, 1, 1,       // active
      6, 7, 8,       // qdes
      1, 1, 0,       // wellid
      2, 3, 4,       // qwval
      0, 1, -1,      // rw
      3, 4, 5,       // skin
      0, 5, 6,       // hlim
      0, 6, 7,       // href
      0, 1, 2,       // dd
      6, 7, -1,      // iwgrp
      1, 2, 3,       // c
      0, 1, 2,       // qcut
      0, 1.4, 30,    // qfrcmn
      0, 4.9, 90,    // qfrcmx
      1, 1, 0};      // mnwsite
    TS_ASSERT_H5_ARRAY(fileH5, "Multi-Node Well/07. Property", expectedProp,
                       15*3);
    TS_ASSERT_EQUALS(15*3, properties.size());
    TS_ASSERT_DELTA(1.0, properties.at(0), CXXDELTA);
    TS_ASSERT_DELTA(90.0, properties.at(41), CXXDELTA);

    itmp = 5;
    nwell2 = 5;
    expMNWStressPeriod(&p, 2, &t);

    t.GetFileContents(Packages::MNW, output);
    expected = "3\n"
               "GMS_HDF5_MNW \"MultiNodeTest.h5\" \"MNW1\" 1\n"
               "5\n"
               "GMS_HDF5_MNW \"MultiNodeTest.h5\" \"MNW1\" 2\n";
    TS_ASSERT_EQUALS2(expected, output);
    TS_ASSERT_H5_INT(fileH5, "Multi-Node Well/00. Number of BCs", 5);
    TS_ASSERT_H5_ARRAY(fileH5, "Multi-Node Well/01. Use Last",
                       expectedUseLast, 2);
    TS_ASSERT_H5_ARRAY(fileH5, "Multi-Node Well/02. Cell IDs",
                       expectedCellIds, 5);
    TS_ASSERT_H5_ARRAY(fileH5, "Multi-Node Well/03. Name", expectedName,
                       35);
    TS_ASSERT_H5_INT_ATT(fileH5, "Multi-Node Well/03. Name", MFBC_MAX_STR_LEN,
                         7);
    GetH5Array(fileH5, "Multi-Node Well/07. Property", properties);
    TS_ASSERT_EQUALS(15*5*2, properties.size());
    TS_ASSERT_DELTA(1.0, properties.at(0), CXXDELTA);
    TS_ASSERT_DELTA(0.0, properties.at(15*5*2-1), CXXDELTA);

    itmp = 3;
    nwell2 = 3;
    expMNWStressPeriod(&p, 3, &t);

    t.GetFileContents(Packages::MNW, output);
    expected = "3\n"
               "GMS_HDF5_MNW \"MultiNodeTest.h5\" \"MNW1\" 1\n"
               "5\n"
               "GMS_HDF5_MNW \"MultiNodeTest.h5\" \"MNW1\" 2\n"
               "3\n"
               "GMS_HDF5_MNW \"MultiNodeTest.h5\" \"MNW1\" 3\n";
    TS_ASSERT_EQUALS2(expected, output);
    TS_ASSERT_H5_INT(fileH5, "Multi-Node Well/00. Number of BCs", 5);
    TS_ASSERT_H5_ARRAY(fileH5, "Multi-Node Well/01. Use Last",
                       expectedUseLast, 3);
    TS_ASSERT_H5_ARRAY(fileH5, "Multi-Node Well/02. Cell IDs",
                       expectedCellIds, 5);
    TS_ASSERT_H5_ARRAY(fileH5, "Multi-Node Well/03. Name", expectedName,
                       35);
    TS_ASSERT_H5_INT_ATT(fileH5, "Multi-Node Well/03. Name", MFBC_MAX_STR_LEN,
                         7);
    GetH5Array(fileH5, "Multi-Node Well/07. Property", properties);
    TS_ASSERT_EQUALS(15*5*3, properties.size());
    TS_ASSERT_DELTA(1.0, properties.at(0), CXXDELTA);
    TS_ASSERT_DELTA(0.0, properties.at(13*5*3-1), CXXDELTA);

    itmp = -1;
    nwell2 = 5;
    expMNWStressPeriod(&p, 4, &t);

    t.GetFileContents(Packages::MNW, output);
    expected = "3\n"
               "GMS_HDF5_MNW \"MultiNodeTest.h5\" \"MNW1\" 1\n"
               "5\n"
               "GMS_HDF5_MNW \"MultiNodeTest.h5\" \"MNW1\" 2\n"
               "3\n"
               "GMS_HDF5_MNW \"MultiNodeTest.h5\" \"MNW1\" 3\n"
               "-1\n";
    TS_ASSERT_EQUALS2(expected, output);
    TS_ASSERT_H5_INT(fileH5, "Multi-Node Well/00. Number of BCs", 5);
    TS_ASSERT_H5_ARRAY(fileH5, "Multi-Node Well/01. Use Last",
                       expectedUseLast, 4);
    TS_ASSERT_H5_ARRAY(fileH5, "Multi-Node Well/02. Cell IDs",
                       expectedCellIds, 5);
    TS_ASSERT_H5_ARRAY(fileH5, "Multi-Node Well/03. Name", expectedName,
                       35);
    TS_ASSERT_H5_INT_ATT(fileH5, "Multi-Node Well/03. Name", MFBC_MAX_STR_LEN,
                         7);
    GetH5Array(fileH5, "Multi-Node Well/07. Property", properties);
    TS_ASSERT_EQUALS(15*5*4, properties.size());
    TS_ASSERT_DELTA(1.0, properties.at(0), CXXDELTA);
    TS_ASSERT_DELTA(0.5, properties.at(13*5*4-1), CXXDELTA);

    itmp = 0;
    nwell2 = 0;
    expMNWStressPeriod(&p, 5, &t);

    t.GetFileContents(Packages::MNW, output);
    expected = "3\n"
               "GMS_HDF5_MNW \"MultiNodeTest.h5\" \"MNW1\" 1\n"
               "5\n"
               "GMS_HDF5_MNW \"MultiNodeTest.h5\" \"MNW1\" 2\n"
               "3\n"
               "GMS_HDF5_MNW \"MultiNodeTest.h5\" \"MNW1\" 3\n"
               "-1\n"
               "0\n";
    TS_ASSERT_EQUALS2(expected, output);
    TS_ASSERT_H5_INT(fileH5, "Multi-Node Well/00. Number of BCs", 5);
    TS_ASSERT_H5_ARRAY(fileH5, "Multi-Node Well/01. Use Last",
                       expectedUseLast, 5);
    TS_ASSERT_H5_ARRAY(fileH5, "Multi-Node Well/02. Cell IDs",
                       expectedCellIds, 5);
    TS_ASSERT_H5_ARRAY(fileH5, "Multi-Node Well/03. Name", expectedName,
                       35);
    TS_ASSERT_H5_INT_ATT(fileH5, "Multi-Node Well/03. Name", MFBC_MAX_STR_LEN,
                         7);
    GetH5Array(fileH5, "Multi-Node Well/07. Property", properties);
    TS_ASSERT_EQUALS(15*5*5, properties.size());
    TS_ASSERT_DELTA(1.0, properties.at(0), CXXDELTA);
    TS_ASSERT_DELTA(0.0, properties.at(13*5*5-1), CXXDELTA);

    H5DataReader::CloseAllH5FilesOpenForWriting();
  }
  H5Reader::CloseAllH5Files();
  TS_ASSERT(!remove(fileMnw));
  TS_ASSERT(!remove(fileH5));
}
static void iFillMap_SUB_Test (TxtExporter* a_exp)
{
  std::map<CStr,CStr> &m(GetTxtLineArrayMap(a_exp));
  m.insert(std::make_pair("SUB/12. RNB1", "SUB_RNB_x_1"));
  m.insert(std::make_pair("SUB/12. RNB2", "SUB_RNB_x_2"));
  m.insert(std::make_pair("SUB/14. HC1", "SUB_HC_x_1"));
  m.insert(std::make_pair("SUB/14. HC2", "SUB_HC_x_2"));
  m.insert(std::make_pair("SUB/16. Sfe1", "SUB_Sfe_x_1"));
  m.insert(std::make_pair("SUB/16. Sfe2", "SUB_Sfe_x_2"));
  m.insert(std::make_pair("SUB/18. Sfv1", "SUB_Sfv_x_1"));
  m.insert(std::make_pair("SUB/18. Sfv2", "SUB_Sfv_x_2"));
  m.insert(std::make_pair("SUB/20. Com1", "SUB_Com_x_1"));
  m.insert(std::make_pair("SUB/20. Com2", "SUB_Com_x_2"));
  m.insert(std::make_pair("SUB/22. Dstart1", "SUB_Dstart_x_1"));
  m.insert(std::make_pair("SUB/22. Dstart2", "SUB_Dstart_x_2"));
  m.insert(std::make_pair("SUB/24. DHC1", "SUB_DHC_x_1"));
  m.insert(std::make_pair("SUB/24. DHC2", "SUB_DHC_x_2"));
  m.insert(std::make_pair("SUB/26. DCOM1", "SUB_DCOM_x_1"));
  m.insert(std::make_pair("SUB/26. DCOM2", "SUB_DCOM_x_2"));
  m.insert(std::make_pair("SUB/28. DZ1", "SUB_DZ_x_1"));
  m.insert(std::make_pair("SUB/28. DZ2", "SUB_DZ_x_2"));
  m.insert(std::make_pair("SUB/30. NZ1", "SUB_NZ_x_1"));
  m.insert(std::make_pair("SUB/30. NZ2", "SUB_NZ_x_2"));
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpSUB ()
{
  using namespace MfData::Packages;
  MfData::MfPackage pSUB(Packages::SUB);

  int isubcb(0), isuboc(1), nndb(0), ndb(0), nmz(2), nn(3), itmin(4),
    idsave(5), idrest(6);
  int ln[] = { 1, 2 };
  int ldn[] = { 3, 4 };
  const Real ac1(1), ac2(2);
  const Real dp[] = { 1, 2, 3, 4, 5, 6 };

  pSUB.SetField("ISUBCB", &isubcb);
  pSUB.SetField("ISUBOC", &isuboc);
  pSUB.SetField("NNDB", &nndb);
  pSUB.SetField("NDB", &ndb);
  pSUB.SetField("NMZ", &nmz);
  pSUB.SetField("NN", &nn);
  pSUB.SetField("ITMIN", &itmin);
  pSUB.SetField("IDSAVE", &idsave);
  pSUB.SetField("IDREST", &idrest);
  pSUB.SetField("AC1", &ac1);

  CStr expected, output;

  {
    // all fields need to be set
    TxtExporter t(TESTBASE);
    iFillMap_SUB_Test(&t);
    expSUB(&pSUB, &t);
    t.GetFileContents(Packages::SUB, output);
    TS_ASSERT(output.empty());
  }

  {
    // zero no-delay systems
    // zero delay systems
    // only outputs first line
    TxtExporter t(TESTBASE);
    iFillMap_SUB_Test(&t);
    pSUB.SetField("AC2", &ac2);
    expSUB(&pSUB, &t);
    t.GetFileContents(Packages::SUB, output);
    expected = "0 1 0 0 2 3 1.0 2.0 4 5 6\n";
    TS_ASSERT_EQUALS2(expected, output);
  }

  {
    // 2 no-delay systems
    // zero delay systems
    TxtExporter t(TESTBASE);
    iFillMap_SUB_Test(&t);
    nndb = 2;
    pSUB.SetField("LN", &ln[0]);
    expSUB(&pSUB, &t);
    t.GetFileContents(Packages::SUB, output);
    expected = 
      "0 1 2 0 2 3 1.0 2.0 4 5 6\n"
      "1 2\n"
      "SUB_HC_x_1\n"
      "SUB_Sfe_x_1\n"
      "SUB_Sfv_x_1\n"
      "SUB_Com_x_1\n"
      "SUB_HC_x_2\n"
      "SUB_Sfe_x_2\n"
      "SUB_Sfv_x_2\n"
      "SUB_Com_x_2\n";
    TS_ASSERT_EQUALS2(expected, output);
  }

  {
    // no no-delay systems
    // 2 delay systems IDREST > 0
    TxtExporter t(TESTBASE);
    iFillMap_SUB_Test(&t);
    nndb = 0;
    ndb = 2;
    pSUB.SetField("LN", (int*)NULL);
    pSUB.SetField("LDN", &ldn[0]);
    pSUB.SetField("DP", &dp[0]);
    expSUB(&pSUB, &t);
    t.GetFileContents(Packages::SUB, output);
    expected = 
      "0 1 0 2 2 3 1.0 2.0 4 5 6\n"
      "3 4\n"
      "SUB_RNB_x_1\n"
      "SUB_RNB_x_2\n"
      "1.0 3.0 5.0\n"
      "2.0 4.0 6.0\n"
      "SUB_DCOM_x_1\n"
      "SUB_DZ_x_1\n"
      "SUB_NZ_x_1\n"
      "SUB_DCOM_x_2\n"
      "SUB_DZ_x_2\n"
      "SUB_NZ_x_2\n";
    TS_ASSERT_EQUALS2(expected, output);
  }

  {
    // no no-delay systems
    // 2 delay systems IDREST <= 0
    TxtExporter t(TESTBASE);
    iFillMap_SUB_Test(&t);
    nndb = 0;
    ndb = 2;
    idrest = 0;
    expSUB(&pSUB, &t);
    t.GetFileContents(Packages::SUB, output);
    expected = 
      "0 1 0 2 2 3 1.0 2.0 4 5 0\n"
      "3 4\n"
      "SUB_RNB_x_1\n"
      "SUB_RNB_x_2\n"
      "1.0 3.0 5.0\n"
      "2.0 4.0 6.0\n"
      "SUB_Dstart_x_1\n"
      "SUB_DHC_x_1\n"
      "SUB_DCOM_x_1\n"
      "SUB_DZ_x_1\n"
      "SUB_NZ_x_1\n"
      "SUB_Dstart_x_2\n"
      "SUB_DHC_x_2\n"
      "SUB_DCOM_x_2\n"
      "SUB_DZ_x_2\n"
      "SUB_NZ_x_2\n";
    TS_ASSERT_EQUALS2(expected, output);
  }

} // ExpGmsH5T::testexpSUB
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpSUBLine15 ()
{
  using namespace MfData::Packages;
  MfData::MfPackage pSUB(Packages::SUBLine15);

  int isbocf[6] = { 1, 2, 3, 4, 5, 6 };
  int isbocu[6] = { 7, 8, 9, 10, 11, 12 };

  pSUB.SetField("ISBOCF", &isbocf[0]);

  CStr expected, output;

  {
    // all fields need to be set
    TxtExporter t(TESTBASE);
    expSUBLine15(&pSUB, &t);
    t.GetFileContents(Packages::SUB, output);
    TS_ASSERT(output.empty());
  }

  {
    TxtExporter t(TESTBASE);
    pSUB.SetField("ISBOCU", &isbocu[0]);
    expSUBLine15(&pSUB, &t);
    t.GetFileContents(Packages::SUB, output);
    expected = "1 7 2 8 3 9 4 10 5 11 6 12\n";
    TS_ASSERT_EQUALS2(expected, output);
  }
} // ExpGmsH5T::testexpSUBLine15
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpSUBLine16 ()
{
  using namespace MfData::Packages;
  MfData::MfPackage pSUB(Packages::SUBLine16);

  int isp1(1), isp2(2), jts1(3), jts2(4);
  int ifl[13] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13 };

  pSUB.SetField("ISP1", &isp1);
  pSUB.SetField("ISP2", &isp2);
  pSUB.SetField("JTS1", &jts1);
  pSUB.SetField("JTS2", &jts2);

  CStr expected, output;

  {
    // all fields need to be set
    TxtExporter t(TESTBASE);
    expSUBLine16(&pSUB, &t);
    t.GetFileContents(Packages::SUB, output);
    TS_ASSERT(output.empty());
  }

  {
    TxtExporter t(TESTBASE);
    pSUB.SetField("IFL", &ifl[0]);
    expSUBLine16(&pSUB, &t);
    t.GetFileContents(Packages::SUB, output);
    expected = "1 2 3 4 1 2 3 4 5 6 7 8 9 10 11 12 13\n";
    TS_ASSERT_EQUALS2(expected, output);
  }
} // ExpGmsH5T::testexpSUBLine16
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpUZFLine1 ()
{
  using namespace MfData::Packages;
  MfData::MfPackage pUZFLine1(Packages::UZFLine1);
  int nuztop(0), iuzfopt(0), irunflg(2), ietflg(3), iuzfcb1(4), iuzfcb2(5),
      ntrail2(6), nsets2(7), nuzgag(8);
  Real surfdep(1.0);

  pUZFLine1.SetField("NUZTOP", &nuztop);
  pUZFLine1.SetField("IUZFOPT", &iuzfopt);
  pUZFLine1.SetField("IRUNFLG", &irunflg);
  pUZFLine1.SetField("IETFLG", &ietflg);
  pUZFLine1.SetField("IUZFCB1", &iuzfcb1);
  pUZFLine1.SetField("IUZFCB2", &iuzfcb2);
  pUZFLine1.SetField("NTRAIL2", &ntrail2);
  pUZFLine1.SetField("NSETS2", &nsets2);
  pUZFLine1.SetField("NUZGAG", &nuzgag);

  CStr expected, output;
  TxtExporter t(TESTBASE);

  // all fields need to be set
  expUZFLine1(&pUZFLine1, &t);
  t.GetFileContents(Packages::UZF, output);
  TS_ASSERT(output.empty());

  // don't write NTRAIL2 and NSETS2 when IUZFOPT <= 0
  pUZFLine1.SetField("SURFDEP", &surfdep);
  expUZFLine1(&pUZFLine1, &t);
  expected += "0 0 2 3 4 5 8 1.0\n";
  t.GetFileContents(Packages::UZF, output);
  TS_ASSERT_EQUALS2(expected, output);

  // write NTRAIL2 and NSETS2 when IUZFOPT > 0
  iuzfopt = 1;
  expUZFLine1(&pUZFLine1, &t);
  expected += "0 1 2 3 4 5 6 7 8 1.0\n";
  t.GetFileContents(Packages::UZF, output);
  TS_ASSERT_EQUALS2(expected, output);
} // ExpGmsH5T::testexpUZFLine1
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpUZFLine8 ()
{
  using namespace MfData::Packages;
  MfData::MfPackage pUZFLine1(Packages::UZFLine1);
  MfData::MfPackage pUZFLine8(Packages::UZFLine8);

  int iuzfopt(0), irunflg(1), nuzgag(0);
  int iuzlist[] = {  1,  2,  3,  4,
                     0,  0,  1,  4,
                     5,  6,  7,  8 };

  pUZFLine1.SetField("IUZFOPT", &iuzfopt);
  pUZFLine1.SetField("IRUNFLG", &irunflg);
  pUZFLine1.SetField("NUZGAG", &nuzgag);

  CStr expected, output;
  TxtExporter t(TESTBASE);

  // all fields need to be set
  expUZFLine8(&pUZFLine1, &pUZFLine8, 2, 3, &t);
  t.GetFileContents(Packages::UZF, output);
  TS_ASSERT(output.empty());

  // write nothing when NUZGAG is zero
  // don't write arrays when they aren't available
  pUZFLine8.SetField("IUZLIST", iuzlist);
  expUZFLine8(&pUZFLine1, &pUZFLine8, 2, 3, &t);
  t.GetFileContents(Packages::UZF, output);
  TS_ASSERT(output.empty());

  GetTxtLineArrayMap(&t).insert(std::make_pair("UZF/12. IUZFBND1", "IUZFBND"));
  GetTxtLineArrayMap(&t).insert(std::make_pair("UZF/14. IRUNBND1", "IRUNBND"));
  GetTxtLineArrayMap(&t).insert(std::make_pair("UZF/16. VKS1", "VKS"));
  GetTxtLineArrayMap(&t).insert(std::make_pair("UZF/18. EPS1", "EPS"));
  GetTxtLineArrayMap(&t).insert(std::make_pair("UZF/20. THTS1", "THTS"));
  GetTxtLineArrayMap(&t).insert(std::make_pair("UZF/22. THTI1", "THTI"));

  // write NUZGAG items
  // write arrays when available in GetTxtLineArrayMap generated from
  // expDataArray
  nuzgag = 3;
  irunflg = 0;
  iuzfopt = 1;
  expUZFLine8(&pUZFLine1, &pUZFLine8, 2, 3, &t);
  expected += "IUZFBND\n"
              "IRUNBND\n"
              "VKS\n"
              "EPS\n"
              "THTS\n"
              "THTI\n"
              "1 2 3 4\n"
              "-1\n"
              "5 6 7 8\n";
  t.GetFileContents(Packages::UZF, output);
  TS_ASSERT_EQUALS2(expected, output);

} // ExpGmsH5T::testexpUZFLine8
static void iFillMap_UZF_Test (TxtExporter* a_exp)
{
  std::map<CStr, CStr>& m = GetTxtLineArrayMap(a_exp);
  m["UZF/07. Property1_0"] = "FINF";
  m["UZF/07. Property1_1"] = "PET";
  m["UZF/07. Property1_2"] = "EXTDP";
  m["UZF/07. Property1_3"] = "EXTWC";
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpUZFStressPeriod ()
{
  using namespace MfData::Packages;
  MfData::MfPackage pUZFLine1(Packages::UZFLine1);
  MfData::MfPackage pUZFSp(Packages::UZFStressPeriod);
  int iuzfopt(1), ietflg(1), nuzf1(1), nuzf2(2), nuzf3(3), nuzf4(4);

  pUZFLine1.SetField("IUZFOPT", &iuzfopt);
  pUZFLine1.SetField("IETFLG", &ietflg);
  pUZFSp.SetField("NUZF1", &nuzf1);
  pUZFSp.SetField("NUZF2", &nuzf2);
  pUZFSp.SetField("NUZF3", &nuzf3);

  CStr expected, output;

  // all fields need to be set
  {
    TxtExporter t(TESTBASE);
    expUZFStressPeriod(&pUZFLine1, &pUZFSp, 1, &t);
    t.GetFileContents(Packages::UZF, output);
    TS_ASSERT(output.empty());
  }

  pUZFSp.SetField("NUZF4", &nuzf4);

  // write everything when all NUZF > 0, IUZFOPT > 0, and IETFLG > 0
  {
    TxtExporter t(TESTBASE);
    iFillMap_UZF_Test(&t);
    expUZFStressPeriod(&pUZFLine1, &pUZFSp, 1, &t);
    expected = "1\n"
               "FINF\n"
               "2\n"
               "PET\n"
               "3\n"
               "EXTDP\n"
               "4\n"
               "EXTWC\n";
    t.GetFileContents(Packages::UZF, output);
    TS_ASSERT_EQUALS2(expected, output);
  }

  // skip lines 11 through 16 when IETFLG <= 0
  {
    TxtExporter t(TESTBASE);
    iFillMap_UZF_Test(&t);
    ietflg = 0;
    expUZFStressPeriod(&pUZFLine1, &pUZFSp, 1, &t);
    expected = "1\n"
               "FINF\n";
    t.GetFileContents(Packages::UZF, output);
    TS_ASSERT_EQUALS2(expected, output);
  }

  // skip lines 15 and 16 when IUZFOPT <= 0
  {
    TxtExporter t(TESTBASE);
    iFillMap_UZF_Test(&t);
    ietflg = 1;
    iuzfopt = 0;
    expUZFStressPeriod(&pUZFLine1, &pUZFSp, 1, &t);
    expected = "1\n"
               "FINF\n"
               "2\n"
               "PET\n"
               "3\n"
               "EXTDP\n";
    t.GetFileContents(Packages::UZF, output);
    TS_ASSERT_EQUALS2(expected, output);
  }

  // don't write arrays when NUZF < 1 and array map empty
  {
    TxtExporter t(TESTBASE);
    //iFillMap_UZF_Test(&t);
    iuzfopt = 1;
    nuzf1 = -nuzf1;
    nuzf2 = -nuzf2;
    nuzf3 = -nuzf3;
    nuzf4 = -nuzf4;
    expUZFStressPeriod(&pUZFLine1, &pUZFSp, 1, &t);
    expected = "-1\n"
               "-2\n"
               "-3\n"
               "-4\n";
    t.GetFileContents(Packages::UZF, output);
    TS_ASSERT_EQUALS2(expected, output);
  }
} // ExpGmsH5T::testexpUZFStressPeriod
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpVDFLine5 ()
{
  using namespace MfData::Packages;
  MfData::MfPackage pVDF(Packages::VDFLine5);
  int mt3drhoflg(0), mfnadvfd(1), nswtcpl(1), iwtable(3);
  Real densemin(4), densemax(5), dnscrit(6), denseref(7), drhodprhd(9),
       prhdref(10);
  int nsrhoeos(11);
  Real firstdt(14);
  Real drhodc[3] = { 8, 9, 10 };
  int mtrhospec[3] = { 12, 13, 14 };
  Real crhoref[3] = { 13, 14, 15 };

  pVDF.SetField("MT3DRHOFLG", &mt3drhoflg);
  pVDF.SetField("MFNADVFD", &mfnadvfd);
  pVDF.SetField("NSWTCPL", &nswtcpl);
  pVDF.SetField("IWTABLE", &iwtable);
  pVDF.SetField("DENSEMIN", &densemin);
  pVDF.SetField("DENSEMAX", &densemax);
  pVDF.SetField("DNSCRIT", &dnscrit);
  pVDF.SetField("DENSEREF", &denseref);
  pVDF.SetField("DRHODC", &drhodc[0]);
  pVDF.SetField("DRHODPRHD", &drhodprhd);
  pVDF.SetField("PRHDREF", &prhdref);
  pVDF.SetField("NSRHOEOS", &nsrhoeos);
  pVDF.SetField("MTRHOSPEC", &mtrhospec[0]);
  pVDF.SetField("CRHOREF", &crhoref[0]);

  CStr expected, output;

  // all fields need to be set
  {
    TxtExporter t(TESTBASE);
    expVDFLine5(&pVDF, &t);
    t.GetFileContents(Packages::VDF, output);
    TS_ASSERT(output.empty());
  }

  pVDF.SetField("FIRSTDT", &firstdt);

  // don't write line 3 (nswtcpl = 1)
  // write line 4 (mt3drhoflg >= 0)
  {
    TxtExporter t(TESTBASE);
    expVDFLine5(&pVDF, &t);
    expected = "0 1 1 3\n"
               "4.0 5.0\n"
               "7.0 8.0\n"
               "14.0\n";
    t.GetFileContents(Packages::VDF, output);
    TS_ASSERT_EQUALS2(expected, output);
  }

  // don't write line 3 (nswtcpl = 0)
  // write line 4 (mt3drhoflg >= 0)
  {
    TxtExporter t(TESTBASE);
    nswtcpl = 0;
    expVDFLine5(&pVDF, &t);
    expected = "0 1 0 3\n"
               "4.0 5.0\n"
               "7.0 8.0\n"
               "14.0\n";
    t.GetFileContents(Packages::VDF, output);
    TS_ASSERT_EQUALS2(expected, output);
  }

  // write line 3 (nswtcpl = -1)
  // write line 4 (mt3drhoflg >= 0)
  {
    TxtExporter t(TESTBASE);
    nswtcpl = -1;
    expVDFLine5(&pVDF, &t);
    expected = "0 1 -1 3\n"
               "4.0 5.0\n"
               "6.0\n"
               "7.0 8.0\n"
               "14.0\n";
    t.GetFileContents(Packages::VDF, output);
    TS_ASSERT_EQUALS2(expected, output);
  }

  // write line 3 (nswtcpl = 2)
  // write line 4a - 4c (mt3drhoflg == -1)
  {
    TxtExporter t(TESTBASE);
    nswtcpl = 2;
    mt3drhoflg = -1;
    nsrhoeos = 3;
    expVDFLine5(&pVDF, &t);
    expected = "-1 1 2 3\n"
               "4.0 5.0\n"
               "6.0\n"
               "7.0 9.0 10.0\n"
               "3\n"
               "12 8.0 13.0\n"
               "13 9.0 14.0\n"
               "14 10.0 15.0\n"
               "14.0\n";
    t.GetFileContents(Packages::VDF, output);
    TS_ASSERT_EQUALS2(expected, output);
  }
} // ExpGmsH5T::testexpVDFLine5
static void iFillMap_VDF_Test (TxtExporter* a_exp)
{
  std::map<CStr, CStr>& m = GetTxtLineArrayMap(a_exp);
  m.clear();
  m["VDF/07. Property1_0"] = "DENSE_LAYER1_1";
  m["VDF/07. Property1_1"] = "DENSE_LAYER1_2";
  m["VDF/07. Property1_2"] = "DENSE_LAYER1_3";
  m["VDF/07. Property2_0"] = "DENSE_LAYER2_1";
  m["VDF/07. Property2_1"] = "DENSE_LAYER2_2";
  m["VDF/07. Property2_2"] = "DENSE_LAYER2_3";
  m["VDF/07. Property3_0"] = "DENSE_LAYER3_1";
  m["VDF/07. Property3_1"] = "DENSE_LAYER3_2";
  m["VDF/07. Property3_2"] = "DENSE_LAYER3_3";
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpVDFStressPeriod ()
{
  using namespace MfData::Packages;
  MfData::MfPackage pVDF(Packages::VDFLine5);
  MfData::MfPackage pVDFSp(Packages::VDFStressPeriod);
  int indense(1), mt3drhoflg(1);

  CStr expected, output;
  CStr f;
  util::GetTempDirectory(f);
  f += "\\vdftest";

  CStr f1(f+".vdf"), f2(f+".h5");
  CreateDefaultMfH5File(f, MfData::SEAWAT);

  pVDF.SetField("MT3DRHOFLG", &mt3drhoflg);

  // all fields need to be set
  {
    TxtExporter t(f);
    iFillMap_VDF_Test(&t);
    expVDFStressPeriod(&pVDF, &pVDFSp, 1, 5, 4, 3, &t);
    t.GetFileContents(Packages::VDF, output);
    TS_ASSERT(output.empty());
  }

  pVDFSp.SetField("INDENSE", &indense);
  
  // don't write when mt3drhoflg != 0
  {
    TxtExporter t(f);
    iFillMap_VDF_Test(&t);
    expVDFStressPeriod(&pVDF, &pVDFSp, 1, 5, 4, 3, &t);
    t.GetFileContents(Packages::VDF, output);
    TS_ASSERT(output.empty());
  }

  // write everything when INDENSE > 0 && mt3drhoflg == 0
  {
    TxtExporter t(f);
    iFillMap_VDF_Test(&t);
    mt3drhoflg = 0;
    expVDFStressPeriod(&pVDF, &pVDFSp, 1, 5, 4, 3, &t);
    expected = "1\n"
               "DENSE_LAYER1_1\n"
               "DENSE_LAYER1_2\n"
               "DENSE_LAYER1_3\n";
    t.GetFileContents(Packages::VDF, output);
    TS_ASSERT_EQUALS2(expected, output);
  }

  // don't write DENSE when INDENSE <= 0
  {
    TxtExporter t(f);
    iFillMap_VDF_Test(&t);
    indense = 0;
    expVDFStressPeriod(&pVDF, &pVDFSp, 2, 5, 4, 3, &t);
    expected = "0\n";
    t.GetFileContents(Packages::VDF, output);
    TS_ASSERT_EQUALS2(expected, output);
  }

  // check use last
  {
    TxtExporter t(f);
    iFillMap_VDF_Test(&t);
    indense = -1;
    expVDFStressPeriod(&pVDF, &pVDFSp, 3, 5, 4, 3, &t);
    H5DataReader::CloseAllH5FilesOpenForWriting();
    expected = "-1\n";
    t.GetFileContents(Packages::VDF, output);
    TS_ASSERT_EQUALS2(expected, output);

    int expectedUseLast[3] = {1, 0, -1};
    TS_ASSERT_H5_ARRAY(f2, "VDF/01. Use Last", expectedUseLast, 3);
  }
  H5Reader::CloseAllH5Files();
  TS_ASSERT(!remove(f1));
  TS_ASSERT(!remove(f2));
} // ExpGmsH5T::testexpVDFStressPeriod
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpVSCLine3 ()
{
  using namespace MfData::Packages;
  MfData::MfPackage pVSC(Packages::VSCLine3);
  int mt3dmuflg(0), nsmueos(3), mutempopt(0), mtmutempspec(1);
  int mtmuspec[3] = { 3, 4, 5 };
  Real viscmin(1), viscmax(2), viscref(3);
  Real dmudc[3] = { 4, 5, 6 };
  Real cmuref[3] = { 5, 6, 7 };
  Real amucoeff[5] = { 6, 7, 8, 9, 10 };

  pVSC.SetField("MT3DMUFLG", &mt3dmuflg);
  pVSC.SetField("NSMUEOS", &nsmueos);
  pVSC.SetField("MUTEMPOPT", &mutempopt);
  pVSC.SetField("MTMUSPEC", &mtmuspec[0]);
  pVSC.SetField("MTMUTEMPSPEC", &mtmutempspec);
  pVSC.SetField("VISCMIN", &viscmin);
  pVSC.SetField("VISCMAX", &viscmax);
  pVSC.SetField("VISCREF", &viscref);
  pVSC.SetField("DMUDC", &dmudc[0]);
  pVSC.SetField("CMUREF", &cmuref[0]);

  CStr expected, output;

  // all fields need to be set
  {
    TxtExporter t(TESTBASE);
    expVSCLine3(&pVSC, &t);
    t.GetFileContents(Packages::VSC, output);
    TS_ASSERT(output.empty());
  }

  pVSC.SetField("AMUCOEFF", &amucoeff[0]);

  // write line 3 (mt3dmuflg >= 0)
  {
    TxtExporter t(TESTBASE);
    expVSCLine3(&pVSC, &t);
    expected = "0\n"
               "1.0 2.0\n"
               "3.0 4.0 5.0\n";
    t.GetFileContents(Packages::VSC, output);
    TS_ASSERT_EQUALS2(expected, output);
  }

  // write line 3a-3c (mt3dmuflg == -1)
  // don't write line 3d (mutempopt <= 0)
  {
    TxtExporter t(TESTBASE);
    mt3dmuflg = -1;
    expVSCLine3(&pVSC, &t);
    expected = "-1\n"
               "1.0 2.0\n"
               "3.0\n"
               "3 0\n"
               "3 4.0 5.0\n"
               "4 5.0 6.0\n"
               "5 6.0 7.0\n";
    t.GetFileContents(Packages::VSC, output);
    TS_ASSERT_EQUALS2(expected, output);
  }

  // write line 3a-3c (mt3dmuflg == -1)
  // write 4 items to line 3d (mutempopt == 1)
  {
    TxtExporter t(TESTBASE);
    mutempopt = 1;
    expVSCLine3(&pVSC, &t);
    expected = "-1\n"
               "1.0 2.0\n"
               "3.0\n"
               "3 1\n"
               "3 4.0 5.0\n"
               "4 5.0 6.0\n"
               "5 6.0 7.0\n"
               "1 6.0 7.0 8.0 9.0\n";
    t.GetFileContents(Packages::VSC, output);
    TS_ASSERT_EQUALS2(expected, output);
  }

  // write line 3a-3c (mt3dmuflg == -1)
  // write 5 items to line 3d (mutempopt == 2)
  {
    TxtExporter t(TESTBASE);
    mutempopt = 2;
    expVSCLine3(&pVSC, &t);
    expected = "-1\n"
               "1.0 2.0\n"
               "3.0\n"
               "3 2\n"
               "3 4.0 5.0\n"
               "4 5.0 6.0\n"
               "5 6.0 7.0\n"
               "1 6.0 7.0 8.0 9.0 10.0\n";
    t.GetFileContents(Packages::VSC, output);
    TS_ASSERT_EQUALS2(expected, output);
  }

  // write line 3a-3c (mt3dmuflg == -1)
  // write 2 items to line 3d (mutempopt == 3)
  {
    TxtExporter t(TESTBASE);
    mutempopt = 3;
    expVSCLine3(&pVSC, &t);
    expected = "-1\n"
               "1.0 2.0\n"
               "3.0\n"
               "3 3\n"
               "3 4.0 5.0\n"
               "4 5.0 6.0\n"
               "5 6.0 7.0\n"
               "1 6.0 7.0\n";
    t.GetFileContents(Packages::VSC, output);
    TS_ASSERT_EQUALS2(expected, output);
  }
} // ExpGmsH5T::testexpVSCLine3
static void iFillMap_VSC_Test (TxtExporter* a_exp)
{
  std::map<CStr, CStr>& m = GetTxtLineArrayMap(a_exp);
  m.clear();
  m["VSC/07. Property1_0"] = "VISC_LAYER1_1";
  m["VSC/07. Property1_1"] = "VISC_LAYER1_2";
  m["VSC/07. Property1_2"] = "VISC_LAYER1_3";
  m["VSC/07. Property2_0"] = "VISC_LAYER2_1";
  m["VSC/07. Property2_1"] = "VISC_LAYER2_2";
  m["VSC/07. Property2_2"] = "VISC_LAYER2_3";
  m["VSC/07. Property3_0"] = "VISC_LAYER3_1";
  m["VSC/07. Property3_1"] = "VISC_LAYER3_2";
  m["VSC/07. Property3_2"] = "VISC_LAYER3_3";
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpVSCStressPeriod ()
{
  using namespace MfData::Packages;
  MfData::MfPackage pVSC(Packages::VSCLine3);
  MfData::MfPackage pVSCSp(Packages::VSCStressPeriod);
  int invisc(1), mt3dmuflg(1);

  CStr expected, output;
  CStr f;
  util::GetTempDirectory(f);
  f += "\\vsctest";

  CStr f1(f+".vsc"), f2(f+".h5");
  CreateDefaultMfH5File(f, MfData::SEAWAT);

  pVSC.SetField("MT3DMUFLG", &mt3dmuflg);

  // all fields need to be set
  {
    TxtExporter t(f);
    expVSCStressPeriod(&pVSC, &pVSCSp, 1, 5, 4, 3, &t);
    t.GetFileContents(Packages::VSC, output);
    TS_ASSERT(output.empty());
  }

  pVSCSp.SetField("INVISC", &invisc);

  // don't write when mt3dmuflg != 0
  {
    TxtExporter t(f);
    iFillMap_VSC_Test(&t);
    expVSCStressPeriod(&pVSC, &pVSCSp, 1, 5, 4, 3, &t);
    t.GetFileContents(Packages::VSC, output);
    TS_ASSERT(output.empty());
  }

  // write everything when INVISC > 0 && mt3dmuflg == 0
  {
    TxtExporter t(f);
    iFillMap_VSC_Test(&t);
    mt3dmuflg = 0;
    expVSCStressPeriod(&pVSC, &pVSCSp, 1, 5, 4, 3, &t);
    expected = "1\n"
               "VISC_LAYER1_1\n"
               "VISC_LAYER1_2\n"
               "VISC_LAYER1_3\n";
    t.GetFileContents(Packages::VSC, output);
    TS_ASSERT_EQUALS2(expected, output);
  }

  // don't write VISC when INVISC <= 0
  {
    TxtExporter t(f);
    iFillMap_VSC_Test(&t);
    invisc = 0;
    expVSCStressPeriod(&pVSC, &pVSCSp, 2, 5, 4, 3, &t);
    expected = "0\n";
    t.GetFileContents(Packages::VSC, output);
    TS_ASSERT_EQUALS2(expected, output);
  }

  // check use last
  {
    TxtExporter t(f);
    iFillMap_VSC_Test(&t);
    invisc = -1;
    expVSCStressPeriod(&pVSC, &pVSCSp, 3, 5, 4, 3, &t);
    H5DataReader::CloseAllH5FilesOpenForWriting();
    expected = "-1\n";
    t.GetFileContents(Packages::VSC, output);
    TS_ASSERT_EQUALS2(expected, output);

    int expectedUseLast[3] = {1, 0, -1};
    TS_ASSERT_H5_ARRAY(f2, "VSC/01. Use Last", expectedUseLast, 3);
  }

  H5Reader::CloseAllH5Files();
  TS_ASSERT(!remove(f1));
  TS_ASSERT(!remove(f2));
} // ExpGmsH5T::testexpVSCStressPeriod
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpGAG ()
{
  using namespace Packages;
  MfPackage p(Packages::GAGE);
  TxtExporter t(TESTBASE);
  int numgage(4);
  int igglst[] =
    { 1, 2, 3, 4,
      -1, 2, 3, 4,
      -5, 6, 7, 8,
      9, 10, 11, 12 };

  p.SetField("IGGLST", igglst);

  expGAG(&p, &t);

  CStr output;
  t.GetFileContents(Packages::GAGE, output);
  TS_ASSERT(output.empty());

  p.SetField("NUMGAGE", &numgage);

  expGAG(&p, &t);

  t.GetFileContents(Packages::GAGE, output);
  CStr expected =
    "4\n"
    "1 2 3 4\n"
    "-1 3 4\n"
    "-5 7 8\n"
    "9 10 11 12\n";
  TS_ASSERT_EQUALS2(expected, output);
} // ExpGmsH5T::testexpGAG
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpLGR_1 ()
{
  using namespace Packages;
  MfPackage p(Packages::LGR_1);
  TxtExporter t(TESTBASE);
  int NGRIDS(3),IUPBHSV(2),IUPBFSV(0);
  p.SetField("NGRIDS", &NGRIDS);
  p.SetField("IUPBHSV", &IUPBHSV);
  p.SetField("IUPBFSV", &IUPBFSV);

  expLGR_1(&p, &t);

  std::vector<CStr>& lines = iLgrPack().StringsToWrite();
  TS_ASSERT_EQUALS(5, lines.size());
  TS_ASSERT("LGR" == lines[0]);
  TS_ASSERT("3" == lines[1]);
  CStr nm; nm.Format("%s.mfn", TESTBASE);
  TS_ASSERT(nm == lines[2]);
  TS_ASSERT("PARENTONLY" == lines[3]);
  TS_ASSERT("2 0" == lines[4]);
  lines.clear();
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpLGR_2 ()
{
  using namespace Packages;
  MfPackage p(Packages::LGR_2);
  TxtExporter t(TESTBASE);
  int  ISHFLG(10),IBFLG(11),IUCBHSV(12),IUCBFSV(13),MXLGRITER(14),IOUTLGR(15),
       NPLBEG(16),NPRBEG(17),NPCBEG(18),NPLEND(19),NPREND(20),NPCEND(21),
       NCPP(3),NCPPL[4] = {30,40,50,60};
  Real RELAXH((Real).1),RELAXF((Real).2),HCLOSELGR((Real).3),FCLOSELGR((Real).4);
  p.SetField("ISHFLG",&ISHFLG);
  p.SetField("IBFLG",&IBFLG);
  p.SetField("IUCBHSV",&IUCBHSV);
  p.SetField("IUCBFSV",&IUCBFSV);
  p.SetField("MXLGRITER",&MXLGRITER);
  p.SetField("IOUTLGR",&IOUTLGR);
  p.SetField("NPLBEG",&NPLBEG);
  p.SetField("NPRBEG",&NPRBEG);
  p.SetField("NPCBEG",&NPCBEG);
  p.SetField("NPLEND",&NPLEND);
  p.SetField("NPREND",&NPREND);
  p.SetField("NPCEND",&NPCEND);
  p.SetField("NCPP",&NCPP);
  p.SetField("NCPPL",NCPPL);
  p.SetField("RELAXH",&RELAXH);
  p.SetField("RELAXF",&RELAXF);
  p.SetField("HCLOSELGR",&HCLOSELGR);
  p.SetField("FCLOSELGR",&FCLOSELGR);

  expLGR_2(&p, &t);

  CStr ln;
  std::vector<CStr>& lines = iLgrPack().StringsToWrite();
  TS_ASSERT_EQUALS(10, lines.size());
  ln.Format("%s.mfn", TESTBASE);
  TS_ASSERT(ln == lines[0]);
  TS_ASSERT("CHILDONLY" == lines[1]);
  TS_ASSERT("10 11 12 13" == lines[2]);
  TS_ASSERT("14 15" == lines[3]);
  ln.Format("%s %s", STR(RELAXH), STR(RELAXF));
  TS_ASSERT(ln == lines[4]);
  ln.Format("%s %s", STR(HCLOSELGR), STR(FCLOSELGR));
  TS_ASSERT(ln == lines[5]);
  TS_ASSERT("16 17 18" == lines[6]);
  TS_ASSERT("19 20 21" == lines[7]);
  TS_ASSERT("3" == lines[8]);
  TS_ASSERT("30 40 50 60 " == lines[9]);
  lines.clear();
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpUseLastAreal ()
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
  H5DataReader::CloseAllH5FilesOpenForWriting();

  {
    std::vector<int> v;
    std::pair<int, int> p(0,1);
    VEC_INT_PAIR indices(2,p);
    indices[0].second = 2;
    H5DataSetReader r(f1, path1, indices);
    r.GetAllData(v);
    TS_ASSERT(v.size() == dat.size());
    H5Reader::CloseAllH5Files();
    TS_ASSERT(!remove(f1));
  }

  CreateDefaultMfH5File(f);
  expUseLastAreal(f, path, 1, dat);
  dat[0] = dat[1] = 1;
  expUseLastAreal(f, path, 2, dat);
  H5DataReader::CloseAllH5FilesOpenForWriting();
  {
    std::vector<int> v;
    std::pair<int, int> p(0,2);
    VEC_INT_PAIR indices(2,p);
    H5DataSetReader r(f1, path1, indices);
    r.GetAllData(v);
    TS_ASSERT(v.size() == 4);
    TS_ASSERT_EQUALS(v[0], 0);
    TS_ASSERT_EQUALS(v[1], 1);
    TS_ASSERT_EQUALS(v[2], 0);
    TS_ASSERT_EQUALS(v[3], 1);
    H5Reader::CloseAllH5Files();
  }

  TS_ASSERT(!remove(f1));
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
void ExpGmsH5T::testexpMultFile ()
{
  CStr f, output;
  util::GetTempDirectory(f);
  f += "\\mtlfile";
  {
    TxtExporter t(f);
    expMultFile(&t);

    t.GetFileContents(Packages::MLT, output);
    TS_ASSERT(output == "0\n");
  }

  {
    TxtExporter t(f);
    MultArrayMap(&t)["mM1"] = "";
    GetArrayOrderVector(&t).push_back("mM1");
    MultArrayMap(&t)["mM2"] = "";
    GetArrayOrderVector(&t).push_back("mM2");
    MultArrayMap(&t)["mMFunc"] = "M1 - M2";
    GetArrayOrderVector(&t).push_back("mMFunc");
    GetTxtLineArrayMap(&t)["mM11"] ="HDF5 1.0 -1 \"out.h5\" \"Arrays/mM1\" 1 0 2940";
    GetTxtLineArrayMap(&t)["mM21"] ="HDF5 1.0 -1 \"out.h5\" \"Arrays/mM2\" 1 0 2940";

    expMultFile(&t);

    t.GetFileContents(Packages::MLT, output);
    TS_ASSERT(output == "3\n"
                        "M1\n"
                        "HDF5 1.0 -1 \"out.h5\" \"Arrays/mM1\" 1 0 2940\n"
                        "M2\n"
                        "HDF5 1.0 -1 \"out.h5\" \"Arrays/mM2\" 1 0 2940\n"
                        "MFunc FUNCTION\n"
                        "M1 - M2\n"
                        );
  }
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpZoneFile ()
{
  CStr f, output;
  util::GetTempDirectory(f);
  f += "\\zonefile";
  {
    TxtExporter t(f);
    expZoneFile(&t);

    t.GetFileContents(Packages::ZON, output);
    TS_ASSERT(output == "0\n");
  }

  {
    TxtExporter t(f);
    ZoneArrayMap(&t)["zKHLAY1"] = "";
    ZoneArrayMap(&t)["zRCHLAY1"] = "";
    GetTxtLineArrayMap(&t)["zKHLAY11"] ="HDF5 1.0 -1 \"out.h5\" \"Arrays/zKHLAY1\" 1 0 2940";
    GetTxtLineArrayMap(&t)["zRCHLAY11"] ="HDF5 1.0 -1 \"out.h5\" \"Arrays/zRCHLAY1\" 1 0 2940";

    expZoneFile(&t);

    t.GetFileContents(Packages::ZON, output);
    TS_ASSERT(output == "2\n"
                        "KHLAY1\n"
                        "HDF5 1.0 -1 \"out.h5\" \"Arrays/zKHLAY1\" 1 0 2940\n"
                        "RCHLAY1\n"
                        "HDF5 1.0 -1 \"out.h5\" \"Arrays/zRCHLAY1\" 1 0 2940\n"
             );
  }
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testexpListParameterData ()
{
  CStr f;
  util::GetTempDirectory(f);
  f += "\\array";
  CStr f2(f+".h5");

  Param p("DRN1", -5, "DRN", 6, 1, 20);
  ParamList *list;
  Parameters::GetParameterList(&list);
  list->Clear();
  list->PushBack(&p);

  int nDrn(3), nFields(8), nAux(3), itmp(3), start(5);
  Real d[56] = {1,3,5,17,4,6,1,1,
                0,0,0,00,0,0,0,0,
                0,0,0,00,0,0,0,0,
                0,0,0,00,0,0,0,0,
                1,2,17,19,100,6,1,1,
                1,2,18,19,100,6,1,1,
                1,2,19,19,100,6,1,1};
  char c[80] = {73,70,65,67,69,32,32,32,32,32,32,32,32,32,32,32,
                67,79,78,68,70,65,67,84,32,32,32,32,32,32,32,32,
                67,69,76,76,71,82,80,32,32,32,32,32,32,32,32,32,
                00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
                00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00};
  const char *ptype = "DRN", *pname="DRN1";

  MfData::MfPackage pack(MfData::Packages::DRN);
  pack.SetField(MfData::Packages::ListPack::ITMP, &itmp);
  pack.SetField(MfData::Packages::ListPack::MAXBC, &nDrn);
  pack.SetField(MfData::Packages::ListPack::NUMBC, &nDrn);
  pack.SetField(MfData::Packages::ListPack::NUMFIELDS, &nFields);
  pack.SetField(MfData::Packages::ListPack::DATA, &d[0]);
  pack.SetField(MfData::Packages::ListPack::NAUX, &nAux);
  pack.SetField(MfData::Packages::ListPack::AUX, c);
  pack.SetField(MfData::Packages::ListParameter::PTYPE, ptype);
  pack.SetField(MfData::Packages::ListParameter::PNAME, pname);
  pack.SetField(MfData::Packages::ListParameter::START, &start);

  CStr cStr, str, f1(f+".drn");
  CreateDefaultMfH5File(f);
  {
    TxtExporter t(f);
    expListParameterData(&pack, 1, 10, 20, &t);
    H5DataReader::CloseAllH5FilesOpenForWriting();
    {
      std::vector<int> vI;
      {
        H5DataSetReader r(f2, "Drain/00. Number of BCs");
        r.GetAllData(vI);
        TS_ASSERT(vI.size() == 1);
        TS_ASSERT_EQUALS(vI.at(0), 3);
      }
      {
        H5DataSetReader r(f2, "Drain/02. Cell IDs");
        r.GetAllData(vI);
        TS_ASSERT(vI.size() == 3);
        TS_ASSERT_EQUALS(vI.at(0), 37);
        TS_ASSERT_EQUALS(vI.at(1), 38);
        TS_ASSERT_EQUALS(vI.at(2), 39);
      }
      {
        H5DataSetReader r(f2, "Drain/06. IFACE");
        r.GetAllData(vI);
        TS_ASSERT(vI.size() == 3);
        TS_ASSERT_EQUALS(vI.at(0), 6);
        TS_ASSERT_EQUALS(vI.at(1), 6);
        TS_ASSERT_EQUALS(vI.at(2), 6);
      }
      {
        std::vector<double> dat;
        H5DataSetReader r(f2, "Drain/07. Property");
        r.GetAllData(dat);
        double d1[12] = {19,19,19,-5,-5,-5,100,100,100,0,0,0};
        TS_ASSERT(dat.size() == 12);
        for (size_t q=0; q<dat.size(); q++)
          TS_ASSERT_EQUALS(dat[q], d1[q]);
      }
      H5Reader::CloseAllH5Files();
    }
  }
  TS_ASSERT(!remove(f2));
  list->Clear();
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
//------------------------------------------------------------------------------
void ExpGmsH5T::testiGetLPFParamTypes ()
{
  std::set<CStr> types = MfExportUtil::LpfParamTypes();
  TS_ASSERT(types.size() == 7);
  TS_ASSERT(types.find("hk") != types.end());
  TS_ASSERT(types.find("hani") != types.end());
  TS_ASSERT(types.find("vk") != types.end());
  TS_ASSERT(types.find("vani") != types.end());
  TS_ASSERT(types.find("ss") != types.end());
  TS_ASSERT(types.find("sy") != types.end());
  TS_ASSERT(types.find("vkcb") != types.end());
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testiCountLPFParams ()
{
  int npar(5);

  ParamList *list(0);
  Parameters::GetParameterList(&list);
  list->Clear();

  npar = MfExportUtil::CountLpfParams(list, true);
  TS_ASSERT_EQUALS(npar, 0);

  {
    Param p("p1", -1, "hk");
    p.m_clust.push_back(PClust("mult", "zone", 1));
    list->PushBack(&p);
  }
  {
    Param p("p2", -2, "hani");
    p.m_clust.push_back(PClust("mult", "zone", 1));
    list->PushBack(&p);
  }
  {
    Param p("p3", -3, "vk");
    p.m_clust.push_back(PClust("mult", "zone", 1));
    list->PushBack(&p);
  }
  {
    Param p("p4", -4, "vani");
    p.m_clust.push_back(PClust("mult", "zone", 1));
    list->PushBack(&p);
  }
  {
    Param p("p5", -5, "ss");
    p.m_clust.push_back(PClust("mult", "zone", 1));
    list->PushBack(&p);
  }
  {
    Param p("p6", -6, "sy");
    p.m_clust.push_back(PClust("mult", "zone", 1));
    list->PushBack(&p);
  }
  {
    Param p("p6", -6, "vkcb");
    p.m_clust.push_back(PClust("mult", "zone", 1));
    list->PushBack(&p);
  }
  {
    Param p("p7", -7, "hk");
    p.m_clust.push_back(PClust("mult", "zone", 1));
    list->PushBack(&p);
  }
  npar = MfExportUtil::CountLpfParams(list, true);
  TS_ASSERT_EQUALS(npar, 7);

  list->Clear();
}
//------------------------------------------------------------------------------
static void iTestMakePar (ParamList *list)
{
  {
    PClust pc("m1", "z1", 1);
    pc.m_hgu = "h1";
    pc.m_iz.push_back(2);
    pc.m_iz.push_back(3);
    Param p("p1", -1, "hk", 5);
    pc.m_hgu = "h2";
    p.m_clust.push_back(pc);
    list->PushBack(&p);
  }
  {
    PClust pc("m4", "z4", 1);
    pc.m_hgu = "h3";
    pc.m_iz.push_back(5);
    Param p("p3", -3, "hk", 6);
    p.m_clust.push_back(pc);
    pc.m_hgu = "h3";
    pc.m_lay = 2;
    pc.m_mlt = "m2";
    p.m_clust.push_back(pc);
    list->PushBack(&p);
  }
  {
    PClust pc("m3", "z2", 3);
    pc.m_iz.push_back(5);
    pc.m_hgu = "h3";
    Param p("p4", -4, "ss", 6);
    p.m_clust.push_back(pc);
    pc.m_hgu = "h4";
    pc.m_lay = 2;
    pc.m_mlt = "m2";
    pc.m_iz.push_back(8);
    p.m_clust.push_back(pc);
    list->PushBack(&p);
  }
  {
    PClust pc("m3", "z2", 1);
    pc.m_iz.push_back(5);
    pc.m_hgu = "h3";
    Param p("vk5", -5, "vk", 2);
    p.m_clust.push_back(pc);
    list->PushBack(&p);
  }
  {
    PClust pc("m3", "z2", 2);
    pc.m_hgu = "h3";
    pc.m_iz.push_back(2);
    Param p("hani6", -6, "hani", 3);
    p.m_clust.push_back(pc);
    list->PushBack(&p);
  }
  {
    PClust pc("m3", "z2", 3);
    pc.m_hgu = "h3";
    pc.m_iz.push_back(2);
    Param p("sy7", -7, "sy", 4);
    p.m_clust.push_back(pc);
    list->PushBack(&p);
  }
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testiWriteHufLine1to5 ()
{
  using namespace MfData::Packages;
  MfPackage p(Packages::HUF);

  int IHUFCB(40), NHUF(2), NPHUF(5), IOHUFHEADS(3), IOHUFFLOWS(7),
      LTHUF[4]={1,0,1,0}, LAYWT[4]={0,0,0,0}, IWETIT(10), IHDWET(1);
  Real HDRY(-888), WETFCT(1.5);

  p.SetField(HUFPack::HDRY, &HDRY);
  p.SetField(HUFPack::IHUFCB, &IHUFCB);
  p.SetField(HUFPack::NHUF, &NHUF);
  p.SetField(HUFPack::NPHUF, &NPHUF);
  p.SetField(HUFPack::IOHUFHEADS, &IOHUFHEADS);
  p.SetField(HUFPack::IOHUFFLOWS, &IOHUFFLOWS);
  p.SetField(HUFPack::LTHUF, LTHUF);

  {
    TxtExporter t(TESTBASE);

    TS_ASSERT(!iWriteHufLine1to5(&p, 4, &t));
    CStr base, out;
    t.GetFileContents(HUF, out);
    TS_ASSERT_EQUALS2("", out);
  }

  p.SetField(HUFPack::LAYWT, LAYWT);
  {
    TxtExporter t(TESTBASE);

    TS_ASSERT(iWriteHufLine1to5(&p, 4, &t));

    CStr base, out;
    t.GetFileContents(HUF, out);
    base = "40 -888.0 2 5 3 7\n"
           "1 0 1 0\n"
           "0 0 0 0\n";
    TS_ASSERT_EQUALS2(base, out);
  }

  LAYWT[0] = 1;
  {
    TxtExporter t(TESTBASE);
    TS_ASSERT(!iWriteHufLine1to5(&p, 4, &t));
  }

  p.SetField(HUFPack::WETFCT, &WETFCT);
  p.SetField(HUFPack::IWETIT, &IWETIT);
  p.SetField(HUFPack::IHDWET, &IHDWET);
  {
    TxtExporter t(TESTBASE);
    std::map<CStr,CStr> &m(GetTxtLineArrayMap(&t));
    m["WET1"] = "wetLay1";
    TS_ASSERT(iWriteHufLine1to5(&p, 4, &t));

    CStr base, out;
    t.GetFileContents(HUF, out);
    base = "40 -888.0 2 5 3 7\n"
           "1 0 1 0\n"
           "1 0 0 0\n"
           "1.5 10 1\n"
           "wetLay1\n";
    TS_ASSERT_EQUALS2(base, out);
  }
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testiWriteHGUs ()
{
  using namespace MfData::Packages;
  MfPackage p(Packages::HUF);

  const int NHUF(4);
  const char *HGUNAM = "HGU1      HGU2      HGU3      HGU4      ";
  Real HGUHANI[4] = {0, 1, 0, 0};
  Real HGUVANI[4] = {0, 1, 1, 0};
  p.SetField(HUFPack::NHUF, &NHUF);
  p.SetField(HUFPack::HGUHANI, HGUHANI);
  p.SetField(HUFPack::HGUVANI, HGUVANI);
  {
    TxtExporter t(TESTBASE);

    TS_ASSERT(!iWriteHGUs(&p, &t));
    CStr base, out;
    t.GetFileContents(HUF, out);
    TS_ASSERT_EQUALS2("", out);
  }

  p.SetField(HUFPack::HGUNAM, HGUNAM);
  {
    TxtExporter t(TESTBASE);

    TS_ASSERT(!iWriteHGUs(&p, &t));
    CStr base, out;
    t.GetFileContents(HUF, out);
    TS_ASSERT_EQUALS2("HGU1\n", out);
  }

  {
    TxtExporter t(TESTBASE);
    std::map<CStr,CStr> &m(GetTxtLineArrayMap(&t));
    // put the arrays in a the map
    m["top_HGU11"] = "top1";
    m["top_HGU21"] = "top2";
    m["top_HGU31"] = "top3";
    m["top_HGU41"] = "top4";
    m["thick_HGU11"] = "thick1";
    m["thick_HGU21"] = "thick2";
    m["thick_HGU31"] = "thick3";
    m["thick_HGU41"] = "thick4";

    TS_ASSERT(iWriteHGUs(&p, &t));
    CStr base, out;
    t.GetFileContents(HUF, out);
    base = "HGU1\n"
           "top1\n"
           "thick1\n"
           "HGU2\n"
           "top2\n"
           "thick2\n"
           "HGU3\n"
           "top3\n"
           "thick3\n"
           "HGU4\n"
           "top4\n"
           "thick4\n"
           "HGU1 0.0 0.0\n"
           "HGU2 1.0 1.0\n"
           "HGU3 0.0 1.0\n"
           "HGU4 0.0 0.0\n";
    TS_ASSERT_EQUALS2(base, out);
  }
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testiGetHUFParamTypes ()
{
  std::set<CStr> types = MfExportUtil::HufParamTypes();
  TS_ASSERT(types.size() == 7);
  TS_ASSERT(types.find("hk") != types.end());
  TS_ASSERT(types.find("hani") != types.end());
  TS_ASSERT(types.find("vk") != types.end());
  TS_ASSERT(types.find("vani") != types.end());
  TS_ASSERT(types.find("ss") != types.end());
  TS_ASSERT(types.find("sy") != types.end());
  TS_ASSERT(types.find("sytp") != types.end());
}
//------------------------------------------------------------------------------
void ExpGmsH5T::testiWriteHGUPar ()
{
  ParamList *list;
  Parameters::GetParameterList(&list);
  list->Clear();

  {
    TxtExporter t(TESTBASE);
    iWriteHGUPar(&t);
    CStr base, out;
    t.GetFileContents(Packages::HUF, out);
    TS_ASSERT_EQUALS2(base, out);
  }

  iTestMakePar(list);
  {
    TxtExporter t(TESTBASE);
    iWriteHGUPar(&t);
    CStr base, out;
    t.GetFileContents(Packages::HUF, out);
    base ="p1 hk 5.0 1\n"
          "h2 m1 z1 2 3 \n"
          "p3 hk 6.0 2\n"
          "h3 m4 z4 5 \n"
          "h3 m2 z4 5 \n"
          "p4 ss 6.0 2\n"
          "h3 m3 z2 5 \n"
          "h4 m2 z2 5 8 \n"
          "vk5 vk 2.0 1\n"
          "h3 m3 z2 5 \n"
          "hani6 hani 3.0 1\n"
          "h3 m3 z2 2 \n"
          "sy7 sy 4.0 1\n"
          "h3 m3 z2 2 \n";
    TS_ASSERT_EQUALS2(base, out);
  }

  list->Clear();
}

#endif

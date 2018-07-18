//------------------------------------------------------------------------------
// FILE      ExpGeoDb.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/MfData/MfExport/private/ExpGeoDb.h>
#include <private/MfData/MfExport/private/Gdb.h>

//#include <cmath>  // for lrint, but it's actually in private/util/util.h
//#include <math.h> // for lrint, but it's actually in private/util/util.h
#include <hash_map>
#include <map>
#include <set>
#include <sys/stat.h>

#include <RunTest.h>

#include <private/ListReader/CellIdToIJK.h>
#include <private/MfData/MfGlobal.h>
#include <private/MfData/MfPackageUtil.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackFields.h>
#include <private/MfData/Packages/MfPackStrings.h>
#include <private/MfData/Packages/ObsHd.h>
#include <private/MNWReader.h>
#include <private/Parameters.h>
#include <private/Parameters/Param.h>
#include <private/Parameters/ParamList.h>
#include <private/util/util.h>

using namespace MfData::Export;

static void ExportNameFileFilename(MfData::MfPackage* a_package);
static void ExportNameFile(bool a_write,
                           MfData::MfPackage* a_package);
static bool ExportPilotPointWts(MfData::MfPackage* a_package,
                                int a_nCellsIJ);
static bool ExportParameters();
static bool ExportListParameterData(MfData::MfPackage* a_package,
                                    int a_sp,
                                    int a_nI,
                                    int a_nJ);
static bool ExportMultArrayFunc(MfData::MfPackage* a_package);
static bool ExportOC(MfData::MfPackage* a_package);
static bool ExportFloObs(MfData::MfPackage* a_package,
                         int a_numI,
                         int a_numJ);
static bool ExportHOBPack(MfData::MfPackage* a_package,
                          int a_numI,
                          int a_numJ);
static bool ExportOBSVars(MfData::MfPackage* a_package);
static bool ExportOBSVars_2005(MfData::MfPackage* a_package);
static bool ExportOB1(MfData::MfPackage* a_package);
static bool ExportPES(MfData::MfPackage* a_package);
static bool ExportSEN(MfData::MfPackage* a_package);
static bool ExportHFB(MfData::MfPackage* a_package,
                      int a_sp,
                      int a_numI,
                      int a_numJ);
static bool ExportBAS(MfData::MfPackage* a_package);
static bool ExportBCF(MfData::MfPackage* a_package);
static bool ExportLPF99(MfData::MfPackage* a_package);
static bool ExportLPF(MfData::MfPackage* a_package);
static bool ExportAreal(MfData::MfPackage *a_package,
                        int a_sp);
static bool iExportArray(const char *a_fname,
                         MfData::MfGlobal *a_global,
                         MfData::MfPackage *a_package);
static bool ExportDISFree(MfData::MfGlobal* a_global,
                          MfData::MfPackage* a_package);
static bool ExportDIS(MfData::MfGlobal* a_global,
                      MfData::MfPackage* a_package);
static bool ExportListPack(MfData::MfPackage* a_package,
                           int a_sp,
                           int a_numI,
                           int a_numJ);
static bool ExportSolverDE4Line1(MfData::MfPackage* a_package);
static bool ExportSolverDE4Line2(MfData::MfPackage* a_package);
static bool ExportSolver(MfData::MfPackage* a_package);
static bool ExportMNWVars(MfData::MfPackage* a_package);
static bool ExportMNWStressPeriod(MfData::MfPackage* a_package,
                                  int a_sp,
                                  int a_numI,
                                  int a_numJ);
static void ExportUZFLine1(MfData::MfPackage *a_p);
static void ExportUZFLine8(MfData::MfPackage *a_pLine1,
                           MfData::MfPackage *a_pLine8);
static void ExportUZFStressPeriod(MfData::MfPackage *a_pLine1,
                                  MfData::MfPackage *a_pSP, int a_sp);
static bool ExportUZFCbf(MfData::MfPackage *a_p);
static bool ExportUZFVars(MfData::MfPackage *a_p);
static bool ExportHUF(MfData::MfPackage* a_package, int a_nlay);
static void ExportHUFCbf(MfData::MfPackage* a_package);
static bool ExportHUFVars(MfData::MfPackage* a_package, int a_nlay);
static bool ExportHUFLayers(MfData::MfPackage* a_package, int a_nlay);
static bool iWriteHGUs(MfData::MfPackage *a_p);
static bool ExportSTR(MfData::MfPackage *a_p, const int a_sp, const int a_nRow,
                      const int a_nCol);
static void ExportSTRCbf(MfData::MfPackage* a_package);
static void iWriteSTRtoStressPeriods(MfData::MfPackage* a_package, int a_sp);
static void ExportLAK(MfData::MfPackage *a_p);
static void ExportLAKSP(MfData::MfGlobal *a_g, MfData::MfPackage *a_p,
                        MfData::MfPackage *a_pLAK);
static void iWriteLAKtoStressPeriods(MfData::MfPackage* a_package, int a_sp);
static void ExportSFRLine1(MfData::MfPackage *a_pSFRLine1);
static void ExportSFRLine2(MfData::MfPackage *a_pSFRLine1,
                           MfData::MfPackage *a_pSFRLine2,
                           int a_nRow, int a_nCol);
static void ExportSFRLine5(MfData::MfPackage *a_pSFRLine5, int a_sp);
static void ExportSFRLine6(MfData::MfPackage *a_pSFRLine1,
                           MfData::MfPackage *a_pSFRLine6, int a_sp);
static void ExportGAG(MfData::MfPackage *a_pGAG);
static bool ExportGAGSFR(MfData::MfPackage *a_pGAG);
static bool ExportGAGLAK(MfData::MfPackage *a_pGAG);


static std::map<CStr, std::vector<CStr> > &GetTableMap();
static bool GetArrayTableAndField(const CStr &a_str,
                                  CStr &a_table,
                                  CStr &a_field);
static bool GetArrayMultTableAndField(const CStr &a_str,
                                      CStr &a_table,
                                      CStr &a_field);
static bool TableUsesStressPeriod(const CStr &a_str);
static bool GetQueryString(const CStr &a_table,
                           int a_sp,
                           int a_etsSegId,
                           int a_startCellId,
                           int a_nCells,
                           CStr &a_queryStr);
static std::map<CStr, std::vector<int> > &iGetBcCellIds();
static std::map<CStr, std::vector<int> > &iGetBcIface();
static std::map<CStr, std::vector<int> > &iGetBcCellGrp();
static int iGetBcIndex(const CStr &a_type,
                       int a_cellid,
                       int a_sp,
                       std::vector<int> &a_cellIds,
                       std::vector<int> &a_iface,
                       std::vector<int> &a_cellgrp);
static void iSizeBcDataArray(const CStr &a_type,
                             const int a_maxIdx,
                             CAR_DBL2D &a_data);
static std::map<CStr, stdext::hash_map<int, VEC_INT_PAIR> > &iGetBcIdxMap();

//------------------------------------------------------------------------------
/// \brief File globals
//------------------------------------------------------------------------------
static int& iModflowVersion ()
{
  static int ver(2000);
  return ver;
}
static int& iEtsSegNumber ()
{
  static int m_num(1);
  return m_num;
}
static std::map<CStr, int> &mapHguNamesId ()
{
  static std::map<CStr, int> m_map;
  return m_map;
}
static std::map<CStr, int> &mapMultNamesId ()
{
  static std::map<CStr, int> m_map;
  return m_map;
} // mapMultNamesId
static int &NumberMultArrays ()
{
  static int m_num(0);
  return m_num;
} // NumberMultArrays
static std::map<CStr, int> &mapZoneNamesId ()
{
  static std::map<CStr, int> m_map;
  return m_map;
} // mapZoneNamesId
static int &NumberZoneArrays ()
{
  static int m_num(0);
  return m_num;
} // NumberMultArrays
//------------------------------------------------------------------------------
/// \brief Functions to get zone and multiplier array ids from the array name
//------------------------------------------------------------------------------
static int ZoneIdFromName (const CStr &a_name)
{
  CStr str(a_name);
  str.ToLower();
  if (mapZoneNamesId().find(str) == mapZoneNamesId().end())
  {
    NumberZoneArrays()++;
    mapZoneNamesId()[str] = NumberZoneArrays();
  }
  return (mapZoneNamesId()[str]);
}
static int MultIdFromName (const CStr &a_name)
{
  CStr str(a_name);
  str.ToLower();
  if (mapMultNamesId().find(str) == mapMultNamesId().end())
  {
    NumberMultArrays()++;
    mapMultNamesId()[str] = NumberMultArrays();
  }
  return (mapMultNamesId()[str]);
}
//------------------------------------------------------------------------------
/// \brief Template function to get data from a package class
//------------------------------------------------------------------------------
template<class T>
bool GetDataFromPackage (MfData::MfPackage* a_pack,
                         std::vector<CStr> &a_fields,
                         std::vector<const T*> &a_data)
{
  if (!a_pack || a_fields.empty())
    return false;

  a_data.resize(0);
  bool rval(true);
  size_t i;
  const T *tVal;
  for (i=0; i<a_fields.size() && rval; i++)
  {
    rval = a_pack->GetField(a_fields[i], &tVal);
    a_data.push_back(tVal);
  }
  return rval;
} // GetDataFromPackage
//------------------------------------------------------------------------------
/// \brief Constructor
//------------------------------------------------------------------------------
ExpGeoDbFree::ExpGeoDbFree () : ExpGeoDb()
{
} // ExpGeoDbFree::ExpGeoDbFree
//------------------------------------------------------------------------------
/// \brief Constructor
//------------------------------------------------------------------------------
bool ExpGeoDbFree::ExportPackage (MfData::MfGlobal* a_global,
                                  MfData::MfPackage* a_package)
{
  if (!a_global ||
      !a_package)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }
  // do something with the data
  CStr packName(a_package->PackageName());
  if (MfData::Packages::DIS == packName)
    ExportDISFree(a_global, a_package);
  else if (ARR_BAS_IBND == packName)
  {
    iExportArray(FileName(), a_global, a_package);
  }

  return true;
} // ExpGeoDbFree::ExportPackage
//------------------------------------------------------------------------------
/// \brief Constructor
//------------------------------------------------------------------------------
ExpGeoDbSQLite::ExpGeoDbSQLite () : ExpGeoDb()
{
} // ExpGeoDbSQLite::ExpGeoDbSQLite
//------------------------------------------------------------------------------
/// \brief Virtual function to create an empty GDB
//------------------------------------------------------------------------------
bool ExpGeoDbSQLite::CreateEmptyGDB (const char* const a_path,
                                     const char* const a_fname,
                                     int* a_personal)
{
  if (Db::db()->CreateEmptyGDBSQLite(a_path, a_fname, a_personal) == 0)
    return false;
  return true;
} // ExpGeoDbSQLite::CreateEmptyGDB
//------------------------------------------------------------------------------
/// \brief Virtual function to open a GDB
//------------------------------------------------------------------------------
bool ExpGeoDbSQLite::OpenExistingDB (const char* const a_path,
                                     const char* const a_fname)
{
  Db::db()->SetFileName(a_path, a_fname);
  return Db::db()->OpenExistingSQLiteDb(a_path, a_fname) ? 1 : 0;
} // ExpGeoDbSQLite::OpenExistingDB
//------------------------------------------------------------------------------
/// \brief Constructor
//------------------------------------------------------------------------------
ExpGeoDb::ExpGeoDb () : MfExporterImpl("geoDataBase")
{
} // ExpGeoDb::ExpGeoDb
//------------------------------------------------------------------------------
/// \brief Destructor
//------------------------------------------------------------------------------
ExpGeoDb::~ExpGeoDb ()
{
  Gdb::ShutDown();
} // ExpGeoDb::~ExpGeoDb
//------------------------------------------------------------------------------
/// \brief writes the modflow version to the MDFGlobals table
//------------------------------------------------------------------------------
static void iWriteMfVersion ()
{
  // write the version to the MDFGlobals table
  Db::db()->BeginCache("MDFGlobals", "OID = 1");
  CStr version;
  version.Format("MODFLOW %d", iModflowVersion());
  Db::db()->AddValue("MODFLOW_Version", version.c_str());
  Db::db()->AddValue("DataModelVersion", "1.0");
  Db::db()->FlushToDb();
} // iWriteMfVersion
//------------------------------------------------------------------------------
/// \brief Sets the filename and other intialization stuff
//------------------------------------------------------------------------------
void ExpGeoDb::SetFileName (const char *a_)
{
  MfExporterImpl::SetFileName(a_);

  CStr fileWPath, ext, path, fname;
  int personalGeoDB(0);
  fileWPath = FileName();
  util::StripFileFromFilename(fileWPath, path);
  util::StripPathFromFilename(fileWPath, fname);
  util::StripAllButExtension(fileWPath, ext);

  CStr tabStr(GetTablesStr());
  if (tabStr.length() > 0)
  {
    // make sure the file exists (DB, MDB) or directory exists (File GDB)
    struct stat stFileInfo;
    int nStat;
    nStat = stat(a_, &stFileInfo);
    if (nStat != 0)
    {
      ErrorStack::Get().PutError("The database must exist to import "
                                 "specific packages.");
    }

    OpenExistingDB(path.c_str(), fname.c_str());
    Gdb::SetExportOnlySpecifiedTablesString(tabStr);
    return;
  }

  // create the generic geodb
  if (ext.CompareNoCase("mdb") == 0)
    personalGeoDB = 1;

  struct stat stFileInfo;
  int nStat;
  nStat = stat(a_, &stFileInfo);
  if (nStat == 0)
  {
    OpenExistingDB(path.c_str(), fname.c_str());
    //CStr msg;
    //msg.Format("The file: %s already exists. Aborting.");
    //ErrorStack::Get().PutError(msg);
    //return;
  }
  else if (!CreateEmptyGDB(path.c_str(), fname.c_str(), &personalGeoDB))
  {
    ErrorStack::Get().PutError("Unable to create database.");
  }
  // write the version to the MDFGlobals table
  iWriteMfVersion();
} // ExpGeoDb::SetFileName
//------------------------------------------------------------------------------
/// \brief Virtual function to create an empty GDB
//------------------------------------------------------------------------------
bool ExpGeoDb::CreateEmptyGDB (const char* const a_path,
                               const char* const a_fname,
                               int* a_personal)
{
  if (!Gdb::CheckLicense())
    return false;

  if (Db::db()->CreateEmptyGDB(a_path, a_fname, a_personal) == 0)
    return false;
  return true;
} // ExpGeoDb::CreateEmptyGDB
//------------------------------------------------------------------------------
/// \brief Virtual function to open a GDB
//------------------------------------------------------------------------------
bool ExpGeoDb::OpenExistingDB (const char* const a_path,
                               const char* const a_fname)
{
  if (!Gdb::CheckLicense())
  {
    ErrorStack::Get().PutError("Unable to get license to ArcGIS.");
    return false;
  }
  Db::db()->SetFileName(a_path, a_fname);
  Db::db()->OpenExistingGDB(a_path, a_fname);
  return true;
} // ExpGeoDb::OpenExistingDB
//------------------------------------------------------------------------------
/// \brief Exports the package data.
//------------------------------------------------------------------------------
bool ExpGeoDb::ExportPackage (MfData::MfGlobal* a_global,
                              MfData::MfPackage* a_package)
{
  if (!a_global ||
      !a_package)
  {
    util::NullFuncArg(__FILE__, __LINE__);
    return false;
  }

  CStr packName(a_package->PackageName()), tab, field;

  bool testsRunning(0);
#ifdef CXX_TEST
  if (testCxx::TestsRunning())
  {
    testsRunning = true;
  }
#endif
  if (!testsRunning)
  {
    printf("Writing data for package: %s\n", packName.c_str());
  }
  if (packName == MfData::Packages::DIS)
  {
    // export the name file
    ExportNameFile(true, 0);
    ExportDIS(a_global, a_package);
  }
  else if (MfData::Packages::NAM == packName)
  {
    ExportNameFile(false, a_package);
  }
  else if ("NAM1" == packName)
  {
    ExportNameFileFilename(a_package);
    ExpGeoDbSQLite *exporter = dynamic_cast<ExpGeoDbSQLite*>(this);
    if (exporter)
    { // reopen the db, sending empty strings will open the last db opened
      exporter->OpenExistingDB("", "");
    }
  }
  else if (packName == MfData::Packages::WEL ||
           packName == MfData::Packages::RIV ||
           packName == MfData::Packages::DRN ||
           packName == MfData::Packages::GHB ||
           packName == MfData::Packages::CHD ||
           packName == MfData::Packages::DRT)
  {
    ExportListPack(a_package, a_global->GetCurrentPeriod(),
                   a_global->NumRow(), a_global->NumCol());
  }
  else if (MfData::Packages::LPRM == packName)
  {
    ExportListParameterData(a_package, a_global->GetCurrentPeriod(),
                            a_global->NumRow(), a_global->NumCol());
  }
  else if (MfData::Packages::SIP == packName ||
           MfData::Packages::PCG == packName ||
           MfData::Packages::SOR == packName ||
           MfData::Packages::GMG == packName ||
           MfData::Packages::LMG == packName )
  {
    ExportSolver(a_package);
  }
  else if (MfData::Packages::DE4Line1 == packName)
  {
    ExportSolverDE4Line1(a_package);
  }
  else if (MfData::Packages::DE4Line2 == packName)
  {
    ExportSolverDE4Line2(a_package);
  }
  else if (packName == MfData::Packages::LPF)
  {
    ExportLPF(a_package);
  }
  else if (packName == "L99")
  {
    ExportLPF99(a_package);
  }
  else if (packName == MfData::Packages::BCF)
  {
    ExportBCF(a_package);
  }
  else if (packName == MfData::Packages::BAS)
  {
    ExportBAS(a_package);
  }
  else if (packName == MfData::Packages::EVT ||
           packName == MfData::Packages::RCH ||
           packName == MfData::Packages::ETS)
  {
    ExportAreal(a_package, a_global->GetCurrentPeriod());
  }
  else if (GetArrayTableAndField(packName, tab, field) ||
           packName.Find("MULT. ARRAY:") != -1 ||
           packName.Find("ZONE ARRAY:") != -1)
  {
    iExportArray(FileName(), a_global, a_package);
  }
  else if (MfData::Packages::HFB == packName)
  {
    ExportHFB(a_package, a_global->GetCurrentPeriod(),
              a_global->NumRow(), a_global->NumCol());
  }
  else if (MfData::Packages::SEN == packName)
  {
    ExportSEN(a_package);
  }
  else if (MfData::Packages::PES == packName)
  {
    ExportPES(a_package);
  }
  else if ("OB1" == packName)
  {
    ExportOB1(a_package);
  }
  else if ("OB2" == packName ||
           "OB3" == packName ||
           "OB4" == packName ||
           "OB5" == packName ||
           "OB6" == packName ||
           "OB7" == packName ||
           "OB8" == packName)
  {
    ExportOBSVars(a_package);
  }
  else if ("OV2" == packName ||
           "OV3" == packName ||
           "OV4" == packName ||
           "OV5" == packName ||
           "OV6" == packName ||
           "OV7" == packName)
  {
    ExportOBSVars_2005(a_package);
  }
  else if (MfData::Packages::HOB == packName)
  {
    ExportHOBPack(a_package, a_global->NumRow(),
                  a_global->NumCol());
  }
  else if (MfData::Packages::FOB == packName)
  {
    ExportFloObs(a_package, a_global->NumRow(),
                 a_global->NumCol());
  }
  else if ("OC" == packName ||
           "OCT" == packName)
  {
    ExportOC(a_package);
  }
  else if ("FNC" == packName)
  {
    ExportMultArrayFunc(a_package);
  }
  else if ("PAR" == packName)
  {
    ExportParameters();
  }
  else if (MfData::Packages::PPT == packName)
  {
    ExportPilotPointWts(a_package, a_global->NumCol()*a_global->NumRow());
  }
  else if (MfData::Packages::MNWSetup == packName)
  {
    ExportMNWVars(a_package);
  }
  else if (MfData::Packages::MNWStressPeriod == packName)
  {
    ExportMNWStressPeriod(a_package, a_global->GetCurrentPeriod(),
                          a_global->NumRow(), a_global->NumCol());
  }
  else if (Packages::UZFLine1 == packName)
  {
    ExportUZFLine1(a_package);
  }
  else if (Packages::UZFLine8 == packName)
  {
    ExportUZFLine8(a_global->GetPackage(Packages::UZFLine1),
                   a_global->GetPackage(Packages::UZFLine8));
  }
  else if (Packages::UZFStressPeriod == packName)
  {
    ExportUZFStressPeriod(a_global->GetPackage(Packages::UZFLine1),
                          a_global->GetPackage(Packages::UZFStressPeriod),
                          a_global->GetCurrentPeriod());
  }
  else if (MfData::Packages::HUF == packName)
  {
    ExportHUF(a_package, a_global->NumLay());
  }
  else if (MfData::Packages::STRSP == packName)
  {
    ExportSTR(a_package, a_global->GetCurrentPeriod(), a_global->NumRow(),
              a_global->NumCol());
  }
  else if (Packages::LAK == packName)
  {
    ExportLAK(a_package);
  }
  else if (Packages::LAKSP == packName)
  {
    ExportLAKSP(a_global,
                a_package,
                a_global->GetPackage(Packages::LAK));
  }
  else if (Packages::SFRLine1 == packName)
  {
    ExportSFRLine1(a_global->GetPackage(Packages::SFRLine1));
  }
  else if (Packages::SFRLine2 == packName)
  {
    ExportSFRLine2(a_global->GetPackage(Packages::SFRLine1),
                   a_global->GetPackage(Packages::SFRLine2),
                   a_global->NumRow(), a_global->NumCol());
  }
  else if (Packages::SFRLine5 == packName)
  {
    ExportSFRLine5(a_global->GetPackage(Packages::SFRLine5),
                  a_global->GetCurrentPeriod());
  }
  else if (Packages::SFRLine6 == packName)
  {
    ExportSFRLine6(a_global->GetPackage(Packages::SFRLine1),
                   a_global->GetPackage(Packages::SFRLine6),
                   a_global->GetCurrentPeriod());
  }
  else if (Packages::GAGE == packName)
  {
    ExportGAG(a_global->GetPackage(Packages::GAGE));
  }
  else if ("ZZZ" == packName)
  { // this has the version of modflow stored in it
    MfData::MfPackage* p(a_global->GetPackage("ZZZ"));
    if (p)
    {
      const int* ival;
      if (p->GetField("VERSION", &ival))
      {
        iModflowVersion() = *ival;
      }
    }
  }
  else if ("STP" == packName)
  {
    Db::db()->CreateMissingTables();
    // this is a KLUGE because we can't figure out how to get the last
    // db write to work without trying to do another valid one
    iWriteMfVersion();

    Gdb::ShutDown();
  }

  return true;
} // ExpGeoDb::ExportPackage
//------------------------------------------------------------------------------
/// \brief Exports the name file, file name to the globals table
//------------------------------------------------------------------------------
static void ExportNameFileFilename (MfData::MfPackage* a_package)
{
  using namespace MfData::Packages;
  const char *f(0);
  if (!a_package->GetField(NameFile::FNAME, &f) || !f)
    return;

  // get the current path
  char buff[2048];
  GetCurrentDirectory(2048, buff);
  CStr dir(buff);

  dir += "\\";
  dir += f;
  dir.ToLower();
  Db::db()->BeginCache("MDFGlobals", "OID = 1");
  Db::db()->AddValue("NameFilePath", dir.c_str());
  Db::db()->FlushToDb();
  Gdb::ShutDown();
} // ExportNameFileFileName
//------------------------------------------------------------------------------
/// \brief Exports the name file
//------------------------------------------------------------------------------
static void ExportNameFile (bool a_write,
                            MfData::MfPackage* a_package)
{
  using namespace MfData::Packages;
  static std::vector<CStr> ftype, fname;
  static std::vector<int> niu;

  if (!a_write)
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
  }
  else if (!ftype.empty())
  {
    const int nRow(static_cast<int>(ftype.size()));
    Db::db()->BeginCacheMutipleRows("NameFile", "", 4, nRow);
    try
    {
      for (int i=0; i<nRow; i++)
      {
        Db::db()->AddValue("FileType", ftype.at(i), i);
        Db::db()->AddValue("Nunit", niu.at(i), i);
        Db::db()->AddValue("Fname", fname.at(i), i);
        Db::db()->AddValue("Use", 1, i);
      }
    }
    catch (std::out_of_range&)
    {
      ASSERT(0);
    }
    Db::db()->FlushToDb();
  }
} // ExportNameFile
//------------------------------------------------------------------------------
/// \brief Exports pilot points
//------------------------------------------------------------------------------
static void PPToMultNameTab (int a_scatIdx,
                             int a_nPts,
                             std::vector<int> &a_ids)
{
  a_ids.resize(0);
  a_ids.reserve(a_nPts);
  CStr name;
  int  multid;
  Db::db()->BeginCacheMutipleRows("MultNames", "", 3, a_nPts);
  for (int i=0; i<a_nPts; i++)
  {
    name.Format("sc%dv%d", a_scatIdx, i+1);
    multid = MultIdFromName(name);
    a_ids.push_back(multid);
    multid = MultIdFromName(name);
    Db::db()->AddValue("MultID", multid, i);
    Db::db()->AddValue("MultName", name, i);
    Db::db()->AddValue("ArrayMult", 1.0, i);
  }
  Db::db()->FlushToDb();
} // PPToMultNameTab
static void PPToMultipliersTab (std::vector<int> &a_ids,
                                int a_nWts,
                                int a_nCells,
                                const int *a_idxs,
                                const Real *a_wts)
{
  int totVal(a_nCells*a_nWts), cnt(0), ptidx, i, cellid(0), multId;
  // we have to run through a_idxs and see if we will use all of them
  // any value less than 1 will not be used
  for (i=0; i<totVal; i++)
  {
    if (a_idxs[i] < 1)
      cnt++;
  }
  cnt = totVal - cnt;

  try
  {
    Db::db()->BeginCacheMutipleRows("Multipliers", "", 3, cnt);
    for (i=0; i<totVal; i++)
    {
      if (i%a_nWts == 0)
        cellid++;
      ptidx = a_idxs[i] - 1;
      multId = a_ids.at(ptidx);

      Db::db()->AddValue("MultID", multId, i);
      Db::db()->AddValue("IJ", cellid, i);
      Db::db()->AddValue("RMLT", a_wts[i], i);
    }
    Db::db()->FlushToDb();
  }
  catch (std::out_of_range &)
  {
    ASSERT(0);
  }
} // PPToMultipliersTab
static void ExportPPMultArray (int a_scatIdx,
                               int a_nPts,
                               int a_nWts,
                               int a_nCells,
                               const int *a_idxs,
                               const Real *a_wts)
{
  // add points to the mult name table
  std::vector<int> ids;
  PPToMultNameTab(a_scatIdx, a_nPts, ids);
  // add wts to the multipliers table
  PPToMultipliersTab(ids, a_nWts, a_nCells, a_idxs, a_wts);
} // ExportPPMultArray
bool ExportPilotPointWts (MfData::MfPackage* a_package,
                          int a_nCellsIJ)
{
  using namespace MfData::Packages::PilotPoints;

  ParamList *list(0);
  Parameters::GetParameterList(&list);
  if (!list || list->Size() < 1)
    return false;

  const int  *idx(0), *nPts(0), *nWts(0), *idxs(0);
  const Real *wts(0);

  if (!a_package->GetField(SCIDX, &idx) || !idx ||
      !a_package->GetField(NPTS, &nPts) || !nPts ||
      !a_package->GetField(NWTS, &nWts) || !nWts ||
      !a_package->GetField(IDX, &idxs) || !idxs ||
      !a_package->GetField(WTS, &wts) || !wts)
    return false;

  // find the parameter with this scat index
  int parId(0);
  int i, nPar(static_cast<int>(list->Size()));
  Param p;
  for (i=0; i<nPar && parId < 1; i++)
  {
    list->At(i, &p);
    if (p.m_scatIndex == *idx)
      parId = i+1;
  }

  // write the weights to the multiplier arrays
  ExportPPMultArray(*idx, *nPts, *nWts, a_nCellsIJ, idxs, wts);
  return true;
} // ExportPilotPointWts
//------------------------------------------------------------------------------
/// \brief Exports parameters
//------------------------------------------------------------------------------
static void ExportParamTable (ParamList *a_list)
{
  int nRow(static_cast<int>(a_list->Size()));
  Param par;

  Db::db()->BeginCacheMutipleRows("Params", "", 14, nRow);
  for (int i=0; i<nRow; i++)
  {
    a_list->At(i, &par);
    Db::db()->AddValue("ParamID", i+1, i);
    Db::db()->AddValue("PARNAM", par.m_name, i);
    Db::db()->AddValue("PARTYPE", par.m_type, i);
    Db::db()->AddValue("Parval", par.m_parVal, i);
    Db::db()->AddValue("B", par.m_b, i);
    Db::db()->AddValue("Keyval", par.m_key, i);
    Db::db()->AddValue("BL", par.m_min, i);
    Db::db()->AddValue("BU", par.m_max, i);
    Db::db()->AddValue("BSCAL", par.m_bscal, i);
    Db::db()->AddValue("LN", par.m_logTrans ? 1 : 0, i);
    Db::db()->AddValue("ISENS", par.m_isens, i);
    Db::db()->AddValue("LogInterp", par.m_logInterp, i);
    Db::db()->AddValue("LogMinVal", par.m_logMinVal, i);
    Db::db()->AddValue("Tied", par.m_tied.IsEmpty() ? " " : par.m_tied, i);
  }
  Db::db()->FlushToDb();
} // ExportParamTable
static void GetInstances (ParamList *a_list,
                          std::vector<CStr> &a_iNames,
                          std::vector<int> &a_parIds,
                          std::vector<int> &a_parInstIdSp,
                          std::vector<int> &a_sp)
{
  Param p;
  int cnt(0);
  for (size_t i=0; i<a_list->Size(); i++)
  {
    a_list->At(i, &p);
    if (p.m_instNames.empty() && !p.m_clust.empty())
    {
      cnt++;
      a_iNames.push_back(p.m_name);
      a_parIds.push_back(static_cast<int>(i+1));
    }
    else
    {
      for (size_t j=0; j<p.m_instNames.size(); j++)
      {
        CStr inst(p.m_instNames[j]);
        cnt++;
        a_iNames.push_back(inst);
        a_parIds.push_back(static_cast<int>(i+1));
        inst.ToLower();
        for (size_t k=0; k<p.m_instStress[inst].size(); k++)
        {
          a_parInstIdSp.push_back(cnt);
          a_sp.push_back(p.m_instStress[inst][k]);
        }
      }
    }
  }
} // GetInstances
static void ExportParInstances (std::vector<CStr> &a_iNames,
                                std::vector<int> &a_parIds)
{
  int nInst, i;
  nInst = static_cast<int>(a_iNames.size());
  Db::db()->BeginCacheMutipleRows("ParInstances", "", 3, nInst);
  for (i=0; i<nInst; i++)
  {
    Db::db()->AddValue("ParInstID", i+1, i);
    Db::db()->AddValue("InstName", a_iNames[i], i);
    Db::db()->AddValue("ParamID", a_parIds[i], i);
  }
  Db::db()->FlushToDb();
} // ExportParInstances
static void ExportParInstSp (std::vector<int> &a_parInstIdSp,
                             std::vector<int> &a_sp)
{
  int nInstSp, i;

  nInstSp = static_cast<int>(a_parInstIdSp.size());
  Db::db()->BeginCacheMutipleRows("ParInstSP", "", 2, nInstSp);
  for (i=0; i<nInstSp; i++)
  {
    Db::db()->AddValue("ParInstID", a_parInstIdSp[i], i);
    Db::db()->AddValue("SPID", a_sp[i], i);
  }
  Db::db()->FlushToDb();
} // ExportParInstSp
static void ExportParamInst (ParamList *a_list,
                             std::map<CStr, int> &a_mapINameId)
{
  std::vector<CStr> iNames;
  std::vector<int>  parIds, parInstIdSp, sp;
  GetInstances(a_list, iNames, parIds, parInstIdSp, sp);
  if (iNames.empty())
    return;
  ExportParInstances(iNames, parIds);
  for (size_t i=0; i<iNames.size(); i++)
    a_mapINameId[iNames[i]] = static_cast<int>(i+1);
  if (!parInstIdSp.empty())
    ExportParInstSp(parInstIdSp, sp);
} // ExportParamInstClust
static void GetClusterData (ParamList *a_list,
                            std::map<CStr, int> &a_mapINameId,
                            std::vector<int> &a_instIds,
                            std::vector<int> &a_lay,
                            std::vector<CStr> &a_mName,
                            std::vector<CStr> &a_zName,
                            std::vector<CStr> &a_hName,
                            std::vector< std::vector<int> > &a_iz)
{
  size_t i, j, k, nInst, nClst;
  std::vector<CStr> iNames;
  Param p;

  for (i=0; i<a_list->Size(); i++)
  {
    a_list->At(i, &p);
    if (!p.m_clust.empty()) // this parameter has clusters
    {
      iNames = p.m_instNames;
      if (iNames.empty())
      {
        nInst = 1;
        iNames.push_back(p.m_name);
      }
      else
        nInst = iNames.size();

      nClst = p.m_clust.size();

      // there should not be a remainder here
      if (nClst % nInst)
        ASSERT(0);
      nClst = nClst / nInst;

      for (j=0; j<nInst; j++)
      {
        for (k=0; k<nClst; k++)
        {
          size_t q(j*nClst); // offset for instances RCH, ET
          a_instIds.push_back(a_mapINameId[iNames[j]]);
          a_lay.push_back(p.m_clust[q+k].m_lay);
          a_mName.push_back(p.m_clust[q+k].m_mlt);
          a_zName.push_back(p.m_clust[q+k].m_zon);
          a_hName.push_back(p.m_clust[q+k].m_hgu);
          a_iz.push_back(p.m_clust[q+k].m_iz);
        }
      }
    }
  }
} // GetClusterData
static void ExportClstTab (std::vector<int> &a_instIds,
                           std::vector<int> &a_lay,
                           std::vector<CStr> &a_mName,
                           std::vector<CStr> &a_zName,
                           std::vector<CStr> &a_hName)
{
  int nClst(static_cast<int>(a_instIds.size())), i, mid, zid;
  Db::db()->BeginCacheMutipleRows("Clusters", "", 5, nClst);
  for (i=0; i<nClst; i++)
  {
    Db::db()->AddValue("ClusterID", i+1, i);
    Db::db()->AddValue("ParInstID", a_instIds[i], i);
    Db::db()->AddValue("Layer", a_lay[i], i);
    mid = MultIdFromName(a_mName[i]);
    zid = ZoneIdFromName(a_zName[i]);
    Db::db()->AddValue("MultID", mid, i);
    Db::db()->AddValue("ZoneID", zid, i);
    if (!a_hName[i].IsEmpty())
      Db::db()->AddValue("HGUNAME", a_hName[i], i);
  }
  Db::db()->FlushToDb();
} // ExportClstTab
static void ExportIZTab (std::vector< std::vector<int> > &a_iz)
{
  size_t j;
  int i, row(0), cnt(0), nIds(static_cast<int>(a_iz.size()));
  for (i=0; i<nIds; i++)
  {
    for (j=0; j<a_iz[i].size(); j++)
    {
      if (a_iz[i][j] != 0)
        cnt++;
    }
  }

  Db::db()->BeginCacheMutipleRows("IZ", "", 2, cnt);
  for (i=0; i<nIds; i++)
  {
    for (j=0; j<a_iz[i].size(); j++)
    {
      if (a_iz[i][j] == 0)
        continue;
      Db::db()->AddValue("ClusterID", i+1, row);
      Db::db()->AddValue("IZ", a_iz[i][j], row);
      row++;
    }
  }
  Db::db()->FlushToDb();
} // ExportIZTab
static void ExportClusters (ParamList *a_list,
                            std::map<CStr, int> &a_mapINameId)
{
  std::vector<int> instIds, lay;
  std::vector<CStr> mName, zName, hName;
  std::vector< std::vector<int> > iz;
  GetClusterData(a_list, a_mapINameId, instIds, lay, mName, zName, hName, iz);
  if (instIds.empty())
    return;
  ExportClstTab(instIds, lay, mName, zName, hName);
  ExportIZTab(iz);
} // ExportClusters
static void ExportPilotPointTab (int a_parId,
                                 int a_scatIdx,
                                 const std::vector<double> &a_svals,
                                 const std::vector<int> &a_isens)
{
  CStr name;
  int nRow(static_cast<int>(a_svals.size())), mid;
  Db::db()->BeginCacheMutipleRows("PilotPts", "", 6, nRow);
  for (int i=0; i<nRow; i++)
  {
    name.Format("sc%dv%d", a_scatIdx, i+1);
    Db::db()->AddValue("ParamID", a_parId, i);
    Db::db()->AddValue("Parval", a_svals[i], i);
    Db::db()->AddValue("B", a_svals[i], i);
    Db::db()->AddValue("ISENS", a_isens[i], i);
    Db::db()->AddValue("PointName", name, i);
    mid = MultIdFromName(name);
    Db::db()->AddValue("MultID", mid, i);
    Db::db()->AddValue("SourceID", -1, i);
  }
  Db::db()->FlushToDb();
} // ExportPilotPointTab
static void ExportPilotPoints (ParamList *a_list)
{
  int i, nPar(static_cast<int>(a_list->Size()));
  std::vector<int> isens;
  std::vector<double> svals;
  Param p;

  for (i=0; i<nPar; i++)
  {
    a_list->At(i, &p);
    if (p.m_pilotPoints)
    {
      a_list->GetPilotPtIsens(p.m_scatIndex, isens);
      a_list->GetPilotPtValues(p.m_scatIndex, svals);
      ExportPilotPointTab(i+1, p.m_scatIndex, svals, isens);
    }
  }
} // ExportPilotPoints
static bool ExportParameters ()
{
  ParamList *list(0);
  Parameters::GetParameterList(&list);
  if (!list || list->Size() < 1)
    return false;
  std::map<CStr, int> mapINameId;

  // write the parameters to the Params table
  ExportParamTable(list);
  // write instances
  ExportParamInst(list, mapINameId);
  // write clusters
  ExportClusters(list, mapINameId);
  // write pilot point data
  Parameters::FillArrayParamData();
  ExportPilotPoints(list);
  return true;
} // ExportParameters
static bool CheckFieldIsParameterFactorFieldAndUpdateName (const CStr& a_tab,
                                                           CStr& a_fieldName)
{
  bool rval(false);
  if (a_tab == "RIV" ||
      a_tab == "DRN" ||
      a_tab == "DRT" ||
      a_tab == "GHB")
  {
    if (0 == a_fieldName.CompareNoCase("Condfact"))
    {
      a_fieldName = "Condfact";
      rval = true;
    }
  }
  else if (a_tab == "CHD")
  {
    if (0 == a_fieldName.CompareNoCase("Shdfact"))
    {
      a_fieldName = "Shdfact";
      rval = true;
    }
    else if (0 == a_fieldName.CompareNoCase("Ehdfact"))
    {
      a_fieldName = "Ehdfact";
      rval = true;
    }
  }
  else if (a_tab == "WEL")
  {
    if (0 == a_fieldName.CompareNoCase("Qfact"))
    {
      a_fieldName = "Qfact";
      rval = true;
    }
  }
  return rval;
} // FieldIsParameterFactorField
static void CalculateNumDbFields (const CStr& a_tabName,
                                  int a_nFields,
                                  int a_nAux,
                                  std::vector<CStr>& a_fieldStrings,
                                  const std::map<int, CStr> &a_srcIdx_destField,
                                  int &a_numBcDbFields,
                                  int &a_numDbFields,
                                  int &a_ifaceIdx)
{
  int paramFactorAux(0);
  for (size_t s=0; s<a_fieldStrings.size(); s++)
  {
    if (a_fieldStrings[s] == "IFACE")
      a_ifaceIdx = static_cast<int>(s);
    else if (s >= (size_t)(a_nFields-a_nAux)) // add AUX fields to DB
    {
      if (!CheckFieldIsParameterFactorFieldAndUpdateName(a_tabName, a_fieldStrings[s]))
        Gdb::AddAuxColToTable(a_tabName, a_fieldStrings[s]);
      else
        paramFactorAux++;
    }
  }
  // minus 3 for the IJK 
  a_numBcDbFields = a_nFields - 3;
  a_numDbFields = a_numBcDbFields + 2; // cellid and spid
  a_numDbFields += static_cast<int>(a_srcIdx_destField.size());
  if (!a_srcIdx_destField.empty())
    a_numDbFields -= paramFactorAux; // don't count parameter factor Aux variables twice
} // CalculateNumDbFields
static int CalcListBcCellId (const Real *a_data,
                             CellIdToIJK &a_grid,
                             int a_idx,
                             int a_nFields)
{
  int k, i, j;
  k = static_cast<int>(a_data[(a_idx*a_nFields)+0]);
  i = static_cast<int>(a_data[(a_idx*a_nFields)+1]);
  j = static_cast<int>(a_data[(a_idx*a_nFields)+2]);
  return(a_grid.IdFromIJK(i, j, k));
} // CalcListBcCellId
static bool ExportHFBListParameterData (int a_nBcs,
                                        const Real *a_data,
                                        int a_start,
                                        Real a_key,
                                        int a_sp,
                                        CellIdToIJK &a_grid)
{
  // write the data to the DB
  int                 k, i1, j1, i2, j2, q, cell1, cell2, idx, row;

  Db::db()->BeginCacheMutipleRows("HFB6", "", 5, a_nBcs);
  for (q=a_start; q<(a_nBcs+a_start); q++)
  {
    idx = q*7;
    row = q - a_start;
    k  = static_cast<int>(a_data[idx+0]);
    i1 = static_cast<int>(a_data[idx+1]);
    j1 = static_cast<int>(a_data[idx+2]);
    i2 = static_cast<int>(a_data[idx+3]);
    j2 = static_cast<int>(a_data[idx+4]);
    cell1 = a_grid.IdFromIJK(i1, j1, k);
    cell2 = a_grid.IdFromIJK(i2, j2, k);
    Db::db()->AddValue("IJK1", cell1, row);
    Db::db()->AddValue("IJK2", cell2, row);
    Db::db()->AddValue("SPID", a_sp, row);
    Db::db()->AddValue("Hydchr", a_key, row);
    Db::db()->AddValue("Factor", a_data[idx+5], row);
  }
  Db::db()->FlushToDb();
  return true;
} // ExportHFBListParameterData
static bool ExportListParameterData (MfData::MfPackage* a_package,
                                     int a_sp,
                                     int a_nI,
                                     int a_nJ)
{
  // get which package this is
  CStr               name;
  if (!MfData::Packages::GetPackNameFromParameter(a_package, name))
    return false;

  // get the list BC data
  const int          *nBcs, *nAux, *nDataFields;
  int                 nFields;
  const Real*         data;
  std::vector<CStr>   fieldStrings;
  MfData::Packages::GetBcData(a_package, name, &nBcs, &nFields,
                              &nAux, &data, &nDataFields, fieldStrings);

  // get some info about the parameter
  int                 start;
  Real                key;
  std::map<int, CStr> srcIdx_destField;
  if (!MfData::Packages::GetParamKeyAndDataStart(a_package, key, start) ||
      !MfData::Packages::GetParamSrcDestFields(name, fieldStrings,
                                               srcIdx_destField))
    return false;

  // if it is an HFB parameter we have to do it a little different
  CellIdToIJK         grid(a_nI, a_nJ);
  if (MfData::Packages::HFB == name)
  {
    return (ExportHFBListParameterData(*nBcs, data, start, key, a_sp, grid));
  }

  // calculate how many fields and if we have an iface field
  int                 numBcDbFields(0), numDbFields, ifaceIdx(-1);
  CalculateNumDbFields(name, nFields, *nAux, fieldStrings, srcIdx_destField,
                       numBcDbFields, numDbFields, ifaceIdx);

  // write the data to the DB
  int                 q, c, cellId, iface, idx, row;

  Db::db()->BeginCacheMutipleRows(name, "", numDbFields, *nBcs);
  for (q=start; q<(*nBcs+start); q++)
  {
    idx = q*(*nDataFields);
    row = q - start;
    cellId = CalcListBcCellId(data, grid, q, *nDataFields);
    Db::db()->AddValue("IJK", cellId, row);
    Db::db()->AddValue("SPID", a_sp, row);
    for (c=0; c<numBcDbFields; c++)
    {
      if (srcIdx_destField.find(3+c) != srcIdx_destField.end())
      {
        Db::db()->AddValue(fieldStrings[3+c], key, row);
        Db::db()->AddValue(srcIdx_destField[3+c], data[idx+3+c], row);
      }
      else if (c+3 != ifaceIdx &&
               !CheckFieldIsParameterFactorFieldAndUpdateName(name, fieldStrings[3+c]))
        Db::db()->AddValue(fieldStrings[3+c], data[idx+3+c], row);
    }
    if (ifaceIdx > -1)
    {
      iface = static_cast<int>(data[idx+ifaceIdx]);
      Db::db()->AddValue("IFACE", iface, row);
    }
  }
  Db::db()->FlushToDb();

  return true;
} // ExportListParameterData
//------------------------------------------------------------------------------
/// \brief Exports a multiplier array function
//------------------------------------------------------------------------------
static bool ExportMultArrayFunc (MfData::MfPackage* a_package)
{
  const char *name(0), *func(0);

  if (!a_package->GetField("NAME", &name) || !name ||
      !a_package->GetField("FUNC", &func) || !func)
    return false;

  CStr sname(name);
  int id(MultIdFromName(sname));

  Db::db()->BeginCache("MultNames", "");
  Db::db()->AddValue("MultID", id);
  Db::db()->AddValue("MultName", name);
  Db::db()->AddValue("Function", func);
  Db::db()->FlushToDb();
  return true;
} // ExportMultArrayFunc
//------------------------------------------------------------------------------
/// \brief Exports the output control data
//------------------------------------------------------------------------------
static bool GetOCFields (const char *a_pack,
                         CStr &a_tab,
                         std::vector<CStr> &a_fields)
{
  a_fields.resize(0);
  a_tab = "";

  bool rval(true);
  CStr pack(a_pack);

  if ("OC" == pack)
  {
    a_tab = "OCVars";
    a_fields.push_back("IHEDFM");
    a_fields.push_back("IDDNFM");
    a_fields.push_back("IHEDUN");
    a_fields.push_back("IDDNUN");
    a_fields.push_back("IBDOPT");
    a_fields.push_back("IAUXSV");
  }
  else if ("OCT" == pack)
  {
    a_tab = "OCTS";
    a_fields.push_back("SPID");
    a_fields.push_back("TSNum");
    a_fields.push_back("IHDDFL");
    a_fields.push_back("IBUDFL");
    a_fields.push_back("ICBCFL");
    a_fields.push_back("Hdpr");
    a_fields.push_back("Ddpr");
    a_fields.push_back("Hdsv");
    a_fields.push_back("Ddsv");
  }
  else
  {
    rval = false;
  }
  return rval;
} // GetOCFields
static bool ExportOC (MfData::MfPackage* a_package)
{
  if (!a_package)
    return false;

  std::vector<const int *> vals;
  std::vector<CStr> fields;
  CStr tab;
  if (!GetOCFields(a_package->PackageName(), tab, fields))
    return false;
  if (!GetDataFromPackage(a_package, fields, vals))
    return false;

  Db::db()->BeginCache(tab, "");
  Db::db()->AddValues(fields, vals);
  Db::db()->FlushToDb();
  return true;
} // ExportOC
//------------------------------------------------------------------------------
/// \brief Exports the flow observations
//------------------------------------------------------------------------------
static void ExportFLOB ()
{
  std::vector<Flob>& f(GetFLOB().m_flob);
  int i, nVal(static_cast<int>(f.size()));
  Db::db()->BeginCacheMutipleRows("FLOB", "", 9, nVal);
  for (i=0; i<nVal; i++)
  {
    Db::db()->AddValue("FLOBID", f[i].m_factorId, i);
    Db::db()->AddValue("FLOBType", f[i].m_type.c_str(), i);
    Db::db()->AddValue("OBSNAM", f[i].m_name.c_str(), i);
    Db::db()->AddValue("IREFSP", f[i].m_IREFSP, i);
    Db::db()->AddValue("TOFFSET", f[i].m_TOFFSET, i);
    Db::db()->AddValue("HOBS", f[i].m_HOB, i);
    Db::db()->AddValue("STATISTIC", f[i].m_STAT, i);
    Db::db()->AddValue("STATFLAG", f[i].m_STATFLG, i);
    Db::db()->AddValue("PLOTSYMBOL", f[i].m_PLOT, i);
  }
  Db::db()->FlushToDb();
} // ExportFLOB
static void ExportFLOBFactors (int a_numI,
                               int a_numJ)
{
  int i, cellId, nVal;
  CellIdToIJK grid(a_numI, a_numJ);
  std::vector<FlobFact>& d(GetFLOB().m_fact);
  nVal = static_cast<int>(d.size());
  Db::db()->BeginCacheMutipleRows("FLOBFactors", "", 3, nVal);
  for (i=0; i<nVal; i++)
  {
    Db::db()->AddValue("FLOBID", d[i].m_factorId, i);
    cellId = grid.IdFromIJK(d[i].m_i, d[i].m_j, d[i].m_k);
    Db::db()->AddValue("IJK", cellId, i);
    Db::db()->AddValue("Factor", d[i].m_factor, i);
  }
  Db::db()->FlushToDb();
} // ExportFLOBFactors
static bool ExportFloObs (MfData::MfPackage* a_package,
                          int a_numI,
                          int a_numJ)
{
  if (GetFLOB().m_fact.empty() || GetFLOB().m_flob.empty() ||
      !a_package)
    return false;

  // write the FLOB table
  ExportFLOB();

  // write the FLOBFactors table
  ExportFLOBFactors(a_numI, a_numJ);

  return true;
} // ExportFloObs
//------------------------------------------------------------------------------
/// \brief Exports the some obs data
//------------------------------------------------------------------------------
static void ExportHOB (int &a_numLayRec,
                       int &a_numTimeRec,
                       CellIdToIJK &a_grid)
{
  a_numLayRec = a_numTimeRec = 0;

  std::vector<HdObs>& d(GetHOB());
  int i, cellid;
  // first write the HOB table
  Db::db()->BeginCacheMutipleRows("HOB", "", 6, static_cast<int>(d.size()));
  for (i=0; i<(int)d.size(); i++)
  {
    Db::db()->AddValue("HOBID", i+1, i);
    Db::db()->AddValue("OBSNAM", d[i].m_name.c_str(), i);
    cellid = a_grid.IdFromIJK(d[i].m_row, d[i].m_col, 1);
    Db::db()->AddValue("IJ", cellid, i);
    //Db::db()->AddValue("ROW", d[i].m_row, i);
    //Db::db()->AddValue("COLUMN", d[i].m_col, i);
    Db::db()->AddValue("ROFF", d[i].m_rOff, i);
    Db::db()->AddValue("COFF", d[i].m_cOff, i);
    Db::db()->AddValue("ITT", d[i].m_ITT, i);
    a_numLayRec += (int)d[i].m_vLay.size();
    a_numTimeRec += (int)d[i].m_vTimes.size();
  }
  Db::db()->FlushToDb();
} // ExportHOB
static void ExportHOBLayers (int a_numLayRec)
{
  // write the HOBLayers table
  int i, j, cnt;
  std::vector<HdObs>& d(GetHOB());
  Db::db()->BeginCacheMutipleRows("HOBLayers", "", 3, a_numLayRec);
  for (i=0, cnt=0; i<(int)d.size(); i++)
  {
    for (j=0; j<(int)d[i].m_vLay.size(); j++)
    {
      Db::db()->AddValue("HOBID", i+1, cnt);
      Db::db()->AddValue("Layer", d[i].m_vLay[j].m_lay, cnt);
      Db::db()->AddValue("PR", d[i].m_vLay[j].m_factor, cnt);
      cnt++;
    }
  }
  Db::db()->FlushToDb();
} // ExportHOBLayers
static void ExportHOBTimes (int a_numTimeRec)
{
  int i, j, cnt;
  std::vector<HdObs>& d(GetHOB());
  // write the HOBTimes table
  Db::db()->BeginCacheMutipleRows("HOBTimes", "", 9, a_numTimeRec);
  for (i=0, cnt=0; i<(int)d.size(); i++)
  {
    for (j=0; j<(int)d[i].m_vTimes.size(); j++)
    {
      Db::db()->AddValue("HOBID", i+1, cnt);
      Db::db()->AddValue("OBSNAM", d[i].m_vTimes[j].m_name.c_str(), cnt);
      Db::db()->AddValue("IREFSP", d[i].m_vTimes[j].m_iRefSp, cnt);
      Db::db()->AddValue("TOFFSET", d[i].m_vTimes[j].m_tOff, cnt);
      Db::db()->AddValue("HOBS", d[i].m_vTimes[j].m_hob, cnt);
      Db::db()->AddValue("STATh", d[i].m_vTimes[j].m_statH, cnt);
      Db::db()->AddValue("STATdd", d[i].m_vTimes[j].m_statdd, cnt);
      Db::db()->AddValue("STATFLAG", d[i].m_vTimes[j].m_statFlag, cnt);
      Db::db()->AddValue("PLOTSYMBOL", d[i].m_vTimes[j].m_plot, cnt);
      cnt++;
    }
  }
  Db::db()->FlushToDb();
} // ExportHOBTimes
static bool ExportHOBPack (MfData::MfPackage* a_package,
                           int a_numI,
                           int a_numJ)
{
  if (!a_package || GetHOB().empty())
    return false;

  CellIdToIJK grid(a_numI, a_numJ);

  int numLayRec(0), numTimeRec(0);

  ExportHOB(numLayRec, numTimeRec, grid);
  ExportHOBLayers(numLayRec);
  ExportHOBTimes(numTimeRec);
  return true;
} // ExportHOB
//------------------------------------------------------------------------------
/// \brief Exports the some obs data
//------------------------------------------------------------------------------
static bool GetOBFields (const char *a_pack,
                         std::vector<CStr> &a_fields)
{
  CStr pack(a_pack);
  a_fields.resize(0);
  if ("OB2" == pack)
  {
    a_fields.push_back("TOMULTH");
    a_fields.push_back("EVH");
  }
  else if ("OB3" == pack)
  {
    a_fields.push_back("TOMULTGB");
    a_fields.push_back("EVFGB");
  }
  else if ("OB4" == pack)
  {
    a_fields.push_back("TOMULTDR");
    a_fields.push_back("EVFDR");
  }
  else if ("OB5" == pack)
  {
    a_fields.push_back("TOMULTRV");
    a_fields.push_back("EVFRV");
  }
  else if ("OB6" == pack)
  {
    a_fields.push_back("TOMULTCH");
    a_fields.push_back("EVFCH");
  }
  else if ("OB7" == pack)
  {
    a_fields.push_back("TOMULTST");
    a_fields.push_back("EVFST");
  }
  else if ("OB8" == pack)
  {
    a_fields.push_back("TOMULTDT");
    a_fields.push_back("EVFDT");
  }
  else
  {
    return false;
  }
  return true;
} // GetOBFields
//------------------------------------------------------------------------------
/// \brief Exports the some obs data
//------------------------------------------------------------------------------
static bool GetOBFields_2005 (const char *a_pack,
                              std::vector<CStr> &a_ffields,
                              std::vector<CStr> &a_ifields)
{
  CStr pack(a_pack);
  a_ffields.resize(0);
  a_ifields.resize(0);
  if ("OV2" == pack)
  {
    a_ffields.push_back("TOMULTH");
    a_ffields.push_back("HOBDRY");
    a_ifields.push_back("IUHOBSV");
  }
  else if ("OV3" == pack)
  {
    a_ffields.push_back("TOMULTGB");
    a_ifields.push_back("IUGBOBSV");
  }
  else if ("OV4" == pack)
  {
    a_ffields.push_back("TOMULTDR");
    a_ifields.push_back("IUDROBSV");
  }
  else if ("OV5" == pack)
  {
    a_ffields.push_back("TOMULTRV");
    a_ifields.push_back("IURVOBSV");
  }
  else if ("OV6" == pack)
  {
    a_ffields.push_back("TOMULTCH");
    a_ifields.push_back("IUCHOBSV");
  }
  else if ("OV7" == pack)
  {
    a_ffields.push_back("TOMULTST");
    a_ifields.push_back("IUSTOBSV");
  }
  else
  {
    return false;
  }
  return true;
} // GetOBFields_2005
static bool ExportOBSVars (MfData::MfPackage* a_package)
{
  CStr pack(a_package->PackageName());
  std::vector<CStr> fields;
  std::vector<const Real *> fVals;

  if (!GetOBFields(pack, fields))
    return false;
  if (!GetDataFromPackage(a_package, fields, fVals))
    return false;

  Db::db()->BeginCache("OBSVars", "OID = 1");
  Db::db()->AddValues(fields, fVals);
  Db::db()->FlushToDb();
  return true;
} // ExportOBSVars
static bool ExportOBSVars_2005 (MfData::MfPackage* a_package)
{
  CStr pack(a_package->PackageName());
  std::vector<CStr> ffields;
  std::vector<CStr> ifields;
  std::vector<const Real *> fVals;
  std::vector<const int *> iVals;

  if (!GetOBFields_2005(pack, ffields, ifields))
    return false;
  if (!GetDataFromPackage(a_package, ffields, fVals))
    return false;
  if (!GetDataFromPackage(a_package, ifields, iVals))
    return false;

  Db::db()->BeginCache("OBSVars", "OID = 1");
  Db::db()->AddValues(ffields, fVals);
  Db::db()->AddValues(ifields, iVals);
  Db::db()->FlushToDb();
  return true;
} // ExportOBSVars
//------------------------------------------------------------------------------
/// \brief Exports the some obs data
//------------------------------------------------------------------------------
static bool ExportOB1 (MfData::MfPackage* a_package)
{
  const char *c(0);
  const int *i(0);

  if (!a_package->GetField("OUTNAM", &c) || !c)
    return false;
  if (!a_package->GetField("ISCALS", &i) || !i)
    return false;

  Db::db()->BeginCache("OBSVars", "OID = 1");
  Db::db()->AddValue("OUTNAM", c);
  Db::db()->AddValue("ISCALS", *i);
  Db::db()->FlushToDb();
  return true;
} // ExportOB1
//------------------------------------------------------------------------------
/// \brief Exports the pes package
//------------------------------------------------------------------------------
static void GetPESFields (std::vector<CStr> &a_iFields,
                          std::vector<CStr> &a_fFields)
{
  a_iFields.push_back(MfData::Packages::PESpack::ITMXP);
  a_iFields.push_back(MfData::Packages::PESpack::IBEFLG);
  a_iFields.push_back(MfData::Packages::PESpack::IYCFLG);
  a_iFields.push_back(MfData::Packages::PESpack::IOSTAR);
  a_iFields.push_back(MfData::Packages::PESpack::NOPT);
  a_iFields.push_back(MfData::Packages::PESpack::NFIT);
  a_iFields.push_back(MfData::Packages::PESpack::IAP);
  a_iFields.push_back(MfData::Packages::PESpack::IPRC);
  a_iFields.push_back(MfData::Packages::PESpack::IPRINT);
  a_iFields.push_back(MfData::Packages::PESpack::LPRINT);
  a_iFields.push_back(MfData::Packages::PESpack::LASTX);

  a_fFields.push_back(MfData::Packages::PESpack::DMAX);
  a_fFields.push_back(MfData::Packages::PESpack::RTOL);
  a_fFields.push_back(MfData::Packages::PESpack::SOSC);
  a_fFields.push_back(MfData::Packages::PESpack::SOSR);
  a_fFields.push_back(MfData::Packages::PESpack::RMAR);
  a_fFields.push_back(MfData::Packages::PESpack::RMARM);
  a_fFields.push_back(MfData::Packages::PESpack::CSA);
  a_fFields.push_back(MfData::Packages::PESpack::FCONV);
} // GetPESFields
static bool ExportPES (MfData::MfPackage* a_package)
{
  std::vector<CStr> iFields, fFields;
  std::vector<const int *> iVals;
  std::vector<const Real *> fVals;

  GetPESFields(iFields, fFields);
  if (!GetDataFromPackage(a_package, iFields, iVals) ||
      !GetDataFromPackage(a_package, fFields, fVals))
    return false;

  Db::db()->BeginCache("PES", "");
  Db::db()->AddValues(iFields, iVals);
  Db::db()->AddValues(fFields, fVals);
  Db::db()->FlushToDb();
  return true;
} // ExportPES
//------------------------------------------------------------------------------
/// \brief Exports the sen package
//------------------------------------------------------------------------------
static void GetSENFields (std::vector<CStr> &a_fields)
{
  a_fields.push_back(MfData::Packages::SENpack::ISENSU);
  a_fields.push_back(MfData::Packages::SENpack::IPRINTS);
  a_fields.push_back(MfData::Packages::SENpack::ISENALL);
  a_fields.push_back(MfData::Packages::SENpack::ISENFM);
  a_fields.push_back(MfData::Packages::SENpack::ISENPU);
  a_fields.push_back(MfData::Packages::SENpack::IUHEAD);
} // GetSENFields
static bool ExportSEN (MfData::MfPackage* a_package)
{
  std::vector<const int *> iVals;
  std::vector<CStr>        fields;

  GetSENFields(fields);
  if (!GetDataFromPackage(a_package, fields, iVals))
    return false;

  Db::db()->BeginCache("SEN", "");
  Db::db()->AddValues(fields, iVals);
  Db::db()->FlushToDb();
  return true;
} // ExportSEN
//------------------------------------------------------------------------------
/// \brief Exports the hfb package
//------------------------------------------------------------------------------
static bool ExportHFB (MfData::MfPackage* a_package,
                       int a_sp,
                       int a_numI,
                       int a_numJ)
{
  using namespace MfData::Packages::HFBpack;
  const int *num(0);
  const Real *dat(0);
  int cell1, cell2, lay, row1, col1, row2, col2;
  Real hydc;

  if (!a_package->GetField(NHFBNP, &num) || !num ||
      !a_package->GetField(HFB, &dat) || !dat)
    return false;

  CellIdToIJK grid(a_numI, a_numJ);

  Db::db()->BeginCacheMutipleRows("HFB6", "", 4, *num);
  for (int i=0; i<*num; i++)
  {
    lay = static_cast<int>(dat[(i*7)+0]);
    row1 = static_cast<int>(dat[(i*7)+1]);
    col1 = static_cast<int>(dat[(i*7)+2]);
    row2 = static_cast<int>(dat[(i*7)+3]);
    col2 = static_cast<int>(dat[(i*7)+4]);
    cell1 = grid.IdFromIJK(row1, col1, lay);
    cell2 = grid.IdFromIJK(row2, col2, lay);
    hydc = dat[(i*7)+5];

    Db::db()->AddValue("SPID", a_sp, i);
    Db::db()->AddValue("IJK1", cell1, i);
    Db::db()->AddValue("IJK2", cell2, i);
    Db::db()->AddValue("Hydchr", hydc, i);
    //Db::db()->AddValue("Factor", (Real)1.0, i);
    //Db::db()->AddValue("SourceID", -1, i);
  }
  Db::db()->FlushToDb();
  return true;
} // ExportHFB
//------------------------------------------------------------------------------
/// \brief Exports the bas package
//------------------------------------------------------------------------------
static bool ExportBAS (MfData::MfPackage* a_package)
{
  using namespace MfData::Packages::BASpack;
  const Real *fptr;
  const char *cptr, *h1ptr, *h2ptr;

  // write the IBCFCB flag
  if (!a_package->GetField(HNOFLO, &fptr) || !fptr ||
      !a_package->GetField(OPT, &cptr) || !cptr ||
      !a_package->GetField(HEADNG1, &h1ptr) || !h1ptr ||
      !a_package->GetField(HEADNG2, &h2ptr) || !h2ptr)
    return false;
  Db::db()->BeginCache("BASVars", "");
  Db::db()->AddValue(HNOFLO, *fptr);
  Db::db()->AddValue(OPT, cptr);
  Db::db()->AddValue(HEADNG1, h1ptr);
  Db::db()->AddValue(HEADNG2, h2ptr);
  Db::db()->FlushToDb();
  return true;
} // ExportBAS
//------------------------------------------------------------------------------
/// \brief Exports the bcf package
//------------------------------------------------------------------------------
static void ExportBCFCbf (MfData::MfPackage* a_package)
{
  const int *iptr;
  // write the IBCFCB flag
  if (a_package->GetField("IBCFCB", &iptr) && iptr)
  {
    Db::db()->BeginCache("CBFlags", "OID = 1");
    Db::db()->AddValue("IBCFCB", *iptr);
    Db::db()->FlushToDb();
  }
} // ExportBCFCbf
static bool ExportBCFVars (MfData::MfPackage* a_package)
{
  std::vector<const int *> iVals;
  std::vector<const Real *> rVals;
  std::vector<CStr> iFields, rFields;
  // write the BCF vars
  {
    using namespace MfData::Packages::BCFpack;
    iFields.push_back(IWDFLG);
    iFields.push_back(IWETIT);
    iFields.push_back(IHDWET);
    rFields.push_back(HDRY);
    rFields.push_back(WETFCT);
  }

  if (!GetDataFromPackage(a_package, iFields, iVals)) return false;
  if (!GetDataFromPackage(a_package, rFields, rVals)) return false;

  Db::db()->BeginCache("BCFVars", "");
  Db::db()->AddValues(iFields, iVals);
  Db::db()->AddValues(rFields, rVals);
  Db::db()->FlushToDb();
  return true;
} // ExportBCFVars
static bool ExportBCFLayers (MfData::MfPackage* a_package)
{
  using namespace MfData::Packages;
  // write the BCF layer data stuff
  const int *NLAY,*LAYAVG,*LAYCON;
  const Real *TRPY;
  if (!a_package->GetField(BCFpack::NLAY, &NLAY) || !NLAY ||
      !a_package->GetField(BCFpack::LAYAVG, &LAYAVG) || !LAYAVG ||
      !a_package->GetField(BCFpack::LAYCON, &LAYCON) || !LAYCON ||
      !a_package->GetField(BCFpack::TRPY, &TRPY) || !TRPY)
    return false;
  CStr query;
  for (int i=0; i<*NLAY; i++)
  {
    query.Format("Layer = %d", i+1);
    Db::db()->BeginCache("BCFLayers", (LPCTSTR)query);
    //Db::db()->AddValue("Layer", i+1, i);
    Db::db()->AddValue(BCFpack::LAYAVG, LAYAVG[i]);
    Db::db()->AddValue(BCFpack::LAYCON, LAYCON[i]);
    Db::db()->AddValue(BCFpack::TRPY, TRPY[i]);
    Db::db()->FlushToDb();
  }
  return true;
} // ExportBCFLayers
static bool ExportBCF (MfData::MfPackage* a_package)
{
  if (!a_package)
    return false;

  ExportBCFCbf(a_package);// write the IBCFCB flag
  if(!ExportBCFVars(a_package) ||
     !ExportBCFLayers(a_package))
    return false;

  return true;
} // ExportBCF
//------------------------------------------------------------------------------
/// \brief Exports the wetting factor stuff for lpf package
//------------------------------------------------------------------------------
static bool ExportLPF99 (MfData::MfPackage* a_package)
{
  const int *iptr[2];
  const Real *fptr;

  // write the LPF vars
  if (a_package->GetField("WETFCT", &fptr) &&
      a_package->GetField("IWETIT", &iptr[0]) &&
      a_package->GetField("IHDWET", &iptr[1]) &&
      fptr && iptr[0] && iptr[1])
  {
    Db::db()->BeginCache("LPFVars", "");
    Db::db()->AddValue("WETFCT", *fptr);
    Db::db()->AddValue("IWETIT", *iptr[0]);
    Db::db()->AddValue("IHDWET", *iptr[1]);
    Db::db()->FlushToDb();
  }
  else
    return false;
  return true;
} // ExportLPF99
//------------------------------------------------------------------------------
/// \brief Exports the lpf package
//------------------------------------------------------------------------------
static void ExportLPFCbf (MfData::MfPackage* a_package)
{
  // write the ILPFCB flag
  const int *iptr;
  if (a_package->GetField("ILPFCB", &iptr) && iptr)
  {
    Db::db()->BeginCache("CBFlags", "OID = 1");
    Db::db()->AddValue("ILPFCB", *iptr);
    Db::db()->FlushToDb();
  }
} // ExportLPFCbf
static void ExportLPFVars (MfData::MfPackage* a_package)
{
  using namespace MfData::Packages;
  // write the LPF vars
  const Real *fptr;
  if (a_package->GetField(LPFpack::HDRY, &fptr) && fptr)
  {
    Db::db()->BeginCache("LPFVars", "");
    Db::db()->AddValue(LPFpack::HDRY, *fptr);
    Db::db()->FlushToDb();
  }
} // ExportLPFVars
static void ExportLPFLayers (MfData::MfPackage* a_package)
{
  using namespace MfData::Packages;
  // write the lpf layers data
  const int *NLAY, *LAYTYP, *LAYAVG, *LAYVKA, *LAYWET;
  const Real *CHANI;
  if (a_package->GetField(LPFpack::NLAY, &NLAY) && NLAY &&
      a_package->GetField(LPFpack::LAYTYP, &LAYTYP) && LAYTYP &&
      a_package->GetField(LPFpack::LAYAVG, &LAYAVG) && LAYAVG &&
      a_package->GetField(LPFpack::LAYVKA, &LAYVKA) && LAYVKA &&
      a_package->GetField(LPFpack::LAYWET, &LAYWET) && LAYWET &&
      a_package->GetField(LPFpack::CHANI, &CHANI) && CHANI)
  {
    Db::db()->BeginCacheMutipleRows("LPFLayers", "", 6, *NLAY);
    for (int i=0; i<*NLAY; i++)
    {
      Db::db()->AddValue("Layer", i+1, i);
      Db::db()->AddValue(LPFpack::LAYTYP, LAYTYP[i], i);
      Db::db()->AddValue(LPFpack::LAYAVG, LAYAVG[i], i);
      Db::db()->AddValue(LPFpack::LAYVKA, LAYVKA[i], i);
      Db::db()->AddValue(LPFpack::LAYWET, LAYWET[i], i);
      Db::db()->AddValue(LPFpack::CHANI, CHANI[i], i);
    }
    Db::db()->FlushToDb();
  }
} // ExportLPFLayers
static bool ExportLPF (MfData::MfPackage* a_package)
{
  using namespace MfData::Packages;

  ExportLPFCbf(a_package); // write the ILPFCB flag
  ExportLPFVars(a_package);
  ExportLPFLayers(a_package);
  return true;
} // ExportLPF
//------------------------------------------------------------------------------
/// \brief Exports the areal package flags
//------------------------------------------------------------------------------
static void GetArealFields (MfData::MfPackage *a_package,
                            CStr &a_cbfField,
                            CStr &a_optField,
                            CStr &a_optTable,
                            std::vector<CStr> &a_fields)
{
  a_fields.resize(0);
  a_cbfField = a_optField = a_optTable = "";
  using namespace MfData::Packages;
  if (a_package->PackageName() == MfData::Packages::EVT)
  {
    a_cbfField = "IEVTCB";
    a_optField = EVTpack::NEVTOP;
    a_optTable = "EVTVars";

    a_fields.push_back(EVTpack::INEVTR);
    a_fields.push_back(EVTpack::INEXDP);
    a_fields.push_back(EVTpack::INSURF);
    a_fields.push_back(EVTpack::INIEVT);
  }
  else if (a_package->PackageName() == MfData::Packages::ETS)
  {
    a_cbfField = "IETSCB";
    a_optField = EVTpack::NEVTOP;
    a_optTable = "ETSVars";

    a_fields.push_back(EVTpack::INEVTR);
    a_fields.push_back(EVTpack::INEXDP);
    a_fields.push_back(EVTpack::INSURF);
    a_fields.push_back(EVTpack::INIEVT);
    a_fields.push_back(EVTpack::INSGDF);
  }
  else
  {
    a_cbfField = "IRCHCB";
    a_optField = RCHpack::NRCHOP;
    a_optTable = "RCHVars";

    a_fields.push_back(RCHpack::INRECH);
    a_fields.push_back(RCHpack::INIRCH);
  }
} // GetArealFields
static void ExportArealCBF (MfData::MfPackage *a_package,
                            const CStr &a_field)
{
  const int *iptr(0);
  // write the CBF flag
  if (a_package->GetField(a_field, &iptr) && iptr)
  {
    Db::db()->BeginCache("CBFlags", "OID = 1");
    Db::db()->AddValue(a_field, *iptr);
    Db::db()->FlushToDb();
  }
} // ExportArealCBF
static void ExportArealOption (MfData::MfPackage *a_package,
                               const CStr &a_optField,
                               const CStr &a_optTable,
                               int &optVal)
{
  const int *iptr(0), *netseg(0);
  CStr writeField(a_optField);
  bool bSeg(false);
  if (a_package->PackageName() == MfData::Packages::ETS)
  {
    writeField = "NETSOP";
    bSeg = true;
  }

  if (a_package->GetField(a_optField, &iptr) && iptr)
  {
    Db::db()->BeginCache(a_optTable, "");
    Db::db()->AddValue(writeField, *iptr);
    if (bSeg &&
        a_package->GetField(MfData::Packages::EVTpack::NETSEG, &netseg) &&
        netseg)
      Db::db()->AddValue("NETSEG", *netseg);
    Db::db()->FlushToDb();
    optVal = *iptr;
  }
} // ExportArealOption
static bool ExportArealArrayFlags (MfData::MfPackage *a_package,
                                   int a_sp,
                                   std::vector<CStr> &a_fields)
{
  using namespace MfData::Packages;
  std::vector<const int *> iVals;
  if (!GetDataFromPackage(a_package, a_fields, iVals))
    return false;
  std::vector<CStr> writeFields(a_fields);
  if (a_package->PackageName() == MfData::Packages::ETS)
  {
    for (size_t i = 0; i < a_fields.size(); ++i)
    {
      if (a_fields.at(i).CompareNoCase(EVTpack::INSURF) == 0)
        writeFields.at(i) = "ETS_INETSS";
      else if (a_fields.at(i).CompareNoCase(EVTpack::INEVTR) == 0)
        writeFields.at(i) = "ETS_INETSR";
      else if (a_fields.at(i).CompareNoCase(EVTpack::INEXDP) == 0)
        writeFields.at(i) = "ETS_INETSX";
      else if (a_fields.at(i).CompareNoCase(EVTpack::INIEVT) == 0)
        writeFields.at(i) = "ETS_INIETS";
      else if (a_fields.at(i).CompareNoCase(EVTpack::INSGDF) == 0)
        writeFields.at(i) = "ETS_INSGDF";
    }
  }

  CStr query;
  query.Format("SPID = %d", a_sp);
  Db::db()->BeginCache("StressPeriods", query);
  Db::db()->AddValues(writeFields, iVals);
  Db::db()->FlushToDb();
  return true;
} // ExportArealArrayFlags
static bool ExportAreal (MfData::MfPackage *a_package,
                         int a_sp)
{
  using namespace MfData::Packages;
  CStr cbfField, optField, optTable;
  CStr pack(a_package->PackageName());
  std::vector<CStr> fields;
  static std::map<CStr, int> arrayOpt;

  if (arrayOpt.empty())
  {
    arrayOpt[RCH] = 0;
    arrayOpt[EVT] = 0;
    arrayOpt[ETS] = 0;
  }

  GetArealFields(a_package, cbfField, optField, optTable, fields);

  if (a_sp == 1)
  {
    // write the CBF flag
    ExportArealCBF(a_package, cbfField);
    // write the package option for where RCH or ET should be applied
    ExportArealOption(a_package, optField, optTable, arrayOpt[pack]);
  }

  if (pack.CompareNoCase(MfData::Packages::ETS) == 0)
  {
    const int *netsop(0), *netseg(0);
    if (a_package->GetField(optField, &netsop) && netsop &&
        a_package->GetField(MfData::Packages::EVTpack::NETSEG, &netseg) &&
        netseg)
    {
      // Remove the last two arrays depending on the options
      if (*netseg < 2) // INSGDF
        fields.pop_back();
      if (*netseg < 2 && arrayOpt[pack] != 2) // INIETS (from INIEVT)
        fields.erase(fields.begin() + 3);
    }
  }
  else
  {
    if (arrayOpt[pack] != 2)
      fields.pop_back();     // remove the last array unless they are specifying
                             // the vertical cell
  }

  return (ExportArealArrayFlags(a_package, a_sp, fields));
} // ExportAreal
//------------------------------------------------------------------------------
/// \brief Exports the zone and multiplier arrays
//------------------------------------------------------------------------------
static bool ExportMultZoneName (MfData::MfPackage *a_package,
                                int &a_id,
                                bool &a_mult)
{
  using namespace MfData::Packages;
  CStr name(a_package->PackageName());
  a_mult = false;
  if (name.find("MULT. ARRAY:") != -1)
    a_mult = true;
  // get the name of the array
  name.Replace("MULT. ARRAY:", "");
  name.Replace("ZONE ARRAY:", "");
  name.Trim();

  const Real  *fptr(0);
  const int   *iptr(0);
  CStr         tab, idField, nmField;
  if (a_mult)
  {
    if (!a_package->GetField(Array::MULT, &fptr) || !fptr)
      return false;
    a_id = MultIdFromName(name);
    tab = "MultNames";
    idField = "MultID";
    nmField = "MultName";
  }
  else
  {
    if (!a_package->GetField(Array::MULT, &iptr) || !iptr)
      return false;
    a_id = ZoneIdFromName(name);
    tab = "ZoneNames";
    idField = "ZoneID";
    nmField = "ZoneName";
  }
  Db::db()->BeginCache(tab, "");
  Db::db()->AddValue(idField, a_id);
  Db::db()->AddValue(nmField, name);
  if (fptr)
    Db::db()->AddValue("ArrayMult", *fptr);
  else
    Db::db()->AddValue("ArrayMult", *iptr);
  Db::db()->FlushToDb();
  return true;
} // ExportMultZoneName
template <class T>
static void ExportMultZoneValsT (const T *a_data,
                                 int a_nCells,
                                 int a_id,
                                 const CStr &a_tab,
                                 const CStr &a_idField,
                                 const CStr &a_field)
{
  Db::db()->BeginCacheMutipleRows(a_tab, "", 3, a_nCells);
  for (int i=0; i<a_nCells; i++)
  {
    Db::db()->AddValue("IJ", i+1, i);
    Db::db()->AddValue(a_idField, a_id, i);
    Db::db()->AddValue(a_field, a_data[i], i);
  }
  Db::db()->FlushToDb();
} // ExportMultZoneValsT
static bool ExportMultZoneVals (MfData::MfPackage *a_package,
                                int a_id,
                                bool a_mult,
                                int a_nCells)
{
  using namespace MfData::Packages;
  const Real *fdata(0);
  const int  *idata(0);
  CStr table, idField, field;
  if (a_mult)
  {
    if (!a_package->GetField(Array::ARRAY, &fdata) || !fdata)
      return false;
    table = "Multipliers";
    idField = "MultID";
    field = "RMLT";
  }
  else
  {
    if (!a_package->GetField(Array::ARRAY, &idata) || !idata)
      return false;
    table = "Zones";
    idField = "ZoneID";
    field = "IZON";
  }
  if (fdata)
    ExportMultZoneValsT(fdata, a_nCells, a_id, table, idField, field);
  else
    ExportMultZoneValsT(idata, a_nCells, a_id, table, idField, field);
  return true;
} // ExportMultZoneVals
static bool ExportMultZone (MfData::MfGlobal *a_global,
                            MfData::MfPackage *a_package)
{
  // see if this is a zone or multiplier array
  bool mult;
  int  id;

  if (!ExportMultZoneName(a_package, id, mult))
    return false;

  int nCells(a_global->NumCol()*a_global->NumRow());
  return(ExportMultZoneVals(a_package, id, mult, nCells));
} // ExportMultZone
//------------------------------------------------------------------------------
/// \brief Exports the array data
//------------------------------------------------------------------------------
static bool iExportArray (const char *a_fname,
                          MfData::MfGlobal *a_global,
                          MfData::MfPackage *a_package)
{
  if (!a_global || !a_package)
    return false;

  // THE PACKAGE HERE IS REALLY JUST ARRAY DATA

  // get db table and field based on the name of the array
  CStr name(a_package->PackageName());
  int hguId(-1), etsSegId(-1);
  bool tableUsesHGUID(false);
  bool tableJustIJ(false);

  // do the zone and multipliers a little different
  if (name.find("MULT. ARRAY:") != -1 ||
      name.find("ZONE ARRAY:") != -1)
  {
    ExportMultZone(a_global, a_package);
  }
  else if (a_global->GetPackage(MfData::Packages::BCF))
  {
    if (name.find(ARR_LPF_HK) == 0)
      name = ARR_BCF_HY;
    else if (name.find(ARR_LPF_WET) == 0)
      name = ARR_BCF_WET;
  }
  else if (name.find(ARR_ETS_PXDP) == 0 ||
           name.find(ARR_ETS_PETM) == 0)
  {
    etsSegId = iEtsSegNumber();
    if (name.find(ARR_ETS_PETM) == 0)
    { // increment the segment number for the next time
      iEtsSegNumber() += 1;
    }
  }
  else if (name.find(ARR_UZF_UBND) == 0 ||
           name.find(ARR_UZF_RBND) == 0 ||
           name.find(ARR_UZF_VKS) == 0 ||
           name.find(ARR_UZF_EPS) == 0 ||
           name.find(ARR_UZF_THTS) == 0 ||
           name.find(ARR_UZF_THTI) == 0)
  {
    tableJustIJ = true;
  }
  else if (a_global->GetPackage(MfData::Packages::HUF))
  {
    if (name.find(ARR_LPF_WET) == 0)
      name = "HUF WETDRY PARAMETER";
    else if (name.find("TOP ELEVATN:") != -1 ||
             name.find("THICKNESS:") != -1)
    {
      CStr tmpName(name);
      tmpName.Replace("TOP ELEVATN:", "");
      tmpName.Replace("THICKNESS:", "");
      tmpName.Trim();
      tmpName.ToLower();
      if (mapHguNamesId().find(tmpName) == mapHguNamesId().end())
      {
        mapHguNamesId()[tmpName] = (int)mapHguNamesId().size() + 1;
      }
      hguId = mapHguNamesId()[tmpName];
      tableUsesHGUID = true;
    }
  }

  CStr table, field, cellField;
  cellField = "IJK";
  int  sp(-1);
  if (!GetArrayTableAndField(name, table, field))
    return false;
  if (TableUsesStressPeriod(table))
  {
    sp = a_global->GetCurrentPeriod();
    cellField = "IJ";
  }
  else if (tableUsesHGUID)
  {
    cellField = "IJ";
    sp = hguId;
  }
  else if (tableJustIJ)
  {
    cellField = "IJ";
  }

  CStr queryStr;
  int nCells, startCellId(1);
  const int* lay;
  const Real* data(0);
  a_package->GetField(MfData::Packages::Array::LAYER, &lay);
  a_package->GetField(MfData::Packages::Array::ARRAY, &data);

  int theLay(*lay);
  if (theLay < 1) theLay = 1;

  nCells = a_global->NumCol()*a_global->NumRow();
  if (sp == -1)
    startCellId = ((theLay-1)*nCells) + 1 ;

  if (!GetQueryString(table, sp, etsSegId, startCellId, nCells, queryStr))
    return false;

  if (data)
  {
    Gdb::WriteArray(a_fname, table.c_str(), cellField.c_str(), 
                    field.c_str(), queryStr.c_str(), nCells,
                    startCellId, sp, etsSegId, data);
  }
  else
  {
    const int *idata(0);
    a_package->GetField(MfData::Packages::Array::ARRAY, &idata);
    Gdb::WriteArray(a_fname, table.c_str(), cellField.c_str(),
                    field.c_str(), queryStr.c_str(), nCells,
                    startCellId, sp, idata);
  }

  if (!GetArrayMultTableAndField(name, table, field))
    return false;
  if (!field.IsEmpty())
  {
    if (tableJustIJ)
      queryStr.Format("OID = 1");
    else if (sp == -1)
      queryStr.Format("Layer = %d", theLay);
    Db::db()->BeginCache(table.c_str(), queryStr.c_str());
    if (sp != -1)
    {
      if (tableUsesHGUID)
      {
        Db::db()->AddValue("HGUID", sp);
      }
      else
      {
        Db::db()->AddValue("SPID", sp);
      }
      if (etsSegId > 0)
      {
        Db::db()->AddValue("SEGID", etsSegId);
      }
    }
    else if (!tableJustIJ)
      Db::db()->AddValue("Layer", theLay);
    data = NULL;
    a_package->GetField(MfData::Packages::Array::MULT, &data);
    if (data)
      Db::db()->AddValue(field, *data);
    else
    {
      const int *idata(0);
      a_package->GetField(MfData::Packages::Array::MULT, &idata);
      Db::db()->AddValue(field, *idata);
    }
    Db::db()->FlushToDb();
  }

  return true;
} // iExportArray
//------------------------------------------------------------------------------
/// \brief Exports the DIS package data to the geodatabase.
//------------------------------------------------------------------------------
static void ExportDISVars (MfData::MfGlobal *a_global)
{
  // DISVars
  CStr fields[5] = { "NLAY", "NROW", "NCOL", "ITMUNI", "LENUNI" };
  int iVal[5] = { a_global->NumLay(), a_global->NumRow(), a_global->NumCol(),
                  a_global->TimeUnit(), a_global->LengthUnit() };
  Db::db()->BeginCache("DISVars", "");
  for (int i=0; i<5; i++)
    Db::db()->AddValue(fields[i], iVal[i]);
  Db::db()->FlushToDb();
} // ExportDISVars
static void ExportStressPeriods (MfData::MfPackage* a_package,
                                 int a_nPer)
{
  using namespace MfData::Packages;

  int nPer(a_nPer);
  const Real *perlen, *tsmult;
  const int   *nstps, *issflag;

  if (a_package->GetField(DisPack::PERLEN, &perlen) && perlen &&
      a_package->GetField(DisPack::NSTP, &nstps) && nstps &&
      a_package->GetField(DisPack::TSMULT, &tsmult) && tsmult &&
      a_package->GetField(DisPack::ISSFLG, &issflag) && issflag)
  {
    Db::db()->BeginCacheMutipleRows("StressPeriods", "", 6, nPer);
    for (int i=0; i<nPer; i++)
    {
      Db::db()->AddValue("SPID", i+1, i);
      Db::db()->AddValue("SpNum", i+1, i);
      Db::db()->AddValue(DisPack::PERLEN, perlen[i], i);
      Db::db()->AddValue(DisPack::NSTP, nstps[i], i);
      Db::db()->AddValue(DisPack::TSMULT, tsmult[i], i);
      Db::db()->AddValue(DisPack::ISSFLG, issflag[i] ? "Ss":"Tr", i);
    }
    Db::db()-> FlushToDb();
  }
} // ExportStressPeriods
static void ExportDelRC (MfData::MfPackage* a_package,
                         int a_nI,
                         int a_nJ)
{
  using namespace MfData::Packages;
  // grid spacing and elevations
  int i, nI(a_nI), nJ(a_nJ);
  const Real *delr, *delc;
  if (a_package->GetField(DisPack::DELR, &delr) && delr &&
      a_package->GetField(DisPack::DELC, &delc) && delc)
  {
    Db::db()->BeginCacheMutipleRows("DELRC", "", 3, nI+nJ);
    for (i=0; i<nJ; i++)
    {
      Db::db()->AddValue("Direction", "R", i);
      Db::db()->AddValue("Num", i+1, i);
      Db::db()->AddValue("Width", delr[i], i);
    }
    for (i=0; i<nI; i++)
    {
      Db::db()->AddValue("Direction", "C", nJ+i);
      Db::db()->AddValue("Num", i+1, nJ+i);
      Db::db()->AddValue("Width", delc[i], nJ+i);
    }
    Db::db()-> FlushToDb();
  }
} // ExportDelRC
static void ExportLayCbd (MfData::MfPackage* a_package,
                          int a_nLay)
{
  using namespace MfData::Packages;
  // the laycbd table
  const int *laycbd;
  int nLay(a_nLay);
  if (a_package->GetField(DisPack::LAYCBD, &laycbd) && laycbd)
  {
    CStr query;
    query.Format("Layer > 0 AND Layer < %d", nLay+1);
    Db::db()->BeginCacheMutipleRows("DISLayers", query, 1, nLay);
    for (int j=0; j<nLay; j++)
    {
      Db::db()->AddValue("LAYCBD", laycbd[j], j);
    }
    Db::db()->FlushToDb();
  }
} // ExportLayCbd
static bool ExportDISFree (MfData::MfGlobal* a_global,
                           MfData::MfPackage* a_package)
{
  if (!a_global || !a_package)
    return false;

  ExportDISVars(a_global);
  ExportDelRC(a_package, a_global->NumRow(), a_global->NumCol());
  return true;
}
static bool ExportDIS (MfData::MfGlobal* a_global,
                       MfData::MfPackage* a_package)
{
  if (!ExportDISFree(a_global, a_package))
    return false;
  ExportStressPeriods(a_package, a_global->NumPeriods());
  ExportLayCbd(a_package, a_global->NumLay());
  return true;
} // ExportDis
//------------------------------------------------------------------------------
/// \brief Exports the WEL/RIV/GHB/DRN/CHD package data to the geodatabase.
//------------------------------------------------------------------------------
static void ExportListPackITMP (MfData::MfPackage *a_package,
                                int a_sp,
                                bool &a_usePrev)
{
  CStr str(a_package->PackageName());
  CStr field;
  CStr query;
  const int *flag(0);
  field.Format("%s_ITMP", str);
  query.Format("SPID = %d", a_sp);
  if (a_package->GetField(MfData::Packages::ListPack::ITMP, &flag) && flag)
  {
    Db::db()->BeginCache("StressPeriods", query);
    Db::db()->AddValue(field, *flag);
    Db::db()->FlushToDb();
    if (*flag < 0)
      a_usePrev = true;
  }
} // ExportListPackITMP
static void ExportListPackCBF (MfData::MfPackage *a_package)
{
  CStr str(a_package->PackageName());
  CStr field;
  const int *flag(0);

  field.Format("I%sCB", str);
  if (a_package->GetField(field, &flag) && flag)
  {
    Db::db()->BeginCache("CBFlags", "OID = 1");
    Db::db()->AddValue(field, *flag);
    Db::db()->FlushToDb();
  }
} // ExportListPackCBF
static bool ExportListPackData (MfData::MfPackage* a_package,
                                int a_sp,
                                int a_numI,
                                int a_numJ)
{
  CStr str(a_package->PackageName());
  const int         *nBcs, *nAux, *nDataFields;
  int                nFields;
  const Real*        data;
  int                q, c, numBcDbFields(0), ifaceIdx(-1),
                     numFields, cellId, iface;
  std::vector<CStr>  fieldStrings;
  std::map<int, CStr> srcIdx_destField;
  CellIdToIJK        grid(a_numI, a_numJ);

  MfData::Packages::GetBcData(a_package, a_package->PackageName(), &nBcs,
                              &nFields, &nAux, &data, &nDataFields,
                              fieldStrings);
  // calculate how many fields and if we have an iface field
  CalculateNumDbFields(str, nFields, *nAux, fieldStrings, srcIdx_destField,
                       numBcDbFields, numFields, ifaceIdx);

  Db::db()->BeginCacheMutipleRows(str, "", numFields, *nBcs);
  for (q=0; q<*nBcs; q++)
  {
    cellId = CalcListBcCellId(data, grid, q, *nDataFields);
    Db::db()->AddValue("IJK", cellId, q);
    Db::db()->AddValue("SPID", a_sp, q);
    for (c=0; c<numBcDbFields; c++)
    {
      if (c+3 == ifaceIdx)
      {
        iface = static_cast<int>(data[(q*(*nDataFields))+ifaceIdx]);
        Db::db()->AddValue("IFACE", iface, q);
      }
      else
        Db::db()->AddValue(fieldStrings[3+c], data[(q*(*nDataFields))+3+c], q);
    }
  }
  Db::db()->FlushToDb();
  return true;
} // ExportListPackData
bool ExportListPack (MfData::MfPackage* a_package,
                     int a_sp,
                     int a_numI,
                     int a_numJ)
{
  CStr str(a_package->PackageName());

  bool               usePrev(false);

  ExportListPackITMP(a_package, a_sp, usePrev);
  if (a_sp == 1)
    ExportListPackCBF(a_package);

  if (!usePrev)
    ExportListPackData(a_package, a_sp, a_numI, a_numJ);

  return true;
} // ExportListPack
//------------------------------------------------------------------------------
/// \brief Gets the fields for the passed in solver
//------------------------------------------------------------------------------
static void GetSolverFieldsFromPackage (MfData::MfPackage* a_package,
                                        CStr &a_table,
                                        std::vector<CStr> &a_intFields,
                                        std::vector<CStr> &a_realFields,
                                        std::vector<CStr> &a_dblFields)
{
  a_table = "";
  a_intFields.resize(0);
  a_realFields.resize(0);
  a_dblFields.resize(0);
  if (!a_package)
    return;

  CStr str(a_package->PackageName());
  if (MfData::Packages::SIP == a_package->PackageName())
  {
    using namespace MfData::Packages::SipPack;
    a_intFields.push_back(MXITER);
    a_intFields.push_back(NPARM);
    a_intFields.push_back(IPCALC);
    a_intFields.push_back(IPRSIP);
    a_realFields.push_back(ACCL);
    a_realFields.push_back(HCLOSE);
    a_realFields.push_back(WSEED);
    a_table = "SIP";
  }
  else if (MfData::Packages::SOR == a_package->PackageName())
  {
    using namespace MfData::Packages::SorPack;
    a_intFields.push_back(MXITER);
    a_intFields.push_back(IPRSOR);
    a_realFields.push_back(ACCL);
    a_realFields.push_back(HCLOSE);
    a_table = "SOR";
  }
  else if (MfData::Packages::PCG == a_package->PackageName())
  {
    using namespace MfData::Packages::PcgPack;
    a_intFields.push_back(MXITER);
    a_intFields.push_back(ITER1);
    a_intFields.push_back(NPCOND);
    a_intFields.push_back(NBPOL);
    a_intFields.push_back(IPRPCG);
    a_intFields.push_back(MUTPCG);
    a_realFields.push_back(HCLOSE);
    a_realFields.push_back(RCLOSE);
    a_realFields.push_back(RELAX);
    a_realFields.push_back(DAMP);
    a_table = "PCG";
  }
  else if (MfData::Packages::LMG == a_package->PackageName())
  {
    using namespace MfData::Packages::LmgPack;
    a_intFields.push_back(ICG);
    a_intFields.push_back(MXITER);
    a_intFields.push_back(MXCYC);
    a_intFields.push_back(IOUTAMG);
    a_intFields.push_back(CONTROL);
    a_realFields.push_back(STOR1);
    a_realFields.push_back(STOR2);
    a_realFields.push_back(STOR3);
    a_realFields.push_back(BCLOSE);
    a_realFields.push_back(DAMP);
    a_realFields.push_back(DUP);
    a_realFields.push_back(DLOW);
    a_realFields.push_back(HCLOSE);
    a_table = "LMG";
  }
  else if (MfData::Packages::GMG == a_package->PackageName())
  {
    using namespace MfData::Packages::GmgPack;
    a_intFields.push_back(IITER);
    a_intFields.push_back(MXITER);
    a_intFields.push_back(IADAMP);
    a_intFields.push_back(IOUTGMG);
    a_intFields.push_back(ISM);
    a_intFields.push_back(ISC);
    a_realFields.push_back(RCLOSE);
    a_realFields.push_back(HCLOSE);
    a_realFields.push_back(DAMP);
    a_dblFields.push_back(RELAX);
    a_table = "GMG";
  }
} // GetSolverFieldsFromPackage
//------------------------------------------------------------------------------
/// \brief Exports the first line of DE4 package data to the geodatabase.
//------------------------------------------------------------------------------
static bool ExportSolverDE4Line1 (MfData::MfPackage* a_package)
{
  using namespace MfData::Packages;
  const int *itmx, *mxup, *mxlow, *mxbw;

  if (a_package->GetField(De4Pack::ITMX, &itmx) && itmx &&
      a_package->GetField(De4Pack::MXUP, &mxup) && mxup &&
      a_package->GetField(De4Pack::MXLOW, &mxlow) && mxlow &&
      a_package->GetField(De4Pack::MXBW, &mxbw) && mxbw)
  {
    Db::db()->BeginCache("DE4", "");
    Db::db()->AddValue("ITMX", *itmx);
    Db::db()->AddValue("MXUP", *mxup);
    Db::db()->AddValue("MXLOW", *mxlow);
    Db::db()->AddValue("MXBW", *mxbw);
    Db::db()->FlushToDb();
    return true;
  }
  return false;
} // ExportSolverDE4Line1
//------------------------------------------------------------------------------
/// \brief Exports the second line of DE4 package data to the geodatabase.
//------------------------------------------------------------------------------
static bool ExportSolverDE4Line2 (MfData::MfPackage* a_package)
{
  using namespace MfData::Packages;
  const int *ifreq, *mutd4, *iprd4;
  const Real *accl, *hclose;

  if (a_package->GetField(De4Pack::IFREQ, &ifreq) && ifreq &&
      a_package->GetField(De4Pack::MUTD4, &mutd4) && mutd4 &&
      a_package->GetField(De4Pack::ACCL, &accl) && accl &&
      a_package->GetField(De4Pack::HCLOSE, &hclose) && hclose &&
      a_package->GetField(De4Pack::IPRD4, &iprd4) && iprd4)
  {
    Db::db()->BeginCache("DE4", "OID = 1");
    Db::db()->AddValue("IFREQ", *ifreq);
    Db::db()->AddValue("MUTD4", *mutd4);
    Db::db()->AddValue("ACCL", *accl);
    Db::db()->AddValue("HCLOSE", *hclose);
    Db::db()->AddValue("IPRD4", *iprd4);
    Db::db()->FlushToDb();
    return true;
  }
  return false;
} // ExportSolverDE4Line2
//------------------------------------------------------------------------------
/// \brief Exports the solver package data to the geodatabase.
//------------------------------------------------------------------------------
bool ExportSolver (MfData::MfPackage* a_package)
{
  CStr str(a_package->PackageName()), table;

  std::vector<CStr>    intFields, realFields, dblFields;
  std::vector<const int*>     intData;
  std::vector<const Real*>    realData;
  std::vector<const double*>  dblData;

  GetSolverFieldsFromPackage(a_package, table, intFields, realFields,
                             dblFields);
  if (!GetDataFromPackage(a_package, intFields, intData) ||
      !GetDataFromPackage(a_package, realFields, realData) ||
      (!dblFields.empty() && 
       !GetDataFromPackage(a_package, dblFields, dblData)))
    return false;

  Db::db()->BeginCache(table, "");
  Db::db()->AddValues(intFields, intData);
  Db::db()->AddValues(realFields, realData);
  Db::db()->AddValues(dblFields, dblData);
  Db::db()->FlushToDb();
  return true;
} // ExportSolver
//------------------------------------------------------------------------------
/// \brief Exports the MNWVars table
//------------------------------------------------------------------------------
static bool ExportMNWVars (MfData::MfPackage* a_package)
{
  using namespace MfData::Packages;

  const char *ftag(0), *prefix(0), *names(0);
  const int *iwl2cb(0), *iwelpt(0), *iowell2(0), *kspref(0), *nomoiter(0);
  const double *ploss(0);

  if (a_package->GetField(MNWpack::IWL2CB, &iwl2cb) && iwl2cb &&
      a_package->GetField(MNWpack::IWELPT, &iwelpt) && iwelpt &&
      a_package->GetField(MNWpack::IOWELL2, &iowell2) && iowell2 &&
      a_package->GetField(MNWpack::NOMOITER, &nomoiter) && nomoiter &&
      a_package->GetField(MNWpack::KSPREF, &kspref) && kspref &&
      a_package->GetField(MNWpack::FTAG, &ftag) && ftag &&
      a_package->GetField(MNWpack::PREFIX, &prefix) && prefix &&
      a_package->GetField(MNWpack::NAMES, &names) && names &&
      a_package->GetField(MNWpack::PLoss, &ploss) && ploss)
  {
    Db::db()->BeginCache("CBFlags", "OID = 1");
    Db::db()->AddValue("IMNW1CB", *iwl2cb);
    Db::db()->FlushToDb();

    int i, j;
    char aChar[257];
    CStr sPrefix, fnames[3];
    aChar[256] = '\0';
    for (i=0; i<256; i++)
      aChar[i] = prefix[i];
    sPrefix = aChar;
    sPrefix.Trim();
    for (i=0; i<3; i++)
    {
      for (j=0; j<256; j++)
      {
        aChar[j] = names[i*256+j];
      }
      fnames[i] = aChar;
      fnames[i].Trim();
    }

    Db::db()->BeginCache("MNW1Vars", "");
    Db::db()->AddValue("IWELPT", *iwelpt);
    Db::db()->AddValue("kspref", *kspref);
    Db::db()->AddValue("PLossMNW", *ploss);
    CStr lossType;
    if (*ploss < .99)
      lossType = "SKIN";
    else if (*ploss > 1.001)
      lossType = "NONLINEAR";
    else
      lossType = "LINEAR";
    Db::db()->AddValue("LOSSTYPE", lossType.c_str());

    if (*nomoiter != 9999)
      Db::db()->AddValue("NOMOITER", *nomoiter);
    if (!sPrefix.empty())
      Db::db()->AddValue("PREFIX", sPrefix.c_str());
    if (!fnames[0].empty())
    {
      Db::db()->AddValue("WEL1_FILE", fnames[0].c_str());
      Db::db()->AddValue("iunw1", abs(iowell2[0]));
    }
    if (!fnames[1].empty())
    {
      Db::db()->AddValue("BYNODE_FILE", fnames[1].c_str());
      Db::db()->AddValue("iunby", abs(iowell2[1]));
      int iVal(0);
      if (iowell2[1] < 0)
        iVal = 1;
      Db::db()->AddValue("BYNODE_ALLTIME", iVal);
    }
    if (!fnames[2].empty())
    {
      Db::db()->AddValue("QSUM_FILE", fnames[2].c_str());
      Db::db()->AddValue("iunqs", abs(iowell2[2]));
      int iVal(0);
      if (iowell2[2] < 0)
        iVal = 1;
      Db::db()->AddValue("QSUM_ALLTIME", iVal);
    }
    Db::db()->FlushToDb();
    return true;
  }
  return false;
} // ExportMNWVars
//------------------------------------------------------------------------------
/// \brief Exports the MNW data to the stress periods table
//------------------------------------------------------------------------------
static void iWriteMNWtoStressPeriods (MfData::MfPackage* a_package,
                                      int a_sp)
{
  using namespace MfData::Packages;
  const int* itmp(0), *nwell2(0);
  if (a_package->GetField(MNWpack::ITMP, &itmp) && itmp &&
      a_package->GetField(MNWpack::NWELL2, &nwell2) && nwell2)
  {
    int iVal(*itmp);
    if (iVal > 0)
      iVal = *nwell2;
    CStr query;
    query.Format("SPID = %d", a_sp);
    Db::db()->BeginCache("StressPeriods", query.c_str());
    Db::db()->AddValue("MNW1_ITMP", iVal);
    Db::db()->FlushToDb();
  }
} // iWriteMNWtoStressPeriods
//------------------------------------------------------------------------------
/// \brief Exports the MNW1 table
//------------------------------------------------------------------------------
static bool ExportMNWStressPeriod (MfData::MfPackage* a_package,
                                   int a_sp,
                                   int a_numI,
                                   int a_numJ)
{
  using namespace MfData::Packages;
  using namespace util;
  // write to the stress periods table
  iWriteMNWtoStressPeriods(a_package, a_sp);

  const int* itmp(0), *nwell2(0);
  const char* mnwsite(0);
  const double* well2(0), *mnwflgs(0);
  if (a_package->GetField(MNWpack::ITMP, &itmp) && itmp &&
      a_package->GetField(MNWpack::NWELL2, &nwell2) && nwell2 &&
      a_package->GetField(MNWpack::MNWSITE, &mnwsite) && mnwsite &&
      a_package->GetField(MNWpack::WELL2, &well2) && well2 &&
      a_package->GetField(MNWpack::MNWFLGS, &mnwflgs) && mnwflgs)
  {
    // don't need to do anything else because of use previous or 0 BCs this sp
    if (*itmp < 1)
      return true;

    int                i, j, count, cellId, wellId, Iwgrp, DD, QCUT, row;
    double             Qdes, QwVal, Rw, Skin, Hlim, Href, CpC, Qfrcmn, Qfrcmx, q;
    CellIdToIJK        grid(a_numI, a_numJ);
    CStr               sitename;
    char               sname[33];

    sname[32] = '\0';

    Db::db()->BeginCacheMutipleRows("MNW1", "", 16, *nwell2);
    count = *nwell2 + 1;
    // create a set of the well ids that are for MN wells
    std::set<int> mnIds, tmp;
    for (i=1; i<count; i++)
    {
      wellId = util::lrint(ForElement(well2, mnw::W2_ID, i, mnw::W2_SIZE));
      if (tmp.find(wellId) != tmp.end())
      {
        mnIds.insert(wellId);
      }
      else
      {
        tmp.insert(wellId);
      }
    }

    for (i=1, row=0; i<count; i++, row++)
    {
      q = ForElement(mnwflgs, mnw::MNWFLGS_QDES, i, mnw::MNWFLGS_SIZE);

      cellId = util::lrint(ForElement(well2, mnw::W2_NODE, i, mnw::W2_SIZE));
      wellId = util::lrint(ForElement(well2, mnw::W2_ID, i, mnw::W2_SIZE));
      if (mnIds.find(wellId) == mnIds.end())
        wellId = 0;
      Qdes = q;
      QwVal = ForElement(well2, mnw::W2_QWVAL, i, mnw::W2_SIZE);
      Rw = ForElement(well2, mnw::W2_RW, i, mnw::W2_SIZE);
      Skin = ForElement(well2, mnw::W2_SKIN, i, mnw::W2_SIZE);
      Hlim = ForElement(mnwflgs, mnw::MNWFLGS_HLIM, i, mnw::MNWFLGS_SIZE);
      Href = ForElement(mnwflgs, mnw::MNWFLGS_HLIM, i, mnw::MNWFLGS_SIZE);
      Iwgrp = util::lrint(ForElement(well2, mnw::W2_IWGRP, i, mnw::W2_SIZE));
      CpC = ForElement(well2, mnw::W2_C, i, mnw::W2_SIZE);
      Qfrcmn = ForElement(well2, mnw::W2_QFRCMN, i, mnw::W2_SIZE);
      Qfrcmx = ForElement(well2, mnw::W2_QFRCMX, i, mnw::W2_SIZE);

      DD = util::lrint(ForElement(mnwflgs, mnw::MNWFLGS_DD, i, mnw::MNWFLGS_SIZE));
      if (DD == mnw::DD_NONE)
      {
        Hlim = 0.0;
        Href = 0.0;
      }

      QCUT = util::lrint(ForElement(mnwflgs, mnw::MNWFLGS_QCUT, i, mnw::MNWFLGS_SIZE));
      if (/*Qdes == 0 ||*/ QCUT == 0) // this needs to be looked at again
      {
        Qfrcmn = 0;
        Qfrcmx = 0;
      }
      else if (QCUT == 1)
      {
        Qfrcmn *= q;
        Qfrcmx *= q;
      }
      else if (QCUT == 2)
      {
        Qfrcmn *= 100;
        Qfrcmx *= 100;
      }

      if (util::lrint(ForElement(mnwflgs, mnw::MNWFLGS_IERR, i,
                           mnw::MNWFLGS_SIZE)) >= 1)
      {
        Iwgrp = -1;
      }

      for (j=0; j<32; j++)
      {
        sname[j] = mnwsite[row*32+j];
      }
      sitename = sname;
      sitename.Trim();
      sitename.Replace("NO-PRINT", "");

      Db::db()->AddValue("IJK", cellId, row);
      Db::db()->AddValue("SPID", a_sp, row);
      Db::db()->AddValue("WellID", wellId, row);
      Db::db()->AddValue("Qdes", Qdes, row);
      Db::db()->AddValue("QwVal", QwVal, row);
      Db::db()->AddValue("Rw", Rw, row);
      Db::db()->AddValue("Skin", Skin, row);
      Db::db()->AddValue("Hlim", Hlim, row);
      Db::db()->AddValue("Href", Href, row);
      Db::db()->AddValue("DD", DD, row);
      Db::db()->AddValue("Iwgrp", Iwgrp, row);
      Db::db()->AddValue("Cp_C", CpC, row);
      Db::db()->AddValue("QCUT", QCUT, row);
      Db::db()->AddValue("Qfrcmn", Qfrcmn, row);
      Db::db()->AddValue("Qfrcmx", Qfrcmx, row);
      Db::db()->AddValue("SITE", sitename.c_str(), row);

    }
    Db::db()->FlushToDb();
    return true;
  }
  return false;
} // ExportMNWStressPeriod
//------------------------------------------------------------------------------
/// \brief Exports the UZF CB flags to the geodatabase.
//------------------------------------------------------------------------------
static bool ExportUZFCbf(MfData::MfPackage *a_p)
{
  using namespace MfData::Packages;
  const int *iuzfcb1, *iuzfcb2;
  if (a_p->GetField(UZFpack::IUZFCB1, &iuzfcb1) && iuzfcb1 &&
      a_p->GetField(UZFpack::IUZFCB2, &iuzfcb2) && iuzfcb2)
  {
    // Write to CBFlags
    Db::db()->BeginCache("CBFlags", "OID = 1");
    Db::db()->AddValue(UZFpack::IUZFCB1, *iuzfcb1);
    Db::db()->AddValue(UZFpack::IUZFCB2, *iuzfcb2);
    Db::db()->FlushToDb();

    return true;
  }
  return false;
} // ExportUZFCbf
//------------------------------------------------------------------------------
/// \brief Exports the UZF vars data to the geodatabase.
//------------------------------------------------------------------------------
static bool ExportUZFVars(MfData::MfPackage *a_p)
{
  using namespace MfData::Packages;
  const int *nuztop, *iuzfopt, *irunflg, *ietflg, *ntrail2,
            *nsets2, *nuzgag;
  const Real *surfdep;
  if (a_p->GetField(UZFpack::NUZTOP, &nuztop) && nuztop &&
      a_p->GetField(UZFpack::IUZFOPT, &iuzfopt) && iuzfopt &&
      a_p->GetField(UZFpack::IRUNFLG, &irunflg) && irunflg &&
      a_p->GetField(UZFpack::IETFLG, &ietflg) && ietflg &&
      a_p->GetField(UZFpack::NTRAIL2, &ntrail2) && ntrail2 &&
      a_p->GetField(UZFpack::NSETS2, &nsets2) && nsets2 &&
      a_p->GetField(UZFpack::NUZGAG, &nuzgag) && nuzgag &&
      a_p->GetField(UZFpack::SURFDEP, &surfdep) && surfdep)
  {
    // Write to UZFVars
    Db::db()->BeginCache("UZFVars", "");
    Db::db()->AddValue(UZFpack::NUZTOP, *nuztop);
    Db::db()->AddValue(UZFpack::IUZFOPT, *iuzfopt);
    Db::db()->AddValue(UZFpack::IRUNFLG, *irunflg);
    Db::db()->AddValue(UZFpack::IETFLG, *ietflg);
    if (*iuzfopt > 0)
    {
      Db::db()->AddValue(UZFpack::NTRAIL2, *ntrail2);
      Db::db()->AddValue(UZFpack::NSETS2, *nsets2);
    }
    Db::db()->AddValue(UZFpack::NUZGAG, *nuzgag);
    Db::db()->AddValue(UZFpack::SURFDEP, *surfdep);
    Db::db()->FlushToDb();

    return true;
  }
  return false;
} // ExportUZFVars
//------------------------------------------------------------------------------
/// \brief Exports the UZF line 1 package data to the geodatabase.
//------------------------------------------------------------------------------
static void ExportUZFLine1(MfData::MfPackage *a_p)
{
  ExportUZFCbf(a_p);
  ExportUZFVars(a_p);
} // ExportUZFLine1
//------------------------------------------------------------------------------
/// \brief Exports the UZF line 1 package data to the geodatabase.
//------------------------------------------------------------------------------
static void ExportUZFLine8(MfData::MfPackage *a_pLine1,
                           MfData::MfPackage *a_pLine8)
{
  using namespace MfData::Packages;
  using util::ForElement;
  const int *iuzfopt, *irunflg, *nuzgag, *iuzlist;
  if (a_pLine1->GetField(UZFpack::IUZFOPT, &iuzfopt) && iuzfopt &&
      a_pLine1->GetField(UZFpack::IRUNFLG, &irunflg) && irunflg &&
      a_pLine1->GetField(UZFpack::NUZGAG, &nuzgag) && nuzgag &&
      a_pLine8->GetField(UZFpack::IUZLIST, &iuzlist) && iuzlist)
  {
    // Line 8: [IUZROW] [IUZCOL] IFTUNIT [IUZOPT]
    for (int i = 1; i <= *nuzgag; ++i)
    {
      const int IUZLIST_SIZE = 4;
      int iuzrow, iuzcol, iftunit, iuzopt;
      iuzrow  = ForElement(iuzlist, 1, i, IUZLIST_SIZE);
      iuzcol  = ForElement(iuzlist, 2, i, IUZLIST_SIZE);
      iftunit = ForElement(iuzlist, 3, i, IUZLIST_SIZE);
      iuzopt  = ForElement(iuzlist, 4, i, IUZLIST_SIZE);

      Db::db()->BeginCache("UZFGages", "");
      if (iuzrow != 0)
      {
        Db::db()->AddValue("IUZROW", iuzrow);
        Db::db()->AddValue("IUZCOL", iuzcol);
        Db::db()->AddValue("IFTUNIT", iftunit);
        Db::db()->AddValue("IUZOPT", iuzopt);
      }
      else
      {
        Db::db()->AddValue("IFTUNIT", -iftunit);
      }
      Db::db()->FlushToDb();
    }
  }
} // ExportUZFLine8
//------------------------------------------------------------------------------
/// \brief Exports the UZF stress period data to the database.
//------------------------------------------------------------------------------
static void ExportUZFStressPeriod(MfData::MfPackage *a_pLine1,
                                  MfData::MfPackage *a_pSP, int a_sp)
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
    // Write NUZF1 [NUZF2] [NUZF3] [NUZF4] to UZFStressArrayMult table
    CStr query;
    query.Format("SPID = %d", a_sp);
    Db::db()->BeginCache("UZFStressArrayMult", query.c_str());
    Db::db()->AddValue("SPID", a_sp);
    Db::db()->AddValue(UZFpack::NUZF1, *nuzf1);
    if (*ietflg > 0)
    {
      Db::db()->AddValue(UZFpack::NUZF2, *nuzf2);
      Db::db()->AddValue(UZFpack::NUZF3, *nuzf3);
      Db::db()->AddValue(UZFpack::NUZF4, *nuzf4);
    }
    Db::db()->FlushToDb();
  }
} // ExportUZFStressPeriod
//------------------------------------------------------------------------------
/// \brief Exports the HUF package data to the geodatabase.
//------------------------------------------------------------------------------
static bool ExportHUF(MfData::MfPackage* a_package, int a_nlay)
{
  if (!a_package)
    return false;

  ExportHUFCbf(a_package);// write the IHUFCB flag

  if(!ExportHUFVars(a_package, a_nlay) ||
     !ExportHUFLayers(a_package, a_nlay) ||
     !iWriteHGUs(a_package))
    return false;

  return true;
} // ExportHUF
//------------------------------------------------------------------------------
/// \brief Exports the HUF package CBFlags to the database
//------------------------------------------------------------------------------
static void ExportHUFCbf (MfData::MfPackage* a_package)
{
  using namespace MfData::Packages;

  const int *iptr;
  // write the IHUFCB flag
  if (a_package->GetField(HUFPack::IHUFCB, &iptr) && iptr)
  {
    Db::db()->BeginCache("CBFlags", "OID = 1");
    Db::db()->AddValue(HUFPack::IHUFCB, *iptr);
    Db::db()->FlushToDb();
  }
} // ExportHUFCbf
//------------------------------------------------------------------------------
/// \brief Exports the HUF package HUFVars table to the database
//------------------------------------------------------------------------------
static bool ExportHUFVars(MfData::MfPackage* a_package, int a_nlay)
{
  using namespace MfData::Packages;
  // write the HUF vars
  const Real *HDRY(0), *WETFCT(0);
  const int *NHUF(0), *IOHUFHEADS(0), *IOHUFFLOWS(0), *IWETIT(0), *IHDWET(0);
  const int *LAYWT(0);

  if (a_package->GetField(HUFPack::HDRY, &HDRY) && HDRY &&
      a_package->GetField(HUFPack::NHUF, &NHUF) && NHUF &&
      a_package->GetField(HUFPack::IOHUFHEADS, &IOHUFHEADS) && IOHUFHEADS &&
      a_package->GetField(HUFPack::IOHUFFLOWS, &IOHUFFLOWS) && IOHUFFLOWS &&
      a_package->GetField(HUFPack::LAYWT, &LAYWT) && LAYWT)
  {
    bool wet(false);
    for (int i=0; i<a_nlay; i++)
    {
      if (LAYWT[i] == 1)
        wet = true;
    }
    if (wet)
    {
      if (!a_package->GetField(HUFPack::WETFCT, &WETFCT) || !WETFCT ||
          !a_package->GetField(HUFPack::IWETIT, &IWETIT) || !IWETIT ||
          !a_package->GetField(HUFPack::IHDWET, &IHDWET) || !IHDWET)
        return false;
    }

    Db::db()->BeginCache("HUFVars", "");
    Db::db()->AddValue(HUFPack::HDRY, *HDRY);
    Db::db()->AddValue(HUFPack::NHUF, *NHUF);
    Db::db()->AddValue(HUFPack::IOHUFHEADS, *IOHUFHEADS);
    Db::db()->AddValue(HUFPack::IOHUFFLOWS, *IOHUFFLOWS);
    if (wet)
    {
      Db::db()->AddValue(HUFPack::WETFCT, *WETFCT);
      Db::db()->AddValue(HUFPack::IWETIT, *IWETIT);
      Db::db()->AddValue(HUFPack::IHDWET, *IHDWET);
    }
    Db::db()->FlushToDb();
    return true;
  }
  return false;
} // ExportHUFVars
//------------------------------------------------------------------------------
/// \brief Exports the HUF package HUFLayers table to the database
//------------------------------------------------------------------------------
static bool ExportHUFLayers (MfData::MfPackage* a_package, int a_nlay)
{
  using namespace MfData::Packages;
  // write the lpf layers data
  const int *LTHUF(0), *LAYWT(0);
  if (a_package->GetField(HUFPack::LTHUF, &LTHUF) && LTHUF &&
      a_package->GetField(HUFPack::LAYWT, &LAYWT) && LAYWT)
  {
    Db::db()->BeginCacheMutipleRows("HUFLayers", "", 3, a_nlay);
    for (int i=0; i<a_nlay; i++)
    {
      Db::db()->AddValue("Layer", i+1, i);
      Db::db()->AddValue(HUFPack::LTHUF, LTHUF[i], i);
      Db::db()->AddValue(HUFPack::LAYWT, LAYWT[i], i);
    }
    Db::db()->FlushToDb();
  }

  return true;
} // Export HUFLayers
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static bool iWriteHGUs(MfData::MfPackage *a_p)
{
  using namespace MfData::Packages;

  const int *NHUF(0), *IHGUFLG(0);
  const char *HGUNAM(0);
  const Real *HGUHANI(0), *HGUVANI(0);

  if (!a_p->GetField(HUFPack::NHUF, &NHUF) || !NHUF ||
      !a_p->GetField(HUFPack::HGUNAM, &HGUNAM) || !HGUNAM ||
      !a_p->GetField(HUFPack::HGUHANI, &HGUHANI) || !HGUHANI ||
      !a_p->GetField(HUFPack::HGUVANI, &HGUVANI) || !HGUVANI ||
      !a_p->GetField(HUFPack::IHGUFLG, &IHGUFLG) || !IHGUFLG
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

  // Write to the HUFUnits table
  CStr query;
  for (i=0; i<*NHUF; ++i)
  {
    CStr hguName(names.at(i));
    hguName.ToLower();
    int hguId;
    if (mapHguNamesId().find(hguName) == mapHguNamesId().end())
    {
      mapHguNamesId()[hguName] = (int)mapHguNamesId().size() + 1;
    }
    hguId = mapHguNamesId()[hguName];
    query.Format("HGUID = %d", hguId);

    Db::db()->BeginCache("HUFUnits", (LPCTSTR)query);
    Db::db()->AddValue("HGUNAME", names.at(i));
    Db::db()->AddValue("HGUID", hguId);
    Db::db()->AddValue(HUFPack::HGUHANI, HGUHANI[i]);
    Db::db()->AddValue(HUFPack::HGUVANI, HGUVANI[i]);
    Db::db()->AddValue("PRINTCODE_HK", IHGUFLG[(i*5) + 0]);
    Db::db()->AddValue("PRINTCODE_HANI", IHGUFLG[(i*5) + 1]);
    Db::db()->AddValue("PRINTCODE_VK", IHGUFLG[(i*5) + 2]);
    Db::db()->AddValue("PRINTCODE_Ss", IHGUFLG[(i*5) + 3]);
    Db::db()->AddValue("PRINTCODE_Sy", IHGUFLG[(i*5) + 4]);
    Db::db()->FlushToDb();
  }

  return true;
} // iWriteHGUs
//------------------------------------------------------------------------------
/// \brief Gets the STRReaches fields
//------------------------------------------------------------------------------
static void GetSTRReachesFields (std::vector<CStr> &a_ifields,
                                 std::vector<CStr> &a_dfields)
{
  a_ifields.push_back("IJK");
  a_ifields.push_back("SPID");
  a_ifields.push_back("Seg");
  a_ifields.push_back("Reach");

  a_dfields.push_back("Flow");
  a_dfields.push_back("Stage");
  a_dfields.push_back("Cond");
  a_dfields.push_back("Sbot");
  a_dfields.push_back("Stop");
  a_dfields.push_back("Width");
  a_dfields.push_back("Slope");
  a_dfields.push_back("Rough");
  a_dfields.push_back("Condfact");
} // GetSENFields
//------------------------------------------------------------------------------
/// \brief Exports the Stream package to the database
//------------------------------------------------------------------------------
static bool ExportSTR(MfData::MfPackage *a_p, const int a_sp, const int a_nRow,
                      const int a_nCol)
{
  using namespace MfData::Packages;

  enum strFields_enum { STR_STAGE=0, STR_COND, STR_BELEV, STR_TELEV,
                        STR_WID, STR_SLP, STR_RGH, STR_CONDFACT,
                        STR_NPROP };

  if (!a_p)
    return false;

  ExportSTRCbf(a_p);

  const int *istrpb(0), *nss(0), *ntrib(0), *ndiv(0), *icalc(0), *istcb1(0),
            *istcb2(0), *itmp(0), *irdflg(0), *iptflg(0), *istrm(0),
            *nstrem(0), *mxstrm(0), *itrbar(0), *idivar(0);
  const Real *constv, *strm(0);
  if (a_p->GetField(STRpack::MXACTS, &istrpb) && istrpb &&
      a_p->GetField(STRpack::NSS, &nss) && nss &&
      a_p->GetField(STRpack::NTRIB, &ntrib) && ntrib &&
      a_p->GetField(STRpack::NDIV, &ndiv) && ndiv &&
      a_p->GetField(STRpack::ICALC, &icalc) && icalc &&
      a_p->GetField(STRpack::CONSTV, &constv) && constv &&
      a_p->GetField(STRpack::ISTCB1, &istcb1) && istcb1 &&
      a_p->GetField(STRpack::ISTCB2, &istcb2) && istcb2 &&
      a_p->GetField(STRpack::ITMP, &itmp) && itmp &&
      a_p->GetField(STRpack::IRDFLG, &irdflg) && irdflg &&
      a_p->GetField(STRpack::IPTFLG, &iptflg) && iptflg &&
      a_p->GetField(STRpack::STRM, &strm) && strm &&
      a_p->GetField(STRpack::ISTRM, &istrm) && istrm &&
      a_p->GetField(STRpack::NSTREM, &nstrem) && nstrem &&
      a_p->GetField(STRpack::MXSTRM, &mxstrm) && mxstrm &&
      a_p->GetField(STRpack::ITRBAR, &itrbar) && itrbar &&
      a_p->GetField(STRpack::IDIVAR, &idivar) && idivar)
  {
    CStr               type("Stream"), line;
    CellIdToIJK        grid(a_nRow, a_nCol);
    std::vector<int>   &cellids(iGetBcCellIds()[type]),
                       &vIface(iGetBcIface()[type]),
                       &vCellgrp(iGetBcCellGrp()[type]);
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

    // Write to the STRVars table
    if (a_sp == 1)
    {
      Db::db()->BeginCache("STRVars", "");
      Db::db()->AddValue(STRpack::ICALC, *icalc);
      Db::db()->AddValue(STRpack::CONSTV, *constv);
      Db::db()->AddValue(STRpack::ISTCB1, *istcb1);
      Db::db()->AddValue(STRpack::ISTCB2, *istcb2);
      Db::db()->FlushToDb();
    }

    // Write to the Stress Periods table
    iWriteSTRtoStressPeriods(a_p, a_sp);


    // first create a vector of indices so we can size the data array
    int maxIdx(-1);
    std::vector<int> idxs, ijkcellids, segs, reaches;
    idxs.reserve(*nBcs);
    for (i=0; i<*nBcs; i++)
    {
      int curseg, curreach;

      // changed from expListPack
      ck = istrm[i*(5)+0];
      ci = istrm[i*(5)+1];
      cj = istrm[i*(5)+2];
      // end change
      curseg = istrm[i*(5)+3];
      curreach = istrm[i*(5)+4];

      cellId = grid.IdFromIJK(ci, cj, ck);
      ijkcellids.push_back(cellId);
      segs.push_back(curseg);
      reaches.push_back(curreach);

      idxs.push_back(iGetBcIndex(type, cellId, a_sp, cellids, vIface, vCellgrp));
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

    // get the segment ids and flow
    std::vector<int> segmentIds;
    std::vector<Real> flow;
    int lastSegmentId = istrm[3] - 1;
    for (i = 0; i < *nBcs; ++i)
    {
      int segmentId = istrm[i*5+3];
      if (segmentId != lastSegmentId)
      {
        segmentIds.push_back(segmentId);
        flow.push_back(strm[i*(*nFields)+0]);
        lastSegmentId = segmentId;
      }
    }

    // Write out to the STRReaches table
    std::vector<CStr> intflds;
    std::vector<CStr> dblflds;
    GetSTRReachesFields(intflds, dblflds);

    Db::db()->BeginCacheMutipleRows("STRReaches", "",
                                     (int)(intflds.size() + dblflds.size()),
                                     *nBcs);
    for (i = 0; i < *nBcs; ++i)
    {
      std::vector<int> curivals;
      std::vector<double> curdvals;

      curivals.assign(4, 0);
      curivals.at(0) = ijkcellids.at(i);
      curivals.at(1) = a_sp;
      curivals.at(2) = segs.at(i);
      curivals.at(3) = reaches.at(i);

      // Get the flow property -- 0 if reach isn't 1
      curdvals.push_back(0.0);
      if (reaches.at(i) == 1)
      {
        // If reach is 1, set the flow value
        for (j = 0; j < (int)segmentIds.size(); ++j)
        {
          if (segs.at(i) == segmentIds.at(j))
            curdvals.at(0) = flow.at(j);
        }
      }
      // Get the other double properties
      for (j=STR_STAGE; j<STR_NPROP; j++)
      {
        curdvals.push_back(bcData.at(j, idxs.at(i)));
      }

      // Write to the STRReaches table
      for (size_t j = 0; j < curivals.size() && j < intflds.size(); ++j)
        Db::db()->AddValue(intflds[j], curivals[j], i);
      for (size_t j = 0; j < curdvals.size() && j < dblflds.size(); ++j)
        Db::db()->AddValue(dblflds[j], curdvals[j], i);
    }
    Db::db()->FlushToDb();


    // write the tributaries and upstream segments
    int itrib, curseg, iupseg;

    Db::db()->BeginCacheMutipleRows("STRItrib", "", 3, *ntrib * *nss);
    for (int segment = 0, nCount = 0; segment < *nss; ++segment)
    {
      curseg = segment + 1;

      for (int trib = 0; trib < *ntrib; ++trib)
      {
        itrib = itrbar[trib*(*nss) + segment];
        Db::db()->AddValue("Seg", curseg, nCount);
        Db::db()->AddValue("SPID", a_sp, nCount);
        Db::db()->AddValue("Itrib", itrib, nCount);
        ++nCount;
      }
    }
    Db::db()->FlushToDb();

    Db::db()->BeginCacheMutipleRows("STRIupseg", "", 3, *ndiv * *nss);
    for (int segment = 0, nCount = 0; segment < *nss; ++segment)
    {
      curseg = segment + 1;

      for (int upseg = 0; upseg < *ndiv; ++upseg)
      {
        iupseg = idivar[upseg*(*nss) + segment];
        Db::db()->AddValue("Seg", curseg, nCount);
        Db::db()->AddValue("SPID", a_sp, nCount);
        Db::db()->AddValue("Iupseg", iupseg, nCount);
        ++nCount;
      }
    }
    Db::db()->FlushToDb();

    return true;
  }
  return false;
} // ExportSTR
//------------------------------------------------------------------------------
/// \brief Exports the STR package CBFlags to the database
//------------------------------------------------------------------------------
static void ExportSTRCbf (MfData::MfPackage* a_package)
{
  using namespace MfData::Packages;

  const int *istcb1_str;
  const int *istcb2_str;
  // write the ISTCB1_STR and ISTCB2_STR CB flags
  if (a_package->GetField(STRpack::ISTCB1, &istcb1_str) && istcb1_str &&
      a_package->GetField(STRpack::ISTCB2, &istcb2_str) && istcb2_str)
  {
    CStr cb1, cb2;
    cb1.Format("%s_STR", STRpack::ISTCB1);
    cb2.Format("%s_STR", STRpack::ISTCB2);
    Db::db()->BeginCache("CBFlags", "OID = 1");
    Db::db()->AddValue(cb1, *istcb1_str);
    Db::db()->AddValue(cb2, *istcb2_str);
    Db::db()->FlushToDb();
  }
} // ExportSTRCbf
//------------------------------------------------------------------------------
/// \brief Exports the STR stress period info to the database
//------------------------------------------------------------------------------
static void iWriteSTRtoStressPeriods(MfData::MfPackage* a_package, int a_sp)
{
  using namespace MfData::Packages;
  const int* itmp(0), *irdflg(0), *iptflg(0);
  if (a_package->GetField(STRpack::ITMP, &itmp) && itmp &&
      a_package->GetField(STRpack::IRDFLG, &irdflg) && irdflg &&
      a_package->GetField(STRpack::IPTFLG, &iptflg) && iptflg)
  {
    CStr query;
    query.Format("SPID = %d", a_sp);
    Db::db()->BeginCache("StressPeriods", query.c_str());
    Db::db()->AddValue("STR_ITMP", *itmp);
    Db::db()->AddValue("STR_IRDFLG", *irdflg);
    Db::db()->AddValue("STR_IPTFLG", *iptflg);
    Db::db()->FlushToDb();
  }
} // iWriteSTRtoStressPeriods
//------------------------------------------------------------------------------
/// \brief Exports the lake package without stress periods
//------------------------------------------------------------------------------
static void ExportLAK (MfData::MfPackage *a_p)
{
  using namespace MfData::Packages;

  const int *nlakes(0), *ilkcb(0), *nssitr(0), *itrss(0);
  const Real *theta(0), *sscncr(0);
  if (a_p->GetField(LAKpack::NLAKES, &nlakes) && nlakes &&
      a_p->GetField(LAKpack::ILKCB, &ilkcb) && ilkcb &&
      a_p->GetField(LAKpack::THETA, &theta) && theta &&
      a_p->GetField(LAKpack::NSSITR, &nssitr) && nssitr &&
      a_p->GetField(LAKpack::SSCNCR, &sscncr) && sscncr &&
      a_p->GetField(LAKpack::ITRSS, &itrss) && itrss)
  {
    // export ILKCB to CBFlags
    Db::db()->BeginCache("CBFlags", "OID = 1");
    Db::db()->AddValue(LAKpack::ILKCB, *ilkcb);
    Db::db()->FlushToDb();

    // export THETA NSSITR SSCNCR to LAKVars
    Db::db()->BeginCache("LAKVars", "");
    Db::db()->AddValue(LAKpack::THETA, *theta);
    Db::db()->AddValue(LAKpack::NSSITR, *nssitr);
    Db::db()->AddValue(LAKpack::SSCNCR, *sscncr);
    Db::db()->FlushToDb();
  }
} // ExportLAK
//------------------------------------------------------------------------------
/// \brief Exports the lake package stress periods
//------------------------------------------------------------------------------
static void ExportLAKSP (MfData::MfGlobal *a_g, MfData::MfPackage *a_p,
                         MfData::MfPackage *a_pLAK)
{
  using namespace MfData::Packages;

  const int *nlakes(0), *dummy(0), *itmp(0), *itmp1(0), *lwrt(0),
            *lkarr(0), *nslms(0), *ic(0), *isub(0), 
            currentPeriod(a_g->GetCurrentPeriod());
  const Real *stages(0), *ssmn(0), *ssmx(0), *clake(0), *bdlknc(0), *sillvt(0),
             *rnf(0), *wthdrw(0), *cppt(0), *crnf(0), *caug(0);
  const double *prcplk(0), *evaplk(0);

  if (a_pLAK->GetField(LAKpack::NLAKES, &nlakes) && nlakes &&
      a_p->GetField(LAKSPpack::NSOL, &dummy) && dummy &&
      a_p->GetField(LAKSPpack::STAGES, &stages) && stages &&
      a_p->GetField(LAKSPpack::SSMN, &ssmn) && ssmn &&
      a_p->GetField(LAKSPpack::SSMX, &ssmx) && ssmx &&
      a_p->GetField(LAKSPpack::CLAKE, &clake) && clake &&
      a_p->GetField(LAKSPpack::ITMP, &itmp) && itmp &&
      a_p->GetField(LAKSPpack::ITMP1, &itmp1) && itmp1 &&
      a_p->GetField(LAKSPpack::LWRT, &lwrt) && lwrt &&
      a_p->GetField(LAKSPpack::LKARR, &lkarr) && lkarr &&
      a_p->GetField(LAKSPpack::BDLKNC, &bdlknc) && bdlknc &&
      a_p->GetField(LAKSPpack::NSLMS, &nslms) && nslms &&
      a_p->GetField(LAKSPpack::IC, &ic) && ic &&
      a_p->GetField(LAKSPpack::ISUB, &isub) && isub &&
      a_p->GetField(LAKSPpack::SILLVT, &sillvt) && sillvt &&
      a_p->GetField(LAKSPpack::PRCPLK, &prcplk) && prcplk &&
      a_p->GetField(LAKSPpack::EVAPLK, &evaplk) && evaplk &&
      a_p->GetField(LAKSPpack::RNF, &rnf) && rnf &&
      a_p->GetField(LAKSPpack::WTHDRW, &wthdrw) && wthdrw &&
      a_p->GetField(LAKSPpack::CPPT, &cppt) && cppt &&
      a_p->GetField(LAKSPpack::CRNF, &crnf) && crnf &&
      a_p->GetField(LAKSPpack::CAUG, &caug) && caug)
  {
    CStr s;

    // Coded to include NSOL but it seems that mf2k gives NSOL=1
    // even though it wasn't included in export file.  Removing
    // for now.
    int nsolutes(0), *nsol=&nsolutes;
    
    if (currentPeriod == 1)
    {
      // export STAGES {SSMN SSMX} {CLAKE(1)... CLAKE(NSOL)} to LAKStages
      Db::db()->BeginCacheMutipleRows("LAKStages", "", 4, *nlakes);
      for (int lake = 0; lake < *nlakes; ++lake)
      {
        Db::db()->AddValue("LakeID", lake + 1, lake);
        Db::db()->AddValue(LAKSPpack::STAGES, stages[lake], lake);
        Db::db()->AddValue(LAKSPpack::SSMN, ssmn[lake], lake);
        Db::db()->AddValue(LAKSPpack::SSMX, ssmx[lake], lake);

        // CLAKE info... not in database
        for (int sol = 0; sol < *nsol; ++sol)
        {
          CStr clakeStr;
          clakeStr.Format(" %s", STR(clake[sol*(*nlakes) + lake]));
          s += clakeStr;
        }
      }
      Db::db()->FlushToDb();
    }

    // Write out to the Stress Periods table for this SP
    iWriteLAKtoStressPeriods(a_p, currentPeriod);

    if (*itmp > 0)
    {
      // Write to LAKArrays
      int nCol(a_g->NumCol()), nRow(a_g->NumRow()), nLay(a_g->NumLay());
      int inLay(nCol * nRow);

      std::vector<int> vIJK;
      std::vector<int> vLKARR;
      std::vector<double> vBDLKNC;
      for (int k = 0; k < nLay; ++k)
      {
        for (int ij = 0; ij < inLay; ++ij)
        {
          int nlkarr = lkarr[ij + k*inLay];
          double dbdlknc = bdlknc[ij + k*inLay];
          int ijk = (inLay * k) + (ij + 1);

          vIJK.push_back(ijk);
          vLKARR.push_back(nlkarr);
          vBDLKNC.push_back(dbdlknc);
        }
      }

      Db::db()->BeginCacheMutipleRows("LAKArrays", "", 4, (int)vIJK.size());
      for (int lakarraycnt = 0; (size_t)lakarraycnt < vIJK.size(); ++lakarraycnt)
      {
        Db::db()->AddValue("IJK", vIJK[lakarraycnt], lakarraycnt);
        Db::db()->AddValue("SPID", currentPeriod, lakarraycnt);
        Db::db()->AddValue(LAKSPpack::LKARR, vLKARR[lakarraycnt],
                           lakarraycnt);
        Db::db()->AddValue(LAKSPpack::BDLKNC, vBDLKNC[lakarraycnt],
                           lakarraycnt);
      }
      Db::db()->FlushToDb();


      // Write to LAKSublakes
      if (*nslms > 0)
      {
        for (int system = 0; system < *nslms && ic[system] > 0; ++system)
        {
          int nSystemID = ic[system];
          std::vector<int> vISUB;
          std::vector<double> vSILLVT;

          for (int subLake = 0; subLake < ic[system]; ++subLake)
          {
            int curISUB = isub[subLake*(*nlakes) + system];
            vISUB.push_back(curISUB);
          }
          for (int subLake = 0; subLake < ic[system] - 1; ++subLake)
          {
            double curSILLVT = sillvt[subLake*(*nlakes) + system];
            vSILLVT.push_back(curSILLVT);
          }

          Db::db()->BeginCacheMutipleRows("LAKSublakes", "", 4,
                                          (int)vISUB.size());
          for (int subcnt = 0; (size_t)subcnt < vISUB.size(); ++subcnt)
          {
            Db::db()->AddValue("SPID", currentPeriod, subcnt);
            Db::db()->AddValue("SystemID", nSystemID, subcnt);
            Db::db()->AddValue(LAKSPpack::ISUB, vISUB[subcnt], subcnt);
            if (subcnt < (int)vSILLVT.size())
              Db::db()->AddValue(LAKSPpack::SILLVT, vSILLVT[subcnt], subcnt);
          }
          Db::db()->FlushToDb();

        }
      }


      // Write to LAKRates
      if (*itmp1 >= 0)
      {
        // PRCPLK EVAPLK RNF WTHDRW

        Db::db()->BeginCacheMutipleRows("LAKRates", "", 8, *nlakes);
        for (int lake = 0; lake < *nlakes; ++lake)
        {
          Db::db()->AddValue("SPID", currentPeriod, lake);
          Db::db()->AddValue("LakeID", lake + 1, lake);
          Db::db()->AddValue(LAKSPpack::PRCPLK, prcplk[lake], lake);
          Db::db()->AddValue(LAKSPpack::EVAPLK, evaplk[lake], lake);
          Db::db()->AddValue(LAKSPpack::RNF, rnf[lake], lake);
          Db::db()->AddValue(LAKSPpack::WTHDRW, wthdrw[lake], lake);
          Db::db()->AddValue(LAKSPpack::SSMN, ssmn[lake], lake);
          Db::db()->AddValue(LAKSPpack::SSMX, ssmx[lake], lake);


          // Not in the data model:
          // CPPT CRNF {CAUG}
          //for (int sol = 0; sol < *nsol; ++sol)
          //{
          //  int i = sol*(*nlakes) + lake;
          //  s.Format("%s %s", STR(cppt[i]), STR(crnf[i]));
          //  CStr caugStr;
          //  if (wthdrw[lake] < 0.0)
          //  {
          //    caugStr.Format(" %s", STR(caug[i]));
          //  }
          //  s += caugStr;
          //  a_exp->WriteLineToFile(Packages::LAK, s);
          //}
        }
        Db::db()->FlushToDb();
      }

    }
  }
} // ExportLAKSP
//------------------------------------------------------------------------------
/// \brief Exports the LAK stress period info to the database
//------------------------------------------------------------------------------
static void iWriteLAKtoStressPeriods(MfData::MfPackage* a_package, int a_sp)
{
  using namespace MfData::Packages;
  const int* itmp(0), *itmp1(0), *lwrt(0);
  if (a_package->GetField(LAKSPpack::ITMP, &itmp) && itmp &&
      a_package->GetField(LAKSPpack::ITMP1, &itmp1) && itmp1 &&
      a_package->GetField(LAKSPpack::LWRT, &lwrt) && lwrt)
  {
    CStr query;
    CStr szITMP, szITMP1, szLWRT;
    szITMP.Format("LAK_%s", LAKSPpack::ITMP);
    szITMP1.Format("LAK_%s", LAKSPpack::ITMP1);
    szLWRT.Format("LAK_%s", LAKSPpack::LWRT);
    query.Format("SPID = %d", a_sp);
    Db::db()->BeginCache("StressPeriods", query.c_str());
    Db::db()->AddValue(szITMP, *itmp);
    Db::db()->AddValue(szITMP1, *itmp1);
    Db::db()->AddValue(szLWRT, *lwrt);
    Db::db()->FlushToDb();
  }
} // iWriteSTRtoStressPeriods
//------------------------------------------------------------------------------
/// \brief Exports the SFR Line 1 data to the database
//------------------------------------------------------------------------------
static void ExportSFRLine1 (MfData::MfPackage *a_pSFRLine1)
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
    // Write to the SFRVars table
    Db::db()->BeginCache("SFRVars", "");
    Db::db()->AddValue(SFRpack::CONSTV, *constv);
    Db::db()->AddValue(SFRpack::DLEAK, *dleak);
    Db::db()->AddValue(SFRpack::ISFROPT, *isfropt);
    Db::db()->AddValue(SFRpack::NSTRAIL, *nstrail);
    Db::db()->AddValue(SFRpack::ISUZN, *isuzn);
    Db::db()->AddValue(SFRpack::NSFRSETS, *nsfrsets);
    Db::db()->FlushToDb();

    // Write to CBFlags
    CStr cb1, cb2;
    cb1.Format("%s_SFR", SFRpack::ISTCB1);
    cb2.Format("%s_SFR", SFRpack::ISTCB2);
    Db::db()->BeginCache("CBFlags", "OID = 1");
    Db::db()->AddValue(cb1, *istcb1);
    Db::db()->AddValue(cb2, *istcb2);
    Db::db()->FlushToDb();
  }
} // ExportSFRLine1
//------------------------------------------------------------------------------
/// \brief Exports the SFR Line 2 data to the database
//------------------------------------------------------------------------------
static void ExportSFRLine2(MfData::MfPackage *a_pSFRLine1,
                           MfData::MfPackage *a_pSFRLine2,
                           int a_nRow, int a_nCol)
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
    const char* type = "Stream (SFR2)";
    std::vector<int>   &cellids(iGetBcCellIds()[type]),
                       &vIface(iGetBcIface()[type]),
                       &vCellgrp(iGetBcCellGrp()[type]);
    CellIdToIJK        grid(a_nRow, a_nCol);
    int numReaches(*nstrm);
    if (numReaches < 0)
      numReaches = -numReaches;

    // first create a vector of indices so we can size the data array
    int maxIdx(-1);
    std::vector<int> idxs, ijkcellids, segs, reaches;
    idxs.reserve(numReaches);
    for (int i = 0; i < numReaches; i++)
    {
      int ck = istrm[i*(*nistrmd)+0];
      int ci = istrm[i*(*nistrmd)+1];
      int cj = istrm[i*(*nistrmd)+2];
      int curseg = istrm[i*(*nistrmd)+3];
      int curreachid = istrm[i*(*nistrmd)+4];

      int cellId = grid.IdFromIJK(ci, cj, ck);
      ijkcellids.push_back(cellId);
      segs.push_back(curseg);
      reaches.push_back(curreachid);
      idxs.push_back(iGetBcIndex(type, cellId, 1, cellids, vIface, vCellgrp));
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
    std::vector<double> vRCHLEN;
    for (int i = 0; i < bcData.GetSize2(); ++i)
      vRCHLEN.push_back(bcData.at(0,i));


    // Write to the SFRReaches table
    Db::db()->BeginCacheMutipleRows("SFRReaches", "", 4, numReaches);
    for (int i = 0; i < numReaches; ++i)
    {
      Db::db()->AddValue("IJK", ijkcellids[i], i);
      Db::db()->AddValue(SFRpack::ISEG, segs[i], i);
      Db::db()->AddValue("IREACH", reaches[i], i);
      Db::db()->AddValue("RCHLEN", vRCHLEN[i], i);
    }
    Db::db()->FlushToDb();
  }
} // ExportSFRLine2
//------------------------------------------------------------------------------
/// \brief Exports the SFR Line 5 data to the database
//------------------------------------------------------------------------------
static void ExportSFRLine5(MfData::MfPackage *a_pSFRLine5, int a_sp)
{
  using namespace MfData::Packages;
  const int *itmp(0), *irdflg(0), *iptflg(0);
  if (a_pSFRLine5->GetField(SFRpack::ITMP, &itmp) && itmp &&
      a_pSFRLine5->GetField(SFRpack::IRDFLG, &irdflg) && irdflg &&
      a_pSFRLine5->GetField(SFRpack::IPTFLG, &iptflg) && iptflg)
  {
    CStr query;
    query.Format("SPID = %d", a_sp);
    Db::db()->BeginCache("StressPeriods", query.c_str());
    Db::db()->AddValue("SFR_ITMP", *itmp);
    Db::db()->AddValue("SFR_IRDFLG", *irdflg);
    Db::db()->AddValue("SFR_IPTFLG", *iptflg);
    Db::db()->FlushToDb();
  }
} // ExportSFRLine5
//------------------------------------------------------------------------------
/// \brief Exports the SFR Line 6 data to the database
//------------------------------------------------------------------------------
static void ExportSFRLine6(MfData::MfPackage *a_pSFRLine1,
                           MfData::MfPackage *a_pSFRLine6, int a_sp)
{
  using namespace MfData::Packages;
  using util::ForElement;

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

  int sz=26;
  MfData::Get().GetIntVar("SFR_SEG_SIZE", sz);
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
    // 14. Segment Property
    CAR_DBL2D v;
    v.SetSize(SFR2S_NPROP, *nss, 0.0);
    std::vector<int> vNSTRPTS;

    for (int i = 0; i < *nss; ++i)
    {
      int segnum = i + 1;
      v.at(SFR2S_ICALC, i)   = ForElement(iseg, 1, segnum, 4);
      vNSTRPTS.push_back(ForElement(iseg, 2, segnum, 4));
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

    // Set up the database field names to write to
    std::vector<CStr> vFldNames;
    for (int j = 0; j < 8; ++j)
    {
      CStr s;
      s.Format("XCPT%d", j+1);
      vFldNames.push_back(s);
    }
    for (int j = 0; j < 8; ++j)
    {
      CStr s;
      s.Format("ZCPT%d", j+1);
      vFldNames.push_back(s);
    }

    // Write to the SFRXsect table
    Db::db()->BeginCacheMutipleRows("SFRXsect", "", 18, *nss);
    for (int i = 0; i < *nss; ++i)
    {
      int segnum = i + 1;

      Db::db()->AddValue("SPID", a_sp, i);
      Db::db()->AddValue("NSEG", segnum, i);
      // XCPT1 XCPT2 ... XCPT8
      // ZCPT1 ZCPT2 ... ZCPT8
      for (int j = 0; j < 16; ++j)
        Db::db()->AddValue(vFldNames.at(j), v.at(SFR2S_XSECT+j, i), i);
    }
    Db::db()->FlushToDb();

    // Write to the SFRSegments table
    Db::db()->BeginCacheMutipleRows("SFRSegments", "", 27, *nss);    
    for (int i = 0; i < *nss; ++i)
    {
      int segnum = i + 1;

      Db::db()->AddValue("SPID", a_sp, i);
      Db::db()->AddValue("NSEG", segnum, i);
      Db::db()->AddValue("ICALC", v.at(SFR2S_ICALC, i), i);
      Db::db()->AddValue("OUTSEG", v.at(SFR2S_OUTSEG, i), i);
      Db::db()->AddValue("IUPSEG", v.at(SFR2S_IUPSEG, i), i);
      Db::db()->AddValue("IPRIOR", v.at(SFR2S_IPRIOR, i), i);
      Db::db()->AddValue("NSTRPTS", vNSTRPTS.at(i), i);
      Db::db()->AddValue("FLOW", v.at(SFR2S_FLOW, i), i);
      Db::db()->AddValue("RUNOFF", v.at(SFR2S_RUNOFF, i), i);
      Db::db()->AddValue("ETSW", v.at(SFR2S_ETSW, i), i);
      Db::db()->AddValue("PPTSW", v.at(SFR2S_PPTSW, i), i);
      Db::db()->AddValue("ROUGHCH", v.at(SFR2S_ROUGHCH, i), i);
      Db::db()->AddValue("ROUGHBK", v.at(SFR2S_ROUGHBK, i), i);
      Db::db()->AddValue("CDPTH", v.at(SFR2S_CDPTH, i), i);
      Db::db()->AddValue("FDPTH", v.at(SFR2S_FDPTH, i), i);
      Db::db()->AddValue("AWDTH", v.at(SFR2S_AWDPTH, i), i);
      Db::db()->AddValue("BWDTH", v.at(SFR2S_BWDTH, i), i);
      Db::db()->AddValue("HCOND1", v.at(SFR2S_HCOND1, i), i);
      Db::db()->AddValue("THICKM1", v.at(SFR2S_THICKM1, i), i);
      Db::db()->AddValue("ELEVUP", v.at(SFR2S_ELEVUP, i), i);
      Db::db()->AddValue("WIDTH1", v.at(SFR2S_WIDTH1, i), i);
      Db::db()->AddValue("DEPTH1", v.at(SFR2S_DEPTH1, i), i);
      Db::db()->AddValue("HCOND2", v.at(SFR2S_HCOND2, i), i);
      Db::db()->AddValue("THICKM2", v.at(SFR2S_THICKM2, i), i);
      Db::db()->AddValue("ELEVDN", v.at(SFR2S_ELEVDN, i), i);
      Db::db()->AddValue("WIDTH2", v.at(SFR2S_WIDTH2, i), i);
      Db::db()->AddValue("DEPTH2", v.at(SFR2S_DEPTH2, i), i);
    }
    Db::db()->FlushToDb();



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

      // Write to the SFRFlowTabl table
      Db::db()->BeginCacheMutipleRows("SFRFlowTab", "", 5, v.GetSize2());
      for (int i = 0; i < v.GetSize2(); ++i)
      {
        Db::db()->AddValue("SPID", static_cast<int>(v.at(1, i)), i);
        Db::db()->AddValue("NSEG", static_cast<int>(v.at(0, i)), i);
        Db::db()->AddValue("FLOWTAB", v.at(2, i), i);
        Db::db()->AddValue("DPTHTAB", v.at(3, i), i);
        Db::db()->AddValue("WDTHTAB", v.at(4, i), i);
      }
      Db::db()->FlushToDb();
    }
  }
} // ExportSFRLine6
//------------------------------------------------------------------------------
/// \brief Exports the GAGE package to the database
//------------------------------------------------------------------------------
static void ExportGAG(MfData::MfPackage *a_pGAG)
{
  using namespace MfData::Packages;
  const int *igglst(0), *numgage(0);
  if (a_pGAG->GetField(GAGpack::IGGLST, &igglst) && igglst &&
      a_pGAG->GetField(GAGpack::NUMGAGE, &numgage) && numgage)
  {
    ExportGAGSFR(a_pGAG);
    ExportGAGLAK(a_pGAG);
  }
} // ExportGAG
//------------------------------------------------------------------------------
/// \brief Writes out gages that are parts of a SFR stream to the database
//------------------------------------------------------------------------------
static bool ExportGAGSFR(MfData::MfPackage *a_pGAG)
{
  using namespace MfData::Packages;
  const int *igglst(0), *numgage(0);
  if (a_pGAG->GetField(GAGpack::IGGLST, &igglst) && igglst &&
      a_pGAG->GetField(GAGpack::NUMGAGE, &numgage) && numgage)
  {
    int i, count(0);
    int nNumSFR(0);

    // Get the number of gages for the SFR streams
    for (i=0; i < *numgage; ++i)
    {
      if (igglst[i*4 + 0] >= 0)
        ++nNumSFR;
    }

    // Write only the gages for the SFR streams
    if (nNumSFR > 0)
    {
      Db::db()->BeginCacheMutipleRows("GAGSFR", "", 4, nNumSFR);
      for (i=0; i < *numgage; ++i)
      {
        if (igglst[i*4 + 0] >= 0)
        {
          Db::db()->AddValue("GAGESEG", igglst[i*4 + 0], count);
          Db::db()->AddValue("GAGERCH", igglst[i*4 + 1], count);
          Db::db()->AddValue("UNIT",    igglst[i*4 + 2], count);
          Db::db()->AddValue("OUTTYPE", igglst[i*4 + 3], count);
          ++count;
        }
      }
      Db::db()->FlushToDb();
    }
    return true;
  }
  return false;
} // ExportGAGSFR
//------------------------------------------------------------------------------
/// \brief Writes out gages that are parts of a LAK lake to the database
//------------------------------------------------------------------------------
static bool ExportGAGLAK(MfData::MfPackage *a_pGAG)
{
  using namespace MfData::Packages;
  const int *igglst(0), *numgage(0);
  if (a_pGAG->GetField(GAGpack::IGGLST, &igglst) && igglst &&
      a_pGAG->GetField(GAGpack::NUMGAGE, &numgage) && numgage)
  {
    int i, count(0);
    int nNumLAK(0);

    // Get the number of gages for the LAK lakes
    for (i=0; i < *numgage; ++i)
    {
      if (igglst[i*4 + 0] < 0)
        ++nNumLAK;
    }

    // Write only the gages for the LAK lakes
    if (nNumLAK > 0)
    {
      Db::db()->BeginCacheMutipleRows("GAGLAK", "", 3, nNumLAK);
      for (i=0; i < *numgage; ++i)
      {
        if (igglst[i*4 + 0] < 0)
        {
          Db::db()->AddValue("LAKE",     igglst[i*4 + 0], count);
          // Don't use igglst[i*4 + 1] for LAK
          Db::db()->AddValue("UNIT",    igglst[i*4 + 2], count);
          Db::db()->AddValue("OUTTYPE", igglst[i*4 + 3], count);
          ++count;
        }
      }
      Db::db()->FlushToDb();
    }
    return true;
  }
  return false;
} // ExportGAGLAK
//------------------------------------------------------------------------------
/// \brief Creates a map with the table and field names with a key string lookup
//------------------------------------------------------------------------------
static std::map<CStr, std::vector<CStr> > &GetTableMap()
{
  static std::map<CStr, std::vector<CStr> > m_map;
  if (m_map.empty())
  {
    std::vector<CStr> p;
    // EVT
    p.push_back("EVTArrays");
    p.push_back("IEVT");
    p.push_back("EVTArrayMult");
    p.push_back("AM_IEVT");
    m_map.insert(std::make_pair(ARR_EVT_LAY, p));
    p[1] = "SURF";
    p[3] = "AM_SURF";
    m_map.insert(std::make_pair(ARR_EVT_SURF, p));
    p[1] = "EVTR";
    p[3] = "AM_EVTR";
    m_map.insert(std::make_pair(ARR_EVT_RATE, p));
    p[1] = "EXPD";
    p[3] = "AM_EXPD";
    m_map.insert(std::make_pair(ARR_EVT_EXT, p));
    // RCH
    p[0] = "RCHArrays";
    p[1] = "RECH";
    p[2] = "RCHArrayMult";
    p[3] = "AM_RECH";
    m_map.insert(std::make_pair(ARR_RCH_RCH, p));
    p[1] = "IRCH";
    p[3] = "AM_IRCH";
    m_map.insert(std::make_pair(ARR_RCH_LAY, p));
    // LPF
    p[0] = "LPFProperties";
    p[1] = "HK";
    p[2] = "LPFLayers";
    p[3] = "AM_HK";
    m_map.insert(std::make_pair(ARR_LPF_HK, p));
    p[1] = "HANI";
    p[3] = "AM_HANI";
    m_map.insert(std::make_pair(ARR_LPF_HANI, p));
    p[1] = "VKA";
    p[3] = "AM_VKA";
    m_map.insert(std::make_pair(ARR_LPF_VK, p));
    m_map.insert(std::make_pair(ARR_LPF_VANI, p));
    p[1] = "Ss";
    p[3] = "AM_Ss";
    m_map.insert(std::make_pair(ARR_LPF_SS, p));
    p[1] = "Sy";
    p[3] = "AM_Sy";
    m_map.insert(std::make_pair(ARR_LPF_SY, p));
    p[1] = "WETDRY";
    p[3] = "AM_WETDRY";
    m_map.insert(std::make_pair(ARR_LPF_WET, p));
    p[1] = "VKCB";
    p[3] = "AM_VKCB";
    m_map.insert(std::make_pair(ARR_LPF_VKCBD, p));
    // BCF
    p[0] = "BCFProperties";
    p[1] = "HY";
    p[2] = "BCFLayers";
    p[3] = "AM_HY";
    m_map.insert(std::make_pair(ARR_BCF_HY, p));
    p[1] = "Tran";
    p[3] = "AM_Tran";
    m_map.insert(std::make_pair(ARR_BCF_TRAN, p));
    p[1] = "Vcont";
    p[3] = "AM_Vcont";
    m_map.insert(std::make_pair(ARR_BCF_VCONT, p));
    p[1] = "Sf1";
    p[3] = "AM_Sf1";
    m_map.insert(std::make_pair(ARR_BCF_SF1, p));
    p[1] = "Sf2";
    p[3] = "AM_Sf2";
    m_map.insert(std::make_pair(ARR_BCF_SF2, p));
    p[1] = "WETDRY";
    p[3] = "AM_WETDRY";
    m_map.insert(std::make_pair(ARR_BCF_WET, p));
    // BAS
    p[0] = "Basic";
    p[1] = "STRT";
    p[3] = "AM_STRT";
    p[2] = "BasicArrayMult";
    m_map.insert(std::make_pair(ARR_BAS_SHEAD, p));
    p[1] = "IBOUND";
    p[3] = "AM_IBOUND";
    m_map.insert(std::make_pair(ARR_BAS_IBND, p));
    // DIS
    p[0] = "TopElev";
    p[1] = "TopElev";
    p[2] = "DISLayers";
    p[3] = "AM_TopElev";
    m_map.insert(std::make_pair(ARR_DIS_TOP, p));
    p[0] = "BotmElev";
    p[1] = "BotmElev";
    p[2] = "DISLayers";
    p[3] = "AM_BotmElev";
    m_map.insert(std::make_pair(ARR_DIS_BOT, p));
    p[0] = "BotmElev";
    p[1] = "BotmElevCBD";
    p[2] = "DISLayers";
    p[3] = "AM_BotmElevCBD";
    m_map.insert(std::make_pair(ARR_DIS_VCB, p));
    // HUF
    p[0] = "HUFUnitProps";
    p[1] = "Top";
    p[2] = "HUFUnits";
    p[3] = "AM_Top";
    m_map.insert(std::make_pair("TOP ELEVATN:", p));
    p[0] = "HUFUnitProps";
    p[1] = "Thick";
    p[2] = "HUFUnits";
    p[3] = "AM_Thick";
    m_map.insert(std::make_pair("THICKNESS:", p));
    p[0] = "HUFWetDry";
    p[1] = "WETDRY";
    p[2] = "HUFLayers";
    p[3] = "AM_WETDRY";
    m_map.insert(std::make_pair("HUF WETDRY PARAMETER", p));
    // multipliers and zones
    p[0] = "Multipliers";
    p[1] = "RMLT";
    p[2] = "MultNames";
    p[3] = "ArrayMult";
    m_map.insert(std::make_pair("MULT. ARRAY:", p));
    p[0] = "Zones";
    p[1] = "IZON";
    p[2] = "ZoneNames";
    p[3] = "ArrayMult";
    m_map.insert(std::make_pair("ZONE ARRAY:", p));
    // ETS
    p[0] = "ETSArrays";
    p[1] = "IETS";
    p[2] = "ETSArrayMult";
    p[3] = "AM_IETS";
    m_map.insert(std::make_pair(ARR_ETS_LAY, p));
    p[1] = "ETSS";
    p[3] = "AM_ETSS";
    m_map.insert(std::make_pair(ARR_ETS_SURF, p));
    p[1] = "ETSR";
    p[3] = "AM_ETSR";
    m_map.insert(std::make_pair(ARR_ETS_RATE, p));
    p[1] = "ETSX";
    p[3] = "AM_ETSX";
    m_map.insert(std::make_pair(ARR_ETS_EXT, p));
    p[0] = "ETSSegArrays";
    p[1] = "PXDP";
    p[2] = "ETSSegArrayMult";
    p[3] = "AM_PXDP";
    m_map.insert(std::make_pair(ARR_ETS_PXDP, p));
    p[1] = "PETM";
    p[3] = "AM_PETM";
    m_map.insert(std::make_pair(ARR_ETS_PETM, p));
    // UZF
    p[0] = "UZFArrays";
    p[1] = "IUZFBND";
    p[2] = "UZFArrayMult";
    p[3] = "AM_IUZFBND";
    m_map.insert(std::make_pair(ARR_UZF_UBND, p));
    p[1] = "IRUNBND";
    p[3] = "AM_IRUNBND";
    m_map.insert(std::make_pair(ARR_UZF_RBND, p));
    p[1] = "VKS";
    p[3] = "AM_VKS";
    m_map.insert(std::make_pair(ARR_UZF_VKS, p));
    p[1] = "EPS";
    p[3] = "AM_EPS";
    m_map.insert(std::make_pair(ARR_UZF_EPS, p));
    p[1] = "THTS";
    p[3] = "AM_THTS";
    m_map.insert(std::make_pair(ARR_UZF_THTS, p));
    p[1] = "THTI";
    p[3] = "AM_THTI";
    m_map.insert(std::make_pair(ARR_UZF_THTI, p));
    p[0] = "UZFStressArrays";
    p[1] = "FINF";
    p[2] = "UZFStressArrayMult";
    p[3] = "AM_FINF";
    m_map.insert(std::make_pair(ARR_UZF_RCH, p));
    p[1] = "PET";
    p[3] = "AM_PET";
    m_map.insert(std::make_pair(ARR_UZF_ET, p));
    p[1] = "EXTDP";
    p[3] = "AM_EXTDP";
    m_map.insert(std::make_pair(ARR_UZF_EXT, p));
    p[1] = "EXTWC";
    p[3] = "AM_EXTWC";
    m_map.insert(std::make_pair(ARR_UZF_EXTWC, p));
  }
  return m_map;
} // GetTableMap
//------------------------------------------------------------------------------
/// \brief Gets the names of the table and field for a given string related
///  to a MODFLOW array.
//------------------------------------------------------------------------------
static bool GetArrayTableAndField (const CStr &a_str,
                                   CStr &a_table,
                                   CStr &a_field)
{
  a_table = a_field = "";

  bool retval(false);
  CStr str(a_str);
  // do a check for the HUF
  if (str.find("TOP ELEVATN:") != -1)
    str = "TOP ELEVATN:";
  else if (str.find("THICKNESS:") != -1)
    str = "THICKNESS:";

  std::map<CStr, std::vector<CStr> > &tabMap(GetTableMap());
  std::map<CStr, std::vector<CStr> >::iterator it;
  it = tabMap.find(str);
  if (it != tabMap.end())
  {
    retval = true;
    a_table = it->second.at(0);
    a_field = it->second.at(1);
  }
  return retval;
} // GetArrayTableAndField
//------------------------------------------------------------------------------
/// \brief Gets the names of the table and field for a given string related
///  to a MODFLOW array multiplier.
//------------------------------------------------------------------------------
static bool GetArrayMultTableAndField (const CStr &a_str,
                                       CStr &a_table,
                                       CStr &a_field)
{
  a_table = a_field = "";

  bool retval(false);
  CStr str(a_str);
  // do a check for the HUF
  if (str.find("TOP ELEVATN:") != -1)
    str = "TOP ELEVATN:";
  else if (str.find("THICKNESS:") != -1)
    str = "THICKNESS:";

  std::map<CStr, std::vector<CStr> > &tabMap(GetTableMap());
  std::map<CStr, std::vector<CStr> >::iterator it;
  it = tabMap.find(str);
  if (it != tabMap.end())
  {
    retval = true;
    a_table = it->second.at(2);
    a_field = it->second.at(3);
  }
  return retval;
} // GetArrayMultTableAndField
//------------------------------------------------------------------------------
/// \brief Tells whether a particular table uses the stress period field
//------------------------------------------------------------------------------
static bool TableUsesStressPeriod (const CStr &a_str)
{
  bool rval(false);

  if (a_str == "EVTArrays" ||
      a_str == "RCHArrays" ||
      a_str == "ETSArrays" ||
      a_str == "ETSSegArrays" ||
      a_str == "UZFStressArrays")
    rval = true;
  return rval;
} // TableUsesStressPeriod
//------------------------------------------------------------------------------
/// \brief Fills in the query string
//------------------------------------------------------------------------------
static bool GetQueryString (const CStr &a_table,
                            int a_sp,
                            int a_etsSegId,
                            int a_startCellId,
                            int a_nCells,
                            CStr &a_queryStr)
{
  bool rval(false);
  if (a_table == "EVTArrays" ||
      a_table == "RCHArrays" ||
      a_table == "ETSArrays" ||
      a_table == "UZFStressArrays")
  {
    rval = true;
    a_queryStr.Format("SPID = %d", a_sp);
  }
  else if (a_table == "ETSSegArrays")
  {
    rval = true;
    a_queryStr.Format("SPID = %d AND SEGID = %d", a_sp, a_etsSegId);
  }
  else if (a_table == "BCFProperties" ||
           a_table == "LPFProperties" ||
           a_table == "Basic" ||
           a_table == "TopElev" ||
           a_table == "BotmElev" ||
           a_table == "HUFWetDry")
  {
    rval = true;
    a_queryStr.Format("IJK > %d AND IJK < %d", a_startCellId-1,
                      a_startCellId+a_nCells);
  }
  else if (a_table == "HUFUnitProps")
  {
    rval = true;
    // we put the HGUID into the stress period variable
    a_queryStr.Format("HGUID = %d", a_sp);
  }
  else if (a_table == "UZFArrays")
  {
    rval = true;
    a_queryStr.Format("IJ > %d AND IJ < %d", a_startCellId-1,
                      a_startCellId+a_nCells);
  }

  return rval;
} // GetQueryString
//------------------------------------------------------------------------------
/// \brief Gets the file global
//------------------------------------------------------------------------------
static std::map<CStr, std::vector<int> > &iGetBcCellIds ()
{
  static std::map<CStr, std::vector<int> > m_ids;
  return (m_ids);
} // iGetBcCellIds
//------------------------------------------------------------------------------
/// \brief Gets the file global
//------------------------------------------------------------------------------
static std::map<CStr, std::vector<int> > &iGetBcIface ()
{
  static std::map<CStr, std::vector<int> > m_ids;
  return (m_ids);
} // iGetBcIface
//------------------------------------------------------------------------------
/// \brief Gets the file global
//------------------------------------------------------------------------------
static std::map<CStr, std::vector<int> > &iGetBcCellGrp ()
{
  static std::map<CStr, std::vector<int> > m_ids;
  return (m_ids);
} // iGetBcCellGrp
//------------------------------------------------------------------------------
/// \brief Gets the index of the bc for use in the arrays of data
//------------------------------------------------------------------------------
static int iGetBcIndex (const CStr &a_type,
                        int a_cellid,
                        int a_sp,
                        std::vector<int> &a_cellids,
                        std::vector<int> &a_iface,
                        std::vector<int> &a_cellgrp)
{
  // there is a hash_map for each type of BC (RIV, DRN...)
  stdext::hash_map<int, VEC_INT_PAIR> &hMap(iGetBcIdxMap()[a_type]);
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
static std::map<CStr, stdext::hash_map<int, VEC_INT_PAIR> > &iGetBcIdxMap ()
{
  static std::map<CStr, stdext::hash_map<int, VEC_INT_PAIR> > m_idx;
  return m_idx;
} // iGetBcIdxMap

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST
#include <io.h>
#include <sstream>
#include <sys/stat.h>
#include <private/MfData/MfExport/private/ExpGeoDb.t.h>
#include <private/MfData/Packages/MfPackStrings.h>
#include <private/MfLibAsserts.h>

#define GDBTOL 1e-4

class DbMock : public Db
{
public:
  DbMock() :
      m_nFields(-1), m_nRow(-1), m_startCache(0)
  {
    if (!m_suspend)
      CreateSingle();
  }
  virtual ~DbMock()
  {
    if (!m_suspend)
    {
      m_suspend = true;
      Destroy();
      m_suspend = false;
    }
  }

  void CheckStartCache ()
  {
    if (m_startCache > 0)
    {
      TS_FAIL("Flush before you start cacheing");
      return;
    }
    else
    {
      m_iVals.clear();
      m_sVals.clear();
      m_fVals.clear();
      m_dVals.clear();
      m_nFields = m_nRow = -1;
    }
    m_startCache++;
  }

  virtual int BeginCache(const char *a_table,
                         const char *a_query)
  {
    CheckStartCache();
    m_table = a_table;
    m_query = a_query;
    return 1;
  }
  virtual int BeginCacheMutipleRows(const char *a_table,
                                    const char *a_query,
                                    int a_nFields,
                                    int a_nRows)
  {
    CheckStartCache();
    m_table = a_table;
    m_query = a_query;
    m_nFields = a_nFields;
    m_nRow = a_nRows;
    return 1;
  }

  virtual int FlushToDb()
  {
    m_startCache--;
    return 1;
  }

  template <class T>
  int AddValueT(const char *a_field,
                T a_val,
                int a_row)
  {
    CStr f(a_field);

    if (GetMap(a_val).find(f) == GetMap(a_val).end())
    {
      if (m_nFields != -1 &&
          (int)GetMap(a_val).size() == m_nFields)
      {
        CStr msg;
        msg.Format("Max # of fields exceeded: %s", f);
        TS_FAIL(msg.c_str());
        return 0;
      }
    }

    if (a_row < 0 && !GetMap(a_val)[f].empty())
    {
      TS_FAIL("Number of rows exceeded");
      return 0;
    }
    else if (a_row > -1 && GetMap(a_val)[f].empty())
    {
      GetMap(a_val)[f].assign(m_nRow, a_val);
    }
    else if (a_row < 0)
      GetMap(a_val)[f].push_back(a_val);

    int row(0);
    if (a_row > 0)
      row = a_row;
    try
    {
      GetMap(a_val)[f].at(row) = a_val;
    }
    catch (std::out_of_range &)
    {
      TS_FAIL("Number of rows exceeded");
      return 0;
    }
    return 1;
  }

  virtual int AddValue(const char *a_field,
                       const char *a_val,
                       int a_row=-1)
  { return(AddValueT(a_field, a_val, a_row)); }
  virtual int AddValue(const char *a_field,
                       int a_val,
                       int a_row=-1)
  { return(AddValueT(a_field, a_val, a_row)); }
  virtual int AddValue(const char *a_field,
                       float a_val,
                       int a_row=-1)
  { return(AddValueT(a_field, a_val, a_row)); }
  virtual int AddValue(const char *a_field,
                       double a_val,
                       int a_row=-1)
  { return(AddValueT(a_field, a_val, a_row)); }

  virtual int AddValues(std::vector<CStr> &a_fields,
                        std::vector<const int *> &a_vals)
  { return AddValuesT(a_fields, a_vals); }
  virtual int AddValues(std::vector<CStr> &a_fields,
                        std::vector<const float *> &a_vals)
  { return AddValuesT(a_fields, a_vals); }
  virtual int AddValues(std::vector<CStr> &a_fields,
                        std::vector<const double *> &a_vals)
  { return AddValuesT(a_fields, a_vals); }
  template <class T>
  int AddValuesT(std::vector<CStr> &a_fields,
                 std::vector<const T *> &a_vals)
  {
    int rval(1);
    size_t i;
    for (i=0; i<a_fields.size(); i++)
    {
      if (a_vals[i])
        rval = AddValue(a_fields[i], *a_vals[i]);
    }
    return rval;
  }

  std::map<CStr, std::vector<int> > &GetMap(int a_)
  {
    a_ = 0; return m_iVals;
  }
  std::map<CStr, std::vector<float> > &GetMap(float a_)
  {
    a_ = 0; return m_fVals;
  }
  std::map<CStr, std::vector<double> > &GetMap(double a_)
  {
    a_ = 0; return m_dVals;
  }
  std::map<CStr, std::vector<CStr> > &GetMap(const char *a_)
  {
    size_t l(strlen(a_));
    l++;
    return m_sVals;
  }

  static bool m_suspend;
  int  m_nFields, m_nRow, m_startCache;
  CStr m_table, m_query;
  std::map<CStr, std::vector<int> > m_iVals;
  std::map<CStr, std::vector<float> > m_fVals;
  std::map<CStr, std::vector<double> > m_dVals;
  std::map<CStr, std::vector<CStr> > m_sVals;

protected:
  virtual void CreateSingle()
  {
    m_suspend = true;
    if (!m_single)
      m_single = new DbMock();
    m_suspend = false;
  }
};
bool DbMock::m_suspend = false;

//------------------------------------------------------------------------------
void ExpGeoDbT::testExportPackage ()
{
  MfData::MfGlobal g(2,5,3,1,4,6,0);
  MfData::MfPackage p("dis");
  MfData::MfGlobal *gp(0);
  MfData::MfPackage *pp(0);

  ExpGeoDb m;
  TS_ASSERT(!m.ExportPackage(gp, pp));
  TS_ASSERT(!m.ExportPackage(&g, pp));
  TS_ASSERT(!m.ExportPackage(gp, &p));
  TS_ASSERT(m.ExportPackage(&g, &p));
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportWEL ()
{
  int nWells(15), nDataFields(6), nAux(1);
  Real d[75]  = {1.0000000,9.0000000,8.0000000,-432000,0.00000000,1.0000000,
                 9.0000000,10.000000,-432000,0.00000000,1.0000000,9.0000000,
                 12.000000,-432000,0.00000000,1.0000000,9.0000000,14.000000,
                 -432000,0.00000000,1.0000000,11.000000,8.0000000,-432000,
                 0.00000000,1.0000000,11.000000,10.000000,-432000,0.00000000,
                 1.0000000,11.000000,12.000000,-432000,0.00000000,1.0000000,
                 11.000000,14.000000,-432000,0.00000000,1.0000000,13.000000,
                 8.0000000,-432000,0.00000000,1.0000000,13.000000,10.000000,
                 -432000,0.00000000,1.0000000,13.000000,12.000000,-432000,
                 0.00000000,1.0000000,13.000000,14.000000,-432000,0.00000000,
                 2.0000000,4.0000000,6.0000000,-432000,0.00000000,2.0000000,
                 6.0000000,12.000000,-432000,0.00000000,3.0000000,5.0000000,
                 11.000000,-5,0.00000000};
  char c[80] = {73,70,65,67,69,32,32,32,32,32,32,32,32,32,32,32,0,0,0,0,
                0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                0,0,0,0,0};

  MfData::MfPackage pack(MfData::Packages::WEL);
  pack.SetField(MfData::Packages::ListPack::MAXBC, &nWells);
  pack.SetField(MfData::Packages::ListPack::NUMBC, &nWells);
  pack.SetField(MfData::Packages::ListPack::NUMFIELDS, &nDataFields);
  pack.SetField(MfData::Packages::ListPack::NAUX, &nAux);
  pack.SetField(MfData::Packages::ListPack::DATA, &d[0]);
  pack.SetField(MfData::Packages::ListPack::AUX, &c[0]);

  const int *nBcs, *nAux1, *nDataFields1;
  int nFields;
  const Real *data;
  std::vector<CStr> names;
  MfData::Packages::GetBcData(&pack, pack.PackageName(), &nBcs, &nFields,
                              &nAux1, &data, &nDataFields1, names);
  TS_ASSERT(nBcs);
  TS_ASSERT(nDataFields1);
  TS_ASSERT(data);
  TS_ASSERT(!names.empty());
  TS_ASSERT(*nBcs == 15);
  TS_ASSERT(*nDataFields1 == 6);
  TS_ASSERT(nFields == 5);
  TS_ASSERT(names.at(0) == "k");
  TS_ASSERT(names.at(1) == "i");
  TS_ASSERT(names.at(2) == "j");
  TS_ASSERT(names.at(3) == "Q");
  TS_ASSERT(names.at(4) == "IFACE");
  TS_ASSERT(data == d);
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportRIV ()
{
  int nRiv(5), nDataFields(9), nAux(3);
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
  pack.SetField(MfData::Packages::ListPack::MAXBC, &nRiv);
  pack.SetField(MfData::Packages::ListPack::NUMBC, &nRiv);
  pack.SetField(MfData::Packages::ListPack::NUMFIELDS, &nDataFields);
  pack.SetField(MfData::Packages::ListPack::NAUX, &nAux);
  pack.SetField(MfData::Packages::ListPack::DATA, &d[0]);
  pack.SetField(MfData::Packages::ListPack::AUX, &c[0]);

  const int *nBcs, *nDataFields1, *nAux1;
  int nFields;
  const Real *data;
  std::vector<CStr> names;
  MfData::Packages::GetBcData(&pack, pack.PackageName(), &nBcs, &nFields,
                              &nAux1, &data, &nDataFields1, names);
  TS_ASSERT(nBcs);
  TS_ASSERT(nDataFields1);
  TS_ASSERT(data);
  TS_ASSERT(*nBcs == 5);
  TS_ASSERT(*nDataFields1 == 9);
  TS_ASSERT(nFields == 9);
  TS_ASSERT(names.at(0) == "k");
  TS_ASSERT(names.at(1) == "i");
  TS_ASSERT(names.at(2) == "j");
  TS_ASSERT(names.at(3) == "Stage");
  TS_ASSERT(names.at(4) == "Cond");
  TS_ASSERT(names.at(5) == "Rbot");
  TS_ASSERT(names.at(6) == "IFACE");
  TS_ASSERT(names.at(7) == "CONDFACT");
  TS_ASSERT(names.at(8) == "CELLGRP");
  TS_ASSERT(data == d);
}
//------------------------------------------------------------------------------
static void GetMfU2drelStrs (std::vector<CStr> &a_strs,
                             std::vector<CStr> &a_tabs,
                             std::vector<CStr> &a_fields,
                             std::vector<CStr> &a_tab1,
                             std::vector<CStr> &a_field1)
{
  a_strs.resize(0);
  a_strs.push_back(ARR_EVT_LAY);
  a_strs.push_back(ARR_EVT_SURF);
  a_strs.push_back(ARR_EVT_RATE);
  a_strs.push_back(ARR_EVT_EXT);
  a_strs.push_back(ARR_RCH_RCH);
  a_strs.push_back(ARR_RCH_LAY);
  a_strs.push_back(ARR_LPF_HK);
  a_strs.push_back(ARR_LPF_HANI);
  a_strs.push_back(ARR_LPF_VK);
  a_strs.push_back(ARR_LPF_VANI);
  a_strs.push_back(ARR_LPF_SS);
  a_strs.push_back(ARR_LPF_SY);
  a_strs.push_back(ARR_LPF_WET);
  a_strs.push_back(ARR_LPF_VKCBD);
  a_strs.push_back(ARR_BCF_HY);
  a_strs.push_back(ARR_BCF_TRAN);
  a_strs.push_back(ARR_BCF_VCONT);
  a_strs.push_back(ARR_BCF_SF1);
  a_strs.push_back(ARR_BCF_SF2);
  a_strs.push_back(ARR_BCF_WET);
  a_strs.push_back(ARR_BAS_SHEAD);
  a_strs.push_back(ARR_BAS_IBND);
  a_strs.push_back(ARR_DIS_TOP);
  a_strs.push_back(ARR_DIS_BOT);
  a_strs.push_back(ARR_DIS_VCB);
  a_strs.push_back("MULT. ARRAY:");
  a_strs.push_back("ZONE ARRAY:");
  a_strs.push_back("TOP ELEVATN:");
  a_strs.push_back("THICKNESS:");
  a_strs.push_back("HUF WETDRY PARAMETER");
  a_strs.push_back(ARR_ETS_LAY);
  a_strs.push_back(ARR_ETS_SURF);
  a_strs.push_back(ARR_ETS_RATE);
  a_strs.push_back(ARR_ETS_EXT);
  a_strs.push_back(ARR_ETS_PXDP);
  a_strs.push_back(ARR_ETS_PETM);
  a_strs.push_back(ARR_UZF_UBND);
  a_strs.push_back(ARR_UZF_RBND);
  a_strs.push_back(ARR_UZF_VKS);
  a_strs.push_back(ARR_UZF_EPS);
  a_strs.push_back(ARR_UZF_THTS);
  a_strs.push_back(ARR_UZF_THTI);
  a_strs.push_back(ARR_UZF_RCH);
  a_strs.push_back(ARR_UZF_ET);
  a_strs.push_back(ARR_UZF_EXT);
  a_strs.push_back(ARR_UZF_EXTWC);

  a_tabs.resize(0);
  a_tabs.push_back("EVTArrays");
  a_tabs.push_back("EVTArrays");
  a_tabs.push_back("EVTArrays");
  a_tabs.push_back("EVTArrays");
  a_tabs.push_back("RCHArrays");
  a_tabs.push_back("RCHArrays");
  a_tabs.push_back("LPFProperties");
  a_tabs.push_back("LPFProperties");
  a_tabs.push_back("LPFProperties");
  a_tabs.push_back("LPFProperties");
  a_tabs.push_back("LPFProperties");
  a_tabs.push_back("LPFProperties");
  a_tabs.push_back("LPFProperties");
  a_tabs.push_back("LPFProperties");
  a_tabs.push_back("BCFProperties");
  a_tabs.push_back("BCFProperties");
  a_tabs.push_back("BCFProperties");
  a_tabs.push_back("BCFProperties");
  a_tabs.push_back("BCFProperties");
  a_tabs.push_back("BCFProperties");
  a_tabs.push_back("Basic");
  a_tabs.push_back("Basic");
  a_tabs.push_back("TopElev");
  a_tabs.push_back("BotmElev");
  a_tabs.push_back("BotmElev");
  a_tabs.push_back("Multipliers");
  a_tabs.push_back("Zones");
  a_tabs.push_back("HUFUnitProps");
  a_tabs.push_back("HUFUnitProps");
  a_tabs.push_back("HUFWetDry");
  a_tabs.push_back("ETSArrays");
  a_tabs.push_back("ETSArrays");
  a_tabs.push_back("ETSArrays");
  a_tabs.push_back("ETSArrays");
  a_tabs.push_back("ETSSegArrays");
  a_tabs.push_back("ETSSegArrays");
  a_tabs.push_back("UZFArrays");
  a_tabs.push_back("UZFArrays");
  a_tabs.push_back("UZFArrays");
  a_tabs.push_back("UZFArrays");
  a_tabs.push_back("UZFArrays");
  a_tabs.push_back("UZFArrays");
  a_tabs.push_back("UZFStressArrays");
  a_tabs.push_back("UZFStressArrays");
  a_tabs.push_back("UZFStressArrays");
  a_tabs.push_back("UZFStressArrays");

  a_fields.resize(0);
  a_fields.push_back("IEVT");
  a_fields.push_back("SURF");
  a_fields.push_back("EVTR");
  a_fields.push_back("EXPD");
  a_fields.push_back("RECH");
  a_fields.push_back("IRCH");
  a_fields.push_back("HK");
  a_fields.push_back("HANI");
  a_fields.push_back("VKA");
  a_fields.push_back("VKA");
  a_fields.push_back("Ss");
  a_fields.push_back("Sy");
  a_fields.push_back("WETDRY");
  a_fields.push_back("VKCB");
  a_fields.push_back("HY");
  a_fields.push_back("Tran");
  a_fields.push_back("Vcont");
  a_fields.push_back("Sf1");
  a_fields.push_back("Sf2");
  a_fields.push_back("WETDRY");
  a_fields.push_back("STRT");
  a_fields.push_back("IBOUND");
  a_fields.push_back("TopElev");
  a_fields.push_back("BotmElev");
  a_fields.push_back("BotmElevCBD");
  a_fields.push_back("RMLT");
  a_fields.push_back("IZON");
  a_fields.push_back("Top");
  a_fields.push_back("Thick");
  a_fields.push_back("WETDRY");
  a_fields.push_back("IETS");
  a_fields.push_back("ETSS");
  a_fields.push_back("ETSR");
  a_fields.push_back("ETSX");
  a_fields.push_back("PXDP");
  a_fields.push_back("PETM");
  a_fields.push_back("IUZFBND");
  a_fields.push_back("IRUNBND");
  a_fields.push_back("VKS");
  a_fields.push_back("EPS");
  a_fields.push_back("THTS");
  a_fields.push_back("THTI");
  a_fields.push_back("FINF");
  a_fields.push_back("PET");
  a_fields.push_back("EXTDP");
  a_fields.push_back("EXTWC");

  a_tab1.resize(0);
  a_tab1.push_back("EVTArrayMult");
  a_tab1.push_back("EVTArrayMult");
  a_tab1.push_back("EVTArrayMult");
  a_tab1.push_back("EVTArrayMult");
  a_tab1.push_back("RCHArrayMult");
  a_tab1.push_back("RCHArrayMult");
  a_tab1.push_back("LPFLayers");
  a_tab1.push_back("LPFLayers");
  a_tab1.push_back("LPFLayers");
  a_tab1.push_back("LPFLayers");
  a_tab1.push_back("LPFLayers");
  a_tab1.push_back("LPFLayers");
  a_tab1.push_back("LPFLayers");
  a_tab1.push_back("LPFLayers");
  a_tab1.push_back("BCFLayers");
  a_tab1.push_back("BCFLayers");
  a_tab1.push_back("BCFLayers");
  a_tab1.push_back("BCFLayers");
  a_tab1.push_back("BCFLayers");
  a_tab1.push_back("BCFLayers");
  a_tab1.push_back("BasicArrayMult");
  a_tab1.push_back("BasicArrayMult");
  a_tab1.push_back("DISLayers");
  a_tab1.push_back("DISLayers");
  a_tab1.push_back("DISLayers");
  a_tab1.push_back("MultNames");
  a_tab1.push_back("ZoneNames");
  a_tab1.push_back("HUFUnits");
  a_tab1.push_back("HUFUnits");
  a_tab1.push_back("HUFLayers");
  a_tab1.push_back("ETSArrayMult");
  a_tab1.push_back("ETSArrayMult");
  a_tab1.push_back("ETSArrayMult");
  a_tab1.push_back("ETSArrayMult");
  a_tab1.push_back("ETSSegArrayMult");
  a_tab1.push_back("ETSSegArrayMult");
  a_tab1.push_back("UZFArrayMult");
  a_tab1.push_back("UZFArrayMult");
  a_tab1.push_back("UZFArrayMult");
  a_tab1.push_back("UZFArrayMult");
  a_tab1.push_back("UZFArrayMult");
  a_tab1.push_back("UZFArrayMult");
  a_tab1.push_back("UZFStressArrayMult");
  a_tab1.push_back("UZFStressArrayMult");
  a_tab1.push_back("UZFStressArrayMult");
  a_tab1.push_back("UZFStressArrayMult");

  a_field1.resize(0);
  a_field1.push_back("AM_IEVT");
  a_field1.push_back("AM_SURF");
  a_field1.push_back("AM_EVTR");
  a_field1.push_back("AM_EXPD");
  a_field1.push_back("AM_RECH");
  a_field1.push_back("AM_IRCH");
  a_field1.push_back("AM_HK");
  a_field1.push_back("AM_HANI");
  a_field1.push_back("AM_VKA");
  a_field1.push_back("AM_VKA");
  a_field1.push_back("AM_Ss");
  a_field1.push_back("AM_Sy");
  a_field1.push_back("AM_WETDRY");
  a_field1.push_back("AM_VKCB");
  a_field1.push_back("AM_HY");
  a_field1.push_back("AM_Tran");
  a_field1.push_back("AM_Vcont");
  a_field1.push_back("AM_Sf1");
  a_field1.push_back("AM_Sf2");
  a_field1.push_back("AM_WETDRY");
  a_field1.push_back("AM_STRT");
  a_field1.push_back("AM_IBOUND");
  a_field1.push_back("AM_TopElev");
  a_field1.push_back("AM_BotmElev");
  a_field1.push_back("AM_BotmElevCBD");
  a_field1.push_back("ArrayMult");
  a_field1.push_back("ArrayMult");
  a_field1.push_back("AM_Top");
  a_field1.push_back("AM_Thick");
  a_field1.push_back("AM_WETDRY");
  a_field1.push_back("AM_IETS");
  a_field1.push_back("AM_ETSS");
  a_field1.push_back("AM_ETSR");
  a_field1.push_back("AM_ETSX");
  a_field1.push_back("AM_PXDP");
  a_field1.push_back("AM_PETM");
  a_field1.push_back("AM_IUZFBND");
  a_field1.push_back("AM_IRUNBND");
  a_field1.push_back("AM_VKS");
  a_field1.push_back("AM_EPS");
  a_field1.push_back("AM_THTS");
  a_field1.push_back("AM_THTI");
  a_field1.push_back("AM_FINF");
  a_field1.push_back("AM_PET");
  a_field1.push_back("AM_EXTDP");
  a_field1.push_back("AM_EXTWC");
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testGetArrayTableAndField ()
{
  CStr tab, field;
  std::vector<CStr> arrStrs, tabs, fields, tab1, field1;
  GetMfU2drelStrs(arrStrs, tabs, fields, tab1, field1);
  TS_ASSERT(GetTableMap().size() == arrStrs.size());
  for (size_t i=0; i<arrStrs.size(); i++)
  {
    TS_ASSERT(GetArrayTableAndField(arrStrs.at(i), tab, field));
    TS_ASSERT_EQUALS2(tab, tabs.at(i));
    TS_ASSERT_EQUALS2(field, fields.at(i));
    TS_ASSERT(GetArrayMultTableAndField(arrStrs.at(i), tab, field));
    TS_ASSERT_EQUALS2(tab, tab1.at(i));
    TS_ASSERT_EQUALS2(field, field1.at(i));
  }
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testTableUsesStressPeriod ()
{
  TS_ASSERT(TableUsesStressPeriod(CStr("EVTArrays")));
  TS_ASSERT(TableUsesStressPeriod(CStr("RCHArrays")));

  TS_ASSERT(!TableUsesStressPeriod(CStr("eVTArrays")));
  TS_ASSERT(!TableUsesStressPeriod(CStr("rCHArrays")));
  TS_ASSERT(!TableUsesStressPeriod(CStr("Not found")));
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testGetQueryString ()
{
  CStr tab, str;
  int sp(3), start(21), nCells(21);

  TS_ASSERT(!GetQueryString(tab, sp, -1, start, nCells, str));
  tab = "EVTArrays";
  TS_ASSERT(GetQueryString(tab, sp, -1, start, nCells, str));
  TS_ASSERT_EQUALS2(str, "SPID = 3");
  tab = "RCHArrays";
  TS_ASSERT(GetQueryString(tab, sp, -1, start, nCells, str));
  TS_ASSERT_EQUALS2(str, "SPID = 3");

  tab = "BCFProperties";
  TS_ASSERT(GetQueryString(tab, sp, -1, start, nCells, str));
  TS_ASSERT_EQUALS2(str, "IJK > 20 AND IJK < 42");
  tab = "LPFProperties";
  TS_ASSERT(GetQueryString(tab, sp, -1, start, nCells, str));
  TS_ASSERT_EQUALS2(str, "IJK > 20 AND IJK < 42");
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testGetSolverFieldsFromPackage ()
{
  CStr table;
  std::vector<CStr> iF, rF, dF;
  {
    MfData::MfPackage pack(MfData::Packages::SIP);
    GetSolverFieldsFromPackage(&pack, table, iF, rF, dF);
    TS_ASSERT(iF.size() == 4);
    TS_ASSERT(rF.size() == 3);
    TS_ASSERT(dF.size() == 0);
    TS_ASSERT_EQUALS2(table, "SIP");
    TS_ASSERT_EQUALS2(iF[0], "MXITER");
    TS_ASSERT_EQUALS2(iF[1], "NPARM");
    TS_ASSERT_EQUALS2(iF[2], "IPCALC");
    TS_ASSERT_EQUALS2(iF[3], "IPRSIP");
    TS_ASSERT_EQUALS2(rF[0], "ACCL");
    TS_ASSERT_EQUALS2(rF[1], "HCLOSE");
    TS_ASSERT_EQUALS2(rF[2], "WSEED");
  }
  {
    MfData::MfPackage pack(MfData::Packages::BCF);
    GetSolverFieldsFromPackage(&pack, table, iF, rF, dF);
    TS_ASSERT(iF.size() == 0);
    TS_ASSERT(rF.size() == 0);
    TS_ASSERT(dF.size() == 0);
    TS_ASSERT_EQUALS2(table, "");
  }
  {
    MfData::MfPackage pack(MfData::Packages::SOR);
    GetSolverFieldsFromPackage(&pack, table, iF, rF, dF);
    TS_ASSERT(iF.size() == 2);
    TS_ASSERT(rF.size() == 2);
    TS_ASSERT(dF.size() == 0);
    TS_ASSERT_EQUALS2(table, "SOR");
    TS_ASSERT_EQUALS2(iF[0], "MXITER");
    TS_ASSERT_EQUALS2(iF[1], "IPRSOR");
    TS_ASSERT_EQUALS2(rF[0], "ACCL");
    TS_ASSERT_EQUALS2(rF[1], "HCLOSE");
  }
  {
    MfData::MfPackage pack(MfData::Packages::PCG);
    GetSolverFieldsFromPackage(&pack, table, iF, rF, dF);
    TS_ASSERT(iF.size() == 6);
    TS_ASSERT(rF.size() == 4);
    TS_ASSERT(dF.size() == 0);
    TS_ASSERT_EQUALS2(table, "PCG");
    TS_ASSERT_EQUALS2(iF[0], "MXITER");
    TS_ASSERT_EQUALS2(iF[1], "ITER1");
    TS_ASSERT_EQUALS2(iF[2], "NPCOND");
    TS_ASSERT_EQUALS2(iF[3], "NBPOL");
    TS_ASSERT_EQUALS2(iF[4], "IPRPCG");
    TS_ASSERT_EQUALS2(iF[5], "MUTPCG");
    TS_ASSERT_EQUALS2(rF[0], "HCLOSE");
    TS_ASSERT_EQUALS2(rF[1], "RCLOSE");
    TS_ASSERT_EQUALS2(rF[2], "RELAX");
    TS_ASSERT_EQUALS2(rF[3], "DAMP");
  }
  {
    MfData::MfPackage pack(MfData::Packages::LMG);
    GetSolverFieldsFromPackage(&pack, table, iF, rF, dF);
    TS_ASSERT(iF.size() == 5);
    TS_ASSERT(rF.size() == 8);
    TS_ASSERT(dF.size() == 0);
    TS_ASSERT_EQUALS2(table, "LMG");
    TS_ASSERT_EQUALS2(iF[0], "ICG");
    TS_ASSERT_EQUALS2(iF[1], "MXITER");
    TS_ASSERT_EQUALS2(iF[2], "MXCYC");
    TS_ASSERT_EQUALS2(iF[3], "IOUTAMG");
    TS_ASSERT_EQUALS2(iF[4], "CONTROL");
    TS_ASSERT_EQUALS2(rF[0], "STOR1");
    TS_ASSERT_EQUALS2(rF[1], "STOR2");
    TS_ASSERT_EQUALS2(rF[2], "STOR3");
    TS_ASSERT_EQUALS2(rF[3], "BCLOSE");
    TS_ASSERT_EQUALS2(rF[4], "DAMP");
    TS_ASSERT_EQUALS2(rF[5], "DUP");
    TS_ASSERT_EQUALS2(rF[6], "DLOW");
    TS_ASSERT_EQUALS2(rF[7], "HCLOSE");
  }
  {
    MfData::MfPackage pack(MfData::Packages::GMG);
    GetSolverFieldsFromPackage(&pack, table, iF, rF, dF);
    TS_ASSERT(iF.size() == 6);
    TS_ASSERT(rF.size() == 3);
    TS_ASSERT(dF.size() == 1);
    TS_ASSERT_EQUALS2(table, "GMG");
    TS_ASSERT_EQUALS2(iF[0], "IITER");
    TS_ASSERT_EQUALS2(iF[1], "MXITER");
    TS_ASSERT_EQUALS2(iF[2], "IADAMP");
    TS_ASSERT_EQUALS2(iF[3], "IOUTGMG");
    TS_ASSERT_EQUALS2(iF[4], "ISM");
    TS_ASSERT_EQUALS2(iF[5], "ISC");
    TS_ASSERT_EQUALS2(rF[0], "RCLOSE");
    TS_ASSERT_EQUALS2(rF[1], "HCLOSE");
    TS_ASSERT_EQUALS2(rF[2], "DAMP");
    TS_ASSERT_EQUALS2(dF[0], "RELAX");
  }
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testGetDataFromPackage ()
{
  MfData::MfPackage p("qqq"), *p1(NULL);
  CStr iF[4] = {"i1","i2","i3","i4"};
  std::vector<CStr> str(&iF[0], &iF[4]), str1;
  const int i[4]={1,2,3,4};
  std::vector<const int *> iV;

  TS_ASSERT(!GetDataFromPackage(p1, str, iV));
  TS_ASSERT(!GetDataFromPackage(&p, str1, iV));
  TS_ASSERT(!GetDataFromPackage(&p, str, iV));

  for (int j=0; j<4; j++)
    p.SetField(iF[j], &i[j]);

  TS_ASSERT(GetDataFromPackage(&p, str, iV));
  str.push_back("stuff");
  TS_ASSERT(!GetDataFromPackage(&p, str, iV));
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testGetSENFields ()
{
  CStr base[6]={"ISENSU", "IPRINTS", "ISENALL", "ISENFM", "ISENPU", "IUHEAD"};
  std::vector<CStr> flds;
  GetSENFields(flds);
  TS_ASSERT(flds.size() == 6);

  for (int i=0; i<6; i++)
    TS_ASSERT_EQUALS2(flds[i], base[i]);
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testGetPESFields ()
{

  CStr ib[11]={"MAXITER", "IBEFLG", "IYCFLG", "IOSTAR", "NOPT", "NFIT", "IAP",
               "IPRCOV", "IPRINT", "LPRINT", "LASTX"};
  CStr fb[8]={"MAXCHANGE", "TOL", "SOSC", "SOSR", "RMAR", "RMARM", "CSA", "FCONV"};
  std::vector<CStr> iflds, fflds;
  GetPESFields(iflds, fflds);
  TS_ASSERT(iflds.size() == 11);
  TS_ASSERT(fflds.size() == 8);
  int i;
  for (i=0; i<11; i++)
    TS_ASSERT_EQUALS2(iflds[i], ib[i]);
  for (i=0; i<8; i++)
    TS_ASSERT_EQUALS2(fflds[i], fb[i]);
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testGetOBFields ()
{
  std::vector<CStr> f;
  TS_ASSERT(!GetOBFields("stuff", f));

  TS_ASSERT(GetOBFields("OB2", f));
  TS_ASSERT(f.size() == 2);
  TS_ASSERT_EQUALS2(f[0], "TOMULTH");
  TS_ASSERT_EQUALS2(f[1], "EVH");

  TS_ASSERT(GetOBFields("OB3", f));
  TS_ASSERT(f.size() == 2);
  TS_ASSERT_EQUALS2(f[0], "TOMULTGB");
  TS_ASSERT_EQUALS2(f[1], "EVFGB");

  TS_ASSERT(GetOBFields("OB4", f));
  TS_ASSERT(f.size() == 2);
  TS_ASSERT_EQUALS2(f[0], "TOMULTDR");
  TS_ASSERT_EQUALS2(f[1], "EVFDR");

  TS_ASSERT(GetOBFields("OB5", f));
  TS_ASSERT(f.size() == 2);
  TS_ASSERT_EQUALS2(f[0], "TOMULTRV");
  TS_ASSERT_EQUALS2(f[1], "EVFRV");

  TS_ASSERT(GetOBFields("OB6", f));
  TS_ASSERT(f.size() == 2);
  TS_ASSERT_EQUALS2(f[0], "TOMULTCH");
  TS_ASSERT_EQUALS2(f[1], "EVFCH");

  TS_ASSERT(GetOBFields("OB7", f));
  TS_ASSERT(f.size() == 2);
  TS_ASSERT_EQUALS2(f[0], "TOMULTST");
  TS_ASSERT_EQUALS2(f[1], "EVFST");

  TS_ASSERT(GetOBFields("OB8", f));
  TS_ASSERT(f.size() == 2);
  TS_ASSERT_EQUALS2(f[0], "TOMULTDT");
  TS_ASSERT_EQUALS2(f[1], "EVFDT");
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testGetOBFields_2005 ()
{
  std::vector<CStr> f;
  std::vector<CStr> i;
  TS_ASSERT(!GetOBFields_2005("stuff", f, i));

  TS_ASSERT(GetOBFields_2005("OV2", f, i));
  TS_ASSERT(f.size() == 2);
  TS_ASSERT_EQUALS2(f[0], "TOMULTH");
  TS_ASSERT_EQUALS2(f[1], "HOBDRY");
  TS_ASSERT(i.size() == 1);
  TS_ASSERT_EQUALS2(i[0], "IUHOBSV");

  TS_ASSERT(GetOBFields_2005("OV3", f, i));
  TS_ASSERT(f.size() == 1);
  TS_ASSERT_EQUALS2(f[0], "TOMULTGB");
  TS_ASSERT(i.size() == 1);
  TS_ASSERT_EQUALS2(i[0], "IUGBOBSV");

  TS_ASSERT(GetOBFields_2005("OV4", f, i));
  TS_ASSERT(f.size() == 1);
  TS_ASSERT_EQUALS2(f[0], "TOMULTDR");
  TS_ASSERT(i.size() == 1);
  TS_ASSERT_EQUALS2(i[0], "IUDROBSV");

  TS_ASSERT(GetOBFields_2005("OV5", f, i));
  TS_ASSERT(f.size() == 1);
  TS_ASSERT_EQUALS2(f[0], "TOMULTRV");
  TS_ASSERT(i.size() == 1);
  TS_ASSERT_EQUALS2(i[0], "IURVOBSV");

  TS_ASSERT(GetOBFields_2005("OV6", f, i));
  TS_ASSERT(f.size() == 1);
  TS_ASSERT_EQUALS2(f[0], "TOMULTCH");
  TS_ASSERT(i.size() == 1);
  TS_ASSERT_EQUALS2(i[0], "IUCHOBSV");

  TS_ASSERT(GetOBFields_2005("OV7", f, i));
  TS_ASSERT(f.size() == 1);
  TS_ASSERT_EQUALS2(f[0], "TOMULTST");
  TS_ASSERT(i.size() == 1);
  TS_ASSERT_EQUALS2(i[0], "IUSTOBSV");
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testGetOCFields ()
{
  CStr b1[6]={"IHEDFM", "IDDNFM", "IHEDUN", "IDDNUN", "IBDOPT", "IAUXSV"};
  CStr b2[9]={"SPID", "TSNum", "IHDDFL", "IBUDFL", "ICBCFL", "Hdpr", "Ddpr",
              "Hdsv", "Ddsv"};
  std::vector<CStr> fields;
  CStr tab;
  int i;

  TS_ASSERT(!GetOCFields("stuff", tab, fields));

  TS_ASSERT(GetOCFields("OC", tab, fields));
  TS_ASSERT(fields.size() == 6);
  TS_ASSERT_EQUALS2(tab, "OCVars");
  for (i=0; i<6; i++)
    TS_ASSERT_EQUALS2(b1[i], fields[i]);

  TS_ASSERT(GetOCFields("OCT", tab, fields));
  TS_ASSERT(fields.size() == 9);
  TS_ASSERT_EQUALS2(tab, "OCTS");
  for (i=0; i<9; i++)
    TS_ASSERT_EQUALS2(b2[i], fields[i]);

  TS_ASSERT(!GetOCFields("stuff", tab, fields));
  TS_ASSERT(fields.size() == 0);
  TS_ASSERT(tab == "");
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportOC ()
{
  {
    MfData::MfPackage *p(NULL);
    TS_ASSERT(!ExportOC(p));
    MfData::MfPackage p1("stuff");
    TS_ASSERT(!ExportOC(&p1));
  }

  {
    DbMock db1;
    MfData::MfPackage p("OC");
    const int i[6]={1,2,3,4,5,6};
    p.SetField("IHEDFM", &i[0]);
    p.SetField("IDDNFM", &i[1]);
    p.SetField("IHEDUN", &i[2]);
    p.SetField("IDDNUN", &i[3]);
    p.SetField("IBDOPT", &i[4]);
    TS_ASSERT(!ExportOC(&p));
    p.SetField("IAUXSV", &i[5]);
    TS_ASSERT(ExportOC(&p));
    DbMock *db = dynamic_cast<DbMock*>(Db::db());
    TS_ASSERT_EQUALS2(db->m_table, "OCVars");
    TS_ASSERT_EQUALS2(db->m_query, "");
    TS_ASSERT(db->m_iVals.size() == 6);
    TS_ASSERT(db->m_iVals.find("IHEDUN") != db->m_iVals.end());
    TS_ASSERT(db->m_iVals["IHEDUN"].front() == 3);
  }

  {
    DbMock db1;
    MfData::MfPackage p("OCT");
    const int i[9]={1,2,3,4,5,6,7,8,9};
    p.SetField("SPID", &i[0]);
    p.SetField("TSNum", &i[1]);
    p.SetField("IHDDFL", &i[2]);
    p.SetField("IBUDFL", &i[3]);
    p.SetField("ICBCFL", &i[4]);
    p.SetField("Hdpr", &i[5]);
    p.SetField("Ddpr", &i[6]);
    p.SetField("Hdsv", &i[7]);
    TS_ASSERT(!ExportOC(&p));
    p.SetField("Ddsv", &i[8]);
    TS_ASSERT(ExportOC(&p));
    DbMock *db = dynamic_cast<DbMock*>(Db::db());
    TS_ASSERT_EQUALS2(db->m_table, "OCTS");
    TS_ASSERT_EQUALS2(db->m_query, "");
    TS_ASSERT(db->m_iVals.size() == 9);
    TS_ASSERT(db->m_iVals.find("Hdpr") != db->m_iVals.end());
    TS_ASSERT(db->m_iVals["Hdpr"].front() == 6);
  }
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportFloObs ()
{
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  MfData::MfPackage p(""), *p1(0);

  TS_ASSERT(!ExportFloObs(p1, 1, 1)); // null package
  TS_ASSERT(!ExportFloObs(&p, 1, 1)); // m_flob is empty
  Flob ob;
  GetFLOB().m_flob.push_back(ob);
  TS_ASSERT(!ExportFloObs(&p, 1, 1)); // m_fact is empty
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportFLOB ()
{
  Flob ob;
  ob.m_factorId = 1;
  ob.m_HOB = (Real)20.2;
  ob.m_IREFSP = 2;
  ob.m_name = "ghb1";
  ob.m_PLOT = 1;
  ob.m_STAT = (Real)120.1;
  ob.m_STATFLG = 1;
  ob.m_TOFFSET = (Real).5;
  ob.m_type = "GHB";
  GetFLOB().m_flob.push_back(ob);
  ob.m_name = "riv 1";
  ob.m_type = "RIV";
  GetFLOB().m_flob.push_back(ob);

  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());

  ExportFLOB();
  TS_ASSERT_EQUALS2(db->m_table, "FLOB");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT_EQUALS2(db->m_nFields, 9);
  TS_ASSERT(db->m_sVals.size() == 2);
  TS_ASSERT(db->m_sVals.find("FLOBType") != db->m_sVals.end());
  TS_ASSERT(db->m_sVals.find("OBSNAM") != db->m_sVals.end());
  TS_ASSERT(db->m_iVals.size() == 4);
  TS_ASSERT(db->m_iVals.find("IREFSP") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find("STATFLAG") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find("PLOTSYMBOL") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find("FLOBID") != db->m_iVals.end());

  Real r(0);
  TS_ASSERT(db->GetMap(r).size() == 3);
  TS_ASSERT(db->GetMap(r).find("TOFFSET") != db->GetMap(r).end());
  TS_ASSERT(db->GetMap(r).find("HOBS") != db->GetMap(r).end());
  TS_ASSERT(db->GetMap(r).find("STATISTIC") != db->GetMap(r).end());
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportFLOBFactors ()
{
  FlobFact f;
  GetFLOB().m_fact.push_back(f);

  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());

  ExportFLOBFactors(10, 15);
  TS_ASSERT_EQUALS2(db->m_table, "FLOBFactors");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT_EQUALS2(db->m_nFields, 3);
  TS_ASSERT(db->m_iVals.size() == 2);
  TS_ASSERT(db->m_iVals.find("FLOBID") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find("IJK") != db->m_iVals.end());
  Real r(0);
  TS_ASSERT(db->GetMap(r).size() == 1);
  TS_ASSERT(db->GetMap(r).find("Factor") != db->GetMap(r).end());
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportHOBPack ()
{
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  MfData::MfPackage *p(0), p1("");
  TS_ASSERT(!ExportHOBPack(p, 4, 5)); // null package
  TS_ASSERT(!ExportHOBPack(&p1, 4, 5)); // empty HOB data
}
//------------------------------------------------------------------------------
static void CreateHOBTestData ()
{
  HdObs hd;
  hd.m_cOff = (Real).1;
  hd.m_col = 2;
  hd.m_ITT = 1;
  hd.m_name = "well1";
  hd.m_rOff = (Real).1;
  hd.m_row = 4;
  hd.m_vLay.assign(2, ObLay());
  hd.m_vLay[0].m_lay = 1;
  hd.m_vLay[0].m_factor = (Real).3;
  hd.m_vLay[0].m_lay = 2;
  hd.m_vLay[0].m_factor = (Real).7;
  hd.m_vTimes.assign(2, ObTime());
  hd.m_vTimes[0].m_hob = (Real)10.1;
  hd.m_vTimes[0].m_iRefSp = 1;
  hd.m_vTimes[0].m_name = "ob1";
  hd.m_vTimes[0].m_plot = 1;
  hd.m_vTimes[0].m_statdd = (Real)1.3;
  hd.m_vTimes[0].m_statFlag = 1;
  hd.m_vTimes[0].m_statH = (Real)1.4;
  hd.m_vTimes[0].m_tOff = (Real).1;
  hd.m_vTimes[1].m_hob = (Real)10.1;
  hd.m_vTimes[1].m_iRefSp = 2;
  hd.m_vTimes[1].m_name = "ob2";
  hd.m_vTimes[1].m_plot = 1;
  hd.m_vTimes[1].m_statdd = (Real)1.3;
  hd.m_vTimes[1].m_statFlag = 1;
  hd.m_vTimes[1].m_statH = (Real)1.4;
  hd.m_vTimes[1].m_tOff = (Real).1;
  GetHOB().push_back(hd);
  hd.m_name = "well2";
  hd.m_vTimes[0].m_name = "ob3";
  hd.m_vTimes[1].m_name = "ob4";
  GetHOB().push_back(hd);
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportHOB ()
{
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());

  CreateHOBTestData();
  int nLayRec, nTimeRec;
  CellIdToIJK grid(10, 12);
  ExportHOB(nLayRec, nTimeRec, grid);
  TS_ASSERT_EQUALS(nLayRec, 4);
  TS_ASSERT_EQUALS(nTimeRec, 4);
  TS_ASSERT_EQUALS2(db->m_table, "HOB");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT_EQUALS2(db->m_nFields, 6);
  TS_ASSERT(db->m_sVals.size() == 1);
  TS_ASSERT(db->m_sVals.find("OBSNAM") != db->m_sVals.end());
  TS_ASSERT(db->m_iVals.size() == 3);
  TS_ASSERT(db->m_iVals.find("HOBID") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find("IJ") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find("ITT") != db->m_iVals.end());
  Real r(0);
  TS_ASSERT(db->GetMap(r).size() == 2);
  TS_ASSERT(db->GetMap(r).find("ROFF") != db->GetMap(r).end());
  TS_ASSERT(db->GetMap(r).find("COFF") != db->GetMap(r).end());

  GetHOB().clear();
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportHOBLayers ()
{
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());

  CreateHOBTestData();
  ExportHOBLayers(4);
  TS_ASSERT_EQUALS2(db->m_table, "HOBLayers");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT_EQUALS2(db->m_nFields, 3);
  TS_ASSERT(db->m_iVals.size() == 2);
  TS_ASSERT(db->m_iVals.find("HOBID") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find("Layer") != db->m_iVals.end());
  Real r(0);
  TS_ASSERT(db->GetMap(r).size() == 1);
  TS_ASSERT(db->GetMap(r).find("PR") != db->GetMap(r).end());
  GetHOB().clear();
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportHOBTimes ()
{
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());

  CreateHOBTestData();
  ExportHOBTimes(4);
  TS_ASSERT_EQUALS2(db->m_table, "HOBTimes");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT_EQUALS2(db->m_nFields, 9);
  TS_ASSERT(db->m_sVals.size() == 1);
  TS_ASSERT(db->m_sVals.find("OBSNAM") != db->m_sVals.end());
  TS_ASSERT(db->m_iVals.size() == 4);
  TS_ASSERT(db->m_iVals.find("HOBID") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find("IREFSP") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find("STATFLAG") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find("PLOTSYMBOL") != db->m_iVals.end());
  Real r(0);
  TS_ASSERT(db->GetMap(r).size() == 4);
  TS_ASSERT(db->GetMap(r).find("TOFFSET") != db->GetMap(r).end());
  TS_ASSERT(db->GetMap(r).find("HOBS") != db->GetMap(r).end());
  TS_ASSERT(db->GetMap(r).find("STATh") != db->GetMap(r).end());
  TS_ASSERT(db->GetMap(r).find("STATdd") != db->GetMap(r).end());
  GetHOB().clear();
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportOBSVars ()
{
  Real r[2] = { 10.0, 30.0 };
  {
    MfData::MfPackage p("stuff");
    TS_ASSERT(ExportOBSVars(&p) == 0);
  }
  const char *pack[7] = {"OB2", "OB3", "OB4", "OB5", "OB6", "OB7", "OB8"};
  const char *flds[7][2] = { {"TOMULTH", "EVH"},
                             {"TOMULTGB", "EVFGB"},
                             {"TOMULTDR", "EVFDR"},
                             {"TOMULTRV", "EVFRV"},
                             {"TOMULTCH", "EVFCH"},
                             {"TOMULTST", "EVFST"},
                             {"TOMULTDT", "EVFDT"} };
  Real r1(0);
  for (int i=0; i<7; i++)
  {
    {
      DbMock db1, *db;
      db = static_cast<DbMock*>(Db::db());
      MfData::MfPackage p(pack[i]);
      p.SetField(flds[i][0], &r[0]);
      p.SetField(flds[i][1], &r[1]);
      TS_ASSERT(ExportOBSVars(&p) != 0);
      TS_ASSERT_EQUALS2(db->m_table, "OBSVars");
      TS_ASSERT_EQUALS2(db->m_query, "OID = 1");
      TS_ASSERT_EQUALS2(db->m_nFields, -1);
      TS_ASSERT(db->GetMap(r1).size() == 2);
    }
  }
}
void ExpGeoDbT::testExportOBSVars_2005 ()
{
  Real r[2] = { 10.0, 30.0 };
  int iv[2] = { 1, 2 };
  {
    MfData::MfPackage p("stuff");
    TS_ASSERT(ExportOBSVars_2005(&p) == 0);
  }
  const char *pack[6] = {"OV2", "OV3", "OV4", "OV5", "OV6", "OV7"};
  const char *fflds[6][2] = { {"TOMULTH", "HOBDRY"},
                              {"TOMULTGB", ""},
                              {"TOMULTDR", ""},
                              {"TOMULTRV", ""},
                              {"TOMULTCH", ""},
                              {"TOMULTST", ""}};
  const char *iflds[6][1] = { {"IUHOBSV"},
                              {"IUGBOBSV"},
                              {"IUDROBSV"},
                              {"IURVOBSV"},
                              {"IUCHOBSV"},
                              {"IUSTOBSV"}};
  Real r1(0);
  int m(0);
  for (int i=0; i<6; i++)
  {
    {
      DbMock db1, *db;
      db = static_cast<DbMock*>(Db::db());
      MfData::MfPackage p(pack[i]);
      p.SetField(fflds[i][0], &r[0]);
      if (i == 0)
        p.SetField(fflds[i][1], &r[1]);
      p.SetField(iflds[i][0], &iv[0]);
      TS_ASSERT(ExportOBSVars_2005(&p) != 0);
      TS_ASSERT_EQUALS2(db->m_table, "OBSVars");
      TS_ASSERT_EQUALS2(db->m_query, "OID = 1");
      TS_ASSERT_EQUALS2(db->m_nFields, -1);
      if (i == 0)
      {
        TS_ASSERT(db->GetMap(r1).size() == 2);
      }
      else
      {
        TS_ASSERT(db->GetMap(r1).size() == 1);
      }
      TS_ASSERT(db->GetMap(m).size() == 1);
    }
  }
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportOB1 ()
{
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());

  MfData::MfPackage p("OB1");
  const char *c = "The out name";
  const int i(5);
  TS_ASSERT(ExportOB1(&p) == 0);
  p.SetField("OUTNAM", c);
  TS_ASSERT(ExportOB1(&p) == 0);
  p.SetField("ISCALS", &i);
  TS_ASSERT(ExportOB1(&p) != 0);
  TS_ASSERT_EQUALS2(db->m_table, "OBSVars");
  TS_ASSERT_EQUALS2(db->m_query, "OID = 1");
  TS_ASSERT_EQUALS2(db->m_nFields, -1);
  TS_ASSERT(db->m_iVals.size() == 1);
  TS_ASSERT(db->m_sVals.size() == 1);
  TS_ASSERT(db->m_sVals["OUTNAM"].front() == "The out name");
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportPES ()
{
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  std::vector<CStr> iFld, rFld;
  GetPESFields(iFld, rFld);

  MfData::MfPackage p("PES");
  std::vector<int> iVals(iFld.size(), 0);
  std::vector<Real> rVals(rFld.size(), 0);
  int i;
  for (i=0; i<(int)iFld.size(); i++)
  {
    iVals[i] = i+1;
    p.SetField(iFld[i], &iVals[i]);
  }
  TS_ASSERT(ExportPES(&p) == 0);
  for (i=0; i<(int)rFld.size(); i++)
  {
    rVals[i] = static_cast<Real>(i+1);
    p.SetField(rFld[i], &rVals[i]);
  }
  TS_ASSERT(ExportPES(&p) != 0);
  TS_ASSERT_EQUALS2(db->m_table, "PES");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT_EQUALS2(db->m_nFields, -1);
  TS_ASSERT(db->m_iVals.size() == 11);
  if (sizeof(Real) == sizeof(float))
  {
    TS_ASSERT(db->m_fVals.size() == 8);
  }
  else
  {
    TS_ASSERT(db->m_dVals.size() == 8);
  }
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportSEN ()
{
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  std::vector<CStr> iFld;
  GetSENFields(iFld);

  MfData::MfPackage p("SEN");
  TS_ASSERT(ExportSEN(&p) == 0);
  std::vector<int> iVals(iFld.size(), 0);
  int i;
  for (i=0; i<(int)iFld.size(); i++)
  {
    iVals[i] = i+1;
    p.SetField(iFld[i], &iVals[i]);
  }
  TS_ASSERT(ExportSEN(&p) != 0);
  TS_ASSERT_EQUALS2(db->m_table, "SEN");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT_EQUALS2(db->m_nFields, -1);
  TS_ASSERT(db->m_iVals.size() == 6);
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportHFB ()
{
  using namespace MfData::Packages::HFBpack;
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  Real hfbDat[14] = {1,2,2,2,3,(Real).001,0,
                     2,2,2,2,3,(Real).001,0};
  int nHfb(2);
  MfData::MfPackage p(MfData::Packages::HFB);

  TS_ASSERT(!ExportHFB(&p, 1, 10, 15));
  p.SetField(NHFBNP, &nHfb);
  p.SetField(HFB, hfbDat);
  TS_ASSERT(ExportHFB(&p, 1, 10, 15));
  TS_ASSERT_EQUALS2(db->m_table, "HFB6");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT_EQUALS2(db->m_nFields, 4);
  TS_ASSERT(db->m_iVals.size() == 3);
  TS_ASSERT(db->m_iVals.find("SPID") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find("IJK1") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find("IJK2") != db->m_iVals.end());
  Real r(0);
  TS_ASSERT(db->GetMap(r).size() == 1);
  TS_ASSERT(db->GetMap(r).find("Hydchr") != db->GetMap(r).end());
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testGetArealFields ()
{
  using namespace MfData::Packages;
  MfData::MfPackage p1(MfData::Packages::RCH), p2(MfData::Packages::EVT);
  CStr cbf, optF, optT;
  std::vector<CStr> fields;
  GetArealFields(&p1, cbf, optF, optT, fields);
  TS_ASSERT_EQUALS2(cbf, "IRCHCB");
  TS_ASSERT_EQUALS2(optF, RCHpack::NRCHOP);
  TS_ASSERT_EQUALS2(optT, "RCHVars");
  TS_ASSERT(fields.size() == 2);
  TS_ASSERT_EQUALS2(fields[0], RCHpack::INRECH);
  TS_ASSERT_EQUALS2(fields[1], RCHpack::INIRCH);
  GetArealFields(&p2, cbf, optF, optT, fields);
  TS_ASSERT_EQUALS2(cbf, "IEVTCB");
  TS_ASSERT_EQUALS2(optF, EVTpack::NEVTOP);
  TS_ASSERT_EQUALS2(optT, "EVTVars");
  TS_ASSERT(fields.size() == 4);
  TS_ASSERT_EQUALS2(fields[0], EVTpack::INEVTR);
  TS_ASSERT_EQUALS2(fields[1], EVTpack::INEXDP);
  TS_ASSERT_EQUALS2(fields[2], EVTpack::INSURF);
  TS_ASSERT_EQUALS2(fields[3], EVTpack::INIEVT);
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportArealCBF ()
{
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  CStr field("IRCHCB");
  MfData::MfPackage p("");
  ExportArealCBF(&p, field);
  TS_ASSERT_EQUALS2(db->m_table, ""); // nothing happened
  int iVal(40);
  p.SetField(field, &iVal);
  ExportArealCBF(&p, field);
  TS_ASSERT_EQUALS2(db->m_table, "CBFlags");
  TS_ASSERT_EQUALS2(db->m_query, "OID = 1");
  TS_ASSERT_EQUALS2(db->m_nFields, -1);
  TS_ASSERT(db->m_iVals.size() == 1);
  TS_ASSERT(db->m_iVals.find(field) != db->m_iVals.end());
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportArealOption ()
{
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  MfData::MfPackage p("EVT");
  CStr optField("NEVTOP"), optTab("EVTVars");
  int optVal, inVal(2);
  ExportArealOption(&p, optField, optTab, optVal);
  TS_ASSERT_EQUALS2(db->m_table, ""); // nothing happened
  p.SetField(optField, &inVal);
  ExportArealOption(&p, optField, optTab, optVal);
  TS_ASSERT_EQUALS(optVal, 2);
  TS_ASSERT_EQUALS2(db->m_table, "EVTVars");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT_EQUALS2(db->m_nFields, -1);
  TS_ASSERT(db->m_iVals.size() == 1);
  TS_ASSERT(db->m_iVals.find(optField) != db->m_iVals.end());
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportArealArrayFlags ()
{
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  CStr c, o, ot;
  int i, iVals[4]={1,-1,1,-1};
  std::vector<CStr> fields;
  MfData::MfPackage p("EVT");
  GetArealFields(&p, c, o, ot, fields);
  TS_ASSERT(!ExportArealArrayFlags(&p, 1, fields));
  for (i=0; i<4; i++)
    p.SetField(fields[i], &iVals[i]);
  TS_ASSERT(ExportArealArrayFlags(&p, 1, fields));
  TS_ASSERT_EQUALS2(db->m_table, "StressPeriods");
  TS_ASSERT_EQUALS2(db->m_query, "SPID = 1");
  TS_ASSERT_EQUALS2(db->m_nFields, -1);
  TS_ASSERT(db->m_iVals.size() == 4);
  for (i=0; i<4; i++)
    TS_ASSERT(db->m_iVals.find(fields[i]) != db->m_iVals.end());
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportAreal ()
{
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());

  CStr c, o, ot;
  int cbf(40), opt(2);
  std::vector<CStr> fields;
  MfData::MfPackage p("EVT");
  TS_ASSERT(!ExportAreal(&p, 3));
  GetArealFields(&p, c, o, ot, fields);
  p.SetField(c, &cbf);
  p.SetField(o, &opt);
  int i, iVals[4]={1,2,3,4};
  for (i=0; i<4; i++)
    p.SetField(fields[i], &iVals[i]);
  TS_ASSERT(ExportAreal(&p, 3));
  TS_ASSERT_EQUALS2(db->m_table, "StressPeriods");
  TS_ASSERT_EQUALS2(db->m_query, "SPID = 3");
  TS_ASSERT_EQUALS2(db->m_nFields, -1);
  TS_ASSERT(db->m_iVals.size() == 3);
  for (i=0; i<3; i++)
    TS_ASSERT(db->m_iVals.find(fields[i]) != db->m_iVals.end());
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportBAS ()
{
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  MfData::MfPackage p("BAS");
  TS_ASSERT(!ExportBAS(&p));
  const char *c = "some string";
  const char *h1 = "heading1";
  const char *h2 = "heading2";
  Real r(7);
  p.SetField(MfData::Packages::BASpack::HNOFLO, &r);
  p.SetField(MfData::Packages::BASpack::OPT, c);
  p.SetField(MfData::Packages::BASpack::HEADNG1, h1);
  p.SetField(MfData::Packages::BASpack::HEADNG2, h2);
  TS_ASSERT(ExportBAS(&p));
  TS_ASSERT_EQUALS2(db->m_table, "BASVars");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT_EQUALS2(db->m_nFields, -1);
  TS_ASSERT(db->m_sVals.size() == 3);
  TS_ASSERT(db->GetMap(r).size() == 1);
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportBCFCbf ()
{
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  MfData::MfPackage p("BCF");
  ExportBCFCbf(&p);
  int i(40);
  TS_ASSERT_EQUALS2(db->m_table, ""); // no data
  p.SetField("IBCFCB", &i);
  ExportBCFCbf(&p);
  TS_ASSERT_EQUALS2(db->m_table, "CBFlags");
  TS_ASSERT_EQUALS2(db->m_query, "OID = 1");
  TS_ASSERT_EQUALS2(db->m_nFields, -1);
  TS_ASSERT(db->m_iVals.size() == 1);
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportBCFVars ()
{
  using namespace MfData::Packages::BCFpack;
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  MfData::MfPackage p("BCF");
  int i[3]={1,2,3};
  Real r[2]={1,2}, r1(0);
  TS_ASSERT(!ExportBCFVars(&p));
  p.SetField(IWDFLG, &i[0]);
  p.SetField(IWETIT, &i[1]);
  p.SetField(IHDWET, &i[2]);
  TS_ASSERT(!ExportBCFVars(&p));
  p.SetField(HDRY, &r[0]);
  p.SetField(WETFCT, &r[1]);
  TS_ASSERT(ExportBCFVars(&p));
  TS_ASSERT_EQUALS2(db->m_table, "BCFVars");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT_EQUALS2(db->m_nFields, -1);
  TS_ASSERT(db->m_iVals.size() == 3);
  TS_ASSERT(db->m_iVals.find(IWDFLG) != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find(IWETIT) != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find(IHDWET) != db->m_iVals.end());
  TS_ASSERT(db->GetMap(r1).size() == 2);
  TS_ASSERT(db->GetMap(r1).find(HDRY) != db->GetMap(r1).end());
  TS_ASSERT(db->GetMap(r1).find(WETFCT) != db->GetMap(r1).end());
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportBCFLayers ()
{
  using namespace MfData::Packages;
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  MfData::MfPackage p("BCF");
  int layAvg[3]={1,2,3};
  int layCon[3]={4,5,6};
  int nlay(3);
  Real trpy[3]={7,8,9};
  TS_ASSERT(!ExportBCFLayers(&p));
  p.SetField(BCFpack::NLAY, &nlay);
  p.SetField(BCFpack::LAYAVG, layAvg);
  p.SetField(BCFpack::LAYCON, layCon);
  p.SetField(BCFpack::TRPY, trpy);
  TS_ASSERT(ExportBCFLayers(&p));
  TS_ASSERT_EQUALS2(db->m_table, "BCFLayers");
  TS_ASSERT_EQUALS2(db->m_query, "Layer = 3");
  TS_ASSERT(db->m_iVals.size() == 2);
  TS_ASSERT(db->m_iVals.find(BCFpack::LAYAVG) != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find(BCFpack::LAYCON) != db->m_iVals.end());
  Real r1(0);
  TS_ASSERT(db->GetMap(r1).size() == 1);
  TS_ASSERT(db->GetMap(r1).find(BCFpack::TRPY) != db->GetMap(r1).end());
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportBCF ()
{
  // this is tested by the previous 3 tests but I am leaving it here
  // to remind me that we don't need it.
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportLPF99 ()
{
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  MfData::MfPackage p("L99");
  TS_ASSERT(!ExportLPF99(&p));
  Real r(5);
  int i[2] = {3, 4};
  p.SetField("WETFCT", &r);
  p.SetField("IWETIT", &i[0]);
  p.SetField("IHDWET", &i[1]);
  TS_ASSERT(ExportLPF99(&p));
  TS_ASSERT_EQUALS2(db->m_table, "LPFVars");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT(db->m_iVals.size() == 2);
  if (sizeof(Real) == sizeof(float))
  {
    TS_ASSERT(db->m_fVals.size() == 1);
  }
  else
  {
    TS_ASSERT(db->m_dVals.size() == 1);
  }
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportLPFCbf ()
{
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  MfData::MfPackage p("LPF");
  int ival(40);
  ExportLPFCbf(&p);
  TS_ASSERT_EQUALS2(db->m_table, "");
  p.SetField("ILPFCB", &ival);
  ExportLPFCbf(&p);
  TS_ASSERT_EQUALS2(db->m_table, "CBFlags");
  TS_ASSERT_EQUALS2(db->m_query, "OID = 1");
  TS_ASSERT_EQUALS2(db->m_nFields, -1);
  TS_ASSERT(db->m_iVals.size() == 1);
  TS_ASSERT(db->m_iVals.find("ILPFCB") != db->m_iVals.end());
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportLPFVars ()
{
  using namespace MfData::Packages;
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  MfData::MfPackage p("LPF");
  Real r(-888);
  ExportLPFVars(&p);
  TS_ASSERT_EQUALS2(db->m_table, "");
  p.SetField(LPFpack::HDRY, &r);
  ExportLPFVars(&p);
  TS_ASSERT_EQUALS2(db->m_table, "LPFVars");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT_EQUALS2(db->m_nFields, -1);
  TS_ASSERT(db->GetMap(r).size() == 1);
  TS_ASSERT(db->GetMap(r).find(LPFpack::HDRY) != db->GetMap(r).end());
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportLPFLayers ()
{
  using namespace MfData::Packages;
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  MfData::MfPackage p("LPF");
  ExportLPFLayers(&p);
  TS_ASSERT_EQUALS2(db->m_table, "");
  int nlay(3);
  int laytyp[3]={1,2,3},layavg[3]={4,5,6}, layvka[3]={7,8,9},
      laywet[3]={1,2,3};
  Real chani[3]={4,5,6};
  p.SetField(LPFpack::NLAY, &nlay);
  p.SetField(LPFpack::LAYTYP, laytyp);
  p.SetField(LPFpack::LAYAVG, layavg);
  p.SetField(LPFpack::LAYVKA, layvka);
  p.SetField(LPFpack::LAYWET, laywet);
  p.SetField(LPFpack::CHANI, chani);
  ExportLPFLayers(&p);
  Real r(0);
  TS_ASSERT_EQUALS2(db->m_table, "LPFLayers");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT_EQUALS2(db->m_nFields, 6);
  TS_ASSERT(db->m_iVals.size() == 5);
  TS_ASSERT(db->m_iVals.find("Layer") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find(LPFpack::LAYTYP) != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find(LPFpack::LAYAVG) != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find(LPFpack::LAYVKA) != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find(LPFpack::LAYWET) != db->m_iVals.end());
  TS_ASSERT(db->GetMap(r).size() == 1);
  TS_ASSERT(db->GetMap(r).find(LPFpack::CHANI) != db->GetMap(r).end());
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportLPF ()
{
  // this is tested by the previous 3 tests but I am leaving it here
  // to remind me that we don't need it.
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportSolver ()
{
  std::vector<CStr>    intFields, realFields, dblFields;
  std::vector<int>     intData;
  std::vector<Real>    realData;
  std::vector<double>  dblData;
  CStr tab;
  size_t j;
  int nFields;
  const char *packs[5] = {"SIP","PCG","LMG","GMG","SOR"};

  for (int i=0; i<5; i++)
  {
    DbMock db1, *db;
    db = static_cast<DbMock*>(Db::db());
    intData.resize(0);
    realData.resize(0);
    dblData.resize(0);
    MfData::MfPackage p(packs[i]);
    GetSolverFieldsFromPackage(&p, tab, intFields, realFields, dblFields);
    nFields = 0;
    for (j=0; j<intFields.size(); j++)
    {
      intData.push_back((int)j);
      p.SetField(intFields[j], &intData.back());
      nFields++;
    }
    for (j=0; j<realFields.size(); j++)
    {
      realData.push_back((Real)j);
      p.SetField(realFields[j], &realData.back());
      nFields++;
    }
    for (j=0; j<dblFields.size(); j++)
    {
      dblData.push_back((double)j);
      p.SetField(dblFields[j], &dblData.back());
      nFields++;
    }

    TS_ASSERT(ExportSolver(&p));
    TS_ASSERT_EQUALS2(db->m_table, tab);
    TS_ASSERT_EQUALS2(db->m_query, "");
    TS_ASSERT_EQUALS2(db->m_nFields, -1);
    TS_ASSERT(db->m_iVals.size() == intFields.size());
    for (j=0; j<intFields.size(); j++)
      TS_ASSERT(db->m_iVals.find(intFields[j]) != db->m_iVals.end());
    Real r(0);
    for (j=0; j<realFields.size(); j++)
      TS_ASSERT(db->GetMap(r).find(realFields[j]) != db->GetMap(r).end());
    for (j=0; j<dblFields.size(); j++)
      TS_ASSERT(db->m_dVals.find(dblFields[j]) != db->m_dVals.end());
  }
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportDE4SolverLine1 ()
{
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  MfData::MfPackage p("DE4");
  TS_ASSERT(!ExportSolverDE4Line1(&p));
  int itmx = 0;
  int mxup = 1;
  int mxlow = 2;
  int mxbw = 3;
  p.SetField(MfData::Packages::De4Pack::ITMX, &itmx);
  p.SetField(MfData::Packages::De4Pack::MXUP, &mxup);
  p.SetField(MfData::Packages::De4Pack::MXLOW, &mxlow);
  p.SetField(MfData::Packages::De4Pack::MXBW, &mxbw);
  TS_ASSERT(ExportSolverDE4Line1(&p));
  TS_ASSERT_EQUALS2(db->m_table, "DE4");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT_EQUALS2(db->m_nFields, -1);
  TS_ASSERT(db->m_iVals.size() == 4);
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportDE4SolverLine2 ()
{
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  MfData::MfPackage p("DE4");
  TS_ASSERT(!ExportSolverDE4Line2(&p));
  int ifreq = 0;
  int mutd4 = 1;
  Real accl = 2;
  Real hclose = 3;
  int iprd4 = 4;
  p.SetField(MfData::Packages::De4Pack::IFREQ, &ifreq);
  p.SetField(MfData::Packages::De4Pack::MUTD4, &mutd4);
  p.SetField(MfData::Packages::De4Pack::ACCL, &accl);
  p.SetField(MfData::Packages::De4Pack::HCLOSE, &hclose);
  p.SetField(MfData::Packages::De4Pack::IPRD4, &iprd4);
  TS_ASSERT(ExportSolverDE4Line2(&p));
  TS_ASSERT_EQUALS2(db->m_table, "DE4");
  TS_ASSERT_EQUALS2(db->m_query, "OID = 1");
  TS_ASSERT_EQUALS2(db->m_nFields, -1);
  TS_ASSERT(db->m_iVals.size() == 3);
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportListPackData ()
{
  int nRiv(5), nFields(9), nAux(3);
  Real d[45] = {1.0f,11.0f,17.0f,305.40f,8.42f,305.25f,6.0f,22.11f,1.0f,
                1.0f,10.0f,17.0f,305.38f,24.15f,305.23f,6.0f,63.39f,1.0f,
                1.0f,10.0f,16.0f,305.34f,61.11f,305.19f,6.0f,160.40f,1.0f,
                1.0f,9.0f,16.0f,305.25f,88.89f,305.10f,6.0f,233.33f,1.0f,
                1.0f,9.0f,15.0f,305.20f,2.60f,305.05f,6.0f,6.83f,1.0f};
  char c[80] = {73,70,65,67,69,32,32,32,32,32,32,32,32,32,32,32,67,79,78,68,
                70,65,67,84,32,32,32,32,32,32,32,32,67,69,76,76,71,82,80,32,
                32,32,32,32,32,32,32,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                0,0,0,0,0,0,0,0,0,0,0,0,0,0};

  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  MfData::MfPackage pack(MfData::Packages::RIV);
  pack.SetField(MfData::Packages::ListPack::MAXBC, &nRiv);
  pack.SetField(MfData::Packages::ListPack::NUMBC, &nRiv);
  pack.SetField(MfData::Packages::ListPack::NUMFIELDS, &nFields);
  pack.SetField(MfData::Packages::ListPack::NAUX, &nAux);
  pack.SetField(MfData::Packages::ListPack::DATA, &d[0]);
  pack.SetField(MfData::Packages::ListPack::AUX, &c[0]);

  ExportListPackData(&pack, 2, 15, 20);
  TS_ASSERT_EQUALS2(db->m_table, "RIV");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT(db->m_iVals.size() == 3);
  TS_ASSERT(db->m_iVals.begin()->second.size() == 5);
  TS_ASSERT(db->m_iVals.find("IJK") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find("SPID") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find("IFACE") != db->m_iVals.end());
  if (sizeof(Real) == sizeof(float))
  {
    TS_ASSERT(db->m_fVals.size() == 5);
    TS_ASSERT(db->m_fVals.find("Rbot") != db->m_fVals.end());
    TS_ASSERT(db->m_fVals.find("Stage") != db->m_fVals.end());
    TS_ASSERT(db->m_fVals.find("Cond") != db->m_fVals.end());
    TS_ASSERT(db->m_fVals.find("Condfact") != db->m_fVals.end());
    TS_ASSERT(db->m_fVals.find("CELLGRP") != db->m_fVals.end());
    TS_ASSERT_DELTA(db->m_fVals["Rbot"][3], 305.10, GDBTOL);
    TS_ASSERT_DELTA(db->m_fVals["Cond"][2], 61.11, GDBTOL);
    TS_ASSERT_DELTA(db->m_fVals["Stage"][4], 305.20, GDBTOL);
    TS_ASSERT_DELTA(db->m_fVals["Condfact"][1], 63.39, GDBTOL);
    TS_ASSERT_DELTA(db->m_fVals["CELLGRP"][1], 1, GDBTOL);
  }
  else
  {
    TS_ASSERT(db->m_dVals.size() == 5);
    TS_ASSERT(db->m_dVals.find("Rbot") != db->m_dVals.end());
    TS_ASSERT(db->m_dVals.find("Stage") != db->m_dVals.end());
    TS_ASSERT(db->m_dVals.find("Cond") != db->m_dVals.end());
    TS_ASSERT(db->m_dVals.find("Condfact") != db->m_dVals.end());
    TS_ASSERT(db->m_dVals.find("CELLGRP") != db->m_dVals.end());
    TS_ASSERT_DELTA(db->m_dVals["Rbot"][3], 305.10, GDBTOL);
    TS_ASSERT_DELTA(db->m_dVals["Cond"][2], 61.11, GDBTOL);
    TS_ASSERT_DELTA(db->m_dVals["Stage"][4], 305.20, GDBTOL);
    TS_ASSERT_DELTA(db->m_dVals["Condfact"][1], 63.39, GDBTOL);
    TS_ASSERT_DELTA(db->m_dVals["CELLGRP"][1], 1, GDBTOL);
  }
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportListPackITMP ()
{
  DbMock db1, *db;

  MfData::MfPackage p("RIV");
  bool usePrev(false);
  const int val(-1);

  db = static_cast<DbMock*>(Db::db());
  ExportListPackITMP(&p, 2, usePrev);
  TS_ASSERT(db->m_iVals.empty());

  p.SetField(MfData::Packages::ListPack::ITMP, &val);
  ExportListPackITMP(&p, 2, usePrev);
  TS_ASSERT(!db->m_iVals.empty());
  TS_ASSERT(db->m_iVals.find("RIV_ITMP") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals["RIV_ITMP"].front() == -1);
  TS_ASSERT(usePrev);
  TS_ASSERT_EQUALS2(db->m_table, "StressPeriods");
  TS_ASSERT_EQUALS2(db->m_query, "SPID = 2")
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportListPackCBF ()
{
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  MfData::MfPackage p("WEL");
  const int val(40);

  ExportListPackCBF(&p);
  TS_ASSERT(db->m_iVals.empty());
  p.SetField("IWELCB", &val);

  ExportListPackCBF(&p);
  TS_ASSERT(!db->m_iVals.empty());
  TS_ASSERT(db->m_iVals.find("IWELCB") != db->m_iVals.end());
  TS_ASSERT_EQUALS(db->m_iVals["IWELCB"].front(), 40);
  TS_ASSERT_EQUALS2(db->m_table, "CBFlags");
  TS_ASSERT_EQUALS2(db->m_query, "OID = 1")
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportDISVars ()
{
  MfData::MfGlobal g(10, 15, 3, 2, 3, 2, 0);
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  ExportDISVars(&g);
  TS_ASSERT_EQUALS2(db->m_table, "DISVars");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT_EQUALS2(db->m_nFields, -1);
  TS_ASSERT(db->m_iVals.size() == 5);
  TS_ASSERT(db->m_iVals.find("NLAY") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find("NROW") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find("NCOL") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find("ITMUNI") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find("LENUNI") != db->m_iVals.end());
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportStressPeriods ()
{
  using namespace MfData::Packages;
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  MfData::MfPackage p("BAS");
  Real perlen[3]={1,2,3}, tsmult[3]={4,5,6};
  int  nstps[3]={1,2,3}, issflag[3]={4,5,6};
  ExportStressPeriods(&p, 3);
  TS_ASSERT_EQUALS2(db->m_table, "");
  p.SetField(DisPack::PERLEN, perlen);
  p.SetField(DisPack::NSTP, nstps);
  p.SetField(DisPack::TSMULT, tsmult);
  p.SetField(DisPack::ISSFLG, issflag);
  ExportStressPeriods(&p, 3);
  TS_ASSERT_EQUALS2(db->m_table, "StressPeriods");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT_EQUALS2(db->m_nFields, 6);
  TS_ASSERT(db->m_iVals.size() == 3);
  TS_ASSERT(db->m_iVals.find("SPID") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find("SpNum") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find(DisPack::NSTP) != db->m_iVals.end());
  TS_ASSERT(db->m_sVals.size() == 1);
  TS_ASSERT(db->m_sVals.find(DisPack::ISSFLG) != db->m_sVals.end());
  Real r(0);
  TS_ASSERT(db->GetMap(r).size() == 2);
  TS_ASSERT(db->GetMap(r).find(DisPack::PERLEN) != db->GetMap(r).end());
  TS_ASSERT(db->GetMap(r).find(DisPack::TSMULT) != db->GetMap(r).end());
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportDelRC ()
{
  using namespace MfData::Packages;
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  MfData::MfPackage p("BAS");
  Real delr[3]={1,2,3}, delc[4]={4,5,6,7};
  int nJ(3), nI(4);
  ExportDelRC(&p, nI, nJ);
  TS_ASSERT_EQUALS2(db->m_table, "");
  p.SetField(DisPack::DELR, delr);
  p.SetField(DisPack::DELC, delc);
  ExportDelRC(&p, nI, nJ);
  TS_ASSERT_EQUALS2(db->m_table, "DELRC");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT_EQUALS2(db->m_nFields, 3);
  TS_ASSERT_EQUALS2(db->m_nRow, 7);
  TS_ASSERT(db->m_sVals.size() == 1);
  TS_ASSERT(db->m_sVals.find("Direction") != db->m_sVals.end());
  TS_ASSERT(db->m_iVals.size() == 1);
  TS_ASSERT(db->m_iVals.find("Num") != db->m_iVals.end());
  Real r(0);
  TS_ASSERT(db->GetMap(r).size() == 1);
  TS_ASSERT(db->GetMap(r).find("Width") != db->GetMap(r).end());
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportLayCBD ()
{
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  MfData::MfPackage p("BAS");
  ExportLayCbd(&p, 3);
  TS_ASSERT_EQUALS2(db->m_table, "");
  int laycbd[3]={1,2,3};
  p.SetField("LAYCBD", laycbd);
  ExportLayCbd(&p, 3);
  TS_ASSERT_EQUALS2(db->m_table, "DISLayers");
  TS_ASSERT_EQUALS2(db->m_query, "Layer > 0 AND Layer < 4");
  TS_ASSERT_EQUALS2(db->m_nFields, 1);
  TS_ASSERT_EQUALS2(db->m_nRow, 3);
  TS_ASSERT(db->m_iVals.size() == 1);
  TS_ASSERT(db->m_iVals.find("LAYCBD") != db->m_iVals.end());
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportMultZoneName ()
{
  {
    DbMock db1, *db;
    db = static_cast<DbMock*>(Db::db());
    Real mult((Real)2.3);
    MfData::MfPackage p("MULT. ARRAY:stuff");
    int id;
    bool bMult;
    TS_ASSERT(!ExportMultZoneName(&p, id, bMult));
    p.SetField(MfData::Packages::Array::MULT, &mult);
    TS_ASSERT(ExportMultZoneName(&p, id, bMult));
    TS_ASSERT_EQUALS(id, 1);
    TS_ASSERT_EQUALS(bMult, true);
    TS_ASSERT_EQUALS2(db->m_table, "MultNames");
    TS_ASSERT_EQUALS2(db->m_query, "");
    TS_ASSERT_EQUALS2(db->m_nFields, -1);
    TS_ASSERT_EQUALS2(db->m_nRow, -1);
    TS_ASSERT(db->m_iVals.size() == 1);
    TS_ASSERT(db->m_iVals.find("MultID") != db->m_iVals.end());
    TS_ASSERT(db->m_sVals.size() == 1);
    TS_ASSERT(db->m_sVals.find("MultName") != db->m_sVals.end());
    TS_ASSERT_EQUALS2(db->m_sVals["MultName"].front(), "stuff");
    TS_ASSERT(db->GetMap(mult).size() == 1);
    TS_ASSERT(db->GetMap(mult).find("ArrayMult") != db->GetMap(mult).end());
  }
  {
    DbMock db1, *db;
    db = static_cast<DbMock*>(Db::db());
    Real mult((Real)2.3);
    MfData::MfPackage p("MULT. ARRAY:stuff1");
    p.SetField(MfData::Packages::Array::MULT, &mult);
    int id;
    bool bMult;
    ExportMultZoneName(&p, id, bMult);
    TS_ASSERT_EQUALS(id, 2); // here is the difference
    TS_ASSERT_EQUALS(bMult, true);
    TS_ASSERT_EQUALS2(db->m_table, "MultNames");
    TS_ASSERT_EQUALS2(db->m_query, "");
    TS_ASSERT_EQUALS2(db->m_nFields, -1);
    TS_ASSERT_EQUALS2(db->m_nRow, -1);
    TS_ASSERT(db->m_iVals.size() == 1);
    TS_ASSERT(db->m_iVals.find("MultID") != db->m_iVals.end());
    TS_ASSERT(db->m_sVals.size() == 1);
    TS_ASSERT(db->m_sVals.find("MultName") != db->m_sVals.end());
    TS_ASSERT_EQUALS2(db->m_sVals["MultName"].front(), "stuff1");
    TS_ASSERT(db->GetMap(mult).size() == 1);
    TS_ASSERT(db->GetMap(mult).find("ArrayMult") != db->GetMap(mult).end());
  }
  {
    DbMock db1, *db;
    db = static_cast<DbMock*>(Db::db());
    int mult(2);
    MfData::MfPackage p("ZONE ARRAY:stuff");
    int id;
    bool bMult;
    TS_ASSERT(!ExportMultZoneName(&p, id, bMult));
    p.SetField(MfData::Packages::Array::MULT, &mult);
    ExportMultZoneName(&p, id, bMult);
    TS_ASSERT_EQUALS(id, 1);
    TS_ASSERT_EQUALS(bMult, false);
    TS_ASSERT_EQUALS2(db->m_table, "ZoneNames");
    TS_ASSERT_EQUALS2(db->m_query, "");
    TS_ASSERT_EQUALS2(db->m_nFields, -1);
    TS_ASSERT_EQUALS2(db->m_nRow, -1);
    TS_ASSERT(db->m_iVals.size() == 2);
    TS_ASSERT(db->m_iVals.find("ZoneID") != db->m_iVals.end());
    TS_ASSERT(db->m_iVals.find("ArrayMult") != db->m_iVals.end());
    TS_ASSERT(db->m_sVals.size() == 1);
    TS_ASSERT(db->m_sVals.find("ZoneName") != db->m_sVals.end());
    TS_ASSERT_EQUALS2(db->m_sVals["ZoneName"].front(), "stuff");
  }
  {
    DbMock db1, *db;
    db = static_cast<DbMock*>(Db::db());
    int mult(2);
    MfData::MfPackage p("ZONE ARRAY:stuff1");
    p.SetField(MfData::Packages::Array::MULT, &mult);
    int id;
    bool bMult;
    ExportMultZoneName(&p, id, bMult);
    TS_ASSERT_EQUALS(id, 2); // here is the difference
    TS_ASSERT_EQUALS(bMult, false);
    TS_ASSERT_EQUALS2(db->m_table, "ZoneNames");
    TS_ASSERT_EQUALS2(db->m_query, "");
    TS_ASSERT_EQUALS2(db->m_nFields, -1);
    TS_ASSERT_EQUALS2(db->m_nRow, -1);
    TS_ASSERT(db->m_iVals.size() == 2);
    TS_ASSERT(db->m_iVals.find("ZoneID") != db->m_iVals.end());
    TS_ASSERT(db->m_iVals.find("ArrayMult") != db->m_iVals.end());
    TS_ASSERT(db->m_sVals.size() == 1);
    TS_ASSERT(db->m_sVals.find("ZoneName") != db->m_sVals.end());
    TS_ASSERT_EQUALS2(db->m_sVals["ZoneName"].front(), "stuff1");
  }
  NumberMultArrays() = 0;
  NumberZoneArrays() = 0;
  mapMultNamesId().clear();
  mapZoneNamesId().clear();
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportMultZoneVals ()
{
  {
    DbMock db1, *db;
    db = static_cast<DbMock*>(Db::db());
    Real mult((Real)2.3);
    Real data[8] = {1,2,3,4,5,6,7,8};
    MfData::MfPackage p("MULT. ARRAY:stuff");
    int id(2);
    bool bMult(true);
    TS_ASSERT(!ExportMultZoneVals(&p, id, bMult, 8));
    p.SetField(MfData::Packages::Array::MULT, &mult);
    p.SetField(MfData::Packages::Array::ARRAY, data);
    TS_ASSERT(ExportMultZoneVals(&p, id, bMult, 8));
    TS_ASSERT_EQUALS2(db->m_table, "Multipliers");
    TS_ASSERT_EQUALS2(db->m_query, "");
    TS_ASSERT_EQUALS2(db->m_nFields, 3);
    TS_ASSERT_EQUALS2(db->m_nRow, 8);
    TS_ASSERT(db->m_iVals.size() == 2);
    TS_ASSERT(db->m_iVals.find("MultID") != db->m_iVals.end());
    TS_ASSERT(db->m_iVals.find("IJ") != db->m_iVals.end());
    TS_ASSERT(db->GetMap(mult).size() == 1);
    TS_ASSERT(db->GetMap(mult).find("RMLT") != db->GetMap(mult).end());
  }
  {
    DbMock db1, *db;
    db = static_cast<DbMock*>(Db::db());
    int mult(3);
    int data[8] = {1,2,3,4,5,6,7,8};
    MfData::MfPackage p("ZONE ARRAY:stuff");
    int id(2);
    bool bMult(false);
    TS_ASSERT(!ExportMultZoneVals(&p, id, bMult, 8));
    p.SetField(MfData::Packages::Array::MULT, &mult);
    p.SetField(MfData::Packages::Array::ARRAY, data);
    TS_ASSERT(ExportMultZoneVals(&p, id, bMult, 8));
    TS_ASSERT_EQUALS2(db->m_table, "Zones");
    TS_ASSERT_EQUALS2(db->m_query, "");
    TS_ASSERT_EQUALS2(db->m_nFields, 3);
    TS_ASSERT_EQUALS2(db->m_nRow, 8);
    TS_ASSERT(db->m_iVals.size() == 3);
    TS_ASSERT(db->m_iVals.find("ZoneID") != db->m_iVals.end());
    TS_ASSERT(db->m_iVals.find("IJ") != db->m_iVals.end());
    TS_ASSERT(db->m_iVals.find("IZON") != db->m_iVals.end());
  }
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportMultArrayFunc ()
{
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  MfData::MfPackage p("FNC");
  TS_ASSERT(!ExportMultArrayFunc(&p));
  const char *n="theName";
  const char *f="m1 * m2";
  p.SetField("NAME", n);
  p.SetField("FUNC", f);
  TS_ASSERT(ExportMultArrayFunc(&p));
  TS_ASSERT_EQUALS2(db->m_table, "MultNames");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT_EQUALS2(db->m_nFields, -1);
  TS_ASSERT_EQUALS2(db->m_nRow, -1);
  TS_ASSERT(db->m_iVals.size() == 1);
  TS_ASSERT(db->m_iVals.find("MultID") != db->m_iVals.end());
  TS_ASSERT(db->m_sVals.size() == 2);
  TS_ASSERT(db->m_sVals.find("MultName") != db->m_sVals.end());
  TS_ASSERT(db->m_sVals.find("Function") != db->m_sVals.end());

  NumberMultArrays()=0;
  mapMultNamesId().clear();
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testCalculateNumDbFields ()
{
  CStr tab("RIV");
  std::vector<CStr> fStrings;
  std::map<int, CStr> srcIdx_destField;
  int nFields, nAux, nBcDbFields, nDbFields, ifaceIdx;

  nFields = 8;
  nAux = 2;
  fStrings.push_back("k");
  fStrings.push_back("i");
  fStrings.push_back("j");
  fStrings.push_back("Stage");
  fStrings.push_back("Cond");
  fStrings.push_back("Rbot");
  fStrings.push_back("IFACE");
  fStrings.push_back("CellGrp");
  srcIdx_destField[4] = "Condfact";
  CalculateNumDbFields(tab, nFields, nAux, fStrings, srcIdx_destField,
                       nBcDbFields, nDbFields, ifaceIdx);
  TS_ASSERT_EQUALS(nBcDbFields, 5);
  TS_ASSERT_EQUALS(nDbFields, 8);
  TS_ASSERT_EQUALS(ifaceIdx, 6);
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testCalcListBcCellId ()
{
  int nF(4);
  CellIdToIJK g(15, 10);
  Real d[8] = {1.0,7.0,9.0,55.0,2.0,3.0,5.0,25.0};
  TS_ASSERT_EQUALS(69, CalcListBcCellId(d,g,0,nF));
  TS_ASSERT_EQUALS(175, CalcListBcCellId(d,g,1,nF));
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportListParameterData ()
{
  using namespace MfData::Packages;
  MfData::MfPackage p(LPRM);
  int nF(8), nBc(1), nAux(3);
  Real d[72] = {1.0,36.0,11.0,300.0,80000.0,6.0,1.0,-1.0,
                0,0,0,0,0,0,0,0,
                0,0,0,0,0,0,0,0,
                0,0,0,0,0,0,0,0,
                1.0,36.0,11.0,300.0,1.0,6.0,1.0,-1.0,
                1.0,37.0,28.0,300.0,(Real)1.2,5.0,(Real)1.2,-2.0,
                1.0,38.0,28.0,300.0,(Real)1.3,5.0,(Real)1.2,-2.0,
                1.0,37.0,28.0,300.0,1.0,6.0,1.0,-1.0,
                1.0,38.0,28.0,300.0,1.0,6.0,1.0,-1.0};
  char aux[80] = {73,70,65,67,69,32,32,32,32,32,32,32,32,32,32,32,
                  67,79,78,68,70,65,67,84,32,32,32,32,32,32,32,32,
                  67,69,76,76,71,82,80,32,32,32,32,32,32,32,32,32,
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
  p.SetField(ListParameter::NUMBC, &nBc);
  p.SetField(ListParameter::NUMFIELDS, &nF);
  p.SetField(ListParameter::NAUX, &nAux);
  p.SetField(ListParameter::DATA, d);
  p.SetField(ListParameter::AUX, aux);

  {
    DbMock db1, *db;
    db = static_cast<DbMock*>(Db::db());
    // make the parameter
    const char *nm="mypar";
    const char *typ="DRN";
    int s(5);
    ParamList *list(0);
    Parameters::GetParameterList(&list);
    list->Clear();
    Param par(nm,-300.0,typ);
    list->PushBack(&par);
    p.SetField(ListParameter::PNAME, nm);
    p.SetField(ListParameter::PTYPE, typ);
    p.SetField(ListParameter::START, &s);

    TS_ASSERT(ExportListParameterData(&p, 1, 70, 42));
    TS_ASSERT_EQUALS2(db->m_table, "DRN");
    TS_ASSERT_EQUALS2(db->m_query, "");
    TS_ASSERT_EQUALS2(db->m_nFields, 7);
    TS_ASSERT_EQUALS2(db->m_nRow, 1);
    TS_ASSERT(db->m_iVals.size() == 3);
    TS_ASSERT(db->m_iVals.find("IFACE") != db->m_iVals.end());
    TS_ASSERT(db->m_iVals.find("IJK") != db->m_iVals.end());
    TS_ASSERT(db->m_iVals.find("SPID") != db->m_iVals.end());
    Real r(0);
    TS_ASSERT(db->GetMap(r).size() == 4);
    TS_ASSERT(db->GetMap(r).find("Elevation") != db->GetMap(r).end());
    TS_ASSERT(db->GetMap(r).find("Cond") != db->GetMap(r).end());
    TS_ASSERT(db->GetMap(r).find("Condfact") != db->GetMap(r).end());
    TS_ASSERT(db->GetMap(r).find("CELLGRP") != db->GetMap(r).end());
    TS_ASSERT(db->GetMap(r)["Cond"][0] == -300);

    list->Clear();
  }
  {
    DbMock db1, *db;
    db = static_cast<DbMock*>(Db::db());
    // make the parameter
    const char *nm="mypar";
    const char *typ="DRN";
    int s(6);
    ParamList *list(0);
    Parameters::GetParameterList(&list);
    Param par(nm,-200.0,typ);
    list->PushBack(&par);
    p.SetField(ListParameter::PNAME, nm);
    p.SetField(ListParameter::PTYPE, typ);
    p.SetField(ListParameter::START, &s);

    // change the number of BCs
    nBc = 2;
    TS_ASSERT(ExportListParameterData(&p, 1, 70, 42));
    TS_ASSERT_EQUALS2(db->m_table, "DRN");
    TS_ASSERT_EQUALS2(db->m_query, "");
    TS_ASSERT_EQUALS2(db->m_nFields, 7);
    TS_ASSERT_EQUALS2(db->m_nRow, 2);
    TS_ASSERT(db->m_iVals.size() == 3);
    TS_ASSERT(db->m_iVals.find("IFACE") != db->m_iVals.end());
    TS_ASSERT(db->m_iVals.find("IJK") != db->m_iVals.end());
    TS_ASSERT(db->m_iVals.find("SPID") != db->m_iVals.end());
    TS_ASSERT(db->m_iVals["IFACE"].size() == 2);
    Real r(0);
    TS_ASSERT(db->GetMap(r).size() == 4);
    TS_ASSERT(db->GetMap(r).find("Elevation") != db->GetMap(r).end());
    TS_ASSERT(db->GetMap(r).find("Cond") != db->GetMap(r).end());
    TS_ASSERT(db->GetMap(r).find("Condfact") != db->GetMap(r).end());
    TS_ASSERT(db->GetMap(r).find("CELLGRP") != db->GetMap(r).end());
    TS_ASSERT(db->GetMap(r)["Condfact"].size() == 2);
    TS_ASSERT(db->GetMap(r)["Cond"][0] == -200);
    TS_ASSERT(db->GetMap(r)["Cond"][1] == -200);

    list->Clear();
  }
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportHFBListParameterData ()
{
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  Real d[35] = {1,2,4,3,4,(Real).001,0,
                1,3,4,4,4,(Real).002,0,
                0,0,0,0,0,0,0,
                2,1,5,2,5,(Real).1,0,
                2,3,5,4,5,(Real).01,0};
  int nBc(2), start(3);
  Real key(-200), r(0);
  CellIdToIJK grid(10,15);
  TS_ASSERT(ExportHFBListParameterData(nBc, d, start, key, 2, grid));
  TS_ASSERT_EQUALS2(db->m_table, "HFB6");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT_EQUALS2(db->m_nFields, 5);
  TS_ASSERT_EQUALS2(db->m_nRow, 2);
  TS_ASSERT(db->m_iVals.size() == 3);
  TS_ASSERT(db->m_iVals.find("IJK1") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find("IJK2") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find("SPID") != db->m_iVals.end());
  TS_ASSERT(db->GetMap(r).size() == 2);
  TS_ASSERT(db->GetMap(r).find("Hydchr") != db->GetMap(r).end());
  TS_ASSERT(db->GetMap(r).find("Factor") != db->GetMap(r).end());
  TS_ASSERT(db->GetMap(r)["Hydchr"].size() == 2);
  TS_ASSERT_EQUALS(db->GetMap(r)["Hydchr"][0], -200);
  TS_ASSERT_EQUALS(db->GetMap(r)["Hydchr"][1], -200);
  TS_ASSERT_EQUALS(db->GetMap(r)["Factor"][0], (Real).1);
  TS_ASSERT_EQUALS(db->GetMap(r)["Factor"][1], (Real).01);
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportParamTable ()
{
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  ParamList list;
  Param p0("p0",-10,"DRN",5,1,10);
  Param p1("p1",-5,"RIV",10,5,20);
  list.PushBack(&p0);
  list.PushBack(&p1);

  ExportParamTable(&list);
  TS_ASSERT_EQUALS2(db->m_table, "Params");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT_EQUALS2(db->m_nFields, 14);
  TS_ASSERT_EQUALS2(db->m_nRow, 2);
  TS_ASSERT(db->m_iVals.size() == 4);
  TS_ASSERT(db->m_iVals.find("ParamID") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find("LN") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find("ISENS") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find("LogInterp") != db->m_iVals.end());
  TS_ASSERT(db->m_sVals.size() == 3);
  TS_ASSERT(db->m_sVals.find("PARNAM") != db->m_sVals.end());
  TS_ASSERT(db->m_sVals.find("PARTYPE") != db->m_sVals.end());
  TS_ASSERT(db->m_sVals.find("Tied") != db->m_sVals.end());
  TS_ASSERT(db->m_dVals.size() == 7);
  TS_ASSERT(db->m_dVals.find("Parval") != db->m_dVals.end());
  TS_ASSERT(db->m_dVals.find("B") != db->m_dVals.end());
  TS_ASSERT(db->m_dVals.find("Keyval") != db->m_dVals.end());
  TS_ASSERT(db->m_dVals.find("BL") != db->m_dVals.end());
  TS_ASSERT(db->m_dVals.find("BU") != db->m_dVals.end());
  TS_ASSERT(db->m_dVals.find("BSCAL") != db->m_dVals.end());
  TS_ASSERT(db->m_dVals.find("LogMinVal") != db->m_dVals.end());
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportParInstances ()
{
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  std::vector<CStr> iNames;
  std::vector<int>  parId;
  iNames.push_back("i1");
  iNames.push_back("i2");
  iNames.push_back("i3");
  parId.push_back(1);
  parId.push_back(1);
  parId.push_back(2);
  ExportParInstances(iNames, parId);
  TS_ASSERT_EQUALS2(db->m_table, "ParInstances");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT_EQUALS2(db->m_nFields, 3);
  TS_ASSERT_EQUALS2(db->m_nRow, 3);
  TS_ASSERT(db->m_iVals.size() == 2);
  TS_ASSERT(db->m_iVals.find("ParInstID") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find("ParamID") != db->m_iVals.end());
  TS_ASSERT(db->m_sVals.size() == 1);
  TS_ASSERT(db->m_sVals.find("InstName") != db->m_sVals.end());
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportParInstSp ()
{
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  int i[4]={9,8,7,6}, j[4]={1,2,3,4};
  std::vector<int> ids(&i[0], &i[4]), sp(&j[0], &j[4]);
  ExportParInstSp(ids, sp);
  TS_ASSERT_EQUALS2(db->m_table, "ParInstSP");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT_EQUALS2(db->m_nFields, 2);
  TS_ASSERT_EQUALS2(db->m_nRow, 4);
  TS_ASSERT(db->m_iVals.size() == 2);
  TS_ASSERT(db->m_iVals.find("ParInstID") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find("SPID") != db->m_iVals.end());
}
static void CreateSomeTestParams (ParamList &list)
{
  PClust clst, clst1, clst2;
  std::vector<int> iz;
  iz.push_back(1);
  iz.push_back(2);
  clst.m_iz = iz;
  clst.m_mlt = "mult1";
  clst.m_zon = "zone1";
  iz.push_back(3);
  iz.push_back(4);
  clst1.m_iz = iz;
  clst1.m_mlt = "mult2";
  clst1.m_zon = "zone2";
  iz.pop_back();
  iz.push_back(5);
  iz.push_back(6);
  clst2.m_iz = iz;
  clst2.m_lay = 2;
  clst2.m_mlt = "mult3";
  clst2.m_zon = "zone4";

  Param p0("p0", -10, "RCH",20);
  Param p1("p1", -11, "HK",21);
  Param p2("p2", -12, "EVT",22);
  p0.m_instNames.push_back("p0_1");
  p0.m_instNames.push_back("p0_2");
  p0.m_instStress["p0_1"].push_back(1);
  p0.m_instStress["p0_2"].push_back(2);
  p0.m_instStress["p0_2"].push_back(3);
  p0.m_clust.push_back(clst);
  p0.m_clust.push_back(clst1);

  p1.m_clust.push_back(clst1);
  p1.m_clust.push_back(clst2);

  p2.m_instNames.push_back("p2_1");
  p2.m_instNames.push_back("p2_2");
  p2.m_instStress["p2_1"].push_back(1);
  p2.m_instStress["p2_1"].push_back(3);
  p2.m_instStress["p2_2"].push_back(2);
  p2.m_clust.push_back(clst);
  p2.m_clust.push_back(clst1);
  p2.m_clust.push_back(clst1);
  p2.m_clust.push_back(clst2);

  list.PushBack(&p0);
  list.PushBack(&p1);
  list.PushBack(&p2);
}
//------------------------------------------------------------------------------
void ExpGeoDbT::test1GetInstances ()
{
  ParamList list;
  CreateSomeTestParams(list);
  int i;
  std::vector<CStr> iNames;
  std::vector<int>  parIds;
  std::vector<int>  parInstIdSp;
  std::vector<int>  sp;
  GetInstances(&list, iNames, parIds, parInstIdSp, sp);
  char *c[5] = {"p0_1", "p0_2", "p1", "p2_1", "p2_2" };
  int ids[5]={1,1,2,3,3};
  int instId[6]={1,2,2,4,4,5};
  int spId[6]={1,2,3,1,3,2};
  TS_ASSERT(iNames.size() == 5);
  TS_ASSERT(parIds.size() == 5);
  TS_ASSERT(parInstIdSp.size() == 6);
  TS_ASSERT(sp.size() == 6);
  for (i=0; i<5; i++)
  {
    TS_ASSERT_EQUALS2(c[i], iNames[i]);
    TS_ASSERT_EQUALS(ids[i], parIds[i]);
  }
  for (i=0; i<6; i++)
  {
    TS_ASSERT_EQUALS(instId[i], parInstIdSp[i]);
    TS_ASSERT_EQUALS(spId[i], sp[i]);
  }
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testGetClusterData ()
{
  ParamList list;
  CreateSomeTestParams(list);
  char *c[5] = {"p0_1", "p0_2", "p1", "p2_1", "p2_2" };
  std::map<CStr, int> mapINameId;
  for (int i=0; i<5; i++)
    mapINameId[c[i]] = i+1;
  std::vector<int> instIds, lay;
  std::vector<CStr> mName, zName, hName;
  std::vector< std::vector<int> > iz;

  GetClusterData(&list, mapINameId, instIds, lay, mName, zName, hName, iz);
  TS_ASSERT(instIds.size() == 8);
  TS_ASSERT(lay.size() == 8);
  TS_ASSERT(mName.size() == 8);
  TS_ASSERT(zName.size() == 8);
  TS_ASSERT(iz.size() == 8);
  int ids[8]={1,2,3,3,4,4,5,5};
  int lays[8]={1,1,1,2,1,1,1,2};
  char *m[8]={"mult1","mult2","mult2","mult3","mult1","mult2","mult2","mult3"};
  char *z[8]={"zone1","zone2","zone2","zone4","zone1","zone2","zone2","zone4"};
  for (int i=0; i<8; i++)
  {
    TS_ASSERT_EQUALS(ids[i], instIds[i]);
    TS_ASSERT_EQUALS(lays[i], lay[i]);
    TS_ASSERT_EQUALS2(m[i], mName[i]);
    TS_ASSERT_EQUALS2(z[i], zName[i]);
  }
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportClstTab ()
{
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  int i[4]={1,2,3,4}, j[4]={9,8,7,6};
  std::vector<int> instIds(&i[0], &i[4]), lay(&j[0], &j[4]);
  char *m[4]={"m1", "m2", "m3", "m4"};
  char *z[4]={"z1", "z2", "z3", "z4"};
  char *h[4]={"h1", "h2", "h3", "h4"};
  std::vector<CStr> mName(&m[0], &m[4]), zName(&z[0], &z[4]),
                    hName(&h[0], &h[4]);

  ExportClstTab(instIds, lay, mName, zName, hName);
  TS_ASSERT_EQUALS2(db->m_table, "Clusters");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT_EQUALS2(db->m_nFields, 5);
  TS_ASSERT_EQUALS2(db->m_nRow, 4);
  TS_ASSERT(db->m_iVals.size() == 5);
  TS_ASSERT(db->m_sVals.size() == 1);
  TS_ASSERT(db->m_iVals.find("ClusterID") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find("ParInstID") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find("Layer") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find("MultID") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find("ZoneID") != db->m_iVals.end());
  TS_ASSERT(db->m_sVals.find("HGUNAME") != db->m_sVals.end());

  NumberMultArrays() = 0;
  mapMultNamesId().clear();
  NumberZoneArrays() = 0;
  mapZoneNamesId().clear();
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportIZTab ()
{
  std::vector< std::vector<int> > iz;
  std::vector<int> tmp;
  tmp.push_back(1);
  iz.push_back(tmp);
  tmp.push_back(2);
  tmp.push_back(3);
  iz.push_back(tmp);
  tmp.pop_back();
  iz.push_back(tmp);

  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  ExportIZTab(iz);
  TS_ASSERT_EQUALS2(db->m_table, "IZ");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT_EQUALS2(db->m_nFields, 2);
  TS_ASSERT_EQUALS2(db->m_nRow, 6);
  TS_ASSERT(db->m_iVals.size() == 2);
  TS_ASSERT(db->m_iVals.find("ClusterID") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find("IZ") != db->m_iVals.end());
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testExportPilotPointTab ()
{
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  int scatIdx(2), parId(3);
  int isens[5] = {1,1,0,1,1};
  double svals[5] = {12,3,4,5};
  std::vector<int> vI(&isens[0], &isens[5]);
  std::vector<double> vR(&svals[0], &svals[5]);

  ExportPilotPointTab(parId, scatIdx, vR, vI);
  TS_ASSERT_EQUALS2(db->m_table, "PilotPts");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT_EQUALS2(db->m_nFields, 6);
  TS_ASSERT_EQUALS2(db->m_nRow, 5);
  TS_ASSERT(db->m_iVals.size() == 4);
  TS_ASSERT(db->m_iVals.find("ParamID") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find("ISENS") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find("MultID") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find("SourceID") != db->m_iVals.end());
  TS_ASSERT(db->m_sVals.size() == 1);
  TS_ASSERT(db->m_sVals.find("PointName") != db->m_sVals.end());
  TS_ASSERT(db->m_dVals.size() == 2);
  TS_ASSERT(db->m_dVals.find("Parval") != db->m_dVals.end());
  TS_ASSERT(db->m_dVals.find("B") != db->m_dVals.end());
  NumberMultArrays() = 0;
  mapMultNamesId().clear();
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testPPToMultNameTab ()
{
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  NumberMultArrays() = 4;
  std::vector<int> ids;

  PPToMultNameTab(2, 10, ids);
  TS_ASSERT_EQUALS2(db->m_table, "MultNames");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT_EQUALS2(db->m_nFields, 3);
  TS_ASSERT_EQUALS2(db->m_nRow, 10);
  TS_ASSERT(db->m_iVals.size() == 1);
  TS_ASSERT(db->m_iVals.find("MultID") != db->m_iVals.end());
  TS_ASSERT(db->m_sVals.size() == 1);
  TS_ASSERT(db->m_sVals.find("MultName") != db->m_sVals.end());
  TS_ASSERT(db->m_dVals.size() == 1);
  TS_ASSERT(db->m_dVals.find("ArrayMult") != db->m_dVals.end());

  TS_ASSERT(ids.size() == 10);
  TS_ASSERT_EQUALS(5, ids.front());
  TS_ASSERT_EQUALS(14, ids.back());

  NumberMultArrays() = 0;
  mapMultNamesId().clear();
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testPPToMultipliersTab ()
{
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());

  int nWts(5), nCells(10), iIds[5]={1,2,3,4,5};
  int idx, idxs[50];
  Real wts[50];
  idx = 2;
  for (int i=0; i<50; i++)
  {
    if (idx%6 == 0)
      idx = 1;
    idxs[i] = idx++;
    wts[i] = 1 / (float)idx;
  }

  std::vector<int> ids(&iIds[0], &iIds[5]);
  PPToMultipliersTab(ids, nWts, nCells, idxs, wts);
  TS_ASSERT_EQUALS2(db->m_table, "Multipliers");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT_EQUALS2(db->m_nFields, 3);
  TS_ASSERT_EQUALS2(db->m_nRow, 50);
  TS_ASSERT(db->m_iVals.size() == 2);
  TS_ASSERT(db->m_iVals.find("MultID") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find("IJ") != db->m_iVals.end());
  Real r(0);
  TS_ASSERT(db->GetMap(r).size() == 1);
  TS_ASSERT(db->GetMap(r).find("RMLT") != db->GetMap(r).end());
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testNameFile ()
{
  using namespace MfData;
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  MfData::MfPackage p("NAM");
  const char *nm[3]={"file.glo", "file.bas", "file.lpf"};
  const char *type[3] = {"GLOBAL", "BAS6", "LPF"};
  int units[3] = {1,3,4};
  for (int i=0; i<3; i++)
  {
    p.SetField(Packages::NameFile::FNAME, nm[i]);
    p.SetField(Packages::NameFile::FTYPE, type[i]);
    p.SetField(Packages::NameFile::NIU, &units[i]);
    ExportNameFile(false, &p);
  }

  ExportNameFile(true, 0);

  TS_ASSERT_EQUALS2(db->m_table, "NameFile");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT_EQUALS2(db->m_nFields, 4);
  TS_ASSERT_EQUALS2(db->m_nRow, 3);
  TS_ASSERT(db->m_iVals.size() == 2);
  TS_ASSERT(db->m_iVals.find("Nunit") != db->m_iVals.end());
  TS_ASSERT(db->m_iVals.find("Use") != db->m_iVals.end());
  TS_ASSERT(db->m_sVals.size() == 2);
  TS_ASSERT(db->m_sVals.find("FileType") != db->m_sVals.end());
  TS_ASSERT(db->m_sVals.find("Fname") != db->m_sVals.end());
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testNameFileFilename ()
{
  using namespace MfData;
  DbMock db1, *db;
  const char *fwpath = "model.mfn";
  db = static_cast<DbMock*>(Db::db());
  MfData::MfPackage p("NAM1");

  ExportNameFileFilename(&p);
  TS_ASSERT_EQUALS2(db->m_table, "");

  p.SetField(Packages::NameFile::FNAME, fwpath);
  ExportNameFileFilename(&p);
  TS_ASSERT_EQUALS2(db->m_table, "MDFGlobals");
  TS_ASSERT_EQUALS2(db->m_query, "OID = 1");
  TS_ASSERT(db->m_sVals.size() == 1);
  TS_ASSERT(db->m_sVals.find("NameFilePath") != db->m_sVals.end());
}
//------------------------------------------------------------------------------
void ExpGeoDbT::testCheckFieldIsParameterFactorFieldAndUpdateName ()
{
  CStr tab[9] = {"RIV", "DRN", "GHB", "DRT", "CHD", "CHD", "WEL", "crap", "RIV"};
  CStr field[9] = {"CoNdFaCt", "CoNdFaCt", "CoNdFaCt", "CoNdFaCt",
                   "ShDfAcT", "EhDfAcT", "QfAcT", "crap", "crap"};
  CStr rf[9] =    {"Condfact", "Condfact", "Condfact", "Condfact",
                   "Shdfact", "Ehdfact", "Qfact", "crap", "crap"};
  bool rvals[9] = {1,1,1,1,1,1,1,0,0};

  for (int i=0; i<9; i++)
  {
    TS_ASSERT_EQUALS(rvals[i],
      CheckFieldIsParameterFactorFieldAndUpdateName(tab[i], field[i]));
    TS_ASSERT(rf[i] == field[i]);
  }
}
void ExpGeoDbT::testExportUZFVars()
{
  using namespace MfData::Packages;
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  MfData::MfPackage pUZFLine1(MfData::Packages::UZFLine1);
  int nuztop(0), iuzfopt(1), irunflg(2), ietflg(3), iuzfcb1(4), iuzfcb2(5),
      ntrail2(6), nsets2(7), nuzgag(8);
  Real surfdep(1.0);

  pUZFLine1.SetField(UZFpack::NUZTOP, &nuztop);
  pUZFLine1.SetField(UZFpack::IUZFOPT, &iuzfopt);
  pUZFLine1.SetField(UZFpack::IRUNFLG, &irunflg);
  pUZFLine1.SetField(UZFpack::IETFLG, &ietflg);
  pUZFLine1.SetField(UZFpack::NTRAIL2, &ntrail2);
  pUZFLine1.SetField(UZFpack::NSETS2, &nsets2);
  pUZFLine1.SetField(UZFpack::NUZGAG, &nuzgag);
  pUZFLine1.SetField(UZFpack::SURFDEP, &surfdep);
  pUZFLine1.SetField(UZFpack::IUZFCB1, &iuzfcb1);
  pUZFLine1.SetField(UZFpack::IUZFCB2, &iuzfcb2);


  TS_ASSERT(ExportUZFVars(&pUZFLine1));

  TS_ASSERT_EQUALS2(db->m_table, "UZFVars");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT(db->m_iVals.size() == 7);
  if (sizeof(Real) == sizeof(float))
  {
    TS_ASSERT(db->m_fVals.size() == 1);
  }
  else
  {
    TS_ASSERT(db->m_dVals.size() == 1);
  }
}
void ExpGeoDbT::testExportUZFCbf()
{
  using namespace MfData::Packages;
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  MfData::MfPackage pUZFLine1(MfData::Packages::UZFLine1);
  int nuztop(0), iuzfopt(1), irunflg(2), ietflg(3), iuzfcb1(4), iuzfcb2(5),
      ntrail2(6), nsets2(7), nuzgag(8);
  Real surfdep(1.0);

  pUZFLine1.SetField(UZFpack::NUZTOP, &nuztop);
  pUZFLine1.SetField(UZFpack::IUZFOPT, &iuzfopt);
  pUZFLine1.SetField(UZFpack::IRUNFLG, &irunflg);
  pUZFLine1.SetField(UZFpack::IETFLG, &ietflg);
  pUZFLine1.SetField(UZFpack::NTRAIL2, &ntrail2);
  pUZFLine1.SetField(UZFpack::NSETS2, &nsets2);
  pUZFLine1.SetField(UZFpack::NUZGAG, &nuzgag);
  pUZFLine1.SetField(UZFpack::SURFDEP, &surfdep);
  pUZFLine1.SetField(UZFpack::IUZFCB1, &iuzfcb1);
  pUZFLine1.SetField(UZFpack::IUZFCB2, &iuzfcb2);


  TS_ASSERT(ExportUZFCbf(&pUZFLine1));

  TS_ASSERT_EQUALS2(db->m_table, "CBFlags");
  TS_ASSERT_EQUALS2(db->m_query, "OID = 1");
  TS_ASSERT(db->m_iVals.size() == 2);
}
void ExpGeoDbT::testExportUZFLine8()
{
  using namespace MfData::Packages;
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  MfData::MfPackage pUZFLine1(MfData::Packages::UZFLine1);
  MfData::MfPackage pUZFLine8(MfData::Packages::UZFLine8);

  int iuzfopt(0), irunflg(1), nuzgag(0);
  int iuzlist[] = {  1,  2,  3,  4,
                     0,  0,  1,  4,
                     5,  6,  7,  8 };

  pUZFLine1.SetField("IUZFOPT", &iuzfopt);
  pUZFLine1.SetField("IRUNFLG", &irunflg);
  pUZFLine1.SetField("NUZGAG", &nuzgag);

  // All fields need to be set
  ExportUZFLine8(&pUZFLine1, &pUZFLine8);
  TS_ASSERT_EQUALS2(db->m_table, "");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT_EQUALS2(db->m_nFields, -1);
  TS_ASSERT_EQUALS2(db->m_nRow, -1);

  // write nothing when NUZGAG is zero
  // don't write arrays when they aren't available
  pUZFLine8.SetField("IUZLIST", iuzlist);
  ExportUZFLine8(&pUZFLine1, &pUZFLine8);
  TS_ASSERT_EQUALS2(db->m_table, "");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT_EQUALS2(db->m_nFields, -1);
  TS_ASSERT_EQUALS2(db->m_nRow, -1);

  // write NUZGAG items
  nuzgag = 3;
  irunflg = 0;
  iuzfopt = 1;
  ExportUZFLine8(&pUZFLine1, &pUZFLine8);
  TS_ASSERT_EQUALS2(db->m_table, "UZFGages");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT_EQUALS2(db->m_iVals.size(), 4);
  TS_ASSERT_EQUALS2(db->m_dVals.size(), 0);
  TS_ASSERT_EQUALS2(db->m_fVals.size(), 0);
  TS_ASSERT_EQUALS2(db->m_iVals["IUZROW"][0], 5);
  TS_ASSERT_EQUALS2(db->m_iVals["IUZCOL"][0], 6);
  TS_ASSERT_EQUALS2(db->m_iVals["IFTUNIT"][0], 7);
  TS_ASSERT_EQUALS2(db->m_iVals["IUZOPT"][0], 8);
}
void ExpGeoDbT::testExportUZFStressPeriod()
{
  using namespace MfData::Packages;
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  MfData::MfPackage pUZFLine1(MfData::Packages::UZFLine1);
  MfData::MfPackage pUZFSp(MfData::Packages::UZFStressPeriod);
  int iuzfopt(1), ietflg(1), nuzf1(1), nuzf2(2), nuzf3(3), nuzf4(4);

  pUZFLine1.SetField("IUZFOPT", &iuzfopt);
  pUZFLine1.SetField("IETFLG", &ietflg);
  pUZFSp.SetField("NUZF1", &nuzf1);
  pUZFSp.SetField("NUZF2", &nuzf2);
  pUZFSp.SetField("NUZF3", &nuzf3);

  // All fields need to be set
  ExportUZFStressPeriod(&pUZFLine1, &pUZFSp, 1);
  TS_ASSERT_EQUALS2(db->m_table, "");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT_EQUALS2(db->m_nFields, -1);
  TS_ASSERT_EQUALS2(db->m_nRow, -1);

  pUZFSp.SetField("NUZF4", &nuzf4);
  ExportUZFStressPeriod(&pUZFLine1, &pUZFSp, 1);
  TS_ASSERT_EQUALS2(db->m_table, "UZFStressArrayMult");
  TS_ASSERT_EQUALS2(db->m_query, "SPID = 1");
  TS_ASSERT(db->m_iVals.size() == 5);
  TS_ASSERT(db->m_fVals.size() == 0);
  TS_ASSERT(db->m_dVals.size() == 0);
}
void ExpGeoDbT::testExportSFRLine1()
{
  using namespace MfData::Packages;
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  MfData::MfPackage pSFRLine1(MfData::Packages::SFRLine1);
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

  // All fields need to be set
  ExportSFRLine1(&pSFRLine1);

  TS_ASSERT_EQUALS2(db->m_table, "");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT(db->m_iVals.size() == 0);

  // Set missing
  pSFRLine1.SetField("NSFRSETS", &nsfrsets);

  ExportSFRLine1(&pSFRLine1);

  TS_ASSERT_EQUALS2(db->m_table, "CBFlags");
  TS_ASSERT_EQUALS2(db->m_query, "OID = 1");
  TS_ASSERT(db->m_iVals.size() == 2);
}
void ExpGeoDbT::testExportSFRLine2()
{
  using namespace MfData::Packages;
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  MfData::MfPackage pSFRLine1(MfData::Packages::SFRLine1);
  MfData::MfPackage pSFRLine2(MfData::Packages::SFRLine2);
  int nstrm(-23), nss(7), isfropt(0), nrow(6), ncol(6), nstrmd(24), nistrmd(5);

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

  // All fields need to be set
  ExportSFRLine2(&pSFRLine1, &pSFRLine2, nrow, ncol);
  TS_ASSERT_EQUALS2(db->m_table, "");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT(db->m_iVals.size() == 0);
  TS_ASSERT(db->m_fVals.size() == 0);
  TS_ASSERT(db->m_dVals.size() == 0);
  TS_ASSERT_EQUALS2(db->m_nFields, -1);
  TS_ASSERT_EQUALS2(db->m_nRow, -1);

  // Set missing
  pSFRLine2.SetField("STRM", &strm[0]);
  ExportSFRLine2(&pSFRLine1, &pSFRLine2, nrow, ncol);
  TS_ASSERT_EQUALS2(db->m_table, "SFRReaches");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT(db->m_iVals.size() == 3);
  TS_ASSERT(db->m_dVals.size() == 1);
  TS_ASSERT_EQUALS2(db->m_nFields, 4);
  TS_ASSERT_EQUALS2(db->m_nRow, 23);
}
void ExpGeoDbT::testExportSFRLine5()
{
  using namespace MfData::Packages;
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  MfData::MfPackage pSFRLine5(MfData::Packages::SFRLine5);
  int itmp(1), irdflg(2), iptflg(3), stressPeriod(2);

  pSFRLine5.SetField("ITMP", &itmp);
  pSFRLine5.SetField("IRDFLG", &irdflg);

  // All fields need to be set
  ExportSFRLine5(&pSFRLine5, stressPeriod);
  TS_ASSERT_EQUALS2(db->m_table, "");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT(db->m_iVals.size() == 0);
  TS_ASSERT(db->m_dVals.size() == 0);
  TS_ASSERT_EQUALS2(db->m_nFields, -1);
  TS_ASSERT_EQUALS2(db->m_nRow, -1);

  // Set missing
  pSFRLine5.SetField("IPTFLG", &iptflg);
  ExportSFRLine5(&pSFRLine5, stressPeriod);
  TS_ASSERT_EQUALS2(db->m_table, "StressPeriods");
  TS_ASSERT_EQUALS2(db->m_query, "SPID = 2");
  TS_ASSERT(db->m_iVals.size() == 3);
  TS_ASSERT(db->m_dVals.size() == 0);
}
void ExpGeoDbT::testExportSFRLine6()
{
  using namespace MfData::Packages;
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());
  MfData::MfPackage pSFRLine1(MfData::Packages::SFRLine1);
  MfData::MfPackage pSFRLine6(MfData::Packages::SFRLine6);
  int sz(26);
  int nstrm(-10), nss(3), isfropt(0), stressPeriod(2);
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

  // All fields need to be set
  ExportSFRLine6(&pSFRLine1, &pSFRLine6, stressPeriod);
  TS_ASSERT_EQUALS2(db->m_table, "");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT(db->m_iVals.size() == 0);
  TS_ASSERT(db->m_dVals.size() == 0);
  TS_ASSERT_EQUALS2(db->m_nFields, -1);
  TS_ASSERT_EQUALS2(db->m_nRow, -1);

  // Set missing
  pSFRLine6.SetField("QSTAGE", &qstage[0]);
  ExportSFRLine6(&pSFRLine1, &pSFRLine6, stressPeriod);
  TS_ASSERT_EQUALS2(db->m_table, "SFRFlowTab");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT(db->m_iVals.size() == 2);
  TS_ASSERT(db->m_dVals.size() == 3);
  TS_ASSERT_EQUALS2(db->m_nFields, 5);
  TS_ASSERT_EQUALS2(db->m_nRow, 5);
}
void ExpGeoDbT::testExportLAK()
{
  using namespace MfData::Packages;

  int nlakes = 3;
  int itrss = 1;
  int ilkcb = 1;
  int nssitr = 20;
  Real theta = (Real)0.1;
  Real sscncr = (Real)0.008;

  MfData::MfPackage p(MfData::Packages::LAK);
  p.SetField("NLAKES", &nlakes);
  p.SetField("ILKCB", &ilkcb);
  p.SetField("THETA", &theta);
  p.SetField("NSSITR", &nssitr);
  p.SetField("SSCNCR", &sscncr);

  {
    DbMock db1, *db;
    db = static_cast<DbMock*>(Db::db());

    // All fields need to be set
    ExportLAK(&p);
    TS_ASSERT_EQUALS2(db->m_table, "");
    TS_ASSERT_EQUALS2(db->m_query, "");
    TS_ASSERT(db->m_iVals.size() == 0);
    TS_ASSERT(db->m_dVals.size() == 0);
    TS_ASSERT_EQUALS2(db->m_nFields, -1);
    TS_ASSERT_EQUALS2(db->m_nRow, -1);

    // Set missing
    p.SetField("ITRSS", &itrss);
    ExportLAK(&p);
    TS_ASSERT_EQUALS2(db->m_table, "LAKVars");
    TS_ASSERT_EQUALS2(db->m_query, "");
    TS_ASSERT(db->m_iVals.size() == 1);
    if (sizeof(Real) == sizeof(float))
    {
      TS_ASSERT(db->m_fVals.size() == 2);
    }
    else
    {
      TS_ASSERT(db->m_dVals.size() == 2);
    }
  }

  // check export for steady state
  {
    DbMock db1, *db;
    db = static_cast<DbMock*>(Db::db());

    itrss = 0;

    ExportLAK(&p);
    TS_ASSERT_EQUALS2(db->m_table, "LAKVars");
    TS_ASSERT_EQUALS2(db->m_query, "");
    TS_ASSERT(db->m_iVals.size() == 1);
    TS_ASSERT(db->m_iVals.find("NSSITR") != db->m_iVals.end());
    TS_ASSERT_EQUALS2(db->m_iVals["NSSITR"][0], nssitr);
    if (sizeof(Real) == sizeof(float))
    {
      TS_ASSERT(db->m_fVals.size() == 2);
      TS_ASSERT(db->m_fVals.find("THETA") != db->m_fVals.end());
      TS_ASSERT(db->m_fVals.find("SSCNCR") != db->m_fVals.end());
      TS_ASSERT_DELTA(db->m_fVals["THETA"][0], theta, GDBTOL);
      TS_ASSERT_DELTA(db->m_fVals["SSCNCR"][0], sscncr, GDBTOL);
    }
    else
    {
      TS_ASSERT(db->m_dVals.size() == 2);
      TS_ASSERT(db->m_dVals.find("THETA") != db->m_dVals.end());
      TS_ASSERT(db->m_dVals.find("SSCNCR") != db->m_dVals.end());
      TS_ASSERT_DELTA(db->m_dVals["THETA"][0], theta, GDBTOL);
      TS_ASSERT_DELTA(db->m_dVals["SSCNCR"][0], sscncr, GDBTOL);
    }
  }

  // check export for negative theta
  {
    DbMock db1, *db;
    db = static_cast<DbMock*>(Db::db());

    itrss = 1;
    theta = (Real)-0.1;

    ExportLAK(&p);
    TS_ASSERT_EQUALS2(db->m_table, "LAKVars");
    TS_ASSERT_EQUALS2(db->m_query, "");
    TS_ASSERT(db->m_iVals.size() == 1);
    TS_ASSERT(db->m_iVals.find("NSSITR") != db->m_iVals.end());
    TS_ASSERT_EQUALS2(db->m_iVals["NSSITR"][0], nssitr);
    if (sizeof(Real) == sizeof(float))
    {
      TS_ASSERT(db->m_fVals.size() == 2);
      TS_ASSERT(db->m_fVals.find("THETA") != db->m_fVals.end());
      TS_ASSERT(db->m_fVals.find("SSCNCR") != db->m_fVals.end());
      TS_ASSERT_DELTA(db->m_fVals["THETA"][0], theta, GDBTOL);
      TS_ASSERT_DELTA(db->m_fVals["SSCNCR"][0], sscncr, GDBTOL);
    }
    else
    {
      TS_ASSERT(db->m_dVals.size() == 2);
      TS_ASSERT(db->m_dVals.find("THETA") != db->m_dVals.end());
      TS_ASSERT(db->m_dVals.find("SSCNCR") != db->m_dVals.end());
      TS_ASSERT_DELTA(db->m_dVals["THETA"][0], theta, GDBTOL);
      TS_ASSERT_DELTA(db->m_dVals["SSCNCR"][0], sscncr, GDBTOL);
    }
  }
}
void ExpGeoDbT::testExportLAKSP()
{
  using namespace MfData::Packages;

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

  MfData::MfPackage pLAK(MfData::Packages::LAK);
  pLAK.SetField("NLAKES", &nlakes);

  MfData::MfPackage p(MfData::Packages::LAKSP);
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
  MfData::MfGlobal g(3, 4, 2, 2, 3, 2, 0);

  // All fields need to be set
  {
    DbMock db1, *db;
    db = static_cast<DbMock*>(Db::db());

    ExportLAKSP(&g, &p, &pLAK);
    TS_ASSERT_EQUALS2(db->m_table, "");
    TS_ASSERT_EQUALS2(db->m_query, "");
    TS_ASSERT(db->m_iVals.size() == 0);
    TS_ASSERT(db->m_dVals.size() == 0);
    TS_ASSERT_EQUALS2(db->m_nFields, -1);
    TS_ASSERT_EQUALS2(db->m_nRow, -1);
  }

  p.SetField("CAUG", caug);

  // check export with number of solutes zero
  // itmp zero
  {
    DbMock db1, *db;
    db = static_cast<DbMock*>(Db::db());

    g.PutCurrentPeriod(1);
    ExportLAKSP(&g, &p, &pLAK);
    TS_ASSERT_EQUALS2(db->m_table, "StressPeriods");
    TS_ASSERT_EQUALS2(db->m_query, "SPID = 1");
    TS_ASSERT(db->m_iVals.size() == 3);
    TS_ASSERT(db->m_fVals.size() == 0);
  }

  // check export for 
  //   nsol 3
  //   itmp 1
  //   nslms 0
  {
    DbMock db1, *db;
    db = static_cast<DbMock*>(Db::db());

    itmp = 1;

    ExportLAKSP(&g, &p, &pLAK);
    TS_ASSERT_EQUALS2(db->m_table, "LAKArrays");
    TS_ASSERT_EQUALS2(db->m_query, "");
    TS_ASSERT(db->m_iVals.size() == 3);
    TS_ASSERT_EQUALS2(db->m_nFields, 4);
    TS_ASSERT_EQUALS2(db->m_nRow, 24);
    TS_ASSERT_EQUALS2(db->m_iVals["LKARR"][0], lkarr[0]);
    TS_ASSERT(db->m_dVals.size() == 1);
    TS_ASSERT_DELTA(db->m_dVals["BDLKNC"][0], bdlknc[0], GDBTOL);
  }

  // check export for 
  //   number of solutes 3
  //   itmp 1
  //   itmp1 >= 0
  //   nslms 2
  {
    DbMock db1, *db;
    db = static_cast<DbMock*>(Db::db());

    itmp = 1;
    itmp1 = 0;
    nslms = 2;
    g.PutCurrentPeriod(2);

    ExportLAKSP(&g, &p, &pLAK);
    TS_ASSERT_EQUALS2(db->m_table, "LAKRates");
    TS_ASSERT_EQUALS2(db->m_query, "");
    TS_ASSERT(db->m_iVals.size() == 2);
    TS_ASSERT_EQUALS2(db->m_nFields, 8);
    TS_ASSERT_EQUALS2(db->m_nRow, 3);
    if (sizeof(Real) == sizeof(float))
    {
      TS_ASSERT(db->m_fVals.size() == 4);
      TS_ASSERT(db->m_dVals.size() == 2);
      TS_ASSERT_DELTA(db->m_dVals[LAKSPpack::PRCPLK][0], prcplk[0], GDBTOL);
      TS_ASSERT_DELTA(db->m_dVals[LAKSPpack::EVAPLK][0], evaplk[0], GDBTOL);
      TS_ASSERT_DELTA(db->m_fVals[LAKSPpack::RNF][0], rnf[0], GDBTOL);
      TS_ASSERT_DELTA(db->m_fVals[LAKSPpack::WTHDRW][0], wthdrw[0], GDBTOL);
      TS_ASSERT_DELTA(db->m_fVals[LAKSPpack::SSMN][0], ssmn[0], GDBTOL);
      TS_ASSERT_DELTA(db->m_fVals[LAKSPpack::SSMX][0], ssmx[0], GDBTOL);
    }
    else
    {
      TS_ASSERT(db->m_dVals.size() == 6);
      TS_ASSERT_DELTA(db->m_dVals[LAKSPpack::PRCPLK][0], prcplk[0], GDBTOL);
      TS_ASSERT_DELTA(db->m_dVals[LAKSPpack::EVAPLK][0], evaplk[0], GDBTOL);
      TS_ASSERT_DELTA(db->m_dVals[LAKSPpack::RNF][0], rnf[0], GDBTOL);
      TS_ASSERT_DELTA(db->m_dVals[LAKSPpack::WTHDRW][0], wthdrw[0], GDBTOL);
      TS_ASSERT_DELTA(db->m_dVals[LAKSPpack::SSMN][0], ssmn[0], GDBTOL);
      TS_ASSERT_DELTA(db->m_dVals[LAKSPpack::SSMX][0], ssmx[0], GDBTOL);
    }
  }
}
void ExpGeoDbT::testExportSTRCbf()
{
  using namespace MfData::Packages;
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());

  int istcb1(40), istcb2(0);
  MfData::MfPackage p(MfData::Packages::STRSP);

  p.SetField("ISTCB1", &istcb1);
  p.SetField("ISTCB2", &istcb2);

  ExportSTRCbf(&p);
  TS_ASSERT_EQUALS2(db->m_table, "CBFlags");
  TS_ASSERT_EQUALS2(db->m_query, "OID = 1");
  TS_ASSERT(db->m_iVals.size() == 2);
  TS_ASSERT_EQUALS2(db->m_nFields, -1);
}
void ExpGeoDbT::testExportSTR()
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
  MfData::MfPackage p(MfData::Packages::STRSP);

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

  // All fields need to be set
  {
    DbMock db1, *db;
    db = static_cast<DbMock*>(Db::db());

    ExportSTR(&p, stressPeriod, nRow, nCol);
    TS_ASSERT_EQUALS2(db->m_table, "CBFlags");
  }

  // Set missing
  p.SetField("IDIVAR", idivar);
  {
    DbMock db1, *db;
    db = static_cast<DbMock*>(Db::db());

    ExportSTR(&p, stressPeriod, nRow, nCol);
    TS_ASSERT_EQUALS2(db->m_table, "STRIupseg");
    TS_ASSERT_EQUALS2(db->m_query, "");
    TS_ASSERT(db->m_iVals.size() == 3);
    TS_ASSERT(db->m_dVals.size() == 0);
    TS_ASSERT_EQUALS2(db->m_nFields, 3);
    TS_ASSERT_EQUALS2(db->m_nRow, 7);
    TS_ASSERT_EQUALS2(db->m_iVals["Seg"][0], 1);
    TS_ASSERT_EQUALS2(db->m_iVals["Seg"][1], 2);
    TS_ASSERT_EQUALS2(db->m_iVals["Seg"][2], 3);
    TS_ASSERT_EQUALS2(db->m_iVals["Seg"][3], 4);
    TS_ASSERT_EQUALS2(db->m_iVals["Seg"][4], 5);
    TS_ASSERT_EQUALS2(db->m_iVals["Seg"][5], 6);
    TS_ASSERT_EQUALS2(db->m_iVals["Seg"][6], 7);
    TS_ASSERT_EQUALS2(db->m_iVals["SPID"][0], 1);
    TS_ASSERT_EQUALS2(db->m_iVals["Iupseg"][0], 0);
    TS_ASSERT_EQUALS2(db->m_iVals["Iupseg"][1], 1);
    TS_ASSERT_EQUALS2(db->m_iVals["Iupseg"][2], 0);
    TS_ASSERT_EQUALS2(db->m_iVals["Iupseg"][3], 0);
    TS_ASSERT_EQUALS2(db->m_iVals["Iupseg"][4], 0);
    TS_ASSERT_EQUALS2(db->m_iVals["Iupseg"][5], 0);
    TS_ASSERT_EQUALS2(db->m_iVals["Iupseg"][6], 0);
  }

  // SP 2
  {
    DbMock db1, *db;
    db = static_cast<DbMock*>(Db::db());

    stressPeriod = 2;

    ExportSTR(&p, stressPeriod, nRow, nCol);
    TS_ASSERT_EQUALS2(db->m_table, "STRIupseg");
    TS_ASSERT_EQUALS2(db->m_query, "");
    TS_ASSERT(db->m_iVals.size() == 3);
    TS_ASSERT(db->m_dVals.size() == 0);
    TS_ASSERT_EQUALS2(db->m_nFields, 3);
    TS_ASSERT_EQUALS2(db->m_nRow, 7);
    TS_ASSERT_EQUALS2(db->m_iVals["Seg"][0], 1);
    TS_ASSERT_EQUALS2(db->m_iVals["Seg"][1], 2);
    TS_ASSERT_EQUALS2(db->m_iVals["Seg"][2], 3);
    TS_ASSERT_EQUALS2(db->m_iVals["Seg"][3], 4);
    TS_ASSERT_EQUALS2(db->m_iVals["Seg"][4], 5);
    TS_ASSERT_EQUALS2(db->m_iVals["Seg"][5], 6);
    TS_ASSERT_EQUALS2(db->m_iVals["Seg"][6], 7);
    TS_ASSERT_EQUALS2(db->m_iVals["SPID"][0], 2);
    TS_ASSERT_EQUALS2(db->m_iVals["Iupseg"][0], 0);
    TS_ASSERT_EQUALS2(db->m_iVals["Iupseg"][1], 1);
    TS_ASSERT_EQUALS2(db->m_iVals["Iupseg"][2], 0);
    TS_ASSERT_EQUALS2(db->m_iVals["Iupseg"][3], 0);
    TS_ASSERT_EQUALS2(db->m_iVals["Iupseg"][4], 0);
    TS_ASSERT_EQUALS2(db->m_iVals["Iupseg"][5], 0);
    TS_ASSERT_EQUALS2(db->m_iVals["Iupseg"][6], 0);
  }
}
void ExpGeoDbT::testExportSTRSP()
{
  using namespace MfData::Packages;
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());

  int str_itmp(1), str_irdflg(2), str_iptflg(3);
  MfData::MfPackage p(MfData::Packages::STRSP);

  p.SetField(STRpack::ITMP, &str_itmp);
  p.SetField(STRpack::IRDFLG, &str_irdflg);

  // All fields need to be set
  iWriteSTRtoStressPeriods(&p, 1);
  TS_ASSERT_EQUALS2(db->m_table, "");
  TS_ASSERT_EQUALS2(db->m_query, "");
  TS_ASSERT_EQUALS2(db->m_nFields, -1);
  TS_ASSERT_EQUALS2(db->m_nRow, -1);

  // Set missing
  p.SetField(STRpack::IPTFLG, &str_iptflg);
  iWriteSTRtoStressPeriods(&p, 1);
  TS_ASSERT_EQUALS2(db->m_table, "StressPeriods");
  TS_ASSERT_EQUALS2(db->m_query, "SPID = 1");
  TS_ASSERT_EQUALS2(db->m_nFields, -1);
  TS_ASSERT_EQUALS2(db->m_nRow, -1);
  TS_ASSERT(db->m_iVals.size() == 3);
}
void ExpGeoDbT::testExportHUFCbf()
{
  using namespace MfData::Packages;
  DbMock db1, *db;
  db = static_cast<DbMock*>(Db::db());

  int ihufcb(1);
  MfData::MfPackage p(MfData::Packages::HUF);

  p.SetField(HUFPack::IHUFCB, &ihufcb);

  ExportHUFCbf(&p);
  TS_ASSERT_EQUALS2(db->m_table, "CBFlags");
  TS_ASSERT_EQUALS2(db->m_query, "OID = 1");
  TS_ASSERT(db->m_iVals.size() == 1);
  TS_ASSERT_EQUALS2(db->m_nFields, -1);
}
void ExpGeoDbT::testExportHUFVars()
{
  using namespace MfData::Packages;
  MfData::MfPackage p(MfData::Packages::HUF);

  int NHUF(2), IOHUFHEADS(3), IOHUFFLOWS(7), LAYWT[4]={0,0,0,0},
      IWETIT(10), IHDWET(1);
  Real HDRY(-888), WETFCT(1.5);

  p.SetField(HUFPack::HDRY, &HDRY);
  p.SetField(HUFPack::NHUF, &NHUF);
  p.SetField(HUFPack::IOHUFHEADS, &IOHUFHEADS);
  p.SetField(HUFPack::IOHUFFLOWS, &IOHUFFLOWS);

  // All fields need to be set
  {
    DbMock db1, *db;
    db = static_cast<DbMock*>(Db::db());

    TS_ASSERT(!ExportHUFVars(&p, 4));
    TS_ASSERT_EQUALS2(db->m_table, "");
    TS_ASSERT_EQUALS2(db->m_query, "");
    TS_ASSERT_EQUALS2(db->m_nFields, -1);
    TS_ASSERT_EQUALS2(db->m_nRow, -1);
  }

  // Set missing
  p.SetField(HUFPack::LAYWT, LAYWT);
  {
    DbMock db1, *db;
    db = static_cast<DbMock*>(Db::db());

    TS_ASSERT(ExportHUFVars(&p, 4));
    TS_ASSERT_EQUALS2(db->m_table, "HUFVars");
    TS_ASSERT_EQUALS2(db->m_query, "");
    TS_ASSERT_EQUALS2(db->m_nFields, -1);
    TS_ASSERT_EQUALS2(db->m_nRow, -1);
    TS_ASSERT(db->m_iVals.size() == 3);
    if (sizeof(Real) == sizeof(float))
    {
      TS_ASSERT(db->m_fVals.size() == 1);
    }
    else
    {
      TS_ASSERT(db->m_dVals.size() == 1);
    }
  }

  // Add wetting
  LAYWT[1] = 1;
  p.SetField(HUFPack::WETFCT, &WETFCT);
  p.SetField(HUFPack::IWETIT, &IWETIT);
  p.SetField(HUFPack::IHDWET, &IHDWET);
  {
    DbMock db1, *db;
    db = static_cast<DbMock*>(Db::db());

    TS_ASSERT(ExportHUFVars(&p, 4));
    TS_ASSERT_EQUALS2(db->m_table, "HUFVars");
    TS_ASSERT_EQUALS2(db->m_query, "");
    TS_ASSERT_EQUALS2(db->m_nFields, -1);
    TS_ASSERT_EQUALS2(db->m_nRow, -1);
    TS_ASSERT(db->m_iVals.size() == 5);
    if (sizeof(Real) == sizeof(float))
    {
      TS_ASSERT(db->m_fVals.size() == 2);
    }
    else
    {
      TS_ASSERT(db->m_dVals.size() == 2);
    }
  }
}
void ExpGeoDbT::testExportHUFLayers()
{
  using namespace MfData::Packages;
  MfData::MfPackage p(MfData::Packages::HUF);

  int LTHUF[4]={1,0,1,0}, LAYWT[4]={0,0,0,0};

  p.SetField(HUFPack::LTHUF, LTHUF);

  // All fields need to be set
  {
    DbMock db1, *db;
    db = static_cast<DbMock*>(Db::db());

    TS_ASSERT(ExportHUFLayers(&p, 4));
    TS_ASSERT_EQUALS2(db->m_table, "");
    TS_ASSERT_EQUALS2(db->m_query, "");
    TS_ASSERT_EQUALS2(db->m_nFields, -1);
    TS_ASSERT_EQUALS2(db->m_nRow, -1);
    TS_ASSERT(db->m_iVals.size() == 0);
  }

  // Set missing
  p.SetField(HUFPack::LAYWT, LAYWT);
  {
    DbMock db1, *db;
    db = static_cast<DbMock*>(Db::db());

    TS_ASSERT(ExportHUFLayers(&p, 4));
    TS_ASSERT_EQUALS2(db->m_table, "HUFLayers");
    TS_ASSERT_EQUALS2(db->m_query, "");
    TS_ASSERT_EQUALS2(db->m_nFields, 3);
    TS_ASSERT_EQUALS2(db->m_nRow, 4);
    TS_ASSERT(db->m_iVals.size() == 3);
  }
}
void ExpGeoDbT::testiWriteHGUs()
{
  using namespace MfData::Packages;
  MfData::MfPackage p(MfData::Packages::HUF);

  const int NHUF(4);
  const char *HGUNAM = "HGU1      HGU2      HGU3      HGU4      ";
  Real HGUHANI[4] = {0, 1, 0, 0};
  Real HGUVANI[4] = {0, 1, 1, 0};
  int IHGUFLG[4] = {0, 0, 0, 1};
  p.SetField(HUFPack::NHUF, &NHUF);
  p.SetField(HUFPack::HGUNAM, HGUNAM);
  p.SetField(HUFPack::HGUHANI, HGUHANI);
  p.SetField(HUFPack::HGUVANI, HGUVANI);

  // All fields need to be set
  {
    DbMock db1, *db;
    db = static_cast<DbMock*>(Db::db());

    TS_ASSERT(!iWriteHGUs(&p));
    TS_ASSERT_EQUALS2(db->m_table, "");
    TS_ASSERT_EQUALS2(db->m_query, "");
    TS_ASSERT_EQUALS2(db->m_nFields, -1);
    TS_ASSERT_EQUALS2(db->m_nRow, -1);
    TS_ASSERT(db->m_iVals.size() == 0);
    TS_ASSERT(db->m_dVals.size() == 0);
    TS_ASSERT(db->m_fVals.size() == 0);
  }

  // Set missing
  p.SetField(HUFPack::IHGUFLG, IHGUFLG);
  {
    DbMock db1, *db;
    db = static_cast<DbMock*>(Db::db());

    TS_ASSERT(iWriteHGUs(&p));
    TS_ASSERT_EQUALS2(db->m_table, "HUFUnits");
    TS_ASSERT_EQUALS2(db->m_query, "HGUID = 4");
    TS_ASSERT_EQUALS2(db->m_nFields, -1);
    TS_ASSERT_EQUALS2(db->m_nRow, -1);
    TS_ASSERT(db->m_iVals.size() == 6);
    if (sizeof(Real) == sizeof(float))
    {
      TS_ASSERT(db->m_fVals.size() == 2);
    }
    else
    {
      TS_ASSERT(db->m_dVals.size() == 2);
    }
    TS_ASSERT(db->m_sVals.size() == 1);
  }
}
void ExpGeoDbT::testExportGAG()
{
  using namespace MfData::Packages;
  MfData::MfPackage p(MfData::Packages::GAGE);

  int numgage_sfr(1), numgage_lak(2);
  int IGGLST_SFR[4]={1,2,3,4}, IGGLST_LAK[8]={-1,2,3,4,-5,6,7,8};

  p.SetField(GAGpack::IGGLST, IGGLST_SFR);

  // All fields need to be set
  {
    DbMock db1, *db;
    db = static_cast<DbMock*>(Db::db());

    ExportGAG(&p);
    TS_ASSERT_EQUALS2(db->m_table, "");
    TS_ASSERT_EQUALS2(db->m_query, "");
    TS_ASSERT_EQUALS2(db->m_nFields, -1);
    TS_ASSERT_EQUALS2(db->m_nRow, -1);
    TS_ASSERT(db->m_iVals.size() == 0);
  }

  // Set missing, for streams
  p.SetField(GAGpack::NUMGAGE, &numgage_sfr);
  {
    DbMock db1, *db;
    db = static_cast<DbMock*>(Db::db());

    ExportGAG(&p);
    TS_ASSERT_EQUALS2(db->m_table, "GAGSFR");
    TS_ASSERT_EQUALS2(db->m_query, "");
    TS_ASSERT_EQUALS2(db->m_nFields, 4);
    TS_ASSERT_EQUALS2(db->m_nRow, 1);
    TS_ASSERT(db->m_iVals.size() == 4);
  }

  // For lakes
  p.SetField(GAGpack::IGGLST, IGGLST_LAK);
  p.SetField(GAGpack::NUMGAGE, &numgage_lak);
  {
    DbMock db1, *db;
    db = static_cast<DbMock*>(Db::db());

    ExportGAG(&p);
    TS_ASSERT_EQUALS2(db->m_table, "GAGLAK");
    TS_ASSERT_EQUALS2(db->m_query, "");
    TS_ASSERT_EQUALS2(db->m_nFields, 3);
    TS_ASSERT_EQUALS2(db->m_nRow, 2);
    TS_ASSERT(db->m_iVals.size() == 3);
  }
}
#endif

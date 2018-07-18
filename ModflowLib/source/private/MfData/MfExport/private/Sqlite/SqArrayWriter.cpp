//------------------------------------------------------------------------------
// FILE      SqBcList.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------

//----- Included files ---------------------------------------------------------

// 1. Precompiled header

// 2. My own header
#include <private/MfData/MfExport/private/Sqlite/SqArrayWriter.h>

// 3. Standard library headers
#include <sstream>

// 4. External library headers

// 5. Shared code headers

// 6. Non-shared code headers
#include <private/ListReader/CellIdToIJK.h>
#include <private/MfData/MfExport/private/Native/NativeExpLstPack.h>
#include <private/MfData/MfExport/private/Native/NativePackExp.h>
#include <private/MfData/MfExport/private/Sqlite/SqFile.h>
#include <private/MfData/MfGlobal.h>
#include <private/MfData/MfPackageUtil.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackFields.h>
#include <private/MfData/Packages/MfPackStrings.h>
#include <private/SQLite/CppSQLite3.h>
#include <private/MfData/MfExport/private/Sqlite/SqMfSchema.h>
#include <private/MfData/MfExport/private/Sqlite/SqExporter.h>
#include <private/MfData/MfExport/private/Sqlite/SqDisu.h>

//----- Forward declarations ---------------------------------------------------

//----- External globals -------------------------------------------------------

//----- Namespace declaration --------------------------------------------------
using namespace MfData::Export;

//----- Constants / Enumerations -----------------------------------------------

//----- Classes / Structs ------------------------------------------------------

////////////////////////////////////////////////////////////////////////////////
class SqArrayWriter::impl
{
public:
  impl(NativePackExp* a_);
  ~impl();

  void CreateTables();
  void CreateTriggers();
  void CompileStatements(CppSQLite3DB *a_db);
  void CreateSpTable();

  void AddStressPeriodData();
  int  CellId(int a_idx);
  void GetCellIds (MfData::Export::NativePackExp* a_package,
                   int a_size, int a_layer,
                   std::vector<int>& a_cellIds);
  void WriteArraySetup1(MfData::Export::NativePackExp* a_package,
                       const std::string& a_arrayName,
                       int a_size, int a_iprn,
                       Real a_mult, int a_layer,
                       CppSQLite3DB** a_db,
                       sqlite_int64& a_arrayOid,
                       std::vector<int>& cellIds);
  void WriteArraySetup2 (MfData::Export::NativePackExp* a_package,
                                       const std::string& a_arrayName,
                                        int a_size, int a_iprn,
                                        Real a_mult, int a_layer,
                                        CppSQLite3DB** a_db,
                                        sqlite_int64& a_arrayOid,
                                        std::vector<int>& a_cellIds,
                                        std::string& a_table,
                                        std::string& a_field);
  void WriteArray(MfData::Export::NativePackExp* a_package,
                const std::string& a_arrayName, int a_size,
                int a_iprn, const int* a_array, Real a_mult,
                int a_layer);
  void WriteArrayToField(MfData::Export::NativePackExp* a_package,
                const std::string& a_arrayName, int a_size,
                int a_iprn, const int* a_array, Real a_mult,
                int a_layer);
  sqlite_int64 AddToArrayInfo(std::string a_name, double a_mult,
                     int a_iprn, int a_layer,
                     CppSQLite3DB* a_db);
  void AddToIntArray(sqlite_int64 a_arrayOid, const std::vector<int>& a_cellIds,
                     const int* a_array, int a_size);
  void AddToTable(const std::vector<int>& a_cellIds,
                  const int* a_array, int a_size,
                  const std::string& a_table,
                  const std::string& a_field,
                  CppSQLite3DB* db);
  template <typename T>
  void WriteArray2T(MfData::Export::NativePackExp* a_package,
                    const std::string& a_arrayName, int a_size,
                    int a_iprn, const T* a_array, Real a_mult,
                    int a_layer);
  void WriteArray2(MfData::Export::NativePackExp* a_package,
                  const std::string& a_arrayName, int a_size,
                  int a_iprn, const float* a_array, Real a_mult,
                  int a_layer);
  void WriteArray2(MfData::Export::NativePackExp* a_package,
                  const std::string& a_arrayName, int a_size,
                  int a_iprn, const double* a_array, Real a_mult,
                  int a_layer);
  void WriteArray2(MfData::Export::NativePackExp* a_package,
                  const std::string& a_arrayName, int a_size,
                  int a_iprn, const int* a_array, Real a_mult,
                  int a_layer);
  SqExporter* MakeExporter(const std::string& a_modflowPackageName);
  SqExporter* GetExporter(const std::string& a_modflowPackageName);

  NativePackExp*        m_pack;
  CellIdToIJK           m_grid;
  CppSQLite3Statement   m_stmtInsertArrayInfo;
  CppSQLite3Statement   m_stmtInsertRealArray;
  CppSQLite3Statement   m_stmtInsertIntArray;
  std::map<std::string, SqExporter*> m_mapSqExporters;
};

static const char* SQFT = "SQLITE_FILE_TIME";
typedef std::pair<std::vector<CStr>, std::vector<CStr>> pVecCStr;
typedef std::map<CStr, pVecCStr> ColMap;

//----- Internal functions -----------------------------------------------------

//----- Class / Function definitions -------------------------------------------

////////////////////////////////////////////////////////////////////////////////
/// \class SqArrayWriter
////////////////////////////////////////////////////////////////////////////////
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
SqArrayWriter::SqArrayWriter (NativePackExp* a_) :
  m_pack(a_)
, m_p(new SqArrayWriter::impl(a_))
{
  m_p->CreateTables();
} // SqArrayWriter::SqArrayWriter
//------------------------------------------------------------------------------
/// \brief Use this constructor if you don't have the package yet.
//------------------------------------------------------------------------------
SqArrayWriter::SqArrayWriter () :
  m_pack(nullptr)
, m_p(new SqArrayWriter::impl(nullptr))
{
  //m_p->CreateTables();
} // SqArrayWriter::SqArrayWriter
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
SqArrayWriter::~SqArrayWriter ()
{
} // SqArrayWriter::~SqArrayWriter
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void SqArrayWriter::EndWriteFile ()
{
  CppSQLite3DB *f = sqLiteDbForPackage(m_pack);
  ASSERT(f);
  if (!f) return;
  CStr tStr, packName = m_pack->GetPackage()->PackageName();
  m_pack->GetGlobal()->GetStrVar(SQFT, tStr);
  std::stringstream ss;
  ss << "UPDATE " << packName << "_LASTEDIT SET LastEdit='"
     << tStr << "' WHERE OID=1;";
  f->execDML(ss.str().c_str());
} // SqArrayWriter::AddStressPeriodData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void SqArrayWriter::AddSqComment ()
{
  sqAddSqliteComment(m_pack);
} // SqArrayWriter::AddSqComment
//------------------------------------------------------------------------------
/// \brief \see SqArrayWriter::impl::WriteArraySetup.
//------------------------------------------------------------------------------
void SqArrayWriter::WriteArraySetup1 (MfData::Export::NativePackExp* a_package,
                                     const std::string& a_arrayName,
                                    int a_size, int a_iprn,
                                    Real a_mult, int a_layer,
                                    CppSQLite3DB** a_db,
                                    sqlite_int64& a_arrayOid,
                                    std::vector<int>& cellIds)
{
  m_p->WriteArraySetup1(a_package, a_arrayName, a_size, a_iprn, a_mult, a_layer,
                       a_db, a_arrayOid, cellIds);
} // SqArrayWriter::WriteArraySetup
//----- OVERLOAD ---------------------------------------------------------------
/// \see SqArrayWriter::impl::WriteArraySetup.
//----- OVERLOAD ---------------------------------------------------------------
void SqArrayWriter::WriteArraySetup2 (
                                       MfData::Export::NativePackExp* a_package,
                                       const std::string& a_arrayName,
                                        int a_size, int a_iprn,
                                        Real a_mult, int a_layer,
                                        CppSQLite3DB** a_db,
                                        sqlite_int64& a_arrayOid,
                                        std::vector<int>& a_cellIds,
                                        std::string& a_table,
                                        std::string& a_field)
{
  m_p->WriteArraySetup2(a_package, a_arrayName, a_size, a_iprn, a_mult, a_layer,
                       a_db, a_arrayOid, a_cellIds, a_table, a_field);
} // SqArrayWriter::WriteArraySetup
//------------------------------------------------------------------------------
/// \brief \see SqArrayWriter::impl::WriteArray.
//------------------------------------------------------------------------------
void SqArrayWriter::WriteArray(MfData::Export::NativePackExp* a_package,
                               const std::string& a_arrayName, int a_size,
                               int a_iprn, const int* a_array, Real a_mult,
                               int a_layer)
{
  m_p->WriteArray(a_package, a_arrayName, a_size, a_iprn, a_array, a_mult,
                  a_layer);
} // SqArrayWriter::WriteArray
//------------------------------------------------------------------------------
/// \brief \see SqArrayWriter::impl::WriteArrayToField
//------------------------------------------------------------------------------
void SqArrayWriter::WriteArrayToField(
                MfData::Export::NativePackExp* a_package,
                const std::string& a_arrayName, int a_size,
                int a_iprn, const int* a_array, Real a_mult,
                int a_layer)
{
  try {
    CppSQLite3DB* db;
    sqlite_int64 arrayOid;
    std::vector<int> cellIds;
    std::string table, field;
    WriteArraySetup2(a_package, a_arrayName, a_size, a_iprn, a_mult, a_layer,
                     &db, arrayOid, cellIds, table, field);
    AddToTable(cellIds, a_array, a_size, table, field, db);
  }
  catch (std::exception&) {
    ASSERT(false);
  }
} // SqArrayWriter::WriteArrayToField
//------------------------------------------------------------------------------
/// \brief \see SqArrayWriter::impl::AddToIntArray.
//------------------------------------------------------------------------------
void SqArrayWriter::AddToIntArray(sqlite_int64 a_arrayOid,
                                        const std::vector<int>& a_cellIds,
                                        const int* a_array, int a_size)
{
  m_p->AddToIntArray(a_arrayOid, a_cellIds, a_array, a_size);
} // SqArrayWriter::AddToIntArray
//------------------------------------------------------------------------------
/// \brief Return the compiled statement for use in the template function.
//------------------------------------------------------------------------------
CppSQLite3Statement* SqArrayWriter::GetInsertRealArrayStatement()
{
  return &m_p->m_stmtInsertRealArray;
} // SqArrayWriter::GetInsertRealArrayStatement
//------------------------------------------------------------------------------
/// \brief \see SqArrayWriter::impl::AddToTable.
//------------------------------------------------------------------------------
void SqArrayWriter::AddToTable(const std::vector<int>& a_cellIds,
                               const int* a_array, int a_size,
                               const std::string& a_table,
                               const std::string& a_field,
                               CppSQLite3DB* db)
{
  m_p->AddToTable(a_cellIds, a_array, a_size, a_table, a_field, db);
} // SqArrayWriter::AddToTable
//------------------------------------------------------------------------------
/// \brief \see SqArrayWriter::impl::WriteArray2.
//------------------------------------------------------------------------------
void SqArrayWriter::WriteArray2(MfData::Export::NativePackExp* a_package,
                const std::string& a_arrayName, int a_size,
                int a_iprn, const float* a_array, Real a_mult,
                int a_layer)
{
  m_p->WriteArray2(a_package, a_arrayName, a_size, a_iprn, a_array, a_mult,
                    a_layer);
} // SqArrayWriter::WriteArray2
//------------------------------------------------------------------------------
/// \brief \see SqArrayWriter::impl::WriteArray2.
//------------------------------------------------------------------------------
void SqArrayWriter::WriteArray2(MfData::Export::NativePackExp* a_package,
                const std::string& a_arrayName, int a_size,
                int a_iprn, const double* a_array, Real a_mult,
                int a_layer)
{
  m_p->WriteArray2(a_package, a_arrayName, a_size, a_iprn, a_array, a_mult,
                    a_layer);
} // SqArrayWriter::WriteArray2
//------------------------------------------------------------------------------
/// \brief \see SqArrayWriter::impl::WriteArray2.
//------------------------------------------------------------------------------
void SqArrayWriter::WriteArray2(MfData::Export::NativePackExp* a_package,
                const std::string& a_arrayName, int a_size,
                int a_iprn, const int* a_array, Real a_mult,
                int a_layer)
{
  m_p->WriteArray2(a_package, a_arrayName, a_size, a_iprn, a_array, a_mult,
                    a_layer);
} // SqArrayWriter::WriteArray2


////////////////////////////////////////////////////////////////////////////////
/// \class SqArrayWriter::impl
////////////////////////////////////////////////////////////////////////////////
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
SqArrayWriter::impl::impl(NativePackExp* a_)
: m_pack(a_)
, m_grid(0,0)
{
  if (m_pack) {
    m_grid = CellIdToIJK(m_pack->GetGlobal()->NumRow(),
                         m_pack->GetGlobal()->NumCol());
  }
}
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
SqArrayWriter::impl::~impl()
{
  m_stmtInsertArrayInfo.finalize();
  m_stmtInsertRealArray.finalize();
  m_stmtInsertIntArray.finalize();

  for (auto it = m_mapSqExporters.begin();
       it != m_mapSqExporters.end(); ++it) {
    delete(it->second);
  }
} // SqArrayWriter::impl::~impl
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void SqArrayWriter::impl::CreateTables ()
{
  CppSQLite3DB *f = sqLiteDbForPackage(m_pack);
  ASSERT(f);
  if (!f) return;
  CStr packName = m_pack->GetPackage()->PackageName();
  CStr tabName = packName;
  tabName += "_VARIABLES";
  if (f->tableExists(tabName)) return;

  {
    std::stringstream ss, ss1, ss2, ss3;
    ss << "CREATE TABLE " << packName
      << "_LASTEDIT (OID INTEGER PRIMARY KEY, LastEdit TEXT);";
    f->execDML(ss.str().c_str());
    ss1 << "INSERT INTO " << packName << "_LASTEDIT Values(?, datetime('now'))";
    f->execDML(ss1.str().c_str());
    // get the time that was written
    CStr sqLiteFileTime;
    if (!m_pack->GetGlobal()->GetStrVar(SQFT, sqLiteFileTime))
    {
      ss2 << "SELECT LastEdit FROM " << packName << "_LASTEDIT WHERE OID=1;";
      CppSQLite3Query q = f->execQuery(ss2.str().c_str());
      ss3 << q.fieldValue(0);
      sqLiteFileTime = ss3.str();
      m_pack->GetGlobal()->SetStrVar(SQFT, sqLiteFileTime);
    }
  }
  {
    std::stringstream ss;
    ss << "CREATE TABLE " << packName
      << "_VARIABLES (Variable TEXT, Value TEXT);";
    f->execDML(ss.str().c_str());
  }
  {
    std::stringstream ss;
    ss << "CREATE TABLE " << packName << "_ITMP (SPID INTEGER, ITMP INTEGER);";
    f->execDML(ss.str().c_str());
  }
  {
    std::stringstream ss;
    ss << "CREATE TABLE " << packName << "_CELLGRP (CELLGRP INTEGER, MAPID TEXT);";
    f->execDML(ss.str().c_str());
  }
  CreateTriggers();
  CompileStatements(f);
} // SqArrayWriter::impl::CreateTables
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void SqArrayWriter::impl::CreateTriggers ()
{
  CppSQLite3DB *f = sqLiteDbForPackage(m_pack);
  ASSERT(f);
  if (!f) return;
  CStr packName = m_pack->GetPackage()->PackageName();
  std::vector<CStr> tabNames, ops;
  tabNames.push_back(packName + "_VARIABLES");
  tabNames.push_back(packName + "_ITMP");
  tabNames.push_back(packName + "_CELLGRP");
  tabNames.push_back(packName + "_SP");
  ops.push_back("INSERT");
  ops.push_back("DELETE");
  ops.push_back("UPDATE");

  std::stringstream st;
  st << "BEGIN"
     << " UPDATE " << packName
     << "_LASTEDIT SET LastEdit = datetime('now') WHERE OID = 1;"
     << "END;";

  for (size_t i=0; i<tabNames.size(); ++i)
  {
    CStr tName = tabNames[i];
    for (size_t j=0; j<ops.size(); ++j)
    {
      tName = tabNames[i] + "_" + ops[j];
      std::stringstream ss;
      ss << "CREATE TRIGGER " << tName << " AFTER " << ops[j] << " ON "
         << tabNames[i] << " " << st.str();
      f->execDML(ss.str().c_str());
    }
  }
} // SqArrayWriter::impl::CreateTriggers
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void SqArrayWriter::impl::CompileStatements (CppSQLite3DB *a_db)
{
  m_stmtInsertArrayInfo = a_db->compileStatement(
    "INSERT INTO ArrayInfo VALUES(?, ?, ?, ?, ?)");
  m_stmtInsertRealArray = a_db->compileStatement(
    "INSERT INTO RealArray VALUES(?, ?, ?, ?);");
  m_stmtInsertIntArray = a_db->compileStatement(
    "INSERT INTO IntArray VALUES(?, ?, ?, ?);");
} // SqArrayWriter::impl::CompileStatements
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
int SqArrayWriter::impl::CellId (int /*a_idx*/)
{
  //int i(a_idx);
  //int nDataFields(*m_pack->m_nDataFields), cellIdOffset(0);
  //const Real *data(m_pack->m_data);
  //int ck, ci, cj, cellId(-1);
  //ck = static_cast<int>(data[i*(nDataFields)+0]);
  //ci = static_cast<int>(data[i*(nDataFields)+1]);
  //cj = static_cast<int>(data[i*(nDataFields)+2]);
  //if (m_pack->m_usg) cellId = (int)data[i*nDataFields+0] - cellIdOffset;
  //else cellId = m_grid.IdFromIJK(ci, cj, ck);
  //return cellId;
  return -1;
} // SqArrayWriter::impl::CellId
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
sqlite_int64 SqArrayWriter::impl::AddToArrayInfo(std::string a_name,
                                                 double a_mult, int a_iprn,
                                                 int a_layer,
                                                 CppSQLite3DB* a_db)
{
  sqlite_int64 arrayOid=0;
  try {
    m_stmtInsertArrayInfo.bind(2, a_name.c_str());
    m_stmtInsertArrayInfo.bind(3, a_mult);
    m_stmtInsertArrayInfo.bind(4, a_iprn);
    m_stmtInsertArrayInfo.bind(5, a_layer);
    m_stmtInsertArrayInfo.execDML();
    m_stmtInsertArrayInfo.reset();
    arrayOid = a_db->lastRowId();
  }
  catch (std::exception&) {
    ASSERT(false);
  }
  return arrayOid;
} // SqArrayWriter::impl::AddToArrayInfo
//------------------------------------------------------------------------------
/// \brief Get the cell IDs to include in the array table.
/// \param[in] a_package:   The package.
/// \param[in] a_size:      Number of values in the array.
/// \param[in] a_layer:     Layer.
/// \param[out] a_cellIds:  Values to write to CellId field (which for some
///                         arrays may not actually correspond to cells).
//------------------------------------------------------------------------------
void SqArrayWriter::impl::GetCellIds (MfData::Export::NativePackExp* a_package,
                                      int a_size, int a_layer,
                                      std::vector<int>& a_cellIds)
{
  if (a_layer <= 0) {
    // Set cell IDs to go from 1 to a_size
    a_cellIds.resize(a_size);
    for (int i = 0; i < a_size; ++i) { a_cellIds[i] = i+1; }
    return;
  }

  // Get nodlay, the array with the number of cells in each layer
  MfGlobal* global = a_package->GetGlobal();
  MfData::MfPackage* p = global->GetPackage(MfData::Packages::DISU);
  const int* nodlay(0);
  p->GetField("NODLAY", &nodlay);
  ASSERT(nodlay);

  if (a_size != nodlay[a_layer - 1]) {
    // a_size doesn't correspond to the num cells in the layer.
    // Set cell IDs to go from 1 to a_size
    a_cellIds.resize(a_size);
    for (int i = 0; i < a_size; ++i) { a_cellIds[i] = i+1; }
  }
  else {
    // Set cell IDs to the IDs of the layer
    int start = 1;
    for (int i = 1; i < a_layer; ++i) { start += nodlay[i-1]; }
    a_cellIds.resize(nodlay[a_layer-1]);
    for (int i = 0; i < nodlay[a_layer-1]; ++i) { a_cellIds[i] = start + i; }
  }

} // SqArrayWriter::impl::GetCellIds
//------------------------------------------------------------------------------
/// \brief Setup before writing the array data. Create DB if needed, create
///        tables, compile statements, add to the ArrayInfo table, get the
///        cell IDs. This code is common between Real/double and Int versions
///        of WriteArray.
/// \param[in] a_package:   The package.
/// \param[in] a_arrayName: Name of the array.
/// \param[in] a_size:      Number of values in the array.
/// \param[in] a_iprn:      IPRN.
/// \param[in] a_mult:      Multiplier.
/// \param[in] a_layer:     Layer.
/// \param[out] a_db:       Database pointer.
/// \param[out] a_arrayOid: OID field in ArrayInfo table for this new array.
/// \param[out] a_cellIds:  Values to write to CellId field (which for some
///                         arrays may not actually correspond to cells).
//------------------------------------------------------------------------------
void SqArrayWriter::impl::WriteArraySetup1 (
                                       MfData::Export::NativePackExp* a_package,
                                       const std::string& a_arrayName,
                                        int a_size, int a_iprn,
                                        Real a_mult, int a_layer,
                                        CppSQLite3DB** a_db,
                                        sqlite_int64& a_arrayOid,
                                        std::vector<int>& a_cellIds)
{
  // Make sure database is set up
  m_pack = a_package;
  std::string mfPackage = sqMfPackageFromArrayName(a_arrayName);
  (*a_db) = sqLiteDbForPackage(m_pack, mfPackage);
  if (!sqDbHasTables(*a_db)) {
    std::vector<std::string> sql;
    sqGetPackageTableSql(mfPackage, sql);
    sqCreatePackageTables((*a_db), mfPackage, sql);
    CompileStatements(*a_db);
  }

  a_arrayOid = AddToArrayInfo(a_arrayName, a_mult, a_iprn, a_layer, *a_db);

  GetCellIds(a_package, a_size, a_layer, a_cellIds);
} // SqArrayWriter::impl::WriteArraySetup
//----- OVERLOAD ---------------------------------------------------------------
/// \brief Setup before writing the array data. Create DB if needed, create
///        tables, compile statements, add to the ArrayInfo table, get the
///        cell IDs. This code is common between Real/double and Int versions
///        of WriteArray.
/// \param[in] a_package:   The package.
/// \param[in] a_arrayName: Name of the array.
/// \param[in] a_size:      Number of values in the array.
/// \param[in] a_iprn:      IPRN.
/// \param[in] a_mult:      Multiplier.
/// \param[in] a_layer:     Layer.
/// \param[out] a_db:       Database pointer.
/// \param[out] a_arrayOid: OID field in ArrayInfo table for this new array.
/// \param[out] a_cellIds:  Values to write to CellId field (which for some
///                         arrays may not actually correspond to cells).
//----- OVERLOAD ---------------------------------------------------------------
void SqArrayWriter::impl::WriteArraySetup2 (
                                       MfData::Export::NativePackExp* a_package,
                                       const std::string& a_arrayName,
                                        int a_size, int a_iprn,
                                        Real a_mult, int a_layer,
                                        CppSQLite3DB** a_db,
                                        sqlite_int64& a_arrayOid,
                                        std::vector<int>& a_cellIds,
                                        std::string& a_table,
                                        std::string& a_field)
{
  WriteArraySetup1(a_package, a_arrayName, a_size, a_iprn, a_mult, a_layer,
                  a_db, a_arrayOid, a_cellIds);
  sqTableAndFieldFromArray(a_arrayName, a_table, a_field);
} // SqArrayWriter::impl::WriteArraySetup
//------------------------------------------------------------------------------
/// \brief Add values to the IntArray table.
/// \param[in] a_arrayOid: OID in ArrayInfo table for ArrayInfo_OID field.
/// \param[in] a_cellIds:  Values for the CellId field.
/// \param[in] a_array:    Values for the Values field.
/// \param[in] a_size:     Number of values.
//------------------------------------------------------------------------------
void SqArrayWriter::impl::AddToIntArray(sqlite_int64 a_arrayOid,
                                        const std::vector<int>& a_cellIds,
                                        const int* a_array, int a_size)
{
  try {
    for (int i = 0; i < a_size; ++i) {
      m_stmtInsertIntArray.bind(2, static_cast<int>(a_arrayOid));
      m_stmtInsertIntArray.bind(3, a_cellIds[i]);
      m_stmtInsertIntArray.bind(4, a_array[i]);
      m_stmtInsertIntArray.execDML();
      m_stmtInsertIntArray.reset();
    }
  }
  catch (std::exception&) {
    ASSERT(false);
  }
} // SqArrayWriter::impl::AddToIntArray
//------------------------------------------------------------------------------
/// \brief Write array to SQLite. Specialization for int arrays.
/// \param[in] a_package:   The package.
/// \param[in] a_arrayName: Name of the array.
/// \param[in] a_size:      Number of values in the array.
/// \param[in] a_iprn:      IPRN.
/// \param[in] a_mult:      Multiplier.
/// \param[in] a_layer:     Layer.
//------------------------------------------------------------------------------
void SqArrayWriter::impl::WriteArray(MfData::Export::NativePackExp* a_package,
                const std::string& a_arrayName, int a_size,
                int a_iprn, const int* a_array, Real a_mult,
                int a_layer)
{
  try {
    CppSQLite3DB* db;
    sqlite_int64 arrayOid;
    std::vector<int> cellIds;
    WriteArraySetup1(a_package, a_arrayName, a_size, a_iprn, a_mult, a_layer,
                    &db, arrayOid, cellIds);
    AddToIntArray(arrayOid, cellIds, a_array, a_size);
  }
  catch (std::exception&) {
    ASSERT(false);
  }
} // SqArrayWriter::impl::WriteArray
//------------------------------------------------------------------------------
/// \brief Write array to a SQLite table and field. Specialization for int arrays.
/// \param[in] a_package:   The package.
/// \param[in] a_arrayName: Name of the array.
/// \param[in] a_size:      Number of values in the array.
/// \param[in] a_iprn:      IPRN.
/// \param[in] a_mult:      Multiplier.
/// \param[in] a_layer:     Layer.
//------------------------------------------------------------------------------
void SqArrayWriter::impl::WriteArrayToField(
                MfData::Export::NativePackExp* a_package,
                const std::string& a_arrayName, int a_size,
                int a_iprn, const int* a_array, Real a_mult,
                int a_layer)
{
  try {
    CppSQLite3DB* db;
    sqlite_int64 arrayOid;
    std::vector<int> cellIds;
    std::string table, field;
    WriteArraySetup2(a_package, a_arrayName, a_size, a_iprn, a_mult, a_layer,
                    &db, arrayOid, cellIds, table, field);
    AddToTable(cellIds, a_array, a_size, table, field, db);
  }
  catch (std::exception&) {
    ASSERT(false);
  }
} // SqArrayWriter::impl::WriteArrayToField
//------------------------------------------------------------------------------
/// \brief Add values in the table.
/// \param[in] a_arrayOid: OID in ArrayInfo table for ArrayInfo_OID field.
/// \param[in] a_cellIds:  Values for the CellId field.
/// \param[in] a_array:    Values for the Values field.
/// \param[in] a_size:     Number of values.
//------------------------------------------------------------------------------
void SqArrayWriter::impl::AddToTable(const std::vector<int>& a_cellIds,
                                      const int* a_array, int a_size,
                                      const std::string& a_table,
                                      const std::string& a_field,
                                      CppSQLite3DB* db)
{
  try {
    for (int i = 0; i < a_size; ++i) {
      std::stringstream ss;
      ss << "UPDATE " << a_table << " SET " << a_field << " = " << a_array[i]
         << "WHERE CellId = " << a_cellIds[i];
      db->execQuery(ss.str().c_str());
    }
  }
  catch (std::exception&) {
    ASSERT(false);
  }
} // SqArrayWriter::impl::AddToTable
//------------------------------------------------------------------------------
/// \brief Given a modflow package name, construct the appropriate SqExporter.
/// \param[in] a_modflowPackageName: Modflow package name.
/// \return SqExporter derived class.
//------------------------------------------------------------------------------
SqExporter* SqArrayWriter::impl::MakeExporter(
  const std::string& a_modflowPackageName)
{
  // I tried using a map but couldn't get it to work in VS 2010
  // http://stackoverflow.com/questions/582331
  //template<typename T> Base * createInstance() { return new T; }

  SqExporter* sqExporter = nullptr;
  if (a_modflowPackageName == MfData::Packages::DISU)
    sqExporter = new SqDisu();
  else
    ASSERT(false);

  return sqExporter;
} // SqArrayWriter::impl::MakeExporter
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
SqExporter* SqArrayWriter::impl::GetExporter(const std::string& a_modflowPackageName)
{
  // See if we already have one to return
  auto it = m_mapSqExporters.find(a_modflowPackageName);
  if (it != m_mapSqExporters.end()) {
    return it->second;
  }
  else {
    // Create one and save it for later
    SqExporter* exporter = MakeExporter(a_modflowPackageName);
    m_mapSqExporters.insert(std::make_pair(a_modflowPackageName,
      exporter));
    return exporter;
  }
} // SqArrayWriter::impl::GetExporter
//------------------------------------------------------------------------------
/// \brief To avoid duplicate code.
//------------------------------------------------------------------------------
template <typename T>
void SqArrayWriter::impl::WriteArray2T(MfData::Export::NativePackExp* a_package,
                                  const std::string& a_arrayName, int a_size,
                                  int a_iprn, const T* a_array, Real a_mult,
                                  int a_layer)
{
  std::string modflowPackageName = sqMfPackageFromArrayName(a_arrayName);
  SqExporter* exporter = GetExporter(modflowPackageName);
  ASSERT(exporter);
  exporter->ExportArray(a_package, a_arrayName, a_size, a_iprn, a_array,
                        a_mult, a_layer);
} // SqArrayWriter::impl::WriteArray2T
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void SqArrayWriter::impl::WriteArray2(MfData::Export::NativePackExp* a_package,
                                  const std::string& a_arrayName, int a_size,
                                  int a_iprn, const float* a_array, Real a_mult,
                                  int a_layer)
{
  WriteArray2T(a_package, a_arrayName, a_size, a_iprn, a_array,
                        a_mult, a_layer);
} // SqArrayWriter::impl::WriteArray2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void SqArrayWriter::impl::WriteArray2(MfData::Export::NativePackExp* a_package,
                const std::string& a_arrayName, int a_size,
                int a_iprn, const double* a_array, Real a_mult,
                int a_layer)
{
  WriteArray2T(a_package, a_arrayName, a_size, a_iprn, a_array,
                        a_mult, a_layer);
} // SqArrayWriter::impl::WriteArray2
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void SqArrayWriter::impl::WriteArray2(MfData::Export::NativePackExp* a_package,
                const std::string& a_arrayName, int a_size,
                int a_iprn, const int* a_array, Real a_mult,
                int a_layer)
{
  WriteArray2T(a_package, a_arrayName, a_size, a_iprn, a_array,
                        a_mult, a_layer);
} // SqArrayWriter::impl::WriteArray2

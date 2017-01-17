//------------------------------------------------------------------------------
// FILE      SqDisu.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------

//----- Included files ---------------------------------------------------------

// 1. Precompiled header

// 2. My own header
#include <private\MfData\MfExport\private\Sqlite\SqDisu.h>

// 3. Standard library headers
#include <sstream>

// 4. External library headers

// 5. Shared code headers

// 6. Non-shared code headers
#include <private/MfData/MfExport/private/Native/NativeExpDisu.h>
#include <private/MfData/MfExport/private/Sqlite/SqFile.h>
#include <private/MfData/MfExport/private/Sqlite/SqMfSchema.h>
#include <private\MfData\MfExport\private\Sqlite\SqArrayWriter.h>
#include <private/MfData/MfGlobal.h>
#include <private/MfData/MfPackageUtil.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/MfData/Packages/MfPackFields.h>
#include <private/SQLite/CppSQLite3.h>

//----- Forward declarations ---------------------------------------------------

//----- External globals -------------------------------------------------------

//----- Namespace declaration --------------------------------------------------
using namespace MfData::Export;

//----- Constants / Enumerations -----------------------------------------------

//----- Classes / Structs ------------------------------------------------------

//----- Internal functions -----------------------------------------------------

//----- Class / Function definitions -------------------------------------------

////////////////////////////////////////////////////////////////////////////////
class SqDisu::impl
{
public:
  impl(NativeExpDisu* a_);
  ~impl();

  void Export ();

private:
  // Variables
  template <typename T>
  void UpdateVariable (const std::string& a_name, T a_value);

  void ExportLaycbd();
  void ExportVariables();

  void TableCreationSql(std::vector<std::string>& a_sql);
  void CreateTables();

  NativeExpDisu*      m_nativeExporter;
  CppSQLite3DB*       m_db;
}; // class SqDisu::impl

////////////////////////////////////////////////////////////////////////////////
struct ArrayVariableNames {
  std::string m_array;
  std::string m_mult;
  std::string m_iprn;
};


////////////////////////////////////////////////////////////////////////////////
/// \class SqDisu
/// \brief Exports the DISU data to sqlite. \see SqDisu::impl
////////////////////////////////////////////////////////////////////////////////
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
SqDisu::SqDisu (NativeExpDisu* a_)
: m_p(new SqDisu::impl(a_))
{
} // SqDisu::SqDisu
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
SqDisu::~SqDisu ()
{
  if (m_p) delete(m_p);
  m_p = nullptr;
} // SqDisu::~SqDisu
//------------------------------------------------------------------------------
/// \brief Export the package to sqlite.
//------------------------------------------------------------------------------
void SqDisu::Export ()
{
  m_p->Export();
} // SqDisu::Export


////////////////////////////////////////////////////////////////////////////////
/// \class SqDisu::impl
/// \brief Exports the DISU data to sqlite.
////////////////////////////////////////////////////////////////////////////////
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
SqDisu::impl::impl (NativeExpDisu* a_)
: m_nativeExporter(a_)
, m_db(nullptr)
{
} // SqDisu::impl::impl
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
SqDisu::impl::~impl ()
{
} // SqDisu::impl::~impl
//------------------------------------------------------------------------------
/// \brief Get the sql to create the necessary tables.
/// \param[in,out] a_sql: Sql table creation statements.
//------------------------------------------------------------------------------
void SqDisu::impl::TableCreationSql (std::vector<std::string>& a_sql)
{
  a_sql.push_back("CREATE TABLE Text (Text TEXT NOT NULL)");
  a_sql.push_back("CREATE TABLE Variable (NODES INTEGER, NLAY INTEGER, NJAG INTEGER, IVSD INTEGER, NPER INTEGER, ITMUNI INTEGER, LENUNI INTEGER, IDSYMRD INTEGER)");
  a_sql.push_back("INSERT INTO Variable DEFAULT VALUES");
  a_sql.push_back("CREATE TABLE ArrayInfo (OID INTEGER PRIMARY KEY NOT NULL, Name TEXT, CNSTNT REAL, Type INTEGER, Layer INTEGER)");
  a_sql.push_back("CREATE TABLE IntArray (OID INTEGER PRIMARY KEY NOT NULL, ArrayInfo_OID INTEGER, CellId INTEGER, Value INTEGER)");
  a_sql.push_back("CREATE TABLE RealArray (OID INTEGER PRIMARY KEY NOT NULL, ArrayInfo_OID INTEGER, CellId INTEGER, Value Real)");
} // SqDisu::impl::TableCreationSql
//------------------------------------------------------------------------------
/// \brief Create the tables for the DISU package.
//------------------------------------------------------------------------------
void SqDisu::impl::CreateTables ()
{
  ASSERT(m_db);
  if (!m_db) return;
  if (sqDbHasTables(m_db)) return;

  // see if the database has already been created
  CStr packName = m_nativeExporter->GetPackage()->PackageName();

  std::vector<std::string> sql;
  TableCreationSql(sql);
  sqCreatePackageTables(m_db, packName, sql);
} // SqDisu::impl::CreateTables
//------------------------------------------------------------------------------
/// \brief Updates a value in the Variable table.
/// \param[in] a_name: Name of the variable.
/// \param[in] a_value: Value of the variable.
//------------------------------------------------------------------------------
template <typename T>
void SqDisu::impl::UpdateVariable (const std::string& a_name, T a_value)
{
  try {
    std::stringstream ss;
    // We don't use a precompiled statement here because those don't work with
    // the variable as a parameter. Only the value could be a parameter.
    ss << "UPDATE Variable SET " << a_name << " = " << a_value << ";";
    m_db->execQuery(ss.str().c_str());
  }
  catch (std::exception&) {
    ASSERT(false);
  }
} // SqDisu::impl::UpdateVariable
//------------------------------------------------------------------------------
/// \brief Export variables to the variables table.
//------------------------------------------------------------------------------
void SqDisu::impl::ExportVariables ()
{
  MfData::MfPackage* package = m_nativeExporter->GetPackage();
  MfData::MfGlobal*  global = m_nativeExporter->GetGlobal();
  ASSERT(package && global);
  if (!package || !global)
    return;

  const int *nodes(0), *njag(0), *ivsd(0), *idsymrd(0);
  using namespace MfData::Packages;
  if (package->GetField(Disu::NODES, &nodes) && nodes &&
      package->GetField(Disu::NJAG, &njag) && njag &&
      package->GetField(Disu::IVSD, &ivsd) && ivsd &&
      package->GetField(Disu::IDSYMRD, &idsymrd) && idsymrd) {

    // Export the variables
    UpdateVariable(Disu::NODES, *nodes);
    UpdateVariable(Disu::NLAY, global->NumLay());
    UpdateVariable(Disu::NJAG, *njag);
    UpdateVariable(Disu::IVSD, *ivsd);
    UpdateVariable(Disu::NPER, global->NumPeriods());
    UpdateVariable(Disu::ITMUNI, global->TimeUnit());
    UpdateVariable(Disu::LENUNI, global->LengthUnit());
    UpdateVariable(Disu::IDSYMRD, *idsymrd);
  }
  else {
    ASSERT(false);
  }
} // SqDisu::impl::ExportVariables
//------------------------------------------------------------------------------
/// \brief Export LAYCBD
//------------------------------------------------------------------------------
void SqDisu::impl::ExportLaycbd ()
{
  using namespace MfData::Packages;

  // LAYCBD for some stupid reason isn't read with U1DINT so the data isn't
  // stored with the other array data. We saved it with the package.
  const int* laycbd(0);
  if (m_nativeExporter->GetPackage()->GetField(Disu::LAYCBD, &laycbd) &&
      laycbd) {
    int nlay = m_nativeExporter->GetGlobal()->NumLay();
    sqWriteArray(m_nativeExporter, Disu::LAYCBD, nlay, 1, laycbd, 1.0, 1);
  }
} // SqDisu::impl::ExportLaycbd
//------------------------------------------------------------------------------
/// \brief Export the package to sqlite.
/// \param
//------------------------------------------------------------------------------
void SqDisu::impl::Export ()
{
  try {
    // Assign the member variables
    m_db = sqLiteDbForPackage(m_nativeExporter);
    ASSERT(m_db);
    if (!m_db) return;

    ExportVariables();
    ExportLaycbd();

    // All other arrays were already written by SqArrayWriter

    // Finish up
    sqStoreLastEditTime(m_db, m_nativeExporter);
    sqAddSqliteComment(m_nativeExporter);
  }
  catch (std::exception&) {
    ASSERT(false);
  }
} // SqDisu::impl::Export

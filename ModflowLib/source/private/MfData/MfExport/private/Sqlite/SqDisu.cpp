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
  void AddVariable(const std::string& a_name, int a_value);
  void AddVariable(const std::string& a_name, Real a_value);
  template <typename T>
  void UpdateVariable (const std::string& a_name, T a_value);

  // Arrays
  bool ArrayExists (const std::string& a_name) const;
  template <typename T>
  bool GetArrayData(const std::string& a_name, int a_layer,
                    std::vector<T>& a_vec,
                    Real& a_mult, int& a_iprn) const;
  __int64 AddToArrayInfoTable (const std::string& a_name,
                            Real a_mult, int a_iprn, int a_layer);
  void AddArrayToIntArrayTable (__int64 a_arrayOid,
                                const std::vector<int>& a_cellId,
                                const std::vector<int>& a_values);
  void AddArrayToRealArrayTable (__int64 a_arrayOid,
                                 const std::vector<int>& a_cellId,
                                 const std::vector<Real>& a_values);
  void AddIntArray (const std::string& a_name,
                    const std::vector<int>& a_cellIds,
                    const std::vector<int>& a_values,
                    Real a_mult, int a_iprn, int a_layer);
  void AddRealArray (const std::string& a_name,
                     const std::vector<int>& a_cellIds,
                     const std::vector<Real>& a_values,
                     Real a_mult, int a_iprn, int a_layer);

  void ExportIntArrayLayer (const std::string& a_name, int a_layer,
                       const std::vector<int>& a_cellIds);
  void ExportIntArraysSizeNumLay();
  void ExportIntArraysSizeNumCells();
  void ExportIntArraysSizeNjag();
  void ExportIntArrays();

  void ExportRealArrayLayer (const std::string& a_name, int a_layer,
                       const std::vector<int>& a_cellIds);
  void ExportRealArray (const std::string& a_name,
                        const std::vector<std::vector<int>>& a_cellIdsPerLayer);
  void ExportRealArraysSizeNumCells ();
  void ExportRealArraysSizeNjag();
  void ExportRealArrays();

  // Other
  void ExportVariables();

  void TableCreationSql(std::vector<std::string>& a_sql);
  void CreateTables();
  void CompileStatements();
  int GetIntField (std::string a_name);
  std::vector<int> GetCellIds (int a_size) const;
  std::vector<std::vector<int>> GetCellIdsPerLayer() const;

  NativeExpDisu*      m_nativeExporter;
  CppSQLite3DB*       m_db;
  CppSQLite3Statement m_stmtInsertArrayInfo;
  CppSQLite3Statement m_stmtInsertIntArray;
  CppSQLite3Statement m_stmtInsertRealArray;
}; // class SqDisu::impl

////////////////////////////////////////////////////////////////////////////////
struct ArrayVariableNames {
  std::string m_array;
  std::string m_mult;
  std::string m_iprn;
};

//------------------------------------------------------------------------------
/// \brief Get the strings for the array, multiplier, iprn and layer used to
///        store the array values.
/// \param[in] a_name: Array name.
/// \param[in] a_layer: Layer.
/// \return ArrayVariableNames struct.
//------------------------------------------------------------------------------
static ArrayVariableNames iGetArrayNames(const std::string& a_name,
                                         int a_layer)
{
  std::stringstream ss;
  ss << "_Layer_" << a_layer;
  std::string layerString = ss.str();

  ArrayVariableNames avn;
  avn.m_array = a_name + "_ARR" + layerString;
  avn.m_mult = a_name + "_MULT" + layerString;
  avn.m_iprn = a_name + "_IPRN" + layerString;
  return avn;
} // iGetArrayNames

//------------------------------------------------------------------------------
/// \brief Convert an integer into a string. Cause we don't have C++11.
//------------------------------------------------------------------------------
//static std::string itos(int a)
//{
//  std::stringstream ss;
//  ss << a;
//  return ss.str();
//}


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
  try {
    //m_stmtAddIntVariable.finalize();
    //m_stmtAddRealVariable.finalize();
    m_stmtInsertArrayInfo.finalize();
    m_stmtInsertIntArray.finalize();
    m_stmtInsertRealArray.finalize();
  }
  catch (std::exception&) {
    ASSERT(false);
  }
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
/// \brief Compile the sql pre-compiled statements.
//------------------------------------------------------------------------------
void SqDisu::impl::CompileStatements ()
{
  try {
    //m_stmtAddIntVariable = m_db->compileStatement(
    //  "ALTER TABLE VARIABLE ADD COLUMN ? INTEGER; UPDATE VARIABLE SET ?=?;");
    //m_stmtAddRealVariable = m_db->compileStatement(
    //  "ALTER TABLE VARIABLE ADD COLUMN ? REAL; UPDATE VARIABLE SET ?=?;");
    m_stmtInsertArrayInfo = m_db->compileStatement(
      "INSERT INTO ArrayInfo VALUES(?, ?, ?, ?, ?)");
    m_stmtInsertIntArray = m_db->compileStatement(
      "INSERT INTO IntArray VALUES(?, ?, ?, ?);");
    m_stmtInsertRealArray = m_db->compileStatement(
      "INSERT INTO RealArray VALUES(?, ?, ?, ?);");
  }
  catch (std::exception& e)
  {
    ASSERT(false);
    (void)e;
  }
} // SqDisu::impl::CompileStatements
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
////------------------------------------------------------------------------------
///// \brief Adds a variable as a new column in the Variable table.
///// \param[in] a_name: Name of the variable.
///// \param[in] a_value: Value of the variable.
////------------------------------------------------------------------------------
//void SqDisu::impl::AddVariable (const std::string& a_name, int a_value)
//{
//  m_stmtAddIntVariable.bind(1, a_name.c_str());
//  m_stmtAddIntVariable.bind(2, a_name.c_str());
//  m_stmtAddIntVariable.bind(3, a_value);
//  m_stmtAddIntVariable.execDML();
//  m_stmtAddIntVariable.reset();
//} // SqDisu::impl::AddVariable
////------------------------------------------------------------------------------
///// \brief Adds a variable as a new column in the Variable table.
///// \param[in] a_name: Name of the variable.
///// \param[in] a_value: Value of the variable.
////------------------------------------------------------------------------------
//void SqDisu::impl::AddVariable (const std::string& a_name, Real a_value)
//{
//  m_stmtAddRealVariable.bind(1, a_name.c_str());
//  m_stmtAddRealVariable.bind(2, a_name.c_str());
//  m_stmtAddRealVariable.bind(3, a_value);
//  m_stmtAddRealVariable.execDML();
//  m_stmtAddRealVariable.reset();
//} // SqDisu::impl::AddVariable
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
/// \brief Get a vector and a multiplier stored in MfGlobal.
///
/// They were stored there in NativeExpArr1d::ExportT or NativeExpArr2d::?
/// \param[in] a_name: Name of the variable, minus "_ARR" or "_MULT".
/// \param[in] a_layer: The layer.
/// \param[out] a_vec: The vector that gets set.
/// \param[out] a_mult: The multiplier that gets set.
/// \param[out] a_iprn: The IPRN flag.
//------------------------------------------------------------------------------
template <typename T>
bool SqDisu::impl::GetArrayData (const std::string& a_name,
                                 int a_layer,
                                 std::vector<T>& a_vec,
                                 Real& a_mult,
                                 int& a_iprn) const
{
  const MfData::MfGlobal* global = m_nativeExporter->GetGlobal();
  ArrayVariableNames avn = iGetArrayNames(a_name, a_layer);
  bool rv1 = global->GetVector(avn.m_array.c_str(), a_vec);
  bool rv2 = global->GetRealVar(avn.m_mult.c_str(), a_mult);
  bool rv3 = global->GetIntVar(avn.m_iprn.c_str(), a_iprn);
  return rv1 && rv2 && rv3;
} // SqDisu::impl::GetArrayData
//------------------------------------------------------------------------------
/// \brief Return the 
/// \param[in] a_name: Field name.
/// \return int value.
//------------------------------------------------------------------------------
int SqDisu::impl::GetIntField (std::string a_name)
{
  const int *ival(0);
  if (m_nativeExporter->GetPackage()->GetField(a_name.c_str(), &ival) && ival) {
    return *ival;
  }
  else {
    ASSERT(false);
    return -9999999;
  }
} // SqDisu::impl::GetIntField
//------------------------------------------------------------------------------
/// \brief Sets up an integer vector with increasing numbers (1,2,3...)
/// \param[in] a_size: Vector size.
/// \return vector.
//------------------------------------------------------------------------------
std::vector<int> SqDisu::impl::GetCellIds (int a_size) const
{
  std::vector<int> cellIds(a_size);
  for (int i = 0; i < a_size; ++i) { cellIds[i] = i+1; } // 1,2,3...
    // Could be replaced by std::iota in C++11
  return cellIds;
} // SqDisu::impl::GetCellIds
//------------------------------------------------------------------------------
/// \brief Sets up a 2D int vector sizeof num layers with cellIds per layer.
/// \return 2D int vector.
//------------------------------------------------------------------------------
std::vector<std::vector<int>> SqDisu::impl::GetCellIdsPerLayer () const
{
  using namespace MfData::Packages;

  // Get the nodes per layer data
  std::vector<int> nodlay;
  Real mult;
  int iprn;
  GetArrayData(Disu::NODLAY, 0, nodlay, mult, iprn); // Mf uses K=0 for NODLAY

  std::vector<std::vector<int>> cellIds(nodlay.size(), std::vector<int>());
  int cellId = 1;
  for (size_t i = 0; i < nodlay.size(); ++i) {
    cellIds[i].resize(nodlay[i]);
    for (int j = 0; j < nodlay[i]; ++j) {
      cellIds[i][j] = cellId;
      ++cellId;
    }
    // Could be replaced by std::iota in C++11
  }
  return cellIds;
} // SqDisu::impl::GetCellIdsPerLayer
//------------------------------------------------------------------------------
/// \brief Returns true if the array exists.
/// \param a_name: Name of the array.
/// \return true or false.
//------------------------------------------------------------------------------
bool SqDisu::impl::ArrayExists (const std::string& a_name) const
{
  // Check for existence of the multiplier
  std::string name = a_name + "_MULT";
  Real mult;
  return m_nativeExporter->GetGlobal()->GetRealVar(name.c_str(), mult);
} // SqDisu::impl::ArrayExists
//------------------------------------------------------------------------------
/// \brief Add an entry to the ArrayInfo table.
/// \param[in] a_name:  The array name.
/// \param[in] a_mult:  The multiplier.
/// \param[in] a_iprn:  The IPRN flag.
/// \param[in] a_layer: The layer.
/// \return The OID in the ArrayInfo table.
//------------------------------------------------------------------------------
__int64 SqDisu::impl::AddToArrayInfoTable (const std::string& a_name,
                                        Real a_mult, int a_iprn, int a_layer)
{
  try {
    m_stmtInsertArrayInfo.bind(2, a_name.c_str());
    m_stmtInsertArrayInfo.bind(3, a_mult);
    m_stmtInsertArrayInfo.bind(4, a_iprn);
    m_stmtInsertArrayInfo.bind(5, a_layer);
    m_stmtInsertArrayInfo.execDML();
    m_stmtInsertArrayInfo.reset();
  }
  catch (std::exception&) {
    ASSERT(false);
  }
  return m_db->lastRowId();
} // SqDisu::impl::AddToArrayInfoTable
//------------------------------------------------------------------------------
/// \brief Add values to the IntArray table.
/// \param[in] a_arrayOid: The OID in the ArrayInfo table.
/// \param[in] a_cellId:   The cellIDs.
/// \param[in] a_values:   The array values.
//------------------------------------------------------------------------------
void SqDisu::impl::AddArrayToIntArrayTable (__int64 a_arrayOid,
                                        const std::vector<int>& a_cellId,
                                        const std::vector<int>& a_values)
{
  try {
    ASSERT(a_cellId.size() == a_values.size());

    for (size_t i = 0; i < a_values.size(); ++i) {
      m_stmtInsertIntArray.bind(2, static_cast<int>(a_arrayOid));
      m_stmtInsertIntArray.bind(3, a_cellId[i]);
      m_stmtInsertIntArray.bind(4, a_values[i]);
      m_stmtInsertIntArray.execDML();
      m_stmtInsertIntArray.reset();
    }
  }
  catch (std::exception&) {
    ASSERT(false);
  }
} // SqDisu::impl::AddArrayToIntArrayTable
//------------------------------------------------------------------------------
/// \brief Add values to the RealArray table.
/// \param[in] a_name:   The array name.
/// \param[in] a_values: The array values.
//------------------------------------------------------------------------------
void SqDisu::impl::AddArrayToRealArrayTable (__int64 a_arrayOid,
                                        const std::vector<int>& a_cellId,
                                        const std::vector<Real>& a_values)
{
  try {
    ASSERT(a_cellId.size() == a_values.size());

    for (size_t i = 0; i < a_values.size(); ++i) {
      m_stmtInsertRealArray.bind(2, static_cast<int>(a_arrayOid));
      m_stmtInsertRealArray.bind(3, a_cellId[i]);
      m_stmtInsertRealArray.bind(4, a_values[i]);
      m_stmtInsertRealArray.execDML();
      m_stmtInsertRealArray.reset();
    }
  }
  catch (std::exception&) {
    ASSERT(false);
  }
} // SqDisu::impl::AddArrayToRealArrayTable
//------------------------------------------------------------------------------
/// \brief Adds to ArrayInfo table and IntArray table.
/// \param[in] a_name:   The array name.
/// \param[in] a_cellId: The cellIDs.
/// \param[in] a_values: The array values.
/// \param[in] a_mult:   The multiplier.
/// \param[in] a_iprn:   The IPRN flag.
/// \param[in] a_layer:  The layer.
//------------------------------------------------------------------------------
void SqDisu::impl::AddIntArray (const std::string& a_name,
                                const std::vector<int>& a_cellIds,
                                const std::vector<int>& a_values,
                                Real a_mult, int a_iprn,
                                int a_layer)
{
  __int64 arrayOid = AddToArrayInfoTable(a_name, a_mult, a_iprn, a_layer);
  AddArrayToIntArrayTable(arrayOid, a_cellIds, a_values);
} // SqDisu::impl::AddIntArray
//------------------------------------------------------------------------------
/// \brief Adds to ArrayInfo table and RealArray table.
/// \param[in] a_name:   The array name.
/// \param[in] a_cellId: The cellIDs.
/// \param[in] a_values: The array values.
/// \param[in] a_mult:   The multiplier.
/// \param[in] a_iprn:   The IPRN flag.
/// \param[in] a_layer:  The layer.
//------------------------------------------------------------------------------
void SqDisu::impl::AddRealArray (const std::string& a_name,
                                 const std::vector<int>& a_cellIds,
                                 const std::vector<Real>& a_values,
                                 Real a_mult, int a_iprn,
                                 int a_layer)
{
  __int64 arrayOid = AddToArrayInfoTable(a_name, a_mult, a_iprn, a_layer);
  AddArrayToRealArrayTable(arrayOid, a_cellIds, a_values);
} // SqDisu::impl::AddRealArray
//------------------------------------------------------------------------------
/// \brief Export an integer array.
/// \param[in] a_name:    The array name.
/// \param[in] a_layer:   The layer.
/// \param[in] a_cellIds: The cell Ids.
//------------------------------------------------------------------------------
void SqDisu::impl::ExportIntArrayLayer (const std::string& a_name, int a_layer,
                                   const std::vector<int>& a_cellIds)
{
  std::vector<int> values;
  Real mult;
  int iprn;
  if (!GetArrayData(a_name, a_layer, values, mult, iprn)) {
    ASSERT(false);
    return;
  }
  AddIntArray(a_name, a_cellIds, values, mult, iprn, a_layer);
} // SqDisu::impl::ExportIntArrayLayer
//------------------------------------------------------------------------------
/// \brief Exports a layer of the array.
/// \param[in] a_name:    The array name.
/// \param[in] a_layer:   The layer.
/// \param[in] a_cellIds: The cell Ids.
//------------------------------------------------------------------------------
void SqDisu::impl::ExportRealArrayLayer (const std::string& a_name, int a_layer,
                                    const std::vector<int>& a_cellIds)
{
  std::vector<Real> values;
  Real mult;
  int iprn;
  if (!GetArrayData(a_name, a_layer, values, mult, iprn)) {
    ASSERT(false);
    return;
  }
  AddRealArray(a_name, a_cellIds, values, mult, iprn, a_layer);
} // SqDisu::impl::ExportRealArrayLayer
//------------------------------------------------------------------------------
/// \brief Export the int arrays that are num lay big: LAYCBD, NODLAY.
//------------------------------------------------------------------------------
void SqDisu::impl::ExportIntArraysSizeNumLay ()
{
  using namespace MfData::Packages;

  // Set up cellIds vector
  int nlay = m_nativeExporter->GetGlobal()->NumLay();
  std::vector<int> cellIds = GetCellIds(nlay);

  // LAYCBD for some stupid reason isn't read with U1DINT so the data isn't
  // stored with the other array data. We saved it with the package.
  {
    const int* laycbd(0);
    if (m_nativeExporter->GetPackage()->GetField(Disu::LAYCBD, &laycbd) &&
        laycbd) {
      std::vector<int> v(&laycbd[0], &laycbd[nlay]); // Copy into a vector
      AddIntArray(Disu::LAYCBD, cellIds, v, 1.0, 1, 1);
    }
  }

  // NODLAY is handled by U1DINT
  ExportIntArrayLayer(Disu::NODLAY, 0, cellIds); // Mf uses K=0 for NODLAY
} // SqDisu::impl::ExportIntArraysSizeNumLay
//------------------------------------------------------------------------------
/// \brief Export the int arrays that are num cells big: IAC.
//------------------------------------------------------------------------------
void SqDisu::impl::ExportIntArraysSizeNumCells ()
{
  using namespace MfData::Packages;

  // Set up cellIds vector
  std::vector<int> cellIds = GetCellIds(GetIntField(Disu::NODES));
  
  ExportIntArrayLayer(Disu::IA, 0, cellIds); // Mf uses K=0 for IA
} // SqDisu::impl::ExportIntArraysSizeNumCells
//------------------------------------------------------------------------------
/// \brief Export the int arrays that are NJAG big: JA, IVC.
//------------------------------------------------------------------------------
void SqDisu::impl::ExportIntArraysSizeNjag ()
{
  using namespace MfData::Packages;

  // Set up cellIds vector
  std::vector<int> cellIds = GetCellIds(GetIntField(Disu::NJAG));
  
  ExportIntArrayLayer(Disu::JA, 0, cellIds); // Mf uses K=0 for JA

  if (ArrayExists(Disu::IVC)) {
    ExportIntArrayLayer(Disu::IVC, 1, cellIds);
  }
} // SqDisu::impl::ExportIntArraysSizeNjag
//------------------------------------------------------------------------------
/// \brief Export the data to the IntArray table.
//------------------------------------------------------------------------------
void SqDisu::impl::ExportIntArrays ()
{
  ExportIntArraysSizeNumLay();
  ExportIntArraysSizeNumCells();
  ExportIntArraysSizeNjag();
} // SqDisu::impl::ExportIntArrays
//------------------------------------------------------------------------------
/// \brief Exports the layers of the array.
//------------------------------------------------------------------------------
void SqDisu::impl::ExportRealArray (const std::string& a_name,
                         const std::vector<std::vector<int>>& a_cellIdsPerLayer)
{
  for (int k = 0; k < a_cellIdsPerLayer.size(); ++k) {
    ExportRealArrayLayer(a_name, k+1, a_cellIdsPerLayer[k]);
  }
} // SqDisu::impl::ExportRealArray
//------------------------------------------------------------------------------
/// \brief Export the Real arrays that are num cells big: Top, Bot, Area.
//------------------------------------------------------------------------------
void SqDisu::impl::ExportRealArraysSizeNumCells ()
{
  using namespace MfData::Packages;
  std::vector<std::vector<int>> cellIdsPerLayer = GetCellIdsPerLayer();
  ExportRealArray(Disu::TOP, cellIdsPerLayer);
  ExportRealArray(Disu::BOT, cellIdsPerLayer);
  ExportRealArray(Disu::AREA, cellIdsPerLayer);
} // SqDisu::impl::ExportRealArraysSizeNumCells
//------------------------------------------------------------------------------
/// \brief Export the Real arrays that are NJAG big: CL12, FAHL.
//------------------------------------------------------------------------------
void SqDisu::impl::ExportRealArraysSizeNjag ()
{
  using namespace MfData::Packages;

  // Set up cellIds vector
  std::vector<int> cellIds = GetCellIds(GetIntField(Disu::NJAG));
  
  if (ArrayExists(Disu::CL12)) {
    ExportRealArrayLayer(Disu::CL12, 1, cellIds);
  }

  if (ArrayExists(Disu::FAHL)) {
    ExportRealArrayLayer(Disu::FAHL, 0, cellIds); // Mf uses K=0 for FAHL
  }
} // SqDisu::impl::ExportRealArraysSizeNjag
//------------------------------------------------------------------------------
/// \brief Export the data to the RealArray table.
//------------------------------------------------------------------------------
void SqDisu::impl::ExportRealArrays ()
{
  ExportRealArraysSizeNumCells();
  ExportRealArraysSizeNjag();
} // SqDisu::impl::ExportRealArrays
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

    CreateTables();
    CompileStatements();

    ExportVariables();
    ExportIntArrays();
    ExportRealArrays();

    // Finish up
    sqStoreLastEditTime(m_db, m_nativeExporter);
    sqAddSqliteComment(m_nativeExporter);
    //m_db->execDML("end transaction;");
  }
  catch (std::exception&) {
    ASSERT(false);
  }
} // SqDisu::impl::Export

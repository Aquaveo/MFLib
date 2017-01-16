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
#include <private\MfData\MfExport\private\Sqlite\SqArrayWriter.h>

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
#include <private/SQLite/CppSQLite3.h>

//----- Forward declarations ---------------------------------------------------

//----- External globals -------------------------------------------------------

//----- Namespace declaration --------------------------------------------------
using namespace MfData::Export;

//----- Constants / Enumerations -----------------------------------------------

//----- Classes / Structs ------------------------------------------------------

//----- Internal functions -----------------------------------------------------

//----- Class / Function definitions -------------------------------------------
class SqArrayWriter::impl
{
public:
  impl(NativeExpLstPack* a_)
    : m_pack(a_)
    , m_grid(m_pack->GetGlobal()->NumRow(), m_pack->GetGlobal()->NumCol())
  {}

  void CreateTables();
  void CreateTriggers();
  void CreateSpTable();

  void AddStressPeriodData();
  int  CellId(int a_idx);

  NativeExpLstPack     *m_pack;
  CellIdToIJK           m_grid;
};

static const char* SQFT = "SQLITE_FILE_TIME";
typedef std::pair<std::vector<CStr>, std::vector<CStr>> pVecCStr;
typedef std::map<CStr, pVecCStr> ColMap;

//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
SqArrayWriter::SqArrayWriter (NativeExpLstPack* a_) :
  m_pack(a_)
, m_p(new SqArrayWriter::impl(a_))
{
  m_p->CreateTables();
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



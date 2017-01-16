//------------------------------------------------------------------------------
// FILE      SqMfSchema.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------

//----- Included files ---------------------------------------------------------

// 1. Precompiled header

// 2. My own header
#include <private\MfData\MfExport\private\Sqlite\SqMfSchema.h>

// 3. Standard library headers
#include <vector>
#include <sstream>

// 4. External library headers

// 5. Shared code headers

// 6. Non-shared code headers
#include <private/SQLite/CppSQLite3.h>
#include <private/MfData/MfExport/private/Sqlite/SqPackDbTables.h>
#include <private/util/StdString.h> // for ASSERT

//----- Forward declarations ---------------------------------------------------

//----- External globals -------------------------------------------------------

//----- Namespace declaration --------------------------------------------------

//----- Constants / Enumerations -----------------------------------------------
typedef const char* ccs;
ccs SQ_TABLENAMES = "SELECT name FROM sqlite_master WHERE type = 'table'";

ccs SQ_LASTEDIT = "LASTEDIT";
ccs SQ_LASTEDIT_CREATE =
  "CREATE TABLE LASTEDIT (OID INTEGER PRIMARY KEY, LastEdit TEXT)";
ccs SQ_LASTEDIT_INIT = "INSERT INTO LASTEDIT Values(?, datetime('now'))";
ccs SQ_LASTEDIT_TRIGGER =
  "BEGIN UPDATE LASTEDIT SET LastEdit = datetime('now') WHERE OID = 1; END";
ccs SQ_LASTEDIT_GET_LASTEDIT = "SELECT LastEdit FROM LASTEDIT WHERE OID=1";

ccs SQ_VARIABLES_CREATE = "CREATE TABLE VARIABLES (Variables TEXT, Value TEXT)";
ccs SQ_VARIABLES_INSERT = "INSERT INTO VARIABLES Values(";

ccs SQ_LST_ITMP_CREATE = "CREATE TABLE ITMP (SPID INTEGER PRIMARY KEY, "
  "ITMP INTEGER)";
ccs SQ_LST_ITMP_INSERT = "INSERT INTO ITMP Values(";
ccs SQ_LST_CELLGRP = "CREATE TABLE MAPIDS (CELLGRP INTEGER PRIMARY KEY, "
  "MAPID TEXT)";


//----- Classes / Structs ------------------------------------------------------

//----- Internal functions -----------------------------------------------------

//----- Class / Function definitions -------------------------------------------

//------------------------------------------------------------------------------
/// \brief replaces single quotes with 2 single quotes
//------------------------------------------------------------------------------
static std::string iEscapeSingleQuote (const std::string &a_str)
{
  std::string s;
  for (size_t i=0; i<a_str.size(); ++i)
  {
    s.push_back(a_str[i]);
    if (s.back() == '\'') s.push_back('\'');
  }
  return s;
} // iEscapeSingleQuote
//------------------------------------------------------------------------------
/// \brief Gets the SQL commands to create the tables
//------------------------------------------------------------------------------
static void iGetDbCommands (
  const std::string &a_packName
  , std::vector<std::string> *a_vSql
  )
{
  a_vSql->resize(0);
  a_vSql->push_back(SQ_LASTEDIT_CREATE);
  a_vSql->push_back(SQ_LASTEDIT_INIT);
  if ("DISU" != a_packName)
    a_vSql->push_back(SQ_VARIABLES_CREATE);
  if ("CHD" == a_packName ||
      "DRN" == a_packName ||
      "DRT" == a_packName ||
      "GHB" == a_packName ||
      "RIV" == a_packName || 
      "WEL" == a_packName)
  {
    a_vSql->push_back(SQ_LST_ITMP_CREATE);
    a_vSql->push_back(SQ_LST_CELLGRP);
  }
} // iGetDbCommands
//------------------------------------------------------------------------------
/// \brief Creates triggers to know the last time the data was edited
//------------------------------------------------------------------------------
static void iCreateLastEditTriggers (
  CppSQLite3DB *a_db
  )
{
  std::vector<std::string> ops(3), triggers;
  ops[0] = "INSERT";
  ops[1] = "DELETE";
  ops[2] = "UPDATE";

  {
    CppSQLite3Query q = a_db->execQuery(SQ_TABLENAMES);
    for (; !q.eof(); q.nextRow())
    {
      std::string tabName = q.getStringField(0);
      if (SQ_LASTEDIT == tabName) continue;
      for (size_t i=0; i<ops.size(); ++i)
      {
        std::string triggerName = tabName + "_" + ops[i];
        std::stringstream ss;
        ss << "CREATE TRIGGER " << triggerName << " AFTER " << ops[i] << " ON "
          << tabName << " " << SQ_LASTEDIT_TRIGGER;
        triggers.push_back(ss.str());
      }
    }
  }
  for (size_t i=0; i<triggers.size(); ++i) a_db->execDML(triggers[i].c_str());
} // iCreateTriggers
//------------------------------------------------------------------------------
/// \brief create the required tables for a MODFLOW package
/// \param a_db SQLite data base that will be modified
/// \param[in] a_packName: Name of the MODFLOW package
/// \param[in] a_sql: Existing list of sql create table strings
//------------------------------------------------------------------------------
void sqCreatePackageTables (
  CppSQLite3DB *a_db
  , const std::string &a_packName
  , const std::vector<std::string> &a_sql
  )
{
  if (!a_db || a_packName.empty()) return;

  try {
    std::vector<std::string> vSql;
    iGetDbCommands(a_packName, &vSql);
    for (size_t i=0; i<vSql.size(); ++i) a_db->execDML(vSql[i].c_str());
    for (size_t i=0; i<a_sql.size(); ++i) a_db->execDML(a_sql[i].c_str());
    iCreateLastEditTriggers(a_db);
  }
  catch (std::exception&) {
    ASSERT(false);
  }
} // sqCreatePackageTables
//------------------------------------------------------------------------------
/// \brief checks if a database has any tables defined
/// \param a_db SQLite data base that will be modified
/// \return true if there are tables in the database
//------------------------------------------------------------------------------
bool sqDbHasTables (
  CppSQLite3DB *a_db
  )
{
  bool r(false);
  if (!a_db) return r;

  CppSQLite3Query q = a_db->execQuery(SQ_TABLENAMES);
  if (!q.eof()) r = true;
  return r;
} // sqDbHasTables
//------------------------------------------------------------------------------
/// \brief Gets the date/time stored as the last edit
/// \param a_db SQLite data base that will be modified
/// \param a_timeStr The string that will be filled with the date/time
//------------------------------------------------------------------------------
void sqGetLastEditTime (
  CppSQLite3DB *a_db
  , std::string *a_timeStr
  )
{
  if (!a_db || !a_timeStr) return;
  *a_timeStr = "";

  CppSQLite3Query q = a_db->execQuery(SQ_LASTEDIT_GET_LASTEDIT);
  if (!q.eof()) *a_timeStr = q.getStringField(0);
} // sqGetLastEditTime
//------------------------------------------------------------------------------
/// \brief Gets the date/time stored as the last edit
/// \param a_db SQLite data base that will be modified
/// \param a_str The string that will be filled with the date/time
//------------------------------------------------------------------------------
void sqSetLastEditTime (
  CppSQLite3DB *a_db
  , const std::string &a_timeStr
  )
{
  if (!a_db || a_timeStr.empty()) return;

  std::stringstream ss;
  ss << "UPDATE LASTEDIT SET LastEdit='" << a_timeStr << "' WHERE OID=1";
  a_db->execDML(ss.str().c_str());
} // sqGetLastEditTime
//------------------------------------------------------------------------------
/// \brief Adds a variable to the SQ_VARIABLE table
/// \param a_db SQLite data base that will be modified
/// \param a_varName Variable name
/// \param a_varVal  Variable value
//------------------------------------------------------------------------------
void sqAddVariable (
  CppSQLite3DB *a_db
  , const std::string &a_varName
  , const std::string &a_varVal
  )
{
  if (!a_db || a_varName.empty()) return;

  // escape any single quotes
  std::string var = iEscapeSingleQuote(a_varName);
  std::string val = iEscapeSingleQuote(a_varVal);

  std::stringstream ss;
  ss << SQ_VARIABLES_INSERT << "'" << var << "', '" << val << "')";
  try {
    a_db->execDML(ss.str().c_str());
  }
  catch (std::exception&) {
    ASSERT(false);
  }
} // sqAddVariable
//------------------------------------------------------------------------------
/// \brief Adds a variable to the SQ_VARIABLE table
/// \param a_db SQLite data base that will be modified
/// \param a_sp stress period id
/// \param a_itmp  ITMP value
//------------------------------------------------------------------------------
void sqAddLstItmp (
  CppSQLite3DB *a_db
  , int a_sp
  , int a_itmp
  )
{
  if (!a_db || a_sp < 1) return;

  std::stringstream ss;
  ss << SQ_LST_ITMP_INSERT << a_sp << ", " << a_itmp << ")";
  a_db->execDML(ss.str().c_str());
} // sqAddLstItmp

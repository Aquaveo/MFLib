//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef SQMFSCHEMA_H
#define SQMFSCHEMA_H

#include <string>
#include <vector>

#include <ModflowLib.h> // For Real
#include <private/MfData/MfExport/private/Native/NativePackExp.h>
#include <private/MfData/MfExport/private/Sqlite/SqFile.h>
#include <private/SQLite/CppSQLite3.h>

//----- Forward declarations ---------------------------------------------------
class CppSQLite3DB;
namespace MfData { class MfPackage; }

//----- Free functions ---------------------------------------------------------
bool sqDbHasTables(
  CppSQLite3DB *a_db
  );
void sqGetPackageTableSql(const std::string &a_packName
  , std::vector<std::string> &a_sql
  );
void sqCreatePackageTables (
  CppSQLite3DB *a_db
  , const std::string &a_packName
  , const std::vector<std::string> &a_sql
  );
void sqGetLastEditTime (
  CppSQLite3DB *a_db
  , std::string *a_packName
  );
void sqSetLastEditTime (
  CppSQLite3DB *a_db
  , const std::string &a_timeStr
  );
void sqAddVariable (
  CppSQLite3DB *a_db
  , const std::string &a_varName
  , const std::string &a_varVal
  );
void sqAddLstItmp (
  CppSQLite3DB *a_db
  , int a_sp
  , int a_itmp
  );
//------------------------------------------------------------------------------
template <typename T>
void sqWriteArray(MfData::Export::NativePackExp* a_package,
                  const std::string& a_arrayName, int a_size,
                  int a_iprn, const T* a_array, Real a_mult,
                  int a_layer)
{
  a_package->GetGlobal()->GetSqArrayWriter()->WriteArray(a_package,
                        a_arrayName, a_size, a_iprn, a_array, a_mult, a_layer);
} // sqWriteArray

#endif

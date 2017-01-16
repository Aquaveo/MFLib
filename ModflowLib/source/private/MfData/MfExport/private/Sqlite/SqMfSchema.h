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
void sqWriteArray(MfData::Export::NativePackExp* a_package, const int* JJ,
                  const int* IPRN, const T* ARR, const Real* MULT,
                  const int* K)
{
  try {
    CppSQLite3DB* db = sqLiteDbForPackage(a_package);
    CStr packageName = a_package->GetPackage()->PackageName();
    if (!sqDbHasTables(db)) {
      std::vector<std::string> sql;
      sqCreatePackageTables(db, packageName, sql);
    }

    // Add to the ArrayInfo table
    int arrayOid;
    {
      CppSQLite3Statement stmtInsertArrayInfo;
      stmtInsertArrayInfo = db->compileStatement(
        "INSERT INTO ArrayInfo VALUES(?, ?, ?, ?, ?)");
      stmtInsertArrayInfo.bind(2, packageName.c_str());
      stmtInsertArrayInfo.bind(3, *MULT);
      stmtInsertArrayInfo.bind(4, *IPRN);
      stmtInsertArrayInfo.bind(5, *K);
      stmtInsertArrayInfo.execDML();
      stmtInsertArrayInfo.reset();
      stmtInsertArrayInfo.finalize();
      arrayOid = db->lastRowId();
    }

    // Create cell IDs
    std::vector<int> cellIds(*JJ);
    for (size_t i = 0; i < *JJ; ++i) { cellIds[i] = i+1; }

    // Add to the RealArray table
    {
      CppSQLite3Statement stmtInsertRealArray = db->compileStatement(
        "INSERT INTO RealArray VALUES(?, ?, ?, ?);");
      for (size_t i = 0; i < *JJ; ++i) {
        stmtInsertRealArray.bind(2, static_cast<int>(arrayOid));
        stmtInsertRealArray.bind(3, cellIds[i]);
        stmtInsertRealArray.bind(4, ARR[i]);
        stmtInsertRealArray.execDML();
        stmtInsertRealArray.reset();
      }
      stmtInsertRealArray.finalize();
    }
  }
  catch (std::exception&) {
    ASSERT(false);
  }
}
//------------------------------------------------------------------------------
template <typename T>
void sqWriteArray(MfData::Export::NativePackExp* a_package, const int* IPRN,
                  const int* ARR, const Real* MULT, const int* K)
{
  //CppSQLite3DB* db = sqLiteDbForPackage(a_package);
  sqLiteDbForPackage(a_package);

  //(void)a_package;
  (void)IPRN;
  (void)ARR;
  (void)MULT;
  (void)K;
}

#endif

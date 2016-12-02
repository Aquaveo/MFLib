//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef SQMFSCHEMA_H
#define SQMFSCHEMA_H

#include <string>
#include <vector>

class CppSQLite3DB;

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

#endif

//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef SQFILE_H
#define SQFILE_H

//----- Forward declarations ---------------------------------------------------
class CppSQLite3DB;

namespace MfData
{
namespace Export
{

static const char* SQFT = "SQLITE_FILE_TIME";

//----- Forward declarations ---------------------------------------------------
class NativePackExp;

//----- Free functions ---------------------------------------------------------
CppSQLite3DB *sqLiteDbForPackage(NativePackExp* a_);
void sqLiteCloseAllDb();
void sqAddSqliteComment (NativePackExp* a_exporter);
void sqStoreLastEditTime (CppSQLite3DB* a_db, NativePackExp* a_exporter);

} // namespace Export
} // namespace MfData
#endif

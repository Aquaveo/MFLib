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
#include <private\MfData\MfExport\private\Sqlite\SqFile.h>

// 3. Standard library headers
#include <map>
#include <sstream>

// 4. External library headers

// 5. Shared code headers

// 6. Non-shared code headers
#include <private/MfData/MfExport/private/Mf2kNative.h>
#include <private/MfData/MfExport/private/TxtExporter.h>
#include <private/MfData/MfExport/private/Native/NativePackExp.h>
#include <private/MfData/Packages/MfPackage.h>
#include <private/SQLite/CppSQLite3.h>

//----- Forward declarations ---------------------------------------------------

//----- External globals -------------------------------------------------------

//----- Namespace declaration --------------------------------------------------
namespace MfData
{
namespace Export
{

//----- Constants / Enumerations -----------------------------------------------

//----- Classes / Structs ------------------------------------------------------

//----- Internal functions -----------------------------------------------------

//----- Class / Function definitions -------------------------------------------

//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static std::map<CStr, CppSQLite3DB*> &iFileMap ()
{
  static std::map<CStr, CppSQLite3DB*> m_;
  return m_;
} // iFileMap
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
static CppSQLite3DB* iGetDbFile (NativePackExp *a_)
{
  CStr packName = a_->GetPackage()->PackageName();
  CStr base = a_->GetNative()->GetExp()->GetBaseFileName();
  base += "_";
  base += packName;
  base += ".db";

  FILE *fp(fopen(base.c_str(), "r"));
  if (fp)
  {
    fclose(fp);
    remove(base.c_str());
  }

  CppSQLite3DB *ret(new CppSQLite3DB);
  ret->open(base.c_str());
  ret->execDML("begin transaction;");
  return ret;
} // iGetDbFile
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CppSQLite3DB *SqLiteDbForPackage (NativePackExp *a_)
{
  std::map<CStr, CppSQLite3DB*> &files(iFileMap());
  CStr packName = a_->GetPackage()->PackageName();
  auto it = files.find(packName);
  if (it == files.end())
  {
    CppSQLite3DB *f = iGetDbFile(a_);
    files.insert(std::make_pair(packName, f));
    it = files.find(packName);
  }
  return it->second;
} // SqLiteDbForPackage
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void SqLiteCloseAllDb ()
{
  std::map<CStr, CppSQLite3DB*> &files(iFileMap());
  auto it = files.begin();
  auto itEnd = files.end();
  for (; it != itEnd; ++it)
  {
    it->second->execDML("commit transaction;");
    it->second->close();
  }
} // SqLiteCloseAllDb

} // namespace Export
} // namespace MfData


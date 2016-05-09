//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef SQFILE_H
#define SQFILE_H

class CppSQLite3DB;
namespace MfData
{
namespace Export
{

class NativePackExp;

CppSQLite3DB *SqLiteDbForPackage(NativePackExp* a_);
void SqLiteCloseAllDb();

} // namespace Export
} // namespace MfData
#endif

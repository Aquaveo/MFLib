//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef EXPGEODB_H
#define EXPGEODB_H

#include <private/MfData/MfExport/private/MfExporterImpl.h>

namespace MfData
{
  class MfGlobal;

  namespace Export
  {
    class ExpGeoDb : public MfExporterImpl
    {
    public:
      ExpGeoDb();
      virtual ~ExpGeoDb();

      virtual void SetFileName(const char *a_);

      virtual bool ExportPackage(MfGlobal *a_global,
                                 MfPackage *a_package);

    private:
      virtual bool CreateEmptyGDB(const char* const a_path,
                                  const char* const a_fname,
                                  int* a_personal);
      virtual bool OpenExistingDB(const char* const a_path,
                                  const char* const a_fname);

      ExpGeoDb(const ExpGeoDb &rhs);
      const ExpGeoDb& operator=(const ExpGeoDb &rhs);
    };

    class ExpGeoDbFree : public ExpGeoDb
    {
    public:
      ExpGeoDbFree();

      virtual bool ExportPackage(MfGlobal *a_global,
                                 MfPackage *a_package);

    private:
      ExpGeoDbFree(const ExpGeoDb &rhs);
      const ExpGeoDbFree& operator=(const ExpGeoDb &rhs);
    };

    class ExpGeoDbSQLite : public ExpGeoDb
    {
    public:
      ExpGeoDbSQLite();

      virtual bool OpenExistingDB(const char* const a_path,
                                  const char* const a_fname);

    private:
      virtual bool CreateEmptyGDB(const char* const a_path,
                                  const char* const a_fname,
                                  int* a_personal);
      ExpGeoDbSQLite(const ExpGeoDbSQLite &rhs);
      const ExpGeoDbSQLite& operator=(const ExpGeoDbSQLite &rhs);
    };
  }
}

#endif

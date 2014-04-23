//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef Gdb_H
#define Gdb_H

#include <private/util/util.h>

class Db;

namespace Gdb
{
  int CheckLicense();
  int ShutDown();
  int WriteArray(const char *a_fname,
                 const char *a_table,
                 const char *a_cellField,
                 const char *a_field,
                 const char *a_query,
                 int a_nData,
                 int a_startCellId,
                 int a_stressPeriod,
                 const int *a_data);
  int WriteArray(const char *a_fname,
                 const char *a_table,
                 const char *a_cellField,
                 const char *a_field,
                 const char *a_query,
                 int a_nData,
                 int a_startCellId,
                 int a_stressPeriod,
                 int a_etsSegId,
                 const float *a_data);
  int WriteArray(const char *a_fname,
                 const char *a_table,
                 const char *a_cellField,
                 const char *a_field,
                 const char *a_query,
                 int a_nData,
                 int a_startCellId,
                 int a_stressPeriod,
                 int a_etsSegId,
                 const double *a_data);
  int AddAuxColToTable(const char* a_tab,
                       const char* a_col);
  void SetExportOnlySpecifiedTablesString(const char* a_str);
};

class Db
{
public:
  virtual ~Db();
  static Db* db();

  int CreateEmptyGDB(const char *a_path,
                     const char *a_file,
                     int *personalGeoDb);
  int CreateEmptyGDBSQLite(const char *a_path,
                           const char *a_file,
                           int *personalGeoDb);

  virtual int BeginCache(const char *a_table,
                         const char *a_query);
  virtual int BeginCacheMutipleRows(const char *a_table,
                                    const char *a_query,
                                    int a_nFields,
                                    int a_nRows);
  virtual int FlushToDb();

  virtual int AddValue(const char *a_field,
                       const char *a_val,
                       int a_row=-1);
  virtual int AddValue(const char *a_field,
                       int a_val,
                       int a_row=-1);
  virtual int AddValue(const char *a_field,
                       float a_val,
                       int a_row=-1);
  virtual int AddValue(const char *a_field,
                       double a_val,
                       int a_row=-1);

  virtual int AddValues(std::vector<CStr> &a_fields,
                        std::vector<const int *> &a_vals);
  virtual int AddValues(std::vector<CStr> &a_fields,
                        std::vector<const float *> &a_vals);
  virtual int AddValues(std::vector<CStr> &a_fields,
                        std::vector<const double *> &a_vals);

  virtual int CreateMissingTables();

  virtual void DeleteAllRowsInTable(const char *a_table);
          void SetFileName(const char* a_path,
                           const char* a_fname);
          bool OpenExistingSQLiteDb(const char* a_path,
                                    const char* a_fname);
          bool OpenExistingGDB(const char* a_path,
                               const char* a_fname);

protected:
  static Db *m_single;
  virtual void CreateSingle();
  void Destroy();
  Db();

private:
  Db(const Db &rhs);
  const Db& operator=(const Db &rhs);


  class impl;
  impl *m_p;
};

#endif

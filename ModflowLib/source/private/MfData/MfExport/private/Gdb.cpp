//------------------------------------------------------------------------------
// FILE      Gdb.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/MfData/MfExport/private/Gdb.h>

#include <map>
#include <set>

#include <private/util/util.h>


// this is a class that wraps calls to a dll
LATELOAD_BEGIN_CLASS(EMfLink,ModflowLink,FALSE,FALSE)
  LATELOAD_FUNC_0(FALSE,int,STDMETHODVCALLTYPE,CheckLicense)
  LATELOAD_FUNC_0(FALSE,int,STDMETHODVCALLTYPE,ShutDown)
  LATELOAD_FUNC_3(FALSE,int,STDMETHODVCALLTYPE,fnCreateEmptyGDB,const char *,const char*,const int *);
  LATELOAD_FUNC_2(FALSE,int,STDMETHODVCALLTYPE,fnOpenExistingGDB,const char *,const char *)
  LATELOAD_FUNC_3(FALSE,int,STDMETHODVCALLTYPE,fnCreateEmptySQLiteDB,const char *,const char*,const int *);
  LATELOAD_FUNC_2(FALSE,int,STDMETHODVCALLTYPE,fnOpenExistingSQLite,const char *,const char *)

  LATELOAD_FUNC_9(FALSE,int,STDMETHODVCALLTYPE,fnWriteArrayInt,const char*,const char*,const char*,const char*,const char *,int,int,int*,const int*)
  LATELOAD_FUNC_9(FALSE,int,STDMETHODVCALLTYPE,fnWriteArrayFlt,const char*,const char*,const char*,const char*,const char *,int,int,int*,const float*)
  LATELOAD_FUNC_9(FALSE,int,STDMETHODVCALLTYPE,fnWriteArrayDbl,const char*,const char*,const char*,const char*,const char *,int,int,int*,const double*)

  LATELOAD_FUNC_3(FALSE,int,STDMETHODVCALLTYPE,fnStartCacheing,const char *,const char*,const char*);
  LATELOAD_FUNC_5(FALSE,int,STDMETHODVCALLTYPE,fnStartCacheing2,const char *,const char*,const char*,int,int);
  LATELOAD_FUNC_0(FALSE,int,STDMETHODVCALLTYPE,fnStopCacheing)

  LATELOAD_FUNC_3(FALSE,int,STDMETHODVCALLTYPE,fnAddDataToCacheStr2,const char *,const char *,int)
  LATELOAD_FUNC_3(FALSE,int,STDMETHODVCALLTYPE,fnAddDataToCacheInt2,const char *,int,int)
  LATELOAD_FUNC_3(FALSE,int,STDMETHODVCALLTYPE,fnAddDataToCacheFlt2,const char *,float,int)
  LATELOAD_FUNC_3(FALSE,int,STDMETHODVCALLTYPE,fnAddDataToCacheDbl2,const char *,double,int)
  LATELOAD_FUNC_2(FALSE,int,STDMETHODVCALLTYPE,fnAddAuxColToTable,const char *,const char *)
  LATELOAD_FUNC_2(FALSE,int,STDMETHODVCALLTYPE,fnDeleteAllRowsInTable,const char *,const char *)

  LATELOAD_FUNC_1(FALSE,int,STDMETHODVCALLTYPE,fnCreateMissingTables,const char *)
LATELOAD_END_CLASS()

Db* Db::m_single = NULL;
class Db::impl
{
public:
  impl();
  ~impl();

  int BeginCache(const char *a_table,
                 const char *a_query);
  int BeginCacheMutipleRows(const char *a_table,
                            const char *a_query,
                            int a_nFields,
                            int a_nRows);
  int FlushToDb();

  int AddValue(const char *a_field,
               const char *a_val,
               int a_row=-1);
  int AddValue(const char *a_field,
               int a_val,
               int a_row=-1);
  int AddValue(const char *a_field,
               float a_val,
               int a_row=-1);
  int AddValue(const char *a_field,
               double a_val,
               int a_row=-1);

  template<class T>
  int AddValues(std::vector<CStr> &a_fields,
                std::vector<const T *> &a_vals);

  int CreateMissingTables();
  void DeleteAllRowsInTable(const char *a_table);
  bool OpenExistingSQLiteDb(const char* a_path,
                            const char* a_fname);
  bool OpenExistingGDB(const char* a_path,
                       const char* a_fname);

  void SetFileName(const CStr &a_) { m_file = a_; }

private:
  CStr        m_file;
  EMfLink     m_link;
};

namespace
{
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
  static int iCheckLicense ()
  {
    static int flag(-1);
    if (flag == -1)
    {
      EMfLink link;
      if (link.CheckLicense() == 0)
      {
        printf("Unable to get Arc View license.\n");
        flag = 0;
      }
      flag = 1;
    }
    return flag;
  } // CheckLicense
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
  static CStr& GetSpecifiedTablesStr ()
  {
    static CStr m_str;
    return m_str;
  } // GetSpecifiedTablesStr
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
  static std::set<CStr>& GetClearedTables ()
  {
    static std::set<CStr> m_set;
    return m_set;
  } // GetClearedTables
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
  static bool iCanExportTable (const char* a_fname,
                               const char* a_table)
  {

    CStr tabs(GetSpecifiedTablesStr());
    // we will do this all the time even if they haven't specified certain tables
    //if (tabs.length() < 1)
    //  return true;

    // if no tables have been specified then we do this to all tables
    if (tabs.IsEmpty() || tabs.Find(a_table) != -1)
    {
      // make sure the rows of the table have been deleted
      std::set<CStr>& clearedTabs(GetClearedTables());
      if (clearedTabs.find(a_table) == clearedTabs.end())
      {
         EMfLink link;
         link.fnDeleteAllRowsInTable(a_fname, a_table);
         clearedTabs.insert(a_table);
      }
      return true;
    }
    else
      return false;
  } // iCanExportTable
}
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
int Gdb::CheckLicense ()
{
  return iCheckLicense();
} // Gdb::CheckLicense
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
int Gdb::ShutDown ()
{
  EMfLink link;
  return link.ShutDown();
} // Gdb::ShutDown
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
int Gdb::WriteArray (const char *a_fname,
                     const char *a_table,
                     const char *a_cellField,
                     const char *a_field,
                     const char *a_query,
                     int a_nData,
                     int a_startCellId,
                     int a_stressPeriod,
                     const int *a_data)
{
  if (!iCanExportTable(a_fname, a_table))
    return 0;
  int vals[2];
  vals[0] = a_stressPeriod;
  vals[1] = -1;
  EMfLink link;
  return link.fnWriteArrayInt(a_fname, a_table, a_cellField, a_field, a_query,
                              a_nData, a_startCellId, vals, a_data);
} // Gdb::WriteArray
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
int Gdb::WriteArray (const char *a_fname,
                     const char *a_table,
                     const char *a_cellField,
                     const char *a_field,
                     const char *a_query,
                     int a_nData,
                     int a_startCellId,
                     int a_stressPeriod,
                     int a_etsSegId,
                     const float *a_data)
{
  if (!iCanExportTable(a_fname, a_table))
    return 0;
  int vals[2];
  vals[0] = a_stressPeriod;
  vals[1] = a_etsSegId;
  EMfLink link;
  return link.fnWriteArrayFlt(a_fname, a_table, a_cellField, a_field, a_query,
                              a_nData, a_startCellId, vals, a_data);
} // Gdb::WriteArray
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
int Gdb::WriteArray (const char *a_fname,
                     const char *a_table,
                     const char *a_cellField,
                     const char *a_field,
                     const char *a_query,
                     int a_nData,
                     int a_startCellId,
                     int a_stressPeriod,
                     int a_etsSegId,
                     const double *a_data)
{
  if (!iCanExportTable(a_fname, a_table))
    return 0;
  int vals[2];
  vals[0] = a_stressPeriod;
  vals[1] = a_etsSegId;
  EMfLink link;
  return link.fnWriteArrayDbl(a_fname, a_table, a_cellField, a_field, a_query,
                              a_nData, a_startCellId, vals, a_data);
} // Gdb::WriteArray
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
int Gdb::AddAuxColToTable (const char* a_tab,
                           const char* a_col)
{
  EMfLink link;
  return link.fnAddAuxColToTable(a_tab, a_col);
} // Gdb::AddAuxColToTable
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void Gdb::SetExportOnlySpecifiedTablesString (const char* a_str)
{
  GetSpecifiedTablesStr() = a_str;
} // Gdb::SetExportOnlySpecifiedTablesString

//------------------------------------------------------------------------------
/// \brief Constructor
//------------------------------------------------------------------------------
Db::Db () :
m_p(new Db::impl())
{
} // Db::Db
//------------------------------------------------------------------------------
/// \brief Destructor
//------------------------------------------------------------------------------
Db::~Db ()
{
  try
  {
    if (m_p)
      delete(m_p);
    m_p = NULL;
  }
  catch (...)
  {
  }
} // Db::~Db
//------------------------------------------------------------------------------
/// \brief Destructor
//------------------------------------------------------------------------------
void Db::Destroy ()
{
  if (m_single)
    delete(m_single);
  m_single = NULL;
} // Db::Destroy
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
Db *Db::db ()
{
  if (!m_single)
    m_single = new Db();
  return m_single;
} // Db::db
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void Db::CreateSingle ()
{
} // Db::AllocateClass
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
int Db::CreateEmptyGDB (const char *a_path,
                        const char *a_file,
                        int *a_personalGeoDb)
{
  EMfLink link;
  int rval(link.fnCreateEmptyGDB(a_path, a_file, a_personalGeoDb));
  if (rval != 0)
  {
    CStr file;
    file.Format("%s\\%s", a_path, a_file);
    m_p->SetFileName(file);
  }
  return rval;
} // Db::CreateEmptyGDB
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
int Db::CreateEmptyGDBSQLite (const char *a_path,
                              const char *a_file,
                              int *a_personalGeoDb)
{
  EMfLink link;
  int rval(link.fnCreateEmptySQLiteDB(a_path, a_file, a_personalGeoDb));
  if (rval != 0)
  {
    CStr file;
    file.Format("%s\\%s", a_path, a_file);
    m_p->SetFileName(file);
  }
  return rval;
} // Db::CreateEmptyGDBSQLite
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
int Db::CreateMissingTables()
{
  return(m_p->CreateMissingTables());
} // Db::CreateMissingTables
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void Db::DeleteAllRowsInTable (const char* a_table)
{
  return(m_p->DeleteAllRowsInTable(a_table));
} // Db::DeleteAllRowsInTable
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void Db::SetFileName (const char* a_path,
                      const char* a_fname)
{
  CStr path(a_path), fname(a_fname);
  if (path.IsEmpty() && fname.IsEmpty()) return;
  CStr file;
  file.Format("%s\\%s", a_path, a_fname);
  m_p->SetFileName(file);
} // Db::SetFileName
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool Db::OpenExistingSQLiteDb (const char* a_path,
                               const char* a_fname)
{
  return(m_p->OpenExistingSQLiteDb(a_path, a_fname));
} // Db::OpenExistingSQLiteDb
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool Db::OpenExistingGDB (const char* a_path,
                          const char* a_fname)
{
  return(m_p->OpenExistingGDB(a_path, a_fname));
} // Db::OpenExistingGDB
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
int Db::BeginCache (const char *a_table,
                    const char *a_query)
{
  return(m_p->BeginCache(a_table, a_query));
} // Db::BeginCache
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
int Db::BeginCacheMutipleRows (const char *a_table,
                               const char *a_query,
                               int a_nFields,
                               int a_nRows)
{
  return(m_p->BeginCacheMutipleRows(a_table,a_query,a_nFields,a_nRows));
} // Db::BeginCacheMutipleRows
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
int Db::FlushToDb ()
{
  return(m_p->FlushToDb());
} // Db::FlushToDb
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
int Db::AddValue (const char *a_field,
                   const char *a_val,
                   int a_row/*-1*/)
{
  if (!a_val) return 0;
  return(m_p->AddValue(a_field,a_val,a_row));
} // Db::AddValue
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
int Db::AddValue (const char *a_field,
                   int a_val,
                   int a_row/*-1*/)
{
  return(m_p->AddValue(a_field,a_val,a_row));
} // Db::AddValue
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
int Db::AddValue (const char *a_field,
                   float a_val,
                   int a_row/*-1*/)
{
  return(m_p->AddValue(a_field,a_val,a_row));
} // Db::AddValue
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
int Db::AddValue (const char *a_field,
                  double a_val,
                  int a_row/*-1*/)
{
  return(m_p->AddValue(a_field,a_val,a_row));
} // Db::AddValue
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
int Db::AddValues (std::vector<CStr> &a_fields,
                   std::vector<const int *> &a_vals)
{
  return(m_p->AddValues(a_fields, a_vals));
} // Db::AddValues
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
int Db::AddValues (std::vector<CStr> &a_fields,
                   std::vector<const float *> &a_vals)
{
  return(m_p->AddValues(a_fields, a_vals));
} // Db::AddValues
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
int Db::AddValues (std::vector<CStr> &a_fields,
                   std::vector<const double *> &a_vals)
{
  return(m_p->AddValues(a_fields, a_vals));
} // Db::AddValues


//------------------------------------------------------------------------------
/// \brief Constructor
//------------------------------------------------------------------------------
Db::impl::impl ()
{
} // Db::impl::impl
//------------------------------------------------------------------------------
/// \brief Destructor
//------------------------------------------------------------------------------
Db::impl::~impl ()
{
} // Db::impl::~impl
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
int Db::impl::CreateMissingTables ()
{
  return m_link.fnCreateMissingTables(m_file);
} // Db::impl::CreateMissingTables
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
void Db::impl::DeleteAllRowsInTable (const char *a_table)
{
  m_link.fnDeleteAllRowsInTable(m_file, a_table);
} // Db::impl::DeleteAllRowsInTable
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool Db::impl::OpenExistingSQLiteDb (const char* a_path,
                                     const char* a_fname)
{
  CStr path(a_path), fname(a_fname);
  if (path.IsEmpty() && fname.IsEmpty())
  {
    CStr filename = m_file;
    util::StripFileFromFilename(filename, path);
    util::StripPathFromFilename(filename, fname);
  }
  return m_link.fnOpenExistingSQLite(path.c_str(), fname.c_str()) ? 1 : 0;
} // Db::impl::OpenExistingSQLiteDb
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
bool Db::impl::OpenExistingGDB (const char* a_path,
                                const char* a_fname)
{
  return m_link.fnOpenExistingGDB(a_path, a_fname) ? 1 : 0;
} // Db::impl::OpenExistingGDB
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
int Db::impl::BeginCache (const char *a_table,
                          const char *a_query)
{
  if (!iCanExportTable(m_file, a_table))
    return 0;
  return m_link.fnStartCacheing(m_file, a_table, a_query);
} // Db::impl::BeginCache
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
int Db::impl::BeginCacheMutipleRows (const char *a_table,
                                     const char *a_query,
                                     int a_nFields,
                                     int a_nRows)
{
  if (!iCanExportTable(m_file, a_table))
    return 0;
  return m_link.fnStartCacheing2(m_file, a_table, a_query, a_nFields, a_nRows);
} // Db::impl::BeginCacheMutipleRows
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
int Db::impl::FlushToDb ()
{
  return m_link.fnStopCacheing();
} // Db::impl::FlushToDb
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
int Db::impl::AddValue (const char *a_field,
                        const char *a_val,
                        int a_row/*-1*/)
{
  return m_link.fnAddDataToCacheStr2(a_field, a_val, a_row);
} // Db::impl::AddValue
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
int Db::impl::AddValue (const char *a_field,
                        int a_val,
                        int a_row/*-1*/)
{
  return m_link.fnAddDataToCacheInt2(a_field, a_val, a_row);
} // Db::impl::AddValue
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
int Db::impl::AddValue (const char *a_field,
                        float a_val,
                        int a_row/*-1*/)
{
  return m_link.fnAddDataToCacheFlt2(a_field, a_val, a_row);
} // Db::impl::AddValue
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
int Db::impl::AddValue (const char *a_field,
                        double a_val,
                        int a_row/*-1*/)
{
  return m_link.fnAddDataToCacheDbl2(a_field, a_val, a_row);
} // Db::impl::AddValue
//------------------------------------------------------------------------------
/// \brief 
//------------------------------------------------------------------------------
template<class T>
int Db::impl::AddValues (std::vector<CStr> &a_fields,
                         std::vector<const T *> &a_vals)
{
  int rval(1);
  size_t i;
  for (i=0; i<a_fields.size(); i++)
  {
    if (a_vals[i])
      rval = AddValue(a_fields[i], *a_vals[i]);
  }
  return rval;
} // Db::impl::AddValues


///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST
#include <private/MfData/MfExport/private/Gdb.t.h>


//------------------------------------------------------------------------------
//void GdbT::testCreateClass ()
//{
//}

#endif

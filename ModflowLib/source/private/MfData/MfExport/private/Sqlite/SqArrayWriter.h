//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef SQARRAYWRITER_H
#define SQARRAYWRITER_H
#include <private\util\util.h>
#include <private\SQLite\CppSQLite3.h>

namespace MfData
{
namespace Export
{

class NativePackExp;
class SqArrayWriter
{
public:
  SqArrayWriter(NativePackExp* a_);
  SqArrayWriter();
  ~SqArrayWriter();

  void EndWriteFile();
  void AddSqComment();
  sqlite_int64 AddToArrayInfo();
  void WriteArraySetup (MfData::Export::NativePackExp* a_package,
                        const std::string& a_arrayName,
                        int a_size, int a_iprn, Real a_mult, int a_layer,
                        CppSQLite3DB** a_db, sqlite_int64& a_arrayOid,
                        std::vector<int>& cellIds);
  void AddToIntArray(sqlite_int64 a_arrayOid, const std::vector<int>& a_cellIds,
                     const int* a_array, int a_size);
  CppSQLite3Statement* GetInsertRealArrayStatement();
  void WriteArray(MfData::Export::NativePackExp* a_package,
                  const std::string& a_arrayName, int a_size,
                  int a_iprn, const int* a_array, Real a_mult,
                  int a_layer);

  //------------------------------------------------------------------------------
  /// \brief Add values to the RealArray table.
  /// \param[in] a_arrayOid: OID in ArrayInfo table, for ArrayInfo_OID field.
  /// \param[in] a_cellIds:  Values for the CellId field.
  /// \param[in] a_array:    Values for the Values field.
  /// \param[in] a_size:     Number of values.
  //------------------------------------------------------------------------------
  template <typename T>
  void AddToRealArray(sqlite_int64 a_arrayOid, const std::vector<int>& a_cellIds,
                      const T* a_array, int a_size)
  {
    try {
      CppSQLite3Statement* stmtInsertRealArray = GetInsertRealArrayStatement();
      for (size_t i = 0; i < a_size; ++i) {
        stmtInsertRealArray->bind(2, static_cast<int>(a_arrayOid));
        stmtInsertRealArray->bind(3, a_cellIds[i]);
        stmtInsertRealArray->bind(4, a_array[i]);
        stmtInsertRealArray->execDML();
        stmtInsertRealArray->reset();
      }
    }
    catch (std::exception&) {
      ASSERT(false);
    }
  } // AddToRealArray
  //------------------------------------------------------------------------------
  /// \brief Write array to SQLite. Template for Real or double arrays.
  /// \param[in] a_package:   The package.
  /// \param[in] a_arrayName: Name of the array.
  /// \param[in] a_size:      Number of values in the array.
  /// \param[in] a_iprn:      IPRN.
  /// \param[in] a_mult:      Multiplier.
  /// \param[in] a_layer:     Layer.
  //------------------------------------------------------------------------------
  template <typename T>
  void WriteArray(MfData::Export::NativePackExp* a_package,
                  const std::string& a_arrayName, int a_size,
                  int a_iprn, const T* a_array, Real a_mult,
                  int a_layer)
  {
    try {
      CppSQLite3DB* db;
      sqlite_int64 arrayOid;
      std::vector<int> cellIds;
      WriteArraySetup(a_package, a_arrayName, a_size, a_mult, a_iprn, a_layer,
                      &db, arrayOid, cellIds);
      AddToRealArray(arrayOid, cellIds, a_array, a_size);
    }
    catch (std::exception&) {
      ASSERT(false);
    }
  } // WriteArray

private:
  class impl;
  impl *m_p;
  NativePackExp* m_pack;
};

} // namespace Export
} // namespace MfData
#endif

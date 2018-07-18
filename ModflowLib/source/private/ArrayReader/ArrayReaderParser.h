//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef ARRAYREADERPARSER_H
#define ARRAYREADERPARSER_H

#include <private/util/util.h>

class ArrayReaderParser
{
public:
  ArrayReaderParser(const CStr &a_);
  ~ArrayReaderParser() {}

  bool   ValidInputString() const { return m_valid; }

  bool   ConstantValue() const { return m_const; }
  double GetConstValue() const { return m_const ? m_mult : 0.0; }
  double GetMultiplier() const { return m_const ? 1.0 : m_mult; }
  int    GetIPRN() const { return m_IPRN; }
  CStr   GetFileName() const { return m_file; }
  CStr   GetPath() const { return m_path; }
  VEC_INT_PAIR GetIndices() const { return m_indices; }
  int    GetArraySize() const;

private:
  ArrayReaderParser(const ArrayReaderParser &rhs);
  const ArrayReaderParser& operator=(const ArrayReaderParser &rhs);

  void   ParseString();

  bool   m_valid, m_const;
  CStr   m_str, m_file, m_path;
  double m_mult;
  int    m_IPRN;
  VEC_INT_PAIR m_indices;
};

#endif

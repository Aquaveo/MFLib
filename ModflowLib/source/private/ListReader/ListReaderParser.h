//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef LISTREADERPARSER_H
#define LISTREADERPARSER_H

#include <private/util/util.h>

class ListReaderParser
{
public:
  ListReaderParser(const CStr &a_);

  bool  ValidInputString() const { return m_valid; }
  const CStr& GetFileName() const { return m_file; }
  const CStr& GetPath() const { return m_path; }
  int   GetStressPeriod() const { return m_stressPeriod; }

private:
  ListReaderParser(const ListReaderParser &rhs);
  const ListReaderParser& operator=(const ListReaderParser &rhs);

  void   ParseString();

  bool m_valid;
  CStr m_str, m_file, m_path;
  int  m_stressPeriod;

};

#endif


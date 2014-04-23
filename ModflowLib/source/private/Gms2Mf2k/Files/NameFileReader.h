//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#pragma once

#include <private/util/util.h>

class NameFileReaderT;

class NameFileReader
{
friend NameFileReaderT;
public:
  NameFileReader(const char *a_);
  ~NameFileReader();

  bool ReadFile();
  int  GetNumFilesToRead();
  bool GetFileNameAtIdx(int a_, CStr &a_file);
  void AddFileToRead(const CStr& a_file);
  void GetDisFile(CStr &a_file);
  void GetDisuFile(CStr &a_file);

private:
  NameFileReader(const NameFileReader &rhs);
  const NameFileReader& operator=(const NameFileReader &rhs);

  class impl;
  impl *m_p;
};

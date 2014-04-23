//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#pragma once

#include <private/util/util.h>

class LgrFileReaderT;

class LgrFileReader
{
friend LgrFileReaderT;
public:
  LgrFileReader(const char *a_);
  ~LgrFileReader();

  bool ReadFile();
  size_t  GetNumFilesToRead();
  CStr GetFileNameAtIdx(size_t a_);

private:
  LgrFileReader(const LgrFileReader &rhs);
  const LgrFileReader& operator=(const LgrFileReader &rhs);

  class impl;
  impl *m_p;
};

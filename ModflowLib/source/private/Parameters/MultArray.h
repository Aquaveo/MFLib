//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef MULTARRAY_H
#define MULTARRAY_H

#include <private/util/util.h>

class MultArray
{
public:
  MultArray(const char* a_fName,
            const char* a_paramName);
  ~MultArray();

  bool GetArray(std::vector<Real> &a_);

private:
  MultArray(const MultArray &rhs);
  const MultArray& operator=(const MultArray &rhs);

  class impl;
  impl *m_p;
};

#endif


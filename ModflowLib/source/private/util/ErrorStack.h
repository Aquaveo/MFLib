//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef ERRORSTACK_H
#define ERRORSTACK_H

#include <iostream>
#include <vector>
#include <private/util/StdString.h>

class ErrorStack
{
public:
  ErrorStack() {}

  static ErrorStack& Get()
  {
    static ErrorStack *stack;
    if (!stack)
    {
      stack = new ErrorStack();
    }
    return(*stack);
  }

  bool ErrorsExist()
  {
    return (m_errors.empty() ? false : true);
  }
  void PutError(const CStr &a_)
  {
    if (!a_.IsEmpty())
      m_errors.push_back(a_);
  }
  void ClearErrors()
  {
    m_errors.clear();
  }

  void PrintErrors (std::ostream &a_)
  {
    for (size_t i=0; i<m_errors.size(); i++)
      a_ << m_errors.at(i) << "\n";
    m_errors.clear();
  }

private:
  ErrorStack(const ErrorStack &rhs);
  const ErrorStack& operator= (const ErrorStack &rhs);

  std::vector<CStr> m_errors;
};

#endif
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef EEXCEPTION_H
#define EEXCEPTION_H

#include <private\util\StdString.h>

///////////////////////////////////////////////////////////////////////////////
// class EException
///////////////////////////////////////////////////////////////////////////////
class EException : public std::exception {
  public:
    EException (const char* a_str="error") { m_str = a_str; }

    virtual const char* what() const throw() {
      return m_str.c_str();
    }

  private:
    CStr m_str;
};


#endif

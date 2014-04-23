//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef H5VECCSTRREADER_T_H
#define H5VECCSTRREADER_T_H

#include <cxxtest/TestSuite.h>

#include <private/util/util.h>
class H5VecCStrReaderT : public CxxTest::TestSuite
{
public:
  void setUp();
  void testCreateClass();
  void testFillInStrings();
private:
  CStr m_file;
};

#endif

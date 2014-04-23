//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef PARAMFILEREADERT_H
#define PARAMFILEREADERT_H

#include <cxxtest/TestSuite.h>
#include <private/util/util.h>

class ParamFileReaderT : public CxxTest::TestSuite
{
public:
  void setUp();
  void testCreateClass();
  void testFileCardToInt();
  void testFillInListFromFile1();
private:
  CStr m_file;
};
#endif


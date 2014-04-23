//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef PROCESSOR_T_H
#define PROCESSOR_T_H
#include <cxxtest/TestSuite.h>

class ProcessorT : public CxxTest::TestSuite
{
public:
  void testCreateClass();
  void testValidSetUp();
  void testDoConvertFile();
  void testReadArray();
  void testWriteArray();
  void testReadList();
  void testWriteList();
  void testInputFile();
  void testOutputFile();
  void testNumRowandNumCol();
};

#endif

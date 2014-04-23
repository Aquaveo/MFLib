//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef SENFILEREADER_T_H
#define SENFILEREADER_T_H

#include <cxxtest/TestSuite.h>

class SenFileReaderT : public CxxTest::TestSuite
{
public:
  void setUp();
  void testFillInStartingVals();
  void testNullParamList();
  void testFileDoesntExist();
  void testFileFormatWrong();
  void testFileFormatWrongPartWayThroughFile();

  char m_path[5000];
  CStr m_file, m_file1, m_parFile;
};
#endif

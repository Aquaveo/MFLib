//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef DISFILEREADER_T_H
#define DISFILEREADER_T_H
#include <cxxtest/TestSuite.h>

class DisFileReader;

class DisFileReaderT : public CxxTest::TestSuite
{
public:
  DisFileReaderT();

  void setUp();
  void tearDown();
  void testCreateClass();
  void testReadDisFile();
  void testReadDisuFile();
  void testGetNumRow();
  void testGetNumCol();

  DisFileReader *m_p, *m_p1, *m_p2;
};

#endif

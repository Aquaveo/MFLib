//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef LISTREADERH5_T_H
#define LISTREADERH5_T_H

#include <cxxtest/TestSuite.h>
#include <private/util/util.h>

class ListReaderH5T : public CxxTest::TestSuite
{
public:
  void setUp();
  void testCreateClass();
  void testFillInKIJ();
  void testGetIface();
  void testGetCellGroup();
  void testGetFactor();
  void testGetCellGroup1();
  void testGetFactor1();
  void testGetFactor2();
  void testGetStressData();
  void testGetVersion();
  void testGetSeawatAuxH5Idx();

private:
  CStr m_file, m_file1, m_strParse, m_strParse1, m_strParse2, m_strParse3;
};

#endif

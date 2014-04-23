//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef MODFLOWLIB_T_H
#define MODFLOWLIB_T_H

#include <private/util/util.h>
#include <cxxtest/TestSuite.h>

class ModflowLibT : public CxxTest::TestSuite
{
public:
  void setUp();
  void tearDown();
  //void testProcessCmdLine();
  void test_imfLib_StripExtension();
  void test_imfLib_GetStr();
  void test_mfLib_U2DREL();
  void test_mfLib_U2DINT();
  void test_mfLib_ULSTRD();
  void test_mfLib_ReadSTR();

private:
  CStr m_file, m_file2;
};

#endif

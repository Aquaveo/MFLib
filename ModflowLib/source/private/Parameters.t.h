//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef PARAMETERS_T_H
#define PARAMETERS_T_H

#include <cxxtest/TestSuite.h>

class ParametersT : public CxxTest::TestSuite
{
public:
  void setUp();
  void tearDown();

  void testSetFileName();
  void testSetSenFileName();

  void testSubstituteList_WrongSizeArrays();
  void testSubstituteList_NoKeys();
  void testSubstituteList_Keys();
  void testSubstituteList_KeysFactors();

  void testSubstituteProperty_WrongSizeArrays();
  void testSubstituteProperty_NoKeys();
  void testSubstituteProperty_KeysFactors();

  void testSubstituteValue_NoParam();
  void testSubstituteValue_NoKey();
  void testSubstituteValue();

  void testSubstituteArray_NoParam();
  void testSubstituteArray_NoKey();
  void testSubstituteArray_NoPilot();
  void testSubstituteArray_NoPilot_WithMultArray();

  void testParTypeFromH5Path();
  void testCheckListSubstituteOk();
  void testCheckArraySubstituteOk();

  void testFillInParType();

private:
  CStr m_file, m_file1;
};

#endif

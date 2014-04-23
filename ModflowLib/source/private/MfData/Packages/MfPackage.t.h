//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef MFPACKAGE_T_H
#define MFPACKAGE_T_H

#include <cxxtest/TestSuite.h>

namespace MfData
{
  class MfPackage;
}

class MfPackageT : public CxxTest::TestSuite
{
public:
  void setUp();
  void tearDown();
  void testCopyConstructor();
  void testOperatorEqual();
  void testPackageName();
  void testSetField();
  void testGetField();
  void CheckAgainstMember(MfData::MfPackage *a_);
  //void testStoreArrayPtr();
  //void testGetArrayPtr();
  //void testStoreValue();
  //void testGetValue();
private:
  MfData::MfPackage *m_p;
  float m_f[3];
  int m_i[2];
};

#endif


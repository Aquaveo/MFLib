//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPBCF_T_H
#define NATIVEEXPBCF_T_H

#include <cxxtest/TestSuite.h>

namespace MfData { namespace Export { class NativeExpBcf; } }

class NativeExpBcfT : public CxxTest::TestSuite
{
public:
  void setUp();
  void tearDown();
  void testCreateClass();

  void testDesc();
  void testLine1();
  void testLine2();
  void testLine3();
  void testCanWriteLine();
  void testGetArrayName();
  void testLine4();
  void testLine5();
  void testLine6();
  void testLine7();
  void testLine8();
  void testLine9();

  MfData::Export::NativeExpBcf* m_p;
};
#endif

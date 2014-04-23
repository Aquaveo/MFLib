//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPDIS_T_H
#define NATIVEEXPDIS_T_H

#include <cxxtest/TestSuite.h>

namespace MfData { namespace Export { class NativeExpDis; } }

class NativeExpDisT : public CxxTest::TestSuite
{
public:
  void setUp();
  void tearDown();
  void testCreateClass();
  void testLine1();
  void testDesc();
  void testLine2();
  void testLine3();
  void testLine4();
  void testLine5();
  void testLine6();
  void testLine7();

  MfData::Export::NativeExpDis* m_p;
};
#endif

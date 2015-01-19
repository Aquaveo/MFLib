//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPDISU_T_H
#define NATIVEEXPDISU_T_H

#include <cxxtest/TestSuite.h>

namespace MfData { namespace Export { class NativeExpDisu; } }

class NativeExpDisuT : public CxxTest::TestSuite
{
public:
  void setUp();
  void tearDown();
  void testCreateClass();
  void testLine1();
  void testLine2();
  void testLine3();
  void testLine4();
  void testLine5();
  void testLine6();
  void testLine13();

  MfData::Export::NativeExpDisu* m_p;
};
#endif

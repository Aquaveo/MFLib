//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPPVAL_T_H
#define NATIVEEXPPVAL_T_H

#include <cxxtest/TestSuite.h>

namespace MfData { namespace Export { class NativeExpPval; } }

class NativeExpPvalT : public CxxTest::TestSuite
{
public:
  void setUp();
  void tearDown();
  void testCreateClass();
  void testLine1();
  void testDesc1();
  void testLine2();
  void testDesc2();

  MfData::Export::NativeExpPval* m_p;
};
#endif

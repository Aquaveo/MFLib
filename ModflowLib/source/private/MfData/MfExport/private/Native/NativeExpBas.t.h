//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPBAS_T_H
#define NATIVEEXPBAS_T_H

#include <cxxtest/TestSuite.h>

namespace MfData { namespace Export { class NativeExpBas; } }

class NativeExpBasT : public CxxTest::TestSuite
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

  MfData::Export::NativeExpBas* m_p;
};
#endif

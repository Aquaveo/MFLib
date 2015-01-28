//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPSWI_T_H
#define NATIVEEXPSWI_T_H

#include <cxxtest/TestSuite.h>

namespace MfData { namespace Export { class NativeExpSwi; } }

class NativeExpSwiT : public CxxTest::TestSuite
{
public:
  void setUp();
  void tearDown();
  void testCreateClass();

  MfData::Export::NativeExpSwi* m_p;
};
#endif

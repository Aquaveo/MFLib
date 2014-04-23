//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPMNW1_T_H
#define NATIVEEXPMNW1_T_H

#include <cxxtest/TestSuite.h>

namespace MfData { namespace Export { class NativeExpMnw1; } }

class NativeExpMnw1T : public CxxTest::TestSuite
{
public:
  void setUp();
  void tearDown();
  void testCreateClass();

  MfData::Export::NativeExpMnw1* m_p;
};
#endif

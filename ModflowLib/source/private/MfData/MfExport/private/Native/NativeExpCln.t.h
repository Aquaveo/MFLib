//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPCLN_T_H
#define NATIVEEXPCLN_T_H

#include <cxxtest/TestSuite.h>

namespace MfData { namespace Export { class NativeExpCln; } }

class NativeExpClnT : public CxxTest::TestSuite
{
public:
  void setUp();
  void tearDown();
  void testCreateClass();

  MfData::Export::NativeExpCln* m_p;
};
#endif

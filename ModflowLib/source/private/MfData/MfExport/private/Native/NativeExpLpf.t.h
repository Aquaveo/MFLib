//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPLPF_T_H
#define NATIVEEXPLPF_T_H

#include <cxxtest/TestSuite.h>

namespace MfData { namespace Export { class NativeExpLpf; } }

class NativeExpLpfT : public CxxTest::TestSuite
{
public:
  void setUp();
  void tearDown();
  void testCreateClass();

  void testDesc();
  void testLine1();
  void testLine2to6();
  void testCanWriteLine();
  void testLine7();

  MfData::Export::NativeExpLpf* m_p;
};
#endif

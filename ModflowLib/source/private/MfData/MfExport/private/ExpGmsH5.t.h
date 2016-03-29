//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef EXPGMSH5_T_H
#define EXPGMSH5_T_H

#include <cxxtest/TestSuite.h>

class ExpGmsH5T : public CxxTest::TestSuite
{
public:
  void testCreateClass();
  void testSupportedPackage();
  void testGetArrayMap();
  void testIsDataArray();
  void testCreateDefaultMfH5File();
};
#endif

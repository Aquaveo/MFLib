//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#pragma once
#include <cxxtest/TestSuite.h>

class LgrFileReaderT : public CxxTest::TestSuite
{
public:
  void testCreateClass();
  void testReadFile();
};

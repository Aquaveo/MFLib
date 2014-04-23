//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef SFR2READER_T_H
#define SFR2READER_T_H

#include <cxxtest/TestSuite.h>

class Sfr2ReaderT : public CxxTest::TestSuite
{
public:
  void testiGetFileFromLine();
  void testiGetReachData();
  void testGetSegData();
};

#endif

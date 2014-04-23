//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef LISTREADERPARSER_T_H
#define LISTREADERPARSER_T_H


#include <cxxtest/TestSuite.h>

class ListReaderParserT : public CxxTest::TestSuite
{
public:
  void testCreateClass();
  void testWrongString();
  void testCorrectString();
  void testAlmostCorrectString();
  void testGetFileName();
  void testGetPath();
  void testGetStressPeriod();
};

#endif


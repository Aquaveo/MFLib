//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef ARRAYREADERPARSER_T_H
#define ARRAYREADERPARSER_T_H


#include <cxxtest/TestSuite.h>

class ArrayReaderParserT : public CxxTest::TestSuite
{
public:
  void testCreateClass();
  void testWrongString();
  void testCorrectString();
  void testAlmostCorrectString();
  void testGetMultiplier();
  void testGetIPRN();
  void testGetFileName();
  void testGetPath();
  void testGetIndices();
  void testConstantValue();

private:
  bool StringIsInvalid(const char *a_);
};

#endif

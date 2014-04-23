//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef CONVERTER_T_H
#define CONVERTER_T_H
#include <cxxtest/TestSuite.h>

class ConverterT : public CxxTest::TestSuite
{
public:
  void testCreateClass();
  void testValidInput();
  void testDoNameFileConversion();
  void testPrintToOutput();
  void testSetUpOutput();
  void testCreateOutDir();
  void testDoLgrConversion();
};


#endif

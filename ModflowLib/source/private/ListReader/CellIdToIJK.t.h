//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef CELLIDTOIJK_T_H
#define CELLIDTOIJK_T_H


#include <cxxtest/TestSuite.h>

class CellIdToIJKT : public CxxTest::TestSuite
{
public:
  void testCreateClass();
  void test7X5X3Grid();
  void test28X33X6Grid();

};

#endif

//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef H5USELASTWRITER_T_H
#define H5USELASTWRITER_T_H

#include <cxxtest/TestSuite.h>

class H5UseLastWriterT : public CxxTest::TestSuite
{
public:
  void testCreateClass();
  void test_iGetAllArealUseLast();
  void test_iArealPropFromUseLast();
  void test_iEtSegFromUseLast();
  void test_iArealLayFromUseLast();
};
#endif

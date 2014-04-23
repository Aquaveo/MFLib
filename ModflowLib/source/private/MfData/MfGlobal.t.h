//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef MfGlobal_T_H
#define MfGlobal_T_H

#include <cxxtest/TestSuite.h>

namespace MfData
{
  class MfGlobal;
}

class MfGlobalT : public CxxTest::TestSuite
{
public:
  void setUp();
  void tearDown();
  void testCreateClass();
  void testNumRows();
  void testNumCols();
  void testNumLay();
  void testNumPeriods();
  void testLengthUnit();
  void testTimeUnit();
  void testAddPackage();
  void testGetPackage();
  void testInit_Get();
  void testAttachExporter();
  void testExport();

private:
  MfData::MfGlobal *m_p;
};

#endif


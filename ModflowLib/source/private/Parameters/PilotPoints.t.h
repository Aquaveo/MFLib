//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef PILOTPOINTS_T_H
#define PILOTPOINTS_T_H

#include <private\util\util.h>
#include <cxxtest/TestSuite.h>

class Param;

class PilotPointsT : public CxxTest::TestSuite
{
public:
  void setUp();
  void tearDown();
  void testCreateClass();
  void testReadWeights();
  void testReadIndices();
  void testReadStartVals();
  void testDoInterpolation();
  void testDoInterpolation2();
  void testDoInterpolationLog();

private:
  CStr m_file, m_f2;
  Param *m_par;
};
#endif


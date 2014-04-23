//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPSOLVER_T_H
#define NATIVEEXPSOLVER_T_H

#include <cxxtest/TestSuite.h>

namespace MfData { namespace Export { class NativeExpSolver; } }

class NativeExpSolverT : public CxxTest::TestSuite
{
public:
  void setUp();
  void tearDown();
  void testCreateClass();

  void testDesc();
  void testLine1_SIP();
  void testLine2_SIP();
  void testLine1_DE4();
  void testLine2_DE4();
  void testLine1_SOR();
  void testLine2_SOR();
  void testLine1_PCG();
  void testLine2_PCG();
  void testLine1_PCGN();
  void testLine2_PCGN();
  void testLine3_PCGN();
  void testLine4_PCGN();
  void testWriteLine3_LMG();
  void testLine1_LMG();
  void testLine2_LMG();
  void testLine3_LMG();
  void testLine4_LMG();
  void testLine1_GMG();
  void testLine2_GMG();
  void testLine3_GMG();
  void testLine4_GMG();
  void testLine1_NWT();
  void testWhichLine2_NWT();
  void testLine2a_NWT();
  void testLine2b_NWT();

  MfData::Export::NativeExpSolver* m_p;
};
#endif

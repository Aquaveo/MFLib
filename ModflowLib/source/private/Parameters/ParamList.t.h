//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef PARAMLIST_T_H
#define PARAMLIST_T_H

#include <cxxtest/TestSuite.h>

class ParamList;

class ParamListT : public CxxTest::TestSuite
{
public:
  void setUp();
  void tearDown();
  void testCreateClass();
  void testSize_PushBack_Clear();
  void testAt();
  void testFindByName();
  void testFindByKey();
  void testSetPilotPtVal();
  void testSetPPValsIsens();
  void testGetPilotPtValues();
  void testGetPilotPtIsens();
  void testUpdateParameter();
  void testIsPilotParName();
  void testUnusedParamKey();
  void testParmOfTypeExists();

private:
  ParamList *m_p;
};
#endif


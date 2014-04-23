//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef MFEXPORTUTIL_T_H
#define MFEXPORTUTIL_T_H

#include <cxxtest/TestSuite.h>

class MfExportUtilT : public CxxTest::TestSuite
{
public:
  void testGetExporter_WrongString();
};

#endif


//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef CMDLINE_T_H
#define CMDLINE_T_H

#include <cxxtest/TestSuite.h>

class CmdLineT : public CxxTest::TestSuite
{
public:
  void testProcessCmdLine();
};

#endif

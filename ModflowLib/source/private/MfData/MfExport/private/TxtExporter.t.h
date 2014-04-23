//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef TXTEXPORTER_T_H
#define TXTEXPORTER_T_H

#include <cxxtest/TestSuite.h>

class TxtExporterT : public CxxTest::TestSuite
{
public:
  void testCreateClass();
  void testGetStream();
  void testGetExtension();
  void testWriteLineToFile();
  void testWriteStringToFile();
  void testGetNewStream();
};
#endif

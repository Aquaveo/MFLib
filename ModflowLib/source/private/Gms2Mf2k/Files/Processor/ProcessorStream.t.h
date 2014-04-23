//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef PROCESSORSTREAM_T_H
#define PROCESSORSTREAM_T_H
#include <cxxtest/TestSuite.h>

class ProcessorStreamT : public CxxTest::TestSuite
{
public:
  void testCreateClass() const;
  void testDoConvertFile() const;
  void testReadStreamData() const;
  void testWriteStreamData() const;

};

#endif

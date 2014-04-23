//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef H5DATASETWRITER_T_H
#define H5DATASETWRITER_T_H

#include <cxxtest/TestSuite.h>

class H5DataSetWriterT : public CxxTest::TestSuite
{
public:
  void tearDown();
  void testCreateClass();
  void testOpenH5ForWriting();
  void testDatasetExists();
  void testOpenDatasetAndCreateIfNeeded();
  void testOpenDataset();
  void testCreateDataset();
  void testAllowTypeConversions();
  void testOpenDataspace();
  void testDoErrorChecks();
  void testDataClassTypeMatches();
  void testDataDimensionsMatch();
  void testSetDimInfoForWriting();
  void testCheckDimensionsAndExtendDataIfNeeded();
  void testCreateFilespaceAndMemoryspace();
  void testWriteDataT();
  void testCreateGroup();
  void testWriteAtt();
};

#endif

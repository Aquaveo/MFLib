//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef DATASETREADER_T_H
#define DATASETREADER_T_H

#include <cxxtest/TestSuite.h>
#include <private/util/util.h>

class H5DataSetReader;
class H5DataSetReaderImpl;

class H5DataSetReaderT : public CxxTest::TestSuite
{
public:
  void setUp();
  void tearDown();
  void testCreateClass();
  void testOpenFile_FileExists();
  void testOpenFile_FileDoesntExist();
  void testOpenDataSet_FileNotOpen();
  void testOpenDataSet_DSExists();
  void testOpenDataSet_DSDoesntExist();
  void testOpenDataSpace_DSNotOpen();
  void testOpenDataSpace_DSOpen();
  void testCheckDataType();
  void testCheckDimensions1D();
  void testCheckDimensions2D();
  void testCheckDimensions3D();
  void testCheckArraySize();
  void testSelectHyperslab();
  void testCreateMemorySpace();
  void testReadData1D();
  void testReadData2D();
  void testReadData3D();
  void testReadDataVecChar();
  void testGetAttFail();
  void testGetAttInt();
  void testGetDataVecDbl();
  void testGetIntFromDblDataSet();
  void testGetDataSetTypeSize();
  void testGetDataSetDimensions();
  void testDataSetExists();

  void testGet_H5T_class_t();
  void testGet_H5_nativetype();

private:
  void Create(const CStr &a_file,
              const CStr &a_path,
              const std::vector<std::pair<int, int>> &a_indices);
  void Create1(const CStr &a_file,
               const CStr &a_path,
               const std::vector<std::pair<int, int>> &a_indices);
  CStr m_file, m_file2;
  H5DataSetReaderImpl *m_p;
  H5DataSetReader *m_p1;
};

#endif

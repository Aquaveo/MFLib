//------------------------------------------------------------------------------
// FILE      H5DataSetReader.t.cpp
// PURPOSE   
// COPYRIGHT Brigham Young University, EMRL, 2006 All rights reserved.
//------------------------------------------------------------------------------
#ifdef CXX_TEST

#include <private\H5DataReader\H5DataSetReader.t.h>

#include <private\H5DataReader\H5DataSetReader.h>
#include <private\H5DataReader\H5DataReaderUtil.h>

//------------------------------------------------------------------------------
void H5DataSetReaderT::setUp ()
{
  H5Initialize::Init();
  char c[5000];
  GetModuleFileName(NULL, c, 5000);
  CStr str(c);
  // strip off the file name
  int pos(str.ReverseFind("\\"));
  m_exeName = str.Left(str.GetLength() - (str.GetLength() - pos));
  m_file.Format("%s\\testFiles\\HDF5_InputFiles\\input.h5", m_exeName.c_str());
  m_file2.Format("%s\\testFiles\\HDF5_InputFiles\\smallGrid_Trans.h5", m_exeName.c_str());
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::tearDown ()
{
  if (m_p)
    delete(m_p);
  m_p = NULL;
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::Create (const CStr &a_file,
                               const CStr &a_path,
                               const std::vector<std::pair<int, int>> &a_indices)
{
  m_p = new H5DataSetReader(a_file, a_path, a_indices);
  if (m_p)
    m_p->DontThrow();
} // H5DataSetReaderT::Create
//------------------------------------------------------------------------------
void H5DataSetReaderT::testCreateClass ()
{
  VEC_INT_PAIR indices;
  Create("file", "path", indices);

  TS_ASSERT(m_p);
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testOpenFile_FileExists ()
{
  VEC_INT_PAIR indices;
  Create(m_file, "path", indices);

  TS_ASSERT(m_p->OpenFile());
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testOpenFile_FileDoesntExist ()
{
  VEC_INT_PAIR indices;
  CStr file;
  file = "crap";
  Create(file, "path", indices);
  TS_ASSERT(!m_p->OpenFile());
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testOpenDataSet_FileNotOpen ()
{
  VEC_INT_PAIR indices;
  Create(m_file, "path", indices);

  TS_ASSERT(!m_p->OpenDataSet())
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testOpenDataSet_DSExists()
{
  VEC_INT_PAIR indices;
  Create(m_file, "1D_Data_Sets/DoubleDataSet", indices);
  TS_ASSERT(m_p->OpenFile());
  TS_ASSERT(m_p->OpenDataSet());
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testOpenDataSet_DSDoesntExist()
{
  VEC_INT_PAIR indices;
  Create(m_file, "stuff", indices);
  TS_ASSERT(m_p->OpenFile());
  TS_ASSERT(!m_p->OpenDataSet());
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testOpenDataSpace_DSNotOpen()
{
  VEC_INT_PAIR indices;
  Create(m_file, "1D_Data_Sets/DoubleDataSet", indices);
  TS_ASSERT(m_p->OpenFile());
  //TS_ASSERT(d.OpenDataSet());
  TS_ASSERT(!m_p->OpenDataSpace());
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testOpenDataSpace_DSOpen()
{
  VEC_INT_PAIR indices;
  Create(m_file, "1D_Data_Sets/DoubleDataSet", indices);
  TS_ASSERT(m_p->OpenFile());
  TS_ASSERT(m_p->OpenDataSet());
  TS_ASSERT(m_p->OpenDataSpace());
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testCheckDataType()
{
  VEC_INT_PAIR indices;
  Create(m_file, "1D_Data_Sets/DoubleDataSet", indices);
  TS_ASSERT(m_p->OpenFile());

  TS_ASSERT(m_p->CheckDataSetType(H5T_FLOAT, sizeof(double)));

  TS_ASSERT(!m_p->CheckDataSetType(H5T_FLOAT, sizeof(float)));
  TS_ASSERT(!m_p->CheckDataSetType(H5T_INTEGER, sizeof(double)));

  // these should all work with type conversions
  m_p->AllowTypeConversions(true);
  TS_ASSERT(m_p->CheckDataSetType(H5T_FLOAT, sizeof(double)));
  TS_ASSERT(m_p->CheckDataSetType(H5T_FLOAT, sizeof(float)));
  TS_ASSERT(m_p->CheckDataSetType(H5T_INTEGER, sizeof(int)));
  TS_ASSERT(m_p->CheckDataSetType(H5T_INTEGER, sizeof(char)));
  // these should now fail
  m_p->AllowTypeConversions(false);
  TS_ASSERT(!m_p->CheckDataSetType(H5T_FLOAT, sizeof(float)));
  TS_ASSERT(!m_p->CheckDataSetType(H5T_INTEGER, sizeof(int)));
  TS_ASSERT(!m_p->CheckDataSetType(H5T_INTEGER, sizeof(char)));
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testCheckDimensions1D ()
{
  std::pair<int, int> myPair;
  VEC_INT_PAIR indices(1, myPair);
  Create(m_file, "1D_Data_Sets/DoubleDataSet", indices);
  TS_ASSERT(m_p->OpenFile());
  TS_ASSERT(m_p->OpenDataSet());
  TS_ASSERT(m_p->OpenDataSpace());
  TS_ASSERT(m_p->CheckDataSetDimensions());
  // the number of dimensions will be wrong here
  delete(m_p);
  indices.clear();
  Create(m_file, "1D_Data_Sets/DoubleDataSet", indices);
  TS_ASSERT(m_p->OpenFile());
  TS_ASSERT(m_p->OpenDataSet());
  TS_ASSERT(m_p->OpenDataSpace());
  TS_ASSERT(!m_p->CheckDataSetDimensions());
  // we are trying to read outside the range here
  delete(m_p);
  myPair.first = 101;
  myPair.second = 27;
  indices.push_back(myPair);
  Create(m_file, "1D_Data_Sets/DoubleDataSet", indices);
  TS_ASSERT(m_p->OpenFile());
  TS_ASSERT(m_p->OpenDataSet());
  TS_ASSERT(m_p->OpenDataSpace());
  TS_ASSERT(!m_p->CheckDataSetDimensions());
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testCheckDimensions2D ()
{
  std::pair<int, int> myPair(1, 1);
  VEC_INT_PAIR indices(2, myPair);
  Create(m_file, "2D_Data_Sets/Double2X50", indices);
  TS_ASSERT(m_p->OpenFile());
  TS_ASSERT(m_p->OpenDataSet());
  TS_ASSERT(m_p->OpenDataSpace());
  TS_ASSERT(m_p->CheckDataSetDimensions());
  delete(m_p);
  indices.clear();
  Create(m_file, "2D_Data_Sets/Double2X50", indices);
  TS_ASSERT(m_p->OpenFile());
  TS_ASSERT(m_p->OpenDataSet());
  TS_ASSERT(m_p->OpenDataSpace());
  TS_ASSERT(!m_p->CheckDataSetDimensions());
  // we are trying to read outside the range here
  delete(m_p);
  myPair.first = 0;
  myPair.second = 3;
  indices.push_back(myPair);
  indices.push_back(myPair);
  Create(m_file, "2D_Data_Sets/Double2X50", indices);
  TS_ASSERT(m_p->OpenFile());
  TS_ASSERT(m_p->OpenDataSet());
  TS_ASSERT(m_p->OpenDataSpace());
  TS_ASSERT(!m_p->CheckDataSetDimensions());
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testCheckDimensions3D ()
{
  std::pair<int, int> myPair;
  myPair.first = 1;
  myPair.second = 1;
  VEC_INT_PAIR indices(3, myPair);
  Create(m_file, "3D_Data_Sets/Double3X10X30", indices);
  TS_ASSERT(m_p->OpenFile());
  TS_ASSERT(m_p->OpenDataSet());
  TS_ASSERT(m_p->OpenDataSpace());
  TS_ASSERT(m_p->CheckDataSetDimensions());
  delete(m_p);
  indices.clear();
  Create(m_file, "3D_Data_Sets/Double3X10X30", indices);
  TS_ASSERT(m_p->OpenFile());
  TS_ASSERT(m_p->OpenDataSet());
  TS_ASSERT(m_p->OpenDataSpace());
  TS_ASSERT(!m_p->CheckDataSetDimensions());
  // we are trying to read outside the range here
  delete(m_p);
  myPair.first = 0;
  myPair.second = 3;
  indices.push_back(myPair);
  indices.push_back(myPair);
  myPair.first = 25;
  myPair.second = 6;
  indices.push_back(myPair);
  Create(m_file, "3D_Data_Sets/Double3X10X30", indices);
  TS_ASSERT(m_p->OpenFile());
  TS_ASSERT(m_p->OpenDataSet());
  TS_ASSERT(m_p->OpenDataSpace());
  TS_ASSERT(!m_p->CheckDataSetDimensions());
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testCheckArraySize ()
{
  std::pair<int, int> myPair;
  myPair.second = 2;
  VEC_INT_PAIR indices(3, myPair);
  Create(m_file, "3D_Data_Sets/Double3X10X30", indices);

  TS_ASSERT(m_p->CheckArraySize(8));
  TS_ASSERT(!m_p->CheckArraySize(1));
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testSelectHyperslab ()
{
  std::pair<int, int> myPair;
  myPair.second = 4;
  VEC_INT_PAIR indices(3, myPair);
  Create(m_file, "3D_Data_Sets/Double3X10X30", indices);

  TS_ASSERT(!m_p->SelectHyperSlab());

  TS_ASSERT(m_p->OpenFile());
  TS_ASSERT(m_p->OpenDataSet());
  TS_ASSERT(m_p->OpenDataSpace());
  // we tried to select beyond the data bounds
  TS_ASSERT(!m_p->SelectHyperSlab());
  
  // this should be a valid select
  delete(m_p);
  indices.at(0).second = 3;
  Create(m_file, "3D_Data_Sets/Double3X10X30", indices);
  TS_ASSERT(m_p->OpenFile());
  TS_ASSERT(m_p->OpenDataSet());
  TS_ASSERT(m_p->OpenDataSpace());
  TS_ASSERT(m_p->SelectHyperSlab());
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testCreateMemorySpace ()
{
  VEC_INT_PAIR indices;
  Create(m_file, "3D_Data_Sets/Double3X10X30", indices);

  TS_ASSERT(!m_p->CreateMemorySpace(0));

  delete(m_p);
  std::pair<int, int> myPair;
  myPair.second = 3;
  indices.assign(3, myPair);
  Create(m_file, "3D_Data_Sets/Double3X10X30", indices);
  TS_ASSERT(m_p->CreateMemorySpace(27));
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testReadData1D ()
{
  std::pair<int, int> myPair(0, 127);
  VEC_INT_PAIR indices(1, myPair);
  Create(m_file, "1D_Data_Sets/DoubleDataSet", indices);
  TS_ASSERT(m_p->OpenFile());
  TS_ASSERT(m_p->OpenDataSet());
  TS_ASSERT(m_p->OpenDataSpace());
  TS_ASSERT(m_p->SelectHyperSlab());
  TS_ASSERT(m_p->CreateMemorySpace(127));
  std::vector<double> data(127, 0.0);
  TS_ASSERT(m_p->ReadDataT(&data[0], 127));
  TS_ASSERT_EQUALS(data.at(18), 19.0);
  TS_ASSERT_EQUALS(data.at(126), 127.0);
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testReadData2D ()
{
  std::pair<int, int> myPair(1, 1);
  VEC_INT_PAIR indices(2, myPair);
  indices.at(1).second = 49;
  Create(m_file, "2D_Data_Sets/Double2X50", indices);
  TS_ASSERT(m_p->OpenFile());
  TS_ASSERT(m_p->OpenDataSet());
  TS_ASSERT(m_p->OpenDataSpace());
  TS_ASSERT(m_p->SelectHyperSlab());
  TS_ASSERT(m_p->CreateMemorySpace(49));
  std::vector<double> data(49, 0.0);
  TS_ASSERT(m_p->ReadDataT(&data[0], 49));
  TS_ASSERT_EQUALS(data.at(17), 76.0);
  TS_ASSERT_EQUALS(data.at(48), 200.0);
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testReadData3D ()
{
  std::pair<int, int> myPair;
  myPair.first = 1;
  myPair.second = 1;
  VEC_INT_PAIR indices(3, myPair);
  indices.at(1).second = 9;
  Create(m_file, "3D_Data_Sets/Double3X10X30", indices);
  TS_ASSERT(m_p->OpenFile());
  TS_ASSERT(m_p->OpenDataSet());
  TS_ASSERT(m_p->OpenDataSpace());
  TS_ASSERT(m_p->SelectHyperSlab());
  TS_ASSERT(m_p->CreateMemorySpace(9));
  std::vector<double> data(9, 0.0);
  TS_ASSERT(m_p->ReadDataT(&data[0], 9));
  TS_ASSERT_EQUALS(data.at(2), 40.0);
  TS_ASSERT_EQUALS(data.at(8), 100.0);
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testReadDataVecChar ()
{
  VEC_INT_PAIR indices;
  Create(m_file2, "General Head/04. Map ID", indices);
  std::vector<char> vChar;
  m_p->GetAllData(vChar);
  TS_ASSERT_EQUALS((int)vChar.size(), (int)684);
  TS_ASSERT_EQUALS(vChar.at(0), 98);
  TS_ASSERT_EQUALS(vChar.at(1), 99);
  TS_ASSERT_EQUALS(vChar.at(257), 97);
  TS_ASSERT_EQUALS(vChar.at(682), 53);
  TS_ASSERT_EQUALS(vChar.at(683), 0);
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testGetAttFail ()
{
  VEC_INT_PAIR indices;
  Create(m_file2, "General Head/04. Map ID", indices);
  int maxStrLen;
  TS_ASSERT(!m_p->GetAtt("crap", maxStrLen));
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testGetAttInt ()
{
  VEC_INT_PAIR indices;
  Create(m_file2, "General Head/04. Map ID", indices);
  int maxStrLen;
  TS_ASSERT(m_p->GetAtt("Max. String Length", maxStrLen));
  TS_ASSERT_EQUALS(maxStrLen, 57);
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testGet_H5T_class_t ()
{
  char   c;
  int    i;
  float  f;
  double d;
  TS_ASSERT(H5DataReader::Get_H5T_class_t(&c) == H5T_INTEGER);
  TS_ASSERT(H5DataReader::Get_H5T_class_t(&i) == H5T_INTEGER);
  TS_ASSERT(H5DataReader::Get_H5T_class_t(&f) == H5T_FLOAT);
  TS_ASSERT(H5DataReader::Get_H5T_class_t(&d) == H5T_FLOAT);
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testGet_H5_nativetype ()
{
  char   c;
  int    i;
  float  f;
  double d;
  TS_ASSERT(H5DataReader::Get_H5_nativetype(&c) == H5T_NATIVE_CHAR);
  TS_ASSERT(H5DataReader::Get_H5_nativetype(&i) == H5T_NATIVE_INT);
  TS_ASSERT(H5DataReader::Get_H5_nativetype(&f) == H5T_NATIVE_FLOAT);
  TS_ASSERT(H5DataReader::Get_H5_nativetype(&d) == H5T_NATIVE_DOUBLE);
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testGetDataVecDbl ()
{
  std::pair<int, int> myPair;
  myPair.first = 1;
  myPair.second = 1;
  VEC_INT_PAIR indices(3, myPair);
  indices.at(1).second = 9;
  Create(m_file, "3D_Data_Sets/Double3X10X30", indices);
  TS_ASSERT(m_p->OpenFile());
  TS_ASSERT(m_p->OpenDataSet());
  TS_ASSERT(m_p->OpenDataSpace());
  TS_ASSERT(m_p->SelectHyperSlab());
  TS_ASSERT(m_p->CreateMemorySpace(9));
  std::vector<double> data;
  TS_ASSERT(m_p->GetData(data));
  TS_ASSERT(data.size() == 9);
  TS_ASSERT_EQUALS(data.at(2), 40.0);
  TS_ASSERT_EQUALS(data.at(8), 100.0);
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testGetIntFromDblDataSet ()
{
  std::pair<int, int> myPair;
  myPair.first = 1;
  myPair.second = 1;
  VEC_INT_PAIR indices(3, myPair);
  indices.at(1).second = 9;
  std::vector<int> data;
  Create(m_file, "3D_Data_Sets/Double3X10X30", indices);
  TS_ASSERT(!m_p->GetData(data));
  m_p->AllowTypeConversions(true);
  TS_ASSERT(m_p->GetData(data));
  TS_ASSERT(data.size() == 9);
  TS_ASSERT_EQUALS(data.at(2), 40);
  TS_ASSERT_EQUALS(data.at(8), 100);
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testGetDataSetTypeSize ()
{
  VEC_INT_PAIR indices;
  Create(m_file2, "General Head/04. Map ID", indices);
  TS_ASSERT(m_p->GetDataSetTypeSize() == sizeof(char));
  delete(m_p); m_p = NULL;
  Create(m_file, "1D_Data_Sets/DoubleDataSet", indices);
  TS_ASSERT(m_p->GetDataSetTypeSize() == sizeof(double));
  delete(m_p); m_p = NULL;
  Create(m_file, "1D_Data_Sets/FloatDataSet", indices);
  TS_ASSERT(m_p->GetDataSetTypeSize() == sizeof(float));
  delete(m_p); m_p = NULL;
  Create(m_file, "1D_Data_Sets/IntDataSet", indices);
  TS_ASSERT(m_p->GetDataSetTypeSize() == sizeof(int));
  delete(m_p); m_p = NULL;
}
//------------------------------------------------------------------------------
void H5DataSetReaderT::testGetDataSetDimensions ()
{
  VEC_INT_PAIR indices;
  CStr file("crap");
  std::vector<hsize_t> sizes;
  Create(file, "path", indices);
  TS_ASSERT(!m_p->GetDataSetDimensions(sizes));

  Create(m_file2, "River/07. Property", indices);
  TS_ASSERT(m_p->GetDataSetDimensions(sizes));
  TS_ASSERT(sizes.size() == 3);
  TS_ASSERT(sizes.at(0) == 3);
  TS_ASSERT(sizes.at(1) == 3);
  TS_ASSERT(sizes.at(2) == 3);
}
#endif // CXX_TEST


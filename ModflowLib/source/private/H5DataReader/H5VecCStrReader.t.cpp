//------------------------------------------------------------------------------
// FILE      H5VecCStrReader.t.cpp
// PURPOSE   
// COPYRIGHT Aquaveo 2006 All rights reserved.
//------------------------------------------------------------------------------
#ifdef CXX_TEST

#include <private/H5DataReader/H5VecCStrReader.t.h>

#include <private/H5DataReader/H5VecCStrReader.h>

//------------------------------------------------------------------------------
void H5VecCStrReaderT::setUp ()
{
  H5Initialize::Init();
  char c[5000];
  GetModuleFileName(NULL, c, 5000);
  CStr str(c);
  // strip off the file name
  int pos(str.ReverseFind("\\"));
  m_exeName = str.Left(str.GetLength() - (str.GetLength() - pos));
  m_file.Format("%s\\testFiles\\HDF5_InputFiles\\smallGrid_Trans.h5", m_exeName.c_str());
}
//------------------------------------------------------------------------------
void H5VecCStrReaderT::testCreateClass ()
{
  CStr file, path;
  H5VecCStrReader *p = new H5VecCStrReader(file, path);
  TS_ASSERT(p);
  if (p)
    delete(p);
}
//------------------------------------------------------------------------------
void H5VecCStrReaderT::testFillInStrings ()
{
  CStr path("General Head/04. Map ID");
  H5VecCStrReader r(m_file, path);
  std::vector<CStr> vStr;
  r.FillInStrings(vStr);
  TS_ASSERT(!vStr.empty());
  TS_ASSERT_EQUALS((int)vStr.size(), 12);
  TS_ASSERT(vStr.at(0) == "bcadec99-ae20-4ba3-b9ef-b578cac035a0 ARC 1 44.995 30.275");
  TS_ASSERT(vStr.at(11) == "bcadec99-ae20-4ba3-b9ef-b578cac035a0 ARC 1 44.995 30.275");
}

#endif

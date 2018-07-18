//------------------------------------------------------------------------------
// FILE      H5VecCStrReader.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------

#include <private/H5DataReader/H5VecCStrReader.h>

#include <private/H5DataReader/H5DataSetReader.h>

//------------------------------------------------------------------------------
/// \brief Constructor.
//------------------------------------------------------------------------------
H5VecCStrReader::H5VecCStrReader (const CStr &a_file,
                                  const CStr &a_path) :
m_file(a_file),
m_path(a_path)
{
} // H5VecCStrReader::H5VecCStrReader
//------------------------------------------------------------------------------
/// \brief Fills in a vector of CStrs from a char data set in an HDF5 file
//------------------------------------------------------------------------------
bool H5VecCStrReader::FillInStrings (std::vector<CStr> &a_) const
{
  a_.resize(0);

  VEC_INT_PAIR myV;
  H5DataSetReader r(m_file, m_path, myV);

  std::vector<char> vChar;

  if (!r.GetAllData(vChar) || vChar.empty())
    return false;

  int maxStrLen;
  if (!r.GetAtt("Max. String Length", maxStrLen) || maxStrLen < 1)
    return false;

  int i;
  const int num(static_cast<int>(vChar.size()) / maxStrLen);
  try
  {
    for (i=0; i<num; i++)
      a_.push_back(&vChar.at(static_cast<size_t>(i*maxStrLen)));
  }
  catch (std::out_of_range&)
  {
    a_.resize(0);
    return false;
  }

  return true;
} // H5VecCStrReader::FillInStrings

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private/H5DataReader/H5VecCStrReader.t.h>

//------------------------------------------------------------------------------
void H5VecCStrReaderT::setUp ()
{
  H5Initialize::Init();
  m_file.Format("%s\\HDF5_InputFiles\\smallGrid_Trans.h5",
                util::GetTestFilesDirectory().c_str());
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

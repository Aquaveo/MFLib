//------------------------------------------------------------------------------
// FILE      MultArray.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------

#include <private/Parameters/MultArray.h>

#include <private/H5DataReader/H5DataSetReader.h>

class MultArray::impl
{
public:
  impl(const char* a_fName,
       const char* a_parName) :
    m_fName(a_fName),
    m_parName(a_parName) {}

  bool GetArray(std::vector<Real> &a_);

  CStr m_fName, m_parName;
};


//------------------------------------------------------------------------------
/// /brief Constructor
//------------------------------------------------------------------------------
MultArray::MultArray (const char* a_fName,
                      const char* a_parName) :
m_p(new MultArray::impl(a_fName, a_parName))
{
} // MultArray::MultArray
//------------------------------------------------------------------------------
/// /brief Destructor
//------------------------------------------------------------------------------
MultArray::~MultArray ()
{
  if (m_p)
    delete(m_p);
} // MultArray::~MultArray
//------------------------------------------------------------------------------
/// /brief Fills in the passed in vector with the multiplier associated
/// with the parameter name that was passed to the constructor
//------------------------------------------------------------------------------
bool MultArray::GetArray (std::vector<Real> &a_)
{
  return(m_p->GetArray(a_));
} // MultArray::GetArray

bool MultArray::impl::GetArray (std::vector<Real> &a_)
{
  CStr path, pname(m_parName);
  pname.ToLower();
  path.Format("Arrays/Param_%s", pname);
  H5DataSetReader r(m_fName, path);
  r.AllowTypeConversions(true);
  return(r.GetAllData(a_));
} // MultArray::impl::GetArray
///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private/Parameters/MultArray.t.h>

//------------------------------------------------------------------------------
void MultArrayT::testCreateClass ()
{
  MultArray *p = new MultArray("file", "param");
  TS_ASSERT(p);
  if (p)
    delete(p);
}
//------------------------------------------------------------------------------
void MultArrayT::testGetArray ()
{
  CStr file;
  file = util::GetTestFilesDirectory() + "\\HDF5_InputFiles\\pest.h5";

  MultArray m(file, "RCH_150");
  std::vector<Real> vFlt;
  TS_ASSERT(m.GetArray(vFlt));
  TS_ASSERT(vFlt.size() == 2940);
  for (size_t i=0; i<vFlt.size(); i++)
    TS_ASSERT_DELTA((Real)1.1, vFlt.at(i), CXXDELTA);
}

#endif


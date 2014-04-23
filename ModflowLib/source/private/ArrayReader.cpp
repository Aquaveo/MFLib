//------------------------------------------------------------------------------
// FILE      ArrayReader.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
//#pragma warning (disable:4996)

#include <private\ArrayReader.h>

#include <private\ArrayReader\ArrayReaderParser.h>
#include <private\H5DataReader\H5DataSetReader.h>
#include <private\Parameters.h>

////////////////////////////////////////////////////////////////////////////////
/// \class ArrayReader::impl
/// \brief The implementation of the ArrayReader class.
////////////////////////////////////////////////////////////////////////////////
class ArrayReader::impl
{
public:
  impl(const CStr &a_);

  // data
  ArrayReaderParser m_parser;
};

//------------------------------------------------------------------------------
/// \brief Constructor.
//------------------------------------------------------------------------------
ArrayReader::ArrayReader (const CStr &a_) :
m_impl (new ArrayReader::impl(a_))
{
} // ArrayReader::ArrayReader 
//------------------------------------------------------------------------------
/// \brief Destructor.
//------------------------------------------------------------------------------
ArrayReader::~ArrayReader ()
{
  if (m_impl)
    delete(m_impl);
} // ArrayReader::~ArrayReader
//------------------------------------------------------------------------------
/// \brief Tells if the string input was formated correctly.
//------------------------------------------------------------------------------
bool ArrayReader::ValidInputString () const
{
  return(m_impl->m_parser.ValidInputString());
} // ArrayReader::ValidInputString
//------------------------------------------------------------------------------
/// \brief Get the IPRN value
//------------------------------------------------------------------------------
int ArrayReader::GetIPRN () const
{
  return(m_impl->m_parser.GetIPRN());
} // ArrayReader::GetIPRN
//------------------------------------------------------------------------------
/// \brief Get the multiplier value
//------------------------------------------------------------------------------
double ArrayReader::GetMultiplier () const
{
  return(m_impl->m_parser.GetMultiplier());
} // ArrayReader::GetIPRN
//------------------------------------------------------------------------------
/// \brief Get the array
//------------------------------------------------------------------------------
void ArrayReader::GetData (double *a_arr,
                           size_t a_size,
                           const CStr& a_name) const
{
  GetDataT(a_arr, a_size, a_name);
} // ArrayReader::GetData
//------------------------------------------------------------------------------
/// \brief Get the array
//------------------------------------------------------------------------------
void ArrayReader::GetData (float *a_arr,
                           size_t a_size,
                           const CStr& a_name) const
{
  GetDataT(a_arr, a_size, a_name);
} // ArrayReader::GetData
//------------------------------------------------------------------------------
/// \brief Get the array
//------------------------------------------------------------------------------
void ArrayReader::GetData (int *a_arr,
                           size_t a_size,
                           const CStr& a_name) const
{
  GetDataT(a_arr, a_size, a_name);
} // ArrayReader::GetData
//------------------------------------------------------------------------------
/// \brief Get the array
//------------------------------------------------------------------------------
template <class T>
void ArrayReader::GetDataT (T *a_arr,
                            size_t a_size,
                            const CStr& a_name) const
{
  if (m_impl->m_parser.ConstantValue())
  {
    double val = m_impl->m_parser.GetConstValue();
    for (size_t i=0; i<a_size; i++)
      a_arr[i] = (T)val;
  }
  else
  {
    H5DataSetReader reader(m_impl->m_parser.GetFileName(),
                           m_impl->m_parser.GetPath(),
                           m_impl->m_parser.GetIndices());

    reader.AllowTypeConversions(true);
    reader.GetData(a_arr, a_size);
  }

  // get the multiplier
  double mult = m_impl->m_parser.GetMultiplier();
  if (Parameters::CheckArraySubstituteOk(m_impl->m_parser.GetPath()))
  {
    // do parameter substitution
    Parameters::SubstituteArray(a_arr, a_size, a_name);
    Parameters::SubstituteValue(&mult);
  }

  // From page 86 of the mf2k documentation. If the multiplier is 0 then it
  // is changed to 1 by modflow.
  if (mult == 0.0)
    mult = 1.0;
  if (mult != 1.0)
  {
    for (size_t i=0; i<a_size; i++)
    {
      a_arr[i] *= (T)mult;
    }
  }
#if _DEBUG
  bool flag(0);
  for (size_t j=0; flag && j<a_size; j++)
  {
    if (a_arr[j] == 1)
      a_arr[j] = 0;
  }
#endif

} // ArrayReader::GetDataT

//------------------------------------------------------------------------------
/// \brief Constructor.
//------------------------------------------------------------------------------
ArrayReader::impl::impl (const CStr &a_) :
m_parser(a_)
{
}

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\ArrayReader.t.h>

//------------------------------------------------------------------------------
void ArrayReaderT::testCreateClass ()
{
  ArrayReader *tmp = new ArrayReader("this is a test");

  TS_ASSERT(tmp);
  if (tmp)
    delete(tmp);
}

#endif // CXX_TEST


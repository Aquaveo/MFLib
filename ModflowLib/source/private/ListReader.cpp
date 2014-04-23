//------------------------------------------------------------------------------
// FILE      ListReader.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
//#pragma warning (disable:4996)

#include <private\ListReader.h>

#include <private\ListReader\ListReaderH5.h>
#include <private\ListReader\ListReaderParser.h>

///////////////////////////////////////////////////////////////////////////////
// ListReader::impl
///////////////////////////////////////////////////////////////////////////////
class ListReader::impl
{
public:
  impl(ListReaderSetUp &a_);

  ListReaderSetUp  m_setup;
  ListReaderParser m_parser;
};


//------------------------------------------------------------------------------
/// \brief Constructor.
//------------------------------------------------------------------------------
ListReader::ListReader (ListReaderSetUp &a_) :
m_impl(new ListReader::impl(a_))
{
} // ListReader::ListReader
//------------------------------------------------------------------------------
/// \brief Tells if the input string was formatted correctly
//------------------------------------------------------------------------------
bool ListReader::ValidSetUp () const
{
  if (!m_impl->m_parser.ValidInputString())
    return false;
  return true;
} // ListReader::ValidSetUp
//------------------------------------------------------------------------------
/// \brief Fills in the data for the passed in array
//------------------------------------------------------------------------------
bool ListReader::GetData (std::vector<double> &a_data) const
{
  int mySize(m_impl->m_setup.m_nRows*m_impl->m_setup.m_nFields);
  a_data.assign(mySize, 0.0);
  return (GetData(&a_data[0]));
} // ListReader::GetData
//------------------------------------------------------------------------------
/// \brief Fills in the data for the passed in array
//------------------------------------------------------------------------------
bool ListReader::GetData (double *a_data) const
{
  ListReaderH5 r(m_impl->m_parser, m_impl->m_setup);
  return (r.FillInData(a_data));
} // ListReader::GetData
//------------------------------------------------------------------------------
/// \brief Fills in the data for the passed in array
//------------------------------------------------------------------------------
bool ListReader::GetData (std::vector<float> &a_data) const
{
  int mySize(m_impl->m_setup.m_nRows*m_impl->m_setup.m_nFields);
  a_data.assign(mySize, 0.0);
  return (GetData(&a_data[0]));
} // ListReader::GetData
//------------------------------------------------------------------------------
/// \brief Fills in the data for the passed in array
//------------------------------------------------------------------------------
bool ListReader::GetData (float *a_data) const
{
  ListReaderH5 r(m_impl->m_parser, m_impl->m_setup);
  return (r.FillInData(a_data));
} // ListReader::GetData
//------------------------------------------------------------------------------
/// \brief Returns a const refernce to the ListReaderSetUp class.
//------------------------------------------------------------------------------
const ListReaderSetUp& ListReader::SetUp () const
{
  return(m_impl->m_setup);
} // ListReader::SetUp
//------------------------------------------------------------------------------
/// \brief Returns a const refernce to the ListReaderParser class.
//------------------------------------------------------------------------------
const ListReaderParser& ListReader::Parser () const
{
  return(m_impl->m_parser);
} // ListReader::SetUp
//------------------------------------------------------------------------------
/// \brief Constructor.
//------------------------------------------------------------------------------
ListReader::impl::impl (ListReaderSetUp &a_) :
m_setup(a_),
m_parser(a_.m_line)
{
} // ListReader::impl::impl


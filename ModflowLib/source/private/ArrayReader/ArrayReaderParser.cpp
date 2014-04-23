//------------------------------------------------------------------------------
// FILE      ArrayReaderParser.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------

#include <private\ArrayReader\ArrayReaderParser.h>

#include <private\util\util.h>

//------------------------------------------------------------------------------
/// \brief Constructor.
//------------------------------------------------------------------------------
ArrayReaderParser::ArrayReaderParser (const CStr &a_) :
m_str(a_),
m_valid(false),
m_const(false),
m_mult(0.0),
m_IPRN(0)
{
  m_str.TrimLeft();
  m_str.TrimRight();
  if (!m_str.IsEmpty())
  {
    ParseString();
  }
}
//------------------------------------------------------------------------------
/// \brief Gets the size necessary to read into a 1-dimensional array
//------------------------------------------------------------------------------
int ArrayReaderParser::GetArraySize () const
{
  int size = 1;
  if (ValidInputString())
  {
    VEC_INT_PAIR indices = GetIndices();
    for (size_t i = 0; i < indices.size(); ++i)
    {
      size *= indices[i].second;
    }
  }
  
  return size;
} // ArrayReaderParser::GetArraySize
//------------------------------------------------------------------------------
/// \brief Parses the string that was passed in
//------------------------------------------------------------------------------
void ArrayReaderParser::ParseString ()
{
  CStr msg("Incorrectly formated string in U2DREL");
  CToken t1(m_str, "\"");;
  CStr   str;

  try 
  {
    m_valid = true;
    str = t1.GetNextToken();
    {
      CToken token(str, " ");
      str = token.GetNextToken();
      if (str.CompareNoCase("HDF5") != 0)
        throw EException(msg);

      str = token.GetNextToken();
      // see if the string is "CONSTANT"
      if (str.CompareNoCase("constant") == 0)
      {
        m_const = true;
        str = token.GetNextToken();
      }

      // read the multiplier
      if(sscanf(str.c_str(), "%lf", &m_mult) != 1)
        throw EException(msg);

      str = token.GetNextToken(); // read the IPRN flag
      if (sscanf(str.c_str(), "%d", &m_IPRN) != 1)
        throw EException(msg);
    }
    if (m_const)
      return;

    m_file = t1.GetNextToken(); // read the filename
    if (m_file.IsEmpty())
      throw EException(msg);

    t1.GetNextToken();
    m_path = t1.GetNextToken(); // read the path
    if (m_path.IsEmpty())
      throw EException(msg);

    str = t1.GetNextToken();
    {
      CToken token(str, " ");
      str = token.GetNextToken(); // read the number of dimensions
      if ("1" != str && "2" != str && "3" != str)
        throw EException(msg);
      unsigned int nDim(0);
      if (sscanf(str.c_str(), "%d", &nDim) != 1)
        throw EException(msg);

      // read the indices so we know how to read the array
      std::pair<int, int> myPair;
      for (unsigned int i=0; i<nDim; i++)
      {
        myPair.first = myPair.second = 0;
        str = token.GetNextToken();
        if (sscanf(str.c_str(), "%d", &myPair.first) != 1)
          throw EException(msg);
        str = token.GetNextToken();
        if (sscanf(str.c_str(), "%d", &myPair.second) != 1)
          throw EException(msg);
        m_indices.push_back(myPair);
      }

      if (nDim != m_indices.size())
        throw EException(msg);
    }
  }
  catch (EException &e)
  {
    CStr out(e.what());
    if (!out.IsEmpty())
    {
      ErrorStack::Get().PutError(out);
    }
    m_valid = false;
  }
} // ArrayReaderParser::ParseString



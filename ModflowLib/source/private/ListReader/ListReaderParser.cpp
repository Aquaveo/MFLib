//------------------------------------------------------------------------------
// FILE      ListReaderParser.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------

#include <private\ListReader\ListReaderParser.h>

//------------------------------------------------------------------------------
/// \brief Constructor.
//------------------------------------------------------------------------------
ListReaderParser::ListReaderParser (const CStr &a_) :
m_valid(false),
m_str(a_),
m_stressPeriod(0)
{
  m_str.TrimRight();
  m_str.TrimLeft();
  if (!m_str.IsEmpty())
  {
    ParseString();
  }
} // ListReaderParser::ListReaderParser
//------------------------------------------------------------------------------
/// \brief Parses the string that was passed in.
//------------------------------------------------------------------------------
void ListReaderParser::ParseString ()
{
  CStr msg("Incorrectly formated string in ULSTRD");
  CToken t1(m_str, "\"");;
  CStr   str;

  try
  {
    m_valid = true;
    str = t1.GetNextToken();
    {
      CToken token(str, " ");
      str = token.GetNextToken();
      if (str.CompareNoCase("GMS_HDF5_01") != 0)
        throw EException(msg);
    }

    m_file = t1.GetNextToken();
    if (m_file.IsEmpty())
      throw EException(msg);

    t1.GetNextToken();
    m_path = t1.GetNextToken();
    if (m_path.IsEmpty())
      throw EException(msg);

    str = t1.GetNextToken();
    {
      CToken token(str, " ");
      str = token.GetNextToken(); // read the stressperiod
      if (sscanf(str.c_str(), "%d", &m_stressPeriod) != 1)
        throw EException(msg);
      if (m_stressPeriod < 1)
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
} // ListReaderParser::ParseString

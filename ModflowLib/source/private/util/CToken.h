//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef CTOKEN_H
#define CTOKEN_H

#include <private\util\StdString.h>

////////////////////////////////////////////////////////////////////////////////
/// \class CToken
/// \brief This class is used to tokenize a string
////////////////////////////////////////////////////////////////////////////////
class CToken
{
public:
  virtual CStr GetNextToken(); // Returns the next token
  virtual bool MoreTokens(); // Do we have any more tokens to retrieve
  void SetToken(const CStr &token) { m_strToken = token; }
  int m_nAmount;
  int m_nCurrent;
  CToken(CStr str,CStr token=" ");
  virtual ~CToken();

protected:
  CStr m_strToToken; // The original string to tokenize
  CStr m_strLeft; // The string we have left after taking tokens out
  CStr m_strToken; // What is the value to tokenize on?
private:
  int GetAmount(CStr str);
};

#endif

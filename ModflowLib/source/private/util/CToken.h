// Token.h: interface for the CToken class.
//
//
//  MFC Simple CString Tokenizer (Version 1)
//
//  Written by Richard Case (case@dcs.kcl.ac.uk)
//  Copyright (c) 1999
//
// This code may be used in compiled form in any way you desire. This
// file may be redistributed unmodified by any means PROVIDING it is 
// not sold for profit without the authors written consent, and 
// providing that this notice and the authors name and all copyright 
// notices remains intact. 
//
// This file is provided "as is" with no expressed or implied warranty.
// The author accepts no liability for any damage/loss of business that
// this product may cause.
//
// Expect bugs!
// 
// Please use and enjoy. Please let me know of any bugs/mods/improvements 
// that you have found/implemented and I will fix/incorporate them into this
// file. 
//
//
//////////////////////////////////////////////////////////////////////

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

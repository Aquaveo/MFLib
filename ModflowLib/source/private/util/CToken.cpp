//------------------------------------------------------------------------------
// FILE      CToken.cpp
// PURPOSE   
// Token.cpp: implementation of the CToken class.
//
//	MFC Simple CString Tokenizer (Version 1)
//	
//	Written by Richard Case (case@dcs.kcl.ac.uk)
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
#include <private\util\CToken.h>

//-----------------------------------------------------------------------------
// NOTES:
//-----------------------------------------------------------------------------
CToken::CToken (CStr str, CStr token)
{
  str.TrimLeft();
  str.TrimRight();
  m_strToToken = str;
  m_strLeft = str;
  m_strToken = token;
  m_nAmount = GetAmount(str);
  m_nCurrent = 0;
} // CToken::CToken
//-----------------------------------------------------------------------------
// NOTES:
//-----------------------------------------------------------------------------
CToken::~CToken ()
{
} // CToken::~CToken
//-----------------------------------------------------------------------------
// NOTES:
//-----------------------------------------------------------------------------
int CToken::GetAmount (CStr str)
{
  int count = 0;

  int pos, len;

  len = str.GetLength();
  pos = str.Find(m_strToken,0);
  while (pos != -1)
  {
    count++;
    pos++;
    str = str.Right(len - pos);
    str.TrimLeft();
    len = str.GetLength();
    pos = str.Find(m_strToken,0);
  }
  if (!str.IsEmpty()) count++;
  return count;
} // CToken::GetAmount
//-----------------------------------------------------------------------------
// NOTES:
//-----------------------------------------------------------------------------
bool CToken::MoreTokens ()
{
  if (m_nCurrent < m_nAmount)
  return TRUE;
  else return FALSE;
} // CToken::MoreTokens
//-----------------------------------------------------------------------------
// NOTES:
//-----------------------------------------------------------------------------
CStr CToken::GetNextToken ()
{

  if(!MoreTokens())
  {
    CStr str(m_strLeft);
    m_strLeft = "";
    return str;
  }

  CStr ret;
  int pos, len;

  len = m_strLeft.GetLength();
  pos = m_strLeft.Find(m_strToken,0);
  if (pos != -1)
  {
    ret = m_strLeft.Left(pos);
    pos++;
    m_strLeft = m_strLeft.Right(len - pos);
    m_strLeft.TrimLeft();
  }
  else ret = m_strLeft;
  m_nCurrent++;
  return ret;
} // CToken::GetNextToken

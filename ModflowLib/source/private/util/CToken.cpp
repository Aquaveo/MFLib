//------------------------------------------------------------------------------
// FILE      CToken.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
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

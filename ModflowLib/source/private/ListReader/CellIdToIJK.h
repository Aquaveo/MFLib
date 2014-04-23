//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef CELLIDTOIJK_H
#define CELLIDTOIJK_H

class CellIdToIJK
{
public:
  CellIdToIJK(int a_i, int a_j) : m_i(a_i), m_j(a_j) {}

  inline int KFromId(int a_) { return ((a_ - 1) / (m_i * m_j) + 1); }
  inline int IFromId(int a_) { return (((a_ - 1) / m_j) % m_i + 1); }
  inline int JFromId(int a_) { return ((a_ - 1) % m_j + 1); }
  inline int IdFromIJK(int a_i, int a_j, int a_k)
  {return (((a_k-1) * (m_i*m_j)) + (a_i-1)*m_j + a_j);}

private:
  CellIdToIJK(const CellIdToIJK &rhs);
  const CellIdToIJK& operator=(const CellIdToIJK &rhs);

  int m_i, m_j;
};

#endif


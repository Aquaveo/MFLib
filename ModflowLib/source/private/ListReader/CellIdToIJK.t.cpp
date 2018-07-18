//------------------------------------------------------------------------------
// FILE      CellIdToIJK.t.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#ifdef CXX_TEST

#include <private/ListReader/CellIdToIJK.t.h>

#include <private/ListReader/CellIdToIJK.h>

//------------------------------------------------------------------------------
void CellIdToIJKT::testCreateClass ()
{
  CellIdToIJK *p = new CellIdToIJK(3, 2);
  TS_ASSERT(p);
  if (p)
    delete(p);
}
//------------------------------------------------------------------------------
void CellIdToIJKT::test7X5X3Grid ()
{
  CellIdToIJK c(7, 5);

  TS_ASSERT_EQUALS(c.IFromId(23), 5);
  TS_ASSERT_EQUALS(c.JFromId(23), 3);
  TS_ASSERT_EQUALS(c.KFromId(23), 1);

  TS_ASSERT_EQUALS(c.IFromId(61), 6);
  TS_ASSERT_EQUALS(c.JFromId(61), 1);
  TS_ASSERT_EQUALS(c.KFromId(61), 2);

  TS_ASSERT_EQUALS(c.IFromId(80), 2);
  TS_ASSERT_EQUALS(c.JFromId(80), 5);
  TS_ASSERT_EQUALS(c.KFromId(80), 3);
}
//------------------------------------------------------------------------------
void CellIdToIJKT::test28X33X6Grid ()
{
  CellIdToIJK c(33, 28);

  TS_ASSERT_EQUALS(c.IFromId(837), 30);
  TS_ASSERT_EQUALS(c.JFromId(837), 25);
  TS_ASSERT_EQUALS(c.KFromId(837), 1);

  TS_ASSERT_EQUALS(c.IFromId(2338), 18);
  TS_ASSERT_EQUALS(c.JFromId(2338), 14);
  TS_ASSERT_EQUALS(c.KFromId(2338), 3);

  TS_ASSERT_EQUALS(c.IFromId(3915), 8);
  TS_ASSERT_EQUALS(c.JFromId(3915), 23);
  TS_ASSERT_EQUALS(c.KFromId(3915), 5);
}
#endif

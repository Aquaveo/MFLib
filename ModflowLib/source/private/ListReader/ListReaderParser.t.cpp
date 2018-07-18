//------------------------------------------------------------------------------
// FILE      ListReaderParser.t.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#ifdef CXX_TEST

#include <private/ListReader/ListReaderParser.t.h>

#include <private/ListReader/ListReaderParser.h>

//------------------------------------------------------------------------------
void ListReaderParserT::testCreateClass ()
{
  ListReaderParser *p = new ListReaderParser("stuff");
  TS_ASSERT(p);
  if (p)
    delete(p);
}
//------------------------------------------------------------------------------
void ListReaderParserT::testWrongString ()
{
  ListReaderParser p("crap");
  TS_ASSERT(!p.ValidInputString());
}
//------------------------------------------------------------------------------
void ListReaderParserT::testCorrectString()
{
  CStr line;
  line = "GMS_HDF5_01 \"input.h5\" \"Drain with spaces\" 3";
  ListReaderParser p(line);
  TS_ASSERT(p.ValidInputString());
}
//------------------------------------------------------------------------------
void ListReaderParserT::testAlmostCorrectString()
{
  CStr line;
  line = "GMS_HDF5_01 \"input.h5\" \"Drain with spaces\" -1";
  {
    ListReaderParser p(line);
    TS_ASSERT(!p.ValidInputString());
  }
  line = "GMS_HDF5_01 \"input.h5\" \"Drain with spaces\" 2147483648";
  {
    ListReaderParser p(line);
    TS_ASSERT(!p.ValidInputString());
  }

  line = "GMS_HDF5_01 input.h5 \"Drain with spaces\" 1";
  {
    ListReaderParser p(line);
    TS_ASSERT(!p.ValidInputString());
  }

  line = "GMS_HDF5_02 \"input.h5\" \"Drain with spaces\" 1";
  {
    ListReaderParser p(line);
    TS_ASSERT(!p.ValidInputString());
  }
  line = "GMS_HDF5_01 \"input.h5\" Drain 2147483648";
  {
    ListReaderParser p(line);
    TS_ASSERT(!p.ValidInputString());
  }
}
//------------------------------------------------------------------------------
void ListReaderParserT::testGetFileName()
{
  CStr line;
  line = "GMS_HDF5_01 \"input.h5\" \"Drain with spaces\" 1";
  {
    ListReaderParser p(line);
    TS_ASSERT(p.ValidInputString());
    TS_ASSERT(p.GetFileName() == "input.h5");
  }

  line = "GMS_HDF5_01 \"input with space.h5.bob\" \"Drain with spaces\" 1";
  {
    ListReaderParser p(line);
    TS_ASSERT(p.ValidInputString());
    TS_ASSERT(p.GetFileName() == "input with space.h5.bob");
  }
}
//------------------------------------------------------------------------------
void ListReaderParserT::testGetPath()
{
  CStr line;
  line = "GMS_HDF5_01 \"input.h5\" \"Drain with spaces\" 1";
  {
    ListReaderParser p(line);
    TS_ASSERT(p.ValidInputString());
    TS_ASSERT(p.GetPath() == "Drain with spaces");
  }
}
//------------------------------------------------------------------------------
void ListReaderParserT::testGetStressPeriod()
{
  CStr line;
  line = "GMS_HDF5_01 \"input.h5\" \"Drain with spaces\" 1";
  {
    ListReaderParser p(line);
    TS_ASSERT(p.ValidInputString());
    TS_ASSERT_EQUALS(p.GetStressPeriod(), 1);
  }
  line = "GMS_HDF5_01 \"input.h5\" \"Drain with spaces\" 25";
  {
    ListReaderParser p(line);
    TS_ASSERT(p.ValidInputString());
    TS_ASSERT_EQUALS(p.GetStressPeriod(), 25);
  }
  line = "GMS_HDF5_01 \"input.h5\" \"Drain with spaces\" 25123456";
  {
    ListReaderParser p(line);
    TS_ASSERT(p.ValidInputString());
    TS_ASSERT_EQUALS(p.GetStressPeriod(), 25123456);
  }

  line = "GMS_HDF5_01 \"input.h5\" \"Drain with spaces\" 2147483647";
  {
    ListReaderParser p(line);
    TS_ASSERT(p.ValidInputString());
    TS_ASSERT_EQUALS(p.GetStressPeriod(), 2147483647);
  }
}
#endif


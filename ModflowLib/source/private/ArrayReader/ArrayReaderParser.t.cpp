//------------------------------------------------------------------------------
// FILE      ArrayReaderParser.t.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#ifdef CXX_TEST

#include <private\ArrayReader\ArrayReaderParser.t.h>

#include <private\ArrayReader\ArrayReaderParser.h>

//------------------------------------------------------------------------------
void ArrayReaderParserT::testCreateClass ()
{
  ArrayReaderParser *p = new ArrayReaderParser("stuff");
  TS_ASSERT(p);
  if (p)
    delete(p);
}
//------------------------------------------------------------------------------
void ArrayReaderParserT::testWrongString ()
{
  ArrayReaderParser p("crap");
  TS_ASSERT(!p.ValidInputString());
}
//------------------------------------------------------------------------------
void ArrayReaderParserT::testCorrectString()
{
  CStr line("HDF5 1.0 0 \"input.h5\" \"Recharge/07. Property\" 3 0 1 0 132 0 1");
  ArrayReaderParser p(line);
  TS_ASSERT(p.ValidInputString());
}
//------------------------------------------------------------------------------
void ArrayReaderParserT::testAlmostCorrectString()
{
  CStr line("HDF5 1.0 0 \"input.h5\" \"Recharge/07. Property\" 3 0 1 0 132");
  TS_ASSERT(StringIsInvalid(line.c_str()));
  
  line ="HDF5 1.0 0 \"input.h5\" \"Recharge/07. Property\" 3 0 1 0 132 0 1 0 1";
  TS_ASSERT(!StringIsInvalid(line.c_str()));

  line = "HDF5 1.0 0 input.h5 \"Recharge/07. Property\" 2 0 1 0 132";
  TS_ASSERT(StringIsInvalid(line.c_str()));
}
//------------------------------------------------------------------------------
bool ArrayReaderParserT::StringIsInvalid (const char *a_str)
{
  CStr str(a_str);
  ArrayReaderParser p(str);
  return (!p.ValidInputString());
}
//------------------------------------------------------------------------------
void ArrayReaderParserT::testGetMultiplier ()
{
  CStr line;
  line = "HDF5 1.0 0 \"input.h5\" \"Recharge/07. Property\" 3 0 1 0 132 0 1";
  ArrayReaderParser p(line);
  TS_ASSERT_EQUALS(1.0, p.GetMultiplier());

  line = "HDF5 1.23456789123456789123 0 \"input.h5\" \"Recharge/07. Property\" 3 0 1 0 132 0 1";
  ArrayReaderParser p1(line);
  TS_ASSERT_EQUALS(1.23456789123456789, p1.GetMultiplier());
}
//------------------------------------------------------------------------------
void ArrayReaderParserT::testGetIPRN ()
{
  CStr line;
  line = "HDF5 1.0 0 \"input.h5\" \"Recharge/07. Property\" 3 0 1 0 132 0 1";
  ArrayReaderParser p(line);
  TS_ASSERT_EQUALS(0, p.GetIPRN());

  line = "HDF5 1.0 -1 \"input.h5\" \"Recharge/07. Property\" 3 0 1 0 132 0 1";
  ArrayReaderParser p1(line);
  TS_ASSERT_EQUALS(-1, p1.GetIPRN());
}
//------------------------------------------------------------------------------
void ArrayReaderParserT::testGetFileName ()
{
  CStr line;
  line = "HDF5 1.0 0 \"a screwed up name with spaces.h5\" \"Recharge/07. Property\" 3 0 1 0 132 0 1";
  ArrayReaderParser p(line);
  TS_ASSERT(p.GetFileName() == "a screwed up name with spaces.h5");

  line = "HDF5 1.0 0 \"simplName.tmp\" \"Recharge/07. Property\" 3 0 1 0 132 0 1";
  ArrayReaderParser p1(line);
  TS_ASSERT(p1.GetFileName() == "simplName.tmp");
}
//------------------------------------------------------------------------------
void ArrayReaderParserT::testGetPath ()
{
  CStr line;
  line = "HDF5 1.0 0 \"input.h5\" \"Recharge/07. Property\" 3 0 1 0 132 0 1";
  ArrayReaderParser p(line);
  TS_ASSERT(p.GetPath() == "Recharge/07. Property");

  line = "HDF5 1.0 0 \"input.h5\" \"path\" 3 0 1 0 132 0 1";
  ArrayReaderParser p1(line);
  TS_ASSERT(p1.GetPath() == "path");
}
//------------------------------------------------------------------------------
void ArrayReaderParserT::testGetIndices ()
{
  VEC_INT_PAIR myIndices, parseIndices;
  std::pair<int, int> myPair;

  myPair.first = 0;
  myPair.second = 1;
  myIndices.push_back(myPair);
  myPair.second = 132;
  myIndices.push_back(myPair);
  myPair.second = 1;
  myIndices.push_back(myPair);

  CStr line;
  line = "HDF5 1.0 0 \"input.h5\" \"Recharge/07. Property\" 3 0 1 0 132 0 1";
  ArrayReaderParser p(line);
  parseIndices = p.GetIndices();
  TS_ASSERT(parseIndices.size() == myIndices.size());
  try
  {
    for (size_t i=0; i<myIndices.size(); i++)
    {
      TS_ASSERT_EQUALS(myIndices.at(i).first, myIndices.at(i).first);
      TS_ASSERT_EQUALS(myIndices.at(i).second, myIndices.at(i).second);
    }
  }
  catch (std::out_of_range)
  {
    TS_FAIL("Threw an exception\n");
  }
  TS_ASSERT(p.GetIndices() == myIndices);
}
//------------------------------------------------------------------------------
void ArrayReaderParserT::testConstantValue ()
{
  CStr line("HDF5 CONSTANT 5.0 1");
  {
    line = "HDF5 2.0 0 \"input.h5\" \"Recharge/07. Property\" 3 0 1 0 132 0 1";
    ArrayReaderParser p(line);
    TS_ASSERT(p.ValidInputString());
    TS_ASSERT(!p.ConstantValue());
    TS_ASSERT_EQUALS(p.GetConstValue(), 0.0);
    TS_ASSERT_EQUALS(p.GetMultiplier(), 2.0);
  }

  {
    line = "HDF5 CONSTANT 5.0 1";
    ArrayReaderParser p(line);
    TS_ASSERT(p.ValidInputString());
    TS_ASSERT(p.ConstantValue());
    TS_ASSERT_EQUALS(p.GetConstValue(), 5.0);
    TS_ASSERT_EQUALS(p.GetMultiplier(), 1.0);
  }
  {
    line = "HDF5 constant -7 1";
    ArrayReaderParser p(line);
    TS_ASSERT(p.ValidInputString());
    TS_ASSERT(p.ConstantValue());
    TS_ASSERT_EQUALS(p.GetConstValue(), -7);
    TS_ASSERT_EQUALS(p.GetMultiplier(), 1.0);
  }
}

#endif // CXX_TEST


//------------------------------------------------------------------------------
// FILE      SenFileReader.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------

#include <private\Parameters\SenFileReader.h>

#include <private\Parameters\Param.h>
#include <private\Parameters\ParamList.h>
#include <private\util\EReadAsciiFile.h>


//------------------------------------------------------------------------------
/// \brief Constructor.
//------------------------------------------------------------------------------
SenFileReader::SenFileReader (const char *a_fName) :
m_fName(a_fName),
m_fp(0)
{
} // SenFileReader::SenFileReader
//------------------------------------------------------------------------------
/// \brief Constructor.
//------------------------------------------------------------------------------
bool SenFileReader::FillInStartingVals (ParamList *a_,
                                        bool a_isPVAL) const
{
  if (!a_ || a_->Size() < 1)
  {
    ErrorStack::Get().PutError("Null parameter passed to FillInStartingVals.");
    return false;
  }

  EReadAsciiFile r(m_fName.c_str());
  r.UseExceptions();
  if (!r.OpenFile())
  {
    CStr msg("Error opening file: " + m_fName + ".");
    ErrorStack::Get().PutError(msg);
    return false;
  }

  bool retval(true);
  try
  {
    // remove any comment lines
    bool done(0);
    while (!done)
    {
      CStr tmp;
      r.GetLine();
      r.ReadData(tmp);
      if (!tmp.IsEmpty() && tmp.at(0) != '#')
        done = true;
    }
    // the first 2 lines of this file we don't need
    // r.GetLine(); we got this line above
    if (!a_isPVAL)
    {
      r.GetLine();
    }

    // now read the parameters
    CStr line;
    while (r.GetLine(&line))
    {
      CStr name;
      int isens, ln;
      double start;
      // string should have the following
      // PARNAM ISENS LN B BL BU BSCAL
      // if it is a PVAL file then it will be like this
      // PARNAME Parval
      line.Replace("'", "\"");
      r.SetLine(line.c_str());
      r.ReadData(name);
      if (!a_isPVAL)
      {
        r.ReadData(isens);
        r.ReadData(ln);
      }
      else
      { // for PVAL there is no isens
        isens = 0;
      }
      r.ReadData(start);

      // find this parameter and set the starting value
      Param p;
      if (!a_->FindByName(name, &p))
      {
        // see if these are pilot points "sc1v1"
        CStr str = name.Left(2);
        if (str.CompareNoCase("sc") == 0)
        {
          a_->SetPilotPtVal(name, start, isens);
        }
      }
      else
      {
        p.m_value = start;
        a_->UpdateParameter(&p);
      }
    }
  }
  catch (ioexception)
  {
    CStr msg("Error reading file: " + m_fName + ".");
    ErrorStack::Get().PutError(msg);
    retval = false;
  }

  return retval;
} // SenFileReader::FillInStartingVals

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\Parameters\SenFileReader.t.h>

#include <private\Parameters\ParamFileReader.h>
#include <private\Parameters\ParamList.h>

//------------------------------------------------------------------------------
void SenFileReaderT::setUp ()
{
  m_parFile = util::GetTestFilesDirectory() + "\\Parameter\\pest.param";
  m_file = util::GetTestFilesDirectory() + "\\Parameter\\pest.snn";
  m_file1 = util::GetTestFilesDirectory() + "\\Parameter\\wrongPest.snn";
}
//------------------------------------------------------------------------------
void SenFileReaderT::testNullParamList ()
{
  ParamList *asdf(NULL);
  SenFileReader sr(m_file);
  TS_ASSERT(!sr.FillInStartingVals(asdf, false));
}
//------------------------------------------------------------------------------
void SenFileReaderT::testFileDoesntExist ()
{
  ParamList asdf;
  SenFileReader sr("stuff");
  TS_ASSERT(!sr.FillInStartingVals(&asdf, false));
}
//------------------------------------------------------------------------------
void SenFileReaderT::testFileFormatWrong ()
{
  ParamList asdf;
  SenFileReader sr(m_parFile);
  TS_ASSERT(!sr.FillInStartingVals(&asdf, false));
}
//------------------------------------------------------------------------------
void SenFileReaderT::testFileFormatWrongPartWayThroughFile ()
{
  ParamList asdf;
  SenFileReader sr(m_file1);
  TS_ASSERT(!sr.FillInStartingVals(&asdf, false));
}
//------------------------------------------------------------------------------
void SenFileReaderT::testFillInStartingVals ()
{
  // I have to read in some parameters first
  ParamList pl;
  ParamFileReader pr(m_parFile);

  pr.FillInListFromFile(&pl);
  TS_ASSERT(pl.Size() == 4);

  SenFileReader sr(m_file);
  sr.FillInStartingVals(&pl, false);

  // verify the values that were read correctly
  Param p;
  TS_ASSERT(pl.FindByKey(-30, &p));
  TS_ASSERT_EQUALS(p.m_value, 0.4535251);
  TS_ASSERT(pl.FindByKey(-150, &p));
  TS_ASSERT_EQUALS(p.m_value, 150);
  TS_ASSERT(pl.FindByKey(-180, &p));
  TS_ASSERT_EQUALS(p.m_value, 180);
  TS_ASSERT(pl.FindByKey(-210, &p));
  TS_ASSERT_EQUALS(p.m_value, 210);

  double ppV[15] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
  std::vector<double> vD;
  TS_ASSERT(pl.GetPilotPtValues(1, vD));
  for (int i=0; i<15; i++)
  {
    TS_ASSERT_EQUALS(vD.at(i), ppV[i]);
  }
}


#endif


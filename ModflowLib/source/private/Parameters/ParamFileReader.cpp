//------------------------------------------------------------------------------
// FILE      ParamFileReader.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------

#include <private\Parameters\ParamFileReader.h>

#include <map>

#include <private\Parameters\Param.h>
#include <private\Parameters\ParamList.h>
#include <private\util\EReadAsciiFile.h>

//------------------------------------------------------------------------------
/// \brief Constructor.
//------------------------------------------------------------------------------
ParamFileReader::ParamFileReader (const char *a_fName) :
m_fName(a_fName),
m_fp(0)
{
} // ParamFileReader::ParamFileReader
//------------------------------------------------------------------------------
/// \brief Destructor.
//------------------------------------------------------------------------------
ParamFileReader::~ParamFileReader ()
{
  if (m_fp)
    fclose(m_fp);
} // ParamFileReader::~ParamFileReader
//------------------------------------------------------------------------------
/// \brief Fills in a ParamList by reading from a file
//------------------------------------------------------------------------------
bool ParamFileReader::FillInListFromFile (ParamList *a_)
{
  if (!a_)
  {
    ErrorStack::Get().PutError("Null parameter passed to FillInListFromFile.");
    return false;
  }

  a_->Clear(); // clear out the list that was passed in

  EReadAsciiFile r(m_fName.c_str());
  r.UseExceptions();
  if (!r.OpenFile())
  {
    CStr msg("Error opening file: " + m_fName + ".");
    ErrorStack::Get().PutError(msg);
    return false;
  }

  a_->SetSourceFile(m_fName);
  try 
  {
    Param p;
    CStr  card;
    int   iCard;

    while (r.GetLine())
    {
      r.ReadData(card);
      iCard = FileCardToInt(card);
      switch (iCard)
      {
      case ParamFileReader::BEGPAR:
        p = Param(); // reinit param
        break;
      case ParamFileReader::NAME:
        r.ReadData(p.m_name);
        break;
      case ParamFileReader::TYPE:
        r.ReadData(p.m_type);
        if (p.m_type.CompareNoCase("well") == 0)
          p.m_type = "Q";
        break;
      case ParamFileReader::KEY:
        r.ReadData(p.m_key);
        break;
      case ParamFileReader::VALUE:
        r.ReadData(p.m_value);
        p.m_start = p.m_b = p.m_value;
        r.ReadData(p.m_min);
        r.ReadData(p.m_max);
        break;
      case ParamFileReader::BEGPILOT:
        p.m_pilotPoints = true;
        break;
      case ParamFileReader::INTERPLOG:
        p.m_logInterp = true;
        r.ReadData(p.m_logMinVal);
        break;
      case ParamFileReader::SCATINDEX:
        r.ReadData(p.m_scatIndex);
        break;
      case ParamFileReader::ARRAYTYPE:
        {
          int tmpI;
          r.ReadData(tmpI);
          if (tmpI)
            p.m_multArray = true;
        }
        break;
      case ParamFileReader::ENDPAR:
        // add the param to the list
        if (!a_->PushBack(&p))
        {
          CStr msg("Unable to add parameter to list. Error reading " + m_fName + ".");
          throw ioError(msg);
        }
        p = Param(); // reinit param
        break;
      case ParamFileReader::BEGTAB:
        // read lines from the file until we get to ENDTAB
        {
          bool done = false;
          CStr ln;
          // next line should be NROWCOL
          r.GetLine();
          r.ReadData(ln);
          int nrow;
          r.ReadData(nrow);
          if (nrow > 0)
          {
            p.m_clustInParamFile = true;
          }

          while (!done && r.GetLine(&ln))
          {
            if (!ln.IsEmpty())
            {
              r.ReadData(ln);
              if (ln.CompareNoCase("endtable") == 0)
                done = true;
            }
          }
        }
        break;
      case ParamFileReader::BSCAL:
        r.ReadData(p.m_bscal);
        break;
      case ParamFileReader::LOGXFORM:
        p.m_logTrans = true;
        break;
      case ParamFileReader::TIED:
        r.ReadData(p.m_tied);
        break;
      }
    }
  }
  catch (ioexception &e)
  {
    CStr msg(e.what());
    if (!msg.IsEmpty())
    {
      ErrorStack::Get().PutError(msg);
    }
    return false;
  }
  return true;
} // ParamFileReader::FillInListFromFile
//------------------------------------------------------------------------------
/// \brief Converts a string to an integer value
//------------------------------------------------------------------------------
int ParamFileReader::FileCardToInt (const CStr &a_card)
{
  std::map<CStr, int> myMap;
  myMap.insert(std::make_pair("begpar", ParamFileReader::BEGPAR));
  myMap.insert(std::make_pair("name", ParamFileReader::NAME));
  myMap.insert(std::make_pair("type", ParamFileReader::TYPE));
  myMap.insert(std::make_pair("key", ParamFileReader::KEY));
  myMap.insert(std::make_pair("value", ParamFileReader::VALUE));
  myMap.insert(std::make_pair("begpilot", ParamFileReader::BEGPILOT));
  myMap.insert(std::make_pair("interplog", ParamFileReader::INTERPLOG));
  myMap.insert(std::make_pair("scatindex", ParamFileReader::SCATINDEX));
  myMap.insert(std::make_pair("arraytype", ParamFileReader::ARRAYTYPE));
  myMap.insert(std::make_pair("endpar", ParamFileReader::ENDPAR));
  myMap.insert(std::make_pair("begtable", ParamFileReader::BEGTAB));
  myMap.insert(std::make_pair("bscal", ParamFileReader::BSCAL));
  myMap.insert(std::make_pair("logxform", ParamFileReader::LOGXFORM));
  myMap.insert(std::make_pair("tied", ParamFileReader::TIED));

  CStr card(a_card);
  card.ToLower();
  if (myMap.find(card) == myMap.end())
    return -1;
  return(myMap[card]);
} // ParamFileReader::FileCardToInt

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////
#ifdef CXX_TEST

#include <private\Parameters\ParamFileReader.t.h>

//------------------------------------------------------------------------------
void ParamFileReaderT::setUp ()
{
  m_file = util::GetTestFilesDirectory() + "\\Parameter\\pest.param";
}
//------------------------------------------------------------------------------
void ParamFileReaderT::testCreateClass ()
{
  CStr f;
  ParamFileReader *p = new ParamFileReader(f);
  TS_ASSERT(p);
  if (p)
    delete(p);
}
//------------------------------------------------------------------------------
void ParamFileReaderT::testFileCardToInt ()
{
  CStr f;
  ParamFileReader p(f);
  TS_ASSERT(p.FileCardToInt(CStr("begpar")) == ParamFileReader::BEGPAR);
  TS_ASSERT(p.FileCardToInt(CStr("BEGPAR")) == ParamFileReader::BEGPAR);
  TS_ASSERT(p.FileCardToInt(CStr("name")) == ParamFileReader::NAME);
  TS_ASSERT(p.FileCardToInt(CStr("NAME")) == ParamFileReader::NAME);
  TS_ASSERT(p.FileCardToInt(CStr("type")) == ParamFileReader::TYPE);
  TS_ASSERT(p.FileCardToInt(CStr("TYPE")) == ParamFileReader::TYPE);
  TS_ASSERT(p.FileCardToInt(CStr("key")) == ParamFileReader::KEY);
  TS_ASSERT(p.FileCardToInt(CStr("KEY")) == ParamFileReader::KEY);
  TS_ASSERT(p.FileCardToInt(CStr("value")) == ParamFileReader::VALUE);
  TS_ASSERT(p.FileCardToInt(CStr("VALUE")) == ParamFileReader::VALUE);
  TS_ASSERT(p.FileCardToInt(CStr("begpilot")) == ParamFileReader::BEGPILOT);
  TS_ASSERT(p.FileCardToInt(CStr("BEGPILOT")) == ParamFileReader::BEGPILOT);
  TS_ASSERT(p.FileCardToInt(CStr("interplog")) == ParamFileReader::INTERPLOG);
  TS_ASSERT(p.FileCardToInt(CStr("INTERPLOG")) == ParamFileReader::INTERPLOG);
  TS_ASSERT(p.FileCardToInt(CStr("scatindex")) == ParamFileReader::SCATINDEX);
  TS_ASSERT(p.FileCardToInt(CStr("SCATINDEX")) == ParamFileReader::SCATINDEX);
  TS_ASSERT(p.FileCardToInt(CStr("arraytype")) == ParamFileReader::ARRAYTYPE);
  TS_ASSERT(p.FileCardToInt(CStr("ARRAYTYPE")) == ParamFileReader::ARRAYTYPE);
  TS_ASSERT(p.FileCardToInt(CStr("begtable")) == ParamFileReader::BEGTAB);
  TS_ASSERT(p.FileCardToInt(CStr("BEGTABLE")) == ParamFileReader::BEGTAB);
  TS_ASSERT(p.FileCardToInt(CStr("endpar")) == ParamFileReader::ENDPAR);
  TS_ASSERT(p.FileCardToInt(CStr("ENDPAR")) == ParamFileReader::ENDPAR);
  TS_ASSERT(p.FileCardToInt(CStr("bscal")) == ParamFileReader::BSCAL);
  TS_ASSERT(p.FileCardToInt(CStr("BSCAL")) == ParamFileReader::BSCAL);
  TS_ASSERT(p.FileCardToInt(CStr("logxform")) == ParamFileReader::LOGXFORM);
  TS_ASSERT(p.FileCardToInt(CStr("LOGXFORM")) == ParamFileReader::LOGXFORM);
  TS_ASSERT(p.FileCardToInt(CStr("tied")) == ParamFileReader::TIED);
  TS_ASSERT(p.FileCardToInt(CStr("TIED")) == ParamFileReader::TIED);
  TS_ASSERT(p.FileCardToInt(CStr("crap")) == -1);
  TS_ASSERT(ParamFileReader::NITEMS == 15);
}
//------------------------------------------------------------------------------
void ParamFileReaderT::testFillInListFromFile1 ()
{
  ParamList l;

  {
    CStr empty;
    ParamFileReader f(empty);

    TS_ASSERT(!f.FillInListFromFile(NULL));
    TS_ASSERT(!f.FillInListFromFile(&l));
  }

  {
    ParamFileReader f(m_file);
    TS_ASSERT(!f.FillInListFromFile(NULL));
    TS_ASSERT(f.FillInListFromFile(&l));
    TS_ASSERT(l.Size() == 4);

    Param p;
    TS_ASSERT(l.FindByKey(-30, &p));
    TS_ASSERT(p.m_logInterp == true);
    TS_ASSERT(p.m_logMinVal == 1e-6);
    TS_ASSERT(p.m_key == -30);
    TS_ASSERT(p.m_max == 30);
    TS_ASSERT(p.m_min == .003);
    TS_ASSERT(p.m_name == "HK_30");
    TS_ASSERT(p.m_pilotPoints == true);
    TS_ASSERT(p.m_type == "HK");
    TS_ASSERT(p.m_value == .4535251);
    TS_ASSERT(p.m_scatIndex == 1);
    TS_ASSERT(p.m_multArray == false);
    TS_ASSERT(p.m_bscal == 1.01);
    TS_ASSERT(p.m_logTrans == true);

    TS_ASSERT(l.FindByKey(-150, &p));
    TS_ASSERT(p.m_logInterp == false);
    TS_ASSERT(p.m_logMinVal == 0);
    TS_ASSERT(p.m_key == -150);
    TS_ASSERT(p.m_max == .0001);
    TS_ASSERT(p.m_min == 1e-10);
    TS_ASSERT(p.m_name == "RCH_150");
    TS_ASSERT(p.m_pilotPoints == false);
    TS_ASSERT(p.m_type == "RCH");
    TS_ASSERT(p.m_value == .00009113173);
    TS_ASSERT(p.m_scatIndex == -1);
    TS_ASSERT(p.m_multArray == true);
    TS_ASSERT(p.m_bscal == 1.1);
    TS_ASSERT(p.m_logTrans == false);

    TS_ASSERT(l.FindByKey(-180, &p));
    TS_ASSERT(p.m_logInterp == false);
    TS_ASSERT(p.m_logMinVal == 0);
    TS_ASSERT(p.m_key == -180);
    TS_ASSERT(p.m_max == .0001);
    TS_ASSERT(p.m_min == 1e-10);
    TS_ASSERT(p.m_name == "RCH_180");
    TS_ASSERT(p.m_pilotPoints == false);
    TS_ASSERT(p.m_type == "RCH");
    TS_ASSERT(p.m_value == .00005433351);
    TS_ASSERT(p.m_scatIndex == -1);
    TS_ASSERT(p.m_multArray == false);
    TS_ASSERT(p.m_bscal == 1.001);
    TS_ASSERT(p.m_logTrans == false);

    TS_ASSERT(l.FindByKey(-210, &p));
    TS_ASSERT(p.m_logInterp == false);
    TS_ASSERT(p.m_logMinVal == 0);
    TS_ASSERT(p.m_key == -210);
    TS_ASSERT(p.m_max == .0001);
    TS_ASSERT(p.m_min == 1e-10);
    TS_ASSERT(p.m_name == "RCH_210");
    TS_ASSERT(p.m_pilotPoints == false);
    TS_ASSERT(p.m_type == "RCH");
    TS_ASSERT(p.m_value == .00004890395);
    TS_ASSERT(p.m_scatIndex == -1);
    TS_ASSERT(p.m_multArray == false);
    TS_ASSERT(p.m_bscal == 1.00001);
    TS_ASSERT(p.m_logTrans == false);
  }
}
#endif


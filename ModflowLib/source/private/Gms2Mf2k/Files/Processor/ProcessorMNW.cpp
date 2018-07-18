//------------------------------------------------------------------------------
// FILE      ProcessorMNW.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/Gms2Mf2k/Files/Processor/ProcessorMNW.h>

#include <fstream>
#include <sstream>

#include <ModflowLib.h>
#include <private/ListReader/CellIdToIJK.h>
#include <private/MNWReader.h>
#include <private/util/EReadAsciiFile.h>

class ProcessorMNW::impl
{
friend ProcessorMNWT;
public:
  impl();
  bool DoConvertFile(const char * const a_inputFile,
                     const char * const a_outputFile,
                     const int &a_nRow,
                     const int &a_nCol);

  bool ReadWellData(const CStr &line);
  bool WriteWellData(FILE *a_fp,
                     const int &a_nRow,
                     const int &a_nCol);

  template <class T>
  CStr StrVal(const T& a_)
  {
    CStr s;
    s.Format("%s ", STR(a_));
    return s;
  }

private:
  bool              m_isH5File;
  int               m_currLine;
  int               m_nwell2;
  int               m_stressPeriod;
  CAR_DBL2D         m_well2;
  std::vector<int>  m_cellids;
  std::vector<CStr> m_names;
};

///////////////////////////////////////////////////////////////////////////////
/// \brief Constructor
///////////////////////////////////////////////////////////////////////////////
ProcessorMNW::ProcessorMNW (const char * const a_inputFile,
                              const char * const a_outputFile,
                              const int& a_nRow,
                              const int& a_nCol) :
Processor(a_inputFile, a_outputFile, a_nRow, a_nCol, false),
m_p(new ProcessorMNW::impl())
{
} // ProcessorMNW::ProcessorMNW
///////////////////////////////////////////////////////////////////////////////
/// \brief Destructor
///////////////////////////////////////////////////////////////////////////////
ProcessorMNW::~ProcessorMNW ()
{
  try
  {
    if (m_p)
      delete(m_p);
  }
  catch (...)
  {
  }
} // ProcessorMNW::~ProcessorMNW
///////////////////////////////////////////////////////////////////////////////
/// \brief Private virtual function that actually does the conversion
///////////////////////////////////////////////////////////////////////////////
bool ProcessorMNW::DoConvertFile ()
{
  return(m_p->DoConvertFile(InputFile(),
                            OutputFile(),
                            NumRow(),
                            NumCol()));
} // ProcessorMNW::DoConvertFile


///////////////////////////////////////////////////////////////////////////////
/// \brief Constructor
///////////////////////////////////////////////////////////////////////////////
ProcessorMNW::impl::impl () :
m_isH5File(false),
m_currLine(0),
m_nwell2(0),
m_stressPeriod(0)
{
} // ProcessorMNW::impl::impl
///////////////////////////////////////////////////////////////////////////////
/// \brief Private virtual function that actually does the conversion
///////////////////////////////////////////////////////////////////////////////
bool ProcessorMNW::impl::DoConvertFile (const char * const a_inputFile,
                                         const char * const a_outputFile,
                                         const int &a_nRow,
                                         const int &a_nCol)
{
  bool rval(1);
  CStr inputFile(a_inputFile), outputFile(a_outputFile);

  try
  {
    //lint --e{534} I am catching exceptions
    EReadAsciiFile e(inputFile);
    if (!e.OpenFile())
      throw ioError();

    FILE *fp(0);
    fp = fopen(outputFile, "w");
    if (!fp)
      throw ioError();

    CStr line, str;
    while (e.GetLine(&line))
    {
      e.ReadData(str);
      if (str == "#GMS_HDF5_01")
      {
        // don't echo this to the file
        m_isH5File = true;
      }
      else if (str == "GMS_HDF5_MNW")
      {
        if (ReadWellData(line))
        {
          WriteWellData(fp, a_nRow, a_nCol);
        }
      }
      else
      {
        if (m_isH5File && line[0] != '#')
        {
          if ((m_currLine == 2 || m_currLine == 3) &&
              line.find("FILE:") != std::string::npos)
            m_currLine = 3;
          else if (m_currLine == 2 || m_currLine == 3)
            m_currLine = 4;
          else
            ++m_currLine;
        }

        // echo the line to the output file
        fprintf(fp, "%s\n", line.c_str());
        if (m_currLine >= 4)
        {
          e.SetLine(line);
          // we need to get the number of wells in current stress period
          e.ReadData(m_nwell2);
        }
      }
    }
    fclose(fp);
  }
  catch (ioexception &)
  {
    CStr msg("Error processing file: ");
    msg += inputFile;
    msg += ".";
    ErrorStack::Get().PutError(msg);
    rval = false;
  }
  return rval;
} // ProcessorMNW::impl::DoConvertFile
//------------------------------------------------------------------------------
/// \brief Reads the mnw well data from the h5 file
//------------------------------------------------------------------------------
bool ProcessorMNW::impl::ReadWellData (const CStr &a_line)
{
  if (m_nwell2 < 1)
    return false;

  return mnw::ReadH5Data(m_cellids, m_well2, m_names, a_line);
} // ProcessorMNW::impl::ReadWellData
//------------------------------------------------------------------------------
/// \brief Reads the mnw reach data from the h5 file
//------------------------------------------------------------------------------
bool ProcessorMNW::impl::WriteWellData (FILE *a_fp,
                                        const int &a_nRow,
                                        const int &a_nCol)
{
  using util::ForElement;
  using util::lrint;
  using mnw::MnwH5;
  if (!a_fp)
    return false;
  try
  {
    int lastWellId = 0;
    for (int w = 0; (size_t)w < m_cellids.size(); ++w)
    {
      if (lrint(m_well2.at(mnw::H5_ACTIVE, w) == mnw::ACTIVE))
      {
        CellIdToIJK cellToIJK(a_nRow, a_nCol);
        int cellid = m_cellids.at(w);
        int i = cellToIJK.IFromId(cellid);
        int j = cellToIJK.JFromId(cellid);
        int k = cellToIJK.KFromId(cellid);

        double qdes = m_well2.at(mnw::H5_QDES, w);

        int wellId = lrint(m_well2.at(mnw::H5_WELLID, w));
        CStr mn;
        if (wellId == lastWellId && wellId != 0)
          mn = "MN";
        CStr mnwsite;
        if ((wellId != lastWellId || wellId == 0) &&
            lrint(m_well2.at(mnw::H5_SITE, w)) == mnw::SITE_PRINT)
        {
          mnwsite = "SITE:";
          mnwsite += m_names.at(w);
        }
        lastWellId = wellId;

        enum { QWVAL, RW, SKIN, HLIM, HREF, IQWGRP };
        CStr val[6];

        int itemsToPrint = 0;
        double qwval = m_well2.at(mnw::H5_QWVAL, w);
        val[QWVAL] = STR(qwval);
        if (qwval >= 0.0)
          itemsToPrint = 1;

        double rw = m_well2.at(mnw::H5_RW, w);
        val[RW] = STR(rw);
        if (rw != 0.0)
          itemsToPrint = 2;

        double skin = m_well2.at(mnw::H5_SKIN, w);
        val[SKIN] = STR(skin);
        if (skin != 0.0)
          itemsToPrint = 3;

        val[HLIM] = STR(m_well2.at(mnw::H5_HLIM, w));
        val[HREF] = STR(m_well2.at(mnw::H5_HREF, w));

        CStr dd;
        if (lrint(m_well2.at(mnw::H5_DD, w)) == mnw::DD_RELATIVE)
          dd = "DD";
        else
          dd = "";
        if (lrint(m_well2.at(mnw::H5_DD, w)) != mnw::DD_NONE)
          itemsToPrint = 5;

        int iwgrp = lrint(m_well2.at(mnw::H5_IWGRP, w));
        val[IQWGRP].Format("%d", iwgrp);
        if (iwgrp >= 0)
          itemsToPrint = 6;

        double cval = m_well2.at(mnw::H5_C, w);
        CStr c;
        if (cval != 0)
          c.Format("Cp: %s", STR(cval));

        CStr qcut;
        // when the flow rate is zero then MNW ignores the QCUT stuff even if it
        // was in the original text file
        if (/*m_well2.at(mnw::H5_QDES, w) == 0 ||*/ // this needs to be looked at again
            lrint(m_well2.at(mnw::H5_QCUT, w)) == mnw::QCUT_NONE)
        {
          qcut = " ";
        }
        else if (lrint(m_well2.at(mnw::H5_QCUT, w)) == mnw::QCUT_RATE)
        {
          qcut.Format("QCUT: %s %s ", STR(m_well2.at(mnw::H5_QFRCMN, w)),
                                      STR(m_well2.at(mnw::H5_QFRCMX, w)));
        }
        else if (lrint(m_well2.at(mnw::H5_QCUT, w)) == mnw::QCUT_PCT)
        {
          qcut.Format("Q-%%CUT: %s %s ", STR(m_well2.at(mnw::H5_QFRCMN, w)),
                                         STR(m_well2.at(mnw::H5_QFRCMX, w)));
        }

        CStr optional;
        for (int i = 0; i < itemsToPrint; ++i)
        {
          optional += val[i];
          optional += " ";
        }
        optional += dd;

        CStr line;
        line.Format("%d %d %d %s %s %s %s %s %s", 
                    k, i, j, STR(qdes), mn.c_str(), optional.c_str(),
                    c.c_str(), qcut.c_str(), mnwsite.c_str());
        fprintf(a_fp, "%s\n", line.c_str());
      }
    }
  }
  catch (std::out_of_range &)
  {
    return false;
  }
  return true;
} // ProcessorMNW::impl::WriteWellData

#ifdef CXX_TEST
///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////

#include <private/Gms2Mf2k/Files/Processor/ProcessorMNW.t.h>
#include <private/H5DataReader/H5DataSetReader.h>
#include <private/MfLibAsserts.h>

//------------------------------------------------------------------------------
void ProcessorMNWT::testCreateClass () const
{
  ProcessorMNW *p;
  try
  {
    p = new ProcessorMNW("stuff", "stuff1", 1, 1);
    delete(p);
  }
  catch (std::bad_alloc&)
  {
    TS_FAIL("Allocation failed");
  }
}
//------------------------------------------------------------------------------
void ProcessorMNWT::testDoConvertFile () const
{
  CStr path;
  util::GetTestFilesDirectory(path);
  path += "\\Gms2Mf2k\\mnw\\mnw1";

  // get from file path
  CStr fromFile(path);
  fromFile += "\\mnw1.mnw";

  // get to file path
  CStr tmpFile;
  util::GetTempDirectory(tmpFile);
  tmpFile += "\\tmpMnw.mnw";

  TS_ASSERT(SetCurrentDirectory(path));

  ProcessorMNW p(fromFile, tmpFile, 21, 14);
  TS_ASSERT(p.DoConvertFile());

  // test the contents of the output file
  CStr baseFile(path);
  if (sizeof(Real) == sizeof(float))
    baseFile += "\\base-float.mnw";
  else
    baseFile += "\\base-double.mnw";
  TS_ASSERT_TXT_FILES_EQUAL(tmpFile, baseFile);
  TS_ASSERT(remove(tmpFile) == 0);
  H5Reader::CloseAllH5Files();
}

#endif

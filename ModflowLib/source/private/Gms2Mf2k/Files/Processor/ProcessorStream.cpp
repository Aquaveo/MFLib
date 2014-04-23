//------------------------------------------------------------------------------
// FILE      ProcessorStream.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/Gms2Mf2k/Files/Processor/ProcessorStream.h>

#include <fstream>
#include <sstream>

#include <ModflowLib.h>
#include <private/util/EReadAsciiFile.h>

class ProcessorStream::impl
{
friend ProcessorStreamT;
public:
  impl();
  bool DoConvertFile(const char * const a_inputFile,
                     const char * const a_outputFile,
                     const int &a_nRow,
                     const int &a_nCol,
                     bool a_unstructured);
  bool ReadStreamData(const CStr &a_line,
                      const int &a_nRow,
                      const int &a_nCol);
  bool WriteStreamData(FILE *a_fp,
                       bool a_unstructured);

private:
  int m_nstream, m_nss, m_ntrib, m_ndiv, m_icalc;
  std::vector<Real> m_strm;
  std::vector<int>   m_iStrm, m_iTrbar, m_iDivar;
};

///////////////////////////////////////////////////////////////////////////////
/// \brief Constructor
///////////////////////////////////////////////////////////////////////////////
ProcessorStream::ProcessorStream (const char * const a_inputFile,
                                  const char * const a_outputFile,
                                  const int& a_nRow,
                                  const int& a_nCol,
                                  bool a_unstructured) :
Processor(a_inputFile, a_outputFile, a_nRow, a_nCol, a_unstructured),
m_p(new ProcessorStream::impl())
{
} // Processor::Processor
///////////////////////////////////////////////////////////////////////////////
/// \brief Destructor
///////////////////////////////////////////////////////////////////////////////
ProcessorStream::~ProcessorStream ()
{
  try
  {
    if (m_p)
      delete(m_p);
  }
  catch (...)
  {
  }
} // ProcessorStream::~ProcessorStream
///////////////////////////////////////////////////////////////////////////////
/// \brief Private virtual function that actually does the conversion
///////////////////////////////////////////////////////////////////////////////
bool ProcessorStream::DoConvertFile ()
{
  return(m_p->DoConvertFile(InputFile(),
                            OutputFile(),
                            NumRow(),
                            NumCol(),
                            Unstructured()));
} // Processor::DoConvertFile


///////////////////////////////////////////////////////////////////////////////
/// \brief Constructor
///////////////////////////////////////////////////////////////////////////////
ProcessorStream::impl::impl () :
m_nstream(0),
m_nss(0),
m_ntrib(0),
m_ndiv(0),
m_icalc(0),
m_strm(),
m_iStrm(),
m_iTrbar(),
m_iDivar()
{
} // Processor::impl::impl
///////////////////////////////////////////////////////////////////////////////
/// \brief Private virtual function that actually does the conversion
///////////////////////////////////////////////////////////////////////////////
bool ProcessorStream::impl::DoConvertFile (const char * const a_inputFile,
                                           const char * const a_outputFile,
                                           const int &a_nRow,
                                           const int &a_nCol,
                                           bool a_unstructured)
{
  bool rval(1), firstTime(1);
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
      }
      else if (str == "GMS_HDF5_01")
      {
        if (ReadStreamData(line, a_nRow, a_nCol))
        {
          WriteStreamData(fp, a_unstructured);
        }
      }
      else
      {
        // echo the line to the output file
        fprintf(fp, "%s\n", line.c_str());
        if (firstTime)
        {
          firstTime = false;
          e.SetLine(line);
          // we need to get the NTRIB, NDIV, & ICALC
          e.ReadData(0,9, m_nstream); // MXACTS
          e.ReadData(10,19, m_nss); // NSS
          e.ReadData(20,29, m_ntrib);
          e.ReadData(30,39, m_ndiv);
          e.ReadData(40,49, m_icalc);
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
} // Processor::impl::DoConvertFile
///////////////////////////////////////////////////////////////////////////////
/// \brief Reads the stream data from the h5 file.
///////////////////////////////////////////////////////////////////////////////
bool ProcessorStream::impl::ReadStreamData (const CStr &a_line,
                                            const int &a_nRow,
                                            const int &a_nCol)
{
  if (m_nss < 1 )
    return false;

  int success;
  // size these vectors
  if (m_strm.empty())
    m_strm.assign(m_nstream*11, 0);
  if (m_iStrm.empty())
    m_iStrm.assign(m_nstream*5, 0);
  if (m_iTrbar.empty())
  {
    int val(1);
    if (m_ntrib > 0)
      val = m_ntrib;
    m_iTrbar.assign(m_nss*val, 0);
  }
  if (m_iDivar.empty())
    m_iDivar.assign(m_nss, 0);

  MFLIB_READSTR(&success, &m_nstream, &m_nss, &m_ntrib, &a_nCol, &a_nRow,
                &m_strm.front(), &m_iStrm.front(), &m_iTrbar.front(),
                &m_iDivar.front(), a_line.c_str(), a_line.GetLength());
  return (success ? 1 : 0);
} // ProcessorStream::impl::ReadStreamData
///////////////////////////////////////////////////////////////////////////////
/// \brief Writes the stream data from to the str file.
///////////////////////////////////////////////////////////////////////////////
bool ProcessorStream::impl::WriteStreamData (FILE *a_fp,
                                             bool a_unstructured)
{
  if (!a_fp)
    return false;

  try
  {
    //lint --e{732} loss of sign for the index into the vector
    int i, j;
    for (i=0; i<m_nstream; i++)
    {
      j = i * 5;
      if (a_unstructured)
      {
        fprintf(a_fp, "%5d%5d%5d", m_iStrm.at(j+2), m_iStrm.at(j+3),
                                   m_iStrm.at(j+4));
      }
      else
      {
        fprintf(a_fp, "%5d%5d%5d%5d%5d", m_iStrm.at(j+0), m_iStrm.at(j+1),
                                         m_iStrm.at(j+2), m_iStrm.at(j+3),
                                         m_iStrm.at(j+4));
      }
      j = i * 11;
      CStr s;
      s = STR(m_strm.at(j+0),-1,15,STR_FULLWIDTH);
      s += STR(m_strm.at(j+1),-1,10,STR_FULLWIDTH);
      s += STR(m_strm.at(j+2),-1,10,STR_FULLWIDTH);
      s += STR(m_strm.at(j+3),-1,10,STR_FULLWIDTH);
      s += STR(m_strm.at(j+4),-1,10,STR_FULLWIDTH);
      fprintf(a_fp, "%s\n", s.c_str());
    }

    for (i=0; m_icalc && i<m_nstream; i++)
    {
      j = i * 11;
      CStr s;
      s = STR(m_strm.at(j+5),-1,10,STR_FULLWIDTH);
      s += STR(m_strm.at(j+6),-1,10,STR_FULLWIDTH);
      s += STR(m_strm.at(j+7),-1,10,STR_FULLWIDTH);
      fprintf(a_fp, "%s\n", s.c_str());
    }

    for (i=0; m_ntrib && i<m_nss; i++)
    {
      for (j=0; j<m_ntrib; j++)
      {
        fprintf(a_fp, "%5d", m_iTrbar.at((j*m_nss)+i));
      }
      fprintf(a_fp, "\n");
    }

    for (i=0; m_ndiv && i<m_nss; i++)
    {
      fprintf(a_fp, "%5d\n", m_iDivar.at(i));
    }
  }
  catch (std::out_of_range &)
  {
    return false;
  }
  return true;
} // ProcessorStream::impl::WriteStreamData

#ifdef CXX_TEST
///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////

#include <private/Gms2Mf2k/Files/Processor/ProcessorStream.t.h>
#include <private/H5DataReader/H5DataSetReader.h>
#include <private/MfLibAsserts.h>

//------------------------------------------------------------------------------
void ProcessorStreamT::testCreateClass () const
{
  ProcessorStream *p;
  try
  {
    p = new ProcessorStream("stuff", "stuff1", 1, 1, false);
    delete(p);
  }
  catch (std::bad_alloc&)
  {
    TS_FAIL("Allocation failed");
  }
}
//------------------------------------------------------------------------------
void ProcessorStreamT::testDoConvertFile () const
{
  CStr path, line, file, outFile, baseFile, fileToCopy, destFile;
  util::GetTempDirectory(outFile);
  util::GetTestFilesDirectory(path);
  outFile += "\\stream.str";
  path += "\\Gms2Mf2k\\stream";
  file = path + "\\smallGrid_Trans_Str1_65.str";
  baseFile = path + "\\base.str";

  // we need to change the stream file if we are using doubles or floats
  if (sizeof(Real) == sizeof(float))
    fileToCopy = path + "\\Flt base.str";
  else
    fileToCopy = path + "\\Dbl base.str";
  destFile = path + "\\base.str";
  util::FileCopy(fileToCopy, destFile);

  TS_ASSERT(SetCurrentDirectory(path));

  ProcessorStream p(file, outFile, 6, 5, false);
  TS_ASSERT(p.ConvertFile());

  // let's test the contents of the output file
  TS_ASSERT_TXT_FILES_EQUAL(outFile, baseFile);
  TS_ASSERT(remove(outFile) == 0);
  H5Reader::CloseAllH5Files();
}
//------------------------------------------------------------------------------
void ProcessorStreamT::testReadStreamData () const
{
  CStr line, file;
  util::GetTestFilesDirectory(file);
  file += "\\Gms2Mf2k\\stream\\smallGrid_Trans_Str1_65.h5";
  line.Format("GMS_HDF5_01 \"%s\" \"Stream\" 1", file);

  ProcessorStream p("in", "out", 6, 5, false);

  // The number of streams stuff not set up. This won't work.
  TS_ASSERT(!p.m_p->ReadStreamData(line, 6, 5));

  // set these variables
  p.m_p->m_nstream = 16;
  p.m_p->m_nss = 6;
  p.m_p->m_ntrib = 2;
  p.m_p->m_ndiv = 1;
  p.m_p->m_icalc = 1;
  TS_ASSERT(p.m_p->ReadStreamData(line, 6, 5));
  TS_ASSERT(p.m_p->m_strm.size() == 176);
  TS_ASSERT(p.m_p->m_iStrm.size() == 80);
  TS_ASSERT(p.m_p->m_iTrbar.size() == 12);
  TS_ASSERT(p.m_p->m_iDivar.size() == 6);

  TS_ASSERT_DELTA(p.m_p->m_strm.at(0), 1, 1e-6);
  TS_ASSERT_DELTA(p.m_p->m_strm.at(1), 19.654154, 1e-6);
  TS_ASSERT_DELTA(p.m_p->m_strm.at(90), 0.67934394, 1e-6);
  TS_ASSERT_DELTA(p.m_p->m_strm.at(135), 18.0, 1e-6);
  TS_ASSERT_DELTA(p.m_p->m_strm.at(175), 0, 1e-6);

  TS_ASSERT_EQUALS(p.m_p->m_iStrm.at(0), 1);
  TS_ASSERT_EQUALS(p.m_p->m_iStrm.at(1), 1);
  TS_ASSERT_EQUALS(p.m_p->m_iStrm.at(11), 2);
  TS_ASSERT_EQUALS(p.m_p->m_iStrm.at(40), 1);
  TS_ASSERT_EQUALS(p.m_p->m_iStrm.at(56), 4);
  TS_ASSERT_EQUALS(p.m_p->m_iStrm.at(79), 3);

  TS_ASSERT_EQUALS(p.m_p->m_iTrbar.at(0), 0);
  TS_ASSERT_EQUALS(p.m_p->m_iTrbar.at(1), 0);
  TS_ASSERT_EQUALS(p.m_p->m_iTrbar.at(11), 0);

  TS_ASSERT_EQUALS(p.m_p->m_iDivar.at(0), 0);
  TS_ASSERT_EQUALS(p.m_p->m_iDivar.at(1), 0);
  TS_ASSERT_EQUALS(p.m_p->m_iDivar.at(5), 1);
  H5Reader::CloseAllH5Files();
}
//------------------------------------------------------------------------------
void ProcessorStreamT::testWriteStreamData () const
{
  CStr line, file;
  util::GetTestFilesDirectory(file);
  file += "\\Gms2Mf2k\\stream\\smallGrid_Trans_Str1_65.h5";
  line.Format("GMS_HDF5_01 \"%s\" \"Stream\" 1", file);

  ProcessorStream p("in", "out", 6, 5, false);
  p.m_p->m_nstream = 16;
  p.m_p->m_nss = 6;
  p.m_p->m_ntrib = 2;
  p.m_p->m_ndiv = 1;
  p.m_p->m_icalc = 1;
  TS_ASSERT(p.m_p->ReadStreamData(line, 6, 5));

  CStr tmpFile;
  util::GetTempDirectory(tmpFile);
  tmpFile += "\\tmpStr.str";
  FILE *fp(0);
  TS_ASSERT(!p.m_p->WriteStreamData(fp, false));
  fp = fopen(tmpFile, "w");
  TS_ASSERT(fp);
  if (!fp)
    return;
  TS_ASSERT(p.m_p->WriteStreamData(fp, false));
  fclose(fp);

  const int lnNum[6] = {1,11,24,34,36,43};
  std::vector<CStr> strs;
  strs.push_back("    1    2    2    1    2              0   19.5989   1.47433        18        20");//1
  strs.push_back("    1    4    3    5    2              0   19.2745   2.22035        18        20");//11
  strs.push_back("         1         0     0.001");//24
  strs.push_back("    1    2");//34
  strs.push_back("    3    4");//36
  strs.push_back("    1");//43

  // now check the contents of the file
  unsigned int cnt(0);
  EReadAsciiFile e(tmpFile);
  while (e.GetLine(&line))
  {
    if (cnt<6)
    {
      if (e.GetLineCount() == lnNum[cnt])
      {
        TS_ASSERT(strs.at(cnt) == line);
        cnt++;
      }
    }
  }
  e.CloseFile();
  TS_ASSERT(remove(tmpFile) == 0);
  H5Reader::CloseAllH5Files();
}

#endif

//------------------------------------------------------------------------------
// FILE      ProcessorSFR2.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/Gms2Mf2k/Files/Processor/ProcessorSFR2.h>

#include <fstream>
#include <sstream>

#include <ModflowLib.h>
#include <private/Sfr2Reader.h>
#include <private/util/EReadAsciiFile.h>

class ProcessorSFR2::impl
{
friend ProcessorSFR2T;
public:
  impl();
  bool DoConvertFile(const char * const a_inputFile,
                     const char * const a_outputFile,
                     const int &a_nRow,
                     const int &a_nCol,
                     bool a_unstructured);

  bool ReadSFR2Reaches(const CStr &line,
                       const int &a_nRow,
                       const int &a_nCol);
  bool WriteSFR2Reaches(FILE *a_fp, bool a_unstructured);

  bool ReadStreamData(const CStr &a_line);
  bool WriteStreamData(FILE *a_fp);

  template <class T>
  CStr StrVal(const T& a_)
  {
    CStr s;
    s.Format("%s ", STR(a_));
    return s;
  }
  
  CStr Iseg(int a_i, int a_j);
  CStr Iotsg(int a_i);
  CStr Idivar(int a_i, int a_j);
  CStr Idivar(int a_indexTwo, int a_indexOneFrom, int a_indexOneTo);
  CStr Seg(int a_i, int a_j);
  CStr Seg(int a_indexTwo, int a_indexOneFrom, int a_indexOneTo);
  CStr Xsec(int a_indexTwo, int a_indexOneFrom, int a_indexOneTo);
  CStr Qstage(int a_indexTwo, int a_indexOneFrom, int a_indexOneTo);

private:
  int               m_nstrm, m_nss, m_isfropt, m_stressPeriod, m_sz;
  std::vector<int>  m_krch, m_irch, m_jrch, m_jseg, m_ireach, m_iseg, m_iotsg,
                    m_idivar;
  std::vector<Real> m_strm, m_seg, m_xsec, m_qstage;
};

///////////////////////////////////////////////////////////////////////////////
/// \brief Constructor
///////////////////////////////////////////////////////////////////////////////
ProcessorSFR2::ProcessorSFR2 (const char * const a_inputFile,
                              const char * const a_outputFile,
                              const int& a_nRow,
                              const int& a_nCol,
                              bool a_unstructured) :
Processor(a_inputFile, a_outputFile, a_nRow, a_nCol, a_unstructured),
m_p(new ProcessorSFR2::impl())
{
} // ProcessorSFR2::ProcessorSFR2
///////////////////////////////////////////////////////////////////////////////
/// \brief Destructor
///////////////////////////////////////////////////////////////////////////////
ProcessorSFR2::~ProcessorSFR2 ()
{
  try
  {
    if (m_p)
      delete(m_p);
  }
  catch (...)
  {
  }
} // ProcessorSFR2::~ProcessorSFR2
///////////////////////////////////////////////////////////////////////////////
/// \brief Private virtual function that actually does the conversion
///////////////////////////////////////////////////////////////////////////////
bool ProcessorSFR2::DoConvertFile ()
{
  return(m_p->DoConvertFile(InputFile(),
                            OutputFile(),
                            NumRow(),
                            NumCol(),
                            Unstructured()));
} // ProcessorSFR2::DoConvertFile


///////////////////////////////////////////////////////////////////////////////
/// \brief Constructor
///////////////////////////////////////////////////////////////////////////////
ProcessorSFR2::impl::impl () :
m_nstrm(0),
m_nss(0),
m_isfropt(0),
m_stressPeriod(0),
m_sz(26) // we can just leave this as 26 because this is our processor code to convert to txt
{
} // ProcessorSFR2::impl::impl
///////////////////////////////////////////////////////////////////////////////
/// \brief Private virtual function that actually does the conversion
///////////////////////////////////////////////////////////////////////////////
bool ProcessorSFR2::impl::DoConvertFile (const char * const a_inputFile,
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
      else if (str == "GMS_HDF5_SFR2_REACH")
      {
        if (ReadSFR2Reaches(line, a_nRow, a_nCol))
        {
          WriteSFR2Reaches(fp, a_unstructured);
        }
      }
      else if (str == "GMS_HDF5_01")
      {
        if (ReadStreamData(line))
        {
          WriteStreamData(fp);
        }
      }
      else
      {
        // echo the line to the output file
        fprintf(fp, "%s\n", line.c_str());
        if (firstTime)
        {
          Real ignoredReal;
          int  ignoredInt;
          firstTime = false;
          e.SetLine(line);
          // we need to get the NTRIB, NDIV, & ICALC
          e.ReadData(m_nstrm); // NSTRM
          e.ReadData(m_nss); // NSS
          e.ReadData(ignoredInt); // NSFRPAR
          e.ReadData(ignoredInt); // NPARSEG
          e.ReadData(ignoredReal); // CONST
          e.ReadData(ignoredReal); // DELAK
          e.ReadData(ignoredInt); // ISTCB1
          e.ReadData(ignoredInt); // ISTCB2
          if (m_nstrm < 0)
          {
            m_nstrm = -m_nstrm;
            e.ReadData(m_isfropt);
          }
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
} // ProcessorSFR2::impl::DoConvertFile
//------------------------------------------------------------------------------
/// \brief Reads the sfr2 reach data from the h5 file
//------------------------------------------------------------------------------
bool ProcessorSFR2::impl::ReadSFR2Reaches (const CStr &a_line,
                                           const int &a_nRow,
                                           const int &a_nCol)
{
  CStr line(a_line);
  if (m_nstrm < 1 || m_nss < 1)
    return false;
  
  m_krch.assign(m_nstrm, 0);
  m_irch.assign(m_nstrm, 0);
  m_jrch.assign(m_nstrm, 0);
  m_jseg.assign(m_nstrm, 0);
  m_ireach.assign(m_nstrm, 0);
  m_strm.assign(24*m_nstrm, 0);
  for (int i = 0; i < m_nstrm; ++i)
  {
    sfr2::GetReachData(i+1, a_nRow, a_nCol, m_krch[i], m_irch[i],
                       m_jrch[i], m_jseg[i], m_ireach[i], &m_strm[0], 24, line);
  }

  return true;
} // ProcessorSFR2::impl::ReadSFR2Reaches
//------------------------------------------------------------------------------
/// \brief Reads the sfr2 reach data from the h5 file
//------------------------------------------------------------------------------
bool ProcessorSFR2::impl::WriteSFR2Reaches (FILE *a_fp,
                                            bool a_unstructured)
{
  if (!a_fp)
    return false;
  try
  {
    for (int i = 0; i < m_nstrm; ++i)
    {
      if (a_unstructured)
      {
        fprintf(a_fp, "%d %d %d %s\n", m_jrch[i],
                                       m_jseg[i], m_ireach[i],
                                       STR(m_strm[i*24]).c_str());
      }
      else
      {
        fprintf(a_fp, "%d %d %d %d %d %s\n", m_krch[i], m_irch[i], m_jrch[i],
                                             m_jseg[i], m_ireach[i],
                                             STR(m_strm[i*24]).c_str());
      }
    }
  }
  catch (std::out_of_range &)
  {
    return false;
  }
  return true;
} // ProcessorSFR2::impl::WriteSFR2Reaches
//------------------------------------------------------------------------------
/// \brief Reads the stream data from the h5 file.
//------------------------------------------------------------------------------
bool ProcessorSFR2::impl::ReadStreamData (const CStr &a_line)
{
  CStr line(a_line);
  if (m_nstrm < 1 || m_nss < 1)
    return false;

  if (m_iseg.empty())
    m_iseg.assign(4*m_nss, 0);
  if (m_iotsg.empty())
    m_iotsg.assign(m_nss, 0);
  if (m_idivar.empty())
    m_idivar.assign(2*m_nss, 0);
  if (m_seg.empty())
    m_seg.assign(m_sz*m_nss, 0);
  if (m_xsec.empty())
    m_xsec.assign(16*m_nss, 0);
  if (m_qstage.empty())
    m_qstage.assign(150*m_nss, 0);

  // get stress period
  size_t kperStart = a_line.find_last_of(" ");
  if (kperStart != std::string::npos)
    sscanf(a_line.c_str() + kperStart, "%d", &m_stressPeriod);

  sfr2::GetSegData(m_nss, m_stressPeriod, &m_iseg[0], &m_iotsg[0], &m_idivar[0],
                   &m_seg[0], &m_xsec[0], &m_qstage[0], line);

  return true;
} // ProcessorSFR2::impl::ReadStreamData
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr ProcessorSFR2::impl::Iseg (int a_i,
                                int a_j)
{
  using namespace util;
  CStr s;
  s.Format("%d ", ForElement(m_iseg, a_i, a_j, 4));
  return s;
} // ProcessorSFR2::impl::Iseg
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr ProcessorSFR2::impl::Iotsg (int a_i)
{
  CStr s;
  s.Format("%d ", m_iotsg[a_i-1]);
  return s;
} // ProcessorSFR2::impl::Iotsg
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr ProcessorSFR2::impl::Idivar (int a_i,
                                  int a_j)
{
  using namespace util;
  CStr s;
  s.Format("%d ", ForElement(m_idivar, a_i, a_j, 2));
  return s;
} // ProcessorSFR2::impl::Idivar
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr ProcessorSFR2::impl::Idivar (int a_indexTwo,
                                  int a_indexOneFrom,
                                  int a_indexOneTo)
{
  using namespace util;
  CStr line;
  for (int i = a_indexOneFrom; i <= a_indexOneTo; ++i)
  {
    line += StrVal(ForElement(m_idivar, i, a_indexTwo, 2));
  }
  return line;
} // ProcessorSFR2::impl::Idivar
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr ProcessorSFR2::impl::Seg (int a_i,
                               int a_j)
{
  using namespace util;
  return StrVal(ForElement(m_seg, a_i, a_j, m_sz));
} // ProcessorSFR2::impl::Seg
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr ProcessorSFR2::impl::Seg (int a_indexTwo,
                               int a_indexOneFrom,
                               int a_indexOneTo)
{
  using namespace util;
  CStr line;
  for (int i = a_indexOneFrom; i <= a_indexOneTo; ++i)
  {
    line += StrVal(ForElement(m_seg, i, a_indexTwo, m_sz));
  }
  return line;
} // ProcessorSFR2::impl::Seg
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr ProcessorSFR2::impl::Xsec (int a_indexTwo,
                                int a_indexOneFrom,
                                int a_indexOneTo)
{
  using namespace util;
  CStr line;
  for (int i = a_indexOneFrom; i <= a_indexOneTo; ++i)
  {
    line += StrVal(ForElement(m_xsec, i, a_indexTwo, 16));
  }
  return line;
} // ProcessorSFR2::impl::Xsec
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
CStr ProcessorSFR2::impl::Qstage (int a_indexTwo,
                                  int a_indexOneFrom,
                                  int a_indexOneTo)
{
  using namespace util;
  CStr line;
  for (int i = a_indexOneFrom; i <= a_indexOneTo; ++i)
  {
    line += StrVal(ForElement(m_qstage, i, a_indexTwo, 16));
  }
  return line;
} // ProcessorSFR2::impl::Qstage
///////////////////////////////////////////////////////////////////////////////
/// \brief Writes the stream data from to the str file.
///////////////////////////////////////////////////////////////////////////////
bool ProcessorSFR2::impl::WriteStreamData (FILE *a_fp)
{
  using namespace util;

  if (!a_fp)
    return false;

  try
  {
    for (int i = 0; i < m_nss; ++i)
    {
      CStr line;

      // setup variables so the following code more closely matches the
      // FORTRAN source
      int nseg = i+1;
      int icalc = ForElement(m_iseg, 1, nseg, 4);
      int iupseg = ForElement(m_idivar, 1, nseg, 2);
      int isfropt = m_isfropt;
      int kper = m_stressPeriod;

      // READ DATA SET 4B OR 6A FOR SEGMENTS THAT ARE NOT DIVERSIONS.
      if (iupseg <= 0)
      {
        if (icalc <= 0)
          line = StrVal(nseg) + Iseg(1, nseg) + Iotsg(nseg) + Idivar(1, nseg) +
                 Seg(nseg, 2, 5);
        else if (icalc == 1)
          line = StrVal(nseg) + Iseg(1, nseg) + Iotsg(nseg) + Idivar(1, nseg) +
                 Seg(nseg, 2, 5) + Seg(16, nseg);
        else if (icalc == 2)
          line = StrVal(nseg) + Iseg(1, nseg) + Iotsg(nseg) +
                 Idivar(1, nseg) + Seg(nseg, 2, 5) +
                 Seg(nseg, 16, 17);
        else if (icalc == 3)
          line = StrVal(nseg) + Iseg(1, nseg) + Iotsg(nseg) +
                 Idivar(1, nseg) + Seg(nseg, 2, 5) +
                 Seg(9, nseg) + Seg(10, nseg) + Seg(14, nseg) +
                 Seg(15, nseg);
        else if (icalc == 4)
          line = StrVal(nseg) + Iseg(1, nseg) + Iotsg(nseg) +
                 Idivar(1, nseg) + Iseg(2, nseg) +
                 Seg(nseg, 2, 5);
      }
      else if (icalc <= 0)
        line = StrVal(nseg) + Iseg(1, nseg) + Iotsg(nseg) +
               Idivar(nseg, 1, 2) +
               Seg(nseg, 2, 5);
      else if (icalc == 1)
        line = StrVal(nseg) + Iseg(1, nseg) + Iotsg(nseg) +
               Idivar(nseg, 1, 2) +
               Seg(nseg, 2, 5) + Seg(16, nseg);
      else if (icalc == 2)
        line = StrVal(nseg) + Iseg(1, nseg) + Iotsg(nseg) +
               Idivar(nseg, 1, 2) +
               Seg(nseg, 2, 5) +
               Seg(nseg, 16, 17);
      else if (icalc == 3)
        line = StrVal(nseg) + Iseg(1, nseg) + Iotsg(nseg) +
               Idivar(nseg, 1, 2) +
               Seg(nseg, 2, 5) + Seg(9, nseg) +
               Seg(10, nseg) + Seg(14, nseg) + Seg(15, nseg);
      else if (icalc == 4)
        line = StrVal(nseg) + Iseg(1, nseg) + Iotsg(nseg) +
               Idivar(nseg, 1, 2) + Iseg(2, nseg) +
               Seg(nseg, 2, 5);
      fprintf(a_fp, "%s\n", line.c_str());

      // READ DATA SET 4C OR 6B.
      line = "";
      //line.clear();
      if (isfropt == 0)
      {
        if (icalc <= 0)
          line = Seg(nseg, 6, 10);
        else if (icalc == 1)
          line = Seg(nseg, 6, 9);
        else if (icalc >= 2 && icalc <= 4)
          line = Seg(nseg, 6, 8);
      }
      else if (isfropt == 1)
      {
        if (icalc <= 0)
          line = Seg(9, nseg) + Seg(10, nseg);
        else if (icalc == 1)
          line = Seg(9, nseg);
      }
      else if (isfropt == 2 || isfropt == 3)
      {
        if (icalc <= 0)
          line = Seg(9, nseg) + Seg(10, nseg);
        else if (icalc == 1 && kper == 1)
          line = Seg(9, nseg);
      }
      else if (isfropt == 4)
      {
        if (icalc <= 0)
          line = Seg(nseg, 6, 10);
        else if (icalc == 1)
          if (kper == 1)
            line = Seg(nseg, 6, 9) +
                   Seg(nseg, 18, 20);
          else
            line = Seg(6, nseg);
        else if (icalc == 2)
          if (kper == 1)
            line = Seg(nseg, 6, 8) +
                   Seg(nseg, 18, 20);
          else
            line = Seg(6, nseg);
        else if (icalc >= 3 && icalc <= 4)
          line = Seg(nseg, 6, 8);
      }
      else if (isfropt == 5)
      {
        if (icalc <= 0)
          line = Seg(nseg, 6, 10);
        else if (icalc == 1)
          if (kper == 1)
            line = Seg(nseg, 6, 9) +
                   Seg(nseg, 18, 21);
          else
            line = Seg(6, nseg);
        else if (icalc == 2)
          if (kper == 1)
            line = Seg(nseg, 6, 8) +
                   Seg(nseg, 18, 21);
          else
            line = Seg(6, nseg);
        else if (icalc >= 3 && icalc <= 4)
          line = Seg(nseg, 6, 8);
      }
      if (line != "")
        fprintf(a_fp, "%s\n", line.c_str());


      // READ DATA SET 4D OR 6C.
      line = "";
      //line.clear();
      if (isfropt == 0)
      {
        if (icalc <= 0)
          line = Seg(nseg, 11, 15);
        else if (icalc == 1)
          line = Seg(nseg, 11, 14);
        else if (icalc >= 2 && icalc <= 4)
          line = Seg(nseg, 11, 13);
      }
      else if (isfropt == 1)
      {
        if (icalc <= 0)
          line = Seg(14, nseg) + Seg(15, nseg);
        else if (icalc == 1)
          line = Seg(14, nseg);
      }
      else if (isfropt == 2)
      {
        if (icalc <= 0)
          line = Seg(14, nseg) + Seg(15, nseg);
        else if (icalc == 1 && kper == 1)
          line = Seg(14, nseg);
      }
      else if (isfropt == 3)
      {
        if (icalc <= 0)
          line = Seg(14, nseg) + Seg(15, nseg);
        else if (icalc == 1 && kper == 1)
          line = Seg(14, nseg);
      }
      else if (isfropt == 4)
      {
        if (icalc <= 0)
          line = Seg(nseg, 11, 15);
        else if (icalc == 1)
          if (kper == 1)
            line = Seg(nseg, 11, 14) +
                   Seg(nseg, 22, 24);
          else
            line = Seg(11, nseg);
        else if (icalc == 2)
          if (kper == 1)
            line = Seg(nseg, 11, 13) +
                   Seg(nseg, 22, 24);
          else
            line = Seg(11, nseg);
        else if (icalc >= 3 && icalc <= 4)
          line = Seg(nseg, 11, 13);
      }
      else if (isfropt == 5)
      {
        if (icalc <= 0)
          line = Seg(nseg, 11, 15);
        else if (icalc == 1)
          if (kper == 1)
            line = Seg(nseg, 11, 14) +
                   Seg(nseg, 22, 25);
          else
            line = Seg(11, nseg);
        else if (icalc == 2)
          if (kper == 1)
            line = Seg(nseg, 11, 13) +
                   Seg(nseg, 22, 25);
          else
            line = Seg(11, nseg);
        else if (icalc >= 3 && icalc <= 4)
          line = Seg(nseg, 11, 13);
      }
      if (line != "")
        fprintf(a_fp, "%s\n", line.c_str());


      // READ DATA SET 4E OR 6D FOR SEGMENT WHEN ICALC IS 2.
      line = "";
      //line.clear();
      if (icalc == 2)
      {
        if (kper == 1 || isfropt <= 1)
        {
          line = Xsec(nseg, 1, 8) + "\n";
          line += Xsec(nseg, 9, 16);
        }
      }
      if (line != "")
        fprintf(a_fp, "%s\n", line.c_str());

      // READ DATA SET 4F OR 6E FOR SEGMENT WHEN ICALC IS 4.
      line = "";
      //line.clear();
      if (icalc == 4)
      {
        int nstrpts = ForElement(m_iseg, 2, nseg, 4);
        line += Qstage(nseg, 1, nstrpts) + "\n";
        line += Qstage(nseg, nstrpts+1, 2*nstrpts) + "\n";
        line += Qstage(nseg, 2*nstrpts+1, 3*nstrpts);
      }
      if (line != "")
        fprintf(a_fp, "%s\n", line.c_str());
    }
  }
  catch (std::out_of_range &)
  {
    return false;
  }
  return true;
} // ProcessorSFR2::impl::WriteStreamData

#ifdef CXX_TEST
///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////

#include <private/Gms2Mf2k/Files/Processor/ProcessorSFR2.t.h>
#include <private/H5DataReader/H5DataSetReader.h>
#include <private/MfLibAsserts.h>
#include <private/MfData/MfGlobal.h>

//------------------------------------------------------------------------------
void ProcessorSFR2T::testCreateClass () const
{
  ProcessorSFR2 *p;
  try
  {
    p = new ProcessorSFR2("stuff", "stuff1", 1, 1, false);
    delete(p);
  }
  catch (std::bad_alloc&)
  {
    TS_FAIL("Allocation failed");
  }
}
//------------------------------------------------------------------------------
void ProcessorSFR2T::testDoConvertFile () const
{
  CStr path;
  util::GetTestFilesDirectory(path);
  path += "\\Gms2Mf2k\\sfr2\\test1ss";

  // get from file path
  CStr fromFile(path);
  fromFile += "\\test1ss.sfr";
  
  // get to file path
  CStr tmpFile;
  util::GetTempDirectory(tmpFile);
  tmpFile += "\\tmpStr.sfr";

  TS_ASSERT(util::SetCurrentDirectory(path));

  MfData::Get().SetUnstructured(0);
  ProcessorSFR2 p(fromFile, tmpFile, 15, 10, false);
  TS_ASSERT(p.DoConvertFile());

  // test the contents of the output file
  CStr baseFile(path);
  if (sizeof(Real) == sizeof(float))
    baseFile += "\\base-float.sfr";
  else
    baseFile += "\\base-double.sfr";
  TS_ASSERT_TXT_FILES_EQUAL(tmpFile, baseFile);
  TS_ASSERT(remove(tmpFile) == 0);
  H5Reader::CloseAllH5Files();
}

#endif

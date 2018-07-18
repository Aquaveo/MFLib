//------------------------------------------------------------------------------
// FILE      Processor.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/Gms2Mf2k/Files/Processor/Processor.h>

#include <fstream>
#include <set>
//#include <sstream>

#include <ModflowLib.h>
#include <private/ArrayReader.h>
#include <private/ArrayReader/ArrayReaderParser.h>
#include <private/H5DataReader/H5DataSetReader.h>
#include <private/ListReader/ListReaderH5.h>
#include <private/util/EReadAsciiFile.h>
#include <private/util/util.h>

#define NUMBCS    "00. Number of BCs"
#define PROPERTY  "07. Property"

class Processor::impl
{
public:
  impl(const char* const a_inputFile,
       const char* const a_outputPath,
       const int& a_nRow,
       const int& a_nCol,
       bool a_unstructured);

  bool ValidSetUp() const;
  bool DoConvertFile() const;
  template <class T>
  bool ReadArray(std::vector<T> &a_d,
                 T& a_mult,
                 int &a_iprn,
                 const CStr &a_line,
                 int &a_size) const;
  bool ReadArray1(std::vector<double> &a_d,
                  int a_size,
                  int &a_iprn,
                  const CStr &a_line) const;
  bool ReadArray1(std::vector<int> &a_d,
                  int a_size,
                  int &a_iprn,
                  const CStr &a_line) const;
  template <class T>
  bool WriteArray(std::ofstream &a_os,
                  std::vector<T> &a_d,
                  int a_size,
                  T& a_mult,
                  int a_iprn) const;
  bool ReadList(const char* const a_fName,
                const char* const a_gName,
                std::vector<double> &a_f,
                int &a_nList,
                int &a_lDim,
                const CStr &a_line,
                const std::vector<CStr>& a_auxNames,
                std::vector<CStr>* a_vComments) const;
  bool WriteList(std::ofstream &a_os,
                 const int& a_nList,
                 const int& a_lDim,
                 const std::vector<CStr>& a_auxNames,
                 std::vector<double> &a_f) const;
  std::set<int> IntegerListItems() const;
  const char * const InputFile() const {return(m_inputFile.c_str());}
  const char * const OutputFile() const {return(m_outputFile.c_str());}
  int Row() const {return(m_nRow);}
  int Col() const {return(m_nCol);}
  bool Unstructured() const {return(m_unstructured);}

private:
  impl();
  CStr m_inputFile, m_outputFile;
  int m_nRow, m_nCol;
  bool m_unstructured;
};

///////////////////////////////////////////////////////////////////////////////
/// \brief Constructor
///////////////////////////////////////////////////////////////////////////////
Processor::Processor (const char* const a_inputFile,
                      const char* const a_outputFile,
                      const int& a_nRow,
                      const int& a_nCol,
                      bool a_unstructured) :
m_p(new Processor::impl(a_inputFile, a_outputFile, a_nRow, a_nCol, a_unstructured))
{
} // Processor::Processor
///////////////////////////////////////////////////////////////////////////////
/// \brief Denstructor
///////////////////////////////////////////////////////////////////////////////
Processor::~Processor ()
{
  try
  {
    if (m_p)
      delete(m_p);
  }
  catch (...) {}
} // Processor::~Processor
///////////////////////////////////////////////////////////////////////////////
/// \brief Converts the file
///////////////////////////////////////////////////////////////////////////////
bool Processor::ConvertFile ()
{
  // check that the setup is valid
  if (!m_p->ValidSetUp())
    return false;

  return(DoConvertFile());
} // Processor::ConvertFile
///////////////////////////////////////////////////////////////////////////////
/// \brief Gets the string for the input file
///////////////////////////////////////////////////////////////////////////////
const char* const Processor::InputFile () const
{
  return(m_p->InputFile());
} // Processor::InputFile
///////////////////////////////////////////////////////////////////////////////
/// \brief Gets the string for the output file
///////////////////////////////////////////////////////////////////////////////
const char* const Processor::OutputFile () const
{
  return(m_p->OutputFile());
} // Processor::InputFile
///////////////////////////////////////////////////////////////////////////////
/// \brief Gets the number of rows in the model grid
///////////////////////////////////////////////////////////////////////////////
int Processor::NumRow () const
{
  return(m_p->Row());
} // Processor::NumRow
///////////////////////////////////////////////////////////////////////////////
/// \brief Gets the number of columns in the model grid
///////////////////////////////////////////////////////////////////////////////
int Processor::NumCol () const
{
  return(m_p->Col());
} // Processor::NumCol
///////////////////////////////////////////////////////////////////////////////
/// \brief Gets if the model is for an unstructured grid
///////////////////////////////////////////////////////////////////////////////
bool Processor::Unstructured () const
{
  return(m_p->Unstructured());
} // Processor::Unstructured
///////////////////////////////////////////////////////////////////////////////
/// \brief Private virtual function that actually does the conversion
///////////////////////////////////////////////////////////////////////////////
bool Processor::DoConvertFile ()
{
 return (m_p->DoConvertFile());
} // Processor::DoConvertFile

///////////////////////////////////////////////////////////////////////////////
/// \brief Constructor
///////////////////////////////////////////////////////////////////////////////
Processor::impl::impl (const char * const a_inputFile,
                       const char * const a_outputFile,
                       const int& a_nRow,
                       const int& a_nCol,
                       bool a_unstructured)
: m_inputFile(a_inputFile)
, m_outputFile(a_outputFile)
, m_nRow(a_nRow)
, m_nCol(a_nCol)
, m_unstructured(a_unstructured)
{
  CStr path, file;
  util::StripFileFromFilename(m_outputFile, path);
  util::StripPathFromFilename(m_outputFile, file);
  file.Replace(" ", "_");
  if (!path.empty())
    path += "\\";
  m_outputFile.Format("%s%s", path, file);
} // Processor::impl::impl
///////////////////////////////////////////////////////////////////////////////
/// \brief sees if the inputs to the class are valid
///////////////////////////////////////////////////////////////////////////////
bool Processor::impl::ValidSetUp () const
{
  // see if the input file exists
  FILE *fp(fopen(m_inputFile, "r"));
  if (!fp)
    return false;
  fclose(fp);

  CStr str(m_outputFile);
  fp = fopen(str, "w");
  if (!fp)
    return false;
  fclose(fp);
  if (remove(str) != 0)
    return false;
  if (m_nRow <= 0 ||
      m_nCol <= 0)
    return false;
  return true;
} // Processor::impl::ValidSetUp
///////////////////////////////////////////////////////////////////////////////
/// \brief Convert the file
///////////////////////////////////////////////////////////////////////////////
bool Processor::impl::DoConvertFile () const 
{
  bool rval(false);

  try
  {
    //lint --e{534} I am catching exceptions
    EReadAsciiFile e(m_inputFile);
    e.UseExceptions();
    if (!e.OpenFile())
      throw ioError("Unable to open input file: " + m_inputFile + ".");

    std::ofstream os(m_outputFile);
    if (!os)
      throw ioError("Unable to open output file: " + m_outputFile + ".");

    CStr ext;
    util::StripAllButExtension(m_outputFile, ext);

    // read the input file
    std::vector<double> d;
    std::vector<double> vecF;
    std::vector<CStr> auxNames, vComments;
    CStr str, line, lineLower, h5FileName, h5GroupName, first2Lines;
    while (e.GetLine(&line))
    {
      lineLower = line;
      lineLower.MakeLower();
      CStr trimmedLine = line;
      trimmedLine.Trim();
      if (trimmedLine != "")
        e.ReadData(str);
      else
        str = "";
      if (str == "#GMS_HDF5_01")
      {
        // don't echo this to the file
      }
      else if (str == "HDF5")
      {
        e.ReadData(str);
        e.ReadData(str);
        e.ReadData(str);
        e.ReadData(str);
        double dMult;
        int iprn, iMult;
        if (str.find("ibound") == -1 &&
            str.find("09. Layer") == -1 &&
            str.find("DISU_IAC") == -1 &&
            str.find("DISU_JA") == -1 &&
            str.find("DISU_NODELAY") == -1)
        {
          int size;
          if (ReadArray(d, dMult, iprn, line, size))
          {
            WriteArray(os, d, size, dMult, iprn);
          }
        }
        else
        {
          std::vector<int> di;
          int size;
          if (ReadArray(di, iMult, iprn, line, size))
          {
            WriteArray(os, di, size, iMult, iprn);
          }
        }
      }
      else if (str == "GMS_HDF5_01")
      {
        // get the h5 file and group
        e.ReadData(h5FileName);
        e.ReadData(h5GroupName);
        int nList, lDim;
        if (ReadList(h5FileName,
                     h5GroupName,
                     vecF,
                     nList,
                     lDim,
                     line,
                     auxNames,
                     !first2Lines.empty() ? &vComments : 0))
        {
          if (!first2Lines.IsEmpty())
          {
            if (vComments.size())
            {
              os << "#           CoverageGUID ObjectType ID X Y\n";
            }
            for (size_t q=0; q<vComments.size(); q++)
            {
              os << "#GMSCOMMENT " << vComments[q] << "\n";
            }
            os << first2Lines << "\n";
            first2Lines = "";
          }
          WriteList(os,
                    nList,
                    lDim,
                    auxNames,
                    vecF);
        }
      }
      else if (ext.CompareNoCase("mfn") == 0)
      {
        if (str.at(0) == '#')
          os << line << "\n";
        else
        {
          if (str.CompareNoCase("asp") == 0)
            os << "#";

          int cnt(19);
          CStr unit, fname;
          e.ReadData(unit);
          e.ReadData(fname);
          fname.Replace(" ", "_");
          os << str;
          while (cnt - str.length())
          {
            os << " ";
            cnt--;
          }
          os << unit << "  ";
          if (unit.GetLength() < 2)
            os << " ";
          os << fname << "\n";
        }
      }
      else if (ext.CompareNoCase("lmt") == 0)
      {
        if (str.CompareNoCase("OUTPUT_FILE_NAME") == 0)
        {
          CStr fname;
          e.ReadData(fname);
          fname.Replace(" ", "_");
          fname.Replace("\"", "");
          os << str << " " << fname << "\n";
        }
        else
          os << line << "\n";
      }
      else
      {
        // echo the line to the output file
        if (lineLower.Find("aux") != -1)
        {
          first2Lines = line;
          auxNames.clear();
          try
          {
            while (e.ReadData(str))
            {
              if (str.CompareNoCase("aux") == 0)
              {
                e.ReadData(str);
                if (!str.IsEmpty())
                {
                  auxNames.push_back(str);
                }
              }
            }
          }
          catch (ioexception&) {}
          if (auxNames.empty())
          {
            first2Lines = "";
            os << line << "\n";
          }
        }
        else if (!first2Lines.IsEmpty())
        {
          first2Lines += "\n";
          first2Lines += line;
        }
        else
        {
          os << line << "\n";
        }
      }
    }

    rval = true;
  }
  catch (ioexception&)
  {
    CStr msg("Error processing file: ");
    msg += m_inputFile;
    msg += ".";
    ErrorStack::Get().PutError(msg);
    rval = false;
  }
  return rval;
} // Processor::impl::DoConvertFile
///////////////////////////////////////////////////////////////////////////////
/// \brief Reads a 2D array from the h5 file
///////////////////////////////////////////////////////////////////////////////
template <class T>
bool Processor::impl::ReadArray (std::vector<T> &a_d,
                                 T& a_mult,
                                 int &a_iprn,
                                 const CStr &a_line,
                                 int &a_size) const
{
  ArrayReaderParser parser(a_line);
  a_size = parser.GetArraySize();
  if ((int)a_d.size() < a_size)
  {
    a_d.assign(a_size, 0);
  }
  CStr lineCopy(a_line);
  if (ReadArray1(a_d, a_size, a_iprn, a_line))
  {
    // get the multiplier
    a_mult = 1;
    ArrayReader reader(lineCopy);
    if (reader.ValidInputString())
    {
      a_mult = (T)reader.GetMultiplier();
    }

    // divide the array by the multiplier
    for (size_t i=0; i<a_d.size(); i++)
    {
      if (a_mult != 0)
        a_d[i] /= a_mult;
    }
    return true;
  }
  return false;
} // Processor::impl::ReadArray
bool Processor::impl::ReadArray1 (std::vector<double> &a_d,
                                  int a_size,
                                  int &a_iprn,
                                  const CStr &a_line) const
{
  // read the array
  int success;
  int nCol=a_size, nRow=1;
  MFLIB_U2DDBL(&success,
               &a_iprn,
               &nCol,
               &nRow,
               &a_d.at(0),
               a_line.c_str(),
               a_line.GetLength());
  return (success ? 1 : 0);
} // Processor::impl::ReadArray1
bool Processor::impl::ReadArray1 (std::vector<int> &a_d,
                                  int a_size,
                                  int &a_iprn,
                                  const CStr &a_line) const
{
  // read the array
  int success;
  int nCol=a_size, nRow=1;
  MFLIB_U2DINT(&success,
               &a_iprn,
               &nCol,
               &nRow,
               &a_d.at(0),
               a_line.c_str(),
               a_line.GetLength());
  return (success ? 1 : 0);
} // Processor::impl::ReadArray1
///////////////////////////////////////////////////////////////////////////////
/// \brief Write a 2D array to the file
///////////////////////////////////////////////////////////////////////////////
template <class T>
bool Processor::impl::WriteArray (std::ofstream &a_os,
                                  std::vector<T> &a_d,
                                  int a_size,
                                  T& a_mult,
                                  int a_iprn) const
{
  if (!a_os)
    return false;

  if ((int)a_d.size() < a_size)
    return false;

  a_os << "INTERNAL " << a_mult << " (free) " << a_iprn << "\n";
  int numToWritePerRow(10);

  if (m_nCol < 10)
    numToWritePerRow = m_nCol;

  int rowCount(0);
  for (int i = 0; i < a_size; ++i)
  {
    // write the value to the file
    a_os << STR(a_d.at(i)) << " ";

    // see if we need to start a new line
    rowCount++;
    if (rowCount % numToWritePerRow == 0 ||
        rowCount % m_nCol == 0 ||
        i+1 == a_size)
    {
      if (rowCount == m_nCol)
        rowCount = 0;
      a_os << "\n";
    }
  }

  return true;
} // Processor::impl::WriteArray
///////////////////////////////////////////////////////////////////////////////
/// \brief Reads list data into the vector
///////////////////////////////////////////////////////////////////////////////
bool Processor::impl::ReadList (const char * const a_fName,
                                const char * const a_gName,
                                std::vector<double> &a_f,
                                int &a_nList,
                                int &a_lDim,
                                const CStr &a_line,
                                const std::vector<CStr>& a_auxNames,
                                std::vector<CStr>* a_vComments) const 
{
  int nAux((int)a_auxNames.size());
  std::map<CStr, int> mapBcNumFields;
  mapBcNumFields["Drain"] = 2;
  mapBcNumFields["Drain Return"] = 6;
  mapBcNumFields["General Head"] = 2;
  mapBcNumFields["River"] = 3;
  mapBcNumFields["Specified Head"] = 2;
  mapBcNumFields["Well"] = 1;
  std::map<CStr, int>::iterator it(mapBcNumFields.find(a_gName));

  if (it == mapBcNumFields.end())
  {
    //ASSERT(0);
    return false;
  }

  const int nBcFields(it->second);

  // do this the first time
  if (a_f.empty())
  {
    // get the number of BCs
    CStr path;
    std::pair<int, int> myPair(0,1);
    VEC_INT_PAIR indices(1, myPair);
    path.Format("%s/%s", a_gName, NUMBCS);
    H5DataSetReader r(a_fName, path, indices);
    if (!r.GetData(&a_nList, 1))
      throw ioError("Unable to read number of BCs.");

    a_lDim = nBcFields + 3 + nAux;

    a_f.assign(a_nList*a_lDim, 0);
  }

  // take care of aux names
  bool cellgrp(0);
  int i;
  char auxNames[5*16];
  for (int i=0; i<80; i++)
    auxNames[i] = '\0';
  for (i=0; i<(int)a_auxNames.size(); i++)
  {
    if (a_auxNames.at(i).CompareNoCase("cellgrp") == 0)
    {
      cellgrp = true;
    }
    for (int j=0; j<a_auxNames.at(i).GetLength() && j<16; j++)
    {
      auxNames[(i*16)+j] = a_auxNames.at(i).GetAt(j);
    }
  }


  // read the data
  int success;
  const int ial(0);
  MFLIB_ULSTRD_DBL(&success,
                   &a_nList,
                   &a_lDim,
                   &ial,
                   &nAux,
                   auxNames,
                   0,
                   &m_nCol,
                   &m_nRow,
                   &a_f.at(0),
                   a_line.c_str(),
                   a_line.GetLength());
  // if we read CELLGRP
  if (cellgrp && a_vComments)
  {
    std::map<CStr, Real>& myMap(listReader_GetMapId());
    std::map<CStr, Real>::iterator it(myMap.begin());
    a_vComments->assign(myMap.size(), "");
    for (; it != myMap.end(); it++)
    {
      if (it->second >= 0 && it->second <= a_vComments->size())
      {
        a_vComments->at((int)(it->second)-1) = it->first;
      }
    }
  }

  return (success ? 1 : 0);
} // Processor::impl::ReadList
///////////////////////////////////////////////////////////////////////////////
/// \brief Writes list data to the file
///////////////////////////////////////////////////////////////////////////////
bool Processor::impl::WriteList (std::ofstream &a_os,
                                 const int& a_nList,
                                 const int& a_lDim,
                                 const std::vector<CStr>& a_auxNames,
                                 std::vector<double> &a_f) const
{
  if (!a_os)
    return false;

  // see if the vector is the correct size
  if (a_nList*a_lDim != static_cast<int>(a_f.size()))
    return false;

  // write the data
  size_t cnt(0);
  try
  {
    std::set<int> integerItems = IntegerListItems();
    for (int i=0; i<a_nList; i++)
    {
      int j;
      if (m_unstructured)
      {
        cnt += 2;
        a_os << static_cast<int>(a_f.at(cnt++)) << " ";
      }
      else
      {
        for (j=0; j<3; j++)
          a_os << static_cast<int>(a_f.at(cnt++)) << " ";
      }
      const int nFields(a_lDim-3-(int)a_auxNames.size());
      for (j=0; j<nFields; j++)
      {
        if (integerItems.find(j) != integerItems.end())
          a_os << static_cast<int>(a_f.at(cnt++)) << " ";
        else
          a_os << STR(a_f.at(cnt++)) << " ";
      }
      // AUX variables
      for (size_t jj=0;jj<a_auxNames.size(); jj++)
      {
        // write int if IFACE or CELLGRP
        if (0 == a_auxNames[jj].CompareNoCase("iface") ||
            0 == a_auxNames[jj].CompareNoCase("cellgrp"))
        {
          a_os << static_cast<int>(a_f.at(cnt++));
        }
        else
        {
          a_os << STR(a_f.at(cnt++));
        }
        if (jj < a_auxNames.size() - 1)
        {
          a_os << " ";
        }
      }
      a_os << "\n";
    }
  }
  catch (std::out_of_range&)
  {
    return false;
  }
  ASSERT(_CrtCheckMemory());
  return true;
} // Processor::impl::WriteList
///////////////////////////////////////////////////////////////////////////////
/// \brief Returns a set of indicees for list that should be written as integers
///////////////////////////////////////////////////////////////////////////////
std::set<int> Processor::impl::IntegerListItems() const
{
  std::set<int> intSet;
  CStr extension;
  util::StripAllButExtension(m_inputFile, extension);
  if (extension == "drt")
  {
    intSet.insert(2);
    intSet.insert(3);
    intSet.insert(4);
  }
  return intSet;
}

#ifdef CXX_TEST
///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////

#include <private/Gms2Mf2k/Files/Processor/Processor.t.h>

#include <sstream>

#include <private/MfLibAsserts.h>

//------------------------------------------------------------------------------
void ProcessorT::testCreateClass ()
{
  Processor *p = new Processor("stuff", "stuff1", 1, 1, false);
  TS_ASSERT(p);
  if (p)
    delete(p);
}
//------------------------------------------------------------------------------
void ProcessorT::testValidSetUp ()
{
  Processor p("stuff", "stuff1", 1, 1, false);
  TS_ASSERT(!p.m_p->ValidSetUp());
  CStr path, pathOut, pathOut1;
  util::GetTestFilesDirectory(path);
  util::GetTempDirectory(pathOut);
  pathOut += "\\run2.dis";
  pathOut1 = path;
  path += "\\mfInputs1\\run2.dis";

  Processor p1(path, pathOut1, 1, 1, false);
  TS_ASSERT(!p1.m_p->ValidSetUp());

  Processor p2(path, pathOut, 0, 1, false);
  TS_ASSERT(!p2.m_p->ValidSetUp());

  Processor p3(path, pathOut, 1, 0, false);
  TS_ASSERT(!p3.m_p->ValidSetUp());

  Processor p4(path, pathOut, 1, 1, false);
  TS_ASSERT(p4.m_p->ValidSetUp());
}
//------------------------------------------------------------------------------
void ProcessorT::testDoConvertFile ()
{
  ErrorStack::Get().ClearErrors();

  CStr wrongFile, inPath, inFile, outPath, outFile;

  util::GetTempDirectory(outPath);
  util::GetTestFilesDirectory(inPath);
  inPath += "\\mfInputs1";
  util::SetCurrentDirectory(inPath);
  inFile = inPath + "\\run2.dis";
  outFile = outPath + "\\run2.dis";

  // invalid input file
  {
    Processor p0(wrongFile, outFile, 300, 125, false);
    TS_ASSERT(!p0.m_p->DoConvertFile());
  }

  // invalid output file
  {
    Processor p0(inFile, wrongFile, 300, 125, false);
    TS_ASSERT(!p0.m_p->DoConvertFile());
  }

  // the rest of the method is tested else where
  
  ErrorStack::Get().ClearErrors();
}
//------------------------------------------------------------------------------
void ProcessorT::testReadArray ()
{
  {
    ErrorStack::Get().ClearErrors();

    CStr wrongLine("stuff"), path, line, group;
    util::GetTestFilesDirectory(path);
    path += "\\mfInputs1\\run2.h5";
    group = "Arrays/top1";
    line.Format("HDF5 2.0 \"%s\" \"Arrays/top1\" 1 0 37500", path);
    Processor p("stuff", "stuff", 300, 125, false);
    std::vector<double> d;
    double dMult;
    int iprn;
    //ErrorStack::Get().ClearErrors();
    //TS_ASSERT(!p.m_p->ReadArray(d, iprn, wrongLine));

    ErrorStack::Get().ClearErrors();
    int size;
    TS_ASSERT(p.m_p->ReadArray(d, dMult, iprn, line, size));
    TS_ASSERT_EQUALS(2, dMult);
    TS_ASSERT(!d.empty());
    TS_ASSERT_EQUALS2(37500, size);
    ErrorStack::Get().ClearErrors();
  }

  {
    ErrorStack::Get().ClearErrors();

    CStr wrongLine("stuff"), path, line, group;
    util::GetTestFilesDirectory(path);
    path += "\\Gms2Mf2k\\readArray\\arrayTest.h5";
    group = "Arrays/HK1";
    line.Format("HDF5 0.0 -1 \"%s\" \"Arrays/HK1\" 1 0 6", path);
    Processor p("stuff", "stuff", 2, 3, false);
    std::vector<double> d;
    double dMult;
    int iprn;
    //ErrorStack::Get().ClearErrors();
    //TS_ASSERT(!p.m_p->ReadArray(d, iprn, wrongLine));

    ErrorStack::Get().ClearErrors();
    int size;
    TS_ASSERT(p.m_p->ReadArray(d, dMult, iprn, line, size));
    TS_ASSERT_EQUALS2(0.0, dMult);
    std::vector<double> expected;
    for (int i = 1; i <= 6; ++i)
      expected.push_back(i);
    TS_ASSERT_EQUALS(expected, d);
    TS_ASSERT(!d.empty());
    TS_ASSERT_EQUALS2(6, size);
    ErrorStack::Get().ClearErrors();
  }
}
//------------------------------------------------------------------------------
void ProcessorT::testWriteArray ()
{
  CStr fName;
  util::GetTempDirectory(fName);
  fName += "\\array.txt";
  {
    double dMult(1.7);
    double d1[12] = {11,12,13,14,21,22,23,24,31,32,33,34};
    std::vector<double> d(&d1[0], &d1[12]), d2;

    Processor p(fName, fName, 3, 4, false);

    {
      std::ofstream fp("");
      TS_ASSERT(!p.m_p->WriteArray(fp, d, 3*4, dMult, 1));
    }

    std::ofstream fp(fName);
    TS_ASSERT(fp);
    if (!fp)
      return;

    TS_ASSERT(!p.m_p->WriteArray(fp, d2, 3*4, dMult, 1));

    TS_ASSERT(p.m_p->WriteArray(fp, d, 3*4, dMult, 1));
    fp.close();

    std::vector<CStr> strs;
    CStr str;
    strs.push_back("INTERNAL 1.7 (free) 1");
    strs.push_back("11 12 13 14 ");
    strs.push_back("21 22 23 24 ");
    strs.push_back("31 32 33 34 ");
    EReadAsciiFile e(fName);
    int i(0);
    while (e.GetLine(&str))
    {
      TS_ASSERT(str == strs.at(i++));
    }
    e.CloseFile();
    remove(fName);
  }
  {
    double dMult(2.1);
    double d1[33] = {11,12,13,14,15,16,17,18,19,110,111,
                     21,22,23,24,25,26,27,28,29,210,211,
                     31,32,33,34,35,36,37,38,39,310,311};
    std::vector<double> d(&d1[0], &d1[33]);
    Processor p(fName, fName, 3, 11, false);

    std::ofstream fp(fName);
    TS_ASSERT(fp);
    if (!fp)
      return;
    TS_ASSERT(p.m_p->WriteArray(fp, d, 3*11, dMult, 1));
    fp.close();

    std::vector<CStr> strs;
    CStr str;
    strs.push_back("INTERNAL 2.1 (free) 1");
    strs.push_back("11.0 12.0 13.0 14.0 15.0 16.0 17.0 18.0 19.0 110.0 ");
    strs.push_back("111.0 ");
    strs.push_back("21.0 22.0 23.0 24.0 25.0 26.0 27.0 28.0 29.0 210.0 ");
    strs.push_back("211.0 ");
    strs.push_back("31.0 32.0 33.0 34.0 35.0 36.0 37.0 38.0 39.0 310.0 ");
    strs.push_back("311.0 ");
    EReadAsciiFile e(fName);
    int i(0);
    e.OpenFile();
    while (e.GetLine(&str))
    {
      TS_ASSERT(str == strs.at(i++));
    }
    e.CloseFile();
    remove(fName);
  }
}
//------------------------------------------------------------------------------
void ProcessorT::testReadList ()
{
  ErrorStack::Get().ClearErrors();
  CStr fName, drnFile, h5File, line;
  util::GetTempDirectory(fName);
  fName += "\\list.txt";
  util::GetTestFilesDirectory(drnFile);
  h5File = drnFile;
  drnFile += "\\mfInputs1\\run2.drn";
  h5File += "\\mfInputs1\\run2.h5";
  line.Format("GMS_HDF5_01 \"%s\" \"Drain\" 1", h5File);
  int nList, lDim;
  std::vector<CStr> auxNames, vComments;
  auxNames.push_back("IFACE");
  auxNames.push_back("CONDFACT");
  auxNames.push_back("CELLGRP");
    
  // no h5 file
  {
    std::vector<double> f;
    Processor p(drnFile, fName, 300, 125, false);
    TS_ASSERT_THROWS(p.m_p->ReadList("no file", "Drain", f, nList, lDim, line,
                                     auxNames, &vComments),
                     ioError);
    ErrorStack::Get().ClearErrors();
  }

  // no group
  {
    std::vector<double> f;
    Processor p(drnFile, fName, 300, 125, false);
    TS_ASSERT(!p.m_p->ReadList(h5File, "no group", f, nList, lDim, line,
                               auxNames, &vComments));
    ErrorStack::Get().ClearErrors();
  }

  // poorly formatted line
  {
    // we other tests that cover this

    //std::vector<Real> f;
    //Processor p(drnFile, fName, 300, 125);
    //CStr wrongLine("stuff");
    //TS_ASSERT(!p.m_p->ReadList(h5File, "Drain", f, nList, lDim, wrongLine));
    //ErrorStack::Get().ClearErrors();
  }

  // this should work
  {
    std::vector<double> f;
    Processor p(drnFile, fName, 300, 125, false);
    TS_ASSERT(p.m_p->ReadList(h5File, "Drain", f, nList, lDim, line,
                              auxNames, &vComments));
  }
}
//------------------------------------------------------------------------------
void ProcessorT::testWriteList ()
{
  double f1[99] = {8, 2,1,  0,80000,6,5,4,3,1,-1,
                   8, 3,1,  0,80000,7,6,5,4,1,-1,
                   8, 4,1, 10,80000,8,7,6,5,1,-1,
                   8, 5,1, 20,80000,9,8,7,6,1,-1,
                   8, 6,1, 30,80000,0,9,8,7,1,-1,
                   8, 7,1, 50,80000,1,0,9,8,1,-1,
                   8, 8,1, 70,80000,2,1,0,9,1,-1,
                   8, 9,1, 90,80000,3,2,1,0,1,-1,
                   8,10,1,100,80000,4,3,2,1,1,-1};
  std::vector<double> f(&f1[0], &f1[99]);
  std::vector<CStr> auxNames;
  auxNames.push_back("IFACE");
  auxNames.push_back("CONDFACT");
  auxNames.push_back("CELLGRP");

  CStr dName, fName;
  util::GetTempDirectory(dName);
  fName = dName + "\\array.txt";

  // pass a null file
  {
    std::ofstream fp("");
    Processor p(fName, fName, 15, 15, false);

    TS_ASSERT(!p.m_p->WriteList(fp, 9, 11, auxNames, f));
  }

  // pass invalid dimensions for the vector
  {
    std::ofstream fp(fName);
    TS_ASSERT(fp);
    if (!fp)
      return;
    Processor p(fName, fName, 15, 15, false);

    TS_ASSERT(!p.m_p->WriteList(fp, 1, 11, auxNames, f));
    TS_ASSERT(!p.m_p->WriteList(fp, 9, 1, auxNames, f));
    fp.close();
    remove(fName);
  }

  // this should work
  {
    std::ofstream fp(fName);
    TS_ASSERT(fp);
    if (!fp)
      return;
    Processor p(fName, fName, 15, 15, false);

    TS_ASSERT(p.m_p->WriteList(fp, 9, 11, auxNames, f));
    fp.close();

    std::vector<CStr> strs;
    strs.push_back("8 2 1 0.0 80000.0 6.0 5.0 4.0 3 1.0 -1");
    strs.push_back("8 3 1 0.0 80000.0 7.0 6.0 5.0 4 1.0 -1");
    strs.push_back("8 4 1 10.0 80000.0 8.0 7.0 6.0 5 1.0 -1");
    strs.push_back("8 5 1 20.0 80000.0 9.0 8.0 7.0 6 1.0 -1");
    strs.push_back("8 6 1 30.0 80000.0 0.0 9.0 8.0 7 1.0 -1");
    strs.push_back("8 7 1 50.0 80000.0 1.0 0.0 9.0 8 1.0 -1");
    strs.push_back("8 8 1 70.0 80000.0 2.0 1.0 0.0 9 1.0 -1");
    strs.push_back("8 9 1 90.0 80000.0 3.0 2.0 1.0 0 1.0 -1");
    strs.push_back("8 10 1 100.0 80000.0 4.0 3.0 2.0 1 1.0 -1");

    EReadAsciiFile e(fName);
    e.OpenFile();
    CStr line;
    int i(0);
    while (e.GetLine(&line))
    {
      CStr expected(strs.at(i++));
      TS_ASSERT_EQUALS2(expected, line);
    }
    e.CloseFile();
    remove(fName);
  }

  // for drt files return cells should print as integers
  {
    fName = dName + "\\array.drt";
    std::ofstream fp(fName);
    TS_ASSERT(fp);
    if (!fp)
      return;
    Processor p(fName, fName, 15, 15, false);

    TS_ASSERT(p.m_p->WriteList(fp, 9, 11, auxNames, f));
    fp.close();

    std::vector<CStr> strs;
    strs.push_back("8 2 1 0.0 80000.0 6 5 4 3 1.0 -1");
    strs.push_back("8 3 1 0.0 80000.0 7 6 5 4 1.0 -1");
    strs.push_back("8 4 1 10.0 80000.0 8 7 6 5 1.0 -1");
    strs.push_back("8 5 1 20.0 80000.0 9 8 7 6 1.0 -1");
    strs.push_back("8 6 1 30.0 80000.0 0 9 8 7 1.0 -1");
    strs.push_back("8 7 1 50.0 80000.0 1 0 9 8 1.0 -1");
    strs.push_back("8 8 1 70.0 80000.0 2 1 0 9 1.0 -1");
    strs.push_back("8 9 1 90.0 80000.0 3 2 1 0 1.0 -1");
    strs.push_back("8 10 1 100.0 80000.0 4 3 2 1 1.0 -1");

    EReadAsciiFile e(fName);
    e.OpenFile();
    CStr line;
    int i(0);
    while (e.GetLine(&line))
    {
      CStr expected(strs.at(i++));
      TS_ASSERT_EQUALS2(expected, line);
    }
    e.CloseFile();
    remove(fName);
  }
}
//------------------------------------------------------------------------------
void ProcessorT::testInputFile ()
{
  Processor p("in", "out", 1, 1, false);
  CStr str(p.InputFile());
  TS_ASSERT(str == "in");
}
//------------------------------------------------------------------------------
void ProcessorT::testOutputFile ()
{
  Processor p("in", "out", 1, 1, false);
  CStr str(p.OutputFile());
  TS_ASSERT(str == "out");
}
//------------------------------------------------------------------------------
void ProcessorT::testNumRowandNumCol ()
{
  Processor p("in", "out", 3, 5, false);
  TS_ASSERT_EQUALS(p.NumRow(), 3);
  TS_ASSERT_EQUALS(p.NumCol(), 5);
}

#endif

//------------------------------------------------------------------------------
// FILE      EmrlAsserts.cpp
// PURPOSE   
// (C)opyright Aquaveo LLC, 2012 All rights reserved.
//------------------------------------------------------------------------------
#include <private/MfLibAsserts.h>

#include <fstream>
#include <sstream>

#include <cxxtest/TestSuite.h>

namespace MfLibAsserts {

//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void TextFilesEqual (const std::string& a_srcFile, unsigned a_line,
  const std::string& a_file1, const std::string& a_file2)
{
  std::string msg;
  if (!TextFilesEqual(a_file1, a_file2, msg))
    _TS_FAIL(a_srcFile.c_str(), a_line, msg.c_str());
} // TextFilesEqual
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool TextFilesEqual (const std::string& a_file1, const std::string& a_file2,
                     std::string& a_message)
{
  std::ifstream iOut(a_file1.c_str()), iBase(a_file2.c_str());
  std::string lineOut, lineBase;
  int  lineCnt(1);
  if (!iOut.is_open() || !iBase.is_open())
  {
    std::stringstream msg;
    msg << "Unable to open file: " << std::endl;
    if (!iOut.is_open())
    {
      msg << a_file1;
    }
    else
    {
      msg << a_file2;
    }
    a_message = msg.str();
    return false;
  }
  while (!iOut.eof() && !iBase.eof())
  {
    std::getline(iOut, lineOut);
    std::getline(iBase, lineBase);
    if (lineOut != lineBase)
    {
      std::stringstream msg;
      msg << "Files different on line " << lineCnt << "." << std::endl \
          << "File: " << a_file1 << "." << std::endl << lineOut << std::endl \
          << "File: " << a_file2 << "." << std::endl << lineBase << std::endl;
      a_message = msg.str();
      return false;
    }
    lineCnt++;
  }
  return true;
} // TextFilesEqual
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void StreamsEqual (const std::string& a_src, unsigned int a_line,
  std::istream& a_strm1, std::istream& a_strm2)
{
  std::string   line1, line2;
  unsigned int  line_count(1);

    // clear the flags and rewind the streams
  a_strm1.clear();
  a_strm2.clear();

  a_strm1.seekg(0);
  a_strm2.seekg(0);

  while (!a_strm1.eof() && !a_strm2.eof()) {
    std::getline(a_strm1, line1);
    std::getline(a_strm2, line2);

    if (line1 != line2) {
      std::stringstream msg;

      msg << "Streams different on line " << line_count << ".\n";
      msg << "Stream1:  \"" << line1 << "\"\n";
      msg << "Stream2:  \"" << line2 << "\"\n";

      _TS_FAIL(a_src.c_str(), a_line, msg.str().c_str());
    }

    ++line_count;
  }

  if ((a_strm1.eof() && !a_strm2.eof()) || (!a_strm1.eof() && a_strm2.eof())) {
    _TS_FAIL(a_src.c_str(), a_line, "Streams of different lengths");
  }
} // TextFilesEqual

} // end namespace MfLibAsserts

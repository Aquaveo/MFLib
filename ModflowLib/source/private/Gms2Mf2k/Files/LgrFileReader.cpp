//------------------------------------------------------------------------------
// FILE      LgrFileReader.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/Gms2Mf2k/Files/LgrFileReader.h>

#include <fstream>
#include <set>
#include <sstream>

#include <ModflowLib.h>
#include <private/util/EReadAsciiFile.h>
#include <private/util/util.h>

class LgrFileReader::impl
{
friend LgrFileReaderT;
public:
  impl (const char *a_);
  bool ReadFile();
  CStr m_inputFile;
  std::vector<CStr> m_files;
};

///////////////////////////////////////////////////////////////////////////////
/// \brief Constructor
///////////////////////////////////////////////////////////////////////////////
LgrFileReader::LgrFileReader (const char *a_) :
m_p(new LgrFileReader::impl(a_))
{
} // LgrFileReader::LgrFileReader
///////////////////////////////////////////////////////////////////////////////
/// \brief Constructor
///////////////////////////////////////////////////////////////////////////////
LgrFileReader::~LgrFileReader ()
{
  if (m_p)
    delete(m_p);
} // LgrFileReader::LgrFileReader
///////////////////////////////////////////////////////////////////////////////
/// \brief Reads the contents of the name file
///////////////////////////////////////////////////////////////////////////////
bool LgrFileReader::ReadFile ()
{
  return (m_p->ReadFile());
} // LgrFileReader::ReadFile
///////////////////////////////////////////////////////////////////////////////
/// \brief Returns the number of files to read
///////////////////////////////////////////////////////////////////////////////
size_t LgrFileReader::GetNumFilesToRead ()
{
  return m_p->m_files.size();
} // LgrFileReader::GetNumFilesToRead
///////////////////////////////////////////////////////////////////////////////
/// \brief Gets the file name from an index
///////////////////////////////////////////////////////////////////////////////
CStr LgrFileReader::GetFileNameAtIdx (size_t a_)
{
  CStr file;
  if (a_ < (int)m_p->m_files.size())
    file = m_p->m_files.at(a_);

  return file;
} // LgrFileReader::GetFileNameAtIdx

///////////////////////////////////////////////////////////////////////////////
/// \brief Constructor
///////////////////////////////////////////////////////////////////////////////
LgrFileReader::impl::impl (const char *a_) :
m_inputFile(a_)
{
} // LgrFileReader::impl::impl
///////////////////////////////////////////////////////////////////////////////
/// \brief Reads the contents of the name file
///////////////////////////////////////////////////////////////////////////////
bool LgrFileReader::impl::ReadFile ()
{
  EReadAsciiFile fp(m_inputFile);
  if (!fp.OpenFile())
    return false;

  try
  {
    CStr line;

    // LINE 0: [#Text] comment
    bool isComment;
    do
    {
      fp.GetLine(&line);

      if (!line.IsEmpty() && line.at(0) == '#')
        isComment = true;
      else
        isComment = false;
    } while (isComment);

    // LINE 1: LGR
    CStr firstItem;
    if (!fp.ReadData(firstItem) || firstItem != "LGR")
      return false;

    // LINE 2: NGRIDS
    int ngrids;
    fp.GetLine(&line);
    if (!fp.ReadData(ngrids) || ngrids < 1)
      return false;

    // LINE 3: NAME FILE
    CStr nameFile;
    fp.GetLine(&line);
    if (!fp.ReadData(nameFile) || nameFile == "")
      return false;
    m_files.push_back(nameFile);

    // LINE 4: GRID STATUS
    if (!fp.GetLine())
      return false;
    // LINE 5: IUPBHSV IUPBFSV
    if (!fp.GetLine())
      return false;

    for (int i = 0; i < ngrids-1; ++i)
    {
      // LINE 6: NAME FILE
      fp.GetLine(&line);
      if (!fp.ReadData(nameFile) || nameFile == "")
        return false;
      m_files.push_back(nameFile);

      // LINE 7: GRIDSTATUS
      if (!fp.GetLine())
        return false;
      // LINE 8: ISHFLG IBFLG IUCBHSV IUCBFSV
      if (!fp.GetLine())
        return false;
      // LINE 9: MXLGRITER IOUTLGR
      if (!fp.GetLine())
        return false;
      // LINE 10: RELAXH RELAXF
      if (!fp.GetLine())
        return false;
      // LINE 11: HCLOSELGR FCLOSELGR
      if (!fp.GetLine())
        return false;
      // LINE 12: NPLBEG NPRBEG NPCBEG
      if (!fp.GetLine())
        return false;
      // LINE 13: NPLEND NPREND NPCEND
      if (!fp.GetLine())
        return false;
      // LINE 14: NCPP
      if (!fp.GetLine())
        return false;
      // LINE 15: NCPPL [Repeat NCPPL a total of (NPLEND + 1 - NPLBEG) times]
      if (!fp.GetLine())
        return false;
    }
  }
  catch(ioexception)
  {
    ErrorStack::Get().PutError("Error in input file format. Aborting.");
    return false;
  }

  return true;
} // LgrFileReader::impl::ReadFile

#ifdef CXX_TEST
///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////

#include <private/Gms2Mf2k/Files/LgrFileReader.t.h>
#include <private/MfLibAsserts.h>

//------------------------------------------------------------------------------
static CStr iCreateTempLgrFile (bool a_comments)
{
  CStr filePath;
  util::GetTempDirectory(filePath);
  filePath += "\\test.lgr";
  std::ofstream out(filePath);
  if (a_comments)
  {
    out << "# comment 1\n";
    out << "# comment 2\n";
  }

  out << 
    "LGR\n"
    "3\n"
    "PARENT.nam\n"
    "PARENTONLY           ;ISCHILD\n"
    "70 71                ;IUBFSV, IUBHSV\n"
    "CHILD1.nam\n"
    "CHILDONLY            ;ISCHILD\n"
    "1 -59 80 81          ;ISHFLG, IBFLG, IUBHSV, IUBFSV\n"
    "15 -1                ;MXLGRITER, IOUTLGR\n"
    "0.50 0.50            ;RFH, RFF   relaxation for heads and flows\n"
    "1.0E-5 1.0E-5        ;HCLOSELGR, FCLOSELGR\n"
    "1 20 22              ;NPLBEG,NPRBEG,NPCBEG\n"
    "1 31 39              ;NPLEND,NPREND,NPCEND\n"
    "9                    ;NCPP\n"
    "1                    ;NCPPL\n"
    "CHILD2.nam\n"
    "CHILDONLY            ;ISCHILD\n"
    "1 -39 90 91          ;ISHFLG, IBFLG, IUBHSV, IUBFSV\n"
    "15 -1                ;MXLGRITER, IOUTLGR\n"
    "0.50 0.50            ;RFH, RFF   relaxation for heads and flows\n"
    "1.0E-5 1.0E-5        ;HCLOSELGR, FCLOSELGR\n"
    "1 20 70              ;NPLBEG,NPRBEG,NPCBEG\n"
    "1 31 87              ;NPLEND,NPREND,NPCEND\n"
    "9                    ;NCPP\n"
    "1                    ;NCPPL\n";
  return filePath;
} // iCreateLgrFile
//------------------------------------------------------------------------------
void LgrFileReaderT::testCreateClass ()
{
  LgrFileReader *n = new LgrFileReader("stuff");
  TS_ASSERT(n);
  if (n)
    delete(n);
}
//------------------------------------------------------------------------------
void LgrFileReaderT::testReadFile ()
{
  CStr testFilesPath;
  util::GetTestFilesDirectory(testFilesPath);

  {
    // test empty file
    LgrFileReader r("");
    TS_ASSERT(!r.ReadFile());
  }

  {
    // test name file
    CStr toTest = testFilesPath + "\\mfInputs1\\run2.mfn";
    LgrFileReader r(toTest);
    TS_ASSERT(!r.ReadFile());
  }

  {
    // test LGR file without comments
    CStr toTest = iCreateTempLgrFile(false);
    LgrFileReader r(toTest);
    TS_ASSERT(r.ReadFile());
    TS_ASSERT_EQUALS2(3, r.GetNumFilesToRead());
    TS_ASSERT_EQUALS2("PARENT.nam", r.GetFileNameAtIdx(0));
    TS_ASSERT_EQUALS2("CHILD1.nam", r.GetFileNameAtIdx(1));
    TS_ASSERT_EQUALS2("CHILD2.nam", r.GetFileNameAtIdx(2));
  }

  {
    // test LGR file with comments
    CStr toTest = iCreateTempLgrFile(true);
    LgrFileReader r(toTest);
    TS_ASSERT(r.ReadFile());
    TS_ASSERT_EQUALS2(3, r.GetNumFilesToRead());
    TS_ASSERT_EQUALS2("PARENT.nam", r.GetFileNameAtIdx(0));
    TS_ASSERT_EQUALS2("CHILD1.nam", r.GetFileNameAtIdx(1));
    TS_ASSERT_EQUALS2("CHILD2.nam", r.GetFileNameAtIdx(2));
  }
}

#endif

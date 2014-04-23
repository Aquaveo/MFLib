//------------------------------------------------------------------------------
// FILE      NameFileReader.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/Gms2Mf2k/Files/NameFileReader.h>

#include <set>

#include <ModflowLib.h>
#include <private/util/EReadAsciiFile.h>
#include <private/util/util.h>

class NameFileReader::impl
{
friend NameFileReaderT;
public:
  bool DoNotProcessType (const char *a_fType);

  impl (const char *a_);
  bool ReadFile();
  CStr m_nameFile, m_disFile, m_disuFile;
  std::vector<CStr> m_files;
};

///////////////////////////////////////////////////////////////////////////////
/// \brief Constructor
///////////////////////////////////////////////////////////////////////////////
NameFileReader::NameFileReader (const char *a_) :
m_p(new NameFileReader::impl(a_))
{
} // NameFileReader::NameFileReader
///////////////////////////////////////////////////////////////////////////////
/// \brief Constructor
///////////////////////////////////////////////////////////////////////////////
NameFileReader::~NameFileReader ()
{
  if (m_p)
    delete(m_p);
} // NameFileReader::NameFileReader
///////////////////////////////////////////////////////////////////////////////
/// \brief Reads the contents of the name file
///////////////////////////////////////////////////////////////////////////////
bool NameFileReader::ReadFile ()
{
  return (m_p->ReadFile());
} // NameFileReader::ReadFile
///////////////////////////////////////////////////////////////////////////////
/// \brief Returns the number of files to read
///////////////////////////////////////////////////////////////////////////////
int NameFileReader::GetNumFilesToRead ()
{
  return (int)(m_p->m_files.size());
} // NameFileReader::GetNumFilesToRead
///////////////////////////////////////////////////////////////////////////////
/// \brief Gets the file name from an index
///////////////////////////////////////////////////////////////////////////////
bool NameFileReader::GetFileNameAtIdx (int a_, CStr &a_file)
{
  try
  {
    a_file = m_p->m_files.at(a_);
  }
  catch (std::out_of_range)
  {
    return false;
  }
  return true;
} // NameFileReader::GetFileNameAtIdx
///////////////////////////////////////////////////////////////////////////////
/// \brief Add file to read not found in name file
///////////////////////////////////////////////////////////////////////////////
void NameFileReader::AddFileToRead (const CStr& a_file)
{
  m_p->m_files.push_back(a_file);
} // NameFileReader::AddFileToRead
///////////////////////////////////////////////////////////////////////////////
/// \brief Gets the file name for the discretization file
///////////////////////////////////////////////////////////////////////////////
void NameFileReader::GetDisFile (CStr &a_)
{
  a_ = m_p->m_disFile;
} // NameFileReader::GetDisFile
///////////////////////////////////////////////////////////////////////////////
/// \brief Gets the file name for the unstructured discretization file
///////////////////////////////////////////////////////////////////////////////
void NameFileReader::GetDisuFile (CStr &a_)
{
  a_ = m_p->m_disuFile;
} // NameFileReader::GetDisuFile

///////////////////////////////////////////////////////////////////////////////
/// \brief Constructor
///////////////////////////////////////////////////////////////////////////////
NameFileReader::impl::impl (const char *a_) :
m_nameFile(a_)
{
} // NameFileReader::impl::impl
///////////////////////////////////////////////////////////////////////////////
/// \brief Sees if this file type should be processed
///////////////////////////////////////////////////////////////////////////////
bool NameFileReader::impl::DoNotProcessType (const char *a_fType)
{
  std::set<CStr> m_types;
  m_types.insert("global");
  m_types.insert("list");
  //m_types.insert("lmt6");
  m_types.insert("data(binary)");
  m_types.insert("data");

  CStr str(a_fType);
  str.ToLower();
  if (m_types.find(str) != m_types.end())
    return true;
  return false;
} // NameFileReader::impl::DoNotProcessType
///////////////////////////////////////////////////////////////////////////////
/// \brief Reads the contents of the name file
///////////////////////////////////////////////////////////////////////////////
bool NameFileReader::impl::ReadFile ()
{
  EReadAsciiFile fp(m_nameFile);
  if (!fp.OpenFile())
    return false;

  CStr str, line;
  util::StripPathFromFilename(m_nameFile, line);
  m_files.push_back(line);
  try
  {
    fp.UseExceptions();
    while (fp.GetLine(&line))
    {
      if (line.IsEmpty())
        continue;
      // see if this is a comment line
      if (line.at(0) == '#')
        continue;

      CStr fType, unitNo, fName;
      // read the file type
      fp.ReadData(fType);
      // read the unit number
      fp.ReadData(unitNo);
      // read the file name
      fp.ReadData(fName);

      // don't add some of the files that we don't do anything with
      if (DoNotProcessType(fType))
        continue;

      m_files.push_back(fName);
      if (fType.CompareNoCase("dis") == 0)
        m_disFile = fName;
      else if (fType.CompareNoCase("disu") == 0)
        m_disuFile = fName;
      else if (fType.CompareNoCase("sen") == 0)
        MFLIB_SETSENFNAME(line.c_str(),
                          line.GetLength());
    }
  }
  catch (ioexception)
  {
    ErrorStack::Get().PutError("Error in name file format. Aborting.");
    return false;
  }

  return true;
} // NameFileReader::impl::ReadFile

#ifdef CXX_TEST
///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////

#include <private/Gms2Mf2k/Files/NameFileReader.t.h>

//------------------------------------------------------------------------------
void NameFileReaderT::testCreateClass ()
{
  NameFileReader *n = new NameFileReader("stuff");
  TS_ASSERT(n);
  if (n)
    delete(n);
}
//------------------------------------------------------------------------------
void NameFileReaderT::testReadFile ()
{
  CStr str, str1;
  util::GetTestFilesDirectory(str);
  str1 = str;
  str += "\\mfInputs1\\run2.mfn";
  {
    NameFileReader n("");
    TS_ASSERT(!n.ReadFile());
  }

  {
    NameFileReader n(str);
    TS_ASSERT(n.ReadFile());
  }
  str1 += "\\mfInputs1\\run2WrongFormat.mfn";
  {
    NameFileReader n(str1);
    TS_ASSERT(!n.ReadFile());
  }
}
//------------------------------------------------------------------------------
void NameFileReaderT::testGetNumFilesToRead ()
{
  CStr str;
  util::GetTestFilesDirectory(str);
  str += "\\mfInputs1\\run2.mfn";

  {
    NameFileReader n("");
    TS_ASSERT_EQUALS(n.GetNumFilesToRead(), 0);
  }

  {
    NameFileReader n(str);
    TS_ASSERT(n.ReadFile());
    TS_ASSERT_EQUALS(n.GetNumFilesToRead(), 17);
  }

  {
    NameFileReader n(str);
    n.AddFileToRead("run2.lgr");
    TS_ASSERT(n.ReadFile());
    TS_ASSERT_EQUALS(n.GetNumFilesToRead(), 18);
  }
}
//------------------------------------------------------------------------------
void NameFileReaderT::testGetFileNameAtIdx ()
{
  CStr nameFile;
  util::GetTestFilesDirectory(nameFile);
  nameFile += "\\mfInputs1\\run2.mfn";

  {
    CStr f;
    NameFileReader n("");
    TS_ASSERT(!n.GetFileNameAtIdx(0, f));
  }

  {
    CStr f;
    NameFileReader n(nameFile);
    TS_ASSERT(n.ReadFile());
    TS_ASSERT(n.GetFileNameAtIdx(0, f));
    TS_ASSERT(f == "run2.mfn");
    TS_ASSERT(n.GetFileNameAtIdx(1, f));
    TS_ASSERT(f == "run2.lmt");
    TS_ASSERT(n.GetFileNameAtIdx(16, f));
    TS_ASSERT(f == "run2.pcg");
    TS_ASSERT(!n.GetFileNameAtIdx(-1, f));
    TS_ASSERT(!n.GetFileNameAtIdx(17, f));
  }

  {
    CStr f;
    NameFileReader n(nameFile);
    n.AddFileToRead("run2.lgr");
    TS_ASSERT(n.ReadFile());
    TS_ASSERT(n.GetFileNameAtIdx(0, f));
    TS_ASSERT(f == "run2.lgr");
    TS_ASSERT(n.GetFileNameAtIdx(17, f));
    TS_ASSERT(f == "run2.pcg");
  }
}
//------------------------------------------------------------------------------
void NameFileReaderT::testGetDisFile ()
{
  CStr str, str1;
  util::GetTestFilesDirectory(str);
  str += "\\mfInputs1\\run2.mfn";
  {
    NameFileReader n("");
    TS_ASSERT(!n.ReadFile());
    n.GetDisFile(str1);
    TS_ASSERT(str1 == "");
    n.GetDisuFile(str1);
    TS_ASSERT(str1 == "");
  }

  {
    NameFileReader n(str);
    TS_ASSERT(n.ReadFile());
    n.GetDisFile(str1);
    TS_ASSERT(str1 == "run2.dis");
    n.GetDisuFile(str1);
    TS_ASSERT(str1 == "");
  }
}
//------------------------------------------------------------------------------
void NameFileReaderT::testGetDisuFile ()
{
  CStr str, str1;
  util::GetTestFilesDirectory(str);
  str += "\\mfInputs1\\run2_Disu.mfn";
  {
    NameFileReader n("");
    TS_ASSERT(!n.ReadFile());
    n.GetDisuFile(str1);
    TS_ASSERT(str1 == "");
    n.GetDisFile(str1);
    TS_ASSERT(str1 == "");
  }

  {
    NameFileReader n(str);
    TS_ASSERT(n.ReadFile());
    n.GetDisuFile(str1);
    TS_ASSERT(str1 == "run2.disu");
    n.GetDisFile(str1);
    TS_ASSERT(str1 == "");
  }
}
//------------------------------------------------------------------------------
void NameFileReaderT::testDoNotProcessType ()
{
  NameFileReader::impl n("");
  TS_ASSERT(n.DoNotProcessType("data"));
  TS_ASSERT(n.DoNotProcessType("DATA(BINARY)"));
  TS_ASSERT(n.DoNotProcessType("LiSt"));
  TS_ASSERT(n.DoNotProcessType("GlObAl"));
  TS_ASSERT(!n.DoNotProcessType("LmT6"));

  TS_ASSERT(!n.DoNotProcessType("anything else"));
}

#endif


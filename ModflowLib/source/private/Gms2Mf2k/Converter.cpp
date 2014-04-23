//------------------------------------------------------------------------------
// FILE      Converter.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------

#include <private/Gms2Mf2k/Converter.h>

#include <sstream>

#include <ModflowLib.h>
#include <private/Gms2Mf2k/Files/DisFileReader.h>
#include <private/Gms2Mf2k/Files/FileProcessor.h>
#include <private/Gms2Mf2k/Files/LgrFileReader.h>
#include <private/Gms2Mf2k/Files/NameFileReader.h>
#include <private/util/EReadAsciiFile.h>

class Converter::impl
{
  friend ConverterT;
public:
  impl(const char * const a_inputFile,
       const char * const a_outFile,
       std::ostream &a_out);

  const char * InputFile() const {return(m_inputFile.c_str());}
  bool InputFileExists() const;
  bool SetUpOutput();
  void PrintToOutput(const CStr &a_);
  bool DoConversion();

private:
  impl();

  bool CreateOutDir(CStr a_inPath,
                    CStr &a_outPath) const;
  bool DoLgrConversion(LgrFileReader& a_lgrReader);
  bool DoNameFileConversion(const CStr& a_nameFile, const CStr& a_lgrFile);


  CStr m_inputFile, m_outFile, m_outPath, m_inputPath;
  std::ostream& m_out;
};

///////////////////////////////////////////////////////////////////////////////
/// \brief Constructor
///////////////////////////////////////////////////////////////////////////////
Converter::Converter (const char * const a_inputFile,
                      std::ostream &a_out,
                      const char * const a_outFile/*""*/) :
m_p(new Converter::impl(a_inputFile, a_outFile, a_out))
{
} // Converter::Converter
Converter::~Converter ()
{
  try
  {
    if (m_p)
      delete(m_p);
  }
  catch (...) {}
} // Converter::~Converter
///////////////////////////////////////////////////////////////////////////////
/// \brief Returns true if there were no problems with the conversion
///////////////////////////////////////////////////////////////////////////////
bool Converter::DoConversion ()
{
  return (m_p->DoConversion());
} // Converter::DoConversion
///////////////////////////////////////////////////////////////////////////////
/// \brief Constructor
///////////////////////////////////////////////////////////////////////////////
Converter::impl::impl (const char * const a_inputFile,
                       const char * const a_outFile,
                       std::ostream &a_out) :
m_inputFile(a_inputFile),
m_outFile(a_outFile),
m_outPath(),
m_inputPath(),
m_out(a_out)
{
} // Converter::impl::impl

///////////////////////////////////////////////////////////////////////////////
/// \brief Returns true if the input file exists.
///////////////////////////////////////////////////////////////////////////////
bool Converter::impl::InputFileExists () const
{
  bool rval(false);

  // see if the input file exists
  FILE *fp(NULL);
  fp = fopen(m_inputFile, "r");
  if (!fp)
    return rval;
  fclose(fp);
  rval = true;

  if (!rval)
    ErrorStack::Get().PutError("Unable to read input file. Aborting.");

  return (rval);
} // Converter::imp::InputFileExists
///////////////////////////////////////////////////////////////////////////////
/// \brief sets up the output location
///////////////////////////////////////////////////////////////////////////////
bool Converter::impl::SetUpOutput ()
{
  // We are going to create an output directory called Out_Mf2k. If it
  // already exists then we will append numbers.

  // get the path to the name file
  CStr path;
  util::StripFileFromFilename(m_inputFile, path);

  // see if this directory exists
  if (!InputFileExists())
    return false;

  if (path.IsEmpty())
  {
    char c[5001];
    if (!GetCurrentDirectory(5000, &c[0]))
    {
      ErrorStack::Get().PutError("Unable to get current directory.");
      return false;
    }
    path = c;
  }

  // see if an output file was specified
  if (!m_outFile.IsEmpty())
  {
    util::StripFileFromFilename(m_outFile, m_outPath);
    // make sure this directory exists

    // if we can open the outfile then it does
    FILE *fp(fopen(m_outFile, "r"));
    if (!fp)
    {
      // or try creating some dumb file in this directory
      CStr tmpName(m_outPath + "95459558-4a55-4f6a-8c8b-6bf1bfae6208");
      FILE *fp1(fopen(tmpName, "w"));
      if (fp1)
      {
        fclose(fp1);
        remove(tmpName);
      }
      else
      {
        CStr msg;
        msg.Format("Unable to write files to:\n%s\n", m_outPath);
        ErrorStack::Get().PutError(msg);
        m_outPath = "";
        return false;
      }
    }
    else
    {
      fclose(fp);
    }
  }
  else
  {
    if (!CreateOutDir(path, path))
      return false;
  }


#if 0
  // now we allow the user to specify an output file
  path += "Out_Mf2k";
  testPath = path;
  while (::CreateDirectory(testPath, NULL) != 0 &&
         cnt < 11)
  {
    CStr cntStr;
    cntStr.Format("%2d", cnt++);
    testPath = path + cntStr;
  }
  if (cnt >= 11)
  {
    ErrorStack::Get().PutError("Unable to create output directory. Aborting.");
    return false;
  }
#endif

  // now set the output name file name
  //util::StripPathFromFilename(m_inputFile, name);

  // print where the output will be written
  m_outPath = path;
  m_out << "Output will be written to " << "\n" << m_outPath << "\n";

  return true;
} // Converter::impl::SetUpOutput
///////////////////////////////////////////////////////////////////////////////
/// \brief sets up the output directory and filename
///////////////////////////////////////////////////////////////////////////////
bool Converter::impl::CreateOutDir (CStr a_inPath,
                                    CStr &a_outPath) const
{
  a_outPath = "";

  bool retval(true);

  CStr path(a_inPath), testPath;
  path += "Out_Mf2k";
  testPath = path;
  int  cnt(1);

  while (::CreateDirectory(testPath, NULL) == 0 &&
         cnt < 11)
  {
    CStr cntStr;
    cntStr.Format("%2d", cnt++);
    testPath.Format("%s%s", path, cntStr);
  }

  if (cnt >= 11)
  {
    ErrorStack::Get().PutError("Unable to create output directory. Aborting.");
    retval = false;
  }
  else
  {
    a_outPath = testPath;
  }


  return retval;
} // Converter::impl::CreateOutDir
///////////////////////////////////////////////////////////////////////////////
/// \brief Prints the string to the output
///////////////////////////////////////////////////////////////////////////////
void Converter::impl::PrintToOutput (const CStr &a_)
{
  m_out << a_;
} // Converter::impl::PrintToOutput
///////////////////////////////////////////////////////////////////////////////
/// \brief Returns true if there were no problems with the conversion
///////////////////////////////////////////////////////////////////////////////
bool Converter::impl::DoConversion ()
{
  bool retval(true);
  try
  {
    if (!InputFileExists() || !SetUpOutput())
      throw std::exception();
    
    LgrFileReader lgrReader(m_inputFile);
    if (lgrReader.ReadFile())
      retval = DoLgrConversion(lgrReader);
    else
      retval = DoNameFileConversion(m_inputFile, "");
  }
  catch (std::exception&)
  {
    ErrorStack::Get().PrintErrors(m_out);
    retval = false;
  }

  return retval;
} // Converter::impl::DoConversion
///////////////////////////////////////////////////////////////////////////////
/// \brief Write LGR models to output folder
///////////////////////////////////////////////////////////////////////////////
bool Converter::impl::DoLgrConversion (LgrFileReader& a_lgrReader)
{
  // get path to each name file
  CStr lgrPath;
  util::StripFileFromFilename(m_inputFile.c_str(), lgrPath);
  std::vector<CStr> paths;
  std::vector<CStr> nameFiles;
  for (size_t i = 0; i < a_lgrReader.GetNumFilesToRead(); ++i)
  {
    CStr relativePath = a_lgrReader.GetFileNameAtIdx(i);
    CStr absolutePath = util::ResolveRelativePath(lgrPath, relativePath);
    nameFiles.push_back(absolutePath);
    util::StripFileFromFilename(absolutePath.c_str(), absolutePath);
    paths.push_back(absolutePath);
  }

  // determine directory that needs to be made for each name file
  util::RemoveCommonPath(paths);

  bool retval = true;
  CStr outPath = m_outPath;
  for (size_t i = 0; i < a_lgrReader.GetNumFilesToRead(); ++i)
  {
    // create subfolder if not all files in same path
    if (paths[i] != "")
    {
      m_outPath = outPath + "\\" + paths[i];
      if (::CreateDirectory(m_outPath.c_str(), NULL) == 0)
      {
        ErrorStack::Get().PutError("Unable to create output directory. Aborting.");
        return false;
      }
    }
    else
    {
      m_outPath = outPath;
    }

    CStr lgrFile;
    if (i == 0)
      util::StripPathFromFilename(m_inputFile.c_str(), lgrFile);
    retval = DoNameFileConversion(nameFiles[i], lgrFile);
  }

  return retval;
} // Converter::impl::DoLgrConversion
///////////////////////////////////////////////////////////////////////////////
/// \brief Write model files into output folder
///////////////////////////////////////////////////////////////////////////////
bool Converter::impl::DoNameFileConversion (const CStr& a_nameFile,
                                            const CStr& a_lgrFile)
{
  bool retval = true;

  CStr nameFileDir;
  util::StripFileFromFilename(a_nameFile, nameFileDir);
  SetCurrentDirectory(nameFileDir);

  // see if there is a mfs file, if so then copy it to the destination
  // folder
  {
    CStr mfsFile, mfsFileNoPath, mfsOut;
    util::StripExtensionFromFilename(a_nameFile, mfsFile);
    mfsFile += ".mfs";
    util::StripPathFromFilename(mfsFile, mfsFileNoPath);
    FILE *fp(fopen(mfsFile, "r"));
    if (fp)
    {
      fclose(fp);
      mfsOut = m_outPath;
      mfsOut += "\\";
      mfsOut += mfsFileNoPath;
      util::FileCopy(mfsFile, mfsOut);
    }
  }

  MFLIB_SETPARFNAME(a_nameFile.c_str(), a_nameFile.GetLength());
  NameFileReader n(a_nameFile);

  if (!a_lgrFile.empty())
    n.AddFileToRead(a_lgrFile);

  // Read the name file
  if (!n.ReadFile())
    throw std::exception();
  // figure out the number of rows and columns
  CStr disFile;
  int nRow, nCol;
  n.GetDisFile(disFile);
  bool unstructured = false;
  if (disFile == "")
  {
    n.GetDisuFile(disFile);
    unstructured = true;
  }
  DisFileReader d(disFile, unstructured);
  if (!d.ReadFile() ||
      !d.GetNumRow(&nRow) ||
      !d.GetNumCol(&nCol))
    throw std::exception();

  // process files in name file
  FileProcessor f(m_outPath, d);
  CStr path, file, inputFile;
  // get the path to the name file
  util::StripFileFromFilename(a_nameFile, path);
  for (int i=0; i<n.GetNumFilesToRead(); i++)
  {
    if (!n.GetFileNameAtIdx(i, file))
      continue;
    m_out << "Processing file: " << file << "\n";

    inputFile = path + "\\" + file;
    if (!f.ProcessFile(inputFile) && retval)
      retval = false;

    if (ErrorStack::Get().ErrorsExist())
      retval = false;
  }

  return retval;
} // Converter::impl::DoNameFileConversion

#ifdef CXX_TEST
///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////

#include <private/Gms2Mf2k/Converter.t.h>
#include <cxxtest/TestSuite.h>
#include <set>

//------------------------------------------------------------------------------
void ConverterT::testCreateClass ()
{
  Converter *c = new Converter("NameFile.nam", std::cout);
  TS_ASSERT(c);
  if (c)
    delete(c);
}
//------------------------------------------------------------------------------
void ConverterT::testValidInput ()
{
  CStr temp;
  util::GetTempDirectory(temp);

  Converter c("name.nam", std::cout);
  TS_ASSERT(!c.m_p->InputFileExists());

  temp += "\\name.nam";
  FILE *fp = fopen(temp, "w");
  if (fp)
    fclose(fp);
  Converter c1(temp, std::cout);
  TS_ASSERT(c1.m_p->InputFileExists());
  remove(temp);
}
//------------------------------------------------------------------------------
void ConverterT::testDoNameFileConversion ()
{
  std::stringstream out;

  CStr filePath, file, dir;
  util::GetTestFilesDirectory(filePath);
  filePath += "\\mfInputs1";
  file = filePath + "\\noFile.mfn";
  dir = filePath + "\\Out_Mf2k";
  // name file doesn't exist
  {
    Converter c(file, out);
    TS_ASSERT(!c.DoConversion());
    out.clear();
  }

  file = filePath + "\\run2_WrongFormat.mfn";
  // name file has the wrong format
  {
    Converter c(file, out);
    TS_ASSERT(!c.DoConversion());
    TS_ASSERT(util::DeleteDir(dir));
    out.clear();
  }

  file = filePath + "\\run2_WrongDis.mfn";
  // dis file doesn't exist
  {
    Converter c(file, out);
    TS_ASSERT(!c.DoConversion());
    TS_ASSERT(util::DeleteDir(dir));
    out.clear();
  }

  // the rest of the function is handled by the system tests
}
//------------------------------------------------------------------------------
void ConverterT::testPrintToOutput ()
{
  std::stringstream out;
  Converter c("stuff", out);
  CStr str("a test print"), str1;
  c.m_p->PrintToOutput(str);
  char ch[100];
  out.getline(ch, 100);
  str1 = ch;
  TS_ASSERT(str == str1);
}
//------------------------------------------------------------------------------
void ConverterT::testSetUpOutput ()
{
  CStr str;
  util::GetTestFilesDirectory(str);
  str += "\\mfInputs1\\run2.mfn";

  std::stringstream out;
  {
    Converter c("stuff", out);
    TS_ASSERT(!c.m_p->SetUpOutput());
  }

  {
    CStr outFile;
    util::StripFileFromFilename(str, outFile);
    outFile += "out\\run1.mfn";
    Converter c(str, out, outFile);
    TS_ASSERT(!c.m_p->SetUpOutput());
  }

  {
    CStr outFile, path;
    util::StripFileFromFilename(str, path);
    path += "out\\";
    outFile = path + "run1.mfn";
    // create the directory
    TS_ASSERT(::CreateDirectory(path, NULL) != 0);

    Converter c(str, out, outFile);
    TS_ASSERT(c.m_p->SetUpOutput());

    // delete the directory
    util::StripFileFromFilename(str, path);
    path += "out";
    TS_ASSERT(util::DeleteDir(path));
  }
}
//------------------------------------------------------------------------------
void ConverterT::testCreateOutDir ()
{
  std::stringstream out;
  CStr str;
  util::GetTestFilesDirectory(str);
  str += "\\mfInputs1\\run2.mfn";

  {
    CStr path, path1;
    Converter c(str, out);
    util::StripFileFromFilename(c.m_p->InputFile(), path);

    TS_ASSERT(c.m_p->CreateOutDir(path, path1));
    for (int i=0; i<9; i++)
    {
      bool r;
      r = c.m_p->CreateOutDir(path, path1);
      TS_ASSERT(r);
      if (!r)
        printf("Failed on i = %d\n", i);
    }
    TS_ASSERT(!c.m_p->CreateOutDir(path, path1));

    // remove the created directories
    CStr testPath, cntStr;
    util::StripFileFromFilename(c.m_p->InputFile(), path);
    path += "Out_Mf2k";
    testPath = path;
    for (int i=0; i<11; i++)
    {
      if (i>0)
      {
        cntStr.Format("%2d", i);
        testPath = path + cntStr;
      }
      TS_ASSERT(util::DeleteDir(testPath));
    }
  }
}
//------------------------------------------------------------------------------
void ConverterT::testDoLgrConversion ()
{
  std::stringstream out;

  CStr filePath, file, dir;
  util::GetTestFilesDirectory(filePath);
  filePath += "\\Gms2Mf2k\\lgr\\";
  file = filePath + "\\does_not_exist.lgr";
  dir = filePath + "\\Out_Mf2k";
  // lgr file doesn't exist
  {
    Converter c(file, out);
    TS_ASSERT(!c.DoConversion());
    out.clear();
  }

  file = filePath + "\\wrong_file_format.lgr";
  util::DeleteDir(dir);
  // lgr file has the wrong file format
  {
    Converter c(file, out);
    TS_ASSERT(!c.DoConversion());
    TS_ASSERT(util::DeleteDir(dir));
    out.clear();
  }

  file = filePath + "\\flattened\\HDF5_dualrefine.lgr";
  CStr output = filePath + "\\flattened\\Out_Mf2k";
  CStr base = filePath + "\\flattened\\base";
  util::DeleteDir(output);
  // lgr file in flattened directory
  {
    Converter c(file, out);
    TS_ASSERT(c.DoConversion());
    TS_DIRECTORY_FILES_EQUAL(base, output);
    TS_ASSERT(util::DeleteDir(output));
    out.clear();
  }
  
  file = filePath + "\\tree\\MODFLOW_parent\\parent.lgr";
  output = filePath + "\\tree\\MODFLOW_parent\\Out_Mf2k";
  base = filePath + "\\tree\\base";
  util::DeleteDir(output);
  // lgr file in directory tree
  {
    Converter c(file, out);
    TS_ASSERT(c.DoConversion());
    TS_DIRECTORY_FILES_EQUAL(base + "\\MODFLOW_parent",
                             output + "\\MODFLOW_parent");
    TS_DIRECTORY_FILES_EQUAL(base + "\\MODFLOW_child1",
                             output + "\\MODFLOW_child1");
    TS_DIRECTORY_FILES_EQUAL(base + "\\MODFLOW_child2",
                             output + "\\MODFLOW_child2");
    TS_ASSERT(util::DeleteDir(output));
    out.clear();
  }
}

#endif

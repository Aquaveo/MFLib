//------------------------------------------------------------------------------
// FILE      Gms2Mf2k.t.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <sstream>

#include <private/util/util.h>
#include <private/CmdLine.h>
#include <private/H5DataReader/H5DataSetReader.h>
#include <private/Parameters.h>

#ifdef CXX_TEST
///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////

#include <private/Gms2Mf2k/Gms2Mf2k.t.h>
#include <private/MfLibAsserts.h>
#include <RunTest.h>
//------------------------------------------------------------------------------
void Gms2Mf2kT::testConvert_SmallGrid_Trans ()
{
  // this only is done when Real is compiled as a double
  if (sizeof(Real) == sizeof(float))
    return;
  CStr nameFile, path, pathBase, pathOut, fileToCopy, destFile;
  util::GetTestFilesDirectory(path);
  path += "\\Gms2Mf2k\\smallGrid_Trans\\";
  pathBase = path + "base\\smallGrid_Trans_Str1_65.";
  pathOut = path + "Out_Mf2k\\smallGrid_Trans_Str1_65.";
  nameFile = path + "smallGrid_Trans_Str1_65.mfn";
  std::vector<const char *> argV;
  std::vector<CStr> cmdLine;
  cmdLine.push_back("-name");
  cmdLine.push_back(nameFile);
  argV.push_back(cmdLine.at(0).c_str());
  argV.push_back(cmdLine.at(1).c_str());

  // we need to change the stream file if we are using doubles or floats
  if (sizeof(Real) == sizeof(float))
    fileToCopy = path + "base\\Flt smallGrid_Trans_Str1_65.str";
  else
    fileToCopy = path + "base\\Dbl smallGrid_Trans_Str1_65.str";
  destFile = pathBase + "str";
  util::FileCopy(fileToCopy, destFile);

  Parameters::test_ClearParData();
  ErrorStack::Get().ClearErrors();
  std::stringstream out;
  TS_ASSERT(ProcessCmdLineArgs(2, &argV.front(), out));

  // now I need to compare the files in the base directory with the files
  // that we just created
  int i;
  const char *ext[17] = {"asp", "ba6", "chd", "dis", "drn", "evt", "gbob",
                         "ghb", "lpf", "mfn", "obs", "oc", "pcg", "rch",
                         "riv", "str", "wel"};
  for (i=0; i<17; i++)
  {
    CStr file1, file2;
    file1 = pathBase + ext[i];
    file2 = pathOut + ext[i];
    TS_ASSERT_TXT_FILES_EQUAL(file1, file2);
  }

  // now we need to delete the directory that we just created
  TS_ASSERT(util::DeleteDir(path + "Out_Mf2k"));
  H5Reader::CloseAllH5Files();
}
//------------------------------------------------------------------------------
void Gms2Mf2kT::testConvert_sg_t_pest ()
{
  // this only is done when Real is compiled as a double
  if (sizeof(Real) == sizeof(float))
    return;
  CStr nameFile, path, pathBase, pathOut, fileToCopy, destFile;
  util::GetTestFilesDirectory(path);
  path += "\\Gms2Mf2k\\smallGrid_Trans\\";
  pathBase = path + "base\\smallGrid_Trans_Str1_65.";
  pathOut = path + "sg_t_pest\\Out_Mf2k\\smallGrid_Trans_Str1_65.";
  nameFile = path + "sg_t_pest\\smallGrid_Trans_Str1_65.mfn";

  std::vector<const char *> argV;
  std::vector<CStr> cmdLine;
  cmdLine.push_back("-name");
  cmdLine.push_back(nameFile);
  argV.push_back(cmdLine.at(0).c_str());
  argV.push_back(cmdLine.at(1).c_str());

  // we need to change the stream file if we are using doubles or floats
  if (sizeof(Real) == sizeof(float))
    fileToCopy = path + "base\\Flt smallGrid_Trans_Str1_65.str";
  else
    fileToCopy = path + "base\\Dbl smallGrid_Trans_Str1_65.str";
  destFile = pathBase + "str";
  util::FileCopy(fileToCopy, destFile);

  ErrorStack::Get().ClearErrors();
  std::stringstream out;
  TS_ASSERT(ProcessCmdLineArgs(2, &argV.front(), out));

  // now I need to compare the files in the base directory with the files
  // that we just created
  int i;
  const char *ext[17] = {"asp", "ba6", "chd", "dis", "drn", "evt", "gbob",
                         "ghb", "lpf", "mfn", "obs", "oc", "pcg", "rch",
                         "riv", "str", "wel"};
  for (i=0; i<17; i++)
  {
    CStr file1, file2;
    file1 = pathBase + ext[i];
    file2 = pathOut + ext[i];
    TS_ASSERT_TXT_FILES_EQUAL(file1, file2);
  }

  // now we need to delete the directory that we just created
  TS_ASSERT(util::DeleteDir(path + "sg_t_pest\\Out_Mf2k"));
  H5Reader::CloseAllH5Files();
}
//------------------------------------------------------------------------------
void Gms2Mf2kT::testConvert_sg_ss_pest ()
{
  // this only is done when Real is compiled as a double
  if (sizeof(Real) == sizeof(float))
    return;
  CStr nameFile, path, pathBase, pathOut, fileToCopy, destFile;
  util::GetTestFilesDirectory(path);
  path += "\\Gms2Mf2k\\sg_ss_pest\\";
  pathBase = path + "base\\smallGrid_65.";
  pathOut = path + "Out_Mf2k\\smallGrid_65.";
  nameFile = path + "smallGrid_65.mfn";

  std::vector<const char *> argV;
  std::vector<CStr> cmdLine;
  cmdLine.push_back("-name");
  cmdLine.push_back(nameFile);
  argV.push_back(cmdLine.at(0).c_str());
  argV.push_back(cmdLine.at(1).c_str());

  // we need to change the stream file if we are using doubles or floats
  if (sizeof(Real) == sizeof(float))
    fileToCopy = path + "base\\Flt smallGrid_65.str";
  else
    fileToCopy = path + "base\\Dbl smallGrid_65.str";
  destFile = pathBase + "str";
  util::FileCopy(fileToCopy, destFile);

  Parameters::test_ClearParData();
  ErrorStack::Get().ClearErrors();
  std::stringstream out;
  TS_ASSERT(ProcessCmdLineArgs(2, &argV.front(), out));

  // now I need to compare the files in the base directory with the files
  // that we just created
  int i;
  const char *ext[17] = {"asp", "ba6", "chd", "dis", "drn", "evt", "gbob",
                         "ghb", "lpf", "mfn", "obs", "oc", "pcg", "rch",
                         "riv", "str", "wel"};
  for (i=0; i<17; i++)
  {
    CStr file1, file2;
    file1 = pathBase + ext[i];
    file2 = pathOut + ext[i];
    TS_ASSERT_TXT_FILES_EQUAL(file1, file2);
  }

  // now we need to delete the directory that we just created
  TS_ASSERT(util::DeleteDir(path + "Out_Mf2k"));
  H5Reader::CloseAllH5Files();
}
//------------------------------------------------------------------------------
void Gms2Mf2kT::testConvertNameWithSpace ()
{
  // this only is done when Real is compiled as a double
  if (sizeof(Real) == sizeof(float))
    return;
  CStr nameFile, path, pathBase, pathOut, fileToCopy, destFile;
  util::GetTestFilesDirectory(path);
  path += "\\Gms2Mf2k\\calib space\\";
  pathBase = path + "base\\calib_space.";
  pathOut = path + "Out_Mf2k\\calib_space.";
  nameFile = path + "calib space.mfn";

  std::vector<const char *> argV;
  std::vector<CStr> cmdLine;
  cmdLine.push_back("-name");
  cmdLine.push_back(nameFile);
  argV.push_back(cmdLine.at(0).c_str());
  argV.push_back(cmdLine.at(1).c_str());

  Parameters::test_ClearParData();
  ErrorStack::Get().ClearErrors();
  std::stringstream out;
  TS_ASSERT(ProcessCmdLineArgs(2, &argV.front(), out));

  // now I need to compare the files in the base directory with the files
  // that we just created
  int i;
  const char *ext[16] = {"asp", "ba6", "chd", "chob", "dis", "hob", "lmt",
                         "lpf", "mfn", "obs", "oc", "pcg", "rch", "riv",
                         "rvob", "wel"};
  for (i=0; i<16; i++)
  {
    CStr file1, file2;
    file1 = pathBase + ext[i];
    file2 = pathOut + ext[i];
    TS_ASSERT_TXT_FILES_EQUAL(file1, file2);
  }

  // now we need to delete the directory that we just created
  TS_ASSERT(util::DeleteDir(path + "Out_Mf2k"));
  H5Reader::CloseAllH5Files();
}//------------------------------------------------------------------------------
void Gms2Mf2kT::testConvertNoSeawatAux ()
{
  // this only is done when Real is compiled as a double
  if (sizeof(Real) == sizeof(float))
    return;
  CStr nameFile, path, pathBase, pathOut, fileToCopy, destFile;
  util::GetTestFilesDirectory(path);
  path += "\\Gms2Mf2k\\noSeawatAux\\";
  pathBase = path + "base\\noSeawatAux.";
  pathOut = path + "Out_Mf2k\\noSeawatAux.";
  nameFile = path + "noSeawatAux.mfn";

  std::vector<const char *> argV;
  std::vector<CStr> cmdLine;
  cmdLine.push_back("-name");
  cmdLine.push_back(nameFile);
  argV.push_back(cmdLine.at(0).c_str());
  argV.push_back(cmdLine.at(1).c_str());

  ErrorStack::Get().ClearErrors();
  std::stringstream out;
  TS_ASSERT(ProcessCmdLineArgs(2, &argV.front(), out));

  // now I need to compare the files in the base directory with the files
  // that we just created
  int i;
  const char *ext[12] = {"asp", "ba6", "chd", "dis", "drn",
                         "ghb", "lpf", "mfn", "oc", "pcg",
                         "riv", "wel"};
  for (i=0; i<12; i++)
  {
    CStr file1, file2;
    file1 = pathBase + ext[i];
    file2 = pathOut + ext[i];
    TS_ASSERT_TXT_FILES_EQUAL(file1, file2);
  }

  // now we need to delete the directory that we just created
  TS_ASSERT(util::DeleteDir(path + "Out_Mf2k"));
  H5Reader::CloseAllH5Files();
} // testConvertNoSeawatAux
//------------------------------------------------------------------------------
void Gms2Mf2kT::testConvertWithSeawatAux ()
{
  // this only is done when Real is compiled as a double
  if (sizeof(Real) == sizeof(float))
    return;
  CStr nameFile, path, pathBase, pathOut, fileToCopy, destFile;
  util::GetTestFilesDirectory(path);
  path += "\\Gms2Mf2k\\withSeawatAux\\";
  pathBase = path + "base\\withSeawatAux.";
  pathOut = path + "Out_Mf2k\\withSeawatAux.";
  nameFile = path + "withSeawatAux.mfn";

  std::vector<const char *> argV;
  std::vector<CStr> cmdLine;
  cmdLine.push_back("-name");
  cmdLine.push_back(nameFile);
  argV.push_back(cmdLine.at(0).c_str());
  argV.push_back(cmdLine.at(1).c_str());

  ErrorStack::Get().ClearErrors();
  std::stringstream out;
  TS_ASSERT(ProcessCmdLineArgs(2, &argV.front(), out));

  // now I need to compare the files in the base directory with the files
  // that we just created
  int i;
  const char *ext[12] = {"asp", "ba6", "chd", "dis", "drn",
                         "ghb", "lpf", "mfn", "oc", "pcg",
                         "riv", "wel"};
  for (i=0; i<12; i++)
  {
    CStr file1, file2;
    file1 = pathBase + ext[i];
    file2 = pathOut + ext[i];
    TS_ASSERT_TXT_FILES_EQUAL(file1, file2);
  }

  // now we need to delete the directory that we just created
  TS_ASSERT(util::DeleteDir(path + "Out_Mf2k"));
  H5Reader::CloseAllH5Files();
} // testConvertWithSeawatAux
//------------------------------------------------------------------------------
void Gms2Mf2kT::testConvertWrongOrderedAux ()
{
  // this only is done when Real is compiled as a double
  if (sizeof(Real) == sizeof(float))
    return;
  CStr nameFile, path, pathBase, pathOut, fileToCopy, destFile;
  util::GetTestFilesDirectory(path);
  path += "\\Gms2Mf2k\\wrongOrderedAux\\";
  pathBase = path + "base\\wrongOrderedAux.";
  pathOut = path + "Out_Mf2k\\wrongOrderedAux.";
  nameFile = path + "wrongOrderedAux.mfn";

  std::vector<const char *> argV;
  std::vector<CStr> cmdLine;
  cmdLine.push_back("-name");
  cmdLine.push_back(nameFile);
  argV.push_back(cmdLine.at(0).c_str());
  argV.push_back(cmdLine.at(1).c_str());

  ErrorStack::Get().ClearErrors();
  std::stringstream out;
  TS_ASSERT(ProcessCmdLineArgs(2, &argV.front(), out));

  // now I need to compare the files in the base directory with the files
  // that we just created
  int i;
  const char *ext[12] = {"asp", "ba6", "chd", "dis", "drn",
                         "ghb", "lpf", "mfn", "oc", "pcg",
                         "riv", "wel"};
  for (i=0; i<12; i++)
  {
    CStr file1, file2;
    file1 = pathBase + ext[i];
    file2 = pathOut + ext[i];
    TS_ASSERT_TXT_FILES_EQUAL(file1, file2);
  }

  // now we need to delete the directory that we just created
  TS_ASSERT(util::DeleteDir(path + "Out_Mf2k"));
  H5Reader::CloseAllH5Files();
} // testConvertWrongOrderedAux
//------------------------------------------------------------------------------
void Gms2Mf2kT::testConvertMNW2 ()
{
  CStr nameFile, path, pathBase, pathOut, fileToCopy, destFile;
  util::GetTestFilesDirectory(path);
  path += "\\Gms2Mf2k\\MNW2\\";
  pathBase = path + "base\\mnw2.";
  pathOut = path + "Out_Mf2k\\mnw2.";
  nameFile = path + "mnw2.mfn";

  std::vector<const char *> argV;
  std::vector<CStr> cmdLine;
  cmdLine.push_back("-name");
  cmdLine.push_back(nameFile);
  argV.push_back(cmdLine.at(0).c_str());
  argV.push_back(cmdLine.at(1).c_str());

  ErrorStack::Get().ClearErrors();
  std::stringstream out;
  TS_ASSERT(ProcessCmdLineArgs(2, &argV.front(), out));

  // now I need to compare the files in the base directory with the files
  // that we just created
  int i;
  const char *ext[9] = {"ba6", "dis", "lpf", "mfn", "mfs", "mnw2", "mnwi",
                         "oc", "pcg"};
  for (i=0; i<9; i++)
  {
    CStr file1, file2;
    file1 = pathBase + ext[i];
    file2 = pathOut + ext[i];
    TS_ASSERT_TXT_FILES_EQUAL(file1, file2);
  }

  // now we need to delete the directory that we just created
  TS_ASSERT(util::DeleteDir(path + "Out_Mf2k"));
  H5Reader::CloseAllH5Files();
}
//------------------------------------------------------------------------------
void Gms2Mf2kT::testUsgArrayAndList ()
{
  CStr path;
  util::GetTestFilesDirectory(path);
  path += "\\Gms2Mf2k\\usg\\array_and_list\\";
  CStr base = path + "base";
  CStr output = path + "Out_Mf2k";
  CStr nameFile = path + "bcf2ss.mfn";

  std::vector<const char *> argV;
  std::vector<CStr> cmdLine;
  cmdLine.push_back("-name");
  cmdLine.push_back(nameFile);
  argV.push_back(cmdLine.at(0).c_str());
  argV.push_back(cmdLine.at(1).c_str());

  ErrorStack::Get().ClearErrors();
  std::stringstream out;
  TS_ASSERT(ProcessCmdLineArgs(2, &argV.front(), out));
  TS_DIRECTORY_FILES_EQUAL(base, output);

  // now we need to delete the directory that we just created
  TS_ASSERT(util::DeleteDir(path + "Out_Mf2k"));
  H5Reader::CloseAllH5Files();
}
//------------------------------------------------------------------------------
void Gms2Mf2kT::testUsgSfr ()
{
  CStr path;
  util::GetTestFilesDirectory(path);
  path += "\\Gms2Mf2k\\usg\\sfr\\";
  CStr base = path + "base";
  CStr output = path + "Out_Mf2k";
  CStr nameFile = path + "test1ss.mfn";

  std::vector<const char *> argV;
  std::vector<CStr> cmdLine;
  cmdLine.push_back("-name");
  cmdLine.push_back(nameFile);
  argV.push_back(cmdLine.at(0).c_str());
  argV.push_back(cmdLine.at(1).c_str());

  ErrorStack::Get().ClearErrors();
  std::stringstream out;
  TS_ASSERT(ProcessCmdLineArgs(2, &argV.front(), out));
  TS_DIRECTORY_FILES_EQUAL(base, output);

  // now we need to delete the directory that we just created
  TS_ASSERT(util::DeleteDir(path + "Out_Mf2k"));
  H5Reader::CloseAllH5Files();
}
//------------------------------------------------------------------------------
void Gms2Mf2kT::testUsgStr ()
{
  CStr path;
  util::GetTestFilesDirectory(path);
  path += "\\Gms2Mf2k\\usg\\str\\";
  CStr base = path + "base";
  CStr output = path + "Out_Mf2k";
  CStr nameFile = path + "str.mfn";

  std::vector<const char *> argV;
  std::vector<CStr> cmdLine;
  cmdLine.push_back("-name");
  cmdLine.push_back(nameFile);
  argV.push_back(cmdLine.at(0).c_str());
  argV.push_back(cmdLine.at(1).c_str());

  ErrorStack::Get().ClearErrors();
  std::stringstream out;
  TS_ASSERT(ProcessCmdLineArgs(2, &argV.front(), out));
  TS_DIRECTORY_FILES_EQUAL(base, output);

  // now we need to delete the directory that we just created
  TS_ASSERT(util::DeleteDir(path + "Out_Mf2k"));
  H5Reader::CloseAllH5Files();
}

#endif

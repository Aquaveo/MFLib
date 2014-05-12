//------------------------------------------------------------------------------
// FILE      CmdLine.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------
#include <private/CmdLine.h>

#include <RunTest.h>

#include <private/Gms2Mf2k/Converter.h>
#include <private/util/util.h>

static bool ProcessCmdLine(const std::vector<CStr> &a_cmds,
                           CStr &a_nameFile,
                           CStr &a_outFile,
                           bool &a_pauseAtEnd);

///////////////////////////////////////////////////////////////////////////////
/// \brief Takes care of any cmd line arguments
///////////////////////////////////////////////////////////////////////////////
bool ProcessCmdLineArgs (int argc,
                         const char **argv,
                         std::ostream &a_out)
{
  std::vector<CStr> cmds;

  int i;
  for (i=0; i<argc; i++)
    cmds.push_back(argv[i]);

#if CXX_TEST
  bool runTests(false);
  for (i=0; i<argc; i++)
  {
    if (cmds.at(i).CompareNoCase("-launch_tests") == 0)
      runTests = true;
  }

  if (runTests)
  {
    return testCxx::RunUnitTests() != 0;
  }
#endif
  
  bool pauseAtEnd(false);
  try 
  {
    CStr nameFile, outFile;
    if (!ProcessCmdLine(cmds, nameFile, outFile, pauseAtEnd))
      throw std::exception();

    Converter c(nameFile, a_out, outFile);
    if (!c.DoConversion())
      throw std::exception();
  }
  catch (std::exception)
  {
    if (ErrorStack::Get().ErrorsExist())
    {
      ErrorStack::Get().PrintErrors(std::cout);
    }
  }

  if (pauseAtEnd)
  {
    char c;
    printf("Press the enter key to exit.\n");
    scanf("%c", &c);
  }

  return true;
} // ProcessCmdLine
///////////////////////////////////////////////////////////////////////////////
/// \brief Processes the command line arguments.
/// The commands that we can handle are:
/// -name "string"      This is the modflow name file. This is required.
/// -outdir "string"    This is the output directory. This is not required. If
///                     this command is not present then the out directory will
///                     be ./MF2K_OUT from where the name file is.
/// -outprefix "string" This is the prefix for the files that are written. This
///                     is not required. If it is not present then the output
///                     files have the same name as the input files.
///////////////////////////////////////////////////////////////////////////////
static bool ProcessCmdLine (const std::vector<CStr> &a_cmds,
                            CStr &a_nameFile,
                            CStr &a_outFile,
                            bool &a_pauseAtEnd)
{
  bool name(false);
  a_pauseAtEnd = false;

  a_nameFile = a_outFile = "";
  for (size_t i=0; i<a_cmds.size(); i++)
  {
    if (a_cmds.at(i).CompareNoCase("-name") == 0)
    {
      // read the input name file
      if (i+1 < a_cmds.size())
      {
        name = true;
        a_nameFile = a_cmds.at(i+1);
        i++;
      }
    }
    if (a_cmds.at(i).CompareNoCase("-out_file") == 0)
    {
      // read the output file name
      if (i+1 < a_cmds.size())
      {
        a_outFile = a_cmds.at(i+1);
      }
    }
    if (a_cmds.at(i).CompareNoCase("-pause_at_end") == 0)
    {
      a_pauseAtEnd = true;
    }
  }

  if (!name)
    ErrorStack::Get().PutError("No name file specified as input. Aborting.");

  return name;
} // ProcessCmdLine

#if CXX_TEST
#include <private/CmdLine.t.h>

//------------------------------------------------------------------------------
void CmdLineT::testProcessCmdLine ()
{
  std::vector<CStr> strs;
  CStr name, out;
  bool pause;
  TS_ASSERT(!ProcessCmdLine(strs, name, out, pause));
  TS_ASSERT(ErrorStack::Get().ErrorsExist());
  ErrorStack::Get().ClearErrors();

  strs.push_back("-name");
  TS_ASSERT(!ProcessCmdLine(strs, name, out, pause));
  TS_ASSERT(ErrorStack::Get().ErrorsExist());
  ErrorStack::Get().ClearErrors();

  strs.push_back("file");
  TS_ASSERT(ProcessCmdLine(strs, name, out, pause));
  TS_ASSERT(!ErrorStack::Get().ErrorsExist());

  strs.push_back("-out_file");
  TS_ASSERT(ProcessCmdLine(strs, name, out, pause));
  TS_ASSERT(out == "");

  strs.push_back("myout.out");
  TS_ASSERT(ProcessCmdLine(strs, name, out, pause));
  TS_ASSERT(out == "myout.out");

  strs.push_back("-pause_at_end");
  TS_ASSERT(ProcessCmdLine(strs, name, out, pause));
  TS_ASSERT(pause);

  TS_FAIL("testProcessCmdLine");
}

#endif

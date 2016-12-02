// Dan was here
//------------------------------------------------------------------------------
// FILE      ModflowLib.cpp
// PURPOSE   
//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
//------------------------------------------------------------------------------

#include <ModflowLib.h>

#include <private\util\util.h>

#include <private\ArrayReader.h>
#include <private\CmdLine.h>
#include <private\CopyProtect.h>
#include <private\ListReader.h>
#include <private\ListReaderStr.h>
#include <private\MfData\MfGlobal.h>
#include <private\MNWReader.h>
#include <private\Parameters.h>
#include <private\Sfr2Reader.h>
#include <private\H5DataReader\H5DataSetReader.h>
#include <private\samg\samg.h>
#include <private\SQLite\CppSQLite3.h>


template <class T>
static void iReadArray(int* a_SUCCESS,
                       int* a_IPRN,
                       const int* a_I,
                       const int* a_J,
                       const int* a_K,
                       T* a_A,
                       const char* a_CNTRL,
                       int a_CNTRLlen,
                       const char* a_NAME,
                       int a_NAMElen);
static void iReadSQLite(int *a_SUCCESS,
                        const int *a_I,
                        const int *a_J,
                        int *a_arr,
                        const char *a_line,
                        int a_lineLen);
static void imfLib_StripExtension(CStr &filename);

static void Init ()
{
  H5Initialize::Init();
} // Init

DLLEXPORT void MFLIB_TESTNOARG() {}
DLLEXPORT void MFLIB_TESTARG(int* ) {}
DLLEXPORT void __stdcall MFLIB_TESTCNOARG() {}
DLLEXPORT void __stdcall MFLIB_TESTCARG(int* ) {}
DLLEXPORT void MFLIB_CDECSTRING(char* ,
                                int ) {}
DLLEXPORT void __stdcall MFLIB_STDDECSTRING(char* ,
                                            int ) {}

//------------------------------------------------------------------------------
/// \brief used by SEAWAT
//------------------------------------------------------------------------------
CStr& modflowFilePath ()
{
  static CStr fg_modflowFilePath; // ok to leave static
  return fg_modflowFilePath;
}
//------------------------------------------------------------------------------
/// \brief Sets the path for any H5 files to be opened
//------------------------------------------------------------------------------
DLLEXPORT void MFLIB_SETMFFILEPATH (const char* a_path,
                                    int a_len)
{
  CStr line(util::GetStr(a_path, a_len)), str;
  util::StripFileFromFilename(a_path, str);
  H5Reader::SetH5FilePath(str);
  modflowFilePath() = str;
} // MFLIB_SETMFFILEPATH
//------------------------------------------------------------------------------
/// \brief Sets the path for any H5 files to be opened
//------------------------------------------------------------------------------
DLLEXPORT void MFLIB_GETMFFILEPATH (char* a_path,
                                    int a_len)
{
  memset(a_path, ' ', a_len);
  if (a_len > (int)modflowFilePath().size())
    a_len = (int)modflowFilePath().size();
  if (a_len > 0)
    memcpy(a_path, modflowFilePath().c_str(), a_len);
} // MFLIB_GETMFFILEPATH
//------------------------------------------------------------------------------
/// \brief used by SEAWAT
//------------------------------------------------------------------------------
CStr& mt3dFilePath ()
{
  static CStr fg_mt3dPath; // ok to leave static
  return fg_mt3dPath;
}
//------------------------------------------------------------------------------
/// \brief Sets the path to MT3D files so SEAWAT output can be written there
//------------------------------------------------------------------------------
DLLEXPORT void MFLIB_SETMT3DPATH (const char* a_path,
                                  int a_len)
{
  CStr line(util::GetStr(a_path, a_len)), str;
  util::StripFileFromFilename(a_path, str);
  mt3dFilePath() = str;
} // MFLIB_SETMFFILEPATH
//------------------------------------------------------------------------------
/// \brief Sets the path to MT3D files so SEAWAT output can be written there
//------------------------------------------------------------------------------
DLLEXPORT void MFLIB_GETMT3DPATH (char* a_path,
                                    int a_len)
{
  memset(a_path, ' ', a_len);
  if (a_len > (int)mt3dFilePath().size())
    a_len = (int)mt3dFilePath().size();
  if (a_len > 0)
    memcpy(a_path, mt3dFilePath().c_str(), a_len);
} // MFLIB_GETMT3DPATH
//------------------------------------------------------------------------------
/// \brief This is a utility function to copy a char array to a CStr class.
//------------------------------------------------------------------------------
static void imfLib_StripExtension (CStr &filename)
{
  int i, j;

  // Find the last period
  i = filename.ReverseFind('.');

  // This fixes the case when c:\temp.path\filename is sent in and we want to
  // return c:\temp.path\filename NOT c:\temp
  j = filename.ReverseFind('\\');
  if (i < j)
    return;

  if (i != -1) {
    filename = filename.Left(i);
  }
} // imfLib_StripExtension
//------------------------------------------------------------------------------
/// \brief Debugging function
//------------------------------------------------------------------------------
DLLEXPORT void MFLIB_CHECKMEMORY ()
{
  ASSERT(_CrtCheckMemory());
} // MFLIB_CHECKMEMORY
//------------------------------------------------------------------------------
/// \brief This closes all of the H5 files that were opened.
//------------------------------------------------------------------------------
DLLEXPORT void MFLIB_CLOSEALLH5FILES ()
{
  H5Reader::CloseAllH5Files();
} // MFLIB_CLOSEALLH5FILES
//------------------------------------------------------------------------------
/// \brief This is called to set the filename of the parameter file that GMS
/// uses.
//------------------------------------------------------------------------------
DLLEXPORT void MFLIB_SETPARFNAME (const char* a_fName,
                                  int a_fNameLen)
{
  CStr line1 = util::GetStr(a_fName, a_fNameLen);
  CStr line = line1;

  imfLib_StripExtension(line);
  line += ".param";
  Parameters::SetFileName(line);
  MfData::MfGlobal::Get().SetStrVar("NAME_FILE_STR", line1);
} // MFLIB_SETPARAMFILENAME
//------------------------------------------------------------------------------
/// \brief This is called to set the filename of the parameter file that GMS
/// uses.
//------------------------------------------------------------------------------
DLLEXPORT void MFLIB_SETPARFNAME_SEAWAT (const char* a_fName,
                                         int a_fNameLen)
{
  CStr line1 = util::GetStr(a_fName, a_fNameLen);
  if (line1.find("_MODFLOW") == -1) return;

  MFLIB_SETPARFNAME(a_fName, a_fNameLen);
} // MFLIB_SETPARFNAME_SEAWAT
//------------------------------------------------------------------------------
/// \brief This is called to set the filename of the modflow parameter file.
//------------------------------------------------------------------------------
DLLEXPORT void MFLIB_SETSENFNAME (const char* a_fName,
                                  int a_fNameLen)
{
  CStr line(util::GetStr(a_fName, a_fNameLen)), str;
  str = line.Left(4);
  str.Trim();
  str.ToLower();
  if (str == "sen" || str == "pval")
  {
    // the line should be formatted like this
    // SEN    57  "filename.snn"      OR
    // PVAL  775  "filenam.pval"
    Parameters::SetSenFileName(line);
  }
} // MFLIB_SETSENFNAME
//------------------------------------------------------------------------------
/// \brief This exports an array that uses pilot points to a data set that
/// can be read as part of the solution.
//------------------------------------------------------------------------------
DLLEXPORT void MFLIB_AREALOPT_EXDATA (const int* a_OPT,
                                      const char* a_NAME,
                                      int a_NAMElen)
{
  if (!a_OPT) return;
  CStr name(util::GetStr(a_NAME, a_NAMElen));
  MfData::Get().SetIntVar(name, *a_OPT);
} // MFLIB_AREALOPT_EXDATA
//------------------------------------------------------------------------------
/// \brief This exports an array that uses pilot points to a data set that
/// can be read as part of the solution.
//------------------------------------------------------------------------------
DLLEXPORT void MFLIB_U2DINT_EXDATA (const char* a_NAME,
                                    int a_NAMElen,
                                    const int* a_ARR,
                                    const int* a_MULT,
                                    const int* a_K,
                                    const int* a_JJ,
                                    const int* a_II)
{
  CStr name(util::GetStr(a_NAME, a_NAMElen));
  Parameters::ExportParameterArrayToDataSet(name,a_ARR,a_MULT,a_K,a_JJ,a_II);
} // MFLIB_U2DINT_EXDATA
//------------------------------------------------------------------------------
/// \brief This exports an array that uses pilot points to a data set that
/// can be read as part of the solution.
//------------------------------------------------------------------------------
DLLEXPORT void MFLIB_U2DREL_EXDATA (const char* a_NAME,
                                    int a_NAMElen,
                                    const Real* a_ARR,
                                    const Real* a_MULT,
                                    const int* a_K,
                                    const int* a_JJ,
                                    const int* a_II)
{
  CStr name(util::GetStr(a_NAME, a_NAMElen));
  Parameters::ExportParameterArrayToDataSet(name,a_ARR,a_MULT,a_K,a_JJ,a_II);
} // MFLIB_U2DREL_EXDATA
//------------------------------------------------------------------------------
/// \brief This exports an array that uses pilot points to a data set that
/// can be read as part of the solution.
//------------------------------------------------------------------------------
DLLEXPORT void MFLIB_U2DREL8_EXDATA (const char* a_NAME,
                                     int a_NAMElen,
                                     const double* a_ARR,
                                     const Real* a_MULT,
                                     const int* a_K,
                                     const int* a_JJ,
                                     const int* a_II)
{
  CStr name(util::GetStr(a_NAME, a_NAMElen));
  Parameters::ExportParameterArrayToDataSet8(name,a_ARR,a_MULT,a_K,a_JJ,a_II);
} // MFLIB_U2DREL_EXDATA
//------------------------------------------------------------------------------
/// \brief This is called from MODFLOWs U2DREL subroutine to read data from
/// an HDF5 file. The passed in array is filled in. This routine will also
/// substitute parameter values into the array (which may include pilot point
/// interpolation).
/// \param a_IPRN a print flag for MODFLOW output
/// \param a_I the number of cells in the I direction in the MODFLOW grid
/// \param a_J the number of cells in the J direction in the MODFLOW grid
/// \param a_A the array where the data from the HDF5 file will be placed
/// \param a_CNTRL the line of text from the MODFLOW input file. The contents
/// \param a_CNTRLlen the length of the text
/// of this variable describe where the HDF5 file and data set are located 
/// and how to read the data set. Here is an example line:
/// "HDF5 mult IPRN "fileName" "pathInFile" nDim start1 nToRead1 start2 nToRead2 start3 nToRead3
/// "HDF5 1.0 0 "input.h5" "Recharge/07. Property" 3 0 1 0 132 0 1"
///  HDF5         - a card to indicate that this is read from an HDF5 file
///  mult         - the multiplier for the array
///  IPRN         - print flag for the output control for MODFLOW
///  "fileName"   - the HDF5 file
///  "pathInFile" - path to the data set in the HDF5 file
///  nDim         - number of dimensions that the data set has (this is 1, 2, 3)
///  start1       - the index (NOTE: these are 0 based _NOT_ 1) for the
///                 starting point to read the data set in the first
///                 dimension
///  nToRead1     - the number of values to read in the first dimension
///  start2       - the index for the starting point to read the data set in
///                 the second dimension
///  nToRead2     - the number of values to read in the second dimension 
///  start3       - the index for the starting point to read the data set in
///                 the third dimension
///  nToRead3     - the number of values to read in the third dimension 
/// This is another supported format
/// "HDF5 CONSTANT value IPRN"
/// "HDF5 CONSTANT 5.0 1"
//------------------------------------------------------------------------------
DLLEXPORT void MFLIB_U2DREL (int *a_SUCCESS,
                             int *a_IPRN,
                             const int *a_I,
                             const int *a_J,
                             const int *a_K,
                             Real *a_A,
                             const char* a_CNTRL,
                             int a_CNTRLlen,
                             const char* a_NAME,
                             int a_NAMElen)
{
  iReadArray(a_SUCCESS, a_IPRN, a_I, a_J, a_K, a_A, a_CNTRL, a_CNTRLlen, a_NAME,
             a_NAMElen);
} // MFLIB_U2DREL
//----- OVERLOAD ---------------------------------------------------------------
// Same as mfLib_U2DREL but with double precision array.
DLLEXPORT void MFLIB_U2DREL8 (int *a_SUCCESS,
                              int *a_IPRN,
                              const int *a_I,
                              const int *a_J,
                              const int *a_K,
                              double *a_A,
                              const char* a_CNTRL,
                              int a_CNTRLlen,
                              const char* a_NAME,
                              int a_NAMElen)
{
  iReadArray(a_SUCCESS, a_IPRN, a_I, a_J, a_K, a_A, a_CNTRL, a_CNTRLlen, a_NAME,
             a_NAMElen);
} // MFLIB_U2DREL8
//------------------------------------------------------------------------------
DLLEXPORT void MFLIB_U2DDBL (int *a_SUCCESS,
                             int *a_IPRN,
                             const int *a_I,
                             const int *a_J,
                             double *a_A,
                             const char* a_CNTRL,
                             int a_CNTRLlen)
{
  int k(-1);
  iReadArray(a_SUCCESS, a_IPRN, a_I, a_J, &k, a_A, a_CNTRL, a_CNTRLlen, 0, 0);
} // MFLIB_U2DDBL
//------------------------------------------------------------------------------
/// \brief This is called from MODFLOWs U2DREL subroutine to read data from
/// an HDF5 file. The passed in array is filled in. This routine will also
/// substitute parameter values into the array (which may include pilot point
/// interpolation).
/// \param a_IPRN a print flag for MODFLOW output
/// \param a_I the number of cells in the I direction in the MODFLOW grid
/// \param a_J the number of cells in the J direction in the MODFLOW grid
/// \param a_A the array where the data from the HDF5 file will be placed
/// \param a_CNTRL the line of text from the MODFLOW input file. The contents
/// \param a_CNTRLlen the length of the text
/// of this variable describe where the HDF5 file and data set are located 
/// and how to read the data set. Here is an example line:
/// "HDF% mult IPRN "fileName" "pathInFile" nDim start1 nToRead1 start2 nToRead2 start3 nToRead3
/// "HDF5 1.0 0 "input.h5" "Recharge/07. Property" 3 0 1 0 132 0 1"
///  HDF5         - a card to indicate that this is read from an HDF5 file
///  mult         - the multiplier for the array
///  IPRN         - print flag for the output control for MODFLOW
///  "fileName"   - the HDF5 file
///  "pathInFile" - path to the data set in the HDF5 file
///  nDim         - number of dimensions that the data set has (this is 1, 2, 3)
///  start1       - the index (NOTE: these are 0 based _NOT_ 1) for the
///                 starting point to read the data set in the first
///                 dimension
///  nToRead1     - the number of values to read in the first dimension
///  start2       - the index for the starting point to read the data set in
///                 the second dimension
///  nToRead2     - the number of values to read in the second dimension 
///  start3       - the index for the starting point to read the data set in
///                 the third dimension
///  nToRead3     - the number of values to read in the third dimension 
//------------------------------------------------------------------------------
DLLEXPORT void MFLIB_U2DINT (int *a_SUCCESS,
                             int *a_IPRN,
                             const int *a_I,
                             const int *a_J,
                             int *a_A,
                             const char* a_CNTRL,
                             int a_CNTRLlen)
{
  int k(-1);
  iReadArray(a_SUCCESS, a_IPRN, a_I, a_J, &k, a_A, a_CNTRL, a_CNTRLlen, 0, 0);
} // MFLIB_U2DINT
//------------------------------------------------------------------------------
DLLEXPORT void MFLIB_SQLITE_U2DINT(int *a_SUCCESS,
                                   int* /*a_IPRN*/,
                                   const int *a_I,
                                   const int *a_J,
                                   int *a_arr,
                                   const char *a_line,
                                   int a_lineLen)
{
  iReadSQLite(a_SUCCESS, a_I, a_J, a_arr, a_line, a_lineLen);
} // MFLIB_SQLITE_U2DINT
//------------------------------------------------------------------------------
static void iReadSQLite(int *a_SUCCESS,
                        const int *a_I,
                        const int *a_J,
                        int *a_arr,
                        const char *a_line,
                        int a_lineLen)
{
  if (a_SUCCESS)
    *a_SUCCESS = 1;
  try
  {
    if (!a_I || !a_J || !a_arr || !a_line)
    {
      ErrorStack::Get().PutError("Null parameters passed to mfLib_SQLITE_U2DINT."
                                 "Aborting.");
      throw EException();
    }

    //Fill the data.
    CStr line = util::GetStr(a_line, a_lineLen);
    CToken t1(line, "\"");
    CStr str = t1.GetNextToken();
    str = t1.GetNextToken();
    CppSQLite3DB db;
    db.open(str);
    CStr str2 = t1.GetNextToken();
    str2 = t1.GetNextToken();
    CppSQLite3Query q = db.execQuery(str2);
    int i = 0;
    while (!q.eof())
    {
      a_arr[i] = atoi(q.fieldValue(0));
      ++i;
      q.nextRow();
    }
  }
  catch (EException)
  {
  }

  if (ErrorStack::Get().ErrorsExist())
  {
    ErrorStack::Get().PrintErrors(std::cout);
    if (a_SUCCESS)
      *a_SUCCESS = 0;
  }
} // iReadSQLite
//------------------------------------------------------------------------------
/// \brief This is an internal template for the U2DREL and U2DINT functions
//------------------------------------------------------------------------------
template <class T>
static void iReadArray (int* a_SUCCESS,
                        int* a_IPRN,
                        const int* a_I,
                        const int* a_J,
                        const int* a_K,
                        T* a_A,
                        const char* a_CNTRL,
                        int a_CNTRLlen,
                        const char* a_NAME,
                        int a_NAMElen)
{
  Init();
  if (a_SUCCESS)
    *a_SUCCESS = 1;
  try 
  {
    // do some checks on what was passed in
    if (!a_IPRN || !a_I || !a_J || !a_K || !a_A)
    {
      ErrorStack::Get().PutError("Null parameters passed to mfLib_U2DREL."
                                 "Aborting.");
      throw EException();
    }

    CStr line = util::GetStr(a_CNTRL, a_CNTRLlen);
    ArrayReader reader(line);
    reader.SetKvar(*a_K);
    if (!reader.ValidInputString())
      throw EException();
    *a_IPRN = reader.GetIPRN();

    CStr name;
    if (a_NAMElen > 0)
    {
      name = util::GetStr(a_NAME, a_NAMElen);
    }
    reader.GetData(a_A, (*a_I * *a_J), name);

  }
  catch (EException)
  {
  }

  if (ErrorStack::Get().ErrorsExist())
  {
    ErrorStack::Get().PrintErrors(std::cout);
    if (a_SUCCESS)
      *a_SUCCESS = 0;
  }
  //ASSERT(_CrtCheckMemory());
} // ReadArray
//------------------------------------------------------------------------------
/// \brief This is called from MODFLOWs ULSTRD subroutine to read data from
/// an HDF5 file. The passed in array is filled in. This routine will also
/// substitute parameter values into the array.
/// \param a_NLIST number of rows in the list of data
/// \param a_LDIM number of fields in the list
/// \param a_NAUX the number of auxiliary fields to in the list
/// \param a_NCOL number of columns in the model grid
/// \param a_NROW number of rows in the model grid
/// \param a_RLIST the array that is filled in by the function
/// \param a_LINE the text from the modflow input file
/// \param a_LINElen the length of the string passed a a_LINE
//------------------------------------------------------------------------------
template<class T>
static void mfLib_ULSTRD_T (int *a_SUCCESS,
                            const int* a_NLIST,
                            const int *a_LDIM,
                            const int *a_IAL,
                            const int *a_NAUX,
                            const char * a_CAUX,
                            const int *a_NCOL,
                            const int *a_NROW,
                            T *a_RLIST,
                            const char *a_LINE,
                            int a_LINElen)
{
  Init();
  if (a_SUCCESS)
    *a_SUCCESS = 1;
  try 
  {
    // do some checks on what was passed in
    if (!a_SUCCESS || !a_NLIST || !a_LDIM || !a_IAL || !a_NAUX ||
        !a_NCOL || !a_NROW || !a_RLIST || !a_LINE)
    {
      ErrorStack::Get().PutError("Null parameters passed to mfLib_ULSTRD."
                                 "Aborting.");
      throw EException();
    }
    std::vector<CStr> auxStr;
    if (a_CAUX)
    {
      char tmpAux[17];
      tmpAux[16] = '\0';
      int i, j, cnt(0);
      for (i=0; i<*a_NAUX; i++)
      {
        for (j=0; j<16; j++)
        {
          tmpAux[j] = a_CAUX[cnt++];
        }
        auxStr.push_back(tmpAux);
        auxStr.back().Trim();
      }
    }

    CStr line = util::GetStr(a_LINE, a_LINElen);
    ListReaderSetUp setUp(*a_NLIST, *a_LDIM, *a_IAL, *a_NAUX,
                          *a_NCOL, *a_NROW, line, auxStr);
    ListReader reader(setUp);
    if (reader.ValidSetUp())
    {
      reader.GetData(a_RLIST);
    }
  }
  catch (EException)
  {
  }
  if (ErrorStack::Get().ErrorsExist())
  {
    ErrorStack::Get().PrintErrors(std::cout);
    if (a_SUCCESS)
      *a_SUCCESS = 0;
  }
  //ASSERT(_CrtCheckMemory());
} // mfLib_ULSTRD
DLLEXPORT void MFLIB_ULSTRD (int *a_SUCCESS,
                             const int* a_NLIST,
                             const int *a_LDIM,
                             const int *a_IAL,
                             const int *a_NAUX,
                             const char *a_CAUX,
                             int /*a_dummy*/,
                             const int *a_NCOL,
                             const int *a_NROW,
                             Real *a_RLIST,
                             const char *a_LINE,
                             int a_LINElen)
{
  mfLib_ULSTRD_T(a_SUCCESS,
                 a_NLIST,
                 a_LDIM,
                 a_IAL,
                 a_NAUX,
                 a_CAUX,
                 a_NCOL,
                 a_NROW,
                 a_RLIST,
                 a_LINE,
                 a_LINElen);
} // MFLIB_ULSTRD
DLLEXPORT void MFLIB_ULSTRD_DBL (int *a_SUCCESS,
                                 const int* a_NLIST,
                                 const int *a_LDIM,
                                 const int *a_IAL,
                                 const int *a_NAUX,
                                 const char *a_CAUX,
                                 int /*a_dummy*/,
                                 const int *a_NCOL,
                                 const int *a_NROW,
                                 double *a_RLIST,
                                 const char *a_LINE,
                                 int a_LINElen)
{
  mfLib_ULSTRD_T(a_SUCCESS,
                 a_NLIST,
                 a_LDIM,
                 a_IAL,
                 a_NAUX,
                 a_CAUX,
                 a_NCOL,
                 a_NROW,
                 a_RLIST,
                 a_LINE,
                 a_LINElen);
} // MFLIB_ULSTRD_DBL
//------------------------------------------------------------------------------
/// \brief This is called from MODFLOW USG to set the array size of the a_STRM
/// array listed below. By default the size is 11 but it could be bigger if
/// the user has specified AUX variables
//------------------------------------------------------------------------------
DLLEXPORT void MFLIB_STR_AUX (int *a_NAUX,
                              const char* a_STRAUX,
                              int /*a_dummy*/)
{
  MfData::MfGlobal::Get().SetIntVar("NSTRVL", 11+*a_NAUX);
  char myChar[17];
  myChar[16] = '\0';
  int cnt(0), idx(0);
  for (int i=0; i<*a_NAUX*16; ++i)
  {
    myChar[cnt] = a_STRAUX[i];
    cnt++;
    if (cnt == 16)
    {
      cnt = 0;
      CStr str = myChar;
      str.Trim();
      if ("SEGID" == str || "CELLGRP" == str)
      {
        MfData::MfGlobal::Get().SetIntVar("AUX_SEGID_IDX", idx);
      }
      idx++;
    }
  }
} // MFLIB_STR_NSTRVL
//------------------------------------------------------------------------------
/// \brief This is called from MODFLOWs SGWF1STR6R subroutine to read data from
/// an HDF5 file. The 2 arrays STRM and ISTRM are filled in.
/// \param a_NLIST number of rows in the list of data
/// \param a_NCOL number of columns in the model grid
/// \param a_NROW number of rows in the model grid
/// \param a_STRM the array that is filled in by the function
/// \param a_ISTRM the array that is filled in by the function
/// \param a_ITRBAR the array that is filled in by the function
/// \param a_IDVIAR the array that is filled in by the function
/// \param a_LINE the text from the modflow input file
/// \param a_LINElen the length of the string passed a a_LINE
//------------------------------------------------------------------------------
DLLEXPORT void MFLIB_READSTR (int *a_SUCCESS,
                              const int *a_NSTREM,
                              const int *a_NSS,
                              const int *a_NTRIB,
                              const int *a_NCOL,
                              const int *a_NROW,
                              Real *a_STRM,
                              int *a_ISTRM,
                              int *a_ITRBAR,
                              int *a_IDVIAR,
                              const char *a_LINE,
                              int a_LINElen)
{
  Init();
  if (a_SUCCESS)
    *a_SUCCESS = 1;
  try 
  {
    // do some checks on what was passed in
    if (!a_SUCCESS || !a_NSTREM || !a_NSS || !a_NTRIB || !a_NCOL ||
        !a_NROW || !a_STRM || !a_ISTRM || !a_ITRBAR || !a_IDVIAR ||
        !a_LINE)
    {
      ErrorStack::Get().PutError("Null parameters passed to mfLib_ReadSTR."
                                 "Aborting.");
      throw EException();
    }

    CStr line = util::GetStr(a_LINE, a_LINElen);
    ListReaderSetUp setUp(*a_NSTREM, 10, 0, 0, *a_NCOL, *a_NROW, line);
    ListReaderStr reader(setUp);
    if (reader.ValidSetUp())
    {
      reader.GetDataStr(a_STRM, a_ISTRM, a_ITRBAR, a_IDVIAR, *a_NSS, *a_NTRIB);
    }
  }
  catch (EException)
  {
  }
  if (ErrorStack::Get().ErrorsExist())
  {
    ErrorStack::Get().PrintErrors(std::cout);
    if (a_SUCCESS)
      *a_SUCCESS = 0;
  }
  //ASSERT(_CrtCheckMemory());
} // MFLIB_READSTR
//------------------------------------------------------------------------------
/// \brief This is called from the MODFLOW GWF1MNW1RP subroutine to read MNW
/// data from an HDF5 file.  This fills the data for all the wells in a stress
/// period matching line 5 from MNW text input.
/// \param a_ITMP If greater than zero the number of wells in the stress period.
///               If equal to zero then no wells in stress period.
///               If less than zero then use last stress period.
/// \param a_WELL2 output array containing double values
/// \param a_NWELL2 output number of wells in stress period
/// \param a_MNWSITE site name for wells
/// \param a_MNWFLGS flags used to determine values specified
/// \param a_LINE line read from text file telling where to get HDF5 data.
//------------------------------------------------------------------------------
DLLEXPORT void MFLIB_READMNW(int *a_SUCCESS,
                             const int *a_ITMP,
                             double *a_WELL2,
                             char *a_MNWSITE,
                             int /*a_MNWSITElen*/,
                             double *a_MNWFLGS,
                             const char *a_LINE,
                             int a_LINElen)
{
  Init();
  if (a_SUCCESS)
    *a_SUCCESS = 1;
  try 
  {
    // do some checks on what was passed in
    if (!a_SUCCESS || !a_ITMP || !a_WELL2 || !a_MNWSITE ||
        !a_MNWFLGS || !a_LINE)
    {
      ErrorStack::Get().PutError("Null parameters passed to mfLib_ReadMNW."
                                 "Aborting.");
      throw EException();
    }

    CStr line = util::GetStr(a_LINE, a_LINElen);
    mnw::ReadMfH5Data(*a_ITMP, a_WELL2, a_MNWSITE, a_MNWFLGS, line);
  }
  catch (EException)
  {
  }
  if (ErrorStack::Get().ErrorsExist())
  {
    ErrorStack::Get().PrintErrors(std::cout);
    if (a_SUCCESS)
      *a_SUCCESS = 0;
  }
} // MFLIB_READMNW
//------------------------------------------------------------------------------
/// \brief This is called from MODFLOW GWF1SFR2RPP subroutine to read data from
/// an HDF5 file. This fills in the data for a single reach indicated by the 
/// index "ii"
/// \param ii the index of the current reach
/// \param krch K layer that the reach is in
/// \param irch I column that the reach is in
/// \param jrch J row that the reach is in
/// \param jseg Segment id that the reach is associate with
/// \param ireach The index of the particular reach relative to the segment
/// \param Strm Array of stream data. We set the value located at (1, ii)
//------------------------------------------------------------------------------
DLLEXPORT void MFLIB_SFR2REACH (const int* ii,
                                const int* NROW,
                                const int* NCOL,
                                int* krch,
                                int* irch,
                                int* jrch,
                                int* jseg,
                                int* ireach,
                                Real* Strm,
                                int NStrmD,
                                const char *line,
                                int a_dummy)
{
  Init();
  try 
  {
    // do some checks on what was passed in
    if (!ii || !NROW|| !NCOL || !krch || !irch || !jrch || !jseg ||
        !ireach || !Strm)
    {
      ErrorStack::Get().PutError("Null parameters passed to mfLib_SFR2Reach."
                                 "Aborting.");
      throw EException();
    }
    CStr line1 = util::GetStr(line, a_dummy);
    sfr2::GetReachData(*ii, *NROW, *NCOL, *krch, *irch, *jrch, *jseg,
                       *ireach, Strm, NStrmD, line1);
  }
  catch (EException)
  {
  }
  if (ErrorStack::Get().ErrorsExist())
  {
    ErrorStack::Get().PrintErrors(std::cout);
  }
} // MFLIB_SFR2REACH
//------------------------------------------------------------------------------
/// \brief This is called from MODFLOW SGWF1SFR2RDSEG subroutine to read data
/// from an HDF5 file. This fills in the data for a single segment indicated by
/// the index "iqseg"
/// \param Kper the current stress period
/// \param iqseg the index to get current stream segment
/// \param n the current stream segment
/// \param icalc icalc value for the current stream segment
/// \param noutseg 
/// \param iupseg
//------------------------------------------------------------------------------
DLLEXPORT void MFLIB_SFR (const int* Nlst,
                          const int* Kper,
                          int* Iseg,
                          int* Iotsg,
                          int* Idivar,
                          Real* Seg,
                          Real* Xsec,
                          Real* Qstage,
                          const char *line,
                          int a_dummy)
{
  Init();
  try 
  {
    // do some checks on what was passed in
    if (!Nlst || !Kper || !Iseg|| !Iotsg || !Idivar || !Seg || !Xsec || !Qstage)
    {
      ErrorStack::Get().PutError("Null parameters passed to mfLib_Sfr."
                                 "Aborting.");
      throw EException();
    }
    CStr line1 = util::GetStr(line, a_dummy);
    sfr2::GetSegData(*Nlst, *Kper, Iseg, Iotsg, Idivar, Seg, Xsec, Qstage, 
                     line1);
  }
  catch (EException)
  {
  }
  if (ErrorStack::Get().ErrorsExist())
  {
    ErrorStack::Get().PrintErrors(std::cout);
  }
} // MFLIB_SFR
//------------------------------------------------------------------------------
/// \brief This is called after reading a PVAL file in mf2k5 to set the
/// PARTYP variable. If we don't set it then MODFLOW stops running.
/// \param PARNAM the parameter names
/// \param PARTYP the parameter types
//------------------------------------------------------------------------------
DLLEXPORT void MFLIB_FILLINPARTYPE (const int* NPVAL,
                                    const char* PARNAM,
                                    const int /*a_dummy*/,
                                    char* PARTYP,
                                    const int /*a_dummy1*/)
{
  Parameters::FillInParType(NPVAL, PARNAM, PARTYP);
} // MFLIB_FILLINPARTYPE
//------------------------------------------------------------------------------
/// \brief Read the MNW2 stress period data
/// \param FLAG flag to catch errors
/// \param MNW2 array holding the MNW2 data
/// \param MNWMAX number of MNW2 wells
/// \param LnDesc string with the info for reading the data from the h5 file
//------------------------------------------------------------------------------
DLLEXPORT void MFLIB_READMNW2SP (int *FLAG,
                                 double* MNW2,
                                 const int* MNWMAX,
                                 const int* NMNWVL,
                                 const int* NAUX,
                                 const char* LnDesc,
                                 int a_dummy1)
{
  Init();
  if (FLAG)
    *FLAG = 1;
  try 
  {
    // do some checks on what was passed in
    if (!FLAG || !MNW2 || !MNWMAX || !NMNWVL || !NAUX || !LnDesc)
    {
      ErrorStack::Get().PutError("Null parameters passed to MFLIB_READMNW2SP."
                                 "Aborting.");
      throw EException();
    }

    CStr line = util::GetStr(LnDesc, a_dummy1);
    if (!mnw::ReadMnw2H5Data(MNW2,MNWMAX,NMNWVL,NAUX,line))
      *FLAG = 0;
  }
  catch (EException)
  {
  }
  if (ErrorStack::Get().ErrorsExist())
  {
    ErrorStack::Get().PrintErrors(std::cout);
    if (FLAG)
      *FLAG = 0;
  }
} // MFLIB_READMNW2
//------------------------------------------------------------------------------
/// \brief set the array size for sfr SEG
/// \param SZ the array size
//------------------------------------------------------------------------------
DLLEXPORT void MFLIB_SETSFRSEGSIZE (int *SZ)
{
  if (!SZ) return;
  MfData::Get().SetIntVar("SFR_SEG_SIZE", *SZ);
} // MFLIB_SETSFRSEGSIZE
//------------------------------------------------------------------------------
/// \brief releases the license used by SAMG if SAMG was used in the model
//------------------------------------------------------------------------------
DLLEXPORT void MFLIB_SAMG_REL_LIC ()
{
  SamgReleaseLicense();
} // MFLIB_SAMG_REL_LIC
//------------------------------------------------------------------------------
/// \brief calls the samg solver
//------------------------------------------------------------------------------
DLLEXPORT void MFLIB_SAMGUSG (double* A,
                              double* RHS,
                              double* HNEW,
                              int*    IA,
                              int*    JA,
                              int*    NNA,
                              int*    NNU,
                              int*    KPER,
                              int*    KSTP,
                              int*    ncyc,
                              int*    NCYC_DONE,
                              double* EPSSAMG,
                              int*    IBOUND,
                              int*    SAMGLOG,
                              int*    IERR,
                              int*    aqLicense)
{
  samgUsg(A,RHS,HNEW,IA,JA,NNA,NNU,KPER,KSTP,ncyc,NCYC_DONE,EPSSAMG,IBOUND,
          SAMGLOG,IERR,aqLicense);
} // MFLIB_SAMGUSG
//------------------------------------------------------------------------------
/// \brief samg for mf2k
//------------------------------------------------------------------------------
DLLEXPORT void MFLIB_LMG1ALSAMG (
  int* ISUM, int* ISUMI, int* LCA, int* LCIA, int* LCJA, int* LCU1, int* LCFRHS,
  int* LCIG, int* ISIZ1, int* ISIZ2, int* ISIZ3, int* ISIZ4, int* ICG,
  int* NCOL, int* NROW, int* NLAY, int* samg_logio, Real* stor1, Real* stor2,
  Real* stor3, char* samg_logfile, int a_samg_logfile_len)
{
  CStr log = util::GetStr(samg_logfile, a_samg_logfile_len);
  char mychar[500];
  strcpy_s(mychar, log.c_str());
  samgLMG1ALSAMG(ISUM,ISUMI,LCA,LCIA,LCJA,LCU1,LCFRHS,LCIG,ISIZ1,ISIZ2,ISIZ3,
                 ISIZ4,ICG,NCOL,NROW,NLAY,samg_logio,stor1,stor2,stor3,
                 mychar);
} // MFLIB_LMG1ALSAMG
//------------------------------------------------------------------------------
/// \brief samg for mf2k
//------------------------------------------------------------------------------
DLLEXPORT void MFLIB_LMG1RPSAMG (
  int* MXITER, int* MXCYC, Real* rcloselmg, Real* damplmg, Real* damplmgt,
  int* ioutamg, int* ICG, int* IADAMPlmg, Real* DUPlmg, Real* DLOWlmg,
  Real* HCLOSE, int* CONTROLlmg, int* samg_logio, char* SAMG_LOGFILE,
  int a_SAMG_LOGFILE_len)
{
  CStr log = util::GetStr(SAMG_LOGFILE, a_SAMG_LOGFILE_len);
  char mychar[500];
  strcpy_s(mychar, log.c_str());
  samgLMG1RPsamg(MXITER,MXCYC,rcloselmg,damplmg,damplmgt,ioutamg,ICG,IADAMPlmg,
                 DUPlmg,DLOWlmg,HCLOSE,CONTROLlmg,samg_logio,mychar);
} // MFLIB_LMG1RPsamg
//------------------------------------------------------------------------------
/// \brief samg for mf2k
//------------------------------------------------------------------------------
DLLEXPORT void MFLIB_LMG1APSAMG (
  double* HNEW,int* IBOUND,Real* CR,Real* CC,Real* CV,Real* HCOF,Real* RHS,     //7
  double* A,int* IA,int* JA,double* U,double* FRHS,int* IG,int* ISIZ1,          //7
  int* ISIZ2,int* ISIZ3,int* ISIZ4,int* KITER,Real* BCLOSE,Real* DAMP,          //6
  int* ICNVG,int* KSTP,int* KPER,int* MXITER,int* MXCYC,int* NCOL,int* NROW,    //7
  int * NLAY,int* NODES,Real* HNOFLO,int* IOUTAMG,int* ICG,int* IADAMP,         //6
  Real* DUP,Real* DLOW,int* samg_logio,int* IHCOFADD,Real* start_res,           //5
  Real* end_res,int* iter_done,int* setup_done,int* iLicense,char* samg_logfile,//5
  int a_samg_logfile_len)
{
  CStr log = util::GetStr(samg_logfile, a_samg_logfile_len);
  char mychar[500];
  strcpy_s(mychar, log.c_str());
  samgLMG1APsamg(HNEW,IBOUND,CR,CC,CV,HCOF,RHS,A,IA,JA,U,FRHS,IG,ISIZ1,ISIZ2,
                 ISIZ3,ISIZ4,KITER,BCLOSE,DAMP,ICNVG,KSTP,KPER,MXITER,MXCYC,
                 NCOL,NROW,NLAY,NODES,HNOFLO,IOUTAMG,ICG,IADAMP,DUP,DLOW,
                 samg_logio,IHCOFADD,start_res,end_res,iter_done,setup_done,
                 iLicense, mychar);
} // MFLIB_LMG1APSAMG

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////

#if CXX_TEST
int main (int argc, const char **argv)
{
  return ProcessCmdLineArgs(argc, argv, std::cout);
} // main
#endif


#if CXX_TEST
#include <private\ModflowLib.t.h>
//------------------------------------------------------------------------------
void ModflowLibT::setUp ()
{
  m_file = util::GetTestFilesDirectory() + "\\HDF5_InputFiles\\pest.h5";
  m_file2 = util::GetTestFilesDirectory() +
            "\\HDF5_InputFiles\\smallGrid_Trans.h5";
  ErrorStack::Get().ClearErrors();
}
//------------------------------------------------------------------------------
void ModflowLibT::tearDown ()
{
  ErrorStack::Get().ClearErrors();
}
//------------------------------------------------------------------------------
void ModflowLibT::test_imfLib_StripExtension ()
{
  CStr str("c:\\temp.path\\filename");
  imfLib_StripExtension(str);
  TS_ASSERT(str == "c:\\temp.path\\filename");
  str += ".txt";
  imfLib_StripExtension(str);
  TS_ASSERT(str == "c:\\temp.path\\filename");
}
//------------------------------------------------------------------------------
void ModflowLibT::test_imfLib_GetStr ()
{
  CStr str;
  str = util::GetStr("the str", 7);
  TS_ASSERT(str == "the str");
  str = util::GetStr("   the str\t ", 12);
  TS_ASSERT(str == "the str");
}
//------------------------------------------------------------------------------
void ModflowLibT::test_mfLib_U2DREL ()
{
  int s(0), iprn, i(70), j(42), k(-1);
  std::vector<Real> vFlt(2940, 0);
  Real f;
  CStr str;
  str.Format("HDF5 1.0 1 \"%s\" \"Arrays/HANI1\" 1 0 2940", m_file);
  MFLIB_U2DREL(&s, &iprn, &i, &j, &k, &vFlt.at(0), str.c_str(), str.GetLength(),0,0);
  TS_ASSERT(s);
  TS_ASSERT_EQUALS(vFlt.at(0), 1);
  TS_ASSERT_EQUALS(vFlt.at(2939), 1);
  str.Format("HDF5 3.54 1 \"%s\" \"Arrays/HANI1\" 1 0 2940", m_file);
  MFLIB_U2DREL(&s, &iprn, &i, &j, &k, &vFlt.at(0), str.c_str(), str.GetLength(),0,0);
  TS_ASSERT(s);
  f = (Real)3.54;
  TS_ASSERT_EQUALS(vFlt.at(0), f);
  TS_ASSERT_EQUALS(vFlt.at(2939), f);
  str.Format("HDF5 CONSTANT 5.6");
  MFLIB_U2DREL(&s, &iprn, &i, &j, &k, &vFlt.at(0), str.c_str(), str.GetLength(),0,0);
  TS_ASSERT(s);
  f = (Real)5.6;
  TS_ASSERT_EQUALS(vFlt.at(0), f);
  TS_ASSERT_EQUALS(vFlt.at(2939), f);
}
//------------------------------------------------------------------------------
void ModflowLibT::test_mfLib_U2DINT ()
{
  int s(0), iprn, i(70), j(42);
  std::vector<int> vInt(2940, 0);
  CStr str;
  str.Format("HDF5 1.0 1 \"%s\" \"Arrays/ibound1\" 1 0 2940", m_file);
  MFLIB_U2DINT(&s, &iprn, &i, &j, &vInt.at(0), str.c_str(), str.GetLength());
  TS_ASSERT(s);
  TS_ASSERT_EQUALS(vInt.at(0), 0);
  TS_ASSERT_EQUALS(vInt.at(138), 1);
  TS_ASSERT_EQUALS(vInt.at(2939), 0);
}
//------------------------------------------------------------------------------
void ModflowLibT::test_mfLib_ULSTRD ()
{
  int s(0), nrow(70), ncol(42), nlist(117), ldim(9), ial(0), naux(3);
  std::vector<Real> vFlt(1053, 0);
  CStr str;
  CStr str1 = "IFACE           "
              "CONDFACT        "
              "CELLGRP         "
              "                "
              "                ";
  char c[80];
  int i;
  for (i=0; i<80; i++)
    c[i] = str1.GetAt(i);

  str.Format("GMS_HDF5_01 \"%s\" \"River\" 1", m_file);
  MFLIB_ULSTRD(&s, &nlist, &ldim, &ial, &naux, c, 0, &ncol, &nrow, &vFlt.at(0),
               str.c_str(), str.GetLength());
  TS_ASSERT(s);
  Real vals[9] ={1,11,17,(Real)305.40534330960003,(Real)8.4256006747420003,
                  (Real)305.25534330959999,6,(Real)22.114437466514001,1};
  for (i=0; i<9; i++)
  {
    TS_ASSERT_DELTA(vFlt.at(i), vals[i], CXXDELTA);
  }
  Real v2[9] ={1,34,25,(Real)307.25161526880999,(Real)19.412680373861999,
                  (Real)307.10161526881001,6,(Real)50.951916991765003,6};
  for (i=0; i<9; i++)
  {
    TS_ASSERT_DELTA(vFlt.at(1044+i), v2[i], CXXDELTA);
  }
}
//------------------------------------------------------------------------------
void ModflowLibT::test_mfLib_ReadSTR ()
{
  CStr str;
  str.Format("GMS_HDF5_01 \"%s\" \"Stream\" 1", m_file2);
  int s(0), nstrem(16), nss(6), ntrib(2), ncol(5), nrow(6);
  std::vector<Real> vFlt(11*nstrem, 0);
  std::vector<int> vIstrm(5*nstrem, 0), vItrbar(nss*ntrib, 0), vIdivar(nss, 0);

  MFLIB_READSTR(&s, &nstrem, &nss, &ntrib, &ncol, &nrow, &vFlt.at(0),
                &vIstrm.at(0), &vItrbar.at(0), &vIdivar.at(0), str.c_str(),
                str.GetLength());
  TS_ASSERT(s);
  int i;

  Real Flt[22] = {1,(Real)19.654153846153847,(Real)7.1868522016940144,18,20,1,0,
                   (Real)0.001,0,0,0,0,(Real)19.598902704601336,
                   (Real)1.4743277447255569,18,20,1,0,(Real)0.001,0,0,0};
  for (i=0; i<22; i++)
  {
    TS_ASSERT_DELTA(Flt[i], vFlt.at(i), CXXDELTA);
  }
  Real Flt1[22] = {0,(Real)19.446751922671393,(Real)1.9485884868374197,18,20,1,0,
                    (Real)0.001,0,0,0,0,(Real)19.419901857208533,
                    (Real)5.5813712548221837,18,20,1,0,(Real)0.001,0,0,0};
  for (i=0; i<22; i++)
  {
    TS_ASSERT_DELTA(Flt1[i], vFlt.at(154+i), CXXDELTA);
  }

  int Istrm[80] = {1,1,2,1,1,1,2,2,1,2,1,2,3,1,3,1,1,3,2,1,1,2,3,2,2,1,2,3,3,
                   1,1,3,3,3,2,1,4,2,4,1,1,4,3,4,2,1,3,3,4,3,1,3,3,5,1,1,4,3,
                   5,2,1,4,4,5,3,1,2,3,6,1,1,2,4,6,2,1,3,4,6,3};
  for (i=0; i<80; i++)
  {
    TS_ASSERT_EQUALS(Istrm[i], vIstrm.at(i));
  }
  int Itrbar[12] = {0,0,1,0,3,0,0,0,2,0,4,0};
  for (i=0; i<12; i++)
  {
    TS_ASSERT_EQUALS(Itrbar[i], vItrbar.at(i));
  }
  int Idivar[6] = {0,0,0,0,0,1};
  for (i=0; i<6; i++)
  {
    TS_ASSERT_EQUALS(Idivar[i], vIdivar.at(i));
  }
}

#endif // CXX_TEST


//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#include <private/util/util.h>

#include <math.h>
#include <fstream>

#ifdef __linux__
#include <libgen.h>
#include <unistd.h>
#include <limits.h>
#define _finite std::isfinite
#endif


#define BIT_TEST(flags, bit) ((((flags) & (bit)) == 0) ? 0 : 1 )

template<class _T, class _U, class _V>
bool  LT_TOL(_T A, _U B, _V tolerance) {return (((B) - (A)) > (tolerance));}
template<class _T>
int Round (_T a) {return (((a) > 0.0) ? ((int)((a) + 0.5)) : (((a) < 0.0) ? ((int)((a) - 0.5)) : 0));}


//------------------------------------------------------------------------------
/// \brief Returns the field width of a floating point number 
//------------------------------------------------------------------------------
int util::RealWidth ()
{
  int width = 20;
  if (sizeof(float) == sizeof(Real)) width = 12;
  return width;
} // util::RealWidth
//------------------------------------------------------------------------------
/// \brief Creates a new string object and gives the reference
/// Sometimes we need some memory to stick around
//------------------------------------------------------------------------------
char* util::NewCharArray (size_t a_)
{
  static std::vector< std::vector<char> > m_strings; // ok to leave static
  m_strings.push_back(std::vector<char>(a_));
  return &m_strings.back()[0];
} // util::NewCharArray
int* util::NewIntArray (size_t a_)
{
  static std::vector< std::vector<int> > m_vals; // ok to leave static
  m_vals.push_back(std::vector<int>(a_, 0));
  return &m_vals.back()[0];
} // util::NewIntArray
Real* util::NewRealArray (size_t a_)
{
  static std::vector< std::vector<Real> > m_vals; // ok to leave static
  m_vals.push_back(std::vector<Real>(a_, 0));
  return &m_vals.back()[0];
} // util::NewIntArray
//------------------------------------------------------------------------------
/// \brief This is a utility function to copy a char array to a CStr class.
//------------------------------------------------------------------------------
CStr util::GetStr (const char *a_,
                   int a_strLen)
{
  ASSERT(_CrtCheckMemory());
  std::vector<char> myChar(((size_t)a_strLen)+1, ' ');
  myChar.back() = '\0';
  memcpy(&myChar.at(0), a_, (size_t)a_strLen);
  //for (int i=0; i<a_strLen; i++)
  //  myChar.at(i) = a_[i];
  CStr str(&myChar.front());
  str.TrimLeft();
  str.TrimRight();
  return (str);
} // imfLib_GetStr
///////////////////////////////////////////////////////////////////////////////
/// \brief Useful for debugging
///////////////////////////////////////////////////////////////////////////////
void util::NullFuncArg (const char *a_,
                        int a_line)
{
  CStr file;
  util::StripPathFromFilename(a_, file);
  CStr str;
  str.Format("\nNull paramter passed to method.\nFile:%s\nLine:%d\n",
             file.c_str(),a_line);
  ErrorStack::Get().PutError(str.c_str());
} // util::NullFuncArg
///////////////////////////////////////////////////////////////////////////////
/// \brief Fills in the string with the path to the bin directory
///////////////////////////////////////////////////////////////////////////////
void util::GetBinDirectory (CStr &a_)
{
#if __linux__
  char result[ PATH_MAX ];
  ssize_t count = readlink("/proc/self/exe", result, PATH_MAX);
  const char *path;
  if (count != -1) {
      path = dirname(result);
  }
  a_ = path;
#elif _MSC_VER
  char c[5000];
  GetModuleFileName(NULL, c, 5000);
  CStr str(c);
  int pos(str.ReverseFind("\\"));
  a_ = str.Left(str.GetLength() - (str.GetLength() - pos));
#else
  char self[PATH_MAX];

    if (!progname_u8)
      return nil;

    if (realpath(progname_u8, self))
      return string_utf8(self);

    return nil;
#endif
}
///////////////////////////////////////////////////////////////////////////////
/// \brief Fills in the string with the path to the temp files directory
///////////////////////////////////////////////////////////////////////////////
void util::GetTempDirectory (CStr &a_)
{
  char *c = getenv("TEMP");
  a_ = c;
  if (a_.IsEmpty())
  {
    c = getenv("TMP");
    a_ = c;
  }
} // util::GetTempDirectory
///////////////////////////////////////////////////////////////////////////////
/// \brief Fills in the string with the path to the test files directory
///////////////////////////////////////////////////////////////////////////////
void util::GetTestFilesDirectory (CStr &a_)
{
  a_ = util::GetTestFilesDirectory();
}
///////////////////////////////////////////////////////////////////////////////
/// \brief Fills in the string with the path to the test files directory
///////////////////////////////////////////////////////////////////////////////
CStr util::GetTestFilesDirectory ()
{
  CStr d;
  util::GetBinDirectory(d);
  d += "\\..\\TestFiles";
  return d;
}

///////////////////////////////////////////////////////////////////////////////
/// \brief Returns the current working directory
///////////////////////////////////////////////////////////////////////////////
CStr util::GetCurrentDirectory() {
  char buff[2048];
#ifdef _MSC_VER
  GetCurrentDirectory(2048, buff);
#else
  getcwd(buff, 2048) ? std::string( buff ) : std::string("");
#endif
  return CStr(buff);
}

///////////////////////////////////////////////////////////////////////////////
/// \brief Sets the current working directory
///////////////////////////////////////////////////////////////////////////////
bool util::SetCurrentDirectory(CStr &a_newDir) {
#ifdef _MSC_VER
  return SetCurrentDirectory(a_newDir);
#else
    // chdir returns zero if sucessful, -1 otherwise.
    // This function, however, should return true (ie. non-zero) on success.
  return (chdir(a_newDir) ? false : true);
#endif
}
///////////////////////////////////////////////////////////////////////////////
/// \brief Creates the specified directory in the current working directory
/// \param a_dir Name of the directory to create
///////////////////////////////////////////////////////////////////////////////
bool util::CreateDirectory(CStr &a_dir) {
#ifdef _MSC_VER
    return ::CreateDirectory(a_dir, NULL);
#else
    // the system call gives us the return code of the command run, which will
    // be zero if sucessful. This function, however, should return true (ie.
    // non-zero) on success.
    return system("mkdir -p " + a_dir) ? false : true;
#endif
}
///////////////////////////////////////////////////////////////////////////////
/// \brief Strips the file from a file name with path so that you get back
/// the path.
///////////////////////////////////////////////////////////////////////////////
void util::StripFileFromFilename (const char *a_fWithPath,
                                  CStr &a_path)
{
  int i;
  CStr cpathandfile(a_fWithPath);

  i = cpathandfile.ReverseFind('\\');
  if (i == -1) {
    i = cpathandfile.ReverseFind('/');
    if (i == -1)
      a_path= "";
  }

  a_path = cpathandfile.Left(i+1); // we'd use i+1 if we wanted the last '\'
} // util::StripFileFromFilename
///////////////////////////////////////////////////////////////////////////////
/// \brief Strips the path from the file name.
///////////////////////////////////////////////////////////////////////////////
void util::StripPathFromFilename (const char *a_fWithPath,
                                  CStr &a_file)
{
  CStr filename(a_fWithPath);
  int i;

  i = filename.ReverseFind('\\');
  if (i == -1) {
    i = filename.ReverseFind('/');
    if (i == -1)
      a_file = a_fWithPath;
  }
  a_file = filename.Right(filename.GetLength() - i - 1);
} // util::StripPathFromFilename
///////////////////////////////////////////////////////////////////////////////
/// \brief Strips the extension from the string.
///Given "c:\temp.path\filename.whatever.txt", returns
///      "c:\temp.path\filename.whatever".  Looks for the last period in the
///       string.
///////////////////////////////////////////////////////////////////////////////
void util::StripExtensionFromFilename (const char *a_fWithPath,
                                       CStr &a_filename)
{
  int i, j;

  // Find the last period
  CStr filename(a_fWithPath);
  i = filename.ReverseFind('.');

  // This fixes the case when c:\temp.path\filename is sent in and we want to
  // return c:\temp.path\filename NOT c:\temp
  j = filename.ReverseFind('\\');
  if (i < j)
  {
    a_filename = filename;
    return;
  }

  if (i != -1) {
    filename = filename.Left(i);
  }

  a_filename = filename;
} // util::StripExtensionFromFilename
///////////////////////////////////////////////////////////////////////////////
/// \brief
///////////////////////////////////////////////////////////////////////////////
//CStr util::ResolveRelativePath (const CStr& a_path, const CStr& a_filename)
//{
//  using namespace boost::filesystem;
//  path p = absolute(a_filename.c_str(), a_path.c_str());
//  
//  // separate into parts removing any . or .. from the path
//  std::vector<std::string> parts;
//  for (path::iterator it = p.begin(); it != p.end(); ++it)
//  {
//    std::string s = it->string();
//    if (s == "..")
//    {
//      if (!parts.empty())
//        parts.pop_back();
//    }
//    else if (s != ".")
//    {
//      parts.push_back(s);
//    }
//  }
//
//  // combine parts back together
//  path result;
//  std::vector<std::string>::iterator i;
//  for (i = parts.begin(); i != parts.end(); ++i)
//    result /= i->c_str();
//
//  return result.make_preferred().c_str();
//} // util::ResolveRelativePath
/* ---------------------------------------------------------------------------
PURPOSE: Given a string that has a full path and an empty vector, will divide
         the path up by directories and put them into the vector.  Given string
         'c:\temp\junk.exe' the vector will become "c:", "temp", "junk.exe"
PRE: 
POST:
RETURN: none.
---------------------------------------------------------------------------- */
void daPutAddressIntoVector (CStr a_address, std::vector<CStr> &a_vec)
{
  CStr str;
  int val;
  bool done = false;

  if (a_address.IsEmpty())
    return;
    // find the first directory and put it into the vector
  val = a_address.Find('\\'); 
    // return if there is no value
  if (val == -1) {
    a_vec.push_back(a_address);
    return;
  }

    // loop thru the path and put each directory into the vector
  while (!done) {
      //put directory into vector
    a_vec.push_back(a_address.Left(val));
    a_address = a_address.Right(a_address.GetLength() - val - 1);
      // find the next directory
    val = a_address.Find('\\');
      // done when there are no more directories
    if (val == -1) {
      a_vec.push_back(a_address);
      done = true;
    }
  }
} /* daPutAddressIntoVector */
/* ---------------------------------------------------------------------------
NOTES: Given a path as a constant and a filename with a relative address, the
       filename will be changed to include its full address.  If the two are
       completely different, nothing is changed.  Handles going up and down
       the path.  Here are examples:

         Path          |          Filename         |      New Filename
------------------------------------------------------------------------------
C:\temp                | filename.txt              | C:\temp\filename.txt
C:\temp                | .\model\filename.txt      | C:\temp\model\filename.txt
C:\temp\model          | ..\filename.txt           | C:\temp\filename.txt
\\maple\c\temp\model   | ..\filename.txt           | \\maple\c\temp\filename.txt
C:\temp                | D:\temp\filename.txt      | D:\temp\filename.txt
---------------------------------------------------------------------------- */
CStr util::ResolveRelativePath (const CStr& a_path, const CStr &a_filename1)
{
  CStr      temppath, str, thefilename, filenamepath, a_filename(a_filename1);
  CStr      pathcompname;
  std::vector<CStr> path;      // vector that will divide up the path by dir
  std::vector<CStr> filename; // vector that will divide up the filename by dir
  int          val, i=0, pcnt, fcnt;
  bool         done=FALSE, compname=FALSE;

  temppath = a_path;

  // if first two characters are '\\' computer name is included in address
  str = temppath.Mid(0, 2);
  if (str == "\\\\") {
    //gets rid of the computer name.
    temppath = temppath.Right(temppath.GetLength() - 2);
    val = temppath.Find('\\');
    pathcompname = temppath.Left(val);
    temppath = temppath.Right(temppath.GetLength() - val - 1);
    compname = TRUE;
  }
  thefilename = a_filename;
  str = thefilename.Mid(0, 2);
  if (str == "\\\\") {
    // If the file name has a computer name in it then we shouldn't try to
    // resolve it. It should be the full path.
    return a_filename;
  }

  util::StripFileFromFilename(thefilename, filenamepath);
  util::StripPathFromFilename(thefilename, thefilename);

  // get rid of all .\ on filename (if any)
  while (!done) {
    if ((!filenamepath.IsEmpty() && filenamepath[0] == '.') &&
        (filenamepath.GetLength() > 1 && filenamepath[1] == '\\'))
      filenamepath = filenamepath.Right(filenamepath.GetLength()-2);
    else
      done = true;
  }
  done = false;
  // Fix paths so they don't have a \ at the end
  if (temppath.GetLength() > 0 && temppath[temppath.GetLength()-1] == '\\')
    temppath = temppath.Left(temppath.GetLength()-1);
  if (filenamepath.GetLength() > 0 &&
      filenamepath[filenamepath.GetLength()-1] == '\\')
    filenamepath = filenamepath.Left(filenamepath.GetLength()-1);

  // get rid of .\ on filename (if any)--theoretically this 
  // should be taken care of above
  if ((!filenamepath.IsEmpty() && filenamepath[0] == '.') &&
      ((filenamepath.GetLength() > 1 && filenamepath[1] != '.') ||
        filenamepath.GetLength() == 1))
    filenamepath = filenamepath.Right(filenamepath.GetLength()-1);
  else if (filenamepath.GetLength() > 1 && filenamepath[1] == ':')
    return a_filename;

  // Enter the directories of the path into a vector;
  daPutAddressIntoVector(temppath, path);

  // Enter the directories of the filename into a vector;
  daPutAddressIntoVector(filenamepath, filename);

  // Clear out filename
  a_filename = "";
  if (compname) {
    a_filename = "\\\\";
    a_filename += pathcompname;
    a_filename += "\\";
  }

  fcnt=0;
  pcnt = static_cast<int>(path.size());
  while (!done) {
    if (fcnt < (int)filename.size() && filename[fcnt] == "..")
      pcnt--;
    else {
      for (i=0; i<pcnt; i++) {
        a_filename += path[i];
        a_filename += "\\";
      }
      for (i=fcnt; i<(int)filename.size(); i++) {
        a_filename += filename[i];
        a_filename += "\\";
      }
      a_filename += thefilename;
      done = true;
    }
    fcnt++;
  }
  return a_filename;
} // util:ResolveRelativePath
///////////////////////////////////////////////////////////////////////////////
/// \brief
///////////////////////////////////////////////////////////////////////////////
void util::RemoveCommonPath (std::vector<CStr>& a_paths)
{
  if (a_paths.empty())
    return;

  CStr common = a_paths[0];
  for (size_t i = 1; common != "" && i < a_paths.size(); ++i)
  {
    CStr& current = a_paths[i];
    CStr::iterator comm = common.begin();
    CStr::iterator curr = current.begin();
    bool match = true;
    while (match && comm != common.end() && curr != current.end())
    {
      if (*comm == *curr)
      {
        ++comm;
        ++curr;
      }
      else
      {
        match = false;
      }
    }
    common.erase(comm, common.end());
  }

  if (common == "")
    return;

  size_t dirEnd = common.find_last_of("\\");
  if (dirEnd != std::string::npos)
    common.erase(common.begin() + dirEnd + 1, common.end());

  for (size_t i = 0; i < a_paths.size(); ++i)
  {
    a_paths[i].erase(a_paths[i].begin(), a_paths[i].begin() + common.size());
  }
} // util::RemoveCommonPath
///////////////////////////////////////////////////////////////////////////////
/// \brief Strips everything except the extension.
///////////////////////////////////////////////////////////////////////////////
void util::StripAllButExtension (const char *a_fWithPath,
                                 CStr &a_ext)
{
  int i;
  CStr filename(a_fWithPath);

  // Find the last period
  i = filename.ReverseFind('.');
  if (i != -1) {
    filename = filename.Right(filename.GetLength() - i - 1);
  }
  else {
    filename = "";
  }
  a_ext = filename;
} // util::StripAllButExtension
///////////////////////////////////////////////////////////////////////////////
/// \brief Deletes the directory that was passed in.
///////////////////////////////////////////////////////////////////////////////
#ifndef STATICLIB
bool util::DeleteDir (const char *a_)
{
  int len;
  len = (int)strlen(a_);
  std::vector<char> ch(len+2);
  strcpy(&ch[0], a_);
  ch.back() = '\0';
  SHFILEOPSTRUCT fileop;
  fileop.hwnd   = NULL;    // no status display
  fileop.wFunc  = FO_DELETE;  // delete operation
  fileop.pFrom  = &ch[0];  // source file name as double null terminated string
  fileop.pTo    = NULL;    // no destination needed
  fileop.fFlags = FOF_NOCONFIRMATION|FOF_SILENT;  // do not prompt the user
  fileop.fAnyOperationsAborted = FALSE;
  fileop.lpszProgressTitle     = NULL;
  fileop.hNameMappings         = NULL;
  return(SHFileOperation(&fileop) == 0);
} // util::DeleteDir
#else
bool util::DeleteDir (const char *)
{
  return false;
} // util::DeleteDir
#endif
///////////////////////////////////////////////////////////////////////////////
/// \brief Copies the src dir that was passed in to the dest dir.
///////////////////////////////////////////////////////////////////////////////
#ifndef STATICLIB
bool util::CopyDir (const char *a_src,
                    const char *a_dest)
{
  int len;

  // according to the docs both paths should be full paths or funny things can
  // happen like possibly below
  std::vector<char> ch(MAX_PATH+2);
  len = GetFullPathName(a_src, MAX_PATH+1, &ch[0], NULL);
  if (len > MAX_PATH)
    return false;
  ch[len] = 0;
  ch[len+1] = 0;

  // can cut off file name to eight chars if destination isn't a full path
  std::vector<char> ch1(MAX_PATH+2);
  len = GetFullPathName(a_dest, MAX_PATH+1, &ch1[0], NULL);
  if (len > MAX_PATH)
    return false;
  ch1[len] = 0;
  ch1[len+1] = 0;

  SHFILEOPSTRUCT fileop;
  fileop.hwnd   = NULL;    // no status display
  fileop.wFunc  = FO_COPY;  // copy operation
  fileop.pFrom  = &ch[0];  // source file name as double null terminated string
  fileop.pTo    = &ch1[0];    // destination directory
  fileop.fFlags = FOF_NOCONFIRMATION|FOF_SILENT;  // do not prompt the user
  fileop.fAnyOperationsAborted = FALSE;
  fileop.lpszProgressTitle     = NULL;
  fileop.hNameMappings         = NULL;
  int result = SHFileOperation(&fileop);
  return(result == 0);
} // util::CopyDir
#else
bool util::CopyDir (const char *,
                    const char *)
{
  return false;
} // util::CopyDir
#endif
///////////////////////////////////////////////////////////////////////////////
/// \brief Copies the src dir that was passed in to the dest dir.
///////////////////////////////////////////////////////////////////////////////
bool util::FileCopy (const char *a_src,
                     const char *a_dest)
{
  std::ifstream src(a_src, std::ios::binary);
  std::ofstream dst(a_dest, std::ios::binary);
  dst << src.rdbuf();
  return true;
} // CopyFile
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool util::FileExists(const char *a_filePath)
{
  FILE* file = fopen(a_filePath, "r");
  bool exists = file != NULL;
  if (file != NULL)
    fclose(file);
  return exists;
}
/* -----------------------------------------------------------------------------
PURPOSE: Returns precision, or the number of digits to the right of the
         decimal needed to display the [value].  This is used in conjunction
         with xgFloatToString() or STR() to get the string.  The precision
         is calculated such that the total string length (including the
         minus sign, decimal, E etc.) will never end up longer than
         <length> (by default, 15 characters) so all edit fields can be
         about 60 dialog units wide on PC.
NOTES:   <flags> can be either 0, or STR_FLOAT, STR_SCIENTIFIC, or
         STR_FLOAT | STR_SCIENTIFIC.  So, if your number is a float, or you
         want it in scientific notation, you can specify that with the flags.
AUTHOR:  MJK
----------------------------------------------------------------------------- */
int xgPrec (double value, int &flags, int length /* =15 */)
{
  if (!_finite(value))
    return 0;
#define MAXFLOATDIGITS   7 // max sig figs available with single precision
#define MAXDOUBLEDIGITS 14 // max sig figs available with double precision

const int MAX_STRING_LENGTH(30);
const int MAX_FORMAT_LENGTH(10);

  double i; // integer part
  double f; // fractional part
  char istring[MAX_STRING_LENGTH];
  char fstring[MAX_STRING_LENGTH];
  char format[MAX_FORMAT_LENGTH];
  char *charptr;
  long long ipart=0;
  short ilength, flength; // string lengths of integer and fractional parts
  short prec;  // ends up as num sig figs to right of decimal
  short maxdigits; // max num digits to right of decimal to meet limit of 15
  bool isFraction = false;

  // Step 1 ////////////////////////////////////////////////////////////////////

  /* First test if we need scientific notation for example
     123456789012345

     -1000000000000.0  <- this number gets changed to...
     -1.0000000e+012   <- this
     -0.000001234567   <- this number gets changed to...
     -1.2345678e-006   <- this
  */

  if (length >= 8) { // Have to have at least 8 spaces to do sci notation
    double max;
    max = pow(10.0, (double)(length-3));
    const double min = 1e-5; // arbitrary value to use scientific notation below
    //const double toleranceValueForZero = 1e-15; // arbitrary value to make values
    //                                            // below zero (perhaps should be
    //                                            // user defined in future)
      // I'm returning this to the way it was because this change broke one of
      // our tutorials in which 2e-17 is a valid input but this change made it
      // impossible to enter that number and switched it to 0.0. I don't think
      // we can impose an arbitrary value here in this macro that will be
      // satisfactory to everyone in all cases. So, it should be done outside
      // of this macro. Please don't change this without discussing it with
      // everyone. -MJK
    if (GT_EPS(fabs(value), max, DBL_EPS) ||
        EQ_EPS(fabs(value), max, DBL_EPS) ||
        //(GT_TOL(fabs(value), 0.0, toleranceValueForZero) &&
        (GT_EPS(fabs(value), 0.0, DBL_EPS) &&
         LT_EPS(fabs(value), min, DBL_EPS)))
    {
      flags |= STR_SCIENTIFIC;
    }
  }

  // Step 2 ////////////////////////////////////////////////////////////////////

  /* Now find the number of sig figs there are to the right
     of the decimal.  The basic strategy is to
     get a string from the number and count the characters that are sig
     figs.  There are different ways to get a string from the number and
     we have tried and compared several in an attempt to get the best
     results.  The switch statement below has the different methods we've
     tried.  We keep the old stuff around because when we find that the
     current thing isn't working for some case, we can look back and see
     what we've already tried. */

  if (flags & STR_SCIENTIFIC) {
    int rval;
    if (flags & STR_FLOAT)
      rval = printf(istring, MAX_STRING_LENGTH, "%1.6e", value);
    else
      rval = printf(istring, MAX_STRING_LENGTH, "%1.15e", value);
    ASSERT(rval >= 0);
    strcpy(fstring, istring);
    charptr = strchr(istring, '.');
    (*charptr) = '\0';
    charptr = strchr(fstring, '.');
    if (charptr) {
      charptr++;
      strcpy(fstring, charptr);
    }
    charptr = strchr(fstring, 'e');
    (*charptr) = '\0';
  }
  else {
    int myvar = 3;
    CStr theval;
    switch (myvar) {
      case 1: // here we try getting all the digits we can and then formatting
              // the string afterward

        {
        // force the number into scientific and get all available valid digits
        if (flags & STR_FLOAT)
          theval.Format("%1.6e", value);
        else
          theval.Format("%1.13e", value);

        f = modf(value, &i);
        if (i > LLONG_MAX || i < LLONG_MIN)
          throw std::exception();
        ipart = (long long)i;
          // get a string from the number
#ifdef _DEBUG
        int rval(printf(istring, MAX_STRING_LENGTH, "%lld", ipart));
        ASSERT(rval >= 0);
#endif
        ilength = (short)strlen(istring); // (length doesn't include null terminator)

        // remove the period from the string
        theval.Remove('.');
        // remove the integer part from the front of the string
        theval.Delete(0, ilength);
        // delete the end of the string where 'e+004' is
        int indx;
        indx = theval.Find("e");
        theval.Delete(indx, theval.GetLength()-indx);
        // copy what is left into fstring
        strcpy(fstring, (LPCTSTR)theval);
        }
        break;
      case 2:
        {
        f = modf(value, &i);
        if (i > LLONG_MAX || i < LLONG_MIN)
          throw std::exception();
        ipart = (long long)i;
        int rval(printf(istring, MAX_STRING_LENGTH, "%lld", ipart));
        ASSERT(rval >= 0);
        // this sometimes results in a bunch of zeros when you don't want them.
        //sprintf(fstring, "%.25lf", f); // get a string from the number
          // get a string from the number
        rval = sprintf(fstring, "%Lg", f);
        ASSERT(rval >= 0);
        // Sometimes %g will use scientific notation, sometimes it won't.  If it
        // does, we've got to switch it to non-scientific notation.
        if (strstr(fstring, "e") || strstr(fstring, "E")) {
          double f2; // fractional part
          sscanf(fstring, "%lf", &f2);    // cast string back to a number
          sprintf(fstring, "%.16lf", f2); // get a string from the number
        }
        charptr = strchr(fstring, '.'); // get rid of the . on right hand side
        if (charptr) {
          charptr++;
          strcpy(fstring, charptr);
        }
        }
        break;
      case 3:
        {
        modf(value, &i);
        if (i > LLONG_MAX || i < LLONG_MIN)
          throw std::exception();
        ipart = (long long)i;
        int rval(printf(istring, MAX_STRING_LENGTH, "%lld", ipart));
        ASSERT(rval >= 0);
        ilength = (short)strlen(istring);
        if (i < 0) ilength--;

        int maxDigits;
        if (flags & STR_FLOAT)
          maxDigits = MAXFLOATDIGITS;
        else
          maxDigits = MAXDOUBLEDIGITS;
        if (ipart == 0 && value != 0.0)
        {
          isFraction = true;
          modf(log10(fabs(value)), &i);
          int digits = maxDigits - Round(i);
          if (value > 0.0)
            ilength = (short)(digits + 2);
          else
            ilength = (short)(digits + 3);
          rval = printf(format, MAX_FORMAT_LENGTH, "%s%d.%d%s",
                  "%", ilength, digits, "f");
        }
        else
          rval = printf(format, MAX_FORMAT_LENGTH, "%s%d.%d%s",
                  "%", ilength, Miabs(maxDigits - ilength), "f");
        ASSERT(rval >= 0);

        rval = printf(fstring, MAX_STRING_LENGTH, format, value);
        ASSERT(rval >= 0);

        charptr = strchr(fstring, '.'); // get rid of the . on right hand side
        if (charptr) {
          charptr++;
          strcpy(fstring, charptr);
        }
        }
        break;
    }
  }
  ilength = (short)strlen(istring); // (length doesn't include null terminator)
  flength = (short)strlen(fstring); // (length doesn't include null terminator)

  /* "Only about 7 decimal digits are representable in single-precision IEEE
     format, and about 16 in double-precision IEEE format" (The Perils of
     Floating-Point).  So subtract ilength (the length of the left side) from
     7 or 16 and see how many are left over on the right.  Start from that
     point and work left, skipping zeros until we hit a non-zero number. */
  if (value < 0) {
    ilength--; /* don't count the - sign as a sig fig. */
  }
  if (flags & STR_FLOAT) {
    if (isFraction)
      prec = flength;
    else
      prec = MAXFLOATDIGITS - ilength - 1;
  }
  else {
    prec = MAXDOUBLEDIGITS - ilength - 1;
  }
  if (ilength == 1 && ipart == 0) {
    prec++; // 0 on left of 0.1 isn't a sig fig, one more avail for right
  }
  if (prec >= flength) {
    prec = flength - 1; // just in case we screwed up some how
  }
  for (; prec >= 0; prec--) {
    if (fstring[prec] != '0')
      break;
  }
  prec++;
  // At this point prec is number of sig figs to the right of the decimal

  // Step 3 ////////////////////////////////////////////////////////////////////

  /* Find the max number of digits to the right of the decimal we can allow,
     based on a total number of <length> digits including the '-' and the '.'
     This is necessary because we might get round off errors above causing
     garbage values towards the end of the string causing prec to be more
     than it should be.  Here's an example if <length> is 15. */

  /*      123456789012345                        123456789012345  maxdigits  */
  /*                         fabs(value) >= fabs(-100000000000.0)     1      */
  /* fabs(-100000000000.0) > fabs(value) >= fabs(-10000000000.00)     2      */
  /* fabs(-10000000000.00) > fabs(value) >= fabs(-1000000000.000)     3      */
  /* fabs(-1000000000.000) > fabs(value) >= fabs(-100000000.0000)     4      */
  /* fabs(-100000000.0000) > fabs(value) >= fabs(-10000000.00000)     5      */
  /* fabs(-10000000.00000) > fabs(value) >= fabs(-1000000.000000)     6      */
  /* fabs(-1000000.000000) > fabs(value) >= fabs(-100000.0000000)     7      */
  /* fabs(-100000.0000000) > fabs(value) >= fabs(-10000.00000000)     8      */
  /* fabs(-10000.00000000) > fabs(value) >= fabs(-1000.000000000)     9      */
  /* fabs(-1000.000000000) > fabs(value) >= fabs(-100.0000000000)     10     */
  /* fabs(-100.0000000000) > fabs(value) >= fabs(-10.00000000000)     11     */
  /* fabs(-10.00000000000) > fabs(value) >= fabs(-1.000000000000)     12     */
  /* fabs(-1.000000000000) > fabs(value) >= fabs(-.1000000000000)     12     */
  /* fabs(-.1000000000000) > fabs(value) >= fabs(-.0100000000000)     12     */

  if (flags & STR_SCIENTIFIC) {
    maxdigits = (short)(length - 8); // 8 places used for -1. and e+012
  }
  else {
    maxdigits = (short)(length - 3); // 3 because 1 for + or -, 1 for 0, 1 for . (-0.)
    double testval;
    testval = 10.0;
    if (!LT_EPS(fabs(value), testval, DBL_EPS)) {
      do {
        maxdigits--;
        testval *= 10.0;
      }
      while (!LT_EPS(fabs(value), testval, DBL_EPS));
    }
  }
  if (!(value < 0.0)) {
    maxdigits++; // If value is positive, you get one more
  }

  // Step 4 ////////////////////////////////////////////////////////////////////

  // Compare the maximum number of significant digits to the right of the
  // decimal allowed <maxdigits> with how many we have <prec>.  If we have
  // more than we're allowed, we have to throw out the extra.   If we have
  // to throw some out, we have to see if the ones we have left are
  // still significant - if the ones on the right are 0, then they're no
  // longer significant.

  if (prec > maxdigits) {
    prec = maxdigits;
    for (; prec >= 0; prec--) {
      if (fstring[prec-1] != '0') {
        break;
      }
    }
  }
  else if (prec < 1) {
    prec = 1; // always show 5.0 instead of just 5 to indicate it's a float
  }

  return prec;

} // xgPrec
///////////////////////////////////////////////////////////////////////////////
//  PURPOSE     Get a properly formatted string from a double
//  NOTES       The reason for the short name is so that we can easily
//                replace code like this:
//                  fprintf(fp, "%g %g", value1, value2);
//                with code like this:
//                  fprintf(fp, "%s %s", STR(value1), STR(value2));
//                <val> is the floating point number to be formatted as a string
//                <n> is the MAX precision, -1 (default) is auto compute
//                <width> is the MAX num characters (15 by default)
//                <flags> bitwise flags:
//                  STR_FLOAT
//                    treat value as floating point value (i.e. only 7 digits of
//                     precision are available)
//                  STR_SCIENTIFIC
//                    force use of scientific notation
//                  STR_USEMAXPREC
//                    this only applies when a_n is not -1.
//                    a_n value is the desired precision. When this flag is set,
//                     the returned string may have less precision because
//                     trailing zeros are removed. When this flag is not set,
//                     they are not removed.
//                    example:  a_n = 3 and val = 5.0003
//                     with this flag, returned string is "5.0". Without this
//                     flag, the returned string is "5.000".
//                  STR_FULLWIDTH
//                    forces string to be 'width' in length. If the number is
//                     less than this width, it is padded with spaces on the right
//                     side
///////////////////////////////////////////////////////////////////////////////
CStr STR (int val, int  /*=-1*/, int  /*=15*/, int  /*=0*/)
{
  CStr str;
  str.Format("%d", val);
  return str;
}
CStr STR (float val, int a_n /*=-1*/, int width /*=15*/, int flags /*=0*/)
{
  double d(val);
  flags |= STR_FLOAT;
  return (STR(d, a_n, width, flags));
}
CStr STR (double val, int a_n /*=-1*/, int width /*=15*/, int flags /*=0*/)
{
  CStr            str;
  int             prec=0;

    // put this check in for old stuff
  if (a_n >= 800) {
    ASSERT(FALSE);
    a_n = -1;
  }

    // check for invalid values
  if (!_finite(val)) {
    return "NaN";
  }

    // if not specifying an exact prec, autocompute
  if ((a_n == -1) || (flags & STR_USEMAXPREC)) {
      // if using max precision and the number is small, truncate the number to
      // max precision here before Prec() switches the number to scientific
      // notation (a number that is beyond the limited precision range)
    if (BIT_TEST(flags, STR_USEMAXPREC) && LT_TOL(fabs(val), 1e-4, DBL_EPS)) {
      val *= pow(10.0, a_n);
      val = floor(val + 0.5);
      val /= pow(10.0, a_n);
    }
      // figure out the auto-computed prec is
    try {
      prec = xgPrec(val, flags, width);
    }
    catch (std::exception &) {
        // we had an error. Generally with casting the modf integer part
      return "";
    }
    if ((flags & STR_USEMAXPREC) && prec > a_n) {
      prec = a_n;
    }
  }
  else {
    prec = a_n;
  }

  // Format the format string, then use it to format str

  //char            format[20];
  CStr            format;
  if (flags & STR_SCIENTIFIC)
  {
    //sprintf(format, "%%.%de", Miabs(prec));
    format.Format("%%.%de", Miabs(prec));
  }
  else
  {
    //sprintf(format, "%%.%dlf", prec);
     // I added Miabs because according to my book it must be nonnegative
     // and some combinations result in a negative prec, which crashes. -MJK
    format.Format("%%.%dlf", Miabs(prec));
  }

  str.Format(format, val);

    // in some cases when specifying max prec, trailing zeros can occur. For
    // example, if the number is 5.0003 and the maxprec is 3, the string right
    // here will be 5.000 instead of 5.0.
  if ((a_n == -1) || (flags & STR_USEMAXPREC)) {
      // see if there's a decimal point
    int index;
    index = str.Find('.');
    if (index >= 0) {
      if ((index = str.FindOneOf("eE")) >= 0) {
        // Scientific.  Start at 'e' or 'E' and go left, removing zeros
        index--;
        while (index > 0 && str.GetAt(index) == '0' &&
               str.GetAt(index - 1) != '.') {
          str.Delete(index);
          index--;
        }
      }
      else {
          // take off any trailing zeros
        str.TrimRight('0');
          // make sure there's at least one zero
        if (str[str.GetLength()-1] == '.') {
          str += '0';
        }
      }
    }
  }

  // in some cases you want the string to always be a certain number of
  // character. This is most common with FORTRASH fixed format garbage.
  if (flags & STR_FULLWIDTH) {
    int     diff;
    diff = width - str.GetLength();
    if (diff > 0) {
      CStr str1, str2;
      for (int i = 0; i < diff; i++)
        str1 += " ";
      str2 = str;
      str = str1 + str2;
    }
  }

    // if the string is "-0.0" remove the negative sign
  if (str == "-0.0") {
    str = "0.0";
  }

  return str;
} // STR
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
util::CaseInsensitiveEqual::CaseInsensitiveEqual(const CStr& a_toFind)
: m_toFind(a_toFind)
{
  m_toFind.ToLower();
}
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
bool util::CaseInsensitiveEqual::operator()(const CStr& a_toTest) const
{
  CStr toTest(a_toTest);
  toTest.ToLower();
  return m_toFind == toTest;
}
//------------------------------------------------------------------------------
/// \brief Round a double to an nearest integer value
//------------------------------------------------------------------------------
long util::lrint(double x)
{
  if (x > 0.0)
    return static_cast<long>(x + 0.5);
  else
    return static_cast<long>(x - 0.5);
}
//----------------------------------------------------------------------------
/// \brief Retrieve index from two dimensional FORTRAN array (1 based).
//----------------------------------------------------------------------------
int util::ForIndex (int a_first, int a_second, int a_firstSize)
{
  return (a_second - 1)*a_firstSize + a_first - 1;
} // ForIndex

#ifdef CXX_TEST

#include <private/util/util.t.h>
#include <sstream>
#include <private/MfLibAsserts.h>

#define TS_ASSERT_STR_MATCH(expected, value)                                   \
  AssertStrMatch(__FILE__, __LINE__, expected, value)
template <class T>
void AssertStrMatch(const char* a_file, int a_line, const char* a_expected,
                    T a_value)
{
  CStr output(STR(a_value));
  CStr expected(a_expected);
  _TS_ASSERT_EQUALS2(a_file, a_line, expected, output);
}
#define TS_ASSERT_STR_MATCH_FULL(expected, value, n, width, flags)             \
  AssertStrMatchFull(__FILE__, __LINE__, expected, value, n, width, flags)
template <class T>
void AssertStrMatchFull(const char* a_file, int a_line, const char* a_expected,
                        T a_value, int a_n, int a_width, int a_flags)
{
  CStr output(STR(a_value, a_n, a_width, a_flags));
  CStr expected(a_expected);
  _TS_ASSERT_EQUALS2(a_file, a_line, expected, output);
}

namespace {
  //----------------------------------------------------------------------------
  /// \brief
  //----------------------------------------------------------------------------
  std::set<std::string> iGetDirFiles (const std::string& a_directory)
  {
    std::set<std::string> modelFiles;
#ifndef _MSC_VER
    throw std::runtime_error("This function is not implemented outside of windows");
#else
    // loop through the files in directory
    WIN32_FIND_DATA filedata;
    HANDLE h=NULL;

    // get current folder if we can (return on error)
    CStr folder = a_directory.c_str();
    folder += "\\*.*";

    // find first one
    h = FindFirstFile(folder, &filedata);
    if (h == INVALID_HANDLE_VALUE)
    {
      // unable to find folder
      return modelFiles;
    }

    // loop until done finding files
    do {
      std::string currentFile = filedata.cFileName;

      // skip current and parent directories
      if (currentFile == "." || currentFile == "..")
        continue;

      currentFile = a_directory + "\\" + currentFile;
      modelFiles.insert(currentFile);

    } while (FindNextFile(h, &filedata) != 0);

      // close handle
    FindClose(h);
#endif // _MSC_VER
    return modelFiles;
  } // iGetDirFiles
  //----------------------------------------------------------------------------
  /// \brief
  //----------------------------------------------------------------------------
  void iCompareTwoTextFiles (const std::string& a_file, unsigned a_line,
                              const std::string& a_expected,
                              const std::string& a_output)
  {
    
    MfLibAsserts::TextFilesEqual(a_file, a_line, a_expected, a_output);
  } // iCompareTwoTextFiles
}

namespace util {
//------------------------------------------------------------------------------
/// \brief
//------------------------------------------------------------------------------
void DirectoryFilesEqual (const char* a_file,
                          unsigned a_line,
                          const std::string& a_dirExpected,
                          const std::string& a_dirOut)
{
  using std::set;
  using std::string;
  string result;

  set<string> eFiles = iGetDirFiles(a_dirExpected);
  set<string> oFiles = iGetDirFiles(a_dirOut);

  // if either directory has no files then report the directory missing
  if (eFiles.empty() || oFiles.empty())
  {
    if (eFiles.empty())
    {
      result += "Folder missing or empty: ";
      result += a_dirExpected;
      result += "\n";
    }

    if (oFiles.empty())
    {
      result += "Folder missing or empty: ";
      result += a_dirOut;
      result += "\n";
    }
    _TS_FAIL(a_file, a_line, result.c_str());
    return;
  }

  set<string>::iterator eCurr = eFiles.begin();
  set<string>::iterator oCurr = oFiles.begin();

  while (eCurr != eFiles.end() || oCurr != oFiles.end())
  {
    CStr o, e;
    if (eCurr != eFiles.end())
    {
      util::StripPathFromFilename(eCurr->c_str(), e);
    }
    if (oCurr != oFiles.end())
    {
      util::StripPathFromFilename(oCurr->c_str(), o);
    }
    if (eCurr == eFiles.end())
    {
      // report file missing from expected
      string message = "No matching file found for: ";
      message += *oCurr;
      _TS_FAIL(a_file, a_line, message.c_str());
      ++oCurr;
    }
    else if (oCurr == oFiles.end())
    {
      // report file missing from output
      string message = "No matching file found for: ";
      message += *eCurr;
      _TS_FAIL(a_file, a_line, message.c_str());
      ++eCurr;
    }
    else if (o > e)
    {
      // report file missing from output
      string message = "No matching file found for: ";
      message += *eCurr;
      _TS_FAIL(a_file, a_line, message.c_str());
      ++eCurr;
    }
    else if (e > o)
    {
      // report file missing from expected
      string message = "No matching file found for: ";
      message += *oCurr;
      _TS_FAIL(a_file, a_line, message.c_str());
      ++oCurr;
    }
    else
    {
      iCompareTwoTextFiles(a_file, a_line, *eCurr, *oCurr);
      ++eCurr;
      ++oCurr;
    }
  }
} // DirectoryFilesEqual
} // namespace util {

////////////////////////////////////////////////////////////////////////////////
// Class UtilsTests
////////////////////////////////////////////////////////////////////////////////
// ---------------------------------------------------------------------------
/// \brief
// ---------------------------------------------------------------------------
void UtilsTests::testLargeNumbers ()
{
  TS_ASSERT_STR_MATCH("987654321.0", 987654321.);
  TS_ASSERT_STR_MATCH("987654321012.0", 987654321012.);
  TS_ASSERT_STR_MATCH("9.87654321e+012", 9876543210123.);
  TS_ASSERT_STR_MATCH("9.87654321e+028", 98765432101234567890123456789.);
  TS_ASSERT_STR_MATCH("9876.54321012", 9876.54321012);
  TS_ASSERT_STR_MATCH("9876.5432101235", 9876.5432101234567890);
  TS_ASSERT_STR_MATCH_FULL("", 3.8548460522580426e+141, -1, 5, 0);
  TS_ASSERT_STR_MATCH_FULL("3.9e+141", 3.8548460522580426e+141, -1, 8, 0);
} // UtilsTests::testLargeNumbers
// ---------------------------------------------------------------------------
/// \brief
// ---------------------------------------------------------------------------
void UtilsTests::testSmallNumbers ()
{
  TS_ASSERT_STR_MATCH("0.987654321", 0.987654321);
  TS_ASSERT_STR_MATCH("0.9876543210123", 0.9876543210123456789);
  TS_ASSERT_STR_MATCH("9.87654321e-006", 0.000009876543210123);

  TS_ASSERT_STR_MATCH("1.0e-011", 0.00000000001);
  TS_ASSERT_STR_MATCH("1.0e-017", 0.00000000000000001);
  TS_ASSERT_STR_MATCH("-1.0e-017", -0.00000000000000001);

  // Test STR_USEMAXPREC
    // The test results seem to me to be the inverse of the name of the flag.
  TS_ASSERT_STR_MATCH_FULL("5.000", 5.0003, 3, 15, 0);
  TS_ASSERT_STR_MATCH_FULL("5.0", 5.0003, 3, 15, STR_USEMAXPREC);

  TS_ASSERT_STR_MATCH("0.012345", 0.012345);
  TS_ASSERT_STR_MATCH("0.0012345", 0.0012345);
  TS_ASSERT_STR_MATCH("0.00012345", 0.00012345);
  TS_ASSERT_STR_MATCH("0.000012345", 0.000012345);
  TS_ASSERT_STR_MATCH("1.2345e-006", 0.0000012345);
  TS_ASSERT_STR_MATCH("1.2345e-007", 0.00000012345);
  TS_ASSERT_STR_MATCH("-0.012345", -0.012345);
  TS_ASSERT_STR_MATCH("-0.0012345", -0.0012345);
  TS_ASSERT_STR_MATCH("-0.00012345", -0.00012345);
  TS_ASSERT_STR_MATCH("-0.000012345", -0.000012345);
  TS_ASSERT_STR_MATCH_FULL("-0.00001235", -0.000012345, -1, 11, 0);
  TS_ASSERT_STR_MATCH("-1.2345e-006", -0.0000012345);
  TS_ASSERT_STR_MATCH("-1.2345e-007", -0.00000012345);
  TS_ASSERT_STR_MATCH("0.0000656", 6.56E-05);
  TS_ASSERT_STR_MATCH("0.00006561234", 6.561234E-05);
  TS_ASSERT_STR_MATCH("0.3", 0.3);
  TS_ASSERT_STR_MATCH("0.03", 0.03);
  TS_ASSERT_STR_MATCH("0.003", 0.003);
  TS_ASSERT_STR_MATCH("0.0003", 0.0003);
  TS_ASSERT_STR_MATCH("0.00003", 0.00003);
  TS_ASSERT_STR_MATCH("3.0e-006", 0.000003);
  TS_ASSERT_STR_MATCH("3.0e-007", 0.0000003);
  TS_ASSERT_STR_MATCH("-0.3", -0.3);
  TS_ASSERT_STR_MATCH("-0.03", -0.03);
  TS_ASSERT_STR_MATCH("-0.003", -0.003);
  TS_ASSERT_STR_MATCH("-0.0003", -0.0003);
  TS_ASSERT_STR_MATCH("-0.00003", -0.00003);
  TS_ASSERT_STR_MATCH("-3.0e-006", -0.000003);
  TS_ASSERT_STR_MATCH("-3.0e-007", -0.0000003);

  TS_ASSERT_STR_MATCH("0.012345", (float)0.012345);
  TS_ASSERT_STR_MATCH("0.0012345", (float)0.0012345);
  TS_ASSERT_STR_MATCH("0.00012345", (float)0.00012345);
  TS_ASSERT_STR_MATCH("0.000012345", (float)0.000012345);
  TS_ASSERT_STR_MATCH("1.2345e-006", (float)0.0000012345);
  TS_ASSERT_STR_MATCH("1.2345e-007", (float)0.00000012345);
  TS_ASSERT_STR_MATCH("-0.012345", (float)-0.012345);
  TS_ASSERT_STR_MATCH("-0.0012345", (float)-0.0012345);
  TS_ASSERT_STR_MATCH("-0.00012345", (float)-0.00012345);
  TS_ASSERT_STR_MATCH("-0.000012345", (float)-0.000012345);
  TS_ASSERT_STR_MATCH("-1.2345e-006", (float)-0.0000012345);
  TS_ASSERT_STR_MATCH("-1.2345e-007", (float)-0.00000012345);
  TS_ASSERT_STR_MATCH("0.0000656", (float)6.56E-05);
  TS_ASSERT_STR_MATCH("0.00006560123", (float)6.5601234E-05);
  TS_ASSERT_STR_MATCH("0.3", (float)0.3);
  TS_ASSERT_STR_MATCH("0.03", (float)0.03);
  TS_ASSERT_STR_MATCH("0.003", (float)0.003);
  TS_ASSERT_STR_MATCH("0.0003", (float)0.0003);
  TS_ASSERT_STR_MATCH("0.00003", (float)0.00003);
  TS_ASSERT_STR_MATCH("3.0e-006", (float)0.000003);
  TS_ASSERT_STR_MATCH("3.0e-007", (float)0.0000003);
  TS_ASSERT_STR_MATCH("-0.3", (float)-0.3);
  TS_ASSERT_STR_MATCH("-0.03", (float)-0.03);
  TS_ASSERT_STR_MATCH("-0.003", (float)-0.003);
  TS_ASSERT_STR_MATCH("-0.0003", (float)-0.0003);
  TS_ASSERT_STR_MATCH("-0.00003", (float)-0.00003);
  TS_ASSERT_STR_MATCH("-3.0e-006", (float)-0.000003);
  TS_ASSERT_STR_MATCH("-3.0e-007", (float)-0.0000003);
} // UtilsTests::testSmallNumbers
// ---------------------------------------------------------------------------
/// \brief
// ---------------------------------------------------------------------------
void UtilsTests::testStripFileFromName ()
{
  CStr path;
  util::StripFileFromFilename("c:\\path\\to\\file\\file.txt", path);
  TS_ASSERT_EQUALS2("c:\\path\\to\\file\\", path);

  util::StripFileFromFilename("c:\\path\\to\\file\\", path);
  TS_ASSERT_EQUALS2("c:\\path\\to\\file\\", path);
} // UtilsTests::testStripFileFromName
// ---------------------------------------------------------------------------
/// \brief
// ---------------------------------------------------------------------------
void UtilsTests::testResolveRelativePath ()
{
  using util::ResolveRelativePath;
  TS_ASSERT_EQUALS2("C:\\temp\\filename.txt",
                    ResolveRelativePath("C:\\temp\\", "filename.txt"));
  TS_ASSERT_EQUALS2("C:\\temp\\model\\filename.txt",
                    ResolveRelativePath("C:\\temp", ".\\model\\filename.txt"));
  TS_ASSERT_EQUALS2("C:\\temp\\filename.txt",
                    ResolveRelativePath("C:\\temp\\model", "..\\filename.txt"));
  TS_ASSERT_EQUALS2("\\\\maple\\c\\temp\\filename.txt",
                    ResolveRelativePath("\\\\maple\\c\\temp\\model",
                                        "..\\filename.txt"));
  TS_ASSERT_EQUALS2("D:\\temp\\filename.txt",
                    ResolveRelativePath("C:\\temp", "D:\\temp\\filename.txt"));
  TS_ASSERT_EQUALS2("C:\\temp\\..\\okay\\filename.txt",
                    ResolveRelativePath("C:\\temp\\..\\okay", "filename.txt"));
} // UtilsTests::testResolveRelativePath
// ---------------------------------------------------------------------------
/// \brief
// ---------------------------------------------------------------------------
void UtilsTests::testRemoveCommonPath ()
{
  using util::ResolveRelativePath;
  
  {
    // all different
    std::vector<CStr> paths;
    paths.push_back("C:\\");
    paths.push_back("D:\\");
    paths.push_back("E:\\");
    util::RemoveCommonPath(paths);
    CStr expected[] = {
      "C:\\",
      "D:\\",
      "E:\\"
    };
    TS_ASSERT_EQUALS_AVEC(expected, 3, paths);
  }
  {
    // all the same
    std::vector<CStr> paths;
    paths.push_back("C:\\");
    paths.push_back("C:\\");
    paths.push_back("C:\\");
    util::RemoveCommonPath(paths);
    CStr expected[] = {
      "",
      "",
      ""
    };
    TS_ASSERT_EQUALS_AVEC(expected, 3, paths);
  }
  {
    // all the same
    std::vector<CStr> paths;
    paths.push_back("C:\\hello\\world\\usa.nam");
    paths.push_back("C:\\hello\\world_mexico\\mexico.nam");
    paths.push_back("C:\\hello\\world_canada\\canada.nam");
    util::RemoveCommonPath(paths);
    CStr expected[] = {
      "world\\usa.nam",
      "world_mexico\\mexico.nam",
      "world_canada\\canada.nam"
    };
    TS_ASSERT_EQUALS_AVEC(expected, 3, paths);
  }
} // UtilsTests::testResolveRelativePath

#endif // CXX_TEST

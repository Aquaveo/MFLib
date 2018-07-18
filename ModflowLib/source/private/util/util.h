//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef UTIL_H
#define UTIL_H

// Use old HDF5 1.6.6 interface in drastically changed HDF5 1.8.1
#define H5Gcreate_vers 1
#define H5Gopen_vers 1
#define H5Eset_auto_vers 1
#define H5Eget_auto_vers 1
#define H5Eset_auto_vers 1
#define H5Dopen_vers 1
#define H5Dcreate_vers 1
#define H5Acreate_vers 1

#include <float.h>
#include <vector>
#include <map>
#include <set>
#include <string>
#include <hdf5.h>
#include <private/util/StdString.h>
#include <private/util/CToken.h>
#include <private/util/dblprec.h>
#include <private/util/EException.h>
#include <private/util/ErrorStack.h>
#include <private/util/H5Initialize.h>
#include <private/util/ContArray.h>
#include <private/util/LateLoad.h>

#define CXXDELTA 1e-6

// turns constant into float or double depending on build type
#ifdef DBLPREC
#define REAL(x) x
#else
#define REAL(x) x ## f
#endif

// for TODO #pragma
#define CAT_STRINGS(x) #x
#define INT_TO_STRING(x) CAT_STRINGS(x)
#define TODO(x) (__FILE__"(" INT_TO_STRING(__LINE__) "): "x)

#ifdef CXX_TEST
#define _TS_DIRECTORY_FILES_EQUAL(f, l, expected, out)                         \
  util::DirectoryFilesEqual(f, l, expected, out)
#define TS_DIRECTORY_FILES_EQUAL(expected, out)                                \
  _TS_DIRECTORY_FILES_EQUAL(__FILE__, __LINE__, expected, out)
#endif

typedef std::vector< std::pair<int, int> > VEC_INT_PAIR;
typedef unsigned int Uint;

namespace util
{
  int   RealWidth();
  char* NewCharArray(size_t a_);
  int*  NewIntArray(size_t a_);
  Real* NewRealArray(size_t a_);
  CStr GetStr(const char *a_,
              int a_strLen);
  void NullFuncArg(const char *a_,
                   int a_line);
  void GetBinDirectory(CStr &a_);
  void GetTempDirectory(CStr &a_);
  void GetTestFilesDirectory(CStr &a_);
  CStr GetTestFilesDirectory();
  void GetTempDirectory(CStr &a_);
  void StripFileFromFilename(const char *a_fWithPath,
                             CStr &a_path);
  void StripPathFromFilename(const char *a_fWithPath,
                             CStr &a_file);
  void StripAllButExtension(const char *a_fWithPath,
                            CStr &a_ext);
  void StripExtensionFromFilename(const char *a_fWithPath,
                                  CStr &a_filename);
  CStr ResolveRelativePath(const CStr& a_path, const CStr& a_filename);
  void RemoveCommonPath(std::vector<CStr>& a_paths);
  bool DeleteDir(const char *a_);
  bool CopyDir(const char *a_src,
               const char *a_dest);
  bool FileCopy(const char *a_src,
                const char *a_dest);
  bool FileExists(const char *a_filePath);
  long lrint(double x);
  int ForIndex(int a_first, int a_second, int a_firstSize);

#ifdef CXX_TEST
  void DirectoryFilesEqual(const char* a_file,
                           unsigned a_line,
                           const std::string& a_dirExpected,
                           const std::string& a_dirOut);
#endif

  //----------------------------------------------------------------------------
  /// \brief Retrieve element from two dimensional FORTRAN array (1 based).
  //----------------------------------------------------------------------------
  template <class T>
  T& ForElement(T* a_, int a_first, int a_second, int a_firstSize)
  {
    T& result = a_[ForIndex(a_first, a_second, a_firstSize)];
    return result;
  } // ForElement

  //----------------------------------------------------------------------------
  /// \brief Retrieve element from two dimensional FORTRAN array (1 based).
  //----------------------------------------------------------------------------
  template <class T>
  T& ForElement(std::vector<T>& a_, int a_first, int a_second, int a_firstSize)
  {
    T& result = a_[ForIndex(a_first, a_second, a_firstSize)];
    return result;
  } // ForElement
  
  ////////////////////////////////////////////////////////////////////////////////
  /// \class CaseInsensitiveEqual
  ////////////////////////////////////////////////////////////////////////////////
  //------------------------------------------------------------------------------
  /// \brief Functor class that can be used for case insensitive equal in STL
  ///        algorithm templates like find_if.
  //------------------------------------------------------------------------------
  class CaseInsensitiveEqual
  {
  public:
    CaseInsensitiveEqual(const CStr& a_toFind);
    bool operator()(const CStr& a_toTest) const;

  private:
    CStr m_toFind;
  };
}

#define FLT_EPS    FLT_EPSILON         // 1.19209290E-07
#define DBL_EPS    DBL_EPSILON         // 2.2204460492503131E-16
#define STR_FLOAT      0x01
#define STR_SCIENTIFIC 0x02
#define STR_USEMAXPREC 0x04
#define STR_FULLWIDTH  0x08
#define Miabs(a)     (((a) >= 0) ? (a) : (-(a)))
// The following 3 macros ending with EPS should have an epsilon
// value passed to them.  This should be something like FLT_EPS or
// DBL_EPS or 1e-6 etc.  The epsilon value is multiplied by the
// sum of the two floats to compute a tolerance

#define EQ_EPS(A, B, epsilon) (fabs((A) - (B)) <= fabs(((A) + (B)) * (epsilon)))
#define LT_EPS(A, B, epsilon) (((B) - (A)) > fabs(((A) + (B)) * (epsilon)))
#define GT_EPS(A, B, epsilon) (((A) - (B)) > fabs(((A) + (B)) * (epsilon)))
#define LTEQ_EPS(A, B, epsilon) (LT_EPS((A), (B), (epsilon)) || EQ_EPS((A), (B), (epsilon)))
#define GTEQ_EPS(A, B, epsilon) (GT_EPS((A), (B), (epsilon)) || EQ_EPS((A), (B), (epsilon)))

CStr STR(double val, int a_n=-1, int width=15, int flags=0);
CStr STR(float val, int a_n=-1, int width=15, int flags=0);
CStr STR(int val, int a_n=-1, int width=15, int flags=0);

#endif


//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef MFPACKAGE_H
#define MFPACKAGE_H

//----- Included files ---------------------------------------------------------
#include <vector>
#include <private\util\StdString.h>

//----- Forward declarations ---------------------------------------------------

//----- Classes ----------------------------------------------------------------

namespace MfData
{
  class TmpPackageNameChanger;
  class MfPackage
  {
    friend TmpPackageNameChanger;
  public:
    MfPackage(const char *a_);
    MfPackage(const MfPackage &rhs);
    ~MfPackage();
    const MfPackage& operator=(const MfPackage &rhs);

    CStr PackageName() const;

    void RemoveField(const char* a_field);
    bool SetField(const char *a_field,
                  const float *a_);
    bool SetField(const char *a_field,
                  const double *a_);
    bool SetField(const char *a_field,
                  const int *a_);
    bool SetField(const char *a_field,
                  const char *a_);
    bool GetField(const char *a_field,
                  const float **a_) const;
    bool GetField(const char *a_field,
                  const double **a_) const;
    bool GetField(const char *a_field,
                  const int** a_) const;
    bool GetField(const char *a_field,
                  const char **a_) const;

    std::vector<CStr> FieldNames();
    std::vector<CStr>& StringsToWrite();
    std::vector<CStr>& StringDescriptions();
    void SetLineNumber(const char *a_);
    const char* GetLineNumber() const;

  private:

    class impl;
    impl *m_p;
  };
  class TmpPackageNameChanger
  {
  public:
    TmpPackageNameChanger(MfPackage* a_p,
                          const char* a_tmpName);
    ~TmpPackageNameChanger();
  private:
    MfPackage* m_pack;
    CStr       m_origName;
  };
}

#endif


//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEPACKEXP_H
#define NATIVEPACKEXP_H

#include <private\util\util.h>

namespace MfData
{
  class MfGlobal;
  class MfPackage;
  namespace Export
  {
    class Mf2kNative;
    class NeArealPar;
    class NativeExpSTP;
    class H5ArrayWriter;
    class H5UseLastWriter;
    class H5BcList;

    class NativePackExp
    {
      friend NativeExpSTP;
      friend NeArealPar;

    public:
      NativePackExp();
      virtual ~NativePackExp();
      virtual bool Export()=0;

      void SetData(Mf2kNative* a_native,
                   MfGlobal* a_global,
                   MfPackage* a_package);
      void SetH5Flag (bool a_h5);

      Mf2kNative*    GetNative();
      MfGlobal*      GetGlobal();
      const MfGlobal* GetGlobal() const;
      MfPackage*     GetPackage();
    protected:
      bool           GetH5Flag();
      virtual void   AddToStoredLinesDesc(const char* a_line,
                                          const char* a_desc);
      void           AddToStoredLinesDesc(const std::vector<CStr>& a_line,
                                          const std::vector<CStr>& a_desc);
      bool           WriteComments();
      bool           WriteStoredLines();
      bool           SkipPar_Pval_Sen(const CStr& a_name);
      virtual void   LastChanceBeforeWriting();
      bool           ClearFile();

      void           UnitTestingDeletePointers();

    private:
      NativePackExp(const NativePackExp& rhs);
      const NativePackExp& operator=(const NativePackExp& rhs);

      virtual void OnSetData() {}

      class impl;
      impl* m_p;
    };

  }
}

#endif

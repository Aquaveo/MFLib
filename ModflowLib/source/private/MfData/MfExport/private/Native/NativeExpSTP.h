//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPSTP_H
#define NATIVEEXPSTP_H

class NativeExpSTPT;

#include <private\MfData\MfExport\private\Native\NativePackExp.h>
namespace MfData
{
  namespace Export
  {
    class NativeExpSTP : public NativePackExp
    {
      friend NativeExpSTPT;
    public:
      NativeExpSTP();
      ~NativeExpSTP();
      virtual bool Export();

    private:
      NativeExpSTP(const NativeExpSTP& rhs);
      const NativeExpSTP& operator=(const NativeExpSTP& rhs);

      void ForcePackageWrite(const char* const a_);
      void CopyAdditionalFiles();
      void ShowWarnings();
      void ExpParamFile();
      void ExpSuperFile();
    };

  }
}

#endif


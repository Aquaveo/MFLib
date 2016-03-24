//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPVSC_H
#define NATIVEEXPVSC_H

class NativeExpVscT;

#include <private\MfData\MfExport\private\Native\NativePackExp.h>
namespace MfData
{
  namespace Export
  {
    class NativeExpVsc : public NativePackExp
    {
      friend NativeExpVscT;
    public:
      NativeExpVsc();
      ~NativeExpVsc();
      virtual bool Export();

    private:
      NativeExpVsc(const NativeExpVsc& rhs);
      const NativeExpVsc& operator=(const NativeExpVsc& rhs);

      void Lines1to3();
      void Lines4to5();
      CStr Desc(const char* a_line);

    };

  }
}

#endif


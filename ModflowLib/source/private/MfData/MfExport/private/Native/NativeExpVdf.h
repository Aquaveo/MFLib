//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPVDF_H
#define NATIVEEXPVDF_H

class NativeExpVdfT;

#include <private\MfData\MfExport\private\Native\NativePackExp.h>
namespace MfData
{
  namespace Export
  {
    class NativeExpVdf : public NativePackExp
    {
      friend NativeExpVdfT;
    public:
      NativeExpVdf();
      ~NativeExpVdf();
      virtual bool Export();

    private:
      NativeExpVdf(const NativeExpVdf& rhs);
      const NativeExpVdf& operator=(const NativeExpVdf& rhs);

      void Lines1to5();
      void Lines6to7();
      CStr Desc(const char* a_line);

    };

  }
}

#endif


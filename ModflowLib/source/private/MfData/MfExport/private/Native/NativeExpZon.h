//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPZON_H
#define NATIVEEXPZON_H

class NativeExpZonT;

#include <private\MfData\MfExport\private\Native\NativePackExp.h>
namespace MfData
{
  namespace Export
  {
    class NativeExpZon : public NativePackExp
    {
      friend NativeExpZonT;
    public:
      NativeExpZon();
      ~NativeExpZon();
      virtual bool Export();

    private:
      NativeExpZon(const NativeExpZon& rhs);
      const NativeExpZon& operator=(const NativeExpZon& rhs);

      CStr Desc(int a_line);
    };

  }
}

#endif


//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPPES_H
#define NATIVEEXPPES_H

class NativeExpPesT;

#include <private\MfData\MfExport\private\Native\NativePackExp.h>
namespace MfData
{
  namespace Export
  {
    class NativeExpPes : public NativePackExp
    {
      friend NativeExpPesT;
    public:
      NativeExpPes();
      ~NativeExpPes();
      virtual bool Export();

    private:
      NativeExpPes(const NativeExpPes& rhs);
      const NativeExpPes& operator=(const NativeExpPes& rhs);

      CStr Desc(int a_line);

    };

  }
}

#endif


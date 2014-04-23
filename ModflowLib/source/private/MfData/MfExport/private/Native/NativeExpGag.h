//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NativeExpGag_H
#define NativeExpGag_H

class NativeExpGagT;

#include <private\MfData\MfExport\private\Native\NativePackExp.h>
namespace MfData
{
  namespace Export
  {
    class NativeExpGag : public NativePackExp
    {
      friend NativeExpGagT;
    public:
      NativeExpGag();
      ~NativeExpGag();
      virtual bool Export();

    private:
      NativeExpGag(const NativeExpGag& rhs);
      const NativeExpGag& operator=(const NativeExpGag& rhs);

      CStr Desc(int a_line);
      CStr Line1();
      void Line2();
    };

  }
}

#endif


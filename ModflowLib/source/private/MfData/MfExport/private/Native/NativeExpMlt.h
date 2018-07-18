//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPMLT_H
#define NATIVEEXPMLT_H

class NativeExpMltT;

#include <private/MfData/MfExport/private/Native/NativePackExp.h>
namespace MfData
{
  namespace Export
  {
    class NativeExpMlt : public NativePackExp
    {
      friend NativeExpMltT;
    public:
      NativeExpMlt();
      ~NativeExpMlt();
      virtual bool Export();

    private:
      NativeExpMlt(const NativeExpMlt& rhs);
      const NativeExpMlt& operator=(const NativeExpMlt& rhs);

      CStr Desc(int a_line);
    };

  }
}

#endif


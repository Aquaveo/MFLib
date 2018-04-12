//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPMF6IC_H
#define NATIVEEXPMF6IC_H

#include <private\util\StdString.h>

namespace MfData
{
  namespace Export
  {
    class NativePackExp;
    class NativeExpMf6Ic
    {
    public:
      NativeExpMf6Ic(NativePackExp* a_);
      ~NativeExpMf6Ic();
      virtual bool Export();

    private:
      NativeExpMf6Ic(const NativeExpMf6Ic& rhs);
      const NativeExpMf6Ic& operator=(const NativeExpMf6Ic& rhs);
      CStr GenerateHeadlayer();    

      NativePackExp* m_pack;
    };

  }
}

#endif


//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPMF6RCH_H
#define NATIVEEXPMF6RCH_H

#include <private/util/StdString.h>

namespace MfData
{
  namespace Export
  {
    class NativePackExp;
    class NativeExpMf6Rch
    {
    public:
      NativeExpMf6Rch(NativePackExp* a_);
      ~NativeExpMf6Rch();
      virtual bool Export();

    private:
      NativeExpMf6Rch(const NativeExpMf6Rch& rhs);
      const NativeExpMf6Rch& operator=(const NativeExpMf6Rch& rhs);

      NativePackExp* m_pack;
    };

  }
}

#endif


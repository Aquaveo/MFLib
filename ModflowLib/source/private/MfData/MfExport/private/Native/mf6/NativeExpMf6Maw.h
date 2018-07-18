//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NativeExpMf6Maw_H
#define NativeExpMf6Maw_H

#include <private/util/StdString.h>

namespace MfData
{
  namespace Export
  {
    class NativePackExp;
    class NativeExpMf6Maw
    {
    public:
      NativeExpMf6Maw(NativePackExp* a_);
      ~NativeExpMf6Maw();
      virtual bool Export();

    private:
      NativeExpMf6Maw(const NativeExpMf6Maw& rhs);
      const NativeExpMf6Maw& operator=(const NativeExpMf6Maw& rhs);

      class impl;
      impl* m_p;
    };

  }
}

#endif


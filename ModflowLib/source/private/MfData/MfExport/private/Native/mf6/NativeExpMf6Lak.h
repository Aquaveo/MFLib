//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPMF6LAK_H
#define NATIVEEXPMF6LAK_H

#include <private\util\StdString.h>

namespace MfData
{
  namespace Export
  {
    class NativePackExp;
    class NativeExpMf6Lak
    {
    public:
      NativeExpMf6Lak(NativePackExp* a_);
      ~NativeExpMf6Lak();
      virtual bool Export();

    private:
      NativeExpMf6Lak(const NativeExpMf6Lak& rhs);
      const NativeExpMf6Lak& operator=(const NativeExpMf6Lak& rhs);

      class impl;
      impl* m_p;
    };

  }
}

#endif


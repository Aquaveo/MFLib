//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPMF6EVT_H
#define NATIVEEXPMF6EVT_H

#include <private/util/StdString.h>

namespace MfData
{
  namespace Export
  {
    class NativePackExp;
    class NativeExpMf6Evt
    {
    public:
      NativeExpMf6Evt(NativePackExp* a_);
      ~NativeExpMf6Evt();
      virtual bool Export();

    private:
      NativeExpMf6Evt(const NativeExpMf6Evt& rhs);
      const NativeExpMf6Evt& operator=(const NativeExpMf6Evt& rhs);

      NativePackExp* m_pack;
    };

  }
}

#endif


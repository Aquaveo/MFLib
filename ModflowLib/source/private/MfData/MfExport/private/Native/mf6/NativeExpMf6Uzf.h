//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPMF6UZF_H
#define NATIVEEXPMF6UZF_H

#include <private/util/StdString.h>

namespace MfData
{
  namespace Export
  {
    class NativePackExp;
    class NativeExpMf6Uzf
    {
    public:
      NativeExpMf6Uzf(NativePackExp* a_);
      ~NativeExpMf6Uzf();
      virtual bool Export();

    private:
      NativeExpMf6Uzf(const NativeExpMf6Uzf& rhs);
      const NativeExpMf6Uzf& operator=(const NativeExpMf6Uzf& rhs);

      class impl;
      impl* m_p;
    };

  }
}

#endif


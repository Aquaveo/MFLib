//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPMF6SFR_H
#define NATIVEEXPMF6SFR_H

#include <private\util\StdString.h>

namespace MfData
{
  namespace Export
  {
    class NativePackExp;
    class NativeExpMf6Sfr
    {
    public:
      NativeExpMf6Sfr(NativePackExp* a_);
      ~NativeExpMf6Sfr();
      virtual bool Export();

    private:
      NativeExpMf6Sfr(const NativeExpMf6Sfr& rhs);
      const NativeExpMf6Sfr& operator=(const NativeExpMf6Sfr& rhs);

      class impl;
      impl* m_p;

    };

  }
}

#endif


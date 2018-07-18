//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPMF6MFSIM_H
#define NATIVEEXPMF6MFSIM_H

#include <private/util/StdString.h>

namespace MfData
{
  namespace Export
  {
    class NativePackExp;
    class NativeExpMf6Mfsim
    {
    public:
      NativeExpMf6Mfsim(NativePackExp* a_);
      ~NativeExpMf6Mfsim();
      virtual bool Export();

    private:
      NativeExpMf6Mfsim(const NativeExpMf6Mfsim& rhs);
      const NativeExpMf6Mfsim& operator=(const NativeExpMf6Mfsim& rhs);  
   
      NativePackExp* m_pack;
    };

  }
}

#endif


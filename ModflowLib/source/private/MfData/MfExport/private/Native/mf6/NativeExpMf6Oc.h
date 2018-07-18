//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPMF6OC_H
#define NATIVEEXPMF6OC_H

#include <private/util/StdString.h>

namespace MfData
{
  namespace Export
  {
    class NativePackExp;
    class NativeExpMf6Oc
    {
    public:
      NativeExpMf6Oc(NativePackExp* a_);
      ~NativeExpMf6Oc();
      virtual bool Export();

      void WriteFinal();

    private:
      NativeExpMf6Oc(const NativeExpMf6Oc& rhs);
      const NativeExpMf6Oc& operator=(const NativeExpMf6Oc& rhs);
      int GetNumTimeStepsForPeriod(int a_sp);

      NativePackExp* m_pack;
    };

  }
}

#endif


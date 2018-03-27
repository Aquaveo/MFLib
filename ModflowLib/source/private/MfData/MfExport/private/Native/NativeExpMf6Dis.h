//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPMF6DIS_H
#define NATIVEEXPMF6DIS_H

namespace MfData
{
  namespace Export
  {
    class NativePackExp;
    class NativeExpMf6Dis
    {
    public:
      NativeExpMf6Dis(NativePackExp* a_);
      ~NativeExpMf6Dis();
      virtual bool Export();

    private:
      NativeExpMf6Dis(const NativeExpMf6Dis& rhs);
      const NativeExpMf6Dis& operator=(const NativeExpMf6Dis& rhs);

      NativePackExp* m_pack;
    };

  }
}

#endif


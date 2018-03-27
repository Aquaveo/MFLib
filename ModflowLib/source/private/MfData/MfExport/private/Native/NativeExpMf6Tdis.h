//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef NATIVEEXPMF6TDIS_H
#define NATIVEEXPMF6TDIS_H

namespace MfData
{
  namespace Export
  {
    class NativePackExp;
    class NativeExpMf6Tdis
    {
    public:
      NativeExpMf6Tdis(NativePackExp* a_);
      ~NativeExpMf6Tdis();
      virtual bool Export();

    private:
      NativeExpMf6Tdis(const NativeExpMf6Tdis& rhs);
      const NativeExpMf6Tdis& operator=(const NativeExpMf6Tdis& rhs);

      NativePackExp* m_pack;
    };

  }
}

#endif

